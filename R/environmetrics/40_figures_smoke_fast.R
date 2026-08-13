###############################################################################
# Smoke-only figures module
# Purpose:
#   - Produce a minimal figure set quickly for run-scoped post smoke validation.
#   - Reuse in-memory objects from earlier modules without touching model logic.
###############################################################################

as_trace_vector <- function(x) {
  if (is.null(x)) {
    return(numeric(0))
  }
  if (is.atomic(x)) {
    return(as.numeric(x))
  }
  numeric(0)
}

fetch_numeric_object <- function(name) {
  as_trace_vector(get0(name, ifnotfound = NULL, inherits = TRUE))
}

fetch_first_numeric <- function(candidates) {
  for (nm in candidates) {
    vals <- fetch_numeric_object(nm)
    if (length(vals) > 0L) {
      return(vals)
    }
  }
  numeric(0)
}

plot_trace <- function(values, title_txt, ylab_txt) {
  vals <- as.numeric(values)
  if (length(vals) == 0L || all(!is.finite(vals))) {
    plot.new()
    title(main = paste0(title_txt, " (missing)"))
    return(invisible(NULL))
  }
  plot.ts(vals, main = title_txt, xlab = "Iteration", ylab = ylab_txt, lwd = 1.5)
  invisible(NULL)
}

safe_obj_list <- function(name) {
  obj <- get0(name, ifnotfound = NULL, inherits = TRUE)
  if (!is.list(obj)) {
    return(NULL)
  }
  obj
}

matrix_sample_time <- function(x, horizon = NA_integer_) {
  mat <- as.matrix(x)
  nr <- nrow(mat)
  nc <- ncol(mat)
  if (is.finite(horizon)) {
    hz <- as.integer(horizon[[1L]])
    if (nr == hz && nc != hz) return(t(mat))
    if (nc == hz) return(mat)
  }
  if (nr >= nc) mat else t(mat)
}

col_quantiles <- function(mat, probs = c(0.025, 0.5, 0.975)) {
  m <- matrix_sample_time(mat)
  if (!is.matrix(m) || ncol(m) == 0L) {
    return(matrix(NA_real_, nrow = length(probs), ncol = 0L))
  }
  out <- apply(m, 2, quantile, probs = probs, na.rm = TRUE, type = 8, names = FALSE)
  matrix(out, nrow = length(probs), byrow = FALSE)
}

legacy_univar_q_probs <- c(0.05, 0.20, 0.35, 0.50, 0.65, 0.80, 0.95)

quantile_prob_label <- function(prob) {
  if (!is.finite(prob)) {
    return("NA")
  }
  sprintf("%02d", as.integer(round(prob * 100)))
}

fetch_univar_active_q_probs_smoke <- function(expected_n = NA_integer_) {
  candidates <- list(
    get0("q_s_active", ifnotfound = NULL, inherits = TRUE),
    get0("univar_active_q_probs", ifnotfound = NULL, inherits = TRUE)
  )

  cache_path <- get0("POST_CACHE_DIR", ifnotfound = "", inherits = TRUE)
  if (nzchar(cache_path)) {
    cache_file <- file.path(cache_path, "univar_active_q_probs.rds")
    if (file.exists(cache_file)) {
      candidates[[length(candidates) + 1L]] <- tryCatch(readRDS(cache_file), error = function(...) NULL)
    }
  }

  expected_n_int <- suppressWarnings(as.integer(expected_n[[1L]]))
  for (cand in candidates) {
    vals <- suppressWarnings(as.numeric(cand))
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0L) {
      next
    }
    if (!is.finite(expected_n_int) || length(vals) == expected_n_int) {
      return(vals)
    }
  }

  if (is.finite(expected_n_int) && expected_n_int == length(legacy_univar_q_probs)) {
    return(legacy_univar_q_probs)
  }

  numeric(0)
}

select_univar_quantile_samples <- function(arr, target_prob, q_probs = numeric(0)) {
  if (!(is.array(arr) || is.matrix(arr))) {
    return(NULL)
  }
  dims <- dim(arr)
  if (is.null(dims) || length(dims) < 2L || dims[[1L]] <= 0L) {
    return(NULL)
  }

  if (length(q_probs) == 0L) {
    q_probs <- fetch_univar_active_q_probs_smoke(expected_n = dims[[1L]])
  }
  if (length(q_probs) != dims[[1L]]) {
    if (dims[[1L]] == length(legacy_univar_q_probs)) {
      q_probs <- legacy_univar_q_probs
    } else {
      return(NULL)
    }
  }

  idx <- which.min(abs(q_probs - as.numeric(target_prob)))
  actual_prob <- as.numeric(q_probs[[idx]])
  samples <- if (length(dims) == 3L) {
    arr[idx, , , drop = TRUE]
  } else {
    arr[idx, , drop = TRUE]
  }
  list(
    samples = samples,
    index = idx,
    actual_prob = actual_prob,
    label = quantile_prob_label(actual_prob)
  )
}

pad_to_horizon <- function(x, horizon) {
  out <- rep(NA_real_, horizon)
  vals <- as.numeric(x)
  n <- min(length(vals), horizon)
  if (n > 0L) {
    out[seq_len(n)] <- vals[seq_len(n)]
  }
  out
}

resolve_future_truth <- function(horizon) {
  h <- as.integer(horizon[[1L]])
  truth <- rep(NA_real_, h)

  infer_start_from_forecasts <- function() {
    fallback_start <- if (exists("FORECAST_START_DATE", inherits = TRUE)) {
      suppressWarnings(as.Date(get("FORECAST_START_DATE", inherits = TRUE)))
    } else {
      as.Date("2022-12-26")
    }
    if (is.na(fallback_start)) fallback_start <- as.Date("2022-12-26")

    starts <- as.Date(character(0))
    if (exists("glofas_forecast", inherits = TRUE) && is.data.frame(glofas_forecast) && "target_date" %in% names(glofas_forecast)) {
      d <- suppressWarnings(as.Date(glofas_forecast$target_date))
      d <- d[!is.na(d)]
      if (length(d) > 0L) starts <- c(starts, min(d))
    }
    if (exists("nws_forecast", inherits = TRUE) && is.data.frame(nws_forecast) && "Date" %in% names(nws_forecast)) {
      d <- suppressWarnings(as.Date(nws_forecast$Date))
      d <- d[!is.na(d)]
      if (length(d) > 0L) starts <- c(starts, min(d))
    }
    starts <- starts[!is.na(starts)]
    if (length(starts) > 0L) min(starts) else fallback_start
  }

  start_date <- infer_start_from_forecasts()
  target_dates <- seq.Date(start_date, by = "day", length.out = h)

  # Current publication univariate diagnostics stay on log1p(cms).
  if (exists("San_Lorenzo_Daily_USGS_R", inherits = TRUE) &&
      is.data.frame(San_Lorenzo_Daily_USGS_R) &&
      "data0" %in% names(San_Lorenzo_Daily_USGS_R)) {
    sl <- San_Lorenzo_Daily_USGS_R
    date_col <- if ("Date" %in% names(sl)) {
      suppressWarnings(as.Date(sl$Date))
    } else if ("timestamp" %in% names(sl)) {
      suppressWarnings(as.Date(sl$timestamp))
    } else if ("time" %in% names(sl)) {
      suppressWarnings(as.Date(sl$time))
    } else {
      as.Date(rep(NA_character_, nrow(sl)))
    }
    flow_log1p <- suppressWarnings(as.numeric(sl$data0))
    ok <- !is.na(date_col) & is.finite(flow_log1p) & (flow_log1p > 0)
    if (sum(ok) > 0L) {
      idx_map <- match(target_dates, date_col[ok])
      valid <- !is.na(idx_map)
      if (any(valid)) {
        truth[valid] <- flow_log1p[ok][idx_map[valid]]
      }
    }
  }

  if (all(!is.finite(truth))) {
    idx_tt <- suppressWarnings(as.integer(get0("TT", ifnotfound = NA_integer_, inherits = TRUE)))
    if (is.finite(idx_tt) && exists("Y", inherits = TRUE) && is.matrix(Y) && nrow(Y) >= 1L) {
      obs <- as.numeric(Y[1, ])
      idx <- idx_tt + seq_len(h)
      valid <- idx >= 1L & idx <= length(obs)
      if (any(valid)) {
        truth[valid] <- obs[idx[valid]]
      }
    }
  }

  truth
}

quantile_tag <- function(label) {
  as.character(as.integer(to_quantile_label(label)))
}

load_univar_bundle_with_alias_smoke <- function(path, target_label, source_label = target_label, assign_env = parent.frame()) {
  path <- as.character(path)
  if (!nzchar(path) || !file.exists(path)) {
    return(FALSE)
  }

  source_tag <- quantile_tag(if (is.null(source_label) || !nzchar(as.character(source_label))) target_label else source_label)
  target_tag <- quantile_tag(target_label)

  tmp <- new.env(parent = emptyenv())
  load(path, envir = tmp)
  obj_names <- ls(tmp, all.names = TRUE)
  src_token <- paste0("_", source_tag, "_exAL_synth_DISC_uni")
  tgt_token <- paste0("_", target_tag, "_exAL_synth_DISC_uni")

  for (nm in obj_names) {
    value <- get(nm, envir = tmp, inherits = FALSE)
    assign(nm, value, envir = assign_env)
    if (!identical(source_tag, target_tag)) {
      alias_name <- sub(src_token, tgt_token, nm, fixed = TRUE)
      if (!identical(alias_name, nm)) {
        assign(alias_name, value, envir = assign_env)
      }
    }
  }
  TRUE
}

ensure_univar_bundles_loaded <- function() {
  run_univar <- isTRUE(get0("MODEL_RUN_EXDQLM_UNIVAR", ifnotfound = FALSE, inherits = TRUE))
  if (!run_univar) {
    return(FALSE)
  }
  if (length(fetch_numeric_object("seq.elbo_50_exAL_synth_DISC_uni")) > 0L &&
      !is.null(safe_obj_list("new.theta.out_50_exAL_synth_DISC_uni"))) {
    return(TRUE)
  }

  q_labels <- c("05", "20", "35", "50", "65", "80", "95")
  assign_env <- parent.frame()
  loaded_any <- FALSE
  for (q in q_labels) {
    path <- get0(paste0("UNI_VAR_", q), ifnotfound = "", inherits = TRUE)
    src <- get0(paste0("UNI_VAR_SRC_", q), ifnotfound = q, inherits = TRUE)
    loaded_any <- load_univar_bundle_with_alias_smoke(
      path,
      target_label = q,
      source_label = src,
      assign_env = assign_env
    ) || loaded_any
  }
  loaded_any
}

build_univar_location_forecast_summary <- function() {
  if (!exists("FF", inherits = TRUE) ||
      !exists("GG", inherits = TRUE) ||
      !exists("X_f", inherits = TRUE) ||
      !exists("ranges", inherits = TRUE) ||
      !exists("TT", inherits = TRUE)) {
    return(NULL)
  }

  tt <- suppressWarnings(as.integer(TT[[1L]]))
  horizon <- suppressWarnings(as.integer(ranges[[1L]]))
  if (!is.finite(tt) || !is.finite(horizon) || tt <= 0L || horizon <= 0L) {
    return(NULL)
  }

  p_core <- suppressWarnings(as.integer(get0("p", ifnotfound = 7L, inherits = TRUE)))
  if (!is.finite(p_core) || p_core <= 0L) {
    p_core <- 7L
  }

  x_future <- as.matrix(X_f)
  if (!is.matrix(x_future) || nrow(x_future) == 0L || ncol(x_future) == 0L) {
    return(NULL)
  }
  horizon <- min(horizon, nrow(x_future))
  if (horizon <= 0L) {
    return(NULL)
  }

  px <- ncol(x_future)
  state_dim <- p_core + 1L + px
  if (nrow(FF) < state_dim) {
    state_dim <- nrow(FF)
    px <- max(0L, state_dim - p_core - 1L)
  }
  if (px <= 0L) {
    return(NULL)
  }

  x_future <- x_future[seq_len(horizon), seq_len(px), drop = FALSE]
  if (dim(GG)[1] < p_core || dim(GG)[2] < p_core || dim(GG)[3] < tt) {
    return(NULL)
  }

  delta_vals <- as.numeric(get0("initial_delta", ifnotfound = NA_real_, inherits = TRUE))
  lambda2 <- if (length(delta_vals) >= 6L && is.finite(delta_vals[[6L]])) delta_vals[[6L]] else as.numeric(get0("lam2", ifnotfound = 0.8995, inherits = TRUE))
  if (!is.finite(lambda2)) {
    lambda2 <- 0.8995
  }

  gx_base <- as.matrix(bdiag(GG[1:p_core, 1:p_core, tt], lambda2, diag(px)))
  gx_arr <- array(rep(gx_base, horizon), dim = c(state_dim, state_dim, horizon))
  cov_cols <- (p_core + 2L):(p_core + 1L + px)
  gx_arr[p_core + 1L, cov_cols, ] <- t(x_future)

  ff_vec <- matrix(FF[seq_len(state_dim), 1, 1], ncol = 1)
  ff_vec[p_core + 1L] <- 1

  forecast_mu_path <- function(state_vec) {
    sm <- matrix(as.numeric(state_vec), ncol = 1)
    out <- rep(NA_real_, horizon)
    out[1L] <- sum(ff_vec * sm)
    if (horizon > 1L) {
      for (k in 2:horizon) {
        sm <- gx_arr[, , k] %*% sm
        out[k] <- sum(ff_vec * sm)
      }
    }
    out
  }

  deterministic_mu <- function(q_tag) {
    obj <- safe_obj_list(sprintf("new.theta.out_%s_exAL_synth_DISC_uni", q_tag))
    if (is.null(obj) || is.null(obj$sm) || !is.matrix(obj$sm) || nrow(obj$sm) < state_dim || ncol(obj$sm) < tt) {
      return(numeric(0))
    }
    forecast_mu_path(obj$sm[seq_len(state_dim), tt])
  }

  active_q_probs <- fetch_univar_active_q_probs_smoke()
  resolve_available_tag <- function(target_prob, fallback_tag) {
    candidate_probs <- active_q_probs
    if (length(candidate_probs) > 0L) {
      idx <- which.min(abs(candidate_probs - as.numeric(target_prob)))
      cand_tag <- quantile_prob_label(candidate_probs[[idx]])
      obj <- safe_obj_list(sprintf("new.theta.out_%s_exAL_synth_DISC_uni", cand_tag))
      if (!is.null(obj)) {
        return(list(tag = cand_tag, prob = as.numeric(candidate_probs[[idx]])))
      }
    }
    list(tag = fallback_tag, prob = as.numeric(target_prob))
  }

  low_sel <- resolve_available_tag(0.05, "5")
  mid_sel <- resolve_available_tag(0.50, "50")
  high_sel <- resolve_available_tag(0.95, "95")

  mu_50 <- deterministic_mu(mid_sel$tag)
  if (length(mu_50) == 0L) {
    return(NULL)
  }
  mu_05 <- deterministic_mu(low_sel$tag)
  if (length(mu_05) == 0L) mu_05 <- mu_50
  mu_95 <- deterministic_mu(high_sel$tag)
  if (length(mu_95) == 0L) mu_95 <- mu_50

  q50_samples <- NULL
  samp_50 <- get0("samp.theta_50_exAL_synth_DISC_uni", ifnotfound = NULL, inherits = TRUE)
  if (!is.null(samp_50) && is.array(samp_50) && length(dim(samp_50)) == 3L &&
      dim(samp_50)[1] >= state_dim && dim(samp_50)[2] >= tt && dim(samp_50)[3] >= 1L) {
    n_keep <- min(400L, as.integer(dim(samp_50)[3]))
    q50_samples <- matrix(NA_real_, nrow = n_keep, ncol = horizon)
    for (i in seq_len(n_keep)) {
      q50_samples[i, ] <- forecast_mu_path(samp_50[seq_len(state_dim), tt, i])
    }
  }

  loc_q05 <- rbind(mu_05, mu_05, mu_05)
  loc_q50 <- rbind(mu_50, mu_50, mu_50)
  if (!is.null(q50_samples)) {
    loc_q50 <- col_quantiles(q50_samples, probs = c(0.025, 0.5, 0.975))
  }
  loc_q95 <- rbind(mu_95, mu_95, mu_95)

  list(
    horizon = horizon,
    loc_q05 = loc_q05,
    loc_q50 = loc_q50,
    loc_q95 = loc_q95,
    q50_samples = q50_samples,
    low_prob = if (length(mu_05) > 0L) low_sel$prob else mid_sel$prob,
    mid_prob = mid_sel$prob,
    high_prob = if (length(mu_95) > 0L) high_sel$prob else mid_sel$prob
  )
}

posterior_table_exports_enabled <- post_export_tables_enabled(default = TRUE)
posterior_table_output_dir <- if (exists("OUT_DIR", inherits = TRUE)) {
  file.path(get("OUT_DIR", inherits = TRUE), "tables")
} else {
  file.path(getwd(), "tables")
}
posterior_table_formats <- post_table_formats(default = c("csv"))
posterior_table_keep_na <- TRUE
posterior_table_keep_na_env <- tolower(trimws(Sys.getenv("ENV_SORT_KEEP_NA", "")))
if (identical(posterior_table_keep_na_env, "true")) {
  posterior_table_keep_na <- TRUE
} else if (identical(posterior_table_keep_na_env, "false")) {
  posterior_table_keep_na <- FALSE
}
posterior_table_export_manifest <- NULL
smoke_figure_export_manifest <- NULL

smoke_next_idx_block <- function(prev_idx, block_len) {
  block_len <- suppressWarnings(as.integer(block_len[[1L]]))
  start <- if (length(prev_idx) == 0L) 0L else as.integer(prev_idx[[length(prev_idx)]])
  if (!is.finite(block_len) || block_len <= 0L) return(integer(0))
  seq_len(block_len) + start
}

smoke_forecast_core_dim <- function(seg_id) {
  as.integer(p * (J - as.integer(seg_id) + 2L))
}

smoke_build_usgs_projection_weights <- function(ff_seg, state_len, seg_id, context = "smoke.forecast_projection") {
  state_len <- as.integer(state_len)
  if (!is.finite(state_len) || state_len <= 0L) {
    stop(sprintf("[%s_STATE_LEN] invalid state_len=%s.", context, as.character(state_len)), call. = FALSE)
  }
  if (!is.matrix(ff_seg) || ncol(ff_seg) < 1L) {
    stop(sprintf("[%s_FF_SHAPE] expected FF segment matrix with at least one column.", context), call. = FALSE)
  }

  forecast_transfer_mode_local <- tolower(trimws(Sys.getenv("UNIFIED_MULTIVAR_FORECAST_TRANSFER_MODE", "drop")))
  if (!forecast_transfer_mode_local %in% c("drop", "keep")) {
    forecast_transfer_mode_local <- "drop"
  }
  ppx_local <- if (exists("ppx", inherits = TRUE)) suppressWarnings(as.integer(get("ppx", inherits = TRUE))) else 0L
  use_transfer_forecast_projection <- isTRUE(get0("use_covariates", ifnotfound = FALSE, inherits = TRUE)) &&
    identical(forecast_transfer_mode_local, "keep") &&
    is.finite(ppx_local) &&
    ppx_local > 0L

  ff_n <- nrow(ff_seg)
  weights <- rep(0, state_len)
  base_len <- min(get("p", inherits = TRUE), ff_n, state_len)
  if (base_len > 0L) {
    base_vals <- as.numeric(ff_seg[seq_len(base_len), 1, drop = TRUE])
    base_vals[!is.finite(base_vals)] <- 0
    weights[seq_len(base_len)] <- base_vals
  }

  if (isTRUE(use_transfer_forecast_projection)) {
    core_dim <- smoke_forecast_core_dim(seg_id)
    zeta_idx <- core_dim + 1L
    if (zeta_idx <= ff_n && zeta_idx <= state_len) {
      zeta_w <- as.numeric(ff_seg[zeta_idx, 1, drop = TRUE])
      if (!is.finite(zeta_w)) zeta_w <- 0
      weights[zeta_idx] <- zeta_w
    }
  }

  weights
}

smoke_project_state_gaussian <- function(Mu, Sigma, ff_seg, seg_id, eps_reg = 0, context = "smoke.forecast_projection") {
  if (!is.numeric(Mu)) {
    stop(sprintf("[%s_MU] expected numeric Mu vector.", context), call. = FALSE)
  }
  if (!is.numeric(Sigma) || is.null(dim(Sigma)) || length(dim(Sigma)) != 2L) {
    stop(sprintf("[%s_SIGMA] expected 2D numeric Sigma matrix.", context), call. = FALSE)
  }
  if (nrow(Sigma) < length(Mu) || ncol(Sigma) < length(Mu)) {
    stop(
      sprintf(
        "[%s_SIGMA_DIM] Sigma dims %dx%d do not cover Mu length %d.",
        context, as.integer(nrow(Sigma)), as.integer(ncol(Sigma)), as.integer(length(Mu))
      ),
      call. = FALSE
    )
  }

  w <- smoke_build_usgs_projection_weights(ff_seg, state_len = length(Mu), seg_id = seg_id, context = context)
  idx_use <- which(abs(w) > 0)
  if (length(idx_use) == 0L) {
    return(c(mean = NA_real_, sd = NA_real_))
  }

  Mu_use <- as.numeric(Mu[idx_use])
  Mu_use[!is.finite(Mu_use)] <- 0
  S_use <- as.matrix(Sigma[idx_use, idx_use, drop = FALSE])
  S_use[!is.finite(S_use)] <- 0
  if (is.finite(eps_reg) && eps_reg > 0) {
    S_use <- S_use + diag(length(idx_use)) * eps_reg
  }
  w_use <- as.numeric(w[idx_use])
  mean_use <- sum(w_use * Mu_use)
  var_use <- as.numeric(crossprod(w_use, S_use %*% w_use))
  if (!is.finite(var_use) || var_use < 0) var_use <- 0
  c(mean = mean_use, sd = sqrt(var_use))
}

smoke_multivar_likelihood_mode <- function() {
  lik <- tolower(trimws(Sys.getenv("UNIFIED_EXDQLM_MULTIVAR_LIKELIHOOD_MODE", "exal")))
  if (!lik %in% c("exal", "al")) lik <- "exal"
  lik
}

smoke_multivar_transfer_mode <- function() {
  mode <- tolower(trimws(Sys.getenv("UNIFIED_MULTIVAR_FORECAST_TRANSFER_MODE", "drop")))
  if (!mode %in% c("drop", "keep")) mode <- "drop"
  mode
}

smoke_multivar_meta <- function() {
  mode <- smoke_multivar_transfer_mode()
  lik <- smoke_multivar_likelihood_mode()
  meta <- post_crps_synth_model_meta(
    family = "multivar",
    likelihood_mode = lik,
    transfer_mode = mode
  )
  meta$transfer_mode <- mode
  meta$likelihood_mode <- lik
  meta
}

smoke_read_numeric_matrix_rds <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  mat <- suppressWarnings(as.matrix(obj))
  if (!is.numeric(mat) || nrow(mat) <= 0L || ncol(mat) <= 0L) return(NULL)
  mat
}

smoke_multivar_synthesis_grid_M <- function(default = 1001L) {
  grid_M <- suppressWarnings(as.integer(Sys.getenv("POST_QUANTILE_SYNTHESIS_GRID_M", as.character(default))))
  if (!is.finite(grid_M) || grid_M < 11L) {
    grid_M <- as.integer(default)
  }
  grid_M
}

smoke_multivar_synthesis_sort_draws <- function(default = TRUE) {
  isTRUE(as.logical(Sys.getenv("POST_QUANTILE_SYNTHESIS_SORT_DRAWS_BY_TIME", if (isTRUE(default)) "TRUE" else "FALSE")))
}

smoke_multivar_synthesis_method_tag <- function() {
  post_quantile_synthesis_method_tag(
    enforce_isotonic = TRUE,
    rearrange = TRUE,
    grid_M = smoke_multivar_synthesis_grid_M(),
    method = "exdqlm"
  )
}

smoke_multivar_synthesis_cache_path <- function(base_name, model_id, transfer_mode, method_tag = smoke_multivar_synthesis_method_tag()) {
  post_cache_path(post_quantile_synthesis_cache_file_name(
    base_name = base_name,
    method_tag = method_tag,
    model_id = model_id,
    transfer_mode = transfer_mode
  ))
}

smoke_matrix_quantiles <- function(sample_mat, probs = c(0.05, 0.20, 0.35, 0.50, 0.65, 0.80, 0.95)) {
  if (!is.matrix(sample_mat) || ncol(sample_mat) <= 0L) {
    return(matrix(NA_real_, nrow = length(probs), ncol = 0L))
  }
  out <- apply(sample_mat, 2L, stats::quantile, probs = probs, na.rm = TRUE, type = 8, names = FALSE)
  matrix(out, nrow = length(probs), byrow = FALSE)
}

smoke_resolve_hist_dates <- function() {
  ts_obj <- get0("timestamps", ifnotfound = NULL, inherits = TRUE)
  if (is.null(ts_obj)) {
    ts_obj <- get0("timestamps_keep", ifnotfound = NULL, inherits = TRUE)
  }
  dates <- suppressWarnings(as.Date(ts_obj))
  if (length(dates) > 0L && any(!is.na(dates))) {
    return(dates)
  }
  tt <- suppressWarnings(as.integer(get0("TT", ifnotfound = NA_integer_, inherits = TRUE)))
  fc_start <- suppressWarnings(as.Date(get0("FORECAST_START_DATE", ifnotfound = NA_character_, inherits = TRUE)))
  if (is.finite(tt) && !is.na(fc_start) && tt > 0L) {
    return(seq.Date(fc_start - tt, by = "day", length.out = tt))
  }
  as.Date(character(0))
}

smoke_usgs_log1p_by_dates <- function(dates) {
  dates <- suppressWarnings(as.Date(dates))
  out <- rep(NA_real_, length(dates))
  if (!exists("San_Lorenzo_Daily_USGS_R", inherits = TRUE)) {
    return(out)
  }
  sl <- get("San_Lorenzo_Daily_USGS_R", inherits = TRUE)
  if (!is.data.frame(sl) || !"data0" %in% names(sl)) {
    return(out)
  }
  sl_dates <- if ("Date" %in% names(sl)) {
    suppressWarnings(as.Date(sl$Date))
  } else if ("time" %in% names(sl)) {
    suppressWarnings(as.Date(sl$time))
  } else {
    as.Date(rep(NA_character_, nrow(sl)))
  }
  idx <- match(dates, sl_dates)
  keep <- !is.na(idx)
  if (any(keep)) out[keep] <- as.numeric(sl$data0[idx[keep]])
  out
}

smoke_multivar_quantile_spec <- function() {
  post_requested_quantile_spec()
}

smoke_multivar_can_synthesize_quantile_grid <- function(q_probs, context = "multivar.synthesis") {
  q_probs <- as.numeric(q_probs)
  if (length(q_probs) < 2L) {
    warning(
      sprintf(
        "[%s_SKIP_SINGLE_Q] Skipping multivariate quantile synthesis because at least two active quantile probabilities are required; got %d.",
        context,
        length(q_probs)
      ),
      call. = FALSE
    )
    return(FALSE)
  }
  if (any(!is.finite(q_probs)) || any(q_probs <= 0 | q_probs >= 1) || is.unsorted(q_probs)) {
    warning(
      sprintf(
        "[%s_SKIP_BAD_Q_PROBS] Skipping multivariate quantile synthesis because q_probs are not sorted finite probabilities in (0, 1).",
        context
      ),
      call. = FALSE
    )
    return(FALSE)
  }
  TRUE
}

smoke_multivar_required_object_names <- function(spec) {
  c(
    unlist(lapply(spec$tags, function(tag) sprintf("new.theta.out_%s_exAL_synth_DISC", tag)), use.names = FALSE),
    unlist(lapply(spec$tags, function(tag) sprintf("samp.gamma_%s_exAL_synth_DISC", tag)), use.names = FALSE),
    unlist(lapply(spec$tags, function(tag) sprintf("samp.sigma_%s_exAL_synth_DISC", tag)), use.names = FALSE),
    c("ranges", "J", "FF_list", "p")
  )
}

smoke_build_multivar_synth_f <- function() {
  multivar_meta <- smoke_multivar_meta()
  method_tag <- smoke_multivar_synthesis_method_tag()
  cache_path <- post_cache_path(post_cache_file_name(
    "synth_multivar_forecast_log1p.rds",
    model_id = multivar_meta$model_id,
    transfer_mode = multivar_meta$transfer_mode
  ))
  method_cache_path <- smoke_multivar_synthesis_cache_path(
    "synth_multivar_forecast_log1p.rds",
    model_id = multivar_meta$model_id,
    transfer_mode = multivar_meta$transfer_mode,
    method_tag = method_tag
  )
  quantile_cache_path <- post_cache_path(post_cache_file_name(
    "synth_multivar_forecast_quantiles_log1p.rds",
    model_id = multivar_meta$model_id,
    transfer_mode = multivar_meta$transfer_mode
  ))
  method_quantile_cache_path <- smoke_multivar_synthesis_cache_path(
    "synth_multivar_forecast_quantiles_log1p.rds",
    model_id = multivar_meta$model_id,
    transfer_mode = multivar_meta$transfer_mode,
    method_tag = method_tag
  )
  diagnostics_cache_path <- smoke_multivar_synthesis_cache_path(
    "synth_multivar_forecast_diagnostics.rds",
    model_id = multivar_meta$model_id,
    transfer_mode = multivar_meta$transfer_mode,
    method_tag = method_tag
  )
  cache_draws_path <- post_cache_path(post_cache_file_name(
    "y_reps_f_new_smoke.rds",
    model_id = multivar_meta$model_id,
    transfer_mode = multivar_meta$transfer_mode
  ))

  if (exists("synth_f", inherits = TRUE)) {
    synth_obj <- get("synth_f", inherits = TRUE)
    synth_existing <- as.matrix(synth_obj)
    if (identical(attr(synth_obj, "post_quantile_synthesis_method_tag"), method_tag) &&
        is.numeric(synth_existing) && nrow(synth_existing) > 0L && ncol(synth_existing) > 0L) {
      return(synth_existing)
    }
  }

  synth_cached <- smoke_read_numeric_matrix_rds(method_cache_path)
  if (!is.null(synth_cached)) {
    attr(synth_cached, "post_quantile_synthesis_method_tag") <- method_tag
    saveRDS(synth_cached, file = cache_path)
    diagnostics_cached <- tryCatch(readRDS(diagnostics_cache_path), error = function(e) NULL)
    if (is.list(diagnostics_cached)) {
      smoke_write_quantile_synthesis_diagnostics(
        result = structure(list(diagnostics = diagnostics_cached), class = "post_quantile_synthesis"),
        model_id = multivar_meta$model_id,
        segment = "forecast"
      )
    }
    return(synth_cached)
  }

  spec <- smoke_multivar_quantile_spec()
  q_probs <- spec$probs
  if (!smoke_multivar_can_synthesize_quantile_grid(
    q_probs,
    context = sprintf("%s.multivar.forecast_synthesis", multivar_meta$model_id)
  )) {
    return(NULL)
  }
  required_objs <- smoke_multivar_required_object_names(spec)
  missing_objs <- required_objs[!vapply(required_objs, exists, logical(1), inherits = TRUE)]
  if (length(missing_objs) > 0L) {
    warning(
      sprintf(
        "[CRPS_MULTIVAR_SKIP] Unable to compute multivariate synthesis CRPS (missing objects: %s).",
        paste(missing_objs, collapse = ", ")
      ),
      call. = FALSE
    )
    return(NULL)
  }

  theta_objs <- lapply(spec$tags, function(tag) get(sprintf("new.theta.out_%s_exAL_synth_DISC", tag), inherits = TRUE))
  gamma_mats <- lapply(spec$tags, function(tag) get(sprintf("samp.gamma_%s_exAL_synth_DISC", tag), inherits = TRUE))
  sigma_mats <- lapply(spec$tags, function(tag) get(sprintf("samp.sigma_%s_exAL_synth_DISC", tag), inherits = TRUE))

  n_samp <- min(vapply(gamma_mats, function(x) if (is.null(dim(x))) length(x) else dim(x)[2], integer(1)))
  if (!is.finite(n_samp) || n_samp <= 1L) {
    warning("[CRPS_MULTIVAR_SKIP] Unable to compute multivariate synthesis CRPS (invalid sample size).", call. = FALSE)
    return(NULL)
  }
  horizon <- suppressWarnings(as.integer(get("ranges", inherits = TRUE)[[1L]]))
  if (!is.finite(horizon) || horizon <= 0L) {
    warning("[CRPS_MULTIVAR_SKIP] Unable to compute multivariate synthesis CRPS (invalid forecast horizon).", call. = FALSE)
    return(NULL)
  }

  xbs <- array(NA_real_, c(length(q_probs), horizon, n_samp))
  ks <- -diff(c(as.integer(get("ranges", inherits = TRUE)), 0L))
  J_use <- min(
    suppressWarnings(as.integer(get("J", inherits = TRUE))),
    length(get("FF_list", inherits = TRUE)),
    min(vapply(theta_objs, function(obj) min(length(obj$sm_ens), length(obj$sC_ens)), integer(1)))
  )
  idx <- c(0L)

  for (j in seq_len(J_use)) {
    idx <- smoke_next_idx_block(idx, ks[J_use - j + 1L])
    if (length(idx) == 0L) next

    seg_cap <- min(vapply(theta_objs, function(obj) {
      sm_j <- obj$sm_ens[[j]]
      sC_j <- obj$sC_ens[[j]]
      if (!is.numeric(sm_j) || is.null(dim(sm_j)) || length(dim(sm_j)) != 2L ||
          !is.numeric(sC_j) || is.null(dim(sC_j)) || length(dim(sC_j)) != 3L) {
        return(0L)
      }
      as.integer(min(ncol(sm_j), dim(sC_j)[3]))
    }, integer(1)))
    if (!is.finite(seg_cap) || seg_cap <= 0L) next

    tt <- 1L
    for (t_idx in idx[seq_len(min(length(idx), seg_cap))]) {
      for (row_idx in seq_along(theta_objs)) {
        Mu <- theta_objs[[row_idx]]$sm_ens[[j]][, tt]
        Sigma <- theta_objs[[row_idx]]$sC_ens[[j]][, , tt]
        proj <- smoke_project_state_gaussian(
          Mu = Mu,
          Sigma = Sigma,
          ff_seg = get("FF_list", inherits = TRUE)[[j]],
          seg_id = j,
          eps_reg = 0,
          context = sprintf("smoke_xbs_qrow%d_seg%d_t%d", as.integer(row_idx), as.integer(j), as.integer(tt))
        )
        mu_use <- as.numeric(proj[["mean"]])
        sd_use <- as.numeric(proj[["sd"]])
        if (!is.finite(mu_use)) mu_use <- NA_real_
        if (!is.finite(sd_use) || sd_use < 0) sd_use <- 0
        xbs[row_idx, t_idx, ] <- stats::rnorm(n = n_samp, mean = mu_use, sd = sd_use)
      }
      tt <- tt + 1L
    }
  }

  for (t_idx in seq_len(horizon)) {
    for (row_idx in seq_len(dim(xbs)[1])) {
      xbs[row_idx, t_idx, ] <- sort_keep_na(xbs[row_idx, t_idx, ])
    }
  }

  y_reps_f_new <- array(NA_real_, c(length(q_probs), n_samp, horizon))
  gamma_vecs <- lapply(gamma_mats, function(mat) as.numeric(mat[1L, seq_len(n_samp)]))
  sigma_vecs <- lapply(sigma_mats, function(mat) as.numeric(mat[1L, seq_len(n_samp)]))

  for (t_idx in seq_len(horizon)) {
    for (s_idx in seq_len(n_samp)) {
      for (row_idx in seq_along(q_probs)) {
        y_reps_f_new[row_idx, s_idx, t_idx] <- exdqlm::rexal(
          1L,
          q_probs[[row_idx]],
          xbs[row_idx, t_idx, s_idx],
          sigma_vecs[[row_idx]][[s_idx]],
          gamma_vecs[[row_idx]][[s_idx]]
        )
      }
    }
  }

  for (t_idx in seq_len(horizon)) {
    for (row_idx in seq_len(dim(y_reps_f_new)[1])) {
      y_reps_f_new[row_idx, , t_idx] <- sort_keep_na(y_reps_f_new[row_idx, , t_idx])
    }
  }

  forecast_log1p_guard <- post_transform_internal_array_to_log1p(
    y_reps_f_new,
    from_scale = post_resolve_analysis_scale_post_internal(),
    context = sprintf("%s.multivar.forecast_internal", multivar_meta$model_id),
    report_path = post_cache_path(post_cache_file_name(
      "synth_multivar_forecast_exp_guard.txt",
      model_id = multivar_meta$model_id,
      transfer_mode = multivar_meta$transfer_mode
    ))
  )
  synth_result <- post_synthesize_rearranged_sample_cube(
    sample_cube = forecast_log1p_guard$values,
    q_probs = q_probs,
    n_samp = n_samp,
    seed = suppressWarnings(as.integer(Sys.getenv("DISC_BASE_SEED", "777"))) + 612L,
    enforce_isotonic = TRUE,
    rearrange = TRUE,
    grid_M = smoke_multivar_synthesis_grid_M(),
    sort_draws_by_time = smoke_multivar_synthesis_sort_draws(),
    context = sprintf("%s.multivar.forecast_synthesis", multivar_meta$model_id)
  )
  synth_f <- synth_result$sample_mat
  attr(synth_f, "post_quantile_synthesis_method_tag") <- synth_result$method_tag

  saveRDS(y_reps_f_new, file = cache_draws_path)
  saveRDS(synth_result$quantiles, file = method_quantile_cache_path)
  saveRDS(synth_result$quantiles, file = quantile_cache_path)
  saveRDS(synth_result$diagnostics, file = diagnostics_cache_path)
  smoke_write_quantile_synthesis_diagnostics(
    result = synth_result,
    model_id = multivar_meta$model_id,
    segment = "forecast"
  )
  saveRDS(synth_f, file = method_cache_path)
  saveRDS(synth_f, file = cache_path)
  synth_f
}

smoke_build_multivar_forecast_location_summary <- function() {
  meta <- smoke_multivar_meta()
  cache_path <- post_cache_path(post_cache_file_name(
    "multivar_forecast_usgs_location_summary_log1p.rds",
    model_id = meta$model_id,
    transfer_mode = meta$transfer_mode
  ))
  if (file.exists(cache_path)) {
    cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (is.list(cached) && is.matrix(cached$mean_mat)) {
      return(cached)
    }
  }

  spec <- smoke_multivar_quantile_spec()
  required_objs <- smoke_multivar_required_object_names(spec)
  missing_objs <- required_objs[!vapply(required_objs, exists, logical(1), inherits = TRUE)]
  if (length(missing_objs) > 0L) {
    return(NULL)
  }

  q_probs <- spec$probs
  q_labels <- sprintf("q%02d", as.integer(round(100 * q_probs)))
  theta_objs <- lapply(spec$tags, function(tag) get(sprintf("new.theta.out_%s_exAL_synth_DISC", tag), inherits = TRUE))
  horizon <- suppressWarnings(as.integer(get("ranges", inherits = TRUE)[[1L]]))
  if (!is.finite(horizon) || horizon <= 0L) {
    return(NULL)
  }
  mean_internal <- matrix(NA_real_, nrow = length(q_probs), ncol = horizon)
  sd_internal <- matrix(NA_real_, nrow = length(q_probs), ncol = horizon)
  ks <- -diff(c(as.integer(get("ranges", inherits = TRUE)), 0L))
  J_use <- min(
    suppressWarnings(as.integer(get("J", inherits = TRUE))),
    length(get("FF_list", inherits = TRUE)),
    min(vapply(theta_objs, function(obj) min(length(obj$sm_ens), length(obj$sC_ens)), integer(1)))
  )
  idx <- c(0L)

  for (j in seq_len(J_use)) {
    idx <- smoke_next_idx_block(idx, ks[J_use - j + 1L])
    if (length(idx) == 0L) next
    seg_cap <- min(vapply(theta_objs, function(obj) {
      sm_j <- obj$sm_ens[[j]]
      sC_j <- obj$sC_ens[[j]]
      if (!is.numeric(sm_j) || is.null(dim(sm_j)) || length(dim(sm_j)) != 2L ||
          !is.numeric(sC_j) || is.null(dim(sC_j)) || length(dim(sC_j)) != 3L) {
        return(0L)
      }
      as.integer(min(ncol(sm_j), dim(sC_j)[3]))
    }, integer(1)))
    if (!is.finite(seg_cap) || seg_cap <= 0L) next

    tt <- 1L
    for (t_idx in idx[seq_len(min(length(idx), seg_cap))]) {
      for (row_idx in seq_along(theta_objs)) {
        Mu <- theta_objs[[row_idx]]$sm_ens[[j]][, tt]
        Sigma <- theta_objs[[row_idx]]$sC_ens[[j]][, , tt]
        proj <- smoke_project_state_gaussian(
          Mu = Mu,
          Sigma = Sigma,
          ff_seg = get("FF_list", inherits = TRUE)[[j]],
          seg_id = j,
          eps_reg = 0,
          context = sprintf("smoke_loc_qrow%d_seg%d_t%d", as.integer(row_idx), as.integer(j), as.integer(tt))
        )
        mean_internal[row_idx, t_idx] <- as.numeric(proj[["mean"]])
        sd_internal[row_idx, t_idx] <- as.numeric(proj[["sd"]])
      }
      tt <- tt + 1L
    }
  }

  mean_mat <- matrix(NA_real_, nrow = nrow(mean_internal), ncol = ncol(mean_internal))
  q025_mat <- matrix(NA_real_, nrow = nrow(mean_internal), ncol = ncol(mean_internal))
  q500_mat <- matrix(NA_real_, nrow = nrow(mean_internal), ncol = ncol(mean_internal))
  q975_mat <- matrix(NA_real_, nrow = nrow(mean_internal), ncol = ncol(mean_internal))
  for (row_idx in seq_len(nrow(mean_internal))) {
    mean_mat[row_idx, ] <- as.numeric(post_transform_internal_to_log1p_mat(
      matrix(mean_internal[row_idx, ], nrow = 1L),
      from_scale = post_resolve_analysis_scale_post_internal(),
      context = sprintf("%s.loc_forecast_mean.%s", meta$model_id, q_labels[[row_idx]])
    ))
    loc_band <- smoke_quantile_band_from_moments(
      mean_vec = mean_internal[row_idx, ],
      var_vec = sd_internal[row_idx, ]^2,
      probs = c(0.025, 0.5, 0.975),
      transform = "internal_to_log1p",
      context = sprintf("%s.loc_forecast_band.%s", meta$model_id, q_labels[[row_idx]])
    )
    q025_mat[row_idx, ] <- loc_band[1L, ]
    q500_mat[row_idx, ] <- loc_band[2L, ]
    q975_mat[row_idx, ] <- loc_band[3L, ]
  }

  summary <- list(
    model_id = meta$model_id,
    transfer_mode = meta$transfer_mode,
    dates = as.character(smoke_forecast_dates(horizon)),
    q_labels = q_labels,
    q_probs = as.numeric(q_probs),
    mean_mat = mean_mat,
    q025_mat = q025_mat,
    q500_mat = q500_mat,
    q975_mat = q975_mat
  )
  saveRDS(summary, cache_path)
  summary
}

smoke_build_ndlm_main_raw_draws <- function() {
  if (exists("xbs_ndlm", inherits = TRUE)) {
    ndlm_existing <- get("xbs_ndlm", inherits = TRUE)
    if (is.numeric(ndlm_existing) && !is.null(dim(ndlm_existing)) && length(dim(ndlm_existing)) == 3L) {
      return(ndlm_existing)
    }
  }
  if (!exists("new.theta.out_50_NDLM_synth_DISC", inherits = TRUE)) {
    return(NULL)
  }

  ndlm_obj <- get("new.theta.out_50_NDLM_synth_DISC", inherits = TRUE)
  if (is.list(ndlm_obj) &&
      is.matrix(ndlm_obj$forecast_mean_draws_loglog1p) &&
      is.numeric(ndlm_obj$forecast_mean_draws_loglog1p) &&
      nrow(ndlm_obj$forecast_mean_draws_loglog1p) > 1L &&
      ncol(ndlm_obj$forecast_mean_draws_loglog1p) > 0L &&
      all(is.finite(ndlm_obj$forecast_mean_draws_loglog1p))) {
    ndlm_direct_mean_draws <- as.matrix(ndlm_obj$forecast_mean_draws_loglog1p)
    xbs_ndlm <- array(NA_real_, c(1L, ncol(ndlm_direct_mean_draws), nrow(ndlm_direct_mean_draws)))
    xbs_ndlm[1L, , ] <- t(ndlm_direct_mean_draws)
    return(xbs_ndlm)
  }

  if (!exists("samp.sigma_50_NDLM_synth_DISC", inherits = TRUE) ||
      !exists("ranges", inherits = TRUE) ||
      !exists("FF_list", inherits = TRUE)) {
    return(NULL)
  }

  n_samp_ndlm <- suppressWarnings(as.integer(length(as.numeric(get("samp.sigma_50_NDLM_synth_DISC", inherits = TRUE)))))
  if (!is.finite(n_samp_ndlm) || n_samp_ndlm <= 1L) {
    return(NULL)
  }

  post_build_ndlm_state_draw_array(
    ndlm_obj = ndlm_obj,
    ranges = get("ranges", inherits = TRUE),
    FF_list = get("FF_list", inherits = TRUE),
    n_samp = n_samp_ndlm,
    p_state = if (exists("p", inherits = TRUE)) get("p", inherits = TRUE) else 7L,
    eps_reg = 0,
    seed = 777L,
    context = "crps.ndlm.smoke"
  )
}

smoke_inverse_cdf_al <- function(U, mu, sigma, p) {
  ifelse(
    U < p,
    mu + (sigma / (1 - p)) * log(U / p),
    mu - (sigma / p) * log((1 - U) / (1 - p))
  )
}

smoke_window_dates <- function(start_date, end_date) {
  start_date <- suppressWarnings(as.Date(start_date))
  end_date <- suppressWarnings(as.Date(end_date))
  if (is.na(start_date) || is.na(end_date) || end_date < start_date) {
    return(as.Date(character(0)))
  }
  seq.Date(start_date, end_date, by = "day")
}

smoke_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  post_write_csv_deterministic(df, path, numeric_digits = 15L)
  invisible(path)
}

smoke_write_quantile_synthesis_diagnostics <- function(result, model_id, segment) {
  if (!inherits(result, "post_quantile_synthesis")) {
    return(invisible(NULL))
  }
  diagnostics <- result$diagnostics
  if (!is.list(diagnostics)) {
    return(invisible(NULL))
  }

  prefix <- sprintf("%s_%s_quantile_synthesis", as.character(model_id), as.character(segment))
  add_meta <- function(df, table_name) {
    if (!is.data.frame(df)) return(NULL)
    cbind(
      data.frame(
        model_id = as.character(model_id),
        segment = as.character(segment),
        table_name = as.character(table_name),
        stringsAsFactors = FALSE
      ),
      df
    )
  }

  tables <- list(
    summary = diagnostics$summary,
    raw_sample_crossing_per_time = diagnostics$raw_sample_crossing$per_time,
    raw_curve_crossing_per_time = diagnostics$raw_curve_crossing$per_time,
    anchor_curve_crossing_per_time = diagnostics$anchor_curve_crossing$per_time,
    empirical_curve_crossing_per_time = diagnostics$empirical_curve_crossing$per_time
  )

  for (nm in names(tables)) {
    df <- add_meta(tables[[nm]], nm)
    if (!is.null(df)) {
      smoke_write_csv(df, file.path(OUT_DIR, sprintf("%s_%s.csv", prefix, nm)))
    }
  }

  invisible(NULL)
}

smoke_quantile_df <- function(model_id, dates, observed, quantile_mat, probs, segment, center_name = "q50") {
  out <- data.frame(
    model_id = as.character(model_id),
    date = as.character(as.Date(dates)),
    segment = as.character(segment),
    observed = as.numeric(observed),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(probs)) {
    out[[sprintf("q%02d", as.integer(round(100 * probs[[i]])))]] <- as.numeric(quantile_mat[i, ])
  }
  if (!(center_name %in% names(out)) && nrow(quantile_mat) >= 1L) {
    out[[center_name]] <- as.numeric(quantile_mat[ceiling(nrow(quantile_mat) / 2L), ])
  }
  out
}

smoke_sample_subset_df <- function(model_id, sample_mat, dates, segment, cap = 128L) {
  empty_df <- data.frame(
    model_id = character(0),
    draw_id = character(0),
    sample_index = integer(0),
    date = character(0),
    segment = character(0),
    value = numeric(0),
    stringsAsFactors = FALSE
  )
  if (!is.matrix(sample_mat) || nrow(sample_mat) <= 0L || ncol(sample_mat) <= 0L) {
    return(empty_df)
  }
  idx <- post_plot_sample_indices(nrow(sample_mat), cap = cap)
  if (length(idx) == 0L) return(empty_df)
  sub_mat <- sample_mat[idx, , drop = FALSE]
  out <- data.frame(
    model_id = as.character(model_id),
    draw_id = rep(sprintf("draw_%03d", seq_along(idx)), each = ncol(sub_mat)),
    sample_index = rep(idx, each = ncol(sub_mat)),
    date = rep(as.character(as.Date(dates)), times = nrow(sub_mat)),
    segment = as.character(segment),
    value = as.numeric(t(sub_mat)),
    stringsAsFactors = FALSE
  )
  out
}

smoke_synthesis_y_limits <- function(default = c(0, 6.5)) {
  raw <- trimws(Sys.getenv("UNIFIED_POST_SYNTHESIS_Y_LIMITS", ""))
  vals <- if (nzchar(raw)) {
    suppressWarnings(as.numeric(strsplit(raw, ",", fixed = TRUE)[[1L]]))
  } else {
    as.numeric(default)
  }
  if (length(vals) != 2L || any(!is.finite(vals)) || vals[[1L]] >= vals[[2L]]) {
    stop("UNIFIED_POST_SYNTHESIS_Y_LIMITS must be two increasing numeric values, e.g. 0,6.5", call. = FALSE)
  }
  vals
}

smoke_plot_synthesis_window <- function(
  model_id,
  title_text,
  out_file,
  hist_dates,
  hist_obs,
  hist_samples,
  hist_q,
  fc_dates,
  fc_obs,
  fc_samples,
  fc_q,
  probs = c(0.05, 0.20, 0.35, 0.50, 0.65, 0.80, 0.95)
) {
  all_dates <- c(as.Date(hist_dates), as.Date(fc_dates))
  ylim_vals <- smoke_synthesis_y_limits()

  open_png(out_file, width = 3200, height = 1600, res = 320)
  on.exit(dev.off(), add = TRUE)

  plot(all_dates, c(as.numeric(hist_obs), as.numeric(fc_obs)), type = "n",
       xlab = "Date", ylab = "USGS / model scale (log1p cms)",
       main = title_text, ylim = ylim_vals)
  abline(v = as.Date(CUTOFF_DATE), lty = 2, col = "black", lwd = 1.2)

  if (is.matrix(hist_samples) && nrow(hist_samples) > 0L) {
    hist_idx <- post_plot_sample_indices(nrow(hist_samples), cap = 128L)
    matlines(as.Date(hist_dates), t(hist_samples[hist_idx, , drop = FALSE]), col = "#bdbdbd55", lwd = 0.6)
  }
  if (is.matrix(fc_samples) && nrow(fc_samples) > 0L) {
    fc_idx <- post_plot_sample_indices(nrow(fc_samples), cap = 128L)
    matlines(as.Date(fc_dates), t(fc_samples[fc_idx, , drop = FALSE]), col = "#9ecae155", lwd = 0.6)
  }

  line_cols <- c("#b2182b", "#d6604d", "#f4a582", "#1b7837", "#92c5de", "#4393c3", "#2166ac")
  for (i in seq_along(probs)) {
    lines(as.Date(hist_dates), hist_q[i, ], col = line_cols[i], lwd = if (i == 4L) 2.2 else 1.3)
    lines(as.Date(fc_dates), fc_q[i, ], col = line_cols[i], lwd = if (i == 4L) 2.2 else 1.3)
  }

  lines(as.Date(hist_dates), hist_obs, col = "black", lwd = 1.6)
  lines(as.Date(fc_dates), fc_obs, col = "black", lwd = 1.6)
  legend(
    "topleft",
    legend = c("Observed USGS", "Posterior/predictive spaghetti", paste0("q=", sprintf("%0.2f", probs))),
    col = c("black", "#9ecae1", line_cols[4L]),
    lwd = c(1.6, 1.0, 2.2),
    bty = "n"
  )
}

smoke_plot_ndlm_window <- function(
  model_id,
  title_text,
  out_file,
  hist_dates,
  hist_obs,
  hist_q,
  fc_dates,
  fc_obs,
  fc_q
) {
  all_dates <- c(as.Date(hist_dates), as.Date(fc_dates))
  ylim_vals <- range(c(hist_obs, fc_obs, hist_q, fc_q), finite = TRUE)
  if (!all(is.finite(ylim_vals))) ylim_vals <- c(0, 1)

  open_png(out_file, width = 3200, height = 1600, res = 320)
  on.exit(dev.off(), add = TRUE)

  plot(all_dates, c(as.numeric(hist_obs), as.numeric(fc_obs)), type = "n",
       xlab = "Date", ylab = "USGS / model scale (log1p cms)",
       main = title_text, ylim = ylim_vals)
  abline(v = as.Date(CUTOFF_DATE), lty = 2, col = "black", lwd = 1.2)

  polygon(c(as.Date(hist_dates), rev(as.Date(hist_dates))),
          c(hist_q[1, ], rev(hist_q[3, ])),
          col = "#a6bddb55", border = NA)
  polygon(c(as.Date(fc_dates), rev(as.Date(fc_dates))),
          c(fc_q[1, ], rev(fc_q[3, ])),
          col = "#74a9cf55", border = NA)

  lines(as.Date(hist_dates), hist_q[2, ], col = "#045a8d", lwd = 2.0)
  lines(as.Date(fc_dates), fc_q[2, ], col = "#045a8d", lwd = 2.0)
  lines(as.Date(hist_dates), hist_obs, col = "black", lwd = 1.6)
  lines(as.Date(fc_dates), fc_obs, col = "black", lwd = 1.6)
  legend(
    "topleft",
    legend = c("Observed USGS", "Model median", "90% band"),
    col = c("black", "#045a8d", "#74a9cf"),
    lwd = c(1.6, 2.0, 6.0),
    bty = "n"
  )
}

smoke_build_multivar_hist_synth <- function() {
  meta <- smoke_multivar_meta()
  method_tag <- smoke_multivar_synthesis_method_tag()
  hist_cache <- post_cache_path(post_cache_file_name(
    "synth_multivar_hist_log1p.rds",
    model_id = meta$model_id,
    transfer_mode = meta$transfer_mode
  ))
  hist_method_cache <- smoke_multivar_synthesis_cache_path(
    "synth_multivar_hist_log1p.rds",
    model_id = meta$model_id,
    transfer_mode = meta$transfer_mode,
    method_tag = method_tag
  )
  hist_q_cache <- post_cache_path(post_cache_file_name(
    "synth_multivar_hist_quantiles_log1p.rds",
    model_id = meta$model_id,
    transfer_mode = meta$transfer_mode
  ))
  hist_q_method_cache <- smoke_multivar_synthesis_cache_path(
    "synth_multivar_hist_quantiles_log1p.rds",
    model_id = meta$model_id,
    transfer_mode = meta$transfer_mode,
    method_tag = method_tag
  )
  diagnostics_cache_path <- smoke_multivar_synthesis_cache_path(
    "synth_multivar_hist_diagnostics.rds",
    model_id = meta$model_id,
    transfer_mode = meta$transfer_mode,
    method_tag = method_tag
  )

  hist_dates_all <- smoke_resolve_hist_dates()
  hist_idx <- which(hist_dates_all >= as.Date(PLOT_START_DATE) & hist_dates_all <= as.Date(CUTOFF_DATE))
  if (length(hist_idx) <= 0L) {
    return(NULL)
  }

  hist_cached <- smoke_read_numeric_matrix_rds(hist_method_cache)
  hist_q_cached <- smoke_read_numeric_matrix_rds(hist_q_method_cache)
  if (!is.null(hist_cached) && !is.null(hist_q_cached) &&
      ncol(hist_cached) == length(hist_idx) && ncol(hist_q_cached) == length(hist_idx)) {
    attr(hist_cached, "post_quantile_synthesis_method_tag") <- method_tag
    saveRDS(hist_cached, hist_cache)
    saveRDS(hist_q_cached, hist_q_cache)
    diagnostics_cached <- tryCatch(readRDS(diagnostics_cache_path), error = function(e) NULL)
    if (is.list(diagnostics_cached)) {
      smoke_write_quantile_synthesis_diagnostics(
        result = structure(list(diagnostics = diagnostics_cached), class = "post_quantile_synthesis"),
        model_id = meta$model_id,
        segment = "history"
      )
    }
    return(list(
      sample_mat = hist_cached,
      quantiles = hist_q_cached,
      dates = hist_dates_all[hist_idx]
    ))
  }

  spec <- smoke_multivar_quantile_spec()
  q_tags <- spec$tags
  q_probs <- spec$probs
  if (!smoke_multivar_can_synthesize_quantile_grid(
    q_probs,
    context = sprintf("%s.multivar.hist_synthesis", meta$model_id)
  )) {
    return(NULL)
  }
  required_objs <- c("FF", unlist(lapply(q_tags, function(tag) {
    c(
      sprintf("samp.theta_%s_exAL_synth_DISC", tag),
      sprintf("samp.sts_%s_exAL_synth_DISC", tag),
      sprintf("samp.gamma_%s_exAL_synth_DISC", tag),
      sprintf("samp.sigma_%s_exAL_synth_DISC", tag)
    )
  }), use.names = FALSE))
  if (any(!vapply(required_objs, exists, logical(1), inherits = TRUE))) {
    return(NULL)
  }

  n_samp <- min(vapply(q_tags, function(q) {
    theta_obj <- get(sprintf("samp.theta_%s_exAL_synth_DISC", q), inherits = TRUE)
    dim(theta_obj$samp_theta)[3]
  }, integer(1)))
  if (!is.finite(n_samp) || n_samp <= 1L) {
    return(NULL)
  }

  y_hist_cube <- array(NA_real_, c(length(q_tags), n_samp, length(hist_idx)))
  FF_use <- get("FF", inherits = TRUE)

  for (i in seq_along(q_tags)) {
    q_tag <- q_tags[[i]]
    p0 <- q_probs[[i]]
    theta_obj <- get(sprintf("samp.theta_%s_exAL_synth_DISC", q_tag), inherits = TRUE)
    th <- theta_obj$samp_theta[, , seq_len(n_samp), drop = FALSE]
    sts_arr <- get(sprintf("samp.sts_%s_exAL_synth_DISC", q_tag), inherits = TRUE)[1L, hist_idx, seq_len(n_samp), drop = FALSE]
    stj <- matrix(sts_arr, nrow = length(hist_idx), ncol = n_samp)
    gamj <- as.numeric(get(sprintf("samp.gamma_%s_exAL_synth_DISC", q_tag), inherits = TRUE)[1L, seq_len(n_samp)])
    sigj <- as.numeric(get(sprintf("samp.sigma_%s_exAL_synth_DISC", q_tag), inherits = TRUE)[1L, seq_len(n_samp)])
    p_exAL <- p_fn(p0, gamj)

    xb <- matrix(NA_real_, nrow = length(hist_idx), ncol = n_samp)
    for (k in seq_along(hist_idx)) {
      t_idx <- hist_idx[[k]]
      th_t <- matrix(th[, t_idx, ], nrow = dim(th)[1], ncol = n_samp)
      p_use <- min(nrow(FF_use), nrow(th_t))
      xb[k, ] <- as.vector(t(FF_use[seq_len(p_use), 1L, t_idx, drop = FALSE][, 1L, 1L]) %*% th_t[seq_len(p_use), , drop = FALSE])
    }

    set.seed(770L + i)
    u_values <- matrix(stats::runif(length(hist_idx) * n_samp), nrow = length(hist_idx), ncol = n_samp)
    mu <- xb + sweep(stj, 2L, sigj * abs(gamj) * C_fn(p0, gamj), `*`)
    y_hist <- t(smoke_inverse_cdf_al(u_values, mu, sigj, p_exAL))
    for (k in seq_len(ncol(y_hist))) {
      y_hist[, k] <- sort_keep_na(y_hist[, k])
    }
    y_hist_cube[i, , ] <- post_transform_internal_to_log1p_mat(
      y_hist,
      from_scale = post_resolve_analysis_scale_post_internal(),
      context = sprintf("%s.multivar.hist_internal.q%s", meta$model_id, q_tag)
    )
  }

  synth_result <- post_synthesize_rearranged_sample_cube(
    sample_cube = y_hist_cube,
    q_probs = q_probs,
    n_samp = n_samp,
    seed = suppressWarnings(as.integer(Sys.getenv("DISC_BASE_SEED", "777"))) + 611L,
    enforce_isotonic = TRUE,
    rearrange = TRUE,
    grid_M = smoke_multivar_synthesis_grid_M(),
    sort_draws_by_time = smoke_multivar_synthesis_sort_draws(),
    context = sprintf("%s.multivar.hist_synthesis", meta$model_id)
  )
  synth_hist <- synth_result$sample_mat
  attr(synth_hist, "post_quantile_synthesis_method_tag") <- synth_result$method_tag
  hist_q <- synth_result$quantiles
  saveRDS(synth_hist, hist_method_cache)
  saveRDS(hist_q, hist_q_method_cache)
  saveRDS(synth_result$diagnostics, diagnostics_cache_path)
  smoke_write_quantile_synthesis_diagnostics(
    result = synth_result,
    model_id = meta$model_id,
    segment = "history"
  )
  saveRDS(synth_hist, hist_cache)
  saveRDS(hist_q, hist_q_cache)
  list(sample_mat = synth_hist, quantiles = hist_q, dates = hist_dates_all[hist_idx])
}

smoke_build_multivar_hist_location_summary <- function() {
  meta <- smoke_multivar_meta()
  cache_path <- post_cache_path(post_cache_file_name(
    "multivar_hist_usgs_location_summary_log1p.rds",
    model_id = meta$model_id,
    transfer_mode = meta$transfer_mode
  ))
  if (file.exists(cache_path)) {
    cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (is.list(cached) && is.matrix(cached$mean_mat)) {
      return(cached)
    }
  }

  hist_dates_all <- smoke_resolve_hist_dates()
  hist_idx <- which(hist_dates_all >= as.Date(PLOT_START_DATE) & hist_dates_all <= as.Date(CUTOFF_DATE))
  if (length(hist_idx) <= 0L) {
    return(NULL)
  }

  spec <- smoke_multivar_quantile_spec()
  q_tags <- spec$tags
  q_probs <- spec$probs
  q_labels <- sprintf("q%02d", as.integer(round(100 * q_probs)))
  required_objs <- c("FF", unlist(lapply(q_tags, function(tag) {
    c(
      sprintf("samp.theta_%s_exAL_synth_DISC", tag),
      sprintf("samp.sts_%s_exAL_synth_DISC", tag),
      sprintf("samp.gamma_%s_exAL_synth_DISC", tag),
      sprintf("samp.sigma_%s_exAL_synth_DISC", tag)
    )
  }), use.names = FALSE))
  if (any(!vapply(required_objs, exists, logical(1), inherits = TRUE))) {
    return(NULL)
  }

  n_samp <- min(vapply(q_tags, function(q) {
    theta_obj <- get(sprintf("samp.theta_%s_exAL_synth_DISC", q), inherits = TRUE)
    dim(theta_obj$samp_theta)[3]
  }, integer(1)))
  if (!is.finite(n_samp) || n_samp <= 1L) {
    return(NULL)
  }

  FF_use <- get("FF", inherits = TRUE)
  mean_mat <- matrix(NA_real_, nrow = length(q_tags), ncol = length(hist_idx))
  q025_mat <- matrix(NA_real_, nrow = length(q_tags), ncol = length(hist_idx))
  q500_mat <- matrix(NA_real_, nrow = length(q_tags), ncol = length(hist_idx))
  q975_mat <- matrix(NA_real_, nrow = length(q_tags), ncol = length(hist_idx))

  for (i in seq_along(q_tags)) {
    q_tag <- q_tags[[i]]
    p0 <- q_probs[[i]]
    theta_obj <- get(sprintf("samp.theta_%s_exAL_synth_DISC", q_tag), inherits = TRUE)
    th <- theta_obj$samp_theta[, , seq_len(n_samp), drop = FALSE]
    sts_arr <- get(sprintf("samp.sts_%s_exAL_synth_DISC", q_tag), inherits = TRUE)[1L, hist_idx, seq_len(n_samp), drop = FALSE]
    stj <- matrix(sts_arr, nrow = length(hist_idx), ncol = n_samp)
    gamj <- as.numeric(get(sprintf("samp.gamma_%s_exAL_synth_DISC", q_tag), inherits = TRUE)[1L, seq_len(n_samp)])
    sigj <- as.numeric(get(sprintf("samp.sigma_%s_exAL_synth_DISC", q_tag), inherits = TRUE)[1L, seq_len(n_samp)])

    xb <- matrix(NA_real_, nrow = length(hist_idx), ncol = n_samp)
    for (k in seq_along(hist_idx)) {
      t_idx <- hist_idx[[k]]
      th_t <- matrix(th[, t_idx, ], nrow = dim(th)[1], ncol = n_samp)
      p_use <- min(nrow(FF_use), nrow(th_t))
      xb[k, ] <- as.vector(t(FF_use[seq_len(p_use), 1L, t_idx, drop = FALSE][, 1L, 1L]) %*% th_t[seq_len(p_use), , drop = FALSE])
    }

    mu <- xb + sweep(stj, 2L, sigj * abs(gamj) * C_fn(p0, gamj), `*`)
    mu_log1p <- post_transform_internal_to_log1p_mat(
      t(mu),
      from_scale = post_resolve_analysis_scale_post_internal(),
      context = sprintf("%s.loc_hist.%s", meta$model_id, q_labels[[i]])
    )
    mean_mat[i, ] <- colMeans(mu_log1p, na.rm = TRUE)
    q_loc <- smoke_matrix_quantiles(mu_log1p, probs = c(0.025, 0.5, 0.975))
    q025_mat[i, ] <- q_loc[1L, ]
    q500_mat[i, ] <- q_loc[2L, ]
    q975_mat[i, ] <- q_loc[3L, ]
  }

  summary <- list(
    model_id = meta$model_id,
    transfer_mode = meta$transfer_mode,
    dates = as.character(hist_dates_all[hist_idx]),
    q_labels = q_labels,
    q_probs = as.numeric(q_probs),
    mean_mat = mean_mat,
    q025_mat = q025_mat,
    q500_mat = q500_mat,
    q975_mat = q975_mat
  )
  saveRDS(summary, cache_path)
  summary
}

smoke_build_multivar_gamma_sigma_quantiles <- function() {
  spec <- smoke_multivar_quantile_spec()
  q_tags <- spec$tags
  q_labels <- paste0(as.integer(spec$labels), "th")
  sources <- c("USGS", "GLOFAS", "NWS")
  rows <- list()

  for (i in seq_along(q_tags)) {
    gamma_name <- sprintf("samp.gamma_%s_exAL_synth_DISC", q_tags[[i]])
    sigma_name <- sprintf("samp.sigma_%s_exAL_synth_DISC", q_tags[[i]])
    if (!exists(gamma_name, inherits = TRUE) || !exists(sigma_name, inherits = TRUE)) {
      return(NULL)
    }
    gamma_mat <- as.matrix(get(gamma_name, inherits = TRUE))
    sigma_mat <- as.matrix(get(sigma_name, inherits = TRUE))
    if (!is.numeric(gamma_mat) || !is.numeric(sigma_mat) ||
        nrow(gamma_mat) < 3L || nrow(sigma_mat) < 3L ||
        ncol(gamma_mat) < 2L || ncol(sigma_mat) < 2L) {
      return(NULL)
    }

    for (src_idx in seq_along(sources)) {
      gamma_draws <- as.numeric(gamma_mat[src_idx, ])
      sigma_draws <- as.numeric(sigma_mat[src_idx, ])
      rows[[length(rows) + 1L]] <- data.frame(
        variable = "Gamma",
        source = sources[[src_idx]],
        quantile = q_labels[[i]],
        quantile_025 = stats::quantile(gamma_draws, 0.025, na.rm = TRUE),
        median = stats::quantile(gamma_draws, 0.5, na.rm = TRUE),
        quantile_975 = stats::quantile(gamma_draws, 0.975, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      rows[[length(rows) + 1L]] <- data.frame(
        variable = "Sigma",
        source = sources[[src_idx]],
        quantile = q_labels[[i]],
        quantile_025 = stats::quantile(sigma_draws, 0.025, na.rm = TRUE),
        median = stats::quantile(sigma_draws, 0.5, na.rm = TRUE),
        quantile_975 = stats::quantile(sigma_draws, 0.975, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

smoke_build_multivar_covariate_effects_summary <- function() {
  tt <- suppressWarnings(as.integer(get0("TT", ifnotfound = NA_integer_, inherits = TRUE)))
  if (!is.finite(tt) || tt <= 0L) {
    return(NULL)
  }

  theta_qs <- c("5", "50", "95")
  theta_labels <- c("5th", "50th", "95th")
  theta_names <- sprintf("samp.theta_%s_exAL_synth_DISC", theta_qs)
  if (any(!vapply(theta_names, exists, logical(1), inherits = TRUE))) {
    return(NULL)
  }

  scale_vec <- rep(1, 9L)
  data_cbind_path <- file.path(OUT_DIR, "data_cbind_tY_X.csv")
  if (file.exists(data_cbind_path)) {
    design_df <- tryCatch(utils::read.csv(data_cbind_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(design_df)) {
      scale_cols <- c("PPT", "SOIL", "PCA", "PPT_sq", "SOIL_sq", "PPT_x_SOIL", "PPT_lag1", "PPT_lag2", "PPT_lag3")
      if (all(scale_cols %in% names(design_df))) {
        scale_vals <- vapply(scale_cols, function(col) stats::sd(design_df[[col]], na.rm = TRUE), numeric(1))
        scale_vals[!is.finite(scale_vals) | scale_vals <= 0] <- 1
        scale_vec <- as.numeric(scale_vals)
      }
    }
  }

  component_idx <- 23:31
  rows <- list()
  for (j in seq_along(component_idx)) {
    comp <- component_idx[[j]]
    scale_fac <- scale_vec[[j]]
    for (i in seq_along(theta_names)) {
      theta_obj <- get(theta_names[[i]], inherits = TRUE)
      if (is.null(theta_obj$samp_theta) || length(dim(theta_obj$samp_theta)) != 3L ||
          dim(theta_obj$samp_theta)[1] < comp || dim(theta_obj$samp_theta)[2] < tt) {
        return(NULL)
      }
      draws <- scale_fac * as.numeric(theta_obj$samp_theta[comp, tt, ])
      rows[[length(rows) + 1L]] <- data.frame(
        Component = comp,
        Quantile = theta_labels[[i]],
        Lower = stats::quantile(draws, 0.025, na.rm = TRUE),
        Mean = mean(draws, na.rm = TRUE),
        Upper = stats::quantile(draws, 0.975, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

smoke_trim_dates <- function(dates, n_expected) {
  dates <- suppressWarnings(as.Date(dates))
  n_expected <- suppressWarnings(as.integer(n_expected[[1L]]))
  if (!is.finite(n_expected) || n_expected <= 0L) {
    return(as.Date(character(0)))
  }
  if (length(dates) >= n_expected) {
    return(dates[seq_len(n_expected)])
  }
  if (length(dates) == 0L || all(is.na(dates))) {
    return(as.Date(rep(NA_character_, n_expected)))
  }
  last_date <- max(dates, na.rm = TRUE)
  extra <- seq.Date(last_date + 1L, by = "day", length.out = n_expected - length(dates))
  c(dates, extra)
}

smoke_forecast_dates <- function(horizon) {
  smoke_trim_dates(
    smoke_window_dates(FORECAST_START_DATE, PLOT_END_DATE),
    n_expected = horizon
  )
}

smoke_manifest_row <- function(model_id, plot_type, path, note = "") {
  data.frame(
    model_id = as.character(model_id),
    plot_type = as.character(plot_type),
    path = normalizePath(path, mustWork = FALSE),
    source_run = as.character(get0("RUN_ID", ifnotfound = "", inherits = TRUE)),
    note = as.character(note),
    stringsAsFactors = FALSE
  )
}

smoke_register_figure_artifact <- function(model_id, plot_type, path, note = "") {
  smoke_figure_export_manifest <<- rbind(
    smoke_figure_export_manifest,
    smoke_manifest_row(model_id = model_id, plot_type = plot_type, path = path, note = note)
  )
  invisible(path)
}

smoke_write_figure_manifest <- function() {
  if (is.null(smoke_figure_export_manifest) || nrow(smoke_figure_export_manifest) == 0L) {
    return(invisible(NULL))
  }
  manifest_path <- file.path(OUT_DIR, "figure_manifest.csv")
  existing <- if (file.exists(manifest_path)) {
    utils::read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    NULL
  }
  merged <- rbind(existing, smoke_figure_export_manifest)
  merged[] <- lapply(merged, function(col) if (is.factor(col)) as.character(col) else col)
  key <- do.call(paste, c(lapply(merged, function(col) ifelse(is.na(col), "<NA>", as.character(col))), sep = "\r"))
  merged <- merged[!duplicated(key), , drop = FALSE]
  ord <- order(merged$model_id, merged$plot_type, merged$path, method = "radix", na.last = TRUE)
  merged <- merged[ord, , drop = FALSE]
  rownames(merged) <- NULL
  smoke_write_csv(merged, manifest_path)
  invisible(manifest_path)
}

smoke_quantile_band_from_moments <- function(
  mean_vec,
  var_vec,
  probs = c(0.05, 0.50, 0.95),
  transform = c("identity", "internal_to_log1p", "loglog1p_to_log1p"),
  context = "smoke.quantile_band"
) {
  transform <- match.arg(transform)
  mean_vec <- as.numeric(mean_vec)
  var_vec <- pmax(as.numeric(var_vec), 1e-10)
  sd_vec <- sqrt(var_vec)
  z <- stats::qnorm(probs)
  latent <- matrix(mean_vec, nrow = length(probs), ncol = length(mean_vec), byrow = TRUE) +
    outer(z, sd_vec)
  if (!identical(transform, "identity")) {
    return(post_transform_internal_to_log1p_mat(
      latent,
      from_scale = post_resolve_analysis_scale_post_internal(),
      context = context
    ))
  }
  latent
}

smoke_emit_synthesis_bundle <- function(
  model_id,
  title_text,
  hist_dates,
  hist_obs,
  hist_samples,
  hist_q,
  fc_dates,
  fc_obs,
  fc_samples,
  fc_q,
  probs = c(0.05, 0.20, 0.35, 0.50, 0.65, 0.80, 0.95),
  sample_cap = 128L,
  note = "plot_scale=log1p_cms; deterministic_sample_cap=128"
) {
  plot_path <- file.path(OUT_DIR, sprintf("%s_cutoff_window_posterior_samples.png", model_id))
  quant_path <- file.path(OUT_DIR, sprintf("%s_cutoff_window_quantiles.csv", model_id))
  sample_path <- file.path(OUT_DIR, sprintf("%s_cutoff_window_sample_subset.csv", model_id))

  smoke_plot_synthesis_window(
    model_id = model_id,
    title_text = title_text,
    out_file = plot_path,
    hist_dates = hist_dates,
    hist_obs = hist_obs,
    hist_samples = hist_samples,
    hist_q = hist_q,
    fc_dates = fc_dates,
    fc_obs = fc_obs,
    fc_samples = fc_samples,
    fc_q = fc_q,
    probs = probs
  )

  quant_df <- rbind(
    smoke_quantile_df(model_id, hist_dates, hist_obs, hist_q, probs, segment = "history"),
    smoke_quantile_df(model_id, fc_dates, fc_obs, fc_q, probs, segment = "forecast")
  )
  sample_df <- rbind(
    smoke_sample_subset_df(model_id, hist_samples, hist_dates, segment = "history", cap = sample_cap),
    smoke_sample_subset_df(model_id, fc_samples, fc_dates, segment = "forecast", cap = sample_cap)
  )
  smoke_write_csv(quant_df, quant_path)
  smoke_write_csv(sample_df, sample_path)

  smoke_register_figure_artifact(model_id, "cutoff_window_posterior_samples", plot_path, note = note)
  smoke_register_figure_artifact(model_id, "cutoff_window_quantiles", quant_path, note = "segment_quantiles_on_log1p_cms")
  smoke_register_figure_artifact(model_id, "cutoff_window_sample_subset", sample_path, note = sprintf("deterministic_sample_cap=%d", as.integer(sample_cap)))
}

smoke_emit_ndlm_bundle <- function(
  model_id,
  title_text,
  hist_dates,
  hist_obs,
  hist_q,
  fc_dates,
  fc_obs,
  fc_q,
  note = "plot_scale=log1p_cms; historical_band_from_exps_vars"
) {
  plot_path <- file.path(OUT_DIR, sprintf("%s_cutoff_window_predictive_bands.png", model_id))
  quant_path <- file.path(OUT_DIR, sprintf("%s_cutoff_window_quantiles.csv", model_id))

  smoke_plot_ndlm_window(
    model_id = model_id,
    title_text = title_text,
    out_file = plot_path,
    hist_dates = hist_dates,
    hist_obs = hist_obs,
    hist_q = hist_q,
    fc_dates = fc_dates,
    fc_obs = fc_obs,
    fc_q = fc_q
  )

  quant_df <- rbind(
    smoke_quantile_df(model_id, hist_dates, hist_obs, hist_q, probs = c(0.05, 0.50, 0.95), segment = "history"),
    smoke_quantile_df(model_id, fc_dates, fc_obs, fc_q, probs = c(0.05, 0.50, 0.95), segment = "forecast")
  )
  smoke_write_csv(quant_df, quant_path)

  smoke_register_figure_artifact(model_id, "cutoff_window_predictive_bands", plot_path, note = note)
  smoke_register_figure_artifact(model_id, "cutoff_window_quantiles", quant_path, note = "segment_quantiles_on_log1p_cms")
}

smoke_load_ndlm_univar_artifact <- function() {
  ndlm_univar_path <- if (exists("NDLM_UNIVAR_VAR_50", inherits = TRUE)) {
    as.character(get("NDLM_UNIVAR_VAR_50", inherits = TRUE))
  } else {
    ""
  }
  ndlm_univar_enabled <- isTRUE(exists("MODEL_RUN_NDLM_UNIVAR", inherits = TRUE) &&
    get("MODEL_RUN_NDLM_UNIVAR", inherits = TRUE))
  if (!(ndlm_univar_enabled || nzchar(ndlm_univar_path)) ||
      !nzchar(ndlm_univar_path) || !file.exists(ndlm_univar_path)) {
    return(NULL)
  }
  ndlm_univar_env <- new.env(parent = emptyenv())
  load(ndlm_univar_path, envir = ndlm_univar_env)
  obj_name <- "new.theta.out_50_NDLM_univar_synth_DISC"
  if (!exists(obj_name, envir = ndlm_univar_env, inherits = FALSE)) {
    obj_candidates <- grep("^new\\.theta\\.out_.*NDLM_univar.*$", ls(ndlm_univar_env), value = TRUE)
    obj_name <- if (length(obj_candidates) > 0L) obj_candidates[[1L]] else ""
  }
  sigma_name <- "samp.sigma_50_NDLM_univar_synth_DISC"
  if (!exists(sigma_name, envir = ndlm_univar_env, inherits = FALSE)) {
    sigma_candidates <- grep("^samp\\.sigma_.*NDLM_univar.*$", ls(ndlm_univar_env), value = TRUE)
    sigma_name <- if (length(sigma_candidates) > 0L) sigma_candidates[[1L]] else ""
  }
  obj <- if (nzchar(obj_name) && exists(obj_name, envir = ndlm_univar_env, inherits = FALSE)) {
    get(obj_name, envir = ndlm_univar_env, inherits = FALSE)
  } else {
    NULL
  }
  sigma_draws <- if (nzchar(sigma_name) && exists(sigma_name, envir = ndlm_univar_env, inherits = FALSE)) {
    get(sigma_name, envir = ndlm_univar_env, inherits = FALSE)
  } else {
    NULL
  }
  if (is.null(obj) || is.null(sigma_draws)) {
    return(NULL)
  }
  list(obj = obj, sigma_draws = sigma_draws, path = ndlm_univar_path)
}

profile_section("figures_smoke_fast.univar_load_inputs", {
  ensure_univar_bundles_loaded()
})

profile_section("figures_smoke_fast.elbo_traces", {
  out_file <- file.path(OUT_DIR, "All_ELBOS_DISC.png")
  multiv_q_suffixes <- c("5", "20", "35", "50", "65", "80", "95")
  multiv_traces <- stats::setNames(
    lapply(multiv_q_suffixes, function(q) fetch_numeric_object(sprintf("seq.elbo_%s_exAL_synth_DISC", q))),
    sprintf("exAL_multiv_%s", multiv_q_suffixes)
  )
  traces <- c(
    list(NDLM = fetch_numeric_object("seq.elbo_50_NDLM_synth_DISC")),
    multiv_traces,
    list(exAL_univar_50 = fetch_numeric_object("seq.elbo_50_exAL_synth_DISC_uni"))
  )
  show_names <- names(traces)[vapply(traces, function(x) length(x) > 0L, logical(1))]
  if (length(show_names) == 0L) {
    show_names <- names(traces)
  }
  n <- length(show_names)
  ncol <- min(2L, n)
  nrow <- ceiling(n / ncol)

  png(out_file, width = 2400, height = max(1200, 600 * nrow), res = 300)
  on.exit(dev.off(), add = TRUE)
  par(mfrow = c(nrow, ncol), mar = c(3, 3, 2, 1))

  for (nm in show_names) {
    vals <- traces[[nm]]
    if (length(vals) > 0L) {
      vals[1] <- NA_real_
    }
    plot_trace(vals, nm, "ELBO")
  }
  mtext("Smoke Figure Set", side = 3, outer = TRUE, line = -2, cex = 0.9)
})

profile_section("figures_smoke_fast.observed_series", {
  out_file <- file.path(OUT_DIR, "SMOKE_OBSERVED_SERIES_DISC.png")
  png(out_file, width = 2400, height = 1200, res = 300)
  on.exit(dev.off(), add = TRUE)

  if (exists("Y", inherits = TRUE) && is.matrix(Y) && nrow(Y) >= 1L) {
    yy <- as.numeric(Y[1, ])
    idx <- which(is.finite(yy))
    if (length(idx) > 0L) {
      plot(idx, yy[idx], type = "l", col = "black", lwd = 1.5,
           xlab = "Time index", ylab = "log-flow", main = "Observed series (row 1)")
    } else {
      plot.new()
      title(main = "Observed series unavailable (no finite values)")
    }
  } else {
    plot.new()
    title(main = "Observed series unavailable (Y missing)")
  }
})

profile_section("figures_smoke_fast.univar_traces", {
  run_univar <- isTRUE(get0("MODEL_RUN_EXDQLM_UNIVAR", ifnotfound = FALSE, inherits = TRUE))
  has_univar_objs <- length(fetch_numeric_object("seq.elbo_50_exAL_synth_DISC_uni")) > 0L
  if (!(run_univar || has_univar_objs)) {
    return(invisible(NULL))
  }

  q_tags <- c("5", "20", "35", "50", "65", "80", "95")

  collect_metric <- function(metric_name) {
    out <- list()
    for (q in q_tags) {
      vals <- fetch_first_numeric(c(
        sprintf("seq.%s_%s_exAL_synth_DISC_uni", metric_name, q),
        sprintf("samp.%s_%s_exAL_synth_DISC_uni", metric_name, q)
      ))
      if (length(vals) > 0L) {
        out[[q]] <- vals
      }
    }
    out
  }

  draw_metric_grid <- function(metric_key, ylab_txt, file_name) {
    traces <- collect_metric(metric_key)
    if (length(traces) == 0L) {
      return(invisible(NULL))
    }
    out_file <- file.path(OUT_DIR, file_name)
    png(out_file, width = 2800, height = 1600, res = 300)
    on.exit(dev.off(), add = TRUE)
    n <- length(traces)
    ncol <- min(3L, n)
    nrow <- ceiling(n / ncol)
    par(mfrow = c(nrow, ncol), mar = c(3, 3, 2, 1))
    for (nm in names(traces)) {
      plot_trace(traces[[nm]], paste0(metric_key, " q=", nm), ylab_txt)
    }
    invisible(NULL)
  }

  draw_metric_grid("elbo", "ELBO", "univar_elbo_traces.png")
  draw_metric_grid("sigma", "sigma", "univar_sigma_traces.png")
  draw_metric_grid("gamma", "gamma", "univar_gamma_traces.png")
})

profile_section("figures_smoke_fast.univar_fit_mu_vs_obs", {
  run_univar <- isTRUE(get0("MODEL_RUN_EXDQLM_UNIVAR", ifnotfound = FALSE, inherits = TRUE))
  if (!run_univar || !exists("Y", inherits = TRUE) || !is.matrix(Y) || nrow(Y) < 1L) {
    return(invisible(NULL))
  }

  q_tags <- c("5", "20", "35", "50", "65", "80", "95")
  q_cols <- c(
    "5" = "#b2182b",
    "20" = "#d6604d",
    "35" = "#f4a582",
    "50" = "#1b7837",
    "65" = "#92c5de",
    "80" = "#4393c3",
    "95" = "#2166ac"
  )

  exps_by_q <- list()
  for (q in q_tags) {
    obj <- safe_obj_list(sprintf("new.theta.out_%s_exAL_synth_DISC_uni", q))
    if (is.null(obj) || is.null(obj$exps) || !is.matrix(obj$exps) || nrow(obj$exps) < 1L) {
      next
    }
    exps_by_q[[q]] <- as.numeric(obj$exps[1, ])
  }
  if (length(exps_by_q) == 0L) {
    return(invisible(NULL))
  }

  obs <- as.numeric(Y[1, ])
  fit_len <- min(length(obs), max(vapply(exps_by_q, length, integer(1))))
  if (!is.finite(fit_len) || fit_len <= 0L) {
    return(invisible(NULL))
  }
  idx <- seq_len(fit_len)

  out_file <- file.path(OUT_DIR, "univar_fit_mu_vs_observed_log1p.png")
  png(out_file, width = 2800, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  y_min <- min(obs[idx], unlist(lapply(exps_by_q, function(v) v[idx])), na.rm = TRUE)
  y_max <- max(obs[idx], unlist(lapply(exps_by_q, function(v) v[idx])), na.rm = TRUE)
  plot(idx, obs[idx], type = "p", pch = 16, cex = 0.35, col = "gray20",
       xlab = "Time index", ylab = "log(1 + flow)",
       main = "Univariate exDQLM expected location vs observed (in-sample)",
       ylim = c(y_min, y_max))
  for (q in names(exps_by_q)) {
    lines(idx, exps_by_q[[q]][idx], col = q_cols[[q]], lwd = if (q == "50") 2 else 1.3)
  }
  legend("topright",
         legend = c("Observed", paste0("mu_t q=", names(exps_by_q))),
         col = c("gray20", unname(q_cols[names(exps_by_q)])),
         lwd = c(NA, rep(2, length(exps_by_q))),
         pch = c(16, rep(NA, length(exps_by_q))),
         pt.cex = 0.7,
         bty = "n")

  recent_n <- min(900L, fit_len)
  idx_recent <- seq.int(fit_len - recent_n + 1L, fit_len)
  out_file_recent <- file.path(OUT_DIR, "univar_fit_mu_vs_observed_recent_log1p.png")
  png(out_file_recent, width = 2800, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  y_min_r <- min(obs[idx_recent], unlist(lapply(exps_by_q, function(v) v[idx_recent])), na.rm = TRUE)
  y_max_r <- max(obs[idx_recent], unlist(lapply(exps_by_q, function(v) v[idx_recent])), na.rm = TRUE)
  plot(idx_recent, obs[idx_recent], type = "p", pch = 16, cex = 0.55, col = "gray20",
       xlab = "Time index", ylab = "log(1 + flow)",
       main = sprintf("Univariate exDQLM expected location vs observed (recent %d points)", recent_n),
       ylim = c(y_min_r, y_max_r))
  for (q in names(exps_by_q)) {
    lines(idx_recent, exps_by_q[[q]][idx_recent], col = q_cols[[q]], lwd = if (q == "50") 2.2 else 1.5)
  }
  legend("topright",
         legend = c("Observed", paste0("mu_t q=", names(exps_by_q))),
         col = c("gray20", unname(q_cols[names(exps_by_q)])),
         lwd = c(NA, rep(2, length(exps_by_q))),
         pch = c(16, rep(NA, length(exps_by_q))),
         pt.cex = 0.7,
         bty = "n")
})

profile_section("figures_smoke_fast.univar_forecast_window", {
  run_univar <- isTRUE(get0("MODEL_RUN_EXDQLM_UNIVAR", ifnotfound = FALSE, inherits = TRUE))
  if (!run_univar) {
    return(invisible(NULL))
  }

  loc_q05 <- NULL
  loc_q50 <- NULL
  loc_q95 <- NULL
  pred_q50 <- NULL
  horizon <- NA_integer_
  label_q05 <- "05"
  label_q50 <- "50"
  label_q95 <- "95"
  active_q_probs <- numeric(0)

  if (exists("xb_forecast", inherits = TRUE) && !is.null(dim(xb_forecast)) && length(dim(xb_forecast)) == 3L) {
    horizon <- dim(xb_forecast)[3]
    active_q_probs <- fetch_univar_active_q_probs_smoke(expected_n = dim(xb_forecast)[1])
    loc_low_sel <- select_univar_quantile_samples(xb_forecast, 0.05, active_q_probs)
    loc_mid_sel <- select_univar_quantile_samples(xb_forecast, 0.50, active_q_probs)
    loc_high_sel <- select_univar_quantile_samples(xb_forecast, 0.95, active_q_probs)

    if (!is.null(loc_low_sel) && !is.null(loc_mid_sel) && !is.null(loc_high_sel)) {
      loc_q05 <- col_quantiles(loc_low_sel$samples, probs = c(0.025, 0.5, 0.975))
      loc_q50 <- col_quantiles(loc_mid_sel$samples, probs = c(0.025, 0.5, 0.975))
      loc_q95 <- col_quantiles(loc_high_sel$samples, probs = c(0.025, 0.5, 0.975))
      label_q05 <- loc_low_sel$label
      label_q50 <- loc_mid_sel$label
      label_q95 <- loc_high_sel$label
    }

    if (exists("y_forecast", inherits = TRUE) && is.array(y_forecast) && length(dim(y_forecast)) == 3L) {
      pred_sel <- select_univar_quantile_samples(y_forecast, 0.50, active_q_probs)
      if (!is.null(pred_sel)) {
        pred_q50 <- col_quantiles(pred_sel$samples, probs = c(0.025, 0.5, 0.975))
      }
    }
  } else {
    fallback_fc <- build_univar_location_forecast_summary()
    if (!is.null(fallback_fc)) {
      horizon <- fallback_fc$horizon
      loc_q05 <- fallback_fc$loc_q05
      loc_q50 <- fallback_fc$loc_q50
      loc_q95 <- fallback_fc$loc_q95
      label_q05 <- quantile_prob_label(fallback_fc$low_prob)
      label_q50 <- quantile_prob_label(fallback_fc$mid_prob)
      label_q95 <- quantile_prob_label(fallback_fc$high_prob)
      if (!is.null(fallback_fc$q50_samples)) {
        pred_q50 <- col_quantiles(fallback_fc$q50_samples, probs = c(0.025, 0.5, 0.975))
      }
    }
  }

  if (!is.finite(horizon) || horizon <= 0L || is.null(loc_q50)) {
    return(invisible(NULL))
  }

  x_idx <- seq_len(horizon)
  truth <- resolve_future_truth(horizon)

  out_file <- file.path(OUT_DIR, "univar_forecast_window_mu_vs_future_usgs.png")
  png(out_file, width = 2800, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  y_min <- min(c(loc_q05, loc_q50, loc_q95, truth), na.rm = TRUE)
  y_max <- max(c(loc_q05, loc_q50, loc_q95, truth), na.rm = TRUE)
  plot(x_idx, loc_q50[2, ], type = "l", lwd = 2.2, col = "#1b7837",
       xlab = "Forecast day", ylab = "log(1 + flow)",
       main = "Univariate exDQLM forecast-window location vs future USGS",
       ylim = c(y_min, y_max))
  lines(x_idx, loc_q50[1, ], lty = 2, lwd = 1.2, col = "#1b7837")
  lines(x_idx, loc_q50[3, ], lty = 2, lwd = 1.2, col = "#1b7837")
  lines(x_idx, loc_q05[2, ], lwd = 1.5, col = "#b2182b")
  lines(x_idx, loc_q95[2, ], lwd = 1.5, col = "#2166ac")
  points(x_idx, truth, pch = 16, cex = 0.8, col = "black")
  lines(x_idx, truth, lwd = 1.1, col = "black")
  legend("topleft",
         legend = c(
           sprintf("mu_t q=%s (median)", label_q50),
           sprintf("mu_t q=%s 95%% interval", label_q50),
           sprintf("mu_t q=%s", label_q05),
           sprintf("mu_t q=%s", label_q95),
           "Future USGS (withheld)"
         ),
         col = c("#1b7837", "#1b7837", "#b2182b", "#2166ac", "black"),
         lty = c(1, 2, 1, 1, 1),
         lwd = c(2.2, 1.2, 1.5, 1.5, 1.1),
         pch = c(NA, NA, NA, NA, 16),
         bty = "n")

  if (!is.null(pred_q50) && ncol(pred_q50) == horizon) {
    out_file2 <- file.path(OUT_DIR, "univar_forecast_window_predictive_q50_vs_future_usgs.png")
    png(out_file2, width = 2800, height = 1400, res = 300)
    on.exit(dev.off(), add = TRUE)
    y_min2 <- min(c(pred_q50, truth), na.rm = TRUE)
    y_max2 <- max(c(pred_q50, truth), na.rm = TRUE)
    plot(x_idx, pred_q50[2, ], type = "l", lwd = 2.1, col = "#1b7837",
         xlab = "Forecast day", ylab = "log(1 + flow)",
         main = "Univariate exDQLM predictive q=50 vs future USGS",
         ylim = c(y_min2, y_max2))
    lines(x_idx, pred_q50[1, ], lty = 2, lwd = 1.2, col = "#1b7837")
    lines(x_idx, pred_q50[3, ], lty = 2, lwd = 1.2, col = "#1b7837")
    points(x_idx, truth, pch = 16, cex = 0.8, col = "black")
    lines(x_idx, truth, lwd = 1.1, col = "black")
    legend("topleft",
           legend = c(
             sprintf("Predictive q=%s median", label_q50),
             sprintf("Predictive q=%s 95%% interval", label_q50),
             "Future USGS (withheld)"
           ),
           col = c("#1b7837", "#1b7837", "black"),
           lty = c(1, 2, 1),
           lwd = c(2.1, 1.2, 1.1),
           pch = c(NA, NA, 16),
           bty = "n")
  }

  if (exists("ensembles", inherits = TRUE) && is.list(ensembles) && length(ensembles) >= 2L) {
    glofas <- as.matrix(ensembles[[1]])
    nws <- as.matrix(ensembles[[2]])
    glofas_mean <- pad_to_horizon(rowMeans(glofas, na.rm = TRUE), horizon)
    nws_mean <- pad_to_horizon(rowMeans(nws, na.rm = TRUE), horizon)

    out_file3 <- file.path(OUT_DIR, "univar_forecast_window_univar_vs_ensembles.png")
    png(out_file3, width = 2800, height = 1400, res = 300)
    on.exit(dev.off(), add = TRUE)
    y_min3 <- min(c(loc_q50, glofas_mean, nws_mean, truth), na.rm = TRUE)
    y_max3 <- max(c(loc_q50, glofas_mean, nws_mean, truth), na.rm = TRUE)
    plot(x_idx, loc_q50[2, ], type = "l", lwd = 2.4, col = "#1b7837",
         xlab = "Forecast day", ylab = "log(1 + flow)",
         main = "Forecast window: univariate exDQLM vs ensemble means",
         ylim = c(y_min3, y_max3))
    lines(x_idx, loc_q50[1, ], lty = 2, lwd = 1.1, col = "#1b7837")
    lines(x_idx, loc_q50[3, ], lty = 2, lwd = 1.1, col = "#1b7837")
    lines(x_idx, glofas_mean, col = "#2166ac", lwd = 1.7)
    lines(x_idx, nws_mean, col = "#762a83", lwd = 1.7)
    points(x_idx, truth, pch = 16, cex = 0.8, col = "black")
    lines(x_idx, truth, lwd = 1.1, col = "black")
    legend("topleft",
           legend = c(
             sprintf("Univar mu_t q=%s median", label_q50),
             sprintf("Univar mu_t q=%s 95%% interval", label_q50),
             "GLOFAS ensemble mean",
             "NWS ensemble mean",
             "Future USGS (withheld)"
           ),
           col = c("#1b7837", "#1b7837", "#2166ac", "#762a83", "black"),
           lty = c(1, 2, 1, 1, 1),
           lwd = c(2.4, 1.1, 1.7, 1.7, 1.1),
           pch = c(NA, NA, NA, NA, 16),
           bty = "n")

    out_file4 <- file.path(OUT_DIR, "univar_forecast_window_ensemble_members.png")
    png(out_file4, width = 2800, height = 1400, res = 300)
    on.exit(dev.off(), add = TRUE)
    y_min4 <- min(c(glofas, nws, loc_q50[2, ], truth), na.rm = TRUE)
    y_max4 <- max(c(glofas, nws, loc_q50[2, ], truth), na.rm = TRUE)
    plot(x_idx, loc_q50[2, ], type = "l", lwd = 2.6, col = "#1b7837",
         xlab = "Forecast day", ylab = "log(1 + flow)",
         main = "Forecast window: ensemble members + univariate median + future USGS",
         ylim = c(y_min4, y_max4))
    if (ncol(glofas) > 0L) {
      matlines(seq_len(nrow(glofas)), glofas, lty = 1, lwd = 0.5, col = adjustcolor("#2166ac", alpha.f = 0.28))
    }
    if (ncol(nws) > 0L) {
      matlines(seq_len(nrow(nws)), nws, lty = 1, lwd = 0.5, col = adjustcolor("#762a83", alpha.f = 0.28))
    }
    lines(x_idx, loc_q50[2, ], lwd = 2.6, col = "#1b7837")
    points(x_idx, truth, pch = 16, cex = 0.85, col = "black")
    lines(x_idx, truth, lwd = 1.1, col = "black")
    legend("topleft",
           legend = c(
             sprintf("Univar mu_t q=%s median", label_q50),
             "GLOFAS members",
             "NWS members",
             "Future USGS (withheld)"
           ),
           col = c("#1b7837", "#2166ac", "#762a83", "black"),
           lty = c(1, 1, 1, 1),
           lwd = c(2.6, 1.0, 1.0, 1.1),
           pch = c(NA, NA, NA, 16),
           bty = "n")
  }
})

crps_transfer_mode <- tolower(trimws(Sys.getenv("UNIFIED_MULTIVAR_FORECAST_TRANSFER_MODE", "drop")))
if (!crps_transfer_mode %in% c("drop", "keep")) {
  crps_transfer_mode <- NA_character_
}
crps_univar_likelihood_mode <- tolower(trimws(Sys.getenv("UNIFIED_EXDQLM_UNIVAR_LIKELIHOOD_MODE", "exal")))
if (!crps_univar_likelihood_mode %in% c("exal", "al")) {
  crps_univar_likelihood_mode <- "exal"
}
crps_multivar_likelihood_mode <- tolower(trimws(Sys.getenv("UNIFIED_EXDQLM_MULTIVAR_LIKELIHOOD_MODE", "exal")))
if (!crps_multivar_likelihood_mode %in% c("exal", "al")) {
  crps_multivar_likelihood_mode <- "exal"
}
crps_ndlm_transfer_mode <- tolower(trimws(Sys.getenv("UNIFIED_NDLM_FORECAST_TRANSFER_MODE", "keep")))
if (!crps_ndlm_transfer_mode %in% c("drop", "keep")) {
  crps_ndlm_transfer_mode <- NA_character_
}
crps_ndlm_univar_transfer_mode <- tolower(trimws(Sys.getenv(
  "UNIFIED_NDLM_UNIVAR_FORECAST_TRANSFER_MODE",
  if (is.na(crps_ndlm_transfer_mode)) "keep" else crps_ndlm_transfer_mode
)))
if (!crps_ndlm_univar_transfer_mode %in% c("drop", "keep")) {
  crps_ndlm_univar_transfer_mode <- NA_character_
}
crps_output_suffix <- Sys.getenv("UNIFIED_POST_OUTPUT_SUFFIX", "")
crps_exports_enabled <- isTRUE(as.logical(Sys.getenv("UNIFIED_POST_EXPORT_CRPS", "TRUE")))
crps_input_health_enabled <- isTRUE(as.logical(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_ENABLED", "TRUE")))
crps_input_health_fail_fast <- isTRUE(as.logical(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_FAIL_FAST", "FALSE")))
crps_input_health_min_finite_share <- suppressWarnings(as.numeric(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_MIN_FINITE_SHARE", "1")))
if (!is.finite(crps_input_health_min_finite_share) ||
    crps_input_health_min_finite_share < 0 || crps_input_health_min_finite_share > 1) {
  crps_input_health_min_finite_share <- 1
}
crps_input_health_max_abs <- suppressWarnings(as.numeric(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_MAX_ABS", "NA")))
if (!is.finite(crps_input_health_max_abs) || crps_input_health_max_abs <= 0) {
  crps_input_health_max_abs <- NA_real_
}

if (crps_exports_enabled && posterior_table_exports_enabled) {
  profile_section("figures_smoke_fast.export_crps_tables", {
    crps_cutoff_date <- as.Date(CUTOFF_DATE)
    crps_forecast_start <- as.Date(FORECAST_START_DATE)
    usgs_date_col <- if ("Date" %in% names(San_Lorenzo_Daily_USGS_R)) {
      as.Date(San_Lorenzo_Daily_USGS_R$Date)
    } else {
      as.Date(San_Lorenzo_Daily_USGS_R$time)
    }
    usgs_truth_all <- as.numeric(San_Lorenzo_Daily_USGS_R$data0)
    crps_truth_availability_rows <- list()

    truth_from_start <- function(horizon, context) {
      resolved <- post_truth_from_start_or_na(
        usgs_dates = usgs_date_col,
        usgs_truth = usgs_truth_all,
        forecast_start_date = crps_forecast_start,
        horizon = horizon,
        context = context
      )
      crps_truth_availability_rows[[length(crps_truth_availability_rows) + 1L]] <<- resolved$availability
      resolved$truth
    }

    crps_per_time_rows <- list()
    crps_summary_rows <- list()
    crps_input_health_rows <- list()
    crps_input_health_per_time_rows <- list()
    crps_input_health_failures <- character(0)

    collect_crps_input_health <- function(
      model_id,
      model_family,
      model_variant,
      sample_mat,
      forecast_dates,
      transfer_mode,
      context
    ) {
      if (!isTRUE(crps_input_health_enabled)) return(invisible(NULL))
      health <- post_crps_input_health_tables(
        model_id = model_id,
        model_family = model_family,
        model_variant = model_variant,
        sample_mat = sample_mat,
        forecast_dates = forecast_dates,
        cutoff_date = crps_cutoff_date,
        forecast_start_date = crps_forecast_start,
        transfer_mode = transfer_mode,
        min_finite_share = crps_input_health_min_finite_share,
        max_abs = crps_input_health_max_abs,
        context = paste0(context, ".input_health")
      )
      crps_input_health_rows[[length(crps_input_health_rows) + 1L]] <<- health$summary
      crps_input_health_per_time_rows[[length(crps_input_health_per_time_rows) + 1L]] <<- health$per_time
      if (isTRUE(crps_input_health_fail_fast) && !isTRUE(health$pass)) {
        violation_msg <- if (length(health$violations) > 0L) {
          paste(health$violations, collapse = " | ")
        } else {
          "input health failed"
        }
        crps_input_health_failures <<- c(
          crps_input_health_failures,
          sprintf("%s (%s): %s", model_id, context, violation_msg)
        )
      }
      invisible(NULL)
    }

    if (length(ensembles) >= 1L) {
      glofas_mat <- as.matrix(ensembles[[1]])
      if (is.numeric(glofas_mat) && nrow(glofas_mat) > 0L && ncol(glofas_mat) >= 2L) {
        glofas_tbl <- post_crps_model_tables(
          model_id = "glofas_ensemble",
          model_family = "ensemble",
          model_variant = "glofas",
          sample_mat = t(glofas_mat),
          obs = truth_from_start(nrow(glofas_mat), "crps.glofas.truth"),
          forecast_dates = daily_dates_for_matrix_rows(glofas_mat, start_date = crps_forecast_start, context = "crps.glofas.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = NA_character_,
          score_scale = "log_cms_plus1",
          context = "crps.glofas"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- glofas_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- glofas_tbl$summary
        collect_crps_input_health(
          model_id = "glofas_ensemble",
          model_family = "ensemble",
          model_variant = "glofas",
          sample_mat = t(glofas_mat),
          forecast_dates = daily_dates_for_matrix_rows(
            glofas_mat,
            start_date = crps_forecast_start,
            context = "crps.glofas.health.dates"
          ),
          transfer_mode = NA_character_,
          context = "crps.glofas"
        )
      } else {
        warning("[CRPS_GLOFAS_SKIP] Unable to compute GloFAS CRPS (invalid ensemble matrix).", call. = FALSE)
      }
    } else {
      warning("[CRPS_GLOFAS_SKIP] Unable to compute GloFAS CRPS (ensembles[[1]] missing).", call. = FALSE)
    }

    if (length(ensembles) >= 2L) {
      nws_mat <- as.matrix(ensembles[[2]])
      if (is.numeric(nws_mat) && nrow(nws_mat) > 0L && ncol(nws_mat) >= 2L) {
        nws_tbl <- post_crps_model_tables(
          model_id = "nws_nwm_ensemble",
          model_family = "ensemble",
          model_variant = "nws_nwm",
          sample_mat = t(nws_mat),
          obs = truth_from_start(nrow(nws_mat), "crps.nws.truth"),
          forecast_dates = daily_dates_for_matrix_rows(nws_mat, start_date = crps_forecast_start, context = "crps.nws.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = NA_character_,
          score_scale = "log_cms_plus1",
          context = "crps.nws"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- nws_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- nws_tbl$summary
        collect_crps_input_health(
          model_id = "nws_nwm_ensemble",
          model_family = "ensemble",
          model_variant = "nws_nwm",
          sample_mat = t(nws_mat),
          forecast_dates = daily_dates_for_matrix_rows(
            nws_mat,
            start_date = crps_forecast_start,
            context = "crps.nws.health.dates"
          ),
          transfer_mode = NA_character_,
          context = "crps.nws"
        )
      } else {
        warning("[CRPS_NWS_SKIP] Unable to compute NWS/NWM CRPS (invalid ensemble matrix).", call. = FALSE)
      }
    } else {
      warning("[CRPS_NWS_SKIP] Unable to compute NWS/NWM CRPS (ensembles[[2]] missing).", call. = FALSE)
    }

    if (exists("synth_f2", inherits = TRUE)) {
      synth_uni_mat <- as.matrix(get("synth_f2", inherits = TRUE))
      if (is.numeric(synth_uni_mat) && nrow(synth_uni_mat) > 0L && ncol(synth_uni_mat) > 0L) {
        univar_meta <- post_crps_synth_model_meta(
          family = "univar",
          likelihood_mode = crps_univar_likelihood_mode,
          transfer_mode = NA_character_
        )
        univar_tbl <- post_crps_model_tables(
          model_id = univar_meta$model_id,
          model_family = "synthesis",
          model_variant = univar_meta$model_variant,
          sample_mat = synth_uni_mat,
          obs = truth_from_start(ncol(synth_uni_mat), "crps.univar.truth"),
          forecast_dates = daily_dates_for_matrix_cols(synth_uni_mat, start_date = crps_forecast_start, context = "crps.univar.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = NA_character_,
          score_scale = "log_cms_plus1",
          context = "crps.univar"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- univar_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- univar_tbl$summary
        collect_crps_input_health(
          model_id = univar_meta$model_id,
          model_family = "synthesis",
          model_variant = univar_meta$model_variant,
          sample_mat = synth_uni_mat,
          forecast_dates = daily_dates_for_matrix_cols(
            synth_uni_mat,
            start_date = crps_forecast_start,
            context = "crps.univar.health.dates"
          ),
          transfer_mode = NA_character_,
          context = "crps.univar"
        )
      } else {
        warning("[CRPS_UNIVAR_SKIP] Unable to compute univariate synthesis CRPS (invalid synth_f2 matrix).", call. = FALSE)
      }
    } else {
      warning("[CRPS_UNIVAR_SKIP] Unable to compute univariate synthesis CRPS (synth_f2 missing).", call. = FALSE)
    }

    synth_f_smoke <- smoke_build_multivar_synth_f()
    if (!is.null(synth_f_smoke)) {
      multivar_meta <- post_crps_synth_model_meta(
        family = "multivar",
        likelihood_mode = crps_multivar_likelihood_mode,
        transfer_mode = crps_transfer_mode
      )

      synth_multivar_mat <- as.matrix(synth_f_smoke)
      if (is.numeric(synth_multivar_mat) && nrow(synth_multivar_mat) > 0L && ncol(synth_multivar_mat) > 0L) {
        multivar_tbl <- post_crps_model_tables(
          model_id = multivar_meta$model_id,
          model_family = "synthesis",
          model_variant = multivar_meta$model_variant,
          sample_mat = synth_multivar_mat,
          obs = truth_from_start(ncol(synth_multivar_mat), "crps.multivar.truth"),
          forecast_dates = daily_dates_for_matrix_cols(synth_multivar_mat, start_date = crps_forecast_start, context = "crps.multivar.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = crps_transfer_mode,
          score_scale = "log_cms_plus1",
          context = "crps.multivar"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- multivar_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- multivar_tbl$summary
        collect_crps_input_health(
          model_id = multivar_meta$model_id,
          model_family = "synthesis",
          model_variant = multivar_meta$model_variant,
          sample_mat = synth_multivar_mat,
          forecast_dates = daily_dates_for_matrix_cols(
            synth_multivar_mat,
            start_date = crps_forecast_start,
            context = "crps.multivar.health.dates"
          ),
          transfer_mode = crps_transfer_mode,
          context = "crps.multivar"
        )
      } else {
        warning("[CRPS_MULTIVAR_SKIP] Unable to compute multivariate synthesis CRPS (invalid synth_f matrix).", call. = FALSE)
      }
    } else {
      warning("[CRPS_MULTIVAR_SKIP] Unable to compute multivariate synthesis CRPS (synth_f unavailable after smoke rebuild).", call. = FALSE)
    }

    ndlm_main_enabled <- isTRUE(exists("MODEL_RUN_NDLM_MAIN", inherits = TRUE) &&
      get("MODEL_RUN_NDLM_MAIN", inherits = TRUE))
    ndlm_raw <- if (ndlm_main_enabled) smoke_build_ndlm_main_raw_draws() else NULL
    if (ndlm_main_enabled && !is.null(ndlm_raw)) {
      ndlm_sigma_draws <- if (exists("samp.sigma_50_NDLM_synth_DISC", inherits = TRUE)) {
        get("samp.sigma_50_NDLM_synth_DISC", inherits = TRUE)
      } else {
        NULL
      }

      ndlm_pred <- tryCatch(
        post_ndlm_predictive_draws(
          ndlm_raw = ndlm_raw,
          sigma_draws = ndlm_sigma_draws,
          context = "crps.ndlm",
          seed = 777L
        ),
        error = function(e) e
      )

      if (!inherits(ndlm_pred, "error")) {
        ndlm_sample_mat_log1p <- ndlm_pred$predictive_log1p
        saveRDS(ndlm_pred$mean_loglog1p, file = post_cache_path("xbs_ndlm_mean_loglog1p.rds"))
        saveRDS(ndlm_pred$predictive_loglog1p, file = post_cache_path("y_reps_ndlm_loglog1p.rds"))
        saveRDS(ndlm_sample_mat_log1p, file = post_cache_path("xbs_ndlm_log1p.rds"))
        saveRDS(ndlm_sample_mat_log1p, file = post_cache_path("y_reps_ndlm_log1p.rds"))
        ndlm_meta <- post_crps_synth_model_meta(
          family = "ndlm",
          likelihood_mode = "exal",
          transfer_mode = crps_ndlm_transfer_mode
        )
        saveRDS(
          ndlm_sample_mat_log1p,
          file = post_cache_path(post_cache_file_name(
            "ndlm_predictive_log1p.rds",
            model_id = ndlm_meta$model_id,
            transfer_mode = crps_ndlm_transfer_mode
          ))
        )
        ndlm_tbl <- post_crps_model_tables(
          model_id = ndlm_meta$model_id,
          model_family = "synthesis",
          model_variant = ndlm_meta$model_variant,
          sample_mat = ndlm_sample_mat_log1p,
          obs = truth_from_start(ncol(ndlm_sample_mat_log1p), "crps.ndlm.truth"),
          forecast_dates = daily_dates_for_matrix_cols(ndlm_sample_mat_log1p, start_date = crps_forecast_start, context = "crps.ndlm.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = crps_ndlm_transfer_mode,
          score_scale = "log_cms_plus1",
          context = "crps.ndlm"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- ndlm_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- ndlm_tbl$summary
        collect_crps_input_health(
          model_id = ndlm_meta$model_id,
          model_family = "synthesis",
          model_variant = ndlm_meta$model_variant,
          sample_mat = ndlm_sample_mat_log1p,
          forecast_dates = daily_dates_for_matrix_cols(
            ndlm_sample_mat_log1p,
            start_date = crps_forecast_start,
            context = "crps.ndlm.health.dates"
          ),
          transfer_mode = crps_ndlm_transfer_mode,
          context = "crps.ndlm"
        )
      } else {
        warning(
          sprintf(
            "[CRPS_NDLM_SKIP] Unable to compute NDLM CRPS (%s).",
            conditionMessage(ndlm_pred)
          ),
          call. = FALSE
        )
      }
    } else if (ndlm_main_enabled) {
      warning("[CRPS_NDLM_SKIP] Unable to compute NDLM CRPS (xbs_ndlm unavailable after smoke rebuild).", call. = FALSE)
    }

    ndlm_univar_path <- if (exists("NDLM_UNIVAR_VAR_50", inherits = TRUE)) {
      as.character(get("NDLM_UNIVAR_VAR_50", inherits = TRUE))
    } else {
      ""
    }
    ndlm_univar_enabled <- isTRUE(exists("MODEL_RUN_NDLM_UNIVAR", inherits = TRUE) &&
      get("MODEL_RUN_NDLM_UNIVAR", inherits = TRUE))
    if ((ndlm_univar_enabled || nzchar(ndlm_univar_path)) &&
        length(ndlm_univar_path) > 0L && nzchar(ndlm_univar_path) && file.exists(ndlm_univar_path)) {
      ndlm_univar_env <- new.env(parent = emptyenv())
      load(ndlm_univar_path, envir = ndlm_univar_env)
      ndlm_univar_obj_name <- "new.theta.out_50_NDLM_univar_synth_DISC"
      if (!exists(ndlm_univar_obj_name, envir = ndlm_univar_env, inherits = FALSE)) {
        obj_candidates <- grep("^new\\.theta\\.out_.*NDLM_univar.*$", ls(ndlm_univar_env), value = TRUE)
        ndlm_univar_obj_name <- if (length(obj_candidates) > 0L) obj_candidates[[1L]] else ""
      }
      ndlm_univar_sigma_name <- "samp.sigma_50_NDLM_univar_synth_DISC"
      if (!exists(ndlm_univar_sigma_name, envir = ndlm_univar_env, inherits = FALSE)) {
        sigma_candidates <- grep("^samp\\.sigma_.*NDLM_univar.*$", ls(ndlm_univar_env), value = TRUE)
        ndlm_univar_sigma_name <- if (length(sigma_candidates) > 0L) sigma_candidates[[1L]] else ""
      }
      ndlm_univar_sample_mat <- tryCatch({
        ndlm_univar_obj <- if (nzchar(ndlm_univar_obj_name) && exists(ndlm_univar_obj_name, envir = ndlm_univar_env, inherits = FALSE)) {
          get(ndlm_univar_obj_name, envir = ndlm_univar_env, inherits = FALSE)
        } else {
          NULL
        }
        ndlm_univar_sigma_draws <- if (nzchar(ndlm_univar_sigma_name) && exists(ndlm_univar_sigma_name, envir = ndlm_univar_env, inherits = FALSE)) {
          get(ndlm_univar_sigma_name, envir = ndlm_univar_env, inherits = FALSE)
        } else {
          NULL
        }
        if (is.null(ndlm_univar_obj) || is.null(ndlm_univar_sigma_draws) ||
            !exists("ranges", inherits = TRUE) || !exists("FF_list", inherits = TRUE)) {
          NULL
        } else {
          n_samp_ndlm_univar <- suppressWarnings(as.integer(length(as.numeric(ndlm_univar_sigma_draws))))
          if (!is.finite(n_samp_ndlm_univar) || n_samp_ndlm_univar <= 1L) {
            NULL
          } else {
            ndlm_univar_mean_draws <- post_build_ndlm_state_draw_array(
              ndlm_obj = ndlm_univar_obj,
              ranges = get("ranges", inherits = TRUE),
              FF_list = get("FF_list", inherits = TRUE),
              n_samp = n_samp_ndlm_univar,
              p_state = if (exists("p", inherits = TRUE)) get("p", inherits = TRUE) else 7L,
              eps_reg = 0,
              seed = 777L,
              context = "crps.ndlm_univar.mean"
            )
            post_ndlm_predictive_draws(
              ndlm_raw = ndlm_univar_mean_draws,
              sigma_draws = ndlm_univar_sigma_draws,
              context = "crps.ndlm_univar",
              seed = 777L
            )$predictive_log1p
          }
        }
      }, error = function(e) {
        warning(
          sprintf("[CRPS_NDLM_UNIVAR_SKIP] Unable to compute NDLM univar CRPS (%s).", conditionMessage(e)),
          call. = FALSE
        )
        NULL
      })
      if (!is.null(ndlm_univar_sample_mat) && is.numeric(ndlm_univar_sample_mat) &&
          nrow(ndlm_univar_sample_mat) > 1L && ncol(ndlm_univar_sample_mat) > 0L) {
        ndlm_univar_meta <- post_crps_synth_model_meta(
          family = "ndlm_univar",
          likelihood_mode = "exal",
          transfer_mode = crps_ndlm_univar_transfer_mode
        )
        saveRDS(
          ndlm_univar_sample_mat,
          file = post_cache_path(post_cache_file_name(
            "ndlm_univar_predictive_log1p.rds",
            model_id = ndlm_univar_meta$model_id,
            transfer_mode = crps_ndlm_univar_transfer_mode
          ))
        )
        ndlm_univar_tbl <- post_crps_model_tables(
          model_id = ndlm_univar_meta$model_id,
          model_family = "synthesis",
          model_variant = ndlm_univar_meta$model_variant,
          sample_mat = ndlm_univar_sample_mat,
          obs = truth_from_start(ncol(ndlm_univar_sample_mat), "crps.ndlm_univar.truth"),
          forecast_dates = daily_dates_for_matrix_cols(ndlm_univar_sample_mat, start_date = crps_forecast_start, context = "crps.ndlm_univar.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = crps_ndlm_univar_transfer_mode,
          score_scale = "log_cms_plus1",
          context = "crps.ndlm_univar"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- ndlm_univar_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- ndlm_univar_tbl$summary
        collect_crps_input_health(
          model_id = ndlm_univar_meta$model_id,
          model_family = "synthesis",
          model_variant = ndlm_univar_meta$model_variant,
          sample_mat = ndlm_univar_sample_mat,
          forecast_dates = daily_dates_for_matrix_cols(
            ndlm_univar_sample_mat,
            start_date = crps_forecast_start,
            context = "crps.ndlm_univar.health.dates"
          ),
          transfer_mode = crps_ndlm_univar_transfer_mode,
          context = "crps.ndlm_univar"
        )
      } else {
        warning("[CRPS_NDLM_UNIVAR_SKIP] Unable to compute NDLM univar CRPS (invalid predictive draw matrix).", call. = FALSE)
      }
    } else if (ndlm_univar_enabled || nzchar(ndlm_univar_path)) {
      warning("[CRPS_NDLM_UNIVAR_SKIP] Unable to compute NDLM univar CRPS (artifact path missing).", call. = FALSE)
    }

    if (length(crps_per_time_rows) > 0L && length(crps_summary_rows) > 0L) {
      crps_per_time_df <- do.call(rbind, crps_per_time_rows)
      crps_summary_df <- do.call(rbind, crps_summary_rows)
      rownames(crps_per_time_df) <- NULL
      rownames(crps_summary_df) <- NULL

      crps_export <- post_export_crps_tables(
        per_time_df = crps_per_time_df,
        summary_df = crps_summary_df,
        output_dir = posterior_table_output_dir,
        table_formats = posterior_table_formats,
        keep_na = posterior_table_keep_na,
        numeric_digits = 17L,
        file_suffix = crps_output_suffix
      )
      posterior_table_export_manifest <<- rbind(posterior_table_export_manifest, crps_export$manifest)
    } else {
      warning("[CRPS_EXPORT_SKIP] No CRPS rows were produced for export.", call. = FALSE)
    }

    if (length(crps_truth_availability_rows) > 0L) {
      crps_truth_availability_df <- do.call(rbind, crps_truth_availability_rows)
      rownames(crps_truth_availability_df) <- NULL
      truth_export <- post_export_tables(
        tables = list(crps_truth_availability = crps_truth_availability_df),
        output_dir = posterior_table_output_dir,
        formats = posterior_table_formats,
        keep_na = posterior_table_keep_na,
        numeric_digits = 17L
      )
      posterior_table_export_manifest <<- rbind(posterior_table_export_manifest, truth_export)
    }

    multivar_gamma_sigma <- smoke_build_multivar_gamma_sigma_quantiles()
    if (!is.null(multivar_gamma_sigma) && nrow(multivar_gamma_sigma) > 0L) {
      gs_export <- post_export_gamma_sigma_tables(
        all_quantiles = multivar_gamma_sigma,
        output_dir = posterior_table_output_dir,
        ci_digits = 5L,
        write_tex = TRUE,
        table_formats = posterior_table_formats,
        keep_na = posterior_table_keep_na,
        numeric_digits = 15L
      )
      posterior_table_export_manifest <<- rbind(posterior_table_export_manifest, gs_export$manifest)
    } else {
      warning("[POSTERIOR_EXPORT_SKIP] Unable to assemble multivariate gamma/sigma summaries.", call. = FALSE)
    }

    multivar_covariate_summary <- smoke_build_multivar_covariate_effects_summary()
    if (!is.null(multivar_covariate_summary) && nrow(multivar_covariate_summary) > 0L) {
      cov_export <- post_export_covariate_effects_table(
        summary_df = multivar_covariate_summary,
        output_dir = posterior_table_output_dir,
        time_index = suppressWarnings(as.integer(get0("TT", ifnotfound = NA_integer_, inherits = TRUE))),
        ci_digits = 5L,
        write_tex = TRUE,
        table_formats = posterior_table_formats,
        keep_na = posterior_table_keep_na,
        numeric_digits = 15L
      )
      posterior_table_export_manifest <<- rbind(posterior_table_export_manifest, cov_export$manifest)
    } else {
      warning("[POSTERIOR_EXPORT_SKIP] Unable to assemble multivariate covariate-effects summary.", call. = FALSE)
    }

    if (isTRUE(crps_input_health_enabled) &&
        length(crps_input_health_rows) > 0L &&
        length(crps_input_health_per_time_rows) > 0L) {
      crps_input_health_df <- do.call(rbind, crps_input_health_rows)
      crps_input_health_per_time_df <- do.call(rbind, crps_input_health_per_time_rows)
      rownames(crps_input_health_df) <- NULL
      rownames(crps_input_health_per_time_df) <- NULL
      health_export <- post_export_crps_input_health_tables(
        summary_df = crps_input_health_df,
        per_time_df = crps_input_health_per_time_df,
        output_dir = posterior_table_output_dir,
        table_formats = posterior_table_formats,
        keep_na = posterior_table_keep_na,
        numeric_digits = 17L,
        file_suffix = crps_output_suffix
      )
      posterior_table_export_manifest <<- rbind(posterior_table_export_manifest, health_export$manifest)
    } else if (isTRUE(crps_input_health_enabled)) {
      warning("[CRPS_INPUT_HEALTH_EXPORT_SKIP] No CRPS input-health rows were produced for export.", call. = FALSE)
    }

    if (isTRUE(crps_input_health_fail_fast) && length(crps_input_health_failures) > 0L) {
      stop(
        sprintf(
          "[CRPS_INPUT_HEALTH_FAIL_FAST] non-finite or out-of-contract draws detected: %s",
          paste(crps_input_health_failures, collapse = " || ")
        ),
        call. = FALSE
      )
    }

    if (!is.null(posterior_table_export_manifest) && nrow(as.data.frame(posterior_table_export_manifest)) > 0L) {
      post_write_table_exports_manifest(
        manifest_df = posterior_table_export_manifest,
        output_dir = posterior_table_output_dir
      )
      post_write_table_exports_readme(
        output_dir = posterior_table_output_dir,
        ci_digits = 5L,
        table_formats = posterior_table_formats
      )
    }
  })
}

profile_section("figures_smoke_fast.comparison_figures", {
  q_probs_synth <- smoke_multivar_quantile_spec()$probs
  q_probs_ndlm <- c(0.05, 0.50, 0.95)
  secondary_multivar_keep_pass <- nzchar(crps_output_suffix)

  if (!secondary_multivar_keep_pass && isTRUE(get0("MODEL_RUN_EXDQLM_UNIVAR", ifnotfound = FALSE, inherits = TRUE))) {
    univar_meta <- post_crps_synth_model_meta(
      family = "univar",
      likelihood_mode = crps_univar_likelihood_mode,
      transfer_mode = NA_character_
    )
    hist_samples <- smoke_read_numeric_matrix_rds(post_cache_path("synth_univar_hist_log1p.rds"))
    hist_q <- smoke_read_numeric_matrix_rds(post_cache_path("synth_univar_hist_quantiles_log1p.rds"))
    fc_samples <- smoke_read_numeric_matrix_rds(post_cache_path("synth_univar_forecast_log1p.rds"))
    fc_q <- smoke_read_numeric_matrix_rds(post_cache_path("synth_univar_forecast_quantiles_log1p.rds"))
    if (!is.null(hist_samples) && !is.null(fc_samples)) {
      hist_dates <- smoke_trim_dates(smoke_window_dates(PLOT_START_DATE, CUTOFF_DATE), ncol(hist_samples))
      fc_dates <- smoke_forecast_dates(ncol(fc_samples))
      if (is.null(hist_q)) hist_q <- smoke_matrix_quantiles(hist_samples, probs = q_probs_synth)
      if (is.null(fc_q)) fc_q <- smoke_matrix_quantiles(fc_samples, probs = q_probs_synth)
      smoke_emit_synthesis_bundle(
        model_id = univar_meta$model_id,
        title_text = sprintf("%s cutoff-window posterior synthesis", univar_meta$model_id),
        hist_dates = hist_dates,
        hist_obs = smoke_usgs_log1p_by_dates(hist_dates),
        hist_samples = hist_samples,
        hist_q = hist_q,
        fc_dates = fc_dates,
        fc_obs = smoke_usgs_log1p_by_dates(fc_dates),
        fc_samples = fc_samples,
        fc_q = fc_q,
        probs = q_probs_synth
      )
    }
  }

  if (isTRUE(get0("MODEL_RUN_EXDQLM_MULTIVAR", ifnotfound = FALSE, inherits = TRUE))) {
    multivar_meta <- post_crps_synth_model_meta(
      family = "multivar",
      likelihood_mode = crps_multivar_likelihood_mode,
      transfer_mode = crps_transfer_mode
    )
    invisible(smoke_build_multivar_hist_location_summary())
    invisible(smoke_build_multivar_forecast_location_summary())
    hist_bundle <- smoke_build_multivar_hist_synth()
    fc_samples <- smoke_build_multivar_synth_f()
    if (!is.null(hist_bundle) && !is.null(fc_samples)) {
      fc_quant_cache <- post_cache_path(post_cache_file_name(
        "synth_multivar_forecast_quantiles_log1p.rds",
        model_id = multivar_meta$model_id,
        transfer_mode = crps_transfer_mode
      ))
      fc_q <- smoke_matrix_quantiles(fc_samples, probs = q_probs_synth)
      saveRDS(fc_q, fc_quant_cache)
      fc_dates <- smoke_forecast_dates(ncol(fc_samples))
      smoke_emit_synthesis_bundle(
        model_id = multivar_meta$model_id,
        title_text = sprintf("%s cutoff-window posterior synthesis", multivar_meta$model_id),
        hist_dates = hist_bundle$dates,
        hist_obs = smoke_usgs_log1p_by_dates(hist_bundle$dates),
        hist_samples = hist_bundle$sample_mat,
        hist_q = hist_bundle$quantiles,
        fc_dates = fc_dates,
        fc_obs = smoke_usgs_log1p_by_dates(fc_dates),
        fc_samples = fc_samples,
        fc_q = fc_q,
        probs = q_probs_synth
      )
    }
  }

  if (!secondary_multivar_keep_pass &&
      isTRUE(get0("MODEL_RUN_NDLM_MAIN", ifnotfound = FALSE, inherits = TRUE)) &&
      exists("new.theta.out_50_NDLM_synth_DISC", inherits = TRUE)) {
    ndlm_meta <- post_crps_synth_model_meta(
      family = "ndlm",
      likelihood_mode = "exal",
      transfer_mode = crps_ndlm_transfer_mode
    )
    ndlm_obj <- get("new.theta.out_50_NDLM_synth_DISC", inherits = TRUE)
    fc_cache <- post_cache_path(post_cache_file_name(
      "ndlm_predictive_log1p.rds",
      model_id = ndlm_meta$model_id,
      transfer_mode = crps_ndlm_transfer_mode
    ))
    fc_samples <- smoke_read_numeric_matrix_rds(fc_cache)
    if (is.null(fc_samples)) {
      fc_samples <- smoke_read_numeric_matrix_rds(post_cache_path("y_reps_ndlm_log1p.rds"))
    }
    if (!is.null(fc_samples) && is.matrix(ndlm_obj$exps) && is.matrix(ndlm_obj$vars)) {
      hist_dates_all <- smoke_trim_dates(smoke_resolve_hist_dates(), ncol(ndlm_obj$exps))
      hist_idx <- which(hist_dates_all >= as.Date(PLOT_START_DATE) & hist_dates_all <= as.Date(CUTOFF_DATE))
      if (length(hist_idx) > 0L) {
        hist_dates <- hist_dates_all[hist_idx]
        hist_q <- smoke_quantile_band_from_moments(
          mean_vec = ndlm_obj$exps[1L, hist_idx],
          var_vec = ndlm_obj$vars[1L, hist_idx],
          probs = q_probs_ndlm,
          transform = "loglog1p_to_log1p",
          context = paste0(ndlm_meta$model_id, ".history")
        )
        fc_dates <- smoke_forecast_dates(ncol(fc_samples))
        fc_q <- fast_col_quantiles_t(fc_samples, probs = q_probs_ndlm)
        saveRDS(
          fc_q,
          file = post_cache_path(post_cache_file_name(
            "ndlm_predictive_quantiles_log1p.rds",
            model_id = ndlm_meta$model_id,
            transfer_mode = crps_ndlm_transfer_mode
          ))
        )
        smoke_emit_ndlm_bundle(
          model_id = ndlm_meta$model_id,
          title_text = sprintf("%s cutoff-window predictive bands", ndlm_meta$model_id),
          hist_dates = hist_dates,
          hist_obs = smoke_usgs_log1p_by_dates(hist_dates),
          hist_q = hist_q,
          fc_dates = fc_dates,
          fc_obs = smoke_usgs_log1p_by_dates(fc_dates),
          fc_q = fc_q,
          note = "plot_scale=log1p_cms; historical_band_from_exps_vars_internal_scale_aware"
        )
      }
    }
  }

  if (!secondary_multivar_keep_pass && isTRUE(get0("MODEL_RUN_NDLM_UNIVAR", ifnotfound = FALSE, inherits = TRUE))) {
    ndlm_univar_meta <- post_crps_synth_model_meta(
      family = "ndlm_univar",
      likelihood_mode = "exal",
      transfer_mode = crps_ndlm_univar_transfer_mode
    )
    ndlm_univar_artifact <- smoke_load_ndlm_univar_artifact()
    fc_cache <- post_cache_path(post_cache_file_name(
      "ndlm_univar_predictive_log1p.rds",
      model_id = ndlm_univar_meta$model_id,
      transfer_mode = crps_ndlm_univar_transfer_mode
    ))
    fc_samples <- smoke_read_numeric_matrix_rds(fc_cache)
    if (is.null(fc_samples) && !is.null(ndlm_univar_artifact) &&
        exists("ranges", inherits = TRUE) && exists("FF_list", inherits = TRUE)) {
      n_samp_ndlm_univar <- suppressWarnings(as.integer(length(as.numeric(ndlm_univar_artifact$sigma_draws))))
      if (is.finite(n_samp_ndlm_univar) && n_samp_ndlm_univar > 1L) {
        ndlm_univar_mean_draws <- post_build_ndlm_state_draw_array(
          ndlm_obj = ndlm_univar_artifact$obj,
          ranges = get("ranges", inherits = TRUE),
          FF_list = get("FF_list", inherits = TRUE),
          n_samp = n_samp_ndlm_univar,
          p_state = if (exists("p", inherits = TRUE)) get("p", inherits = TRUE) else 7L,
          eps_reg = 0,
          seed = 777L,
          context = "figures.ndlm_univar.mean"
        )
        fc_samples <- post_ndlm_predictive_draws(
          ndlm_raw = ndlm_univar_mean_draws,
          sigma_draws = ndlm_univar_artifact$sigma_draws,
          context = "figures.ndlm_univar",
          seed = 777L
        )$predictive_log1p
        saveRDS(fc_samples, fc_cache)
      }
    }
    if (!is.null(ndlm_univar_artifact) && !is.null(fc_samples) &&
        is.matrix(ndlm_univar_artifact$obj$exps) && is.matrix(ndlm_univar_artifact$obj$vars)) {
      hist_dates_all <- smoke_trim_dates(smoke_resolve_hist_dates(), ncol(ndlm_univar_artifact$obj$exps))
      hist_idx <- which(hist_dates_all >= as.Date(PLOT_START_DATE) & hist_dates_all <= as.Date(CUTOFF_DATE))
      if (length(hist_idx) > 0L) {
        hist_dates <- hist_dates_all[hist_idx]
        hist_q <- smoke_quantile_band_from_moments(
          mean_vec = ndlm_univar_artifact$obj$exps[1L, hist_idx],
          var_vec = ndlm_univar_artifact$obj$vars[1L, hist_idx],
          probs = q_probs_ndlm,
          transform = "identity",
          context = paste0(ndlm_univar_meta$model_id, ".history")
        )
        fc_dates <- smoke_forecast_dates(ncol(fc_samples))
        fc_q <- fast_col_quantiles_t(fc_samples, probs = q_probs_ndlm)
        saveRDS(
          fc_q,
          file = post_cache_path(post_cache_file_name(
            "ndlm_univar_predictive_quantiles_log1p.rds",
            model_id = ndlm_univar_meta$model_id,
            transfer_mode = crps_ndlm_univar_transfer_mode
          ))
        )
        smoke_emit_ndlm_bundle(
          model_id = ndlm_univar_meta$model_id,
          title_text = sprintf("%s cutoff-window predictive bands", ndlm_univar_meta$model_id),
          hist_dates = hist_dates,
          hist_obs = smoke_usgs_log1p_by_dates(hist_dates),
          hist_q = hist_q,
          fc_dates = fc_dates,
          fc_obs = smoke_usgs_log1p_by_dates(fc_dates),
          fc_q = fc_q,
          note = "plot_scale=log1p_cms; ndlm_univar_lightweight_cache"
        )
      }
    }
  }

  smoke_write_figure_manifest()
})
