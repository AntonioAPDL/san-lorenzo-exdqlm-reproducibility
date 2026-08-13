# Function to concatenate matrices horizontally based on row numbers
concatenate_matrices <- function(FFF_list) {
  concatenated_list <- list()
  J <- length(FFF_list)

  if (J == 1) {
    concatenated_list[[1]] <- FFF_list[[1]]
    return(concatenated_list)
  }

  start_row <- 1
  for (j in J:2) {
    row_num <- nrow(FFF_list[[j]])
    if (is.na(row_num) || row_num <= 0L) next

    idx <- start_row:(start_row + row_num - 1L)
    concatenated_matrix <- do.call(
      cbind,
      lapply(FFF_list[1:J], function(mat) {
        out <- matrix(NA_real_, nrow = row_num, ncol = ncol(mat))
        valid <- idx[idx <= nrow(mat)]
        if (length(valid) > 0L) {
          out[seq_along(valid), ] <- mat[valid, , drop = FALSE]
        }
        out
      })
    )

    concatenated_list[[J - j + 1]] <- concatenated_matrix
    start_row <- start_row + row_num
  }

  # Handle the last remaining rows from the first matrix
  row_num <- nrow(FFF_list[[1]]) - start_row + 1L
  if (!is.na(row_num) && row_num > 0L) {
    concatenated_list[[length(concatenated_list) + 1L]] <- FFF_list[[1]][start_row:(start_row + row_num - 1L), , drop = FALSE]
  }

  return(concatenated_list)
}

ensembles_forecast <- concatenate_matrices(ensembles)
ensembles_forecast <- lapply(ensembles_forecast, t)
#############################################################################################################################################
#############################################################################################################################################
 
dM <- 1 #Fix to one?
Ones <- matrix(1, dim(model$GG)[1], dim(model$GG)[1])
Ones_ens <- matrix(1, dim(GG_list[[1]])[1], dim(GG_list[[1]])[1])
########################
C0 <- as.matrix(model$C0)
m0 <- model$m0
ex.df.mat <- as.matrix(ex.df.mat)
ex.df.mat.k <- as.matrix(ex.df.mat.k)
########################
y <- Y

crit_ELBO <- 0
ELBO <- 0
seq.elbo = ELBO
iter = 0
FLAG = TRUE
tol1 <- 1e-2
tol2 <- 1e-2
conv.check <- 0
max_iter <- 800
fast <- 0

write.csv(timestamps, TIMESTAMPS_CSV, row.names = FALSE)
# timestamps_loaded <- read.csv("SOURCE_WORKFLOW_ROOT/timestamps.csv")
# timestamps_loaded$Date <- as.Date(timestamps_loaded$Date)
# head(timestamps_loaded)

###############################################################################
# Univariate block + synthesis utilities (always runs)
# Inputs:
#   - variables_*_exAL_synth_DISC_uni.RData
#   - X_f, timestamps, Y and model objects from prior modules
# Outputs:
#   - Univariate diagnostics and plots (saved in figures module)
# Dependencies:
#   - 00_paths.R, 02_helpers_core.R
###############################################################################

synthesize_samples <- function(y_reps, q_s, k = 1) {
  n.q     <- dim(y_reps)[1]
  n.samp  <- dim(y_reps)[2]
  n.times <- dim(y_reps)[3]
  stopifnot(length(q_s) == n.q, !is.unsorted(q_s))
  total_samp <- k * n.samp
  out <- matrix(NA_real_, nrow = total_samp, ncol = n.times)
  for (t_idx in seq_len(n.times)) {
      for (i in 1:total_samp) {
      u <- runif(1)
      idx <- findInterval(u, q_s)
      if((idx != 0) && (idx != n.q) ){
        q_lo <- q_s[idx]
        q_hi <- q_s[idx + 1]
        w <- (u - q_lo) / (q_hi - q_lo)
        y_lower <- quantile(y_reps[idx, , t_idx], probs = u, type = 7L, names = FALSE)
        y_upper <- quantile(y_reps[idx + 1, , t_idx], probs = u, type = 7L, names = FALSE)
        result <- (1 - w) * y_lower + w * y_upper
        out[i, t_idx] <- result
      }else{
        if(idx == 0){
          out[i, t_idx] <- quantile(y_reps[idx + 1, , t_idx], probs = u, type = 7L, names = FALSE)
        }else{
          out[i, t_idx] <- quantile(y_reps[idx, , t_idx], probs = u, type = 7L, names = FALSE)
        }
      }
    }
  }
  return(out)
}

post_transform_latent_log1p_cap <- function(arr, context, report_name = NULL) {
  report_path <- NULL
  if (!is.null(report_name) && nzchar(report_name)) {
    report_path <- file.path(OUT_DIR, report_name)
  }
  post_transform_internal_array_to_log1p(
    arr,
    from_scale = post_resolve_analysis_scale_post_internal(),
    context = paste0(context, ".internal"),
    report_path = report_path
  )$values
}

file_path <- UNI_VAR_05
load_rdata_with_retry <- function(path, attempts = 3L, sleep_sec = 0.5, envir = parent.frame()) {
  stopifnot(is.character(path), length(path) == 1L, attempts >= 1L)
  last_err <- NULL
  for (i in seq_len(attempts)) {
    ok <- tryCatch({
      # Ensure loaded objects persist in the caller's scope (this module's run env).
      load(path, envir = envir)
      TRUE
    }, error = function(e) {
      last_err <<- e
      FALSE
    })
    if (ok) return(invisible(TRUE))
    Sys.sleep(sleep_sec)
  }
  stop(last_err)
}

quantile_label_tag <- function(label) {
  as.character(as.integer(to_quantile_label(label)))
}

load_quantile_bundle_with_alias <- function(
  path,
  target_label,
  source_label,
  suffix,
  target_suffix = suffix,
  attempts = 3L,
  sleep_sec = 0.5
) {
  path <- as.character(path)
  if (!nzchar(path) || !file.exists(path)) {
    return(invisible(FALSE))
  }
  target_tag <- quantile_label_tag(target_label)
  source_tag <- quantile_label_tag(if (is.null(source_label) || !nzchar(as.character(source_label))) target_label else source_label)
  bundle_env <- new.env(parent = emptyenv())
  load_rdata_with_retry(path, attempts = attempts, sleep_sec = sleep_sec, envir = bundle_env)

  obj_names <- ls(bundle_env, all.names = TRUE)
  src_token <- paste0("_", source_tag, "_", suffix)
  tgt_token <- paste0("_", target_tag, "_", target_suffix)
  for (nm in obj_names) {
    value <- get(nm, envir = bundle_env, inherits = FALSE)
    assign(nm, value, envir = parent.frame())
    if (!identical(source_tag, target_tag) || !identical(suffix, target_suffix)) {
      alias_name <- sub(src_token, tgt_token, nm, fixed = TRUE)
      if (!identical(alias_name, nm)) {
        assign(alias_name, value, envir = parent.frame())
      }
    }
  }
  invisible(TRUE)
}

alias_univar_bundle_if_missing <- function(target_label, source_label = "50", suffix = "exAL_synth_DISC_uni") {
  target_tag <- quantile_label_tag(target_label)
  source_tag <- quantile_label_tag(source_label)
  name_map <- c(
    new_theta = sprintf("new.theta.out_%s_%s", target_tag, suffix),
    samp_theta = sprintf("samp.theta_%s_%s", target_tag, suffix),
    samp_sigma = sprintf("samp.sigma_%s_%s", target_tag, suffix),
    samp_gamma = sprintf("samp.gamma_%s_%s", target_tag, suffix),
    seq_elbo = sprintf("seq.elbo_%s_%s", target_tag, suffix)
  )
  source_map <- c(
    new_theta = sprintf("new.theta.out_%s_%s", source_tag, suffix),
    samp_theta = sprintf("samp.theta_%s_%s", source_tag, suffix),
    samp_sigma = sprintf("samp.sigma_%s_%s", source_tag, suffix),
    samp_gamma = sprintf("samp.gamma_%s_%s", source_tag, suffix),
    seq_elbo = sprintf("seq.elbo_%s_%s", source_tag, suffix)
  )
  if (exists(name_map[["new_theta"]], inherits = TRUE)) {
    return(invisible(FALSE))
  }
  if (!exists(source_map[["new_theta"]], inherits = TRUE)) {
    return(invisible(FALSE))
  }
  for (nm in names(name_map)) {
    src <- source_map[[nm]]
    tgt <- name_map[[nm]]
    if (exists(src, inherits = TRUE) && !exists(tgt, inherits = TRUE)) {
      assign(tgt, get(src, inherits = TRUE), envir = parent.frame())
    }
  }
  invisible(TRUE)
}

univar_theory_post_q_row_map <- function() {
  c("5" = 1L, "20" = 2L, "35" = 3L, "50" = 4L, "65" = 5L, "80" = 6L, "95" = 7L)
}

univar_post_impl_mode <- function(default = "theory_aligned") {
  raw <- tolower(trimws(Sys.getenv("UNIFIED_EXDQLM_UNIVAR_IMPLEMENTATION_MODE", default)))
  if (!nzchar(raw) || !(raw %in% c("theory_aligned", "legacy_bridge"))) {
    raw <- default
  }
  raw
}

univar_post_use_theory_rebuild <- function() {
  identical(univar_post_impl_mode(), "theory_aligned")
}

univar_theory_post_prob <- function(q_tag) {
  as.numeric(as.integer(q_tag)) / 100
}

univar_theory_post_first_state <- function(q_tags) {
  for (q in q_tags) {
    obj <- get0(sprintf("exdqlm_univar_theory_state_%s", q), ifnotfound = NULL, inherits = TRUE)
    if (is.list(obj)) return(obj)
  }
  obj <- get0("exdqlm_univar_theory_state", ifnotfound = NULL, inherits = TRUE)
  if (is.list(obj)) return(obj)
  NULL
}

univar_theory_post_state_info <- function(q_tags) {
  st <- univar_theory_post_first_state(q_tags)
  d_act <- if (is.list(st) && !is.null(st$active_dim)) suppressWarnings(as.integer(st$active_dim[[1L]])) else NA_integer_
  if (!is.finite(d_act) || d_act < 1L) d_act <- 6L
  q_diag <- if (is.list(st) && !is.null(st$q_diag)) suppressWarnings(as.numeric(st$q_diag)) else numeric(0)
  if (length(q_diag) != d_act || any(!is.finite(q_diag)) || any(q_diag <= 0)) {
    q_diag <- c(0.05, rep(0.01, max(d_act - 1L, 0L)))
  }
  likelihood_mode <- if (is.list(st) && !is.null(st$likelihood_mode)) {
    tolower(trimws(as.character(st$likelihood_mode[[1L]])))
  } else {
    tolower(trimws(Sys.getenv("UNIFIED_EXDQLM_UNIVAR_LIKELIHOOD_MODE", "exal")))
  }
  if (!(likelihood_mode %in% c("exal", "al"))) likelihood_mode <- "exal"
  list(active_dim = d_act, q_diag = q_diag, likelihood_mode = likelihood_mode)
}

univar_theory_post_design_mats <- function(active_dim, TT, horizon, hist_len) {
  x_hist <- if (exists("X", inherits = TRUE) && is.numeric(X)) as.matrix(X) else matrix(0, nrow = TT, ncol = max(active_dim - 1L, 0L))
  if (nrow(x_hist) < TT) {
    x_hist <- rbind(x_hist, matrix(0, nrow = TT - nrow(x_hist), ncol = ncol(x_hist)))
  }
  x_hist <- x_hist[seq_len(TT), , drop = FALSE]

  x_future <- if (exists("X_f", inherits = TRUE) && is.numeric(X_f)) as.matrix(X_f) else matrix(0, nrow = horizon, ncol = max(active_dim - 1L, 0L))
  if (nrow(x_future) < horizon) {
    pad_n <- horizon - nrow(x_future)
    pad_row <- if (nrow(x_future) > 0L) x_future[nrow(x_future), , drop = FALSE] else matrix(0, nrow = 1L, ncol = ncol(x_future))
    x_future <- rbind(x_future, matrix(rep(pad_row, each = pad_n), nrow = pad_n, byrow = TRUE))
  }
  x_future <- x_future[seq_len(horizon), , drop = FALSE]

  n_cov <- max(active_dim - 1L, 0L)
  if (ncol(x_hist) < n_cov) {
    x_hist <- cbind(x_hist, matrix(0, nrow = nrow(x_hist), ncol = n_cov - ncol(x_hist)))
  }
  if (ncol(x_future) < n_cov) {
    x_future <- cbind(x_future, matrix(0, nrow = nrow(x_future), ncol = n_cov - ncol(x_future)))
  }
  x_hist <- x_hist[, seq_len(n_cov), drop = FALSE]
  x_future <- x_future[, seq_len(n_cov), drop = FALSE]

  hist_idx <- seq.int(max(1L, TT - hist_len + 1L), TT)
  F_hist <- cbind(1, x_hist[hist_idx, , drop = FALSE])
  F_future <- cbind(1, x_future[seq_len(horizon), , drop = FALSE])
  list(F_hist = F_hist, F_future = F_future, hist_idx = hist_idx)
}

univar_theory_post_bundle_info <- function(q_tag, active_dim, n_keep) {
  suffix <- sprintf("%s_exAL_synth_DISC_uni", q_tag)
  theta_name <- sprintf("samp.theta_%s", suffix)
  sigma_name <- sprintf("samp.sigma_%s", suffix)
  gamma_name <- sprintf("samp.gamma_%s", suffix)
  theta <- get0(theta_name, ifnotfound = NULL, inherits = TRUE)
  sigma <- get0(sigma_name, ifnotfound = NULL, inherits = TRUE)
  gamma <- get0(gamma_name, ifnotfound = NULL, inherits = TRUE)
  if (!is.array(theta) || length(dim(theta)) != 3L) return(NULL)
  if (!is.matrix(sigma) || nrow(sigma) < 1L) return(NULL)
  if (!is.matrix(gamma) || nrow(gamma) < 1L) return(NULL)
  keep <- min(n_keep, dim(theta)[3], ncol(sigma), ncol(gamma))
  if (!is.finite(keep) || keep < 1L) return(NULL)
  list(
    theta = theta[seq_len(min(active_dim, dim(theta)[1])), , seq_len(keep), drop = FALSE],
    sigma = as.numeric(sigma[1, seq_len(keep)]),
    gamma = as.numeric(gamma[1, seq_len(keep)]),
    keep = as.integer(keep),
    p0 = univar_theory_post_prob(q_tag)
  )
}

univar_theory_post_rebuild_outputs <- function(days_hist_uni = 19L) {
  q_map <- univar_theory_post_q_row_map()
  q_tags <- names(q_map)
  state_info <- univar_theory_post_state_info(q_tags)
  active_dim <- state_info$active_dim
  likelihood_mode <- state_info$likelihood_mode
  q_diag <- as.numeric(state_info$q_diag)

  TT_local <- suppressWarnings(as.integer(TT))
  horizon <- suppressWarnings(as.integer(ranges[[1L]]))
  if (!is.finite(TT_local) || TT_local < 2L || !is.finite(horizon) || horizon < 1L) {
    return(NULL)
  }

  keep_counts <- integer(0)
  bundle_cache <- list()
  for (q in q_tags) {
    info <- univar_theory_post_bundle_info(q, active_dim = active_dim, n_keep = Inf)
    if (is.null(info)) next
    bundle_cache[[q]] <- info
    keep_counts <- c(keep_counts, info$keep)
  }
  if (length(bundle_cache) < 1L) return(NULL)
  n_keep <- min(keep_counts)
  if (!is.finite(n_keep) || n_keep < 1L) return(NULL)
  bundle_cache <- lapply(bundle_cache, function(x) {
    x$theta <- x$theta[, , seq_len(n_keep), drop = FALSE]
    x$sigma <- x$sigma[seq_len(n_keep)]
    x$gamma <- x$gamma[seq_len(n_keep)]
    x$keep <- n_keep
    x
  })

  days_hist_uni <- min(as.integer(days_hist_uni), TT_local)
  design <- univar_theory_post_design_mats(
    active_dim = active_dim,
    TT = TT_local,
    horizon = horizon,
    hist_len = days_hist_uni
  )
  F_hist <- as.matrix(design$F_hist)
  F_future <- as.matrix(design$F_future)
  hist_idx <- as.integer(design$hist_idx)
  qchol <- diag(sqrt(q_diag), nrow = active_dim, ncol = active_dim)

  xb_hist_uni <- array(NA_real_, dim = c(7L, n_keep, days_hist_uni))
  y_hist_uni <- array(NA_real_, dim = c(7L, n_keep, days_hist_uni))
  xb_forecast <- array(NA_real_, dim = c(7L, n_keep, horizon))
  y_forecast <- array(NA_real_, dim = c(7L, n_keep, horizon))

  seed_local <- suppressWarnings(as.integer(Sys.getenv("DISC_BASE_SEED", "777")))
  if (!is.finite(seed_local)) seed_local <- 777L
  set.seed(seed_local + 301L)

  for (q in names(bundle_cache)) {
    row_idx <- q_map[[q]]
    info <- bundle_cache[[q]]
    gamma_use <- if (identical(likelihood_mode, "al")) rep(0, n_keep) else as.numeric(info$gamma)

    for (i in seq_len(n_keep)) {
      sigma_i <- max(as.numeric(info$sigma[[i]]), 1e-8)
      gamma_i <- as.numeric(gamma_use[[i]])

      for (tt in seq_len(days_hist_uni)) {
        theta_t <- as.numeric(info$theta[, hist_idx[[tt]], i])
        mu_t <- sum(F_hist[tt, seq_len(active_dim)] * theta_t)
        xb_hist_uni[row_idx, i, tt] <- mu_t
        y_hist_uni[row_idx, i, tt] <- rexal(1, info$p0, mu_t, sigma_i, gamma_i)
      }

      theta_k <- as.numeric(info$theta[, TT_local, i])
      for (k in seq_len(horizon)) {
        theta_k <- theta_k + as.numeric(qchol %*% stats::rnorm(active_dim))
        mu_k <- sum(F_future[k, seq_len(active_dim)] * theta_k)
        xb_forecast[row_idx, i, k] <- mu_k
        y_forecast[row_idx, i, k] <- rexal(1, info$p0, mu_k, sigma_i, gamma_i)
      }
    }
  }

  for (row_idx in seq_len(dim(y_hist_uni)[1])) {
    for (tt in seq_len(days_hist_uni)) {
      y_hist_uni[row_idx, , tt] <- sort_keep_na(y_hist_uni[row_idx, , tt])
    }
    for (k in seq_len(horizon)) {
      y_forecast[row_idx, , k] <- sort_keep_na(y_forecast[row_idx, , k])
    }
  }

  active_rows <- unname(q_map[names(bundle_cache)])
  q_s_active <- vapply(names(bundle_cache), univar_theory_post_prob, numeric(1))
  active_order <- order(q_s_active)
  active_rows <- active_rows[active_order]
  q_s_active <- q_s_active[active_order]

  theory_hist_cube_log1p <- profile_section(
    "univariate.transform_hist.theory_rebuild",
    post_transform_latent_log1p_cap(
      y_hist_uni[active_rows, , , drop = FALSE],
      context = "univariate.theory.hist",
      report_name = "univar_theory_hist_exp_guard.txt"
    )
  )
  synth_hist_uni <- profile_section(
    "univariate.synthesize_hist.theory_rebuild",
    synthesize_samples(theory_hist_cube_log1p, q_s_active)
  )
  synth_hist_uni_q <- t(colQuantiles(synth_hist_uni, probs = q_s_active, type = 8))
  for (tt in seq_len(days_hist_uni)) synth_hist_uni[, tt] <- sort_keep_na(synth_hist_uni[, tt])

  theory_forecast_cube_log1p <- profile_section(
    "univariate.transform_forecast.theory_rebuild",
    post_transform_latent_log1p_cap(
      y_forecast[active_rows, , , drop = FALSE],
      context = "univariate.theory.forecast",
      report_name = "univar_theory_forecast_exp_guard.txt"
    )
  )
  synth_f2 <- profile_section(
    "univariate.synthesize_forecast.theory_rebuild",
    synthesize_samples(theory_forecast_cube_log1p, q_s_active)
  )
  synth_f2_q <- t(colQuantiles(synth_f2, probs = q_s_active, type = 8))
  for (k in seq_len(horizon)) synth_f2[, k] <- sort_keep_na(synth_f2[, k])

  crossing <- post_quantile_crossing_summary(
    sample_cube = y_forecast[active_rows, , , drop = FALSE],
    q_probs = q_s_active,
    context = "univariate.theory_forecast"
  )
  write.csv(
    crossing$per_time,
    file = file.path(OUT_DIR, "univar_theory_forecast_quantile_crossing_per_time.csv"),
    row.names = FALSE
  )
  write.csv(
    crossing$summary,
    file = file.path(OUT_DIR, "univar_theory_forecast_quantile_crossing_summary.csv"),
    row.names = FALSE
  )

  saveRDS(y_hist_uni, file = post_cache_path("y_hist_uni.rds"))
  saveRDS(y_forecast, file = post_cache_path("y_forecast_uni.rds"))

  list(
    xb_hist_uni = xb_hist_uni,
    y_hist_uni = y_hist_uni,
    xb_forecast = xb_forecast,
    y_forecast = y_forecast,
    synth_hist_uni = synth_hist_uni,
    synth_hist_uni_q = synth_hist_uni_q,
    synth_f2 = synth_f2,
    synth_f2_q = synth_f2_q,
    q_s_active = q_s_active,
    active_rows = active_rows,
    n_samp = as.integer(n_keep),
    days_hist_uni = as.integer(days_hist_uni)
  )
}

is_numeric_matrix <- function(x) {
  is.numeric(x) && !is.null(dim(x)) && length(dim(x)) == 2L
}

is_numeric_array3 <- function(x) {
  is.numeric(x) && !is.null(dim(x)) && length(dim(x)) == 3L
}

is_numeric_matrix_list <- function(x) {
  is.list(x) && length(x) > 0L && all(vapply(x, is_numeric_matrix, logical(1)))
}

is_numeric_array3_list <- function(x) {
  is.list(x) && length(x) > 0L && all(vapply(x, is_numeric_array3, logical(1)))
}

extract_nested_list_field <- function(x, field_name, validator) {
  if (validator(x)) {
    return(x)
  }
  if (!is.list(x)) {
    return(NULL)
  }
  nested <- x[[field_name]]
  if (!is.null(nested) && validator(nested)) {
    return(nested)
  }
  idx <- which(vapply(x, validator, logical(1)))
  if (length(idx) > 0L) {
    return(x[[idx[[1L]]]])
  }
  NULL
}

normalize_ndlm_ensemble_fields <- function(
  obj_name = "new.theta.out_50_NDLM_synth_DISC",
  assign_env = parent.frame()
) {
  if (!exists(obj_name, envir = assign_env, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  obj <- get(obj_name, envir = assign_env, inherits = FALSE)
  if (!is.list(obj)) {
    return(invisible(FALSE))
  }

  sm_raw <- obj$sm_ens
  sc_raw <- obj$sC_ens
  sm_fixed <- extract_nested_list_field(sm_raw, "sm_ens", is_numeric_matrix_list)
  sc_fixed <- extract_nested_list_field(sc_raw, "sC_ens", is_numeric_array3_list)

  normalized <- FALSE
  if (!is.null(sm_fixed) && !identical(sm_raw, sm_fixed)) {
    obj$sm_ens <- sm_fixed
    normalized <- TRUE
  }
  if (!is.null(sc_fixed) && !identical(sc_raw, sc_fixed)) {
    obj$sC_ens <- sc_fixed
    normalized <- TRUE
  }

  if (normalized) {
    assign(obj_name, obj, envir = assign_env)
    warning(
      sprintf(
        "Normalized %s ensemble fields to canonical structure (sm_ens=%d, sC_ens=%d).",
        obj_name,
        length(obj$sm_ens),
        length(obj$sC_ens)
      ),
      call. = FALSE
    )
  }
  invisible(normalized)
}

load_ndlm_bundle_with_normalize <- function(
  path,
  obj_name = "new.theta.out_50_NDLM_synth_DISC",
  attempts = 3L,
  sleep_sec = 0.5
) {
  load_rdata_with_retry(path, attempts = attempts, sleep_sec = sleep_sec, envir = parent.frame())
  normalize_ndlm_ensemble_fields(obj_name = obj_name, assign_env = parent.frame())
  invisible(TRUE)
}

univar_legacy_post_bundle_info <- function(q_tag, state_idx, n_keep) {
  suffix <- sprintf("%s_exAL_synth_DISC_uni", q_tag)
  theta_name <- sprintf("samp.theta_%s", suffix)
  sigma_name <- sprintf("samp.sigma_%s", suffix)
  gamma_name <- sprintf("samp.gamma_%s", suffix)
  state_name <- sprintf("new.theta.out_%s", suffix)

  theta <- get0(theta_name, ifnotfound = NULL, inherits = TRUE)
  sigma <- get0(sigma_name, ifnotfound = NULL, inherits = TRUE)
  gamma <- get0(gamma_name, ifnotfound = NULL, inherits = TRUE)
  state_obj <- get0(state_name, ifnotfound = NULL, inherits = TRUE)
  if (!is.array(theta) || length(dim(theta)) != 3L) return(NULL)
  if (!is.matrix(sigma) || nrow(sigma) < 1L) return(NULL)
  if (!is.matrix(gamma) || nrow(gamma) < 1L) return(NULL)
  if (!is.list(state_obj) || !is.array(state_obj$sC) || length(dim(state_obj$sC)) != 3L) return(NULL)

  keep <- min(n_keep, dim(theta)[3], ncol(sigma), ncol(gamma))
  if (!is.finite(keep) || keep < 1L) return(NULL)

  list(
    theta = theta[state_idx, , seq_len(keep), drop = FALSE],
    sigma = as.numeric(sigma[1, seq_len(keep)]),
    gamma = as.numeric(gamma[1, seq_len(keep)]),
    sC_T = {
      sC_slice <- state_obj$sC[state_idx, state_idx, TT]
      sC_slice <- as.matrix(sC_slice)
      0.5 * (sC_slice + t(sC_slice))
    },
    p0 = univar_theory_post_prob(q_tag),
    keep = as.integer(keep)
  )
}

univar_legacy_post_rebuild_outputs <- function(active_q_tags, days_hist_uni = 19L) {
  active_q_tags <- unique(as.character(active_q_tags))
  active_q_tags <- active_q_tags[nzchar(active_q_tags)]
  if (length(active_q_tags) < 2L) {
    stop("legacy univariate repair requires at least two fitted quantiles.", call. = FALSE)
  }

  q_probs <- vapply(active_q_tags, univar_theory_post_prob, numeric(1))
  ord <- order(q_probs)
  active_q_tags <- active_q_tags[ord]
  q_probs <- q_probs[ord]

  p <- 7L
  lambda2 <- initial_delta[6]
  Gx <- as.matrix(bdiag(GG[1:p, 1:p, TT], lambda2, diag(px)))
  Gx <- array(rep(Gx, ranges[1]), dim = c(p + ppx, p + ppx, ranges[1]))
  Gx[(p + 1L), (p + 2L:ppx), ] <- as.matrix(t(X_f)) * 1

  state_idx <- seq_len(p + ppx)
  FF_f <- matrix(FF[1:(p + ppx), 1, 1], ncol = 1)
  FF_f[p + 1L] <- 1

  keep_counts <- integer(0)
  bundle_cache <- list()
  for (q in active_q_tags) {
    info <- univar_legacy_post_bundle_info(q, state_idx = state_idx, n_keep = Inf)
    if (is.null(info)) next
    bundle_cache[[q]] <- info
    keep_counts <- c(keep_counts, info$keep)
  }
  if (length(bundle_cache) < 2L) {
    stop("legacy univariate repair could not load at least two active quantile bundles.", call. = FALSE)
  }

  n_keep <- min(min(keep_counts), suppressWarnings(as.integer(n.samp)))
  if (!is.finite(n_keep) || n_keep < 1L) {
    stop("legacy univariate repair could not resolve a positive sample count.", call. = FALSE)
  }

  L <- length(bundle_cache)
  horizon <- as.integer(ranges[1])
  days_hist_uni <- min(as.integer(days_hist_uni), as.integer(TT))

  xb_forecast <- array(NA_real_, dim = c(L, n_keep, horizon))
  y_forecast <- array(NA_real_, dim = c(L, n_keep, horizon))
  xb_hist_uni <- array(NA_real_, dim = c(L, n_keep, days_hist_uni))
  y_hist_uni <- array(NA_real_, dim = c(L, n_keep, days_hist_uni))

  compute_W_list <- function(sC_T) {
    lapply(seq_len(horizon), function(k) {
      G <- Gx[, , k]
      G %*% sC_T %*% t(G)
    })
  }
  W_lists <- lapply(bundle_cache, function(info) compute_W_list(info$sC_T * 1))

  for (row_idx in seq_along(bundle_cache)) {
    info <- bundle_cache[[row_idx]]
    theta_arr <- info$theta
    gamma_vec <- as.numeric(info$gamma[seq_len(n_keep)])
    sigma_vec <- as.numeric(info$sigma[seq_len(n_keep)])
    p00 <- as.numeric(info$p0)
    W_list <- W_lists[[row_idx]]

    for (i in seq_len(n_keep)) {
      state_k <- as.numeric(theta_arr[, TT, i])
      for (k in seq_len(horizon)) {
        innov <- mvtnorm::rmvnorm(n = 1L, sigma = W_list[[k]])
        state_k <- Gx[, , k] %*% state_k + t(innov)
        mu <- sum(FF_f * state_k)
        xb_forecast[row_idx, i, k] <- mu
        y_forecast[row_idx, i, k] <- exdqlm::rexal(1L, p00, mu, sigma_vec[[i]], gamma_vec[[i]])
      }

      for (t in seq_len(days_hist_uni)) {
        src_t <- TT - days_hist_uni + t
        mu_t <- sum(FF_f * theta_arr[, src_t, i])
        xb_hist_uni[row_idx, i, t] <- mu_t
        y_hist_uni[row_idx, i, t] <- exdqlm::rexal(1L, p00, mu_t, sigma_vec[[i]], gamma_vec[[i]])
      }
    }
  }

  for (row_idx in seq_len(L)) {
    for (t in seq_len(days_hist_uni)) {
      y_hist_uni[row_idx, , t] <- sort_keep_na(y_hist_uni[row_idx, , t])
    }
    for (k in seq_len(horizon)) {
      y_forecast[row_idx, , k] <- sort_keep_na(y_forecast[row_idx, , k])
    }
  }

  hist_cube_log1p <- profile_section(
    "univariate.transform_hist.legacy",
    post_transform_latent_log1p_cap(
      y_hist_uni,
      context = "univariate.legacy.hist",
      report_name = "univar_legacy_hist_exp_guard.txt"
    )
  )
  forecast_cube_log1p <- profile_section(
    "univariate.transform_forecast.legacy",
    post_transform_latent_log1p_cap(
      y_forecast,
      context = "univariate.legacy.forecast",
      report_name = "univar_legacy_forecast_exp_guard.txt"
    )
  )

  raw_hist_q <- post_quantile_curve_from_sample_cube(hist_cube_log1p, q_probs, context = "univar.legacy.hist_curve")
  raw_forecast_q <- post_quantile_curve_from_sample_cube(forecast_cube_log1p, q_probs, context = "univar.legacy.forecast_curve")
  crossing <- post_quantile_curve_crossing_summary(raw_forecast_q, q_probs, context = "univar.legacy.forecast_curve")

  hist_synth <- post_exdqlm_synthesize_from_sample_cube(
    sample_cube = hist_cube_log1p,
    q_probs = q_probs,
    n_samp = n_keep,
    seed = suppressWarnings(as.integer(Sys.getenv("DISC_BASE_SEED", "777"))) + 401L,
    context = "univar.legacy.synth_hist"
  )
  fore_synth <- post_exdqlm_synthesize_from_sample_cube(
    sample_cube = forecast_cube_log1p,
    q_probs = q_probs,
    n_samp = n_keep,
    seed = suppressWarnings(as.integer(Sys.getenv("DISC_BASE_SEED", "777"))) + 402L,
    context = "univar.legacy.synth_forecast"
  )

  write.csv(
    crossing$per_time,
    file = file.path(OUT_DIR, "univar_forecast_quantile_crossing_per_time.csv"),
    row.names = FALSE
  )
  write.csv(
    crossing$summary,
    file = file.path(OUT_DIR, "univar_forecast_quantile_crossing_summary.csv"),
    row.names = FALSE
  )

  saveRDS(q_probs, file = post_cache_path("univar_active_q_probs.rds"))
  saveRDS(raw_hist_q, file = post_cache_path("univar_raw_hist_quantiles_log1p.rds"))
  saveRDS(raw_forecast_q, file = post_cache_path("univar_raw_forecast_quantiles_log1p.rds"))
  saveRDS(hist_synth$anchor_quantiles, file = post_cache_path("univar_synth_hist_anchor_quantiles_log1p.rds"))
  saveRDS(fore_synth$anchor_quantiles, file = post_cache_path("univar_synth_forecast_anchor_quantiles_log1p.rds"))

  list(
    xb_hist_uni = xb_hist_uni,
    y_hist_uni = y_hist_uni,
    xb_forecast = xb_forecast,
    y_forecast = y_forecast,
    synth_hist_uni = hist_synth$draws,
    synth_hist_uni_q = hist_synth$empirical_quantiles,
    synth_hist_uni_anchor_q = hist_synth$anchor_quantiles,
    synth_f2 = fore_synth$draws,
    synth_f2_q = fore_synth$empirical_quantiles,
    synth_f2_anchor_q = fore_synth$anchor_quantiles,
    raw_hist_q = raw_hist_q,
    raw_forecast_q = raw_forecast_q,
    q_s_active = q_probs,
    active_q_tags = active_q_tags,
    n_samp = as.integer(n_keep),
    days_hist_uni = as.integer(days_hist_uni)
  )
}

has_ndlm_bundle <- function() {
  isTRUE(MODEL_RUN_NDLM_MAIN) ||
    isTRUE(MODEL_RUN_NDLM_UNIVAR) ||
    nzchar(NDLM_VAR_50) ||
    nzchar(NDLM_UNIVAR_VAR_50)
}

has_disc_w_bundle <- function() {
  isTRUE(MODEL_RUN_EXDQLM_MULTIVAR) || any(nzchar(c(
    DISC_W_VAR_05, DISC_W_VAR_20, DISC_W_VAR_35,
    DISC_W_VAR_50, DISC_W_VAR_65, DISC_W_VAR_80, DISC_W_VAR_95
  )))
}

has_univar_bundle <- function() {
  isTRUE(MODEL_RUN_EXDQLM_UNIVAR) || any(nzchar(c(
    UNI_VAR_05, UNI_VAR_20, UNI_VAR_35,
    UNI_VAR_50, UNI_VAR_65, UNI_VAR_80, UNI_VAR_95
  )))
}

univar_only_mode <- isTRUE(MODEL_RUN_EXDQLM_UNIVAR) &&
  !isTRUE(MODEL_RUN_EXDQLM_MULTIVAR) &&
  !isTRUE(MODEL_RUN_NDLM_MAIN) &&
  !isTRUE(MODEL_RUN_NDLM_UNIVAR)

univar_quantile_path_spec <- function() {
  list(
    "05" = list(path = UNI_VAR_05, source = UNI_VAR_SRC_05),
    "20" = list(path = UNI_VAR_20, source = UNI_VAR_SRC_20),
    "35" = list(path = UNI_VAR_35, source = UNI_VAR_SRC_35),
    "50" = list(path = UNI_VAR_50, source = UNI_VAR_SRC_50),
    "65" = list(path = UNI_VAR_65, source = UNI_VAR_SRC_65),
    "80" = list(path = UNI_VAR_80, source = UNI_VAR_SRC_80),
    "95" = list(path = UNI_VAR_95, source = UNI_VAR_SRC_95)
  )
}

univar_requested_fit_labels <- function(default = c("05", "20", "35", "50", "65", "80", "95")) {
  raw <- trimws(unlist(strsplit(Sys.getenv("UNIFIED_FIT_QUANTILE_LABELS", ""), ",", fixed = TRUE), use.names = FALSE))
  raw <- raw[nzchar(raw)]
  if (length(raw) == 0L) return(default)
  unique(vapply(raw, to_quantile_label, character(1)))
}

if (has_univar_bundle()) {
  legacy_repair_mode <- isTRUE(univar_only_mode) && !univar_post_use_theory_rebuild()
  q_spec <- univar_quantile_path_spec()
  requested_labels <- if (legacy_repair_mode) {
    univar_requested_fit_labels(default = c("05", "50", "95"))
  } else {
    c("05", "50", "95", "20", "35", "65", "80")
  }
  requested_labels <- requested_labels[requested_labels %in% names(q_spec)]

  loaded_requested_labels <- character(0)
  for (label in requested_labels) {
    spec <- q_spec[[label]]
    file_path <- spec$path
    source_label <- if (legacy_repair_mode) label else spec$source
    ok <- profile_section(
      sprintf("univariate.load_vars_%s", label),
      load_quantile_bundle_with_alias(
        file_path,
        target_label = label,
        source_label = source_label,
        suffix = "exAL_synth_DISC_uni"
      )
    )
    if (isTRUE(ok)) loaded_requested_labels <- c(loaded_requested_labels, label)
  }

  loaded_q_tags <- c("5", "20", "35", "50", "65", "80", "95")[
    vapply(
      c("5", "20", "35", "50", "65", "80", "95"),
      function(q) exists(sprintf("new.theta.out_%s_exAL_synth_DISC_uni", q), inherits = TRUE),
      logical(1)
    )
  ]
  loaded_q_tags_actual <- unique(vapply(loaded_requested_labels, quantile_label_tag, character(1)))
  loaded_q_tags_actual <- loaded_q_tags_actual[
    vapply(
      loaded_q_tags_actual,
      function(q) exists(sprintf("new.theta.out_%s_exAL_synth_DISC_uni", q), inherits = TRUE),
      logical(1)
    )
  ]
  if (!legacy_repair_mode && length(loaded_q_tags) > 0L && length(loaded_q_tags) < 7L) {
    alias_source_q <- if ("50" %in% loaded_q_tags) "50" else loaded_q_tags[[1L]]
    missing_q_tags <- setdiff(c("5", "20", "35", "50", "65", "80", "95"), loaded_q_tags)
    for (q in missing_q_tags) {
      alias_univar_bundle_if_missing(q, source_label = alias_source_q, suffix = "exAL_synth_DISC_uni")
    }
  }

  loaded_q_tags <- c("5", "20", "35", "50", "65", "80", "95")[
  vapply(
    c("5", "20", "35", "50", "65", "80", "95"),
    function(q) exists(sprintf("new.theta.out_%s_exAL_synth_DISC_uni", q), inherits = TRUE),
    logical(1)
  )
  ]

  n.samp_candidates <- vapply(
    c("5", "20", "35", "50", "65", "80", "95"),
    function(q) {
      obj <- get0(sprintf("samp.theta_%s_exAL_synth_DISC_uni", q), ifnotfound = NULL, inherits = TRUE)
      if (is.array(obj) && length(dim(obj)) == 3L) dim(obj)[3] else NA_real_
    },
    numeric(1)
  )
  n.samp_candidates <- n.samp_candidates[is.finite(n.samp_candidates) & n.samp_candidates > 0]
  n.samp <- if (length(n.samp_candidates) > 0L) {
    as.integer(min(2000L, min(n.samp_candidates)))
  } else {
    2000L
  }

if (isTRUE(univar_only_mode) && !univar_post_use_theory_rebuild()) {
  legacy_univar_rebuild <- univar_legacy_post_rebuild_outputs(
    active_q_tags = loaded_q_tags_actual,
    days_hist_uni = 19L
  )
  xb_hist_uni <- legacy_univar_rebuild$xb_hist_uni
  y_hist_uni <- legacy_univar_rebuild$y_hist_uni
  xb_forecast <- legacy_univar_rebuild$xb_forecast
  y_forecast <- legacy_univar_rebuild$y_forecast
  synth_hist_uni <- legacy_univar_rebuild$synth_hist_uni
  synth_hist_uni_q <- legacy_univar_rebuild$synth_hist_uni_q
  synth_f2 <- legacy_univar_rebuild$synth_f2
  synth_f2_q <- legacy_univar_rebuild$synth_f2_q
  q_s <- legacy_univar_rebuild$q_s_active
  n.samp <- legacy_univar_rebuild$n_samp
} else {

dim(new.theta.out_50_exAL_synth_DISC_uni$exps)
TTT_temp <- dim(new.theta.out_50_exAL_synth_DISC_uni$exps)[2]
TT
diff <- TT-TTT_temp+1
length(timestamps)
diff <- 0

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(4, 0, 0, 0))
time_cuts <- resolve_time_cuts(
  timestamps = timestamps,
  cutoff_date = CUTOFF_DATE,
  context = "30_univariate_and_misc.univariate_demo"
)
dates_ts_usgs <- timestamps
# idx <- time_cuts[3]:time_cuts[4]
idx <- safe_time_index(TT - 1000 - diff, TT - diff - 500, TT, context = "30_univariate.demo_window")
# TTT_temp <- dim(new.theta.out_50_exAL_synth_DISC_uni$exps)[2]
# idx <- (TTT_temp-200):(TTT_temp)
percentiles <- c(0.025, 0.5, 0.975)

plot(idx, (new.theta.out_50_exAL_synth_DISC_uni$exps[1,idx])*0, ylim = c(-2, 2),  type="l", lwd = 1,
        main = "Quantile Dynamics     -    2017-2019",
        xlab = " ", ylab = "log-flow", xaxt = "n")

ac <- 0.5        
lines(idx, new.theta.out_5_exAL_synth_DISC_uni$exps[1,idx], col = 'darkred', lwd = ac)
lines(idx, new.theta.out_20_exAL_synth_DISC_uni$exps[1,idx], col = 'purple', lwd = ac)
lines(idx, new.theta.out_35_exAL_synth_DISC_uni$exps[1,idx], col = 'purple', lwd = ac)
lines(idx, new.theta.out_50_exAL_synth_DISC_uni$exps[1,idx], col = 'forestgreen', lwd = ac)
lines(idx, new.theta.out_65_exAL_synth_DISC_uni$exps[1,idx], col = 'purple', lwd = ac)
lines(idx, new.theta.out_80_exAL_synth_DISC_uni$exps[1,idx], col = 'purple', lwd = ac)
lines(idx, new.theta.out_95_exAL_synth_DISC_uni$exps[1,idx], col = 'darkblue', lwd = ac)

lines(idx, Y[1,idx+diff], col = 'black', lwd = 0.1)
points(idx, Y[1,idx+diff], col = 'gray')
points(idx, Y[1,idx+diff], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)

plot(ppt_data$time,ppt_data$ppt, type = 'line')
plot(principal_components_df$time,principal_components_df$Static_PCA, type = 'line')

# start_date_idx <- which(ppt_data$time == '2022-12-26')
# end_date_idx <- which(ppt_data$time == '2022-12-26') + ranges[1]
# X_ppt_f <- ppt_data[start_date_idx:end_date_idx,c('ppt','time')]

# start_date_idx <- which(principal_components_df$time == '2022-12-26')
# end_date_idx <- which(principal_components_df$time == '2022-12-26') + ranges[1]
# X_pca_f <- principal_components_df[start_date_idx:end_date_idx,c('Static_PCA','time')]

# X_ppt_pca_f <- merge(X_ppt_f, X_pca_f, by = "time")
# X_ppt_pca_soil_f <- merge(X_ppt_pca_f, soil_moisture_data, by = "time")
# X_f <- cbind(X_ppt_pca_soil_f$ppt,X_ppt_pca_soil_f$soil)

# covariates2 <- apply(X_f, 2, standardize)
# for(i in 1:dim(covariates2)[2] ){
#     covariates2[,i] <- covariates2[,i]-min(covariates2[,i])+1
# }
# covariates2 <- log(log(covariates2+1))

# plot.ts(c(X[(TT-100):(TT),1],X_f[,1]))
# plot.ts(c(covariates[(TT-100):(TT),1],covariates2[,1]))

# X_ppt_pca_soil_f$Static_PCA
# rep(1, ranges[1])

# ###########################################################################################
# ####################################### Forecasts ######################################### 
# ###########################################################################################
# nws_forecast <- read.csv('SOURCE_WORKFLOW_ROOT/nws_forecast.csv')
# OBSOLETE: legacy log-log forecast transform removed; use the scale-contract bridge.
# num_ens_nws <- dim(nws_forecast)[2]-1

# glofas_forecast <- read.csv('SOURCE_WORKFLOW_ROOT/weighted_time_series.csv')
# glofas_forecast$target_date <- as.Date(glofas_forecast$target_date)
# specific_date <- as.Date("2022-12-26")
# glofas_forecast <- glofas_forecast[glofas_forecast$target_date >= specific_date, ]
# OBSOLETE: legacy log-log forecast transform removed; use the scale-contract bridge.

# num_ens_glofas <- dim(glofas_forecast)[2]-1

# ensembles <- list(glofas_forecast[,-c(1)], nws_forecast[,-c(1)])
# J <- length(ensembles)
# num_mem <- rep(NA_real_, J)
# ranges <- rep(NA_real_, J)
# for(j in 1:J){
#   num_mem[j] <- dim(ensembles[[j]])[2]
#   ranges[j] <- dim(ensembles[[j]])[1]
# }

# row_means_list <- vector("list", J + 1)
# row_means_list[[1]] <- rep(NA_real_, ranges[1])
# for (j in 1:J) {
#   row_means_list[[j + 1]] <- rep(NA_real_, ranges[1])
#   row_means_list[[j + 1]][1:ranges[j]] <- rowMeans(ensembles[[j]])
# }
# mean_forecast <- do.call(rbind, row_means_list)

# ###########################################################################################
# ####################################### Covs, Retros, More ################################ 
# ###########################################################################################

# #########
# ## PPT ##
# #########
# file_path <- "SOURCE_WORKFLOW_ROOT/prism_precipitation_santa_cruz_1987_2023.csv"
# ppt_data <- read_csv(file_path, show_col_types = FALSE)
# ppt_data$Date <- as.Date(ppt_data$Date)
# colnames(ppt_data) <- c('time','ppt')
# X_ppt <- ppt_data[ppt_data$time <= '2022-12-25',]

# start_date_idx <- which(ppt_data$time == '2022-12-26')
# end_date_idx <- which(ppt_data$time == '2022-12-26') + ranges[1]
# X_ppt_f <- ppt_data[start_date_idx:end_date_idx,c('ppt','time')]

# ##########
# ## SOIL ##
# ##########
# csv_file_path <- "SOURCE_WORKFLOW_ROOT/soil_moisture_data/soil_moisture_big_trees_daily_avg_1987_2023.csv"
# soil_moisture_data <- read.csv(csv_file_path)
# soil_moisture_data$Date <- as.Date(soil_moisture_data$Date)
# colnames(soil_moisture_data) <- c('time','soil')
# X_soil <- soil_moisture_data[soil_moisture_data$time <= '2022-12-25',]

# start_date_idx <- which(soil_moisture_data$time == '2022-12-26')
# end_date_idx <- which(soil_moisture_data$time == '2022-12-26') + ranges[1]
# X_soil_f <- soil_moisture_data[start_date_idx:end_date_idx,c('soil','time')]

# #########
# ## PCA ##
# #########
# components_file_path <- "SOURCE_WORKFLOW_ROOT/pca.csv"
# principal_components_df <- read_csv(components_file_path, show_col_types = FALSE)
# colnames(principal_components_df) <- c('time','Static_PCA')
# X_pca <- principal_components_df[principal_components_df$time <= '2022-12-25',]

# start_date_idx <- which(principal_components_df$time == '2022-12-26')
# end_date_idx <- which(principal_components_df$time == '2022-12-26') + ranges[1]
# X_pca_f <- principal_components_df[start_date_idx:end_date_idx,c('Static_PCA','time')]

# ###########
# ## Merge ##
# ###########
# X <- merge(X_ppt, X_soil, by = "time")
# X <- merge(X, X_pca, by = "time")

# X_f <- merge(X_ppt_f, X_soil_f, by = "time")
# X_f <- merge(X_f, X_pca_f, by = "time")

# #############
# ## Retrosp ##
# #############
# data_path <- "SOURCE_WORKFLOW_ROOT/retros_2022-12-25.csv"
# streamflow_data <- read_csv(data_path, show_col_types = FALSE)
# time_series_matrix <- as.matrix(streamflow_data[, c('USGS', 'GloFAS', 'NWS3.0')])
# timestamps <- as.Date(streamflow_data$Date)
# Y_usgs <- data.frame(time = timestamps, time_series_matrix)
# all_data <- merge(X, Y_usgs, by = "time")
# Y <- t(as.matrix(all_data[, c('USGS', 'GloFAS', 'NWS3.0')]))
# OBSOLETE: legacy log-log response transform removed; use the scale-contract bridge.
# TT <- dim(Y)[2]
# J <- dim(Y)[1] - 1
# timestamps <- all_data[, 'time']

# #############################
# ## Add Constant at the end ##
# #############################
# X <- cbind(all_data[,c('ppt','soil','Static_PCA')], rep(1, TT))
# X_f <- cbind(X_f[,-1], rep(1, ranges[1]))

# #####################
# ## STANDARDIZATION ##
# #####################
# sd_ppt  <- sd(X[,1]) 
# sd_soil <- sd(X[,2]) 
# sd_pca  <- sd(X[,3]) 

# X[,1] <- X[,1]/sd_ppt
# X[,2] <- X[,2]/sd_soil
# X[,3] <- X[,3]/sd_pca

# X_f[,1] <- X_f[,1]/sd_ppt
# X_f[,2] <- X_f[,2]/sd_soil
# X_f[,3] <- X_f[,3]/sd_pca

a <- 30
for(i in 1:8){
plot.ts(c(X[(TT-a):TT,i],X_f[,i]))
abline(v=a, col = 'darkred')
}

sm_T95 <- matrix(new.theta.out_95_exAL_synth_DISC_uni$sm[,TT], ncol = 1)
sC_T95 <- new.theta.out_95_exAL_synth_DISC_uni$sC[,,TT]
sm_T50 <- matrix(new.theta.out_50_exAL_synth_DISC_uni$sm[,TT], ncol = 1)
sC_T50 <- new.theta.out_50_exAL_synth_DISC_uni$sC[,,TT]
sm_T5 <- matrix(new.theta.out_5_exAL_synth_DISC_uni$sm[,TT], ncol = 1)
sC_T5 <- new.theta.out_5_exAL_synth_DISC_uni$sC[,,TT]
cbind(sm_T95[8:18],sm_T50[8:18],sm_T5[8:18])

sm_T95 <- new.theta.out_95_exAL_synth_DISC_uni$sm[,] 
sm_T50 <- new.theta.out_50_exAL_synth_DISC_uni$sm[,]  
sm_T5  <- new.theta.out_5_exAL_synth_DISC_uni$sm[,]  

plot.ts(sm_T95[11,], ylim = c(-0.005,0.005) ) 
lines(sm_T50[11,])
lines(sm_T5[11,])

plot.ts(sm_T95[10,], ylim = c(0.12,0.65) )
lines(sm_T50[10,])
lines(sm_T5[10,])

plot.ts(sm_T95[9,], ylim = c(0.1,0.15) )
lines(sm_T50[9,])
lines(sm_T5[9,])


sm_T95 <- matrix(new.theta.out_95_exAL_synth_DISC_uni$sm[,TT], ncol = 1)
sC_T95 <- new.theta.out_95_exAL_synth_DISC_uni$sC[,,TT]
sm_T50 <- matrix(new.theta.out_50_exAL_synth_DISC_uni$sm[,TT], ncol = 1)
sC_T50 <- new.theta.out_50_exAL_synth_DISC_uni$sC[,,TT]
sm_T5 <- matrix(new.theta.out_5_exAL_synth_DISC_uni$sm[,TT], ncol = 1)
sC_T5 <- new.theta.out_5_exAL_synth_DISC_uni$sC[,,TT]
cbind(sm_T95[8:18],sm_T50[8:18],sm_T5[8:18])

p <- 7

print(initial_delta)

lambda2 <- initial_delta[6]
Gx <- as.matrix(bdiag(GG[1:p,1:p,TT],lambda2, diag(px)))


Gx <- array(rep(Gx, ranges[1]), dim = c(p+ppx, p+ppx, ranges[1]))

Gx[(p+1), (p+2:ppx), ] <- as.matrix(t(X_f)) * 1

print(dim(Gx))
print(c(p,ppx))
print(length(p+2:ppx))

print(dim(Gx[(p+1), (p+2:ppx), ]))
print(dim(as.matrix(t(X_f)) * 1))

c <- (1)^2
state_idx <- seq_len(p + ppx)
###############################################
sm_T <- matrix(new.theta.out_95_exAL_synth_DISC_uni$sm[state_idx,TT], ncol = 1)
sC_T <- new.theta.out_95_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT]*c

FF_f <- matrix(FF[1:(p+ppx),1,1], ncol = 1)
FF_f[p+1] <- 1 
y_forecast <- array(NA_real_, dim = c(1, ranges[1]))
sm_k <- array(NA_real_, dim = c(p+ppx, 1, ranges[1]))
sm_k[,1,1] <- sm_T
y_forecast[1,1] <- sum(t(FF_f)*sm_k[,1,1])
for(k in 2:ranges[1]){
    sm_k[,1,k] <- Gx[,,k] %*% sm_k[,1,k-1]
    y_forecast[1,k] <- sum(t(FF_f)*sm_k[,1,k])
}

plot.ts(y_forecast[1,], ylim = c(-1,4), col = 'darkblue', lwd = 2)
truth_log <- log(San_Lorenzo_Daily_USGS_R$data0[San_Lorenzo_Daily_USGS_R$Date >= FORECAST_START_DATE][1:ranges[1]])
lines(truth_log, col = 'black')
points(truth_log, col = 'black')

###############################################
sm_T <- matrix(new.theta.out_50_exAL_synth_DISC_uni$sm[state_idx,TT], ncol = 1)
sC_T <- new.theta.out_50_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT]*c

FF_f <- matrix(FF[1:(p+ppx),1,1], ncol = 1)
FF_f[p+1] <- 1 
y_forecast <- array(NA_real_, dim = c(1, ranges[1]))
sm_k <- array(NA_real_, dim = c(p+ppx, 1, ranges[1]))
sm_k[,1,1] <- sm_T
y_forecast[1,1] <- sum(t(FF_f)*sm_k[,1,1])
for(k in 2:ranges[1]){
    sm_k[,1,k] <- Gx[,,k] %*% sm_k[,1,k-1]
    y_forecast[1,k] <- sum(t(FF_f)*sm_k[,1,k])
}
lines(y_forecast[1,], col = 'forestgreen', lwd = 2)


###############################################
sm_T <- matrix(new.theta.out_5_exAL_synth_DISC_uni$sm[state_idx,TT], ncol = 1)
sC_T <- new.theta.out_5_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT]*c

FF_f <- matrix(FF[1:(p+ppx),1,1], ncol = 1)
FF_f[p+1] <- 1 
y_forecast <- array(NA_real_, dim = c(1, ranges[1]))
sm_k <- array(NA_real_, dim = c(p+ppx, 1, ranges[1]))
sm_k[,1,1] <- sm_T
y_forecast[1,1] <- sum(t(FF_f)*sm_k[,1,1])
for(k in 2:ranges[1]){
    sm_k[,1,k] <- Gx[,,k] %*% sm_k[,1,k-1]
    y_forecast[1,k] <- sum(t(FF_f)*sm_k[,1,k])
}
lines(y_forecast[1,], col = 'darkred', lwd = 2)


truth_log <- log(San_Lorenzo_Daily_USGS_R$data0[San_Lorenzo_Daily_USGS_R$Date >= FORECAST_START_DATE][1:ranges[1]])
plot.ts(truth_log, col = 'black', ylim = c(-1,4))
points(truth_log, col = 'black')

###############################################
sm_T <- matrix(samp.theta_95_exAL_synth_DISC_uni[state_idx,TT,], nrow = length(state_idx))
sC_T <- new.theta.out_95_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT]*c

FF_f <- matrix(FF[1:(p+ppx),1,1], ncol = 1)
FF_f[p+1] <- 1 
y_forecast <- array(NA_real_, dim = c(1, ranges[1], n.samp))

for(i in 1:n.samp){
    sm_k <- array(NA_real_, dim = c(p+ppx, 1, ranges[1]))
    sm_k[,1,1] <- sm_T[,i]
    y_forecast[1,1,i] <- sum(t(FF_f)*sm_k[,1,1])
    for(k in 2:ranges[1]){
        sm_k[,1,k] <- Gx[,,k] %*% sm_k[,1,k-1]
        y_forecast[1,k,i] <- sum(t(FF_f)*sm_k[,1,k])
    }
    lines(y_forecast[1,,i], ylim = c(-1,4), col = 'lightblue', lwd = 0.1)
}

###############################################
sm_T <- matrix(samp.theta_50_exAL_synth_DISC_uni[state_idx,TT,], nrow = length(state_idx))
sC_T <- new.theta.out_50_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT]*c

FF_f <- matrix(FF[1:(p+ppx),1,1], ncol = 1)
FF_f[p+1] <- 1 
y_forecast <- array(NA_real_, dim = c(1, ranges[1], n.samp))

for(i in 1:n.samp){
    sm_k <- array(NA_real_, dim = c(p+ppx, 1, ranges[1]))
    sm_k[,1,1] <- sm_T[,i]
    y_forecast[1,1,i] <- sum(t(FF_f)*sm_k[,1,1])
    for(k in 2:ranges[1]){
        sm_k[,1,k] <- Gx[,,k] %*% sm_k[,1,k-1]
        y_forecast[1,k,i] <- sum(t(FF_f)*sm_k[,1,k])
    }
    lines(y_forecast[1,,i], col = 'lightgreen', lwd = 0.1)
}

###############################################
sm_T <- matrix(samp.theta_5_exAL_synth_DISC_uni[state_idx,TT,], nrow = length(state_idx))
sC_T <- new.theta.out_5_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT]*c

FF_f <- matrix(FF[1:(p+ppx),1,1], ncol = 1)
FF_f[p+1] <- 1 
y_forecast <- array(NA_real_, dim = c(1, ranges[1], n.samp))

for(i in 1:n.samp){
    sm_k <- array(NA_real_, dim = c(p+ppx, 1, ranges[1]))
    sm_k[,1,1] <- sm_T[,i]
    y_forecast[1,1,i] <- sum(t(FF_f)*sm_k[,1,1])
    for(k in 2:ranges[1]){
        sm_k[,1,k] <- Gx[,,k] %*% sm_k[,1,k-1]
        y_forecast[1,k,i] <- sum(t(FF_f)*sm_k[,1,k])
    }
    lines(y_forecast[1,,i], col = 'red', lwd = 0.1)
}


# sm_{T+1} <- Gx_{T+1} %*% sm_T + N(0,W_{T+1}) 
# y_{T+1}  <- F_{T+1} %*% sm_{T+1} + exAL_p0(V,0,gamma) 
p <- 7

xb_forecast <- array(NA_real_,c(7,n.samp,ranges[1]))
y_forecast <- array(NA_real_,c(7,n.samp,ranges[1]))

FF_f <- matrix(FF[1:(p+ppx),1,1], ncol = 1) 
FF_f[p+1] <- 1 

# Precompute W_k = Gx_k %*% sC_T %*% t(Gx_k) for each quantile and forecast time k.
# This preserves exact behavior (same RNG calls/order) but avoids repeated matrix multiplications inside loops.
compute_W_list <- function(sC_T) {
  lapply(seq_len(ranges[1]), function(k) {
    G <- Gx[,,k]
    G %*% sC_T %*% t(G)
  })
}

sC_5_T  <- new.theta.out_5_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT]  * c
sC_20_T <- new.theta.out_20_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT] * c
sC_35_T <- new.theta.out_35_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT] * c
sC_50_T <- new.theta.out_50_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT] * c
sC_65_T <- new.theta.out_65_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT] * c
sC_80_T <- new.theta.out_80_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT] * c
sC_95_T <- new.theta.out_95_exAL_synth_DISC_uni$sC[state_idx, state_idx, TT] * c

W_list_5  <- compute_W_list(sC_5_T)
W_list_20 <- compute_W_list(sC_20_T)
W_list_35 <- compute_W_list(sC_35_T)
W_list_50 <- compute_W_list(sC_50_T)
W_list_65 <- compute_W_list(sC_65_T)
W_list_80 <- compute_W_list(sC_80_T)
W_list_95 <- compute_W_list(sC_95_T)


for(i in 1:n.samp){
    sm_k1 <- samp.theta_5_exAL_synth_DISC_uni[state_idx,TT,i]
    e <- rmvnorm(n = 1, sigma = W_list_5[[1]])
    sm_k1 <- Gx[,,1] %*% sm_k1 +t(e)
    xb_forecast[1,i,1] <- sum((FF_f)*sm_k1)
    
    sm_k2 <- samp.theta_20_exAL_synth_DISC_uni[state_idx,TT,i]
    e <- rmvnorm(n = 1, sigma = W_list_20[[1]])
    sm_k2 <- Gx[,,1] %*% sm_k2 +t(e)
    xb_forecast[2,i,1] <- sum((FF_f)*sm_k2)

    sm_k3 <- samp.theta_35_exAL_synth_DISC_uni[state_idx,TT,i]
    e <- rmvnorm(n = 1, sigma = W_list_35[[1]])
    sm_k3 <- Gx[,,1] %*% sm_k3 +t(e)
    xb_forecast[3,i,1] <- sum((FF_f)*sm_k3)
    
    sm_k4 <- samp.theta_50_exAL_synth_DISC_uni[state_idx,TT,i]
    e <- rmvnorm(n = 1, sigma = W_list_50[[1]])
    sm_k4 <- Gx[,,1] %*% sm_k4 +t(e)
    xb_forecast[4,i,1] <- sum((FF_f)*sm_k4)
    
    sm_k5 <- samp.theta_65_exAL_synth_DISC_uni[state_idx,TT,i]
    e <- rmvnorm(n = 1, sigma = W_list_65[[1]])
    sm_k5 <- Gx[,,1] %*% sm_k5 +t(e)
    xb_forecast[5,i,1] <- sum((FF_f)*sm_k5)
    
    sm_k6 <- samp.theta_80_exAL_synth_DISC_uni[state_idx,TT,i]
    e <- rmvnorm(n = 1, sigma = W_list_80[[1]])
    sm_k6 <- Gx[,,1] %*% sm_k6 +t(e)
    xb_forecast[6,i,1] <- sum((FF_f)*sm_k6)

    sm_k7 <- samp.theta_95_exAL_synth_DISC_uni[state_idx,TT,i]
    e <- rmvnorm(n = 1, sigma = W_list_95[[1]])
    sm_k7 <- Gx[,,1] %*% sm_k7 +t(e)
    xb_forecast[7,i,1] <- sum((FF_f)*sm_k7)

    # Cache per-sample params once (used for all k)
    gamma_95 <- samp.gamma_95_exAL_synth_DISC_uni[1,i]
    sigma_95 <- samp.sigma_95_exAL_synth_DISC_uni[1,i]
    gamma_80 <- samp.gamma_80_exAL_synth_DISC_uni[1,i]
    sigma_80 <- samp.sigma_80_exAL_synth_DISC_uni[1,i]
    gamma_65 <- samp.gamma_65_exAL_synth_DISC_uni[1,i]
    sigma_65 <- samp.sigma_65_exAL_synth_DISC_uni[1,i]
    gamma_50 <- samp.gamma_50_exAL_synth_DISC_uni[1,i]
    sigma_50 <- samp.sigma_50_exAL_synth_DISC_uni[1,i]
    gamma_35 <- samp.gamma_35_exAL_synth_DISC_uni[1,i]
    sigma_35 <- samp.sigma_35_exAL_synth_DISC_uni[1,i]
    gamma_20 <- samp.gamma_20_exAL_synth_DISC_uni[1,i]
    sigma_20 <- samp.sigma_20_exAL_synth_DISC_uni[1,i]
    gamma_5 <- samp.gamma_5_exAL_synth_DISC_uni[1,i]
    sigma_5 <- samp.sigma_5_exAL_synth_DISC_uni[1,i]
    
    p00 <- 0.95
    mu <- xb_forecast[7,i,1]
    y_forecast[7,i,1] <- rexal(1, p00, mu, sigma_95, gamma_95) 

    p00 <- 0.8
    mu <- xb_forecast[6,i,1]
    y_forecast[6,i,1] <- rexal(1, p00, mu, sigma_80, gamma_80) 

    p00 <- 0.65
    mu <- xb_forecast[5,i,1]
    y_forecast[5,i,1] <- rexal(1, p00, mu, sigma_65, gamma_65) 

    p00 <- 0.5
    mu <- xb_forecast[4,i,1]
    y_forecast[4,i,1] <- rexal(1, p00, mu, sigma_50, gamma_50) 

    p00 <- 0.35
    mu <- xb_forecast[3,i,1]
    y_forecast[3,i,1] <- rexal(1, p00, mu, sigma_35, gamma_35) 

    p00 <- 0.20
    mu <- xb_forecast[2,i,1]
    y_forecast[2,i,1] <- rexal(1, p00, mu, sigma_20, gamma_20) 

    p00 <- 0.05
    mu <- xb_forecast[1,i,1]
    y_forecast[1,i,1] <- rexal(1, p00, mu, sigma_5, gamma_5) 
        
    for(k in 2:ranges[1]){
        e <- rmvnorm(n = 1, sigma = W_list_5[[k]])
        sm_k1 <- Gx[,,k] %*% sm_k1 +t(e)
        xb_forecast[1,i,k] <- sum((FF_f)*sm_k1)

        e <- rmvnorm(n = 1, sigma = W_list_20[[k]])
        sm_k2 <- Gx[,,k] %*% sm_k2 +t(e)
        xb_forecast[2,i,k] <- sum((FF_f)*sm_k2)

        e <- rmvnorm(n = 1, sigma = W_list_35[[k]])
        sm_k3 <- Gx[,,k] %*% sm_k3 +t(e)
        xb_forecast[3,i,k] <- sum((FF_f)*sm_k3)

        e <- rmvnorm(n = 1, sigma = W_list_50[[k]])
        sm_k4 <- Gx[,,k] %*% sm_k4 +t(e)
        xb_forecast[4,i,k] <- sum((FF_f)*sm_k4)

        e <- rmvnorm(n = 1, sigma = W_list_65[[k]])
        sm_k5 <- Gx[,,k] %*% sm_k5 +t(e)
        xb_forecast[5,i,k] <- sum((FF_f)*sm_k5)

        e <- rmvnorm(n = 1, sigma = W_list_80[[k]])
        sm_k6 <- Gx[,,k] %*% sm_k6 +t(e)
        xb_forecast[6,i,k] <- sum((FF_f)*sm_k6)
        
        e <- rmvnorm(n = 1, sigma = W_list_95[[k]])
        sm_k7 <- Gx[,,k] %*% sm_k7 +t(e)
        xb_forecast[7,i,k] <- sum((FF_f)*sm_k7)

        p00 <- 0.95
        mu <- xb_forecast[7,i,k]
        y_forecast[7,i,k] <- rexal(1, p00, mu, sigma_95, gamma_95) 

        p00 <- 0.8
        mu <- xb_forecast[6,i,k]
        y_forecast[6,i,k] <- rexal(1, p00, mu, sigma_80, gamma_80) 

        p00 <- 0.65
        mu <- xb_forecast[5,i,k]
        y_forecast[5,i,k] <- rexal(1, p00, mu, sigma_65, gamma_65) 

        p00 <- 0.5
        mu <- xb_forecast[4,i,k]
        y_forecast[4,i,k] <- rexal(1, p00, mu, sigma_50, gamma_50) 

        p00 <- 0.35
        mu <- xb_forecast[3,i,k]
        y_forecast[3,i,k] <- rexal(1, p00, mu, sigma_35, gamma_35) 

        p00 <- 0.20
        mu <- xb_forecast[2,i,k]
        y_forecast[2,i,k] <- rexal(1, p00, mu, sigma_20, gamma_20) 

        p00 <- 0.05
        mu <- xb_forecast[1,i,k]
        y_forecast[1,i,k] <- rexal(1, p00, mu, sigma_5, gamma_5) 


    }
}

days_hist_uni <- 19
xb_hist_uni <- array(NA_real_,c(7,n.samp,days_hist_uni))
y_hist_uni <- array(NA_real_,c(7,n.samp,days_hist_uni))
FF_hist_uni <- matrix(FF[1:(p+ppx),1,1], ncol = 1) 
FF_hist_uni[p+1] <- 1 

for(i in 1:n.samp){
    for(t in (TT-days_hist_uni+1):TT){
        tt <- ( t -(TT-days_hist_uni+1) + 1 )
        xb_hist_uni[7,i,tt] <- sum((FF_hist_uni)*samp.theta_95_exAL_synth_DISC_uni[state_idx,t,i])
        gamma <- samp.gamma_95_exAL_synth_DISC_uni[1,i]
        sigma <- samp.sigma_95_exAL_synth_DISC_uni[1,i]
        p00 <- 0.95
        mu  <- xb_hist_uni[7,i,tt]
        y_hist_uni[7,i,tt] <- rexal(1, p00, mu, sigma, gamma)

        xb_hist_uni[6,i,tt] <- sum((FF_hist_uni)*samp.theta_80_exAL_synth_DISC_uni[state_idx,t,i])
        gamma <- samp.gamma_80_exAL_synth_DISC_uni[1,i]
        sigma <- samp.sigma_80_exAL_synth_DISC_uni[1,i]
        p00 <- 0.80
        mu  <- xb_hist_uni[6,i,tt]
        y_hist_uni[6,i,tt] <- rexal(1, p00, mu, sigma, gamma)

        xb_hist_uni[5,i,tt] <- sum((FF_hist_uni)*samp.theta_65_exAL_synth_DISC_uni[state_idx,t,i])
        gamma <- samp.gamma_65_exAL_synth_DISC_uni[1,i]
        sigma <- samp.sigma_65_exAL_synth_DISC_uni[1,i]
        p00 <- 0.65
        mu  <- xb_hist_uni[5,i,tt]
        y_hist_uni[5,i,tt] <- rexal(1, p00, mu, sigma, gamma)

        xb_hist_uni[4,i,tt] <- sum((FF_hist_uni)*samp.theta_50_exAL_synth_DISC_uni[state_idx,t,i])
        gamma <- samp.gamma_50_exAL_synth_DISC_uni[1,i]
        sigma <- samp.sigma_50_exAL_synth_DISC_uni[1,i]
        p00 <- 0.50
        mu  <- xb_hist_uni[4,i,tt]
        y_hist_uni[4,i,tt] <- rexal(1, p00, mu, sigma, gamma)

        xb_hist_uni[3,i,tt] <- sum((FF_hist_uni)*samp.theta_35_exAL_synth_DISC_uni[state_idx,t,i])
        gamma <- samp.gamma_35_exAL_synth_DISC_uni[1,i]
        sigma <- samp.sigma_35_exAL_synth_DISC_uni[1,i]
        p00 <- 0.35
        mu  <- xb_hist_uni[3,i,tt]
        y_hist_uni[3,i,tt] <- rexal(1, p00, mu, sigma, gamma)

        xb_hist_uni[2,i,tt] <- sum((FF_hist_uni)*samp.theta_20_exAL_synth_DISC_uni[state_idx,t,i])
        gamma <- samp.gamma_20_exAL_synth_DISC_uni[1,i]
        sigma <- samp.sigma_20_exAL_synth_DISC_uni[1,i]
        p00 <- 0.20
        mu  <- xb_hist_uni[2,i,tt]
        y_hist_uni[2,i,tt] <- rexal(1, p00, mu, sigma, gamma)

        xb_hist_uni[1,i,tt] <- sum((FF_hist_uni)*samp.theta_5_exAL_synth_DISC_uni[state_idx,t,i])
        gamma <- samp.gamma_5_exAL_synth_DISC_uni[1,i]
        sigma <- samp.sigma_5_exAL_synth_DISC_uni[1,i]
        p00 <- 0.05
        mu  <- xb_hist_uni[1,i,tt]
        y_hist_uni[1,i,tt] <- rexal(1, p00, mu, sigma, gamma)
    }   
}

y_reps_5 <- y_hist_uni[1,,]
y_reps_20 <- y_hist_uni[2,,]
y_reps_35 <- y_hist_uni[3,,]
y_reps_50 <- y_hist_uni[4,,]
y_reps_65 <- y_hist_uni[5,,]
y_reps_80 <- y_hist_uni[6,,]
y_reps_95 <- y_hist_uni[7,,]
for(t in 1:days_hist_uni){
    y_reps_5[,t] <- sort(y_reps_5[,t])
    y_reps_20[,t] <- sort(y_reps_20[,t])
    y_reps_35[,t] <- sort(y_reps_35[,t])
    y_reps_50[,t] <- sort(y_reps_50[,t])
    y_reps_65[,t] <- sort(y_reps_65[,t])
    y_reps_80[,t] <- sort(y_reps_80[,t])
    y_reps_95[,t] <- sort(y_reps_95[,t])
}

y_hist_uni[1,,] <- y_reps_5  
y_hist_uni[2,,] <- y_reps_20  
y_hist_uni[3,,] <- y_reps_35  
y_hist_uni[4,,] <- y_reps_50  
y_hist_uni[5,,] <- y_reps_65  
y_hist_uni[6,,] <- y_reps_80  
y_hist_uni[7,,] <- y_reps_95 

# Save in run-scoped cache
saveRDS(y_hist_uni, file = post_cache_path("y_hist_uni.rds"))

y_reps_hist_uni <- y_hist_uni

q_s    <- c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95)
n.q     <- length(q_s)
n.samp  <- n.samp
n.times <- ranges[1]

hist_cube_log1p_full <- profile_section(
  "univariate.transform_hist.full",
  post_transform_latent_log1p_cap(
    y_reps_hist_uni,
    context = "univariate.full.hist",
    report_name = "univar_full_hist_exp_guard.txt"
  )
)
synth_hist_uni <- profile_section("univariate.synthesize_hist", synthesize_samples(hist_cube_log1p_full, q_s))
dim(synth_hist_uni)

synth_hist_uni_q <- colQuantiles(synth_hist_uni, probs = q_s, type = 8)
synth_hist_uni_q <- t(synth_hist_uni_q)
dim(synth_hist_uni_q)

for (t in 1:days_hist_uni) {
    synth_hist_uni[,t] <- sort(synth_hist_uni[,t])
}

y_reps_f_5 <- y_forecast[1,,]
y_reps_f_20 <- y_forecast[2,,]
y_reps_f_35 <- y_forecast[3,,]
y_reps_f_50 <- y_forecast[4,,]
y_reps_f_65 <- y_forecast[5,,]
y_reps_f_80 <- y_forecast[6,,]
y_reps_f_95 <- y_forecast[7,,]
for(t in 1:ranges[1]){
    y_reps_f_5[,t] <- sort(y_reps_f_5[,t])
    y_reps_f_20[,t] <- sort(y_reps_f_20[,t])
    y_reps_f_35[,t] <- sort(y_reps_f_35[,t])
    y_reps_f_50[,t] <- sort(y_reps_f_50[,t])
    y_reps_f_65[,t] <- sort(y_reps_f_65[,t])
    y_reps_f_80[,t] <- sort(y_reps_f_80[,t])
    y_reps_f_95[,t] <- sort(y_reps_f_95[,t])
}

y_forecast[1,,] <- y_reps_f_5  
y_forecast[2,,] <- y_reps_f_20  
y_forecast[3,,] <- y_reps_f_35  
y_forecast[4,,] <- y_reps_f_50  
y_forecast[5,,] <- y_reps_f_65  
y_forecast[6,,] <- y_reps_f_80  
y_forecast[7,,] <- y_reps_f_95 

# Save in run-scoped cache
saveRDS(y_forecast, file = post_cache_path("y_forecast_uni.rds"))

y_reps_uni <- y_forecast

crossing_legacy <- post_quantile_crossing_summary(
  sample_cube = y_forecast,
  q_probs = q_s,
  context = "univariate.legacy_forecast"
)
write.csv(
  crossing_legacy$per_time,
  file = file.path(OUT_DIR, "univar_forecast_quantile_crossing_per_time.csv"),
  row.names = FALSE
)
write.csv(
  crossing_legacy$summary,
  file = file.path(OUT_DIR, "univar_forecast_quantile_crossing_summary.csv"),
  row.names = FALSE
)

q_s    <- c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95)
n.q     <- length(q_s)
n.samp  <- n.samp
n.times <- ranges[1]

forecast_cube_log1p_full <- profile_section(
  "univariate.transform_forecast.full",
  post_transform_latent_log1p_cap(
    y_reps_uni,
    context = "univariate.full.forecast",
    report_name = "univar_full_forecast_exp_guard.txt"
  )
)
synth_f2 <- profile_section("univariate.synthesize_forecast", synthesize_samples(forecast_cube_log1p_full, q_s))
dim(synth_f2)

synth_f2_q <- colQuantiles(synth_f2, probs = q_s, type = 8)
synth_f2_q <- t(synth_f2_q)
dim(synth_f2_q)

for (t in 1:ranges[1]) {
    synth_f2[,t] <- sort(synth_f2[,t])
}

plot.ts(rep(0,ranges[1]), ylim = c(0,12))

SL <- San_Lorenzo_Daily_USGS_R[San_Lorenzo_Daily_USGS_R$Date >= timestamps[1] , ]
SL <- SL[(TT+1):(TT+ranges[1]) , ]

for (s in 1:dim(synth_f2)[1]) {
   lines(synth_f2[s,], col = 'pink', lwd = 0.5)
}

points(SL$data0, lwd = 0.8)

for (i in 1:n.q) {
   lines(synth_f2_q[i,], col = 'gray', lwd = 2)
}

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_col_quantiles_t(
  post_transform_latent_log1p_cap(xb_forecast[7, , ], context = "univariate.xb_forecast.q95"),
  probs = c(0.025, 0.5, 0.975)
)
lines(result[1,], col = 'blue', lty = 2, lwd = 1)
lines(result[2,], col = 'darkblue', lwd = 1.5)
lines(result[3,], col = 'blue', lty = 2, lwd = 1)

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_col_quantiles_t(
  post_transform_latent_log1p_cap(xb_forecast[1, , ], context = "univariate.xb_forecast.q05"),
  probs = c(0.025, 0.5, 0.975)
)
lines(result[1,], col = 'red', lty = 2, lwd = 1)
lines(result[2,], col = 'darkred', lwd = 1.5)
lines(result[3,], col = 'red', lty = 2, lwd = 1)

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_col_quantiles_t(
  post_transform_latent_log1p_cap(xb_forecast[4, , ], context = "univariate.xb_forecast.q50"),
  probs = c(0.025, 0.5, 0.975)
)
lines(result[1,], col = 'green', lty = 2, lwd = 1)
lines(result[2,], col = 'forestgreen', lwd = 1.5)
lines(result[3,], col = 'green', lty = 2, lwd = 1)


result <- fast_col_quantiles_t(
  post_transform_latent_log1p_cap(y_reps_f_95, context = "univariate.forecast.q95_curve"),
  probs = 0.95
)
lines(as.numeric(result), col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_latent_log1p_cap(y_reps_f_80, context = "univariate.forecast.q80_curve"),
  probs = 0.80
)
lines(as.numeric(result), col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_latent_log1p_cap(y_reps_f_65, context = "univariate.forecast.q65_curve"),
  probs = 0.65
)
lines(as.numeric(result), col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_latent_log1p_cap(y_reps_f_50, context = "univariate.forecast.q50_curve"),
  probs = 0.50
)
lines(as.numeric(result), col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_latent_log1p_cap(y_reps_f_35, context = "univariate.forecast.q35_curve"),
  probs = 0.35
)
lines(as.numeric(result), col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_latent_log1p_cap(y_reps_f_20, context = "univariate.forecast.q20_curve"),
  probs = 0.20
)
lines(as.numeric(result), col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_latent_log1p_cap(y_reps_f_5, context = "univariate.forecast.q05_curve"),
  probs = 0.05
)
lines(as.numeric(result), col = 'black', lwd = 0.5)

points(SL$data0, lwd = 0.8, pch = 16)

dim(synth_hist_uni)
dim(synth_hist_uni_q)
dim(synth_f2_q)
dim(synth_f2)
}

if (univar_post_use_theory_rebuild()) {
  theory_univar_rebuild <- univar_theory_post_rebuild_outputs(days_hist_uni = days_hist_uni)
  if (is.list(theory_univar_rebuild)) {
    xb_hist_uni <- theory_univar_rebuild$xb_hist_uni
    y_hist_uni <- theory_univar_rebuild$y_hist_uni
    xb_forecast <- theory_univar_rebuild$xb_forecast
    y_forecast <- theory_univar_rebuild$y_forecast
    synth_hist_uni <- theory_univar_rebuild$synth_hist_uni
    synth_hist_uni_q <- theory_univar_rebuild$synth_hist_uni_q
    synth_f2 <- theory_univar_rebuild$synth_f2
    synth_f2_q <- theory_univar_rebuild$synth_f2_q
    y_reps_hist_uni <- y_hist_uni
    y_reps_uni <- y_forecast
    n.samp <- theory_univar_rebuild$n_samp
    q_s <- theory_univar_rebuild$q_s_active
    message(
      sprintf(
        "Applied theory-aligned univariate post rebuild: active_quantiles=%s n_samp=%d horizon=%d",
        paste(sprintf("%0.2f", q_s), collapse = ","),
        as.integer(n.samp),
        as.integer(dim(y_forecast)[3])
      )
    )
  }
} else {
  message(
    sprintf(
      "Skipping theory-aligned univariate post rebuild for implementation_mode=%s",
      univar_post_impl_mode()
    )
  )
}

# Save finalized univariate caches after any implementation-specific rebuild so
# downstream isolated post modules can replay diagnostics without recomputing.
saveRDS(y_hist_uni, file = post_cache_path("y_hist_uni.rds"))
saveRDS(y_forecast, file = post_cache_path("y_forecast_uni.rds"))
saveRDS(synth_hist_uni, file = post_cache_path("synth_univar_hist_log1p.rds"))
saveRDS(synth_f2, file = post_cache_path("synth_univar_forecast_log1p.rds"))
saveRDS(synth_hist_uni_q, file = post_cache_path("synth_univar_hist_quantiles_log1p.rds"))
saveRDS(synth_f2_q, file = post_cache_path("synth_univar_forecast_quantiles_log1p.rds"))
} else {
  warning("Skipping univariate load/diagnostic block in 30_univariate_and_misc.R because univariate family is disabled.", call. = FALSE)
}


if (has_ndlm_bundle()) {
  p <- 7
  file_path <- NDLM_VAR_50
  profile_section("univariate.load_disc_vars_ndlm_50", load_ndlm_bundle_with_normalize(file_path))

  par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(4, 0, 0, 0))
  time_cuts <- resolve_time_cuts(
    timestamps = timestamps,
    cutoff_date = CUTOFF_DATE,
    context = "30_univariate_and_misc.ndlm_demo"
  )
  dates_ts_usgs <- timestamps
  idx <- time_cuts[3]:time_cuts[4]
  percentiles <- c(0.025, 0.5, 0.975)

  plot.ts(idx, (new.theta.out_50_NDLM_synth_DISC$exps[1,idx])*0, ylim = c(-2, 2),  type="l", lwd = 1,
          main = "Quantile Dynamics     -    2017-2019",
          xlab = " ", ylab = "log-flow", xaxt = "n")
  lines(idx, Y[1,idx], col = 'black', lwd = 0.1)
  points(idx, Y[1,idx], col = 'gray')
  points(idx, Y[1,idx], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)

  # lines(idx, Y[2,idx], col = 'black', lwd = 0.1)
  # points(idx, Y[2,idx], col = 'gray')
  # points(idx, Y[2,idx], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)

  # lines(idx, Y[3,idx], col = 'black', lwd = 0.1)
  # points(idx, Y[3,idx], col = 'gray')
  # points(idx, Y[3,idx], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)

  result <- new.theta.out_50_NDLM_synth_DISC$exps[1,idx]
  lines(idx, result, col = 'pink', lwd=2)

  result <- new.theta.out_50_NDLM_synth_DISC$sm[1,idx]
  lines(idx, result, col = 'blue', lwd=2)

  result <- new.theta.out_50_NDLM_synth_DISC$sm[2,idx]
  lines(idx, result, col = 'green', lwd=2)

  result <- new.theta.out_50_NDLM_synth_DISC$sm[6,idx]
  lines(idx, result, col = 'orange', lwd=2)

  selected_dates <- dates_ts_usgs[idx]
  num_ticks <- 25
  tick_positions <- pretty(idx, num_ticks)
  if (length(tick_positions) > num_ticks) {
      tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
  }
  tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")
  axis(1, at = tick_positions, labels = FALSE)
  text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
    mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)

  if (nrow(new.theta.out_50_NDLM_synth_DISC$sm) >= 26L) {
    plot.ts(t(new.theta.out_50_NDLM_synth_DISC$sm[22:26, ]))
  } else {
    warning("Skipping NDLM demo block sm[22:26,] because state dimension < 26.", call. = FALSE)
  }

  plot.ts(idx, (new.theta.out_50_NDLM_synth_DISC$sm[c(1), idx]), ylim = c(-2, 2))
  lines(idx, Y[1, idx], col = 'gray')
  if (nrow(new.theta.out_50_NDLM_synth_DISC$sm) >= 22L) {
    lines(new.theta.out_50_NDLM_synth_DISC$sm[22, ] + (new.theta.out_50_NDLM_synth_DISC$sm[c(1), ]), col = 'red')
    lines(
      idx,
      new.theta.out_50_NDLM_synth_DISC$sm[22, idx] +
        (new.theta.out_50_NDLM_synth_DISC$sm[c(2), idx]) +
        (new.theta.out_50_NDLM_synth_DISC$sm[c(1), idx]),
      col = 'blue'
    )
  } else {
    warning("Skipping NDLM demo lines that require state index 22.", call. = FALSE)
  }

  invisible(try({
    covs_list <- vector("list", J)
    ranges_per <- ranges-c(ranges[2:(J)],0)
    dim_theta <- p*(J:1)
    for(i in 1:J){
      covs_list[[i]] <- array(NA_real_,c(dim_theta[i],dim_theta[i],ranges_per[(J-i)+1]))
    }

    # Precompute dimensions and replication counts
    dim_theta <- p * (J:1)
    ranges_per <- ranges - c(ranges[2:J], 0)
    r_vec <- rev(ranges_per)

    # Hyperparams for prior
    epsilon <- 1
    nu <- dim_theta + 1 + epsilon

    # Preallocate the list of 3D arrays (diagonal matrices)
    covs_list <- mapply(function(n, r) {
      replicate(r, diag(0.01, n), simplify = "array")
    }, n = dim_theta, r = r_vec, SIMPLIFY = FALSE)

    # Example: inspect the first covariance matrix of the first period.
    # replicate(..., simplify="array") may return 2D when r == 1.
    cov2 <- covs_list[[2]]
    if (length(dim(cov2)) == 3L) {
      print(cov2[, , 1, drop = FALSE])
    } else {
      print(cov2)
    }

    GG_T <- (GG[,,TT])
    #### This Requires to define the prior inside the kalman filtering!
    sC_T <- new.theta.out_50_NDLM_synth_DISC$sC[,,TT]
    ####
    W_T <- ex.df.mat * GG_T%*%sC_T%*%t(GG_T)

    S_list <- mapply(function(n, factor) {
      # Extract the top-left submatrix of W_T of size n x n
      subW <- W_T[1:n, 1:n]
      # Multiply by factor: (nu - n - 1)
      subW * factor
    }, n = dim_theta, factor = nu - dim_theta - 1, SIMPLIFY = FALSE)

    # Check the result for the first element:
    print(S_list[[2]])

    dim(new.theta.out_50_NDLM_synth_DISC$sC_ens[[1]])
    dim(new.theta.out_50_NDLM_synth_DISC$sC_ens[[2]])
    dim(new.theta.out_50_NDLM_synth_DISC$sm_ens[[1]])
    dim(new.theta.out_50_NDLM_synth_DISC$sm_ens[[2]])
  }, silent = TRUE))
} else {
  warning("Skipping NDLM load/diagnostic block in 30_univariate_and_misc.R because NDLM family is disabled.", call. = FALSE)
}

if (has_disc_w_bundle()) {
  disc_w_source_suffix <- sprintf("exAL_synth_%s", DISC_W_OBJECT_SUFFIX)
  disc_w_target_suffix <- "exAL_synth_DISC"
  file_path <- DISC_W_VAR_05
  profile_section(
    "univariate.load_disc_vars_exal_05",
    load_quantile_bundle_with_alias(
      file_path,
      target_label = "05",
      source_label = DISC_W_VAR_SRC_05,
      suffix = disc_w_source_suffix,
      target_suffix = disc_w_target_suffix
    )
  )

  file_path <- DISC_W_VAR_50
  profile_section(
    "univariate.load_disc_vars_exal_50",
    load_quantile_bundle_with_alias(
      file_path,
      target_label = "50",
      source_label = DISC_W_VAR_SRC_50,
      suffix = disc_w_source_suffix,
      target_suffix = disc_w_target_suffix
    )
  )

  file_path <- DISC_W_VAR_95
  profile_section(
    "univariate.load_disc_vars_exal_95",
    load_quantile_bundle_with_alias(
      file_path,
      target_label = "95",
      source_label = DISC_W_VAR_SRC_95,
      suffix = disc_w_source_suffix,
      target_suffix = disc_w_target_suffix
    )
  )

  file_path <- DISC_W_VAR_20
  profile_section(
    "univariate.load_disc_vars_exal_20",
    load_quantile_bundle_with_alias(
      file_path,
      target_label = "20",
      source_label = DISC_W_VAR_SRC_20,
      suffix = disc_w_source_suffix,
      target_suffix = disc_w_target_suffix
    )
  )

  file_path <- DISC_W_VAR_35
  profile_section(
    "univariate.load_disc_vars_exal_35",
    load_quantile_bundle_with_alias(
      file_path,
      target_label = "35",
      source_label = DISC_W_VAR_SRC_35,
      suffix = disc_w_source_suffix,
      target_suffix = disc_w_target_suffix
    )
  )

  file_path <- DISC_W_VAR_65
  profile_section(
    "univariate.load_disc_vars_exal_65",
    load_quantile_bundle_with_alias(
      file_path,
      target_label = "65",
      source_label = DISC_W_VAR_SRC_65,
      suffix = disc_w_source_suffix,
      target_suffix = disc_w_target_suffix
    )
  )

  file_path <- DISC_W_VAR_80
  profile_section(
    "univariate.load_disc_vars_exal_80",
    load_quantile_bundle_with_alias(
      file_path,
      target_label = "80",
      source_label = DISC_W_VAR_SRC_80,
      suffix = disc_w_source_suffix,
      target_suffix = disc_w_target_suffix
    )
  )
} else {
  warning("Skipping DISC-W load block in 30_univariate_and_misc.R because multivariate family is disabled.", call. = FALSE)
}

if (has_ndlm_bundle()) {
  file_path <- NDLM_VAR_50
  profile_section("univariate.load_disc_vars_ndlm_50_repeat", load_ndlm_bundle_with_normalize(file_path))
}


n.samp <- 2000

p <- 7
