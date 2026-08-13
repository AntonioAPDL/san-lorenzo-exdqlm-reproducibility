###############################################################################
# Multivariate exDQLM-only figures module (q=50 capable with q-alias support)
# Purpose:
#   - Robust post figures for multivariate-only runs without NDLM dependencies.
#   - Focus on fit diagnostics, traces, and forecast-window comparisons.
###############################################################################

safe_get <- function(name, default = NULL) {
  get0(name, ifnotfound = default, inherits = TRUE)
}

multivar_component_analysis_scale <- function(default = "log1p_cms") {
  if (exists("post_resolve_analysis_scale_post_internal", mode = "function", inherits = TRUE)) {
    return(post_resolve_analysis_scale_post_internal(default = default))
  }
  Sys.getenv("UNIFIED_ANALYSIS_SCALE_POST_INTERNAL", default)
}

multivar_component_y_label <- function() {
  scale <- multivar_component_analysis_scale()
  label <- if (exists("post_flow_scale_label", mode = "function", inherits = TRUE)) {
    post_flow_scale_label(scale)
  } else {
    scale
  }
  sprintf("USGS / model scale (%s)", label)
}

multivar_component_pre_days <- function(default = 30L) {
  raw <- suppressWarnings(as.integer(Sys.getenv("UNIFIED_POST_MULTIVAR_COMPONENT_PRE_DAYS", as.character(default))))
  if (!is.finite(raw) || raw < 0L) as.integer(default) else raw
}

as_numeric_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  if (is.atomic(x)) return(as.numeric(x))
  numeric(0)
}

as_trace_matrix <- function(x) {
  if (is.null(x)) return(matrix(numeric(0), nrow = 0L, ncol = 0L))
  if (is.matrix(x)) {
    return(matrix(as.numeric(x), nrow = nrow(x), ncol = ncol(x)))
  }
  if (is.atomic(x)) {
    vals <- as.numeric(x)
    return(matrix(vals, nrow = 1L))
  }
  matrix(numeric(0), nrow = 0L, ncol = 0L)
}

pad_to_len <- function(x, n) {
  out <- rep(NA_real_, n)
  vals <- as.numeric(x)
  n_use <- min(length(vals), n)
  if (n_use > 0L) out[seq_len(n_use)] <- vals[seq_len(n_use)]
  out
}

pad_matrix_rows <- function(mat, n_rows) {
  if (!is.matrix(mat)) return(matrix(NA_real_, nrow = n_rows, ncol = 0L))
  out <- matrix(NA_real_, nrow = n_rows, ncol = ncol(mat))
  keep <- min(n_rows, nrow(mat))
  if (keep > 0L) out[seq_len(keep), ] <- mat[seq_len(keep), , drop = FALSE]
  out
}

safe_row_quantiles <- function(mat, probs = c(0.025, 0.5, 0.975)) {
  if (!is.matrix(mat) || nrow(mat) == 0L || ncol(mat) == 0L) {
    return(matrix(NA_real_, nrow = length(probs), ncol = 0L))
  }
  out <- apply(
    mat,
    1L,
    function(v) {
      vv <- as.numeric(v)
      vv <- vv[is.finite(vv)]
      if (length(vv) == 0L) return(rep(NA_real_, length(probs)))
      quantile(vv, probs = probs, na.rm = TRUE, type = 8, names = FALSE)
    }
  )
  matrix(out, nrow = length(probs), byrow = FALSE)
}

resolve_future_truth_multivar <- function(horizon) {
  h <- as.integer(horizon[[1L]])
  truth <- rep(NA_real_, h)
  if (!is.finite(h) || h <= 0L) return(truth)

  infer_start_from_forecasts <- function() {
    fallback_start <- if (exists("FORECAST_START_DATE", inherits = TRUE)) {
      suppressWarnings(as.Date(get("FORECAST_START_DATE", inherits = TRUE)))
    } else {
      as.Date("2022-12-26")
    }
    if (is.na(fallback_start)) fallback_start <- as.Date("2022-12-26")

    starts <- as.Date(character(0))
    if (exists("glofas_forecast", inherits = TRUE) &&
        is.data.frame(glofas_forecast) &&
        "target_date" %in% names(glofas_forecast)) {
      d <- suppressWarnings(as.Date(glofas_forecast$target_date))
      d <- d[!is.na(d)]
      if (length(d) > 0L) starts <- c(starts, min(d))
    }
    if (exists("nws_forecast", inherits = TRUE) && is.data.frame(nws_forecast)) {
      nws_date_col <- if ("target_date" %in% names(nws_forecast)) "target_date" else if ("Date" %in% names(nws_forecast)) "Date" else ""
      if (nzchar(nws_date_col)) {
        d <- suppressWarnings(as.Date(nws_forecast[[nws_date_col]]))
        d <- d[!is.na(d)]
        if (length(d) > 0L) starts <- c(starts, min(d))
      }
    }
    starts <- starts[!is.na(starts)]
    if (length(starts) > 0L) min(starts) else fallback_start
  }

  start_date <- infer_start_from_forecasts()
  target_dates <- seq.Date(start_date, by = "day", length.out = h)

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
    # `data0` is shared USGS truth on log1p(cms). Keep it on log1p for the
    # repaired production contract; convert only for explicitly older scales.
    flow_log1p <- suppressWarnings(as.numeric(sl$data0))
    flow_analysis <- if (exists("post_transform_usgs_log1p_truth_to_analysis_scale", mode = "function", inherits = TRUE)) {
      post_transform_usgs_log1p_truth_to_analysis_scale(
        flow_log1p,
        target_scale = multivar_component_analysis_scale(),
        context = "multivar_component.future_truth"
      )
    } else if (identical(multivar_component_analysis_scale(), "log_log1p_cms")) {
      out <- rep(NA_real_, length(flow_log1p))
      ok_pos <- is.finite(flow_log1p) & flow_log1p > 0
      out[ok_pos] <- log(flow_log1p[ok_pos])
      out
    } else {
      flow_log1p
    }
    ok <- !is.na(date_col) & is.finite(flow_analysis)
    if (sum(ok) > 0L) {
      idx_map <- match(target_dates, date_col[ok])
      valid <- !is.na(idx_map)
      if (any(valid)) {
        truth[valid] <- flow_analysis[ok][idx_map[valid]]
      }
    }
  }

  if (all(!is.finite(truth))) {
    tt <- suppressWarnings(as.integer(safe_get("TT", NA_integer_)))
    y <- safe_get("Y", NULL)
    if (is.finite(tt) && is.matrix(y) && nrow(y) >= 1L) {
      obs <- as.numeric(y[1, ])
      idx <- tt + seq_len(h)
      valid <- idx >= 1L & idx <= length(obs)
      if (any(valid)) truth[valid] <- obs[idx[valid]]
    }
  }

  truth
}

infer_theta_segment_layout <- function(arr, seg_len) {
  d <- dim(arr)
  if (length(d) != 3L) return(NULL)
  if (d[2] == seg_len) return(list(time_dim = 2L, sample_dim = 3L))
  if (d[3] == seg_len) return(list(time_dim = 3L, sample_dim = 2L))
  # Fallback: choose the dim that is closest to expected segment length.
  dist2 <- abs(d[2] - seg_len)
  dist3 <- abs(d[3] - seg_len)
  if (dist2 <= dist3) list(time_dim = 2L, sample_dim = 3L) else list(time_dim = 3L, sample_dim = 2L)
}

project_location_from_theta_segment <- function(theta_arr, f_vec, seg_len, n_keep) {
  if (!is.array(theta_arr) || length(dim(theta_arr)) != 3L) {
    return(matrix(NA_real_, nrow = seg_len, ncol = 0L))
  }
  d <- dim(theta_arr)
  layout <- infer_theta_segment_layout(theta_arr, seg_len)
  if (is.null(layout)) {
    return(matrix(NA_real_, nrow = seg_len, ncol = 0L))
  }

  p_use <- min(length(f_vec), d[1])
  if (p_use <= 0L) {
    return(matrix(NA_real_, nrow = seg_len, ncol = 0L))
  }
  f_use <- as.numeric(f_vec[seq_len(p_use)])

  t_dim <- layout$time_dim
  s_dim <- layout$sample_dim
  n_time <- d[t_dim]
  n_samp <- d[s_dim]
  t_use <- min(seg_len, n_time)
  s_use <- min(n_keep, n_samp)
  if (t_use <= 0L || s_use <= 0L) {
    return(matrix(NA_real_, nrow = seg_len, ncol = 0L))
  }

  out <- matrix(NA_real_, nrow = seg_len, ncol = s_use)
  for (s in seq_len(s_use)) {
    if (t_dim == 2L) {
      st <- theta_arr[seq_len(p_use), seq_len(t_use), s, drop = FALSE]
    } else {
      st <- theta_arr[seq_len(p_use), s, seq_len(t_use), drop = FALSE]
      st <- aperm(st, c(1, 3, 2))
    }
    st <- matrix(st, nrow = p_use, ncol = t_use)
    out[seq_len(t_use), s] <- as.vector(crossprod(f_use, st))
  }

  out
}

extract_multivar_q50_forecast_samples <- function(horizon) {
  h <- as.integer(horizon[[1L]])
  if (!is.finite(h) || h <= 0L) {
    return(matrix(NA_real_, nrow = 0L, ncol = 0L))
  }

  theta_ens <- safe_get("samp.theta_ens_50_exAL_synth_DISC", NULL)
  ff_list <- safe_get("FF_list", NULL)
  ranges <- as.numeric(safe_get("ranges", NA_real_))
  p_core <- suppressWarnings(as.integer(safe_get("p", 7L)))
  if (!is.list(theta_ens) || !is.list(ff_list) || any(!is.finite(ranges)) || length(ranges) == 0L || !is.finite(p_core) || p_core <= 0L) {
    return(matrix(NA_real_, nrow = h, ncol = 0L))
  }

  j_total <- length(ranges)
  ks <- -diff(c(ranges, 0))
  n_seg <- min(length(theta_ens), length(ff_list), j_total)
  if (!is.finite(n_seg) || n_seg <= 0L) {
    return(matrix(NA_real_, nrow = h, ncol = 0L))
  }

  first_arr <- theta_ens[[1]]$samp_theta
  if (!is.array(first_arr) || length(dim(first_arr)) != 3L) {
    return(matrix(NA_real_, nrow = h, ncol = 0L))
  }
  first_seg_len <- suppressWarnings(as.integer(ks[j_total]))
  if (!is.finite(first_seg_len) || first_seg_len <= 0L) {
    first_seg_len <- dim(first_arr)[2]
  }
  first_layout <- infer_theta_segment_layout(first_arr, first_seg_len)
  if (is.null(first_layout)) {
    return(matrix(NA_real_, nrow = h, ncol = 0L))
  }
  first_sample_n <- dim(first_arr)[first_layout$sample_dim]
  if (!is.finite(first_sample_n) || first_sample_n <= 0L) {
    return(matrix(NA_real_, nrow = h, ncol = 0L))
  }
  # user-tunable cap for forecast sample traces in post-only diagnostics
  sample_cap <- suppressWarnings(as.integer(Sys.getenv("UNIFIED_POST_FORECAST_NSAMP", "600")))
  if (!is.finite(sample_cap) || sample_cap <= 0L) sample_cap <- 600L
  n_keep <- min(sample_cap, first_sample_n)
  if (n_keep <= 0L) {
    return(matrix(NA_real_, nrow = h, ncol = 0L))
  }

  out <- matrix(NA_real_, nrow = h, ncol = n_keep)
  cursor <- 0L
  for (j in seq_len(n_seg)) {
    seg_len <- suppressWarnings(as.integer(ks[j_total - j + 1L]))
    if (!is.finite(seg_len) || seg_len <= 0L) next
    idx <- seq.int(cursor + 1L, length.out = seg_len)
    cursor <- cursor + seg_len
    if (min(idx) > h) next

    theta_arr <- theta_ens[[j]]$samp_theta
    ff_seg <- as.matrix(ff_list[[j]])
    if (!is.array(theta_arr) || length(dim(theta_arr)) != 3L || !is.matrix(ff_seg) || ncol(ff_seg) < 1L) {
      next
    }
    p_use <- min(p_core, nrow(theta_arr), nrow(ff_seg))
    if (p_use <= 0L) next
    f_s <- as.numeric(ff_seg[seq_len(p_use), 1])
    seg_proj <- project_location_from_theta_segment(
      theta_arr = theta_arr,
      f_vec = f_s,
      seg_len = seg_len,
      n_keep = n_keep
    )
    if (!is.matrix(seg_proj) || ncol(seg_proj) == 0L) next

    idx_use <- idx[idx <= h]
    n_use <- min(length(idx_use), nrow(seg_proj))
    if (n_use <= 0L) next
    out[idx_use[seq_len(n_use)], seq_len(ncol(seg_proj))] <- seg_proj[seq_len(n_use), , drop = FALSE]
  }

  out
}

compute_multivar_q50_state_interval <- function(horizon) {
  h <- as.integer(horizon[[1L]])
  out <- list(mean = rep(NA_real_, h), lower = rep(NA_real_, h), upper = rep(NA_real_, h))
  if (!is.finite(h) || h <= 0L) return(out)

  theta_obj <- safe_get("new.theta.out_50_exAL_synth_DISC", NULL)
  ff_list <- safe_get("FF_list", NULL)
  ranges <- as.numeric(safe_get("ranges", NA_real_))
  p_core <- suppressWarnings(as.integer(safe_get("p", 7L)))
  if (!is.list(theta_obj) || !is.list(theta_obj$sm_ens) || !is.list(theta_obj$sC_ens) ||
      !is.list(ff_list) || any(!is.finite(ranges)) || !is.finite(p_core) || p_core <= 0L) {
    return(out)
  }

  j_total <- length(ranges)
  ks <- -diff(c(ranges, 0))
  n_seg <- min(length(theta_obj$sm_ens), length(theta_obj$sC_ens), length(ff_list), j_total)
  if (!is.finite(n_seg) || n_seg <= 0L) return(out)

  cursor <- 0L
  for (j in seq_len(n_seg)) {
    seg_len <- suppressWarnings(as.integer(ks[j_total - j + 1L]))
    if (!is.finite(seg_len) || seg_len <= 0L) next
    idx <- seq.int(cursor + 1L, length.out = seg_len)
    cursor <- cursor + seg_len
    if (min(idx) > h) next

    sm <- as.matrix(theta_obj$sm_ens[[j]])
    sC <- theta_obj$sC_ens[[j]]
    ff_seg <- as.matrix(ff_list[[j]])
    if (!is.matrix(sm) || !is.array(sC) || length(dim(sC)) != 3L || !is.matrix(ff_seg) || ncol(ff_seg) < 1L) next

    p_use <- min(p_core, nrow(sm), dim(sC)[1], dim(sC)[2], nrow(ff_seg))
    t_use <- min(seg_len, ncol(sm), dim(sC)[3])
    if (p_use <= 0L || t_use <= 0L) next
    f_s <- matrix(as.numeric(ff_seg[seq_len(p_use), 1]), ncol = 1L)

    mean_seg <- rep(NA_real_, t_use)
    sd_seg <- rep(NA_real_, t_use)
    for (tt in seq_len(t_use)) {
      mu <- as.numeric(sm[seq_len(p_use), tt])
      sigma <- as.matrix(sC[seq_len(p_use), seq_len(p_use), tt, drop = FALSE])
      mean_seg[tt] <- as.numeric(crossprod(f_s, mu))
      var_seg <- as.numeric(t(f_s) %*% sigma %*% f_s)
      sd_seg[tt] <- if (is.finite(var_seg)) sqrt(max(var_seg, 0)) else NA_real_
    }

    idx_use <- idx[idx <= h]
    n_use <- min(length(idx_use), t_use)
    if (n_use <= 0L) next
    out$mean[idx_use[seq_len(n_use)]] <- mean_seg[seq_len(n_use)]
    out$lower[idx_use[seq_len(n_use)]] <- mean_seg[seq_len(n_use)] - 1.96 * sd_seg[seq_len(n_use)]
    out$upper[idx_use[seq_len(n_use)]] <- mean_seg[seq_len(n_use)] + 1.96 * sd_seg[seq_len(n_use)]
  }

  out
}

build_multivar_q50_forecast_summary <- function() {
  theta <- safe_get("new.theta.out_50_exAL_synth_DISC", NULL)
  y <- safe_get("Y", NULL)
  tt <- suppressWarnings(as.integer(safe_get("TT", NA_integer_)))
  ranges <- as.numeric(safe_get("ranges", NA_real_))
  h <- if (!is.null(ranges) && length(ranges) > 0L && is.finite(ranges[1])) as.integer(ranges[1]) else 0L

  if (!is.list(theta) || !is.matrix(y) || !is.finite(tt) || h <= 0L) {
    return(NULL)
  }

  exps <- as.matrix(theta$exps)
  obs <- as.numeric(y[1, ])
  fit_n <- min(tt, length(obs), if (is.matrix(exps)) ncol(exps) else 0L)
  if (fit_n <= 0L) return(NULL)

  mu_fit <- rep(NA_real_, fit_n)
  mu_fore <- rep(NA_real_, h)
  if (is.matrix(exps) && nrow(exps) >= 1L) {
    mu_fit <- as.numeric(exps[1, seq_len(fit_n)])
    if (ncol(exps) >= tt + 1L) {
      mu_fore <- pad_to_len(exps[1, (tt + 1L):ncol(exps)], h)
    }
  }

  forecast_samples <- extract_multivar_q50_forecast_samples(horizon = h)
  q_fore <- safe_row_quantiles(forecast_samples, probs = c(0.025, 0.5, 0.975))
  lower <- rep(NA_real_, h)
  upper <- rep(NA_real_, h)
  if (is.matrix(q_fore) && ncol(q_fore) == h) {
    lower <- q_fore[1, ]
    upper <- q_fore[3, ]
    q50_sample <- q_fore[2, ]
    miss_mu <- !is.finite(mu_fore)
    if (any(miss_mu)) mu_fore[miss_mu] <- q50_sample[miss_mu]
  }

  if (all(!is.finite(lower)) || all(!is.finite(upper))) {
    state_int <- compute_multivar_q50_state_interval(horizon = h)
    lower <- state_int$lower
    upper <- state_int$upper
    if (all(!is.finite(mu_fore))) mu_fore <- state_int$mean
  }

  # Use state-based reconstruction of mu_usgs in forecast when available:
  # mu_usgs = mu_glofas - aggregated_discrepancy_glofas.
  payload <- suppressWarnings(build_transfer_state_window_q50(pre_days = 0L))
  if (!is.null(payload) && is.data.frame(payload$state_df)) {
    sf <- payload$state_df[payload$state_df$phase == "forecast", , drop = FALSE]
    if (nrow(sf) > 0L) {
      idx <- suppressWarnings(as.integer(sf$day_rel))
      ok <- is.finite(idx) & idx >= 1L & idx <= h
      if (any(ok)) {
        idx_use <- idx[ok]
        mu_state <- as.numeric(sf$mu_usgs[ok])
        lo_state <- as.numeric(sf$mu_usgs_lower_95[ok])
        up_state <- as.numeric(sf$mu_usgs_upper_95[ok])
        take_mu <- is.finite(mu_state)
        if (any(take_mu)) mu_fore[idx_use[take_mu]] <- mu_state[take_mu]
        take_lo <- is.finite(lo_state)
        if (any(take_lo)) lower[idx_use[take_lo]] <- lo_state[take_lo]
        take_up <- is.finite(up_state)
        if (any(take_up)) upper[idx_use[take_up]] <- up_state[take_up]
      }
    }
  }

  truth <- resolve_future_truth_multivar(h)

  ens <- safe_get("ensembles", NULL)
  glofas <- matrix(NA_real_, nrow = h, ncol = 0L)
  nws <- matrix(NA_real_, nrow = h, ncol = 0L)
  if (is.list(ens) && length(ens) >= 1L) {
    glofas <- pad_matrix_rows(as.matrix(ens[[1]]), h)
  }
  if (is.list(ens) && length(ens) >= 2L) {
    nws <- pad_matrix_rows(as.matrix(ens[[2]]), h)
  }
  glofas_mean <- if (ncol(glofas) > 0L) rowMeans(glofas, na.rm = TRUE) else rep(NA_real_, h)
  nws_mean <- if (ncol(nws) > 0L) rowMeans(nws, na.rm = TRUE) else rep(NA_real_, h)

  list(
    fit_obs = obs[seq_len(fit_n)],
    fit_mu = mu_fit,
    fit_n = fit_n,
    horizon = h,
    mu_forecast = mu_fore,
    lower_forecast = lower,
    upper_forecast = upper,
    truth_future = truth,
    glofas_members = glofas,
    nws_members = nws,
    glofas_mean = glofas_mean,
    nws_mean = nws_mean
  )
}

infer_transfer_layout_q50 <- function(theta_obj, p_hint = NA_integer_) {
  out <- list(
    valid = FALSE,
    reason = "unavailable",
    J = NA_integer_,
    p = NA_integer_,
    ppx = NA_integer_,
    TT_hist = NA_integer_,
    core_hist_dim = NA_integer_,
    seg_contract = data.frame()
  )

  if (!is.list(theta_obj) || !is.matrix(theta_obj$sm) || !is.list(theta_obj$sm_ens)) {
    out$reason <- "missing_state_objects"
    return(out)
  }

  j_total <- length(theta_obj$sm_ens)
  if (!is.finite(j_total) || j_total < 1L) {
    out$reason <- "no_forecast_segments"
    return(out)
  }
  j_total <- as.integer(j_total)

  p_val <- suppressWarnings(as.integer(p_hint))
  if (!is.finite(p_val) || p_val <= 0L) {
    if (j_total >= 2L) {
      d1 <- nrow(as.matrix(theta_obj$sm_ens[[1L]]))
      d2 <- nrow(as.matrix(theta_obj$sm_ens[[2L]]))
      p_val <- suppressWarnings(as.integer(abs(d1 - d2)))
    }
  }
  if (!is.finite(p_val) || p_val <= 0L) {
    out$reason <- "cannot_infer_p"
    return(out)
  }

  full_hist_dim <- nrow(theta_obj$sm)
  core_hist_dim <- as.integer(p_val * (j_total + 1L))
  ppx_val <- as.integer(full_hist_dim - core_hist_dim)
  if (!is.finite(ppx_val) || ppx_val < 0L) {
    out$reason <- "state_dim_smaller_than_expected_core"
    return(out)
  }

  seg_rows <- vapply(theta_obj$sm_ens, function(x) nrow(as.matrix(x)), integer(1))
  seg_cols <- vapply(theta_obj$sm_ens, function(x) ncol(as.matrix(x)), integer(1))
  seg_idx <- seq_len(j_total)
  expected_core <- as.integer(p_val * (j_total - seg_idx + 2L))
  expected_with_transfer <- expected_core + ppx_val
  transfer_retained <- if (ppx_val > 0L) {
    seg_rows >= expected_with_transfer
  } else {
    rep(FALSE, length(seg_rows))
  }

  seg_contract <- data.frame(
    segment = seg_idx,
    segment_horizon = seg_cols,
    segment_state_dim = seg_rows,
    expected_core_state_dim = expected_core,
    expected_transfer_state_dim = expected_with_transfer,
    transfer_retained = transfer_retained,
    stringsAsFactors = FALSE
  )

  out$valid <- TRUE
  out$reason <- "ok"
  out$J <- j_total
  out$p <- p_val
  out$ppx <- ppx_val
  out$TT_hist <- ncol(theta_obj$sm)
  out$core_hist_dim <- core_hist_dim
  out$seg_contract <- seg_contract
  out
}

safe_diag_sd <- function(cube_arr, idx, n_time) {
  out <- rep(NA_real_, n_time)
  if (!is.array(cube_arr) || length(dim(cube_arr)) != 3L || n_time <= 0L) return(out)
  if (idx < 1L || idx > dim(cube_arr)[1] || idx > dim(cube_arr)[2]) return(out)
  t_use <- min(n_time, dim(cube_arr)[3])
  if (t_use <= 0L) return(out)
  vv <- as.numeric(cube_arr[idx, idx, seq_len(t_use)])
  out[seq_len(t_use)] <- sqrt(pmax(vv, 0))
  out
}

safe_linear_sd <- function(cube_arr, w, t_idx) {
  if (!is.array(cube_arr) || length(dim(cube_arr)) != 3L) return(NA_real_)
  if (!is.numeric(w) || length(w) != dim(cube_arr)[1] || dim(cube_arr)[1] != dim(cube_arr)[2]) return(NA_real_)
  tt <- suppressWarnings(as.integer(t_idx))
  if (!is.finite(tt) || tt < 1L || tt > dim(cube_arr)[3]) return(NA_real_)
  s <- matrix(
    as.numeric(cube_arr[, , tt, drop = TRUE]),
    nrow = dim(cube_arr)[1],
    ncol = dim(cube_arr)[2]
  )
  v <- suppressWarnings(as.numeric(t(w) %*% s %*% w))
  if (!is.finite(v)) return(NA_real_)
  sqrt(max(v, 0))
}

infer_baseline_ff_q50 <- function(p) {
  p_use <- suppressWarnings(as.integer(p))
  if (!is.finite(p_use) || p_use <= 0L) return(rep(0, 0L))

  m_simp <- safe_get("model_simp", NULL)
  if (is.list(m_simp) && !is.null(m_simp$FF)) {
    ff <- m_simp$FF
    if (is.array(ff) && length(dim(ff)) == 3L && dim(ff)[1] >= p_use && dim(ff)[2] >= 1L) {
      out <- as.numeric(ff[seq_len(p_use), 1, 1])
      if (any(is.finite(out) & abs(out) > 0)) return(out)
    }
    if (is.matrix(ff) && nrow(ff) >= p_use && ncol(ff) >= 1L) {
      out <- as.numeric(ff[seq_len(p_use), 1])
      if (any(is.finite(out) & abs(out) > 0)) return(out)
    }
  }

  ff_list <- safe_get("FF_list", NULL)
  if (is.list(ff_list) && length(ff_list) > 0L) {
    ff0 <- ff_list[[1L]]
    ffm <- if (is.array(ff0) && length(dim(ff0)) == 3L) as.matrix(ff0[, , 1]) else as.matrix(ff0)
    if (is.matrix(ffm) && ncol(ffm) >= 1L && nrow(ffm) >= p_use) {
      n_blocks <- as.integer(floor(nrow(ffm) / p_use))
      if (is.finite(n_blocks) && n_blocks >= 1L) {
        for (b in seq_len(n_blocks)) {
          rows <- seq.int((b - 1L) * p_use + 1L, b * p_use)
          cand <- as.numeric(ffm[rows, 1])
          if (any(is.finite(cand) & abs(cand) > 0)) return(cand)
        }
      }
    }
  }

  out <- rep(0, p_use)
  out[1] <- 1
  out
}

infer_trend_indices_q50 <- function(p) {
  p_use <- suppressWarnings(as.integer(p))
  if (!is.finite(p_use) || p_use <= 0L) {
    return(list(trend_idx = integer(0), season_idx = integer(0)))
  }
  harms <- safe_get("harmonics", NULL)
  n_harm <- if (is.numeric(harms)) length(harms) else 0L
  trend_dim <- p_use - 2L * n_harm
  if (!is.finite(trend_dim) || trend_dim <= 0L || trend_dim >= p_use) {
    trend_dim <- 1L
  }
  trend_idx <- seq_len(trend_dim)
  season_idx <- if (trend_dim < p_use) seq.int(trend_dim + 1L, p_use) else integer(0)
  list(trend_idx = trend_idx, season_idx = season_idx)
}

build_transfer_state_window_q50 <- function(pre_days = 30L) {
  theta_obj <- safe_get("new.theta.out_50_exAL_synth_DISC", NULL)
  y <- safe_get("Y", NULL)
  p_hint <- suppressWarnings(as.integer(safe_get("p", NA_integer_)))
  layout <- infer_transfer_layout_q50(theta_obj, p_hint = p_hint)
  if (!isTRUE(layout$valid)) return(NULL)

  j_total <- layout$J
  p <- layout$p
  ppx <- layout$ppx
  tt_hist <- layout$TT_hist
  core_hist_dim <- layout$core_hist_dim
  seg_contract <- layout$seg_contract
  psi_count <- max(ppx - 1L, 0L)
  ff_base <- infer_baseline_ff_q50(p)
  idx_split <- infer_trend_indices_q50(p)
  trend_idx <- idx_split$trend_idx
  season_idx <- idx_split$season_idx

  pre_n <- suppressWarnings(as.integer(pre_days))
  if (!is.finite(pre_n) || pre_n < 0L) pre_n <- 30L
  hist_start <- max(1L, tt_hist - pre_n)
  hist_idx <- seq.int(hist_start, tt_hist)
  n_hist <- length(hist_idx)
  hist_day_rel <- seq.int(-n_hist + 1L, 0L)

  sm_hist <- as.matrix(theta_obj$sm)
  sC_hist <- theta_obj$sC

  h_obs <- rep(NA_real_, n_hist)
  if (is.matrix(y) && nrow(y) >= 1L && ncol(y) >= max(hist_idx)) {
    h_obs <- as.numeric(y[1, hist_idx])
  }

  seg_h <- vapply(theta_obj$sm_ens, function(x) ncol(as.matrix(x)), integer(1))
  h <- sum(seg_h)
  fore_day_rel <- if (h > 0L) seq_len(h) else integer(0)
  truth_future <- resolve_future_truth_multivar(h)

  state_hist <- data.frame(
    day_rel = hist_day_rel,
    phase = "history",
    mu_usgs = rep(NA_real_, n_hist),
    mu_usgs_lower_95 = rep(NA_real_, n_hist),
    mu_usgs_upper_95 = rep(NA_real_, n_hist),
    mu_glofas = rep(NA_real_, n_hist),
    mu_glofas_lower_95 = rep(NA_real_, n_hist),
    mu_glofas_upper_95 = rep(NA_real_, n_hist),
    mu_nws = rep(NA_real_, n_hist),
    mu_nws_lower_95 = rep(NA_real_, n_hist),
    mu_nws_upper_95 = rep(NA_real_, n_hist),
    agg_discrep_glofas = rep(NA_real_, n_hist),
    agg_discrep_glofas_lower_95 = rep(NA_real_, n_hist),
    agg_discrep_glofas_upper_95 = rep(NA_real_, n_hist),
    agg_discrep_nws = rep(NA_real_, n_hist),
    agg_discrep_nws_lower_95 = rep(NA_real_, n_hist),
    agg_discrep_nws_upper_95 = rep(NA_real_, n_hist),
    zeta_mean = rep(NA_real_, n_hist),
    zeta_lower_95 = rep(NA_real_, n_hist),
    zeta_upper_95 = rep(NA_real_, n_hist),
    trend_agg = rep(NA_real_, n_hist),
    trend_agg_lower_95 = rep(NA_real_, n_hist),
    trend_agg_upper_95 = rep(NA_real_, n_hist),
    season_agg = rep(NA_real_, n_hist),
    season_agg_lower_95 = rep(NA_real_, n_hist),
    season_agg_upper_95 = rep(NA_real_, n_hist),
    mu_without_transfer = rep(NA_real_, n_hist),
    mu_without_transfer_lower_95 = rep(NA_real_, n_hist),
    mu_without_transfer_upper_95 = rep(NA_real_, n_hist),
    usgs_observed = h_obs,
    stringsAsFactors = FALSE
  )

  zeta_hist_idx <- if (ppx > 0L) core_hist_dim + 1L else NA_integer_
  has_hist_transfer <- is.finite(zeta_hist_idx) && zeta_hist_idx <= nrow(sm_hist)

  for (ii in seq_len(n_hist)) {
    tt <- hist_idx[ii]
    mt <- as.numeric(sm_hist[, tt])
    theta_idx <- seq_len(p)
    delta_g_idx <- if (j_total >= 1L) seq.int(p + 1L, 2L * p) else integer(0)
    delta_n_idx <- if (j_total >= 2L) seq.int(2L * p + 1L, 3L * p) else integer(0)

    base_no_transfer <- sum(ff_base * mt[theta_idx])
    trend_mean <- if (length(trend_idx) > 0L) sum(ff_base[trend_idx] * mt[trend_idx]) else NA_real_
    season_mean <- if (length(season_idx) > 0L) sum(ff_base[season_idx] * mt[season_idx]) else 0
    zeta_mean <- if (has_hist_transfer) mt[zeta_hist_idx] else 0
    disc_g_mean <- if (length(delta_g_idx) == p) sum(ff_base * mt[delta_g_idx]) else NA_real_
    disc_n_mean <- if (length(delta_n_idx) == p) sum(ff_base * mt[delta_n_idx]) else NA_real_

    mu_usgs <- base_no_transfer + zeta_mean
    mu_g <- if (is.finite(disc_g_mean)) mu_usgs + disc_g_mean else NA_real_
    mu_n <- if (is.finite(disc_n_mean)) mu_usgs + disc_n_mean else NA_real_

    w_usgs <- rep(0, nrow(sm_hist))
    w_usgs[theta_idx] <- ff_base
    if (has_hist_transfer) w_usgs[zeta_hist_idx] <- 1
    sd_usgs <- safe_linear_sd(sC_hist, w_usgs, tt)

    w_base <- rep(0, nrow(sm_hist)); w_base[theta_idx] <- ff_base
    sd_base <- safe_linear_sd(sC_hist, w_base, tt)
    w_trend <- rep(0, nrow(sm_hist)); if (length(trend_idx) > 0L) w_trend[trend_idx] <- ff_base[trend_idx]
    sd_trend <- safe_linear_sd(sC_hist, w_trend, tt)
    w_season <- rep(0, nrow(sm_hist)); if (length(season_idx) > 0L) w_season[season_idx] <- ff_base[season_idx]
    sd_season <- safe_linear_sd(sC_hist, w_season, tt)

    z_sd <- if (has_hist_transfer) safe_diag_sd(sC_hist, zeta_hist_idx, tt)[tt] else 0
    d_g_sd <- NA_real_
    d_n_sd <- NA_real_
    mu_g_sd <- NA_real_
    mu_n_sd <- NA_real_

    if (length(delta_g_idx) == p) {
      w_disc_g <- rep(0, nrow(sm_hist)); w_disc_g[delta_g_idx] <- ff_base
      d_g_sd <- safe_linear_sd(sC_hist, w_disc_g, tt)
      w_mu_g <- w_usgs + w_disc_g
      mu_g_sd <- safe_linear_sd(sC_hist, w_mu_g, tt)
    }
    if (length(delta_n_idx) == p) {
      w_disc_n <- rep(0, nrow(sm_hist)); w_disc_n[delta_n_idx] <- ff_base
      d_n_sd <- safe_linear_sd(sC_hist, w_disc_n, tt)
      w_mu_n <- w_usgs + w_disc_n
      mu_n_sd <- safe_linear_sd(sC_hist, w_mu_n, tt)
    }

    state_hist$mu_usgs[ii] <- mu_usgs
    state_hist$mu_usgs_lower_95[ii] <- mu_usgs - 1.96 * sd_usgs
    state_hist$mu_usgs_upper_95[ii] <- mu_usgs + 1.96 * sd_usgs
    state_hist$mu_glofas[ii] <- mu_g
    state_hist$mu_glofas_lower_95[ii] <- if (is.finite(mu_g_sd)) mu_g - 1.96 * mu_g_sd else NA_real_
    state_hist$mu_glofas_upper_95[ii] <- if (is.finite(mu_g_sd)) mu_g + 1.96 * mu_g_sd else NA_real_
    state_hist$mu_nws[ii] <- mu_n
    state_hist$mu_nws_lower_95[ii] <- if (is.finite(mu_n_sd)) mu_n - 1.96 * mu_n_sd else NA_real_
    state_hist$mu_nws_upper_95[ii] <- if (is.finite(mu_n_sd)) mu_n + 1.96 * mu_n_sd else NA_real_
    state_hist$agg_discrep_glofas[ii] <- disc_g_mean
    state_hist$agg_discrep_glofas_lower_95[ii] <- if (is.finite(d_g_sd)) disc_g_mean - 1.96 * d_g_sd else NA_real_
    state_hist$agg_discrep_glofas_upper_95[ii] <- if (is.finite(d_g_sd)) disc_g_mean + 1.96 * d_g_sd else NA_real_
    state_hist$agg_discrep_nws[ii] <- disc_n_mean
    state_hist$agg_discrep_nws_lower_95[ii] <- if (is.finite(d_n_sd)) disc_n_mean - 1.96 * d_n_sd else NA_real_
    state_hist$agg_discrep_nws_upper_95[ii] <- if (is.finite(d_n_sd)) disc_n_mean + 1.96 * d_n_sd else NA_real_
    state_hist$zeta_mean[ii] <- zeta_mean
    state_hist$zeta_lower_95[ii] <- zeta_mean - 1.96 * z_sd
    state_hist$zeta_upper_95[ii] <- zeta_mean + 1.96 * z_sd
    state_hist$trend_agg[ii] <- trend_mean
    state_hist$trend_agg_lower_95[ii] <- if (is.finite(sd_trend)) trend_mean - 1.96 * sd_trend else NA_real_
    state_hist$trend_agg_upper_95[ii] <- if (is.finite(sd_trend)) trend_mean + 1.96 * sd_trend else NA_real_
    state_hist$season_agg[ii] <- season_mean
    state_hist$season_agg_lower_95[ii] <- if (is.finite(sd_season)) season_mean - 1.96 * sd_season else NA_real_
    state_hist$season_agg_upper_95[ii] <- if (is.finite(sd_season)) season_mean + 1.96 * sd_season else NA_real_
    state_hist$mu_without_transfer[ii] <- base_no_transfer
    state_hist$mu_without_transfer_lower_95[ii] <- base_no_transfer - 1.96 * sd_base
    state_hist$mu_without_transfer_upper_95[ii] <- base_no_transfer + 1.96 * sd_base
  }

  state_fore <- data.frame(
    day_rel = fore_day_rel,
    phase = "forecast",
    mu_usgs = rep(NA_real_, h),
    mu_usgs_lower_95 = rep(NA_real_, h),
    mu_usgs_upper_95 = rep(NA_real_, h),
    mu_glofas = rep(NA_real_, h),
    mu_glofas_lower_95 = rep(NA_real_, h),
    mu_glofas_upper_95 = rep(NA_real_, h),
    mu_nws = rep(NA_real_, h),
    mu_nws_lower_95 = rep(NA_real_, h),
    mu_nws_upper_95 = rep(NA_real_, h),
    agg_discrep_glofas = rep(NA_real_, h),
    agg_discrep_glofas_lower_95 = rep(NA_real_, h),
    agg_discrep_glofas_upper_95 = rep(NA_real_, h),
    agg_discrep_nws = rep(NA_real_, h),
    agg_discrep_nws_lower_95 = rep(NA_real_, h),
    agg_discrep_nws_upper_95 = rep(NA_real_, h),
    zeta_mean = rep(NA_real_, h),
    zeta_lower_95 = rep(NA_real_, h),
    zeta_upper_95 = rep(NA_real_, h),
    trend_agg = rep(NA_real_, h),
    trend_agg_lower_95 = rep(NA_real_, h),
    trend_agg_upper_95 = rep(NA_real_, h),
    season_agg = rep(NA_real_, h),
    season_agg_lower_95 = rep(NA_real_, h),
    season_agg_upper_95 = rep(NA_real_, h),
    mu_without_transfer = rep(NA_real_, h),
    mu_without_transfer_lower_95 = rep(NA_real_, h),
    mu_without_transfer_upper_95 = rep(NA_real_, h),
    usgs_observed = truth_future,
    stringsAsFactors = FALSE
  )

  psi_hist_mean <- matrix(NA_real_, nrow = psi_count, ncol = n_hist)
  psi_hist_sd <- matrix(NA_real_, nrow = psi_count, ncol = n_hist)
  if (psi_count > 0L) {
    psi_hist_idx <- seq.int(core_hist_dim + 2L, core_hist_dim + ppx)
    for (k in seq_len(psi_count)) {
      if (psi_hist_idx[k] <= nrow(sm_hist)) {
        psi_hist_mean[k, ] <- as.numeric(sm_hist[psi_hist_idx[k], hist_idx])
        psi_hist_sd[k, ] <- safe_diag_sd(sC_hist, psi_hist_idx[k], n_hist)
      }
    }
  }

  psi_fore_mean <- matrix(NA_real_, nrow = psi_count, ncol = h)
  psi_fore_sd <- matrix(NA_real_, nrow = psi_count, ncol = h)
  forecast_has_transfer <- FALSE

  cursor <- 0L
  for (j in seq_len(j_total)) {
    seg_len <- seg_h[j]
    if (!is.finite(seg_len) || seg_len <= 0L) next
    seg_idx <- seq.int(cursor + 1L, cursor + seg_len)
    cursor <- cursor + seg_len

    sm_seg <- as.matrix(theta_obj$sm_ens[[j]])
    sC_seg <- theta_obj$sC_ens[[j]]
    if (!is.matrix(sm_seg) || !is.array(sC_seg) || length(dim(sC_seg)) != 3L) next
    t_use <- min(seg_len, ncol(sm_seg), dim(sC_seg)[3])
    if (!is.finite(t_use) || t_use <= 0L) next

    jj <- j_total - j + 1L
    core_dim_j <- as.integer(p * (jj + 1L))
    has_transfer <- isTRUE(seg_contract$transfer_retained[j])
    if (has_transfer) forecast_has_transfer <- TRUE
    zeta_idx_j <- if (has_transfer) core_dim_j + 1L else NA_integer_

    for (tt in seq_len(t_use)) {
      g_idx <- seg_idx[tt]
      mt <- as.numeric(sm_seg[, tt])

      theta_idx <- seq_len(p)
      delta_g_idx <- if (jj >= 1L) seq.int(p + 1L, 2L * p) else integer(0)
      delta_n_idx <- if (jj >= 2L) seq.int(2L * p + 1L, 3L * p) else integer(0)

      base_no_transfer <- sum(ff_base * mt[theta_idx])
      trend_mean <- if (length(trend_idx) > 0L) sum(ff_base[trend_idx] * mt[trend_idx]) else NA_real_
      season_mean <- if (length(season_idx) > 0L) sum(ff_base[season_idx] * mt[season_idx]) else 0
      zeta_mean <- if (isTRUE(has_transfer) && is.finite(zeta_idx_j) && zeta_idx_j <= length(mt)) mt[zeta_idx_j] else 0
      disc_g_mean <- if (length(delta_g_idx) == p) sum(ff_base * mt[delta_g_idx]) else NA_real_
      disc_n_mean <- if (length(delta_n_idx) == p) sum(ff_base * mt[delta_n_idx]) else NA_real_

      mu_usgs <- base_no_transfer + zeta_mean
      mu_g <- if (is.finite(disc_g_mean)) mu_usgs + disc_g_mean else NA_real_
      mu_n <- if (is.finite(disc_n_mean)) mu_usgs + disc_n_mean else NA_real_

      w_usgs <- rep(0, nrow(sm_seg))
      w_usgs[theta_idx] <- ff_base
      if (isTRUE(has_transfer) && is.finite(zeta_idx_j) && zeta_idx_j <= nrow(sm_seg)) w_usgs[zeta_idx_j] <- 1
      sd_usgs <- safe_linear_sd(sC_seg, w_usgs, tt)

      w_base <- rep(0, nrow(sm_seg)); w_base[theta_idx] <- ff_base
      sd_base <- safe_linear_sd(sC_seg, w_base, tt)
      w_trend <- rep(0, nrow(sm_seg)); if (length(trend_idx) > 0L) w_trend[trend_idx] <- ff_base[trend_idx]
      sd_trend <- safe_linear_sd(sC_seg, w_trend, tt)
      w_season <- rep(0, nrow(sm_seg)); if (length(season_idx) > 0L) w_season[season_idx] <- ff_base[season_idx]
      sd_season <- safe_linear_sd(sC_seg, w_season, tt)

      z_sd <- if (isTRUE(has_transfer) && is.finite(zeta_idx_j)) safe_diag_sd(sC_seg, zeta_idx_j, tt)[tt] else 0
      d_g_sd <- NA_real_
      d_n_sd <- NA_real_
      mu_g_sd <- NA_real_
      mu_n_sd <- NA_real_

      if (length(delta_g_idx) == p) {
        w_disc_g <- rep(0, nrow(sm_seg)); w_disc_g[delta_g_idx] <- ff_base
        d_g_sd <- safe_linear_sd(sC_seg, w_disc_g, tt)
        w_mu_g <- w_usgs + w_disc_g
        mu_g_sd <- safe_linear_sd(sC_seg, w_mu_g, tt)
      }
      if (length(delta_n_idx) == p) {
        w_disc_n <- rep(0, nrow(sm_seg)); w_disc_n[delta_n_idx] <- ff_base
        d_n_sd <- safe_linear_sd(sC_seg, w_disc_n, tt)
        w_mu_n <- w_usgs + w_disc_n
        mu_n_sd <- safe_linear_sd(sC_seg, w_mu_n, tt)
      }

      state_fore$mu_usgs[g_idx] <- mu_usgs
      state_fore$mu_usgs_lower_95[g_idx] <- mu_usgs - 1.96 * sd_usgs
      state_fore$mu_usgs_upper_95[g_idx] <- mu_usgs + 1.96 * sd_usgs
      state_fore$mu_glofas[g_idx] <- mu_g
      state_fore$mu_glofas_lower_95[g_idx] <- if (is.finite(mu_g_sd)) mu_g - 1.96 * mu_g_sd else NA_real_
      state_fore$mu_glofas_upper_95[g_idx] <- if (is.finite(mu_g_sd)) mu_g + 1.96 * mu_g_sd else NA_real_
      state_fore$mu_nws[g_idx] <- mu_n
      state_fore$mu_nws_lower_95[g_idx] <- if (is.finite(mu_n_sd)) mu_n - 1.96 * mu_n_sd else NA_real_
      state_fore$mu_nws_upper_95[g_idx] <- if (is.finite(mu_n_sd)) mu_n + 1.96 * mu_n_sd else NA_real_
      state_fore$agg_discrep_glofas[g_idx] <- disc_g_mean
      state_fore$agg_discrep_glofas_lower_95[g_idx] <- if (is.finite(d_g_sd)) disc_g_mean - 1.96 * d_g_sd else NA_real_
      state_fore$agg_discrep_glofas_upper_95[g_idx] <- if (is.finite(d_g_sd)) disc_g_mean + 1.96 * d_g_sd else NA_real_
      state_fore$agg_discrep_nws[g_idx] <- disc_n_mean
      state_fore$agg_discrep_nws_lower_95[g_idx] <- if (is.finite(d_n_sd)) disc_n_mean - 1.96 * d_n_sd else NA_real_
      state_fore$agg_discrep_nws_upper_95[g_idx] <- if (is.finite(d_n_sd)) disc_n_mean + 1.96 * d_n_sd else NA_real_
      state_fore$zeta_mean[g_idx] <- zeta_mean
      state_fore$zeta_lower_95[g_idx] <- zeta_mean - 1.96 * z_sd
      state_fore$zeta_upper_95[g_idx] <- zeta_mean + 1.96 * z_sd
      state_fore$trend_agg[g_idx] <- trend_mean
      state_fore$trend_agg_lower_95[g_idx] <- if (is.finite(sd_trend)) trend_mean - 1.96 * sd_trend else NA_real_
      state_fore$trend_agg_upper_95[g_idx] <- if (is.finite(sd_trend)) trend_mean + 1.96 * sd_trend else NA_real_
      state_fore$season_agg[g_idx] <- season_mean
      state_fore$season_agg_lower_95[g_idx] <- if (is.finite(sd_season)) season_mean - 1.96 * sd_season else NA_real_
      state_fore$season_agg_upper_95[g_idx] <- if (is.finite(sd_season)) season_mean + 1.96 * sd_season else NA_real_
      state_fore$mu_without_transfer[g_idx] <- base_no_transfer
      state_fore$mu_without_transfer_lower_95[g_idx] <- base_no_transfer - 1.96 * sd_base
      state_fore$mu_without_transfer_upper_95[g_idx] <- base_no_transfer + 1.96 * sd_base
    }

    if (psi_count > 0L && isTRUE(has_transfer)) {
      psi_idx_j <- seq.int(core_dim_j + 2L, core_dim_j + ppx)
      for (k in seq_len(min(psi_count, length(psi_idx_j)))) {
        psi_k_idx <- psi_idx_j[k]
        if (psi_k_idx <= nrow(sm_seg)) {
          psi_fore_mean[k, seg_idx[seq_len(t_use)]] <- as.numeric(sm_seg[psi_k_idx, seq_len(t_use)])
          psi_fore_sd[k, seg_idx[seq_len(t_use)]] <- safe_diag_sd(sC_seg, psi_k_idx, t_use)[seq_len(t_use)]
        }
      }
    }
  }

  state_df <- rbind(state_hist, state_fore)
  state_df$mu_total <- state_df$mu_usgs
  state_df$mu_usgs_from_glofas <- state_df$mu_glofas - state_df$agg_discrep_glofas
  state_df$identity_err_glofas <- state_df$mu_glofas - state_df$mu_usgs - state_df$agg_discrep_glofas
  state_df$identity_err_nws <- state_df$mu_nws - state_df$mu_usgs - state_df$agg_discrep_nws

  psi_rows <- list()
  if (psi_count > 0L) {
    for (k in seq_len(psi_count)) {
      coeff_name <- sprintf("psi_%02d", k)
      psi_hist <- data.frame(
        coefficient = coeff_name,
        day_rel = hist_day_rel,
        phase = "history",
        mean = psi_hist_mean[k, ],
        lower_95 = psi_hist_mean[k, ] - 1.96 * psi_hist_sd[k, ],
        upper_95 = psi_hist_mean[k, ] + 1.96 * psi_hist_sd[k, ],
        stringsAsFactors = FALSE
      )
      psi_fore <- data.frame(
        coefficient = coeff_name,
        day_rel = fore_day_rel,
        phase = "forecast",
        mean = if (h > 0L) psi_fore_mean[k, ] else numeric(0),
        lower_95 = if (h > 0L) psi_fore_mean[k, ] - 1.96 * psi_fore_sd[k, ] else numeric(0),
        upper_95 = if (h > 0L) psi_fore_mean[k, ] + 1.96 * psi_fore_sd[k, ] else numeric(0),
        stringsAsFactors = FALSE
      )
      psi_rows[[length(psi_rows) + 1L]] <- rbind(psi_hist, psi_fore)
    }
  }
  psi_df <- if (length(psi_rows) > 0L) do.call(rbind, psi_rows) else data.frame()

  err_g_max <- suppressWarnings(max(abs(state_df$identity_err_glofas[is.finite(state_df$identity_err_glofas)]), na.rm = TRUE))
  err_n_max <- suppressWarnings(max(abs(state_df$identity_err_nws[is.finite(state_df$identity_err_nws)]), na.rm = TRUE))
  if (!is.finite(err_g_max)) err_g_max <- NA_real_
  if (!is.finite(err_n_max)) err_n_max <- NA_real_

  list(
    state_df = state_df,
    psi_df = psi_df,
    seg_contract = seg_contract,
    meta = list(
      J = j_total,
      p = p,
      ppx = ppx,
      TT_hist = tt_hist,
      forecast_horizon = h,
      forecast_has_transfer = isTRUE(forecast_has_transfer),
      trend_state_dim = length(trend_idx),
      season_state_dim = length(season_idx),
      identity_max_abs_err_glofas = err_g_max,
      identity_max_abs_err_nws = err_n_max
    )
  )
}

multivar_vb_location_quantile_specs <- function() {
  raw <- Sys.getenv("UNIFIED_FIT_QUANTILE_LABELS", "")
  if (nzchar(raw)) {
    labels <- unlist(strsplit(raw, ",", fixed = TRUE), use.names = FALSE)
  } else {
    labels <- safe_get("quantile_labels", c("05", "20", "35", "50", "65", "80", "95"))
  }
  labels <- trimws(as.character(labels))
  labels <- labels[nzchar(labels)]
  labels <- gsub("^q", "", labels, ignore.case = TRUE)
  q_num <- suppressWarnings(as.integer(labels))
  q_num <- q_num[is.finite(q_num)]
  if (length(q_num) == 0L) q_num <- c(5L, 20L, 35L, 50L, 65L, 80L, 95L)
  q_num <- unique(q_num)
  q_num <- q_num[order(q_num)]
  data.frame(
    quantile = sprintf("q%02d", q_num),
    suffix = as.character(q_num),
    probability = q_num / 100,
    stringsAsFactors = FALSE
  )
}

multivar_get_vb_theta_for_quantile <- function(suffix) {
  suffix <- as.character(suffix[[1L]])
  candidates <- unique(c(
    sprintf("new.theta.out_%s_exAL_synth_DISC", suffix),
    sprintf("new.theta.out_%02d_exAL_synth_DISC", suppressWarnings(as.integer(suffix)))
  ))
  for (nm in candidates) {
    obj <- safe_get(nm, NULL)
    if (is.list(obj)) return(obj)
  }
  NULL
}

build_multivar_vb_usgs_location_rows_for_quantile <- function(theta_obj, quantile_label, probability, pre_days = 30L) {
  y <- safe_get("Y", NULL)
  p_hint <- suppressWarnings(as.integer(safe_get("p", NA_integer_)))
  layout <- infer_transfer_layout_q50(theta_obj, p_hint = p_hint)
  if (!isTRUE(layout$valid)) return(data.frame())

  j_total <- layout$J
  p <- layout$p
  ppx <- layout$ppx
  tt_hist <- layout$TT_hist
  core_hist_dim <- layout$core_hist_dim
  seg_contract <- layout$seg_contract
  ff_base <- infer_baseline_ff_q50(p)

  pre_n <- suppressWarnings(as.integer(pre_days))
  if (!is.finite(pre_n) || pre_n < 0L) pre_n <- 30L
  hist_start <- max(1L, tt_hist - pre_n)
  hist_idx <- seq.int(hist_start, tt_hist)
  n_hist <- length(hist_idx)
  hist_day_rel <- seq.int(-n_hist + 1L, 0L)

  cutoff_date <- suppressWarnings(as.Date(Sys.getenv("UNIFIED_CUTOFF_DATE", "")))
  make_dates <- function(day_rel) {
    if (is.na(cutoff_date)) {
      return(rep(as.Date(NA), length(day_rel)))
    }
    cutoff_date + as.integer(day_rel)
  }

  sm_hist <- as.matrix(theta_obj$sm)
  exps <- as.matrix(theta_obj$exps)
  h_obs <- rep(NA_real_, n_hist)
  if (is.matrix(y) && nrow(y) >= 1L && ncol(y) >= max(hist_idx)) {
    h_obs <- as.numeric(y[1, hist_idx])
  }

  hist_rows <- data.frame(
    quantile = quantile_label,
    probability = as.numeric(probability),
    day_rel = hist_day_rel,
    date = make_dates(hist_day_rel),
    phase = "history",
    mu_usgs = rep(NA_real_, n_hist),
    mu_usgs_state = rep(NA_real_, n_hist),
    mu_usgs_exps = rep(NA_real_, n_hist),
    mu_glofas_exps = rep(NA_real_, n_hist),
    agg_discrep_glofas = rep(NA_real_, n_hist),
    mu_usgs_from_glofas = rep(NA_real_, n_hist),
    mu_nws_exps = rep(NA_real_, n_hist),
    agg_discrep_nws = rep(NA_real_, n_hist),
    mu_usgs_from_nws = rep(NA_real_, n_hist),
    identity_err_glofas = rep(NA_real_, n_hist),
    identity_err_nws = rep(NA_real_, n_hist),
    usgs_observed = h_obs,
    source_basis = rep("history_exps", n_hist),
    stringsAsFactors = FALSE
  )

  zeta_hist_idx <- if (ppx > 0L) core_hist_dim + 1L else NA_integer_
  has_hist_transfer <- is.finite(zeta_hist_idx) && zeta_hist_idx <= nrow(sm_hist)
  for (ii in seq_len(n_hist)) {
    tt <- hist_idx[ii]
    mt <- as.numeric(sm_hist[, tt])
    theta_idx <- seq_len(p)
    delta_g_idx <- if (j_total >= 1L) seq.int(p + 1L, 2L * p) else integer(0)
    delta_n_idx <- if (j_total >= 2L) seq.int(2L * p + 1L, 3L * p) else integer(0)

    base_no_transfer <- sum(ff_base * mt[theta_idx])
    zeta_mean <- if (has_hist_transfer) mt[zeta_hist_idx] else 0
    mu_state <- base_no_transfer + zeta_mean
    disc_g <- if (length(delta_g_idx) == p) sum(ff_base * mt[delta_g_idx]) else NA_real_
    disc_n <- if (length(delta_n_idx) == p) sum(ff_base * mt[delta_n_idx]) else NA_real_
    mu_exps <- if (is.matrix(exps) && nrow(exps) >= 1L && ncol(exps) >= tt) exps[1L, tt] else NA_real_
    mu_g <- if (is.finite(disc_g)) mu_state + disc_g else NA_real_
    mu_n <- if (is.finite(disc_n)) mu_state + disc_n else NA_real_

    hist_rows$mu_usgs_state[ii] <- mu_state
    hist_rows$mu_usgs_exps[ii] <- mu_exps
    hist_rows$mu_usgs[ii] <- if (is.finite(mu_exps)) mu_exps else mu_state
    hist_rows$mu_glofas_exps[ii] <- mu_g
    hist_rows$agg_discrep_glofas[ii] <- disc_g
    hist_rows$mu_usgs_from_glofas[ii] <- if (is.finite(mu_g) && is.finite(disc_g)) mu_g - disc_g else NA_real_
    hist_rows$mu_nws_exps[ii] <- mu_n
    hist_rows$agg_discrep_nws[ii] <- disc_n
    hist_rows$mu_usgs_from_nws[ii] <- if (is.finite(mu_n) && is.finite(disc_n)) mu_n - disc_n else NA_real_
    hist_rows$identity_err_glofas[ii] <- hist_rows$mu_usgs_from_glofas[ii] - mu_state
    hist_rows$identity_err_nws[ii] <- hist_rows$mu_usgs_from_nws[ii] - mu_state
    if (!is.finite(mu_exps) && is.finite(mu_state)) hist_rows$source_basis[ii] <- "history_state"
  }

  seg_h <- vapply(theta_obj$sm_ens, function(x) ncol(as.matrix(x)), integer(1))
  h <- sum(seg_h)
  fore_day_rel <- if (h > 0L) seq_len(h) else integer(0)
  truth_future <- resolve_future_truth_multivar(h)
  fore_rows <- data.frame(
    quantile = quantile_label,
    probability = as.numeric(probability),
    day_rel = fore_day_rel,
    date = make_dates(fore_day_rel),
    phase = "forecast",
    mu_usgs = rep(NA_real_, h),
    mu_usgs_state = rep(NA_real_, h),
    mu_usgs_exps = rep(NA_real_, h),
    mu_glofas_exps = rep(NA_real_, h),
    agg_discrep_glofas = rep(NA_real_, h),
    mu_usgs_from_glofas = rep(NA_real_, h),
    mu_nws_exps = rep(NA_real_, h),
    agg_discrep_nws = rep(NA_real_, h),
    mu_usgs_from_nws = rep(NA_real_, h),
    identity_err_glofas = rep(NA_real_, h),
    identity_err_nws = rep(NA_real_, h),
    usgs_observed = truth_future,
    source_basis = rep("forecast_unresolved", h),
    stringsAsFactors = FALSE
  )

  cursor <- 0L
  for (j in seq_len(j_total)) {
    seg_len <- seg_h[j]
    if (!is.finite(seg_len) || seg_len <= 0L) next
    seg_idx <- seq.int(cursor + 1L, cursor + seg_len)
    cursor <- cursor + seg_len

    sm_seg <- as.matrix(theta_obj$sm_ens[[j]])
    if (!is.matrix(sm_seg)) next
    t_use <- min(seg_len, ncol(sm_seg))
    if (!is.finite(t_use) || t_use <= 0L) next

    jj <- j_total - j + 1L
    core_dim_j <- as.integer(p * (jj + 1L))
    has_transfer <- isTRUE(seg_contract$transfer_retained[j])
    zeta_idx_j <- if (has_transfer) core_dim_j + 1L else NA_integer_

    for (tt in seq_len(t_use)) {
      g_idx <- seg_idx[tt]
      mt <- as.numeric(sm_seg[, tt])
      theta_idx <- seq_len(p)
      delta_g_idx <- if (jj >= 1L) seq.int(p + 1L, 2L * p) else integer(0)
      delta_n_idx <- if (jj >= 2L) seq.int(2L * p + 1L, 3L * p) else integer(0)

      base_no_transfer <- sum(ff_base * mt[theta_idx])
      zeta_mean <- if (isTRUE(has_transfer) && is.finite(zeta_idx_j) && zeta_idx_j <= length(mt)) mt[zeta_idx_j] else 0
      mu_state <- base_no_transfer + zeta_mean
      disc_g <- if (length(delta_g_idx) == p) sum(ff_base * mt[delta_g_idx]) else NA_real_
      disc_n <- if (length(delta_n_idx) == p) sum(ff_base * mt[delta_n_idx]) else NA_real_

      exps_col <- tt_hist + g_idx
      mu_exps <- if (is.matrix(exps) && nrow(exps) >= 1L && ncol(exps) >= exps_col) exps[1L, exps_col] else NA_real_
      mu_g <- if (is.matrix(exps) && nrow(exps) >= 2L && ncol(exps) >= exps_col) exps[2L, exps_col] else NA_real_
      mu_n <- if (is.matrix(exps) && nrow(exps) >= 3L && ncol(exps) >= exps_col) exps[3L, exps_col] else NA_real_
      usgs_from_g <- if (is.finite(mu_g) && is.finite(disc_g)) mu_g - disc_g else NA_real_
      usgs_from_n <- if (is.finite(mu_n) && is.finite(disc_n)) mu_n - disc_n else NA_real_

      mu_usgs <- NA_real_
      source_basis <- "forecast_unresolved"
      if (is.finite(usgs_from_g)) {
        mu_usgs <- usgs_from_g
        source_basis <- "forecast_glofas_minus_discrepancy"
      } else if (is.finite(usgs_from_n)) {
        mu_usgs <- usgs_from_n
        source_basis <- "forecast_nws_minus_discrepancy"
      } else if (is.finite(mu_exps)) {
        mu_usgs <- mu_exps
        source_basis <- "forecast_exps_row1"
      } else if (is.finite(mu_state)) {
        mu_usgs <- mu_state
        source_basis <- "forecast_state"
      }

      fore_rows$mu_usgs[g_idx] <- mu_usgs
      fore_rows$mu_usgs_state[g_idx] <- mu_state
      fore_rows$mu_usgs_exps[g_idx] <- mu_exps
      fore_rows$mu_glofas_exps[g_idx] <- mu_g
      fore_rows$agg_discrep_glofas[g_idx] <- disc_g
      fore_rows$mu_usgs_from_glofas[g_idx] <- usgs_from_g
      fore_rows$mu_nws_exps[g_idx] <- mu_n
      fore_rows$agg_discrep_nws[g_idx] <- disc_n
      fore_rows$mu_usgs_from_nws[g_idx] <- usgs_from_n
      fore_rows$identity_err_glofas[g_idx] <- usgs_from_g - mu_state
      fore_rows$identity_err_nws[g_idx] <- usgs_from_n - mu_state
      fore_rows$source_basis[g_idx] <- source_basis
    }
  }

  rbind(hist_rows, fore_rows)
}

summarize_multivar_vb_location_quantiles <- function(rows, tol = 1e-10) {
  if (!is.data.frame(rows) || nrow(rows) == 0L) return(data.frame())
  phases <- unique(as.character(rows$phase))
  phases <- phases[nzchar(phases)]
  do.call(rbind, lapply(phases, function(ph) {
    dd <- rows[rows$phase == ph, , drop = FALSE]
    days <- sort(unique(as.integer(dd$day_rel[is.finite(dd$day_rel)])))
    n_days <- length(days)
    n_all <- 0L
    n_cross <- 0L
    worst_gap <- NA_real_
    for (day in days) {
      di <- dd[as.integer(dd$day_rel) == day & is.finite(dd$mu_usgs), , drop = FALSE]
      if (nrow(di) < 2L) next
      di <- di[order(di$probability), , drop = FALSE]
      gaps <- diff(as.numeric(di$mu_usgs))
      if (nrow(di) >= length(unique(rows$probability))) n_all <- n_all + 1L
      if (length(gaps) > 0L && any(gaps < -tol, na.rm = TRUE)) n_cross <- n_cross + 1L
      finite_gaps <- gaps[is.finite(gaps)]
      if (length(finite_gaps) > 0L) {
        gap_min <- min(finite_gaps)
        if (!is.finite(worst_gap) || gap_min < worst_gap) worst_gap <- gap_min
      }
    }
    max_id_g <- suppressWarnings(max(abs(dd$identity_err_glofas[is.finite(dd$identity_err_glofas)]), na.rm = TRUE))
    max_id_n <- suppressWarnings(max(abs(dd$identity_err_nws[is.finite(dd$identity_err_nws)]), na.rm = TRUE))
    if (!is.finite(max_id_g)) max_id_g <- NA_real_
    if (!is.finite(max_id_n)) max_id_n <- NA_real_
    data.frame(
      phase = ph,
      n_rows = nrow(dd),
      n_days = n_days,
      n_days_all_quantiles = n_all,
      n_crossing_days = n_cross,
      crossing_share = if (n_days > 0L) n_cross / n_days else NA_real_,
      worst_adjacent_quantile_gap = worst_gap,
      max_abs_identity_err_glofas = max_id_g,
      max_abs_identity_err_nws = max_id_n,
      tol_crossing = tol,
      stringsAsFactors = FALSE
    )
  }))
}

build_multivar_vb_usgs_location_quantile_window <- function(pre_days = 30L) {
  specs <- multivar_vb_location_quantile_specs()
  rows <- list()
  missing <- character(0)
  for (i in seq_len(nrow(specs))) {
    theta_obj <- multivar_get_vb_theta_for_quantile(specs$suffix[i])
    if (!is.list(theta_obj)) {
      missing <- c(missing, specs$quantile[i])
      next
    }
    qq <- build_multivar_vb_usgs_location_rows_for_quantile(
      theta_obj = theta_obj,
      quantile_label = specs$quantile[i],
      probability = specs$probability[i],
      pre_days = pre_days
    )
    if (is.data.frame(qq) && nrow(qq) > 0L) rows[[length(rows) + 1L]] <- qq
  }
  if (length(rows) == 0L) return(NULL)
  state_df <- do.call(rbind, rows)
  rownames(state_df) <- NULL
  summary_df <- summarize_multivar_vb_location_quantiles(state_df)
  list(state_df = state_df, summary_df = summary_df, missing_quantiles = missing)
}

plot_multivar_vb_usgs_location_quantiles <- function(state_df, summary_df, out_png, out_pdf = NULL) {
  if (!is.data.frame(state_df) || nrow(state_df) == 0L) return(invisible(FALSE))
  ok_y <- is.finite(state_df$mu_usgs) | is.finite(state_df$usgs_observed)
  if (!any(ok_y)) return(invisible(FALSE))

  palette <- c(
    q05 = "#8b1a1a",
    q20 = "#d95f02",
    q35 = "#e6ab02",
    q50 = "#1b9e77",
    q65 = "#1f9fb0",
    q80 = "#386cb0",
    q95 = "#762a83"
  )
  draw_one <- function() {
    y_vals <- c(state_df$mu_usgs[ok_y], state_df$usgs_observed[ok_y])
    ylim_use <- range(y_vals, na.rm = TRUE)
    if (!all(is.finite(ylim_use)) || diff(ylim_use) <= 0) ylim_use <- c(-1, 1)
    pad <- max(0.05, diff(ylim_use) * 0.06)
    ylim_use <- ylim_use + c(-pad, pad)
    xlim_use <- range(state_df$day_rel[is.finite(state_df$day_rel)], na.rm = TRUE)
    if (!all(is.finite(xlim_use)) || diff(xlim_use) <= 0) xlim_use <- c(-30, 28)

    plot(
      NA_real_, NA_real_,
      xlim = xlim_use,
      ylim = ylim_use,
      xlab = "Day relative to cutoff (0 = T)",
      ylab = multivar_component_y_label(),
      main = "VB USGS location quantiles around cutoff (theta.out means; no sampling)"
    )
    rect(0, ylim_use[1], xlim_use[2], ylim_use[2], col = grDevices::adjustcolor("#f0f0f0", alpha.f = 0.35), border = NA)
    abline(v = 0, lty = 3, lwd = 1.2, col = "gray40")
    abline(h = pretty(ylim_use), col = grDevices::adjustcolor("gray80", alpha.f = 0.45), lwd = 0.6)

    obs <- state_df[!duplicated(paste(state_df$phase, state_df$day_rel)) & is.finite(state_df$usgs_observed), , drop = FALSE]
    hist_obs <- obs[obs$phase == "history", , drop = FALSE]
    fore_obs <- obs[obs$phase == "forecast", , drop = FALSE]
    if (nrow(hist_obs) > 0L) {
      lines(hist_obs$day_rel, hist_obs$usgs_observed, lwd = 1.4, col = "black")
      points(hist_obs$day_rel, hist_obs$usgs_observed, pch = 16, cex = 0.45, col = "black")
    }
    if (nrow(fore_obs) > 0L) {
      lines(fore_obs$day_rel, fore_obs$usgs_observed, lwd = 1.4, col = "#4d4d4d")
      points(fore_obs$day_rel, fore_obs$usgs_observed, pch = 16, cex = 0.55, col = "#4d4d4d")
    }

    q_levels <- unique(as.character(state_df$quantile))
    q_levels <- q_levels[order(suppressWarnings(as.numeric(sub("^q", "", q_levels))))]
    for (qq in q_levels) {
      dd <- state_df[state_df$quantile == qq & is.finite(state_df$mu_usgs), , drop = FALSE]
      if (nrow(dd) < 1L) next
      dd <- dd[order(dd$day_rel), , drop = FALSE]
      col <- palette[[qq]]
      if (is.null(col) || !nzchar(col)) col <- "#525252"
      lines(dd$day_rel, dd$mu_usgs, lwd = if (identical(qq, "q50")) 2.6 else 2.0, col = col)
    }

    leg_q <- q_levels[q_levels %in% names(palette)]
    legend(
      "topleft",
      legend = c(leg_q, "USGS observed/held-out"),
      col = c(unname(palette[leg_q]), "black"),
      lwd = c(rep(2.0, length(leg_q)), 1.4),
      pch = c(rep(NA, length(leg_q)), 16),
      bty = "n",
      cex = 0.82
    )
    if (is.data.frame(summary_df) && nrow(summary_df) > 0L) {
      msg <- paste(
        sprintf(
          "%s crossing=%s/%s",
          summary_df$phase,
          summary_df$n_crossing_days,
          summary_df$n_days
        ),
        collapse = "   "
      )
      mtext(msg, side = 3, line = 0.25, cex = 0.8, col = "gray30")
    }
  }

  grDevices::png(out_png, width = 3200, height = 1600, res = 300)
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_one()
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)

  if (!is.null(out_pdf) && nzchar(out_pdf)) {
    grDevices::pdf(out_pdf, width = 11, height = 5.5, onefile = TRUE)
    on.exit(grDevices::dev.off(), add = TRUE)
    draw_one()
    grDevices::dev.off()
    on.exit(NULL, add = FALSE)
  }
  invisible(TRUE)
}

draw_phase_band <- function(df_phase, y_lo, y_hi, fill_col) {
  if (!is.data.frame(df_phase) || nrow(df_phase) == 0L) return(invisible(NULL))
  ok <- is.finite(df_phase[[y_lo]]) & is.finite(df_phase[[y_hi]]) & is.finite(df_phase$day_rel)
  if (!any(ok)) return(invisible(NULL))
  xx <- df_phase$day_rel[ok]
  lo <- df_phase[[y_lo]][ok]
  hi <- df_phase[[y_hi]][ok]
  polygon(c(xx, rev(xx)), c(lo, rev(hi)), border = NA, col = fill_col)
  invisible(NULL)
}

plot_transfer_zeta_window_q50 <- function(state_df, out_file, forecast_has_transfer) {
  if (!is.data.frame(state_df) || nrow(state_df) == 0L) return(invisible(FALSE))
  ok <- is.finite(state_df$zeta_mean) | is.finite(state_df$zeta_lower_95) | is.finite(state_df$zeta_upper_95)
  if (!any(ok)) return(invisible(FALSE))

  ylim_use <- range(
    c(state_df$zeta_mean[ok], state_df$zeta_lower_95[ok], state_df$zeta_upper_95[ok]),
    na.rm = TRUE
  )
  if (!all(is.finite(ylim_use)) || diff(ylim_use) <= 0) ylim_use <- c(-1, 1)

  png(out_file, width = 3200, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  plot(
    state_df$day_rel[ok], state_df$zeta_mean[ok], type = "n",
    xlab = "Day relative to cutoff (0 = T)",
    ylab = expression(zeta[t]),
    main = "Transfer state zeta_t around cutoff (history + forecast)",
    ylim = ylim_use
  )
  abline(v = 0, lty = 3, lwd = 1.2, col = "gray45")

  hist_df <- state_df[state_df$phase == "history", , drop = FALSE]
  fore_df <- state_df[state_df$phase == "forecast", , drop = FALSE]
  draw_phase_band(hist_df, "zeta_lower_95", "zeta_upper_95", adjustcolor("#2166ac", alpha.f = 0.18))
  draw_phase_band(fore_df, "zeta_lower_95", "zeta_upper_95", adjustcolor("#b2182b", alpha.f = 0.18))

  if (any(is.finite(hist_df$zeta_mean))) lines(hist_df$day_rel, hist_df$zeta_mean, lwd = 2.4, col = "#2166ac")
  if (any(is.finite(fore_df$zeta_mean))) lines(fore_df$day_rel, fore_df$zeta_mean, lwd = 2.4, col = "#b2182b")
  if (!isTRUE(forecast_has_transfer)) {
    mtext("Forecast transfer coordinates absent (drop mode: zeta not propagated past cutoff)", side = 3, col = "#b2182b", line = 0.3)
  }
  legend(
    "topleft",
    legend = c("History zeta_t", "Forecast zeta_t", "95% interval"),
    col = c("#2166ac", "#b2182b", "gray35"),
    lwd = c(2.4, 2.4, 1.0),
    lty = c(1, 1, 2),
    bty = "n"
  )
  invisible(TRUE)
}

plot_transfer_coefficients_window_q50 <- function(psi_df, out_file, forecast_has_transfer) {
  if (!is.data.frame(psi_df) || nrow(psi_df) == 0L) return(invisible(FALSE))
  coeffs <- unique(as.character(psi_df$coefficient))
  coeffs <- coeffs[nzchar(coeffs)]
  if (length(coeffs) == 0L) return(invisible(FALSE))

  n_pan <- length(coeffs)
  n_col <- min(3L, n_pan)
  n_row <- as.integer(ceiling(n_pan / n_col))
  png(out_file, width = 3200, height = max(1200L, 900L * n_row), res = 300)
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)
  par(mfrow = c(n_row, n_col), mar = c(4.2, 4.2, 2.8, 1.2))

  for (coeff in coeffs) {
    dd <- psi_df[psi_df$coefficient == coeff, , drop = FALSE]
    ok <- is.finite(dd$mean) | is.finite(dd$lower_95) | is.finite(dd$upper_95)
    if (!any(ok)) {
      plot.new()
      title(main = sprintf("%s (no finite values)", coeff))
      next
    }
    ylim_use <- range(c(dd$mean[ok], dd$lower_95[ok], dd$upper_95[ok]), na.rm = TRUE)
    if (!all(is.finite(ylim_use)) || diff(ylim_use) <= 0) ylim_use <- c(-1, 1)

    plot(
      dd$day_rel[ok], dd$mean[ok], type = "n",
      xlab = "Day rel. cutoff",
      ylab = coeff,
      main = sprintf("Transfer coefficient %s", coeff),
      ylim = ylim_use
    )
    abline(v = 0, lty = 3, lwd = 1.0, col = "gray45")
    hdd <- dd[dd$phase == "history", , drop = FALSE]
    fdd <- dd[dd$phase == "forecast", , drop = FALSE]
    draw_phase_band(hdd, "lower_95", "upper_95", adjustcolor("#2166ac", alpha.f = 0.18))
    draw_phase_band(fdd, "lower_95", "upper_95", adjustcolor("#b2182b", alpha.f = 0.18))
    if (any(is.finite(hdd$mean))) lines(hdd$day_rel, hdd$mean, lwd = 2.0, col = "#2166ac")
    if (any(is.finite(fdd$mean))) lines(fdd$day_rel, fdd$mean, lwd = 2.0, col = "#b2182b")
  }
  if (!isTRUE(forecast_has_transfer)) {
    mtext("Forecast coefficients absent in drop mode", side = 1, outer = TRUE, line = -1.2, col = "#b2182b")
  }
  invisible(TRUE)
}

plot_transfer_observation_decomp_q50 <- function(state_df, out_file, forecast_has_transfer) {
  if (!is.data.frame(state_df) || nrow(state_df) == 0L) return(invisible(FALSE))
  ok <- is.finite(state_df$mu_usgs) |
    is.finite(state_df$mu_without_transfer) |
    is.finite(state_df$zeta_mean) |
    is.finite(state_df$trend_agg) |
    is.finite(state_df$season_agg) |
    is.finite(state_df$usgs_observed)
  if (!any(ok)) return(invisible(FALSE))

  ylim_use <- range(
    c(
      state_df$mu_usgs[ok],
      state_df$mu_without_transfer[ok],
      state_df$zeta_mean[ok],
      state_df$trend_agg[ok],
      state_df$season_agg[ok],
      state_df$usgs_observed[ok]
    ),
    na.rm = TRUE
  )
  if (!all(is.finite(ylim_use)) || diff(ylim_use) <= 0) ylim_use <- c(-1, 1)

  png(out_file, width = 3200, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  plot(
    state_df$day_rel[ok], state_df$mu_usgs[ok], type = "n",
    xlab = "Day relative to cutoff (0 = T)",
    ylab = multivar_component_y_label(),
    main = "USGS location decomposition around cutoff: total, trend, seasonal, transfer, and observed USGS",
    ylim = ylim_use
  )
  abline(v = 0, lty = 3, lwd = 1.2, col = "gray45")

  draw_phase_band(
    state_df[state_df$phase == "history", , drop = FALSE],
    "mu_usgs_lower_95",
    "mu_usgs_upper_95",
    adjustcolor("#1b7837", alpha.f = 0.14)
  )
  draw_phase_band(
    state_df[state_df$phase == "forecast", , drop = FALSE],
    "mu_usgs_lower_95",
    "mu_usgs_upper_95",
    adjustcolor("#1b7837", alpha.f = 0.10)
  )

  if (any(is.finite(state_df$mu_usgs))) lines(state_df$day_rel, state_df$mu_usgs, lwd = 2.5, col = "#1b7837")
  if (any(is.finite(state_df$mu_without_transfer))) lines(state_df$day_rel, state_df$mu_without_transfer, lwd = 2.1, col = "#762a83")
  if (any(is.finite(state_df$zeta_mean))) lines(state_df$day_rel, state_df$zeta_mean, lwd = 1.9, lty = 2, col = "#b2182b")
  if (any(is.finite(state_df$trend_agg))) lines(state_df$day_rel, state_df$trend_agg, lwd = 1.8, lty = 3, col = "#2166ac")
  if (any(is.finite(state_df$season_agg))) lines(state_df$day_rel, state_df$season_agg, lwd = 1.8, lty = 4, col = "#e08214")
  if (any(is.finite(state_df$usgs_observed))) {
    points(state_df$day_rel, state_df$usgs_observed, pch = 16, cex = 0.55, col = "black")
    lines(state_df$day_rel, state_df$usgs_observed, lwd = 0.9, col = "black")
  }

  if (!isTRUE(forecast_has_transfer)) {
    mtext("drop mode: transfer block is omitted in forecast (zeta forecast contribution is fixed to zero)", side = 3, col = "#b2182b", line = 0.3)
  }
  legend(
    "topleft",
    legend = c("mu_usgs (state-reconstructed)", "mu_usgs 95% band", "mu_without_transfer", "zeta_t", "trend (aggregated)", "seasonal (aggregated)", "USGS observed/withheld"),
    col = c("#1b7837", "#1b7837", "#762a83", "#b2182b", "#2166ac", "#e08214", "black"),
    lwd = c(2.5, 1.1, 2.1, 1.9, 1.8, 1.8, 0.9),
    lty = c(1, 2, 1, 2, 3, 4, 1),
    pch = c(NA, NA, NA, NA, NA, NA, 16),
    bty = "n"
  )
  invisible(TRUE)
}

plot_transfer_sources_window_q50 <- function(state_df, out_file, forecast_has_transfer) {
  if (!is.data.frame(state_df) || nrow(state_df) == 0L) return(invisible(FALSE))
  ok <- is.finite(state_df$mu_usgs) | is.finite(state_df$mu_glofas) | is.finite(state_df$mu_nws) | is.finite(state_df$usgs_observed)
  if (!any(ok)) return(invisible(FALSE))

  ylim_use <- range(
    c(
      state_df$mu_usgs[ok], state_df$mu_usgs_lower_95[ok], state_df$mu_usgs_upper_95[ok],
      state_df$mu_glofas[ok], state_df$mu_glofas_lower_95[ok], state_df$mu_glofas_upper_95[ok],
      state_df$mu_nws[ok], state_df$mu_nws_lower_95[ok], state_df$mu_nws_upper_95[ok],
      state_df$usgs_observed[ok]
    ),
    na.rm = TRUE
  )
  if (!all(is.finite(ylim_use)) || diff(ylim_use) <= 0) ylim_use <- c(-1, 1)

  png(out_file, width = 3200, height = 1500, res = 300)
  on.exit(dev.off(), add = TRUE)
  plot(
    state_df$day_rel[ok], state_df$mu_usgs[ok], type = "n",
    xlab = "Day relative to cutoff (0 = T)",
    ylab = multivar_component_y_label(),
    main = "Source-level location reconstruction around cutoff: USGS, GLOFAS, NWS (95% bands)",
    ylim = ylim_use
  )
  abline(v = 0, lty = 3, lwd = 1.2, col = "gray45")

  draw_phase_band(state_df, "mu_usgs_lower_95", "mu_usgs_upper_95", adjustcolor("#1b7837", alpha.f = 0.10))
  draw_phase_band(state_df, "mu_glofas_lower_95", "mu_glofas_upper_95", adjustcolor("#2166ac", alpha.f = 0.08))
  draw_phase_band(state_df, "mu_nws_lower_95", "mu_nws_upper_95", adjustcolor("#762a83", alpha.f = 0.08))

  if (any(is.finite(state_df$mu_usgs))) lines(state_df$day_rel, state_df$mu_usgs, lwd = 2.5, col = "#1b7837")
  if (any(is.finite(state_df$mu_glofas))) lines(state_df$day_rel, state_df$mu_glofas, lwd = 2.2, col = "#2166ac")
  if (any(is.finite(state_df$mu_nws))) lines(state_df$day_rel, state_df$mu_nws, lwd = 2.2, col = "#762a83")
  if (any(is.finite(state_df$usgs_observed))) {
    points(state_df$day_rel, state_df$usgs_observed, pch = 16, cex = 0.55, col = "black")
    lines(state_df$day_rel, state_df$usgs_observed, lwd = 0.9, col = "black")
  }

  if (!isTRUE(forecast_has_transfer)) {
    mtext("drop mode: transfer coordinates are not propagated after cutoff", side = 3, line = 0.3, col = "#b2182b")
  }
  legend(
    "topleft",
    legend = c("mu_usgs", "mu_glofas", "mu_nws", "USGS observed/withheld"),
    col = c("#1b7837", "#2166ac", "#762a83", "black"),
    lwd = c(2.5, 2.2, 2.2, 0.9),
    lty = c(1, 1, 1, 1),
    pch = c(NA, NA, NA, 16),
    bty = "n"
  )
  invisible(TRUE)
}

plot_transfer_discrepancy_identity_q50 <- function(state_df, out_file) {
  if (!is.data.frame(state_df) || nrow(state_df) == 0L) return(invisible(FALSE))
  ok_g <- is.finite(state_df$mu_glofas) & is.finite(state_df$mu_usgs) & is.finite(state_df$agg_discrep_glofas)
  ok_n <- is.finite(state_df$mu_nws) & is.finite(state_df$mu_usgs) & is.finite(state_df$agg_discrep_nws)
  if (!any(ok_g) && !any(ok_n)) return(invisible(FALSE))

  png(out_file, width = 3200, height = 1700, res = 300)
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)
  par(mfrow = c(2, 1), mar = c(4.0, 4.2, 2.6, 1.0))

  draw_panel <- function(ok, source_name, source_col, disc_col) {
    if (!any(ok)) {
      plot.new()
      title(main = sprintf("%s discrepancy identity (no finite points)", source_name))
      return(invisible(NULL))
    }
    x <- state_df$day_rel[ok]
    lhs <- state_df[[source_col]][ok] - state_df$mu_usgs[ok]
    rhs <- state_df[[disc_col]][ok]
    ylim_use <- range(c(lhs, rhs), na.rm = TRUE)
    if (!all(is.finite(ylim_use)) || diff(ylim_use) <= 0) ylim_use <- c(-1, 1)
    plot(x, lhs, type = "l", lwd = 2.2, col = "#2166ac",
         xlab = "Day relative to cutoff (0 = T)", ylab = "Discrepancy scale",
         main = sprintf("%s identity check: (mu_%s - mu_usgs) vs aggregated discrepancy", source_name, tolower(source_name)),
         ylim = ylim_use)
    abline(v = 0, lty = 3, lwd = 1.0, col = "gray45")
    lines(x, rhs, lwd = 2.0, lty = 2, col = "#b2182b")
    err <- lhs - rhs
    if (any(is.finite(err))) {
      max_abs <- max(abs(err), na.rm = TRUE)
      mtext(sprintf("max |lhs-rhs| = %.3e", max_abs), side = 3, line = 0.25, col = "gray30", adj = 1)
    }
    legend("topleft",
           legend = c(sprintf("mu_%s - mu_usgs", tolower(source_name)), sprintf("aggregated_discrepancy_%s", tolower(source_name))),
           col = c("#2166ac", "#b2182b"), lwd = c(2.2, 2.0), lty = c(1, 2), bty = "n")
    invisible(NULL)
  }

  draw_panel(ok_g, "GLOFAS", "mu_glofas", "agg_discrep_glofas")
  draw_panel(ok_n, "NWS", "mu_nws", "agg_discrep_nws")
  invisible(TRUE)
}

authoritative_support_env_flag <- function(name, default = FALSE) {
  raw <- tolower(trimws(Sys.getenv(name, if (isTRUE(default)) "TRUE" else "FALSE")))
  raw %in% c("1", "true", "yes", "on")
}

authoritative_support_quantile_specs <- function() {
  labels <- c("05", "20", "35", "50", "65", "80", "95")
  data.frame(
    suffix = as.character(as.integer(labels)),
    label = paste0("q", labels),
    probability = as.numeric(as.integer(labels)) / 100,
    stringsAsFactors = FALSE
  )
}

authoritative_support_theta_obj <- function(suffix) {
  get0(sprintf("new.theta.out_%s_exAL_synth_DISC", suffix), ifnotfound = NULL, inherits = TRUE)
}

authoritative_support_samp_theta <- function(suffix) {
  obj <- get0(sprintf("samp.theta_%s_exAL_synth_DISC", suffix), ifnotfound = NULL, inherits = TRUE)
  if (is.list(obj) && is.array(obj$samp_theta)) return(obj$samp_theta)
  if (is.array(obj)) return(obj)
  NULL
}

authoritative_support_dates <- function(n_time) {
  dates <- safe_get("dates_ts_usgs", NULL)
  if (is.null(dates)) dates <- safe_get("timestamps", NULL)
  dates <- suppressWarnings(as.Date(dates))
  if (length(dates) < n_time || all(is.na(dates[seq_len(n_time)]))) {
    return(as.Date(rep(NA_character_, n_time)))
  }
  dates[seq_len(n_time)]
}

authoritative_support_Ft <- function(t, p_use) {
  ff <- safe_get("FF", NULL)
  if (is.array(ff) && length(dim(ff)) == 3L && dim(ff)[3] >= t) {
    return(as.numeric(ff[seq_len(min(p_use, dim(ff)[1])), 1L, t]))
  }
  if (is.matrix(ff) && ncol(ff) >= t) {
    return(as.numeric(ff[seq_len(min(p_use, nrow(ff))), t]))
  }
  out <- rep(0, p_use)
  if (p_use > 0L) out[[1L]] <- 1
  out
}

authoritative_support_project_theta <- function(theta_obj, probs = c(0.025, 0.5, 0.975)) {
  if (!is.list(theta_obj) || !is.matrix(theta_obj$sm) || !is.array(theta_obj$sC) || length(dim(theta_obj$sC)) != 3L) {
    return(NULL)
  }
  n_time <- min(ncol(theta_obj$sm), dim(theta_obj$sC)[3])
  if (!is.finite(n_time) || n_time < 1L) return(NULL)
  rows <- vector("list", n_time)
  for (t in seq_len(n_time)) {
    p_use <- min(nrow(theta_obj$sm), dim(theta_obj$sC)[1], dim(theta_obj$sC)[2])
    Ft <- authoritative_support_Ft(t, p_use)
    p_use <- min(length(Ft), p_use)
    if (p_use < 1L) {
      rows[[t]] <- c(mu_usgs = NA_real_, sd_usgs = NA_real_, lower_025 = NA_real_, median_500 = NA_real_, upper_975 = NA_real_)
      next
    }
    Ft <- matrix(Ft[seq_len(p_use)], ncol = 1L)
    Mu <- as.numeric(theta_obj$sm[seq_len(p_use), t])
    Sigma <- matrix(
      as.numeric(theta_obj$sC[seq_len(p_use), seq_len(p_use), t]),
      nrow = p_use,
      ncol = p_use
    )
    mu <- as.numeric(crossprod(Ft, Mu))
    var <- as.numeric(t(Ft) %*% Sigma %*% Ft)
    sd <- sqrt(max(var, 0, na.rm = TRUE))
    qs <- as.numeric(stats::qnorm(probs, mean = mu, sd = sd))
    rows[[t]] <- c(mu_usgs = mu, sd_usgs = sd, lower_025 = qs[[1L]], median_500 = qs[[2L]], upper_975 = qs[[3L]])
  }
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  out$time_index <- seq_len(nrow(out))
  out
}

authoritative_support_observed_usgs <- function(n_time) {
  y <- safe_get("Y", NULL)
  if (is.matrix(y) && nrow(y) >= 1L && ncol(y) >= n_time) {
    return(as.numeric(y[1L, seq_len(n_time)]))
  }
  rep(NA_real_, n_time)
}

build_authoritative_usgs_quantile_dynamics_summary <- function() {
  specs <- authoritative_support_quantile_specs()
  rows <- list()
  for (i in seq_len(nrow(specs))) {
    theta_obj <- authoritative_support_theta_obj(specs$suffix[[i]])
    projected <- authoritative_support_project_theta(theta_obj)
    if (is.null(projected)) next
    n_time <- nrow(projected)
    projected$date <- authoritative_support_dates(n_time)
    projected$observed_usgs <- authoritative_support_observed_usgs(n_time)
    projected$quantile <- specs$label[[i]]
    projected$probability <- specs$probability[[i]]
    projected$source_object <- sprintf("new.theta.out_%s_exAL_synth_DISC", specs$suffix[[i]])
    rows[[length(rows) + 1L]] <- projected[, c(
      "date", "time_index", "quantile", "probability", "mu_usgs", "sd_usgs",
      "lower_025", "median_500", "upper_975", "observed_usgs", "source_object"
    ), drop = FALSE]
  }
  if (length(rows) == 0L) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

authoritative_support_theta_array_layout <- function(arr, n_time_hint) {
  d <- dim(arr)
  if (length(d) != 3L) return(NULL)
  if (d[2L] == n_time_hint) return(list(time_dim = 2L, sample_dim = 3L))
  if (d[3L] == n_time_hint) return(list(time_dim = 3L, sample_dim = 2L))
  if (d[2L] >= d[3L]) list(time_dim = 2L, sample_dim = 3L) else list(time_dim = 3L, sample_dim = 2L)
}

authoritative_support_component_matrix <- function(arr, component, n_time, layout) {
  if (!is.array(arr) || length(dim(arr)) != 3L || is.null(layout)) return(NULL)
  component <- suppressWarnings(as.integer(component))
  n_time <- suppressWarnings(as.integer(n_time))
  if (!is.finite(component) || component < 1L || component > dim(arr)[1L]) return(NULL)
  if (!is.finite(n_time) || n_time < 1L) return(NULL)
  if (layout$time_dim == 2L) {
    mat <- arr[component, seq_len(n_time), , drop = FALSE]
  } else {
    mat <- arr[component, , seq_len(n_time), drop = FALSE]
  }
  matrix(mat, nrow = n_time)
}

authoritative_support_component_summary_row <- function(
  mat,
  dates,
  label,
  probability,
  component,
  component_contract,
  source_object,
  probs = c(0.025, 0.5, 0.975)
) {
  if (!is.matrix(mat) || nrow(mat) == 0L) return(data.frame())
  qs <- safe_row_quantiles(mat, probs = probs)
  data.frame(
    date = dates,
    time_index = seq_len(nrow(mat)),
    quantile = label,
    probability = probability,
    component = component,
    component_contract = component_contract,
    lower_025 = as.numeric(qs[1L, ]),
    median_500 = as.numeric(qs[2L, ]),
    upper_975 = as.numeric(qs[3L, ]),
    source_object = source_object,
    stringsAsFactors = FALSE
  )
}

authoritative_support_component_summary_for_quantile <- function(suffix, label, probability, probs = c(0.025, 0.5, 0.975)) {
  arr <- authoritative_support_samp_theta(suffix)
  if (!is.array(arr) || length(dim(arr)) != 3L) return(data.frame())
  n_time_hint <- suppressWarnings(as.integer(safe_get("TT", NA_integer_)))
  if (!is.finite(n_time_hint) || n_time_hint < 1L) {
    n_time_hint <- max(dim(arr)[2L], dim(arr)[3L])
  }
  layout <- authoritative_support_theta_array_layout(arr, n_time_hint)
  if (is.null(layout)) return(data.frame())
  d <- dim(arr)
  n_time <- d[layout$time_dim]
  n_component <- min(suppressWarnings(as.integer(safe_get("p", 7L))), d[1L])
  if (!is.finite(n_component) || n_component < 1L) n_component <- min(7L, d[1L])
  dates <- authoritative_support_dates(n_time)
  rows <- list()
  source_object <- sprintf("samp.theta_%s_exAL_synth_DISC", suffix)
  for (component in seq_len(n_component)) {
    mat <- authoritative_support_component_matrix(arr, component, n_time, layout)
    rows[[length(rows) + 1L]] <- authoritative_support_component_summary_row(
      mat = mat,
      dates = dates,
      label = label,
      probability = probability,
      component = component,
      component_contract = "raw_state_component",
      source_object = source_object,
      probs = probs
    )
  }
  out <- do.call(rbind, rows)
  if (n_component >= 6L) {
    trend_mat <- authoritative_support_component_matrix(arr, 1L, n_time, layout)
    component6_mat <- authoritative_support_component_matrix(arr, 6L, n_time, layout)
    samplewise_component6_plus <- authoritative_support_component_summary_row(
      mat = trend_mat + component6_mat,
      dates = dates,
      label = label,
      probability = probability,
      component = 6L,
      component_contract = "component_6_plus_trend_component_1_samplewise",
      source_object = source_object,
      probs = probs
    )
    samplewise_component6_minus <- authoritative_support_component_summary_row(
      mat = component6_mat - trend_mat,
      dates = dates,
      label = label,
      probability = probability,
      component = 6L,
      component_contract = "component_6_minus_trend_component_1_samplewise",
      source_object = source_object,
      probs = probs
    )
    trend_shift <- rowMeans(trend_mat, na.rm = TRUE)
    component6 <- out[out$component == 6L, , drop = FALSE]
    component6$component_contract <- "component_6_shifted_by_posterior_mean_trend_component_1"
    component6$lower_025 <- component6$lower_025 + trend_shift
    component6$median_500 <- component6$median_500 + trend_shift
    component6$upper_975 <- component6$upper_975 + trend_shift
    out <- rbind(out, samplewise_component6_plus, samplewise_component6_minus, component6)
  }
  out
}

build_authoritative_component_summary <- function() {
  specs <- authoritative_support_quantile_specs()
  specs <- specs[specs$label %in% c("q05", "q50", "q95"), , drop = FALSE]
  rows <- list()
  for (i in seq_len(nrow(specs))) {
    rows[[length(rows) + 1L]] <- authoritative_support_component_summary_for_quantile(
      suffix = specs$suffix[[i]],
      label = specs$label[[i]],
      probability = specs$probability[[i]]
    )
  }
  rows <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, rows)
  if (length(rows) == 0L) return(data.frame())
  do.call(rbind, rows)
}

authoritative_component_analysis_slug <- function(component, contract) {
  contract_slug <- gsub("[^A-Za-z0-9]+", "_", as.character(contract))
  contract_slug <- gsub("^_+|_+$", "", tolower(contract_slug))
  sprintf("component_%02d_%s.png", as.integer(component), contract_slug)
}

authoritative_component_analysis_label <- function(component, contract) {
  component <- as.integer(component)
  contract <- as.character(contract)
  if (identical(contract, "component_6_plus_trend_component_1_samplewise")) {
    return("Component 6 plus trend component 1 (samplewise)")
  }
  if (identical(contract, "component_6_minus_trend_component_1_samplewise")) {
    return("Component 6 minus trend component 1 (samplewise)")
  }
  if (identical(contract, "raw_state_component")) {
    return(sprintf("Raw state component %d", component))
  }
  sprintf("Component %d (%s)", component, contract)
}

authoritative_component_analysis_specs <- function(comp) {
  if (!is.data.frame(comp) || nrow(comp) == 0L) return(data.frame())
  required <- c("component", "component_contract")
  if (!all(required %in% names(comp))) return(data.frame())

  raw_components <- sort(unique(as.integer(comp$component[comp$component_contract == "raw_state_component"])))
  raw_components <- raw_components[is.finite(raw_components)]
  rows <- list()
  for (component in raw_components) {
    rows[[length(rows) + 1L]] <- data.frame(
      component = as.integer(component),
      component_contract = "raw_state_component",
      display_label = authoritative_component_analysis_label(component, "raw_state_component"),
      filename = authoritative_component_analysis_slug(component, "raw_state_component"),
      include_in_manuscript = FALSE,
      stringsAsFactors = FALSE
    )
  }

  has_samplewise_a1 <- any(
    comp$component == 6L &
      comp$component_contract == "component_6_plus_trend_component_1_samplewise",
    na.rm = TRUE
  )
  if (isTRUE(has_samplewise_a1)) {
    contract <- "component_6_plus_trend_component_1_samplewise"
    rows[[length(rows) + 1L]] <- data.frame(
      component = 6L,
      component_contract = contract,
      display_label = authoritative_component_analysis_label(6L, contract),
      filename = authoritative_component_analysis_slug(6L, contract),
      include_in_manuscript = FALSE,
      stringsAsFactors = FALSE
    )
  }

  has_samplewise_minus <- any(
    comp$component == 6L &
      comp$component_contract == "component_6_minus_trend_component_1_samplewise",
    na.rm = TRUE
  )
  if (isTRUE(has_samplewise_minus)) {
    contract <- "component_6_minus_trend_component_1_samplewise"
    rows[[length(rows) + 1L]] <- data.frame(
      component = 6L,
      component_contract = contract,
      display_label = authoritative_component_analysis_label(6L, contract),
      filename = authoritative_component_analysis_slug(6L, contract),
      include_in_manuscript = FALSE,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L) return(data.frame())
  do.call(rbind, rows)
}

authoritative_component_analysis_regime_periods <- function() {
  data.frame(
    xmin = as.Date(c("2012-01-01", "2017-01-01")),
    xmax = as.Date(c("2016-12-31", "2019-12-31")),
    period = c("Dry", "Wet"),
    fill = c("#fff0b3", "#cfe8f7"),
    stringsAsFactors = FALSE
  )
}

authoritative_component_analysis_axis_label <- function(contract) {
  if (identical(as.character(contract), "raw_state_component")) {
    return(sprintf("State component (%s)", multivar_component_analysis_scale()))
  }
  multivar_component_y_label()
}

plot_authoritative_component_analysis_figure <- function(dd, obs, spec, out_file) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required to render authoritative component analysis figures", call. = FALSE)
  }
  if (!is.data.frame(dd) || nrow(dd) == 0L) {
    stop(sprintf("No component rows available for %s", spec$display_label[[1L]]), call. = FALSE)
  }

  dd$date <- as.Date(dd$date)
  max_time <- suppressWarnings(max(dd$time_index, na.rm = TRUE))
  if (is.finite(max_time) && max_time > 0L) {
    min_time <- ceiling(max_time / 10)
    dd <- dd[dd$time_index >= min_time, , drop = FALSE]
  }
  if (nrow(dd) == 0L) {
    stop(sprintf("No component rows remain after warm-history trim for %s", spec$display_label[[1L]]), call. = FALSE)
  }

  obs <- obs[!is.na(obs$date) & is.finite(obs$observed_usgs), , drop = FALSE]
  obs <- obs[obs$date >= min(dd$date, na.rm = TRUE) & obs$date <= max(dd$date, na.rm = TRUE), , drop = FALSE]

  ylim <- range(c(dd$lower_025, dd$upper_975, obs$observed_usgs), na.rm = TRUE)
  if (!all(is.finite(ylim)) || diff(ylim) <= 0) ylim <- c(0, 1)
  ylim <- c(min(0, ylim[[1L]]), ylim[[2L]] + diff(ylim) * 0.08)

  shade_periods <- authoritative_component_analysis_regime_periods()
  shade_periods <- shade_periods[
    shade_periods$xmax >= min(dd$date, na.rm = TRUE) &
      shade_periods$xmin <= max(dd$date, na.rm = TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(shade_periods) > 0L) {
    shade_periods$xmin <- pmax(shade_periods$xmin, min(dd$date, na.rm = TRUE))
    shade_periods$xmax <- pmin(shade_periods$xmax, max(dd$date, na.rm = TRUE))
  }

  col <- c(q05 = "#b2182b", q50 = "#238b45", q95 = "#2171b5")
  fill <- c(q05 = "#fdbba1", q50 = "#b2df8a", q95 = "#a6bddb")

  p <- ggplot2::ggplot()
  if (nrow(shade_periods) > 0L) {
    p <- p +
      ggplot2::geom_rect(
        data = shade_periods,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
        alpha = 0.48,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }
  p <- p +
    ggplot2::geom_ribbon(
      data = dd,
      ggplot2::aes(x = date, ymin = lower_025, ymax = upper_975, fill = quantile),
      alpha = 0.12
    ) +
    ggplot2::geom_line(data = dd, ggplot2::aes(x = date, y = median_500, color = quantile), linewidth = 0.45) +
    ggplot2::geom_line(data = dd, ggplot2::aes(x = date, y = lower_025, color = quantile), linewidth = 0.12) +
    ggplot2::geom_line(data = dd, ggplot2::aes(x = date, y = upper_975, color = quantile), linewidth = 0.12) +
    ggplot2::geom_line(data = obs, ggplot2::aes(x = date, y = observed_usgs), color = "black", linewidth = 0.12) +
    ggplot2::geom_point(data = obs, ggplot2::aes(x = date, y = observed_usgs), color = "black", size = 0.1, alpha = 0.9) +
    ggplot2::scale_color_manual(values = col, breaks = c("q05", "q50", "q95")) +
    ggplot2::scale_fill_manual(values = c(fill, stats::setNames(shade_periods$fill, shade_periods$period))) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::scale_x_date(date_breaks = "24 months", date_labels = "%Y-%m") +
    ggplot2::labs(
      title = sprintf("%s: selected model", spec$display_label[[1L]]),
      x = NULL,
      y = authoritative_component_analysis_axis_label(spec$component_contract[[1L]])
    )

  if (nrow(shade_periods) > 0L) {
    label_y <- ylim[[1L]] + 0.035 * diff(ylim)
    p <- p +
      ggplot2::annotate(
        "text",
        x = shade_periods$xmin + (shade_periods$xmax - shade_periods$xmin) / 2,
        y = label_y,
        label = shade_periods$period,
        size = 3.4,
        color = "#555555",
        fontface = "italic"
      )
  }

  if (exists("theme_manuscript_standard", mode = "function", inherits = TRUE)) {
    p <- p + theme_manuscript_standard(
      base_size = 15,
      title_size = 16,
      legend_position = "none",
      axis_text_y_size = 12,
      x_angle = 35,
      major_grid_x = TRUE,
      major_grid_y = TRUE,
      plot_margin = ggplot2::margin(12, 12, 12, 12)
    )
  } else {
    p <- p + ggplot2::theme_bw(base_size = 15) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
        plot.margin = ggplot2::margin(12, 12, 12, 12)
      )
  }

  ggplot2::ggsave(out_file, plot = p, width = 12, height = 6, units = "in", dpi = 350)
  invisible(TRUE)
}

write_authoritative_component_analysis_readme <- function(out_dir, manifest) {
  readme_path <- file.path(out_dir, "README.md")
  n_figures <- if (is.data.frame(manifest)) nrow(manifest) else 0L
  writeLines(
    c(
      "# Authoritative Component Analysis Figures",
      "",
      "This analysis-only gallery is rendered automatically from the compact selected-model support tables.",
      "It is not a manuscript asset family and should not be added to the article figure manifest by default.",
      "",
      "Included contracts:",
      "",
      "- `raw_state_component` for every retained state component present in the support CSV.",
      "- `component_6_plus_trend_component_1_samplewise`, the samplewise 80-month component plus trend diagnostic.",
      "- `component_6_minus_trend_component_1_samplewise`, the samplewise 80-month component minus trend diagnostic.",
      "",
      "The older `component_6_shifted_by_posterior_mean_trend_component_1` diagnostic rows are intentionally excluded from the automatic gallery.",
      "",
      sprintf("Rendered figures: %d", as.integer(n_figures))
    ),
    con = readme_path
  )
  readme_path
}

write_authoritative_component_analysis_figures <- function(dyn, comp, root_dir = OUT_DIR) {
  if (!is.data.frame(dyn) || nrow(dyn) == 0L) {
    stop("authoritative dynamics support is required for component analysis figures", call. = FALSE)
  }
  if (!is.data.frame(comp) || nrow(comp) == 0L) {
    stop("authoritative component support is required for component analysis figures", call. = FALSE)
  }
  required_comp <- c("date", "time_index", "quantile", "component", "component_contract", "lower_025", "median_500", "upper_975")
  required_dyn <- c("date", "quantile", "observed_usgs")
  missing_comp <- setdiff(required_comp, names(comp))
  missing_dyn <- setdiff(required_dyn, names(dyn))
  if (length(missing_comp) > 0L) {
    stop(sprintf("component support is missing required columns: %s", paste(missing_comp, collapse = ", ")), call. = FALSE)
  }
  if (length(missing_dyn) > 0L) {
    stop(sprintf("dynamics support is missing required columns: %s", paste(missing_dyn, collapse = ", ")), call. = FALSE)
  }

  out_dir <- file.path(root_dir, "analysis_figures", "component_evolution")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  specs <- authoritative_component_analysis_specs(comp)
  if (!is.data.frame(specs) || nrow(specs) == 0L) {
    stop("no component analysis figure specifications were available", call. = FALSE)
  }

  dyn$date <- as.Date(dyn$date)
  comp$date <- as.Date(comp$date)
  obs <- dyn[dyn$quantile == "q50", c("date", "observed_usgs"), drop = FALSE]

  rows <- list()
  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, , drop = FALSE]
    dd <- comp[
      comp$quantile %in% c("q05", "q50", "q95") &
        comp$component == spec$component[[1L]] &
        comp$component_contract == spec$component_contract[[1L]] &
        !is.na(comp$date),
      ,
      drop = FALSE
    ]
    out_file <- file.path(out_dir, spec$filename[[1L]])
    plot_authoritative_component_analysis_figure(dd, obs, spec, out_file)
    rows[[length(rows) + 1L]] <- data.frame(
      component = as.integer(spec$component[[1L]]),
      component_contract = spec$component_contract[[1L]],
      display_label = spec$display_label[[1L]],
      filename = spec$filename[[1L]],
      relative_path = file.path("analysis_figures", "component_evolution", spec$filename[[1L]]),
      include_in_manuscript = FALSE,
      rows = as.integer(nrow(dd)),
      start_date = as.character(min(dd$date, na.rm = TRUE)),
      end_date = as.character(max(dd$date, na.rm = TRUE)),
      rendered_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      stringsAsFactors = FALSE
    )
  }

  manifest <- do.call(rbind, rows)
  manifest_path <- file.path(out_dir, "component_analysis_manifest.csv")
  write.csv(manifest, manifest_path, row.names = FALSE)
  write_authoritative_component_analysis_readme(out_dir, manifest)
  manifest
}

write_authoritative_selected_support <- function() {
  enabled <- authoritative_support_env_flag("UNIFIED_POST_AUTHORITATIVE_SELECTED_SUPPORT", default = FALSE)
  fail_fast <- authoritative_support_env_flag("UNIFIED_POST_AUTHORITATIVE_SELECTED_SUPPORT_FAIL_FAST", default = TRUE)
  if (!isTRUE(enabled)) return(invisible(FALSE))

  status <- data.frame(
    artifact = character(0),
    status = character(0),
    rows = integer(0),
    path = character(0),
    detail = character(0),
    stringsAsFactors = FALSE
  )
  add_status <- function(artifact, ok, rows, path, detail = "") {
    status <<- rbind(status, data.frame(
      artifact = artifact,
      status = if (isTRUE(ok)) "pass" else "fail",
      rows = as.integer(rows),
      path = as.character(path),
      detail = as.character(detail),
      stringsAsFactors = FALSE
    ))
  }

  dyn <- build_authoritative_usgs_quantile_dynamics_summary()
  comp <- build_authoritative_component_summary()

  dyn_csv <- file.path(OUT_DIR, "authoritative_usgs_quantile_dynamics_summary.csv")
  dyn_rds <- file.path(OUT_DIR, "authoritative_usgs_quantile_dynamics_summary.rds")
  comp_csv <- file.path(OUT_DIR, "authoritative_component_summary.csv")
  comp_rds <- file.path(OUT_DIR, "authoritative_component_summary.rds")
  lineage_csv <- file.path(OUT_DIR, "authoritative_selected_support_lineage.csv")
  manifest_json <- file.path(OUT_DIR, "authoritative_selected_support_manifest.json")

  if (is.data.frame(dyn) && nrow(dyn) > 0L) {
    write.csv(dyn, dyn_csv, row.names = FALSE)
    saveRDS(dyn, dyn_rds)
    add_status("authoritative_usgs_quantile_dynamics_summary", TRUE, nrow(dyn), dyn_csv)
  } else {
    add_status("authoritative_usgs_quantile_dynamics_summary", FALSE, 0L, dyn_csv, "no dynamic rows were generated")
  }

  if (is.data.frame(comp) && nrow(comp) > 0L) {
    write.csv(comp, comp_csv, row.names = FALSE)
    saveRDS(comp, comp_rds)
    add_status("authoritative_component_summary", TRUE, nrow(comp), comp_csv)
  } else {
    add_status("authoritative_component_summary", FALSE, 0L, comp_csv, "no component rows were generated")
  }

  analysis_manifest <- data.frame()
  analysis_manifest_path <- file.path(OUT_DIR, "analysis_figures", "component_evolution", "component_analysis_manifest.csv")
  if (is.data.frame(dyn) && nrow(dyn) > 0L && is.data.frame(comp) && nrow(comp) > 0L) {
    analysis_result <- tryCatch(
      write_authoritative_component_analysis_figures(dyn, comp, OUT_DIR),
      error = function(e) e
    )
    if (inherits(analysis_result, "error")) {
      add_status("authoritative_component_analysis_figures", FALSE, 0L, analysis_manifest_path, conditionMessage(analysis_result))
    } else {
      analysis_manifest <- analysis_result
      add_status("authoritative_component_analysis_figures", TRUE, nrow(analysis_manifest), analysis_manifest_path)
    }
  } else {
    add_status(
      "authoritative_component_analysis_figures",
      FALSE,
      0L,
      analysis_manifest_path,
      "requires non-empty dynamics and component support"
    )
  }

  lineage <- data.frame(
    run_id = as.character(safe_get("RUN_ID", Sys.getenv("RUN_ID", "")))[1L],
    run_root = as.character(safe_get("RUN_ROOT", Sys.getenv("UNIFIED_RUN_ROOT", "")))[1L],
    cutoff_date = Sys.getenv("UNIFIED_CUTOFF_DATE", ""),
    forecast_start_date = Sys.getenv("UNIFIED_FORECAST_START_DATE", ""),
    scale_contract = multivar_component_analysis_scale(),
    active_quantiles = Sys.getenv("UNIFIED_FIT_QUANTILE_LABELS", ""),
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  write.csv(lineage, lineage_csv, row.names = FALSE)
  add_status("authoritative_selected_support_lineage", TRUE, nrow(lineage), lineage_csv)

  manifest <- list(
    artifact_family = "authoritative_selected_model_support",
    run_id = lineage$run_id[[1L]],
    run_root = lineage$run_root[[1L]],
    cutoff_date = lineage$cutoff_date[[1L]],
    forecast_start_date = lineage$forecast_start_date[[1L]],
    scale_contract = lineage$scale_contract[[1L]],
    active_quantiles = lineage$active_quantiles[[1L]],
    representative_quantiles = c("q05", "q50", "q95"),
    dynamics_rows = if (is.data.frame(dyn)) nrow(dyn) else 0L,
    component_rows = if (is.data.frame(comp)) nrow(comp) else 0L,
    files = list(
      dynamics_csv = basename(dyn_csv),
      dynamics_rds = basename(dyn_rds),
      component_csv = basename(comp_csv),
      component_rds = basename(comp_rds),
      lineage_csv = basename(lineage_csv)
    ),
    analysis_component_figures = list(
      directory = file.path("analysis_figures", "component_evolution"),
      manifest = file.path("analysis_figures", "component_evolution", "component_analysis_manifest.csv"),
      figure_count = if (is.data.frame(analysis_manifest)) as.integer(nrow(analysis_manifest)) else 0L,
      files = if (is.data.frame(analysis_manifest) && "filename" %in% names(analysis_manifest)) as.character(analysis_manifest$filename) else character(0)
    ),
    generated_at_utc = lineage$generated_at_utc[[1L]]
  )
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(manifest, path = manifest_json, auto_unbox = TRUE, pretty = TRUE)
  } else {
    writeLines("{\"artifact_family\":\"authoritative_selected_model_support\"}", manifest_json)
  }
  add_status("authoritative_selected_support_manifest", TRUE, 1L, manifest_json)

  status_path <- file.path(OUT_DIR, "authoritative_selected_support_status.csv")
  write.csv(status, status_path, row.names = FALSE)
  failed <- status$status != "pass"
  if (any(failed) && isTRUE(fail_fast)) {
    stop(
      sprintf(
        "authoritative selected-model support export failed: %s",
        paste(status$artifact[failed], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(!any(failed))
}

plot_trace_lines <- function(values, title_txt, ylab_txt, file_name, line_cols = NULL) {
  mat <- as_trace_matrix(values)
  if (ncol(mat) == 0L) return(invisible(FALSE))
  if (is.null(line_cols)) {
    line_cols <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")
  }
  keep <- seq_len(nrow(mat))
  cols <- line_cols[(keep - 1L) %% length(line_cols) + 1L]
  out_file <- file.path(OUT_DIR, file_name)
  png(out_file, width = 2800, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  mat[, 1L] <- NA_real_
  ylim_use <- range(mat, na.rm = TRUE, finite = TRUE)
  if (!all(is.finite(ylim_use)) || diff(ylim_use) <= 0) ylim_use <- c(-1, 1)
  plot(seq_len(ncol(mat)), mat[1, ], type = "l", lwd = 2, col = cols[1],
       xlab = "Iteration", ylab = ylab_txt, main = title_txt, ylim = ylim_use)
  if (nrow(mat) > 1L) {
    for (i in 2:nrow(mat)) {
      lines(seq_len(ncol(mat)), mat[i, ], lwd = 1.7, col = cols[i])
    }
  }
  legend("topright",
         legend = paste0("Series ", seq_len(nrow(mat))),
         col = cols, lwd = 2, bty = "n")
  invisible(TRUE)
}

run_multivar_vb_latent_audit_report <- function() {
  enabled <- tolower(trimws(Sys.getenv("UNIFIED_POST_MULTIVAR_VB_LATENT_AUDIT", "TRUE")))
  if (enabled %in% c("0", "false", "no", "off")) return(invisible(FALSE))

  run_root <- as.character(safe_get("RUN_ROOT", ""))[1L]
  project_root <- as.character(safe_get("PROJECT_ROOT", getwd()))[1L]
  rdata_paths <- as.character(safe_get("DISC_W_RDATA_PATHS", character(0)))
  rdata_paths <- rdata_paths[nzchar(rdata_paths)]
  if (!nzchar(run_root) || !dir.exists(run_root) || !length(rdata_paths)) {
    return(invisible(FALSE))
  }

  script_path <- file.path(project_root, "scripts", "audit_exdqlm_multivar_keep_vb_latents.R")
  if (!file.exists(script_path)) {
    warning(sprintf("multivar VB latent audit script not found: %s", script_path), call. = FALSE)
    return(invisible(FALSE))
  }

  out_dir <- file.path(OUT_DIR, "vb_latent_component_audit")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(out_dir, "audit_rscript.log")
  err_path <- file.path(out_dir, "audit_rscript.err")
  status_path <- file.path(OUT_DIR, "multivar_vb_latent_audit_status.csv")
  window_days <- suppressWarnings(as.integer(Sys.getenv(
    "UNIFIED_POST_MULTIVAR_LATENT_AUDIT_WINDOW_DAYS",
    as.character(max(90L, multivar_component_pre_days()))
  )))
  if (!is.finite(window_days) || window_days < 1L) window_days <- 90L

  cmd <- file.path(R.home("bin"), "Rscript")
  args <- c(
    "--vanilla",
    script_path,
    "--run-root", run_root,
    "--out-dir", out_dir,
    "--window-days", as.character(window_days)
  )
  started <- Sys.time()
  status <- suppressWarnings(system2(cmd, args = args, stdout = log_path, stderr = err_path))
  finished <- Sys.time()
  if (is.null(status)) status <- 0L
  status <- suppressWarnings(as.integer(status)[1L])
  ok <- identical(status, 0L)
  status_df <- data.frame(
    audit_enabled = TRUE,
    ok = ok,
    exit_status = status,
    run_root = run_root,
    out_dir = out_dir,
    window_days = as.integer(window_days),
    log_path = log_path,
    err_path = err_path,
    started_at = format(started, "%Y-%m-%d %H:%M:%S %Z"),
    finished_at = format(finished, "%Y-%m-%d %H:%M:%S %Z"),
    stringsAsFactors = FALSE
  )
  write.csv(status_df, status_path, row.names = FALSE)

  strict <- tolower(trimws(Sys.getenv("UNIFIED_POST_MULTIVAR_VB_LATENT_AUDIT_STRICT", "FALSE")))
  if (!ok) {
    msg <- sprintf("multivar VB latent audit failed with status %d; see %s and %s", status, log_path, err_path)
    if (strict %in% c("1", "true", "yes", "on")) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
  }
  invisible(ok)
}

profile_section("figures_multivar_only.authoritative_selected_support", {
  write_authoritative_selected_support()
})

profile_section("figures_multivar_only.trace_plots", {
  plot_trace_lines(
    values = safe_get("seq.elbo_50_exAL_synth_DISC", NULL),
    title_txt = "Multivariate exDQLM ELBO Trace (q=50)",
    ylab_txt = "ELBO",
    file_name = "multivar_elbo_trace_q50.png",
    line_cols = "#1b7837"
  )
  plot_trace_lines(
    values = safe_get("seq.sigma_50_exAL_synth_DISC", NULL),
    title_txt = "Multivariate exDQLM Sigma Traces (q=50)",
    ylab_txt = "sigma",
    file_name = "multivar_sigma_traces_q50.png"
  )
  plot_trace_lines(
    values = safe_get("seq.gamma_50_exAL_synth_DISC", NULL),
    title_txt = "Multivariate exDQLM Gamma Traces (q=50)",
    ylab_txt = "gamma",
    file_name = "multivar_gamma_traces_q50.png"
  )
})

profile_section("figures_multivar_only.trace_summary", {
  trace_summary <- data.frame(
    parameter = character(0),
    component = integer(0),
    iter_n = integer(0),
    first = numeric(0),
    last = numeric(0),
    mean = numeric(0),
    sd = numeric(0),
    stringsAsFactors = FALSE
  )

  append_trace_rows <- function(param_name, obj_name) {
    mat <- as_trace_matrix(safe_get(obj_name, NULL))
    if (ncol(mat) == 0L) return(invisible(NULL))
    for (i in seq_len(nrow(mat))) {
      vals <- as.numeric(mat[i, ])
      vals <- vals[is.finite(vals)]
      if (length(vals) == 0L) next
      trace_summary[nrow(trace_summary) + 1L, ] <<- list(
        param_name, i, length(vals), vals[1], vals[length(vals)], mean(vals), stats::sd(vals)
      )
    }
    invisible(NULL)
  }

  append_trace_rows("elbo", "seq.elbo_50_exAL_synth_DISC")
  append_trace_rows("sigma", "seq.sigma_50_exAL_synth_DISC")
  append_trace_rows("gamma", "seq.gamma_50_exAL_synth_DISC")

  if (nrow(trace_summary) > 0L) {
    write.csv(trace_summary, file.path(OUT_DIR, "multivar_trace_summary_q50.csv"), row.names = FALSE)
  }
})

profile_section("figures_multivar_only.fit_and_forecast", {
  fs <- build_multivar_q50_forecast_summary()
  if (is.null(fs)) return(invisible(NULL))
  scale_ylab <- multivar_component_y_label()

  idx_fit <- seq_len(fs$fit_n)
  fit_obs <- as.numeric(fs$fit_obs)
  fit_mu <- as.numeric(fs$fit_mu)

  out_fit <- file.path(OUT_DIR, "multivar_fit_mu_vs_observed_loglog.png")
  png(out_fit, width = 3000, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  y_min <- min(c(fit_obs, fit_mu), na.rm = TRUE)
  y_max <- max(c(fit_obs, fit_mu), na.rm = TRUE)
  plot(idx_fit, fit_obs, type = "p", pch = 16, cex = 0.35, col = "gray25",
       xlab = "Time index", ylab = scale_ylab,
       main = "Multivariate exDQLM expected location vs observed (in-sample)",
       ylim = c(y_min, y_max))
  lines(idx_fit, fit_mu, col = "#1b7837", lwd = 2.2)
  legend("topright",
         legend = c("Observed USGS", "mu_t (q=50)"),
         col = c("gray25", "#1b7837"),
         pch = c(16, NA), lwd = c(NA, 2.2), bty = "n")

  recent_n <- min(900L, fs$fit_n)
  idx_recent <- seq.int(fs$fit_n - recent_n + 1L, fs$fit_n)
  out_fit_recent <- file.path(OUT_DIR, "multivar_fit_mu_vs_observed_recent_loglog.png")
  png(out_fit_recent, width = 3000, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  y_min_r <- min(c(fit_obs[idx_recent], fit_mu[idx_recent]), na.rm = TRUE)
  y_max_r <- max(c(fit_obs[idx_recent], fit_mu[idx_recent]), na.rm = TRUE)
  plot(idx_recent, fit_obs[idx_recent], type = "p", pch = 16, cex = 0.55, col = "gray25",
       xlab = "Time index", ylab = scale_ylab,
       main = sprintf("Multivariate exDQLM expected location vs observed (recent %d points)", recent_n),
       ylim = c(y_min_r, y_max_r))
  lines(idx_recent, fit_mu[idx_recent], col = "#1b7837", lwd = 2.3)
  legend("topright",
         legend = c("Observed USGS", "mu_t (q=50)"),
         col = c("gray25", "#1b7837"),
         pch = c(16, NA), lwd = c(NA, 2.3), bty = "n")

  h <- fs$horizon
  x_f <- seq_len(h)
  mu_f <- as.numeric(fs$mu_forecast)
  lo_f <- as.numeric(fs$lower_forecast)
  up_f <- as.numeric(fs$upper_forecast)
  truth <- as.numeric(fs$truth_future)
  glofas_mean <- as.numeric(fs$glofas_mean)
  nws_mean <- as.numeric(fs$nws_mean)
  mu_minus_zeta_f <- rep(NA_real_, h)
  forecast_has_transfer <- FALSE

  transfer_payload <- build_transfer_state_window_q50(pre_days = 0L)
  if (!is.null(transfer_payload) && is.data.frame(transfer_payload$state_df)) {
    state_df <- transfer_payload$state_df
    forecast_has_transfer <- isTRUE(transfer_payload$meta$forecast_has_transfer)
    sf <- state_df[state_df$phase == "forecast", , drop = FALSE]
    if (nrow(sf) > 0L) {
      idx <- suppressWarnings(as.integer(sf$day_rel))
      use <- is.finite(idx) & idx >= 1L & idx <= h
      if (any(use)) {
        mu_minus_zeta_f[idx[use]] <- as.numeric(sf$mu_without_transfer[use])
      }
    }
  }
  fill <- !is.finite(mu_minus_zeta_f)
  if (any(fill)) {
    # In drop mode transfer is not propagated in forecast; mu_t - zeta_t collapses to mu_t.
    mu_minus_zeta_f[fill] <- mu_f[fill]
  }

  forecast_df <- data.frame(
    forecast_day = x_f,
    mu_q50 = mu_f,
    mu_minus_zeta_q50 = mu_minus_zeta_f,
    lower_95 = lo_f,
    upper_95 = up_f,
    usgs_future_withheld = truth,
    glofas_mean = glofas_mean,
    nws_mean = nws_mean
  )
  write.csv(forecast_df, file.path(OUT_DIR, "multivar_forecast_window_q50_summary.csv"), row.names = FALSE)

  valid_mu <- is.finite(mu_f) & is.finite(truth)
  valid_g <- is.finite(glofas_mean) & is.finite(truth)
  valid_n <- is.finite(nws_mean) & is.finite(truth)
  coverage <- is.finite(lo_f) & is.finite(up_f) & is.finite(truth) & truth >= lo_f & truth <= up_f
  metric_df <- data.frame(
    metric = c("n_overlap_mu", "mae_mu", "rmse_mu", "coverage_mu_95", "n_overlap_glofas_mean", "mae_glofas_mean", "n_overlap_nws_mean", "mae_nws_mean"),
    value = c(
      sum(valid_mu),
      if (any(valid_mu)) mean(abs(mu_f[valid_mu] - truth[valid_mu])) else NA_real_,
      if (any(valid_mu)) sqrt(mean((mu_f[valid_mu] - truth[valid_mu])^2)) else NA_real_,
      if (any(is.finite(coverage))) mean(coverage, na.rm = TRUE) else NA_real_,
      sum(valid_g),
      if (any(valid_g)) mean(abs(glofas_mean[valid_g] - truth[valid_g])) else NA_real_,
      sum(valid_n),
      if (any(valid_n)) mean(abs(nws_mean[valid_n] - truth[valid_n])) else NA_real_
    )
  )
  write.csv(metric_df, file.path(OUT_DIR, "multivar_forecast_window_q50_metrics.csv"), row.names = FALSE)

  out_mu <- file.path(OUT_DIR, "multivar_forecast_window_mu_vs_future_usgs.png")
  png(out_mu, width = 3000, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  y_min_f <- min(c(lo_f, up_f, mu_f, truth), na.rm = TRUE)
  y_max_f <- max(c(lo_f, up_f, mu_f, truth), na.rm = TRUE)
  plot(x_f, mu_f, type = "l", lwd = 2.4, col = "#1b7837",
       xlab = "Forecast day", ylab = scale_ylab,
       main = "Forecast window: multivariate exDQLM mu_t (q=50) vs future USGS",
       ylim = c(y_min_f, y_max_f))
  if (any(is.finite(lo_f)) && any(is.finite(up_f))) {
    polygon(
      x = c(x_f, rev(x_f)),
      y = c(lo_f, rev(up_f)),
      border = NA,
      col = adjustcolor("#1b7837", alpha.f = 0.18)
    )
    lines(x_f, lo_f, lty = 2, lwd = 1.2, col = "#1b7837")
    lines(x_f, up_f, lty = 2, lwd = 1.2, col = "#1b7837")
    lines(x_f, mu_f, lwd = 2.4, col = "#1b7837")
  }
  points(x_f, truth, pch = 16, cex = 0.8, col = "black")
  lines(x_f, truth, lwd = 1.1, col = "black")
  legend("topleft",
         legend = c("Multivar mu_t q=50", "Multivar q50 95% band", "Future USGS (withheld)"),
         col = c("#1b7837", "#1b7837", "black"),
         lty = c(1, 2, 1), lwd = c(2.4, 1.2, 1.1), pch = c(NA, NA, 16), bty = "n")

  out_mu_minus_zeta <- file.path(OUT_DIR, "multivar_forecast_window_mu_minus_zeta_vs_future_usgs.png")
  png(out_mu_minus_zeta, width = 3000, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  y_min_mz <- min(c(mu_minus_zeta_f, mu_f, truth), na.rm = TRUE)
  y_max_mz <- max(c(mu_minus_zeta_f, mu_f, truth), na.rm = TRUE)
  plot(x_f, mu_minus_zeta_f, type = "l", lwd = 2.6, col = "#762a83",
       xlab = "Forecast day", ylab = scale_ylab,
       main = "Forecast window: multivariate exDQLM (mu_t - zeta_t) vs future USGS",
       ylim = c(y_min_mz, y_max_mz))
  lines(x_f, mu_f, lwd = 1.6, lty = 3, col = "#1b7837")
  points(x_f, truth, pch = 16, cex = 0.8, col = "black")
  lines(x_f, truth, lwd = 1.1, col = "black")
  if (!isTRUE(forecast_has_transfer)) {
    mtext("drop mode: transfer omitted in forecast, so mu_t - zeta_t aligns with mu_t", side = 3, line = 0.3, col = "#762a83")
  }
  legend("topleft",
         legend = c("mu_t - zeta_t (q50 dynamic)", "mu_t (q50)", "Future USGS (withheld)"),
         col = c("#762a83", "#1b7837", "black"),
         lty = c(1, 3, 1), lwd = c(2.6, 1.6, 1.1), pch = c(NA, NA, 16), bty = "n")

  out_vs_ens <- file.path(OUT_DIR, "multivar_forecast_window_multivar_vs_ensembles.png")
  png(out_vs_ens, width = 3000, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  y_min_e <- min(c(lo_f, up_f, mu_f, glofas_mean, nws_mean, truth), na.rm = TRUE)
  y_max_e <- max(c(lo_f, up_f, mu_f, glofas_mean, nws_mean, truth), na.rm = TRUE)
  plot(x_f, mu_f, type = "l", lwd = 2.5, col = "#1b7837",
       xlab = "Forecast day", ylab = scale_ylab,
       main = "Forecast window: multivariate exDQLM vs ensemble means",
       ylim = c(y_min_e, y_max_e))
  lines(x_f, lo_f, lty = 2, lwd = 1.1, col = "#1b7837")
  lines(x_f, up_f, lty = 2, lwd = 1.1, col = "#1b7837")
  lines(x_f, glofas_mean, col = "#2166ac", lwd = 1.8)
  lines(x_f, nws_mean, col = "#762a83", lwd = 1.8)
  points(x_f, truth, pch = 16, cex = 0.8, col = "black")
  lines(x_f, truth, lwd = 1.1, col = "black")
  legend("topleft",
         legend = c("Multivar mu_t q=50", "Multivar q50 95% band", "GLOFAS ensemble mean", "NWS ensemble mean", "Future USGS (withheld)"),
         col = c("#1b7837", "#1b7837", "#2166ac", "#762a83", "black"),
         lty = c(1, 2, 1, 1, 1), lwd = c(2.5, 1.1, 1.8, 1.8, 1.1), pch = c(NA, NA, NA, NA, 16), bty = "n")

  out_members <- file.path(OUT_DIR, "multivar_forecast_window_ensemble_members.png")
  png(out_members, width = 3000, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)
  gmat <- as.matrix(fs$glofas_members)
  nmat <- as.matrix(fs$nws_members)
  y_min_m <- min(c(gmat, nmat, lo_f, up_f, mu_f, truth), na.rm = TRUE)
  y_max_m <- max(c(gmat, nmat, lo_f, up_f, mu_f, truth), na.rm = TRUE)
  plot(x_f, mu_f, type = "l", lwd = 2.6, col = "#1b7837",
       xlab = "Forecast day", ylab = scale_ylab,
       main = "Forecast window: ensemble members + multivariate q50 (95% band) + future USGS",
       ylim = c(y_min_m, y_max_m))
  if (any(is.finite(lo_f)) && any(is.finite(up_f))) {
    polygon(
      x = c(x_f, rev(x_f)),
      y = c(lo_f, rev(up_f)),
      border = NA,
      col = adjustcolor("#1b7837", alpha.f = 0.14)
    )
    lines(x_f, lo_f, lty = 2, lwd = 1.0, col = "#1b7837")
    lines(x_f, up_f, lty = 2, lwd = 1.0, col = "#1b7837")
  }
  if (ncol(gmat) > 0L) {
    matlines(seq_len(nrow(gmat)), gmat, lty = 1, lwd = 0.5, col = adjustcolor("#2166ac", alpha.f = 0.28))
  }
  if (ncol(nmat) > 0L) {
    matlines(seq_len(nrow(nmat)), nmat, lty = 1, lwd = 0.5, col = adjustcolor("#762a83", alpha.f = 0.28))
  }
  lines(x_f, mu_f, lwd = 2.6, col = "#1b7837")
  points(x_f, truth, pch = 16, cex = 0.85, col = "black")
  lines(x_f, truth, lwd = 1.1, col = "black")
  legend("topleft",
         legend = c("Multivar mu_t q=50", "Multivar q50 95% band", "GLOFAS members", "NWS members", "Future USGS (withheld)"),
         col = c("#1b7837", "#1b7837", "#2166ac", "#762a83", "black"),
         lty = c(1, 2, 1, 1, 1), lwd = c(2.6, 1.0, 1.0, 1.0, 1.1), pch = c(NA, NA, NA, NA, 16), bty = "n")
})

profile_section("figures_multivar_only.vb_usgs_location_quantiles", {
  payload <- build_multivar_vb_usgs_location_quantile_window(pre_days = multivar_component_pre_days())
  if (is.null(payload)) return(invisible(NULL))

  state_df <- payload$state_df
  summary_df <- payload$summary_df
  if (is.data.frame(state_df) && nrow(state_df) > 0L) {
    write.csv(
      state_df,
      file.path(OUT_DIR, "multivar_vb_usgs_location_quantiles_cutoff_window.csv"),
      row.names = FALSE
    )
  }
  if (is.data.frame(summary_df) && nrow(summary_df) > 0L) {
    write.csv(
      summary_df,
      file.path(OUT_DIR, "multivar_vb_usgs_location_quantile_summary.csv"),
      row.names = FALSE
    )
  }
  if (length(payload$missing_quantiles) > 0L) {
    writeLines(
      payload$missing_quantiles,
      con = file.path(OUT_DIR, "multivar_vb_usgs_location_quantiles_missing.txt"),
      useBytes = TRUE
    )
  }
  plot_multivar_vb_usgs_location_quantiles(
    state_df = state_df,
    summary_df = summary_df,
    out_png = file.path(OUT_DIR, "multivar_vb_usgs_location_quantiles_cutoff_window.png"),
    out_pdf = file.path(OUT_DIR, "multivar_vb_usgs_location_quantiles_cutoff_window.pdf")
  )
})

profile_section("figures_multivar_only.transfer_state_verification", {
  payload <- build_transfer_state_window_q50(pre_days = multivar_component_pre_days())
  if (is.null(payload)) return(invisible(NULL))

  transfer_mode <- tolower(trimws(Sys.getenv("UNIFIED_MULTIVAR_FORECAST_TRANSFER_MODE", "drop")))
  if (!transfer_mode %in% c("drop", "keep")) transfer_mode <- "drop"

  state_df <- payload$state_df
  psi_df <- payload$psi_df
  seg_contract <- payload$seg_contract
  forecast_has_transfer <- isTRUE(payload$meta$forecast_has_transfer)

  if (is.data.frame(state_df) && nrow(state_df) > 0L) {
    write.csv(state_df, file.path(OUT_DIR, "multivar_transfer_state_window_q50.csv"), row.names = FALSE)
  }
  if (is.data.frame(psi_df) && nrow(psi_df) > 0L) {
    write.csv(psi_df, file.path(OUT_DIR, "multivar_transfer_coefficients_window_q50.csv"), row.names = FALSE)
  }
  if (is.data.frame(seg_contract) && nrow(seg_contract) > 0L) {
    write.csv(seg_contract, file.path(OUT_DIR, "multivar_transfer_state_contract_q50.csv"), row.names = FALSE)
  }

  if (is.data.frame(state_df) && nrow(state_df) > 0L) {
    identity_summary <- do.call(
      rbind,
      lapply(unique(state_df$phase), function(ph) {
        dd <- state_df[state_df$phase == ph, , drop = FALSE]
        data.frame(
          phase = ph,
          n = nrow(dd),
          max_abs_identity_err_glofas = if (any(is.finite(dd$identity_err_glofas))) max(abs(dd$identity_err_glofas), na.rm = TRUE) else NA_real_,
          max_abs_identity_err_nws = if (any(is.finite(dd$identity_err_nws))) max(abs(dd$identity_err_nws), na.rm = TRUE) else NA_real_,
          stringsAsFactors = FALSE
        )
      })
    )
    write.csv(identity_summary, file.path(OUT_DIR, "multivar_transfer_identity_check_q50.csv"), row.names = FALSE)

    tol_identity <- 1e-8
    tol_decomp <- 1e-8
    eq_err <- state_df$mu_usgs - state_df$mu_without_transfer - state_df$zeta_mean
    max_abs_eq_err <- if (any(is.finite(eq_err))) max(abs(eq_err), na.rm = TRUE) else NA_real_

    max_abs_err_g <- if (any(is.finite(state_df$identity_err_glofas))) {
      max(abs(state_df$identity_err_glofas), na.rm = TRUE)
    } else {
      NA_real_
    }
    max_abs_err_n <- if (any(is.finite(state_df$identity_err_nws))) {
      max(abs(state_df$identity_err_nws), na.rm = TRUE)
    } else {
      NA_real_
    }

    forecast_rows <- state_df[state_df$phase == "forecast", , drop = FALSE]
    finite_zeta_forecast <- if (nrow(forecast_rows) > 0L) sum(is.finite(forecast_rows$zeta_mean)) else 0L
    finite_mu_without_transfer_forecast <- if (nrow(forecast_rows) > 0L) sum(is.finite(forecast_rows$mu_without_transfer)) else 0L

    contract_summary <- data.frame(
      transfer_mode = transfer_mode,
      forecast_has_transfer = isTRUE(forecast_has_transfer),
      n_forecast_rows = nrow(forecast_rows),
      finite_zeta_forecast = as.integer(finite_zeta_forecast),
      finite_mu_without_transfer_forecast = as.integer(finite_mu_without_transfer_forecast),
      max_abs_mu_decomp_error = max_abs_eq_err,
      max_abs_identity_err_glofas = max_abs_err_g,
      max_abs_identity_err_nws = max_abs_err_n,
      tol_decomp = tol_decomp,
      tol_identity = tol_identity,
      stringsAsFactors = FALSE
    )
    write.csv(contract_summary, file.path(OUT_DIR, "multivar_transfer_contract_q50.csv"), row.names = FALSE)

    violations <- character(0)
    if (is.finite(max_abs_eq_err) && max_abs_eq_err > tol_decomp) {
      violations <- c(violations, sprintf("mu decomposition error exceeds tolerance: %.6e > %.6e", max_abs_eq_err, tol_decomp))
    }
    if (is.finite(max_abs_err_g) && max_abs_err_g > tol_identity) {
      violations <- c(violations, sprintf("glofas identity error exceeds tolerance: %.6e > %.6e", max_abs_err_g, tol_identity))
    }
    if (is.finite(max_abs_err_n) && max_abs_err_n > tol_identity) {
      violations <- c(violations, sprintf("nws identity error exceeds tolerance: %.6e > %.6e", max_abs_err_n, tol_identity))
    }
    if (identical(transfer_mode, "keep")) {
      if (!isTRUE(forecast_has_transfer)) {
        violations <- c(violations, "keep mode expected forecast transfer retention, but forecast_has_transfer is FALSE")
      }
      if (nrow(forecast_rows) > 0L && finite_zeta_forecast == 0L) {
        violations <- c(violations, "keep mode expected finite forecast zeta_mean values, but none were found")
      }
    }
    if (identical(transfer_mode, "drop")) {
      if (isTRUE(forecast_has_transfer)) {
        violations <- c(violations, "drop mode expected no forecast transfer retention, but forecast_has_transfer is TRUE")
      }
    }
    if (length(violations) > 0L) {
      stop(
        paste(
          c("[MULTIVAR_TRANSFER_CONTRACT_FAIL]", violations),
          collapse = " | "
        ),
        call. = FALSE
      )
    }
  }

  plot_transfer_zeta_window_q50(
    state_df = state_df,
    out_file = file.path(OUT_DIR, "multivar_transfer_zeta_window_q50.png"),
    forecast_has_transfer = forecast_has_transfer
  )
  plot_transfer_coefficients_window_q50(
    psi_df = psi_df,
    out_file = file.path(OUT_DIR, "multivar_transfer_coefficients_window_q50.png"),
    forecast_has_transfer = forecast_has_transfer
  )
  plot_transfer_observation_decomp_q50(
    state_df = state_df,
    out_file = file.path(OUT_DIR, "multivar_transfer_observation_decomposition_q50.png"),
    forecast_has_transfer = forecast_has_transfer
  )
  plot_transfer_sources_window_q50(
    state_df = state_df,
    out_file = file.path(OUT_DIR, "multivar_transfer_source_mu_window_q50.png"),
    forecast_has_transfer = forecast_has_transfer
  )
  plot_transfer_discrepancy_identity_q50(
    state_df = state_df,
    out_file = file.path(OUT_DIR, "multivar_transfer_discrepancy_identity_q50.png")
  )
})

profile_section("figures_multivar_only.vb_latent_component_audit", {
  run_multivar_vb_latent_audit_report()
})
