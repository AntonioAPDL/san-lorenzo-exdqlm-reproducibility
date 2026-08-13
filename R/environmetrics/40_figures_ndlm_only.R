###############################################################################
# NDLM-only full post module
# Purpose:
#   - Run a self-contained NDLM post path for isolated repair/validation runs.
#   - Avoid any dependency on exDQLM/univariate notebook objects.
#   - Emit forecast-window figures, NDLM predictive caches, and CRPS exports.
###############################################################################

if (!isTRUE(MODEL_RUN_NDLM_MAIN) && !isTRUE(MODEL_RUN_NDLM_UNIVAR)) {
  stop("[POST_NDLM_ONLY] NDLM-only figures module requires an NDLM family enabled.", call. = FALSE)
}
if (isTRUE(MODEL_RUN_EXDQLM_MULTIVAR) || isTRUE(MODEL_RUN_EXDQLM_UNIVAR)) {
  stop("[POST_NDLM_ONLY] NDLM-only figures module was selected while exDQLM families are enabled.", call. = FALSE)
}
if (!exists("new.theta.out_50_NDLM_synth_DISC", inherits = TRUE)) {
  stop("[POST_NDLM_ONLY] missing new.theta.out_50_NDLM_synth_DISC in NDLM-only post module.", call. = FALSE)
}
if (!exists("samp.sigma_50_NDLM_synth_DISC", inherits = TRUE)) {
  stop("[POST_NDLM_ONLY] missing samp.sigma_50_NDLM_synth_DISC in NDLM-only post module.", call. = FALSE)
}

ndlm_obj <- get("new.theta.out_50_NDLM_synth_DISC", inherits = TRUE)
ndlm_sigma_draws <- get("samp.sigma_50_NDLM_synth_DISC", inherits = TRUE)
ndlm_direct_mean_draws <- NULL
if (is.list(ndlm_obj) &&
    is.matrix(ndlm_obj$forecast_mean_draws_loglog1p) &&
    is.numeric(ndlm_obj$forecast_mean_draws_loglog1p) &&
    nrow(ndlm_obj$forecast_mean_draws_loglog1p) > 1L &&
    ncol(ndlm_obj$forecast_mean_draws_loglog1p) > 0L &&
    all(is.finite(ndlm_obj$forecast_mean_draws_loglog1p))) {
  ndlm_direct_mean_draws <- as.matrix(ndlm_obj$forecast_mean_draws_loglog1p)
}

ndlm_forecast_start <- suppressWarnings(as.Date(FORECAST_START_DATE))
if (is.na(ndlm_forecast_start)) {
  stop("[POST_NDLM_ONLY] FORECAST_START_DATE is missing or invalid.", call. = FALSE)
}
ndlm_cutoff_date <- suppressWarnings(as.Date(CUTOFF_DATE))
ndlm_transfer_mode <- tolower(trimws(Sys.getenv("UNIFIED_NDLM_FORECAST_TRANSFER_MODE", "drop")))
if (!ndlm_transfer_mode %in% c("drop", "keep")) ndlm_transfer_mode <- NA_character_

ndlm_truth_from_start <- function(horizon, context) {
  hz <- as.integer(horizon[[1L]])
  if (!is.finite(hz) || hz < 1L) {
    stop(sprintf("[%s_HORIZON] horizon must be positive.", context), call. = FALSE)
  }
  if (!exists("San_Lorenzo_Daily_USGS_R", inherits = TRUE)) {
    stop(sprintf("[%s_TRUTH_SOURCE] San_Lorenzo_Daily_USGS_R is unavailable.", context), call. = FALSE)
  }
  sl <- get("San_Lorenzo_Daily_USGS_R", inherits = TRUE)
  if (!is.data.frame(sl) || !"data0" %in% names(sl)) {
    stop(sprintf("[%s_TRUTH_SOURCE] San_Lorenzo_Daily_USGS_R must contain data0.", context), call. = FALSE)
  }
  date_col <- if ("Date" %in% names(sl)) {
    suppressWarnings(as.Date(sl$Date))
  } else if ("time" %in% names(sl)) {
    suppressWarnings(as.Date(sl$time))
  } else {
    as.Date(rep(NA_character_, nrow(sl)))
  }
  ok <- !is.na(date_col) & is.finite(sl$data0) & date_col >= ndlm_forecast_start
  if (!any(ok)) {
    stop(sprintf("[%s_TRUTH_SOURCE] no realized USGS rows at/after %s.", context, as.character(ndlm_forecast_start)), call. = FALSE)
  }
  truth <- as.numeric(sl$data0[ok])
  if (length(truth) < hz) {
    truth <- c(truth, rep(NA_real_, hz - length(truth)))
  } else {
    truth <- truth[seq_len(hz)]
  }
  truth
}

ndlm_daily_dates <- function(horizon, start_date = ndlm_forecast_start) {
  hz <- as.integer(horizon[[1L]])
  if (!is.finite(hz) || hz < 1L) return(as.Date(character(0)))
  seq.Date(start_date, by = "day", length.out = hz)
}

if (!exists("xbs_ndlm", inherits = TRUE)) {
  if (!is.null(ndlm_direct_mean_draws)) {
    xbs_ndlm <- array(NA_real_, c(1L, ncol(ndlm_direct_mean_draws), nrow(ndlm_direct_mean_draws)))
    xbs_ndlm[1, , ] <- t(ndlm_direct_mean_draws)
  } else {
  next_idx_block <- function(prev_idx, block_len) {
    block_len <- as.integer(block_len[[1L]])
    start <- if (length(prev_idx) == 0L) 0L else as.integer(prev_idx[[length(prev_idx)]])
    if (!is.finite(block_len) || block_len <= 0L) return(integer(0))
    seq_len(block_len) + start
  }

  ndlm_warn_once <- local({
    warned <- new.env(parent = emptyenv())
    function(key, message_text) {
      if (!exists(key, envir = warned, inherits = FALSE)) {
        assign(key, TRUE, envir = warned)
        warning(message_text, call. = FALSE)
      }
      invisible(NULL)
    }
  })

  n_samp_ndlm <- suppressWarnings(as.integer(length(as.numeric(ndlm_sigma_draws))))
  if (exists("samp.theta_50_NDLM_synth_DISC", inherits = TRUE)) {
    ndlm_theta_obj <- get("samp.theta_50_NDLM_synth_DISC", inherits = TRUE)
    theta_draws <- if (is.list(ndlm_theta_obj)) ndlm_theta_obj$samp_theta else NULL
    if (is.numeric(theta_draws) && !is.null(dim(theta_draws)) && length(dim(theta_draws)) == 3L) {
      n_samp_ndlm <- min(n_samp_ndlm, as.integer(dim(theta_draws)[3]))
    }
  }
  if (!is.finite(n_samp_ndlm) || n_samp_ndlm <= 1L) {
    stop("[POST_NDLM_ONLY] unable to resolve NDLM predictive sample size.", call. = FALSE)
  }
  if (!exists("ranges", inherits = TRUE) || !exists("J", inherits = TRUE) || !exists("FF_list", inherits = TRUE)) {
    stop("[POST_NDLM_ONLY] missing ranges/J/FF_list needed to build xbs_ndlm.", call. = FALSE)
  }

  ks <- -diff(c(ranges, 0))
  xbs_ndlm <- array(NA_real_, c(1L, as.integer(ranges[[1L]]), n_samp_ndlm))
  idx <- c(0L)
  p_state <- if (exists("p", inherits = TRUE)) as.integer(get("p", inherits = TRUE)) else 7L
  eps_reg <- 0

  for (j in seq_len(as.integer(J))) {
    idx <- next_idx_block(idx, ks[J - j + 1L])
    if (length(idx) == 0L) next

    if (j > length(ndlm_obj$sm_ens) || j > length(ndlm_obj$sC_ens)) {
      ndlm_warn_once(
        paste0("missing_seg:", j),
        sprintf("NDLM ensemble segment j=%d is missing; skipping this segment.", as.integer(j))
      )
      next
    }

    sm_j <- ndlm_obj$sm_ens[[j]]
    sC_j <- ndlm_obj$sC_ens[[j]]
    if (!is.numeric(sm_j) || is.null(dim(sm_j)) || length(dim(sm_j)) != 2L ||
        !is.numeric(sC_j) || is.null(dim(sC_j)) || length(dim(sC_j)) != 3L) {
      ndlm_warn_once(
        paste0("shape_seg:", j),
        sprintf("NDLM ensemble segment j=%d has invalid shape; skipping this segment.", as.integer(j))
      )
      next
    }

    n_avail <- min(length(idx), ncol(sm_j), dim(sC_j)[3])
    if (!is.finite(n_avail) || n_avail <= 0L) {
      ndlm_warn_once(
        paste0("empty_seg:", j),
        sprintf("NDLM ensemble segment j=%d has no overlapping forecast horizon; skipping.", as.integer(j))
      )
      next
    }

    Ft <- FF_list[[j]][seq_len(min(p_state, nrow(FF_list[[j]]))), 1]
    for (tt in seq_len(n_avail)) {
      t_idx <- idx[[tt]]
      Mu <- sm_j[, tt]
      Sigma <- sC_j[, , tt]
      p_use <- min(length(Ft), length(Mu), nrow(Sigma), ncol(Sigma))
      if (!is.finite(p_use) || p_use <= 0L) next
      Ft_use <- matrix(Ft[seq_len(p_use)], ncol = 1L)
      S <- Sigma[seq_len(p_use), seq_len(p_use), drop = FALSE] + diag(p_use) * eps_reg
      mean_use <- as.numeric(crossprod(Ft_use, Mu[seq_len(p_use)]))
      var_use <- as.numeric(t(Ft_use) %*% S %*% Ft_use)
      sd_use <- sqrt(max(var_use, 0))
      xbs_ndlm[1, t_idx, ] <- stats::rnorm(n = n_samp_ndlm, mean = mean_use, sd = sd_use)
    }
  }
  }
}

ndlm_predictive_draws <- profile_section(
  "figures_ndlm_only.predictive_draws",
  post_ndlm_predictive_draws(
    ndlm_raw = get("xbs_ndlm", inherits = TRUE),
    sigma_draws = ndlm_sigma_draws,
    context = "ndlm.only",
    seed = 777L
  )
)

ndlm_sample_mat_log1p <- ndlm_predictive_draws$predictive_log1p
ndlm_sample_mat_loglog1p <- ndlm_predictive_draws$predictive_loglog1p
ndlm_mean_mat_loglog1p <- ndlm_predictive_draws$mean_loglog1p

saveRDS(ndlm_mean_mat_loglog1p, file = post_cache_path("xbs_ndlm_mean_loglog1p.rds"))
saveRDS(ndlm_sample_mat_loglog1p, file = post_cache_path("y_reps_ndlm_loglog1p.rds"))
saveRDS(ndlm_sample_mat_log1p, file = post_cache_path("xbs_ndlm_log1p.rds"))
saveRDS(ndlm_sample_mat_log1p, file = post_cache_path("y_reps_ndlm_log1p.rds"))

forecast_horizon <- ncol(ndlm_sample_mat_log1p)
forecast_dates <- ndlm_daily_dates(forecast_horizon)
truth_log1p <- ndlm_truth_from_start(forecast_horizon, context = "ndlm.only.truth")
truth_raw_cms <- pmax(expm1(truth_log1p), 0)
quantile_probs <- c(0.05, 0.20, 0.35, 0.50, 0.65, 0.80, 0.95)
ndlm_q_log1p <- fast_col_quantiles_t(ndlm_sample_mat_log1p, probs = quantile_probs)
ndlm_q_raw <- pmax(expm1(ndlm_q_log1p), 0)

profile_section("figures_ndlm_only.elbo_trace", {
  out_file <- file.path(OUT_DIR, "All_ELBOS_DISC.png")
  open_png(out_file, width = 2400, height = 1200, res = 300)
  on.exit(dev.off(), add = TRUE)
  vals <- suppressWarnings(as.numeric(get0("seq.elbo_50_NDLM_synth_DISC", ifnotfound = numeric(0), inherits = TRUE)))
  if (length(vals) > 0L) vals[1] <- NA_real_
  if (length(vals) > 0L && any(is.finite(vals))) {
    plot.ts(vals, main = "NDLM ELBO Trace", xlab = "Iteration", ylab = "ELBO", lwd = 1.6)
  } else {
    plot.new()
    title(main = "NDLM ELBO Trace (missing)")
  }
})

profile_section("figures_ndlm_only.fit_recent", {
  out_file <- file.path(OUT_DIR, "ndlm_fit_recent_log1p.png")
  open_png(out_file, width = 2800, height = 1400, res = 300)
  on.exit(dev.off(), add = TRUE)

  y_obs <- if (exists("Y", inherits = TRUE) && is.matrix(Y) && nrow(Y) >= 1L) as.numeric(Y[1, ]) else numeric(0)
  y_fit <- if (is.list(ndlm_obj) && is.numeric(ndlm_obj$exps) && !is.null(dim(ndlm_obj$exps)) && nrow(ndlm_obj$exps) >= 1L) as.numeric(ndlm_obj$exps[1, ]) else numeric(0)
  t_max <- min(length(y_obs), length(y_fit))
  if (t_max < 2L) {
    plot.new()
    title(main = "NDLM fit recent plot unavailable")
  } else {
    idx <- seq.int(max(1L, t_max - 119L), t_max)
    ylim_vals <- range(c(y_obs[idx], y_fit[idx]), finite = TRUE)
    plot(idx, y_obs[idx], type = "l", col = "black", lwd = 1.4,
         xlab = "Time index", ylab = "log(log(flow + 1))",
         main = "NDLM retrospective fit (recent window)", ylim = ylim_vals)
    lines(idx, y_fit[idx], col = "#1b7837", lwd = 1.8)
    legend("topleft", legend = c("Observed USGS", "NDLM fitted mean"),
           col = c("black", "#1b7837"), lwd = c(1.4, 1.8), bty = "n")
  }
})

profile_section("figures_ndlm_only.forecast_window_quantiles", {
  out_file <- file.path(OUT_DIR, "ndlm_forecast_window_quantiles_raw_cms.png")
  open_png(out_file, width = 3200, height = 1600, res = 320)
  on.exit(dev.off(), add = TRUE)

  x_idx <- seq_len(forecast_horizon)
  ylim_vals <- range(c(ndlm_q_raw, truth_raw_cms), finite = TRUE)
  if (!all(is.finite(ylim_vals))) ylim_vals <- c(0, 1)
  plot(x_idx, ndlm_q_raw[4, ], type = "l", lwd = 2.2, col = "#1b7837",
       xlab = "Forecast day", ylab = "Flow (cms)",
       main = sprintf("NDLM forecast window quantiles (%s mode)", ifelse(is.na(ndlm_transfer_mode), "unknown", ndlm_transfer_mode)),
       ylim = ylim_vals)
  line_cols <- c("#b2182b", "#d6604d", "#f4a582", "#1b7837", "#92c5de", "#4393c3", "#2166ac")
  for (i in seq_along(quantile_probs)) {
    lines(x_idx, ndlm_q_raw[i, ], col = line_cols[i], lwd = if (i == 4L) 2.2 else 1.5)
  }
  points(x_idx, truth_raw_cms, pch = 16, cex = 0.85, col = "black")
  lines(x_idx, truth_raw_cms, lwd = 1.0, col = "black")
  legend(
    "topleft",
    legend = c(
      paste0("q=", sprintf("%0.2f", quantile_probs)),
      "Future USGS (withheld)"
    ),
    col = c(line_cols, "black"),
    lwd = c(rep(1.6, length(quantile_probs)), 1.0),
    pch = c(rep(NA_integer_, length(quantile_probs)), 16),
    bty = "n",
    ncol = 2
  )
})

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

forecast_quantile_tbl <- data.frame(
  forecast_date = as.character(forecast_dates),
  truth_log1p = truth_log1p,
  truth_raw_cms = truth_raw_cms,
  q05_log1p = ndlm_q_log1p[1, ],
  q20_log1p = ndlm_q_log1p[2, ],
  q35_log1p = ndlm_q_log1p[3, ],
  q50_log1p = ndlm_q_log1p[4, ],
  q65_log1p = ndlm_q_log1p[5, ],
  q80_log1p = ndlm_q_log1p[6, ],
  q95_log1p = ndlm_q_log1p[7, ],
  q05_raw_cms = ndlm_q_raw[1, ],
  q20_raw_cms = ndlm_q_raw[2, ],
  q35_raw_cms = ndlm_q_raw[3, ],
  q50_raw_cms = ndlm_q_raw[4, ],
  q65_raw_cms = ndlm_q_raw[5, ],
  q80_raw_cms = ndlm_q_raw[6, ],
  q95_raw_cms = ndlm_q_raw[7, ],
  stringsAsFactors = FALSE
)

if (posterior_table_exports_enabled) {
  forecast_quantile_export <- post_export_tables(
    tables = list(ndlm_forecast_window_quantiles = forecast_quantile_tbl),
    output_dir = posterior_table_output_dir,
    file_stems = list(ndlm_forecast_window_quantiles = "ndlm_forecast_window_quantiles"),
    formats = posterior_table_formats,
    keep_na = posterior_table_keep_na,
    sort_keys = list(ndlm_forecast_window_quantiles = c("forecast_date")),
    numeric_digits = 15L
  )
  posterior_table_export_manifest <- rbind(posterior_table_export_manifest, forecast_quantile_export$manifest)

  ndlm_meta <- post_crps_synth_model_meta(
    family = if (isTRUE(MODEL_RUN_NDLM_UNIVAR)) "ndlm_univar" else "ndlm",
    likelihood_mode = "exal",
    transfer_mode = ndlm_transfer_mode
  )
  ndlm_tbl <- post_crps_model_tables(
    model_id = ndlm_meta$model_id,
    model_family = "synthesis",
    model_variant = ndlm_meta$model_variant,
    sample_mat = ndlm_sample_mat_log1p,
    obs = truth_log1p,
    forecast_dates = forecast_dates,
    cutoff_date = ndlm_cutoff_date,
    forecast_start_date = ndlm_forecast_start,
    transfer_mode = ndlm_transfer_mode,
    score_scale = "log_cms_plus1",
    context = "crps.ndlm.only"
  )
  crps_export <- post_export_crps_tables(
    per_time_df = ndlm_tbl$per_time,
    summary_df = ndlm_tbl$summary,
    output_dir = posterior_table_output_dir,
    table_formats = posterior_table_formats,
    keep_na = posterior_table_keep_na,
    numeric_digits = 17L,
    file_suffix = ""
  )
  posterior_table_export_manifest <- rbind(posterior_table_export_manifest, crps_export$manifest)

  health <- post_crps_input_health_tables(
    model_id = ndlm_meta$model_id,
    model_family = "synthesis",
    model_variant = ndlm_meta$model_variant,
    sample_mat = ndlm_sample_mat_log1p,
    forecast_dates = forecast_dates,
    cutoff_date = ndlm_cutoff_date,
    forecast_start_date = ndlm_forecast_start,
    transfer_mode = ndlm_transfer_mode,
    min_finite_share = suppressWarnings(as.numeric(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_MIN_FINITE_SHARE", "1"))),
    max_abs = suppressWarnings(as.numeric(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_MAX_ABS", "NA"))),
    context = "crps.ndlm.only.input_health"
  )
  health_export <- post_export_crps_input_health_tables(
    summary_df = health$summary,
    per_time_df = health$per_time,
    output_dir = posterior_table_output_dir,
    table_formats = posterior_table_formats,
    keep_na = posterior_table_keep_na,
    numeric_digits = 17L,
    file_suffix = ""
  )
  posterior_table_export_manifest <- rbind(posterior_table_export_manifest, health_export$manifest)

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
