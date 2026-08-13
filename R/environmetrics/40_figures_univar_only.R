###############################################################################
# Univariate-only full post module
# Purpose:
#   - Complete the isolated univariate repair lane with CRPS/table exports.
#   - Use active fitted quantiles only.
#   - Plot raw fitted quantiles, isotone synthesis anchors, and empirical
#     synthesis quantiles over the forecast window.
###############################################################################

if (!isTRUE(MODEL_RUN_EXDQLM_UNIVAR)) {
  stop("[POST_UNIVAR_ONLY] univariate-only figures module requires MODEL_RUN_EXDQLM_UNIVAR=TRUE.", call. = FALSE)
}
if (isTRUE(MODEL_RUN_EXDQLM_MULTIVAR) || isTRUE(MODEL_RUN_NDLM_MAIN) || isTRUE(MODEL_RUN_NDLM_UNIVAR)) {
  stop("[POST_UNIVAR_ONLY] univariate-only figures module was selected while other families are enabled.", call. = FALSE)
}
if (!exists("y_forecast", inherits = TRUE) || !exists("y_hist_uni", inherits = TRUE)) {
  stop("[POST_UNIVAR_ONLY] missing y_forecast/y_hist_uni from 30_univariate_and_misc.R.", call. = FALSE)
}
if (!exists("synth_f2", inherits = TRUE) || !exists("synth_hist_uni", inherits = TRUE)) {
  stop("[POST_UNIVAR_ONLY] missing synth_f2/synth_hist_uni from 30_univariate_and_misc.R.", call. = FALSE)
}

univar_forecast_start <- suppressWarnings(as.Date(FORECAST_START_DATE))
if (is.na(univar_forecast_start)) {
  stop("[POST_UNIVAR_ONLY] FORECAST_START_DATE is missing or invalid.", call. = FALSE)
}
univar_cutoff_date <- suppressWarnings(as.Date(CUTOFF_DATE))
univar_likelihood_mode <- tolower(trimws(Sys.getenv("UNIFIED_EXDQLM_UNIVAR_LIKELIHOOD_MODE", "exal")))
if (!univar_likelihood_mode %in% c("exal", "al")) {
  univar_likelihood_mode <- "exal"
}

univar_truth_from_start <- function(horizon, context) {
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
  ok <- !is.na(date_col) & is.finite(sl$data0) & date_col >= univar_forecast_start
  if (!any(ok)) {
    stop(sprintf("[%s_TRUTH_SOURCE] no realized USGS rows at/after %s.", context, as.character(univar_forecast_start)), call. = FALSE)
  }
  truth <- as.numeric(sl$data0[ok])
  if (length(truth) < hz) {
    truth <- c(truth, rep(NA_real_, hz - length(truth)))
  } else {
    truth <- truth[seq_len(hz)]
  }
  truth
}

univar_daily_dates <- function(horizon, start_date = univar_forecast_start) {
  hz <- as.integer(horizon[[1L]])
  if (!is.finite(hz) || hz < 1L) return(as.Date(character(0)))
  seq.Date(start_date, by = "day", length.out = hz)
}

univar_cube_slice <- function(cube, idx, context) {
  dims <- dim(cube)
  if (is.null(dims) || length(dims) != 3L) {
    stop(sprintf("[%s_SHAPE] expected 3D cube.", context), call. = FALSE)
  }
  mat <- cube[idx, , , drop = TRUE]
  if (!is.matrix(mat)) {
    mat <- matrix(mat, nrow = dims[[2L]], ncol = dims[[3L]])
  }
  mat
}

read_cache_optional <- function(name) {
  path <- post_cache_path(name)
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

make_quantile_long_df <- function(forecast_dates, q_probs, mat_log1p, curve_type, truth_log1p, truth_raw_cms) {
  n_q <- length(q_probs)
  horizon <- length(forecast_dates)
  if (!is.matrix(mat_log1p) || nrow(mat_log1p) != n_q || ncol(mat_log1p) != horizon) {
    stop("[POST_UNIVAR_ONLY_QUANTILE_TABLE] mat_log1p must be [quantile x horizon].", call. = FALSE)
  }
  out <- data.frame(
    forecast_date = rep(as.character(forecast_dates), each = n_q),
    lead_day = rep(seq_len(horizon), each = n_q),
    quantile = rep(q_probs, times = horizon),
    curve_type = rep(curve_type, times = n_q * horizon),
    value_log1p = post_quantile_curve_long_values(mat_log1p, q_probs = q_probs, horizon = horizon, context = "univar.only.quantile_table"),
    value_raw_cms = pmax(expm1(post_quantile_curve_long_values(mat_log1p, q_probs = q_probs, horizon = horizon, context = "univar.only.quantile_table")), 0),
    truth_log1p = rep(as.numeric(truth_log1p), each = n_q),
    truth_raw_cms = rep(as.numeric(truth_raw_cms), each = n_q),
    stringsAsFactors = FALSE
  )
  out
}

univar_y_forecast <- get("y_forecast", inherits = TRUE)
univar_y_hist <- get("y_hist_uni", inherits = TRUE)
univar_synth_forecast <- get("synth_f2", inherits = TRUE)
univar_synth_hist <- get("synth_hist_uni", inherits = TRUE)

saveRDS(univar_y_hist, file = post_cache_path("y_hist_uni.rds"))
saveRDS(univar_y_forecast, file = post_cache_path("y_forecast_uni.rds"))
saveRDS(univar_synth_hist, file = post_cache_path("synth_univar_hist_log1p.rds"))
saveRDS(univar_synth_forecast, file = post_cache_path("synth_univar_forecast_log1p.rds"))

forecast_horizon <- dim(univar_y_forecast)[3]
forecast_dates <- univar_daily_dates(forecast_horizon)
truth_log1p <- univar_truth_from_start(forecast_horizon, context = "univar.only.truth")
truth_raw_cms <- pmax(expm1(truth_log1p), 0)

univar_q_probs <- read_cache_optional("univar_active_q_probs.rds")
if (is.null(univar_q_probs)) {
  nq <- dim(univar_y_forecast)[1]
  if (identical(nq, 3L)) {
    univar_q_probs <- c(0.05, 0.50, 0.95)
  } else {
    univar_q_probs <- c(0.05, 0.20, 0.35, 0.50, 0.65, 0.80, 0.95)[seq_len(nq)]
  }
}
univar_q_probs <- as.numeric(univar_q_probs)

raw_model_q_log1p <- read_cache_optional("univar_raw_forecast_quantiles_log1p.rds")
if (is.null(raw_model_q_log1p)) {
  raw_model_q_log1p <- post_quantile_curve_from_sample_cube(
    sample_cube = exp(univar_y_forecast),
    q_probs = univar_q_probs,
    context = "univar.only.raw_curve"
  )
}

synth_anchor_q_log1p <- read_cache_optional("univar_synth_forecast_anchor_quantiles_log1p.rds")
if (is.null(synth_anchor_q_log1p)) {
  synth_tmp <- post_exdqlm_synthesize_from_sample_cube(
    sample_cube = exp(univar_y_forecast),
    q_probs = univar_q_probs,
    n_samp = nrow(univar_synth_forecast),
    seed = suppressWarnings(as.integer(Sys.getenv("DISC_BASE_SEED", "777"))) + 499L,
    context = "univar.only.anchor_fallback"
  )
  synth_anchor_q_log1p <- synth_tmp$anchor_quantiles
}

synth_empirical_q_log1p <- fast_col_quantiles_t(univar_synth_forecast, probs = univar_q_probs)

raw_model_q_raw <- pmax(expm1(raw_model_q_log1p), 0)
synth_anchor_q_raw <- pmax(expm1(synth_anchor_q_log1p), 0)
synth_empirical_q_raw <- pmax(expm1(synth_empirical_q_log1p), 0)

crossing_summary <- post_quantile_curve_crossing_summary(
  q_curve = raw_model_q_log1p,
  q_probs = univar_q_probs,
  context = "univar.only.raw_curve"
)
write.csv(
  crossing_summary$per_time,
  file = file.path(OUT_DIR, "univar_forecast_quantile_crossing_per_time.csv"),
  row.names = FALSE
)
write.csv(
  crossing_summary$summary,
  file = file.path(OUT_DIR, "univar_forecast_quantile_crossing_summary.csv"),
  row.names = FALSE
)

forecast_quantile_tbl <- rbind(
  make_quantile_long_df(forecast_dates, univar_q_probs, raw_model_q_log1p, "raw_model", truth_log1p, truth_raw_cms),
  make_quantile_long_df(forecast_dates, univar_q_probs, synth_anchor_q_log1p, "synth_anchor", truth_log1p, truth_raw_cms),
  make_quantile_long_df(forecast_dates, univar_q_probs, synth_empirical_q_log1p, "synth_empirical", truth_log1p, truth_raw_cms)
)
write.csv(
  forecast_quantile_tbl,
  file = file.path(OUT_DIR, "univar_forecast_window_quantiles.csv"),
  row.names = FALSE
)

profile_section("figures_univar_only.forecast_window_quantiles", {
  out_file <- file.path(OUT_DIR, "univar_forecast_window_quantiles_raw_cms.png")
  open_png(out_file, width = 3200, height = 1600, res = 320)
  on.exit(dev.off(), add = TRUE)

  x_idx <- seq_len(forecast_horizon)
  ylim_vals <- range(c(raw_model_q_raw, synth_anchor_q_raw, synth_empirical_q_raw, truth_raw_cms), finite = TRUE)
  if (!all(is.finite(ylim_vals))) ylim_vals <- c(0, 1)

  mid_idx <- which.min(abs(univar_q_probs - 0.5))
  plot(
    x_idx,
    synth_empirical_q_raw[mid_idx, ],
    type = "l",
    lwd = 2.3,
    col = "#1b7837",
    xlab = "Forecast day",
    ylab = "Flow (cms)",
    main = sprintf("Univariate forecast window quantiles (%s)", toupper(univar_likelihood_mode)),
    ylim = ylim_vals
  )

  raw_cols <- rep("#7f7f7f", length(univar_q_probs))
  anchor_cols <- grDevices::colorRampPalette(c("#ca0020", "#f4a582", "#0571b0"))(length(univar_q_probs))
  synth_cols <- grDevices::colorRampPalette(c("#762a83", "#5aae61", "#1b7837"))(length(univar_q_probs))

  for (i in seq_along(univar_q_probs)) {
    lines(x_idx, raw_model_q_raw[i, ], col = raw_cols[i], lwd = 1.0, lty = 3)
  }
  for (i in seq_along(univar_q_probs)) {
    lines(x_idx, synth_anchor_q_raw[i, ], col = anchor_cols[i], lwd = 1.3, lty = 2)
  }
  for (i in seq_along(univar_q_probs)) {
    lines(x_idx, synth_empirical_q_raw[i, ], col = synth_cols[i], lwd = if (i == mid_idx) 2.3 else 1.5, lty = 1)
  }
  lines(x_idx, truth_raw_cms, col = "black", lwd = 1.1)
  points(x_idx, truth_raw_cms, col = "black", pch = 16, cex = 0.8)

  legend(
    "topleft",
    legend = c(
      paste0("raw q=", sprintf("%0.2f", univar_q_probs)),
      paste0("anchor q=", sprintf("%0.2f", univar_q_probs)),
      paste0("empirical synth q=", sprintf("%0.2f", univar_q_probs)),
      "Future USGS (withheld)"
    ),
    col = c(raw_cols, anchor_cols, synth_cols, "black"),
    lty = c(
      rep(3, length(univar_q_probs)),
      rep(2, length(univar_q_probs)),
      rep(1, length(univar_q_probs)),
      1
    ),
    lwd = c(
      rep(1.0, length(univar_q_probs)),
      rep(1.3, length(univar_q_probs)),
      rep(1.5, length(univar_q_probs)),
      1.1
    ),
    pch = c(rep(NA_integer_, length(univar_q_probs) * 3L), 16),
    bty = "n",
    ncol = 3
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

if (posterior_table_exports_enabled) {
  quantile_export <- post_export_tables(
    tables = list(
      univar_forecast_window_quantiles = forecast_quantile_tbl,
      univar_forecast_quantile_crossing_per_time = crossing_summary$per_time,
      univar_forecast_quantile_crossing_summary = crossing_summary$summary
    ),
    output_dir = posterior_table_output_dir,
    file_stems = list(
      univar_forecast_window_quantiles = "univar_forecast_window_quantiles",
      univar_forecast_quantile_crossing_per_time = "univar_forecast_quantile_crossing_per_time",
      univar_forecast_quantile_crossing_summary = "univar_forecast_quantile_crossing_summary"
    ),
    formats = posterior_table_formats,
    keep_na = posterior_table_keep_na,
    sort_keys = list(
      univar_forecast_window_quantiles = c("forecast_date", "curve_type", "quantile"),
      univar_forecast_quantile_crossing_per_time = c("lead_day"),
      univar_forecast_quantile_crossing_summary = c("n_horizon")
    ),
    numeric_digits = 15L
  )
  posterior_table_export_manifest <- rbind(posterior_table_export_manifest, quantile_export$manifest)

  univar_meta <- post_crps_synth_model_meta(
    family = "univar",
    likelihood_mode = univar_likelihood_mode
  )
  univar_tbl <- post_crps_model_tables(
    model_id = univar_meta$model_id,
    model_family = "synthesis",
    model_variant = univar_meta$model_variant,
    sample_mat = univar_synth_forecast,
    obs = truth_log1p,
    forecast_dates = forecast_dates,
    cutoff_date = univar_cutoff_date,
    forecast_start_date = univar_forecast_start,
    transfer_mode = NA_character_,
    score_scale = "log_cms_plus1",
    context = "crps.univar.only"
  )
  crps_export <- post_export_crps_tables(
    per_time_df = univar_tbl$per_time,
    summary_df = univar_tbl$summary,
    output_dir = posterior_table_output_dir,
    table_formats = posterior_table_formats,
    keep_na = posterior_table_keep_na,
    numeric_digits = 17L,
    file_suffix = ""
  )
  posterior_table_export_manifest <- rbind(posterior_table_export_manifest, crps_export$manifest)

  health <- post_crps_input_health_tables(
    model_id = univar_meta$model_id,
    model_family = "synthesis",
    model_variant = univar_meta$model_variant,
    sample_mat = univar_synth_forecast,
    forecast_dates = forecast_dates,
    cutoff_date = univar_cutoff_date,
    forecast_start_date = univar_forecast_start,
    transfer_mode = NA_character_,
    min_finite_share = suppressWarnings(as.numeric(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_MIN_FINITE_SHARE", "1"))),
    max_abs = suppressWarnings(as.numeric(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_MAX_ABS", "NA"))),
    context = "crps.univar.only.input_health"
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
