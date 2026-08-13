ndlm_univar_read_csv <- function(path, label) {
  family_shared_read_csv(path, label)
}

ndlm_univar_pick_numeric_column <- function(df, preferred = character(0)) {
  family_shared_pick_numeric_column(df, preferred = preferred, exclude_time_like = TRUE)
}

ndlm_univar_pick_date_column <- function(df) {
  family_shared_pick_date_column(df, cov_name = "GENERIC")
}

ndlm_univar_align_cov_by_dates <- function(dates_src, values_src, target_dates, fill_value = 0) {
  family_shared_align_by_dates(dates_src, values_src, target_dates, fill_value = fill_value)
}

ndlm_univar_build_covariate_series <- function(path, cov_name, history_dates, forecast_dates) {
  family_shared_build_covariate_series(
    path = path,
    cov_name = cov_name,
    history_dates = history_dates,
    forecast_dates = forecast_dates,
    fill_value = 0,
    scale_with_history = TRUE
  )
}

ndlm_univar_find_input <- function(env_key, shared_root, rel_path) {
  p <- Sys.getenv(env_key, "")
  if (nzchar(p)) {
    return(normalizePath(p, mustWork = FALSE))
  }
  if (!nzchar(shared_root)) return("")
  normalizePath(file.path(shared_root, rel_path), mustWork = FALSE)
}

ndlm_univar_load_inputs <- function(constants) {
  shared_root <- Sys.getenv("NDLM_SHARED_INPUT_ROOT", "")
  if (nzchar(shared_root)) shared_root <- normalizePath(shared_root, mustWork = FALSE)

  retros_path <- ndlm_univar_find_input("NDLM_RETROS_CSV", shared_root, file.path("retros", "retros.csv"))
  nws_path <- ndlm_univar_find_input("NDLM_NWS_FORECAST_CSV", shared_root, file.path("forecasts", "nws_forecast.csv"))
  glofas_path <- ndlm_univar_find_input("NDLM_GLOFAS_FORECAST_CSV", shared_root, file.path("forecasts", "glofas_forecast.csv"))

  retros_df <- ndlm_univar_read_csv(retros_path, "retros")
  nws_df <- ndlm_univar_read_csv(nws_path, "nws_forecast")
  glofas_df <- ndlm_univar_read_csv(glofas_path, "glofas_forecast")

  y_raw <- ndlm_univar_pick_numeric_column(
    retros_df,
    preferred = c("USGS", "usgs", "y", "obs", "flow", "value")
  )
  if (is.null(y_raw)) {
    stop(sprintf("ndlm_univar retros has no usable numeric target column: %s", retros_path), call. = FALSE)
  }

  retros_dates <- ndlm_univar_pick_date_column(retros_df)
  cutoff_raw <- Sys.getenv("UNIFIED_CUTOFF_DATE", "")
  cutoff_date <- suppressWarnings(as.Date(cutoff_raw))
  if (is.na(cutoff_date)) {
    finite_dates <- retros_dates[!is.na(retros_dates)]
    cutoff_date <- if (length(finite_dates) > 0L) max(finite_dates, na.rm = TRUE) else as.Date("2022-12-25")
  }
  forecast_start_date <- cutoff_date + 1L

  keep_idx <- is.finite(y_raw)
  if (any(!is.na(retros_dates))) {
    keep_idx <- keep_idx & !is.na(retros_dates) & retros_dates <= cutoff_date
  }
  y_hist <- as.numeric(y_raw[keep_idx])
  if (length(y_hist) < 30L) {
    stop("ndlm_univar requires at least 30 finite historical observations", call. = FALSE)
  }

  dates_hist <- retros_dates[keep_idx]
  if (all(is.na(dates_hist))) {
    dates_hist <- seq(cutoff_date - (length(y_hist) - 1L), cutoff_date, by = "1 day")
  }

  nws_vec <- family_shared_extract_forecast_mean(nws_df, label = "ndlm_univar_nws_forecast", transform = "log1p")
  glofas_vec <- family_shared_extract_forecast_mean(glofas_df, label = "ndlm_univar_glofas_forecast", transform = "log1p")
  nws_vec <- as.numeric(nws_vec[is.finite(nws_vec)])
  glofas_vec <- as.numeric(glofas_vec[is.finite(glofas_vec)])

  k_nws <- min(length(nws_vec), constants$horizon_cap)
  k_glofas <- min(length(glofas_vec), constants$horizon_cap)
  if (k_nws < 3L || k_glofas < 3L) {
    stop("ndlm_univar requires at least 3 finite forecast leads for both nws and glofas", call. = FALSE)
  }

  K_overlap <- min(k_nws, k_glofas)
  K_max <- max(k_nws, k_glofas)
  forecast_dates <- seq(forecast_start_date, by = "1 day", length.out = K_max)

  feature_path <- Sys.getenv("UNIFIED_COVARIATE_FEATURES_CSV", "")
  cov_paths <- c(
    ELI = Sys.getenv("NDLM_COV1_ELI_CSV", ""),
    ONI = Sys.getenv("NDLM_COV2_ONI_CSV", ""),
    PPT = Sys.getenv("NDLM_PPT_CSV", ""),
    SOIL = Sys.getenv("NDLM_SOIL_CSV", ""),
    PCA = Sys.getenv("NDLM_PCA_CSV", "")
  )

  if (nzchar(feature_path) && file.exists(feature_path)) {
    feature_bundle <- family_shared_build_feature_matrices(
      path = feature_path,
      history_dates = dates_hist,
      forecast_dates = forecast_dates,
      fill_value = 0,
      scale_with_history = TRUE
    )
    cov_hist <- feature_bundle$history
    cov_future <- feature_bundle$forecast
  } else {
    cov_hist <- matrix(0, nrow = length(y_hist), ncol = length(cov_paths))
    cov_future <- matrix(0, nrow = K_max, ncol = length(cov_paths))
    colnames(cov_hist) <- names(cov_paths)
    colnames(cov_future) <- names(cov_paths)

    for (j in seq_along(cov_paths)) {
      ser <- ndlm_univar_build_covariate_series(
        path = cov_paths[[j]],
        cov_name = names(cov_paths)[[j]],
        history_dates = dates_hist,
        forecast_dates = forecast_dates
      )
      cov_hist[, j] <- as.numeric(ser$history)
      cov_future[, j] <- as.numeric(ser$forecast)
    }
  }

  list(
    y = as.numeric(y_hist),
    dates_hist = as.Date(dates_hist),
    cutoff_date = as.Date(cutoff_date),
    forecast_start_date = as.Date(forecast_start_date),
    forecast_dates = as.Date(forecast_dates),
    X_hist = cov_hist,
    X_future = cov_future,
    forecast = list(
      nws = as.numeric(nws_vec[seq_len(k_nws)]),
      glofas = as.numeric(glofas_vec[seq_len(k_glofas)]),
      K_overlap = as.integer(K_overlap),
      K_max = as.integer(K_max),
      K_vec = c(nws = as.integer(k_nws), glofas = as.integer(k_glofas)),
      K_cap = as.integer(constants$horizon_cap)
    ),
    input_paths = list(
      retros = retros_path,
      nws = nws_path,
      glofas = glofas_path,
      covariates = as.list(cov_paths)
    )
  )
}
