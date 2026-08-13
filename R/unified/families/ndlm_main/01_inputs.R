ndlm_theory_read_csv <- function(path, label) {
  family_shared_read_csv(path, label)
}

ndlm_theory_pick_numeric_column <- function(df, preferred = character(0)) {
  family_shared_pick_numeric_column(df, preferred = preferred, exclude_time_like = TRUE)
}

ndlm_theory_pick_numeric_column_fuzzy <- function(df, preferred = character(0)) {
  exact <- ndlm_theory_pick_numeric_column(df, preferred = preferred)
  if (!is.null(exact)) {
    return(exact)
  }
  if (length(preferred) == 0L || ncol(df) < 1L) {
    return(NULL)
  }
  nm_norm <- gsub("[^a-z0-9]+", "", tolower(names(df)))
  for (cand in preferred) {
    key <- gsub("[^a-z0-9]+", "", tolower(as.character(cand)))
    if (!nzchar(key)) next
    hit <- which(nm_norm == key)
    if (length(hit) < 1L) next
    idx <- hit[[1L]]
    if (is.numeric(df[[idx]])) {
      return(df[[idx]])
    }
  }
  NULL
}

ndlm_theory_align_series <- function(x, target_len, fill = 0) {
  family_shared_tail_align_series(x, target_len = target_len, fill = fill)
}

ndlm_theory_internal_from_log1p <- function(x, label) {
  vals <- as.numeric(x)
  ok <- is.finite(vals)
  out <- rep(NA_real_, length(vals))
  if (!any(ok)) {
    return(out)
  }
  if (any(vals[ok] < 0)) {
    stop(
      sprintf(
        "%s contains negative log1p values; cannot keep log1p internal scale safely",
        label
      ),
      call. = FALSE
    )
  }
  out[ok] <- vals[ok]
  out
}

ndlm_theory_find_input <- function(env_key, shared_root, rel_path) {
  p <- Sys.getenv(env_key, "")
  if (nzchar(p)) {
    return(normalizePath(p, mustWork = FALSE))
  }
  if (!nzchar(shared_root)) return("")
  normalizePath(file.path(shared_root, rel_path), mustWork = FALSE)
}

ndlm_theory_load_inputs <- function(horizon_cap = 14L) {
  shared_root <- Sys.getenv("NDLM_SHARED_INPUT_ROOT", "")
  if (nzchar(shared_root)) shared_root <- normalizePath(shared_root, mustWork = FALSE)

  retros_path <- ndlm_theory_find_input("NDLM_RETROS_CSV", shared_root, file.path("retros", "retros.csv"))
  nws_path <- ndlm_theory_find_input("NDLM_NWS_FORECAST_CSV", shared_root, file.path("forecasts", "nws_forecast.csv"))
  glofas_path <- ndlm_theory_find_input("NDLM_GLOFAS_FORECAST_CSV", shared_root, file.path("forecasts", "glofas_forecast.csv"))

  retros_df <- ndlm_theory_read_csv(retros_path, "retros")
  nws_df <- ndlm_theory_read_csv(nws_path, "nws_forecast")
  glofas_df <- ndlm_theory_read_csv(glofas_path, "glofas_forecast")

  y_log1p <- ndlm_theory_pick_numeric_column_fuzzy(
    retros_df,
    preferred = c("USGS", "y", "obs", "flow", "value")
  )
  if (is.null(y_log1p)) {
    stop(sprintf("ndlm theory retros has no numeric target column: %s", retros_path), call. = FALSE)
  }
  y_log1p <- as.numeric(y_log1p)
  y_internal_all <- ndlm_theory_internal_from_log1p(y_log1p, label = "ndlm theory retros target")
  retros_dates_all <- family_shared_pick_date_column(retros_df, cov_name = "RETROS")
  valid_idx <- which(is.finite(y_internal_all))
  y <- y_internal_all[valid_idx]
  if (length(y) < 30L) {
    stop("ndlm theory requires at least 30 finite observations in retros", call. = FALSE)
  }
  Tn <- length(y)
  dates_hist <- as.Date(retros_dates_all[valid_idx])
  if (all(is.na(dates_hist))) {
    dates_hist <- seq(as.Date("1970-01-01"), by = "1 day", length.out = Tn)
  }

  retros_nws_log1p <- ndlm_theory_pick_numeric_column_fuzzy(
    retros_df,
    preferred = c("NWS3.0", "NWS", "nws", "z_nws", "retros_nws")
  )
  retros_glofas_log1p <- ndlm_theory_pick_numeric_column_fuzzy(
    retros_df,
    preferred = c("GloFAS", "glofas", "GLOFAS", "z_glofas", "retros_glofas")
  )
  if (is.null(retros_nws_log1p)) {
    retros_nws_log1p <- rep(NA_real_, nrow(retros_df))
  }
  if (is.null(retros_glofas_log1p)) {
    retros_glofas_log1p <- rep(NA_real_, nrow(retros_df))
  }
  retros_nws_log1p <- as.numeric(retros_nws_log1p)
  retros_glofas_log1p <- as.numeric(retros_glofas_log1p)
  if (length(retros_nws_log1p) < max(valid_idx)) {
    retros_nws_log1p <- c(retros_nws_log1p, rep(NA_real_, max(valid_idx) - length(retros_nws_log1p)))
  }
  if (length(retros_glofas_log1p) < max(valid_idx)) {
    retros_glofas_log1p <- c(retros_glofas_log1p, rep(NA_real_, max(valid_idx) - length(retros_glofas_log1p)))
  }
  retros_hist <- list(
    usgs = y,
    nws = ndlm_theory_internal_from_log1p(retros_nws_log1p[valid_idx], label = "ndlm theory retros nws"),
    glofas = ndlm_theory_internal_from_log1p(retros_glofas_log1p[valid_idx], label = "ndlm theory retros glofas")
  )

  horizon_cap <- suppressWarnings(as.integer(horizon_cap[[1L]]))
  if (!is.finite(horizon_cap) || horizon_cap <= 0L) {
    stop(sprintf("ndlm theory forecast horizon cap must be a positive integer; got '%s'", as.character(horizon_cap)), call. = FALSE)
  }

  forecast_start <- suppressWarnings(as.Date(max(dates_hist, na.rm = TRUE) + 1))
  if (!is.finite(forecast_start) || is.na(forecast_start)) {
    forecast_start <- as.Date("1970-01-01")
  }
  forecast_dates_cap <- seq.Date(forecast_start, by = "day", length.out = horizon_cap)

  nws_forecast <- family_shared_extract_forecast_ensemble(
    nws_df,
    label = "ndlm_theory_nws_forecast",
    transform = "log1p",
    target_dates = forecast_dates_cap
  )
  glofas_forecast <- family_shared_extract_forecast_ensemble(
    glofas_df,
    label = "ndlm_theory_glofas_forecast",
    transform = "log1p",
    target_dates = forecast_dates_cap
  )

  last_active_lead <- function(member_mat) {
    if (!is.matrix(member_mat) || nrow(member_mat) < 1L) return(0L)
    active_rows <- which(rowSums(is.finite(member_mat)) > 0L)
    if (length(active_rows) < 1L) return(0L)
    as.integer(max(active_rows))
  }

  nws_len_raw <- last_active_lead(nws_forecast$members)
  glofas_len_raw <- last_active_lead(glofas_forecast$members)
  nws_len <- min(nws_len_raw, horizon_cap)
  glofas_len <- min(glofas_len_raw, horizon_cap)
  K_overlap <- min(nws_len, glofas_len)
  K_max <- max(nws_len, glofas_len)
  if (K_overlap < 3L) {
    stop("ndlm theory requires at least 3 overlapping finite forecast leads across sources", call. = FALSE)
  }
  if (K_max < 3L) {
    stop("ndlm theory requires forecast matrices with at least 3 active leads after horizon capping", call. = FALSE)
  }

  forecast_dates <- forecast_dates_cap[seq_len(K_max)]
  nws_members <- nws_forecast$members[seq_len(K_max), , drop = FALSE]
  glofas_members <- glofas_forecast$members[seq_len(K_max), , drop = FALSE]
  nws_mean <- as.numeric(nws_forecast$row_means[seq_len(K_max)])
  glofas_mean <- as.numeric(glofas_forecast$row_means[seq_len(K_max)])

  feature_path <- Sys.getenv("UNIFIED_COVARIATE_FEATURES_CSV", "")
  if (nzchar(feature_path) && file.exists(feature_path)) {
    feature_bundle <- family_shared_build_feature_matrices(
      path = feature_path,
      history_dates = dates_hist,
      forecast_dates = forecast_dates,
      fill_value = 0,
      scale_with_history = TRUE
    )
    X <- feature_bundle$history
    X_future <- feature_bundle$forecast
  } else {
    cov_keys <- c(
      "NDLM_COV1_ELI_CSV",
      "NDLM_COV2_ONI_CSV",
      "NDLM_PPT_CSV",
      "NDLM_SOIL_CSV",
      "NDLM_PCA_CSV"
    )
    cov_series_hist <- vector("list", length(cov_keys))
    cov_series_fore <- vector("list", length(cov_keys))
    cov_names <- c("ELI", "ONI", "PPT", "SOIL", "PCA")
    for (i in seq_along(cov_keys)) {
      pth <- Sys.getenv(cov_keys[[i]], "")
      cov_piece <- family_shared_build_covariate_series(
        path = pth,
        cov_name = cov_names[[i]],
        history_dates = dates_hist,
        forecast_dates = forecast_dates,
        fill_value = 0,
        scale_with_history = TRUE
      )
      cov_series_hist[[i]] <- cov_piece$history
      cov_series_fore[[i]] <- cov_piece$forecast
    }
    X <- do.call(cbind, cov_series_hist)
    X_future <- do.call(cbind, cov_series_fore)
    colnames(X) <- cov_names
    colnames(X_future) <- cov_names
  }

  list(
    y = y,
    retros = retros_hist,
    X = X,
    X_future = X_future,
    T = Tn,
    forecast = list(
      nws = nws_mean[seq_len(nws_len)],
      glofas = glofas_mean[seq_len(glofas_len)],
      nws_members = nws_members,
      glofas_members = glofas_members,
      K = K_max,
      K_overlap = K_overlap,
      K_max = K_max,
      K_vec = c(nws = nws_len, glofas = glofas_len),
      K_cap = horizon_cap,
      nws_len = nws_len,
      glofas_len = glofas_len,
      nws_len_raw = nws_len_raw,
      glofas_len_raw = glofas_len_raw,
      forecast_dates = forecast_dates
    ),
    input_paths = list(
      retros = retros_path,
      nws = nws_path,
      glofas = glofas_path,
      parameters = Sys.getenv("NDLM_PARAMETERS_TXT", "")
    )
  )
}
