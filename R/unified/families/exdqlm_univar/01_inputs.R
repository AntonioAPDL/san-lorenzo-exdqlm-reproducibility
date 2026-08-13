univar_theory_read_csv <- function(path, label) {
  family_shared_read_csv(path, label)
}

univar_theory_pick_numeric_column <- function(df, preferred = character(0)) {
  family_shared_pick_numeric_column(df, preferred = preferred, exclude_time_like = TRUE)
}

univar_theory_align_series <- function(x, target_len, fill = 0) {
  family_shared_tail_align_series(x, target_len = target_len, fill = fill)
}

univar_theory_load_inputs <- function() {
  retros_path <- Sys.getenv("UNIV_RETROS_CSV", "")
  if (!nzchar(retros_path)) {
    shared_root <- Sys.getenv("UNIV_SHARED_INPUT_ROOT", "")
    if (nzchar(shared_root)) {
      retros_path <- file.path(shared_root, "retros", "retros.csv")
    }
  }
  retros_df <- univar_theory_read_csv(retros_path, "retros")
  y <- univar_theory_pick_numeric_column(
    retros_df,
    preferred = c("USGS", "y", "obs", "flow", "value")
  )
  if (is.null(y)) {
    stop(
      sprintf("univar theory retros has no numeric target column: %s", retros_path),
      call. = FALSE
    )
  }
  y <- as.numeric(y)
  retros_dates <- family_shared_pick_date_column(retros_df, cov_name = "RETROS")
  keep_idx <- is.finite(y)
  y <- y[keep_idx]
  if (length(y) < 30L) {
    stop("univar theory requires at least 30 finite observations in retros", call. = FALSE)
  }
  Tn <- length(y)
  history_dates <- as.Date(retros_dates[keep_idx])
  if (all(is.na(history_dates))) {
    history_dates <- seq(as.Date("1970-01-01"), by = "1 day", length.out = Tn)
  }

  feature_path <- Sys.getenv("UNIFIED_COVARIATE_FEATURES_CSV", "")
  if (nzchar(feature_path) && file.exists(feature_path)) {
    feature_bundle <- family_shared_build_feature_matrices(
      path = feature_path,
      history_dates = history_dates,
      forecast_dates = as.Date(character(0)),
      fill_value = 0,
      scale_with_history = TRUE
    )
    X <- feature_bundle$history
  } else {
    cov_keys <- c(
      "UNIV_COV1_ELI_CSV",
      "UNIV_COV2_ONI_CSV",
      "UNIV_PPT_CSV",
      "UNIV_SOIL_CSV",
      "UNIV_PCA_CSV"
    )
    cov_series <- vector("list", length(cov_keys))
    cov_names <- c("ELI", "ONI", "PPT", "SOIL", "PCA")
    for (i in seq_along(cov_keys)) {
      pth <- Sys.getenv(cov_keys[[i]], "")
      cov_piece <- family_shared_build_covariate_series(
        path = pth,
        cov_name = cov_names[[i]],
        history_dates = history_dates,
        forecast_dates = as.Date(character(0)),
        fill_value = 0,
        scale_with_history = TRUE
      )
      cov_series[[i]] <- cov_piece$history
    }
    X <- do.call(cbind, cov_series)
    colnames(X) <- cov_names
  }

  list(
    y = y,
    X = X,
    T = Tn,
    input_paths = list(
      retros = retros_path,
      parameters = Sys.getenv("UNIV_PARAMETERS_TXT", ""),
      nws = Sys.getenv("UNIV_NWS_FORECAST_CSV", ""),
      glofas = Sys.getenv("UNIV_GLOFAS_FORECAST_CSV", "")
    )
  )
}
