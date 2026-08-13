ndlm_univar_env_int <- function(name, default, min_val = 1L) {
  raw <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default))))
  if (!is.finite(raw)) raw <- as.integer(default)
  raw <- as.integer(raw)
  if (is.finite(min_val)) raw <- max(raw, as.integer(min_val))
  raw
}

ndlm_univar_env_num <- function(name, default, min_val = NA_real_) {
  raw <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
  if (!is.finite(raw)) raw <- as.numeric(default)
  if (is.finite(min_val)) raw <- max(raw, as.numeric(min_val))
  raw
}

ndlm_univar_env_prob <- function(name, default, min_val = 1e-8, max_val = 1 - 1e-8) {
  raw <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
  if (!is.finite(raw)) raw <- as.numeric(default)
  raw <- min(max(raw, as.numeric(min_val)), as.numeric(max_val))
  raw
}

ndlm_univar_env_choice <- function(name, choices, default) {
  raw <- tolower(trimws(Sys.getenv(name, as.character(default))))
  if (!nzchar(raw)) raw <- as.character(default)
  if (!(raw %in% choices)) raw <- as.character(default)
  raw
}

ndlm_univar_env_num_vec <- function(name, default) {
  raw_chr <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw_chr)) {
    return(as.numeric(default))
  }
  parts <- trimws(strsplit(raw_chr, ",", fixed = TRUE)[[1L]])
  if (length(parts) < 1L) {
    return(as.numeric(default))
  }
  vals <- suppressWarnings(as.numeric(parts))
  if (length(vals) != length(default) || any(!is.finite(vals))) {
    return(as.numeric(default))
  }
  vals
}

ndlm_univar_theory_constants <- function(seed = 777L) {
  horizon_cap <- ndlm_univar_env_int("NDLM_UNIV_FORECAST_HORIZON_CAP", default = 1080L, min_val = 1L)
  n_draws <- ndlm_univar_env_int("NDLM_UNIV_POSTERIOR_DRAWS", default = 64L, min_val = 1L)
  kalman_backend <- ndlm_univar_env_choice("NDLM_UNIV_KALMAN_BACKEND", c("cpp", "r"), "cpp")
  forecast_transfer_mode <- ndlm_univar_env_choice("NDLM_UNIV_FORECAST_TRANSFER_MODE", c("keep", "drop"), "keep")

  df_t <- ndlm_univar_env_prob("NDLM_UNIV_DF_T", default = 0.95)
  df_s1 <- ndlm_univar_env_prob("NDLM_UNIV_DF_S1", default = 0.98)
  df_s2 <- ndlm_univar_env_prob("NDLM_UNIV_DF_S2", default = 0.98)
  df_s67 <- ndlm_univar_env_prob("NDLM_UNIV_DF_S67", default = 0.98)
  df_trans <- ndlm_univar_env_prob("NDLM_UNIV_DF_TRANS", default = 0.99999999)
  df_covs <- ndlm_univar_env_prob("NDLM_UNIV_DF_COVS", default = 0.99999)
  lambda <- ndlm_univar_env_prob("NDLM_UNIV_LAMBDA", default = 0.99)
  period <- ndlm_univar_env_num("NDLM_UNIV_SEASONAL_PERIOD", default = 363.5854, min_val = 1e-8)
  harmonics <- ndlm_univar_env_num_vec("NDLM_UNIV_SEASONAL_HARMONICS", default = c(1, 2, 1 / 6.8068493))

  n0 <- ndlm_univar_env_num("NDLM_UNIV_N0", default = 20, min_val = 1e-6)
  S0 <- ndlm_univar_env_num("NDLM_UNIV_S0", default = 1, min_val = 1e-8)

  cov_eig_floor <- ndlm_univar_env_num("NDLM_UNIV_COV_EIG_FLOOR", default = 1e-8, min_val = 1e-12)
  cov_eig_cap <- ndlm_univar_env_num("NDLM_UNIV_COV_EIG_CAP", default = 1e8, min_val = cov_eig_floor * 10)
  cov_diag_jitter <- ndlm_univar_env_num("NDLM_UNIV_COV_DIAG_JITTER", default = 1e-10, min_val = 0)

  list(
    seed = as.integer(seed),
    kalman_backend = kalman_backend,
    forecast_transfer_mode = forecast_transfer_mode,
    transfer_active_forecast_window = identical(forecast_transfer_mode, "keep"),
    horizon_cap = horizon_cap,
    n_draws = n_draws,
    n0 = as.numeric(n0),
    S0 = as.numeric(S0),
    df_t = df_t,
    df_s1 = df_s1,
    df_s2 = df_s2,
    df_s67 = df_s67,
    df_trans = df_trans,
    df_covs = df_covs,
    lambda = lambda,
    period = period,
    harmonics = harmonics,
    covariate_names = c("ELI", "ONI", "PPT", "SOIL", "PCA"),
    stabilization = list(
      cov_eig_floor = cov_eig_floor,
      cov_eig_cap = cov_eig_cap,
      cov_diag_jitter = cov_diag_jitter
    )
  )
}
