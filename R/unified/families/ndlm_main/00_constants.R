ndlm_theory_constants <- function(seed = 777L) {
  env_int <- function(name, default, min_val = 1L) {
    raw <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default))))
    if (!is.finite(raw)) raw <- as.integer(default)
    raw <- as.integer(raw)
    if (is.finite(min_val)) raw <- max(raw, as.integer(min_val))
    raw
  }
  env_num <- function(name, default, min_val = NA_real_) {
    raw <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
    if (!is.finite(raw)) raw <- as.numeric(default)
    if (is.finite(min_val)) raw <- max(raw, as.numeric(min_val))
    raw
  }
  env_num_optional <- function(name, default = NA_real_, min_val = NA_real_) {
    raw_chr <- trimws(Sys.getenv(name, ""))
    if (!nzchar(raw_chr) || tolower(raw_chr) %in% c("na", "null", "none")) {
      return(as.numeric(default))
    }
    raw <- suppressWarnings(as.numeric(raw_chr))
    if (!is.finite(raw)) return(as.numeric(default))
    if (is.finite(min_val)) raw <- max(raw, as.numeric(min_val))
    raw
  }
  env_num_vec <- function(name, default) {
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
  env_prob <- function(name, default, min_val = 1e-8, max_val = 1 - 1e-8) {
    raw <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
    if (!is.finite(raw)) raw <- as.numeric(default)
    raw <- max(raw, as.numeric(min_val))
    raw <- min(raw, as.numeric(max_val))
    raw
  }
  env_choice <- function(name, choices, default) {
    raw <- tolower(trimws(Sys.getenv(name, as.character(default))))
    if (!nzchar(raw)) raw <- as.character(default)
    if (!(raw %in% choices)) raw <- as.character(default)
    raw
  }

  horizon_cap <- suppressWarnings(as.integer(Sys.getenv("NDLM_FORECAST_HORIZON_CAP", "1080")))
  if (!is.finite(horizon_cap) || horizon_cap <= 0L) {
    horizon_cap <- 1080L
  }
  kalman_backend <- tolower(trimws(Sys.getenv("NDLM_KALMAN_BACKEND", "cpp")))
  if (!(kalman_backend %in% c("r", "cpp"))) {
    kalman_backend <- "cpp"
  }
  forecast_transfer_mode <- env_choice(
    "NDLM_FORECAST_TRANSFER_MODE",
    choices = c("keep", "drop"),
    default = "keep"
  )

  max_iter <- env_int("NDLM_GAMSIG_MAX_ITER", default = 100L, min_val = 1L)
  min_total_iters <- env_int("NDLM_GAMSIG_MIN_TOTAL_ITERS", default = 50L, min_val = 1L)
  min_total_iters <- min(min_total_iters, max_iter)
  convergence_tol <- env_num("NDLM_GAMSIG_CONVERGENCE_TOL", default = 1e-6, min_val = 1e-12)
  elbo_tol <- env_num("NDLM_GAMSIG_ELBO_TOL", default = convergence_tol, min_val = 1e-12)
  elbo_rel_tol <- env_num("NDLM_GAMSIG_ELBO_REL_TOL", default = 2.5e-4, min_val = 1e-12)
  n_draws <- env_int("NDLM_POSTERIOR_DRAWS", default = 48L, min_val = 1L)
  df_t <- env_prob("NDLM_DF_T", default = 0.95)
  df_s1 <- env_prob("NDLM_DF_S1", default = 0.98)
  df_s2 <- env_prob("NDLM_DF_S2", default = 0.98)
  df_s67 <- env_prob("NDLM_DF_S67", default = 0.98)
  df_discrep <- env_prob("NDLM_DF_DISCREP", default = 0.98)
  lambda <- env_prob("NDLM_LAMBDA", default = 0.99)
  df_trans <- env_prob("NDLM_DF_TRANS", default = 0.99999999)
  df_covs <- env_prob("NDLM_DF_COVS", default = 0.99999)
  period <- env_num("NDLM_SEASONAL_PERIOD", default = 363.5854, min_val = 1e-8)
  harmonics <- env_num_vec("NDLM_SEASONAL_HARMONICS", default = c(1, 2, 1 / 6.8068493))
  cov_eig_floor <- env_num("NDLM_COV_EIG_FLOOR", default = 1e-8, min_val = 1e-12)
  cov_eig_cap <- env_num("NDLM_COV_EIG_CAP", default = 1e8, min_val = cov_eig_floor * 10)
  cov_diag_jitter <- env_num("NDLM_COV_DIAG_JITTER", default = 1e-10, min_val = 0)
  sigma_upper_cap <- env_num("NDLM_SIGMA_UPPER_CAP", default = 1e12, min_val = 1e-6)
  sigma_update_damping <- env_num("NDLM_SIGMA_UPDATE_DAMPING", default = 1.0, min_val = 0)
  sigma_update_damping <- min(sigma_update_damping, 1.0)
  latent_var_cap_mult <- env_num("NDLM_LATENT_VAR_CAP_MULT", default = 1e4, min_val = 1)
  latent_var_cap_abs <- env_num("NDLM_LATENT_VAR_CAP_ABS", default = 1e8, min_val = 1e-6)
  forecast_iw_c_factor <- env_num("NDLM_FORECAST_IW_C_FACTOR", default = 1.0, min_val = 1e-12)
  forecast_iw_epsilon0 <- env_num_optional("NDLM_FORECAST_IW_EPSILON0", default = NA_real_, min_val = 1e-12)
  forecast_iw_dof_offset <- env_int("NDLM_FORECAST_IW_DOF_OFFSET", default = 4L, min_val = 2L)
  forecast_iw_scale_mult <- env_num("NDLM_FORECAST_IW_SCALE_MULT", default = 1.0, min_val = 1e-8)
  forecast_iw_jitter <- env_num("NDLM_FORECAST_IW_JITTER", default = 1e-8, min_val = 0)

  list(
    state_dim = 26L,
    active_hist_dim = 14L,
    forecast_horizon_cap = horizon_cap,
    kalman_backend = kalman_backend,
    forecast_transfer_mode = forecast_transfer_mode,
    ensemble_block_dim = 7L,
    max_iter = max_iter,
    min_total_iters = min_total_iters,
    convergence_tol = convergence_tol,
    convergence = list(
      elbo_tol = elbo_tol,
      elbo_rel_tol = elbo_rel_tol
    ),
    n_draws = n_draws,
    seed = as.integer(seed),
    a_sigma = 2.0,
    b_sigma = 2.0,
    df_t = df_t,
    df_s1 = df_s1,
    df_s2 = df_s2,
    df_s67 = df_s67,
    df_discrep = df_discrep,
    lambda = lambda,
    df_trans = df_trans,
    df_covs = df_covs,
    period = period,
    harmonics = harmonics,
    forecast_iw_c_factor = forecast_iw_c_factor,
    forecast_iw_epsilon0 = forecast_iw_epsilon0,
    forecast_iw_dof_offset = forecast_iw_dof_offset,
    forecast_iw_scale_mult = forecast_iw_scale_mult,
    forecast_iw_jitter = forecast_iw_jitter,
    stabilization = list(
      cov_eig_floor = cov_eig_floor,
      cov_eig_cap = cov_eig_cap,
      cov_diag_jitter = cov_diag_jitter,
      sigma_upper_cap = sigma_upper_cap,
      sigma_update_damping = sigma_update_damping,
      latent_var_cap_mult = latent_var_cap_mult,
      latent_var_cap_abs = latent_var_cap_abs
    ),
    p0 = 0.5
  )
}
