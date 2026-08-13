ndlm_theory_pack_compat_outputs <- function(fit_result) {
  suffix <- "50_NDLM_synth_DISC"

  out_env <- new.env(parent = emptyenv())
  assign(sprintf("samp.sigma_%s", suffix), fit_result$samp_sigma, envir = out_env)
  assign(sprintf("samp.theta_%s", suffix), fit_result$samp_theta, envir = out_env)
  assign(sprintf("samp.theta_ens_%s", suffix), fit_result$samp_theta_ens, envir = out_env)
  assign(sprintf("new.theta.out_%s", suffix), fit_result$new_theta, envir = out_env)
  assign(sprintf("seq.sigma_%s", suffix), fit_result$seq_sigma, envir = out_env)
  assign(sprintf("seq.scale_%s", suffix), fit_result$seq_scale, envir = out_env)
  assign(sprintf("seq.elbo_%s", suffix), ndlm_theory_elbo_trace(fit_result), envir = out_env)
  assign(sprintf("delta_%s", suffix), fit_result$delta, envir = out_env)

  assign(
    "ndlm_main_theory_state",
    list(
      sigma = fit_result$sigma,
      sigma_by_source = fit_result$sigma_by_source,
      sigma_mean = fit_result$sigma_mean,
      w_hist = fit_result$w_hist,
      w_fore = fit_result$w_fore,
      discount_factors = fit_result$discount_factors,
      T = fit_result$T,
      K = fit_result$K,
      K_overlap = fit_result$K_overlap,
      K_max = fit_result$K_max,
      K_vec = fit_result$K_vec,
      segment_lengths = fit_result$segment_lengths,
      extension_source = fit_result$extension_source,
      bridge_source = fit_result$bridge_source,
      active_set_by_lead = fit_result$active_set_by_lead,
      state_dim_by_lead = fit_result$state_dim_by_lead,
      forecast_prior = fit_result$forecast_prior,
      forecast_cov_factors = fit_result$forecast_cov_factors,
      forecast_cov_diagnostics = fit_result$forecast_cov_diagnostics,
      state_registry = fit_result$state_registry,
      covariance_diagnostics = fit_result$covariance_diagnostics,
      fit_diagnostics = fit_result$fit_diagnostics,
      stabilization = fit_result$stabilization,
      K_cap = fit_result$K_cap,
      nws_len = fit_result$nws_len,
      glofas_len = fit_result$glofas_len
    ),
    envir = out_env
  )

  out_env
}
