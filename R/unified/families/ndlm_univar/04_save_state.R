ndlm_univar_pack_compat_outputs <- function(fit_result) {
  suffix <- "50_NDLM_univar_synth_DISC"
  legacy_suffix <- "50_NDLM_synth_DISC"

  out_env <- new.env(parent = emptyenv())
  assign(sprintf("samp.sigma_%s", suffix), fit_result$samp_sigma, envir = out_env)
  assign(sprintf("samp.theta_%s", suffix), fit_result$samp_theta, envir = out_env)
  assign(sprintf("samp.theta.ens_%s", suffix), fit_result$samp_theta_ens, envir = out_env)
  assign(sprintf("new.theta.out_%s", suffix), fit_result$new_theta, envir = out_env)
  assign(sprintf("seq.sigma_%s", suffix), fit_result$seq_sigma, envir = out_env)
  assign(sprintf("seq.scale_%s", suffix), fit_result$seq_scale, envir = out_env)
  assign(sprintf("seq.elbo_%s", suffix), fit_result$seq_elbo, envir = out_env)
  assign(sprintf("delta_%s", suffix), fit_result$delta, envir = out_env)
  assign(sprintf("y.fore.draws_%s", suffix), fit_result$y_fore_draws, envir = out_env)

  # Legacy NDLM object aliases keep post-stage loaders stable when running
  # ndlm_univar in isolation.
  assign(sprintf("samp.sigma_%s", legacy_suffix), fit_result$samp_sigma, envir = out_env)
  assign(sprintf("samp.theta_%s", legacy_suffix), fit_result$samp_theta, envir = out_env)
  assign(sprintf("samp.theta.ens_%s", legacy_suffix), fit_result$samp_theta_ens, envir = out_env)
  assign(sprintf("new.theta.out_%s", legacy_suffix), fit_result$new_theta, envir = out_env)
  assign(sprintf("seq.sigma_%s", legacy_suffix), fit_result$seq_sigma, envir = out_env)
  assign(sprintf("seq.scale_%s", legacy_suffix), fit_result$seq_scale, envir = out_env)
  assign(sprintf("seq.elbo_%s", legacy_suffix), fit_result$seq_elbo, envir = out_env)
  assign(sprintf("delta_%s", legacy_suffix), fit_result$delta, envir = out_env)
  assign(sprintf("y.fore.draws_%s", legacy_suffix), fit_result$y_fore_draws, envir = out_env)

  assign(
    "ndlm_univar_theory_state",
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
      forecast_transfer_mode = fit_result$forecast_transfer_mode,
      transfer_active_forecast_window = fit_result$transfer_active_forecast_window,
      active_set_by_lead = fit_result$active_set_by_lead,
      state_dim_by_lead = fit_result$state_dim_by_lead,
      covariance_diagnostics = fit_result$covariance_diagnostics,
      fit_diagnostics = fit_result$fit_diagnostics,
      stabilization = fit_result$stabilization,
      K_cap = fit_result$K_cap,
      nws_len = fit_result$nws_len,
      glofas_len = fit_result$glofas_len,
      n_T = fit_result$n_T,
      S_T = fit_result$S_T,
      p_total = fit_result$p_total
    ),
    envir = out_env
  )
  assign("ndlm_main_theory_state", get("ndlm_univar_theory_state", envir = out_env, inherits = FALSE), envir = out_env)

  out_env
}
