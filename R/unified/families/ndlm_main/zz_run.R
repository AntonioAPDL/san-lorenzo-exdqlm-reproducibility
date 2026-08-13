unified_run_ndlm_main_theory <- function(seed, output_path, log_path = NULL) {
  constants <- ndlm_theory_constants(seed = seed)
  inputs <- ndlm_theory_load_inputs(horizon_cap = constants$forecast_horizon_cap)
  fit_result <- ndlm_theory_fit(inputs = inputs, constants = constants)
  out_env <- ndlm_theory_pack_compat_outputs(fit_result)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  save(list = ls(out_env), file = output_path, envir = out_env)

  summary_lines <- c(
    "implementation_mode=theory_aligned",
    sprintf("kalman_backend=%s", constants$kalman_backend),
    sprintf("forecast_transfer_mode=%s", as.character(constants$forecast_transfer_mode)),
    sprintf(
      "transfer_active_forecast_window=%s",
      if (!is.null(fit_result$transfer_active_forecast_window) && isTRUE(fit_result$transfer_active_forecast_window)) "true" else "false"
    ),
    sprintf("output_path=%s", output_path),
    sprintf("max_iter=%d", fit_result$max_iter),
    sprintf("min_total_iters=%d", constants$min_total_iters),
    sprintf("converged=%s", if (isTRUE(fit_result$converged)) "true" else "false"),
    sprintf("iterations_completed=%d", fit_result$iterations_completed),
    sprintf("convergence_reason=%s", fit_result$convergence_reason),
    sprintf("elbo_tol=%s", as.character(fit_result$convergence_metrics[["elbo_tol"]])),
    sprintf("elbo_rel_tol=%s", as.character(fit_result$convergence_metrics[["elbo_rel_tol"]])),
    sprintf("crit_elbo=%s", as.character(fit_result$convergence_metrics[["crit_elbo"]])),
    sprintf("crit_elbo_rel=%s", as.character(fit_result$convergence_metrics[["crit_elbo_rel"]])),
    sprintf("sigma=%.8f", fit_result$sigma),
    sprintf("sigma_mean=%.8f", suppressWarnings(as.numeric(fit_result$sigma_mean))),
    sprintf("sigma_usgs=%.8f", suppressWarnings(as.numeric(fit_result$sigma_by_source[["usgs"]]))),
    sprintf("sigma_nws=%.8f", suppressWarnings(as.numeric(fit_result$sigma_by_source[["nws"]]))),
    sprintf("sigma_glofas=%.8f", suppressWarnings(as.numeric(fit_result$sigma_by_source[["glofas"]]))),
    sprintf("w_hist=%.8f", fit_result$w_hist),
    sprintf("w_fore=%.8f", fit_result$w_fore),
    sprintf("df_t=%.8f", fit_result$discount_factors[["df_t"]]),
    sprintf("df_s1=%.8f", fit_result$discount_factors[["df_s1"]]),
    sprintf("df_s2=%.8f", fit_result$discount_factors[["df_s2"]]),
    sprintf("df_s67=%.8f", fit_result$discount_factors[["df_s67"]]),
    sprintf("df_discrep=%.8f", fit_result$discount_factors[["df_discrep"]]),
    sprintf("lambda=%.8f", fit_result$discount_factors[["lambda"]]),
    sprintf("df_trans=%.8f", fit_result$discount_factors[["df_trans"]]),
    sprintf("df_covs=%.8f", fit_result$discount_factors[["df_covs"]]),
    sprintf("T=%d", fit_result$T),
    sprintf("K=%d", fit_result$K),
    sprintf("K_overlap=%d", fit_result$K_overlap),
    sprintf("K_max=%d", fit_result$K_max),
    sprintf("K_vec.nws=%d", fit_result$K_vec[["nws"]]),
    sprintf("K_vec.glofas=%d", fit_result$K_vec[["glofas"]]),
    sprintf("segment_lengths=[%d,%d]", fit_result$segment_lengths[["overlap"]], fit_result$segment_lengths[["extension"]]),
    sprintf("extension_source=%s", fit_result$extension_source),
    sprintf("bridge_source=%s", fit_result$bridge_source),
    sprintf("K_cap=%d", fit_result$K_cap),
    sprintf("nws_len=%d", fit_result$nws_len),
    sprintf("glofas_len=%d", fit_result$glofas_len),
    sprintf("progress_log_path=%s", as.character(fit_result$progress_log_path %||% NA_character_))
  )
  if (is.data.frame(fit_result$covariance_diagnostics) && nrow(fit_result$covariance_diagnostics) > 0L) {
    cov_diag <- fit_result$covariance_diagnostics
    cov_min <- suppressWarnings(min(as.numeric(cov_diag$min_eig_min), na.rm = TRUE))
    cov_fail <- suppressWarnings(sum(as.integer(cov_diag$base_chol_fail_slices), na.rm = TRUE))
    cov_nonfinite <- suppressWarnings(sum(as.integer(cov_diag$nonfinite_slices), na.rm = TRUE))
    if (!is.finite(cov_min)) cov_min <- NA_real_
    if (!is.finite(cov_fail)) cov_fail <- NA_integer_
    if (!is.finite(cov_nonfinite)) cov_nonfinite <- NA_integer_
    summary_lines <- c(
      summary_lines,
      sprintf("cov_diag.min_eig_min=%s", as.character(cov_min)),
      sprintf("cov_diag.base_chol_fail_slices_total=%s", as.character(cov_fail)),
      sprintf("cov_diag.nonfinite_slices_total=%s", as.character(cov_nonfinite))
    )
  }
  if (is.list(fit_result$stabilization)) {
    stab <- fit_result$stabilization
    summary_lines <- c(
      summary_lines,
      sprintf("stabilization.cov_calls=%s", as.character(stab[["cov_calls"]])),
      sprintf("stabilization.cov_projected=%s", as.character(stab[["cov_projected"]])),
      sprintf("stabilization.cov_floor_clipped=%s", as.character(stab[["cov_floor_clipped"]])),
      sprintf("stabilization.cov_cap_clipped=%s", as.character(stab[["cov_cap_clipped"]])),
      sprintf("stabilization.cov_nonfinite_inputs=%s", as.character(stab[["cov_nonfinite_inputs"]])),
      sprintf("stabilization.sigma_upper_cap=%s", as.character(stab[["sigma_upper_cap"]])),
      sprintf("stabilization.sigma_update_damping=%s", as.character(stab[["sigma_update_damping"]])),
      sprintf("stabilization.sigma_capped_total=%s", as.character(stab[["sigma_capped_total"]])),
      sprintf("stabilization.sigma_damped_total=%s", as.character(stab[["sigma_damped_total"]])),
      sprintf("stabilization.latent_var_cap_last=%s", as.character(stab[["latent_var_cap_last"]])),
      sprintf("stabilization.latent_var_clipped_total=%s", as.character(stab[["latent_var_clipped_total"]]))
    )
  }
  if (!is.null(log_path) && nzchar(log_path)) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(summary_lines, con = log_path)
  } else {
    writeLines(summary_lines)
  }

  invisible(
    list(
      output_path = output_path,
      sigma = fit_result$sigma,
      sigma_by_source = fit_result$sigma_by_source,
      sigma_mean = fit_result$sigma_mean,
      w_hist = fit_result$w_hist,
      w_fore = fit_result$w_fore,
      discount_factors = fit_result$discount_factors,
      kalman_backend = constants$kalman_backend,
      forecast_transfer_mode = as.character(constants$forecast_transfer_mode),
      transfer_active_forecast_window = if (!is.null(fit_result$transfer_active_forecast_window)) {
        isTRUE(fit_result$transfer_active_forecast_window)
      } else {
        FALSE
      }
    )
  )
}
