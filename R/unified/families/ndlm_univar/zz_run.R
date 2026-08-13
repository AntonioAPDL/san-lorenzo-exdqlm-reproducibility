unified_run_ndlm_univar_theory <- function(seed, output_path, log_path = NULL) {
  constants <- ndlm_univar_theory_constants(seed = seed)
  inputs <- ndlm_univar_load_inputs(constants = constants)
  fit_result <- ndlm_univar_fit(inputs = inputs, constants = constants)
  out_env <- ndlm_univar_pack_compat_outputs(fit_result)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  save(list = ls(out_env), file = output_path, envir = out_env)

  summary_lines <- c(
    "implementation_mode=theory_aligned_closed_form",
    sprintf("kalman_backend=%s", constants$kalman_backend),
    sprintf("forecast_transfer_mode=%s", as.character(constants$forecast_transfer_mode)),
    sprintf(
      "transfer_active_forecast_window=%s",
      if (isTRUE(fit_result$transfer_active_forecast_window)) "true" else "false"
    ),
    sprintf("output_path=%s", output_path),
    sprintf("iterations_completed=%d", fit_result$iterations_completed),
    sprintf("converged=%s", if (isTRUE(fit_result$converged)) "true" else "false"),
    sprintf("convergence_reason=%s", as.character(fit_result$convergence_reason)),
    sprintf("sigma=%.8f", suppressWarnings(as.numeric(fit_result$sigma))),
    sprintf("sigma_mean=%.8f", suppressWarnings(as.numeric(fit_result$sigma_mean))),
    sprintf("n_T=%.4f", suppressWarnings(as.numeric(fit_result$n_T))),
    sprintf("S_T=%.8f", suppressWarnings(as.numeric(fit_result$S_T))),
    sprintf("T=%d", as.integer(fit_result$T)),
    sprintf("K=%d", as.integer(fit_result$K)),
    sprintf("K_overlap=%d", as.integer(fit_result$K_overlap)),
    sprintf("K_max=%d", as.integer(fit_result$K_max)),
    sprintf("K_vec.nws=%d", as.integer(fit_result$K_vec[["nws"]])),
    sprintf("K_vec.glofas=%d", as.integer(fit_result$K_vec[["glofas"]])),
    sprintf("segment_lengths=[%d,%d]", as.integer(fit_result$segment_lengths[["overlap"]]), as.integer(fit_result$segment_lengths[["extension"]])),
    sprintf("extension_source=%s", as.character(fit_result$extension_source)),
    sprintf("bridge_source=%s", as.character(fit_result$bridge_source)),
    sprintf("df_t=%.8f", suppressWarnings(as.numeric(fit_result$discount_factors[["df_t"]]))),
    sprintf("df_s1=%.8f", suppressWarnings(as.numeric(fit_result$discount_factors[["df_s1"]]))),
    sprintf("df_s2=%.8f", suppressWarnings(as.numeric(fit_result$discount_factors[["df_s2"]]))),
    sprintf("df_s67=%.8f", suppressWarnings(as.numeric(fit_result$discount_factors[["df_s67"]]))),
    sprintf("lambda=%.8f", suppressWarnings(as.numeric(fit_result$discount_factors[["lambda"]]))),
    sprintf("df_trans=%.8f", suppressWarnings(as.numeric(fit_result$discount_factors[["df_trans"]]))),
    sprintf("df_covs=%.8f", suppressWarnings(as.numeric(fit_result$discount_factors[["df_covs"]]))),
    sprintf("n_draws=%d", as.integer(constants$n_draws)),
    sprintf("horizon_cap=%d", as.integer(constants$horizon_cap))
  )

  cov_diag <- fit_result$covariance_diagnostics
  if (is.data.frame(cov_diag) && nrow(cov_diag) > 0L) {
    summary_lines <- c(
      summary_lines,
      sprintf(
        "cov_diag.min_eig_min=%s",
        as.character(suppressWarnings(min(as.numeric(cov_diag$min_eig_min), na.rm = TRUE)))
      ),
      sprintf(
        "cov_diag.nonfinite_slices_total=%s",
        as.character(suppressWarnings(sum(as.integer(cov_diag$nonfinite_slices), na.rm = TRUE)))
      )
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
      sprintf("stabilization.cov_nonfinite_inputs=%s", as.character(stab[["cov_nonfinite_inputs"]]))
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
      sigma_mean = fit_result$sigma_mean,
      discount_factors = fit_result$discount_factors,
      kalman_backend = constants$kalman_backend,
      forecast_transfer_mode = as.character(constants$forecast_transfer_mode),
      transfer_active_forecast_window = isTRUE(fit_result$transfer_active_forecast_window)
    )
  )
}
