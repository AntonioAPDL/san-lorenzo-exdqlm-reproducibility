unified_run_exdqlm_univar_theory <- function(
  q,
  seed,
  output_path,
  log_path = NULL,
  gamma_sigma_policy = NULL,
  likelihood_mode = "exal"
) {
  q_num <- as.integer(round(as.numeric(q) * 100))
  constants <- univar_theory_constants(
    q_num = q_num,
    seed = seed,
    gamma_sigma_policy = gamma_sigma_policy,
    likelihood_mode = likelihood_mode
  )
  inputs <- univar_theory_load_inputs()
  fit_result <- univar_theory_run_cavi(inputs, constants)
  out_env <- univar_theory_pack_compat_outputs(fit_result, constants)
  policy <- constants$gamma_sigma_policy

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  save(list = ls(out_env), file = output_path, envir = out_env)

  summary_lines <- c(
    sprintf("implementation_mode=theory_aligned"),
    sprintf("likelihood_mode=%s", constants$likelihood_mode),
    sprintf("quantile=%0.2f", as.numeric(q)),
    sprintf("q_num=%d", q_num),
    sprintf("output_path=%s", output_path),
    sprintf("sigma=%.8f", fit_result$sigma),
    sprintf("gamma=%.8f", fit_result$gamma),
    sprintf("T=%d", inputs$T),
    sprintf("p0=%.6f", fit_result$p0),
    sprintf("iterations_completed=%d", as.integer(fit_result$iterations_completed)),
    sprintf("gamsig_update_iters=%d", as.integer(fit_result$gamsig_update_iters)),
    sprintf("converged=%s", if (isTRUE(fit_result$converged)) "true" else "false"),
    sprintf("convergence_reason=%s", as.character(fit_result$convergence_reason)),
    sprintf("elbo_final=%.10f", as.numeric(utils::tail(fit_result$elbo, 1))),
    sprintf("gamsig.freeze_target=%s", policy$freeze_target),
    sprintf("gamsig.warmup_freeze_iters=%d", as.integer(policy$warmup_freeze_iters)),
    sprintf("gamsig.min_update_iters=%d", as.integer(policy$min_update_iters)),
    sprintf("gamsig.min_total_iters=%d", as.integer(policy$min_total_iters)),
    sprintf("gamsig.convergence_tol=%g", as.numeric(policy$convergence_tol)),
    sprintf("gamsig.convergence.contract=%s", "abs_or_rel"),
    sprintf("gamsig.convergence.elbo_tol=%g", as.numeric(policy$convergence$elbo_tol)),
    sprintf("gamsig.convergence.elbo_rel_tol=%g", as.numeric(policy$convergence$elbo_rel_tol)),
    sprintf("gamsig.convergence.state_norm_sq_tol=%g", as.numeric(policy$convergence$state_norm_sq_tol)),
    sprintf("gamsig.convergence.state_norm_sq_rel_tol=%g", as.numeric(policy$convergence$state_norm_sq_rel_tol)),
    sprintf("gamsig.convergence.sigma_exp_tol=%g", as.numeric(policy$convergence$sigma_exp_tol)),
    sprintf("gamsig.convergence.sigma_exp_rel_tol=%g", as.numeric(policy$convergence$sigma_exp_rel_tol)),
    sprintf("gamsig.convergence.gamma_exp_tol=%g", as.numeric(policy$convergence$gamma_exp_tol)),
    sprintf("gamsig.convergence.gamma_exp_rel_tol=%g", as.numeric(policy$convergence$gamma_exp_rel_tol)),
    sprintf("gamsig.guard_refreeze_iters=%d", as.integer(policy$guard_refreeze_iters)),
    sprintf("gamsig.init.mode=%s", policy$init$mode),
    sprintf("gamsig.objective_guard.enabled=%s", if (isTRUE(policy$objective_guard$enabled)) "true" else "false"),
    sprintf("gamsig.objective_guard.mode=%s", policy$objective_guard$mode)
  )
  if (!is.null(log_path) && nzchar(log_path)) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(summary_lines, con = log_path)
  } else {
    writeLines(summary_lines)
  }

  invisible(
    list(
      quantile = as.numeric(q),
      q_num = q_num,
      output_path = output_path,
      sigma = fit_result$sigma,
      gamma = fit_result$gamma,
      likelihood_mode = constants$likelihood_mode
    )
  )
}
