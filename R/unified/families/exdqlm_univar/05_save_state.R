univar_theory_embed_states <- function(fit_result, constants) {
  d <- constants$state_dim
  d_act <- constants$active_dim
  Tn <- ncol(fit_result$smooth_mean)

  sm <- matrix(0, nrow = d, ncol = Tn)
  sm[seq_len(d_act), ] <- fit_result$smooth_mean[seq_len(d_act), , drop = FALSE]

  sC <- array(0, dim = c(d, d, Tn))
  for (t in seq_len(Tn)) {
    sC[seq_len(d_act), seq_len(d_act), t] <- fit_result$smooth_cov[seq_len(d_act), seq_len(d_act), t, drop = FALSE]
  }

  list(sm = sm, sC = sC)
}

univar_theory_sample_theta <- function(sm, sC, constants) {
  set.seed(constants$seed + 11L)
  d <- nrow(sm)
  Tn <- ncol(sm)
  n_draws <- constants$n_draws
  out <- array(0, dim = c(d, Tn, n_draws))

  for (t in seq_len(Tn)) {
    Sigma <- sC[, , t]
    Sigma <- as.matrix(Sigma)
    Sigma <- (Sigma + t(Sigma)) / 2 + diag(1e-8, d)
    L <- tryCatch(chol(Sigma), error = function(e) chol(Sigma + diag(1e-6, d)))
    Z <- matrix(stats::rnorm(d * n_draws), nrow = d, ncol = n_draws)
    draws_t <- sm[, t] + L %*% Z
    out[, t, ] <- draws_t
  }
  out
}

univar_theory_pack_compat_outputs <- function(fit_result, constants) {
  q_num <- as.integer(constants$q_num)
  embedded <- univar_theory_embed_states(fit_result, constants)
  sm <- embedded$sm
  sC <- embedded$sC
  Tn <- ncol(sm)

  exps <- rbind(fit_result$fitted_mean, fit_result$fitted_mean)
  rownames(exps) <- c("median", "mean")

  sm_ens_1 <- sm[seq_len(7), , drop = FALSE]
  sm_ens_2 <- sm[8:14, , drop = FALSE]
  sC_ens_1 <- sC[seq_len(7), seq_len(7), , drop = FALSE]
  sC_ens_2 <- sC[8:14, 8:14, , drop = FALSE]

  n_draws <- constants$n_draws
  samp_theta <- univar_theory_sample_theta(sm, sC, constants)
  samp_sigma <- matrix(rep(fit_result$sigma, n_draws), nrow = 1)
  samp_gamma <- matrix(rep(fit_result$gamma, n_draws), nrow = 1)

  suffix <- sprintf("%d_exAL_synth_DISC_uni", q_num)
  obj_names <- list(
    new_theta = sprintf("new.theta.out_%s", suffix),
    samp_theta = sprintf("samp.theta_%s", suffix),
    samp_sigma = sprintf("samp.sigma_%s", suffix),
    samp_gamma = sprintf("samp.gamma_%s", suffix),
    seq_elbo = sprintf("seq.elbo_%s", suffix)
  )

  out_env <- new.env(parent = emptyenv())
  assign(
    obj_names$new_theta,
    list(
      sm = sm,
      sC = sC,
      exps = exps,
      sm_ens = list(sm_ens_1, sm_ens_2),
      sC_ens = list(sC_ens_1, sC_ens_2)
    ),
    envir = out_env
  )
  assign(obj_names$samp_theta, samp_theta, envir = out_env)
  assign(obj_names$samp_sigma, samp_sigma, envir = out_env)
  assign(obj_names$samp_gamma, samp_gamma, envir = out_env)
  assign(obj_names$seq_elbo, univar_theory_elbo_trace(fit_result), envir = out_env)

  assign(
    "exdqlm_univar_theory_state",
    list(
      quantile = q_num,
      p0 = fit_result$p0,
      sigma = fit_result$sigma,
      gamma = fit_result$gamma,
      T = Tn,
      active_dim = as.integer(constants$active_dim),
      state_dim = as.integer(constants$state_dim),
      state_model = "rw_identity",
      q_diag = c(0.05, rep(0.01, max(as.integer(constants$active_dim) - 1L, 0L))),
      likelihood_mode = if (!is.null(fit_result$likelihood_mode)) {
        as.character(fit_result$likelihood_mode)
      } else if (!is.null(constants$likelihood_mode)) {
        as.character(constants$likelihood_mode)
      } else {
        "exal"
      }
    ),
    envir = out_env
  )

  out_env
}
