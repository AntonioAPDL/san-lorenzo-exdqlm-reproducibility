univar_theory_log_joint_sigma_gamma <- function(
  sigma,
  gamma,
  y,
  eta,
  Ev,
  Es,
  p0,
  constants,
  bounds = NULL,
  likelihood_mode = "exal"
) {
  likelihood_mode <- univar_theory_normalize_likelihood_mode(likelihood_mode, default = "exal")
  if (!is.finite(sigma) || sigma <= 0 || !is.finite(gamma)) return(-Inf)

  if (identical(likelihood_mode, "al")) {
    map <- tryCatch(univar_theory_exal_map(p0, 0), error = function(e) NULL)
    if (is.null(map)) return(-Inf)
    v <- pmax(Ev, 1e-10)
    resid <- y - eta - map$A * v
    ll <- -0.5 * sum(log(sigma * map$B * v) + resid^2 / (sigma * map$B * v))

    a_sigma <- constants$a_sigma
    b_sigma <- constants$b_sigma
    lp_sigma <- a_sigma * log(b_sigma) - lgamma(a_sigma) - (a_sigma + 1) * log(sigma) - b_sigma / sigma
    return(ll + lp_sigma)
  }

  if (is.null(bounds)) {
    bounds <- univar_theory_gamma_bounds(p0)
  }
  if (gamma <= bounds["L"] || gamma >= bounds["U"]) return(-Inf)

  map <- tryCatch(univar_theory_exal_map(p0, gamma), error = function(e) NULL)
  if (is.null(map)) return(-Inf)

  v <- pmax(Ev, 1e-10)
  s <- pmax(Es, 0)
  resid <- y - eta - map$A * v - map$C * sigma * abs(gamma) * s

  ll <- -0.5 * sum(log(sigma * map$B * v) + resid^2 / (sigma * map$B * v))

  a_sigma <- constants$a_sigma
  b_sigma <- constants$b_sigma
  lp_sigma <- a_sigma * log(b_sigma) - lgamma(a_sigma) - (a_sigma + 1) * log(sigma) - b_sigma / sigma

  m_gamma <- constants$m_gamma
  s_gamma <- constants$s_gamma
  nu_gamma <- constants$nu_gamma
  z <- (gamma - m_gamma) / s_gamma
  Z <- stats::pt((bounds["U"] - m_gamma) / s_gamma, df = nu_gamma) -
    stats::pt((bounds["L"] - m_gamma) / s_gamma, df = nu_gamma)
  lp_gamma <- stats::dt(z, df = nu_gamma, log = TRUE) - log(s_gamma) - log(max(Z, 1e-12))

  ll + lp_sigma + lp_gamma
}

univar_theory_metric_delta <- function(current, previous, abs_tol, rel_tol) {
  abs_delta <- NA_real_
  rel_delta <- NA_real_
  conv_abs <- FALSE
  conv_rel <- FALSE

  if (is.finite(current) && is.finite(previous)) {
    abs_delta <- abs(current - previous)
    scale <- max(1.0, abs(current), abs(previous))
    rel_delta <- abs_delta / scale
    conv_abs <- is.finite(abs_delta) && is.finite(abs_tol) && (abs_delta < abs_tol)
    conv_rel <- is.finite(rel_delta) && is.finite(rel_tol) && (rel_delta < rel_tol)
  }

  list(
    abs_delta = abs_delta,
    rel_delta = rel_delta,
    converged = isTRUE(conv_abs) || isTRUE(conv_rel),
    conv_abs = isTRUE(conv_abs),
    conv_rel = isTRUE(conv_rel)
  )
}

univar_theory_run_cavi <- function(inputs, constants) {
  fmt_iter_num <- function(x, digits = 8L) {
    if (!is.finite(x)) {
      return("NA")
    }
    format(signif(as.numeric(x), digits = as.integer(digits)), trim = TRUE, scientific = FALSE)
  }

  set.seed(constants$seed)

  y <- as.numeric(inputs$y)
  X <- as.matrix(inputs$X)
  if (!all(is.finite(y))) {
    stop("univar theory run received non-finite y", call. = FALSE)
  }
  if (nrow(X) != length(y)) {
    stop("univar theory covariate rows must match y length", call. = FALSE)
  }

  p0 <- constants$p0
  likelihood_mode <- univar_theory_normalize_likelihood_mode(constants$likelihood_mode, default = "exal")
  bounds <- if (identical(likelihood_mode, "al")) c(L = -1, U = 1) else univar_theory_gamma_bounds(p0)
  policy <- constants$gamma_sigma_policy
  if (!is.list(policy)) {
    policy <- univar_theory_default_gamma_sigma_policy()
  }

  if (identical(policy$init$mode, "robust")) {
    robust_spread <- suppressWarnings(stats::mad(y, center = stats::median(y), constant = 1.4826, na.rm = TRUE))
    if (!is.finite(robust_spread) || robust_spread <= 0) {
      robust_spread <- suppressWarnings(stats::sd(y))
    }
    if (!is.finite(robust_spread) || robust_spread <= 0) {
      robust_spread <- 0.1
    }
    sigma <- max(policy$init$sigma_floor, policy$init$sigma_scale * robust_spread)
    gamma <- if (identical(likelihood_mode, "al")) {
      0
    } else {
      min(max(policy$init$gamma, bounds["L"] + 1e-6), bounds["U"] - 1e-6)
    }
    if (isTRUE(policy$objective_guard$log_failures)) {
      cat(
        sprintf(
          "[gamsig_init] p0=%s mode=robust likelihood_mode=%s gamma_seed=%0.6f sigma_seed=%0.6f\n",
          as.character(p0),
          as.character(likelihood_mode),
          as.numeric(gamma),
          as.numeric(sigma)
        )
      )
    }
  } else {
    gamma <- if (identical(likelihood_mode, "al")) 0 else max(min(0, bounds["U"] - 1e-4), bounds["L"] + 1e-4)
    sigma <- max(stats::sd(y), 0.1)
  }

  Tn <- length(y)
  d_act <- constants$active_dim
  F_mat <- cbind(1, X[, seq_len(d_act - 1), drop = FALSE])
  m0 <- rep(0, d_act)
  C0 <- diag(c(5, rep(1, d_act - 1)), d_act)
  q_diag <- c(0.05, rep(0.01, d_act - 1))

  min_update_iters <- suppressWarnings(as.integer(policy$min_update_iters))
  if (!is.finite(min_update_iters) || min_update_iters < 0L) {
    min_update_iters <- 50L
  }
  min_total_iters <- suppressWarnings(as.integer(policy$min_total_iters))
  if (!is.finite(min_total_iters) || min_total_iters < 1L) {
    min_total_iters <- 50L
  }
  convergence_tol <- suppressWarnings(as.numeric(policy$convergence_tol))
  if (!is.finite(convergence_tol) || convergence_tol <= 0) {
    convergence_tol <- 1e-6
  }
  convergence <- policy$convergence
  if (!is.list(convergence)) convergence <- list()
  elbo_tol <- suppressWarnings(as.numeric(convergence$elbo_tol))
  if (!is.finite(elbo_tol) || elbo_tol <= 0) {
    elbo_tol <- convergence_tol
  }
  elbo_rel_tol <- suppressWarnings(as.numeric(convergence$elbo_rel_tol))
  if (!is.finite(elbo_rel_tol) || elbo_rel_tol <= 0) {
    elbo_rel_tol <- 2.5e-4
  }
  state_norm_sq_tol <- suppressWarnings(as.numeric(convergence$state_norm_sq_tol))
  if (!is.finite(state_norm_sq_tol) || state_norm_sq_tol <= 0) {
    state_norm_sq_tol <- 1e-6
  }
  state_norm_sq_rel_tol <- suppressWarnings(as.numeric(convergence$state_norm_sq_rel_tol))
  if (!is.finite(state_norm_sq_rel_tol) || state_norm_sq_rel_tol <= 0) {
    state_norm_sq_rel_tol <- 2.5e-4
  }
  sigma_exp_tol <- suppressWarnings(as.numeric(convergence$sigma_exp_tol))
  if (!is.finite(sigma_exp_tol) || sigma_exp_tol <= 0) {
    sigma_exp_tol <- 1e-6
  }
  sigma_exp_rel_tol <- suppressWarnings(as.numeric(convergence$sigma_exp_rel_tol))
  if (!is.finite(sigma_exp_rel_tol) || sigma_exp_rel_tol <= 0) {
    sigma_exp_rel_tol <- 5e-5
  }
  gamma_exp_tol <- suppressWarnings(as.numeric(convergence$gamma_exp_tol))
  if (!is.finite(gamma_exp_tol) || gamma_exp_tol <= 0) {
    gamma_exp_tol <- 1e-6
  }
  gamma_exp_rel_tol <- suppressWarnings(as.numeric(convergence$gamma_exp_rel_tol))
  if (!is.finite(gamma_exp_rel_tol) || gamma_exp_rel_tol <= 0) {
    gamma_exp_rel_tol <- 5e-5
  }
  policy_max_iter <- suppressWarnings(as.integer(policy$max_iter))
  if (!is.finite(policy_max_iter) || policy_max_iter < 1L) {
    policy_max_iter <- 100L
  }
  max_iter <- suppressWarnings(as.integer(max(
    constants$n_iter,
    as.integer(policy$warmup_freeze_iters) + min_update_iters + 5L,
    min_total_iters + 5L,
    policy_max_iter
  )))
  if (!is.finite(max_iter) || max_iter < 1L) {
    max_iter <- 100L
  }

  Ev <- rep(1, Tn)
  E1v <- rep(1, Tn)
  Es <- if (identical(likelihood_mode, "al")) rep(0, Tn) else rep(sqrt(2 / pi), Tn)
  elbo <- rep(NA_real_, max_iter)
  gamsig_update_iters <- 0L
  iterations_completed <- 0L
  converged <- FALSE
  convergence_reason <- "max_iter_reached"
  prev_elbo <- NA_real_
  prev_state_norm_sq <- NA_real_
  prev_sigma_exp <- NA_real_
  prev_gamma_exp <- NA_real_
  crit_elbo <- Inf
  crit_elbo_rel <- Inf
  crit_state_norm_sq <- Inf
  crit_state_norm_sq_rel <- Inf
  crit_sigma_exp <- Inf
  crit_sigma_exp_rel <- Inf
  crit_gamma_exp <- Inf
  crit_gamma_exp_rel <- Inf

  gamsig_dynamic_freeze_until_iter <- as.integer(policy$warmup_freeze_iters)
  if (!is.finite(gamsig_dynamic_freeze_until_iter) || gamsig_dynamic_freeze_until_iter < 0L) {
    gamsig_dynamic_freeze_until_iter <- 0L
  }
  if (isTRUE(policy$objective_guard$log_failures)) {
    cat(
      sprintf(
        "[gamsig_policy] p0=%s freeze_target=%s warmup_freeze_iters=%d min_update_iters=%d min_total_iters=%d max_iter=%d elbo_tol=%g elbo_rel_tol=%g state_norm_sq_tol=%g state_norm_sq_rel_tol=%g sigma_exp_tol=%g sigma_exp_rel_tol=%g gamma_exp_tol=%g gamma_exp_rel_tol=%g guard_mode=%s guard_refreeze_iters=%d\n",
        as.character(p0),
        policy$freeze_target,
        as.integer(policy$warmup_freeze_iters),
        as.integer(min_update_iters),
        as.integer(min_total_iters),
        as.integer(max_iter),
        as.numeric(elbo_tol),
        as.numeric(elbo_rel_tol),
        as.numeric(state_norm_sq_tol),
        as.numeric(state_norm_sq_rel_tol),
        as.numeric(sigma_exp_tol),
        as.numeric(sigma_exp_rel_tol),
        as.numeric(gamma_exp_tol),
        as.numeric(gamma_exp_rel_tol),
        policy$objective_guard$mode,
        as.integer(policy$guard_refreeze_iters)
      )
    )
    cat(sprintf("[gamsig_policy] likelihood_mode=%s\n", as.character(likelihood_mode)))
  }

  smoother <- NULL
  eta <- rep(0, Tn)
  for (iter in seq_len(max_iter)) {
    iter_int <- as.integer(iter)
    iterations_completed <- iter_int
    state_frozen_now <- identical(policy$freeze_target, "states") &&
      (gamsig_dynamic_freeze_until_iter > 0L) &&
      (iter_int <= gamsig_dynamic_freeze_until_iter) &&
      (iter_int > 1L)
    if (state_frozen_now && isTRUE(policy$objective_guard$log_failures)) {
      cat(
        sprintf(
          "[gamsig_freeze] p0=%s iter=%d freeze_until_iter=%d target=states\n",
          as.character(p0),
          iter_int,
          as.integer(gamsig_dynamic_freeze_until_iter)
        )
      )
    }

    if (!state_frozen_now) {
      gamma_eff <- if (identical(likelihood_mode, "al")) 0 else gamma
      map <- univar_theory_exal_map(p0, gamma_eff)
      if (identical(likelihood_mode, "al")) {
        y_tilde <- y - map$A * Ev
      } else {
        y_tilde <- y - map$C * sigma * abs(gamma_eff) * Es - map$A * Ev
      }
      R_vec <- pmax(sigma * map$B * Ev, 1e-8)
      smoother <- univar_theory_kalman_smoother(y_tilde, F_mat, R_vec, q_diag, m0, C0)
      eta <- smoother$fitted_mean

      if (identical(likelihood_mode, "al")) {
        r <- y - eta
      } else {
        r <- y - eta - map$C * sigma * abs(gamma_eff) * Es
      }
      chi <- pmax(r^2 / (sigma * map$B), 1e-10)
      psi <- pmax((map$A^2) / (sigma * map$B) + 2 / sigma, 1e-10)

      Ev_new <- univar_theory_gig_moment(lambda = 0.5, chi = chi, psi = psi, r = 1)
      E1v_new <- univar_theory_gig_moment(lambda = 0.5, chi = chi, psi = psi, r = -1)
      Ev_new[!is.finite(Ev_new)] <- Ev[!is.finite(Ev_new)]
      E1v_new[!is.finite(E1v_new)] <- E1v[!is.finite(E1v_new)]
      Ev <- pmax(Ev_new, 1e-8)
      E1v <- pmax(E1v_new, 1e-8)

      if (identical(likelihood_mode, "al")) {
        Es <- rep(0, Tn)
      } else {
        y_circ <- y - eta - map$A * Ev
        Vs <- 1 / (1 + (map$C^2) * sigma * gamma^2 / (map$B * Ev))
        ms <- Vs * (map$C * abs(gamma) / (map$B * Ev)) * y_circ
        tm <- univar_theory_truncnorm_pos_moments(ms, Vs)
        Es <- pmax(tm$mean, 1e-8)
      }
    }

    gamsig_frozen_now <- identical(policy$freeze_target, "gamma_sigma") &&
      (gamsig_dynamic_freeze_until_iter > 0L) &&
      (iter_int <= gamsig_dynamic_freeze_until_iter)
    if (gamsig_frozen_now && isTRUE(policy$objective_guard$log_failures)) {
      cat(
        sprintf(
          "[gamsig_freeze] p0=%s iter=%d freeze_until_iter=%d target=gamma_sigma\n",
          as.character(p0),
          iter_int,
          as.integer(gamsig_dynamic_freeze_until_iter)
        )
      )
    }

    guard_triggered <- FALSE
    guard_message <- NULL
    guard_eval <- function(obj_raw, context_label, theta_s = NA_real_, theta_g = NA_real_) {
      if (is.finite(obj_raw)) return(obj_raw)

      msg <- sprintf(
        "non-finite dq_transf at p0=%s context=%s iter=%d theta_s=%s theta_g=%s",
        as.character(p0),
        context_label,
        iter_int,
        as.character(theta_s),
        as.character(theta_g)
      )
      if (isTRUE(policy$objective_guard$log_failures)) {
        cat(sprintf("[gamsig_guard] %s\n", msg))
      }
      if (isTRUE(policy$objective_guard$enabled)) {
        guard_triggered <<- TRUE
        guard_message <<- msg
        if (isTRUE(policy$objective_guard$fail_fast)) {
          stop(msg, call. = FALSE)
        }
        return(as.numeric(policy$objective_guard$penalty))
      }
      if (isTRUE(policy$objective_guard$fail_fast)) {
        stop(msg, call. = FALSE)
      }
      Inf
    }

    gamsig_updated_now <- FALSE
    if (!gamsig_frozen_now) {
      gamsig_updated_now <- TRUE
      if (!identical(likelihood_mode, "al")) {
        gamma_obj <- function(g) {
          raw <- -univar_theory_log_joint_sigma_gamma(
            sigma = sigma,
            gamma = g,
            y = y,
            eta = eta,
            Ev = Ev,
            Es = Es,
            p0 = p0,
            constants = constants,
            bounds = bounds,
            likelihood_mode = likelihood_mode
          )
          guard_eval(raw, context_label = "univar_gamma_opt", theta_s = sigma, theta_g = g)
        }
        gamma_opt <- tryCatch(
          stats::optimize(gamma_obj, interval = c(bounds["L"] + 1e-5, bounds["U"] - 1e-5)),
          error = function(e) e
        )
        if (inherits(gamma_opt, "error")) {
          msg <- sprintf("gamma optimize failed at iter=%d: %s", iter_int, conditionMessage(gamma_opt))
          if (isTRUE(policy$objective_guard$log_failures)) cat(sprintf("[gamsig_guard] %s\n", msg))
          if (isTRUE(policy$objective_guard$enabled)) {
            guard_triggered <- TRUE
            guard_message <- msg
            if (isTRUE(policy$objective_guard$fail_fast)) stop(msg, call. = FALSE)
          } else if (isTRUE(policy$objective_guard$fail_fast)) {
            stop(msg, call. = FALSE)
          }
        } else {
          gamma <- max(min(gamma_opt$minimum, bounds["U"] - 1e-6), bounds["L"] + 1e-6)
        }
      } else {
        gamma <- 0
      }

      sigma_obj <- function(log_sigma) {
        sigma_candidate <- exp(log_sigma)
        raw <- -univar_theory_log_joint_sigma_gamma(
          sigma = sigma_candidate,
          gamma = gamma,
          y = y,
          eta = eta,
          Ev = Ev,
          Es = Es,
          p0 = p0,
          constants = constants,
          bounds = bounds,
          likelihood_mode = likelihood_mode
        )
        guard_eval(raw, context_label = "univar_sigma_opt", theta_s = sigma_candidate, theta_g = gamma)
      }
      sigma_opt <- tryCatch(
        stats::optimize(sigma_obj, interval = log(c(1e-5, 1e3))),
        error = function(e) e
      )
      if (inherits(sigma_opt, "error")) {
        msg <- sprintf("sigma optimize failed at iter=%d: %s", iter_int, conditionMessage(sigma_opt))
        if (isTRUE(policy$objective_guard$log_failures)) cat(sprintf("[gamsig_guard] %s\n", msg))
        if (isTRUE(policy$objective_guard$enabled)) {
          guard_triggered <- TRUE
          guard_message <- msg
          if (isTRUE(policy$objective_guard$fail_fast)) stop(msg, call. = FALSE)
        } else if (isTRUE(policy$objective_guard$fail_fast)) {
          stop(msg, call. = FALSE)
        }
      } else {
        sigma <- max(exp(sigma_opt$minimum), 1e-8)
      }
    }
    if (isTRUE(gamsig_updated_now) && !isTRUE(guard_triggered)) {
      gamsig_update_iters <- as.integer(gamsig_update_iters + 1L)
    }

    if (isTRUE(guard_triggered) &&
        identical(policy$objective_guard$mode, "adaptive_freeze") &&
        as.integer(policy$guard_refreeze_iters) > 0L) {
      old_freeze_until <- as.integer(gamsig_dynamic_freeze_until_iter)
      gamsig_dynamic_freeze_until_iter <- max(
        as.integer(gamsig_dynamic_freeze_until_iter),
        as.integer(iter + as.integer(policy$guard_refreeze_iters))
      )
      if (isTRUE(policy$objective_guard$log_failures)) {
        cat(
          sprintf(
            "[gamsig_refreeze] p0=%s iter=%d old_until=%d new_until=%d reason=%s\n",
            as.character(p0),
            iter_int,
            old_freeze_until,
            as.integer(gamsig_dynamic_freeze_until_iter),
            ifelse(is.null(guard_message), "", as.character(guard_message))
          )
        )
      }
    }

    elbo[iter] <- univar_theory_log_joint_sigma_gamma(
      sigma = sigma,
      gamma = gamma,
      y = y,
      eta = eta,
      Ev = Ev,
      Es = Es,
      p0 = p0,
      constants = constants,
      bounds = bounds,
      likelihood_mode = likelihood_mode
    )
    delta_elbo <- univar_theory_metric_delta(
      current = elbo[iter],
      previous = prev_elbo,
      abs_tol = elbo_tol,
      rel_tol = elbo_rel_tol
    )
    crit_elbo <- if (is.finite(delta_elbo$abs_delta)) delta_elbo$abs_delta else Inf
    crit_elbo_rel <- if (is.finite(delta_elbo$rel_delta)) delta_elbo$rel_delta else Inf
    prev_elbo <- elbo[iter]
    sigma_exp <- suppressWarnings(as.numeric(sigma))
    gamma_exp <- suppressWarnings(as.numeric(gamma))
    state_norm_sq <- NA_real_
    if (!is.null(smoother) && !is.null(smoother$smooth_mean)) {
      state_norm_sq <- suppressWarnings(as.numeric(sum(smoother$smooth_mean^2, na.rm = TRUE)))
    }
    if (!is.finite(sigma_exp)) sigma_exp <- NA_real_
    if (!is.finite(gamma_exp)) gamma_exp <- NA_real_
    if (!is.finite(state_norm_sq)) state_norm_sq <- NA_real_

    delta_state <- univar_theory_metric_delta(
      current = state_norm_sq,
      previous = prev_state_norm_sq,
      abs_tol = state_norm_sq_tol,
      rel_tol = state_norm_sq_rel_tol
    )
    delta_sigma <- univar_theory_metric_delta(
      current = sigma_exp,
      previous = prev_sigma_exp,
      abs_tol = sigma_exp_tol,
      rel_tol = sigma_exp_rel_tol
    )
    delta_gamma <- univar_theory_metric_delta(
      current = gamma_exp,
      previous = prev_gamma_exp,
      abs_tol = gamma_exp_tol,
      rel_tol = gamma_exp_rel_tol
    )
    crit_state_norm_sq <- if (is.finite(delta_state$abs_delta)) delta_state$abs_delta else Inf
    crit_state_norm_sq_rel <- if (is.finite(delta_state$rel_delta)) delta_state$rel_delta else Inf
    crit_sigma_exp <- if (is.finite(delta_sigma$abs_delta)) delta_sigma$abs_delta else Inf
    crit_sigma_exp_rel <- if (is.finite(delta_sigma$rel_delta)) delta_sigma$rel_delta else Inf
    crit_gamma_exp <- if (is.finite(delta_gamma$abs_delta)) delta_gamma$abs_delta else Inf
    crit_gamma_exp_rel <- if (is.finite(delta_gamma$rel_delta)) delta_gamma$rel_delta else Inf
    prev_state_norm_sq <- state_norm_sq
    prev_sigma_exp <- sigma_exp
    prev_gamma_exp <- gamma_exp

    if (isTRUE(policy$objective_guard$log_failures)) {
      cat(
        sprintf(
          "[gamsig_progress] family=exdqlm_univar p0=%s iter=%d elbo=%s crit_elbo_abs=%s crit_elbo_rel=%s sigma_exp=%s crit_sigma_exp_abs=%s crit_sigma_exp_rel=%s gamma_exp=%s crit_gamma_exp_abs=%s crit_gamma_exp_rel=%s state_norm_sq=%s crit_state_norm_sq_abs=%s crit_state_norm_sq_rel=%s gamsig_update_iters=%d min_update_iters=%d min_total_iters=%d frozen=%s\n",
          as.character(p0),
          iter_int,
          fmt_iter_num(elbo[iter]),
          fmt_iter_num(crit_elbo),
          fmt_iter_num(crit_elbo_rel),
          fmt_iter_num(sigma_exp),
          fmt_iter_num(crit_sigma_exp),
          fmt_iter_num(crit_sigma_exp_rel),
          fmt_iter_num(gamma_exp),
          fmt_iter_num(crit_gamma_exp),
          fmt_iter_num(crit_gamma_exp_rel),
          fmt_iter_num(state_norm_sq),
          fmt_iter_num(crit_state_norm_sq),
          fmt_iter_num(crit_state_norm_sq_rel),
          as.integer(gamsig_update_iters),
          as.integer(min_update_iters),
          as.integer(min_total_iters),
          ifelse(isTRUE(gamsig_frozen_now), "true", "false")
        )
      )
      cat(sprintf("[gamsig_progress] likelihood_mode=%s\n", as.character(likelihood_mode)))
    }

    conv_elbo <- isTRUE(delta_elbo$converged)
    conv_state <- isTRUE(delta_state$converged)
    conv_sigma <- isTRUE(delta_sigma$converged)
    conv_gamma <- isTRUE(delta_gamma$converged)

    if (iter_int > 1L &&
        iter_int >= min_total_iters &&
        is.finite(elbo[iter]) &&
        conv_elbo &&
        conv_state &&
        conv_sigma &&
        conv_gamma &&
        gamsig_update_iters >= min_update_iters) {
      converged <- TRUE
      convergence_reason <- "all_convergence_criteria_met"
      break
    }
  }

  if (gamsig_update_iters < min_update_iters) {
    stop(
      sprintf(
        "univar theory stopped before required gamma/sigma updates: got=%d required=%d",
        as.integer(gamsig_update_iters),
        as.integer(min_update_iters)
      ),
      call. = FALSE
    )
  }
  if (!converged) {
    convergence_reason <- "max_iter_reached"
  }
  if (iterations_completed > 0L) {
    elbo <- elbo[seq_len(iterations_completed)]
  } else {
    elbo <- numeric(0)
  }

  if (is.null(smoother)) {
    stop("univar theory smoother failed to initialize", call. = FALSE)
  }

  list(
    smooth_mean = smoother$smooth_mean,
    smooth_cov = smoother$smooth_cov,
    fitted_mean = smoother$fitted_mean,
    fitted_var = smoother$fitted_var,
    sigma = sigma,
    gamma = gamma,
    Ev = Ev,
    E1v = E1v,
    Es = Es,
    elbo = elbo,
    iterations_completed = as.integer(iterations_completed),
    gamsig_update_iters = as.integer(gamsig_update_iters),
    converged = isTRUE(converged),
    convergence_reason = as.character(convergence_reason),
    p0 = p0,
    bounds = bounds,
    likelihood_mode = likelihood_mode
  )
}
