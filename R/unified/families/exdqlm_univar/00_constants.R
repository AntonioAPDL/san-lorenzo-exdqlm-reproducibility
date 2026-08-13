univar_theory_default_gamma_sigma_policy <- function() {
  list(
    warmup_freeze_iters = 5L,
    min_update_iters = 50L,
    min_total_iters = 50L,
    max_iter = 100L,
    freeze_target = "gamma_sigma",
    guard_refreeze_iters = 10L,
    convergence_tol = 1e-6,
    convergence = list(
      elbo_tol = 1e-6,
      elbo_rel_tol = 2.5e-4,
      state_norm_sq_tol = 1e-6,
      state_norm_sq_rel_tol = 2.5e-4,
      sigma_exp_tol = 1e-6,
      sigma_exp_rel_tol = 5e-5,
      gamma_exp_tol = 1e-6,
      gamma_exp_rel_tol = 5e-5
    ),
    init = list(
      mode = "robust",
      gamma = 0.0,
      sigma_floor = 1e-3,
      sigma_scale = 1.0
    ),
    objective_guard = list(
      enabled = TRUE,
      fail_fast = FALSE,
      log_failures = TRUE,
      mode = "adaptive_freeze",
      penalty = 1e12
    )
  )
}

univar_theory_normalize_likelihood_mode <- function(mode = "exal", default = "exal") {
  raw <- as.character(mode)
  if (!length(raw) || is.na(raw[[1L]]) || !nzchar(raw[[1L]])) {
    raw <- default
  } else {
    raw <- raw[[1L]]
  }
  raw <- tolower(trimws(raw))
  if (!(raw %in% c("exal", "al"))) {
    raw <- tolower(trimws(as.character(default)[[1L]]))
    if (!(raw %in% c("exal", "al"))) raw <- "exal"
  }
  raw
}

univar_theory_resolve_gamma_sigma_policy <- function(policy = NULL) {
  out <- univar_theory_default_gamma_sigma_policy()
  if (is.null(policy) || !is.list(policy)) {
    return(out)
  }

  if (!is.null(policy$warmup_freeze_iters)) out$warmup_freeze_iters <- suppressWarnings(as.integer(policy$warmup_freeze_iters))
  if (!is.null(policy$min_update_iters)) out$min_update_iters <- suppressWarnings(as.integer(policy$min_update_iters))
  if (!is.null(policy$min_total_iters)) out$min_total_iters <- suppressWarnings(as.integer(policy$min_total_iters))
  if (!is.null(policy$max_iter)) out$max_iter <- suppressWarnings(as.integer(policy$max_iter))
  if (!is.null(policy$freeze_target)) out$freeze_target <- as.character(policy$freeze_target)
  if (!is.null(policy$guard_refreeze_iters)) out$guard_refreeze_iters <- suppressWarnings(as.integer(policy$guard_refreeze_iters))
  if (!is.null(policy$convergence_tol)) out$convergence_tol <- suppressWarnings(as.numeric(policy$convergence_tol))
  if (is.list(policy$convergence)) {
    if (!is.null(policy$convergence$elbo_tol)) out$convergence$elbo_tol <- suppressWarnings(as.numeric(policy$convergence$elbo_tol))
    if (!is.null(policy$convergence$elbo_rel_tol)) out$convergence$elbo_rel_tol <- suppressWarnings(as.numeric(policy$convergence$elbo_rel_tol))
    if (!is.null(policy$convergence$state_norm_sq_tol)) out$convergence$state_norm_sq_tol <- suppressWarnings(as.numeric(policy$convergence$state_norm_sq_tol))
    if (!is.null(policy$convergence$state_norm_sq_rel_tol)) out$convergence$state_norm_sq_rel_tol <- suppressWarnings(as.numeric(policy$convergence$state_norm_sq_rel_tol))
    if (!is.null(policy$convergence$sigma_exp_tol)) out$convergence$sigma_exp_tol <- suppressWarnings(as.numeric(policy$convergence$sigma_exp_tol))
    if (!is.null(policy$convergence$sigma_exp_rel_tol)) out$convergence$sigma_exp_rel_tol <- suppressWarnings(as.numeric(policy$convergence$sigma_exp_rel_tol))
    if (!is.null(policy$convergence$gamma_exp_tol)) out$convergence$gamma_exp_tol <- suppressWarnings(as.numeric(policy$convergence$gamma_exp_tol))
    if (!is.null(policy$convergence$gamma_exp_rel_tol)) out$convergence$gamma_exp_rel_tol <- suppressWarnings(as.numeric(policy$convergence$gamma_exp_rel_tol))
  }

  init <- policy$init
  if (is.list(init)) {
    if (!is.null(init$mode)) out$init$mode <- as.character(init$mode)
    if (!is.null(init$gamma)) out$init$gamma <- suppressWarnings(as.numeric(init$gamma))
    if (!is.null(init$sigma_floor)) out$init$sigma_floor <- suppressWarnings(as.numeric(init$sigma_floor))
    if (!is.null(init$sigma_scale)) out$init$sigma_scale <- suppressWarnings(as.numeric(init$sigma_scale))
  }

  objective_guard <- policy$objective_guard
  if (is.list(objective_guard)) {
    if (!is.null(objective_guard$enabled)) out$objective_guard$enabled <- isTRUE(objective_guard$enabled)
    if (!is.null(objective_guard$fail_fast)) out$objective_guard$fail_fast <- isTRUE(objective_guard$fail_fast)
    if (!is.null(objective_guard$log_failures)) out$objective_guard$log_failures <- isTRUE(objective_guard$log_failures)
    if (!is.null(objective_guard$mode)) out$objective_guard$mode <- as.character(objective_guard$mode)
    if (!is.null(objective_guard$penalty)) out$objective_guard$penalty <- suppressWarnings(as.numeric(objective_guard$penalty))
  }

  if (!is.finite(out$warmup_freeze_iters) || out$warmup_freeze_iters < 0L) {
    out$warmup_freeze_iters <- 5L
  }
  out$warmup_freeze_iters <- as.integer(out$warmup_freeze_iters)

  if (!is.finite(out$min_update_iters) || out$min_update_iters < 0L) {
    out$min_update_iters <- 50L
  }
  out$min_update_iters <- as.integer(out$min_update_iters)

  if (!is.finite(out$min_total_iters) || out$min_total_iters < 1L) {
    out$min_total_iters <- 50L
  }
  out$min_total_iters <- as.integer(out$min_total_iters)

  if (!is.finite(out$max_iter) || out$max_iter < 1L) {
    out$max_iter <- 100L
  }
  out$max_iter <- as.integer(out$max_iter)

  if (!(out$freeze_target %in% c("gamma_sigma", "states"))) {
    out$freeze_target <- "gamma_sigma"
  }

  if (!is.finite(out$guard_refreeze_iters) || out$guard_refreeze_iters < 0L) {
    out$guard_refreeze_iters <- 10L
  }
  out$guard_refreeze_iters <- as.integer(out$guard_refreeze_iters)

  if (!is.finite(out$convergence_tol) || out$convergence_tol <= 0) {
    out$convergence_tol <- 1e-6
  }
  if (!is.list(out$convergence)) out$convergence <- list()
  if (!is.finite(out$convergence$elbo_tol) || out$convergence$elbo_tol <= 0) {
    out$convergence$elbo_tol <- as.numeric(out$convergence_tol)
  }
  if (!is.finite(out$convergence$elbo_rel_tol) || out$convergence$elbo_rel_tol <= 0) {
    out$convergence$elbo_rel_tol <- 2.5e-4
  }
  if (!is.finite(out$convergence$state_norm_sq_tol) || out$convergence$state_norm_sq_tol <= 0) {
    out$convergence$state_norm_sq_tol <- 1e-6
  }
  if (!is.finite(out$convergence$state_norm_sq_rel_tol) || out$convergence$state_norm_sq_rel_tol <= 0) {
    out$convergence$state_norm_sq_rel_tol <- 2.5e-4
  }
  if (!is.finite(out$convergence$sigma_exp_tol) || out$convergence$sigma_exp_tol <= 0) {
    out$convergence$sigma_exp_tol <- 1e-6
  }
  if (!is.finite(out$convergence$sigma_exp_rel_tol) || out$convergence$sigma_exp_rel_tol <= 0) {
    out$convergence$sigma_exp_rel_tol <- 5e-5
  }
  if (!is.finite(out$convergence$gamma_exp_tol) || out$convergence$gamma_exp_tol <= 0) {
    out$convergence$gamma_exp_tol <- 1e-6
  }
  if (!is.finite(out$convergence$gamma_exp_rel_tol) || out$convergence$gamma_exp_rel_tol <= 0) {
    out$convergence$gamma_exp_rel_tol <- 5e-5
  }

  if (!(out$init$mode %in% c("legacy", "robust"))) {
    out$init$mode <- "robust"
  }
  if (!is.finite(out$init$gamma)) out$init$gamma <- 0.0
  if (!is.finite(out$init$sigma_floor) || out$init$sigma_floor <= 0) out$init$sigma_floor <- 1e-3
  if (!is.finite(out$init$sigma_scale) || out$init$sigma_scale <= 0) out$init$sigma_scale <- 1.0

  if (!is.logical(out$objective_guard$enabled)) out$objective_guard$enabled <- TRUE
  if (!is.logical(out$objective_guard$fail_fast)) out$objective_guard$fail_fast <- FALSE
  if (!is.logical(out$objective_guard$log_failures)) out$objective_guard$log_failures <- TRUE
  if (!(out$objective_guard$mode %in% c("penalty", "adaptive_freeze"))) {
    out$objective_guard$mode <- "adaptive_freeze"
  }
  if (!is.finite(out$objective_guard$penalty) || out$objective_guard$penalty <= 0) {
    out$objective_guard$penalty <- 1e12
  }

  out
}

univar_theory_constants <- function(q_num, seed = 777L, gamma_sigma_policy = NULL, likelihood_mode = "exal") {
  q_num <- as.integer(q_num)
  p0 <- max(min(q_num / 100, 0.995), 0.005)

  list(
    q_num = q_num,
    q_label = sprintf("%02d", q_num),
    p0 = p0,
    state_dim = 26L,
    active_dim = 6L,
    n_iter = 12L,
    n_draws = 32L,
    seed = as.integer(seed),
    a_sigma = 2.0,
    b_sigma = 2.0,
    m_gamma = 0.0,
    s_gamma = 1.0,
    nu_gamma = 6.0,
    gamma_sigma_policy = univar_theory_resolve_gamma_sigma_policy(gamma_sigma_policy),
    likelihood_mode = univar_theory_normalize_likelihood_mode(likelihood_mode, default = "exal")
  )
}
