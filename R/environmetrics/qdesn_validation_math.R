###############################################################################
# QDESN/exDQLM derivation validation helpers
# Inputs:
#   - Scalar/vector parameters in the same parameterization used by
#     R/environmetrics/20_model_setup.R
# Outputs:
#   - Pure helper functions for test-time consistency and finite checks
# Dependencies:
#   - Base R only
###############################################################################

qdesn_log_g <- function(gam) {
  log(2) + stats::pnorm(-abs(gam), log = TRUE) + 0.5 * gam^2
}

qdesn_L_fn <- function(p0) {
  stats::uniroot(function(gam) exp(qdesn_log_g(gam)) - (1 - p0), c(-1000, 0))$root
}

qdesn_U_fn <- function(p0) {
  stats::uniroot(function(gam) exp(qdesn_log_g(gam)) - p0, c(0, 1000))$root
}

qdesn_p_fn <- function(p0, gam) {
  (p0 - as.numeric(gam < 0)) / exp(qdesn_log_g(gam)) + as.numeric(gam < 0)
}

qdesn_A_fn <- function(p0, gam) {
  temp_p <- qdesn_p_fn(p0, gam)
  (1 - 2 * temp_p) / (temp_p * (1 - temp_p))
}

qdesn_B_fn <- function(p0, gam) {
  temp_p <- qdesn_p_fn(p0, gam)
  2 / (temp_p * (1 - temp_p))
}

qdesn_C_fn <- function(p0, gam) {
  temp_p <- qdesn_p_fn(p0, gam)
  (as.numeric(gam > 0) - temp_p)^(-1)
}

qdesn_mu_univariate <- function(h_t, alpha_t, sigma, gamma, s_t, v_t, p0) {
  as.numeric(h_t * alpha_t) +
    qdesn_C_fn(p0, gamma) * sigma * abs(gamma) * s_t +
    qdesn_A_fn(p0, gamma) * v_t
}

qdesn_mu_multivariate <- function(H_t, alpha_t, sigma, gamma, s_t, v_t, p0) {
  stopifnot(length(sigma) == length(gamma), length(gamma) == length(s_t), length(s_t) == length(v_t))

  A_vec <- vapply(gamma, function(g) qdesn_A_fn(p0, g), numeric(1))
  C_vec <- vapply(gamma, function(g) qdesn_C_fn(p0, g), numeric(1))

  as.numeric(crossprod(H_t, alpha_t)) + C_vec * sigma * abs(gamma) * s_t + A_vec * v_t
}

qdesn_obs_scale_diag <- function(sigma, gamma, v_t, p0) {
  stopifnot(length(sigma) == length(gamma), length(gamma) == length(v_t))
  B_vec <- vapply(gamma, function(g) qdesn_B_fn(p0, g), numeric(1))
  vals <- (sigma^2) * B_vec * v_t
  diag(vals, nrow = length(vals), ncol = length(vals))
}

qdesn_update_uts_chi <- function(y, exps, exps2, sts, sts2, invb_inv_sigma, c_invb_absgam, c2_invb_absgam2_sigma) {
  chi <- invb_inv_sigma * (y^2 - 2 * y * exps + exps2) -
    2 * c_invb_absgam * sts * (y - exps) +
    c2_invb_absgam2_sigma * sts2
  chi[chi <= 0] <- 1e-6
  chi
}

qdesn_theta_to_sig_gam <- function(theta_s, theta_g, p0, boundary_eps = 1e-3) {
  L <- qdesn_L_fn(p0)
  U <- qdesn_U_fn(p0)
  LL <- L + boundary_eps
  UU <- U - boundary_eps

  sig <- exp(theta_s)
  gam <- LL + (-LL + UU) * exp(-exp(theta_g))

  list(sig = sig, gam = gam, L = L, U = U, LL = LL, UU = UU)
}

qdesn_log_prior_gamma_trunc_t <- function(gamma, prior_g, L, U) {
  loc <- prior_g[1]
  scale <- prior_g[2]
  df <- prior_g[3]

  if (gamma <= L || gamma >= U) {
    return(-Inf)
  }

  num <- stats::dt((gamma - loc) / scale, df = df, log = FALSE) / scale
  den <- stats::pt((U - loc) / scale, df = df) - stats::pt((L - loc) / scale, df = df)

  log(num) - log(den)
}

qdesn_dq_transf_no_climate <- function(theta_s, theta_g, y, exps, exps2, sts, sts2, uts, inv_uts, prior_g, prior_s, p0) {
  stopifnot(
    length(y) == length(exps),
    length(exps) == length(exps2),
    length(exps2) == length(sts),
    length(sts) == length(sts2),
    length(sts2) == length(uts),
    length(uts) == length(inv_uts)
  )

  nn <- length(y)
  mapped <- qdesn_theta_to_sig_gam(theta_s = theta_s, theta_g = theta_g, p0 = p0)
  sig <- mapped$sig
  gam <- mapped$gam

  a <- qdesn_A_fn(p0, gam)
  b <- qdesn_B_fn(p0, gam)
  c_val <- qdesn_C_fn(p0, gam)

  yy <- qdesn_log_prior_gamma_trunc_t(gam, prior_g = prior_g, L = mapped$L, U = mapped$U)
  yy <- yy - (prior_s[1] + 1) * log(sig) - prior_s[2] / sig

  yy <- yy - (1.5 * nn) * log(sig) - (0.5 * nn) * log(b) - sum(uts) / sig

  ll_terms <- inv_uts * (y^2 - 2 * y * exps + exps2) / sig -
    (y - exps) * 2 * (inv_uts * c_val * abs(gam) * sts + a / sig) +
    sig * inv_uts * (c_val^2) * (abs(gam)^2) * sts2 +
    2 * c_val * abs(gam) * sts * a +
    (uts * a^2) / sig

  yy <- yy - 0.5 * sum(ll_terms / b)

  # Jacobian from (theta_s, theta_g) -> (sigma, gamma)
  yy <- yy + theta_s + theta_g - exp(theta_g)
  yy
}

qdesn_central_diff <- function(f, x, h = 1e-6) {
  stopifnot(is.numeric(x))
  g <- numeric(length(x))
  for (i in seq_along(x)) {
    xp <- x
    xm <- x
    xp[i] <- xp[i] + h
    xm[i] <- xm[i] - h
    g[i] <- (f(xp) - f(xm)) / (2 * h)
  }
  g
}
