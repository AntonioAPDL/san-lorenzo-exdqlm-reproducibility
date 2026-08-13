univar_theory_exal_log_g <- function(gamma) {
  log(2) + stats::pnorm(-abs(gamma), log.p = TRUE) + (gamma^2) / 2
}

univar_theory_exal_g <- function(gamma) {
  exp(univar_theory_exal_log_g(gamma))
}

univar_theory_gamma_bounds <- function(
  p0,
  search_upper = 12,
  search_max = 200,
  growth = 1.5
) {
  stopifnot(is.finite(p0), p0 > 0, p0 < 1)
  if (!is.finite(search_upper) || search_upper <= 0) search_upper <- 12
  if (!is.finite(search_max) || search_max <= search_upper) search_max <- max(200, search_upper * 2)
  if (!is.finite(growth) || growth <= 1) growth <- 1.5

  find_root <- function(target) {
    if (!is.finite(target) || target <= 0 || target >= 1) {
      stop(sprintf("invalid gamma-bound target: %s", as.character(target)), call. = FALSE)
    }
    log_target <- log(target)
    h <- function(x) univar_theory_exal_log_g(x) - log_target

    hi <- as.numeric(search_upper)
    h_hi <- h(hi)
    it <- 0L
    while (is.finite(h_hi) && h_hi > 0 && hi < search_max) {
      hi <- min(search_max, hi * growth)
      h_hi <- h(hi)
      it <- it + 1L
      if (it > 512L) {
        break
      }
    }

    if (!is.finite(h_hi)) {
      stop(
        sprintf(
          "failed to bracket gamma feasibility root: non-finite objective at hi=%s for target=%s",
          format(hi, digits = 16),
          format(target, digits = 16)
        ),
        call. = FALSE
      )
    }
    if (h_hi > 0) {
      stop(
        sprintf(
          "failed to bracket gamma feasibility root: target=%s search_max=%s",
          format(target, digits = 16),
          format(search_max, digits = 16)
        ),
        call. = FALSE
      )
    }

    root <- tryCatch(
      stats::uniroot(h, lower = 0, upper = hi)$root,
      error = function(e) e
    )
    if (inherits(root, "error") || !is.finite(root)) {
      msg <- if (inherits(root, "error")) conditionMessage(root) else "non-finite root"
      stop(
        sprintf(
          "failed to solve gamma feasibility root for target=%s: %s",
          format(target, digits = 16),
          msg
        ),
        call. = FALSE
      )
    }
    as.numeric(root)
  }

  c(
    L = -find_root(1 - p0),
    U = find_root(p0)
  )
}

univar_theory_exal_map <- function(p0, gamma, eps = 1e-10) {
  log_g <- univar_theory_exal_log_g(gamma)
  if (!is.finite(log_g)) {
    stop("invalid log g(p0, gamma)", call. = FALSE)
  }
  g <- exp(log_g)
  ind_neg <- as.numeric(gamma < 0)
  ind_pos <- as.numeric(gamma > 0)
  signed_num <- p0 - ind_neg
  ratio_abs <- exp(log(abs(signed_num)) - log_g)
  if (!is.finite(ratio_abs)) {
    stop("invalid p(p0, gamma) ratio", call. = FALSE)
  }
  p <- ind_neg + sign(signed_num) * ratio_abs

  if (!is.finite(p) || p <= eps || p >= 1 - eps) {
    stop("invalid p(p0, gamma) outside (0,1)", call. = FALSE)
  }

  A <- (1 - 2 * p) / (p * (1 - p))
  B <- 2 / (p * (1 - p))
  denom <- ind_pos - p
  if (!is.finite(denom) || abs(denom) < eps) {
    stop("invalid C denominator", call. = FALSE)
  }
  C <- 1 / denom

  list(g = g, p = p, A = A, B = B, C = C)
}

univar_theory_gig_moment <- function(lambda, chi, psi, r) {
  chi <- pmax(chi, 1e-10)
  psi <- pmax(psi, 1e-10)
  delta <- sqrt(chi * psi)
  kn <- besselK(delta, nu = lambda + r, expon.scaled = TRUE)
  kd <- besselK(delta, nu = lambda, expon.scaled = TRUE)
  ratio <- kn / kd
  out <- (chi / psi)^(r / 2) * ratio
  out[!is.finite(out)] <- NA_real_
  out
}

univar_theory_truncnorm_pos_moments <- function(mu, var) {
  var <- pmax(var, 1e-10)
  sdv <- sqrt(var)
  alpha <- -mu / sdv
  tail_prob <- stats::pnorm(alpha, lower.tail = FALSE)
  ratio <- stats::dnorm(alpha) / pmax(tail_prob, 1e-15)
  large <- alpha > 8
  ratio[large] <- alpha[large] + 1 / pmax(alpha[large], 1e-6)

  mean <- mu + sdv * ratio
  second <- var + mu^2 + sdv * mu * ratio
  list(mean = mean, var = pmax(second - mean^2, 1e-10))
}

univar_theory_kalman_smoother <- function(y, F_mat, R_vec, q_diag, m0, C0) {
  y <- as.numeric(y)
  R_vec <- pmax(as.numeric(R_vec), 1e-10)
  F_mat <- as.matrix(F_mat)
  Tn <- length(y)
  d <- ncol(F_mat)
  if (nrow(F_mat) != Tn) {
    stop("F_mat row count must match y length", call. = FALSE)
  }

  Q <- diag(pmax(as.numeric(q_diag), 1e-10), d)
  m0 <- as.numeric(m0)
  C0 <- as.matrix(C0)

  a <- matrix(0, nrow = d, ncol = Tn)
  m <- matrix(0, nrow = d, ncol = Tn)
  Rpred <- array(0, dim = c(d, d, Tn))
  C <- array(0, dim = c(d, d, Tn))

  m_prev <- m0
  C_prev <- C0

  for (t in seq_len(Tn)) {
    F_t <- matrix(F_mat[t, ], ncol = 1)
    a_t <- m_prev
    R_t <- C_prev + Q
    Qy <- as.numeric(crossprod(F_t, R_t %*% F_t)) + R_vec[t]
    Qy <- max(Qy, 1e-10)
    K <- as.vector((R_t %*% F_t) / Qy)
    innov <- y[t] - as.numeric(crossprod(F_t, a_t))
    m_t <- a_t + K * innov
    C_t <- R_t - (R_t %*% (F_t %*% t(F_t)) %*% R_t) / Qy
    C_t <- (C_t + t(C_t)) / 2

    a[, t] <- a_t
    m[, t] <- m_t
    Rpred[, , t] <- R_t
    C[, , t] <- C_t
    m_prev <- m_t
    C_prev <- C_t
  }

  ms <- m
  Cs <- C
  lag_cov <- array(0, dim = c(d, d, Tn))
  if (Tn >= 2) {
    for (t in (Tn - 1):1) {
      R_next <- Rpred[, , t + 1]
      R_next_inv <- tryCatch(
        solve(R_next),
        error = function(e) {
          solve(R_next + diag(1e-8, d))
        }
      )
      J_t <- C[, , t] %*% R_next_inv
      ms[, t] <- m[, t] + as.vector(J_t %*% (ms[, t + 1] - a[, t + 1]))
      Cs_t <- C[, , t] + J_t %*% (Cs[, , t + 1] - R_next) %*% t(J_t)
      Cs[, , t] <- (Cs_t + t(Cs_t)) / 2
      lag_cov[, , t + 1] <- J_t %*% Cs[, , t + 1]
    }
  }

  fitted_mean <- rowSums(F_mat * t(ms))
  fitted_var <- vapply(
    seq_len(Tn),
    function(t) as.numeric(crossprod(F_mat[t, ], Cs[, , t] %*% F_mat[t, ])),
    numeric(1)
  )

  list(
    smooth_mean = ms,
    smooth_cov = Cs,
    lag_cov = lag_cov,
    fitted_mean = fitted_mean,
    fitted_var = pmax(fitted_var, 1e-10)
  )
}
