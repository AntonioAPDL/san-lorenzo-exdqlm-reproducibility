ndlm_univar_kalman_backend_normalize <- function(backend = "cpp") {
  backend <- tolower(trimws(as.character(backend[[1L]])))
  if (!nzchar(backend)) backend <- "cpp"
  if (!(backend %in% c("r", "cpp"))) {
    stop(sprintf("ndlm_univar kalman backend must be one of: r, cpp; got '%s'", backend), call. = FALSE)
  }
  backend
}

ndlm_univar_kalman_load_cpp <- function() {
  if (exists("ndlm_univar_filter_forward_cpp", mode = "function", inherits = TRUE) &&
      exists("ndlm_univar_forecast_h_cpp", mode = "function", inherits = TRUE) &&
      exists("ndlm_univar_backward_smoother_cpp", mode = "function", inherits = TRUE)) {
    return(invisible(TRUE))
  }
  if (!requireNamespace("Rcpp", quietly = TRUE)) {
    stop("NDLM univar cpp backend requires package 'Rcpp'", call. = FALSE)
  }

  env_cpp <- Sys.getenv("NDLM_UNIVAR_KALMAN_CPP_PATH", "")
  candidates <- c(
    env_cpp,
    file.path(getwd(), "R", "unified", "families", "ndlm_univar", "ndlm_univar_kalman_backend.cpp"),
    file.path(getwd(), "..", "R", "unified", "families", "ndlm_univar", "ndlm_univar_kalman_backend.cpp"),
    file.path(getwd(), "..", "..", "R", "unified", "families", "ndlm_univar", "ndlm_univar_kalman_backend.cpp")
  )
  candidates <- unique(candidates[nzchar(candidates)])

  cpp_path <- ""
  for (cand in candidates) {
    cand_norm <- normalizePath(cand, mustWork = FALSE)
    if (file.exists(cand_norm)) {
      cpp_path <- cand_norm
      break
    }
  }
  if (!nzchar(cpp_path)) {
    stop(
      sprintf(
        "NDLM univar cpp backend source not found in any candidate path: %s",
        paste(candidates, collapse = " | ")
      ),
      call. = FALSE
    )
  }
  Rcpp::sourceCpp(cpp_path)
  if (!exists("ndlm_univar_filter_forward_cpp", mode = "function", inherits = TRUE)) {
    stop("NDLM univar cpp backend compiled but exported symbols were not found", call. = FALSE)
  }
  invisible(TRUE)
}

ndlm_univar_cov_stabilization_defaults <- function(stabilization = NULL) {
  if (!is.list(stabilization)) stabilization <- list()
  read_num <- function(x, default, min_val = -Inf, max_val = Inf) {
    out <- suppressWarnings(if (is.null(x) || length(x) < 1L) NA_real_ else as.numeric(x[[1L]]))
    if (!is.finite(out)) out <- suppressWarnings(as.numeric(default))
    if (!is.finite(out)) out <- 0
    out <- max(out, as.numeric(min_val))
    out <- min(out, as.numeric(max_val))
    out
  }
  cov_eig_floor <- read_num(stabilization$cov_eig_floor, 1e-8, min_val = 1e-12)
  cov_eig_cap <- read_num(stabilization$cov_eig_cap, 1e8, min_val = cov_eig_floor * 10)
  cov_diag_jitter <- read_num(stabilization$cov_diag_jitter, 1e-10, min_val = 0)
  list(
    cov_eig_floor = cov_eig_floor,
    cov_eig_cap = cov_eig_cap,
    cov_diag_jitter = cov_diag_jitter
  )
}

ndlm_univar_cov_stabilize <- function(Sigma, stabilization = NULL) {
  params <- ndlm_univar_cov_stabilization_defaults(stabilization)
  Sigma <- as.matrix(Sigma)
  if (!is.numeric(Sigma) || nrow(Sigma) != ncol(Sigma)) {
    stop("NDLM univar covariance stabilization requires a numeric square matrix", call. = FALSE)
  }
  if (!all(is.finite(Sigma))) {
    Sigma[!is.finite(Sigma)] <- 0
  }
  Sigma <- (Sigma + t(Sigma)) / 2

  eig <- tryCatch(suppressWarnings(eigen(Sigma, symmetric = TRUE)), error = function(e) NULL)
  if (is.null(eig) || any(!is.finite(eig$values)) || any(!is.finite(eig$vectors))) {
    Sigma <- diag(params$cov_eig_floor, nrow(Sigma))
  } else {
    vals <- pmin(pmax(as.numeric(eig$values), params$cov_eig_floor), params$cov_eig_cap)
    Sigma <- eig$vectors %*% diag(vals, length(vals)) %*% t(eig$vectors)
  }

  Sigma <- (Sigma + t(Sigma)) / 2
  if (params$cov_diag_jitter > 0) {
    Sigma <- Sigma + diag(params$cov_diag_jitter, nrow(Sigma))
  }
  for (ii in seq_len(3L)) {
    min_eval <- tryCatch(
      suppressWarnings(min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values)),
      error = function(e) NA_real_
    )
    if (is.finite(min_eval) && min_eval >= params$cov_eig_floor) {
      break
    }
    shift <- if (!is.finite(min_eval)) params$cov_eig_floor else (params$cov_eig_floor - min_eval)
    shift <- max(shift + params$cov_diag_jitter, params$cov_diag_jitter)
    Sigma <- Sigma + diag(shift, nrow(Sigma))
  }
  (Sigma + t(Sigma)) / 2
}

ndlm_univar_safe_inv <- function(M, jitter = 1e-10) {
  M <- as.matrix(M)
  M <- (M + t(M)) / 2
  out <- tryCatch(solve(M), error = function(e) NULL)
  if (!is.null(out) && all(is.finite(out))) {
    return(out)
  }
  out <- tryCatch(solve(M + diag(max(jitter, 1e-8), nrow(M))), error = function(e) NULL)
  if (!is.null(out) && all(is.finite(out))) {
    return(out)
  }
  sv <- svd(M + diag(max(jitter, 1e-8), nrow(M)))
  d_inv <- 1 / pmax(sv$d, 1e-10)
  sv$v %*% (diag(d_inv, length(d_inv)) %*% t(sv$u))
}

ndlm_univar_filter_step_r <- function(
  F_t,
  G_t,
  W_star_t,
  y_t,
  m_prev,
  C_prev_star,
  n_prev,
  S_prev,
  stabilization = NULL
) {
  F_t <- as.numeric(F_t)
  m_prev <- as.numeric(m_prev)
  G_t <- as.matrix(G_t)
  W_star_t <- as.matrix(W_star_t)
  C_prev_star <- as.matrix(C_prev_star)
  p <- length(m_prev)

  if (length(F_t) != p) stop("F_t length must equal p", call. = FALSE)
  if (!all(dim(G_t) == c(p, p))) stop("G_t must be p x p", call. = FALSE)
  if (!all(dim(W_star_t) == c(p, p))) stop("W_star_t must be p x p", call. = FALSE)
  if (!all(dim(C_prev_star) == c(p, p))) stop("C_prev_star must be p x p", call. = FALSE)

  C_prev_star <- ndlm_univar_cov_stabilize(C_prev_star, stabilization = stabilization)
  W_star_t <- ndlm_univar_cov_stabilize(W_star_t, stabilization = stabilization)

  a_t <- as.vector(G_t %*% m_prev)
  P_t_star <- ndlm_univar_cov_stabilize(G_t %*% C_prev_star %*% t(G_t), stabilization = stabilization)
  R_t_star <- ndlm_univar_cov_stabilize(P_t_star + W_star_t, stabilization = stabilization)

  f_t <- as.numeric(crossprod(F_t, a_t))
  Q_t_star <- as.numeric(1 + crossprod(F_t, R_t_star %*% F_t))
  Q_t_star <- max(Q_t_star, 1e-10)
  e_t <- as.numeric(y_t - f_t)

  A_t <- as.vector((R_t_star %*% F_t) / Q_t_star)
  m_t <- as.vector(a_t + A_t * e_t)
  C_t_star <- ndlm_univar_cov_stabilize(
    R_t_star - (A_t %*% t(A_t)) * Q_t_star,
    stabilization = stabilization
  )

  n_t <- as.numeric(n_prev + 1)
  S_t <- as.numeric((n_prev * S_prev + (e_t^2) / Q_t_star) / n_t)
  if (!is.finite(S_t) || S_t <= 0) S_t <- as.numeric(S_prev)

  R_scale <- S_prev * R_t_star
  Q_scale <- S_prev * Q_t_star
  C_scale <- S_t * C_t_star

  post_scale <- as.numeric(crossprod(F_t, C_scale %*% F_t))
  pred_var_actual <- if (isTRUE(n_prev > 2)) (n_prev / (n_prev - 2)) * Q_scale else NA_real_
  post_var_actual <- if (isTRUE(n_t > 2)) (n_t / (n_t - 2)) * post_scale else NA_real_

  list(
    a = a_t,
    P_star = P_t_star,
    W_star = W_star_t,
    R_star = R_t_star,
    f = f_t,
    Q_star = Q_t_star,
    e = e_t,
    A = A_t,
    m = m_t,
    C_star = C_t_star,
    n = n_t,
    S = S_t,
    R_scale = R_scale,
    Q_scale = Q_scale,
    C_scale = C_scale,
    pred_var_actual = pred_var_actual,
    post_var_actual = post_var_actual,
    stabilization = list(
      calls = 0L,
      cov_projected = 0L,
      cov_floor_clipped = 0L,
      cov_cap_clipped = 0L,
      cov_nonfinite_inputs = 0L
    )
  )
}

ndlm_univar_filter_forward_r <- function(
  y,
  F_mat,
  G_array,
  W_star_array = NULL,
  discount_mat = NULL,
  m0,
  C0_star,
  n0,
  S0,
  stabilization = NULL
) {
  y <- as.numeric(y)
  F_mat <- as.matrix(F_mat)
  G_array <- as.array(G_array)
  m0 <- as.numeric(m0)
  C0_star <- as.matrix(C0_star)

  Tn <- length(y)
  p <- ncol(F_mat)
  if (nrow(F_mat) != Tn) stop("F_mat row count must match y length", call. = FALSE)
  if (!identical(dim(G_array), c(p, p, Tn))) stop("G_array must be p x p x T", call. = FALSE)
  if (length(m0) != p) stop("m0 length must equal p", call. = FALSE)
  if (!all(dim(C0_star) == c(p, p))) stop("C0_star must be p x p", call. = FALSE)

  use_w <- !is.null(W_star_array)
  use_discount <- !is.null(discount_mat)
  if (use_w) {
    W_star_array <- as.array(W_star_array)
    if (!identical(dim(W_star_array), c(p, p, Tn))) {
      stop("W_star_array must be p x p x T", call. = FALSE)
    }
  }
  if (use_discount) {
    discount_mat <- as.matrix(discount_mat)
    if (!all(dim(discount_mat) == c(p, p))) {
      stop("discount_mat must be p x p", call. = FALSE)
    }
    discount_mat <- pmax((discount_mat + t(discount_mat)) / 2, 0)
  }

  a <- matrix(0, nrow = p, ncol = Tn)
  m <- matrix(0, nrow = p, ncol = Tn)
  A <- matrix(0, nrow = p, ncol = Tn)
  P_star <- array(0, dim = c(p, p, Tn))
  W_star <- array(0, dim = c(p, p, Tn))
  R_star <- array(0, dim = c(p, p, Tn))
  C_star <- array(0, dim = c(p, p, Tn))
  f <- rep(NA_real_, Tn)
  Q_star <- rep(NA_real_, Tn)
  e <- rep(NA_real_, Tn)
  n_prev_seq <- rep(NA_real_, Tn)
  S_prev_seq <- rep(NA_real_, Tn)
  n <- rep(NA_real_, Tn)
  S <- rep(NA_real_, Tn)
  Q_scale <- rep(NA_real_, Tn)
  pred_var_actual <- rep(NA_real_, Tn)
  fitted_mean <- rep(NA_real_, Tn)
  fitted_scale <- rep(NA_real_, Tn)
  fitted_var_actual <- rep(NA_real_, Tn)

  m_prev <- m0
  C_prev <- ndlm_univar_cov_stabilize(C0_star, stabilization = stabilization)
  n_prev <- as.numeric(n0)
  S_prev <- as.numeric(S0)

  for (tt in seq_len(Tn)) {
    F_t <- as.numeric(F_mat[tt, ])
    G_t <- as.matrix(G_array[, , tt])
    P_t <- ndlm_univar_cov_stabilize(G_t %*% C_prev %*% t(G_t), stabilization = stabilization)

    if (use_w) {
      W_t <- as.matrix(W_star_array[, , tt])
    } else if (use_discount) {
      W_t <- discount_mat * P_t
    } else {
      W_t <- matrix(0, nrow = p, ncol = p)
    }

    out <- ndlm_univar_filter_step_r(
      F_t = F_t,
      G_t = G_t,
      W_star_t = W_t,
      y_t = y[tt],
      m_prev = m_prev,
      C_prev_star = C_prev,
      n_prev = n_prev,
      S_prev = S_prev,
      stabilization = stabilization
    )

    a[, tt] <- as.numeric(out$a)
    m[, tt] <- as.numeric(out$m)
    A[, tt] <- as.numeric(out$A)
    P_star[, , tt] <- as.matrix(out$P_star)
    W_star[, , tt] <- as.matrix(out$W_star)
    R_star[, , tt] <- as.matrix(out$R_star)
    C_star[, , tt] <- as.matrix(out$C_star)
    f[tt] <- as.numeric(out$f)
    Q_star[tt] <- as.numeric(out$Q_star)
    e[tt] <- as.numeric(out$e)
    n_prev_seq[tt] <- as.numeric(n_prev)
    S_prev_seq[tt] <- as.numeric(S_prev)
    n[tt] <- as.numeric(out$n)
    S[tt] <- as.numeric(out$S)
    Q_scale[tt] <- as.numeric(out$Q_scale)
    pred_var_actual[tt] <- as.numeric(out$pred_var_actual)

    F_col <- matrix(F_t, ncol = 1L)
    C_scale_t <- out$S * as.matrix(out$C_star)
    fitted_mean[tt] <- as.numeric(crossprod(F_t, out$m))
    fitted_scale[tt] <- as.numeric(crossprod(F_col, C_scale_t %*% F_col))
    fitted_var_actual[tt] <- if (isTRUE(out$n > 2)) (out$n / (out$n - 2)) * fitted_scale[tt] else NA_real_

    m_prev <- as.numeric(out$m)
    C_prev <- as.matrix(out$C_star)
    n_prev <- as.numeric(out$n)
    S_prev <- as.numeric(out$S)
  }

  list(
    a = a,
    m = m,
    A = A,
    P_star = P_star,
    W_star = W_star,
    R_star = R_star,
    C_star = C_star,
    f = f,
    Q_star = Q_star,
    e = e,
    n_prev = n_prev_seq,
    S_prev = S_prev_seq,
    n = n,
    S = S,
    Q_scale = Q_scale,
    pred_var_actual = pred_var_actual,
    fitted_mean = fitted_mean,
    fitted_scale = fitted_scale,
    fitted_var_actual = fitted_var_actual,
    stabilization = list(
      calls = 0L,
      cov_projected = 0L,
      cov_floor_clipped = 0L,
      cov_cap_clipped = 0L,
      cov_nonfinite_inputs = 0L
    )
  )
}

ndlm_univar_forecast_h_r <- function(
  F_future,
  G_future,
  W_star_future = NULL,
  discount_mat = NULL,
  m_t,
  C_t_star,
  n_t,
  S_t,
  stabilization = NULL
) {
  F_future <- as.matrix(F_future)
  G_future <- as.array(G_future)
  m_t <- as.numeric(m_t)
  C_t_star <- as.matrix(C_t_star)

  H <- nrow(F_future)
  p <- ncol(F_future)
  if (H < 1L || p < 1L) stop("F_future must be H x p with H >= 1", call. = FALSE)
  if (!identical(dim(G_future), c(p, p, H))) stop("G_future must be p x p x H", call. = FALSE)
  if (length(m_t) != p) stop("m_t length must equal p", call. = FALSE)
  if (!all(dim(C_t_star) == c(p, p))) stop("C_t_star must be p x p", call. = FALSE)

  use_w <- !is.null(W_star_future)
  use_discount <- !is.null(discount_mat)
  if (use_w) {
    W_star_future <- as.array(W_star_future)
    if (!identical(dim(W_star_future), c(p, p, H))) {
      stop("W_star_future must be p x p x H", call. = FALSE)
    }
  }
  if (use_discount) {
    discount_mat <- as.matrix(discount_mat)
    if (!all(dim(discount_mat) == c(p, p))) {
      stop("discount_mat must be p x p", call. = FALSE)
    }
    discount_mat <- pmax((discount_mat + t(discount_mat)) / 2, 0)
  }

  a <- matrix(0, nrow = p, ncol = H + 1L)
  R_star <- array(0, dim = c(p, p, H + 1L))
  a[, 1] <- m_t
  R_star[, , 1] <- ndlm_univar_cov_stabilize(C_t_star, stabilization = stabilization)

  f <- rep(NA_real_, H)
  Q_star <- rep(NA_real_, H)
  Q_scale <- rep(NA_real_, H)
  Q_var_actual <- rep(NA_real_, H)

  for (hh in seq_len(H)) {
    G_h <- as.matrix(G_future[, , hh])
    F_h <- as.numeric(F_future[hh, ])

    a_h <- as.numeric(G_h %*% a[, hh])
    P_h <- ndlm_univar_cov_stabilize(G_h %*% R_star[, , hh] %*% t(G_h), stabilization = stabilization)

    if (use_w) {
      W_h <- as.matrix(W_star_future[, , hh])
    } else if (use_discount) {
      W_h <- discount_mat * P_h
    } else {
      W_h <- matrix(0, nrow = p, ncol = p)
    }

    R_h <- ndlm_univar_cov_stabilize(P_h + W_h, stabilization = stabilization)
    Q_h_star <- as.numeric(1 + crossprod(F_h, R_h %*% F_h))
    Q_h_star <- max(Q_h_star, 1e-10)

    a[, hh + 1L] <- a_h
    R_star[, , hh + 1L] <- R_h
    f[hh] <- as.numeric(crossprod(F_h, a_h))
    Q_star[hh] <- Q_h_star
    Q_scale[hh] <- as.numeric(S_t) * Q_h_star
    Q_var_actual[hh] <- if (isTRUE(n_t > 2)) (as.numeric(n_t) / (as.numeric(n_t) - 2)) * Q_scale[hh] else NA_real_
  }

  list(
    a = a,
    R_star = R_star,
    f = f,
    Q_star = Q_star,
    Q_scale = Q_scale,
    Q_var_actual = Q_var_actual,
    stabilization = list(
      calls = 0L,
      cov_projected = 0L,
      cov_floor_clipped = 0L,
      cov_cap_clipped = 0L,
      cov_nonfinite_inputs = 0L
    )
  )
}

ndlm_univar_backward_smoother_r <- function(
  m_mat,
  C_star_cube,
  a_mat,
  R_star_cube,
  G_array,
  n_T,
  S_T,
  stabilization = NULL
) {
  m_mat <- as.matrix(m_mat)
  a_mat <- as.matrix(a_mat)
  C_star_cube <- as.array(C_star_cube)
  R_star_cube <- as.array(R_star_cube)
  G_array <- as.array(G_array)

  p <- nrow(m_mat)
  Tn <- ncol(m_mat)
  if (!identical(dim(C_star_cube), c(p, p, Tn))) stop("C_star_cube must be p x p x T", call. = FALSE)
  if (!identical(dim(R_star_cube), c(p, p, Tn))) stop("R_star_cube must be p x p x T", call. = FALSE)
  if (!identical(dim(G_array), c(p, p, Tn))) stop("G_array must be p x p x T", call. = FALSE)
  if (!all(dim(a_mat) == c(p, Tn))) stop("a_mat must be p x T", call. = FALSE)

  a_smooth <- m_mat
  R_smooth_star <- C_star_cube

  if (Tn >= 2L) {
    for (tt in (Tn - 1L):1L) {
      C_t <- ndlm_univar_cov_stabilize(C_star_cube[, , tt], stabilization = stabilization)
      G_next <- as.matrix(G_array[, , tt + 1L])
      R_next <- ndlm_univar_cov_stabilize(R_star_cube[, , tt + 1L], stabilization = stabilization)
      B_t <- C_t %*% t(G_next) %*% ndlm_univar_safe_inv(R_next)

      a_smooth[, tt] <- m_mat[, tt] + as.vector(B_t %*% (a_smooth[, tt + 1L] - a_mat[, tt + 1L]))
      R_t <- C_t + B_t %*% (R_smooth_star[, , tt + 1L] - R_next) %*% t(B_t)
      R_smooth_star[, , tt] <- ndlm_univar_cov_stabilize(R_t, stabilization = stabilization)
    }
  }

  R_smooth_scale <- R_smooth_star
  for (tt in seq_len(Tn)) {
    R_smooth_scale[, , tt] <- as.numeric(S_T) * R_smooth_star[, , tt]
  }

  list(
    a_smooth = a_smooth,
    R_smooth_star = R_smooth_star,
    R_smooth_scale = R_smooth_scale,
    var_factor = if (isTRUE(n_T > 2)) as.numeric(n_T / (n_T - 2)) else NA_real_,
    stabilization = list(
      calls = 0L,
      cov_projected = 0L,
      cov_floor_clipped = 0L,
      cov_cap_clipped = 0L,
      cov_nonfinite_inputs = 0L
    )
  )
}

ndlm_univar_filter_step <- function(..., backend = "cpp", stabilization = NULL) {
  backend <- ndlm_univar_kalman_backend_normalize(backend)
  stab <- ndlm_univar_cov_stabilization_defaults(stabilization)
  if (identical(backend, "cpp")) {
    ndlm_univar_kalman_load_cpp()
    return(ndlm_univar_filter_step_cpp(...,
      cov_eig_floor = as.numeric(stab$cov_eig_floor),
      cov_eig_cap = as.numeric(stab$cov_eig_cap),
      cov_diag_jitter = as.numeric(stab$cov_diag_jitter)
    ))
  }
  ndlm_univar_filter_step_r(..., stabilization = stab)
}

ndlm_univar_filter_forward <- function(..., backend = "cpp", stabilization = NULL) {
  backend <- ndlm_univar_kalman_backend_normalize(backend)
  stab <- ndlm_univar_cov_stabilization_defaults(stabilization)
  if (identical(backend, "cpp")) {
    ndlm_univar_kalman_load_cpp()
    out <- ndlm_univar_filter_forward_cpp(...,
      cov_eig_floor = as.numeric(stab$cov_eig_floor),
      cov_eig_cap = as.numeric(stab$cov_eig_cap),
      cov_diag_jitter = as.numeric(stab$cov_diag_jitter)
    )
    out$f <- as.numeric(out$f)
    out$Q_star <- as.numeric(out$Q_star)
    out$e <- as.numeric(out$e)
    out$n_prev <- as.numeric(out$n_prev)
    out$S_prev <- as.numeric(out$S_prev)
    out$n <- as.numeric(out$n)
    out$S <- as.numeric(out$S)
    out$Q_scale <- as.numeric(out$Q_scale)
    out$pred_var_actual <- as.numeric(out$pred_var_actual)
    out$fitted_mean <- as.numeric(out$fitted_mean)
    out$fitted_scale <- as.numeric(out$fitted_scale)
    out$fitted_var_actual <- as.numeric(out$fitted_var_actual)
    return(out)
  }
  ndlm_univar_filter_forward_r(..., stabilization = stab)
}

ndlm_univar_forecast_h <- function(..., backend = "cpp", stabilization = NULL) {
  backend <- ndlm_univar_kalman_backend_normalize(backend)
  stab <- ndlm_univar_cov_stabilization_defaults(stabilization)
  if (identical(backend, "cpp")) {
    ndlm_univar_kalman_load_cpp()
    out <- ndlm_univar_forecast_h_cpp(...,
      cov_eig_floor = as.numeric(stab$cov_eig_floor),
      cov_eig_cap = as.numeric(stab$cov_eig_cap),
      cov_diag_jitter = as.numeric(stab$cov_diag_jitter)
    )
    out$f <- as.numeric(out$f)
    out$Q_star <- as.numeric(out$Q_star)
    out$Q_scale <- as.numeric(out$Q_scale)
    out$Q_var_actual <- as.numeric(out$Q_var_actual)
    return(out)
  }
  ndlm_univar_forecast_h_r(..., stabilization = stab)
}

ndlm_univar_backward_smoother <- function(..., backend = "cpp", stabilization = NULL) {
  backend <- ndlm_univar_kalman_backend_normalize(backend)
  stab <- ndlm_univar_cov_stabilization_defaults(stabilization)
  if (identical(backend, "cpp")) {
    ndlm_univar_kalman_load_cpp()
    return(ndlm_univar_backward_smoother_cpp(...,
      cov_eig_floor = as.numeric(stab$cov_eig_floor),
      cov_eig_cap = as.numeric(stab$cov_eig_cap),
      cov_diag_jitter = as.numeric(stab$cov_diag_jitter)
    ))
  }
  ndlm_univar_backward_smoother_r(..., stabilization = stab)
}
