ndlm_univar_make_df_block <- function(df, dim_df, n) {
  if (sum(dim_df) != n) {
    stop("sum(dim_df) must equal n for NDLM univar discount construction", call. = FALSE)
  }
  if (length(df) != length(dim_df)) {
    stop("length(df) must match length(dim_df) for NDLM univar discount construction", call. = FALSE)
  }
  dfs <- pmin(pmax(rep(as.numeric(df), as.integer(dim_df)), 1e-8), 1 - 1e-8)
  idx <- c(0L, cumsum(as.integer(dim_df)))
  out <- matrix(0, nrow = n, ncol = n)
  for (j in seq_len(length(dim_df))) {
    cur <- dfs[idx[j + 1L]]
    scale <- (1 - cur) / cur
    out[(idx[j] + 1L):idx[j + 1L], (idx[j] + 1L):idx[j + 1L]] <- scale
  }
  out
}

ndlm_univar_rotation_block <- function(freq) {
  matrix(c(cos(freq), sin(freq), -sin(freq), cos(freq)), nrow = 2, byrow = TRUE)
}

ndlm_univar_build_base_G <- function(constants) {
  harmonics <- as.numeric(constants$harmonics)
  if (length(harmonics) != 3L || any(!is.finite(harmonics))) {
    stop("ndlm_univar currently requires exactly three finite harmonics", call. = FALSE)
  }
  G <- matrix(0, nrow = 7, ncol = 7)
  G[1, 1] <- 1
  period <- as.numeric(constants$period)
  if (!is.finite(period) || period <= 0) {
    stop("ndlm_univar period must be finite and > 0", call. = FALSE)
  }
  for (j in seq_along(harmonics)) {
    h <- harmonics[[j]]
    w <- 2 * pi * h / period
    blk <- ndlm_univar_rotation_block(w)
    i0 <- 2 + 2 * (j - 1L)
    G[i0:(i0 + 1L), i0:(i0 + 1L)] <- blk
  }
  G
}

ndlm_univar_build_transfer_G <- function(cov_row, lambda) {
  cov_row <- as.numeric(cov_row)
  p_cov <- length(cov_row)
  out <- diag(1, p_cov + 1L)
  out[1, 1] <- as.numeric(lambda)
  if (p_cov > 0L) {
    out[1, 2:(p_cov + 1L)] <- cov_row
  }
  out
}

ndlm_univar_safe_chol <- function(Sigma) {
  Sigma <- as.matrix(Sigma)
  Sigma <- ndlm_univar_cov_stabilize(Sigma)
  jitters <- c(0, 1e-10, 1e-8, 1e-6, 1e-4)
  for (j in jitters) {
    out <- tryCatch(chol(Sigma + diag(j, nrow(Sigma))), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  eig <- eigen((Sigma + t(Sigma)) / 2, symmetric = TRUE)
  vals <- pmax(as.numeric(eig$values), 1e-8)
  S_pd <- eig$vectors %*% diag(vals, length(vals)) %*% t(eig$vectors)
  S_pd <- (S_pd + t(S_pd)) / 2
  chol(S_pd + diag(1e-8, nrow(S_pd)))
}

ndlm_univar_cov_diag_one <- function(object_name, cov_arr) {
  dims <- dim(cov_arr)
  if (is.null(dims) || length(dims) != 3L || dims[1] != dims[2]) {
    return(data.frame(
      object = object_name,
      n_slices = 0L,
      matrix_dim = NA_integer_,
      nonfinite_slices = NA_integer_,
      min_eig_min = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  n_slices <- as.integer(dims[3])
  if (n_slices < 1L) {
    return(data.frame(
      object = object_name,
      n_slices = 0L,
      matrix_dim = as.integer(dims[1]),
      nonfinite_slices = 0L,
      min_eig_min = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  min_eigs <- rep(NA_real_, n_slices)
  nonfinite <- rep(FALSE, n_slices)
  for (k in seq_len(n_slices)) {
    S <- as.matrix(cov_arr[, , k, drop = TRUE])
    if (!all(is.finite(S))) {
      nonfinite[k] <- TRUE
      next
    }
    S <- (S + t(S)) / 2
    min_eigs[k] <- suppressWarnings(min(eigen(S, symmetric = TRUE, only.values = TRUE)$values))
  }
  data.frame(
    object = object_name,
    n_slices = n_slices,
    matrix_dim = as.integer(dims[1]),
    nonfinite_slices = as.integer(sum(nonfinite)),
    min_eig_min = if (all(is.na(min_eigs))) NA_real_ else min(min_eigs, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

ndlm_univar_cov_stabilize_array <- function(cov_arr, stabilization = NULL) {
  dims <- dim(cov_arr)
  if (is.null(dims) || length(dims) != 3L || dims[1] != dims[2]) {
    stop("ndlm_univar covariance array stabilization expects a square 3D array", call. = FALSE)
  }
  out <- cov_arr
  n_slices <- as.integer(dims[3])
  if (!is.finite(n_slices) || n_slices < 1L) {
    return(out)
  }
  for (k in seq_len(n_slices)) {
    out[, , k] <- ndlm_univar_cov_stabilize(out[, , k, drop = TRUE], stabilization = stabilization)
  }
  out
}

ndlm_univar_build_model_sequences <- function(inputs, constants) {
  y <- as.numeric(inputs$y)
  X_hist <- as.matrix(inputs$X_hist)
  X_future <- as.matrix(inputs$X_future)
  Tn <- length(y)
  K <- as.integer(inputs$forecast$K_max)
  if (!is.finite(K) || K < 1L) {
    stop("ndlm_univar requires K_max >= 1", call. = FALSE)
  }

  p_cov <- ncol(X_hist)
  if (!is.finite(p_cov) || p_cov < 1L) {
    stop("ndlm_univar requires at least one transfer covariate", call. = FALSE)
  }
  ppx <- 1L + p_cov
  p_total <- 7L + ppx

  base_G <- ndlm_univar_build_base_G(constants)
  transfer_keep <- isTRUE(constants$transfer_active_forecast_window)

  F_hist <- matrix(0, nrow = Tn, ncol = p_total)
  F_hist[, 1] <- 1
  F_hist[, 8] <- 1

  F_future <- matrix(0, nrow = K, ncol = p_total)
  F_future[, 1] <- 1
  F_future[, 8] <- if (isTRUE(transfer_keep)) 1 else 0

  G_hist <- array(0, dim = c(p_total, p_total, Tn))
  for (tt in seq_len(Tn)) {
    G_t <- matrix(0, nrow = p_total, ncol = p_total)
    G_t[1:7, 1:7] <- base_G
    G_t[8:p_total, 8:p_total] <- ndlm_univar_build_transfer_G(X_hist[tt, ], constants$lambda)
    G_hist[, , tt] <- G_t
  }

  G_future <- array(0, dim = c(p_total, p_total, K))
  for (hh in seq_len(K)) {
    cov_h <- if (isTRUE(transfer_keep)) X_future[hh, ] else rep(0, p_cov)
    G_h <- matrix(0, nrow = p_total, ncol = p_total)
    G_h[1:7, 1:7] <- base_G
    G_h[8:p_total, 8:p_total] <- ndlm_univar_build_transfer_G(cov_h, constants$lambda)
    G_future[, , hh] <- G_h
  }

  y_sd <- suppressWarnings(stats::sd(y, na.rm = TRUE))
  if (!is.finite(y_sd) || y_sd < 1e-3) y_sd <- 1
  y_mu <- suppressWarnings(mean(y, na.rm = TRUE))
  if (!is.finite(y_mu)) y_mu <- 0

  m0 <- rep(0, p_total)
  m0[1] <- y_mu
  C0_star <- diag(c(5, rep(1.5, 6), 2, rep(1, p_cov)), p_total)

  df_base <- ndlm_univar_make_df_block(
    df = c(constants$df_t, constants$df_s1, constants$df_s2, constants$df_s67),
    dim_df = c(1L, 2L, 2L, 2L),
    n = 7L
  )
  df_transfer <- ndlm_univar_make_df_block(
    df = c(constants$df_trans, constants$df_covs),
    dim_df = c(1L, p_cov),
    n = ppx
  )
  discount_mat <- matrix(0, nrow = p_total, ncol = p_total)
  discount_mat[1:7, 1:7] <- df_base
  discount_mat[8:p_total, 8:p_total] <- df_transfer

  list(
    y = y,
    F_hist = F_hist,
    G_hist = G_hist,
    F_future = F_future,
    G_future = G_future,
    m0 = m0,
    C0_star = C0_star,
    discount_mat = discount_mat,
    p_total = as.integer(p_total),
    p_cov = as.integer(p_cov),
    transfer_keep = isTRUE(transfer_keep)
  )
}

ndlm_univar_loglik_sequence <- function(filter_out, n0, S0) {
  Tn <- length(filter_out$e)
  out <- rep(NA_real_, Tn)
  for (tt in seq_len(Tn)) {
    n_prev <- if (tt == 1L) as.numeric(n0) else as.numeric(filter_out$n[tt - 1L])
    S_prev <- if (tt == 1L) as.numeric(S0) else as.numeric(filter_out$S[tt - 1L])
    Q_t <- as.numeric(S_prev * filter_out$Q_star[tt])
    e_t <- as.numeric(filter_out$e[tt])
    if (!is.finite(n_prev) || !is.finite(S_prev) || !is.finite(Q_t) || !is.finite(e_t) ||
        n_prev <= 0 || S_prev <= 0 || Q_t <= 0) {
      out[tt] <- NA_real_
      next
    }
    out[tt] <- lgamma((n_prev + 1) / 2) -
      lgamma(n_prev / 2) -
      0.5 * (log(pi) + log(n_prev) + log(Q_t)) -
      ((n_prev + 1) / 2) * log1p((e_t^2) / (n_prev * Q_t))
  }
  out
}

ndlm_univar_draw_state_t <- function(mean_mat, scale_cov_arr, df, n_draws, seed) {
  set.seed(seed)
  mean_mat <- as.matrix(mean_mat)
  scale_cov_arr <- as.array(scale_cov_arr)
  df <- as.numeric(df)
  if (!is.finite(df) || df <= 0) df <- 1
  p <- nrow(mean_mat)
  Tn <- ncol(mean_mat)
  out <- array(0, dim = c(p, Tn, n_draws))
  for (tt in seq_len(Tn)) {
    L <- ndlm_univar_safe_chol(scale_cov_arr[, , tt, drop = TRUE])
    Z <- matrix(stats::rnorm(p * n_draws), nrow = p, ncol = n_draws)
    chi <- stats::rchisq(n_draws, df = df)
    scale_vec <- sqrt(df / pmax(chi, 1e-10))
    mean_draw <- matrix(mean_mat[, tt], nrow = p, ncol = n_draws)
    out[, tt, ] <- mean_draw + L %*% sweep(Z, MARGIN = 2L, STATS = scale_vec, FUN = "*")
  }
  out
}

ndlm_univar_draw_segment_t <- function(mean_mat, scale_cov_arr, df, n_draws, seed) {
  mean_mat <- as.matrix(mean_mat)
  k_len <- ncol(mean_mat)
  df <- as.numeric(df)
  if (!is.finite(df) || df <= 0) df <- 1
  p <- nrow(mean_mat)
  if (k_len < 1L) {
    return(array(0, dim = c(p, 0L, n_draws)))
  }
  set.seed(seed)
  out <- array(0, dim = c(p, k_len, n_draws))
  for (kk in seq_len(k_len)) {
    L <- ndlm_univar_safe_chol(scale_cov_arr[, , kk, drop = TRUE])
    Z <- matrix(stats::rnorm(p * n_draws), nrow = p, ncol = n_draws)
    chi <- stats::rchisq(n_draws, df = df)
    scale_vec <- sqrt(df / pmax(chi, 1e-10))
    mean_draw <- matrix(mean_mat[, kk], nrow = p, ncol = n_draws)
    out[, kk, ] <- mean_draw + L %*% sweep(Z, MARGIN = 2L, STATS = scale_vec, FUN = "*")
  }
  out
}

ndlm_univar_run_closed_form <- function(inputs, constants) {
  model <- ndlm_univar_build_model_sequences(inputs, constants)
  y <- model$y
  Tn <- length(y)
  K <- nrow(model$F_future)

  filter_out <- ndlm_univar_filter_forward(
    y = y,
    F_mat = model$F_hist,
    G_array = model$G_hist,
    W_star_array = NULL,
    discount_mat = model$discount_mat,
    m0 = model$m0,
    C0_star = model$C0_star,
    n0 = constants$n0,
    S0 = constants$S0,
    backend = constants$kalman_backend,
    stabilization = constants$stabilization
  )

  smoother_out <- ndlm_univar_backward_smoother(
    m_mat = filter_out$m,
    C_star_cube = filter_out$C_star,
    a_mat = filter_out$a,
    R_star_cube = filter_out$R_star,
    G_array = model$G_hist,
    n_T = filter_out$n[Tn],
    S_T = filter_out$S[Tn],
    backend = constants$kalman_backend,
    stabilization = constants$stabilization
  )

  forecast_out <- ndlm_univar_forecast_h(
    F_future = model$F_future,
    G_future = model$G_future,
    W_star_future = NULL,
    discount_mat = model$discount_mat,
    m_t = filter_out$m[, Tn],
    C_t_star = filter_out$C_star[, , Tn],
    n_t = filter_out$n[Tn],
    S_t = filter_out$S[Tn],
    backend = constants$kalman_backend,
    stabilization = constants$stabilization
  )

  # Exported covariances are consumed by diagnostics/post; harden all slices
  # to avoid residual numerical PSD drift after backend scaling/conversions.
  smoother_out$R_smooth_star <- ndlm_univar_cov_stabilize_array(
    smoother_out$R_smooth_star,
    stabilization = constants$stabilization
  )
  smoother_out$R_smooth_scale <- ndlm_univar_cov_stabilize_array(
    smoother_out$R_smooth_scale,
    stabilization = constants$stabilization
  )

  n_T <- as.numeric(filter_out$n[Tn])
  S_T <- as.numeric(filter_out$S[Tn])
  sigma_post_mean <- if (isTRUE(n_T > 2)) (n_T * S_T) / (n_T - 2) else S_T
  sigma_post_mean <- as.numeric(max(sigma_post_mean, 1e-8))

  y_smoothed <- vapply(
    seq_len(Tn),
    function(tt) as.numeric(crossprod(model$F_hist[tt, ], smoother_out$a_smooth[, tt])),
    numeric(1)
  )
  y_smoothed_scale <- vapply(
    seq_len(Tn),
    function(tt) {
      Ft <- matrix(model$F_hist[tt, ], ncol = 1L)
      as.numeric(crossprod(Ft, smoother_out$R_smooth_scale[, , tt] %*% Ft))
    },
    numeric(1)
  )

  K_overlap <- as.integer(inputs$forecast$K_overlap)
  K_max <- as.integer(inputs$forecast$K_max)
  K_tail <- max(K_max - K_overlap, 0L)
  K_vec <- as.integer(inputs$forecast$K_vec)
  names(K_vec) <- names(inputs$forecast$K_vec)

  sm_fore <- forecast_out$a[, -1, drop = FALSE]
  sC_fore <- forecast_out$R_star[, , -1, drop = FALSE]
  for (kk in seq_len(dim(sC_fore)[3])) {
    sC_fore[, , kk] <- ndlm_univar_cov_stabilize(
      S_T * sC_fore[, , kk],
      stabilization = constants$stabilization
    )
  }
  sC_fore <- ndlm_univar_cov_stabilize_array(sC_fore, stabilization = constants$stabilization)

  sm_ens_1 <- if (K_overlap > 0L) sm_fore[, seq_len(K_overlap), drop = FALSE] else matrix(0, nrow = nrow(sm_fore), ncol = 0L)
  sm_ens_2 <- if (K_tail > 0L) sm_fore[, seq.int(K_overlap + 1L, K_max), drop = FALSE] else matrix(0, nrow = nrow(sm_fore), ncol = 0L)

  sC_ens_1 <- if (K_overlap > 0L) sC_fore[, , seq_len(K_overlap), drop = FALSE] else array(0, dim = c(nrow(sm_fore), nrow(sm_fore), 0L))
  sC_ens_2 <- if (K_tail > 0L) sC_fore[, , seq.int(K_overlap + 1L, K_max), drop = FALSE] else array(0, dim = c(nrow(sm_fore), nrow(sm_fore), 0L))
  if (dim(sC_ens_1)[3] > 0L) {
    sC_ens_1 <- ndlm_univar_cov_stabilize_array(sC_ens_1, stabilization = constants$stabilization)
  }
  if (dim(sC_ens_2)[3] > 0L) {
    sC_ens_2 <- ndlm_univar_cov_stabilize_array(sC_ens_2, stabilization = constants$stabilization)
  }

  n_draws <- as.integer(constants$n_draws)
  if (!is.finite(n_draws) || n_draws < 2L) n_draws <- 64L

  samp_theta_retro <- ndlm_univar_draw_state_t(
    mean_mat = smoother_out$a_smooth,
    scale_cov_arr = smoother_out$R_smooth_scale,
    df = n_T,
    n_draws = n_draws,
    seed = constants$seed + 11L
  )
  samp_theta_ens_1 <- ndlm_univar_draw_segment_t(
    mean_mat = sm_ens_1,
    scale_cov_arr = sC_ens_1,
    df = n_T,
    n_draws = n_draws,
    seed = constants$seed + 21L
  )
  samp_theta_ens_2 <- ndlm_univar_draw_segment_t(
    mean_mat = sm_ens_2,
    scale_cov_arr = sC_ens_2,
    df = n_T,
    n_draws = n_draws,
    seed = constants$seed + 22L
  )

  set.seed(constants$seed + 31L)
  samp_sigma <- matrix((n_T * S_T) / pmax(stats::rchisq(n_draws, df = n_T), 1e-10), nrow = 1L)
  rownames(samp_sigma) <- "usgs"

  set.seed(constants$seed + 41L)
  y_fore_draws <- matrix(NA_real_, nrow = n_draws, ncol = K)
  for (hh in seq_len(K)) {
    y_fore_draws[, hh] <- as.numeric(forecast_out$f[hh]) +
      sqrt(max(as.numeric(forecast_out$Q_scale[hh]), 1e-10)) * stats::rt(n_draws, df = n_T)
  }

  standard_forecast_errors <- rep(NA_real_, K_max)
  standard_forecast_errors[seq_len(K_overlap)] <- inputs$forecast$nws[seq_len(K_overlap)] - inputs$forecast$glofas[seq_len(K_overlap)]
  if (K_tail > 0L) {
    if (K_vec[["nws"]] >= K_vec[["glofas"]]) {
      tail_idx <- seq.int(K_overlap + 1L, K_vec[["nws"]])
      bridge <- as.numeric(inputs$forecast$glofas[K_overlap])
      if (!is.finite(bridge)) bridge <- 0
      standard_forecast_errors[seq.int(K_overlap + 1L, K_max)] <- inputs$forecast$nws[tail_idx] - bridge
    } else {
      tail_idx <- seq.int(K_overlap + 1L, K_vec[["glofas"]])
      bridge <- as.numeric(inputs$forecast$nws[K_overlap])
      if (!is.finite(bridge)) bridge <- 0
      standard_forecast_errors[seq.int(K_overlap + 1L, K_max)] <- bridge - inputs$forecast$glofas[tail_idx]
    }
  }
  standard_forecast_errors[!is.finite(standard_forecast_errors)] <- 0
  standard_forecast_errors <- matrix(standard_forecast_errors, nrow = 1L)

  seq_ll <- ndlm_univar_loglik_sequence(filter_out, n0 = constants$n0, S0 = constants$S0)
  seq_elbo <- cumsum(ifelse(is.finite(seq_ll), seq_ll, 0))
  seq_sigma <- matrix(as.numeric(filter_out$S), ncol = 1L)
  colnames(seq_sigma) <- "sigma_exp"
  seq_scale <- cbind(
    sigma_exp = as.numeric(filter_out$S),
    n = as.numeric(filter_out$n),
    Q_scale = as.numeric(filter_out$Q_scale)
  )
  delta <- c(diff(seq_elbo), 0)

  vars <- rbind(y_smoothed_scale, y_smoothed_scale)
  exps <- rbind(y_smoothed, y_smoothed)
  exps2 <- exps^2 + vars

  cov_diag <- rbind(
    ndlm_univar_cov_diag_one("smooth_cov", smoother_out$R_smooth_scale),
    ndlm_univar_cov_diag_one("forecast_cov_segment_1", sC_ens_1),
    ndlm_univar_cov_diag_one("forecast_cov_segment_2", sC_ens_2)
  )
  rownames(cov_diag) <- NULL

  active_set_by_lead <- data.frame(
    lead = seq_len(K_max),
    active_nws = as.integer(seq_len(K_max) <= K_vec[["nws"]]),
    active_glofas = as.integer(seq_len(K_max) <= K_vec[["glofas"]]),
    active_count = as.integer((seq_len(K_max) <= K_vec[["nws"]]) + (seq_len(K_max) <= K_vec[["glofas"]])),
    stringsAsFactors = FALSE
  )
  state_dim_by_lead <- data.frame(
    lead = seq_len(K_max),
    state_dim = rep(model$p_total, K_max),
    stringsAsFactors = FALSE
  )

  fit_diagnostics <- list(
    y_observed = y,
    y_predicted_one_step = as.numeric(filter_out$f),
    y_filtered = as.numeric(filter_out$fitted_mean),
    y_smoothed = as.numeric(y_smoothed),
    var_predicted_one_step = as.numeric(filter_out$Q_scale),
    var_filtered = as.numeric(filter_out$fitted_scale),
    var_smoothed = as.numeric(y_smoothed_scale),
    residual_one_step = as.numeric(y - filter_out$f),
    residual_filtered = as.numeric(y - filter_out$fitted_mean),
    residual_smoothed = as.numeric(y - y_smoothed)
  )

  transfer_mode <- if (isTRUE(model$transfer_keep)) "keep" else "drop"

  new_theta <- list(
    sm = smoother_out$a_smooth,
    sC = smoother_out$R_smooth_scale,
    exps = exps,
    exps2 = exps2,
    vars = vars,
    sm_ens = list(sm_ens_1, sm_ens_2),
    sC_ens = list(sC_ens_1, sC_ens_2),
    standard_forecast_errors = standard_forecast_errors,
    forecast_horizon = list(
      K_vec = K_vec,
      K_overlap = K_overlap,
      K_max = K_max,
      segment_lengths = c(overlap = K_overlap, extension = K_tail),
      extension_source = if (K_vec[["nws"]] >= K_vec[["glofas"]]) "nws" else "glofas",
      bridge_source = if (K_vec[["nws"]] >= K_vec[["glofas"]]) "glofas" else "nws",
      forecast_transfer_mode = transfer_mode,
      transfer_active_forecast_window = isTRUE(model$transfer_keep)
    )
  )

  list(
    new_theta = new_theta,
    samp_theta = list(samp_theta = samp_theta_retro),
    samp_theta_ens = list(list(samp_theta = samp_theta_ens_1), list(samp_theta = samp_theta_ens_2)),
    samp_sigma = samp_sigma,
    y_fore_draws = y_fore_draws,
    seq_sigma = seq_sigma,
    seq_scale = seq_scale,
    seq_elbo = seq_elbo,
    delta = delta,
    iterations_completed = 1L,
    max_iter = 1L,
    converged = TRUE,
    convergence_reason = "closed_form_filter",
    convergence_metrics = c(crit_elbo = NA_real_, crit_elbo_rel = NA_real_),
    sigma = sigma_post_mean,
    sigma_by_source = c(usgs = sigma_post_mean),
    sigma_mean = sigma_post_mean,
    w_hist = mean(diag(model$discount_mat[1:7, 1:7])),
    w_fore = mean(diag(model$discount_mat)),
    discount_factors = c(
      df_t = constants$df_t,
      df_s1 = constants$df_s1,
      df_s2 = constants$df_s2,
      df_s67 = constants$df_s67,
      lambda = constants$lambda,
      df_trans = constants$df_trans,
      df_covs = constants$df_covs
    ),
    K = K_max,
    K_overlap = K_overlap,
    K_max = K_max,
    K_vec = K_vec,
    segment_lengths = c(overlap = K_overlap, extension = K_tail),
    extension_source = if (K_vec[["nws"]] >= K_vec[["glofas"]]) "nws" else "glofas",
    bridge_source = if (K_vec[["nws"]] >= K_vec[["glofas"]]) "glofas" else "nws",
    forecast_transfer_mode = transfer_mode,
    transfer_active_forecast_window = isTRUE(model$transfer_keep),
    active_set_by_lead = active_set_by_lead,
    state_dim_by_lead = state_dim_by_lead,
    covariance_diagnostics = cov_diag,
    fit_diagnostics = fit_diagnostics,
    stabilization = list(
      cov_calls = as.integer(filter_out$stabilization$calls + smoother_out$stabilization$calls + forecast_out$stabilization$calls),
      cov_projected = as.integer(filter_out$stabilization$cov_projected + smoother_out$stabilization$cov_projected + forecast_out$stabilization$cov_projected),
      cov_floor_clipped = as.integer(filter_out$stabilization$cov_floor_clipped + smoother_out$stabilization$cov_floor_clipped + forecast_out$stabilization$cov_floor_clipped),
      cov_cap_clipped = as.integer(filter_out$stabilization$cov_cap_clipped + smoother_out$stabilization$cov_cap_clipped + forecast_out$stabilization$cov_cap_clipped),
      cov_nonfinite_inputs = as.integer(filter_out$stabilization$cov_nonfinite_inputs + smoother_out$stabilization$cov_nonfinite_inputs + forecast_out$stabilization$cov_nonfinite_inputs)
    ),
    K_cap = as.integer(inputs$forecast$K_cap),
    nws_len = as.integer(K_vec[["nws"]]),
    glofas_len = as.integer(K_vec[["glofas"]]),
    T = Tn,
    n_T = n_T,
    S_T = S_T,
    p_total = model$p_total
  )
}
