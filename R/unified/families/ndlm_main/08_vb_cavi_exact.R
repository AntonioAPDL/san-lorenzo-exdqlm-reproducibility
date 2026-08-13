ndlm_exact_sigma_mean <- function(shape, rate) {
  nms <- union(names(shape), names(rate))
  shape <- as.numeric(shape)
  rate <- as.numeric(rate)
  out <- rate / pmax(shape - 1, 1.01)
  out[!is.finite(out) | out <= 0] <- NA_real_
  if (length(nms) == length(out)) names(out) <- nms
  out
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) < 1L) y else x
}

ndlm_exact_log_paths <- function() {
  summary_log <- Sys.getenv("NDLM_THEORY_SUMMARY_LOG", "")
  out_dir <- Sys.getenv("NDLM_OUT_DIR", "")
  if (!nzchar(summary_log) && nzchar(out_dir)) {
    summary_log <- file.path(out_dir, "ndlm_theory_summary.log")
  }
  progress_log <- Sys.getenv("NDLM_THEORY_PROGRESS_LOG", "")
  if (!nzchar(progress_log)) {
    if (nzchar(summary_log)) {
      progress_log <- file.path(dirname(summary_log), "ndlm_theory_progress.log")
    } else if (nzchar(out_dir)) {
      progress_log <- file.path(out_dir, "ndlm_theory_progress.log")
    }
  }
  list(
    summary_log = if (nzchar(summary_log)) normalizePath(summary_log, mustWork = FALSE) else "",
    progress_log = if (nzchar(progress_log)) normalizePath(progress_log, mustWork = FALSE) else ""
  )
}

ndlm_exact_log_write <- function(path, lines, append = TRUE) {
  if (!nzchar(path) || length(lines) < 1L) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = if (isTRUE(append)) "a" else "w")
  on.exit(close(con), add = TRUE)
  writeLines(as.character(lines), con = con, sep = "\n")
  invisible(TRUE)
}

ndlm_exact_progress_line <- function(tag, ..., timestamp = TRUE) {
  fields <- list(...)
  parts <- vapply(names(fields), function(nm) {
    val <- fields[[nm]]
    if (length(val) < 1L || is.null(val)) val <- NA
    sprintf("%s=%s", nm, as.character(val[[1L]]))
  }, character(1))
  prefix <- if (isTRUE(timestamp)) sprintf("[%s] ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")) else ""
  paste0(prefix, "[", tag, "] ", paste(parts, collapse = " "))
}

ndlm_exact_sigma_precision <- function(shape, rate) {
  nms <- union(names(shape), names(rate))
  out <- as.numeric(shape) / pmax(as.numeric(rate), 1e-10)
  out[!is.finite(out) | out <= 0] <- 1
  if (length(nms) == length(out)) names(out) <- nms
  out
}

ndlm_exact_iw_expected_precision <- function(nu, S, constants) {
  S_inv <- ndlm_theory_safe_inverse_spd(S, constants = constants)
  as.numeric(nu) * S_inv
}

ndlm_exact_iw_pseudo_cov <- function(nu, S, constants) {
  out <- as.matrix(S) / max(as.numeric(nu), 1e-8)
  ndlm_theory_stabilize_covariance_local(out, constants = constants)$cov
}

ndlm_exact_sequential_update <- function(m_in, C_in, y_vec, H_mat, R_vec, constants) {
  m_cur <- as.numeric(m_in)
  C_cur <- ndlm_theory_stabilize_covariance_local(C_in, constants = constants)$cov
  y_vec <- as.numeric(y_vec)
  H_mat <- as.matrix(H_mat)
  R_vec <- as.numeric(R_vec)
  if (length(y_vec) < 1L) {
    return(list(m = m_cur, C = C_cur, loglik = 0))
  }
  loglik <- 0
  for (ii in seq_along(y_vec)) {
    h <- matrix(as.numeric(H_mat[ii, ]), ncol = 1L)
    r <- max(as.numeric(R_vec[[ii]]), 1e-10)
    f <- as.numeric(crossprod(h, m_cur))
    qy <- max(as.numeric(crossprod(h, C_cur %*% h)) + r, 1e-10)
    A <- as.vector((C_cur %*% h) / qy)
    e <- as.numeric(y_vec[[ii]] - f)
    m_cur <- as.numeric(m_cur + A * e)
    C_cur <- ndlm_theory_stabilize_covariance_local(C_cur - tcrossprod(A, A) * qy, constants = constants)$cov
    loglik <- loglik - 0.5 * (log(2 * pi * qy) + e^2 / qy)
  }
  list(m = m_cur, C = C_cur, loglik = loglik)
}

ndlm_exact_hist_forward_pass <- function(inputs, registry, tau_by_source, constants) {
  Tn <- as.integer(inputs$T)
  d <- as.integer(registry$hist_dim)
  m_prev <- as.numeric(registry$m0)
  C_prev <- ndlm_theory_stabilize_covariance_local(registry$C0, constants = constants)$cov
  Q_hist <- vector("list", Tn)
  loglik <- 0

  build_hist_obs <- function(t) {
    vals <- c(
      usgs = as.numeric(inputs$retros$usgs[[t]]),
      glofas = as.numeric(inputs$retros$glofas[[t]]),
      nws = as.numeric(inputs$retros$nws[[t]])
    )
    rows <- rbind(registry$hist_obs$usgs, registry$hist_obs$glofas, registry$hist_obs$nws)
    srcs <- c("usgs", "glofas", "nws")
    ok <- is.finite(vals)
    list(
      y = as.numeric(vals[ok]),
      H = rows[ok, , drop = FALSE],
      sources = srcs[ok]
    )
  }

  hist_filter_mean <- matrix(NA_real_, nrow = d, ncol = Tn)
  hist_filter_cov <- array(NA_real_, dim = c(d, d, Tn))

  for (t in seq_len(Tn)) {
    if (t == 1L) {
      a_t <- m_prev
      R_t <- C_prev
      Q_hist[[t]] <- NULL
    } else {
      G_t <- registry$hist_G_list[[t]]
      P_t <- ndlm_theory_stabilize_covariance_local(G_t %*% C_prev %*% t(G_t), constants = constants)$cov
      Q_t <- ndlm_theory_stabilize_covariance_local(registry$hist_discount * P_t + diag(1e-8, d), constants = constants)$cov
      R_t <- ndlm_theory_stabilize_covariance_local(P_t + Q_t, constants = constants)$cov
      a_t <- as.numeric(G_t %*% m_prev)
      Q_hist[[t]] <- Q_t
    }
    obs_t <- build_hist_obs(t)
    R_vec <- vapply(obs_t$sources, function(nm) 1 / max(as.numeric(tau_by_source[[nm]]), 1e-10), numeric(1))
    upd <- ndlm_exact_sequential_update(a_t, R_t, obs_t$y, obs_t$H, R_vec, constants = constants)
    hist_filter_mean[, t] <- upd$m
    hist_filter_cov[, , t] <- upd$C
    loglik <- loglik + as.numeric(upd$loglik)
    m_prev <- upd$m
    C_prev <- upd$C
  }

  list(
    Q_hist = Q_hist,
    filter_mean = hist_filter_mean,
    filter_cov = hist_filter_cov,
    terminal_mean = m_prev,
    terminal_cov = C_prev,
    loglik = as.numeric(loglik)
  )
}

ndlm_exact_forecast_prior_anchor <- function(registry, hist_forward, inputs, constants) {
  K_max <- registry$K_max
  lead_specs <- registry$lead_specs
  nu0_list <- vector("list", K_max)
  S0_list <- vector("list", K_max)
  W_T_list <- vector("list", K_max)
  trace_W_T_k <- rep(NA_real_, K_max)
  c_factor <- as.numeric(constants$forecast_iw_c_factor)
  if (!is.finite(c_factor) || c_factor <= 0) c_factor <- 1
  epsilon0 <- suppressWarnings(as.numeric(constants$forecast_iw_epsilon0))
  if (!is.finite(epsilon0) || epsilon0 <= 0) {
    epsilon0 <- as.numeric(inputs$T)
  }
  dof_offset <- suppressWarnings(as.numeric(constants$forecast_iw_dof_offset))
  if (!is.finite(dof_offset) || dof_offset < 2) {
    dof_offset <- 4
  }
  scale_mult <- as.numeric(constants$forecast_iw_scale_mult)
  if (!is.finite(scale_mult) || scale_mult <= 0) {
    scale_mult <- 1
  }
  iw_jitter <- as.numeric(constants$forecast_iw_jitter)
  if (!is.finite(iw_jitter) || iw_jitter < 0) iw_jitter <- 1e-8
  W_T_hist <- hist_forward$Q_hist[[inputs$T]]
  if (!is.matrix(W_T_hist) || !all(dim(W_T_hist) == c(registry$hist_dim, registry$hist_dim))) {
    stop("NDLM exact forecast prior anchor requires terminal historical Q_T from the discount recursion.", call. = FALSE)
  }
  W_T_hist <- ndlm_theory_stabilize_covariance_local(
    as.matrix(W_T_hist) + diag(max(iw_jitter, 1e-8), registry$hist_dim),
    constants = constants
  )$cov
  trace_W_T_hist <- sum(diag(W_T_hist))

  for (k in seq_len(K_max)) {
    spec <- lead_specs[[k]]
    idx_k <- as.integer(spec$hist_to_fore_idx)
    d_k <- as.integer(spec$state_dim)
    W_T_k <- ndlm_theory_stabilize_covariance_local(
      as.matrix(W_T_hist[idx_k, idx_k, drop = FALSE]) + diag(max(iw_jitter, 1e-8), d_k),
      constants = constants
    )$cov
    nu0 <- as.numeric(d_k + dof_offset + epsilon0)
    S0 <- ndlm_theory_stabilize_covariance_local(
      epsilon0 * c_factor * scale_mult * W_T_k + diag(iw_jitter, d_k),
      constants = constants
    )$cov
    nu0_list[[k]] <- nu0
    S0_list[[k]] <- S0
    W_T_list[[k]] <- W_T_k
    trace_W_T_k[[k]] <- sum(diag(W_T_k))
  }

  list(
    nu0 = nu0_list,
    S0 = S0_list,
    W_T_hist = W_T_hist,
    trace_W_T_hist = trace_W_T_hist,
    W_T = W_T_list,
    trace_W_T_k = trace_W_T_k,
    c_factor = c_factor,
    epsilon0 = epsilon0,
    dof_offset = dof_offset,
    scale_mult = scale_mult,
    anchor_mode = "terminal_Q_hist"
  )
}

ndlm_exact_build_unified_sequence <- function(inputs, registry, tau_by_source, hist_Q, Q_fore) {
  Tn <- as.integer(inputs$T)
  K_max <- registry$K_max
  total_n <- Tn + K_max
  G_list <- vector("list", total_n)
  Q_list <- vector("list", total_n)
  y_list <- vector("list", total_n)
  H_list <- vector("list", total_n)
  R_list <- vector("list", total_n)
  sources_list <- vector("list", total_n)
  meta <- vector("list", total_n)

  build_hist_obs <- function(t) {
    vals <- c(
      usgs = as.numeric(inputs$retros$usgs[[t]]),
      glofas = as.numeric(inputs$retros$glofas[[t]]),
      nws = as.numeric(inputs$retros$nws[[t]])
    )
    rows <- rbind(registry$hist_obs$usgs, registry$hist_obs$glofas, registry$hist_obs$nws)
    srcs <- c("usgs", "glofas", "nws")
    ok <- is.finite(vals)
    list(
      y = as.numeric(vals[ok]),
      H = rows[ok, , drop = FALSE],
      R = vapply(srcs[ok], function(nm) 1 / max(as.numeric(tau_by_source[[nm]]), 1e-10), numeric(1)),
      sources = srcs[ok]
    )
  }

  build_forecast_obs <- function(k, spec) {
    vals <- numeric(0)
    rows <- list()
    srcs <- character(0)
    if ("glofas" %in% spec$active_sources) {
      if (is.matrix(inputs$forecast$glofas_members) && k <= nrow(inputs$forecast$glofas_members)) {
        cur <- as.numeric(inputs$forecast$glofas_members[k, , drop = TRUE])
        cur <- cur[is.finite(cur)]
      } else {
        cur <- if (k <= length(inputs$forecast$glofas)) as.numeric(inputs$forecast$glofas[[k]]) else numeric(0)
        cur <- cur[is.finite(cur)]
      }
      if (length(cur) > 0L) {
        vals <- c(vals, cur)
        rows[[length(rows) + 1L]] <- matrix(rep(spec$h_glofas, each = length(cur)), nrow = length(cur), byrow = TRUE)
        srcs <- c(srcs, rep("glofas", length(cur)))
      }
    }
    if ("nws" %in% spec$active_sources) {
      if (is.matrix(inputs$forecast$nws_members) && k <= nrow(inputs$forecast$nws_members)) {
        cur <- as.numeric(inputs$forecast$nws_members[k, , drop = TRUE])
        cur <- cur[is.finite(cur)]
      } else {
        cur <- if (k <= length(inputs$forecast$nws)) as.numeric(inputs$forecast$nws[[k]]) else numeric(0)
        cur <- cur[is.finite(cur)]
      }
      if (length(cur) > 0L) {
        vals <- c(vals, cur)
        rows[[length(rows) + 1L]] <- matrix(rep(spec$h_nws, each = length(cur)), nrow = length(cur), byrow = TRUE)
        srcs <- c(srcs, rep("nws", length(cur)))
      }
    }
    H <- if (length(rows) > 0L) do.call(rbind, rows) else matrix(0, nrow = 0L, ncol = spec$state_dim)
    list(
      y = as.numeric(vals),
      H = H,
      R = vapply(srcs, function(nm) 1 / max(as.numeric(tau_by_source[[nm]]), 1e-10), numeric(1)),
      sources = srcs
    )
  }

  for (t in seq_len(Tn)) {
    obs <- build_hist_obs(t)
    G_list[[t]] <- if (t == 1L) NULL else registry$hist_G_list[[t]]
    Q_list[[t]] <- if (t == 1L) NULL else hist_Q[[t]]
    y_list[[t]] <- obs$y
    H_list[[t]] <- obs$H
    R_list[[t]] <- obs$R
    sources_list[[t]] <- obs$sources
    meta[[t]] <- list(kind = "history", t = as.integer(t), lead = NA_integer_)
  }
  for (k in seq_len(K_max)) {
    idx <- Tn + k
    spec <- registry$lead_specs[[k]]
    obs <- build_forecast_obs(k, spec)
    G_list[[idx]] <- registry$forecast_G[[k]]
    Q_list[[idx]] <- Q_fore[[k]]
    y_list[[idx]] <- obs$y
    H_list[[idx]] <- obs$H
    R_list[[idx]] <- obs$R
    sources_list[[idx]] <- obs$sources
    meta[[idx]] <- list(kind = "forecast", t = NA_integer_, lead = as.integer(k))
  }

  list(
    m0 = as.numeric(registry$m0),
    C0 = as.matrix(registry$C0),
    G = G_list,
    Q = Q_list,
    y = y_list,
    H = H_list,
    R = R_list,
    sources = sources_list,
    meta = meta,
    T = Tn,
    K_max = K_max
  )
}

ndlm_exact_state_sequence_smoother <- function(sequence, backend = "cpp", constants) {
  ndlm_theory_tv_kalman_smoother(
    y_list = sequence$y,
    H_list = sequence$H,
    R_list = sequence$R,
    G_list = sequence$G,
    Q_list = sequence$Q,
    m0 = sequence$m0,
    C0 = sequence$C0,
    backend = backend,
    stabilization = constants$stabilization
  )
}

ndlm_exact_extract_block <- function(smoother, idx) {
  idx <- as.integer(idx)
  means <- lapply(idx, function(i) as.numeric(smoother$smooth_mean[[i]]))
  covs <- lapply(idx, function(i) as.matrix(smoother$smooth_cov[[i]]))
  list(mean = means, cov = covs)
}

ndlm_exact_state_outer <- function(m, C) {
  C + tcrossprod(m)
}

ndlm_exact_forecast_scatter <- function(sequence, smoother, registry, constants) {
  Tn <- sequence$T
  K_max <- registry$K_max
  out <- vector("list", K_max)
  for (k in seq_len(K_max)) {
    cur_idx <- Tn + k
    prev_idx <- cur_idx - 1L
    m_cur <- as.numeric(smoother$smooth_mean[[cur_idx]])
    C_cur <- as.matrix(smoother$smooth_cov[[cur_idx]])
    m_prev <- as.numeric(smoother$smooth_mean[[prev_idx]])
    C_prev <- as.matrix(smoother$smooth_cov[[prev_idx]])
    G_k <- as.matrix(sequence$G[[cur_idx]])
    cov_prev_cur <- as.matrix(smoother$lag_cov_next[[prev_idx]])
    Ex_prev_cur <- cov_prev_cur + tcrossprod(m_prev, m_cur)
    Exx_cur <- ndlm_exact_state_outer(m_cur, C_cur)
    Exx_prev <- ndlm_exact_state_outer(m_prev, C_prev)
    scatter <- Exx_cur - G_k %*% Ex_prev_cur - t(Ex_prev_cur) %*% t(G_k) + G_k %*% Exx_prev %*% t(G_k)
    out[[k]] <- ndlm_theory_stabilize_covariance_local(scatter, constants = constants)$cov
  }
  out
}

ndlm_exact_sigma_update <- function(sequence, smoother, source_names, constants, damping = 1) {
  shape <- stats::setNames(rep(constants$a_sigma, length(source_names)), source_names)
  rate <- stats::setNames(rep(constants$b_sigma, length(source_names)), source_names)
  sse <- stats::setNames(rep(0, length(source_names)), source_names)
  counts <- stats::setNames(integer(length(source_names)), source_names)

  for (tt in seq_along(sequence$y)) {
    y_t <- as.numeric(sequence$y[[tt]])
    H_t <- as.matrix(sequence$H[[tt]])
    src_t <- as.character(sequence$sources[[tt]])
    if (length(y_t) < 1L) next
    m_t <- as.numeric(smoother$smooth_mean[[tt]])
    C_t <- as.matrix(smoother$smooth_cov[[tt]])
    for (ii in seq_along(y_t)) {
      nm <- src_t[[ii]]
      h <- matrix(as.numeric(H_t[ii, ]), ncol = 1L)
      mu <- as.numeric(crossprod(h, m_t))
      vv <- max(as.numeric(crossprod(h, C_t %*% h)), 1e-10)
      sse[[nm]] <- sse[[nm]] + (y_t[[ii]] - mu)^2 + vv
      counts[[nm]] <- counts[[nm]] + 1L
    }
  }

  for (nm in source_names) {
    shape[[nm]] <- constants$a_sigma + counts[[nm]] / 2
    rate[[nm]] <- constants$b_sigma + 0.5 * sse[[nm]]
  }

  list(shape = shape, rate = rate, sse = sse, counts = counts)
}

ndlm_exact_sigma_damp <- function(shape, rate, prev_mean, damping, constants) {
  damping <- as.numeric(damping)
  if (!is.finite(damping) || damping <= 0 || damping >= 1) {
    return(list(shape = shape, rate = rate, sigma_mean = ndlm_exact_sigma_mean(shape, rate)))
  }
  cur_mean <- ndlm_exact_sigma_mean(shape, rate)
  tgt_mean <- damping * cur_mean + (1 - damping) * as.numeric(prev_mean[names(cur_mean)])
  tgt_mean <- pmax(tgt_mean, 1e-8)
  rate_new <- tgt_mean * pmax(as.numeric(shape) - 1, 1.01)
  list(shape = shape, rate = rate_new, sigma_mean = tgt_mean)
}

ndlm_exact_objective_proxy <- function(sequence, smoother, sigma_update, sigma_shape, sigma_rate, w_factors) {
  obs_term <- 0
  for (tt in seq_along(sequence$y)) {
    y_t <- as.numeric(sequence$y[[tt]])
    H_t <- as.matrix(sequence$H[[tt]])
    src_t <- as.character(sequence$sources[[tt]])
    if (length(y_t) < 1L) next
    m_t <- as.numeric(smoother$smooth_mean[[tt]])
    C_t <- as.matrix(smoother$smooth_cov[[tt]])
    for (ii in seq_along(y_t)) {
      nm <- src_t[[ii]]
      tau <- ndlm_exact_sigma_precision(sigma_shape[[nm]], sigma_rate[[nm]])
      elogsig <- log(sigma_rate[[nm]]) - digamma(sigma_shape[[nm]])
      h <- matrix(as.numeric(H_t[ii, ]), ncol = 1L)
      mu <- as.numeric(crossprod(h, m_t))
      vv <- max(as.numeric(crossprod(h, C_t %*% h)), 1e-10)
      obs_term <- obs_term - 0.5 * (log(2 * pi) + elogsig + tau * ((y_t[[ii]] - mu)^2 + vv))
    }
  }
  sigma_term <- 0
  for (nm in names(sigma_shape)) {
    a <- sigma_shape[[nm]]
    b <- sigma_rate[[nm]]
    a0 <- 2.0
    b0 <- 2.0
    elogsig <- log(b) - digamma(a)
    etau <- a / b
    prior <- a0 * log(b0) - lgamma(a0) - (a0 + 1) * elogsig - b0 * etau
    entropy <- a + log(b) + lgamma(a) - (1 + a) * digamma(a)
    sigma_term <- sigma_term + prior + entropy
  }
  w_term <- 0
  if (length(w_factors) > 0L) {
    for (fac in w_factors) {
      d <- nrow(fac$S)
      nu <- fac$nu
      S <- fac$S
      nu0 <- fac$nu0
      S0 <- fac$S0
      e_log_det <- determinant(S, logarithm = TRUE)$modulus[[1L]] - sum(digamma((nu + 1 - seq_len(d)) / 2)) - d * log(2)
      e_prec <- ndlm_exact_iw_expected_precision(nu, S, constants = list(stabilization = list(cov_eig_floor = 1e-8, cov_eig_cap = 1e8, cov_diag_jitter = 1e-10)))
      prior <- -0.5 * (nu0 + d + 1) * e_log_det - 0.5 * sum(diag(S0 %*% e_prec))
      entropy <- 0.5 * (nu + d + 1) * e_log_det + 0.5 * nu * d + 0.5 * d * (d + 1) * log(2) + multigammaln_custom(nu / 2, d)
      w_term <- w_term + prior + entropy
    }
  }
  as.numeric(obs_term + sigma_term + w_term)
}

multigammaln_custom <- function(a, p) {
  out <- p * (p - 1) / 4 * log(pi)
  out <- out + sum(lgamma(a + (1 - seq_len(p)) / 2))
  out
}

ndlm_exact_collect_state_summaries <- function(sequence, smoother, registry, sigma_mean, inputs, constants) {
  Tn <- sequence$T
  K_max <- registry$K_max
  hist_idx <- seq_len(Tn)
  fore_idx <- Tn + seq_len(K_max)

  hist_mean <- do.call(cbind, lapply(hist_idx, function(i) as.numeric(smoother$smooth_mean[[i]])))
  hist_cov <- array(0, dim = c(registry$hist_dim, registry$hist_dim, Tn))
  for (t in seq_len(Tn)) hist_cov[, , t] <- as.matrix(smoother$smooth_cov[[hist_idx[[t]]]])

  hist_mu_usgs <- vapply(hist_idx, function(i) as.numeric(crossprod(registry$hist_obs$usgs, smoother$smooth_mean[[i]])), numeric(1))
  hist_mu_glofas <- vapply(hist_idx, function(i) as.numeric(crossprod(registry$hist_obs$glofas, smoother$smooth_mean[[i]])), numeric(1))
  hist_mu_nws <- vapply(hist_idx, function(i) as.numeric(crossprod(registry$hist_obs$nws, smoother$smooth_mean[[i]])), numeric(1))
  hist_delta_g <- hist_mu_glofas - hist_mu_usgs
  hist_delta_n <- hist_mu_nws - hist_mu_usgs
  hist_y_var <- vapply(hist_idx, function(i) {
    C_t <- as.matrix(smoother$smooth_cov[[i]])
    h <- matrix(registry$hist_obs$usgs, ncol = 1L)
    max(as.numeric(crossprod(h, C_t %*% h)) + sigma_mean[["usgs"]], 1e-10)
  }, numeric(1))

  forecast_summary <- vector("list", K_max)
  for (k in seq_len(K_max)) {
    spec <- registry$lead_specs[[k]]
    i <- fore_idx[[k]]
    m_t <- as.numeric(smoother$smooth_mean[[i]])
    C_t <- as.matrix(smoother$smooth_cov[[i]])
    mu_usgs <- as.numeric(crossprod(spec$h_usgs, m_t))
    var_mu_usgs <- max(as.numeric(crossprod(spec$h_usgs, C_t %*% spec$h_usgs)), 1e-10)
    mu_glofas <- if ("glofas" %in% spec$active_sources) as.numeric(crossprod(spec$h_glofas, m_t)) else NA_real_
    mu_nws <- if ("nws" %in% spec$active_sources) as.numeric(crossprod(spec$h_nws, m_t)) else NA_real_
    delta_glofas <- if ("glofas" %in% spec$active_sources) as.numeric(crossprod(spec$h_delta_glofas, m_t)) else NA_real_
    delta_nws <- if ("nws" %in% spec$active_sources) as.numeric(crossprod(spec$h_delta_nws, m_t)) else NA_real_
    forecast_summary[[k]] <- list(
      mu_usgs_post = mu_usgs,
      var_mu_usgs_post = var_mu_usgs,
      var_y_usgs_post = var_mu_usgs + sigma_mean[["usgs"]],
      mu_glofas_post = mu_glofas,
      mu_nws_post = mu_nws,
      delta_glofas_post = delta_glofas,
      delta_nws_post = delta_nws,
      usgs_from_glofas_post = if (is.finite(mu_glofas) && is.finite(delta_glofas)) mu_glofas - delta_glofas else NA_real_,
      usgs_from_nws_post = if (is.finite(mu_nws) && is.finite(delta_nws)) mu_nws - delta_nws else NA_real_,
      identity_err_glofas = if (is.finite(mu_glofas) && is.finite(delta_glofas)) mu_glofas - mu_usgs - delta_glofas else NA_real_,
      identity_err_nws = if (is.finite(mu_nws) && is.finite(delta_nws)) mu_nws - mu_usgs - delta_nws else NA_real_,
      mean = m_t,
      cov = C_t
    )
  }

  list(
    hist_mean = hist_mean,
    hist_cov = hist_cov,
    hist_mu_usgs = hist_mu_usgs,
    hist_mu_glofas = hist_mu_glofas,
    hist_mu_nws = hist_mu_nws,
    hist_delta_g = hist_delta_g,
    hist_delta_n = hist_delta_n,
    hist_y_var = hist_y_var,
    forecast = forecast_summary
  )
}

ndlm_exact_segment_exports <- function(summary, registry) {
  K_overlap <- registry$K_overlap
  K_max <- registry$K_max
  K_tail <- max(K_max - K_overlap, 0L)
  overlap_spec <- registry$lead_specs[[1L]]
  tail_spec <- if (K_tail > 0L) registry$lead_specs[[K_overlap + 1L]] else NULL
  sm_ens_1 <- if (K_overlap > 0L) do.call(cbind, lapply(seq_len(K_overlap), function(k) summary$forecast[[k]]$mean)) else matrix(0, nrow = overlap_spec$state_dim, ncol = 0L)
  sC_ens_1 <- if (K_overlap > 0L) {
    arr <- array(0, dim = c(overlap_spec$state_dim, overlap_spec$state_dim, K_overlap))
    for (k in seq_len(K_overlap)) arr[, , k] <- summary$forecast[[k]]$cov
    arr
  } else {
    array(0, dim = c(overlap_spec$state_dim, overlap_spec$state_dim, 0L))
  }
  sm_ens_2 <- if (K_tail > 0L) do.call(cbind, lapply(seq.int(K_overlap + 1L, K_max), function(k) summary$forecast[[k]]$mean)) else matrix(0, nrow = if (is.null(tail_spec)) 0L else tail_spec$state_dim, ncol = 0L)
  sC_ens_2 <- if (K_tail > 0L) {
    arr <- array(0, dim = c(tail_spec$state_dim, tail_spec$state_dim, K_tail))
    for (kk in seq_len(K_tail)) arr[, , kk] <- summary$forecast[[K_overlap + kk]]$cov
    arr
  } else {
    array(0, dim = c(if (is.null(tail_spec)) 0L else tail_spec$state_dim, if (is.null(tail_spec)) 0L else tail_spec$state_dim, 0L))
  }
  list(sm_ens = list(sm_ens_1, sm_ens_2), sC_ens = list(sC_ens_1, sC_ens_2))
}

ndlm_exact_fit <- function(inputs, constants) {
  fmt_iter_num <- function(x, digits = 8L) {
    if (!is.finite(x)) return("NA")
    format(signif(as.numeric(x), digits = as.integer(digits)), trim = TRUE, scientific = FALSE)
  }

  log_paths <- ndlm_exact_log_paths()
  registry <- ndlm_exact_build_registry(inputs = inputs, constants = constants)
  source_names <- c("usgs", "nws", "glofas")
  sigma_init <- vapply(source_names, function(nm) {
    x <- if (identical(nm, "usgs")) as.numeric(inputs$retros$usgs) else if (identical(nm, "nws")) c(as.numeric(inputs$retros$nws), as.numeric(inputs$forecast$nws)) else c(as.numeric(inputs$retros$glofas), as.numeric(inputs$forecast$glofas))
    sdv <- suppressWarnings(stats::sd(x, na.rm = TRUE))
    if (!is.finite(sdv) || sdv < 0.05) sdv <- 0.05
    as.numeric(sdv^2)
  }, numeric(1))
  names(sigma_init) <- source_names

  sigma_shape <- stats::setNames(rep(constants$a_sigma + 1, length(source_names)), source_names)
  sigma_rate <- sigma_init * pmax(sigma_shape - 1, 1.01)

  hist_df_components <- ndlm_theory_df_components(constants, mode = "hist", k = 1L)
  fore_df_components <- ndlm_theory_df_components(constants, mode = "fore", k = 1L)
  w_hist <- mean((1 - hist_df_components) / hist_df_components)
  w_fore <- mean((1 - fore_df_components) / fore_df_components)

  hist_forward_init <- ndlm_exact_hist_forward_pass(
    inputs = inputs,
    registry = registry,
    tau_by_source = ndlm_exact_sigma_precision(sigma_shape, sigma_rate),
    constants = constants
  )
  prior_anchor <- ndlm_exact_forecast_prior_anchor(
    registry = registry,
    hist_forward = hist_forward_init,
    inputs = inputs,
    constants = constants
  )
  if (nzchar(log_paths$progress_log)) {
    ndlm_exact_log_write(log_paths$progress_log, character(), append = FALSE)
    ndlm_exact_log_write(
      log_paths$progress_log,
      c(
        ndlm_exact_progress_line(
          "ndlm_fit_start",
          implementation_mode = "theory_aligned",
          forecast_transfer_mode = if (isTRUE(registry$keep_mode)) "keep" else "drop",
          kalman_backend = constants$kalman_backend,
          T = inputs$T,
          K_max = registry$K_max,
          K_overlap = registry$K_overlap,
          hist_dim = registry$hist_dim,
          state_dim_lead1 = registry$lead_specs[[1L]]$state_dim,
          state_dim_lead_last = registry$lead_specs[[registry$K_max]]$state_dim,
          epsilon0 = prior_anchor$epsilon0,
          c_factor = prior_anchor$c_factor,
          anchor_mode = prior_anchor$anchor_mode,
          trace_W_T_hist = format(signif(prior_anchor$trace_W_T_hist, 8), scientific = TRUE),
          df_t = constants$df_t,
          df_s1 = constants$df_s1,
          df_s2 = constants$df_s2,
          df_s67 = constants$df_s67,
          df_discrep = constants$df_discrep,
          lambda = constants$lambda,
          df_trans = constants$df_trans,
          df_covs = constants$df_covs,
          summary_log = if (nzchar(log_paths$summary_log)) log_paths$summary_log else NA,
          progress_log = log_paths$progress_log
        ),
        ndlm_exact_progress_line(
          "ndlm_fit_start_members",
          glofas_len = inputs$forecast$glofas_len,
          nws_len = inputs$forecast$nws_len,
          K_cap = inputs$forecast$K_cap,
          overlap_glofas_members = registry$active_set_by_lead$glofas_member_count[[1L]],
          overlap_nws_members = registry$active_set_by_lead$nws_member_count[[1L]],
          tail_glofas_members = registry$active_set_by_lead$glofas_member_count[[registry$K_max]],
          tail_nws_members = registry$active_set_by_lead$nws_member_count[[registry$K_max]]
        )
      ),
      append = TRUE
    )
  }

  max_iter <- suppressWarnings(as.integer(constants$max_iter))
  if (!is.finite(max_iter) || max_iter < 1L) max_iter <- 100L
  min_total_iters <- suppressWarnings(as.integer(constants$min_total_iters))
  if (!is.finite(min_total_iters) || min_total_iters < 1L) min_total_iters <- min(50L, max_iter)
  min_total_iters <- min(min_total_iters, max_iter)
  elbo_tol <- suppressWarnings(as.numeric(constants$convergence$elbo_tol))
  elbo_rel_tol <- suppressWarnings(as.numeric(constants$convergence$elbo_rel_tol))
  if (!is.finite(elbo_tol) || elbo_tol <= 0) elbo_tol <- 1e-6
  if (!is.finite(elbo_rel_tol) || elbo_rel_tol <= 0) elbo_rel_tol <- 2.5e-4

  seq_sigma <- matrix(NA_real_, nrow = max_iter, ncol = length(source_names))
  colnames(seq_sigma) <- sprintf("sigma_%s_exp", source_names)
  seq_elbo <- rep(NA_real_, max_iter)
  scale_colnames <- c(
    "sigma_exp",
    "sigma_usgs_exp",
    "sigma_nws_exp",
    "sigma_glofas_exp",
    "w_hist",
    "w_fore",
    "df_t",
    "df_s1",
    "df_s2",
    "df_s67",
    "df_discrep",
    "lambda",
    "df_trans",
    "df_covs"
  )
  seq_scale <- matrix(NA_real_, nrow = max_iter, ncol = length(scale_colnames))
  colnames(seq_scale) <- scale_colnames

  w_factors <- lapply(seq_len(registry$K_max), function(k) {
    list(
      nu0 = as.numeric(prior_anchor$nu0[[k]]),
      S0 = as.matrix(prior_anchor$S0[[k]]),
      nu = as.numeric(prior_anchor$nu0[[k]]),
      S = as.matrix(prior_anchor$S0[[k]])
    )
  })

  prev_elbo <- NA_real_
  crit_elbo <- Inf
  crit_elbo_rel <- Inf
  max_param_rel_change <- Inf
  iterations_completed <- 0L
  converged <- FALSE
  convergence_reason <- "max_iter_reached"
  hist_forward <- hist_forward_init
  smoother <- NULL
  sequence <- NULL
  sigma_counts <- stats::setNames(integer(length(source_names)), source_names)
  sigma_sse <- stats::setNames(rep(NA_real_, length(source_names)), source_names)
  fit_started_at <- Sys.time()

  for (iter in seq_len(max_iter)) {
    tau_by_source <- ndlm_exact_sigma_precision(sigma_shape, sigma_rate)
    hist_forward <- ndlm_exact_hist_forward_pass(inputs = inputs, registry = registry, tau_by_source = tau_by_source, constants = constants)
    Q_fore <- lapply(w_factors, function(fac) ndlm_exact_iw_pseudo_cov(fac$nu, fac$S, constants = constants))
    sequence <- ndlm_exact_build_unified_sequence(
      inputs = inputs,
      registry = registry,
      tau_by_source = tau_by_source,
      hist_Q = hist_forward$Q_hist,
      Q_fore = Q_fore
    )
    smoother <- ndlm_exact_state_sequence_smoother(sequence = sequence, backend = constants$kalman_backend, constants = constants)

    sigma_prev_mean <- ndlm_exact_sigma_mean(sigma_shape, sigma_rate)
    sigma_upd <- ndlm_exact_sigma_update(sequence = sequence, smoother = smoother, source_names = source_names, constants = constants)
    sigma_counts <- sigma_upd$counts
    sigma_sse <- sigma_upd$sse
    damped <- ndlm_exact_sigma_damp(
      shape = sigma_upd$shape,
      rate = sigma_upd$rate,
      prev_mean = sigma_prev_mean,
      damping = constants$stabilization$sigma_update_damping,
      constants = constants
    )
    sigma_shape <- damped$shape
    sigma_rate <- damped$rate
    sigma_mean <- damped$sigma_mean

    scatter_list <- ndlm_exact_forecast_scatter(sequence = sequence, smoother = smoother, registry = registry, constants = constants)
    prev_Q <- lapply(w_factors, function(fac) ndlm_exact_iw_pseudo_cov(fac$nu, fac$S, constants = constants))
    for (k in seq_along(w_factors)) {
      w_factors[[k]]$nu <- as.numeric(w_factors[[k]]$nu0) + 1
      w_factors[[k]]$S <- ndlm_theory_stabilize_covariance_local(w_factors[[k]]$S0 + scatter_list[[k]], constants = constants)$cov
    }
    cur_Q <- lapply(w_factors, function(fac) ndlm_exact_iw_pseudo_cov(fac$nu, fac$S, constants = constants))

    max_sigma_rel <- max(abs(sigma_mean - sigma_prev_mean) / pmax(abs(sigma_prev_mean), 1e-8), na.rm = TRUE)
    max_w_rel <- 0
    for (k in seq_along(cur_Q)) {
      denom <- pmax(abs(prev_Q[[k]]), 1e-8)
      rel <- max(abs(cur_Q[[k]] - prev_Q[[k]]) / denom, na.rm = TRUE)
      if (is.finite(rel)) max_w_rel <- max(max_w_rel, rel)
    }
    max_param_rel_change <- max(max_sigma_rel, max_w_rel)

    seq_sigma[iter, ] <- as.numeric(sigma_mean[source_names])
    seq_scale[iter, ] <- c(
      sigma_mean[["usgs"]],
      sigma_mean[["usgs"]],
      sigma_mean[["nws"]],
      sigma_mean[["glofas"]],
      w_hist,
      mean(vapply(cur_Q, function(Q) mean(diag(Q)), numeric(1))),
      constants$df_t,
      constants$df_s1,
      constants$df_s2,
      constants$df_s67,
      constants$df_discrep,
      constants$lambda,
      constants$df_trans,
      constants$df_covs
    )
    seq_elbo[iter] <- ndlm_exact_objective_proxy(sequence, smoother, sigma_upd, sigma_shape, sigma_rate, w_factors)
    if (is.finite(prev_elbo) && is.finite(seq_elbo[iter])) {
      crit_elbo <- abs(seq_elbo[iter] - prev_elbo)
      crit_elbo_rel <- crit_elbo / max(abs(prev_elbo), 1e-12)
    } else {
      crit_elbo <- Inf
      crit_elbo_rel <- Inf
    }
    prev_elbo <- seq_elbo[iter]
    iterations_completed <- as.integer(iter)

    state_norm_sq <- sum(vapply(sequence$meta, function(mm) {
      idx <- which(vapply(sequence$meta, identical, logical(1), mm))
      0
    }, numeric(1)))
    state_norm_sq <- suppressWarnings(sum(unlist(lapply(smoother$smooth_mean, function(x) sum(as.numeric(x)^2))), na.rm = TRUE))
    q_traces <- vapply(cur_Q, function(Q) sum(diag(Q)), numeric(1))
    scatter_traces <- vapply(scatter_list, function(S) sum(diag(S)), numeric(1))
    forecast_covs <- smoother$smooth_cov[seq.int(inputs$T + 1L, inputs$T + registry$K_max)]
    state_cov_traces <- vapply(forecast_covs, function(Cm) sum(diag(as.matrix(Cm))), numeric(1))
    rep_leads <- unique(pmax(1L, pmin(registry$K_max, c(1L, registry$K_overlap, registry$K_overlap + 1L, registry$K_max))))
    rep_leads <- rep_leads[is.finite(rep_leads)]
    iter_line <- ndlm_exact_progress_line(
      "gamsig_progress",
      family = "ndlm_main",
      p0 = "NA",
      iter = as.integer(iter),
      elapsed_sec = sprintf("%.1f", as.numeric(difftime(Sys.time(), fit_started_at, units = "secs"))),
      elbo = fmt_iter_num(seq_elbo[iter]),
      crit_elbo = fmt_iter_num(crit_elbo),
      crit_elbo_rel = fmt_iter_num(crit_elbo_rel),
      sigma_exp = fmt_iter_num(sigma_mean[["usgs"]]),
      sigma_usgs_exp = fmt_iter_num(sigma_mean[["usgs"]]),
      sigma_nws_exp = fmt_iter_num(sigma_mean[["nws"]]),
      sigma_glofas_exp = fmt_iter_num(sigma_mean[["glofas"]]),
      gamma_exp = "NA",
      state_norm_sq = fmt_iter_num(state_norm_sq),
      w_hist = fmt_iter_num(w_hist),
      w_fore = fmt_iter_num(mean(vapply(cur_Q, function(Q) mean(diag(Q)), numeric(1)))),
      df_t = fmt_iter_num(constants$df_t),
      df_s1 = fmt_iter_num(constants$df_s1),
      df_s2 = fmt_iter_num(constants$df_s2),
      df_s67 = fmt_iter_num(constants$df_s67),
      df_discrep = fmt_iter_num(constants$df_discrep),
      lambda = fmt_iter_num(constants$lambda),
      max_param_rel_change = fmt_iter_num(max_param_rel_change),
      q_trace_mean = fmt_iter_num(mean(q_traces)),
      q_trace_max = fmt_iter_num(max(q_traces)),
      scatter_trace_mean = fmt_iter_num(mean(scatter_traces)),
      scatter_trace_max = fmt_iter_num(max(scatter_traces)),
      state_cov_trace_mean = fmt_iter_num(mean(state_cov_traces)),
      state_cov_trace_max = fmt_iter_num(max(state_cov_traces)),
      cov_cap_clipped = as.integer(smoother$stabilization$cov_cap_clipped %||% 0L),
      cov_floor_clipped = as.integer(smoother$stabilization$cov_floor_clipped %||% 0L),
      cov_projected = as.integer(smoother$stabilization$cov_projected %||% 0L)
    )
    rep_lines <- vapply(rep_leads, function(k) {
      ndlm_exact_progress_line(
        "gamsig_progress_lead",
        iter = as.integer(iter),
        lead = as.integer(k),
        state_dim = registry$lead_specs[[k]]$state_dim,
        q_trace = fmt_iter_num(q_traces[[k]]),
        scatter_trace = fmt_iter_num(scatter_traces[[k]]),
        state_cov_trace = fmt_iter_num(state_cov_traces[[k]]),
        nws_members = registry$active_set_by_lead$nws_member_count[[k]],
        glofas_members = registry$active_set_by_lead$glofas_member_count[[k]]
      )
    }, character(1))
    cat(iter_line, "\n", sep = "")
    if (length(rep_lines) > 0L) {
      cat(paste0(rep_lines, collapse = "\n"), "\n", sep = "")
    }
    if (nzchar(log_paths$progress_log)) {
      ndlm_exact_log_write(log_paths$progress_log, c(iter_line, rep_lines), append = TRUE)
    }

    if (iter >= min_total_iters && is.finite(max_param_rel_change) && max_param_rel_change <= elbo_rel_tol && is.finite(crit_elbo) && crit_elbo <= elbo_tol) {
      converged <- TRUE
      convergence_reason <- "parameter_change_and_objective_stabilized"
      break
    }
  }

  seq_sigma <- seq_sigma[seq_len(iterations_completed), , drop = FALSE]
  seq_scale <- seq_scale[seq_len(iterations_completed), , drop = FALSE]
  seq_elbo <- seq_elbo[seq_len(iterations_completed)]
  sigma_mean <- ndlm_exact_sigma_mean(sigma_shape, sigma_rate)
  summary <- ndlm_exact_collect_state_summaries(sequence = sequence, smoother = smoother, registry = registry, sigma_mean = sigma_mean, inputs = inputs, constants = constants)
  seg <- ndlm_exact_segment_exports(summary = summary, registry = registry)
  latent_var_cap_last <- max(vapply(summary$forecast, function(x) as.numeric(x$var_mu_usgs_post), numeric(1)), na.rm = TRUE)
  if (!is.finite(latent_var_cap_last)) {
    latent_var_cap_last <- NA_real_
  }

  n_draws <- suppressWarnings(as.integer(constants$n_draws))
  if (!is.finite(n_draws) || n_draws < 4L) n_draws <- 48L
  samp_theta_retro <- ndlm_theory_state_draws(summary$hist_mean, summary$hist_cov, n_draws = n_draws, seed = constants$seed + 11L)
  draw_segment <- function(mean_mat, cov_arr, seed) {
    k_len <- ncol(mean_mat)
    d_seg <- nrow(mean_mat)
    out <- array(0, dim = c(d_seg, k_len, n_draws))
    if (k_len < 1L || d_seg < 1L) return(out)
    set.seed(seed)
    for (kk in seq_len(k_len)) {
      L <- ndlm_theory_safe_chol(cov_arr[, , kk, drop = TRUE])
      Z <- matrix(stats::rnorm(d_seg * n_draws), nrow = d_seg, ncol = n_draws)
      out[, kk, ] <- mean_mat[, kk] + L %*% Z
    }
    out
  }
  samp_theta_ens_1 <- draw_segment(seg$sm_ens[[1L]], seg$sC_ens[[1L]], constants$seed + 21L)
  samp_theta_ens_2 <- draw_segment(seg$sm_ens[[2L]], seg$sC_ens[[2L]], constants$seed + 22L)

  mean_draws_loglog1p <- matrix(NA_real_, nrow = n_draws, ncol = registry$K_max)
  set.seed(constants$seed + 23L)
  for (k in seq_len(registry$K_max)) {
    mean_draws_loglog1p[, k] <- stats::rnorm(
      n_draws,
      mean = as.numeric(summary$forecast[[k]]$mu_usgs_post),
      sd = sqrt(max(as.numeric(summary$forecast[[k]]$var_mu_usgs_post), 1e-10))
    )
  }

  forecast_cov_diagnostics <- do.call(rbind, lapply(seq_len(registry$K_max), function(k) {
    fs <- summary$forecast[[k]]
    w_t_k <- as.matrix(prior_anchor$W_T[[k]])
    q_pseudo <- as.matrix(cur_Q[[k]])
    scatter_k <- as.matrix(scatter_list[[k]])
    data.frame(
      lead = as.integer(k),
      state_dim = as.integer(registry$lead_specs[[k]]$state_dim),
      trace_W_T_hist = as.numeric(prior_anchor$trace_W_T_hist),
      trace_W_T_k = sum(diag(w_t_k)),
      max_diag_W_T_k = max(diag(w_t_k)),
      trace_Q_anchor = sum(diag(w_t_k)),
      max_diag_Q_anchor = max(diag(w_t_k)),
      nu0 = as.numeric(w_factors[[k]]$nu0),
      epsilon0 = as.numeric(prior_anchor$epsilon0),
      c_factor = as.numeric(prior_anchor$c_factor),
      dof_offset = as.numeric(prior_anchor$dof_offset),
      scale_mult = as.numeric(prior_anchor$scale_mult),
      trace_S0 = sum(diag(w_factors[[k]]$S0)),
      trace_S0_over_epsilon0 = sum(diag(w_factors[[k]]$S0 / max(as.numeric(prior_anchor$epsilon0), 1e-8))),
      trace_scatter = sum(diag(scatter_k)),
      max_diag_scatter = max(diag(scatter_k)),
      trace_Q_pseudo = sum(diag(q_pseudo)),
      max_diag_Q_pseudo = max(diag(q_pseudo)),
      trace_state_cov = sum(diag(fs$cov)),
      max_diag_state_cov = max(diag(fs$cov)),
      mu_usgs_post = as.numeric(fs$mu_usgs_post),
      var_mu_usgs_post = as.numeric(fs$var_mu_usgs_post),
      max_abs_mean_draw_loglog1p = max(abs(mean_draws_loglog1p[, k])),
      stringsAsFactors = FALSE
    )
  }))

  samp_sigma <- matrix(NA_real_, nrow = length(source_names), ncol = n_draws)
  rownames(samp_sigma) <- source_names
  set.seed(constants$seed + 33L)
  for (j in seq_along(source_names)) {
    nm <- source_names[[j]]
    samp_sigma[j, ] <- 1 / stats::rgamma(n_draws, shape = sigma_shape[[nm]], rate = sigma_rate[[nm]])
  }

  standard_forecast_errors <- rep(NA_real_, registry$K_max)
  if (registry$K_overlap > 0L) {
    standard_forecast_errors[seq_len(registry$K_overlap)] <- inputs$forecast$nws[seq_len(registry$K_overlap)] - inputs$forecast$glofas[seq_len(registry$K_overlap)]
  }
  if (registry$K_max > registry$K_overlap) {
    if (identical(registry$extension_source, "nws")) {
      tail_idx <- seq.int(registry$K_overlap + 1L, inputs$forecast$K_vec[["nws"]])
      bridge_raw <- as.numeric(inputs$forecast$glofas[[registry$K_overlap]])
      if (!is.finite(bridge_raw)) bridge_raw <- 0
      standard_forecast_errors[seq.int(registry$K_overlap + 1L, registry$K_max)] <- as.numeric(inputs$forecast$nws[tail_idx]) - bridge_raw
    } else {
      tail_idx <- seq.int(registry$K_overlap + 1L, inputs$forecast$K_vec[["glofas"]])
      bridge_raw <- as.numeric(inputs$forecast$nws[[registry$K_overlap]])
      if (!is.finite(bridge_raw)) bridge_raw <- 0
      standard_forecast_errors[seq.int(registry$K_overlap + 1L, registry$K_max)] <- bridge_raw - as.numeric(inputs$forecast$glofas[tail_idx])
    }
  }
  standard_forecast_errors[!is.finite(standard_forecast_errors)] <- 0
  standard_forecast_errors <- matrix(standard_forecast_errors, nrow = 1L)

  hist_identity <- data.frame(
    t = seq_len(inputs$T),
    usgs_obs = as.numeric(inputs$retros$usgs),
    mu_usgs = summary$hist_mu_usgs,
    mu_glofas = summary$hist_mu_glofas,
    mu_nws = summary$hist_mu_nws,
    delta_glofas = summary$hist_delta_g,
    delta_nws = summary$hist_delta_n,
    identity_err_glofas = summary$hist_mu_glofas - summary$hist_mu_usgs - summary$hist_delta_g,
    identity_err_nws = summary$hist_mu_nws - summary$hist_mu_usgs - summary$hist_delta_n,
    stringsAsFactors = FALSE
  )

  forecast_identity <- do.call(rbind, lapply(seq_len(registry$K_max), function(k) {
    fs <- summary$forecast[[k]]
    data.frame(
      lead = k,
      mu_usgs_post = fs$mu_usgs_post,
      var_mu_usgs_post = fs$var_mu_usgs_post,
      var_y_usgs_post = fs$var_y_usgs_post,
      mu_glofas_post = fs$mu_glofas_post,
      mu_nws_post = fs$mu_nws_post,
      delta_glofas_post = fs$delta_glofas_post,
      delta_nws_post = fs$delta_nws_post,
      usgs_from_glofas_post = fs$usgs_from_glofas_post,
      usgs_from_nws_post = fs$usgs_from_nws_post,
      identity_err_glofas = fs$identity_err_glofas,
      identity_err_nws = fs$identity_err_nws,
      glofas_member_count = registry$active_set_by_lead$glofas_member_count[[k]],
      nws_member_count = registry$active_set_by_lead$nws_member_count[[k]],
      stringsAsFactors = FALSE
    )
  }))

  exps <- rbind(summary$hist_mu_usgs, summary$hist_mu_usgs)
  vars <- rbind(summary$hist_y_var, summary$hist_y_var)
  exps2 <- exps^2 + vars
  cov_diag <- ndlm_theory_collect_covariance_diagnostics(summary$hist_cov, seg$sC_ens[[1L]], seg$sC_ens[[2L]])

  new_theta <- list(
    sm = summary$hist_mean,
    sC = summary$hist_cov,
    exps = exps,
    exps2 = exps2,
    vars = vars,
    sm_ens = seg$sm_ens,
    sC_ens = seg$sC_ens,
    standard_forecast_errors = standard_forecast_errors,
    forecast_mean_draws_loglog1p = mean_draws_loglog1p,
    forecast_horizon = list(
      K_vec = inputs$forecast$K_vec,
      K_overlap = registry$K_overlap,
      K_max = registry$K_max,
      segment_lengths = c(overlap = registry$K_overlap, extension = registry$K_max - registry$K_overlap),
      extension_source = registry$extension_source,
      bridge_source = registry$bridge_source,
      forecast_transfer_mode = if (isTRUE(registry$keep_mode)) "keep" else "drop",
      transfer_active_forecast_window = isTRUE(registry$keep_mode)
    )
  )

  if (nzchar(log_paths$progress_log)) {
    ndlm_exact_log_write(
      log_paths$progress_log,
      ndlm_exact_progress_line(
        "ndlm_fit_end",
        converged = if (isTRUE(converged)) "true" else "false",
        iterations_completed = as.integer(iterations_completed),
        convergence_reason = convergence_reason,
        elapsed_sec = sprintf("%.1f", as.numeric(difftime(Sys.time(), fit_started_at, units = "secs"))),
        w_fore = fmt_iter_num(mean(vapply(w_factors, function(fac) mean(diag(ndlm_exact_iw_pseudo_cov(fac$nu, fac$S, constants = constants))), numeric(1)))),
        max_var_mu_usgs_post = fmt_iter_num(max(vapply(summary$forecast, function(x) as.numeric(x$var_mu_usgs_post), numeric(1)), na.rm = TRUE)),
        max_abs_mean_draw_loglog1p = fmt_iter_num(max(abs(mean_draws_loglog1p), na.rm = TRUE)),
        cov_cap_clipped = as.integer(smoother$stabilization$cov_cap_clipped %||% 0L),
        cov_floor_clipped = as.integer(smoother$stabilization$cov_floor_clipped %||% 0L),
        cov_projected = as.integer(smoother$stabilization$cov_projected %||% 0L),
        latent_var_cap_last = fmt_iter_num(latent_var_cap_last)
      ),
      append = TRUE
    )
  }

  list(
    new_theta = new_theta,
    samp_theta = list(samp_theta = samp_theta_retro),
    samp_theta_ens = list(list(samp_theta = samp_theta_ens_1), list(samp_theta = samp_theta_ens_2)),
    samp_sigma = samp_sigma,
    seq_sigma = seq_sigma,
    seq_scale = seq_scale,
    seq_elbo = seq_elbo,
    delta = c(diff(seq_elbo), 0),
    iterations_completed = as.integer(iterations_completed),
    max_iter = as.integer(max_iter),
    converged = converged,
    convergence_reason = convergence_reason,
    convergence_metrics = c(
      crit_elbo = suppressWarnings(as.numeric(crit_elbo)),
      crit_elbo_rel = suppressWarnings(as.numeric(crit_elbo_rel)),
      elbo_tol = elbo_tol,
      elbo_rel_tol = elbo_rel_tol,
      max_param_rel_change = suppressWarnings(as.numeric(max_param_rel_change))
    ),
    sigma = as.numeric(sigma_mean[["usgs"]]),
    sigma_by_source = sigma_mean[source_names],
    sigma_mean = mean(as.numeric(sigma_mean[source_names])),
    sigma_shape = sigma_shape[source_names],
    sigma_rate = sigma_rate[source_names],
    sigma_counts = sigma_counts[source_names],
    sigma_sse = sigma_sse[source_names],
    w_hist = w_hist,
    w_fore = mean(vapply(w_factors, function(fac) mean(diag(ndlm_exact_iw_pseudo_cov(fac$nu, fac$S, constants = constants))), numeric(1))),
    discount_factors = c(
      df_t = constants$df_t,
      df_s1 = constants$df_s1,
      df_s2 = constants$df_s2,
      df_s67 = constants$df_s67,
      df_discrep = constants$df_discrep,
      lambda = constants$lambda,
      df_trans = constants$df_trans,
      df_covs = constants$df_covs
    ),
    K = registry$K_max,
    K_overlap = registry$K_overlap,
    K_max = registry$K_max,
    K_vec = inputs$forecast$K_vec,
    segment_lengths = c(overlap = registry$K_overlap, extension = registry$K_max - registry$K_overlap),
    extension_source = registry$extension_source,
    bridge_source = registry$bridge_source,
    forecast_transfer_mode = if (isTRUE(registry$keep_mode)) "keep" else "drop",
    transfer_active_forecast_window = isTRUE(registry$keep_mode),
    active_set_by_lead = registry$active_set_by_lead,
    forecast_member_counts = registry$active_set_by_lead[, c("lead", "nws_member_count", "glofas_member_count", "active_member_count"), drop = FALSE],
    state_dim_by_lead = registry$state_dim_by_lead,
    covariance_diagnostics = cov_diag,
    fit_diagnostics = list(
      y_observed = as.numeric(inputs$retros$usgs),
      y_predicted_one_step = as.numeric(summary$hist_mu_usgs),
      y_filtered = as.numeric(summary$hist_mu_usgs),
      y_smoothed = as.numeric(summary$hist_mu_usgs),
      var_predicted_one_step = as.numeric(summary$hist_y_var),
      var_filtered = as.numeric(summary$hist_y_var),
      var_smoothed = as.numeric(summary$hist_y_var),
      residual_source_usgs = as.numeric(inputs$retros$usgs - summary$hist_mu_usgs),
      residual_source_nws = as.numeric(inputs$retros$nws - summary$hist_mu_nws),
      residual_source_glofas = as.numeric(inputs$retros$glofas - summary$hist_mu_glofas),
      hist_identity = hist_identity,
      forecast_identity = forecast_identity
    ),
    stabilization = list(
      cov_calls = as.integer(smoother$stabilization$calls %||% 0L),
      cov_projected = as.integer(smoother$stabilization$cov_projected %||% 0L),
      cov_floor_clipped = as.integer(smoother$stabilization$cov_floor_clipped %||% 0L),
      cov_cap_clipped = as.integer(smoother$stabilization$cov_cap_clipped %||% 0L),
      cov_nonfinite_inputs = as.integer(smoother$stabilization$cov_nonfinite_inputs %||% 0L),
      sigma_upper_cap = as.numeric(constants$stabilization$sigma_upper_cap),
      sigma_update_damping = as.numeric(constants$stabilization$sigma_update_damping),
      sigma_capped_total = 0L,
      sigma_damped_total = if (isTRUE(constants$stabilization$sigma_update_damping < 1)) as.integer(iterations_completed) else 0L,
      latent_var_cap_last = latent_var_cap_last,
      latent_var_clipped_total = 0L,
      forecast_iw_c_factor = as.numeric(prior_anchor$c_factor),
      forecast_iw_epsilon0 = as.numeric(prior_anchor$epsilon0),
      forecast_iw_dof_offset = as.numeric(prior_anchor$dof_offset),
      forecast_iw_scale_mult = as.numeric(prior_anchor$scale_mult),
      forecast_iw_anchor_mode = as.character(prior_anchor$anchor_mode)
    ),
    forecast_prior = list(
      c_factor = as.numeric(prior_anchor$c_factor),
      epsilon0 = as.numeric(prior_anchor$epsilon0),
      dof_offset = as.numeric(prior_anchor$dof_offset),
      scale_mult = as.numeric(prior_anchor$scale_mult),
      anchor_mode = as.character(prior_anchor$anchor_mode),
      trace_W_T_hist = as.numeric(prior_anchor$trace_W_T_hist)
    ),
    K_cap = inputs$forecast$K_cap,
    nws_len = inputs$forecast$nws_len,
    glofas_len = inputs$forecast$glofas_len,
    T = inputs$T,
    progress_log_path = if (nzchar(log_paths$progress_log)) log_paths$progress_log else NA_character_,
    forecast_cov_factors = lapply(seq_along(w_factors), function(k) {
      fac <- w_factors[[k]]
      data.frame(
        lead = k,
        state_dim = nrow(fac$S),
        nu0 = fac$nu0,
        nu = fac$nu,
        epsilon0 = as.numeric(prior_anchor$epsilon0),
        c_factor = as.numeric(prior_anchor$c_factor),
        dof_offset = as.numeric(prior_anchor$dof_offset),
        scale_mult = as.numeric(prior_anchor$scale_mult),
        trace_W_T_k = as.numeric(sum(diag(prior_anchor$W_T[[k]]))),
        trace_S0 = sum(diag(fac$S0)),
        trace_S = sum(diag(fac$S)),
        trace_pseudo_cov = sum(diag(ndlm_exact_iw_pseudo_cov(fac$nu, fac$S, constants = constants))),
        stringsAsFactors = FALSE
      )
    }),
    forecast_cov_diagnostics = forecast_cov_diagnostics,
    state_registry = registry
  )
}
