ndlm_exact_hist_discount_matrix <- function(constants, q, transfer_dim) {
  theta_df <- ndlm_theory_make_df_mat(
    c(constants$df_t, constants$df_s1, constants$df_s2, constants$df_s67),
    dim_df = c(1L, 2L, 2L, 2L),
    n = q,
    power = 1L
  )
  transfer_df <- ndlm_theory_make_df_mat(
    c(constants$df_trans, constants$df_covs),
    dim_df = c(1L, transfer_dim - 1L),
    n = transfer_dim,
    power = 1L
  )
  discrep_df <- ndlm_theory_make_df_mat(
    pmin(
      pmax(constants$df_discrep * c(constants$df_t, constants$df_s1, constants$df_s2, constants$df_s67), 1e-8),
      1 - 1e-8
    ),
    dim_df = c(1L, 2L, 2L, 2L),
    n = q,
    power = 1L
  )
  ndlm_theory_block_diag(theta_df, transfer_df, discrep_df, discrep_df)
}

ndlm_exact_build_registry <- function(inputs, constants) {
  base <- ndlm_theory_build_base_state_model(
    period = constants$period,
    harmonics = constants$harmonics
  )
  F_base <- as.numeric(base$F_base)
  G_base <- as.matrix(base$G_base)
  q <- length(F_base)
  p_cov <- ncol(as.matrix(inputs$X))
  transfer_dim <- 1L + p_cov
  keep_mode <- identical(as.character(constants$forecast_transfer_mode), "keep")
  hist_dim <- q + transfer_dim + 2L * q
  idx_theta <- seq_len(q)
  idx_transfer <- seq.int(max(idx_theta) + 1L, max(idx_theta) + transfer_dim)
  idx_zeta <- idx_transfer[[1L]]
  idx_psi <- idx_transfer[-1L]
  idx_delta_glofas <- seq.int(max(idx_transfer) + 1L, max(idx_transfer) + q)
  idx_delta_nws <- seq.int(max(idx_delta_glofas) + 1L, max(idx_delta_glofas) + q)

  h_hist_usgs <- rep(0, hist_dim)
  h_hist_usgs[idx_theta] <- F_base
  h_hist_usgs[idx_zeta] <- 1
  h_hist_glofas <- h_hist_usgs
  h_hist_glofas[idx_delta_glofas] <- F_base
  h_hist_nws <- h_hist_usgs
  h_hist_nws[idx_delta_nws] <- F_base

  K_vec <- inputs$forecast$K_vec
  K_nws <- suppressWarnings(as.integer(K_vec[["nws"]]))
  K_glofas <- suppressWarnings(as.integer(K_vec[["glofas"]]))
  K_max <- max(K_nws, K_glofas)
  lead_specs <- vector("list", K_max)

  make_lead_spec <- function(k) {
    active_sources <- character(0)
    if (k <= K_glofas) active_sources <- c(active_sources, "glofas")
    if (k <= K_nws) active_sources <- c(active_sources, "nws")

    pos <- 0L
    idx_theta_f <- seq.int(pos + 1L, pos + q)
    pos <- max(idx_theta_f)
    idx_transfer_f <- integer(0)
    idx_zeta_f <- integer(0)
    idx_psi_f <- integer(0)
    if (isTRUE(keep_mode)) {
      idx_transfer_f <- seq.int(pos + 1L, pos + transfer_dim)
      idx_zeta_f <- idx_transfer_f[[1L]]
      idx_psi_f <- idx_transfer_f[-1L]
      pos <- max(idx_transfer_f)
    }
    idx_delta_f <- list()
    for (src in active_sources) {
      idx_delta_f[[src]] <- seq.int(pos + 1L, pos + q)
      pos <- max(idx_delta_f[[src]])
    }
    d_k <- max(pos, 0L)
    hist_to_fore_idx <- idx_theta
    if (isTRUE(keep_mode)) {
      hist_to_fore_idx <- c(hist_to_fore_idx, idx_transfer)
    }
    for (src in active_sources) {
      if (identical(src, "glofas")) {
        hist_to_fore_idx <- c(hist_to_fore_idx, idx_delta_glofas)
      } else if (identical(src, "nws")) {
        hist_to_fore_idx <- c(hist_to_fore_idx, idx_delta_nws)
      }
    }
    hist_to_fore_idx <- as.integer(hist_to_fore_idx)
    h_usgs <- rep(0, d_k)
    h_usgs[idx_theta_f] <- F_base
    if (isTRUE(keep_mode)) h_usgs[idx_zeta_f] <- 1
    h_glofas <- rep(NA_real_, d_k)
    h_nws <- rep(NA_real_, d_k)
    h_delta_glofas <- rep(NA_real_, d_k)
    h_delta_nws <- rep(NA_real_, d_k)
    if ("glofas" %in% active_sources) {
      h_glofas[] <- h_usgs
      h_glofas[idx_delta_f[["glofas"]]] <- h_glofas[idx_delta_f[["glofas"]]] + F_base
      h_delta_glofas[] <- 0
      h_delta_glofas[idx_delta_f[["glofas"]]] <- F_base
    }
    if ("nws" %in% active_sources) {
      h_nws[] <- h_usgs
      h_nws[idx_delta_f[["nws"]]] <- h_nws[idx_delta_f[["nws"]]] + F_base
      h_delta_nws[] <- 0
      h_delta_nws[idx_delta_f[["nws"]]] <- F_base
    }
    list(
      lead = as.integer(k),
      active_sources = active_sources,
      state_dim = as.integer(d_k),
      idx_theta = idx_theta_f,
      idx_transfer = idx_transfer_f,
      idx_zeta = idx_zeta_f,
      idx_psi = idx_psi_f,
      idx_delta = idx_delta_f,
      hist_to_fore_idx = hist_to_fore_idx,
      h_usgs = h_usgs,
      h_glofas = h_glofas,
      h_nws = h_nws,
      h_delta_glofas = h_delta_glofas,
      h_delta_nws = h_delta_nws,
      keep_mode = keep_mode
    )
  }

  for (k in seq_len(K_max)) {
    lead_specs[[k]] <- make_lead_spec(k)
  }

  hist_discount <- ndlm_exact_hist_discount_matrix(constants = constants, q = q, transfer_dim = transfer_dim)

  build_hist_G <- function(cov_row) {
    ndlm_theory_block_diag(
      ndlm_theory_block_diag(G_base, ndlm_theory_build_transfer_G(cov_row = cov_row, lambda = constants$lambda)),
      G_base,
      G_base
    )
  }

  build_bridge_G <- function(spec, cov_row) {
    G <- matrix(0, nrow = spec$state_dim, ncol = hist_dim)
    G[spec$idx_theta, idx_theta] <- G_base
    if (isTRUE(keep_mode)) {
      G[spec$idx_transfer, idx_transfer] <- ndlm_theory_build_transfer_G(cov_row = cov_row, lambda = constants$lambda)
    }
    if ("glofas" %in% spec$active_sources) {
      G[spec$idx_delta[["glofas"]], idx_delta_glofas] <- G_base
    }
    if ("nws" %in% spec$active_sources) {
      G[spec$idx_delta[["nws"]], idx_delta_nws] <- G_base
    }
    G
  }

  build_forecast_G <- function(prev_spec, cur_spec, cov_row) {
    G <- matrix(0, nrow = cur_spec$state_dim, ncol = prev_spec$state_dim)
    G[cur_spec$idx_theta, prev_spec$idx_theta] <- G_base
    if (isTRUE(keep_mode)) {
      G[cur_spec$idx_transfer, prev_spec$idx_transfer] <- ndlm_theory_build_transfer_G(cov_row = cov_row, lambda = constants$lambda)
    }
    for (src in intersect(cur_spec$active_sources, prev_spec$active_sources)) {
      G[cur_spec$idx_delta[[src]], prev_spec$idx_delta[[src]]] <- G_base
    }
    G
  }

  hist_G_list <- lapply(seq_len(inputs$T), function(tt) build_hist_G(as.numeric(inputs$X[tt, ])))
  bridge_G <- build_bridge_G(lead_specs[[1L]], cov_row = as.numeric(inputs$X_future[1L, ]))
  forecast_G <- vector("list", K_max)
  forecast_G[[1L]] <- bridge_G
  if (K_max >= 2L) {
    for (k in 2:K_max) {
      forecast_G[[k]] <- build_forecast_G(
        prev_spec = lead_specs[[k - 1L]],
        cur_spec = lead_specs[[k]],
        cov_row = as.numeric(inputs$X_future[k, ])
      )
    }
  }

  active_set_by_lead <- data.frame(
    lead = seq_len(K_max),
    active_nws = as.integer(seq_len(K_max) <= K_nws),
    active_glofas = as.integer(seq_len(K_max) <= K_glofas),
    active_count = as.integer((seq_len(K_max) <= K_nws) + (seq_len(K_max) <= K_glofas)),
    nws_member_count = if (is.matrix(inputs$forecast$nws_members)) rowSums(is.finite(inputs$forecast$nws_members[seq_len(K_max), , drop = FALSE])) else as.integer(seq_len(K_max) <= K_nws),
    glofas_member_count = if (is.matrix(inputs$forecast$glofas_members)) rowSums(is.finite(inputs$forecast$glofas_members[seq_len(K_max), , drop = FALSE])) else as.integer(seq_len(K_max) <= K_glofas),
    stringsAsFactors = FALSE
  )
  active_set_by_lead$active_member_count <- as.integer(active_set_by_lead$nws_member_count + active_set_by_lead$glofas_member_count)
  state_dim_by_lead <- data.frame(
    lead = seq_len(K_max),
    state_dim = vapply(lead_specs, function(x) as.integer(x$state_dim), integer(1)),
    stringsAsFactors = FALSE
  )

  y_mu <- suppressWarnings(mean(as.numeric(inputs$y), na.rm = TRUE))
  if (!is.finite(y_mu)) y_mu <- 0
  m0 <- rep(0, hist_dim)
  m0[1L] <- y_mu
  C0 <- diag(c(5, rep(1.5, q - 1L), 2, rep(1, p_cov), rep(1.5, 2L * q)), hist_dim)

  list(
    q = q,
    F_base = F_base,
    G_base = G_base,
    p_cov = p_cov,
    transfer_dim = transfer_dim,
    hist_dim = hist_dim,
    idx_hist = list(
      theta = idx_theta,
      transfer = idx_transfer,
      zeta = idx_zeta,
      psi = idx_psi,
      delta_glofas = idx_delta_glofas,
      delta_nws = idx_delta_nws
    ),
    hist_obs = list(
      usgs = h_hist_usgs,
      glofas = h_hist_glofas,
      nws = h_hist_nws
    ),
    hist_discount = hist_discount,
    hist_G_list = hist_G_list,
    lead_specs = lead_specs,
    forecast_G = forecast_G,
    keep_mode = keep_mode,
    m0 = m0,
    C0 = C0,
    active_set_by_lead = active_set_by_lead,
    state_dim_by_lead = state_dim_by_lead,
    K_overlap = min(K_nws, K_glofas),
    K_max = K_max,
    extension_source = if (K_nws >= K_glofas) "nws" else "glofas",
    bridge_source = if (K_nws >= K_glofas) "glofas" else "nws"
  )
}
