ndlm_theory_state_draws <- function(sm, sC, n_draws, seed) {
  set.seed(seed)
  d <- nrow(sm)
  Tn <- ncol(sm)
  out <- array(0, dim = c(d, Tn, n_draws))
  for (t in seq_len(Tn)) {
    Sigma <- as.matrix(sC[, , t])
    if (!all(is.finite(Sigma))) {
      stop(sprintf("[NDLM_COV_NONFINITE] smooth covariance slice t=%d contains non-finite values", as.integer(t)), call. = FALSE)
    }
    L <- ndlm_theory_safe_chol(Sigma)
    Z <- matrix(stats::rnorm(d * n_draws), nrow = d, ncol = n_draws)
    out[, t, ] <- sm[, t] + L %*% Z
  }
  out
}

ndlm_theory_standardize <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  mu <- mean(x, na.rm = TRUE)
  sdv <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(mu)) mu <- 0
  if (!is.finite(sdv) || sdv < 1e-8) {
    return(rep(0, length(x)))
  }
  z <- (x - mu) / sdv
  z[!is.finite(z)] <- 0
  z
}

ndlm_theory_build_ragged_horizon <- function(forecast) {
  k_nws <- suppressWarnings(as.integer(forecast$K_vec[["nws"]]))
  k_glofas <- suppressWarnings(as.integer(forecast$K_vec[["glofas"]]))
  if (!is.finite(k_nws) || k_nws < 1L || !is.finite(k_glofas) || k_glofas < 1L) {
    stop("ndlm theory ragged horizon requires positive K_vec entries for nws and glofas", call. = FALSE)
  }

  k_overlap <- min(k_nws, k_glofas)
  k_max <- max(k_nws, k_glofas)
  k_tail <- max(k_max - k_overlap, 0L)
  extension_source <- if (k_nws >= k_glofas) "nws" else "glofas"
  bridge_source <- if (identical(extension_source, "nws")) "glofas" else "nws"

  active_sources <- lapply(seq_len(k_max), function(k) {
    out <- character(0)
    if (k <= k_nws) out <- c(out, "nws")
    if (k <= k_glofas) out <- c(out, "glofas")
    out
  })

  list(
    K_overlap = as.integer(k_overlap),
    K_max = as.integer(k_max),
    K_tail = as.integer(k_tail),
    K_vec = c(nws = as.integer(k_nws), glofas = as.integer(k_glofas)),
    extension_source = extension_source,
    bridge_source = bridge_source,
    segment_lengths = c(overlap = as.integer(k_overlap), extension = as.integer(k_tail)),
    active_sources = active_sources
  )
}

ndlm_theory_make_df_mat <- function(df, dim_df, n, power = 1L) {
  if (sum(dim_df) != n) {
    stop("sum(dim_df) must equal n for ndlm discount matrix construction", call. = FALSE)
  }
  if (length(df) != length(dim_df)) {
    stop("length(df) must match length(dim_df) for ndlm discount matrix construction", call. = FALSE)
  }
  pwr <- suppressWarnings(as.numeric(power))
  if (!is.finite(pwr) || pwr < 1) pwr <- 1
  dfs <- rep(as.numeric(df), as.integer(dim_df))
  dfs <- pmin(pmax(dfs, 1e-8), 1 - 1e-8)
  idx <- c(0L, cumsum(as.integer(dim_df)))
  out <- matrix(0, nrow = n, ncol = n)
  for (j in seq_len(length(dim_df))) {
    cur <- dfs[idx[j + 1L]]
    scale <- (1 - cur^pwr) / (cur^pwr)
    out[(idx[j] + 1L):idx[j + 1L], (idx[j] + 1L):idx[j + 1L]] <- scale
  }
  out
}

ndlm_theory_df_components <- function(constants, mode = c("hist", "fore"), k = 1L) {
  mode <- match.arg(mode)
  k <- suppressWarnings(as.integer(k[[1L]]))
  if (!is.finite(k) || k < 1L) k <- 1L
  df_hist <- c(constants$df_t, constants$df_s1, constants$df_s2, constants$df_s67)
  if (identical(mode, "hist")) {
    return(df_hist)
  }
  trend_df <- constants$df_t * constants$df_discrep * (constants$lambda ^ max(k - 1L, 0L))
  trend_df <- pmin(pmax(trend_df, 1e-8), 1 - 1e-8)
  c(trend_df, constants$df_s1, constants$df_s2, constants$df_s67)
}

ndlm_theory_q_diag_from_discount <- function(constants, state_dim) {
  state_dim <- suppressWarnings(as.integer(state_dim[[1L]]))
  if (!is.finite(state_dim) || state_dim < 14L) {
    stop("state_dim must be >= 14 for ndlm discount-based q_diag construction", call. = FALSE)
  }
  hist_diag <- rep(1e-8, 7L)
  fore_diag <- rep(1e-8, 7L)
  extra_len <- state_dim - 14L
  extra_diag <- numeric(0)
  if (extra_len > 0L) {
    extra_diag <- rep(1e-8, extra_len)
  }
  q_diag <- c(hist_diag, fore_diag, extra_diag)
  pmax(as.numeric(q_diag), 1e-8)
}

ndlm_theory_local_stabilization_defaults <- function(constants) {
  st <- constants$stabilization
  if (!is.list(st)) st <- list()
  read_num <- function(x, default, min_val = -Inf, max_val = Inf) {
    out <- suppressWarnings(if (is.null(x) || length(x) < 1L) NA_real_ else as.numeric(x[[1L]]))
    if (!is.finite(out)) out <- suppressWarnings(as.numeric(default))
    if (!is.finite(out)) out <- 0
    out <- max(out, as.numeric(min_val))
    out <- min(out, as.numeric(max_val))
    out
  }
  cov_eig_floor <- read_num(st$cov_eig_floor, 1e-8, min_val = 1e-12)
  cov_eig_cap <- read_num(st$cov_eig_cap, 1e8, min_val = cov_eig_floor * 10)
  cov_diag_jitter <- read_num(st$cov_diag_jitter, 1e-10, min_val = 0)
  sigma_upper_cap <- read_num(st$sigma_upper_cap, 1e12, min_val = 1e-6)
  sigma_update_damping <- read_num(st$sigma_update_damping, 1.0, min_val = 0, max_val = 1)
  latent_var_cap_mult <- read_num(st$latent_var_cap_mult, 1e4, min_val = 1)
  latent_var_cap_abs <- read_num(st$latent_var_cap_abs, 1e8, min_val = 1e-6)
  list(
    cov_eig_floor = cov_eig_floor,
    cov_eig_cap = cov_eig_cap,
    cov_diag_jitter = cov_diag_jitter,
    sigma_upper_cap = sigma_upper_cap,
    sigma_update_damping = sigma_update_damping,
    latent_var_cap_mult = latent_var_cap_mult,
    latent_var_cap_abs = latent_var_cap_abs
  )
}

ndlm_theory_stabilize_covariance_local <- function(Sigma, constants) {
  params <- ndlm_theory_local_stabilization_defaults(constants)
  stats <- list(
    calls = 1L,
    cov_projected = 0L,
    cov_floor_clipped = 0L,
    cov_cap_clipped = 0L,
    cov_nonfinite_inputs = 0L
  )
  Sigma <- as.matrix(Sigma)
  if (!is.numeric(Sigma) || nrow(Sigma) != ncol(Sigma)) {
    stop("NDLM local covariance stabilization requires a numeric square matrix", call. = FALSE)
  }
  d <- nrow(Sigma)
  if (!all(is.finite(Sigma))) {
    Sigma[!is.finite(Sigma)] <- 0
    stats$cov_nonfinite_inputs <- 1L
  }
  Sigma <- (Sigma + t(Sigma)) / 2

  eig_vals <- tryCatch(
    suppressWarnings(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values),
    error = function(e) rep(NA_real_, d)
  )
  has_nonfinite_eigs <- any(!is.finite(eig_vals))
  floor_hit <- has_nonfinite_eigs || min(eig_vals, na.rm = TRUE) < params$cov_eig_floor
  cap_hit <- has_nonfinite_eigs || max(eig_vals, na.rm = TRUE) > params$cov_eig_cap
  if (isTRUE(floor_hit) || isTRUE(cap_hit)) {
    stats$cov_projected <- 1L
    stats$cov_floor_clipped <- as.integer(isTRUE(floor_hit))
    stats$cov_cap_clipped <- as.integer(isTRUE(cap_hit))
    eig <- tryCatch(suppressWarnings(eigen(Sigma, symmetric = TRUE)), error = function(e) NULL)
    if (is.null(eig) || any(!is.finite(eig$values)) || any(!is.finite(eig$vectors))) {
      Sigma <- diag(params$cov_eig_floor, d)
    } else {
      vals <- as.numeric(eig$values)
      vals <- pmin(pmax(vals, params$cov_eig_floor), params$cov_eig_cap)
      Sigma <- eig$vectors %*% diag(vals, length(vals)) %*% t(eig$vectors)
    }
  }
  Sigma <- (Sigma + t(Sigma)) / 2
  if (isTRUE(params$cov_diag_jitter > 0)) {
    Sigma <- Sigma + diag(params$cov_diag_jitter, d)
  }
  for (ii in seq_len(3L)) {
    final_min <- tryCatch(
      suppressWarnings(min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values)),
      error = function(e) NA_real_
    )
    if (is.finite(final_min) && final_min >= params$cov_eig_floor) {
      break
    }
    shift <- if (!is.finite(final_min)) params$cov_eig_floor else (params$cov_eig_floor - final_min)
    shift <- max(shift + params$cov_diag_jitter, params$cov_diag_jitter)
    Sigma <- Sigma + diag(shift, d)
    stats$cov_projected <- max(stats$cov_projected, 1L)
    stats$cov_floor_clipped <- max(stats$cov_floor_clipped, 1L)
  }
  chol_pad <- max(params$cov_eig_floor, params$cov_diag_jitter, 1e-12)
  has_chol_contract <- !is.null(tryCatch(
    chol(Sigma + diag(chol_pad, d)),
    error = function(e) NULL
  ))
  if (!isTRUE(has_chol_contract)) {
    eig <- tryCatch(suppressWarnings(eigen(Sigma, symmetric = TRUE)), error = function(e) NULL)
    if (!is.null(eig) && all(is.finite(eig$values)) && all(is.finite(eig$vectors))) {
      vals <- as.numeric(eig$values)
      vals <- pmin(pmax(vals, params$cov_eig_floor), params$cov_eig_cap)
      Sigma <- eig$vectors %*% diag(vals, length(vals)) %*% t(eig$vectors)
      Sigma <- (Sigma + t(Sigma)) / 2
    }
    for (ii in seq_len(6L)) {
      has_chol_contract <- !is.null(tryCatch(
        chol(Sigma + diag(chol_pad, d)),
        error = function(e) NULL
      ))
      if (isTRUE(has_chol_contract)) break
      bump <- max(chol_pad * (2 ^ (ii - 1L)), params$cov_diag_jitter)
      Sigma <- Sigma + diag(bump, d)
      Sigma <- (Sigma + t(Sigma)) / 2
    }
    stats$cov_projected <- max(stats$cov_projected, 1L)
    stats$cov_floor_clipped <- max(stats$cov_floor_clipped, 1L)
  }
  list(cov = Sigma, stats = stats)
}

ndlm_theory_discount_matrix_full <- function(constants, state_dim, k = 1L) {
  state_dim <- suppressWarnings(as.integer(state_dim[[1L]]))
  if (!is.finite(state_dim) || state_dim < 14L) {
    stop("state_dim must be >= 14 for ndlm discount matrix construction", call. = FALSE)
  }
  hist_df <- ndlm_theory_df_components(constants, mode = "hist", k = k)
  discrep_df <- pmin(pmax(constants$df_discrep * hist_df, 1e-8), 1 - 1e-8)
  df_hist <- ndlm_theory_make_df_mat(hist_df, dim_df = c(1L, 2L, 2L, 2L), n = 7L, power = 1L)
  df_discrep <- ndlm_theory_make_df_mat(discrep_df, dim_df = c(1L, 2L, 2L, 2L), n = 7L, power = 1L)
  out <- matrix(0, nrow = state_dim, ncol = state_dim)
  out[1:7, 1:7] <- df_hist
  out[8:14, 8:14] <- df_discrep
  if (state_dim > 14L) {
    extra_n <- state_dim - 14L
    extra_df <- rep(constants$df_covs, extra_n)
    extra_df[[1L]] <- constants$df_trans
    extra_df <- pmin(pmax(extra_df, 1e-8), 1 - 1e-8)
    out[15:state_dim, 15:state_dim] <- diag((1 - extra_df) / extra_df, extra_n)
  }
  out
}

ndlm_theory_safe_chol <- function(Sigma) {
  try_chol <- function(M, jitters) {
    for (j in jitters) {
      out <- tryCatch(chol(M + diag(j, nrow(M))), error = function(e) NULL)
      if (!is.null(out)) return(out)
    }
    NULL
  }

  Sigma <- as.matrix(Sigma)
  if (!all(is.finite(Sigma))) {
    stop("[NDLM_COV_NONFINITE] covariance contains non-finite values", call. = FALSE)
  }
  if (!is.numeric(Sigma) || nrow(Sigma) != ncol(Sigma)) {
    stop("[NDLM_COV_SHAPE] covariance must be a finite square numeric matrix", call. = FALSE)
  }
  Sigma <- (Sigma + t(Sigma)) / 2
  jitters <- c(0, 1e-12, 1e-10, 1e-8, 1e-6, 1e-4, 1e-2)

  out <- try_chol(Sigma, jitters)
  if (!is.null(out)) return(out)

  eig <- eigen(Sigma, symmetric = TRUE)
  vals <- pmax(as.numeric(eig$values), 1e-8)
  Sigma_psd <- eig$vectors %*% diag(vals, length(vals)) %*% t(eig$vectors)
  Sigma_psd <- (Sigma_psd + t(Sigma_psd)) / 2
  if (all(is.finite(Sigma_psd))) {
    out <- try_chol(Sigma_psd, jitters)
    if (!is.null(out)) return(out)
  }

  # Last-resort SPD projection with nearPD for extreme numerical instability.
  if (requireNamespace("Matrix", quietly = TRUE)) {
    np <- tryCatch(
      Matrix::nearPD(
        Sigma,
        corr = FALSE,
        keepDiag = FALSE,
        do2eigen = TRUE,
        doSym = TRUE,
        base.matrix = TRUE
      ),
      error = function(e) NULL
    )
    if (!is.null(np) && !is.null(np$mat)) {
      S_np <- as.matrix(np$mat)
      if (all(is.finite(S_np))) {
        S_np <- (S_np + t(S_np)) / 2
        out <- try_chol(S_np, c(0, jitters, 1e-1))
        if (!is.null(out)) return(out)
      }
    }
  }

  min_eig <- suppressWarnings(min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values))
  stop(
    sprintf(
      "[NDLM_COV_NOT_SPD] unable to obtain SPD covariance for Cholesky (n=%d, min_eig=%s)",
      as.integer(nrow(Sigma)),
      as.character(signif(min_eig, 6))
    ),
    call. = FALSE
  )
}

ndlm_theory_safe_inverse_spd <- function(Sigma, constants = NULL) {
  Sigma <- as.matrix(Sigma)
  if (!is.numeric(Sigma) || nrow(Sigma) != ncol(Sigma)) {
    stop("ndlm_theory_safe_inverse_spd requires a numeric square matrix", call. = FALSE)
  }
  if (!all(is.finite(Sigma))) {
    Sigma[!is.finite(Sigma)] <- 0
  }
  Sigma <- (Sigma + t(Sigma)) / 2
  if (!is.null(constants)) {
    Sigma <- ndlm_theory_stabilize_covariance_local(Sigma, constants = constants)$cov
    params <- ndlm_theory_local_stabilization_defaults(constants)
    eig_floor <- params$cov_eig_floor
    diag_jitter <- params$cov_diag_jitter
  } else {
    eig_floor <- 1e-8
    diag_jitter <- 1e-10
  }
  jitters <- c(0, diag_jitter, 1e-8, 1e-6, 1e-4, 1e-2)
  for (jj in jitters) {
    ch <- tryCatch(chol(Sigma + diag(jj, nrow(Sigma))), error = function(e) NULL)
    if (!is.null(ch)) {
      return(chol2inv(ch))
    }
  }
  eig <- tryCatch(eigen(Sigma, symmetric = TRUE), error = function(e) NULL)
  if (is.null(eig) || any(!is.finite(eig$values)) || any(!is.finite(eig$vectors))) {
    if (requireNamespace("MASS", quietly = TRUE)) {
      return(MASS::ginv(Sigma))
    }
    stop("ndlm_theory_safe_inverse_spd failed to invert covariance matrix", call. = FALSE)
  }
  vals <- pmax(as.numeric(eig$values), eig_floor)
  eig$vectors %*% diag(1 / vals, length(vals)) %*% t(eig$vectors)
}

ndlm_theory_covariance_diagnostics_one <- function(object_name, cov_arr) {
  dims <- dim(cov_arr)
  if (is.null(dims) || length(dims) != 3L || dims[1] != dims[2]) {
    stop(sprintf("[NDLM_COV_SHAPE] %s must be a square 3D covariance array", object_name), call. = FALSE)
  }
  n_slices <- as.integer(dims[3])
  min_eigs <- rep(NA_real_, n_slices)
  min_diags <- rep(NA_real_, n_slices)
  max_asym <- rep(NA_real_, n_slices)
  nonfinite <- rep(FALSE, n_slices)
  base_chol_fail <- rep(FALSE, n_slices)

  for (k in seq_len(n_slices)) {
    S <- as.matrix(cov_arr[, , k, drop = TRUE])
    if (!all(is.finite(S))) {
      nonfinite[k] <- TRUE
      next
    }
    S <- (S + t(S)) / 2
    max_asym[k] <- max(abs(S - t(S)))
    min_diags[k] <- min(diag(S))
    min_eigs[k] <- min(eigen(S, symmetric = TRUE, only.values = TRUE)$values)
    base_try <- tryCatch(chol(S + diag(1e-8, nrow(S))), error = function(e) NULL)
    base_chol_fail[k] <- is.null(base_try)
  }

  data.frame(
    object = object_name,
    n_slices = n_slices,
    matrix_dim = as.integer(dims[1]),
    nonfinite_slices = as.integer(sum(nonfinite)),
    asymmetry_max = if (all(is.na(max_asym))) NA_real_ else max(max_asym, na.rm = TRUE),
    min_diag_min = if (all(is.na(min_diags))) NA_real_ else min(min_diags, na.rm = TRUE),
    min_eig_min = if (all(is.na(min_eigs))) NA_real_ else min(min_eigs, na.rm = TRUE),
    min_eig_p01 = if (all(is.na(min_eigs))) NA_real_ else as.numeric(stats::quantile(min_eigs, probs = 0.01, na.rm = TRUE, names = FALSE)),
    base_chol_fail_slices = as.integer(sum(base_chol_fail, na.rm = TRUE)),
    base_chol_fail_rate = mean(base_chol_fail, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

ndlm_theory_collect_covariance_diagnostics <- function(fit_sC, sC_ens_1, sC_ens_2) {
  rows <- list(
    ndlm_theory_covariance_diagnostics_one("smooth_cov", fit_sC),
    ndlm_theory_covariance_diagnostics_one("forecast_cov_segment_1", sC_ens_1),
    ndlm_theory_covariance_diagnostics_one("forecast_cov_segment_2", sC_ens_2)
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

ndlm_theory_stabilize_cov_array <- function(cov_arr, constants) {
  dims <- dim(cov_arr)
  if (is.null(dims) || length(dims) != 3L || dims[1] != dims[2]) {
    stop("ndlm_theory_stabilize_cov_array expects a square 3D covariance array", call. = FALSE)
  }
  out <- cov_arr
  stats <- list(
    calls = 0L,
    cov_projected = 0L,
    cov_floor_clipped = 0L,
    cov_cap_clipped = 0L,
    cov_nonfinite_inputs = 0L
  )
  add_stats <- function(piece) {
    for (nm in names(stats)) {
      cur <- suppressWarnings(as.integer(piece[[nm]]))
      if (!is.finite(cur)) cur <- 0L
      stats[[nm]] <<- stats[[nm]] + cur
    }
  }
  for (k in seq_len(dims[3])) {
    cur <- ndlm_theory_stabilize_covariance_local(out[, , k, drop = TRUE], constants = constants)
    out[, , k] <- cur$cov
    add_stats(cur$stats)
  }
  list(cov = out, stats = stats)
}

ndlm_theory_alloc_segment_cov <- function(k_len, constants, base_cov, inactive_row = integer(0), start_k = 1L) {
  k_len <- suppressWarnings(as.integer(k_len[[1L]]))
  if (!is.finite(k_len) || k_len < 0L) k_len <- 0L
  start_k <- suppressWarnings(as.integer(start_k[[1L]]))
  if (!is.finite(start_k) || start_k < 1L) start_k <- 1L
  out <- array(0, dim = c(7L, 7L, k_len))
  stab_stats <- list(
    calls = 0L,
    cov_projected = 0L,
    cov_floor_clipped = 0L,
    cov_cap_clipped = 0L,
    cov_nonfinite_inputs = 0L
  )
  add_stats <- function(piece) {
    for (nm in names(stab_stats)) {
      cur <- suppressWarnings(as.integer(piece[[nm]]))
      if (!is.finite(cur)) cur <- 0L
      stab_stats[[nm]] <<- stab_stats[[nm]] + cur
    }
  }
  if (k_len == 0L) {
    attr(out, "stabilization_stats") <- stab_stats
    return(out)
  }
  base_cov <- as.matrix(base_cov)
  if (!all(dim(base_cov) == c(7L, 7L))) {
    stop("base_cov must be 7x7 for ndlm forecast segment covariance construction", call. = FALSE)
  }
  base_stab <- ndlm_theory_stabilize_covariance_local(base_cov, constants = constants)
  add_stats(base_stab$stats)
  P_prev <- base_stab$cov
  inactive_row <- suppressWarnings(as.integer(inactive_row))
  for (k in seq_len(k_len)) {
    k_abs <- as.integer(start_k + k - 1L)
    df_fore_k <- ndlm_theory_df_components(constants, mode = "fore", k = k_abs)
    d <- ndlm_theory_make_df_mat(
      df = df_fore_k,
      dim_df = c(1L, 2L, 2L, 2L),
      n = 7L,
      power = 1L
    )
    Wk <- d * P_prev
    d <- (P_prev + Wk)
    d <- (d + t(d)) / 2
    if (length(inactive_row) > 0L) {
      keep <- inactive_row[inactive_row >= 1L & inactive_row <= 7L]
      if (length(keep) > 0L) {
        d[keep, ] <- 0
        d[, keep] <- 0
        d[cbind(keep, keep)] <- 1e-8
      }
    }
    d <- (d + t(d)) / 2 + diag(1e-8, 7L)
    d_stab <- ndlm_theory_stabilize_covariance_local(d, constants = constants)
    add_stats(d_stab$stats)
    out[, , k] <- d_stab$cov
    P_prev <- d_stab$cov
  }
  attr(out, "stabilization_stats") <- stab_stats
  out
}

ndlm_theory_build_hist_pseudo_obs <- function(
  source_obs,
  sigma_by_source,
  source_names = c("usgs", "nws", "glofas"),
  fallback_y = NULL,
  fallback_var = 1e12
) {
  if (!is.list(source_obs)) {
    stop("source_obs must be a named list of source vectors", call. = FALSE)
  }
  source_names <- unique(as.character(source_names))
  source_names <- source_names[nzchar(source_names)]
  if (length(source_names) < 1L) {
    stop("source_names must include at least one source label", call. = FALSE)
  }

  lengths <- vapply(source_names, function(nm) length(as.numeric(source_obs[[nm]])), integer(1))
  Tn <- suppressWarnings(as.integer(max(lengths)))
  if (!is.finite(Tn) || Tn < 1L) {
    stop("source_obs must include at least one non-empty source vector", call. = FALSE)
  }

  fallback_y <- as.numeric(fallback_y)
  if (length(fallback_y) < Tn) {
    fallback_y <- c(fallback_y, rep(NA_real_, Tn - length(fallback_y)))
  }
  fallback_y <- fallback_y[seq_len(Tn)]

  fallback_var <- suppressWarnings(as.numeric(fallback_var[[1L]]))
  if (!is.finite(fallback_var) || fallback_var <= 0) {
    fallback_var <- 1e12
  }

  y_pseudo <- rep(0, Tn)
  R_vec <- rep(fallback_var, Tn)
  n_sources <- integer(Tn)

  for (t in seq_len(Tn)) {
    obs_vals <- numeric(0)
    prec_vals <- numeric(0)
    for (nm in source_names) {
      obs_nm <- as.numeric(source_obs[[nm]])
      if (length(obs_nm) < t) next
      y_nt <- obs_nm[[t]]
      sigma_nt <- suppressWarnings(as.numeric(sigma_by_source[[nm]]))
      if (!is.finite(y_nt) || !is.finite(sigma_nt) || sigma_nt <= 0) next
      obs_vals <- c(obs_vals, y_nt)
      prec_vals <- c(prec_vals, 1 / max(sigma_nt, 1e-10))
    }

    if (length(obs_vals) > 0L) {
      prec_sum <- sum(prec_vals)
      if (!is.finite(prec_sum) || prec_sum <= 0) {
        prec_sum <- 1e-12
      }
      y_pseudo[[t]] <- sum(obs_vals * prec_vals) / prec_sum
      R_vec[[t]] <- 1 / prec_sum
      n_sources[[t]] <- as.integer(length(obs_vals))
    } else {
      y_fallback <- fallback_y[[t]]
      if (!is.finite(y_fallback)) {
        y_fallback <- 0
      }
      y_pseudo[[t]] <- y_fallback
      R_vec[[t]] <- fallback_var
      n_sources[[t]] <- 0L
    }
  }

  list(
    y = as.numeric(y_pseudo),
    R_vec = pmax(as.numeric(R_vec), 1e-10),
    n_sources = as.integer(n_sources)
  )
}

ndlm_theory_rotation_block <- function(freq) {
  matrix(c(cos(freq), sin(freq), -sin(freq), cos(freq)), nrow = 2L, byrow = TRUE)
}

ndlm_theory_build_base_state_model <- function(period = 363.5854, harmonics = c(1, 2, 1 / 6.8068493)) {
  harms <- as.numeric(harmonics)
  if (length(harms) != 3L || any(!is.finite(harms))) {
    stop("ndlm main requires exactly three finite harmonics for the base state model", call. = FALSE)
  }
  if (!is.finite(period) || period <= 0) {
    stop("ndlm main period must be positive and finite", call. = FALSE)
  }
  F_base <- c(1, rep(c(1, 0), length(harms)))
  G_base <- matrix(0, nrow = 1L + 2L * length(harms), ncol = 1L + 2L * length(harms))
  G_base[1, 1] <- 1
  for (j in seq_along(harms)) {
    i0 <- 2L + 2L * (j - 1L)
    G_base[i0:(i0 + 1L), i0:(i0 + 1L)] <- ndlm_theory_rotation_block(2 * pi * harms[[j]] / period)
  }
  list(F_base = as.numeric(F_base), G_base = G_base)
}

ndlm_theory_build_transfer_G <- function(cov_row, lambda) {
  cov_row <- as.numeric(cov_row)
  p_cov <- length(cov_row)
  out <- diag(1, p_cov + 1L)
  out[1, 1] <- as.numeric(lambda)
  if (p_cov > 0L) {
    out[1, 2:(p_cov + 1L)] <- cov_row
  }
  out
}

ndlm_theory_block_diag <- function(...) {
  mats <- list(...)
  if (length(mats) == 1L && is.list(mats[[1L]]) && !is.matrix(mats[[1L]])) {
    mats <- mats[[1L]]
  }
  mats <- mats[vapply(mats, function(x) !is.null(x) && is.matrix(x) && nrow(x) > 0L, logical(1))]
  if (length(mats) < 1L) {
    return(matrix(0, nrow = 0L, ncol = 0L))
  }
  nr <- sum(vapply(mats, nrow, integer(1)))
  nc <- sum(vapply(mats, ncol, integer(1)))
  out <- matrix(0, nrow = nr, ncol = nc)
  r0 <- 1L
  c0 <- 1L
  for (M in mats) {
    rr <- nrow(M)
    cc <- ncol(M)
    out[r0:(r0 + rr - 1L), c0:(c0 + cc - 1L)] <- M
    r0 <- r0 + rr
    c0 <- c0 + cc
  }
  out
}

ndlm_theory_build_obslist_sequences <- function(inputs, constants) {
  base <- ndlm_theory_build_base_state_model(
    period = constants$period,
    harmonics = constants$harmonics
  )
  F_base <- base$F_base
  G_base <- base$G_base
  q <- length(F_base)
  Tn <- as.integer(inputs$T)
  K_max <- as.integer(inputs$forecast$K_max)
  X_hist <- as.matrix(inputs$X)
  X_future <- as.matrix(inputs$X_future)
  p_cov <- ncol(X_hist)
  transfer_dim <- 1L + p_cov
  alpha_dim <- q + transfer_dim
  state_dim <- alpha_dim + 2L * q
  idx_alpha <- seq_len(alpha_dim)
  idx_transfer <- seq.int(q + 1L, alpha_dim)
  idx_delta_glofas <- seq.int(alpha_dim + 1L, alpha_dim + q)
  idx_delta_nws <- seq.int(alpha_dim + q + 1L, alpha_dim + 2L * q)

  alpha_obs_hist <- c(F_base, 1, rep(0, p_cov))
  alpha_obs_fore <- c(F_base, if (identical(as.character(constants$forecast_transfer_mode), "keep")) 1 else 0, rep(0, p_cov))
  delta_obs <- F_base

  alpha_df <- ndlm_theory_block_diag(
    ndlm_theory_make_df_mat(
      c(constants$df_t, constants$df_s1, constants$df_s2, constants$df_s67),
      dim_df = c(1L, 2L, 2L, 2L),
      n = q,
      power = 1L
    ),
    ndlm_theory_make_df_mat(
      c(constants$df_trans, constants$df_covs),
      dim_df = c(1L, p_cov),
      n = transfer_dim,
      power = 1L
    )
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
  discount_mat <- ndlm_theory_block_diag(alpha_df, discrep_df, discrep_df)

  G_hist <- array(0, dim = c(state_dim, state_dim, Tn))
  for (tt in seq_len(Tn)) {
    G_hist[, , tt] <- ndlm_theory_block_diag(
      ndlm_theory_block_diag(G_base, ndlm_theory_build_transfer_G(X_hist[tt, ], constants$lambda)),
      G_base,
      G_base
    )
  }

  G_future <- array(0, dim = c(state_dim, state_dim, K_max))
  for (kk in seq_len(K_max)) {
    cov_row <- if (identical(as.character(constants$forecast_transfer_mode), "keep")) X_future[kk, ] else rep(0, p_cov)
    G_future[, , kk] <- ndlm_theory_block_diag(
      ndlm_theory_block_diag(G_base, ndlm_theory_build_transfer_G(cov_row, constants$lambda)),
      G_base,
      G_base
    )
  }

  m0 <- rep(0, state_dim)
  y_mu <- suppressWarnings(mean(as.numeric(inputs$y), na.rm = TRUE))
  if (!is.finite(y_mu)) y_mu <- 0
  m0[1] <- y_mu
  C0 <- diag(c(5, rep(1.5, q - 1L), 2, rep(1, p_cov), rep(1.5, 2L * q)), state_dim)

  build_full_h <- function(alpha_obs, delta_idx = integer(0)) {
    h <- rep(0, state_dim)
    h[idx_alpha] <- as.numeric(alpha_obs)
    if (length(delta_idx) == q) {
      h[delta_idx] <- delta_obs
    }
    h
  }

  hist_sources <- list(
    usgs = as.numeric(inputs$retros$usgs),
    glofas = as.numeric(inputs$retros$glofas),
    nws = as.numeric(inputs$retros$nws)
  )
  hist_H <- list(
    usgs = build_full_h(alpha_obs_hist),
    glofas = build_full_h(alpha_obs_hist, idx_delta_glofas),
    nws = build_full_h(alpha_obs_hist, idx_delta_nws)
  )
  future_H <- list(
    usgs = build_full_h(alpha_obs_fore),
    glofas = build_full_h(alpha_obs_fore, idx_delta_glofas),
    nws = build_full_h(alpha_obs_fore, idx_delta_nws)
  )
  hist_seq <- vector("list", Tn)
  for (tt in seq_len(Tn)) {
    y_vec <- c(hist_sources$usgs[tt], hist_sources$glofas[tt], hist_sources$nws[tt])
    H_mat <- rbind(hist_H$usgs, hist_H$glofas, hist_H$nws)
    sources <- c("usgs", "glofas", "nws")
    ok <- is.finite(y_vec)
    hist_seq[[tt]] <- list(
      y = as.numeric(y_vec[ok]),
      H = H_mat[ok, , drop = FALSE],
      sources = sources[ok],
      n_sources = as.integer(sum(ok))
    )
  }

  future_seq <- vector("list", K_max)
  for (kk in seq_len(K_max)) {
    vals <- numeric(0)
    H_rows <- list()
    srcs <- character(0)
    if (is.matrix(inputs$forecast$glofas_members) && kk <= nrow(inputs$forecast$glofas_members)) {
      glofas_row <- as.numeric(inputs$forecast$glofas_members[kk, , drop = TRUE])
      glofas_row <- glofas_row[is.finite(glofas_row)]
      if (length(glofas_row) > 0L) {
        vals <- c(vals, glofas_row)
        H_rows[[length(H_rows) + 1L]] <- matrix(
          rep(build_full_h(alpha_obs_fore, idx_delta_glofas), times = length(glofas_row)),
          nrow = length(glofas_row),
          byrow = TRUE
        )
        srcs <- c(srcs, rep("glofas", length(glofas_row)))
      }
    } else if (kk <= length(inputs$forecast$glofas) && is.finite(inputs$forecast$glofas[[kk]])) {
      vals <- c(vals, as.numeric(inputs$forecast$glofas[[kk]]))
      H_rows[[length(H_rows) + 1L]] <- matrix(build_full_h(alpha_obs_fore, idx_delta_glofas), nrow = 1L)
      srcs <- c(srcs, "glofas")
    }
    if (is.matrix(inputs$forecast$nws_members) && kk <= nrow(inputs$forecast$nws_members)) {
      nws_row <- as.numeric(inputs$forecast$nws_members[kk, , drop = TRUE])
      nws_row <- nws_row[is.finite(nws_row)]
      if (length(nws_row) > 0L) {
        vals <- c(vals, nws_row)
        H_rows[[length(H_rows) + 1L]] <- matrix(
          rep(build_full_h(alpha_obs_fore, idx_delta_nws), times = length(nws_row)),
          nrow = length(nws_row),
          byrow = TRUE
        )
        srcs <- c(srcs, rep("nws", length(nws_row)))
      }
    } else if (kk <= length(inputs$forecast$nws) && is.finite(inputs$forecast$nws[[kk]])) {
      vals <- c(vals, as.numeric(inputs$forecast$nws[[kk]]))
      H_rows[[length(H_rows) + 1L]] <- matrix(build_full_h(alpha_obs_fore, idx_delta_nws), nrow = 1L)
      srcs <- c(srcs, "nws")
    }
    H_mat <- if (length(H_rows) > 0L) do.call(rbind, H_rows) else matrix(0, nrow = 0L, ncol = state_dim)
    future_seq[[kk]] <- list(
      y = as.numeric(vals),
      H = H_mat,
      sources = srcs,
      n_sources = as.integer(length(srcs))
    )
  }

  overlap_idx <- c(seq_len(q), idx_delta_glofas, idx_delta_nws, idx_transfer)
  extension_source <- if (inputs$forecast$K_vec[["nws"]] >= inputs$forecast$K_vec[["glofas"]]) "nws" else "glofas"
  tail_delta_idx <- if (identical(extension_source, "nws")) idx_delta_nws else idx_delta_glofas
  tail_idx <- c(seq_len(q), tail_delta_idx, idx_transfer)

  list(
    q = q,
    p_cov = p_cov,
    transfer_dim = transfer_dim,
    alpha_dim = alpha_dim,
    state_dim = state_dim,
    idx_alpha = idx_alpha,
    idx_transfer = idx_transfer,
    idx_delta_glofas = idx_delta_glofas,
    idx_delta_nws = idx_delta_nws,
    alpha_obs_hist = alpha_obs_hist,
    alpha_obs_fore = alpha_obs_fore,
    hist_H = hist_H,
    future_H = future_H,
    hist_sources = hist_sources,
    hist_seq = hist_seq,
    future_seq = future_seq,
    discount_mat = discount_mat,
    G_hist = G_hist,
    G_future = G_future,
    m0 = m0,
    C0 = C0,
    overlap_export_idx = overlap_idx,
    tail_export_idx = tail_idx,
    extension_source = extension_source,
    bridge_source = if (identical(extension_source, "nws")) "glofas" else "nws"
  )
}

ndlm_theory_has_converged <- function(
  iter,
  min_total_iters,
  crit_elbo,
  crit_elbo_rel,
  elbo_tol,
  elbo_rel_tol
) {
  if (!is.finite(iter) || !is.finite(min_total_iters) || as.integer(iter) < as.integer(min_total_iters)) {
    return(FALSE)
  }
  if (!is.finite(crit_elbo) || !is.finite(crit_elbo_rel)) {
    return(FALSE)
  }
  (crit_elbo <= elbo_tol) && (crit_elbo_rel <= elbo_rel_tol)
}

ndlm_theory_run_obslist <- function(inputs, constants) {
  fmt_iter_num <- function(x, digits = 8L) {
    if (!is.finite(x)) return("NA")
    format(signif(as.numeric(x), digits = as.integer(digits)), trim = TRUE, scientific = FALSE)
  }

  model <- ndlm_theory_build_obslist_sequences(inputs = inputs, constants = constants)
  Tn <- as.integer(inputs$T)
  K_overlap <- as.integer(inputs$forecast$K_overlap)
  K_max <- as.integer(inputs$forecast$K_max)
  K_tail <- max(K_max - K_overlap, 0L)
  source_names <- c("usgs", "nws", "glofas")

  sigma_init <- vapply(source_names, function(nm) {
    x <- as.numeric(model$hist_sources[[nm]])
    sdv <- suppressWarnings(stats::sd(x, na.rm = TRUE))
    if (!is.finite(sdv) || sdv < 0.05) sdv <- 0.05
    as.numeric(sdv)
  }, numeric(1))
  names(sigma_init) <- source_names
  sigma_by_source <- pmax(sigma_init, 1e-6)

  hist_df_components <- ndlm_theory_df_components(constants, mode = "hist", k = 1L)
  fore_df_components <- ndlm_theory_df_components(constants, mode = "fore", k = 1L)
  w_hist <- mean((1 - hist_df_components) / hist_df_components)
  w_fore <- mean((1 - fore_df_components) / fore_df_components)

  max_iter <- suppressWarnings(as.integer(constants$max_iter))
  if (!is.finite(max_iter) || max_iter < 1L) max_iter <- 100L
  min_total_iters <- suppressWarnings(as.integer(constants$min_total_iters))
  if (!is.finite(min_total_iters) || min_total_iters < 1L) min_total_iters <- min(50L, max_iter)
  min_total_iters <- min(min_total_iters, max_iter)

  conv <- constants$convergence
  elbo_tol <- suppressWarnings(as.numeric(conv$elbo_tol))
  elbo_rel_tol <- suppressWarnings(as.numeric(conv$elbo_rel_tol))
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

  prev_elbo <- NA_real_
  crit_elbo <- Inf
  crit_elbo_rel <- Inf
  sigma_shape_final <- rep(constants$a_sigma, length(source_names))
  sigma_rate_final <- rep(constants$b_sigma, length(source_names))
  names(sigma_shape_final) <- source_names
  names(sigma_rate_final) <- source_names
  converged <- FALSE
  convergence_reason <- "max_iter_reached"
  iterations_completed <- 0L

  stab_params <- ndlm_theory_local_stabilization_defaults(constants)
  cov_stab_totals <- list(
    calls = 0L,
    cov_projected = 0L,
    cov_floor_clipped = 0L,
    cov_cap_clipped = 0L,
    cov_nonfinite_inputs = 0L
  )
  accumulate_cov_stats <- function(piece) {
    if (!is.list(piece)) return(invisible(NULL))
    for (nm in names(cov_stab_totals)) {
      cur <- suppressWarnings(as.integer(piece[[nm]]))
      if (!is.finite(cur)) cur <- 0L
      cov_stab_totals[[nm]] <<- cov_stab_totals[[nm]] + cur
    }
    invisible(NULL)
  }
  stabilize_cov <- function(Sigma) {
    out <- ndlm_theory_stabilize_covariance_local(Sigma, constants = constants)
    accumulate_cov_stats(out$stats)
    out$cov
  }

  seq_update <- function(m_in, C_in, y_vec, H_mat, R_vec) {
    m_cur <- as.numeric(m_in)
    C_cur <- stabilize_cov(C_in)
    if (length(y_vec) < 1L) {
      return(list(m = m_cur, C = C_cur))
    }
    for (ii in seq_along(y_vec)) {
      h <- matrix(as.numeric(H_mat[ii, ]), ncol = 1L)
      r <- max(as.numeric(R_vec[[ii]]), 1e-10)
      f <- as.numeric(crossprod(h, m_cur))
      qy <- max(as.numeric(crossprod(h, C_cur %*% h)) + r, 1e-10)
      A <- as.vector((C_cur %*% h) / qy)
      e <- as.numeric(y_vec[[ii]] - f)
      m_cur <- as.numeric(m_cur + A * e)
      C_cur <- stabilize_cov(C_cur - tcrossprod(A, A) * qy)
    }
    list(m = m_cur, C = C_cur)
  }

  filter_hist <- function(cur_sigma) {
    d <- model$state_dim
    a <- matrix(0, nrow = d, ncol = Tn)
    m <- matrix(0, nrow = d, ncol = Tn)
    Rpred <- array(0, dim = c(d, d, Tn))
    C <- array(0, dim = c(d, d, Tn))
    pred_mean <- rep(NA_real_, Tn)
    pred_var <- rep(NA_real_, Tn)
    filt_mean <- rep(NA_real_, Tn)
    filt_var <- rep(NA_real_, Tn)
    n_obs_hist <- integer(Tn)

    m_prev <- as.numeric(model$m0)
    C_prev <- stabilize_cov(model$C0)
    h_usgs <- matrix(as.numeric(model$hist_H$usgs), ncol = 1L)
    sigma_usgs <- max(as.numeric(cur_sigma[["usgs"]]), 1e-10)

    for (tt in seq_len(Tn)) {
      G_t <- model$G_hist[, , tt, drop = TRUE]
      a_t <- as.numeric(G_t %*% m_prev)
      P_t <- stabilize_cov(G_t %*% C_prev %*% t(G_t))
      R_t <- stabilize_cov(P_t + model$discount_mat * P_t + diag(1e-8, d))

      pred_mean[[tt]] <- as.numeric(crossprod(h_usgs, a_t))
      pred_var[[tt]] <- max(as.numeric(crossprod(h_usgs, R_t %*% h_usgs)) + sigma_usgs, 1e-10)

      obs_t <- model$hist_seq[[tt]]
      n_obs_hist[[tt]] <- as.integer(obs_t$n_sources)
      R_vec <- vapply(obs_t$sources, function(nm) max(as.numeric(cur_sigma[[nm]]), 1e-10), numeric(1))
      upd <- seq_update(a_t, R_t, obs_t$y, obs_t$H, R_vec)

      a[, tt] <- a_t
      m[, tt] <- upd$m
      Rpred[, , tt] <- R_t
      C[, , tt] <- upd$C
      filt_mean[[tt]] <- as.numeric(crossprod(h_usgs, upd$m))
      filt_var[[tt]] <- max(as.numeric(crossprod(h_usgs, upd$C %*% h_usgs)) + sigma_usgs, 1e-10)
      m_prev <- upd$m
      C_prev <- upd$C
    }

    list(
      a = a,
      m = m,
      Rpred = Rpred,
      C = C,
      pred_mean = pred_mean,
      pred_var = pred_var,
      filt_mean = filt_mean,
      filt_var = filt_var,
      n_obs_hist = n_obs_hist
    )
  }

  smooth_hist <- function(filter_out) {
    d <- nrow(filter_out$m)
    ms <- filter_out$m
    Cs <- filter_out$C
    if (Tn >= 2L) {
      for (tt in (Tn - 1L):1L) {
        R_next <- stabilize_cov(filter_out$Rpred[, , tt + 1L, drop = TRUE])
        G_next <- model$G_hist[, , tt + 1L, drop = TRUE]
        R_next_inv <- ndlm_theory_safe_inverse_spd(R_next, constants = constants)
        B_t <- filter_out$C[, , tt, drop = TRUE] %*% t(G_next) %*% R_next_inv
        ms[, tt] <- as.numeric(filter_out$m[, tt] + B_t %*% (ms[, tt + 1L] - filter_out$a[, tt + 1L]))
        Cs[, , tt] <- stabilize_cov(
          filter_out$C[, , tt, drop = TRUE] +
            B_t %*% (Cs[, , tt + 1L, drop = TRUE] - R_next) %*% t(B_t)
        )
      }
    }
    list(m = ms, C = Cs)
  }

  source_stats_from_smoother <- function(smoother, h_vec, obs_vec) {
    h <- matrix(as.numeric(h_vec), ncol = 1L)
    mu <- vapply(seq_len(Tn), function(tt) as.numeric(crossprod(h, smoother$m[, tt])), numeric(1))
    vv <- vapply(
      seq_len(Tn),
      function(tt) max(as.numeric(crossprod(h, smoother$C[, , tt, drop = TRUE] %*% h)), 1e-10),
      numeric(1)
    )
    list(mean = mu, var = vv, obs = as.numeric(obs_vec))
  }

  forecast_filter <- function(m_start, C_start, cur_sigma) {
    d <- length(m_start)
    a <- matrix(0, nrow = d, ncol = K_max)
    m <- matrix(0, nrow = d, ncol = K_max)
    Rpred <- array(0, dim = c(d, d, K_max))
    C <- array(0, dim = c(d, d, K_max))
    usgs_mean_prior <- rep(NA_real_, K_max)
    usgs_var_prior <- rep(NA_real_, K_max)
    usgs_mean_post <- rep(NA_real_, K_max)
    usgs_var_post <- rep(NA_real_, K_max)
    delta_glofas_post <- rep(NA_real_, K_max)
    delta_nws_post <- rep(NA_real_, K_max)

    m_prev <- as.numeric(m_start)
    C_prev <- stabilize_cov(C_start)
    h_usgs <- matrix(as.numeric(model$future_H$usgs), ncol = 1L)
    h_dg <- matrix(as.numeric(model$future_H$glofas - model$future_H$usgs), ncol = 1L)
    h_dn <- matrix(as.numeric(model$future_H$nws - model$future_H$usgs), ncol = 1L)

    for (kk in seq_len(K_max)) {
      G_k <- model$G_future[, , kk, drop = TRUE]
      a_k <- as.numeric(G_k %*% m_prev)
      P_k <- stabilize_cov(G_k %*% C_prev %*% t(G_k))
      R_k <- stabilize_cov(P_k + model$discount_mat * P_k + diag(1e-8, d))

      usgs_mean_prior[[kk]] <- as.numeric(crossprod(h_usgs, a_k))
      usgs_var_prior[[kk]] <- max(as.numeric(crossprod(h_usgs, R_k %*% h_usgs)), 1e-10)

      obs_k <- model$future_seq[[kk]]
      R_vec <- vapply(obs_k$sources, function(nm) max(as.numeric(cur_sigma[[nm]]), 1e-10), numeric(1))
      upd <- seq_update(a_k, R_k, obs_k$y, obs_k$H, R_vec)

      a[, kk] <- a_k
      m[, kk] <- upd$m
      Rpred[, , kk] <- R_k
      C[, , kk] <- upd$C
      usgs_mean_post[[kk]] <- as.numeric(crossprod(h_usgs, upd$m))
      usgs_var_post[[kk]] <- max(as.numeric(crossprod(h_usgs, upd$C %*% h_usgs)), 1e-10)
      delta_glofas_post[[kk]] <- as.numeric(crossprod(h_dg, upd$m))
      delta_nws_post[[kk]] <- as.numeric(crossprod(h_dn, upd$m))

      m_prev <- upd$m
      C_prev <- upd$C
    }

    list(
      a = a,
      m = m,
      Rpred = Rpred,
      C = C,
      usgs_mean_prior = usgs_mean_prior,
      usgs_var_prior = usgs_var_prior,
      usgs_mean_post = usgs_mean_post,
      usgs_var_post = usgs_var_post,
      delta_glofas_post = delta_glofas_post,
      delta_nws_post = delta_nws_post
    )
  }

  fit_hist <- NULL
  smooth_hist_out <- NULL
  obs_stats <- NULL
  latent_var_clipped_total <- 0L
  sigma_capped_total <- 0L
  sigma_damped_total <- 0L
  latent_var_cap_last <- stab_params$latent_var_cap_abs

  for (iter in seq_len(max_iter)) {
    fit_hist <- filter_hist(sigma_by_source)
    smooth_hist_out <- smooth_hist(fit_hist)

    obs_stats <- list(
      usgs = source_stats_from_smoother(smooth_hist_out, model$hist_H$usgs, model$hist_sources$usgs),
      nws = source_stats_from_smoother(smooth_hist_out, model$hist_H$nws, model$hist_sources$nws),
      glofas = source_stats_from_smoother(smooth_hist_out, model$hist_H$glofas, model$hist_sources$glofas)
    )

    source_elbo <- rep(0, length(source_names))
    names(source_elbo) <- source_names
    sigma_next <- sigma_by_source
    for (nm in source_names) {
      stat <- obs_stats[[nm]]
      ok <- is.finite(stat$obs) & is.finite(stat$mean) & is.finite(stat$var)
      n_obs <- sum(ok)
      sigma_shape <- constants$a_sigma + n_obs / 2
      sigma_rate <- constants$b_sigma
      if (n_obs > 0L) {
        obs_var_ref <- suppressWarnings(stats::var(stat$obs[ok], na.rm = TRUE))
        if (!is.finite(obs_var_ref) || obs_var_ref <= 0) obs_var_ref <- 1.0
        latent_var_cap <- max(stab_params$latent_var_cap_abs, stab_params$latent_var_cap_mult * obs_var_ref)
        latent_var_cap_last <- latent_var_cap
        v_use <- pmin(pmax(stat$var[ok], 1e-10), latent_var_cap)
        latent_var_clipped_total <- latent_var_clipped_total +
          as.integer(sum(is.finite(stat$var[ok]) & stat$var[ok] > latent_var_cap))
        resid <- stat$obs[ok] - stat$mean[ok]
        sigma_rate <- sigma_rate + 0.5 * sum(resid^2 + v_use)
      }
      sigma_new_raw <- sigma_rate / max(sigma_shape - 1, 1.01)
      sigma_prev <- max(as.numeric(sigma_by_source[[nm]]), 1e-6)
      if (!is.finite(sigma_new_raw) || sigma_new_raw <= 0) sigma_new_raw <- sigma_prev
      if (sigma_new_raw > stab_params$sigma_upper_cap) sigma_capped_total <- sigma_capped_total + 1L
      sigma_new <- min(max(sigma_new_raw, 1e-6), stab_params$sigma_upper_cap)
      if (stab_params$sigma_update_damping < 1) sigma_damped_total <- sigma_damped_total + 1L
      sigma_new <- stab_params$sigma_update_damping * sigma_new +
        (1 - stab_params$sigma_update_damping) * sigma_prev
      sigma_new <- min(max(sigma_new, 1e-6), stab_params$sigma_upper_cap)
      sigma_next[[nm]] <- sigma_new
      sigma_shape_final[[nm]] <- sigma_shape
      sigma_rate_final[[nm]] <- sigma_rate

      prior_term <- constants$a_sigma * log(constants$b_sigma) -
        lgamma(constants$a_sigma) -
        (constants$a_sigma + 1) * log(sigma_new) -
        constants$b_sigma / sigma_new
      ll_term <- 0
      if (n_obs > 0L) {
        resid <- stat$obs[ok] - stat$mean[ok]
        ll_term <- -0.5 * sum(log(2 * pi * sigma_new) + (resid^2 + pmax(stat$var[ok], 1e-10)) / sigma_new)
      }
      source_elbo[[nm]] <- ll_term + prior_term
    }

    sigma_by_source <- sigma_next
    seq_sigma[iter, ] <- as.numeric(sigma_by_source[source_names])
    seq_elbo[iter] <- sum(as.numeric(source_elbo))
    seq_scale[iter, ] <- c(
      sigma_by_source[["usgs"]],
      sigma_by_source[["usgs"]],
      sigma_by_source[["nws"]],
      sigma_by_source[["glofas"]],
      w_hist,
      w_fore,
      constants$df_t,
      constants$df_s1,
      constants$df_s2,
      constants$df_s67,
      constants$df_discrep,
      constants$lambda,
      constants$df_trans,
      constants$df_covs
    )
    if (is.finite(prev_elbo) && is.finite(seq_elbo[iter])) {
      crit_elbo <- abs(seq_elbo[iter] - prev_elbo)
      crit_elbo_rel <- crit_elbo / max(abs(prev_elbo), 1e-12)
    } else {
      crit_elbo <- Inf
      crit_elbo_rel <- Inf
    }
    prev_elbo <- seq_elbo[iter]
    iterations_completed <- as.integer(iter)

    state_norm_sq <- suppressWarnings(as.numeric(sum(smooth_hist_out$m^2, na.rm = TRUE)))
    if (!is.finite(state_norm_sq)) state_norm_sq <- NA_real_
    cat(
      sprintf(
        "[gamsig_progress] family=ndlm_main p0=NA iter=%d elbo=%s crit_elbo=%s crit_elbo_rel=%s sigma_exp=%s sigma_usgs_exp=%s sigma_nws_exp=%s sigma_glofas_exp=%s gamma_exp=NA state_norm_sq=%s w_hist=%s w_fore=%s df_t=%s df_s1=%s df_s2=%s df_s67=%s df_discrep=%s lambda=%s latent_var_cap=%s cov_proj_total=%d sigma_cap_total=%d\n",
        as.integer(iter),
        fmt_iter_num(seq_elbo[iter]),
        fmt_iter_num(crit_elbo),
        fmt_iter_num(crit_elbo_rel),
        fmt_iter_num(sigma_by_source[["usgs"]]),
        fmt_iter_num(sigma_by_source[["usgs"]]),
        fmt_iter_num(sigma_by_source[["nws"]]),
        fmt_iter_num(sigma_by_source[["glofas"]]),
        fmt_iter_num(state_norm_sq),
        fmt_iter_num(w_hist),
        fmt_iter_num(w_fore),
        fmt_iter_num(constants$df_t),
        fmt_iter_num(constants$df_s1),
        fmt_iter_num(constants$df_s2),
        fmt_iter_num(constants$df_s67),
        fmt_iter_num(constants$df_discrep),
        fmt_iter_num(constants$lambda),
        fmt_iter_num(latent_var_cap_last),
        as.integer(cov_stab_totals$cov_projected),
        as.integer(sigma_capped_total)
      )
    )

    if (ndlm_theory_has_converged(
      iter = iter,
      min_total_iters = min_total_iters,
      crit_elbo = crit_elbo,
      crit_elbo_rel = crit_elbo_rel,
      elbo_tol = elbo_tol,
      elbo_rel_tol = elbo_rel_tol
    )) {
      converged <- TRUE
      convergence_reason <- "all_convergence_criteria_met"
      break
    }
  }

  if (is.null(fit_hist) || is.null(smooth_hist_out)) {
    stop("ndlm main observation-list fit failed to initialize", call. = FALSE)
  }
  if (iterations_completed < 1L) iterations_completed <- max_iter
  seq_sigma <- seq_sigma[seq_len(iterations_completed), , drop = FALSE]
  seq_elbo <- seq_elbo[seq_len(iterations_completed)]
  seq_scale <- seq_scale[seq_len(iterations_completed), , drop = FALSE]

  smooth_hist_out$C <- ndlm_theory_stabilize_cov_array(smooth_hist_out$C, constants = constants)$cov
  fit_hist$C <- ndlm_theory_stabilize_cov_array(fit_hist$C, constants = constants)$cov
  fit_hist$Rpred <- ndlm_theory_stabilize_cov_array(fit_hist$Rpred, constants = constants)$cov

  forecast_out <- forecast_filter(
    m_start = smooth_hist_out$m[, Tn, drop = TRUE],
    C_start = smooth_hist_out$C[, , Tn, drop = TRUE],
    cur_sigma = sigma_by_source
  )
  forecast_out$C <- ndlm_theory_stabilize_cov_array(forecast_out$C, constants = constants)$cov
  forecast_out$Rpred <- ndlm_theory_stabilize_cov_array(forecast_out$Rpred, constants = constants)$cov

  y_smoothed <- obs_stats$usgs$mean
  y_smoothed_var <- pmax(obs_stats$usgs$var + sigma_by_source[["usgs"]], 1e-10)
  exps <- rbind(y_smoothed, y_smoothed)
  vars <- rbind(y_smoothed_var, y_smoothed_var)
  exps2 <- exps^2 + vars

  overlap_idx <- model$overlap_export_idx
  tail_idx <- model$tail_export_idx
  sm_ens_1 <- if (K_overlap > 0L) forecast_out$m[overlap_idx, seq_len(K_overlap), drop = FALSE] else matrix(0, nrow = length(overlap_idx), ncol = 0L)
  sC_ens_1 <- if (K_overlap > 0L) {
    arr <- array(0, dim = c(length(overlap_idx), length(overlap_idx), K_overlap))
    for (kk in seq_len(K_overlap)) {
      arr[, , kk] <- stabilize_cov(forecast_out$C[overlap_idx, overlap_idx, kk, drop = TRUE])
    }
    arr
  } else {
    array(0, dim = c(length(overlap_idx), length(overlap_idx), 0L))
  }
  sm_ens_2 <- if (K_tail > 0L) forecast_out$m[tail_idx, seq.int(K_overlap + 1L, K_max), drop = FALSE] else matrix(0, nrow = length(tail_idx), ncol = 0L)
  sC_ens_2 <- if (K_tail > 0L) {
    arr <- array(0, dim = c(length(tail_idx), length(tail_idx), K_tail))
    for (kk in seq_len(K_tail)) {
      arr[, , kk] <- stabilize_cov(forecast_out$C[tail_idx, tail_idx, K_overlap + kk, drop = TRUE])
    }
    arr
  } else {
    array(0, dim = c(length(tail_idx), length(tail_idx), 0L))
  }

  n_draws <- suppressWarnings(as.integer(constants$n_draws))
  if (!is.finite(n_draws) || n_draws < 4L) n_draws <- 48L
  samp_theta_retro <- ndlm_theory_state_draws(
    sm = smooth_hist_out$m,
    sC = smooth_hist_out$C,
    n_draws = n_draws,
    seed = constants$seed + 11L
  )
  draw_segment <- function(mean_mat, cov_arr, seed) {
    k_len <- ncol(mean_mat)
    d_seg <- nrow(mean_mat)
    out <- array(0, dim = c(d_seg, k_len, n_draws))
    if (k_len < 1L) return(out)
    set.seed(seed)
    for (kk in seq_len(k_len)) {
      L <- ndlm_theory_safe_chol(cov_arr[, , kk, drop = TRUE])
      Z <- matrix(stats::rnorm(d_seg * n_draws), nrow = d_seg, ncol = n_draws)
      out[, kk, ] <- mean_mat[, kk] + L %*% Z
    }
    out
  }
  samp_theta_ens_1 <- draw_segment(sm_ens_1, sC_ens_1, constants$seed + 21L)
  samp_theta_ens_2 <- draw_segment(sm_ens_2, sC_ens_2, constants$seed + 22L)

  mean_draws_loglog1p <- matrix(NA_real_, nrow = n_draws, ncol = K_max)
  set.seed(constants$seed + 23L)
  for (kk in seq_len(K_max)) {
    mean_draws_loglog1p[, kk] <- stats::rnorm(
      n_draws,
      mean = as.numeric(forecast_out$usgs_mean_post[[kk]]),
      sd = sqrt(max(as.numeric(forecast_out$usgs_var_post[[kk]]), 1e-10))
    )
  }

  set.seed(constants$seed + 33L)
  samp_sigma <- matrix(NA_real_, nrow = length(source_names), ncol = n_draws)
  rownames(samp_sigma) <- source_names
  for (j in seq_along(source_names)) {
    nm <- source_names[[j]]
    shp <- sigma_shape_final[[nm]]
    rte <- sigma_rate_final[[nm]]
    if (!is.finite(shp) || shp <= 0) shp <- constants$a_sigma
    if (!is.finite(rte) || rte <= 0) rte <- constants$b_sigma
    samp_sigma[j, ] <- 1 / stats::rgamma(n_draws, shape = shp, rate = rte)
  }

  standard_forecast_errors <- rep(NA_real_, K_max)
  standard_forecast_errors[seq_len(K_overlap)] <- inputs$forecast$nws[seq_len(K_overlap)] - inputs$forecast$glofas[seq_len(K_overlap)]
  if (K_tail > 0L) {
    if (identical(model$extension_source, "nws")) {
      tail_idx_raw <- seq.int(K_overlap + 1L, inputs$forecast$K_vec[["nws"]])
      bridge_raw <- as.numeric(inputs$forecast$glofas[K_overlap])
      if (!is.finite(bridge_raw)) bridge_raw <- 0
      standard_forecast_errors[seq.int(K_overlap + 1L, K_max)] <- inputs$forecast$nws[tail_idx_raw] - bridge_raw
    } else {
      tail_idx_raw <- seq.int(K_overlap + 1L, inputs$forecast$K_vec[["glofas"]])
      bridge_raw <- as.numeric(inputs$forecast$nws[K_overlap])
      if (!is.finite(bridge_raw)) bridge_raw <- 0
      standard_forecast_errors[seq.int(K_overlap + 1L, K_max)] <- bridge_raw - inputs$forecast$glofas[tail_idx_raw]
    }
  }
  standard_forecast_errors[!is.finite(standard_forecast_errors)] <- 0
  standard_forecast_errors <- matrix(standard_forecast_errors, nrow = 1L)

  active_set_by_lead <- data.frame(
    lead = seq_len(K_max),
    active_nws = as.integer(seq_len(K_max) <= inputs$forecast$K_vec[["nws"]]),
    active_glofas = as.integer(seq_len(K_max) <= inputs$forecast$K_vec[["glofas"]]),
    active_count = as.integer((seq_len(K_max) <= inputs$forecast$K_vec[["nws"]]) + (seq_len(K_max) <= inputs$forecast$K_vec[["glofas"]])),
    nws_member_count = if (is.matrix(inputs$forecast$nws_members)) {
      rowSums(is.finite(inputs$forecast$nws_members[seq_len(K_max), , drop = FALSE]))
    } else {
      as.integer(seq_len(K_max) <= inputs$forecast$K_vec[["nws"]])
    },
    glofas_member_count = if (is.matrix(inputs$forecast$glofas_members)) {
      rowSums(is.finite(inputs$forecast$glofas_members[seq_len(K_max), , drop = FALSE]))
    } else {
      as.integer(seq_len(K_max) <= inputs$forecast$K_vec[["glofas"]])
    },
    stringsAsFactors = FALSE
  )
  active_set_by_lead$active_member_count <- as.integer(active_set_by_lead$nws_member_count + active_set_by_lead$glofas_member_count)
  state_dim_by_lead <- data.frame(
    lead = seq_len(K_max),
    state_dim = c(rep(length(overlap_idx), K_overlap), rep(length(tail_idx), K_tail)),
    stringsAsFactors = FALSE
  )

  hist_identity <- data.frame(
    t = seq_len(Tn),
    usgs_obs = as.numeric(model$hist_sources$usgs),
    mu_usgs = as.numeric(obs_stats$usgs$mean),
    mu_glofas = as.numeric(obs_stats$glofas$mean),
    mu_nws = as.numeric(obs_stats$nws$mean),
    delta_glofas = as.numeric(obs_stats$glofas$mean - obs_stats$usgs$mean),
    delta_nws = as.numeric(obs_stats$nws$mean - obs_stats$usgs$mean),
    identity_err_glofas = as.numeric(obs_stats$glofas$mean - obs_stats$usgs$mean - (obs_stats$glofas$mean - obs_stats$usgs$mean)),
    identity_err_nws = as.numeric(obs_stats$nws$mean - obs_stats$usgs$mean - (obs_stats$nws$mean - obs_stats$usgs$mean)),
    n_sources = as.integer(fit_hist$n_obs_hist),
    stringsAsFactors = FALSE
  )
  forecast_identity <- data.frame(
    lead = seq_len(K_max),
    mu_usgs_prior = as.numeric(forecast_out$usgs_mean_prior),
    mu_usgs_post = as.numeric(forecast_out$usgs_mean_post),
    var_usgs_post = as.numeric(forecast_out$usgs_var_post),
    delta_glofas_post = as.numeric(forecast_out$delta_glofas_post),
    delta_nws_post = as.numeric(forecast_out$delta_nws_post),
    glofas_obs = c(as.numeric(inputs$forecast$glofas), rep(NA_real_, K_max - length(inputs$forecast$glofas)))[seq_len(K_max)],
    nws_obs = c(as.numeric(inputs$forecast$nws), rep(NA_real_, K_max - length(inputs$forecast$nws)))[seq_len(K_max)],
    glofas_member_count = if (is.matrix(inputs$forecast$glofas_members)) rowSums(is.finite(inputs$forecast$glofas_members[seq_len(K_max), , drop = FALSE])) else as.integer(seq_len(K_max) <= length(inputs$forecast$glofas)),
    nws_member_count = if (is.matrix(inputs$forecast$nws_members)) rowSums(is.finite(inputs$forecast$nws_members[seq_len(K_max), , drop = FALSE])) else as.integer(seq_len(K_max) <= length(inputs$forecast$nws)),
    usgs_from_glofas = as.numeric(c(as.numeric(inputs$forecast$glofas), rep(NA_real_, K_max - length(inputs$forecast$glofas)))[seq_len(K_max)] - forecast_out$delta_glofas_post),
    usgs_from_nws = as.numeric(c(as.numeric(inputs$forecast$nws), rep(NA_real_, K_max - length(inputs$forecast$nws)))[seq_len(K_max)] - forecast_out$delta_nws_post),
    stringsAsFactors = FALSE
  )

  fit_diagnostics <- list(
    y_observed = as.numeric(model$hist_sources$usgs),
    y_predicted_one_step = as.numeric(fit_hist$pred_mean),
    y_filtered = as.numeric(fit_hist$filt_mean),
    y_smoothed = as.numeric(obs_stats$usgs$mean),
    var_predicted_one_step = as.numeric(fit_hist$pred_var),
    var_filtered = as.numeric(fit_hist$filt_var),
    var_smoothed = as.numeric(y_smoothed_var),
    residual_one_step = as.numeric(model$hist_sources$usgs - fit_hist$pred_mean),
    residual_filtered = as.numeric(model$hist_sources$usgs - fit_hist$filt_mean),
    residual_smoothed = as.numeric(model$hist_sources$usgs - obs_stats$usgs$mean),
    residual_source_usgs = as.numeric(model$hist_sources$usgs - obs_stats$usgs$mean),
    residual_source_nws = as.numeric(model$hist_sources$nws - obs_stats$nws$mean),
    residual_source_glofas = as.numeric(model$hist_sources$glofas - obs_stats$glofas$mean),
    hist_identity = hist_identity,
    forecast_identity = forecast_identity,
    latent_var_cap_last = latent_var_cap_last,
    latent_var_clipped_total = as.integer(latent_var_clipped_total)
  )

  cov_diag <- ndlm_theory_collect_covariance_diagnostics(
    fit_sC = smooth_hist_out$C,
    sC_ens_1 = sC_ens_1,
    sC_ens_2 = sC_ens_2
  )

  new_theta <- list(
    sm = smooth_hist_out$m,
    sC = smooth_hist_out$C,
    exps = exps,
    exps2 = exps2,
    vars = vars,
    sm_ens = list(sm_ens_1, sm_ens_2),
    sC_ens = list(sC_ens_1, sC_ens_2),
    standard_forecast_errors = standard_forecast_errors,
    forecast_mean_draws_loglog1p = mean_draws_loglog1p,
    forecast_horizon = list(
      K_vec = inputs$forecast$K_vec,
      K_overlap = K_overlap,
      K_max = K_max,
      segment_lengths = c(overlap = K_overlap, extension = K_tail),
      extension_source = model$extension_source,
      bridge_source = model$bridge_source,
      forecast_transfer_mode = if (identical(as.character(constants$forecast_transfer_mode), "keep")) "keep" else "drop",
      transfer_active_forecast_window = identical(as.character(constants$forecast_transfer_mode), "keep")
    )
  )

  list(
    new_theta = new_theta,
    samp_theta = list(samp_theta = samp_theta_retro),
    samp_theta_ens = list(list(samp_theta = samp_theta_ens_1), list(samp_theta = samp_theta_ens_2)),
    samp_sigma = samp_sigma,
    seq_sigma = seq_sigma,
    seq_scale = seq_scale,
    seq_elbo = seq_elbo,
    delta = c(diff(seq_elbo), 0),
    iterations_completed = iterations_completed,
    max_iter = max_iter,
    converged = converged,
    convergence_reason = convergence_reason,
    convergence_metrics = c(
      crit_elbo = suppressWarnings(as.numeric(crit_elbo)),
      crit_elbo_rel = suppressWarnings(as.numeric(crit_elbo_rel)),
      elbo_tol = elbo_tol,
      elbo_rel_tol = elbo_rel_tol
    ),
    sigma = as.numeric(sigma_by_source[["usgs"]]),
    sigma_by_source = sigma_by_source[source_names],
    sigma_mean = mean(as.numeric(sigma_by_source[source_names])),
    w_hist = w_hist,
    w_fore = w_fore,
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
    K = K_max,
    K_overlap = K_overlap,
    K_max = K_max,
    K_vec = inputs$forecast$K_vec,
    segment_lengths = c(overlap = K_overlap, extension = K_tail),
    extension_source = model$extension_source,
    bridge_source = model$bridge_source,
    forecast_transfer_mode = if (identical(as.character(constants$forecast_transfer_mode), "keep")) "keep" else "drop",
    transfer_active_forecast_window = identical(as.character(constants$forecast_transfer_mode), "keep"),
    active_set_by_lead = active_set_by_lead,
    forecast_member_counts = active_set_by_lead[, c("lead", "nws_member_count", "glofas_member_count", "active_member_count"), drop = FALSE],
    state_dim_by_lead = state_dim_by_lead,
    covariance_diagnostics = cov_diag,
    fit_diagnostics = fit_diagnostics,
    stabilization = list(
      cov_calls = as.integer(cov_stab_totals$calls),
      cov_projected = as.integer(cov_stab_totals$cov_projected),
      cov_floor_clipped = as.integer(cov_stab_totals$cov_floor_clipped),
      cov_cap_clipped = as.integer(cov_stab_totals$cov_cap_clipped),
      cov_nonfinite_inputs = as.integer(cov_stab_totals$cov_nonfinite_inputs),
      sigma_upper_cap = as.numeric(stab_params$sigma_upper_cap),
      sigma_update_damping = as.numeric(stab_params$sigma_update_damping),
      sigma_capped_total = as.integer(sigma_capped_total),
      sigma_damped_total = as.integer(sigma_damped_total),
      latent_var_cap_abs = as.numeric(stab_params$latent_var_cap_abs),
      latent_var_cap_mult = as.numeric(stab_params$latent_var_cap_mult),
      latent_var_cap_last = as.numeric(latent_var_cap_last),
      latent_var_clipped_total = as.integer(latent_var_clipped_total)
    ),
    K_cap = inputs$forecast$K_cap,
    nws_len = inputs$forecast$nws_len,
    glofas_len = inputs$forecast$glofas_len,
    T = Tn
  )
}

ndlm_theory_run_vb <- function(inputs, constants) {
  fmt_iter_num <- function(x, digits = 8L) {
    if (!is.finite(x)) {
      return("NA")
    }
    format(signif(as.numeric(x), digits = as.integer(digits)), trim = TRUE, scientific = FALSE)
  }

  set.seed(constants$seed)
  Tn <- inputs$T
  d <- constants$state_dim
  ragged <- ndlm_theory_build_ragged_horizon(inputs$forecast)
  K_overlap <- ragged$K_overlap
  K_tail <- ragged$K_tail
  K_max <- ragged$K_max

  H_mat <- matrix(0, nrow = Tn, ncol = d)
  H_mat[, 1] <- 1
  H_mat[, 2:6] <- inputs$X[, 1:5, drop = FALSE]
  if (Tn >= 2) {
    H_mat[-1, 7] <- diff(inputs$y)
  }
  H_mat[, 8:12] <- inputs$X[, 1:5, drop = FALSE]

  m0 <- rep(0, d)
  C0 <- diag(c(5, rep(1, d - 1)), d)

  source_names <- c("usgs", "nws", "glofas")
  retros <- inputs$retros
  if (!is.list(retros)) {
    retros <- list()
  }
  source_obs <- list(
    usgs = as.numeric(if (!is.null(retros$usgs)) retros$usgs else inputs$y),
    nws = as.numeric(if (!is.null(retros$nws)) retros$nws else rep(NA_real_, Tn)),
    glofas = as.numeric(if (!is.null(retros$glofas)) retros$glofas else rep(NA_real_, Tn))
  )
  for (nm in source_names) {
    cur <- source_obs[[nm]]
    if (length(cur) < Tn) {
      cur <- c(cur, rep(NA_real_, Tn - length(cur)))
    }
    source_obs[[nm]] <- as.numeric(cur[seq_len(Tn)])
  }

  sigma_init <- vapply(source_names, function(nm) {
    x <- source_obs[[nm]]
    sdv <- suppressWarnings(stats::sd(x, na.rm = TRUE))
    if (!is.finite(sdv) || sdv < 0.1) sdv <- 0.1
    as.numeric(sdv)
  }, numeric(1))
  names(sigma_init) <- source_names
  sigma_by_source <- pmax(sigma_init, 1e-6)

  hist_df_components <- ndlm_theory_df_components(constants, mode = "hist", k = 1L)
  fore_df_components <- ndlm_theory_df_components(constants, mode = "fore", k = 1L)
  w_hist <- mean((1 - hist_df_components) / hist_df_components)
  w_fore <- mean((1 - fore_df_components) / fore_df_components)

  max_iter <- suppressWarnings(as.integer(constants$max_iter))
  if (!is.finite(max_iter) || max_iter < 1L) {
    max_iter <- 100L
  }
  min_total_iters <- suppressWarnings(as.integer(constants$min_total_iters))
  if (!is.finite(min_total_iters) || min_total_iters < 1L) {
    min_total_iters <- min(50L, max_iter)
  }
  min_total_iters <- min(min_total_iters, max_iter)
  conv <- constants$convergence
  elbo_tol <- suppressWarnings(as.numeric(conv$elbo_tol))
  elbo_rel_tol <- suppressWarnings(as.numeric(conv$elbo_rel_tol))
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
  prev_elbo <- NA_real_
  crit_elbo <- Inf
  crit_elbo_rel <- Inf
  sigma_shape_final <- rep(constants$a_sigma, length(source_names))
  sigma_rate_final <- rep(constants$b_sigma, length(source_names))
  names(sigma_shape_final) <- source_names
  names(sigma_rate_final) <- source_names
  fit <- NULL
  converged <- FALSE
  convergence_reason <- "max_iter_reached"
  iterations_completed <- 0L
  df_mat_full <- ndlm_theory_discount_matrix_full(constants, state_dim = d, k = 1L)
  q_diag <- ndlm_theory_q_diag_from_discount(constants, state_dim = d)
  stab_params <- ndlm_theory_local_stabilization_defaults(constants)
  cov_stab_totals <- list(
    calls = 0L,
    cov_projected = 0L,
    cov_floor_clipped = 0L,
    cov_cap_clipped = 0L,
    cov_nonfinite_inputs = 0L
  )
  accumulate_cov_stats <- function(piece) {
    if (!is.list(piece)) return(invisible(NULL))
    for (nm in names(cov_stab_totals)) {
      cur <- suppressWarnings(as.integer(piece[[nm]]))
      if (!is.finite(cur)) cur <- 0L
      cov_stab_totals[[nm]] <<- cov_stab_totals[[nm]] + cur
    }
    invisible(NULL)
  }
  latent_var_clipped_total <- 0L
  sigma_capped_total <- 0L
  sigma_damped_total <- 0L
  latent_var_cap_last <- stab_params$latent_var_cap_abs
  hist_assim <- ndlm_theory_build_hist_pseudo_obs(
    source_obs = source_obs,
    sigma_by_source = sigma_by_source,
    source_names = source_names,
    fallback_y = inputs$y,
    fallback_var = max(as.numeric(sigma_by_source[["usgs"]]), 1e6, na.rm = TRUE)
  )

  for (iter in seq_len(max_iter)) {
    hist_assim <- ndlm_theory_build_hist_pseudo_obs(
      source_obs = source_obs,
      sigma_by_source = sigma_by_source,
      source_names = source_names,
      fallback_y = inputs$y,
      fallback_var = max(as.numeric(sigma_by_source[["usgs"]]), 1e6, na.rm = TRUE)
    )

    fit <- ndlm_theory_kalman_smoother(
      y = hist_assim$y,
      H_mat = H_mat,
      R_vec = hist_assim$R_vec,
      q_diag = q_diag,
      df_mat = df_mat_full,
      m0 = m0,
      C0 = C0,
      backend = constants$kalman_backend,
      stabilization = stab_params
    )
    accumulate_cov_stats(fit$stabilization)

    fitted_mean <- as.numeric(fit$fitted_mean)
    fitted_latent_var_raw <- pmax(vapply(
      seq_len(Tn),
      function(tt) as.numeric(crossprod(H_mat[tt, ], fit$smooth_cov[, , tt] %*% H_mat[tt, ])),
      numeric(1)
    ), 1e-10)
    assim_var <- suppressWarnings(stats::var(hist_assim$y[is.finite(hist_assim$y)], na.rm = TRUE))
    if (!is.finite(assim_var) || assim_var <= 0) {
      assim_var <- suppressWarnings(stats::var(inputs$y[is.finite(inputs$y)], na.rm = TRUE))
    }
    if (!is.finite(assim_var) || assim_var <= 0) {
      assim_var <- 1.0
    }
    latent_var_cap <- max(stab_params$latent_var_cap_abs, stab_params$latent_var_cap_mult * assim_var)
    latent_var_cap_last <- latent_var_cap
    latent_var_clipped_total <- latent_var_clipped_total +
      as.integer(sum(is.finite(fitted_latent_var_raw) & (fitted_latent_var_raw > latent_var_cap)))
    fitted_latent_var <- pmin(fitted_latent_var_raw, latent_var_cap)
    fitted_latent_var <- pmax(fitted_latent_var, 1e-10)

    source_elbo <- rep(0, length(source_names))
    names(source_elbo) <- source_names
    sigma_next <- sigma_by_source
    for (nm in source_names) {
      obs <- as.numeric(source_obs[[nm]])
      ok <- is.finite(obs) & is.finite(fitted_mean) & is.finite(fitted_latent_var)
      n_obs <- sum(ok)

      sigma_shape <- constants$a_sigma + n_obs / 2
      sigma_rate <- constants$b_sigma
      if (n_obs > 0L) {
        resid <- obs[ok] - fitted_mean[ok]
        sigma_rate <- sigma_rate + 0.5 * sum(resid^2 + fitted_latent_var[ok])
      }
      sigma_new_raw <- sigma_rate / max(sigma_shape - 1, 1.01)
      sigma_prev <- suppressWarnings(as.numeric(sigma_by_source[[nm]]))
      if (!is.finite(sigma_prev) || sigma_prev <= 0) sigma_prev <- 1e-6
      if (!is.finite(sigma_new_raw) || sigma_new_raw <= 0) sigma_new_raw <- sigma_prev
      if (sigma_new_raw > stab_params$sigma_upper_cap) {
        sigma_capped_total <- sigma_capped_total + 1L
      }
      sigma_new <- min(max(sigma_new_raw, 1e-6), stab_params$sigma_upper_cap)
      if (stab_params$sigma_update_damping < 1) {
        sigma_damped_total <- sigma_damped_total + 1L
      }
      sigma_new <- stab_params$sigma_update_damping * sigma_new +
        (1 - stab_params$sigma_update_damping) * sigma_prev
      sigma_new <- min(max(sigma_new, 1e-6), stab_params$sigma_upper_cap)
      sigma_next[[nm]] <- sigma_new
      sigma_shape_final[[nm]] <- sigma_shape
      sigma_rate_final[[nm]] <- sigma_rate

      prior_term <- constants$a_sigma * log(constants$b_sigma) -
        lgamma(constants$a_sigma) -
        (constants$a_sigma + 1) * log(sigma_new) -
        constants$b_sigma / sigma_new
      ll_term <- 0
      if (n_obs > 0L) {
        resid <- obs[ok] - fitted_mean[ok]
        ll_term <- -0.5 * sum(log(2 * pi * sigma_new) + (resid^2 + fitted_latent_var[ok]) / sigma_new)
      }
      source_elbo[[nm]] <- ll_term + prior_term
    }
    sigma_by_source <- sigma_next

    seq_sigma[iter, ] <- as.numeric(sigma_by_source[source_names])
    seq_elbo[iter] <- sum(as.numeric(source_elbo))
    seq_scale[iter, ] <- c(
      sigma_by_source[["usgs"]],
      sigma_by_source[["usgs"]],
      sigma_by_source[["nws"]],
      sigma_by_source[["glofas"]],
      w_hist,
      w_fore,
      constants$df_t,
      constants$df_s1,
      constants$df_s2,
      constants$df_s67,
      constants$df_discrep,
      constants$lambda,
      constants$df_trans,
      constants$df_covs
    )
    if (is.finite(prev_elbo) && is.finite(seq_elbo[iter])) {
      crit_elbo <- abs(seq_elbo[iter] - prev_elbo)
      denom <- max(abs(prev_elbo), 1e-12)
      crit_elbo_rel <- crit_elbo / denom
    } else {
      crit_elbo <- Inf
      crit_elbo_rel <- Inf
    }
    prev_elbo <- seq_elbo[iter]
    iterations_completed <- as.integer(iter)

    state_norm_sq <- suppressWarnings(as.numeric(sum(fit$smooth_mean^2, na.rm = TRUE)))
    if (!is.finite(state_norm_sq)) {
      state_norm_sq <- NA_real_
    }
    cat(
      sprintf(
        "[gamsig_progress] family=ndlm_main p0=NA iter=%d elbo=%s crit_elbo=%s crit_elbo_rel=%s sigma_exp=%s sigma_usgs_exp=%s sigma_nws_exp=%s sigma_glofas_exp=%s gamma_exp=NA state_norm_sq=%s w_hist=%s w_fore=%s df_t=%s df_s1=%s df_s2=%s df_s67=%s df_discrep=%s lambda=%s latent_var_cap=%s cov_proj_total=%d sigma_cap_total=%d\n",
        as.integer(iter),
        fmt_iter_num(seq_elbo[iter]),
        fmt_iter_num(crit_elbo),
        fmt_iter_num(crit_elbo_rel),
        fmt_iter_num(sigma_by_source[["usgs"]]),
        fmt_iter_num(sigma_by_source[["usgs"]]),
        fmt_iter_num(sigma_by_source[["nws"]]),
        fmt_iter_num(sigma_by_source[["glofas"]]),
        fmt_iter_num(state_norm_sq),
        fmt_iter_num(w_hist),
        fmt_iter_num(w_fore),
        fmt_iter_num(constants$df_t),
        fmt_iter_num(constants$df_s1),
        fmt_iter_num(constants$df_s2),
        fmt_iter_num(constants$df_s67),
        fmt_iter_num(constants$df_discrep),
        fmt_iter_num(constants$lambda),
        fmt_iter_num(latent_var_cap_last),
        as.integer(cov_stab_totals$cov_projected),
        as.integer(sigma_capped_total)
      )
    )

    if (ndlm_theory_has_converged(
      iter = iter,
      min_total_iters = min_total_iters,
      crit_elbo = crit_elbo,
      crit_elbo_rel = crit_elbo_rel,
      elbo_tol = elbo_tol,
      elbo_rel_tol = elbo_rel_tol
    )) {
      converged <- TRUE
      convergence_reason <- "all_convergence_criteria_met"
      break
    }
  }

  if (is.null(fit)) {
    stop("ndlm theory VB failed to initialize", call. = FALSE)
  }
  if (iterations_completed < 1L) {
    iterations_completed <- max_iter
  }
  seq_sigma <- seq_sigma[seq_len(iterations_completed), , drop = FALSE]
  seq_elbo <- seq_elbo[seq_len(iterations_completed)]
  seq_scale <- seq_scale[seq_len(iterations_completed), , drop = FALSE]
  smooth_cov_stab <- ndlm_theory_stabilize_cov_array(fit$smooth_cov, constants = constants)
  fit$smooth_cov <- smooth_cov_stab$cov
  accumulate_cov_stats(smooth_cov_stab$stats)

  exps <- rbind(fit$fitted_mean, fit$fitted_mean)
  rownames(exps) <- c("median", "mean")
  vars <- rbind(fit$fitted_var, fit$fitted_var)
  exps2 <- exps^2 + vars

  pick_fit_vec <- function(name, fallback) {
    val <- fit[[name]]
    if (is.null(val)) {
      return(as.numeric(fallback))
    }
    out <- as.numeric(val)
    if (length(out) != length(fallback)) {
      return(as.numeric(fallback))
    }
    out
  }
  y_obs <- as.numeric(inputs$y)
  y_pred <- pick_fit_vec("predicted_mean", fit$fitted_mean)
  y_filt <- pick_fit_vec("filtered_mean", fit$fitted_mean)
  y_smooth <- pick_fit_vec("smoothed_mean", fit$fitted_mean)
  v_pred <- pmax(pick_fit_vec("predicted_var", fit$fitted_var), 1e-10)
  v_filt <- pmax(pick_fit_vec("filtered_var", fit$fitted_var), 1e-10)
  v_smooth <- pmax(pick_fit_vec("smoothed_var", fit$fitted_var), 1e-10)
  fit_diagnostics <- list(
    y_observed = y_obs,
    y_assim_hist_pseudo = hist_assim$y,
    R_assim_hist_pseudo = hist_assim$R_vec,
    n_sources_assim_hist = hist_assim$n_sources,
    n_sources_assim_hist_mean = mean(hist_assim$n_sources),
    y_predicted_one_step = y_pred,
    y_filtered = y_filt,
    y_smoothed = y_smooth,
    var_predicted_one_step = v_pred,
    var_filtered = v_filt,
    var_smoothed = v_smooth,
    residual_one_step = y_obs - y_pred,
    residual_filtered = y_obs - y_filt,
    residual_smoothed = y_obs - y_smooth,
    residual_source_usgs = as.numeric(source_obs$usgs) - y_smooth,
    residual_source_nws = as.numeric(source_obs$nws) - y_smooth,
    residual_source_glofas = as.numeric(source_obs$glofas) - y_smooth,
    latent_var_cap_last = latent_var_cap_last,
    latent_var_clipped_total = as.integer(latent_var_clipped_total)
  )

  nws_std <- ndlm_theory_standardize(inputs$forecast$nws)
  glofas_std <- ndlm_theory_standardize(inputs$forecast$glofas)
  keep_transfer_forecast <- identical(
    tolower(trimws(as.character(constants$forecast_transfer_mode))),
    "keep"
  )

  base_hist <- fit$smooth_mean[8:14, Tn]
  sm_ens_1 <- matrix(0, nrow = 7L, ncol = K_overlap)
  sm_ens_1[1, ] <- nws_std[seq_len(K_overlap)]
  sm_ens_1[2, ] <- glofas_std[seq_len(K_overlap)]
  if (K_overlap > 0L && isTRUE(keep_transfer_forecast)) {
    decay_1 <- matrix(constants$lambda ^ (seq_len(K_overlap) - 1L), nrow = 1L, ncol = K_overlap)
    sm_ens_1[3:7, ] <- matrix(base_hist[3:7], nrow = 5L, ncol = K_overlap) * matrix(rep(decay_1, 5L), nrow = 5L)
  }

  sm_ens_2 <- matrix(0, nrow = 7L, ncol = K_tail)
  bridge_value <- 0
  inactive_row <- integer(0)
  if (K_tail > 0L) {
    if (identical(ragged$extension_source, "nws")) {
      tail_idx <- seq.int(K_overlap + 1L, ragged$K_vec[["nws"]])
      sm_ens_2[1, ] <- nws_std[tail_idx]
      bridge_value <- as.numeric(glofas_std[K_overlap])
      inactive_row <- 2L
    } else {
      tail_idx <- seq.int(K_overlap + 1L, ragged$K_vec[["glofas"]])
      sm_ens_2[1, ] <- glofas_std[tail_idx]
      bridge_value <- as.numeric(nws_std[K_overlap])
      inactive_row <- 2L
    }
    if (!is.finite(bridge_value)) bridge_value <- 0
    sm_ens_2[2, ] <- rep(bridge_value, K_tail)
    if (isTRUE(keep_transfer_forecast)) {
      decay_2 <- matrix(constants$lambda ^ (seq.int(K_overlap + 1L, K_max) - 1L), nrow = 1L, ncol = K_tail)
      sm_ens_2[3:7, ] <- matrix(base_hist[3:7], nrow = 5L, ncol = K_tail) * matrix(rep(decay_2, 5L), nrow = 5L)
    }
  }

  base_fore_cov_raw <- fit$smooth_cov[8:14, 8:14, Tn, drop = TRUE]
  base_fore_cov_stab <- ndlm_theory_stabilize_covariance_local(base_fore_cov_raw, constants = constants)
  accumulate_cov_stats(base_fore_cov_stab$stats)
  base_fore_cov <- base_fore_cov_stab$cov
  transfer_inactive_rows <- if (isTRUE(keep_transfer_forecast)) integer(0) else 3:7
  sC_ens_1 <- ndlm_theory_alloc_segment_cov(
    k_len = K_overlap,
    constants = constants,
    base_cov = base_fore_cov,
    inactive_row = transfer_inactive_rows,
    start_k = 1L
  )
  accumulate_cov_stats(attr(sC_ens_1, "stabilization_stats"))
  sC_ens_1_post <- ndlm_theory_stabilize_cov_array(sC_ens_1, constants = constants)
  sC_ens_1 <- sC_ens_1_post$cov
  accumulate_cov_stats(sC_ens_1_post$stats)
  inactive_row_2 <- unique(c(inactive_row, transfer_inactive_rows))
  sC_ens_2 <- ndlm_theory_alloc_segment_cov(
    k_len = K_tail,
    constants = constants,
    base_cov = if (K_overlap > 0L) sC_ens_1[, , K_overlap, drop = TRUE] else base_fore_cov,
    inactive_row = inactive_row_2,
    start_k = K_overlap + 1L
  )
  accumulate_cov_stats(attr(sC_ens_2, "stabilization_stats"))
  sC_ens_2_post <- ndlm_theory_stabilize_cov_array(sC_ens_2, constants = constants)
  sC_ens_2 <- sC_ens_2_post$cov
  accumulate_cov_stats(sC_ens_2_post$stats)
  smooth_cov_final <- ndlm_theory_stabilize_cov_array(fit$smooth_cov, constants = constants)
  fit$smooth_cov <- smooth_cov_final$cov
  accumulate_cov_stats(smooth_cov_final$stats)
  sC_ens_1_final <- ndlm_theory_stabilize_cov_array(sC_ens_1, constants = constants)
  sC_ens_1 <- sC_ens_1_final$cov
  accumulate_cov_stats(sC_ens_1_final$stats)
  sC_ens_2_final <- ndlm_theory_stabilize_cov_array(sC_ens_2, constants = constants)
  sC_ens_2 <- sC_ens_2_final$cov
  accumulate_cov_stats(sC_ens_2_final$stats)
  cov_diag <- ndlm_theory_collect_covariance_diagnostics(
    fit_sC = fit$smooth_cov,
    sC_ens_1 = sC_ens_1,
    sC_ens_2 = sC_ens_2
  )

  samp_theta_retro <- ndlm_theory_state_draws(
    sm = fit$smooth_mean,
    sC = fit$smooth_cov,
    n_draws = constants$n_draws,
    seed = constants$seed + 11L
  )

  set.seed(constants$seed + 22L)
  samp_theta_ens <- vector("list", 2)
  for (j in 1:2) {
    mu <- if (j == 1) sm_ens_1 else sm_ens_2
    Sig <- if (j == 1) sC_ens_1 else sC_ens_2
    k_j <- suppressWarnings(as.integer(ncol(mu)))
    if (!is.finite(k_j) || k_j < 0L) k_j <- 0L
    arr <- array(0, dim = c(7L, k_j, constants$n_draws))
    for (k in seq_len(k_j)) {
      L <- ndlm_theory_safe_chol(Sig[, , k])
      Z <- matrix(stats::rnorm(7 * constants$n_draws), nrow = 7)
      arr[, k, ] <- mu[, k] + L %*% Z
    }
    samp_theta_ens[[j]] <- list(samp_theta = arr)
  }

  set.seed(constants$seed + 33L)
  samp_sigma <- matrix(NA_real_, nrow = length(source_names), ncol = constants$n_draws)
  rownames(samp_sigma) <- source_names
  for (j in seq_along(source_names)) {
    nm <- source_names[[j]]
    shp <- sigma_shape_final[[nm]]
    rte <- sigma_rate_final[[nm]]
    if (!is.finite(shp) || shp <= 0) shp <- constants$a_sigma
    if (!is.finite(rte) || rte <= 0) rte <- constants$b_sigma
    samp_sigma[j, ] <- 1 / stats::rgamma(constants$n_draws, shape = shp, rate = rte)
  }

  standard_forecast_errors <- rep(NA_real_, K_max)
  standard_forecast_errors[seq_len(K_overlap)] <- inputs$forecast$nws[seq_len(K_overlap)] - inputs$forecast$glofas[seq_len(K_overlap)]
  if (K_tail > 0L) {
    if (identical(ragged$extension_source, "nws")) {
      tail_idx <- seq.int(K_overlap + 1L, ragged$K_vec[["nws"]])
      bridge_raw <- as.numeric(inputs$forecast$glofas[K_overlap])
      if (!is.finite(bridge_raw)) bridge_raw <- 0
      standard_forecast_errors[seq.int(K_overlap + 1L, K_max)] <- inputs$forecast$nws[tail_idx] - bridge_raw
    } else {
      tail_idx <- seq.int(K_overlap + 1L, ragged$K_vec[["glofas"]])
      bridge_raw <- as.numeric(inputs$forecast$nws[K_overlap])
      if (!is.finite(bridge_raw)) bridge_raw <- 0
      standard_forecast_errors[seq.int(K_overlap + 1L, K_max)] <- bridge_raw - inputs$forecast$glofas[tail_idx]
    }
  }
  standard_forecast_errors[!is.finite(standard_forecast_errors)] <- 0
  standard_forecast_errors <- matrix(standard_forecast_errors, nrow = 1L)

  active_set_by_lead <- data.frame(
    lead = seq_len(K_max),
    active_nws = as.integer(seq_len(K_max) <= ragged$K_vec[["nws"]]),
    active_glofas = as.integer(seq_len(K_max) <= ragged$K_vec[["glofas"]]),
    active_count = as.integer(vapply(ragged$active_sources, length, integer(1))),
    stringsAsFactors = FALSE
  )
  state_dim_by_lead <- data.frame(
    lead = seq_len(K_max),
    state_dim = as.integer(7L * active_set_by_lead$active_count),
    stringsAsFactors = FALSE
  )

  new_theta <- list(
    sm = fit$smooth_mean,
    sC = fit$smooth_cov,
    exps = exps,
    exps2 = exps2,
    vars = vars,
    sm_ens = list(sm_ens_1, sm_ens_2),
    sC_ens = list(sC_ens_1, sC_ens_2),
    standard_forecast_errors = standard_forecast_errors,
    forecast_horizon = list(
      K_vec = ragged$K_vec,
      K_overlap = ragged$K_overlap,
      K_max = ragged$K_max,
      segment_lengths = ragged$segment_lengths,
      extension_source = ragged$extension_source,
      bridge_source = ragged$bridge_source,
      forecast_transfer_mode = if (isTRUE(keep_transfer_forecast)) "keep" else "drop",
      transfer_active_forecast_window = isTRUE(keep_transfer_forecast)
    )
  )

  # Last-mile export hardening: stabilize exactly the covariance arrays that
  # will be persisted, then compute diagnostics from those same arrays.
  smooth_cov_export <- ndlm_theory_stabilize_cov_array(new_theta$sC, constants = constants)
  new_theta$sC <- smooth_cov_export$cov
  accumulate_cov_stats(smooth_cov_export$stats)

  sC_ens_1_export <- ndlm_theory_stabilize_cov_array(new_theta$sC_ens[[1L]], constants = constants)
  new_theta$sC_ens[[1L]] <- sC_ens_1_export$cov
  accumulate_cov_stats(sC_ens_1_export$stats)

  sC_ens_2_export <- ndlm_theory_stabilize_cov_array(new_theta$sC_ens[[2L]], constants = constants)
  new_theta$sC_ens[[2L]] <- sC_ens_2_export$cov
  accumulate_cov_stats(sC_ens_2_export$stats)

  cov_diag <- ndlm_theory_collect_covariance_diagnostics(
    fit_sC = new_theta$sC,
    sC_ens_1 = new_theta$sC_ens[[1L]],
    sC_ens_2 = new_theta$sC_ens[[2L]]
  )

  list(
    new_theta = new_theta,
    samp_theta = list(samp_theta = samp_theta_retro),
    samp_theta_ens = samp_theta_ens,
    samp_sigma = samp_sigma,
    seq_sigma = seq_sigma,
    seq_scale = seq_scale,
    seq_elbo = seq_elbo,
    delta = c(diff(seq_elbo), 0),
    iterations_completed = iterations_completed,
    max_iter = max_iter,
    converged = converged,
    convergence_reason = convergence_reason,
    convergence_metrics = c(
      crit_elbo = suppressWarnings(as.numeric(crit_elbo)),
      crit_elbo_rel = suppressWarnings(as.numeric(crit_elbo_rel)),
      elbo_tol = elbo_tol,
      elbo_rel_tol = elbo_rel_tol
    ),
    sigma = as.numeric(sigma_by_source[["usgs"]]),
    sigma_by_source = sigma_by_source[source_names],
    sigma_mean = mean(as.numeric(sigma_by_source[source_names])),
    w_hist = w_hist,
    w_fore = w_fore,
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
    K = K_max,
    K_overlap = K_overlap,
    K_max = K_max,
    K_vec = ragged$K_vec,
    segment_lengths = ragged$segment_lengths,
    extension_source = ragged$extension_source,
    bridge_source = ragged$bridge_source,
    forecast_transfer_mode = if (isTRUE(keep_transfer_forecast)) "keep" else "drop",
    transfer_active_forecast_window = isTRUE(keep_transfer_forecast),
    active_set_by_lead = active_set_by_lead,
    state_dim_by_lead = state_dim_by_lead,
    covariance_diagnostics = cov_diag,
    fit_diagnostics = fit_diagnostics,
    stabilization = list(
      cov_calls = as.integer(cov_stab_totals$calls),
      cov_projected = as.integer(cov_stab_totals$cov_projected),
      cov_floor_clipped = as.integer(cov_stab_totals$cov_floor_clipped),
      cov_cap_clipped = as.integer(cov_stab_totals$cov_cap_clipped),
      cov_nonfinite_inputs = as.integer(cov_stab_totals$cov_nonfinite_inputs),
      sigma_upper_cap = as.numeric(stab_params$sigma_upper_cap),
      sigma_update_damping = as.numeric(stab_params$sigma_update_damping),
      sigma_capped_total = as.integer(sigma_capped_total),
      sigma_damped_total = as.integer(sigma_damped_total),
      latent_var_cap_abs = as.numeric(stab_params$latent_var_cap_abs),
      latent_var_cap_mult = as.numeric(stab_params$latent_var_cap_mult),
      latent_var_cap_last = as.numeric(latent_var_cap_last),
      latent_var_clipped_total = as.integer(latent_var_clipped_total)
    ),
    K_cap = inputs$forecast$K_cap,
    nws_len = inputs$forecast$nws_len,
    glofas_len = inputs$forecast$glofas_len,
    T = Tn
  )
}
