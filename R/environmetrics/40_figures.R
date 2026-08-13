###############################################################################
# Figures and plotting
# Inputs:
#   - Model outputs and derived objects from prior modules
# Outputs:
#   - PNG figures (redirected by runner into run output folder)
# Dependencies:
#   - ggplot2, patchwork, dplyr, tidyr, etc.
# NOTE:
#   - Many save calls use absolute canonical paths; runner redirects to OUT_DIR.
###############################################################################

# Guard plotting-only failures from mismatched x/y lengths. This is a no-op
# when lengths already match and only trims mismatched pairs to the common size.
safe_lines <- function(x, y = NULL, ...) {
  if (missing(y) || is.null(y)) {
    return(graphics::lines(x, ...))
  }
  if (is.object(x) || is.object(y)) {
    return(graphics::lines(x, y, ...))
  }
  nx <- length(x)
  ny <- length(y)
  n <- min(nx, ny)
  if (!is.finite(n) || n <= 0L) {
    return(invisible(NULL))
  }
  if (nx != ny) {
    warning(sprintf("safe_lines trimmed mismatched lengths x=%d y=%d to %d", nx, ny, n), call. = FALSE)
  }
  graphics::lines(x[seq_len(n)], y[seq_len(n)], ...)
}
lines <- safe_lines

if (!exists("CUTOFF_DATE", inherits = TRUE)) {
  CUTOFF_DATE <- as.Date("2022-12-25")
}
if (!exists("FORECAST_START_DATE", inherits = TRUE)) {
  FORECAST_START_DATE <- CUTOFF_DATE + 1L
}
if (!exists("PLOT_START_DATE", inherits = TRUE)) {
  PLOT_START_DATE <- CUTOFF_DATE - 18L
}
if (!exists("PLOT_END_DATE", inherits = TRUE)) {
  PLOT_END_DATE <- CUTOFF_DATE + 28L
}
cutoff_label_short <- format(as.Date(CUTOFF_DATE), "%b %d")
special_event_date <- suppressWarnings(as.Date(Sys.getenv("UNIFIED_FORECAST_EVENT_DATE", "")))
if (length(special_event_date) == 0L || is.na(special_event_date[[1L]])) {
  special_event_date <- as.Date(NA)
}
special_event_label <- Sys.getenv("UNIFIED_FORECAST_EVENT_LABEL", "")

daily_dates_for_n <- function(start_date, n_days, context = "dates") {
  start_date <- as.Date(start_date)
  if (length(start_date) != 1L || !is.finite(start_date)) {
    stop(sprintf("[%s] start_date must be a valid scalar Date.", context), call. = FALSE)
  }
  n_use <- as.integer(n_days[[1]])
  if (!is.finite(n_use) || n_use <= 0L) {
    stop(
      sprintf("[%s] n_days must be a positive finite integer, got '%s'.", context, as.character(n_days[[1]])),
      call. = FALSE
    )
  }
  seq(start_date, by = "1 day", length.out = n_use)
}

daily_dates_for_matrix_rows <- function(mat, start_date, context = "dates.rows") {
  if (is.null(dim(mat)) || length(dim(mat)) != 2L) {
    stop(sprintf("[%s] expected a 2D object for row-based date construction.", context), call. = FALSE)
  }
  daily_dates_for_n(start_date = start_date, n_days = nrow(mat), context = context)
}

daily_dates_for_matrix_cols <- function(mat, start_date, context = "dates.cols") {
  if (is.null(dim(mat)) || length(dim(mat)) != 2L) {
    stop(sprintf("[%s] expected a 2D object for column-based date construction.", context), call. = FALSE)
  }
  daily_dates_for_n(start_date = start_date, n_days = ncol(mat), context = context)
}

ndlm_warn_once <- local({
  warned <- new.env(parent = emptyenv())
  function(key, message_text) {
    if (!exists(key, envir = warned, inherits = FALSE)) {
      assign(key, TRUE, envir = warned)
      warning(message_text, call. = FALSE)
    }
    invisible(NULL)
  }
})

resolve_post_effective_n_samp <- function(n_available, TT, context = "post.n_samp") {
  n_avail <- as.integer(n_available[[1L]])
  if (!is.finite(n_avail) || n_avail <= 0L) {
    stop(sprintf("[%s] n_available must be a positive finite integer.", context), call. = FALSE)
  }

  explicit_cap <- suppressWarnings(as.integer(Sys.getenv("UNIFIED_POST_NSAMP_CAP", "")))
  if (!is.finite(explicit_cap) || explicit_cap <= 0L) {
    explicit_cap <- NA_integer_
  }

  synth_bytes_cap <- suppressWarnings(as.numeric(Sys.getenv("UNIFIED_POST_SYNTH_MAX_BYTES", "6.0e8")))
  if (!is.finite(synth_bytes_cap) || synth_bytes_cap <= 0) {
    synth_bytes_cap <- 6.0e8
  }
  tt_use <- as.double(max(1L, as.integer(TT[[1L]])))
  # y_reps / y_reps_f arrays are 7 x n.samp x horizon doubles.
  auto_cap <- as.integer(floor(synth_bytes_cap / (7.0 * tt_use * 8.0)))
  if (!is.finite(auto_cap) || auto_cap <= 0L) {
    auto_cap <- 64L
  }
  auto_cap <- min(n_avail, max(64L, auto_cap))

  n_eff <- min(n_avail, auto_cap)
  if (!is.na(explicit_cap)) {
    n_eff <- min(n_eff, explicit_cap)
  }
  n_eff <- max(1L, as.integer(n_eff))

  if (n_eff < n_avail) {
    warning(
      sprintf(
        "[POST_NSAMP_CAP] %s reduced n.samp from %d to %d (UNIFIED_POST_NSAMP_CAP=%s, UNIFIED_POST_SYNTH_MAX_BYTES=%.0f).",
        context,
        n_avail,
        n_eff,
        ifelse(is.na(explicit_cap), "unset", as.character(explicit_cap)),
        synth_bytes_cap
      ),
      call. = FALSE
    )
  }
  n_eff
}

cap_sample_rows <- function(mat, n_keep) {
  if (!is.matrix(mat) || n_keep >= nrow(mat)) {
    return(mat)
  }
  mat[seq_len(n_keep), , drop = FALSE]
}

align_sample_time_matrix <- function(mat, n_samp, horizon, context) {
  if (!is.matrix(mat)) {
    stop(sprintf("[%s] expected matrix input, got %s.", context, class(mat)[1L]), call. = FALSE)
  }
  nr <- nrow(mat)
  nc <- ncol(mat)
  ns <- as.integer(n_samp)
  hz <- as.integer(horizon)

  if (nr == ns && nc >= hz) {
    return(mat[, seq_len(hz), drop = FALSE])
  }
  if (nc == ns && nr >= hz) {
    return(t(mat[seq_len(hz), , drop = FALSE]))
  }
  if (nr >= ns && nc == hz) {
    return(mat[seq_len(ns), , drop = FALSE])
  }
  if (nc >= ns && nr == hz) {
    return(t(mat[, seq_len(ns), drop = FALSE]))
  }

  stop(
    sprintf(
      "[%s] unable to align matrix with dims %dx%d to expected sample/time dims %dx%d.",
      context, nr, nc, ns, hz
    ),
    call. = FALSE
  )
}

agg_disc_warn_once <- local({
  warned <- new.env(parent = emptyenv())
  function(key, message_text) {
    if (!exists(key, envir = warned, inherits = FALSE)) {
      assign(key, TRUE, envir = warned)
      warning(message_text, call. = FALSE)
    }
    invisible(NULL)
  }
})

agg_disc_contract_rows <- list()

resolve_agg_discrep_contract <- function(df, obs, preferred_ylim, contract_key, title) {
  if (!is.data.frame(obs) || !("Discrepancy" %in% names(obs))) {
    stop(sprintf("[%s_OBS_SCHEMA] obs must contain Discrepancy column.", contract_key))
  }
  contract <- resolve_agg_discrep_ylim(
    obs = obs$Discrepancy,
    fitted_df = df,
    preferred_ylim = preferred_ylim,
    context = contract_key
  )

  agg_disc_contract_rows[[length(agg_disc_contract_rows) + 1L]] <<- data.frame(
    panel_id = as.character(contract_key),
    title = as.character(title),
    preferred_min = contract$preferred_min,
    preferred_max = contract$preferred_max,
    used_min = contract$ylim[[1L]],
    used_max = contract$ylim[[2L]],
    mode = as.character(contract$mode),
    preferred_inrange_share = contract$preferred_inrange_share,
    fitted_finite_n = contract$fitted_finite_n,
    obs_finite_n = contract$obs_finite_n,
    combined_min = contract$combined_min,
    combined_max = contract$combined_max,
    stringsAsFactors = FALSE
  )

  if (!identical(contract$mode, "preferred")) {
    agg_disc_warn_once(
      paste0("ylim:", contract_key),
      sprintf(
        "[AGG_DISC_YLIM_EXPANDED] %s expanded ylim from [%s, %s] to [%s, %s] (preferred fitted in-range share=%.3f).",
        contract_key,
        format(contract$preferred_min, digits = 6),
        format(contract$preferred_max, digits = 6),
        format(contract$ylim[[1L]], digits = 6),
        format(contract$ylim[[2L]], digits = 6),
        ifelse(is.finite(contract$preferred_inrange_share), contract$preferred_inrange_share, NA_real_)
      )
    )
  }

  contract$ylim
}

ndlm_state_rows_for_projection <- function(sm_mat, F_constant, context = "ndlm") {
  n_state <- nrow(sm_mat)
  max_rows <- length(F_constant)
  if (!is.finite(n_state) || n_state <= 0L || max_rows <= 0L) {
    return(integer(0))
  }
  if (n_state >= 14L && max_rows >= 7L) {
    return(8:14)
  }
  use_rows <- seq_len(min(n_state, max_rows))
  ndlm_warn_once(
    paste0("rows:", context),
    sprintf(
      "%s has only %d state rows; using rows %s for discrepancy projection.",
      context,
      as.integer(n_state),
      paste(use_rows, collapse = ",")
    )
  )
  use_rows
}

ndlm_project_discrepancy_from_sm <- function(sm_mat, F_constant, context = "ndlm") {
  if (!is.numeric(sm_mat) || is.null(dim(sm_mat)) || length(dim(sm_mat)) != 2L) {
    ndlm_warn_once(
      paste0("sm_shape:", context),
      sprintf("%s has invalid sm_ens entry shape; expected matrix.", context)
    )
    return(numeric(0))
  }
  row_idx <- ndlm_state_rows_for_projection(sm_mat, F_constant, context = context)
  if (length(row_idx) == 0L) {
    return(numeric(0))
  }
  f_use <- as.numeric(F_constant)[seq_len(length(row_idx))]
  as.vector(matrix(f_use, nrow = 1L) %*% sm_mat[row_idx, , drop = FALSE])
}

ndlm_discrepancy_pair <- function(theta_obj, F_constant, target_len, context = "ndlm") {
  sm_ens <- theta_obj$sm_ens
  if (!is.list(sm_ens) || length(sm_ens) < 2L) {
    ndlm_warn_once(
      paste0("pair:", context),
      sprintf("%s is missing a two-segment sm_ens list; returning NA discrepancy.", context)
    )
    return(rep(NA_real_, as.integer(target_len)))
  }
  d1 <- ndlm_project_discrepancy_from_sm(sm_ens[[1]], F_constant, context = paste0(context, ".seg1"))
  d2 <- ndlm_project_discrepancy_from_sm(sm_ens[[2]], F_constant, context = paste0(context, ".seg2"))
  align_to_len(c(d1, d2), target_len, sprintf("%s discrepancy", context))
}

gaussian_projection_mean_sd <- function(Ft, Mu, Sigma, context = "projection") {
  if (!is.numeric(Ft) || !is.numeric(Mu) || !is.numeric(Sigma) ||
      is.null(dim(Sigma)) || length(dim(Sigma)) != 2L) {
    ndlm_warn_once(
      paste0("projection_shape:", context),
      sprintf("%s has invalid Ft/Mu/Sigma shapes; returning NA moments.", context)
    )
    return(c(mean = NA_real_, sd = NA_real_))
  }

  p_use <- min(length(Ft), length(Mu), nrow(Sigma), ncol(Sigma))
  if (!is.finite(p_use) || p_use <= 0L) {
    ndlm_warn_once(
      paste0("projection_empty:", context),
      sprintf("%s has empty overlap across Ft/Mu/Sigma; returning NA moments.", context)
    )
    return(c(mean = NA_real_, sd = NA_real_))
  }

  if (p_use < length(Ft) || p_use < length(Mu) || p_use < nrow(Sigma) || p_use < ncol(Sigma)) {
    ndlm_warn_once(
      paste0("projection_trim:", context),
      sprintf(
        "%s trimmed Ft/Mu/Sigma dimensions to %d for compatible projection.",
        context,
        as.integer(p_use)
      )
    )
  }

  Ft_use <- matrix(as.numeric(Ft)[seq_len(p_use)], ncol = 1L)
  Mu_use <- as.numeric(Mu)[seq_len(p_use)]
  Sigma_use <- as.matrix(Sigma)[seq_len(p_use), seq_len(p_use), drop = FALSE]
  mean_val <- as.numeric(crossprod(Ft_use, Mu_use))
  var_val <- as.numeric(t(Ft_use) %*% Sigma_use %*% Ft_use)
  if (!is.finite(var_val)) {
    return(c(mean = mean_val, sd = NA_real_))
  }
  c(mean = mean_val, sd = sqrt(max(var_val, 0)))
}

profile_section("figures.elbo_traces", {
  png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
  par(mfrow = c(1, 8), mar = c(2, 2, 2, 1), oma = c(0, 0, 3, 0))

  l <- -2500
  u <- -2300
  a <- c(seq.elbo_50_NDLM_synth_DISC)
  a[1:1]=NaN
  plot.ts(a, main = "NDLM", xlab = "Iteration", ylab = "ELBO", lwd=2, ylim = c(l,u))
  a <- c(seq.elbo_5_exAL_synth_DISC)
  a[1:1]=NaN
  plot.ts(a, main = "exAL05", xlab = "Iteration", ylab = "ELBO", lwd=2, ylim = c(l,u))
  a <- c(seq.elbo_20_exAL_synth_DISC)
  a[1:1]=NaN
  plot.ts(a, main = "exAL20", xlab = "Iteration", ylab = "ELBO", lwd=2, ylim = c(l,u))
  a <- c(seq.elbo_35_exAL_synth_DISC)
  a[1:1]=NaN
  plot.ts(a, main = "exAL35", xlab = "Iteration", ylab = "ELBO", lwd=2, ylim = c(l,u))
  a <- c(seq.elbo_50_exAL_synth_DISC)
  a[1:1]=NaN
  plot.ts(a, main = "exAL50", xlab = "Iteration", ylab = "ELBO", lwd=2, ylim = c(l,u))
  a <- c(seq.elbo_65_exAL_synth_DISC)
  a[1:1]=NaN
  plot.ts(a, main = "exAL65", xlab = "Iteration", ylab = "ELBO", lwd=2, ylim = c(l,u))
  a <- c(seq.elbo_80_exAL_synth_DISC)
  a[1:1]=NaN
  plot.ts(a, main = "exAL80", xlab = "Iteration", ylab = "ELBO", lwd=2, ylim = c(l,u))
  a <- c(seq.elbo_95_exAL_synth_DISC)
  a[1:1]=NaN
  plot.ts(a, main = "exAL95", xlab = "Iteration", ylab = "ELBO", lwd=2, ylim = c(l,u))
  mtext("ELBO traces", side = 3, outer = TRUE, line = 0, cex = 0.8)

  dev.off()
})


profile_detail_section("figures.build_xbs_discrep", {
  profile_section("figures.build_xbs_discrep", {
    p <- 7

    next_idx_block <- function(prev_idx, block_len) {
      block_len <- as.integer(block_len[[1]])
      start <- if (length(prev_idx) == 0L) 0L else as.integer(prev_idx[[length(prev_idx)]])
      if (is.na(block_len) || block_len <= 0L) {
        return(integer(0))
      }
      seq_len(block_len) + start
    }

    ks <- -diff(c(ranges,0))
    xbs <- array(NA_real_, c(7,ranges[1],n.samp))
    xbs_ndlm <- array(NA_real_, c(1,ranges[1],n.samp))
    ndlm_direct_mean_draws <- NULL
    if (exists("new.theta.out_50_NDLM_synth_DISC", inherits = TRUE)) {
      ndlm_obj_direct <- get("new.theta.out_50_NDLM_synth_DISC", inherits = TRUE)
      if (is.list(ndlm_obj_direct) &&
          is.matrix(ndlm_obj_direct$forecast_mean_draws_loglog1p) &&
          is.numeric(ndlm_obj_direct$forecast_mean_draws_loglog1p) &&
          nrow(ndlm_obj_direct$forecast_mean_draws_loglog1p) > 1L &&
          ncol(ndlm_obj_direct$forecast_mean_draws_loglog1p) > 0L &&
          all(is.finite(ndlm_obj_direct$forecast_mean_draws_loglog1p))) {
        ndlm_direct_mean_draws <- as.matrix(ndlm_obj_direct$forecast_mean_draws_loglog1p)
        xbs_ndlm <- array(NA_real_, c(1L, ncol(ndlm_direct_mean_draws), nrow(ndlm_direct_mean_draws)))
        xbs_ndlm[1, , ] <- t(ndlm_direct_mean_draws)
      }
    }

	    xb_discrep1 <- array(NA_real_ , c(7,TT,n.samp))
	    xb_discrep2 <- array(NA_real_ , c(7,TT,n.samp))

	    F_constant_disc <- FF[1:7,1,1]
	    forecast_transfer_mode <- tolower(trimws(Sys.getenv("UNIFIED_MULTIVAR_FORECAST_TRANSFER_MODE", "drop")))
	    if (!forecast_transfer_mode %in% c("drop", "keep")) {
	      forecast_transfer_mode <- "drop"
	    }
	    ppx_local <- if (exists("ppx", inherits = TRUE)) as.integer(ppx) else 0L
	    use_transfer_forecast_projection <- isTRUE(use_covariates) &&
	      identical(forecast_transfer_mode, "keep") &&
	      is.finite(ppx_local) &&
	      ppx_local > 0L

	    forecast_core_dim <- function(seg_id) {
	      as.integer(p * (J - as.integer(seg_id) + 2L))
	    }

	    build_usgs_projection_weights <- function(ff_seg, state_len, seg_id, context = "forecast_projection") {
	      state_len <- as.integer(state_len)
	      if (!is.finite(state_len) || state_len <= 0L) {
	        stop(sprintf("[%s_STATE_LEN] invalid state_len=%s.", context, as.character(state_len)), call. = FALSE)
	      }
	      if (!is.matrix(ff_seg) || ncol(ff_seg) < 1L) {
	        stop(sprintf("[%s_FF_SHAPE] expected FF segment matrix with at least one column.", context), call. = FALSE)
	      }

	      ff_n <- nrow(ff_seg)
	      weights <- rep(0, state_len)
	      base_len <- min(p, ff_n, state_len)
	      if (base_len > 0L) {
	        base_vals <- as.numeric(ff_seg[seq_len(base_len), 1, drop = TRUE])
	        base_vals[!is.finite(base_vals)] <- 0
	        weights[seq_len(base_len)] <- base_vals
	      }

	      if (isTRUE(use_transfer_forecast_projection)) {
	        core_dim <- forecast_core_dim(seg_id)
	        zeta_idx <- core_dim + 1L
	        if (zeta_idx <= ff_n && zeta_idx <= state_len) {
	          zeta_w <- as.numeric(ff_seg[zeta_idx, 1, drop = TRUE])
	          if (!is.finite(zeta_w)) zeta_w <- 0
	          weights[zeta_idx] <- zeta_w
	        } else {
	          warning(
	            sprintf(
	              "[%s_TRANSFER_OOB] zeta index %d is outside FF/state dims (ff_n=%d, state_len=%d) for seg=%d.",
	              context, as.integer(zeta_idx), as.integer(ff_n), as.integer(state_len), as.integer(seg_id)
	            ),
	            call. = FALSE
	          )
	        }
	      }

	      weights
	    }

	    project_state_gaussian <- function(Mu, Sigma, ff_seg, seg_id, eps_reg = 0, context = "forecast_projection") {
	      if (!is.numeric(Mu)) {
	        stop(sprintf("[%s_MU] expected numeric Mu vector.", context), call. = FALSE)
	      }
	      if (!is.numeric(Sigma) || is.null(dim(Sigma)) || length(dim(Sigma)) != 2L) {
	        stop(sprintf("[%s_SIGMA] expected 2D numeric Sigma matrix.", context), call. = FALSE)
	      }
	      if (nrow(Sigma) < length(Mu) || ncol(Sigma) < length(Mu)) {
	        stop(
	          sprintf(
	            "[%s_SIGMA_DIM] Sigma dims %dx%d do not cover Mu length %d.",
	            context, as.integer(nrow(Sigma)), as.integer(ncol(Sigma)), as.integer(length(Mu))
	          ),
	          call. = FALSE
	        )
	      }

	      w <- build_usgs_projection_weights(ff_seg, state_len = length(Mu), seg_id = seg_id, context = context)
	      idx_use <- which(abs(w) > 0)
	      if (length(idx_use) == 0L) {
	        return(c(mean = NA_real_, sd = NA_real_))
	      }

	      Mu_use <- as.numeric(Mu[idx_use])
	      Mu_use[!is.finite(Mu_use)] <- 0
	      S_use <- as.matrix(Sigma[idx_use, idx_use, drop = FALSE])
	      S_use[!is.finite(S_use)] <- 0
	      if (is.finite(eps_reg) && eps_reg > 0) {
	        S_use <- S_use + diag(length(idx_use)) * eps_reg
	      }
	      w_use <- as.numeric(w[idx_use])
	      mean_use <- sum(w_use * Mu_use)
	      var_use <- as.numeric(crossprod(w_use, S_use %*% w_use))
	      if (!is.finite(var_use) || var_use < 0) var_use <- 0
	      c(mean = mean_use, sd = sqrt(var_use))
	    }

	    normalize_theta_time_sample <- function(theta_arr, target_time, target_samples, context) {
	      if (!is.numeric(theta_arr) || is.null(dim(theta_arr)) || length(dim(theta_arr)) != 3L) {
        stop(sprintf("[%s_SHAPE] expected theta_arr as numeric 3D array.", context))
      }

      d <- as.integer(dim(theta_arr))
      arr <- theta_arr
      if (d[2] == target_time) {
        arr <- theta_arr
      } else if (d[3] == target_time) {
        arr <- aperm(theta_arr, c(1, 3, 2))
      } else {
        stop(
          sprintf(
            "[%s_DIM] unsupported theta_arr dimensions %s for target_time=%d.",
            context,
            paste(d, collapse = "x"),
            as.integer(target_time)
          )
        )
      }

      s_cur <- as.integer(dim(arr)[3])
      s_tar <- as.integer(target_samples)
      if (!is.finite(s_tar) || s_tar <= 0L) {
        stop(sprintf("[%s_NSAMP_TARGET] target_samples must be positive; got %s.", context, as.character(target_samples)))
      }
      if (!is.finite(s_cur) || s_cur <= 0L) {
        stop(sprintf("[%s_NSAMP_CUR] normalized theta_arr has invalid sample dimension %s.", context, as.character(s_cur)))
      }
      if (s_cur == s_tar) {
        return(arr)
      }

      if (s_cur > s_tar) {
        agg_disc_warn_once(
          paste0("theta_nsamp_truncate:", context),
          sprintf(
            "[%s_NSAMP_TRUNCATE] truncating theta samples from %d to %d.",
            context,
            s_cur,
            s_tar
          )
        )
        return(arr[, , seq_len(s_tar), drop = FALSE])
      }

      agg_disc_warn_once(
        paste0("theta_nsamp_recycle:", context),
        sprintf(
          "[%s_NSAMP_RECYCLE] recycling theta samples from %d to %d (deterministic index repeat).",
          context,
          s_cur,
          s_tar
        )
      )
      idx <- rep(seq_len(s_cur), length.out = s_tar)
      arr[, , idx, drop = FALSE]
    }

    project_discrep_block <- function(theta_arr, row_idx, context) {
      arr <- normalize_theta_time_sample(
        theta_arr = theta_arr,
        target_time = TT,
        target_samples = n.samp,
        context = context
      )
      if (max(row_idx) > dim(arr)[1]) {
        stop(
          sprintf(
            "[%s_ROW_OOB] requested rows %s exceed theta_arr rows=%d.",
            context,
            paste(row_idx, collapse = ","),
            as.integer(dim(arr)[1])
          )
        )
      }

      f_use <- as.numeric(F_constant_disc)[seq_along(row_idx)]
      if (any(!is.finite(f_use))) {
        agg_disc_warn_once(
          paste0("fconst:", context),
          sprintf("[%s_F_CONST_NONFINITE] non-finite projection weights detected; treating non-finite weights as 0.", context)
        )
        f_use[!is.finite(f_use)] <- 0
      }

      theta_mat <- matrix(arr[row_idx, , ], nrow = length(row_idx))
      finite_col_count <- colSums(is.finite(theta_mat))
      theta_mat[!is.finite(theta_mat)] <- 0

      out_vec <- as.vector(crossprod(f_use, theta_mat))
      out <- matrix(out_vec, nrow = TT, ncol = n.samp)
      out[!matrix(finite_col_count > 0L, nrow = TT, ncol = n.samp)] <- NA_real_
      out
    }

	    idx <- c(0)
      ens_segment_lengths <- c(
        length(samp.theta_ens_5_exAL_synth_DISC),
        length(samp.theta_ens_20_exAL_synth_DISC),
        length(samp.theta_ens_35_exAL_synth_DISC),
        length(samp.theta_ens_50_exAL_synth_DISC),
        length(samp.theta_ens_65_exAL_synth_DISC),
        length(samp.theta_ens_80_exAL_synth_DISC),
        length(samp.theta_ens_95_exAL_synth_DISC),
        length(FF_list)
      )
      n_seg_available <- min(c(as.integer(J), as.integer(ens_segment_lengths)))
      if (!is.finite(n_seg_available) || n_seg_available <= 0L) {
        stop("[EXAL_FORECAST_SEGMENTS_EMPTY] exAL forecast segment lists are empty; cannot build forecast sample cube.")
      }
      if (n_seg_available < J) {
        warning(
          sprintf(
            "[EXAL_FORECAST_SEGMENTS_TRUNCATE] exAL forecast segments truncated from J=%d to %d based on available segment lists.",
            as.integer(J),
            as.integer(n_seg_available)
          ),
          call. = FALSE
        )
      }

	    for (j in seq_len(n_seg_available)) {
	        idx <- next_idx_block(idx, ks[J - j + 1])
          if (length(idx) == 0L) {
            next
          }

	        FF_s <- FF_list[[j]]
	        segment_len <- length(idx)

	        fill_xbs_segment <- function(theta_arr, out_row) {
	            if (j == J) {
	                theta_arr <- aperm(theta_arr, c(1, 3, 2))
	            }

	            state_dim <- dim(theta_arr)[1]
	            w_state <- build_usgs_projection_weights(
	              ff_seg = FF_s,
	              state_len = state_dim,
	              seg_id = j,
	              context = sprintf("xbs_segment_qrow%d_seg%d", as.integer(out_row), as.integer(j))
	            )
	            idx_state <- which(abs(w_state) > 0)
	            if (length(idx_state) == 0L) {
	              xbs[out_row, idx, ] <- NA_real_
	              return(invisible(NULL))
	            }

	            theta_mat <- matrix(theta_arr[idx_state, , ], nrow = length(idx_state))
	            xb_vec <- as.vector(crossprod(w_state[idx_state], theta_mat))
	            xb_mat <- matrix(xb_vec, nrow = segment_len, ncol = n.samp)
	            xbs[out_row, idx, ] <- xb_mat
	            invisible(NULL)
	        }

	        fill_xbs_segment(samp.theta_ens_5_exAL_synth_DISC[[j]]$samp_theta, 1)
	        fill_xbs_segment(samp.theta_ens_20_exAL_synth_DISC[[j]]$samp_theta, 2)
	        fill_xbs_segment(samp.theta_ens_35_exAL_synth_DISC[[j]]$samp_theta, 3)
	        fill_xbs_segment(samp.theta_ens_50_exAL_synth_DISC[[j]]$samp_theta, 4)
	        fill_xbs_segment(samp.theta_ens_65_exAL_synth_DISC[[j]]$samp_theta, 5)
	        fill_xbs_segment(samp.theta_ens_80_exAL_synth_DISC[[j]]$samp_theta, 6)
	        fill_xbs_segment(samp.theta_ens_95_exAL_synth_DISC[[j]]$samp_theta, 7)

	        if (j == 1) {
            fill_discrep <- function(theta_arr, out_row) {
                xb_discrep1[out_row, , ] <- project_discrep_block(
                  theta_arr = theta_arr,
                  row_idx = 8:14,
                  context = sprintf("exal_discrep1_qrow%d", as.integer(out_row))
                )
                xb_discrep2[out_row, , ] <- project_discrep_block(
                  theta_arr = theta_arr,
                  row_idx = 15:21,
                  context = sprintf("exal_discrep2_qrow%d", as.integer(out_row))
                )
            }

	            fill_discrep(samp.theta_5_exAL_synth_DISC$samp_theta, 1)
	            fill_discrep(samp.theta_20_exAL_synth_DISC$samp_theta, 2)
	            fill_discrep(samp.theta_35_exAL_synth_DISC$samp_theta, 3)
	            fill_discrep(samp.theta_50_exAL_synth_DISC$samp_theta, 4)
	            fill_discrep(samp.theta_65_exAL_synth_DISC$samp_theta, 5)
	            fill_discrep(samp.theta_80_exAL_synth_DISC$samp_theta, 6)
	            fill_discrep(samp.theta_95_exAL_synth_DISC$samp_theta, 7)
	        }
	    }


prepare_quantile_data <- function(v_d) {
  if (exists("fast_prepare_quantile_data", mode = "function")) {
    return(fast_prepare_quantile_data(v_d, probs = c(0.975, 0.5, 0.025), type = 7L, na.rm = FALSE))
  }

  v_d_transposed <- aperm(v_d, c(3, 1, 2))
  q_d_transposed <- apply(v_d_transposed, 2:3, function(x) quantile(x, probs = c(0.975, 0.5, 0.025)))
  q_d <- aperm(q_d_transposed, c(2, 3, 1))
  q_d
}

build_discrep_quantiles_from_state_posterior <- function(theta_obj_list, row_idx, context) {
  out <- array(NA_real_, dim = c(length(theta_obj_list), TT, 3L))
  w <- as.numeric(F_constant_disc)[seq_along(row_idx)]
  if (any(!is.finite(w))) {
    stop(sprintf("[%s_F_CONST_NONFINITE] discrepancy projection weights are non-finite.", context))
  }

  z025 <- qnorm(0.025)
  z975 <- qnorm(0.975)

  for (i in seq_along(theta_obj_list)) {
    obj <- theta_obj_list[[i]]
    sm <- obj$sm
    sC <- obj$sC
    if (!is.numeric(sm) || is.null(dim(sm)) || length(dim(sm)) != 2L) {
      stop(sprintf("[%s_SM_SHAPE] expected theta$sm as numeric matrix for row %d.", context, as.integer(i)))
    }
    if (!is.numeric(sC) || is.null(dim(sC)) || length(dim(sC)) != 3L) {
      stop(sprintf("[%s_SC_SHAPE] expected theta$sC as numeric 3D array for row %d.", context, as.integer(i)))
    }
    if (nrow(sm) < max(row_idx)) {
      stop(
        sprintf(
          "[%s_ROW_OOB] theta$sm rows=%d cannot support requested row_idx up to %d.",
          context,
          as.integer(nrow(sm)),
          as.integer(max(row_idx))
        )
      )
    }
    if (ncol(sm) < TT || dim(sC)[3] < TT) {
      stop(
        sprintf(
          "[%s_TIME_SHORT] theta$sm cols=%d and theta$sC slices=%d must both cover TT=%d.",
          context,
          as.integer(ncol(sm)),
          as.integer(dim(sC)[3]),
          as.integer(TT)
        )
      )
    }

    means <- as.numeric(crossprod(w, sm[row_idx, seq_len(TT), drop = FALSE]))
    vars <- numeric(TT)
    for (tt in seq_len(TT)) {
      Ct <- sC[row_idx, row_idx, tt, drop = FALSE]
      Ct <- matrix(Ct, nrow = length(row_idx), ncol = length(row_idx))
      vars[[tt]] <- as.numeric(crossprod(w, Ct %*% w))
    }
    vars[!is.finite(vars)] <- NA_real_
    vars[vars < 0] <- 0
    sds <- sqrt(vars)

    lower <- means + z025 * sds
    upper <- means + z975 * sds

    out[i, , 1L] <- lower
    out[i, , 2L] <- means
    out[i, , 3L] <- upper
  }
  out
}

    discrep_theta_objs <- list(
      new.theta.out_5_exAL_synth_DISC,
      new.theta.out_20_exAL_synth_DISC,
      new.theta.out_35_exAL_synth_DISC,
      new.theta.out_50_exAL_synth_DISC,
      new.theta.out_65_exAL_synth_DISC,
      new.theta.out_80_exAL_synth_DISC,
      new.theta.out_95_exAL_synth_DISC
    )

    q_d_discrep1_quantiles <- profile_section(
      "figures.discrep1_quantiles_state_posterior",
      build_discrep_quantiles_from_state_posterior(
        theta_obj_list = discrep_theta_objs,
        row_idx = 8:14,
        context = "EXAL_DISCREP1_STATE_POSTERIOR"
      )
    )
    q_d_discrep2_quantiles <- profile_section(
      "figures.discrep2_quantiles_state_posterior",
      build_discrep_quantiles_from_state_posterior(
        theta_obj_list = discrep_theta_objs,
        row_idx = 15:21,
        context = "EXAL_DISCREP2_STATE_POSTERIOR"
      )
    )
    if (!any(is.finite(q_d_discrep1_quantiles))) {
      stop("[EXAL_DISCREP1_ALL_NONFINITE] discrepancy-1 quantiles are entirely non-finite after projection.")
    }
    if (!any(is.finite(q_d_discrep2_quantiles))) {
      stop("[EXAL_DISCREP2_ALL_NONFINITE] discrepancy-2 quantiles are entirely non-finite after projection.")
    }



eps <- 0.0

    profile_section("figures.sample_xbs_sm_ens", {
    idx <- c(0)
    theta_objs <- list(
      q05 = new.theta.out_5_exAL_synth_DISC,
      q20 = new.theta.out_20_exAL_synth_DISC,
      q35 = new.theta.out_35_exAL_synth_DISC,
      q50 = new.theta.out_50_exAL_synth_DISC,
      q65 = new.theta.out_65_exAL_synth_DISC,
      q80 = new.theta.out_80_exAL_synth_DISC,
      q95 = new.theta.out_95_exAL_synth_DISC
    )

    segment_capacity <- function(theta_obj, seg_id) {
      if (!is.list(theta_obj$sm_ens) || !is.list(theta_obj$sC_ens)) {
        return(0L)
      }
      if (seg_id > length(theta_obj$sm_ens) || seg_id > length(theta_obj$sC_ens)) {
        return(0L)
      }
      sm_j <- theta_obj$sm_ens[[seg_id]]
      sC_j <- theta_obj$sC_ens[[seg_id]]
      if (!is.numeric(sm_j) || is.null(dim(sm_j)) || length(dim(sm_j)) != 2L) {
        return(0L)
      }
      if (!is.numeric(sC_j) || is.null(dim(sC_j)) || length(dim(sC_j)) != 3L) {
        return(0L)
      }
      as.integer(min(ncol(sm_j), dim(sC_j)[3]))
    }

    n_seg_sample <- min(c(
      as.integer(J),
      as.integer(length(FF_list)),
      vapply(theta_objs, function(obj) min(length(obj$sm_ens), length(obj$sC_ens)), integer(1))
    ))
    if (!is.finite(n_seg_sample) || n_seg_sample <= 0L) {
      stop("[EXAL_FORECAST_SAMPLE_SEGMENTS_EMPTY] No usable exAL forecast segments are available for sampled synthesis.")
    }
    if (n_seg_sample < J) {
      warning(
        sprintf(
          "[EXAL_FORECAST_SAMPLE_SEGMENTS_TRUNCATE] sample_xbs uses %d/%d segments based on available sm_ens/sC_ens/FF_list.",
          as.integer(n_seg_sample),
          as.integer(J)
        ),
        call. = FALSE
      )
    }

	    for(j in seq_len(n_seg_sample)){

    idx <- next_idx_block(idx, ks[J-j+1])
    if (length(idx) == 0L) {
      next
    }
    seg_caps <- vapply(theta_objs, segment_capacity, seg_id = j, integer(1))
    seg_cap <- min(seg_caps)
    if (!is.finite(seg_cap) || seg_cap <= 0L) {
      stop(
        sprintf(
          "[EXAL_FORECAST_SAMPLE_SEGMENT_INVALID] Segment j=%d has invalid sm_ens/sC_ens shapes for sampled synthesis.",
          as.integer(j)
        )
      )
    }
    if (seg_cap < length(idx)) {
      stop(
        sprintf(
          "[EXAL_FORECAST_SAMPLE_SEGMENT_SHORT] Segment j=%d expected %d time steps but only %d are available in sampled state objects.",
          as.integer(j),
          as.integer(length(idx)),
          as.integer(seg_cap)
        )
      )
    }
	    tt <- 1
	    for(t in (idx) ){
	        fill_xbs_state <- function(theta_obj, out_row, q_label) {
	          Mu <- theta_obj$sm_ens[[j]][, tt]
	          Sigma <- theta_obj$sC_ens[[j]][,, tt]
	          proj <- project_state_gaussian(
	            Mu = Mu,
	            Sigma = Sigma,
	            ff_seg = FF_list[[j]],
	            seg_id = j,
	            eps_reg = eps,
	            context = sprintf("xbs_sm_ens_q%s_seg%d_t%d", q_label, as.integer(j), as.integer(tt))
	          )
	          mu_use <- as.numeric(proj[["mean"]])
	          sd_use <- as.numeric(proj[["sd"]])
	          if (!is.finite(mu_use)) mu_use <- NA_real_
	          if (!is.finite(sd_use) || sd_use < 0) sd_use <- 0
	          xbs[out_row, t, ] <<- rnorm(n = n.samp, mean = mu_use, sd = sd_use)
	        }

	        fill_xbs_state(new.theta.out_5_exAL_synth_DISC, 1, "05")
	        fill_xbs_state(new.theta.out_20_exAL_synth_DISC, 2, "20")
	        fill_xbs_state(new.theta.out_35_exAL_synth_DISC, 3, "35")
	        fill_xbs_state(new.theta.out_50_exAL_synth_DISC, 4, "50")
	        fill_xbs_state(new.theta.out_65_exAL_synth_DISC, 5, "65")
	        fill_xbs_state(new.theta.out_80_exAL_synth_DISC, 6, "80")
	        fill_xbs_state(new.theta.out_95_exAL_synth_DISC, 7, "95")
	        tt <- tt+1
	    }
	  }
    })


    profile_section("figures.sort_xbs_forecast", {
    for(t in 1:ranges[1]){
    xbs[1,t,] <- sort_keep_na(xbs[1,t,])
    xbs[2,t,] <- sort_keep_na(xbs[2,t,])
    xbs[3,t,] <- sort_keep_na(xbs[3,t,])
    xbs[4,t,] <- sort_keep_na(xbs[4,t,])
    xbs[5,t,] <- sort_keep_na(xbs[5,t,])
    xbs[6,t,] <- sort_keep_na(xbs[6,t,])
    xbs[7,t,] <- sort_keep_na(xbs[7,t,])
}
 
    })

  })
})



if (is.null(ndlm_direct_mean_draws)) {
idx <- c(0)
for(j in 1:J){
    idx <- next_idx_block(idx, ks[J-j+1])
    if (length(idx) == 0L) {
      next
    }

    if (j > length(new.theta.out_50_NDLM_synth_DISC$sm_ens) ||
        j > length(new.theta.out_50_NDLM_synth_DISC$sC_ens)) {
      ndlm_warn_once(
        paste0("missing_seg:", j),
        sprintf("NDLM ensemble segment j=%d is missing; skipping this segment.", as.integer(j))
      )
      next
    }

    sm_j <- new.theta.out_50_NDLM_synth_DISC$sm_ens[[j]]
    sC_j <- new.theta.out_50_NDLM_synth_DISC$sC_ens[[j]]
    if (!is.numeric(sm_j) || is.null(dim(sm_j)) || length(dim(sm_j)) != 2L ||
        !is.numeric(sC_j) || is.null(dim(sC_j)) || length(dim(sC_j)) != 3L) {
      ndlm_warn_once(
        paste0("shape_seg:", j),
        sprintf("NDLM ensemble segment j=%d has invalid shape; skipping this segment.", as.integer(j))
      )
      next
    }

    n_avail <- min(length(idx), ncol(sm_j), dim(sC_j)[3])
    if (!is.finite(n_avail) || n_avail <= 0L) {
      ndlm_warn_once(
        paste0("empty_seg:", j),
        sprintf("NDLM ensemble segment j=%d has no overlapping forecast horizon; skipping.", as.integer(j))
      )
      next
    }
    if (n_avail < length(idx)) {
      ndlm_warn_once(
        paste0("trunc_seg:", j),
        sprintf(
          "NDLM segment j=%d forecast horizon truncated from %d to %d (available sm/sC columns).",
          as.integer(j),
          as.integer(length(idx)),
          as.integer(n_avail)
        )
      )
    }

    Ft <- FF_list[[j]][1:p, 1]
    for (tt in seq_len(n_avail)) {
      t <- idx[[tt]]
      Mu <- sm_j[, tt]
      Sigma <- sC_j[, , tt]
      p_use <- min(
        length(Ft),
        length(Mu),
        nrow(Sigma),
        ncol(Sigma)
      )
      if (!is.finite(p_use) || p_use <= 0L) {
        next
      }
      Ft_use <- matrix(Ft[seq_len(p_use)], ncol = 1L)
      S <- Sigma[seq_len(p_use), seq_len(p_use), drop = FALSE] + diag(p_use) * eps
      mean_use <- as.numeric(crossprod(Ft_use, Mu[seq_len(p_use)]))
      var_use <- as.numeric(t(Ft_use) %*% S %*% Ft_use)
      sd_use <- sqrt(max(var_use, 0))
      xbs_samp <- rnorm(n = n.samp, mean = mean_use, sd = sd_use)
      xbs_ndlm[1, t, ] <- xbs_samp
    }
}
}

set.seed(777)

# Function Definitions
inverse_cdf_AL <- function(U, mu, sigma, p) {
  ifelse(U < p, 
         mu + (sigma / (1 - p)) * log(U / p), 
         mu - (sigma / p) * log((1 - U) / (1 - p)))
}

p_fn <- function(p0, gam) {
  (p0 - as.numeric(gam < 0)) / exp(log_g(gam)) + as.numeric(gam < 0)
}

C_fn <- function(p0, gam) {
  temp_p <- p_fn(p0, gam)
  (as.numeric(gam > 0) - temp_p)^(-1)
}

# Generalized function to handle each case
generate_y_post <- function(p0, xb_matrix, gamma_sample, sigma_sample) {
  n_rows <- dim(xb_matrix)[1]
  n_cols <- dim(xb_matrix)[2]
  y_post <- matrix(NA_real_, nrow = n_rows, ncol = n_cols)
  
  for (t in 1:n_cols) {
    s_0 <- rtruncnorm(1, a=0, b=Inf, mean = 0, sd = 1)
    u <- runif(n_rows)
    y_post[,t] <- xb_matrix[,t] + sigma_sample * abs(gamma_sample) * C_fn(p0, gamma_sample) * s_0 +  
                  sigma_sample * inverse_cdf_AL(u, 0, 1, p_fn(p0, gamma_sample))
  }
  
  return(y_post)
}


# Case 1: p0 = 0.05
p0_05 <- 0.05
xb_05_f <- t(xbs[1,,])
gam_05_f <- samp.gamma_5_exAL_synth_DISC[1,]
sig_05_f <- samp.sigma_5_exAL_synth_DISC[1,]
y_post_5 <- generate_y_post(p0_05, xb_05_f, gam_05_f, sig_05_f)

# Case 2: p0 = 0.5
p0_50 <- 0.5
xb_50_f <- t(xbs[4,,])
gam_50_f <- samp.gamma_50_exAL_synth_DISC[1,]
sig_50_f <- samp.sigma_50_exAL_synth_DISC[1,]
y_post_50 <- generate_y_post(p0_50, xb_50_f, gam_50_f, sig_50_f)

# Case 3: p0 = 0.95
p0_95 <- 0.95
xb_95_f <- t(xbs[7,,])
gam_95_f <- samp.gamma_95_exAL_synth_DISC[1,]
sig_95_f <- samp.sigma_95_exAL_synth_DISC[1,]
y_post_95 <- generate_y_post(p0_95, xb_95_f, gam_95_f, sig_95_f)

# Case 4: p0 = 0.20
p0_20 <- 0.20
xb_20_f <- t(xbs[2,,])
gam_20_f <- samp.gamma_20_exAL_synth_DISC[1,]
sig_20_f <- samp.sigma_20_exAL_synth_DISC[1,]
y_post_20 <- generate_y_post(p0_20, xb_20_f, gam_20_f, sig_20_f)

# Case 5: p0 = 0.80
p0_80 <- 0.80
xb_80_f <- t(xbs[6,,])
gam_80_f <- samp.gamma_80_exAL_synth_DISC[1,]
sig_80_f <- samp.sigma_80_exAL_synth_DISC[1,]
y_post_80 <- generate_y_post(p0_80, xb_80_f, gam_80_f, sig_80_f)

# Case 6: p0 = 0.35
p0_35 <- 0.35
xb_35_f <- t(xbs[3,,])
gam_35_f <- samp.gamma_35_exAL_synth_DISC[1,]
sig_35_f <- samp.sigma_35_exAL_synth_DISC[1,]
y_post_35 <- generate_y_post(p0_35, xb_35_f, gam_35_f, sig_35_f)

# Case 7: p0 = 0.65
p0_65 <- 0.65
xb_65_f <- t(xbs[5,,])
gam_65_f <- samp.gamma_65_exAL_synth_DISC[1,]
sig_65_f <- samp.sigma_65_exAL_synth_DISC[1,]
y_post_65 <- generate_y_post(p0_65, xb_65_f, gam_65_f, sig_65_f)

n_rows_5 <- dim(xb_05_f)[1]
n_cols_5 <- dim(xb_05_f)[2]


# dim(y_post_35)

for(t in 1:ranges[1]){
    xbs[1,t,] <- sort_keep_na(xbs[1,t,])
    xbs[2,t,] <- sort_keep_na(xbs[2,t,])
    xbs[3,t,] <- sort_keep_na(xbs[3,t,])
    xbs[4,t,] <- sort_keep_na(xbs[4,t,])
    xbs[5,t,] <- sort_keep_na(xbs[5,t,])
    xbs[6,t,] <- sort_keep_na(xbs[6,t,])
    xbs[7,t,] <- sort_keep_na(xbs[7,t,])
}

# Function to plot lines for multiple forecasts
plot_forecast_lines <- function(idx, y_post, xb_f, n_rows, truth, color_forecast = 'gray', color_baseline = 'pink') {
  for (s in 1:n_rows) {
    # Forecast lines
    lines((length(idx) + 1):(length(idx) + length(truth)), y_post[s, ], ylab = "", col = color_forecast)
    # Baseline lines
    lines((length(idx) + 1):(length(idx) + length(truth)), xb_f[s, ], ylab = "", col = color_baseline)
  }
}

# Plot for the log-transformed truth data
plot_log_truth_data <- function(idx, Y, truth, y_post_95, y_post_5, y_post_50, y_post_20, y_post_35, y_post_80, y_post_65,
                                xb_95_f, xb_05_f, xb_50_f, xb_20_f, xb_35_f, xb_80_f, xb_65_f, n_rows) {
  plot.ts(rep(0, length(idx) + 30), ylab = "", ylim = c(-1.5, 2.5))
  lines(Y[1, idx], ylab = "")
  points(Y[1, idx], ylab = "", pch = 19)
  
  # Forecast lines
  plot_forecast_lines(idx, y_post_95, xb_95_f, n_rows, truth)
  plot_forecast_lines(idx, y_post_5, xb_05_f, n_rows, truth)
  plot_forecast_lines(idx, y_post_50, xb_50_f, n_rows, truth)
  plot_forecast_lines(idx, y_post_20, xb_20_f, n_rows, truth)
  plot_forecast_lines(idx, y_post_35, xb_35_f, n_rows, truth)
  plot_forecast_lines(idx, y_post_80, xb_80_f, n_rows, truth)
  plot_forecast_lines(idx, y_post_65, xb_65_f, n_rows, truth)
  
  # Add truth points
  points((length(idx) + 1):(length(idx) + length(truth)), truth, ylab = "", col = 'darkred', pch = 19)
}

# Plot for the exp-transformed truth data
plot_exp_truth_data <- function(idx, Y, truth, y_post_95, y_post_5, y_post_50, y_post_20, y_post_35, y_post_80, y_post_65,
                                xb_95_f, xb_05_f, xb_50_f, xb_20_f, xb_35_f, xb_80_f, xb_65_f, n_rows) {
  plot.ts(rep(0, length(idx) + 30), ylab = "", ylim = c(0, 10))
  lines(exp(Y[1, idx]), ylab = "")
  points(exp(Y[1, idx]), ylab = "", pch = 19)
  
  # Forecast lines
  plot_forecast_lines(idx, exp(y_post_95), exp(xb_95_f), n_rows, truth)
  plot_forecast_lines(idx, exp(y_post_5), exp(xb_05_f), n_rows, truth)
  plot_forecast_lines(idx, exp(y_post_50), exp(xb_50_f), n_rows, truth)
  plot_forecast_lines(idx, exp(y_post_20), exp(xb_20_f), n_rows, truth)
  plot_forecast_lines(idx, exp(y_post_35), exp(xb_35_f), n_rows, truth)
  plot_forecast_lines(idx, exp(y_post_80), exp(xb_80_f), n_rows, truth)
  plot_forecast_lines(idx, exp(y_post_65), exp(xb_65_f), n_rows, truth)
  
  # Add truth points
  points((length(idx) + 1):(length(idx) + length(truth)), truth, ylab = "", col = 'darkred', pch = 19)
}

# Applying quantile computations and mean for each case
compute_quantiles_means <- function(y_post) {
  quantiles <- fast_col_quantiles_t(y_post, probs = c(0.05, 0.5, 0.95))
  mean_values <- colMeans(y_post)
  return(list(quantiles = quantiles, means = mean_values))
}

# Generate quantiles and means for each posterior
q50 <- compute_quantiles_means(y_post_50)
q5 <- compute_quantiles_means(y_post_5)
q95 <- compute_quantiles_means(y_post_95)
q20 <- compute_quantiles_means(y_post_20)
q35 <- compute_quantiles_means(y_post_35)
q65 <- compute_quantiles_means(y_post_65)
q80 <- compute_quantiles_means(y_post_80)



# Main Code Execution

# Log-transformed truth data
truth_log <- log(San_Lorenzo_Daily_USGS_R$data0[San_Lorenzo_Daily_USGS_R$Date >= FORECAST_START_DATE][1:ranges[1]])
idx <- safe_time_index(TT - 30, TT, TT, context = "40_figures.tt_minus_30")

# Plot log-transformed data and forecast
plot_log_truth_data(idx, Y, truth_log, y_post_95, y_post_5, y_post_50, y_post_20, y_post_35, y_post_80, y_post_65,
                    xb_95_f, xb_05_f, xb_50_f, xb_20_f, xb_35_f, xb_80_f, xb_65_f, n_rows_5)

# Raw truth data
truth_raw <- San_Lorenzo_Daily_USGS_R$data0[San_Lorenzo_Daily_USGS_R$Date >= FORECAST_START_DATE][1:ranges[1]]

# Plot exp-transformed data and forecast
plot_exp_truth_data(idx, Y, truth_raw, y_post_95, y_post_5, y_post_50, y_post_20, y_post_35, y_post_80, y_post_65,
                    xb_95_f, xb_05_f, xb_50_f, xb_20_f, xb_35_f, xb_80_f, xb_65_f, n_rows_5)



# Applying quantile computations and mean for each case
compute_quantiles_means <- function(y_post, q0) {
  quantiles <- fast_col_quantiles_t(y_post, probs = c(q0, 0.025, 0.5, 0.975))
  mean_values <- colMeans(y_post)
  return(list(quantiles = quantiles, means = mean_values))
}

# Generate quantiles and means for each posterior
q50 <- compute_quantiles_means(y_post_50,0.5)
q5 <- compute_quantiles_means(y_post_5,0.05)
q95 <- compute_quantiles_means(y_post_95,0.95)
q20 <- compute_quantiles_means(y_post_20,0.2)
q35 <- compute_quantiles_means(y_post_35,0.35)
q65 <- compute_quantiles_means(y_post_65,0.65)
q80 <- compute_quantiles_means(y_post_80,0.8)
################################################################################################################################################
n.samp <- resolve_post_effective_n_samp(
  n_available = dim(samp.theta_50_exAL_synth_DISC$samp_theta)[3],
  TT = TT,
  context = "figures.synth_q_forecast"
)
if (n.samp < nrow(y_post_50)) {
  y_post_5 <- cap_sample_rows(y_post_5, n.samp)
  y_post_20 <- cap_sample_rows(y_post_20, n.samp)
  y_post_35 <- cap_sample_rows(y_post_35, n.samp)
  y_post_50 <- cap_sample_rows(y_post_50, n.samp)
  y_post_65 <- cap_sample_rows(y_post_65, n.samp)
  y_post_80 <- cap_sample_rows(y_post_80, n.samp)
  y_post_95 <- cap_sample_rows(y_post_95, n.samp)
}
synth_f <- matrix(NA_real_, nrow = n.samp, ncol = ranges[1])
synth_q_f <- matrix(NA_real_, nrow = n.samp, ncol = ranges[1])
k <- 10
sigma_5  <- samp.sigma_5_exAL_synth_DISC[1, seq_len(n.samp)]
sigma_20 <- samp.sigma_20_exAL_synth_DISC[1, seq_len(n.samp)]
sigma_35 <- samp.sigma_35_exAL_synth_DISC[1, seq_len(n.samp)]
sigma_50 <- samp.sigma_50_exAL_synth_DISC[1, seq_len(n.samp)]
sigma_65 <- samp.sigma_65_exAL_synth_DISC[1, seq_len(n.samp)]
sigma_80 <- samp.sigma_80_exAL_synth_DISC[1, seq_len(n.samp)]
sigma_95 <- samp.sigma_95_exAL_synth_DISC[1, seq_len(n.samp)]

q_refs <- rbind(
  q5$quantiles[1, ],
  q50$quantiles[1, ],
  q95$quantiles[1, ],
  q20$quantiles[1, ],
  q35$quantiles[1, ],
  q80$quantiles[1, ],
  q65$quantiles[1, ]
)

profile_section("figures.synth_weights", {
  for (t in 1:ranges[1]) {
    w1 <- exp(-k * check_loss_fn(0.05, y_post_5[, t]  - q5$quantiles[1, t])  / sigma_5)
    w2 <- exp(-k * check_loss_fn(0.50, y_post_50[, t] - q50$quantiles[1, t]) / sigma_50)
    w3 <- exp(-k * check_loss_fn(0.95, y_post_95[, t] - q95$quantiles[1, t]) / sigma_95)
    w4 <- exp(-k * check_loss_fn(0.20, y_post_20[, t] - q20$quantiles[1, t]) / sigma_20)
    w5 <- exp(-k * check_loss_fn(0.35, y_post_35[, t] - q35$quantiles[1, t]) / sigma_35)
    w6 <- exp(-k * check_loss_fn(0.80, y_post_80[, t] - q80$quantiles[1, t]) / sigma_80)
    w7 <- exp(-k * check_loss_fn(0.65, y_post_65[, t] - q65$quantiles[1, t]) / sigma_65)

    W <- cbind(w1, w2, w3, w4, w5, w6, w7)
    W <- W / rowSums(W)

    q_ref <- q_refs[, t]
    synth_q_f[, t] <- rowSums(W * matrix(q_ref, nrow = n.samp, ncol = 7, byrow = TRUE))
  }
})

q_synth <- fast_col_quantiles_t(synth_q_f, probs = c(0.025, 0.5, 0.975))
m_synth <- colMeans((synth_q_f))

################################################################################################################################################

truth<- San_Lorenzo_Daily_USGS_R$data0[San_Lorenzo_Daily_USGS_R$Date>=FORECAST_START_DATE]
truth <- log(truth[1:ranges[1]])
idx <- safe_time_index(TT - 30, TT, TT, context = "40_figures.tt_minus_30")
plot.ts(rep(0, length(idx)+30), ylab="", ylim=c(-1.5,2.5))
lines((Y[1,idx]), ylab="")
points((Y[1,idx]), ylab="", pch = 19)

x_future <- (length(idx)+1):(length(idx)+length(truth))
matlines(x_future, t(synth_f), lwd = 0.1, col='gray')
matlines(x_future, t(synth_q_f), lwd = 0.1, col='purple')

points((length(idx)+1):(length(idx)+length(truth)), (truth), ylab="", col='darkred', pch = 19)

# lines((length(idx)+1):(length(idx)+length(truth)),q95$quantiles[3,], lwd = 0.6, col='black')
lines((length(idx)+1):(length(idx)+length(truth)),q_synth[3,], lwd = 0.6, col='black')
lines((length(idx)+1):(length(idx)+length(truth)),q_synth[1,], lwd = 0.6, col='black')

################################################################################################################################################

truth<- San_Lorenzo_Daily_USGS_R$data0[San_Lorenzo_Daily_USGS_R$Date>=FORECAST_START_DATE]
truth <- (truth[1:ranges[1]])
idx <- safe_time_index(TT - 30, TT, TT, context = "40_figures.tt_minus_30")
plot.ts(rep(0, length(idx)+30), ylab="", ylim=c(0,6))
lines(exp(Y[1,idx]), ylab="")
points(exp(Y[1,idx]), ylab="", pch = 19)

x_future <- (length(idx)+1):(length(idx)+length(truth))
matlines(x_future, t(exp(synth_f)), lwd = 0.1, col='gray')
matlines(x_future, t(exp(synth_q_f)), lwd = 0.1, col='purple')
points((length(idx)+1):(length(idx)+length(truth)),(truth), ylab="", col='darkred', pch = 19)
lines((length(idx)+1):(length(idx)+length(truth)),exp(q_synth[3,]), lwd = 0.6, col='black')
lines((length(idx)+1):(length(idx)+length(truth)),exp(q_synth[1,]), lwd = 0.6, col='black')



profile_section("figures.sample_xbs_retro", {
  xbs_retro <- array(NA_real_, c(7, TT, n.samp))
  xbs_ndlm_retro <- array(NA_real_, c(1, TT, n.samp))

	  idx <- 1:TT
	  for (j in 1:J) {
	    for (t in idx) {
	        Ft <- FF[, 1, t]

	        Mu <- new.theta.out_50_NDLM_synth_DISC$sm[, t]
	        Sigma <- new.theta.out_50_NDLM_synth_DISC$sC[, , t]
	        stats_ndlm <- gaussian_projection_mean_sd(Ft, Mu, Sigma, context = "retro.ndlm")
	        mean_ndlm <- stats_ndlm[["mean"]]
	        sd_ndlm <- stats_ndlm[["sd"]]

	        Mu <- new.theta.out_95_exAL_synth_DISC$sm[, t]
	        Sigma <- new.theta.out_95_exAL_synth_DISC$sC[, , t]
	        stats_95 <- gaussian_projection_mean_sd(Ft, Mu, Sigma, context = "retro.exal95")
	        mean_95 <- stats_95[["mean"]]
	        sd_95 <- stats_95[["sd"]]

	        Mu <- new.theta.out_80_exAL_synth_DISC$sm[, t]
	        Sigma <- new.theta.out_80_exAL_synth_DISC$sC[, , t]
	        stats_80 <- gaussian_projection_mean_sd(Ft, Mu, Sigma, context = "retro.exal80")
	        mean_80 <- stats_80[["mean"]]
	        sd_80 <- stats_80[["sd"]]

	        Mu <- new.theta.out_65_exAL_synth_DISC$sm[, t]
	        Sigma <- new.theta.out_65_exAL_synth_DISC$sC[, , t]
	        stats_65 <- gaussian_projection_mean_sd(Ft, Mu, Sigma, context = "retro.exal65")
	        mean_65 <- stats_65[["mean"]]
	        sd_65 <- stats_65[["sd"]]

	        Mu <- new.theta.out_50_exAL_synth_DISC$sm[, t]
	        Sigma <- new.theta.out_50_exAL_synth_DISC$sC[, , t]
	        stats_50 <- gaussian_projection_mean_sd(Ft, Mu, Sigma, context = "retro.exal50")
	        mean_50 <- stats_50[["mean"]]
	        sd_50 <- stats_50[["sd"]]

	        Mu <- new.theta.out_35_exAL_synth_DISC$sm[, t]
	        Sigma <- new.theta.out_35_exAL_synth_DISC$sC[, , t]
	        stats_35 <- gaussian_projection_mean_sd(Ft, Mu, Sigma, context = "retro.exal35")
	        mean_35 <- stats_35[["mean"]]
	        sd_35 <- stats_35[["sd"]]

	        Mu <- new.theta.out_20_exAL_synth_DISC$sm[, t]
	        Sigma <- new.theta.out_20_exAL_synth_DISC$sC[, , t]
	        stats_20 <- gaussian_projection_mean_sd(Ft, Mu, Sigma, context = "retro.exal20")
	        mean_20 <- stats_20[["mean"]]
	        sd_20 <- stats_20[["sd"]]

	        Mu <- new.theta.out_5_exAL_synth_DISC$sm[, t]
	        Sigma <- new.theta.out_5_exAL_synth_DISC$sC[, , t]
	        stats_5 <- gaussian_projection_mean_sd(Ft, Mu, Sigma, context = "retro.exal05")
	        mean_5 <- stats_5[["mean"]]
	        sd_5 <- stats_5[["sd"]]

	        means <- c(mean_ndlm, mean_95, mean_80, mean_65, mean_50, mean_35, mean_20, mean_5)
	        sds <- c(sd_ndlm, sd_95, sd_80, sd_65, sd_50, sd_35, sd_20, sd_5)

	        draws <- rnorm(
	          n = n.samp * length(means),
	          mean = rep(means, each = n.samp),
	          sd = rep(sds, each = n.samp)
	        )
	        draws_mat <- matrix(draws, nrow = n.samp, ncol = length(means))

	        xbs_ndlm_retro[1, t, ] <- draws_mat[, 1]
	        xbs_retro[7, t, ] <- draws_mat[, 2]
	        xbs_retro[6, t, ] <- draws_mat[, 3]
	        xbs_retro[5, t, ] <- draws_mat[, 4]
	        xbs_retro[4, t, ] <- draws_mat[, 5]
	        xbs_retro[3, t, ] <- draws_mat[, 6]
	        xbs_retro[2, t, ] <- draws_mat[, 7]
	        xbs_retro[1, t, ] <- draws_mat[, 8]

	    }
	  }
	})

profile_section("figures.sort_xbs_retro", {
  for (t in 1:ranges[1]) {
    xbs_retro[1, t, ] <- sort_keep_na(xbs_retro[1, t, ])
    xbs_retro[2, t, ] <- sort_keep_na(xbs_retro[2, t, ])
    xbs_retro[3, t, ] <- sort_keep_na(xbs_retro[3, t, ])
    xbs_retro[4, t, ] <- sort_keep_na(xbs_retro[4, t, ])
    xbs_retro[5, t, ] <- sort_keep_na(xbs_retro[5, t, ])
    xbs_retro[6, t, ] <- sort_keep_na(xbs_retro[6, t, ])
    xbs_retro[7, t, ] <- sort_keep_na(xbs_retro[7, t, ])
  }
})

truth<- San_Lorenzo_Daily_USGS_R$data0[San_Lorenzo_Daily_USGS_R$Date>=FORECAST_START_DATE]
# truth <- truth[1:ranges[1]]
truth <- log(truth[1:ranges[1]])

FF_t <- aperm(FF, c(2, 1, 3))
multiply_matrices <- function(slice_index) {
    FF_t[,,slice_index] %*% new.theta.out_50_exAL_synth_DISC$sm[,slice_index]
}
result_list <- lapply(1:ncol(new.theta.out_50_exAL_synth_DISC$sm), multiply_matrices)
result_array <- array(unlist(result_list), dim = c(J+1, 1, ncol(new.theta.out_50_exAL_synth_DISC$sm)))
result_array <- aperm(result_array, c(1, 3, 2))[,,1]
dim( result_array )
TT

idx <- safe_time_index(TT - 300, TT, TT, context = "40_figures.tt_minus_300")
plot.ts(Y[1,idx], col = 'gray')
points(Y[1,idx], col = 'black')
lines(new.theta.out_50_exAL_synth_DISC$exps[1,idx], col = 'green')
lines(new.theta.out_5_exAL_synth_DISC$exps[1,idx], col = 'red')
lines(new.theta.out_95_exAL_synth_DISC$exps[1,idx], col = 'blue')
# lines(new.theta.out_50_exAL_synth$exps[1,], col = 'green')
# lines(new.theta.out_50_exAL_synth$exps[1,], col = 'green')
# lines(new.theta.out_50_exAL_synth$exps[1,], col = 'green')

dates_ts_usgs <- timestamps

# Setting up the indices
iii <- 30
idx1 <- (TT-iii):(TT+ranges[2])
idx2 <- (TT-iii):(TT+ranges[1])
idx_all <- (TT-iii):(TT+ranges[1])
idx_T <- (TT-iii):TT
idx_y <- (TT-iii):TT
idx_f <- ((iii+1)+1):((iii+1)+ranges[1])

# Initialize the matrix
q_exps <- matrix(NA_real_, nrow = 7, ncol=(ranges[1]))
align_to_len <- function(x, target_len, context = "q_exps") {
  x <- as.numeric(x)
  target_len <- as.integer(target_len)
  if (length(x) == target_len) {
    return(x)
  }
  if (length(x) == 0L) {
    warning(sprintf("%s is empty; padding with NA to length %d", context, target_len), call. = FALSE)
    return(rep(NA_real_, target_len))
  }
  if (length(x) < target_len) {
    warning(sprintf("%s length %d < %d; padding with NA", context, length(x), target_len), call. = FALSE)
    return(c(x, rep(NA_real_, target_len - length(x))))
  }
  warning(sprintf("%s length %d > %d; truncating", context, length(x), target_len), call. = FALSE)
  x[seq_len(target_len)]
}
safe_exps_index <- function(theta_obj, row, idx, context = "exps.index") {
  idx <- as.integer(idx)
  target_len <- length(idx)
  exps <- theta_obj$exps
  if (!is.matrix(exps) || nrow(exps) < row) {
    warning(sprintf("%s unavailable; returning NA length %d", context, target_len), call. = FALSE)
    return(rep(NA_real_, target_len))
  }
  out <- rep(NA_real_, target_len)
  valid <- idx >= 1L & idx <= ncol(exps)
  if (any(valid)) {
    out[valid] <- as.numeric(exps[row, idx[valid]])
  }
  if (any(!valid)) {
    warning(
      sprintf(
        "%s out-of-bounds for %d indices (ncol(exps)=%d); filling NA",
        context,
        sum(!valid),
        ncol(exps)
      ),
      call. = FALSE
    )
  }
  out
}
safe_exps_range <- function(theta_obj, row, start_idx, end_idx, context = "exps.range") {
  idx <- seq.int(as.integer(start_idx), as.integer(end_idx))
  safe_exps_index(theta_obj, row = row, idx = idx, context = context)
}
png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
# Base plot
plot.ts((new.theta.out_95_exAL_synth_DISC$exps[2,idx_all]) * 0, ylim = c(-2.5, 2.5),
        xlab = " ", ylab = "log-flow", xaxt = "n", col = NA, lwd = 2, main = "Dynamic Quantile:  exAL")

# Adding 'Truth' points
points((1 + length(idx_y)):(length(truth) + length(idx_y)), truth, col = 'deeppink4', pch = 19, cex = 0.7, lwd = 1)

# Adding 'Observations'
lines(Y[1,idx_y], col = 'black', lwd = 1.5)
points(Y[1,idx_y], col = 'black', pch = 16, cex = 0.6)

# # Adding 95th Quantile estimation
# d1 <- new.theta.out_95_exAL_synth_DISC$sm_ens[[1]][8,]
# d2 <- new.theta.out_95_exAL_synth_DISC$sm_ens[[2]][8,]

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_5_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_5_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_5_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[1,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[1,]")
lines(new.theta.out_5_exAL_synth_DISC$exps[1,idx1], col = 'darkred', lwd = 2)

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_20_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_20_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_20_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[2,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[2,]")
lines(new.theta.out_20_exAL_synth_DISC$exps[1,idx1], col = 'purple', lwd = 2)

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_35_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_35_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_35_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[3,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[3,]")
lines(new.theta.out_35_exAL_synth_DISC$exps[1,idx1], col = 'purple', lwd = 2)

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_50_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_50_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_50_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[4,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[4,]")
lines(new.theta.out_50_exAL_synth_DISC$exps[1,idx1], col = 'darkgreen', lwd = 2)

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_65_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_65_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_65_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[5,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[5,]")
lines(new.theta.out_65_exAL_synth_DISC$exps[1,idx1], col = 'purple', lwd = 2)

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_80_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_80_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_80_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[6,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[6,]")
lines(new.theta.out_80_exAL_synth_DISC$exps[1,idx1], col = 'purple', lwd = 2)

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_95_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_95_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_95_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[7,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[7,]")
lines(new.theta.out_95_exAL_synth_DISC$exps[1,idx1], col = 'darkblue', lwd = 2)


# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[6, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[5, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[4, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'green', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'darkgreen', lwd = 1.5)
lines(idx_f, result[3,], col = 'green', lty = 2, lwd = 1)
# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[3, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[2, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[1, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'red', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'darkred', lwd = 1.5)
lines(idx_f, result[3,], col = 'red', lty = 2, lwd = 1)

idx <- safe_time_index(TT - iii, TT, TT, context = "40_figures.tt_minus_iii")

# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[1, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'red', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'darkred', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'red', lty = 2, lwd = 0.5)

# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[2, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)

# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[3, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)

# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[4, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'green', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'darkgreen', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'green', lty = 2, lwd = 0.5)

# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[5, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)


# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[6, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)


# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[7, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'blue', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'darkblue', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'blue', lty = 2, lwd = 0.5)


# # Adding quantile bands (orange) for NDLM estimation
# sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth[1,]))
# result <- apply(xbs_ndlm[1,,] + sd_ndlm * qnorm(0.5), 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'orange', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'orange', lwd = 1.5)
# lines(idx_f, result[3,], col = 'orange', lty = 2, lwd = 1)

# # Adding NDLM estimation
# d1 <- new.theta.out_50_NDLM_synth$sm_ens[[1]][8,]
# d2 <- new.theta.out_50_NDLM_synth$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 
# estim_dqlm <- new.theta.out_50_NDLM_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# lines(idx_f, estim_dqlm + sd_ndlm * qnorm(0.5), col = "orange", lwd = 2)
# lines(new.theta.out_50_NDLM_synth$exps[1,idx1] + sd_ndlm * qnorm(0.5), col = 'orange', lwd = 2)

# # Adding retrospective NDLM estimation (orange)
# result <- apply(xbs_ndlm_retro[1,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx] + sd_ndlm * qnorm(0.5), col = 'orange', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx] + sd_ndlm * qnorm(0.5), col = 'darkorange', lwd = 0.5)
# lines(1:length(idx), result[3,idx] + sd_ndlm * qnorm(0.5), col = 'orange', lty = 2, lwd = 0.5)

# Adding flood levels (horizontal dashed lines) with labels
lev_flood <- c(21.76, 19.5, 16.5, 14) 
flood_labels <- c("Major Flooding", "Moderate Flooding", "Minor Flooding", "Action")
log_flood_levels <- log(lev_flood + 1)
for (i in seq_along(log_flood_levels)) {
  abline(h = log_flood_levels[i], lwd = 1, lty = 2, col = "darkgray")
  text(x = par("usr")[2] + 0.05 * diff(par("usr")[1:2]), y = log_flood_levels[i], 
       labels = flood_labels[i], col = "gray", pos = 4, cex = 0.8, font = 2)
}

# Adding date labels on the x-axis
start_date <- dates_ts_usgs[(TT - iii)]  # Starting date
days_ahead <- ranges[1] + iii            # Number of days ahead
date_sequence <- seq.Date(from = start_date, by = "day", length.out = days_ahead + 1)
selected_dates <- date_sequence
num_ticks <- 13
tick_positions <- pretty(1:length(idx2), num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, 1:length(idx2))], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8, col = "black")

# Adding a vertical line for the forecast start date
forecast_date <- CUTOFF_DATE
forecast_position <- which(selected_dates == forecast_date)
abline(v = forecast_position, col = "black", lty = 1, lwd = 0.8)

# Adding the label for the forecast start date
text(x = forecast_position, y = par("usr")[4] + 0.03 * diff(par("usr")[3:4]), 
     labels = "forecast start date", col = "red", pos = 3, cex = 1.2, font = 2)

abline(h = 0)


# lines(Y[2,idx_y], col = 'gray', lwd = 1.5)
# points(Y[2,idx_y], col = 'gray', pch = 16, cex = 0.6)

# lines(Y[3,idx_y], col = 'gray', lwd = 1.5)
# points(Y[3,idx_y], col = 'gray', pch = 16, cex = 0.6)

# lines(colMeans(Y[,idx_y]), col = 'gray', lwd = 1.5)
# points(colMeans(Y[,idx_y]), col = 'gray', pch = 16, cex = 0.6)

dev.off()

# Setting up the indices
iii <- 30
idx1 <- (TT-iii):(TT+ranges[2])
idx2 <- (TT-iii):(TT+ranges[1])
idx_all <- (TT-iii):(TT+ranges[1])
idx_T <- (TT-iii):TT
idx_y <- (TT-iii):TT
idx_f <- ((iii+1)+1):((iii+1)+ranges[1])

# Initialize the matrix
q_exps <- matrix(NA_real_, nrow = 7, ncol=(ranges[1]))

png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
# Base plot
plot.ts((new.theta.out_95_exAL_synth_DISC$exps[2,idx_all]) * 0, ylim = c(-2.5, 2.5),
        xlab = " ", ylab = "log-flow", xaxt = "n", col = NA, lwd = 2, main = "Dynamic Quantile:  exAL")

# Adding 'Truth' points
points((1 + length(idx_y)):(length(truth) + length(idx_y)), truth, col = 'deeppink4', pch = 19, cex = 0.7, lwd = 1)

# Adding 'Observations'
lines(Y[1,idx_y], col = 'black', lwd = 1.5)
points(Y[1,idx_y], col = 'black', pch = 16, cex = 0.6)

# Adding 95th Quantile estimation
# d1 <- new.theta.out_95_exAL_synth_DISC$sm_ens[[1]][8,]
# d2 <- new.theta.out_95_exAL_synth_DISC$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_5_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_5_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_5_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[1,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[1,]")
lines(new.theta.out_5_exAL_synth_DISC$exps[1,idx1], col = 'darkred', lwd = 2)

# F_constant_disc <- FF[1:7,1,1]
# d1 <- F_constant_disc%*%new.theta.out_20_exAL_synth_DISC$sm_ens[[1]][8:14,]
# d2 <- F_constant_disc%*%new.theta.out_20_exAL_synth_DISC$sm_ens[[2]][8:14,]
# discrep <- c(d1, d2) 
# estim_dqlm <- new.theta.out_20_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[2,] <- estim_dqlm 
# lines(new.theta.out_20_exAL_synth_DISC$exps[1,idx1], col = 'purple', lwd = 2)

# F_constant_disc <- FF[1:7,1,1]
# d1 <- F_constant_disc%*%new.theta.out_35_exAL_synth_DISC$sm_ens[[1]][8:14,]
# d2 <- F_constant_disc%*%new.theta.out_35_exAL_synth_DISC$sm_ens[[2]][8:14,]
# discrep <- c(d1, d2) 
# estim_dqlm <- new.theta.out_35_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[3,] <- estim_dqlm 
# lines(new.theta.out_35_exAL_synth_DISC$exps[1,idx1], col = 'purple', lwd = 2)

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_50_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_50_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_50_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[4,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[4,]")
lines(new.theta.out_50_exAL_synth_DISC$exps[1,idx1], col = 'darkgreen', lwd = 2)

# F_constant_disc <- FF[1:7,1,1]
# d1 <- F_constant_disc%*%new.theta.out_65_exAL_synth_DISC$sm_ens[[1]][8:14,]
# d2 <- F_constant_disc%*%new.theta.out_65_exAL_synth_DISC$sm_ens[[2]][8:14,]
# discrep <- c(d1, d2) 
# estim_dqlm <- new.theta.out_65_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[5,] <- estim_dqlm 
# lines(new.theta.out_65_exAL_synth_DISC$exps[1,idx1], col = 'purple', lwd = 2)

# F_constant_disc <- FF[1:7,1,1]
# d1 <- F_constant_disc%*%new.theta.out_80_exAL_synth_DISC$sm_ens[[1]][8:14,]
# d2 <- F_constant_disc%*%new.theta.out_80_exAL_synth_DISC$sm_ens[[2]][8:14,]
# discrep <- c(d1, d2) 
# estim_dqlm <- new.theta.out_80_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[6,] <- estim_dqlm 
# lines(new.theta.out_80_exAL_synth_DISC$exps[1,idx1], col = 'purple', lwd = 2)

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_95_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_95_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_95_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[7,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[7,]")
lines(new.theta.out_95_exAL_synth_DISC$exps[1,idx1], col = 'darkblue', lwd = 2)

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[7, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'blue', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'darkblue', lwd = 1.5)
lines(idx_f, result[3,], col = 'blue', lty = 2, lwd = 1)

# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[6,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[5,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[4, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'green', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'darkgreen', lwd = 1.5)
lines(idx_f, result[3,], col = 'green', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[3,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[2,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[1, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'red', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'darkred', lwd = 1.5)
lines(idx_f, result[3,], col = 'red', lty = 2, lwd = 1)

idx <- safe_time_index(TT - iii, TT, TT, context = "40_figures.tt_minus_iii")

# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[1, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'red', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'darkred', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'red', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[2,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[3,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)

# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[4, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'green', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'darkgreen', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'green', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[5,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)


# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[6,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)


# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[7, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'blue', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'darkblue', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'blue', lty = 2, lwd = 0.5)


# # Adding quantile bands (orange) for NDLM estimation
# sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth[1,]))
# result <- apply(xbs_ndlm[1,,] + sd_ndlm * qnorm(0.5), 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'orange', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'orange', lwd = 1.5)
# lines(idx_f, result[3,], col = 'orange', lty = 2, lwd = 1)

# # Adding NDLM estimation
# d1 <- new.theta.out_50_NDLM_synth$sm_ens[[1]][8,]
# d2 <- new.theta.out_50_NDLM_synth$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 
# estim_dqlm <- new.theta.out_50_NDLM_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# lines(idx_f, estim_dqlm + sd_ndlm * qnorm(0.5), col = "orange", lwd = 2)
# lines(new.theta.out_50_NDLM_synth$exps[1,idx1] + sd_ndlm * qnorm(0.5), col = 'orange', lwd = 2)

# # Adding retrospective NDLM estimation (orange)
# result <- apply(xbs_ndlm_retro[1,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx] + sd_ndlm * qnorm(0.5), col = 'orange', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx] + sd_ndlm * qnorm(0.5), col = 'darkorange', lwd = 0.5)
# lines(1:length(idx), result[3,idx] + sd_ndlm * qnorm(0.5), col = 'orange', lty = 2, lwd = 0.5)

# Adding flood levels (horizontal dashed lines) with labels
lev_flood <- c(21.76, 19.5, 16.5, 14) 
flood_labels <- c("Major Flooding", "Moderate Flooding", "Minor Flooding", "Action")
log_flood_levels <- log(lev_flood + 1)
for (i in seq_along(log_flood_levels)) {
  abline(h = log_flood_levels[i], lwd = 1, lty = 2, col = "darkgray")
  text(x = par("usr")[2] + 0.05 * diff(par("usr")[1:2]), y = log_flood_levels[i], 
       labels = flood_labels[i], col = "gray", pos = 4, cex = 0.8, font = 2)
}

# Adding date labels on the x-axis
start_date <- dates_ts_usgs[(TT - iii)]  # Starting date
days_ahead <- ranges[1] + iii            # Number of days ahead
date_sequence <- seq.Date(from = start_date, by = "day", length.out = days_ahead + 1)
selected_dates <- date_sequence
num_ticks <- 13
tick_positions <- pretty(1:length(idx2), num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, 1:length(idx2))], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8, col = "black")

# Adding a vertical line for the forecast start date
forecast_date <- CUTOFF_DATE
forecast_position <- which(selected_dates == forecast_date)
abline(v = forecast_position, col = "black", lty = 1, lwd = 0.8)

# Adding the label for the forecast start date
text(x = forecast_position, y = par("usr")[4] + 0.03 * diff(par("usr")[3:4]), 
     labels = "forecast start date", col = "red", pos = 3, cex = 1.2, font = 2)

abline(h = 0)


# lines(Y[2,idx_y], col = 'gray', lwd = 1.5)
# points(Y[2,idx_y], col = 'gray', pch = 16, cex = 0.6)

# lines(Y[3,idx_y], col = 'gray', lwd = 1.5)
# points(Y[3,idx_y], col = 'gray', pch = 16, cex = 0.6)

# lines(colMeans(Y[,idx_y]), col = 'gray', lwd = 1.5)
# points(colMeans(Y[,idx_y]), col = 'gray', pch = 16, cex = 0.6)

dev.off()

# Setting up the indices
iii <- 30
idx1 <- (TT-iii):(TT+ranges[2])
idx2 <- (TT-iii):(TT+ranges[1])
idx_all <- (TT-iii):(TT+ranges[1])
idx_T <- (TT-iii):TT
idx_y <- (TT-iii):TT
idx_f <- ((iii+1)+1):((iii+1)+ranges[1])

# Initialize the matrix
# q_exps <- matrix(NA_real_, nrow = 7, ncol=(ranges[1]))

png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
# Base plot
plot.ts((new.theta.out_95_exAL_synth_DISC$exps[2,idx_all]) * 0, ylim = c(-2.5, 2.5),
        xlab = " ", ylab = "log-flow", xaxt = "n", col = NA, lwd = 2, main = "Dynamic Quantile: NDLM")

# Adding 'Truth' points
points((1 + length(idx_y)):(length(truth) + length(idx_y)), truth, col = 'deeppink4', pch = 19, cex = 0.7, lwd = 1)

# Adding 'Observations'
lines(Y[1,idx_y], col = 'black', lwd = 1.5)
points(Y[1,idx_y], col = 'black', pch = 16, cex = 0.6)

# # Adding 95th Quantile estimation
# d1 <- new.theta.out_95_exAL_synth$sm_ens[[1]][8,]
# d2 <- new.theta.out_95_exAL_synth$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 

# estim_dqlm <- new.theta.out_5_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[1,] <- estim_dqlm 
# lines(new.theta.out_5_exAL_synth$exps[1,idx1], col = 'darkred', lwd = 2)

# estim_dqlm <- new.theta.out_20_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[2,] <- estim_dqlm 
# lines(new.theta.out_20_exAL_synth$exps[1,idx1], col = 'purple', lwd = 2)

# estim_dqlm <- new.theta.out_35_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[3,] <- estim_dqlm 
# lines(new.theta.out_35_exAL_synth$exps[1,idx1], col = 'purple', lwd = 2)

# estim_dqlm <- new.theta.out_50_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[4,] <- estim_dqlm 
# lines(new.theta.out_50_exAL_synth$exps[1,idx1], col = 'darkgreen', lwd = 2)

# estim_dqlm <- new.theta.out_65_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[5,] <- estim_dqlm 
# lines(new.theta.out_65_exAL_synth$exps[1,idx1], col = 'purple', lwd = 2)

# estim_dqlm <- new.theta.out_80_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[6,] <- estim_dqlm 
# lines(new.theta.out_80_exAL_synth$exps[1,idx1], col = 'purple', lwd = 2)

# estim_dqlm <- new.theta.out_95_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[7,] <- estim_dqlm 
# lines(new.theta.out_95_exAL_synth$exps[1,idx1], col = 'darkblue', lwd = 2)

# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[7,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'blue', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'darkblue', lwd = 1.5)
# lines(idx_f, result[3,], col = 'blue', lty = 2, lwd = 1)

# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[6,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[5,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[4,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'green', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'darkgreen', lwd = 1.5)
# lines(idx_f, result[3,], col = 'blue', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[3,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[2,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[1,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'red', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'darkred', lwd = 1.5)
# lines(idx_f, result[3,], col = 'red', lty = 2, lwd = 1)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[1,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'red', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'darkred', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'red', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[2,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[3,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[4,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'green', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'darkgreen', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'green', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[5,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)


# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[6,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)


# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[7,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'blue', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'darkblue', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'blue', lty = 2, lwd = 0.5)


idx <- safe_time_index(TT - iii, TT, TT, context = "40_figures.tt_minus_iii")


sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth_DISC[1,]))

discrep <- ndlm_discrepancy_pair(
  new.theta.out_50_NDLM_synth_DISC,
  F_constant = FF[1:7, 1, 1],
  target_len = ranges[1],
  context = "ndlm.q50.allth"
)
estim_dqlm <- safe_exps_range(
  new.theta.out_50_NDLM_synth_DISC,
  row = 2L,
  start_idx = TT + 1L,
  end_idx = TT + ranges[1],
  context = "ndlm.q50.allth.exps2.forecast"
) - discrep
lines(idx_f, estim_dqlm + sd_ndlm * qnorm(0.5), col = "orange", lwd = 2)
lines(
  safe_exps_index(
    new.theta.out_50_NDLM_synth_DISC,
    row = 1L,
    idx = idx1,
    context = "ndlm.q50.allth.exps1.idx1"
  ) + sd_ndlm * qnorm(0.5),
  col = "orange",
  lwd = 2
)

percs <- c(0.05, 0.2, 0.35, 0.5, 0.65, 0.65, 0.8, 0.95)
for (i in 1:length(percs)) {
    pp<- percs[i]
    result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = c(0.025, 0.5, 0.975))
    lines(1:length(idx), result[1,idx] + sd_ndlm * qnorm(pp), col = 'orange', lty = 2, lwd = 0.5)
    lines(1:length(idx), result[2,idx] + sd_ndlm * qnorm(pp), col = 'darkorange', lwd = 0.5)
    lines(1:length(idx), result[3,idx] + sd_ndlm * qnorm(pp), col = 'orange', lty = 2, lwd = 0.5)
    result <- fast_row_quantiles_t(xbs_ndlm[1, , ] + sd_ndlm * qnorm(pp), probs = c(0.025, 0.5, 0.975))
    lines(idx_f, result[1,], col = 'orange', lty = 2, lwd = 1)
    lines(idx_f, result[2,], col = 'orange', lwd = 1.5)
    lines(idx_f, result[3,], col = 'orange', lty = 2, lwd = 1)
}

lev_flood <- c(21.76, 19.5, 16.5, 14) 
flood_labels <- c("Major Flooding", "Moderate Flooding", "Minor Flooding", "Action")
log_flood_levels <- log(lev_flood + 1)
for (i in seq_along(log_flood_levels)) {
  abline(h = log_flood_levels[i], lwd = 1, lty = 2, col = "darkgray")
  text(x = par("usr")[2] + 0.05 * diff(par("usr")[1:2]), y = log_flood_levels[i], 
       labels = flood_labels[i], col = "gray", pos = 4, cex = 0.8, font = 2)
}

start_date <- dates_ts_usgs[(TT - iii)]  # Starting date
days_ahead <- ranges[1] + iii            # Number of days ahead
date_sequence <- seq.Date(from = start_date, by = "day", length.out = days_ahead + 1)
selected_dates <- date_sequence
num_ticks <- 13
tick_positions <- pretty(1:length(idx2), num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, 1:length(idx2))], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8, col = "black")

# Adding a vertical line for the forecast start date
forecast_date <- CUTOFF_DATE
forecast_position <- which(selected_dates == forecast_date)
abline(v = forecast_position, col = "black", lty = 1, lwd = 0.8)

# Adding the label for the forecast start date
text(x = forecast_position, y = par("usr")[4] + 0.03 * diff(par("usr")[3:4]), 
     labels = "forecast start date", col = "red", pos = 3, cex = 1.2, font = 2)

abline(h = 0)

dev.off()

# Setting up the indices
iii <- 30
idx1 <- (TT-iii):(TT+ranges[2])
idx2 <- (TT-iii):(TT+ranges[1])
idx_all <- (TT-iii):(TT+ranges[1])
idx_T <- (TT-iii):TT
idx_y <- (TT-iii):TT
idx_f <- ((iii+1)+1):((iii+1)+ranges[1])

# Initialize the matrix
# q_exps <- matrix(NA_real_, nrow = 7, ncol=(ranges[1]))

png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
# Base plot
plot.ts((new.theta.out_95_exAL_synth_DISC$exps[2,idx_all]) * 0, ylim = c(-2.5, 2.5),
        xlab = " ", ylab = "log-flow", xaxt = "n", col = NA, lwd = 2, main = "Dynamic Quantile: NDLM")

# Adding 'Truth' points
points((1 + length(idx_y)):(length(truth) + length(idx_y)), truth, col = 'deeppink4', pch = 19, cex = 0.7, lwd = 1)

# Adding 'Observations'
lines(Y[1,idx_y], col = 'black', lwd = 1.5)
points(Y[1,idx_y], col = 'black', pch = 16, cex = 0.6)

# # Adding 95th Quantile estimation
# d1 <- new.theta.out_95_exAL_synth$sm_ens[[1]][8,]
# d2 <- new.theta.out_95_exAL_synth$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 

# estim_dqlm <- new.theta.out_5_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[1,] <- estim_dqlm 
# lines(new.theta.out_5_exAL_synth$exps[1,idx1], col = 'darkred', lwd = 2)

# estim_dqlm <- new.theta.out_20_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[2,] <- estim_dqlm 
# lines(new.theta.out_20_exAL_synth$exps[1,idx1], col = 'purple', lwd = 2)

# estim_dqlm <- new.theta.out_35_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[3,] <- estim_dqlm 
# lines(new.theta.out_35_exAL_synth$exps[1,idx1], col = 'purple', lwd = 2)

# estim_dqlm <- new.theta.out_50_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[4,] <- estim_dqlm 
# lines(new.theta.out_50_exAL_synth$exps[1,idx1], col = 'darkgreen', lwd = 2)

# estim_dqlm <- new.theta.out_65_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[5,] <- estim_dqlm 
# lines(new.theta.out_65_exAL_synth$exps[1,idx1], col = 'purple', lwd = 2)

# estim_dqlm <- new.theta.out_80_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[6,] <- estim_dqlm 
# lines(new.theta.out_80_exAL_synth$exps[1,idx1], col = 'purple', lwd = 2)

# estim_dqlm <- new.theta.out_95_exAL_synth$exps[2,(TT+1):(TT+ranges[1])] - discrep
# q_exps[7,] <- estim_dqlm 
# lines(new.theta.out_95_exAL_synth$exps[1,idx1], col = 'darkblue', lwd = 2)

# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[7,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'blue', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'darkblue', lwd = 1.5)
# lines(idx_f, result[3,], col = 'blue', lty = 2, lwd = 1)

# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[6,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[5,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[4,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'green', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'darkgreen', lwd = 1.5)
# lines(idx_f, result[3,], col = 'blue', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[3,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[2,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'purple', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'purple', lwd = 1.5)
# lines(idx_f, result[3,], col = 'purple', lty = 2, lwd = 1)
# # Adding quantile bands (blue) for 95th Quantile estimation
# result <- apply(xbs[1,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(idx_f, result[1,], col = 'red', lty = 2, lwd = 1)
# lines(idx_f, result[2,], col = 'darkred', lwd = 1.5)
# lines(idx_f, result[3,], col = 'red', lty = 2, lwd = 1)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[1,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'red', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'darkred', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'red', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[2,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[3,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[4,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'green', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'darkgreen', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'green', lty = 2, lwd = 0.5)

# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[5,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)


# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[6,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'purple', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'purple', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'purple', lty = 2, lwd = 0.5)


# # Adding retrospective quantile estimation (blue)
# result <- apply(xbs_retro[7,,], 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
# lines(1:length(idx), result[1,idx], col = 'blue', lty = 2, lwd = 0.5)
# lines(1:length(idx), result[2,idx], col = 'darkblue', lwd = 0.5)
# lines(1:length(idx), result[3,idx], col = 'blue', lty = 2, lwd = 0.5)


idx <- safe_time_index(TT - iii, TT, TT, context = "40_figures.tt_minus_iii")


sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth_DISC[1,]))

# d1 <- new.theta.out_50_NDLM_synth_DISC$sm_ens[[1]][8,]
# d2 <- new.theta.out_50_NDLM_synth_DISC$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 


F_constant_disc <- FF[1:7,1,1]
discrep <- ndlm_discrepancy_pair(
  new.theta.out_50_NDLM_synth_DISC,
  F_constant = F_constant_disc,
  target_len = ranges[1],
  context = "ndlm.q50.all3"
)

estim_dqlm <- safe_exps_range(
  new.theta.out_50_NDLM_synth_DISC,
  row = 2L,
  start_idx = TT + 1L,
  end_idx = TT + ranges[1],
  context = "ndlm.q50.all3.exps2.forecast"
) - discrep
lines(idx_f, estim_dqlm + sd_ndlm * qnorm(0.5), col = "orange", lwd = 2)
lines(
  safe_exps_index(
    new.theta.out_50_NDLM_synth_DISC,
    row = 1L,
    idx = idx1,
    context = "ndlm.q50.all3.exps1.idx1"
  ) + sd_ndlm * qnorm(0.5),
  col = "orange",
  lwd = 2
)

percs <- c(0.05, 0.5, 0.95)
for (i in 1:length(percs)) {
    pp <- percs[i]
    result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = c(0.025, 0.5, 0.975))
    lines(1:length(idx), result[1,idx] + sd_ndlm * qnorm(pp), col = 'orange', lty = 2, lwd = 0.5)
    lines(1:length(idx), result[2,idx] + sd_ndlm * qnorm(pp), col = 'darkorange', lwd = 0.5)
    lines(1:length(idx), result[3,idx] + sd_ndlm * qnorm(pp), col = 'orange', lty = 2, lwd = 0.5)
    result <- fast_row_quantiles_t(xbs_ndlm[1, , ] + sd_ndlm * qnorm(pp), probs = c(0.025, 0.5, 0.975))
    lines(idx_f, result[1,], col = 'orange', lty = 2, lwd = 1)
    lines(idx_f, result[2,], col = 'orange', lwd = 1.5)
    lines(idx_f, result[3,], col = 'orange', lty = 2, lwd = 1)
}

lev_flood <- c(21.76, 19.5, 16.5, 14) 
flood_labels <- c("Major Flooding", "Moderate Flooding", "Minor Flooding", "Action")
log_flood_levels <- log(lev_flood + 1)
for (i in seq_along(log_flood_levels)) {
  abline(h = log_flood_levels[i], lwd = 1, lty = 2, col = "darkgray")
  text(x = par("usr")[2] + 0.05 * diff(par("usr")[1:2]), y = log_flood_levels[i], 
       labels = flood_labels[i], col = "gray", pos = 4, cex = 0.8, font = 2)
}

start_date <- dates_ts_usgs[(TT - iii)]  # Starting date
days_ahead <- ranges[1] + iii            # Number of days ahead
date_sequence <- seq.Date(from = start_date, by = "day", length.out = days_ahead + 1)
selected_dates <- date_sequence
num_ticks <- 13
tick_positions <- pretty(1:length(idx2), num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, 1:length(idx2))], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8, col = "black")

# Adding a vertical line for the forecast start date
forecast_date <- CUTOFF_DATE
forecast_position <- which(selected_dates == forecast_date)
abline(v = forecast_position, col = "black", lty = 1, lwd = 0.8)

# Adding the label for the forecast start date
text(x = forecast_position, y = par("usr")[4] + 0.03 * diff(par("usr")[3:4]), 
     labels = "forecast start date", col = "red", pos = 3, cex = 1.2, font = 2)

abline(h = 0)

dev.off()

# Setting up the indices
iii <- 30
idx1 <- (TT-iii):(TT+ranges[2])
idx2 <- (TT-iii):(TT+ranges[1])
idx_all <- (TT-iii):(TT+ranges[1])
idx_T <- (TT-iii):TT
idx_y <- (TT-iii):TT
idx_f <- ((iii+1)+1):((iii+1)+ranges[1])

# Initialize the matrix
# q_exps <- matrix(NA_real_, nrow = 7, ncol=(ranges[1]))

png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
# Base plot
plot.ts((new.theta.out_95_exAL_synth_DISC$exps[2,idx_all]) * 0, ylim = c(-2.5, 2.5),
        xlab = " ", ylab = "log-flow", xaxt = "n", col = NA, lwd = 2, main = "Dynamic 95th Quantile: exAL vs NDLM")

# Adding 'Truth' points
points((1 + length(idx_y)):(length(truth) + length(idx_y)), truth, col = 'deeppink4', pch = 19, cex = 0.7, lwd = 1)

# Adding 'Observations'
lines(Y[1,idx_y], col = 'black', lwd = 1.5)
points(Y[1,idx_y], col = 'black', pch = 16, cex = 0.6)

# Adding 95th Quantile estimation
# d1 <- new.theta.out_95_exAL_synth_DISC$sm_ens[[1]][8,]
# d2 <- new.theta.out_95_exAL_synth_DISC$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 

F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_95_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_95_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_95_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[7,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[7,]")
lines(new.theta.out_95_exAL_synth_DISC$exps[1,idx1], col = 'darkblue', lwd = 2)

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[7, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'blue', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'darkblue', lwd = 1.5)
lines(idx_f, result[3,], col = 'blue', lty = 2, lwd = 1)

# Adding quantile bands (orange) for NDLM estimation
sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth_DISC[1,]))
result <- fast_row_quantiles_t(xbs_ndlm[1, , ] + sd_ndlm * qnorm(0.95), probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'orange', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'darkorange', lwd = 1.5)
lines(idx_f, result[3,], col = 'orange', lty = 2, lwd = 1)

# # Adding NDLM estimation
# d1 <- new.theta.out_50_NDLM_synth_DISC$sm_ens[[1]][8,]
# d2 <- new.theta.out_50_NDLM_synth_DISC$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 
F_constant_disc <- FF[1:7,1,1]
discrep <- ndlm_discrepancy_pair(
  new.theta.out_50_NDLM_synth_DISC,
  F_constant = F_constant_disc,
  target_len = ranges[1],
  context = "ndlm.q50.p95"
)
estim_dqlm <- safe_exps_range(
  new.theta.out_50_NDLM_synth_DISC,
  row = 2L,
  start_idx = TT + 1L,
  end_idx = TT + ranges[1],
  context = "ndlm.q50.p95.exps2.forecast"
) - discrep
lines(idx_f, estim_dqlm + sd_ndlm * qnorm(0.95), col = "orange", lwd = 2)
lines(
  safe_exps_index(
    new.theta.out_50_NDLM_synth_DISC,
    row = 1L,
    idx = idx1,
    context = "ndlm.q50.p95.exps1.idx1"
  ) + sd_ndlm * qnorm(0.95),
  col = "orange",
  lwd = 2
)


idx <- safe_time_index(TT - iii, TT, TT, context = "40_figures.tt_minus_iii")

# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[7, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'blue', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'darkblue', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'blue', lty = 2, lwd = 0.5)

# Adding retrospective NDLM estimation (orange)
result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx] + sd_ndlm * qnorm(0.95), col = 'orange', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx] + sd_ndlm * qnorm(0.95), col = 'darkorange', lwd = 0.5)
lines(1:length(idx), result[3,idx] + sd_ndlm * qnorm(0.95), col = 'orange', lty = 2, lwd = 0.5)

# Adding flood levels (horizontal dashed lines) with labels
lev_flood <- c(21.76, 19.5, 16.5, 14) 
flood_labels <- c("Major Flooding", "Moderate Flooding", "Minor Flooding", "Action")
log_flood_levels <- log(lev_flood + 1)
for (i in seq_along(log_flood_levels)) {
  abline(h = log_flood_levels[i], lwd = 1, lty = 2, col = "darkgray")
  text(x = par("usr")[2] + 0.05 * diff(par("usr")[1:2]), y = log_flood_levels[i], 
       labels = flood_labels[i], col = "gray", pos = 4, cex = 0.8, font = 2)
}

# Adding date labels on the x-axis
start_date <- dates_ts_usgs[(TT - iii)]  # Starting date
days_ahead <- ranges[1] + iii            # Number of days ahead
date_sequence <- seq.Date(from = start_date, by = "day", length.out = days_ahead + 1)
selected_dates <- date_sequence
num_ticks <- 13
tick_positions <- pretty(1:length(idx2), num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, 1:length(idx2))], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8, col = "black")

# Adding a vertical line for the forecast start date
forecast_date <- CUTOFF_DATE
forecast_position <- which(selected_dates == forecast_date)
abline(v = forecast_position, col = "black", lty = 1, lwd = 0.8)

# Adding the label for the forecast start date
text(x = forecast_position, y = par("usr")[4] + 0.03 * diff(par("usr")[3:4]), 
     labels = "forecast start date", col = "red", pos = 3, cex = 1.2, font = 2)

abline(h = 0)


dev.off()

# Setting up the indices
iii <- 30
idx1 <- (TT-iii):(TT+ranges[2])
idx2 <- (TT-iii):(TT+ranges[1])
idx_all <- (TT-iii):(TT+ranges[1])
idx_T <- (TT-iii):TT
idx_y <- (TT-iii):TT
idx_f <- ((iii+1)+1):((iii+1)+ranges[1])

# Initialize the matrix
# q_exps <- matrix(NA_real_, nrow = 7, ncol=(ranges[1]))
#
png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
# Base plot
plot.ts((new.theta.out_50_exAL_synth_DISC$exps[2,idx_all]) * 0, ylim = c(-2.5, 2.5),
        xlab = " ", ylab = "log-flow", xaxt = "n", col = NA, lwd = 2, main = "Dynamic 50th Quantile: exAL vs NDLM")

# Adding 'Truth' points
points((1 + length(idx_y)):(length(truth) + length(idx_y)), truth, col = 'deeppink4', pch = 19, cex = 0.7, lwd = 1)

# Adding 'Observations'
lines(Y[1,idx_y], col = 'black', lwd = 1.5)
points(Y[1,idx_y], col = 'black', pch = 16, cex = 0.6)

# Adding 95th Quantile estimation
# d1 <- new.theta.out_50_exAL_synth_DISC$sm_ens[[1]][8,]
# d2 <- new.theta.out_50_exAL_synth_DISC$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 
F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_50_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_50_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_50_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[7,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[7,]")
lines(new.theta.out_50_exAL_synth_DISC$exps[1,idx1], col = 'darkgreen', lwd = 2)

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[4, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'green', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'darkgreen', lwd = 1.5)
lines(idx_f, result[3,], col = 'green', lty = 2, lwd = 1)

# Adding quantile bands (orange) for NDLM estimation
sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth_DISC[1,]))
result <- fast_row_quantiles_t(xbs_ndlm[1, , ] + sd_ndlm * qnorm(0.5), probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'orange', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'orange', lwd = 1.5)
lines(idx_f, result[3,], col = 'orange', lty = 2, lwd = 1)

# Adding NDLM estimation
# d1 <- new.theta.out_50_NDLM_synth_DISC$sm_ens[[1]][8,]
# d2 <- new.theta.out_50_NDLM_synth_DISC$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 
F_constant_disc <- FF[1:7,1,1]
discrep <- ndlm_discrepancy_pair(
  new.theta.out_50_NDLM_synth_DISC,
  F_constant = F_constant_disc,
  target_len = ranges[1],
  context = "ndlm.q50.p50"
)
estim_dqlm <- safe_exps_range(
  new.theta.out_50_NDLM_synth_DISC,
  row = 2L,
  start_idx = TT + 1L,
  end_idx = TT + ranges[1],
  context = "ndlm.q50.p50.exps2.forecast"
) - discrep
lines(idx_f, estim_dqlm + sd_ndlm * qnorm(0.5), col = "orange", lwd = 2)
lines(
  safe_exps_index(
    new.theta.out_50_NDLM_synth_DISC,
    row = 1L,
    idx = idx1,
    context = "ndlm.q50.p50.exps1.idx1"
  ) + sd_ndlm * qnorm(0.5),
  col = "orange",
  lwd = 2
)


idx <- safe_time_index(TT - iii, TT, TT, context = "40_figures.tt_minus_iii")

# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[4, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'green', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'darkgreen', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'green', lty = 2, lwd = 0.5)

# Adding retrospective NDLM estimation (orange)
result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx] + sd_ndlm * qnorm(0.5), col = 'orange', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx] + sd_ndlm * qnorm(0.5), col = 'darkorange', lwd = 0.5)
lines(1:length(idx), result[3,idx] + sd_ndlm * qnorm(0.5), col = 'orange', lty = 2, lwd = 0.5)

# Adding flood levels (horizontal dashed lines) with labels
lev_flood <- c(21.76, 19.5, 16.5, 14) 
flood_labels <- c("Major Flooding", "Moderate Flooding", "Minor Flooding", "Action")
log_flood_levels <- log(lev_flood + 1)
for (i in seq_along(log_flood_levels)) {
  abline(h = log_flood_levels[i], lwd = 1, lty = 2, col = "darkgray")
  text(x = par("usr")[2] + 0.05 * diff(par("usr")[1:2]), y = log_flood_levels[i], 
       labels = flood_labels[i], col = "gray", pos = 4, cex = 0.8, font = 2)
}

# Adding date labels on the x-axis
start_date <- dates_ts_usgs[(TT - iii)]  # Starting date
days_ahead <- ranges[1] + iii            # Number of days ahead
date_sequence <- seq.Date(from = start_date, by = "day", length.out = days_ahead + 1)
selected_dates <- date_sequence
num_ticks <- 13
tick_positions <- pretty(1:length(idx2), num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, 1:length(idx2))], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8, col = "black")

# Adding a vertical line for the forecast start date
forecast_date <- CUTOFF_DATE
forecast_position <- which(selected_dates == forecast_date)
abline(v = forecast_position, col = "black", lty = 1, lwd = 0.8)

# Adding the label for the forecast start date
text(x = forecast_position, y = par("usr")[4] + 0.03 * diff(par("usr")[3:4]), 
     labels = "forecast start date", col = "red", pos = 3, cex = 1.2, font = 2)

abline(h = 0)
dev.off()

# Setting up the indices
iii <- 30
idx1 <- (TT-iii):(TT+ranges[2])
idx2 <- (TT-iii):(TT+ranges[1])
idx_all <- (TT-iii):(TT+ranges[1])
idx_T <- (TT-iii):TT
idx_y <- (TT-iii):TT
idx_f <- ((iii+1)+1):((iii+1)+ranges[1])

# Initialize the matrix
# q_exps <- matrix(NsA_real_, nrow = 7, ncol=(ranges[1]))

# Base plot
png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
plot.ts((new.theta.out_5_exAL_synth_DISC$exps[2,idx_all]) * 0, ylim = c(-2.5, 2.5),
        xlab = " ", ylab = "log-flow", xaxt = "n", col = NA, lwd = 2, main = "Dynamic 5th Quantile: exAL vs NDLM")

# Adding 'Truth' points
points((1 + length(idx_y)):(length(truth) + length(idx_y)), truth, col = 'deeppink4', pch = 19, cex = 0.7, lwd = 1)

# Adding 'Observations'
lines(Y[1,idx_y], col = 'black', lwd = 1.5)
points(Y[1,idx_y], col = 'black', pch = 16, cex = 0.6)

# Adding 95th Quantile estimation
# d1 <- new.theta.out_5_exAL_synth_DISC$sm_ens[[1]][8,]
# d2 <- new.theta.out_5_exAL_synth_DISC$sm_ens[[2]][8,]
# discrep <- c(d1, d2) 
F_constant_disc <- FF[1:7,1,1]
d1 <- F_constant_disc%*%new.theta.out_5_exAL_synth_DISC$sm_ens[[1]][8:14,]
d2 <- F_constant_disc%*%new.theta.out_5_exAL_synth_DISC$sm_ens[[2]][8:14,]
discrep <- c(d1, d2) 
estim_dqlm <- new.theta.out_5_exAL_synth_DISC$exps[2,(TT+1):(TT+ranges[1])] - discrep
q_exps[7,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[7,]")
lines(new.theta.out_5_exAL_synth_DISC$exps[1,idx1], col = 'darkred', lwd = 2)

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(xbs[1, , ], probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'red', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'darkred', lwd = 1.5)
lines(idx_f, result[3,], col = 'red', lty = 2, lwd = 1)

# Adding quantile bands (orange) for NDLM estimation
sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth_DISC[1,]))
result <- fast_row_quantiles_t(xbs_ndlm[1, , ] + sd_ndlm * qnorm(0.05), probs = c(0.025, 0.5, 0.975))
lines(idx_f, result[1,], col = 'orange', lty = 2, lwd = 1)
lines(idx_f, result[2,], col = 'orange', lwd = 1.5)
lines(idx_f, result[3,], col = 'orange', lty = 2, lwd = 1)

# Adding NDLM estimation
discrep <- ndlm_discrepancy_pair(
  new.theta.out_50_NDLM_synth_DISC,
  F_constant = FF[1:7, 1, 1],
  target_len = ranges[1],
  context = "ndlm.q50.p05"
)
estim_dqlm <- safe_exps_range(
  new.theta.out_50_NDLM_synth_DISC,
  row = 2L,
  start_idx = TT + 1L,
  end_idx = TT + ranges[1],
  context = "ndlm.q50.p05.exps2.forecast"
) - discrep
q_exps[4,] <- align_to_len(estim_dqlm, ncol(q_exps), "q_exps[4,]")
lines(idx_f, estim_dqlm + sd_ndlm * qnorm(0.05), col = "orange", lwd = 2)
lines(
  safe_exps_index(
    new.theta.out_50_NDLM_synth_DISC,
    row = 1L,
    idx = idx1,
    context = "ndlm.q50.p05.exps1.idx1"
  ) + sd_ndlm * qnorm(0.05),
  col = "orange",
  lwd = 2
)


idx <- safe_time_index(TT - iii, TT, TT, context = "40_figures.tt_minus_iii")

# Adding retrospective quantile estimation (blue)
result <- fast_row_quantiles_t(xbs_retro[1, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx], col = 'red', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx], col = 'darkred', lwd = 0.5)
lines(1:length(idx), result[3,idx], col = 'red', lty = 2, lwd = 0.5)

# Adding retrospective NDLM estimation (orange)
result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = c(0.025, 0.5, 0.975))
lines(1:length(idx), result[1,idx] + sd_ndlm * qnorm(0.05), col = 'orange', lty = 2, lwd = 0.5)
lines(1:length(idx), result[2,idx] + sd_ndlm * qnorm(0.05), col = 'darkorange', lwd = 0.5)
lines(1:length(idx), result[3,idx] + sd_ndlm * qnorm(0.05), col = 'orange', lty = 2, lwd = 0.5)

# Adding flood levels (horizontal dashed lines) with labels
lev_flood <- c(21.76, 19.5, 16.5, 14) 
flood_labels <- c("Major Flooding", "Moderate Flooding", "Minor Flooding", "Action")
log_flood_levels <- log(lev_flood + 1)
for (i in seq_along(log_flood_levels)) {
  abline(h = log_flood_levels[i], lwd = 1, lty = 2, col = "darkgray")
  text(x = par("usr")[2] + 0.05 * diff(par("usr")[1:2]), y = log_flood_levels[i], 
       labels = flood_labels[i], col = "gray", pos = 4, cex = 0.8, font = 2)
}

# Adding date labels on the x-axis
start_date <- dates_ts_usgs[(TT - iii)]  # Starting date
days_ahead <- ranges[1] + iii            # Number of days ahead
date_sequence <- seq.Date(from = start_date, by = "day", length.out = days_ahead + 1)
selected_dates <- date_sequence
num_ticks <- 13
tick_positions <- pretty(1:length(idx2), num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, 1:length(idx2))], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8, col = "black")

# Adding a vertical line for the forecast start date
forecast_date <- CUTOFF_DATE
forecast_position <- which(selected_dates == forecast_date)
abline(v = forecast_position, col = "black", lty = 1, lwd = 0.8)

# Adding the label for the forecast start date
text(x = forecast_position, y = par("usr")[4] + 0.03 * diff(par("usr")[3:4]), 
     labels = "forecast start date", col = "red", pos = 3, cex = 1.2, font = 2)

abline(h = 0)

dev.off()

p <- 7

png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
par(mfrow = c(2, 7), mar = c(2, 2, 2, 1), oma = c(0, 0, 3, 0))

colors <- c("forestgreen", "darkorange", "darkblue")

a <- 0

seq.sigma_5_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.sigma_5_exAL_synth_DISC), col = colors, main = "Sigma 05th", xlab = "Iteration", ylab = "Sigma", lwd = 2, ylim = c(0,0.2))

seq.sigma_20_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.sigma_20_exAL_synth_DISC), col = colors, main = "Sigma 20th", xlab = "Iteration", ylab = "Sigma", lwd = 2, ylim = c(0,0.2))

seq.sigma_35_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.sigma_35_exAL_synth_DISC), col = colors, main = "Sigma 35th", xlab = "Iteration", ylab = "Sigma", lwd = 2, ylim = c(0,0.2))

seq.sigma_50_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.sigma_50_exAL_synth_DISC), col = colors, main = "Sigma 50th", xlab = "Iteration", ylab = "Sigma", lwd = 2, ylim = c(0,0.2))

seq.sigma_65_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.sigma_65_exAL_synth_DISC), col = colors, main = "Sigma 65th", xlab = "Iteration", ylab = "Sigma", lwd = 2, ylim = c(0,0.2))

seq.sigma_80_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.sigma_80_exAL_synth_DISC), col = colors, main = "Sigma 80th", xlab = "Iteration", ylab = "Sigma", lwd = 2, ylim = c(0,0.2))

seq.sigma_95_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.sigma_95_exAL_synth_DISC), col = colors, main = "Sigma 95th", xlab = "Iteration", ylab = "Sigma", lwd = 2, ylim = c(0,0.2))

seq.gamma_5_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.gamma_5_exAL_synth_DISC), col = colors, main = "Gamma 05th", xlab = "Iteration", ylab = "Gamma", lwd = 2, ylim = c(-3,1))

seq.gamma_20_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.gamma_20_exAL_synth_DISC), col = colors, main = "Gamma 20th", xlab = "Iteration", ylab = "Gamma", lwd = 2, ylim = c(-3,1))

seq.gamma_35_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.gamma_35_exAL_synth_DISC), col = colors, main = "Gamma 35th", xlab = "Iteration", ylab = "Gamma", lwd = 2, ylim = c(-3,1))

seq.gamma_50_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.gamma_50_exAL_synth_DISC), col = colors, main = "Gamma 50th", xlab = "Iteration", ylab = "Gamma", lwd = 2, ylim = c(-3,1))

seq.gamma_65_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.gamma_65_exAL_synth_DISC), col = colors, main = "Gamma 65th", xlab = "Iteration", ylab = "Gamma", lwd = 2, ylim = c(-3,1))

seq.gamma_80_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.gamma_80_exAL_synth_DISC), col = colors, main = "Gamma 80th", xlab = "Iteration", ylab = "Sigma", lwd = 2, ylim = c(-3,1))

seq.gamma_95_exAL_synth_DISC[,1:a] = NaN
ts.plot(t(seq.gamma_95_exAL_synth_DISC), col = colors, main = "Gamma 95th", xlab = "Iteration", ylab = "Gamma", lwd = 2, ylim = c(-3,1))


# # Add a common legend to the plot
# # Placing the legend at the top of the first column (adjust `oma` and `mar` for space)
# mtext("Green - USGS, Orange - GLOFAS, Blue - NWS", side = 3, outer = TRUE, line = 0, cex = 0.8)
# par(mfrow = c(2, 4), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))
# # Plot each time series with the specified colors

mtext("Green - USGS, Orange - GLOFAS, Blue - NWS", side = 3, outer = TRUE, line = 0, cex = 0.8)
dev.off()
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

# Define a function to calculate quantiles for each row (source) in the dataset
calculate_quantiles <- function(data, variable_name, quantile_name, source_name) {
  quantile_values <- quantile(data, probs = c(0.025, 0.5, 0.975))
  tibble(
    variable = variable_name,
    source = source_name,
    quantile = quantile_name,
    quantile_025 = quantile_values["2.5%"],
    median = quantile_values["50%"],
    quantile_975 = quantile_values["97.5%"]
  )
}

# List of datasets and their metadata
data_sets <- list(
  gamma_50_M = list(data = samp.gamma_50_exAL_synth_DISC, quantile = "50th", variable = "Gamma"),
  gamma_95_M = list(data = samp.gamma_95_exAL_synth_DISC, quantile = "95th", variable = "Gamma"),
  gamma_05_M = list(data = samp.gamma_5_exAL_synth_DISC, quantile = "05th", variable = "Gamma"),
  gamma_20_M = list(data = samp.gamma_20_exAL_synth_DISC, quantile = "20th", variable = "Gamma"),
  gamma_35_M = list(data = samp.gamma_35_exAL_synth_DISC, quantile = "35th", variable = "Gamma"),
  gamma_65_M = list(data = samp.gamma_65_exAL_synth_DISC, quantile = "65th", variable = "Gamma"),
  gamma_80_M = list(data = samp.gamma_80_exAL_synth_DISC, quantile = "80th", variable = "Gamma"),
  sigma_50_M = list(data = samp.sigma_50_exAL_synth_DISC, quantile = "50th", variable = "Sigma"),
  sigma_95_M = list(data = samp.sigma_95_exAL_synth_DISC, quantile = "95th", variable = "Sigma"),
  sigma_05_M = list(data = samp.sigma_5_exAL_synth_DISC, quantile = "05th", variable = "Sigma"),
  sigma_20_M = list(data = samp.sigma_20_exAL_synth_DISC, quantile = "20th", variable = "Sigma"),
  sigma_35_M = list(data = samp.sigma_35_exAL_synth_DISC, quantile = "35th", variable = "Sigma"),
  sigma_65_M = list(data = samp.sigma_65_exAL_synth_DISC, quantile = "65th", variable = "Sigma"),
  sigma_80_M = list(data = samp.sigma_80_exAL_synth_DISC, quantile = "80th", variable = "Sigma")
)

# Calculate quantiles for each dataset and source
all_quantiles <- bind_rows(
  lapply(data_sets, function(item) {
    bind_rows(
      calculate_quantiles(item$data[1, ], item$variable, item$quantile, "USGS"),
      calculate_quantiles(item$data[2, ], item$variable, item$quantile, "GLOFAS"),
      calculate_quantiles(item$data[3, ], item$variable, item$quantile, "NWS")
    )
  })
)

# Print the complete table of quantiles
print(all_quantiles, n = Inf)

posterior_table_exports_enabled <- post_export_tables_enabled(default = TRUE)
posterior_table_output_dir <- if (exists("OUT_DIR", inherits = TRUE)) {
  file.path(get("OUT_DIR", inherits = TRUE), "tables")
} else {
  file.path(getwd(), "tables")
}
posterior_table_formats <- post_table_formats(default = c("csv"))
posterior_table_keep_na <- TRUE
posterior_table_keep_na_env <- tolower(trimws(Sys.getenv("ENV_SORT_KEEP_NA", "")))
if (identical(posterior_table_keep_na_env, "true")) {
  posterior_table_keep_na <- TRUE
} else if (identical(posterior_table_keep_na_env, "false")) {
  posterior_table_keep_na <- FALSE
}
posterior_table_export_manifest <- NULL

if (posterior_table_exports_enabled) {
  profile_section("figures.export_gamma_sigma_tables", {
    gs_export <- post_export_gamma_sigma_tables(
      all_quantiles = all_quantiles,
      output_dir = posterior_table_output_dir,
      ci_digits = 5L,
      write_tex = TRUE,
      table_formats = posterior_table_formats,
      keep_na = posterior_table_keep_na
    )
    posterior_table_export_manifest <<- rbind(posterior_table_export_manifest, gs_export$manifest)
  })
}


prepare_quantile_data <- function(v_d) {
  if (exists("fast_prepare_quantile_data", mode = "function")) {
    return(fast_prepare_quantile_data(v_d, probs = c(0.975, 0.5, 0.025), type = 7L, na.rm = FALSE))
  }

  v_d_transposed <- aperm(v_d, c(3, 1, 2))
  q_d_transposed <- apply(v_d_transposed, 2:3, function(x) quantile(x, probs = c(0.975, 0.5, 0.025)))
  q_d <- aperm(q_d_transposed, c(2, 3, 1))
  q_d
}
q_d_50 <- prepare_quantile_data(samp.theta_50_exAL_synth_DISC$samp_theta)
q_d_05 <- prepare_quantile_data(samp.theta_5_exAL_synth_DISC$samp_theta)
q_d_95 <- prepare_quantile_data(samp.theta_95_exAL_synth_DISC$samp_theta)

q_d_20 <- prepare_quantile_data(samp.theta_20_exAL_synth_DISC$samp_theta)
q_d_35 <- prepare_quantile_data(samp.theta_35_exAL_synth_DISC$samp_theta)
q_d_65 <- prepare_quantile_data(samp.theta_65_exAL_synth_DISC$samp_theta)
q_d_80 <- prepare_quantile_data(samp.theta_80_exAL_synth_DISC$samp_theta)

prepare_quantile_data <- function(v_d) {
  v_d_transposed <- aperm(v_d, c(3, 1, 2))
  q_d_transposed <- apply(v_d_transposed, 2:3, function(x) quantile(x, probs = c(0.975, 0.5, 0.025)))
  q_d <- aperm(q_d_transposed, c(2, 3, 1))
  return(q_d)
}

q_d_NDLM <- prepare_quantile_data(samp.theta_50_NDLM_synth_DISC$samp_theta)

# 

time_cuts <- resolve_time_cuts(
  timestamps = timestamps,
  cutoff_date = CUTOFF_DATE,
  context = "40_figures.main"
)


png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
par(mar = c(4, 4, 2, 1) + 0.1)

idx <- time_cuts[1]:time_cuts[2]
percentiles <- c(0.025, 0.5, 0.975)
######################################################################################
######################################################################################
## Base
plot.ts(idx, (new.theta.out_50_exAL_synth_DISC$exps[1,idx])*0, ylim = c(-2, 2),  type="l", lwd = 1,
        main = "Quantile Dynamics     -    2012-2016",
        xlab = " ", ylab = "log-flow", xaxt = "n")
lines(idx, Y[1,idx], col = 'black')
points(idx, Y[1,idx], col = 'gray')
points(idx, Y[1,idx], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)
######################################################################################
## 50th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[4, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'green', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkgreen', lwd=0.5)
lines(idx, result[3,idx],col = 'green', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 5th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'red', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkred', lwd=0.5)
lines(idx, result[3,idx],col = 'red', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 95th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[7, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'blue', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkblue', lwd=0.5)
lines(idx, result[3,idx],col = 'blue', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## NDLM
########
result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'orange', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkorange', lwd=0.5)
lines(idx, result[3,idx],col = 'orange', lty = 2, lwd=0.5)



######################################################################################
## 80th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[6, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 65th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[5, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 35th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[3, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 20th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[2, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################

lev_flood <- c(21.76,19.5,16.5,14) 
abline(h=log(lev_flood+1), lwd=0.5, lty = 2)
######################################################################################
# NDLM
# sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth[1,]))
# estim_dqlm <- new.theta.out_50_NDLM_synth$exps[1,idx]
# lines(idx_f, estim_dqlm+sd_ndlm*qnorm(0.5), col="orange", lwd=2)
# lines(new.theta.out_50_NDLM_synth$exps[1,idx1]+sd_ndlm*qnorm(0.5), col='orange')

selected_dates <- dates_ts_usgs[idx] 
num_ticks <- 35
tick_positions <- pretty(idx, num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
dev.off()

png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
par(mar = c(4, 4, 2, 1) + 0.1)

idx <- time_cuts[3]:time_cuts[4]
percentiles <- c(0.025, 0.5, 0.975)
######################################################################################
######################################################################################
## Base
plot.ts(idx, (new.theta.out_50_exAL_synth_DISC$exps[1,idx])*0, ylim = c(-2, 2),  type="l", lwd = 1,
        main = "Quantile Dynamics     -    2017-2019",
        xlab = " ", ylab = "log-flow", xaxt = "n")
lines(idx, Y[1,idx], col = 'black', lwd = 0.1)
points(idx, Y[1,idx], col = 'gray')
points(idx, Y[1,idx], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)
######################################################################################
## 50th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[4, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'green', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkgreen', lwd=0.5)
lines(idx, result[3,idx],col = 'green', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 5th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'red', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkred', lwd=0.5)
lines(idx, result[3,idx],col = 'red', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 95th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[7, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'blue', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkblue', lwd=0.5)
lines(idx, result[3,idx],col = 'blue', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## NDLM
########
result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'orange', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkorange', lwd=0.5)
lines(idx, result[3,idx],col = 'orange', lty = 2, lwd=0.5)



######################################################################################
## 80th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[6, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 65th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[5, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 35th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[3, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 20th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[2, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################

lev_flood <- c(21.76,19.5,16.5,14) 
abline(h=log(lev_flood+1), lwd=0.5, lty = 2)
######################################################################################
# NDLM
# sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth[1,]))
# estim_dqlm <- new.theta.out_50_NDLM_synth$exps[1,idx]
# lines(idx_f, estim_dqlm+sd_ndlm*qnorm(0.5), col="orange", lwd=2)
# lines(new.theta.out_50_NDLM_synth$exps[1,idx1]+sd_ndlm*qnorm(0.5), col='orange')

selected_dates <- dates_ts_usgs[idx] 
num_ticks <- 25
tick_positions <- pretty(idx, num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

dev.off()

png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
par(mar = c(4, 4, 2, 1) + 0.1)
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(4, 0, 0, 0))

idx <- time_cuts[1]:time_cuts[2]
percentiles <- c(0.025, 0.5, 0.975)
######################################################################################
######################################################################################
## Base
plot.ts(idx, (new.theta.out_50_exAL_synth_DISC$exps[1,idx])*0, ylim = c(-2, 2),  type="l", lwd = 1,
        main = "Quantile Dynamics     -    2012-2016",
        xlab = " ", ylab = "log-flow", xaxt = "n")
lines(idx, Y[1,idx], col = 'black', lwd = 0.1)
points(idx, Y[1,idx], col = 'gray')
points(idx, Y[1,idx], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)
######################################################################################
######################################################################################
## 50th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[4, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'green', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkgreen', lwd=0.5)
lines(idx, result[3,idx],col = 'green', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 5th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'red', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkred', lwd=0.5)
lines(idx, result[3,idx],col = 'red', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 95th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[7, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'blue', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkblue', lwd=0.5)
lines(idx, result[3,idx],col = 'blue', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## NDLM
########
result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'orange', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkorange', lwd=0.5)
lines(idx, result[3,idx],col = 'orange', lty = 2, lwd=0.5)


lev_flood <- c(21.76,19.5,16.5,14) 
abline(h=log(lev_flood+1), lwd=0.5, lty = 2)
######################################################################################
# NDLM
# sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth[1,]))
# estim_dqlm <- new.theta.out_50_NDLM_synth$exps[1,idx]
# lines(idx_f, estim_dqlm+sd_ndlm*qnorm(0.5), col="orange", lwd=2)
# lines(new.theta.out_50_NDLM_synth$exps[1,idx1]+sd_ndlm*qnorm(0.5), col='orange')

selected_dates <- dates_ts_usgs[idx] 
num_ticks <- 35
tick_positions <- pretty(idx, num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)

dev.off()


png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
par(mar = c(4, 4, 2, 1) + 0.1)
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(4, 0, 0, 0))

idx <- time_cuts[1]:time_cuts[2]
percentiles <- c(0.025, 0.5, 0.975)
######################################################################################
######################################################################################
## Base
plot.ts(idx, (new.theta.out_50_exAL_synth_DISC$exps[1,idx])*0, ylim = c(-2, 2),  type="l", lwd = 1,
        main = "Quantile Dynamics     -    2012-2016",
        xlab = " ", ylab = "log-flow", xaxt = "n")
lines(idx, Y[1,idx], col = 'black', lwd = 0.1)
points(idx, Y[1,idx], col = 'gray')
points(idx, Y[1,idx], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)
######################################################################################
######################################################################################
## 50th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[4, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'green', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkgreen', lwd=0.5)
lines(idx, result[3,idx],col = 'green', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 5th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'red', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkred', lwd=0.5)
lines(idx, result[3,idx],col = 'red', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 95th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[7, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'blue', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkblue', lwd=0.5)
lines(idx, result[3,idx],col = 'blue', lty = 2, lwd=0.5)
######################################################################################
######################################################################################


lev_flood <- c(21.76,19.5,16.5,14) 
abline(h=log(lev_flood+1), lwd=0.5, lty = 2)
######################################################################################
# NDLM
# sd_ndlm <- mean(sqrt(samp.sigma_50_NDLM_synth[1,]))
# estim_dqlm <- new.theta.out_50_NDLM_synth$exps[1,idx]
# lines(idx_f, estim_dqlm+sd_ndlm*qnorm(0.5), col="orange", lwd=2)
# lines(new.theta.out_50_NDLM_synth$exps[1,idx1]+sd_ndlm*qnorm(0.5), col='orange')

selected_dates <- dates_ts_usgs[idx] 
num_ticks <- 35
tick_positions <- pretty(idx, num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile", side = 1, outer = TRUE, line = 2, cex = 0.8)

dev.off()

png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
par(mar = c(4, 4, 2, 1) + 0.1)
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(4, 0, 0, 0))

idx <- time_cuts[3]:time_cuts[4]
percentiles <- c(0.025, 0.5, 0.975)
######################################################################################
######################################################################################
## Base
plot.ts(idx, (new.theta.out_50_exAL_synth_DISC$exps[1,idx])*0, ylim = c(-2, 2),  type="l", lwd = 1,
        main = "Quantile Dynamics     -    2017-2019",
        xlab = " ", ylab = "log-flow", xaxt = "n")
lines(idx, Y[1,idx], col = 'black', lwd = 0.1)
points(idx, Y[1,idx], col = 'gray')
points(idx, Y[1,idx], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)
######################################################################################
######################################################################################
## 50th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[4, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'green', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkgreen', lwd=0.5)
lines(idx, result[3,idx],col = 'green', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 5th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'red', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkred', lwd=0.5)
lines(idx, result[3,idx],col = 'red', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 95th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[7, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'blue', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkblue', lwd=0.5)
lines(idx, result[3,idx],col = 'blue', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## NDLM
########
result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'orange', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkorange', lwd=0.5)
lines(idx, result[3,idx],col = 'orange', lty = 2, lwd=0.5)


lev_flood <- c(21.76,19.5,16.5,14) 
abline(h=log(lev_flood+1), lwd=0.5, lty = 2)
######################################################################################

selected_dates <- dates_ts_usgs[idx] 
num_ticks <- 25
tick_positions <- pretty(idx, num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)

dev.off()

# png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
par(mar = c(4, 4, 2, 1) + 0.1)
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(4, 0, 0, 0))

idx <- time_cuts[1]:time_cuts[2]
percentiles <- c(0.025, 0.5, 0.975)
######################################################################################
######################################################################################
## Base
plot.ts(idx, (new.theta.out_50_exAL_synth_DISC$exps[1,idx])*0, ylim = c(-2, 2),  type="l", lwd = 1,
        main = "Quantile Dynamics     -    2017-2019",
        xlab = " ", ylab = "log-flow", xaxt = "n")
lines(idx, Y[1,idx], col = 'black', lwd = 0.1)
points(idx, Y[1,idx], col = 'gray')
points(idx, Y[1,idx], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)
######################################################################################
######################################################################################
## 50th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[4, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'green', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkgreen', lwd=0.5)
lines(idx, result[3,idx],col = 'green', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 5th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'red', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkred', lwd=0.5)
lines(idx, result[3,idx],col = 'red', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 95th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[7, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'blue', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkblue', lwd=0.5)
lines(idx, result[3,idx],col = 'blue', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## NDLM
########
result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'orange', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkorange', lwd=0.5)
lines(idx, result[3,idx],col = 'orange', lty = 2, lwd=0.5)


lev_flood <- c(21.76,19.5,16.5,14) 
abline(h=log(lev_flood+1), lwd=0.5, lty = 2)
######################################################################################

selected_dates <- dates_ts_usgs[idx] 
num_ticks <- 25
tick_positions <- pretty(idx, num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)

# dev.off()

idx <- safe_time_index(TT - 2000, TT, TT, context = "40_figures.tt_minus_2000")
# yy <- new.theta.out_95_exAL_synth_DISC$standard_forecast_errors[1,idx]
yy <- Y[1,idx]
yy <- (yy-mean(yy))/sd(yy)
plot.ts(yy, ylim = c(-3,3), col = 'gray')
yy <- new.theta.out_95_exAL_synth_DISC$sm[22,idx]
yy <- (yy-mean(yy))/sd(yy)
lines(yy, col = 'blue')
yy <- new.theta.out_5_exAL_synth_DISC$sm[22,idx]
yy <- 
(yy-mean(yy))/sd(yy)
lines(yy, col = 'red')
yy <- new.theta.out_50_exAL_synth_DISC$sm[22,idx]
yy <- (yy-mean(yy))/sd(yy)
lines(yy, col = 'green')


png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 60)
idx <- time_cuts[3]:time_cuts[4]
percentiles <- c(0.025, 0.5, 0.975)
######################################################################################
######################################################################################
## Base
plot.ts(idx, (new.theta.out_50_exAL_synth_DISC$exps[1,idx])*0, ylim = c(-2, 2),  type="l", lwd = 1,
        main = "Quantile Dynamics     -    2017-2019",
        xlab = " ", ylab = "log-flow", xaxt = "n")
lines(idx, Y[1,idx], col = 'black', lwd = 0.1)
points(idx, Y[1,idx], col = 'gray')
points(idx, Y[1,idx], col = 'black', pch = 19, cex = 0.5, lwd = 0.1)
######################################################################################
######################################################################################
## 50th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[4, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'green', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkgreen', lwd=0.5)
lines(idx, result[3,idx],col = 'green', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 5th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'red', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkred', lwd=0.5)
lines(idx, result[3,idx],col = 'red', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 95th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[7, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'blue', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkblue', lwd=0.5)
lines(idx, result[3,idx],col = 'blue', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## NDLM
########
result <- fast_row_quantiles_t(xbs_ndlm_retro[1, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'orange', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'darkorange', lwd=0.5)
lines(idx, result[3,idx],col = 'orange', lty = 2, lwd=0.5)

######################################################################################
## 80th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[6, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 65th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[5, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 35th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[3, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################
######################################################################################
## 20th Quantile
########
result <- fast_row_quantiles_t(xbs_retro[2, , ], probs = percentiles)
lines(idx, result[1,idx], ylim = c(0,6),col = 'purple', lty = 2, lwd=0.5)
lines(idx, result[2,idx],col = 'purple', lwd=0.5)
lines(idx, result[3,idx],col = 'purple', lty = 2, lwd=0.5)
######################################################################################

lev_flood <- c(21.76,19.5,16.5,14) 
abline(h=log(lev_flood+1), lwd=0.5, lty = 2)

selected_dates <- dates_ts_usgs[idx] 
num_ticks <- 25
tick_positions <- pretty(idx, num_ticks) 
if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")
axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]), labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

dev.off()

# Function to plot with quantiles and dates on x-axis
plot_quantile_component <- function(q_d_50, q_d_05, q_d_95, q_d_20, q_d_35, q_d_65, q_d_80, Y, idx, component, num_ticks,figure_names) {
  png(figure_names, width = 6000, height = 4000, res = 600)
  
  par(mar = c(4, 4, 2, 1) + 0.1)  # Ensure consistent margins in the function

  selected_dates <- dates_ts_usgs[idx]  # Retrieve dates corresponding to the indices

  num_ticks <- 27
  tick_positions <- pretty(idx, num_ticks)  # Using pretty() to generate nice breakpoints

  if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
  }
  
  tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")


  if (component == 1)  {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "Trend Component  -  1991-2022", xaxt = "n")
  } else if (component == 2) {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5),
        xlab = " ", ylab = "log-flow", main = "Yearly Seasonal Effect  -  1991-2022", xaxt = "n")
  } else if (component == 4) {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "6-Month Sasonal Effect  -  1991-2022", xaxt = "n")
  } else if (component == 6) {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "80-Month Sasonal Effect  -  1991-2022", xaxt = "n")
  } else if (component == 8) {
    plot(idx, Y[2, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  1991-2022", xaxt = "n")
  } else if (component == 9) {
    plot(idx, Y[3, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS -  1991-2022", xaxt = "n")
  } else if (component == 10) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  1991-2022", xaxt = "n")
  } else if (component == 11) {
   plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0, 0.1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS - 1991-2022", xaxt = "n")
  } else if (component == 12) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.04,0.0), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  1991-2022", xaxt = "n")
  } else if (component == 13) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS -  1991-2022", xaxt = "n")
 } else if (component == 14) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  1991-2022", xaxt = "n")
  } else if (component == 15) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.08,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS  -  1991-2022", xaxt = "n")
  } else if (component == 16) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.055,0.055), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  1991-2022", xaxt = "n")
  } else if (component == 17) {
   plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0, 0.1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS  -  1991-2022", xaxt = "n")
  } else if (component == 18) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.04,0.0), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  1991-2022", xaxt = "n")
  } else if (component == 19) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  1991-2022", xaxt = "n")
 } else if (component == 20) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS  -  1991-2022", xaxt = "n")
  } else if (component == 21) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.08,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  1991-2022", xaxt = "n")
  } else if (component == 22) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2,2), 
        xlab = " ", ylab = "log-flow", main = "Cummulative Transfer   -  1991-2022", xaxt = "n")
  } else if (component == 23) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(0,0.04), 
        xlab = " ", ylab = "log-flow", main = "PPT   -  1991-2022", xaxt = "n")
  } else if (component == 24) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.01,0.01), 
        xlab = " ", ylab = "log-flow", main = "Soil Misture   -  1991-2022", xaxt = "n")
  } else if (component == 25) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(0,0.001), 
        xlab = " ", ylab = "log-flow", main = "GPCA Component -  1991-2022", xaxt = "n")
  } else {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.04,0), 
        xlab = " ", ylab = "log-flow", main = "Const   -  1991-2022", xaxt = "n")
  
  } 


  lines(idx, q_d_50[component, idx, 2], col = "forestgreen", lwd = 1)
  lines(idx, q_d_50[component, idx, 1], col = "green", lwd = 0.5, lty = 2)
  lines(idx, q_d_50[component, idx, 3], col = "green", lwd = 0.5, lty = 2)

  lines(idx, q_d_05[component, idx, 2], col = "darkred", lwd = 1)
  lines(idx, q_d_05[component, idx, 1], col = "red", lwd = 0.5, lty = 2)
  lines(idx, q_d_05[component, idx, 3], col = "red", lwd = 0.5, lty = 2)

  lines(idx, q_d_95[component, idx, 2], col = "darkblue", lwd = 1)
  lines(idx, q_d_95[component, idx, 1], col = "blue", lwd = 0.5, lty = 2)
  lines(idx, q_d_95[component, idx, 3], col = "blue", lwd = 0.5, lty = 2)

  abline(h=0, col='black')

  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

  mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 
}

profile_section("figures.components_1991_2022", {
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(4, 0, 0, 0))

  idx <- ceiling(TT/10):TT
  components <- c(1:dim(q_d_50)[1])
  figure_names <- paste0("SOURCE_WORKFLOW_REFERENCE", 1:length(components), ".png")

  for (i in 1:length(components)) {
    par(mar = c(4, 4, 2, 1) + 0.1)
    plot_quantile_component(
      q_d_50, q_d_05, q_d_95,
      q_d_20, q_d_35, q_d_65, q_d_80,
      Y, idx,
      components[i], num_ticks = 8, figure_names[i]
    )
  }
})

profile_section("figures.agg_disc_1991_2022", {
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

  num_ticks <- 8
  idx <- ceiling(TT/10):TT
  figure_names <- paste0("SOURCE_WORKFLOW_REFERENCE", 1:J, ".png")

  for (j in 1:J) {
png(figure_names[j], width = 6000, height = 4000, res = 600)  
par(mar = c(4, 4, 2, 1) + 0.1)  # Ensure consistent margins in the function

selected_dates <- dates_ts_usgs[idx]  # Retrieve dates corresponding to the indices
num_ticks <- 27
tick_positions <- pretty(idx, num_ticks)  # Using pretty() to generate nice breakpoints

if (length(tick_positions) > num_ticks) {
tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")

if(j == 1){
plot(idx, Y[2, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  1991-2022", xaxt = "n")
lines(idx, q_d_discrep1_quantiles[4,idx,2], col = 'forestgreen', lwd = 1)
lines(idx, q_d_discrep1_quantiles[4,idx,1], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[4,idx,3], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[1,idx,2], col = 'darkblue', lwd = 1)
lines(idx, q_d_discrep1_quantiles[1,idx,1], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[1,idx,3], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[7,idx,2], col = 'darkred', lwd = 1)
lines(idx, q_d_discrep1_quantiles[7,idx,1], col = 'pink', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[7,idx,3], col = 'pink', lwd = 1, lty = 2)
}else{
plot(idx, Y[3, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  1991-2022", xaxt = "n")
lines(idx, q_d_discrep2_quantiles[4,idx,2], col = 'forestgreen', lwd = 1)
lines(idx, q_d_discrep2_quantiles[4,idx,1], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[4,idx,3], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[1,idx,2], col = 'darkblue', lwd = 1)
lines(idx, q_d_discrep2_quantiles[1,idx,1], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[1,idx,3], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[7,idx,2], col = 'darkred', lwd = 1)
lines(idx, q_d_discrep2_quantiles[7,idx,1], col = 'pink', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[7,idx,3], col = 'pink', lwd = 1, lty = 2)
}

abline(h=0, col='black')

axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
    labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
dev.off() 
}
})


# Function to plot with quantiles and dates on x-axis
plot_quantile_component <- function(q_d_50, q_d_05, q_d_95, q_d_20, q_d_35, q_d_65, q_d_80, Y, idx, component, num_ticks) {
  par(mar = c(4, 4, 2, 1) + 0.1)  # Ensure consistent margins in the function
  
  selected_dates <- dates_ts_usgs[idx]  # Retrieve dates corresponding to the indices

  num_ticks <- 27
  tick_positions <- pretty(idx, num_ticks)  # Using pretty() to generate nice breakpoints

  if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
  }
  
  tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")

     png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
  
  plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2,2), 
       xlab = " ", ylab = "log-flow", main = "Cummulative Transfer  -  1991-2022", xaxt = "n")
  lines(idx, q_d_50[component, idx, 2], col = "forestgreen", lwd = 1)
  lines(idx, q_d_50[component, idx, 1], col = "lightgreen", lwd = 0.5, lty = 2)
  lines(idx, q_d_50[component, idx, 3], col = "lightgreen", lwd = 0.5, lty = 2)
  abline(h=0, col='black')
  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 

   png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
  plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1.3,2.6), 
       xlab = " ", ylab = "log-flow", main = "Cummulative Transfer  -  1991-2022", xaxt = "n")
  lines(idx, q_d_05[component, idx, 2], col = "darkred", lwd = 1)
  lines(idx, q_d_05[component, idx, 1], col = "pink", lwd = 0.5, lty = 2)
  lines(idx, q_d_05[component, idx, 3], col = "pink", lwd = 0.5, lty = 2)
  abline(h=0, col='black')
  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
#   mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 

png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
  
  plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1.3,2.6), 
       xlab = " ", ylab = "log-flow", main = "Cummulative Transfer  -  1991-2022", xaxt = "n")
  lines(idx, q_d_95[component, idx, 2], col = "darkblue", lwd = 1)
  lines(idx, q_d_95[component, idx, 1], col = "lightblue", lwd = 0.5, lty = 2)
  lines(idx, q_d_95[component, idx, 3], col = "lightblue", lwd = 0.5, lty = 2)
  abline(h=0, col='black')
  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
#   mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 
}


  
# par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(4, 0, 0, 0))
par(mar = c(4, 4, 2, 1))

idx <- ceiling(TT/10):TT
trans_idx <- length(model$m0)-ppx+1
plot_quantile_component(q_d_50, q_d_05, q_d_95, 
                        q_d_20, q_d_35, q_d_65, q_d_80,
                        Y, idx, 
                        trans_idx, num_ticks = 8)



par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))


# Function to plot with quantiles and dates on x-axis
plot_quantile_component <- function(q_d_50, q_d_05, q_d_95, q_d_20, q_d_35, q_d_65, q_d_80, Y, idx, component, num_ticks) {
  par(mar = c(4, 4, 2, 1) + 0.1)  # Ensure consistent margins in the function
  
  selected_dates <- dates_ts_usgs[idx]  # Retrieve dates corresponding to the indices

  num_ticks <- 35
  tick_positions <- pretty(idx, num_ticks)  # Using pretty() to generate nice breakpoints

  if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
  }
  
  tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")

     png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
  
  plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2,2), 
       xlab = " ", ylab = "log-flow", main = "Cummulative Transfer  -  1991-2022", xaxt = "n")
  lines(idx, q_d_50[component, idx, 2], col = "forestgreen", lwd = 1)
  lines(idx, q_d_50[component, idx, 1], col = "lightgreen", lwd = 0.5, lty = 2)
  lines(idx, q_d_50[component, idx, 3], col = "lightgreen", lwd = 0.5, lty = 2)
  abline(h=0, col='black')
  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 

     png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
  
  plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1.3,2.6), 
       xlab = " ", ylab = "log-flow", main = "Cummulative Transfer  -  1991-2022", xaxt = "n")
  lines(idx, q_d_05[component, idx, 2], col = "darkred", lwd = 1)
  lines(idx, q_d_05[component, idx, 1], col = "pink", lwd = 0.5, lty = 2)
  lines(idx, q_d_05[component, idx, 3], col = "pink", lwd = 0.5, lty = 2)
  abline(h=0, col='black')
  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
#   mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 

     png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
  
  plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1.3,2.6), 
       xlab = " ", ylab = "log-flow", main = "Cummulative Transfer  -  1991-2022", xaxt = "n")
  lines(idx, q_d_95[component, idx, 2], col = "darkblue", lwd = 1)
  lines(idx, q_d_95[component, idx, 1], col = "lightblue", lwd = 0.5, lty = 2)
  lines(idx, q_d_95[component, idx, 3], col = "lightblue", lwd = 0.5, lty = 2)
  abline(h=0, col='black')
  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
#   mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 
}

par(mar = c(4, 4, 2, 1))

# idx <- ceiling(TT/10):TT
idx <- time_cuts[1]:time_cuts[2]
trans_idx <- length(model$m0)-ppx+1
plot_quantile_component(q_d_50, q_d_05, q_d_95, 
                        q_d_20, q_d_35, q_d_65, q_d_80,
                        Y, idx, 
                        trans_idx, num_ticks = 11)



par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))


# Function to plot with quantiles and dates on x-axis
plot_quantile_component <- function(q_d_50, q_d_05, q_d_95, q_d_20, q_d_35, q_d_65, q_d_80, Y, idx, component, num_ticks,figure_names) {
  png(figure_names[i], width = 6000, height = 4000, res = 600)
  
  par(mar = c(4, 4, 2, 1) + 0.1)  # Ensure consistent margins in the function
  
  selected_dates <- dates_ts_usgs[idx]  # Retrieve dates corresponding to the indices

  tick_positions <- pretty(idx, num_ticks)  # Using pretty() to generate nice breakpoints

  if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
  }
  
  tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")



  if (component == 1)  {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "Trend Component  -  2012-2016", xaxt = "n")
  } else if (component == 2) {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5),
        xlab = " ", ylab = "log-flow", main = "Yearly Seasonal Effect  -  2012-2016", xaxt = "n")
  } else if (component == 4) {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "6-Month Sasonal Effect  -  2012-2016", xaxt = "n")
  } else if (component == 6) {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "80-Month Sasonal Effect  -  2012-2016", xaxt = "n")
  } else if (component == 8) {
    plot(idx, Y[2, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  2012-2016", xaxt = "n")
  } else if (component == 9) {
    plot(idx, Y[3, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS -  2012-2016", xaxt = "n")
  } else if (component == 10) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  2012-2016", xaxt = "n")
  } else if (component == 11) {
   plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0, 0.1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS - 2012-2016", xaxt = "n")
  } else if (component == 12) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.04,0.0), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  2012-2016", xaxt = "n")
  } else if (component == 13) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS -  2012-2016", xaxt = "n")
 } else if (component == 14) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  2012-2016", xaxt = "n")
  } else if (component == 15) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.08,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS  -  2012-2016", xaxt = "n")
  } else if (component == 16) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.055,0.055), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  2012-2016", xaxt = "n")
  } else if (component == 17) {
   plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0, 0.1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS  -  2012-2016", xaxt = "n")
  } else if (component == 18) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.04,0.0), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  2012-2016", xaxt = "n")
  } else if (component == 19) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  2012-2016", xaxt = "n")
 } else if (component == 20) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS  -  2012-2016", xaxt = "n")
  } else if (component == 21) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.08,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  2012-2016", xaxt = "n")
  } else if (component == 22) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
        xlab = " ", ylab = "log-flow", main = "Cummulative Transfer   -  2012-2016", xaxt = "n")
  } else if (component == 23) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(0,0.04), 
        xlab = " ", ylab = "log-flow", main = "PPT   -  2012-2016", xaxt = "n")
  } else if (component == 24) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.01,0.01), 
        xlab = " ", ylab = "log-flow", main = "Soil Misture   -  2012-2016", xaxt = "n")
  } else if (component == 25) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(0,0.001), 
        xlab = " ", ylab = "log-flow", main = "GPCA Component -  2012-2016", xaxt = "n")
  } else {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.04,0), 
        xlab = " ", ylab = "log-flow", main = "Const   -  2012-2016", xaxt = "n")
  } 


  

  lines(idx, q_d_50[component, idx, 2], col = "forestgreen", lwd = 1)
  lines(idx, q_d_50[component, idx, 1], col = "green", lwd = 0.5, lty = 2)
  lines(idx, q_d_50[component, idx, 3], col = "green", lwd = 0.5, lty = 2)

  lines(idx, q_d_05[component, idx, 2], col = "darkred", lwd = 1)
  lines(idx, q_d_05[component, idx, 1], col = "red", lwd = 0.5, lty = 2)
  lines(idx, q_d_05[component, idx, 3], col = "red", lwd = 0.5, lty = 2)

  lines(idx, q_d_95[component, idx, 2], col = "darkblue", lwd = 1)
  lines(idx, q_d_95[component, idx, 1], col = "blue", lwd = 0.5, lty = 2)
  lines(idx, q_d_95[component, idx, 3], col = "blue", lwd = 0.5, lty = 2)
  # lines(idx, q_d_NDLM[component, idx, 2], col = "darkorange", lwd = 1)
  # lines(idx, q_d_NDLM[component, idx, 1], col = "orange", lwd = 0.5, lty = 2)
  # lines(idx, q_d_NDLM[component, idx, 3], col = "orange", lwd = 0.5, lty = 2)


  # # Retained additional lines for future use
  # lines(idx, q_d_20[component, idx, 2], col = "gold", lwd = 1)
  # lines(idx, q_d_20[component, idx, 1], col = "gold", lwd = 0.5, lty = 2)
  # lines(idx, q_d_20[component, idx, 3], col = "gold", lwd = 0.5, lty = 2)

  # lines(idx, q_d_35[component, idx, 2], col = "purple", lwd = 1)
  # lines(idx, q_d_35[component, idx, 1], col = "purple", lwd = 0.5, lty = 2)
  # lines(idx, q_d_35[component, idx, 3], col = "purple", lwd = 0.5, lty = 2)
  
  # lines(idx, q_d_65[component, idx, 2], col = "brown", lwd = 1)
  # lines(idx, q_d_65[component, idx, 1], col = "brown", lwd = 0.5, lty = 2)
  # lines(idx, q_d_65[component, idx, 3], col = "brown", lwd = 0.5, lty = 2)

  # lines(idx, q_d_80[component, idx, 2], col = "orange", lwd = 1)
  # lines(idx, q_d_80[component, idx, 1], col = "orange", lwd = 0.5, lty = 2)
  # lines(idx, q_d_80[component, idx, 3], col = "orange", lwd = 0.5, lty = 2)

  abline(h=0, col='black')

  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

  mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
  dev.off() 
}

profile_section("figures.components_2012_2016", {
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(4, 0, 0, 0))

  # idx <- ceiling(TT/10):TT
  idx <- time_cuts[1]:time_cuts[2]
  components <- c(1, 2, 4, 6, 8, 9:dim(q_d_50)[1])
  figure_names <- paste0("SOURCE_WORKFLOW_REFERENCE", 1:length(components), "_DISC.png")
  for (i in 1:length(components)) {
    par(mar = c(4, 4, 2, 1) + 0.1)
    plot_quantile_component(
      q_d_50, q_d_05, q_d_95,
      q_d_20, q_d_35, q_d_65, q_d_80,
      Y, idx,
      components[i], num_ticks = 35, figure_names
    )
  }
})

profile_section("figures.agg_disc_2012_2016", {
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

  num_ticks <- 8
  idx <- time_cuts[1]:time_cuts[2]
  figure_names <- paste0("SOURCE_WORKFLOW_REFERENCE", 1:J, ".png")

  for (j in 1:J) {
png(figure_names[j], width = 6000, height = 4000, res = 600)  
par(mar = c(4, 4, 2, 1) + 0.1)  # Ensure consistent margins in the function

selected_dates <- dates_ts_usgs[idx]  # Retrieve dates corresponding to the indices
num_ticks <- 27
tick_positions <- pretty(idx, num_ticks)  # Using pretty() to generate nice breakpoints

if (length(tick_positions) > num_ticks) {
tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")

if(j == 1){
plot(idx, Y[2, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  1991-2022", xaxt = "n")
lines(idx, q_d_discrep1_quantiles[4,idx,2], col = 'forestgreen', lwd = 1)
lines(idx, q_d_discrep1_quantiles[4,idx,1], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[4,idx,3], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[1,idx,2], col = 'darkblue', lwd = 1)
lines(idx, q_d_discrep1_quantiles[1,idx,1], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[1,idx,3], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[7,idx,2], col = 'darkred', lwd = 1)
lines(idx, q_d_discrep1_quantiles[7,idx,1], col = 'pink', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[7,idx,3], col = 'pink', lwd = 1, lty = 2)
}else{
plot(idx, Y[3, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  1991-2022", xaxt = "n")
lines(idx, q_d_discrep2_quantiles[4,idx,2], col = 'forestgreen', lwd = 1)
lines(idx, q_d_discrep2_quantiles[4,idx,1], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[4,idx,3], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[1,idx,2], col = 'darkblue', lwd = 1)
lines(idx, q_d_discrep2_quantiles[1,idx,1], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[1,idx,3], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[7,idx,2], col = 'darkred', lwd = 1)
lines(idx, q_d_discrep2_quantiles[7,idx,1], col = 'pink', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[7,idx,3], col = 'pink', lwd = 1, lty = 2)
}

abline(h=0, col='black')

axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
    labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
dev.off() 
}
})


# Function to plot with quantiles and dates on x-axis
plot_quantile_component <- function(q_d_50, q_d_05, q_d_95, q_d_20, q_d_35, q_d_65, q_d_80, Y, idx, component, num_ticks,figure_names) {
  png(figure_names[i], width = 6000, height = 4000, res = 600)
  
  par(mar = c(4, 4, 2, 1) + 0.1)  # Ensure consistent margins in the function

  
  selected_dates <- dates_ts_usgs[idx]  # Retrieve dates corresponding to the indices

  tick_positions <- pretty(idx, num_ticks)  # Using pretty() to generate nice breakpoints

  if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
  }
  
  tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")



  if (component == 1)  {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "Trend Component  -  2017-2019", xaxt = "n")
  } else if (component == 2) {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5),
        xlab = " ", ylab = "log-flow", main = "Yearly Seasonal Effect  -  2017-2019", xaxt = "n")
  } else if (component == 4) {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "6-Month Sasonal Effect  -  2017-2019", xaxt = "n")
  } else if (component == 6) {
    plot(idx, 1*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "80-Month Sasonal Effect  -  2017-2019", xaxt = "n")
  } else if (component == 8) {
    plot(idx, Y[2, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  2017-2019", xaxt = "n")
  } else if (component == 9) {
    plot(idx, Y[3, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS -  2017-2019", xaxt = "n")
  } else if (component == 10) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2.5,2.5), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  2017-2019", xaxt = "n")
  } else if (component == 11) {
   plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0, 0.1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS -  1991-2022", xaxt = "n")
  } else if (component == 12) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.04,0.0), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  2017-2019", xaxt = "n")
  } else if (component == 13) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS -  2017-2019", xaxt = "n")
 } else if (component == 14) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  2017-2019", xaxt = "n")
  } else if (component == 15) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.08,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS  -  2017-2019", xaxt = "n")
  } else if (component == 16) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.055,0.055), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  2017-2019", xaxt = "n")
  } else if (component == 17) {
   plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0, 0.1), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS  -  2017-2019", xaxt = "n")
  } else if (component == 18) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.04,0.0), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  2017-2019", xaxt = "n")
  } else if (component == 19) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  2017-2019", xaxt = "n")
 } else if (component == 20) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.05,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS  -  2017-2019", xaxt = "n")
  } else if (component == 21) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.08,0.05), 
        xlab = " ", ylab = "log-flow", main = "Discrepancy NWS-USGS -  2017-2019", xaxt = "n")
  } else if (component == 22) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
        xlab = " ", ylab = "log-flow", main = "Cummulative Transfer   -  2017-2019", xaxt = "n")
  } else if (component == 23) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(0,0.04), 
        xlab = " ", ylab = "log-flow", main = "PPT   -  2017-2019", xaxt = "n")
  } else if (component == 24) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.01,0.01), 
        xlab = " ", ylab = "log-flow", main = "Soil Misture   -  2017-2019", xaxt = "n")
  } else if (component == 25) {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(0,0.001), 
        xlab = " ", ylab = "log-flow", main = "GPCA Component -  2017-2019", xaxt = "n")
  } else {
    plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-0.04,0), 
        xlab = " ", ylab = "log-flow", main = "Const   -  2017-2019", xaxt = "n")

  } 


  lines(idx, q_d_50[component, idx, 2], col = "forestgreen", lwd = 1)
  lines(idx, q_d_50[component, idx, 1], col = "green", lwd = 0.5, lty = 2)
  lines(idx, q_d_50[component, idx, 3], col = "green", lwd = 0.5, lty = 2)

  lines(idx, q_d_05[component, idx, 2], col = "darkred", lwd = 1)
  lines(idx, q_d_05[component, idx, 1], col = "red", lwd = 0.5, lty = 2)
  lines(idx, q_d_05[component, idx, 3], col = "red", lwd = 0.5, lty = 2)

  lines(idx, q_d_95[component, idx, 2], col = "darkblue", lwd = 1)
  lines(idx, q_d_95[component, idx, 1], col = "blue", lwd = 0.5, lty = 2)
  lines(idx, q_d_95[component, idx, 3], col = "blue", lwd = 0.5, lty = 2)

  # lines(idx, q_d_NDLM[component, idx, 2], col = "darkorange", lwd = 1)
  # lines(idx, q_d_NDLM[component, idx, 1], col = "orange", lwd = 0.5, lty = 2)
  # lines(idx, q_d_NDLM[component, idx, 3], col = "orange", lwd = 0.5, lty = 2)


  # # Retained additional lines for future use
  # lines(idx, q_d_20[component, idx, 2], col = "gold", lwd = 1)
  # lines(idx, q_d_20[component, idx, 1], col = "gold", lwd = 0.5, lty = 2)
  # lines(idx, q_d_20[component, idx, 3], col = "gold", lwd = 0.5, lty = 2)

  # lines(idx, q_d_35[component, idx, 2], col = "purple", lwd = 1)
  # lines(idx, q_d_35[component, idx, 1], col = "purple", lwd = 0.5, lty = 2)
  # lines(idx, q_d_35[component, idx, 3], col = "purple", lwd = 0.5, lty = 2)
  
  # lines(idx, q_d_65[component, idx, 2], col = "brown", lwd = 1)
  # lines(idx, q_d_65[component, idx, 1], col = "brown", lwd = 0.5, lty = 2)
  # lines(idx, q_d_65[component, idx, 3], col = "brown", lwd = 0.5, lty = 2)

  # lines(idx, q_d_80[component, idx, 2], col = "orange", lwd = 1)
  # lines(idx, q_d_80[component, idx, 1], col = "orange", lwd = 0.5, lty = 2)
  # lines(idx, q_d_80[component, idx, 3], col = "orange", lwd = 0.5, lty = 2)

  abline(h=0, col='black')

  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

  mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 
}

profile_section("figures.components_2018_2020", {
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(4, 0, 0, 0))

  # idx <- ceiling(TT/10):TT
  idx <- time_cuts[3]:time_cuts[4]
  components <- c(1:dim(q_d_50)[1])
  figure_names <- paste0("SOURCE_WORKFLOW_REFERENCE", 1:length(components), "_DISC.png")
  for (i in 1:length(components)) {
    par(mar = c(4, 4, 2, 1) + 0.1)
    plot_quantile_component(
      q_d_50, q_d_05, q_d_95,
      q_d_20, q_d_35, q_d_65, q_d_80,
      Y, idx,
      components[i], num_ticks = 25, figure_names
    )
  }
})

profile_section("figures.agg_disc_2018_2020", {
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

  num_ticks <- 8
  idx <- time_cuts[3]:time_cuts[4]
  figure_names <- paste0("SOURCE_WORKFLOW_REFERENCE", 1:J, ".png")

  for (j in 1:J) {
png(figure_names[j], width = 6000, height = 4000, res = 600)  
par(mar = c(4, 4, 2, 1) + 0.1)  # Ensure consistent margins in the function

selected_dates <- dates_ts_usgs[idx]  # Retrieve dates corresponding to the indices
num_ticks <- 27
tick_positions <- pretty(idx, num_ticks)  # Using pretty() to generate nice breakpoints

if (length(tick_positions) > num_ticks) {
tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
}
tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")

if(j == 1){
plot(idx, Y[2, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  1991-2022", xaxt = "n")
lines(idx, q_d_discrep1_quantiles[4,idx,2], col = 'forestgreen', lwd = 1)
lines(idx, q_d_discrep1_quantiles[4,idx,1], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[4,idx,3], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[1,idx,2], col = 'darkblue', lwd = 1)
lines(idx, q_d_discrep1_quantiles[1,idx,1], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[1,idx,3], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[7,idx,2], col = 'darkred', lwd = 1)
lines(idx, q_d_discrep1_quantiles[7,idx,1], col = 'pink', lwd = 1, lty = 2)
lines(idx, q_d_discrep1_quantiles[7,idx,3], col = 'pink', lwd = 1, lty = 2)
}else{
plot(idx, Y[3, idx]-Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1,1), 
xlab = " ", ylab = "log-flow", main = "Discrepancy GloFAS-USGS  -  1991-2022", xaxt = "n")
lines(idx, q_d_discrep2_quantiles[4,idx,2], col = 'forestgreen', lwd = 1)
lines(idx, q_d_discrep2_quantiles[4,idx,1], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[4,idx,3], col = 'green', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[1,idx,2], col = 'darkblue', lwd = 1)
lines(idx, q_d_discrep2_quantiles[1,idx,1], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[1,idx,3], col = 'lightblue', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[7,idx,2], col = 'darkred', lwd = 1)
lines(idx, q_d_discrep2_quantiles[7,idx,1], col = 'pink', lwd = 1, lty = 2)
lines(idx, q_d_discrep2_quantiles[7,idx,3], col = 'pink', lwd = 1, lty = 2)
}

abline(h=0, col='black')

axis(1, at = tick_positions, labels = FALSE) 
text(x = tick_positions, y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
    labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
dev.off() 
}
})


# Function to plot with quantiles and dates on x-axis
plot_quantile_component <- function(q_d_50, q_d_05, q_d_95, q_d_20, q_d_35, q_d_65, q_d_80, Y, idx, component, num_ticks) {
  par(mar = c(4, 4, 2, 1) + 0.1)  # Ensure consistent margins in the function
  
  selected_dates <- dates_ts_usgs[idx]  # Retrieve dates corresponding to the indices

  num_ticks <- 25
  tick_positions <- pretty(idx, num_ticks)  # Using pretty() to generate nice breakpoints

  if (length(tick_positions) > num_ticks) {
    tick_positions <- tick_positions[seq(1, length(tick_positions), length.out = num_ticks)]
  }
  
  tick_labels <- format(selected_dates[match(tick_positions, idx)], "%Y-%m-%d")
     png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
  
  plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-2,2), 
       xlab = " ", ylab = "log-flow", main = "Cummulative Transfer  -  1991-2022", xaxt = "n")
  lines(idx, q_d_50[component, idx, 2], col = "forestgreen", lwd = 1)
  lines(idx, q_d_50[component, idx, 1], col = "lightgreen", lwd = 0.5, lty = 2)
  lines(idx, q_d_50[component, idx, 3], col = "lightgreen", lwd = 0.5, lty = 2)
  abline(h=0, col='black')
  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 

     png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
  
  plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1.3,2.6), 
       xlab = " ", ylab = "log-flow", main = "Cummulative Transfer  -  1991-2022", xaxt = "n")
  lines(idx, q_d_05[component, idx, 2], col = "darkred", lwd = 1)
  lines(idx, q_d_05[component, idx, 1], col = "pink", lwd = 0.5, lty = 2)
  lines(idx, q_d_05[component, idx, 3], col = "pink", lwd = 0.5, lty = 2)
  abline(h=0, col='black')
  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
#   mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 

     png("SOURCE_WORKFLOW_REFERENCE", width = 6000, height = 4000, res = 600)
  
  plot(idx, 0*Y[1, idx], type = "l", col = "gray", lwd = 2, ylim=c(-1.3,2.6), 
       xlab = " ", ylab = "log-flow", main = "Cummulative Transfer  -  1991-2022", xaxt = "n")
  lines(idx, q_d_95[component, idx, 2], col = "darkblue", lwd = 1)
  lines(idx, q_d_95[component, idx, 1], col = "lightblue", lwd = 0.5, lty = 2)
  lines(idx, q_d_95[component, idx, 3], col = "lightblue", lwd = 0.5, lty = 2)
  abline(h=0, col='black')
  axis(1, at = tick_positions, labels = FALSE) 
  text(x = tick_positions, y = par("usr")[3] - 0.025 * diff(par("usr")[3:4]),
       labels = tick_labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
#   mtext("Forest Green: 50th Quantile | Dark Red: 5th Quantile | Dark Blue: 95th Quantile | Orange: Average", side = 1, outer = TRUE, line = 2, cex = 0.8)
  
  dev.off() 
}

par(mar = c(4, 4, 2, 1))

# idx <- ceiling(TT/10):TT
idx <- time_cuts[3]:time_cuts[4]
trans_idx <- length(model$m0)-ppx+1
plot_quantile_component(q_d_50, q_d_05, q_d_95, 
                        q_d_20, q_d_35, q_d_65, q_d_80,
                        Y, idx, 
                        trans_idx, num_ticks = 11)



par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))


# Dimension-safe JSD wrapper: delegates to core helper with explicit context labels.
compute_jsd <- function(p_sample, gridsize = c(100, 100, 100), context = "jsd.sample") {
  compute_jsd_to_standard_normal(p_sample, gridsize = gridsize, context = context)
}

# List of p_sample matrices
sample_list <- list(
  t(new.theta.out_50_NDLM_synth_DISC$standard_forecast_errors),
  t(new.theta.out_5_exAL_synth_DISC$standard_forecast_errors),
  t(new.theta.out_20_exAL_synth_DISC$standard_forecast_errors),
  t(new.theta.out_35_exAL_synth_DISC$standard_forecast_errors),
  t(new.theta.out_50_exAL_synth_DISC$standard_forecast_errors),
  t(new.theta.out_65_exAL_synth_DISC$standard_forecast_errors),
  t(new.theta.out_80_exAL_synth_DISC$standard_forecast_errors),
  t(new.theta.out_95_exAL_synth_DISC$standard_forecast_errors)
)

# Corresponding names for clarity in output
sample_names <- c(
  "new.theta.out_50_NDLM_synth$standard_forecast_errors",
  "new.theta.out_5_exAL_synth$standard_forecast_errors",
  "new.theta.out_20_exAL_synth$standard_forecast_errors",
  "new.theta.out_35_exAL_synth$standard_forecast_errors",
  "new.theta.out_50_exAL_synth$standard_forecast_errors",
  "new.theta.out_65_exAL_synth$standard_forecast_errors",
  "new.theta.out_80_exAL_synth$standard_forecast_errors",
  "new.theta.out_95_exAL_synth$standard_forecast_errors"
)

# Compute JSD for each sample and print the results
results <- list()

for (i in 1:length(sample_list)) {
  cat("Computing JSD for:", sample_names[i], "\n")
  js_divergence <- profile_section(
    paste0("figures.compute_jsd.", i),
    compute_jsd(
      sample_list[[i]],
      gridsize = c(100, 100, 100),
      context = sample_names[i]
    )
  )
  cat("Jensen-Shannon divergence for", sample_names[i], "is", js_divergence, "\n\n")
  results[[sample_names[i]]] <- js_divergence
}

# Print final results
cat("Final JSD Results:\n")
print(results)


matrix_df <- as.data.frame(X)
matrix_df <- cbind(Timestamp = timestamps, matrix_df)
write.csv(matrix_df, "factors.csv", row.names = FALSE)


# Function definitions
k_lb_tot_effect <- function(eps, lambda, tef) {
  lb <- (log(eps) - log(abs(tef))) / log(lambda)
  return(ceiling(lb))
}

# Calculate tef
p_tot <- dim(new.theta.out_20_exAL_synth_DISC$sm)[1]
reg_idx <- (p_tot - (ppx - 1) + 1):(p_tot)

# Define lambda
lambda <- 0.99

# Define epsilon values
epsilon_values <- c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01, 0.005, 0.001)

# Calculate the mean of k_lb_tot_effect for each epsilon for each tef
compute_results <- function(tef) {
  sapply(epsilon_values, function(eps) max(mean(k_lb_tot_effect(eps, lambda, tef)), 0))
}

# Calculate tef for different quantiles
retro_idx <- 1:TT
tef_5 <- rowSums(X * t(new.theta.out_5_exAL_synth_DISC$sm[reg_idx,retro_idx]))
tef_50 <- rowSums(X * t(new.theta.out_50_exAL_synth_DISC$sm[reg_idx,retro_idx]))
tef_95 <- rowSums(X * t(new.theta.out_95_exAL_synth_DISC$sm[reg_idx,retro_idx]))
tef_20 <- rowSums(X * t(new.theta.out_20_exAL_synth_DISC$sm[reg_idx,retro_idx]))

# Calculate results for each tef
results_5 <- compute_results(tef_5)
results_50 <- compute_results(tef_50)
results_95 <- compute_results(tef_95)
results_20 <- compute_results(tef_20)

# Create data frames for plotting
plot_data_5_50_95 <- data.frame(
  epsilon = rep(epsilon_values, 3),
  mean_k_lb = c(results_5, results_50, results_95),
  Quantile = factor(rep(c("5th", "50th", "95th"), each = length(epsilon_values)))
)

plot_data_20 <- data.frame(
  epsilon = epsilon_values,
  mean_k_lb = results_20,
  Quantile = "20th"
)

# Plot for 5th, 50th, and 95th quantiles
p1 <- ggplot(plot_data_5_50_95, aes(x = epsilon, y = mean_k_lb, color = Quantile)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_log10() +
  labs(title = "Total Effect Error Margin vs. Average k-step Ahead",
       x = expression(epsilon),
       y = "Average k-step Ahead to Make it Negligible") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = c("darkgreen", "darkred", "darkblue"))

# Save the plot for 5th, 50th, and 95th quantiles
ggsave(filename = "SOURCE_WORKFLOW_REFERENCE", plot = p1, width = 8, height = 6, dpi = 900)

# Plot for 20th quantile
p2 <- ggplot(plot_data_20, aes(x = epsilon, y = mean_k_lb, color = Quantile)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_log10() +
  labs(title = "Total Effect Error Margin vs. Average k-step Ahead",
       x = expression(epsilon),
       y = "Average k-step Ahead to Make it Negligible") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = c("darkorange"))

# Save the plot for 20th quantile
ggsave(filename = "SOURCE_WORKFLOW_REFERENCE", plot = p2, width = 8, height = 6, dpi = 900)


dim(samp.theta_50_exAL_synth_DISC$samp_theta)
dim(samp.sts_50_exAL_synth_DISC)
dim(samp.gamma_50_exAL_synth_DISC)
dim(samp.sigma_50_exAL_synth_DISC)

inverse_cdf_AL <- function(U, mu, sigma, p) {
  ifelse(U < p, 
         mu + (sigma / (1 - p)) * log(U / p), 
         mu - (sigma / p) * log((1 - U) / (1 - p)))
}

L_fn <- function(p0) {
  stats::uniroot(function(gam) exp(log_g(gam)) - (1 - p0), c(-1000, 0))$root
}

U_fn <- function(p0) {
  stats::uniroot(function(gam) exp(log_g(gam)) - p0, c(0, 1000))$root
}

p_fn <- function(p0, gam) {
  (p0 - as.numeric(gam < 0)) / exp(log_g(gam)) + as.numeric(gam < 0)
}

A_fn <- function(p0, gam) {
  temp_p <- p_fn(p0, gam)
  (1 - 2 * temp_p) / (temp_p * (1 - temp_p))
}

B_fn <- function(p0, gam) {
  temp_p <- p_fn(p0, gam)
  2 / (temp_p * (1 - temp_p))
}

C_fn <- function(p0, gam) {
  temp_p <- p_fn(p0, gam)
  (as.numeric(gam > 0) - temp_p)^(-1)
}


# Set index for parameters
j <- 1
# Set seed for reproducibility
set.seed(777)

# Define the inverse CDF function for the Asymmetric Laplace distribution
inverse_cdf_AL <- function(U, mu, sigma, p) {
  ifelse(U < p, 
         mu + (sigma / (1 - p)) * log(U / p), 
         mu - (sigma / p) * log((1 - U) / (1 - p)))
}

# Define auxiliary functions
p_fn <- function(p0, gam) {
  (p0 - as.numeric(gam < 0)) / exp(log_g(gam)) + as.numeric(gam < 0)
}

C_fn <- function(p0, gam) {
  temp_p <- p_fn(p0, gam)
  (as.numeric(gam > 0) - temp_p)^(-1)
}

# Define quantile levels
p0_5 <- 0.05
p0_20 <- 0.20
p0_35 <- 0.35
p0_50 <- 0.50
p0_65 <- 0.65
p0_80 <- 0.80
p0_95 <- 0.95

profile_section("figures.posterior_y_post_samples", {
# Generate uniform values for inverse sampling, with dimensions [time steps x samples]
n_rows_50 <- dim(samp.theta_50_exAL_synth_DISC$samp_theta)[3]  # Samples
n_cols_50 <- dim(samp.theta_50_exAL_synth_DISC$samp_theta)[2]  # Time steps
u_values_5 <- matrix(runif(n_rows_50 * n_cols_50), ncol = n_rows_50)   # Uniform values for 5th quantile
u_values_20 <- matrix(runif(n_rows_50 * n_cols_50), ncol = n_rows_50)  # Uniform values for 20th quantile
u_values_35 <- matrix(runif(n_rows_50 * n_cols_50), ncol = n_rows_50)  # Uniform values for 35th quantile
u_values_50 <- matrix(runif(n_rows_50 * n_cols_50), ncol = n_rows_50)  # Uniform values for 50th quantile
u_values_65 <- matrix(runif(n_rows_50 * n_cols_50), ncol = n_rows_50)  # Uniform values for 65th quantile
u_values_80 <- matrix(runif(n_rows_50 * n_cols_50), ncol = n_rows_50)  # Uniform values for 80th quantile
u_values_95 <- matrix(runif(n_rows_50 * n_cols_50), ncol = n_rows_50)  # Uniform values for 95th quantile

# Function to compute posterior samples for each quantile
compute_y_post <- function(p0, samp_theta, samp_sts, samp_gamma, samp_sigma, FF, u_values) {
  # Set index for parameters
  j <- 1

  # Extract parameter arrays for computation
  th <- samp_theta$samp_theta                # [parameters x time steps x samples]
  stj <- samp_sts[j, , ]                     # [time steps x samples]
  gamj <- samp_gamma[j, ]                    # [samples]
  sigj <- samp_sigma[j, ]                    # [samples]
  p_exAL <- p_fn(p0, gamj)

  # Reshape FF to align dimensions for matrix multiplication across time steps
  TT <- dim(samp_theta$samp_theta)[2]        # Number of time steps
  FF_reshaped <- array(FF[, j, ], dim = c(dim(samp_theta$samp_theta)[1], 1, TT))

  # Compute XB by applying the matrix multiplication for each time step `t`
  result_list <- lapply(1:TT, function(t) t(FF_reshaped[,,t]) %*% th[,t,])
  XB <- do.call(rbind, result_list)          # [time steps x samples]

  # Compute `mu` using the XB result and additional parameters
  mu <- XB + sigj * abs(gamj) * C_fn(p0, gamj) * stj

  # Compute posterior samples
  inverse_cdf_AL(u_values, mu, sigj, p_exAL)
}

# Compute y_post for each quantile
y_post_5 <- compute_y_post(p0_5, samp.theta_5_exAL_synth_DISC, samp.sts_5_exAL_synth_DISC,
                           samp.gamma_5_exAL_synth_DISC, samp.sigma_5_exAL_synth_DISC, FF, u_values_5)
y_post_5 <- t(y_post_5)
y_post_20 <- compute_y_post(p0_20, samp.theta_20_exAL_synth_DISC, samp.sts_20_exAL_synth_DISC,
                            samp.gamma_20_exAL_synth_DISC, samp.sigma_20_exAL_synth_DISC, FF, u_values_20)
y_post_20 <- t(y_post_20)
y_post_35 <- compute_y_post(p0_35, samp.theta_35_exAL_synth_DISC, samp.sts_35_exAL_synth_DISC,
                            samp.gamma_35_exAL_synth_DISC, samp.sigma_35_exAL_synth_DISC, FF, u_values_35)
y_post_35 <- t(y_post_35)
y_post_50 <- compute_y_post(p0_50, samp.theta_50_exAL_synth_DISC, samp.sts_50_exAL_synth_DISC,
                            samp.gamma_50_exAL_synth_DISC, samp.sigma_50_exAL_synth_DISC, FF, u_values_50)
y_post_50 <- t(y_post_50)
y_post_65 <- compute_y_post(p0_65, samp.theta_65_exAL_synth_DISC, samp.sts_65_exAL_synth_DISC,
                            samp.gamma_65_exAL_synth_DISC, samp.sigma_65_exAL_synth_DISC, FF, u_values_65)
y_post_65 <- t(y_post_65)
y_post_80 <- compute_y_post(p0_80, samp.theta_80_exAL_synth_DISC, samp.sts_80_exAL_synth_DISC,
                            samp.gamma_80_exAL_synth_DISC, samp.sigma_80_exAL_synth_DISC, FF, u_values_80)
y_post_80 <- t(y_post_80)
y_post_95 <- compute_y_post(p0_95, samp.theta_95_exAL_synth_DISC, samp.sts_95_exAL_synth_DISC,
                            samp.gamma_95_exAL_synth_DISC, samp.sigma_95_exAL_synth_DISC, FF, u_values_95)
y_post_95 <- t(y_post_95)


exp_y_post_5 <- exp(y_post_5)
exp_y_post_20 <- exp(y_post_20)
exp_y_post_35 <- exp(y_post_35)
exp_y_post_50 <- exp(y_post_50)
exp_y_post_65 <- exp(y_post_65)
exp_y_post_80 <- exp(y_post_80)
exp_y_post_95 <- exp(y_post_95)

n.samp_available <- nrow(exp_y_post_50)
n.samp <- resolve_post_effective_n_samp(
  n_available = n.samp_available,
  TT = TT,
  context = "figures.posterior"
)
if (n.samp < n.samp_available) {
  y_post_5 <- cap_sample_rows(y_post_5, n.samp)
  y_post_20 <- cap_sample_rows(y_post_20, n.samp)
  y_post_35 <- cap_sample_rows(y_post_35, n.samp)
  y_post_50 <- cap_sample_rows(y_post_50, n.samp)
  y_post_65 <- cap_sample_rows(y_post_65, n.samp)
  y_post_80 <- cap_sample_rows(y_post_80, n.samp)
  y_post_95 <- cap_sample_rows(y_post_95, n.samp)

  exp_y_post_5 <- cap_sample_rows(exp_y_post_5, n.samp)
  exp_y_post_20 <- cap_sample_rows(exp_y_post_20, n.samp)
  exp_y_post_35 <- cap_sample_rows(exp_y_post_35, n.samp)
  exp_y_post_50 <- cap_sample_rows(exp_y_post_50, n.samp)
  exp_y_post_65 <- cap_sample_rows(exp_y_post_65, n.samp)
  exp_y_post_80 <- cap_sample_rows(exp_y_post_80, n.samp)
  exp_y_post_95 <- cap_sample_rows(exp_y_post_95, n.samp)
  invisible(gc(verbose = FALSE))
}

idx <- safe_time_index(TT - 500, TT, TT, context = "40_figures.tt_minus_500")
n.samp <- min(n.samp, dim(samp.theta_50_exAL_synth_DISC$samp_theta)[3])
plot.ts(exp(Y[1,idx]), ylim = c(0,7))
matlines(t(exp_y_post_50[,idx]), lwd = 0.1, col='forestgreen')
matlines(t(exp_y_post_95[,idx]), lwd = 0.1, col='darkblue')
matlines(t(exp_y_post_5[,idx]), lwd = 0.1, col='darkred')
lines(exp(Y[1,idx]))

q50 <- fast_col_quantiles_t(y_post_50, probs = c(0.5, 0.025, 0.5, 0.975))
m50 <- colMeans((y_post_50))
q5 <- fast_col_quantiles_t(y_post_5, probs = c(0.05, 0.025, 0.5, 0.975))
m5 <- colMeans((y_post_5))
q95 <- fast_col_quantiles_t(y_post_95, probs = c(0.95, 0.025, 0.5, 0.975))
m95 <- colMeans((y_post_95))
q20 <- fast_col_quantiles_t(y_post_20, probs = c(0.2, 0.025, 0.5, 0.975))
m20 <- colMeans((y_post_20))
q35 <- fast_col_quantiles_t(y_post_35, probs = c(0.35, 0.025, 0.5, 0.975))
m35 <- colMeans((y_post_35))
q65 <- fast_col_quantiles_t(y_post_65, probs = c(0.65, 0.025, 0.5, 0.975))
m65 <- colMeans((y_post_65))
q80 <- fast_col_quantiles_t(y_post_80, probs = c(0.8, 0.025, 0.5, 0.975))
m80 <- colMeans((y_post_80))

exp_q50 <- fast_col_quantiles_t(exp_y_post_50, probs = c(0.5, 0.025, 0.5, 0.975))
exp_m50 <- colMeans((exp_y_post_50))
exp_q5 <- fast_col_quantiles_t(exp_y_post_5, probs = c(0.05, 0.025, 0.5, 0.975))
exp_m5 <- colMeans((exp_y_post_5))
exp_q95 <- fast_col_quantiles_t(exp_y_post_95, probs = c(0.95, 0.025, 0.5, 0.975))
exp_m95 <- colMeans((exp_y_post_95))
exp_q20 <- fast_col_quantiles_t(exp_y_post_20, probs = c(0.2, 0.025, 0.5, 0.975))
exp_m20 <- colMeans((exp_y_post_20))
exp_q35 <- fast_col_quantiles_t(exp_y_post_35, probs = c(0.35, 0.025, 0.5, 0.975))
exp_m35 <- colMeans((exp_y_post_35))
exp_q65 <- fast_col_quantiles_t(exp_y_post_65, probs = c(0.65, 0.025, 0.5, 0.975))
exp_m65 <- colMeans((exp_y_post_65))
exp_q80 <- fast_col_quantiles_t(exp_y_post_80, probs = c(0.8, 0.025, 0.5, 0.975))
exp_m80 <- colMeans((exp_y_post_80))


# Define the time range and common y-axis limits
idx <- safe_time_index(TT - 500, TT, TT, context = "40_figures.tt_minus_500")
n.samp <- min(n.samp, dim(samp.theta_50_exAL_synth_DISC$samp_theta)[3])
ylim_range <- c(0, 7)
output_dir <- "SOURCE_WORKFLOW_REFERENCE"

# Create a function for plotting posterior predictive samples with legends and labels (without the new.theta.out_p0_exAL_synth$exps)
plot_posterior_samples <- function(y_post, q, quantile_label, p0, color_post, color_quantile, ylim_range, idx) {
  par(mar = c(4, 4, 2, 2))  # Adjust margins
  plot(timestamps[idx], exp(Y[1, idx]), type = "l", ylim = ylim_range, 
       col = 'black', lwd = 1.5, xlab = "Date", ylab = "log(Streamflow)", 
       main = paste(quantile_label, "Qntl.: Post. Pred. Samp."))
  
  matlines(timestamps[idx], t(y_post[, idx]), lwd = 0.1, col = color_post)
  
  lines(timestamps[idx], q[1, idx], lwd = 1, col = color_quantile)  # Post. Pred. Quantile
  lines(timestamps[idx], exp(Y[1, idx]), lwd = 1.5, col = 'black')  # True Obs. Streamflow
  
  # Add a legend, slightly adjusted to the left
  legend(x = "topright", inset = c(0.2, 0), legend = c("Post. Pred. Samp.", paste0(p0, "th Post. Pred. Qntl."), "log(Streamflow)"),
         col = c(color_post, color_quantile, 'black'), lwd = c(1, 1, 1.5), bty = "n")
}

# Function to save individual plots (without new.theta.out_p0_exAL_synth$exps)
save_individual_plot <- function(filename, y_post, q, quantile_label, p0, color_post, color_quantile) {
  png(filename = paste0(output_dir, filename), width = 2000, height = 1200, res = 300)
  plot_posterior_samples(y_post, q, quantile_label, p0, color_post, color_quantile, ylim_range, idx)
  dev.off()
}

# Save individual plots
save_individual_plot("plot_50th_quantile_DISC.png", exp_y_post_50, exp_q50, "50th", "50", "forestgreen", "orange")
save_individual_plot("plot_95th_quantile_DISC.png", exp_y_post_95, exp_q95, "95th", "95", "darkblue", "orange")
save_individual_plot("plot_5th_quantile_DISC.png", exp_y_post_5, exp_q5, "5th", "5", "darkred", "orange")

# Save plot with all posterior samples together (without new.theta.out_p0_exAL_synth$exps)
png(filename = paste0(output_dir, "/plot_all_quantiles_combined_DISC.png"), width = 2000, height = 1200, res = 300)
par(mar = c(4, 4, 2, 2))  # Adjust margins
plot(timestamps[idx], exp(Y[1, idx]), type = "l", ylim = ylim_range, 
     col = 'black', lwd = 1.5, xlab = "Date", ylab = "log(Streamflow)", 
     main = "Post. Pred. Samp.: 50th, 95th, and 5th Qntls.")

matlines(timestamps[idx], t(exp_y_post_50[, idx]), lwd = 0.1, col = 'forestgreen')
matlines(timestamps[idx], t(exp_y_post_95[, idx]), lwd = 0.1, col = 'darkblue')
matlines(timestamps[idx], t(exp_y_post_5[, idx]), lwd = 0.1, col = 'darkred')

lines(timestamps[idx], exp(Y[1, idx]), lwd = 1.5, col = 'black')  # True Obs. Streamflow
legend(x = "topright", inset = c(0.2, 0), legend = c("50th Post. Pred. Samp.", "95th Post. Pred. Samp.", "5th Post. Pred. Samp.", "log(Streamflow)"),
       col = c("forestgreen", "darkblue", "darkred", 'black'), lwd = c(1, 1, 1, 1.5), bty = "n")
dev.off()

# Save matrix plot with 3 rows (without new.theta.out_p0_exAL_synth$exps)
png(filename = paste0(output_dir, "/plot_3_row_matrix_DISC.png"), width = 2000, height = 1600, res = 300)
par(mfrow = c(3, 1), mar = c(4, 4, 2, 2))  # Set the layout to 3 rows and 1 column, adjust margins
plot_posterior_samples(exp_y_post_50, exp_q50, "50th", "50", "forestgreen", "orange", ylim_range, idx)
plot_posterior_samples(exp_y_post_95, exp_q95, "95th", "95", "darkblue", "orange", ylim_range, idx)
plot_posterior_samples(exp_y_post_5, exp_q5, "5th", "5", "darkred", "orange", ylim_range, idx)
dev.off()

# Save matrix plot with combined plot on top and 3 quantiles below (without new.theta.out_p0_exAL_synth$exps)
png(filename = paste0(output_dir, "/plot_combined_matrix_DISC.png"), width = 2000, height = 1800, res = 300)
par(mfrow = c(4, 1), mar = c(4, 4, 2, 2))  # Set the layout to 4 rows and 1 column, adjust margins
plot(timestamps[idx], exp(Y[1, idx]), type = "l", ylim = ylim_range, 
     col = 'black', lwd = 1.5, xlab = "Date", ylab = "log(Streamflow)", 
     main = "Post. Pred. Samp.: 50th, 95th, and 5th Qntls.")

matlines(timestamps[idx], t(exp_y_post_50[, idx]), lwd = 0.1, col = 'forestgreen')
matlines(timestamps[idx], t(exp_y_post_95[, idx]), lwd = 0.1, col = 'darkblue')
matlines(timestamps[idx], t(exp_y_post_5[, idx]), lwd = 0.1, col = 'darkred')

lines(timestamps[idx], exp(Y[1, idx]), lwd = 1.5, col = 'black')  # True Obs. Streamflow
legend(x = "topright", inset = c(0.2, 0), legend = c("50th Post. Pred. Samp.", "95th Post. Pred. Samp.", "5th Post. Pred. Samp.", "log(Streamflow)"),
       col = c("forestgreen", "darkblue", "darkred", 'black'), lwd = c(1, 1, 1, 1.5), bty = "n")

# Individual quantile plots
plot_posterior_samples(exp_y_post_50, exp_q50, "50th", "50", "forestgreen", "orange", ylim_range, idx)
plot_posterior_samples(exp_y_post_95, exp_q95, "95th", "95", "darkblue", "orange", ylim_range, idx)
plot_posterior_samples(exp_y_post_5, exp_q5, "5th", "5", "darkred", "orange", ylim_range, idx)
dev.off()

# Reset plotting parameters
par(mfrow = c(1, 1))  # Return to single plot layout


idx <- safe_time_index(TT - 500, TT, TT, context = "40_figures.tt_minus_500")
n.samp <- min(n.samp, dim(samp.theta_95_exAL_synth_DISC$samp_theta)[3])

plot.ts(exp(Y[1,idx]), ylim = c(0,7))
matlines(t(exp_y_post_95[, idx]), lwd = 0.1, col='darkblue')
lines(exp_q95[2,idx], lwd = 0.8, col='gray')
lines(exp_q95[3,idx], lwd = 0.8, col='gray')
lines(exp_q95[4,idx], lwd = 1, col='gray')
lines(t(exp_m95)[idx], lwd = 1, col='purple')
lines(exp(new.theta.out_95_exAL_synth_DISC$exps[1,idx]), lwd = 1.5, col='orange')
lines(exp(Y[1,idx]))

})



set.seed(777)
############################################################################
# Function Definitions
inverse_cdf_AL <- function(U, mu, sigma, p) {
  ifelse(U < p, 
         mu + (sigma / (1 - p)) * log(U / p), 
         mu - (sigma / p) * log((1 - U) / (1 - p)))
}
############################################################################
p_fn <- function(p0, gam) {
  (p0 - as.numeric(gam < 0)) / exp(log_g(gam)) + as.numeric(gam < 0)
}
############################################################################
C_fn <- function(p0, gam) {
  temp_p <- p_fn(p0, gam)
  (as.numeric(gam > 0) - temp_p)^(-1)
}
############################################################################
# Generalized function to handle each case
generate_y_post <- function(p0, xb_matrix, gamma_sample, sigma_sample) {
  n_rows <- dim(xb_matrix)[1]
  n_cols <- dim(xb_matrix)[2]
  y_post <- matrix(NA_real_, nrow = n_rows, ncol = n_cols)
  
  for (t in 1:n_cols) {
    s_0 <- rtruncnorm(1, a=0, b=Inf, mean = 0, sd = 1)
    u <- runif(n_rows)
    y_post[,t] <- xb_matrix[,t] + sigma_sample * abs(gamma_sample) * C_fn(p0, gamma_sample) * s_0 +  
                  sigma_sample * inverse_cdf_AL(u, 0, 1, p_fn(p0, gamma_sample))
  }

  return(y_post)
}
############################################################################
profile_section("figures.build_y_post_forecast", {
  # Case 1: p0 = 0.05
  p0_05 <- 0.05
  xb_05_f <- t(xbs[1, , ])
  gam_05_f <- samp.gamma_5_exAL_synth_DISC[1, ]
  sig_05_f <- samp.sigma_5_exAL_synth_DISC[1, ]
  y_post_5_f <- generate_y_post(p0_05, xb_05_f, gam_05_f, sig_05_f)
  assert_exp_safe_matrix(y_post_5_f, context = "y_post_5_f")
  exp_y_post_5_f <- exp(y_post_5_f)
  # Case 2: p0 = 0.5
  p0_50 <- 0.5
  xb_50_f <- t(xbs[4, , ])
  gam_50_f <- samp.gamma_50_exAL_synth_DISC[1, ]
  sig_50_f <- samp.sigma_50_exAL_synth_DISC[1, ]
  y_post_50_f <- generate_y_post(p0_50, xb_50_f, gam_50_f, sig_50_f)
  assert_exp_safe_matrix(y_post_50_f, context = "y_post_50_f")
  exp_y_post_50_f <- exp(y_post_50_f)
  # Case 3: p0 = 0.95
  p0_95 <- 0.95
  xb_95_f <- t(xbs[7, , ])
  gam_95_f <- samp.gamma_95_exAL_synth_DISC[1, ]
  sig_95_f <- samp.sigma_95_exAL_synth_DISC[1, ]
  y_post_95_f <- generate_y_post(p0_95, xb_95_f, gam_95_f, sig_95_f)
  assert_exp_safe_matrix(y_post_95_f, context = "y_post_95_f")
  exp_y_post_95_f <- exp(y_post_95_f)
  # Case 4: p0 = 0.20
  p0_20 <- 0.20
  xb_20_f <- t(xbs[2, , ])
  gam_20_f <- samp.gamma_20_exAL_synth_DISC[1, ]
  sig_20_f <- samp.sigma_20_exAL_synth_DISC[1, ]
  y_post_20_f <- generate_y_post(p0_20, xb_20_f, gam_20_f, sig_20_f)
  assert_exp_safe_matrix(y_post_20_f, context = "y_post_20_f")
  exp_y_post_20_f <- exp(y_post_20_f)
  # Case 5: p0 = 0.80
  p0_80 <- 0.80
  xb_80_f <- t(xbs[6, , ])
  gam_80_f <- samp.gamma_80_exAL_synth_DISC[1, ]
  sig_80_f <- samp.sigma_80_exAL_synth_DISC[1, ]
  y_post_80_f <- generate_y_post(p0_80, xb_80_f, gam_80_f, sig_80_f)
  assert_exp_safe_matrix(y_post_80_f, context = "y_post_80_f")
  exp_y_post_80_f <- exp(y_post_80_f)
  # Case 6: p0 = 0.35
  p0_35 <- 0.35
  xb_35_f <- t(xbs[3, , ])
  gam_35_f <- samp.gamma_35_exAL_synth_DISC[1, ]
  sig_35_f <- samp.sigma_35_exAL_synth_DISC[1, ]
  y_post_35_f <- generate_y_post(p0_35, xb_35_f, gam_35_f, sig_35_f)
  assert_exp_safe_matrix(y_post_35_f, context = "y_post_35_f")
  exp_y_post_35_f <- exp(y_post_35_f)
  # Case 7: p0 = 0.65
  p0_65 <- 0.65
  xb_65_f <- t(xbs[5, , ])
  gam_65_f <- samp.gamma_65_exAL_synth_DISC[1, ]
  sig_65_f <- samp.sigma_65_exAL_synth_DISC[1, ]
  y_post_65_f <- generate_y_post(p0_65, xb_65_f, gam_65_f, sig_65_f)
  assert_exp_safe_matrix(y_post_65_f, context = "y_post_65_f")
  exp_y_post_65_f <- exp(y_post_65_f)
})
############################################################################
y_post_5_f <- align_sample_time_matrix(y_post_5_f, n.samp, ranges[1], "y_post_5_f")
y_post_20_f <- align_sample_time_matrix(y_post_20_f, n.samp, ranges[1], "y_post_20_f")
y_post_35_f <- align_sample_time_matrix(y_post_35_f, n.samp, ranges[1], "y_post_35_f")
y_post_50_f <- align_sample_time_matrix(y_post_50_f, n.samp, ranges[1], "y_post_50_f")
y_post_65_f <- align_sample_time_matrix(y_post_65_f, n.samp, ranges[1], "y_post_65_f")
y_post_80_f <- align_sample_time_matrix(y_post_80_f, n.samp, ranges[1], "y_post_80_f")
y_post_95_f <- align_sample_time_matrix(y_post_95_f, n.samp, ranges[1], "y_post_95_f")

exp_y_post_5_f <- align_sample_time_matrix(exp_y_post_5_f, n.samp, ranges[1], "exp_y_post_5_f")
exp_y_post_20_f <- align_sample_time_matrix(exp_y_post_20_f, n.samp, ranges[1], "exp_y_post_20_f")
exp_y_post_35_f <- align_sample_time_matrix(exp_y_post_35_f, n.samp, ranges[1], "exp_y_post_35_f")
exp_y_post_50_f <- align_sample_time_matrix(exp_y_post_50_f, n.samp, ranges[1], "exp_y_post_50_f")
exp_y_post_65_f <- align_sample_time_matrix(exp_y_post_65_f, n.samp, ranges[1], "exp_y_post_65_f")
exp_y_post_80_f <- align_sample_time_matrix(exp_y_post_80_f, n.samp, ranges[1], "exp_y_post_80_f")
exp_y_post_95_f <- align_sample_time_matrix(exp_y_post_95_f, n.samp, ranges[1], "exp_y_post_95_f")

n_rows_5 <- dim(xb_05_f)[1]
n_cols_5 <- dim(xb_05_f)[2]

# Initialize the y_reps array with dimensions 7 x n.samp x TT
y_reps_f <- array(NA, dim = c(7, n.samp, ranges[1]))

# Populate the array as specified
y_reps_f[1,,] <- exp_y_post_5_f[,]
y_reps_f[4,,] <- exp_y_post_50_f[,]
y_reps_f[7,,] <- exp_y_post_95_f[,]
y_reps_f[2,,] <- exp_y_post_20_f[,]
y_reps_f[3,,] <- exp_y_post_35_f[,]
y_reps_f[5,,] <- exp_y_post_80_f[,]
y_reps_f[6,,] <- exp_y_post_65_f[,]

profile_section("figures.sort_y_reps_f", {
  for (t in 1:ranges[1]) {
    target_len_f <- length(y_reps_f[1, , t])
    y_reps_f[1, , t] <- sort_to_len(exp_y_post_5_f[, t], target_len_f, context = sprintf("y_reps_f[1,,%d]", t))
    y_reps_f[4, , t] <- sort_to_len(exp_y_post_50_f[, t], target_len_f, context = sprintf("y_reps_f[4,,%d]", t))
    y_reps_f[7, , t] <- sort_to_len(exp_y_post_95_f[, t], target_len_f, context = sprintf("y_reps_f[7,,%d]", t))
    y_reps_f[2, , t] <- sort_to_len(exp_y_post_20_f[, t], target_len_f, context = sprintf("y_reps_f[2,,%d]", t))
    y_reps_f[3, , t] <- sort_to_len(exp_y_post_35_f[, t], target_len_f, context = sprintf("y_reps_f[3,,%d]", t))
    y_reps_f[5, , t] <- sort_to_len(exp_y_post_80_f[, t], target_len_f, context = sprintf("y_reps_f[5,,%d]", t))
    y_reps_f[6, , t] <- sort_to_len(exp_y_post_65_f[, t], target_len_f, context = sprintf("y_reps_f[6,,%d]", t))
  }
})

forecast_horizon_active <- ranges[1]
profile_section("figures.trim_y_reps_f_effective_horizon", {
  y_reps_f_trim <- trim_forecast_cube_to_effective_horizon(
    y_reps_f,
    context = "figures.y_reps_f"
  )
  y_reps_f <<- y_reps_f_trim$cube
  forecast_horizon_active <<- as.integer(y_reps_f_trim$info$horizon)
})
if (forecast_horizon_active < ranges[1]) {
  warning(
    sprintf(
      "[FIGURES_FORECAST_HORIZON_TRUNCATE] Forecast horizon reduced from %d to %d for synthesis/plotting due to trailing non-finite forecast slices (global ranges kept unchanged).",
      as.integer(ranges[1]),
      as.integer(forecast_horizon_active)
    ),
    call. = FALSE
  )
}

profile_section("figures.save_y_reps_f_rds", {
  saveRDS(y_reps_f, file = post_cache_path("y_reps_f.rds"))
})

print("Array y_reps_f saved as y_reps_f.rds in the current directory.")

y_reps <- array(NA, dim = c(7, n.samp, TT))

# Populate the array as specified
y_reps[1,,] <- exp_y_post_5[,]
y_reps[4,,] <- exp_y_post_50[,]
y_reps[7,,] <- exp_y_post_95[,]
y_reps[2,,] <- exp_y_post_20[,]
y_reps[3,,] <- exp_y_post_35[,]
y_reps[5,,] <- exp_y_post_80[,]
y_reps[6,,] <- exp_y_post_65[,]

profile_section("figures.sort_y_reps_hist", {
  for (t in 1:TT) {
    target_len_hist <- length(y_reps[1, , t])
    y_reps[1, , t] <- sort_to_len(exp_y_post_5[, t], target_len_hist, context = sprintf("y_reps[1,,%d]", t))
    y_reps[4, , t] <- sort_to_len(exp_y_post_50[, t], target_len_hist, context = sprintf("y_reps[4,,%d]", t))
    y_reps[7, , t] <- sort_to_len(exp_y_post_95[, t], target_len_hist, context = sprintf("y_reps[7,,%d]", t))
    y_reps[2, , t] <- sort_to_len(exp_y_post_20[, t], target_len_hist, context = sprintf("y_reps[2,,%d]", t))
    y_reps[3, , t] <- sort_to_len(exp_y_post_35[, t], target_len_hist, context = sprintf("y_reps[3,,%d]", t))
    y_reps[5, , t] <- sort_to_len(exp_y_post_80[, t], target_len_hist, context = sprintf("y_reps[5,,%d]", t))
    y_reps[6, , t] <- sort_to_len(exp_y_post_65[, t], target_len_hist, context = sprintf("y_reps[6,,%d]", t))
  }
})


profile_section("figures.save_y_reps_hist_rds", {
  saveRDS(y_reps, file = post_cache_path("y_reps.rds"))
})

print("Array y_reps saved as y_reps.rds in the current directory.")


synthesize_samples <- function(y_reps, q_s, n_cores = detectCores() - 1) {
  if (!is.numeric(y_reps) || is.null(dim(y_reps)) || length(dim(y_reps)) != 3L) {
    stop("[SYNTH_INPUT_SHAPE] synthesize_samples expects a numeric 3D array [quantile x sample x time].")
  }

  # Get dimensions
  n.q     <- dim(y_reps)[1]
  n.samp  <- dim(y_reps)[2]
  n.times <- dim(y_reps)[3]

  finite_slices <- vapply(
    seq_len(n.times),
    function(t_idx) all(is.finite(y_reps[, , t_idx])),
    logical(1)
  )
  if (!all(finite_slices)) {
    bad_t <- which(!finite_slices)
    stop(
      sprintf(
        "[SYNTH_INPUT_NONFINITE] synthesize_samples received non-finite slices at forecast t=%s.",
        paste(utils::head(bad_t, 10L), collapse = ",")
      )
    )
  }
  
  stopifnot(length(q_s) == n.q, !is.unsorted(q_s))
  k <- 1
  # Generate random uniform matrix
  u_mat <- matrix(runif(k*n.samp * n.times), nrow = k*n.samp, ncol = n.times)
  
  # Function to process a single time point
  process_time <- function(t_idx) {
    u_vec <- u_mat[, t_idx]  # Vector of u's for current time
    
    # Find indices
    idx <- findInterval(u_vec, q_s)
    idx[idx == 0] <- 1
    idx[idx >= n.q] <- n.q - 1
    
    # Interpolation weights
    q_lo <- q_s[idx]
    q_hi <- q_s[idx + 1]
    w <- (u_vec - q_lo) / (q_hi - q_lo)
    
    # Extract corresponding quantiles efficiently
    y_lower <- y_reps[cbind(idx, seq_len(n.samp), t_idx)]
    y_upper <- y_reps[cbind(idx + 1, seq_len(n.samp), t_idx)]
    
    # Interpolate
    result <- (1 - w) * y_lower + w * y_upper
    
    # Boundary conditions (lower)
    lower_mask <- u_vec <= q_s[1]
    if (any(lower_mask)) {
      result[lower_mask] <- quantile(y_reps[1, , t_idx], probs = u_vec[lower_mask], type = 8)
    }
    
    # Boundary conditions (upper)
    upper_mask <- u_vec >= q_s[n.q]
    if (any(upper_mask)) {
      result[upper_mask] <- quantile(y_reps[n.q, , t_idx], probs = u_vec[upper_mask], type = 8)
    }
    
    result
  }
  
  # Run parallelized over time dimension
  cl <- makeCluster(1)
  clusterExport(cl, varlist = c("u_mat", "y_reps", "q_s", "n.q", "n.samp"), envir = environment())
  out <- parSapply(cl, seq_len(n.times), process_time)
  stopCluster(cl)
  
  # Adjust dimension to [n.samp x n.times]
  if (is.vector(out)) {
    out <- matrix(out, nrow = k*n.samp, ncol = n.times)
  } else {
    out <- matrix(out, nrow = k*n.samp, ncol = n.times)
  }
  
  return(out)
}


# set.seed(777)
# y_reps_f <- readRDS("y_reps_f.rds")
# q_s    <- c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95)
# n.q     <- dim(y_reps)[1]
# n.samp  <- dim(y_reps)[2]
# n.times <- dim(y_reps)[3]
# stopifnot(length(q_s) == n.q, !is.unsorted(q_s))
# total_samp <- n.samp
# u_mat <- matrix(runif(total_samp * n.times), nrow = total_samp, ncol = n.times)
# u_vec <- u_mat[, 1]
# idx <- findInterval(u_vec, q_s)

# u_vec[4]
# q_s
# idx[4]
# n.q

# # Weights
# q_lo <- q_s[idx]
# q_hi <- q_s[idx + 1]
# w <- (u_vec - q_lo) / (q_hi - q_lo)

# # Extract quantiles
# y_lower <- y_reps[cbind(idx, (seq_len(total_samp) - 1) %% n.samp + 1, t_idx)]
# y_upper <- y_reps[cbind(idx + 1, (seq_len(total_samp) - 1) %% n.samp + 1, t_idx)]

# # Linear interpolation
# result <- (1 - w) * y_lower + w * y_upper

y_reps_f <- profile_section("figures.read_y_reps_f_rds", readRDS(post_cache_path("y_reps_f.rds")))

q_s    <- c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95)
n.q     <- length(q_s)
n.samp  <- n.samp
n.times <- dim(y_reps_f)[3]

	synth_f <- profile_section("figures.synthesize_samples_y_reps_f", synthesize_samples(y_reps_f, q_s))
	dim(synth_f)

synth_f_q <- colQuantiles(synth_f, probs = q_s, type = 8)
synth_f_q <- t(synth_f_q)
dim(synth_f_q)

# y_reps_f <- readRDS("y_reps_f.rds")

# q_s    <- c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95)
# n.q     <- length(q_s)
# n.samp  <- n.samp
# n.times <- ranges[1]

# synth_f2 <- synthesize_samples(y_reps_f, q_s)
# dim(synth_f2)

# synth_f_q2 <- colQuantiles(synth_f, probs = q_s, type = 8)
# synth_f_q2 <- t(synth_f_q)
# dim(synth_f_q2)

		profile_section("figures.sort_synth_f", {
		  for (t in seq_len(n.times)) {
		    synth_f[, t] <- sort_to_len(synth_f[, t], target_len = nrow(synth_f), context = sprintf("synth_f[,%d]", t))
		    # synth_f2[,t] <- sort(synth_f2[,t])
		  }
		})

plot.ts(rep(0, n.times), ylim = c(0,10))

SL <- San_Lorenzo_Daily_USGS_R[San_Lorenzo_Daily_USGS_R$Date >= timestamps[1] , ]
SL <- SL[(TT+1):(TT+n.times) , ]

matlines(t(synth_f), col = 'pink', lwd = 0.5)

points(SL$data0, lwd = 0.8)

for (i in 1:n.q) {
   lines(synth_f_q[i,], col = 'gray', lwd = 2)
}

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(exp(xbs[7, seq_len(n.times), ]), probs = c(0.025, 0.5, 0.975))
lines(result[1,], col = 'blue', lty = 2, lwd = 1)
lines(result[2,], col = 'darkblue', lwd = 1.5)
lines(result[3,], col = 'blue', lty = 2, lwd = 1)

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(exp(xbs[1, seq_len(n.times), ]), probs = c(0.025, 0.5, 0.975))
lines(result[1,], col = 'red', lty = 2, lwd = 1)
lines(result[2,], col = 'darkred', lwd = 1.5)
lines(result[3,], col = 'red', lty = 2, lwd = 1)

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(
  post_transform_loglog1p_array(
    xbs[4, seq_len(n.times), ],
    context = "figures.xbs.q50",
    overflow_policy = "cap"
  )$values,
  probs = c(0.025, 0.5, 0.975)
)
lines(result[1,], col = 'green', lty = 2, lwd = 1)
lines(result[2,], col = 'forestgreen', lwd = 1.5)
lines(result[3,], col = 'green', lty = 2, lwd = 1)

# idx <- (13171-27):(13171-27+10)
# lines(exp(new.theta.out_95_exAL_synth_DISC$exps[3,idx]), col = 'lightblue', lwd = 2)
# lines(exp(new.theta.out_50_exAL_synth_DISC$exps[3,idx]), col = 'lightgreen', lwd = 2)
# lines(exp(new.theta.out_5_exAL_synth_DISC$exps[3,idx]), col = 'purple', lwd = 2)

# idx <- (13171-27):13171
# lines(exp(new.theta.out_95_exAL_synth_DISC$exps[2,idx]), col = 'lightblue', lwd = 2)
# lines(exp(new.theta.out_50_exAL_synth_DISC$exps[2,idx]), col = 'lightgreen', lwd = 2)
# lines(exp(new.theta.out_5_exAL_synth_DISC$exps[2,idx]), col = 'purple', lwd = 2)

y_reps_f_new <- array(NA_real_,c(7,n.samp,ranges[1]))

profile_section("figures.build_y_reps_f_new", {
xxx <- 1
for(t in 1:ranges[1]){
    for(s in 1:n.samp){
    gamma <- samp.gamma_95_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_95_exAL_synth_DISC[1,s]
    p00 <- 0.95
    mu <- xbs[7,t,s]
    y_reps_f_new[7,s,t] <- rexal(1, p00, mu, sigma, gamma)   
    
    gamma <- samp.gamma_80_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_80_exAL_synth_DISC[1,s]
    p00 <- 0.80
    mu <- xbs[6,t,s]
    y_reps_f_new[6,s,t] <- rexal(1, p00, mu, sigma, gamma)   

    gamma <- samp.gamma_65_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_65_exAL_synth_DISC[1,s]
    p00 <- 0.65
    mu <- xbs[5,t,s]
    y_reps_f_new[5,s,t] <- rexal(1, p00, mu, sigma, gamma)   

    gamma <- samp.gamma_50_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_50_exAL_synth_DISC[1,s]
    p00 <- 0.50
    mu <- xbs[4,t,s]
    y_reps_f_new[4,s,t] <- rexal(1, p00, mu, sigma, gamma)   

    gamma <- samp.gamma_35_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_35_exAL_synth_DISC[1,s]
    p00 <- 0.35
    mu <- xbs[3,t,s]
    y_reps_f_new[3,s,t] <- rexal(1, p00, mu, sigma, gamma)   

    gamma <- samp.gamma_20_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_20_exAL_synth_DISC[1,s]
    p00 <- 0.20
    mu <- xbs[2,t,s]
    y_reps_f_new[2,s,t] <- rexal(1, p00, mu, sigma, gamma)   
        
    gamma <- samp.gamma_5_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_5_exAL_synth_DISC[1,s]
    p00 <- 0.05
    mu <- xbs[1,t,s]
    y_reps_f_new[1,s,t] <- rexal(1, p00, mu, sigma, gamma)   
    
    
    }

}



y_reps_f_5 <- y_reps_f_new[1,,]
y_reps_f_20 <- y_reps_f_new[2,,]
y_reps_f_35 <- y_reps_f_new[3,,]
y_reps_f_50 <- y_reps_f_new[4,,]
y_reps_f_65 <- y_reps_f_new[5,,]
y_reps_f_80 <- y_reps_f_new[6,,]
y_reps_f_95 <- y_reps_f_new[7,,]
for(t in 1:ranges[1]){
    target_len_f_new <- nrow(y_reps_f_5)
    y_reps_f_5[,t] <- sort_to_len(y_reps_f_5[,t], target_len_f_new, context = sprintf("y_reps_f_5[,%d]", t))
    y_reps_f_20[,t] <- sort_to_len(y_reps_f_20[,t], target_len_f_new, context = sprintf("y_reps_f_20[,%d]", t))
    y_reps_f_35[,t] <- sort_to_len(y_reps_f_35[,t], target_len_f_new, context = sprintf("y_reps_f_35[,%d]", t))
    y_reps_f_50[,t] <- sort_to_len(y_reps_f_50[,t], target_len_f_new, context = sprintf("y_reps_f_50[,%d]", t))
    y_reps_f_65[,t] <- sort_to_len(y_reps_f_65[,t], target_len_f_new, context = sprintf("y_reps_f_65[,%d]", t))
    y_reps_f_80[,t] <- sort_to_len(y_reps_f_80[,t], target_len_f_new, context = sprintf("y_reps_f_80[,%d]", t))
    y_reps_f_95[,t] <- sort_to_len(y_reps_f_95[,t], target_len_f_new, context = sprintf("y_reps_f_95[,%d]", t))
}

y_reps_f_new[1,,] <- y_reps_f_5  
y_reps_f_new[2,,] <- y_reps_f_20  
y_reps_f_new[3,,] <- y_reps_f_35  
y_reps_f_new[4,,] <- y_reps_f_50  
y_reps_f_new[5,,] <- y_reps_f_65  
y_reps_f_new[6,,] <- y_reps_f_80  
y_reps_f_new[7,,] <- y_reps_f_95 
})

profile_section("figures.save_y_reps_f_new_rds", {
  saveRDS(y_reps_f_new, file = post_cache_path("y_reps_f_new.rds"))
})



y_reps_f <- profile_section("figures.read_y_reps_f_new_rds", readRDS(post_cache_path("y_reps_f_new.rds")))
y_reps_f_trim2 <- trim_forecast_cube_to_effective_horizon(
  y_reps_f,
  context = "figures.y_reps_f_new"
)
y_reps_f <- y_reps_f_trim2$cube

q_s    <- c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95)
n.q     <- length(q_s)
n.samp  <- n.samp
n.times <- dim(y_reps_f)[3]

forecast_exp_guard <- profile_section(
  "figures.transform_y_reps_f_loglog1p",
  post_transform_loglog1p_array(
    y_reps_f,
    context = "figures.y_reps_f.loglog1p",
    overflow_policy = "cap",
    report_path = post_cache_path("synth_multivar_forecast_exp_guard.txt")
  )
)
synth_f <- profile_section("figures.synthesize_samples_y_reps_f_exp", synthesize_samples(forecast_exp_guard$values, q_s))
dim(synth_f)

synth_f_q <- colQuantiles(synth_f, probs = q_s, type = 8)
synth_f_q <- t(synth_f_q)
dim(synth_f_q)

profile_section("figures.sort_synth_f_exp", {
  for (t in seq_len(n.times)) {
    synth_f[, t] <- sort_to_len(synth_f[, t], target_len = nrow(synth_f), context = sprintf("synth_f_exp[,%d]", t))
  }
})

crps_transfer_mode <- tolower(trimws(Sys.getenv("UNIFIED_MULTIVAR_FORECAST_TRANSFER_MODE", "drop")))
if (!crps_transfer_mode %in% c("drop", "keep")) {
  crps_transfer_mode <- NA_character_
}
crps_univar_likelihood_mode <- tolower(trimws(Sys.getenv("UNIFIED_EXDQLM_UNIVAR_LIKELIHOOD_MODE", "exal")))
if (!crps_univar_likelihood_mode %in% c("exal", "al")) {
  crps_univar_likelihood_mode <- "exal"
}
crps_multivar_likelihood_mode <- tolower(trimws(Sys.getenv("UNIFIED_EXDQLM_MULTIVAR_LIKELIHOOD_MODE", "exal")))
if (!crps_multivar_likelihood_mode %in% c("exal", "al")) {
  crps_multivar_likelihood_mode <- "exal"
}
crps_ndlm_transfer_mode <- tolower(trimws(Sys.getenv("UNIFIED_NDLM_FORECAST_TRANSFER_MODE", "keep")))
if (!crps_ndlm_transfer_mode %in% c("drop", "keep")) {
  crps_ndlm_transfer_mode <- NA_character_
}
crps_ndlm_univar_transfer_mode <- tolower(trimws(Sys.getenv(
  "UNIFIED_NDLM_UNIVAR_FORECAST_TRANSFER_MODE",
  if (is.na(crps_ndlm_transfer_mode)) "keep" else crps_ndlm_transfer_mode
)))
if (!crps_ndlm_univar_transfer_mode %in% c("drop", "keep")) {
  crps_ndlm_univar_transfer_mode <- NA_character_
}
crps_output_suffix <- Sys.getenv("UNIFIED_POST_OUTPUT_SUFFIX", "")
crps_exports_enabled <- isTRUE(as.logical(Sys.getenv("UNIFIED_POST_EXPORT_CRPS", "TRUE")))
crps_input_health_enabled <- isTRUE(as.logical(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_ENABLED", "TRUE")))
crps_input_health_fail_fast <- isTRUE(as.logical(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_FAIL_FAST", "FALSE")))
crps_input_health_min_finite_share <- suppressWarnings(as.numeric(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_MIN_FINITE_SHARE", "1")))
if (!is.finite(crps_input_health_min_finite_share) ||
    crps_input_health_min_finite_share < 0 || crps_input_health_min_finite_share > 1) {
  crps_input_health_min_finite_share <- 1
}
crps_input_health_max_abs <- suppressWarnings(as.numeric(Sys.getenv("UNIFIED_POST_CRPS_INPUT_HEALTH_MAX_ABS", "NA")))
if (!is.finite(crps_input_health_max_abs) || crps_input_health_max_abs <= 0) {
  crps_input_health_max_abs <- NA_real_
}

if (crps_exports_enabled) {
  profile_section("figures.export_crps_tables", {
    crps_cutoff_date <- as.Date(CUTOFF_DATE)
    crps_forecast_start <- as.Date(FORECAST_START_DATE)
    usgs_date_col <- if ("Date" %in% names(San_Lorenzo_Daily_USGS_R)) {
      as.Date(San_Lorenzo_Daily_USGS_R$Date)
    } else {
      as.Date(San_Lorenzo_Daily_USGS_R$time)
    }
    usgs_truth_all <- as.numeric(San_Lorenzo_Daily_USGS_R$data0)
    crps_truth_availability_rows <- list()

    truth_from_start <- function(horizon, context) {
      resolved <- post_truth_from_start_or_na(
        usgs_dates = usgs_date_col,
        usgs_truth = usgs_truth_all,
        forecast_start_date = crps_forecast_start,
        horizon = horizon,
        context = context
      )
      crps_truth_availability_rows[[length(crps_truth_availability_rows) + 1L]] <<- resolved$availability
      resolved$truth
    }

    crps_per_time_rows <- list()
    crps_summary_rows <- list()
    crps_input_health_rows <- list()
    crps_input_health_per_time_rows <- list()
    crps_input_health_failures <- character(0)

    collect_crps_input_health <- function(
      model_id,
      model_family,
      model_variant,
      sample_mat,
      forecast_dates,
      transfer_mode,
      context
    ) {
      if (!isTRUE(crps_input_health_enabled)) return(invisible(NULL))
      health <- post_crps_input_health_tables(
        model_id = model_id,
        model_family = model_family,
        model_variant = model_variant,
        sample_mat = sample_mat,
        forecast_dates = forecast_dates,
        cutoff_date = crps_cutoff_date,
        forecast_start_date = crps_forecast_start,
        transfer_mode = transfer_mode,
        min_finite_share = crps_input_health_min_finite_share,
        max_abs = crps_input_health_max_abs,
        context = paste0(context, ".input_health")
      )
      crps_input_health_rows[[length(crps_input_health_rows) + 1L]] <<- health$summary
      crps_input_health_per_time_rows[[length(crps_input_health_per_time_rows) + 1L]] <<- health$per_time
      if (isTRUE(crps_input_health_fail_fast) && !isTRUE(health$pass)) {
        violation_msg <- if (length(health$violations) > 0L) {
          paste(health$violations, collapse = " | ")
        } else {
          "input health failed"
        }
        crps_input_health_failures <<- c(
          crps_input_health_failures,
          sprintf("%s (%s): %s", model_id, context, violation_msg)
        )
      }
      invisible(NULL)
    }

    if (length(ensembles) >= 1L) {
      glofas_mat <- as.matrix(ensembles[[1]])
      if (is.numeric(glofas_mat) && nrow(glofas_mat) > 0L && ncol(glofas_mat) >= 2L) {
        glofas_tbl <- post_crps_model_tables(
          model_id = "glofas_ensemble",
          model_family = "ensemble",
          model_variant = "glofas",
          sample_mat = t(glofas_mat),
          obs = truth_from_start(nrow(glofas_mat), "crps.glofas.truth"),
          forecast_dates = daily_dates_for_matrix_rows(glofas_mat, start_date = crps_forecast_start, context = "crps.glofas.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = NA_character_,
          score_scale = "log_cms_plus1",
          context = "crps.glofas"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- glofas_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- glofas_tbl$summary
        collect_crps_input_health(
          model_id = "glofas_ensemble",
          model_family = "ensemble",
          model_variant = "glofas",
          sample_mat = t(glofas_mat),
          forecast_dates = daily_dates_for_matrix_rows(
            glofas_mat,
            start_date = crps_forecast_start,
            context = "crps.glofas.health.dates"
          ),
          transfer_mode = NA_character_,
          context = "crps.glofas"
        )
      } else {
        warning("[CRPS_GLOFAS_SKIP] Unable to compute GloFAS CRPS (invalid ensemble matrix).", call. = FALSE)
      }
    } else {
      warning("[CRPS_GLOFAS_SKIP] Unable to compute GloFAS CRPS (ensembles[[1]] missing).", call. = FALSE)
    }

    if (length(ensembles) >= 2L) {
      nws_mat <- as.matrix(ensembles[[2]])
      if (is.numeric(nws_mat) && nrow(nws_mat) > 0L && ncol(nws_mat) >= 2L) {
        nws_tbl <- post_crps_model_tables(
          model_id = "nws_nwm_ensemble",
          model_family = "ensemble",
          model_variant = "nws_nwm",
          sample_mat = t(nws_mat),
          obs = truth_from_start(nrow(nws_mat), "crps.nws.truth"),
          forecast_dates = daily_dates_for_matrix_rows(nws_mat, start_date = crps_forecast_start, context = "crps.nws.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = NA_character_,
          score_scale = "log_cms_plus1",
          context = "crps.nws"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- nws_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- nws_tbl$summary
        collect_crps_input_health(
          model_id = "nws_nwm_ensemble",
          model_family = "ensemble",
          model_variant = "nws_nwm",
          sample_mat = t(nws_mat),
          forecast_dates = daily_dates_for_matrix_rows(
            nws_mat,
            start_date = crps_forecast_start,
            context = "crps.nws.health.dates"
          ),
          transfer_mode = NA_character_,
          context = "crps.nws"
        )
      } else {
        warning("[CRPS_NWS_SKIP] Unable to compute NWS/NWM CRPS (invalid ensemble matrix).", call. = FALSE)
      }
    } else {
      warning("[CRPS_NWS_SKIP] Unable to compute NWS/NWM CRPS (ensembles[[2]] missing).", call. = FALSE)
    }

    if (exists("synth_f2", inherits = TRUE)) {
      synth_uni_mat <- as.matrix(get("synth_f2", inherits = TRUE))
      if (is.numeric(synth_uni_mat) && nrow(synth_uni_mat) > 0L && ncol(synth_uni_mat) > 0L) {
        univar_meta <- post_crps_synth_model_meta(
          family = "univar",
          likelihood_mode = crps_univar_likelihood_mode,
          transfer_mode = NA_character_
        )
        univar_tbl <- post_crps_model_tables(
          model_id = univar_meta$model_id,
          model_family = "synthesis",
          model_variant = univar_meta$model_variant,
          sample_mat = synth_uni_mat,
          obs = truth_from_start(ncol(synth_uni_mat), "crps.univar.truth"),
          forecast_dates = daily_dates_for_matrix_cols(synth_uni_mat, start_date = crps_forecast_start, context = "crps.univar.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = NA_character_,
          score_scale = "log_cms_plus1",
          context = "crps.univar"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- univar_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- univar_tbl$summary
        collect_crps_input_health(
          model_id = univar_meta$model_id,
          model_family = "synthesis",
          model_variant = univar_meta$model_variant,
          sample_mat = synth_uni_mat,
          forecast_dates = daily_dates_for_matrix_cols(
            synth_uni_mat,
            start_date = crps_forecast_start,
            context = "crps.univar.health.dates"
          ),
          transfer_mode = NA_character_,
          context = "crps.univar"
        )
      } else {
        warning("[CRPS_UNIVAR_SKIP] Unable to compute univariate synthesis CRPS (invalid synth_f2 matrix).", call. = FALSE)
      }
    } else {
      warning("[CRPS_UNIVAR_SKIP] Unable to compute univariate synthesis CRPS (synth_f2 missing).", call. = FALSE)
    }

    multivar_meta <- post_crps_synth_model_meta(
      family = "multivar",
      likelihood_mode = crps_multivar_likelihood_mode,
      transfer_mode = crps_transfer_mode
    )

    synth_multivar_mat <- as.matrix(synth_f)
    if (is.numeric(synth_multivar_mat) && nrow(synth_multivar_mat) > 0L && ncol(synth_multivar_mat) > 0L) {
      multivar_tbl <- post_crps_model_tables(
        model_id = multivar_meta$model_id,
        model_family = "synthesis",
        model_variant = multivar_meta$model_variant,
        sample_mat = synth_multivar_mat,
        obs = truth_from_start(ncol(synth_multivar_mat), "crps.multivar.truth"),
        forecast_dates = daily_dates_for_matrix_cols(synth_multivar_mat, start_date = crps_forecast_start, context = "crps.multivar.dates"),
        cutoff_date = crps_cutoff_date,
        forecast_start_date = crps_forecast_start,
        transfer_mode = crps_transfer_mode,
        score_scale = "log_cms_plus1",
        context = "crps.multivar"
      )
      crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- multivar_tbl$per_time
      crps_summary_rows[[length(crps_summary_rows) + 1L]] <- multivar_tbl$summary
      collect_crps_input_health(
        model_id = multivar_meta$model_id,
        model_family = "synthesis",
        model_variant = multivar_meta$model_variant,
        sample_mat = synth_multivar_mat,
        forecast_dates = daily_dates_for_matrix_cols(
          synth_multivar_mat,
          start_date = crps_forecast_start,
          context = "crps.multivar.health.dates"
        ),
        transfer_mode = crps_transfer_mode,
        context = "crps.multivar"
      )
    } else {
      warning("[CRPS_MULTIVAR_SKIP] Unable to compute multivariate synthesis CRPS (invalid synth_f matrix).", call. = FALSE)
    }

    ndlm_main_enabled <- isTRUE(exists("MODEL_RUN_NDLM_MAIN", inherits = TRUE) &&
      get("MODEL_RUN_NDLM_MAIN", inherits = TRUE))
    if (ndlm_main_enabled && exists("xbs_ndlm", inherits = TRUE)) {
      ndlm_raw <- get("xbs_ndlm", inherits = TRUE)
      ndlm_sigma_draws <- if (exists("samp.sigma_50_NDLM_synth_DISC", inherits = TRUE)) {
        get("samp.sigma_50_NDLM_synth_DISC", inherits = TRUE)
      } else {
        NULL
      }

      ndlm_pred <- tryCatch(
        post_ndlm_predictive_draws(
          ndlm_raw = ndlm_raw,
          sigma_draws = ndlm_sigma_draws,
          context = "crps.ndlm",
          seed = 777L
        ),
        error = function(e) e
      )

      if (!inherits(ndlm_pred, "error")) {
        ndlm_sample_mat_log1p <- ndlm_pred$predictive_log1p
        saveRDS(ndlm_pred$mean_loglog1p, file = post_cache_path("xbs_ndlm_mean_loglog1p.rds"))
        saveRDS(ndlm_pred$predictive_loglog1p, file = post_cache_path("y_reps_ndlm_loglog1p.rds"))
        saveRDS(ndlm_sample_mat_log1p, file = post_cache_path("xbs_ndlm_log1p.rds"))
        saveRDS(ndlm_sample_mat_log1p, file = post_cache_path("y_reps_ndlm_log1p.rds"))
        ndlm_meta <- post_crps_synth_model_meta(
          family = "ndlm",
          likelihood_mode = "exal",
          transfer_mode = crps_ndlm_transfer_mode
        )
        ndlm_tbl <- post_crps_model_tables(
          model_id = ndlm_meta$model_id,
          model_family = "synthesis",
          model_variant = ndlm_meta$model_variant,
          sample_mat = ndlm_sample_mat_log1p,
          obs = truth_from_start(ncol(ndlm_sample_mat_log1p), "crps.ndlm.truth"),
          forecast_dates = daily_dates_for_matrix_cols(ndlm_sample_mat_log1p, start_date = crps_forecast_start, context = "crps.ndlm.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = crps_ndlm_transfer_mode,
          score_scale = "log_cms_plus1",
          context = "crps.ndlm"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- ndlm_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- ndlm_tbl$summary
        collect_crps_input_health(
          model_id = ndlm_meta$model_id,
          model_family = "synthesis",
          model_variant = ndlm_meta$model_variant,
          sample_mat = ndlm_sample_mat_log1p,
          forecast_dates = daily_dates_for_matrix_cols(
            ndlm_sample_mat_log1p,
            start_date = crps_forecast_start,
            context = "crps.ndlm.health.dates"
          ),
          transfer_mode = crps_ndlm_transfer_mode,
          context = "crps.ndlm"
        )
      } else {
        warning(
          sprintf(
            "[CRPS_NDLM_SKIP] Unable to compute NDLM CRPS (%s).",
            conditionMessage(ndlm_pred)
          ),
          call. = FALSE
        )
      }
    } else if (ndlm_main_enabled) {
      warning("[CRPS_NDLM_SKIP] Unable to compute NDLM CRPS (xbs_ndlm missing).", call. = FALSE)
    }

    ndlm_univar_path <- if (exists("NDLM_UNIVAR_VAR_50", inherits = TRUE)) {
      as.character(get("NDLM_UNIVAR_VAR_50", inherits = TRUE))
    } else {
      ""
    }
    ndlm_univar_enabled <- isTRUE(exists("MODEL_RUN_NDLM_UNIVAR", inherits = TRUE) &&
      get("MODEL_RUN_NDLM_UNIVAR", inherits = TRUE))
    if ((ndlm_univar_enabled || nzchar(ndlm_univar_path)) &&
        length(ndlm_univar_path) > 0L && nzchar(ndlm_univar_path) && file.exists(ndlm_univar_path)) {
      ndlm_univar_env <- new.env(parent = emptyenv())
      load(ndlm_univar_path, envir = ndlm_univar_env)
      ndlm_univar_obj_name <- "new.theta.out_50_NDLM_univar_synth_DISC"
      if (!exists(ndlm_univar_obj_name, envir = ndlm_univar_env, inherits = FALSE)) {
        obj_candidates <- grep("^new\\.theta\\.out_.*NDLM_univar.*$", ls(ndlm_univar_env), value = TRUE)
        ndlm_univar_obj_name <- if (length(obj_candidates) > 0L) obj_candidates[[1L]] else ""
      }
      ndlm_univar_sigma_name <- "samp.sigma_50_NDLM_univar_synth_DISC"
      if (!exists(ndlm_univar_sigma_name, envir = ndlm_univar_env, inherits = FALSE)) {
        sigma_candidates <- grep("^samp\\.sigma_.*NDLM_univar.*$", ls(ndlm_univar_env), value = TRUE)
        ndlm_univar_sigma_name <- if (length(sigma_candidates) > 0L) sigma_candidates[[1L]] else ""
      }
      ndlm_univar_sample_mat <- tryCatch({
        ndlm_univar_obj <- if (nzchar(ndlm_univar_obj_name) && exists(ndlm_univar_obj_name, envir = ndlm_univar_env, inherits = FALSE)) {
          get(ndlm_univar_obj_name, envir = ndlm_univar_env, inherits = FALSE)
        } else {
          NULL
        }
        ndlm_univar_sigma_draws <- if (nzchar(ndlm_univar_sigma_name) && exists(ndlm_univar_sigma_name, envir = ndlm_univar_env, inherits = FALSE)) {
          get(ndlm_univar_sigma_name, envir = ndlm_univar_env, inherits = FALSE)
        } else {
          NULL
        }
        if (is.null(ndlm_univar_obj) || is.null(ndlm_univar_sigma_draws) ||
            !exists("ranges", inherits = TRUE) || !exists("FF_list", inherits = TRUE)) {
          NULL
        } else {
          n_samp_ndlm_univar <- suppressWarnings(as.integer(length(as.numeric(ndlm_univar_sigma_draws))))
          if (!is.finite(n_samp_ndlm_univar) || n_samp_ndlm_univar <= 1L) {
            NULL
          } else {
            ndlm_univar_mean_draws <- post_build_ndlm_state_draw_array(
              ndlm_obj = ndlm_univar_obj,
              ranges = get("ranges", inherits = TRUE),
              FF_list = get("FF_list", inherits = TRUE),
              n_samp = n_samp_ndlm_univar,
              p_state = if (exists("p", inherits = TRUE)) get("p", inherits = TRUE) else 7L,
              eps_reg = 0,
              seed = 777L,
              context = "crps.ndlm_univar.mean"
            )
            post_ndlm_predictive_draws(
              ndlm_raw = ndlm_univar_mean_draws,
              sigma_draws = ndlm_univar_sigma_draws,
              context = "crps.ndlm_univar",
              seed = 777L
            )$predictive_log1p
          }
        }
      }, error = function(e) {
        warning(
          sprintf("[CRPS_NDLM_UNIVAR_SKIP] Unable to compute NDLM univar CRPS (%s).", conditionMessage(e)),
          call. = FALSE
        )
        NULL
      })
      if (!is.null(ndlm_univar_sample_mat) && is.numeric(ndlm_univar_sample_mat) &&
          nrow(ndlm_univar_sample_mat) > 1L && ncol(ndlm_univar_sample_mat) > 0L) {
        ndlm_univar_meta <- post_crps_synth_model_meta(
          family = "ndlm_univar",
          likelihood_mode = "exal",
          transfer_mode = crps_ndlm_univar_transfer_mode
        )
        ndlm_univar_tbl <- post_crps_model_tables(
          model_id = ndlm_univar_meta$model_id,
          model_family = "synthesis",
          model_variant = ndlm_univar_meta$model_variant,
          sample_mat = ndlm_univar_sample_mat,
          obs = truth_from_start(ncol(ndlm_univar_sample_mat), "crps.ndlm_univar.truth"),
          forecast_dates = daily_dates_for_matrix_cols(ndlm_univar_sample_mat, start_date = crps_forecast_start, context = "crps.ndlm_univar.dates"),
          cutoff_date = crps_cutoff_date,
          forecast_start_date = crps_forecast_start,
          transfer_mode = crps_ndlm_univar_transfer_mode,
          score_scale = "log_cms_plus1",
          context = "crps.ndlm_univar"
        )
        crps_per_time_rows[[length(crps_per_time_rows) + 1L]] <- ndlm_univar_tbl$per_time
        crps_summary_rows[[length(crps_summary_rows) + 1L]] <- ndlm_univar_tbl$summary
        collect_crps_input_health(
          model_id = ndlm_univar_meta$model_id,
          model_family = "synthesis",
          model_variant = ndlm_univar_meta$model_variant,
          sample_mat = ndlm_univar_sample_mat,
          forecast_dates = daily_dates_for_matrix_cols(
            ndlm_univar_sample_mat,
            start_date = crps_forecast_start,
            context = "crps.ndlm_univar.health.dates"
          ),
          transfer_mode = crps_ndlm_univar_transfer_mode,
          context = "crps.ndlm_univar"
        )
      } else {
        warning("[CRPS_NDLM_UNIVAR_SKIP] Unable to compute NDLM univar CRPS (invalid predictive draw matrix).", call. = FALSE)
      }
    } else if (ndlm_univar_enabled || nzchar(ndlm_univar_path)) {
      warning("[CRPS_NDLM_UNIVAR_SKIP] Unable to compute NDLM univar CRPS (artifact path missing).", call. = FALSE)
    }

    if (length(crps_per_time_rows) > 0L && length(crps_summary_rows) > 0L) {
      crps_per_time_df <- do.call(rbind, crps_per_time_rows)
      crps_summary_df <- do.call(rbind, crps_summary_rows)
      rownames(crps_per_time_df) <- NULL
      rownames(crps_summary_df) <- NULL

      crps_export <- post_export_crps_tables(
        per_time_df = crps_per_time_df,
        summary_df = crps_summary_df,
        output_dir = posterior_table_output_dir,
        table_formats = posterior_table_formats,
        keep_na = posterior_table_keep_na,
        numeric_digits = 17L,
        file_suffix = crps_output_suffix
      )
      posterior_table_export_manifest <<- rbind(posterior_table_export_manifest, crps_export$manifest)
    } else {
      warning("[CRPS_EXPORT_SKIP] No CRPS rows were produced for export.", call. = FALSE)
    }

    if (length(crps_truth_availability_rows) > 0L) {
      crps_truth_availability_df <- do.call(rbind, crps_truth_availability_rows)
      rownames(crps_truth_availability_df) <- NULL
      truth_export <- post_export_tables(
        tables = list(crps_truth_availability = crps_truth_availability_df),
        output_dir = posterior_table_output_dir,
        formats = posterior_table_formats,
        keep_na = posterior_table_keep_na,
        numeric_digits = 17L
      )
      posterior_table_export_manifest <<- rbind(posterior_table_export_manifest, truth_export)
    }

    if (isTRUE(crps_input_health_enabled) &&
        length(crps_input_health_rows) > 0L &&
        length(crps_input_health_per_time_rows) > 0L) {
      crps_input_health_df <- do.call(rbind, crps_input_health_rows)
      crps_input_health_per_time_df <- do.call(rbind, crps_input_health_per_time_rows)
      rownames(crps_input_health_df) <- NULL
      rownames(crps_input_health_per_time_df) <- NULL
      health_export <- post_export_crps_input_health_tables(
        summary_df = crps_input_health_df,
        per_time_df = crps_input_health_per_time_df,
        output_dir = posterior_table_output_dir,
        table_formats = posterior_table_formats,
        keep_na = posterior_table_keep_na,
        numeric_digits = 17L,
        file_suffix = crps_output_suffix
      )
      posterior_table_export_manifest <<- rbind(posterior_table_export_manifest, health_export$manifest)
    } else if (isTRUE(crps_input_health_enabled)) {
      warning("[CRPS_INPUT_HEALTH_EXPORT_SKIP] No CRPS input-health rows were produced for export.", call. = FALSE)
    }

    if (isTRUE(crps_input_health_fail_fast) && length(crps_input_health_failures) > 0L) {
      stop(
        sprintf(
          "[CRPS_INPUT_HEALTH_FAIL_FAST] non-finite or out-of-contract draws detected: %s",
          paste(crps_input_health_failures, collapse = " || ")
        ),
        call. = FALSE
      )
    }
  })
}

plot.ts(rep(0,n.times), ylim = c(0,10))

SL <- San_Lorenzo_Daily_USGS_R[San_Lorenzo_Daily_USGS_R$Date >= timestamps[1] , ]
SL <- SL[(TT+1):(TT+n.times) , ]

matlines(t(synth_f), col = 'pink', lwd = 0.5)

points(SL$data0, lwd = 0.8)

for (i in 1:n.q) {
   lines(synth_f_q[i,], col = 'gray', lwd = 2)
}

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(exp(xbs[7, seq_len(n.times), ]), probs = c(0.025, 0.5, 0.975))
lines(result[1,], col = 'blue', lty = 2, lwd = 1)
lines(result[2,], col = 'darkblue', lwd = 1.5)
lines(result[3,], col = 'blue', lty = 2, lwd = 1)

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(exp(xbs[1, seq_len(n.times), ]), probs = c(0.025, 0.5, 0.975))
lines(result[1,], col = 'red', lty = 2, lwd = 1)
lines(result[2,], col = 'darkred', lwd = 1.5)
lines(result[3,], col = 'red', lty = 2, lwd = 1)

# Adding quantile bands (blue) for 95th Quantile estimation
result <- fast_row_quantiles_t(exp(xbs[4, seq_len(n.times), ]), probs = c(0.025, 0.5, 0.975))
lines(result[1,], col = 'green', lty = 2, lwd = 1)
lines(result[2,], col = 'forestgreen', lwd = 1.5)
lines(result[3,], col = 'green', lty = 2, lwd = 1)

# for (s in 1:n.samp) {
#    lines(exp(xbs[4,,s]), col = 'red', lwd = 0.05)
#    lines(exp(xbs[1,,s]), col = 'lightgreen', lwd = 0.05)
#    lines(exp(xbs[7,,s]), col = 'lightblue', lwd = 0.05)
# }


# for (s in 1:n.samp) {
#    lines(exp(y_reps_f_95[s,]), col = 'gray', lwd = 0.1)
# }

result <- fast_col_quantiles_t(
  post_transform_loglog1p_array(
    y_reps_f_95,
    context = "figures.y_reps_f.q95",
    overflow_policy = "cap"
  )$values,
  probs = 0.95
)[1, ]
lines(result, col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_loglog1p_array(
    y_reps_f_80,
    context = "figures.y_reps_f.q80",
    overflow_policy = "cap"
  )$values,
  probs = 0.80
)[1, ]
lines(result, col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_loglog1p_array(
    y_reps_f_65,
    context = "figures.y_reps_f.q65",
    overflow_policy = "cap"
  )$values,
  probs = 0.65
)[1, ]
lines(result, col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_loglog1p_array(
    y_reps_f_50,
    context = "figures.y_reps_f.q50",
    overflow_policy = "cap"
  )$values,
  probs = 0.50
)[1, ]
lines(result, col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_loglog1p_array(
    y_reps_f_35,
    context = "figures.y_reps_f.q35",
    overflow_policy = "cap"
  )$values,
  probs = 0.35
)[1, ]
lines(result, col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_loglog1p_array(
    y_reps_f_20,
    context = "figures.y_reps_f.q20",
    overflow_policy = "cap"
  )$values,
  probs = 0.20
)[1, ]
lines(result, col = 'black', lwd = 0.5)
result <- fast_col_quantiles_t(
  post_transform_loglog1p_array(
    y_reps_f_5,
    context = "figures.y_reps_f.q05",
    overflow_policy = "cap"
  )$values,
  probs = 0.05
)[1, ]
lines(result, col = 'black', lwd = 0.5)


n.samp
dim(y_reps_f)
dim(xbs)

idx_sub <- (TT-19+1):(TT)

profile_section("figures.build_y_reps_new", {
y_reps_new <- array(NA_real_,c(7,n.samp,length(idx_sub)))

xxx <- 1
for(t in 1:length(idx_sub)){
    tt <- idx_sub[t]
    for(s in 1:n.samp){
    gamma <- samp.gamma_95_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_95_exAL_synth_DISC[1,s]
    p00 <- 0.95
    mu <- xbs_retro[7,tt,s]
    y_reps_new[7,s,t] <- rexal(1, p00, mu, sigma, gamma)   
    
    gamma <- samp.gamma_80_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_80_exAL_synth_DISC[1,s]
    p00 <- 0.80
    mu <- xbs_retro[6,tt,s]
    y_reps_new[6,s,t] <- rexal(1, p00, mu, sigma, gamma)   

    gamma <- samp.gamma_65_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_65_exAL_synth_DISC[1,s]
    p00 <- 0.65
    mu <- xbs_retro[5,tt,s]
    y_reps_new[5,s,t] <- rexal(1, p00, mu, sigma, gamma)   

    gamma <- samp.gamma_50_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_50_exAL_synth_DISC[1,s]
    p00 <- 0.50
    mu <- xbs_retro[4,tt,s]
    y_reps_new[4,s,t] <- rexal(1, p00, mu, sigma, gamma)   

    gamma <- samp.gamma_35_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_35_exAL_synth_DISC[1,s]
    p00 <- 0.35
    mu <- xbs_retro[3,tt,s]
    y_reps_new[3,s,t] <- rexal(1, p00, mu, sigma, gamma)   

    gamma <- samp.gamma_20_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_20_exAL_synth_DISC[1,s]
    p00 <- 0.20
    mu <- xbs_retro[2,tt,s]
    y_reps_new[2,s,t] <- rexal(1, p00, mu, sigma, gamma)   
        
    gamma <- samp.gamma_5_exAL_synth_DISC[1,s]*xxx
    sigma <- samp.sigma_5_exAL_synth_DISC[1,s]
    p00 <- 0.05
    mu <- xbs_retro[1,tt,s]
    y_reps_new[1,s,t] <- rexal(1, p00, mu, sigma, gamma)   
    }
}

y_reps_5 <- y_reps_new[1,,]
y_reps_20 <- y_reps_new[2,,]
y_reps_35 <- y_reps_new[3,,]
y_reps_50 <- y_reps_new[4,,]
y_reps_65 <- y_reps_new[5,,]
y_reps_80 <- y_reps_new[6,,]
y_reps_95 <- y_reps_new[7,,]
for(t in 1:length(idx_sub)){
    target_len_hist_new <- nrow(y_reps_5)
    y_reps_5[,t] <- sort_to_len(y_reps_5[,t], target_len_hist_new, context = sprintf("y_reps_5[,%d]", t))
    y_reps_20[,t] <- sort_to_len(y_reps_20[,t], target_len_hist_new, context = sprintf("y_reps_20[,%d]", t))
    y_reps_35[,t] <- sort_to_len(y_reps_35[,t], target_len_hist_new, context = sprintf("y_reps_35[,%d]", t))
    y_reps_50[,t] <- sort_to_len(y_reps_50[,t], target_len_hist_new, context = sprintf("y_reps_50[,%d]", t))
    y_reps_65[,t] <- sort_to_len(y_reps_65[,t], target_len_hist_new, context = sprintf("y_reps_65[,%d]", t))
    y_reps_80[,t] <- sort_to_len(y_reps_80[,t], target_len_hist_new, context = sprintf("y_reps_80[,%d]", t))
    y_reps_95[,t] <- sort_to_len(y_reps_95[,t], target_len_hist_new, context = sprintf("y_reps_95[,%d]", t))
}

y_reps_new[1,,] <- y_reps_5  
y_reps_new[2,,] <- y_reps_20  
y_reps_new[3,,] <- y_reps_35  
y_reps_new[4,,] <- y_reps_50  
y_reps_new[5,,] <- y_reps_65  
y_reps_new[6,,] <- y_reps_80  
y_reps_new[7,,] <- y_reps_95 
})

profile_section("figures.save_y_reps_new_rds", {
  saveRDS(y_reps_new, file = post_cache_path("y_reps_new.rds"))
})

y_reps <- profile_section("figures.read_y_reps_new_rds", readRDS(post_cache_path("y_reps_new.rds")))

q_s    <- c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95)
n.q     <- length(q_s)

hist_exp_guard <- profile_section(
  "figures.transform_y_reps_hist_loglog1p",
  post_transform_loglog1p_array(
    y_reps[, , ],
    context = "figures.y_reps_hist.loglog1p",
    overflow_policy = "cap",
    report_path = post_cache_path("synth_multivar_hist_exp_guard.txt")
  )
)
synth <- profile_section("figures.synthesize_samples_y_reps_hist", synthesize_samples(hist_exp_guard$values, q_s))
dim(synth)

synth_q <- colQuantiles(synth, probs = q_s, type = 8)
synth_q <- t(synth_q)
dim(synth_q)


profile_section("figures.sort_synth_hist", {
  for (t in 1:length(idx_sub)) {
    synth[, t] <- sort_to_len(synth[, t], target_len = nrow(synth), context = sprintf("synth_hist[,%d]", t))
  }
})

idx <- idx_sub
plot.ts(rep(0,length(idx)), ylim = c(0,10))

SL <- San_Lorenzo_Daily_USGS_R[San_Lorenzo_Daily_USGS_R$Date >= timestamps[1] , ]

matlines(t(synth), col = 'pink', lwd = 0.1)

points(SL$data0[idx], lwd = 0.8)

for (i in 1:n.q) {
   lines(synth_q[i,], col = 'gray', lwd = 2)
}

lines(exp(new.theta.out_95_exAL_synth_DISC$exps[1,idx]), col = 'darkblue', lwd = 2)
lines(exp(new.theta.out_50_exAL_synth_DISC$exps[1,idx]), col = 'forestgreen', lwd = 2)
lines(exp(new.theta.out_5_exAL_synth_DISC$exps[1,idx]), col = 'darkred', lwd = 2)

lines(exp(new.theta.out_95_exAL_synth_DISC_uni$exps[1,idx]), col = 'lightblue', lwd = 2)
lines(exp(new.theta.out_50_exAL_synth_DISC_uni$exps[1,idx]), col = 'lightgreen', lwd = 2)
lines(exp(new.theta.out_5_exAL_synth_DISC_uni$exps[1,idx]), col = "#4a235a", lwd = 2)

p <- 7

p_uni <- dim(new.theta.out_50_exAL_synth_DISC_uni$sm)[1]
alpha <- 0.01
for(i in (p+2):p_uni){
    # plot.ts(new.theta.out_95_exAL_synth_DISC_uni$sm[i,], ylim = c(0.0,0.1))
    # lines(new.theta.out_95_exAL_synth_DISC_uni$sm[i,]+qnorm(0.975)*sqrt(new.theta.out_95_exAL_synth_DISC_uni$sC[i,i,]))
    # lines(new.theta.out_95_exAL_synth_DISC_uni$sm[i,]+qnorm(0.025)*sqrt(new.theta.out_95_exAL_synth_DISC_uni$sC[i,i,]))
    l <- new.theta.out_50_exAL_synth_DISC_uni$sm[i,1]+qnorm(alpha/2)*sqrt(new.theta.out_95_exAL_synth_DISC_uni$sC[i,i,1])
    u <- new.theta.out_50_exAL_synth_DISC_uni$sm[i,1]+qnorm(1-alpha/2)*sqrt(new.theta.out_95_exAL_synth_DISC_uni$sC[i,i,1])
    m <- new.theta.out_50_exAL_synth_DISC_uni$sm[i,1]
    print(c(l,m,u))
}

p <- 7

for(i in 2:(ppx)){
    m <- new.theta.out_50_exAL_synth_DISC$sm[p*(J+1)+i,]
    E <- qnorm(0.975)*sqrt(new.theta.out_50_exAL_synth_DISC$sC[p*(J+1)+i,p*(J+1)+i,])
    plot.ts(m, ylim=c(-0.3,0.3), col='darkgreen')
    lines(m+E, col='darkgreen')
    lines(m-E, col='darkgreen')
    m <- new.theta.out_5_exAL_synth_DISC$sm[p*(J+1)+i,]
    E <- qnorm(0.975)*sqrt(new.theta.out_5_exAL_synth_DISC$sC[p*(J+1)+i,p*(J+1)+i,])
    lines(m, ylim=c(-0.3,0.3), col='darkred')
    lines(m+E, col='red')
    lines(m-E, col='red')
    m <- new.theta.out_95_exAL_synth_DISC$sm[p*(J+1)+i,]
    E <- qnorm(0.975)*sqrt(new.theta.out_95_exAL_synth_DISC$sC[p*(J+1)+i,p*(J+1)+i,])
    lines(m, ylim=c(-0.3,0.3), col='darkblue')
    lines(m+E, col='blue')
    lines(m-E, col='blue')
    abline(h = 0, col='gray')
}

# Load libraries (run if not already loaded)

flow_data <- data.frame(Date = timestamps, Flow = Y[1,])


# Flood stage values in feet
flood_stages_ft <- c(21.76, 16.5)^3
# Convert to centimeters
flood_stages_cm <- flood_stages_ft*CFSToCMS_CONVERSION_FACTOR 
# Apply log(log(x + 1)) transformation
flood_stages_trans <- log(log(flood_stages_cm + 1))

event_dates <- as.Date(c(
  "1998-02-03",  # February 1998 Flood
  "2004-06-01",  # Levee and Floodwall Reconstruction
  "2017-02-07",  # February 2017 Flood
  "2023-01-09"  # January 2023 Flood
))

event_numbers <- as.character(1:4)
event_color <- "#D95F02" # Orange for vertical lines & labels

# Calculate y position (10% above the observed max value for clarity)
label_y <- max(flow_data$Flow, na.rm = TRUE) + 0.1 * diff(range(flow_data$Flow, na.rm = TRUE))
# Flood stage labels for annotation
flood_stage_labels <- c("Major Flooding", "Minor Flooding")

# --- Your existing plotting code with new additions ---
p <- ggplot(flow_data, aes(x = Date, y = Flow)) +
  geom_line(color = "#238b45", linewidth = 0.7, alpha = 0.92) +
  geom_vline(xintercept = event_dates, color = event_color, linetype = "dashed", linewidth = 0.5) +
  annotate(
    "text",
    x = event_dates,
    y = rep(label_y, length(event_dates)),
    label = event_numbers,
    fontface = "bold",
    color = event_color,
    size = 4,
    vjust = 0,
    hjust = 2
  ) +
  # Add flood stage horizontal lines
  geom_hline(
    yintercept = flood_stages_trans,
    linetype = c("dashed", "dashed"),
    color = c("gray", "gray"),
    linewidth = 0.8
  ) +
  # Label the flood stages at the rightmost end of the plot
  annotate(
    "text",
    x = max(flow_data$Date),
    y = flood_stages_trans,
    label = flood_stage_labels,
    hjust = 10.5,
    vjust = -0.3,
    color = c("black", "black"),
    fontface = "italic",
    size = 3.5
  ) +
  labs(
    title = "Daily Flow of San Lorenzo River at Big Trees, CA",
    subtitle = sprintf(
      "Measurements from %s to %s",
      format(min(flow_data$Date, na.rm = TRUE), "%B %d, %Y"),
      format(max(flow_data$Date, na.rm = TRUE), "%B %d, %Y")
    ),
    x = "Year",
    y = expression("Water Flow (Log-Log cm^3/s)")
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "italic", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p)

ggsave(
  filename = "SOURCE_WORKFLOW_REFERENCE",
  plot = p,
  width = 12,
  height = 6,
  units = "in",
  dpi = 900
)


# \caption{
# Daily log-log water flow (in cm$^3$/s) of the San Lorenzo River at the Big Trees USGS station from May 29, 1987 to December 25, 2022. The green curve shows the transformed daily flow. Vertical dashed lines and numbered labels mark key flood-related events: (1) February 1998 flood; (2) levee and floodwall reconstruction (2004); (3) February 2017 flood; (4) January 2023 flood. Horizontal dashed lines indicate official flood stages for the river, with the upper line corresponding to "Major Flooding" (21.76~ft, 663~cm) and the lower line to "Minor Flooding" (16.5~ft, 503~cm), both converted and displayed on the $\log(\log(x+1))$ scale. See the main text for further discussion of each event and flood stage threshold.
# }

series_colors <- c(
  "Precipitation" = "#1b9e77",    # green
  "Soil_Moisture" = "#386cb0",    # blue
  "Climate_PC1" = "#e6550d"       # orange
)

df_covariates <- data.frame(
  Date = as.Date(timestamps),
  Precipitation = X[, 1],
  Soil_Moisture = X[, 2],
  GDPC1 = X[, 3]
)
# 1. Select only relevant columns and rename for plotting clarity
df_plot <- df_covariates
colnames(df_plot) <- c("Date", "Precipitation", "Soil_Moisture", "Climate_PC1")

# 2. Convert to long format for ggplot (avoid slow pivot_longer)
df_long <- fast_long_by_row(
  mat = df_plot[, c("Precipitation", "Soil_Moisture", "Climate_PC1")],
  row_values = df_plot$Date,
  col_values = c("Precipitation", "Soil_Moisture", "Climate_PC1"),
  row_name = "Date",
  col_name = "Variable",
  value_name = "Value"
)


# Set Variable factor order
df_long$Variable <- factor(
  df_long$Variable,
  levels = c("Precipitation", "Soil_Moisture", "Climate_PC1")
)

# Custom labels for facets
custom_labels <- c(
  Precipitation = "Precipitation",
  Soil_Moisture = "Soil Moisture",
  Climate_PC1 = "1st Principal Comp."
)

# Facet plot with custom y-axis titles via `labeller`
p_facets <- ggplot(df_long, aes(x = Date, y = Value, color = Variable)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  scale_color_manual(values = series_colors) +
  facet_wrap(
    ~Variable, ncol = 1, scales = "free_y", strip.position = "left",
    labeller = as_labeller(custom_labels)
  ) +
  labs(
    title = "Exogeneous Data (Scaled)",
    subtitle = "at Santa Cruz Area",
    x = "Year",
    y = NULL
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "italic", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 13, color = "black"),
    strip.background = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

print(p_facets)

ggsave(
  filename = "SOURCE_WORKFLOW_REFERENCE",
  plot = p_facets,
  width = 12,
  height = 8,
  units = "in",
  dpi = 900
)


read_kv_map <- function(path) {
  out <- list()
  path <- as.character(path)
  if (!length(path) || is.na(path[[1]]) || !nzchar(path[[1]]) || !file.exists(path[[1]])) return(out)
  path <- path[[1]]
  lines <- readLines(path, warn = FALSE)
  if (!length(lines)) return(out)
  for (ln in lines) {
    pos <- regexpr("=", ln, fixed = TRUE)
    if (pos[[1]] <= 1L) next
    key <- trimws(substr(ln, 1L, pos[[1]] - 1L))
    val <- trimws(substr(ln, pos[[1]] + 1L, nchar(ln)))
    if (!nzchar(key)) next
    out[[key]] <- val
  }
  out
}

choose_source_by_priority <- function(df, source_regex, priorities) {
  rows <- df[grepl(source_regex, df$source_id), c("Date", "source_id", "discharge"), drop = FALSE]
  if (nrow(rows) == 0L) return(data.frame(Date = as.Date(character(0)), value = numeric(0)))
  rows$priority <- match(rows$source_id, priorities)
  rows$priority[is.na(rows$priority)] <- length(priorities) + 1L
  rows <- rows[order(rows$Date, rows$priority, rows$source_id), , drop = FALSE]
  rows <- rows[!duplicated(rows$Date), c("Date", "discharge"), drop = FALSE]
  names(rows)[2] <- "value"
  rows
}

resolve_retros_selection_policy <- function(bundle_root, cutoff_date) {
  glofas_priority <- c(
    "glofas_hist_v40_lisflood_cons",
    "glofas_hist_v31_lisflood_cons",
    "glofas_hist_v21_htessel_cons",
    "glofas_legacy_reanalysis_v30",
    "glofas_synth_retro_ens_mean"
  )
  nws_priority <- c(
    "nws_synth_retro_ens_mean",
    "nws_retro_v30",
    "nws_retro_v21",
    "nws_retro_v20",
    "nws_retro_v12"
  )
  if (!nzchar(bundle_root)) {
    return(list(glofas_priority = glofas_priority, nws_priority = nws_priority))
  }

  meta_path <- file.path(bundle_root, "meta.yaml")
  if (!file.exists(meta_path)) {
    return(list(glofas_priority = glofas_priority, nws_priority = nws_priority))
  }
  meta <- tryCatch(yaml::read_yaml(meta_path), error = function(e) NULL)
  sel <- meta$config$inputs$retros$selection_policy
  if (!is.list(sel)) {
    return(list(glofas_priority = glofas_priority, nws_priority = nws_priority))
  }

  `%or_default%` <- function(x, y) if (is.null(x)) y else x
  cutoff_use <- suppressWarnings(as.Date(cutoff_date))

  pick_window_source <- function(windows, cutoff_date) {
    if (!is.list(windows) || is.na(cutoff_date)) return("")
    for (w in windows) {
      if (!is.list(w)) next
      src <- tolower(as.character(w$source_id %or_default% ""))
      if (!nzchar(src)) next
      start <- suppressWarnings(as.Date(as.character(w$start %or_default% NA_character_)))
      end <- suppressWarnings(as.Date(as.character(w$end %or_default% NA_character_)))
      if (is.na(start) || is.na(end)) next
      if (cutoff_date >= start && cutoff_date <= end) return(src)
    }
    ""
  }

  keep_ids <- tolower(as.character(unlist(sel$keep_source_ids %or_default% character(0), use.names = FALSE)))
  keep_ids <- keep_ids[nzchar(keep_ids)]
  keep_glofas <- keep_ids[grepl("glofas", keep_ids)]
  keep_nws <- keep_ids[grepl("^nws", keep_ids)]
  if (length(keep_glofas) > 0L) {
    glofas_priority <- unique(c(keep_glofas, glofas_priority))
  }
  if (length(keep_nws) > 0L) {
    nws_priority <- unique(c(keep_nws, nws_priority))
  }

  win_glofas <- pick_window_source(sel$glofas_by_cutoff_windows, cutoff_use)
  win_nws <- pick_window_source(sel$nws_by_cutoff_windows, cutoff_use)
  if (nzchar(win_glofas)) {
    glofas_priority <- unique(c(win_glofas, glofas_priority))
  }
  if (nzchar(win_nws)) {
    nws_priority <- unique(c(win_nws, nws_priority))
  }

  list(glofas_priority = glofas_priority, nws_priority = nws_priority)
}

build_forecats_retros_plot <- function() {
  fallback <- data.frame(
    Date = as.Date(timestamps),
    GloFAS = as.numeric(Y[2, ]),
    NWS = as.numeric(Y[3, ]),
    stringsAsFactors = FALSE
  )

  if (!exists("RETROS_PATH", inherits = TRUE)) return(fallback)
  retros_path <- as.character(get("RETROS_PATH", inherits = TRUE))
  if (!nzchar(retros_path) || !file.exists(retros_path)) return(fallback)

  retros_wide <- tryCatch(read.csv(retros_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (!is.data.frame(retros_wide) || nrow(retros_wide) == 0L) return(fallback)
  date_col <- if ("Date" %in% names(retros_wide)) "Date" else if ("date" %in% names(retros_wide)) "date" else ""
  if (!nzchar(date_col)) return(fallback)
  ncol_name <- if ("NWS3.0" %in% names(retros_wide)) "NWS3.0" else if ("NWS" %in% names(retros_wide)) "NWS" else ""
  if (!("USGS" %in% names(retros_wide)) || !("GloFAS" %in% names(retros_wide)) || !nzchar(ncol_name)) return(fallback)

  retros_wide <- data.frame(
    Date = as.Date(retros_wide[[date_col]]),
    USGS = suppressWarnings(as.numeric(retros_wide$USGS)),
    GloFAS = suppressWarnings(as.numeric(retros_wide$GloFAS)),
    NWS = suppressWarnings(as.numeric(retros_wide[[ncol_name]])),
    stringsAsFactors = FALSE
  )
  retros_wide <- retros_wide[!is.na(retros_wide$Date), , drop = FALSE]

  shared_root <- dirname(dirname(retros_path))
  source_map <- read_kv_map(file.path(shared_root, "source_map.txt"))
  snapshot_root <- as.character(source_map[["snapshot_root"]])
  if (!length(snapshot_root) || is.na(snapshot_root[[1]]) || !nzchar(snapshot_root[[1]])) {
    snapshot_root <- ""
  } else {
    snapshot_root <- snapshot_root[[1]]
  }
  snap_map <- read_kv_map(file.path(snapshot_root, "snapshot_source_map.txt"))
  bundle_root <- as.character(snap_map[["bundle_root"]])
  if (!length(bundle_root) || is.na(bundle_root[[1]]) || !nzchar(bundle_root[[1]])) {
    bundle_root <- ""
  } else {
    bundle_root <- bundle_root[[1]]
  }
  long_path <- file.path(bundle_root, "inputs", "retros_daily.csv")
  if (nzchar(bundle_root) && file.exists(long_path)) {
    long_retro <- tryCatch(read.csv(long_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (is.data.frame(long_retro) && ("source_id" %in% names(long_retro)) && ("discharge_cms" %in% names(long_retro))) {
      dcol <- if ("date" %in% names(long_retro)) "date" else if ("Date" %in% names(long_retro)) "Date" else ""
      if (nzchar(dcol)) {
        long_tbl <- data.frame(
          Date = as.Date(long_retro[[dcol]]),
          source_id = tolower(as.character(long_retro$source_id)),
          discharge = suppressWarnings(as.numeric(long_retro$discharge_cms)),
          stringsAsFactors = FALSE
        )
        long_tbl <- long_tbl[!is.na(long_tbl$Date) & is.finite(long_tbl$discharge), , drop = FALSE]
        if (is.finite(CUTOFF_DATE)) {
          long_tbl <- long_tbl[long_tbl$Date <= CUTOFF_DATE, , drop = FALSE]
        }
        policy <- resolve_retros_selection_policy(bundle_root, CUTOFF_DATE)
        glofas_sel <- choose_source_by_priority(long_tbl, "glofas", policy$glofas_priority)
        nws_sel <- choose_source_by_priority(long_tbl, "^nws", policy$nws_priority)
        if (nrow(glofas_sel) > 0L && nrow(nws_sel) > 0L) {
          names(glofas_sel)[2] <- "GloFAS"
          names(nws_sel)[2] <- "NWS"
          retros_wide <- merge(retros_wide[, c("Date", "USGS"), drop = FALSE], glofas_sel, by = "Date", all = FALSE)
          retros_wide <- merge(retros_wide, nws_sel, by = "Date", all = FALSE)
        }
      }
    }
  }

  usgs_ref <- data.frame(
    Date = as.Date(San_Lorenzo_Daily_USGS_R$time),
    usgs_raw = suppressWarnings(as.numeric(San_Lorenzo_Daily_USGS_R$X_00060_00003) * CFSToCMS_CONVERSION_FACTOR),
    stringsAsFactors = FALSE
  )
  cmp <- merge(retros_wide[, c("Date", "USGS"), drop = FALSE], usgs_ref, by = "Date", all = FALSE)
  cmp <- cmp[is.finite(cmp$USGS) & is.finite(cmp$usgs_raw), , drop = FALSE]
  scale_mode <- "log1p_cms"
  if (nrow(cmp) >= 10L) {
    mae_raw <- mean(abs(cmp$USGS - cmp$usgs_raw), na.rm = TRUE)
    mae_log1p <- mean(abs(expm1(cmp$USGS) - cmp$usgs_raw), na.rm = TRUE)
    if (is.finite(mae_raw) && is.finite(mae_log1p) && mae_raw <= mae_log1p) {
      scale_mode <- "raw_cms"
    }
  }

  to_loglog <- function(x, scale_mode) {
    x <- suppressWarnings(as.numeric(x))
    if (identical(scale_mode, "raw_cms")) {
      x <- pmax(x, 1.0e-8)
      return(log(log(x + 1)))
    }
    x <- pmax(x, 1.0e-8)
    log(x)
  }

  out <- data.frame(
    Date = as.Date(retros_wide$Date),
    GloFAS = to_loglog(retros_wide$GloFAS, scale_mode),
    NWS = to_loglog(retros_wide$NWS, scale_mode),
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$GloFAS) & is.finite(out$NWS), , drop = FALSE]
  if (nrow(out) < 10L) return(fallback)
  out
}

df_retro <- build_forecats_retros_plot()

# Reshape to long format for ggplot (avoid slow pivot_longer)
df_retro_long <- fast_long_by_row(
  mat = df_retro[, c("GloFAS", "NWS")],
  row_values = df_retro$Date,
  col_values = c("GloFAS", "NWS"),
  row_name = "Date",
  col_name = "Source",
  value_name = "Value"
)

# Set factor order for consistent legend/order
df_retro_long$Source <- factor(
  df_retro_long$Source,
  levels = c("GloFAS", "NWS")
)

# GloFAS panel (orange)
p_glofas <- ggplot(df_retro, aes(x = Date, y = GloFAS)) +
  geom_line(color = "#E67E22", linewidth = 0.7, alpha = 0.92) +
  labs(
    title = "GloFAS Retrospective Analysis",
    x = NULL,
    y = expression("Water Flow (Log-Log cm^3/s)")
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  ylim(-2, 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

# NWS panel (purple)
p_nws <- ggplot(df_retro, aes(x = Date, y = NWS)) +
  geom_line(color = "#756bb1", linewidth = 0.7, alpha = 0.92) +
  labs(
    title = "NWS Retrospective Analysis",
    x = "Year",
    y = expression("Water Flow (Log-Log cm^3/s)")
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  ylim(-3, 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

# Combine the two plots into a 2-row figure
p_combined <- p_glofas / p_nws + plot_layout(ncol = 1)

print(p_combined)

ggsave(
  filename = "SOURCE_WORKFLOW_REFERENCE",
  plot = p_combined,
  width = 12,
  height = 8,
  units = "in",
  dpi = 900
)




# 1. Filter USGS time series for plotting window
plot_start <- PLOT_START_DATE
plot_end <- PLOT_END_DATE
if (is.na(special_event_date) &&
    !is.na(CUTOFF_DATE) &&
    as.Date(CUTOFF_DATE) == as.Date("2022-12-25")) {
  special_event_date <- as.Date("2023-01-09")
  if (!nzchar(special_event_label)) {
    special_event_label <- "Jan 9: Flood"
  }
}
show_special_event_marker <- !is.na(special_event_date) &&
  nzchar(special_event_label) &&
  special_event_date >= as.Date(plot_start) &&
  special_event_date <= as.Date(plot_end)

compute_jan9_label_y <- function(values, offset = 0.15, fallback = -0.15) {
  y <- suppressWarnings(min(values, na.rm = TRUE))
  if (!is.finite(y)) return(fallback)
  y - offset
}

safe_max_date <- function(values, fallback_date) {
  fallback_date <- as.Date(fallback_date)
  out <- suppressWarnings(max(as.Date(values), na.rm = TRUE))
  if (!is.finite(out)) return(fallback_date)
  as.Date(out, origin = "1970-01-01")
}

add_jan9_flood_marker <- function(plot_obj, label_y) {
  if (!isTRUE(show_special_event_marker)) return(plot_obj)
  plot_obj +
    geom_vline(
      xintercept = as.numeric(special_event_date),
      color = "#4a235a",
      linetype = "dashed",
      linewidth = 0.5,
      alpha = 0.8
    ) +
    annotate(
      "text",
      x = special_event_date,
      y = label_y,
      label = special_event_label,
      color = "#4a235a",
      vjust = 4,
      hjust = -0.1,
      fontface = "bold",
      size = 3.5
    )
}

safe_log_values <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x) | x <= 0] <- NA_real_
  log(x)
}

compute_adaptive_ylim <- function(..., fallback = c(-1, 3.5), pad_frac = 0.06, min_pad = 0.12) {
  vals <- unlist(lapply(list(...), function(v) suppressWarnings(as.numeric(v))), use.names = FALSE)
  vals <- vals[is.finite(vals)]
  if (!length(vals)) {
    return(as.numeric(fallback))
  }
  lo <- min(vals, na.rm = TRUE)
  hi <- max(vals, na.rm = TRUE)
  span <- hi - lo
  if (!is.finite(span) || span <= 0) span <- 1
  pad <- max(min_pad, span * pad_frac)
  c(lo - pad, hi + pad)
}

df_retro_plot <- df_retro_long %>%
  filter(Date >= plot_start & Date < FORECAST_START_DATE)


usgs_plot_df <- San_Lorenzo_Daily_USGS_R %>%
  filter(time >= plot_start & time <= plot_end) %>%
  mutate(
    obs_type = ifelse(time >= FORECAST_START_DATE, "After", "Before"),
    value = log(log(X_00060_00003 * CFSToCMS_CONVERSION_FACTOR + 1))
  ) %>%
  filter(is.finite(value))

cutoff_label_y <- compute_jan9_label_y(usgs_plot_df$value, offset = 0.15, fallback = -0.15)
jan9_label_y <- cutoff_label_y
usgs_right_x <- safe_max_date(usgs_plot_df$time, fallback_date = plot_end)

# 2. Get GloFAS and NWS forecast dates
forecast_start <- FORECAST_START_DATE
glofas_dates <- daily_dates_for_matrix_rows(
  ensembles[[1]],
  start_date = forecast_start,
  context = "ensemble_dates.glofas"
)
nws_dates <- daily_dates_for_matrix_rows(
  ensembles[[2]],
  start_date = forecast_start,
  context = "ensemble_dates.nws"
)

# Set color codes
glofas_color <- "#E67E22"   # Bright orange
nws_color    <- "#756bb1"   # Purple
usgs_green   <- "#238b45"   # Dark green (for line and early points)
usgs_after_color <- "#B22222"  # Post-cutoff USGS marker/line color

# USGS points
usgs_before_df <- usgs_plot_df %>% filter(obs_type == "Before") %>%
  mutate(Source = "USGS")
usgs_after_df  <- usgs_plot_df %>% filter(obs_type == "After") %>%
  mutate(Source = "USGS")
glofas_before_df <- df_retro_plot %>% filter(Source == "GloFAS")
nws_before_df    <- df_retro_plot %>% filter(Source == "NWS")
y_max <- max(
  usgs_before_df$value, 
  glofas_before_df$Value, 
  nws_before_df$Value, 
  usgs_after_df$value,
  na.rm = TRUE
)
if (!is.finite(y_max)) y_max <- 0


p <- ggplot() +
  annotate(
    "text",
    x = usgs_right_x,
    y = flood_stages_trans,
    label = flood_stage_labels,
    hjust = 10.5,     # places label just to the right of the axis
    vjust = -0.5,
    color = c("black", "black"),
    fontface = "italic",
    size = 3.5
  ) +
  annotate(
    "text",
    x = CUTOFF_DATE,
    y = cutoff_label_y,
    label = cutoff_label_short,
    color = "gray40",
	    size = 3.5,
	    fontface = "bold",
	    vjust = 4,
	    hjust = -0.1 
	  ) +

  # USGS before
    # Add flood stage horizontal lines
  geom_hline(
    yintercept = flood_stages_trans,
    linetype = c("dashed", "dashed"),
    color = c("gray", "gray"),
    linewidth = 0.8
  ) +
  geom_line(
    data = usgs_before_df, 
    aes(x = time, y = value, color = Source, linetype = Source), linewidth = 0.5
  ) +
  geom_point(
    data = usgs_before_df, 
    aes(x = time, y = value, color = Source, shape = Source), size = 1.4
  ) +
  # GloFAS before
  geom_line(
    data = glofas_before_df,
    aes(x = Date, y = Value, color = Source, linetype = Source), linewidth = 0.5, alpha = 0.85
  ) +
  geom_point(
    data = glofas_before_df,
    aes(x = Date, y = Value, color = Source, shape = Source), size = 1.4, alpha = 0.85
  ) +
  # NWS before
  geom_line(
    data = nws_before_df,
    aes(x = Date, y = Value, color = Source, linetype = Source), linewidth = 0.5, alpha = 0.85
  ) +
  geom_point(
    data = nws_before_df,
    aes(x = Date, y = Value, color = Source, shape = Source), size = 1.4, alpha = 0.85
  ) +
  # GloFAS ensembles after
  geom_line(
    data = fast_long_ensembles(ensembles[[1]], glofas_dates),
    aes(x = Date, y = value, group = member),
    color = glofas_color, alpha = 0.22, linewidth = 0.5, show.legend = FALSE
  ) +
  # NWS ensembles after
  geom_line(
    data = fast_long_ensembles(ensembles[[2]], nws_dates),
    aes(x = Date, y = value, group = member),
    color = nws_color, alpha = 0.22, linewidth = 0.5, show.legend = FALSE
  ) +
  # USGS after
  geom_line(
    data = usgs_after_df,
    aes(x = time, y = value), color = usgs_after_color, linewidth = 0.5, linetype = "dashed", show.legend = FALSE
  ) +
  geom_point(
    data = usgs_after_df,
    aes(x = time, y = value), color = usgs_after_color, size = 2, show.legend = FALSE
  ) +
  scale_x_date(breaks = pretty_breaks(6), date_labels = "%b %d") +
  scale_color_manual(
    name = "Data Source",
    values = c("USGS" = usgs_green, "GloFAS" = glofas_color, "NWS" = nws_color)
  ) +
  scale_linetype_manual(
    name = "Data Source",
    values = c("USGS" = "solid", "GloFAS" = "solid", "NWS" = "solid")
  ) +
  scale_shape_manual(
    name = "Data Source",
    values = c("USGS" = 16, "GloFAS" = 16, "NWS" = 16)
  ) +
  # Vertical dashed line at forecast start
geom_vline(
  xintercept = as.numeric(CUTOFF_DATE), 
  color = "gray40", linetype = "dashed", linewidth = 0.5, alpha = 0.8
) +
labs(
  title = "Observed and Retrospective River Flow\nwith GloFAS and NWS Forecast Ensembles",
  x = "Date",
  y = expression("Water Flow (Log-Log cm^3/s)")
) +
  guides(
    color = guide_legend(override.aes = list(size = 2)),
    shape = guide_legend(override.aes = list(size = 3))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5, margin = margin(b = 8)),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
p <- add_jan9_flood_marker(p, jan9_label_y)

print(p)

ggsave(
  filename = "SOURCE_WORKFLOW_REFERENCE",
  plot = p,
  width = 12,
  height = 6,
  units = "in",
  dpi = 900
)


  # subtitle = paste(
  #   "Forecasts initiated Dec 26, 2022 (dashed vertical line).",
  #   "\nUSGS (green), GloFAS (orange), and NWS (purple)",
  #   "\nare shown with points and thin lines before the forecast.",
  #   sep = ""
  # ),

idx <- idx_sub


# 1. Dates for fit and forecast
fit_dates <- as.Date(timestamps[idx])
forecast_dates <- daily_dates_for_matrix_cols(
  synth_f,
  start_date = fit_dates[length(fit_dates)] + 1,
  context = "posterior_dates.synth_f"
)

# 2. Posterior samples, tidy for ggplot (long format; avoid pivot_longer)
df_post_fit <- fast_long_by_row(
  mat = synth,
  row_values = seq_len(nrow(synth)),
  col_values = fit_dates,
  row_name = "sample",
  col_name = "Date",
  value_name = "Value"
)
df_post_fit$Type <- "Fit"

df_post_forecast <- fast_long_by_row(
  mat = synth_f,
  row_values = seq_len(nrow(synth_f)),
  col_values = forecast_dates,
  row_name = "sample",
  col_name = "Date",
  value_name = "Value"
)
df_post_forecast$Type <- "Forecast"

df_post <- bind_rows(df_post_fit, df_post_forecast)

# 3. Quantile curves (avoid pivot_longer)
df_q_fit <- fast_long_by_row(
  mat = synth_q,
  row_values = seq_len(nrow(synth_q)),
  col_values = fit_dates,
  row_name = "quantile",
  col_name = "Date",
  value_name = "Value"
)
df_q_fit$Type <- "Fit"

df_q_forecast <- fast_long_by_row(
  mat = synth_f_q,
  row_values = seq_len(nrow(synth_f_q)),
  col_values = forecast_dates,
  row_name = "quantile",
  col_name = "Date",
  value_name = "Value"
)
df_q_forecast$Type <- "Forecast"

df_q <- bind_rows(df_q_fit, df_q_forecast)

# 4. Observed values for USGS
obs_df <- usgs_plot_df %>% 
  mutate(Source = "USGS", colgroup = ifelse(obs_type == "After", "After", "Before"))
obs_label_x <- safe_max_date(obs_df$time, fallback_date = usgs_right_x)
glofas_after_ens_df <- fast_long_ensembles(ensembles[[1]], glofas_dates)
nws_after_ens_df <- fast_long_ensembles(ensembles[[2]], nws_dates)
ylim_post_samples <- compute_adaptive_ylim(
  safe_log_values(df_post$Value),
  safe_log_values(df_q$Value),
  obs_df$value,
  glofas_before_df$Value,
  nws_before_df$Value,
  glofas_after_ens_df$value,
  nws_after_ens_df$value,
  flood_stages_trans,
  cutoff_label_y,
  jan9_label_y
)

p_post <- ggplot() +
  # Flood stage lines and labels
  geom_hline(
    yintercept = flood_stages_trans,
    linetype = "dashed",
    color = "gray",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = CUTOFF_DATE,
    y = cutoff_label_y,
    label = cutoff_label_short,
    color = "gray40",
    size = 3.5,
    fontface = "bold",
    vjust = 4,
    hjust = -0.1 
  ) +
  annotate(
    "text",
    x = obs_label_x,
    y = flood_stages_trans,
    label = flood_stage_labels,
    hjust = 10.5,
    vjust = -0.5,
    color = "black",
    fontface = "italic",
    size = 3.5
  ) +
  # Vertical lines for forecast init and flood
  geom_vline(
    xintercept = as.numeric(CUTOFF_DATE), 
    color = "gray40", linetype = "dashed", linewidth = 0.5, alpha = 0.8
  ) +
  # Posterior samples ("spaghetti")
  geom_line(
    data = df_post, 
    aes(x = Date, y = log(Value), group = interaction(Type, sample)), 
    color = "pink", linewidth = 0.15, alpha = 0.15
  ) +
  # Posterior quantile curves (thinner black lines)
  geom_line(
    data = df_q, 
    aes(x = Date, y = log(Value), group = interaction(Type, quantile)), 
    color = "black", linewidth = 0.1
  ) +
  # USGS obs: before forecast
  geom_point(
    data = obs_df %>% filter(colgroup == "Before"), 
    aes(x = time, y = (value)), 
    color = usgs_green, size = 1.5
  ) +
  # USGS obs: after forecast (light green)
  geom_point(
    data = obs_df %>% filter(colgroup == "After"), 
    aes(x = time, y = (value)), 
    color = usgs_after_color, size = 2
  ) +
  geom_line(
    data = obs_df %>% filter(colgroup == "Before"),
    aes(x = time, y = (value)), color = usgs_green, linewidth = 0.5
  ) +
  geom_line(
    data = obs_df %>% filter(colgroup == "After"),
    aes(x = time, y = (value)), color = usgs_after_color, linewidth = 0.5, linetype = "dashed"
  ) +
  ############################
# GloFAS before (gray)
geom_line(
  data = glofas_before_df,
  aes(x = Date, y = Value, linetype = Source),
  color = "gray", linewidth = 0.5, alpha = 0.85
) +
geom_point(
  data = glofas_before_df,
  aes(x = Date, y = Value, shape = Source),
  color = "gray", size = 1.4, alpha = 0.85
) +
# NWS before (gray)
geom_line(
  data = nws_before_df,
  aes(x = Date, y = Value, linetype = Source),
  color = "gray", linewidth = 0.5, alpha = 0.85
) +
geom_point(
  data = nws_before_df,
  aes(x = Date, y = Value, shape = Source),
  color = "gray", size = 1.4, alpha = 0.85
) +
# GloFAS ensembles after (gray)
geom_line(
  data = glofas_after_ens_df,
  aes(x = Date, y = value, group = member),
  color = "gray", alpha = 0.22, linewidth = 0.5, show.legend = FALSE
) +
# NWS ensembles after (gray)
geom_line(
  data = nws_after_ens_df,
  aes(x = Date, y = value, group = member),
  color = "gray", alpha = 0.22, linewidth = 0.5, show.legend = FALSE
) +
  coord_cartesian(ylim = ylim_post_samples) +
  ############################
  scale_x_date(breaks = pretty_breaks(6), date_labels = "%b %d") +
  labs(
    title = "Posterior Predictive Samples and Quantiles\nwith USGS Observed Flow",
    x = "Date (2022-2023)",
    y = expression("Water Flow (Log-Log cm^3/s)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
p_post <- add_jan9_flood_marker(p_post, jan9_label_y)

print(p_post)

ggsave(
  filename = "SOURCE_WORKFLOW_REFERENCE",
  plot = p_post,
  width = 12,
  height = 6,
  units = "in",
  dpi = 900
)



# Dates for fit (historical) and forecast
fit_dates <- as.Date(timestamps[idx])
forecast_dates <- daily_dates_for_matrix_cols(
  synth_f2,
  start_date = fit_dates[length(fit_dates)] + 1,
  context = "posterior_dates.synth_f2"
)

# 1. Posterior samples: historical (fit) and forecast (avoid pivot_longer)
df_post_fit <- fast_long_by_row(
  mat = log(synth_hist_uni),
  row_values = seq_len(nrow(synth_hist_uni)),
  col_values = fit_dates,
  row_name = "sample",
  col_name = "Date",
  value_name = "Value"
)
df_post_fit$Type <- "Fit"

df_post_forecast <- fast_long_by_row(
  mat = log(synth_f2),
  row_values = seq_len(nrow(synth_f2)),
  col_values = forecast_dates,
  row_name = "sample",
  col_name = "Date",
  value_name = "Value"
)
df_post_forecast$Type <- "Forecast"

df_post <- bind_rows(df_post_fit, df_post_forecast)

# 2. Quantile curves: historical (fit) and forecast (avoid pivot_longer)
df_q_fit <- fast_long_by_row(
  mat = log(synth_hist_uni_q),
  row_values = seq_len(nrow(synth_hist_uni_q)),
  col_values = fit_dates,
  row_name = "quantile",
  col_name = "Date",
  value_name = "Value"
)
df_q_fit$Type <- "Fit"

df_q_forecast <- fast_long_by_row(
  mat = log(synth_f2_q),
  row_values = seq_len(nrow(synth_f2_q)),
  col_values = forecast_dates,
  row_name = "quantile",
  col_name = "Date",
  value_name = "Value"
)
df_q_forecast$Type <- "Forecast"

df_q <- bind_rows(df_q_fit, df_q_forecast)

# 3. Observed values for USGS
obs_df <- usgs_plot_df %>% 
  mutate(Source = "USGS", colgroup = ifelse(obs_type == "After", "After", "Before"))
obs_label_x <- safe_max_date(obs_df$time, fallback_date = usgs_right_x)
ylim_post_counter <- compute_adaptive_ylim(
  df_post$Value,
  df_q$Value,
  obs_df$value,
  flood_stages_trans,
  cutoff_label_y,
  jan9_label_y
)

# 4. Plot (as before, no need to change this part except color for 'After' points/lines)
p_post <- ggplot() +
  # Vertical lines for forecast init and flood
  geom_vline(
    xintercept = as.numeric(CUTOFF_DATE), 
    color = "gray40", linetype = "dashed", linewidth = 0.5, alpha = 0.8
  ) +
  # Posterior samples
  geom_line(
    data = df_post, 
    aes(x = Date, y = Value, group = interaction(Type, sample)), 
    color = "pink", linewidth = 0.15, alpha = 0.15
  ) +
  # Posterior quantile curves
  geom_line(
    data = df_q, 
    aes(x = Date, y = Value, group = interaction(Type, quantile)), 
    color = "black", linewidth = 0.1
  ) +
  # USGS obs: before forecast
  geom_point(
    data = obs_df %>% filter(colgroup == "Before"), 
    aes(x = time, y = (value)), 
    color = usgs_green, size = 1.5
  ) +
  # USGS obs: after forecast (DARK RED)
  geom_point(
    data = obs_df %>% filter(colgroup == "After"), 
    aes(x = time, y = (value)), 
    color = usgs_after_color, size = 2
  ) +
  geom_line(
    data = obs_df %>% filter(colgroup == "Before"),
    aes(x = time, y = (value)), color = usgs_green, linewidth = 0.5
  ) +
  geom_line(
    data = obs_df %>% filter(colgroup == "After"),
    aes(x = time, y = (value)), color = usgs_after_color, linewidth = 0.5, linetype = "dashed"
  ) +
  # Flood stage lines and labels
  geom_hline(
    yintercept = flood_stages_trans,
    linetype = "dashed",
    color = "gray",
    linewidth = 0.8
  ) +
    coord_cartesian(ylim = ylim_post_counter) +
  annotate(
    "text",
    x = obs_label_x,
    y = flood_stages_trans,
    label = flood_stage_labels,
    hjust = 10.5,
    vjust = -0.5,
    color = "black",
    fontface = "italic",
    size = 3.5
  ) +
    annotate(
    "text",
    x = CUTOFF_DATE,
    y = cutoff_label_y,
    label = cutoff_label_short,
    color = "gray40",
    size = 3.5,
    fontface = "bold",
    vjust = 4,
    hjust = -0.1 
  ) +
  ############################
  scale_x_date(breaks = pretty_breaks(6), date_labels = "%b %d") +
  labs(
    title = "Posterior Predictive Samples and Quantiles\nwith USGS Observed Flow",
    x = "Date",
    y = expression("Water Flow (Log-Log cm^3/s)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
p_post <- add_jan9_flood_marker(p_post, jan9_label_y)

print(p_post)

ggsave(
  filename = "SOURCE_WORKFLOW_REFERENCE",
  plot = p_post,
  width = 12,
  height = 6,
  units = "in",
  dpi = 900
)


# -- 1. Prepare Data --
# Flood stage values in feet
flood_stages_ft <- c(21.76, 16.5)^3
# Convert to centimeters
flood_stages_cm <- flood_stages_ft*CFSToCMS_CONVERSION_FACTOR 
# Apply log(log(x + 1)) transformation
flood_stages_trans <- log(log(flood_stages_cm + 1))

idx <- time_cuts[3]:time_cuts[4]
dates <- as.Date(dates_ts_usgs[idx])         # Dates for plotting window
percentiles <- c(0.025, 0.5, 0.975)

# Helper: Extract quantile trajectory for a given quantile
get_quantile_trajectory <- function(arr, qidx, dates, idx, quantile_name) {
  mat <- arr[qidx, idx, , drop = FALSE]
  mat <- matrix(mat, nrow = length(idx), ncol = dim(arr)[3])
  qt_res <- t(fast_row_quantiles_t(mat, probs = percentiles))
  colnames(qt_res) <- c("Lower", "Median", "Upper")
  data.frame(
    Date = dates,
    Quantile = quantile_name,
    Lower = qt_res[, "Lower"],
    Median = qt_res[, "Median"],
    Upper = qt_res[, "Upper"]
  )
}

# Map quantile names to their index in the first dimension
quantiles_map <- list(
  "5th"  = 1,
  "20th" = 2,
  "35th" = 3,
  "50th" = 4,
  "65th" = 5,
  "80th" = 6,
  "95th" = 7
)

# -- 2. All quantile trajectories --
quant_df_list <- lapply(names(quantiles_map), function(qname) {
  qidx <- quantiles_map[[qname]]
  get_quantile_trajectory(xbs_retro, qidx, dates, idx, qname)
})
quant_df <- bind_rows(quant_df_list)

# -- 3. Observed USGS series --
obs_df <- data.frame(
  Date = dates,
  Value = Y[1, idx]
)

flood_lines <- data.frame(
  y = flood_stages_trans,
  Stage = paste0(lev_flood, " ft")
)

alpha_val <- 0.11
# -- 5. Plot: Publication-Ready --
p <- ggplot() +
    annotate(
    "text",
    x =  max(as.Date(dates_ts_usgs[idx])),
    y = flood_stages_trans,
    label = flood_stage_labels,
    hjust = 10.5,
    vjust = -0.5,
    color = "black",
    fontface = "italic",
    size = 3.5
  ) +
  # Flood reference lines
  geom_hline(
    data = flood_lines,
    aes(yintercept = y), linetype = "dashed", color = "gray50", linewidth = 0.6
  ) +
  # 95th quantile (blue, ribbon)
  geom_ribbon(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, ymin = Lower, ymax = Upper),
    fill = "#2171b5", alpha = alpha_val
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, y = Median), color = "#2171b5", linewidth = 0.2
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, y = Lower), color = "blue", linewidth = 0.05
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, y = Upper), color = "blue", linewidth = 0.05
  ) +
  # 5th quantile (red, ribbon)
  geom_ribbon(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, ymin = Lower, ymax = Upper),
    fill = "#b2182b", alpha = alpha_val
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, y = Median), color = "#b2182b", linewidth = 0.2
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, y = Lower), color = "red", linewidth = 0.05
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, y = Upper), color = "red", linewidth = 0.05
  ) +
  # 50th quantile (green, ribbon)
  geom_ribbon(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, ymin = Lower, ymax = Upper),
    fill = "#238b45", alpha = alpha_val
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, y = Median), color = "#238b45", linewidth = 0.2
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, y = Lower), color = "green", linewidth = 0.05
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, y = Upper), color = "green", linewidth = 0.05
  ) +
  # Observed series (black, line and points)
  geom_point(
    data = obs_df, aes(x = Date, y = Value),
    color = "black", size = 0.2
  ) +
  geom_line(
    data = obs_df, aes(x = Date, y = Value),
    color = "black", linewidth = 0.1
  ) +
  labs(
    title = "Quantile Dynamics: 2017–2019",
    x = NULL,
    y = expression("Water Flow (Log-Log cm^3/s)")
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  coord_cartesian(ylim = c(-2, 3)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p)

# Save if desired
ggsave(
  filename = "SOURCE_WORKFLOW_REFERENCE",
  plot = p, width = 12, height = 6, units = "in", dpi = 900
)



# -- 1. Prepare Data --
# Flood stage values in feet
flood_stages_ft <- c(21.76, 16.5)^3
# Convert to centimeters
flood_stages_cm <- flood_stages_ft*CFSToCMS_CONVERSION_FACTOR 
# Apply log(log(x + 1)) transformation
flood_stages_trans <- log(log(flood_stages_cm + 1))

idx <- time_cuts[1]:time_cuts[2]
dates <- as.Date(dates_ts_usgs[idx])         # Dates for plotting window
percentiles <- c(0.025, 0.5, 0.975)

# Helper: Extract quantile trajectory for a given quantile
get_quantile_trajectory <- function(arr, qidx, dates, idx, quantile_name) {
  mat <- arr[qidx, idx, , drop = FALSE]
  mat <- matrix(mat, nrow = length(idx), ncol = dim(arr)[3])
  qt_res <- t(fast_row_quantiles_t(mat, probs = percentiles))
  colnames(qt_res) <- c("Lower", "Median", "Upper")
  data.frame(
    Date = dates,
    Quantile = quantile_name,
    Lower = qt_res[, "Lower"],
    Median = qt_res[, "Median"],
    Upper = qt_res[, "Upper"]
  )
}

# Map quantile names to their index in the first dimension
quantiles_map <- list(
  "5th"  = 1,
  "20th" = 2,
  "35th" = 3,
  "50th" = 4,
  "65th" = 5,
  "80th" = 6,
  "95th" = 7
)

# -- 2. All quantile trajectories --
quant_df_list <- lapply(names(quantiles_map), function(qname) {
  qidx <- quantiles_map[[qname]]
  get_quantile_trajectory(xbs_retro, qidx, dates, idx, qname)
})
quant_df <- bind_rows(quant_df_list)

# -- 3. Observed USGS series --
obs_df <- data.frame(
  Date = dates,
  Value = Y[1, idx]
)

flood_lines <- data.frame(
  y = flood_stages_trans,
  Stage = paste0(lev_flood, " ft")
)

alpha_val <- 0.11
# -- 5. Plot: Publication-Ready --
p <- ggplot() +
    annotate(
    "text",
    x =  max(as.Date(dates_ts_usgs[idx])),
    y = flood_stages_trans,
    label = flood_stage_labels,
    hjust = 10.5,
    vjust = -0.5,
    color = "black",
    fontface = "italic",
    size = 3.5
  ) +
  # Flood reference lines
  geom_hline(
    data = flood_lines,
    aes(yintercept = y), linetype = "dashed", color = "gray50", linewidth = 0.6
  ) +
  # 95th quantile (blue, ribbon)
  geom_ribbon(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, ymin = Lower, ymax = Upper),
    fill = "#2171b5", alpha = alpha_val
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, y = Median), color = "#2171b5", linewidth = 0.2
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, y = Lower), color = "blue", linewidth = 0.05
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, y = Upper), color = "blue", linewidth = 0.05
  ) +
  # 5th quantile (red, ribbon)
  geom_ribbon(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, ymin = Lower, ymax = Upper),
    fill = "#b2182b", alpha = alpha_val
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, y = Median), color = "#b2182b", linewidth = 0.2
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, y = Lower), color = "red", linewidth = 0.05
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, y = Upper), color = "red", linewidth = 0.05
  ) +
  # 50th quantile (green, ribbon)
  geom_ribbon(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, ymin = Lower, ymax = Upper),
    fill = "#238b45", alpha = alpha_val
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, y = Median), color = "#238b45", linewidth = 0.2
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, y = Lower), color = "green", linewidth = 0.05
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, y = Upper), color = "green", linewidth = 0.05
  ) +
  # Observed series (black, line and points)
  geom_point(
    data = obs_df, aes(x = Date, y = Value),
    color = "black", size = 0.2
  ) +
  geom_line(
    data = obs_df, aes(x = Date, y = Value),
    color = "black", linewidth = 0.1
  ) +
  labs(
    title = "Quantile Dynamics: 2012–2016",
    x = NULL,
    y = expression("Water Flow (Log-Log cm^3/s)")
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  coord_cartesian(ylim = c(-2, 3)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p)

# Save if desired
ggsave(
  filename = "SOURCE_WORKFLOW_REFERENCE",
  plot = p, width = 12, height = 6, units = "in", dpi = 900
)



# -- 1. Prepare Data --
# Flood stage values in feet
flood_stages_ft <- c(21.76, 16.5)^3
# Convert to centimeters
flood_stages_cm <- flood_stages_ft*CFSToCMS_CONVERSION_FACTOR 
# Apply log(log(x + 1)) transformation
flood_stages_trans <- log(log(flood_stages_cm + 1))

idx <- time_cuts[1]:time_cuts[2]
dates <- as.Date(dates_ts_usgs[idx])         # Dates for plotting window
percentiles <- c(0.025, 0.5, 0.975)

# Helper: Extract quantile trajectory for a given quantile
get_quantile_trajectory <- function(arr, qidx, dates, idx, quantile_name) {
  mat <- arr[qidx, idx, , drop = FALSE]
  mat <- matrix(mat, nrow = length(idx), ncol = dim(arr)[3])
  qt_res <- t(fast_row_quantiles_t(mat, probs = percentiles))
  colnames(qt_res) <- c("Lower", "Median", "Upper")
  data.frame(
    Date = dates,
    Quantile = quantile_name,
    Lower = qt_res[, "Lower"],
    Median = qt_res[, "Median"],
    Upper = qt_res[, "Upper"]
  )
}

# Map quantile names to their index in the first dimension
quantiles_map <- list(
  "5th"  = 1,
  "20th" = 2,
  "35th" = 3,
  "50th" = 4,
  "65th" = 5,
  "80th" = 6,
  "95th" = 7
)

# -- 2. All quantile trajectories --
quant_df_list <- lapply(names(quantiles_map), function(qname) {
  qidx <- quantiles_map[[qname]]
  get_quantile_trajectory(xbs_retro, qidx, dates, idx, qname)
})
quant_df <- bind_rows(quant_df_list)

# -- 3. Observed USGS series --
obs_df1 <- data.frame(
  Date = dates,
  Value = Y[1, idx]
)
obs_df2 <- data.frame(
  Date = dates,
  Value = Y[2, idx]
)
obs_df3 <- data.frame(
  Date = dates,
  Value = Y[3, idx]
)
flood_lines <- data.frame(
  y = flood_stages_trans,
  Stage = paste0(lev_flood, " ft")
)

alpha_val <- 0.11
# -- 5. Plot: Publication-Ready --
p <- ggplot() +
    annotate(
    "text",
    x =  max(as.Date(dates_ts_usgs[idx])),
    y = flood_stages_trans,
    label = flood_stage_labels,
    hjust = 10.5,
    vjust = -0.5,
    color = "black",
    fontface = "italic",
    size = 3.5
  ) +
  # Flood reference lines
  geom_hline(
    data = flood_lines,
    aes(yintercept = y), linetype = "dashed", color = "gray50", linewidth = 0.6
  ) +
  # 95th quantile (blue, ribbon)
  geom_ribbon(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, ymin = Lower, ymax = Upper),
    fill = "#2171b5", alpha = alpha_val
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, y = Median), color = "#2171b5", linewidth = 0.2
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, y = Lower), color = "blue", linewidth = 0.05
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "95th"),
    aes(x = Date, y = Upper), color = "blue", linewidth = 0.05
  ) +
  # 5th quantile (red, ribbon)
  geom_ribbon(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, ymin = Lower, ymax = Upper),
    fill = "#b2182b", alpha = alpha_val
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, y = Median), color = "#b2182b", linewidth = 0.2
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, y = Lower), color = "red", linewidth = 0.05
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "5th"),
    aes(x = Date, y = Upper), color = "red", linewidth = 0.05
  ) +
  # 50th quantile (green, ribbon)
  geom_ribbon(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, ymin = Lower, ymax = Upper),
    fill = "#238b45", alpha = alpha_val
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, y = Median), color = "#238b45", linewidth = 0.2
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, y = Lower), color = "green", linewidth = 0.05
  ) +
  geom_line(
    data = quant_df %>% filter(Quantile == "50th"),
    aes(x = Date, y = Upper), color = "green", linewidth = 0.05
  ) +
  # Observed series (black, line and points)
  geom_point(
    data = obs_df1, aes(x = Date, y = Value),
    color = "black", size = 0.1
  ) +
  geom_line(
    data = obs_df1, aes(x = Date, y = Value),
    color = "black", linewidth = 0.05
  ) +
  # Observed series (black, line and points)
  geom_point(
    data = obs_df2, aes(x = Date, y = Value),
    color = "purple", size = 0.1
  ) +
  geom_line(
    data = obs_df2, aes(x = Date, y = Value),
    color = "purple", linewidth = 0.05
  ) +
  # Observed series (black, line and points)
  geom_point(
    data = obs_df3, aes(x = Date, y = Value), 
    color = "orange", size = 0.1
  ) +
  geom_line(
    data = obs_df3, aes(x = Date, y = Value),
    color = "orange", linewidth = 0.05
  ) +
  labs(
    title = "Quantile Dynamics: 2012–2016",
    x = NULL,
    y = expression("Water Flow (Log-Log cm^3/s)")
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  coord_cartesian(ylim = c(-3, 2)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p)

# # Save if desired
# ggsave(
#   filename = "SOURCE_WORKFLOW_REFERENCE",
#   plot = p, width = 12, height = 6, units = "in", dpi = 900
# )



# -- Helper: Build tidy data for a single component --
build_quantile_df <- function(q_array, component, idx, date_vec, quantile_label) {
  # q_array: [component, time, quant] where quant 1=lower, 2=median, 3=upper
  tibble(
    Date    = as.Date(date_vec[idx]),
    Lower   = q_array[component, idx, 1],
    Median  = q_array[component, idx, 2],
    Upper   = q_array[component, idx, 3],
    Quantile = quantile_label
  )
}

# -- All quantile ribbons in one tidy dataframe --
make_component_df <- function(component, idx, date_vec,
                             q_d_50, q_d_05, q_d_95) {
  bind_rows(
    build_quantile_df(q_d_50, component, idx, date_vec, "50th"),
    build_quantile_df(q_d_05, component, idx, date_vec, "5th"),
    build_quantile_df(q_d_95, component, idx, date_vec, "95th")
  )
}

# Example: For component 1
component <- 1
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)

# Observed series for this component (customize as needed per component)
obs_vec <- if (component == 1) Y[1, idx] else Y[2, idx] - Y[1, idx] # Example
obs_df <- tibble(Date = as.Date(dates_ts_usgs[idx]), Value = obs_vec)


plot_component_quantiles <- function(
    comp_df, obs_df,
    ylab = "log-flow",
    title = "Component",
    ylim = c(-2.5, 2.5),
    filename = NULL,
    time_cuts = NULL,          # pass the time_cuts vector!
    dates_ts_usgs = NULL       # pass the dates vector!
) {
  # --- 1. Shade periods setup ---
  if (is.null(time_cuts) | is.null(dates_ts_usgs)) stop("Provide time_cuts and dates_ts_usgs!")

  shade_periods <- tibble(
    xmin = as.Date(dates_ts_usgs[time_cuts[c(1, 3)]]),
    xmax = as.Date(dates_ts_usgs[time_cuts[c(2, 4)]]),
    period = c("Dry", "Rainy"),
    fill = c("#ffeead", "#c9e4f6")  # pastel yellow, pastel blue
  )

  # --- 2. Colors (as before) ---
  col_50  <- "#238b45"; band_50 <- "#b2df8a"
  col_05  <- "#b2182b"; band_05 <- "#fdbba1"
  col_95  <- "#2171b5"; band_95 <- "#a6bddb"
  obs_line <- "#222222"; obs_point <- "#222222"
  ribbon_alpha <- 0.11; lnn <- 0.4

  # --- 3. Compose plot ---
  p <- ggplot() +
    # --- Shaded regions ---
    geom_rect(
      data = shade_periods,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
      alpha = 0.6, inherit.aes = FALSE, show.legend = FALSE
    ) +
    scale_fill_manual(values = setNames(shade_periods$fill, shade_periods$period)) +
    # --- Bands: Light, desaturated color ---
    geom_ribbon(data = comp_df %>% filter(Quantile == "50th"),
      aes(x = Date, ymin = Lower, ymax = Upper),
      fill = band_50, alpha = ribbon_alpha) +
    geom_ribbon(data = comp_df %>% filter(Quantile == "5th"),
      aes(x = Date, ymin = Lower, ymax = Upper),
      fill = band_05, alpha = ribbon_alpha) +
    geom_ribbon(data = comp_df %>% filter(Quantile == "95th"),
      aes(x = Date, ymin = Lower, ymax = Upper),
      fill = band_95, alpha = ribbon_alpha) +
    # --- Median/Quantile lines ---
    geom_line(data = comp_df %>% filter(Quantile == "50th"),
      aes(x = Date, y = Median), color = col_50, linewidth = lnn) +
    geom_line(data = comp_df %>% filter(Quantile == "5th"),
      aes(x = Date, y = Median), color = col_05, linewidth = lnn) +
    geom_line(data = comp_df %>% filter(Quantile == "95th"),
      aes(x = Date, y = Median), color = col_95, linewidth = lnn) +
    # --- Dashed, thin quantile CI boundaries ---
    geom_line(data = comp_df %>% filter(Quantile == "50th"),
      aes(x = Date, y = Lower), color = "green", linewidth = 0.1) +
    geom_line(data = comp_df %>% filter(Quantile == "50th"),
      aes(x = Date, y = Upper), color = "green", linewidth = 0.1) +
    geom_line(data = comp_df %>% filter(Quantile == "5th"),
      aes(x = Date, y = Lower), color = "red", linewidth = 0.1) +
    geom_line(data = comp_df %>% filter(Quantile == "5th"),
      aes(x = Date, y = Upper), color = "red", linewidth = 0.1) +
    geom_line(data = comp_df %>% filter(Quantile == "95th"),
      aes(x = Date, y = Lower), color = "blue", linewidth = 0.1) +
    geom_line(data = comp_df %>% filter(Quantile == "95th"),
      aes(x = Date, y = Upper), color = "blue", linewidth = 0.1) +
    # --- Observed: Bold points + strong line for visual anchoring ---
    geom_line(data = obs_df, aes(x = Date, y = Value), color = obs_line, linewidth = 0.1) +
    geom_point(data = obs_df, aes(x = Date, y = Value), color = obs_point, size = 0.1, alpha = 0.95) +
    # --- Period text annotations ---
    annotate("text",
      x = shade_periods$xmin + (shade_periods$xmax - shade_periods$xmin) / 2,
      y = ylim[1] + 0.01 * diff(ylim),
      label = shade_periods$period,
      size = 3.4, color = "#565656", fontface = "italic"
    ) +
    # --- Axes and theme ---
    labs(title = title, x = NULL, y = ylab) +
    coord_cartesian(ylim = ylim, expand = TRUE) +
    scale_x_date(date_breaks = "24 months", date_labels = "%Y-%m") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 8)),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 11),
      axis.text.y = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.3, color = "#e5e5e5"),
      panel.grid.major.y = element_line(linewidth = 0.4, color = "#e5e5e5"),
      plot.margin = margin(12, 12, 12, 12)
    )

  print(p)
  if (!is.null(filename)) {
    ff <- paste0("SOURCE_WORKFLOW_REFERENCE", filename)
    ggsave(ff, plot = p, width = 12, height = 6, units = "in", dpi = 350)
  }
}


idx <- ceiling(TT/10):TT
obs_vec <- Y[1, idx]  
obs_df <- tibble(Date = as.Date(dates_ts_usgs[idx]), Value = obs_vec)

# 

# Example usage for any component
component <- 1
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "Trend Component – 1991–2022",
  ylim = c(-2, 2),
  filename = "trend_component_1991_2022.png",
  time_cuts = time_cuts,        # <-- NEW: pass this in!
  dates_ts_usgs = dates_ts_usgs # <-- NEW: pass this in!
)

# Example usage for any component
component <- 2
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "Yearly Effect – 1991–2022",
  ylim = c(-2, 2),
  filename = "yearly_component_1991_2022.png",
  time_cuts = time_cuts,        # <-- NEW: pass this in!
  dates_ts_usgs = dates_ts_usgs # <-- NEW: pass this in!
)

# Example usage for any component
component <- 4
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "Semestral Effect – 1991–2022",
  ylim = c(-2, 2),
  filename = "sem_component_1991_2022.png",
  time_cuts = time_cuts,        # <-- NEW: pass this in!
  dates_ts_usgs = dates_ts_usgs # <-- NEW: pass this in!
)

# Example usage for any component
component <- 6
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "80-month Effect – 1991–2022",
  ylim = c(-2, 2),
  filename = "80_component_1991_2022.png",
  time_cuts = time_cuts,        # <-- NEW: pass this in!
  dates_ts_usgs = dates_ts_usgs # <-- NEW: pass this in!
)


plot_component_quantiles <- function(
    comp_df, obs_df,
    ylab = "log-flow",
    title = "Component",
    ylim = c(-2.5, 2.5),
    filename = NULL
) {
  # Define custom colors (muted and colorblind-friendly)
  col_50  <- "#238b45"
  band_50 <- "#b2df8a"
  col_05  <- "#b2182b"
  band_05 <- "#fdbba1"
  col_95  <- "#2171b5"
  band_95 <- "#a6bddb"
  obs_line <- "#222222"
  obs_point <- "#222222"
  
  ribbon_alpha <- 0.11
  lnn <- 0.4
  p <- ggplot() +
    # --- Bands: Light, desaturated color ---
    geom_ribbon(
      data = comp_df %>% filter(Quantile == "50th"),
      aes(x = Date, ymin = Lower, ymax = Upper),
      fill = band_50, alpha = ribbon_alpha
    ) +
    geom_ribbon(
      data = comp_df %>% filter(Quantile == "5th"),
      aes(x = Date, ymin = Lower, ymax = Upper),
      fill = band_05, alpha = ribbon_alpha
    ) +
    geom_ribbon(
      data = comp_df %>% filter(Quantile == "95th"),
      aes(x = Date, ymin = Lower, ymax = Upper),
      fill = band_95, alpha = ribbon_alpha
    ) +

    # --- Median/Quantile lines: Strong, moderately thin ---
    geom_line(
      data = comp_df %>% filter(Quantile == "50th"),
      aes(x = Date, y = Median), color = col_50, linewidth = lnn
    ) +
    geom_line(
      data = comp_df %>% filter(Quantile == "5th"),
      aes(x = Date, y = Median), color = col_05, linewidth = lnn
    ) +
    geom_line(
      data = comp_df %>% filter(Quantile == "95th"),
      aes(x = Date, y = Median), color = col_95, linewidth = lnn
    ) +
    # --- Dashed, thin ---
    geom_line(
      data = comp_df %>% filter(Quantile == "50th"),
      aes(x = Date, y = Lower), color = "green", linewidth = 0.1
    ) +
    geom_line(
      data = comp_df %>% filter(Quantile == "50th"),
      aes(x = Date, y = Upper), color = "green", linewidth = 0.1
    ) +
    geom_line(
      data = comp_df %>% filter(Quantile == "5th"),
      aes(x = Date, y = Lower), color = "red", linewidth = 0.1
    ) +
    geom_line(
      data = comp_df %>% filter(Quantile == "5th"),
      aes(x = Date, y = Upper), color = "red", linewidth = 0.1
    ) +
    geom_line(
      data = comp_df %>% filter(Quantile == "95th"),
      aes(x = Date, y = Lower), color = "blue", linewidth = 0.1
    ) +
    geom_line(
      data = comp_df %>% filter(Quantile == "95th"),
      aes(x = Date, y = Upper), color = "blue", linewidth = 0.1
    ) +
    # --- Observed: Bold points + strong line for visual anchoring ---
    geom_line(
      data = obs_df, aes(x = Date, y = Value),
      color = obs_line, linewidth = 0.1
    ) +
    geom_point(
      data = obs_df, aes(x = Date, y = Value),
      color = obs_point, size = 0.1, alpha = 0.95
    ) +

    # --- Axes and theme ---
    labs(title = title, x = NULL, y = ylab) +
    coord_cartesian(ylim = ylim, expand = TRUE) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 8)),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 11),
      axis.text.y = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.3, color = "#e5e5e5"),
      panel.grid.major.y = element_line(linewidth = 0.4, color = "#e5e5e5"),
      plot.margin = margin(12, 12, 12, 12)
    )
  
  print(p)
  if (!is.null(filename)) {
    ff <- paste0("SOURCE_WORKFLOW_REFERENCE",filename)
    ggsave(ff, plot = p, width = 12, height = 6, units = "in", dpi = 350)
  }
}

idx <- time_cuts[1]:time_cuts[2]
obs_vec <- Y[1, idx]  
obs_df <- tibble(Date = as.Date(dates_ts_usgs[idx]), Value = obs_vec)

component <- 1
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "Trend Component – 2012–2016",
  ylim = c(-2, 2),
  filename = "trend_component_2012_2016.png"
)

component <- 2
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "Yearly Effect – 2012–2016",
  ylim = c(-2, 2),
  filename = "yearly_component_2012_2016.png"
)

component <- 4
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "Semestral Effect – 2012–2016",
  ylim = c(-2, 2),
  filename = "sem_component_2012_2016.png"
)

component <- 6
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "80-month Effect – 2012–2016",
  ylim = c(-2, 2),
  filename = "80_component_2012_2016.png"
)

idx <- time_cuts[3]:time_cuts[4]
obs_vec <- Y[1, idx]  
obs_df <- tibble(Date = as.Date(dates_ts_usgs[idx]), Value = obs_vec)

component <- 1
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "Trend Component – 2017–2019",
  ylim = c(-2, 2),
  filename = "trend_component_2017_2019.png"
)

component <- 2
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "Yearly Effect – 2017–2019",
  ylim = c(-2, 2),
  filename = "yearly_component_2017_2019.png"
)

component <- 4
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "Semestral Effect – 2017–2019",
  ylim = c(-2, 2),
  filename = "sem_component_2017_2019.png"
)

component <- 6
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "80-month Effect – 2017–2019",
  ylim = c(-2, 2),
  filename = "80_component_2017_2019.png"
)

idx <- ceiling(TT/10):TT
# Helper to tidy the quantile array
make_quantile_df <- function(q_array, idx, dates, quantiles = c("5th", "50th", "95th")) {
  build_agg_discrep_quantile_df(
    q_array = q_array,
    idx = idx,
    dates = dates,
    quantile_rows = c(1L, 4L, 7L),
    quantile_labels = quantiles,
    context = "agg_disc_1991_2022"
  )
}

# Dates for x axis
dates <- as.Date(dates_ts_usgs[idx])

# Tidy quantile data
quantiles_labels <- c("5th", "50th", "95th")
df1 <- make_quantile_df(q_d_discrep1_quantiles, idx, dates, quantiles_labels)
df2 <- make_quantile_df(q_d_discrep2_quantiles, idx, dates, quantiles_labels)

# Observed discrepancy
obs1 <- data.frame(Date = dates, Discrepancy = Y[2, idx] - Y[1, idx])
obs2 <- data.frame(Date = dates, Discrepancy = Y[3, idx] - Y[1, idx])

make_discrepancy_plot <- function(df, obs, title, ylab = "log-flow", ylim = c(-2, 1), contract_key = "agg_disc_1991_2022") {
  # Reduce number of date ticks
  n_ticks <- 16
  date_breaks <- scales::pretty_breaks(n = n_ticks)(range(obs$Date))

  # Colors
  colors <- c(
    "5th"  = "#b2182b",    # Dark red
    "50th" = "#238b45",    # Forest green
    "95th" = "#2171b5"     # Dark blue
  )
  ribbon_alpha <- 0.11
  ylim_use <- resolve_agg_discrep_contract(
    df = df,
    obs = obs,
    preferred_ylim = ylim,
    contract_key = contract_key,
    title = title
  )

  ggplot() +
      # Observed discrepancy
    geom_line(data = obs, aes(x = Date, y = Discrepancy), color = "gray40", linewidth = 0.2) +
    geom_point(data = obs, aes(x = Date, y = Discrepancy), color = "black", size = 0.2) +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted", linewidth = 0.2) +
    # Quantile ribbons (95th/5th)
    geom_ribbon(data = df %>% filter(Quantile == "5th"),
                aes(x = Date, ymin = Lower, ymax = Upper),
                fill = colors["5th"], alpha = ribbon_alpha) +
    geom_ribbon(data = df %>% filter(Quantile == "50th"),
                aes(x = Date, ymin = Lower, ymax = Upper),
                fill = colors["50th"], alpha = ribbon_alpha) +
    geom_ribbon(data = df %>% filter(Quantile == "95th"),
                aes(x = Date, ymin = Lower, ymax = Upper),
                fill = colors["95th"], alpha = ribbon_alpha) +
    # Median lines
    geom_line(data = df %>% filter(Quantile == "5th"),
              aes(x = Date, y = Median), color = colors["5th"], linewidth = 0.5) +
    geom_line(data = df %>% filter(Quantile == "50th"),
              aes(x = Date, y = Median), color = colors["50th"], linewidth = 0.5) +
    geom_line(data = df %>% filter(Quantile == "95th"),
              aes(x = Date, y = Median), color = colors["95th"], linewidth = 0.5) +
    scale_x_date(
      breaks = date_breaks,
      date_labels = "%Y-%m"
    ) +
    coord_cartesian(ylim = ylim_use) +
    labs(
      title = title,
      x = NULL,
      y = ylab
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 11),
      axis.text.y = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}

# GloFAS-USGS
p1 <- make_discrepancy_plot(
  df1 %>% filter(Quantile %in% c("5th", "50th", "95th")),
  obs1,
  title = "Discrepancy GloFAS–USGS   1991–2022",
  contract_key = "agg_disc_1991_2022_1"
)
p1
ggsave("SOURCE_WORKFLOW_REFERENCE", p1, width = 12, height = 6, units = "in", dpi = 900)

# NWS-USGS (if J==2)
p2 <- make_discrepancy_plot(
  df2 %>% filter(Quantile %in% c("5th", "50th", "95th")),
  obs2,
  title = "Discrepancy NWS–USGS   1991–2022",
  contract_key = "agg_disc_1991_2022_2"
)
p2
ggsave("SOURCE_WORKFLOW_REFERENCE", p2, width = 12, height = 6, units = "in", dpi = 900)


idx <- time_cuts[1]:time_cuts[2]
# Helper to tidy the quantile array
make_quantile_df <- function(q_array, idx, dates, quantiles = c("5th", "50th", "95th")) {
  build_agg_discrep_quantile_df(
    q_array = q_array,
    idx = idx,
    dates = dates,
    quantile_rows = c(1L, 4L, 7L),
    quantile_labels = quantiles,
    context = "agg_disc_2012_2016"
  )
}

# Dates for x axis
dates <- as.Date(dates_ts_usgs[idx])

# Tidy quantile data
quantiles_labels <- c("5th", "50th", "95th")
df1 <- make_quantile_df(q_d_discrep1_quantiles, idx, dates, quantiles_labels)
df2 <- make_quantile_df(q_d_discrep2_quantiles, idx, dates, quantiles_labels)

# Observed discrepancy
obs1 <- data.frame(Date = dates, Discrepancy = Y[2, idx] - Y[1, idx])
obs2 <- data.frame(Date = dates, Discrepancy = Y[3, idx] - Y[1, idx])

make_discrepancy_plot <- function(df, obs, title, ylab = "log-flow", ylim = c(-1, 1), contract_key = "agg_disc_2012_2016") {
  # Reduce number of date ticks
  n_ticks <- 8
  date_breaks <- scales::pretty_breaks(n = n_ticks)(range(obs$Date))

  # Colors
  colors <- c(
    "5th"  = "#b2182b",    # Dark red
    "50th" = "#238b45",    # Forest green
    "95th" = "#2171b5"     # Dark blue
  )
  ribbon_alpha <- 0.11
  ylim_use <- resolve_agg_discrep_contract(
    df = df,
    obs = obs,
    preferred_ylim = ylim,
    contract_key = contract_key,
    title = title
  )

  ggplot() +
      # Observed discrepancy
    geom_line(data = obs, aes(x = Date, y = Discrepancy), color = "gray40", linewidth = 0.2) +
    geom_point(data = obs, aes(x = Date, y = Discrepancy), color = "black", size = 0.2) +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted", linewidth = 0.2) +
    # Quantile ribbons (95th/5th)
    geom_ribbon(data = df %>% filter(Quantile == "5th"),
                aes(x = Date, ymin = Lower, ymax = Upper),
                fill = colors["5th"], alpha = ribbon_alpha) +
    geom_ribbon(data = df %>% filter(Quantile == "50th"),
                aes(x = Date, ymin = Lower, ymax = Upper),
                fill = colors["50th"], alpha = ribbon_alpha) +
    geom_ribbon(data = df %>% filter(Quantile == "95th"),
                aes(x = Date, ymin = Lower, ymax = Upper),
                fill = colors["95th"], alpha = ribbon_alpha) +
    # Median lines
    geom_line(data = df %>% filter(Quantile == "5th"),
              aes(x = Date, y = Median), color = colors["5th"], linewidth = 0.1) +
    geom_line(data = df %>% filter(Quantile == "5th"),
              aes(x = Date, y = Lower), color = "red", linewidth = 0.051) +
    geom_line(data = df %>% filter(Quantile == "5th"),
              aes(x = Date, y = Upper), color = "red", linewidth = 0.051) +

    geom_line(data = df %>% filter(Quantile == "50th"),
              aes(x = Date, y = Median), color = colors["50th"], linewidth = 0.1) +
    geom_line(data = df %>% filter(Quantile == "50th"),
              aes(x = Date, y = Lower), color = "green", linewidth = 0.051) +
    geom_line(data = df %>% filter(Quantile == "50th"),
              aes(x = Date, y = Upper), color = "green", linewidth = 0.051) +

    geom_line(data = df %>% filter(Quantile == "95th"),
              aes(x = Date, y = Median), color = colors["95th"], linewidth = 0.1) +
    geom_line(data = df %>% filter(Quantile == "95th"),
              aes(x = Date, y = Lower), color = "blue", linewidth = 0.051) +
    geom_line(data = df %>% filter(Quantile == "95th"),
              aes(x = Date, y = Upper), color = "blue", linewidth = 0.051) +

    scale_x_date(
      breaks = date_breaks,
      date_labels = "%Y-%m"
    ) +
    coord_cartesian(ylim = ylim_use) +
    labs(
      title = title,
      x = NULL,
      y = ylab
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 11),
      axis.text.y = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}

# GloFAS-USGS
p1 <- make_discrepancy_plot(
  df1 %>% filter(Quantile %in% c("5th", "50th", "95th")),
  obs1,
  title = "Discrepancy GloFAS–USGS   1991–2022",
  ylim = c(-1.5, 1),
  contract_key = "agg_disc_2012_2016_1"
)
p1
ggsave("SOURCE_WORKFLOW_REFERENCE", p1, width = 12, height = 6, units = "in", dpi = 900)

# NWS-USGS (if J==2)
p2 <- make_discrepancy_plot(
  df2 %>% filter(Quantile %in% c("5th", "50th", "95th")),
  obs2,
  title = "Discrepancy NWS–USGS   1991–2022",
  ylim = c(-2.5, 1),
  contract_key = "agg_disc_2012_2016_2"
)
p2
ggsave("SOURCE_WORKFLOW_REFERENCE", p2, width = 12, height = 6, units = "in", dpi = 900)


idx <- time_cuts[3]:time_cuts[4]
# Helper to tidy the quantile array
make_quantile_df <- function(q_array, idx, dates, quantiles = c("5th", "50th", "95th")) {
  build_agg_discrep_quantile_df(
    q_array = q_array,
    idx = idx,
    dates = dates,
    quantile_rows = c(1L, 4L, 7L),
    quantile_labels = quantiles,
    context = "agg_disc_2017_2019"
  )
}

# Dates for x axis
dates <- as.Date(dates_ts_usgs[idx])

# Tidy quantile data
quantiles_labels <- c("5th", "50th", "95th")
df1 <- make_quantile_df(q_d_discrep1_quantiles, idx, dates, quantiles_labels)
df2 <- make_quantile_df(q_d_discrep2_quantiles, idx, dates, quantiles_labels)

# Observed discrepancy
obs1 <- data.frame(Date = dates, Discrepancy = Y[2, idx] - Y[1, idx])
obs2 <- data.frame(Date = dates, Discrepancy = Y[3, idx] - Y[1, idx])

make_discrepancy_plot <- function(df, obs, title, ylab = "log-flow", ylim = c(-1, 1), contract_key = "agg_disc_2017_2019") {
  # Reduce number of date ticks
  n_ticks <- 8
  date_breaks <- scales::pretty_breaks(n = n_ticks)(range(obs$Date))

  # Colors
  colors <- c(
    "5th"  = "#b2182b",    # Dark red
    "50th" = "#238b45",    # Forest green
    "95th" = "#2171b5"     # Dark blue
  )
  ribbon_alpha <- 0.11
  ylim_use <- resolve_agg_discrep_contract(
    df = df,
    obs = obs,
    preferred_ylim = ylim,
    contract_key = contract_key,
    title = title
  )

  ggplot() +
      # Observed discrepancy
    geom_line(data = obs, aes(x = Date, y = Discrepancy), color = "gray40", linewidth = 0.2) +
    geom_point(data = obs, aes(x = Date, y = Discrepancy), color = "black", size = 0.2) +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted", linewidth = 0.2) +
    # Quantile ribbons (95th/5th)
    geom_ribbon(data = df %>% filter(Quantile == "5th"),
                aes(x = Date, ymin = Lower, ymax = Upper),
                fill = colors["5th"], alpha = ribbon_alpha) +
    geom_ribbon(data = df %>% filter(Quantile == "50th"),
                aes(x = Date, ymin = Lower, ymax = Upper),
                fill = colors["50th"], alpha = ribbon_alpha) +
    geom_ribbon(data = df %>% filter(Quantile == "95th"),
                aes(x = Date, ymin = Lower, ymax = Upper),
                fill = colors["95th"], alpha = ribbon_alpha) +
    # Median lines
    geom_line(data = df %>% filter(Quantile == "5th"),
              aes(x = Date, y = Median), color = colors["5th"], linewidth = 0.1) +
    geom_line(data = df %>% filter(Quantile == "5th"),
              aes(x = Date, y = Lower), color = "red", linewidth = 0.051) +
    geom_line(data = df %>% filter(Quantile == "5th"),
              aes(x = Date, y = Upper), color = "red", linewidth = 0.051) +

    geom_line(data = df %>% filter(Quantile == "50th"),
              aes(x = Date, y = Median), color = colors["50th"], linewidth = 0.1) +
    geom_line(data = df %>% filter(Quantile == "50th"),
              aes(x = Date, y = Lower), color = "green", linewidth = 0.051) +
    geom_line(data = df %>% filter(Quantile == "50th"),
              aes(x = Date, y = Upper), color = "green", linewidth = 0.051) +

    geom_line(data = df %>% filter(Quantile == "95th"),
              aes(x = Date, y = Median), color = colors["95th"], linewidth = 0.1) +
    geom_line(data = df %>% filter(Quantile == "95th"),
              aes(x = Date, y = Lower), color = "blue", linewidth = 0.051) +
    geom_line(data = df %>% filter(Quantile == "95th"),
              aes(x = Date, y = Upper), color = "blue", linewidth = 0.051) +

    scale_x_date(
      breaks = date_breaks,
      date_labels = "%Y-%m"
    ) +
    coord_cartesian(ylim = ylim_use) +
    labs(
      title = title,
      x = NULL,
      y = ylab
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 11),
      axis.text.y = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}

# GloFAS-USGS
p1 <- make_discrepancy_plot(
  df1 %>% filter(Quantile %in% c("5th", "50th", "95th")),
  obs1,
  title = "Discrepancy GloFAS–USGS   1991–2022",
  ylim = c(-1.6, 0.8),
  contract_key = "agg_disc_2017_2019_1"
)
p1
ggsave("SOURCE_WORKFLOW_REFERENCE", p1, width = 12, height = 6, units = "in", dpi = 900)

# NWS-USGS (if J==2)
p2 <- make_discrepancy_plot(
  df2 %>% filter(Quantile %in% c("5th", "50th", "95th")),
  obs2,
  title = "Discrepancy NWS–USGS   1991–2022",
  ylim = c(-1.6, 0.8),
  contract_key = "agg_disc_2017_2019_2"
)
p2
ggsave("SOURCE_WORKFLOW_REFERENCE", p2, width = 12, height = 6, units = "in", dpi = 900)

# Keep legacy filename aliases in sync with the same fitted-overlay payload.
ggsave("SOURCE_WORKFLOW_REFERENCE", p1, width = 12, height = 6, units = "in", dpi = 900)
ggsave("SOURCE_WORKFLOW_REFERENCE", p2, width = 12, height = 6, units = "in", dpi = 900)

if (exists("OUT_DIR", inherits = TRUE) && length(agg_disc_contract_rows) > 0L) {
  agg_contract_df <- dplyr::bind_rows(agg_disc_contract_rows)
  write.csv(
    agg_contract_df,
    file.path(get("OUT_DIR", inherits = TRUE), "agg_disc_plot_contract.csv"),
    row.names = FALSE
  )
}


# 

idx <- 1:TT
idx <- time_cuts[3]:time_cuts[4]
obs_vec <- Y[1, idx] * 0 
obs_df <- tibble(Date = as.Date(dates_ts_usgs[idx]), Value = obs_vec)

# component <- 23
# comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
# plot_component_quantiles(
#   comp_df, obs_df,
#   ylab = expression("Water Flow (Log-Log cm^3/s)"),
#   title = "Precipitation – 2012–2016",
#   ylim = c(0, 0.5),
#   filename = "trend_component_2012_2016.png"
# )

# component <- 24
# comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
# plot_component_quantiles(
#   comp_df, obs_df,
#   ylab = expression("Water Flow (Log-Log cm^3/s)"),
#   title = "Soil Moisture – 2012–2016",
#   ylim = c(0.5, 0),
#   filename = "trend_component_2012_2016.png"
# )

# component <- 25
# comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
# plot_component_quantiles(
#   comp_df, obs_df,
#   ylab = expression("Water Flow (Log-Log cm^3/s)"),
#   title = "PCA – 2012–2016",
#   ylim = c(-0.005, 0),
#   filename = "trend_component_2012_2016.png"
# )

component <- 22
comp_df <- make_component_df(component, idx, dates_ts_usgs, q_d_50, q_d_05, q_d_95)
plot_component_quantiles(
  comp_df, obs_df,
  ylab = expression("Water Flow (Log-Log cm^3/s)"),
  title = "Cumm Effect – 2012–2016",
  ylim = c(-2.5, 3),
  filename = "trend_component_2012_2016.png"
)

names(new.theta.out_50_exAL_synth_DISC_uni)

# Y

# 

# for(i in 23:31){
# s5 <- samp.theta_5_exAL_synth_DISC$samp_theta[i,TT,]
# s20 <- samp.theta_20_exAL_synth_DISC$samp_theta[i,TT,]
# s35 <- samp.theta_35_exAL_synth_DISC$samp_theta[i,TT,]
# s50 <- samp.theta_50_exAL_synth_DISC$samp_theta[i,TT,]
# s65 <- samp.theta_65_exAL_synth_DISC$samp_theta[i,TT,]
# s80 <- samp.theta_80_exAL_synth_DISC$samp_theta[i,TT,]
# s95 <- samp.theta_95_exAL_synth_DISC$samp_theta[i,TT,]

# print(c(quantile(s5,0.025),mean(s5),quantile(s5,0.975)))
# print(c(quantile(s20,0.025),mean(s20),quantile(s20,0.975)))
# print(c(quantile(s35,0.025),mean(s35),quantile(s35,0.975)))
# print(c(quantile(s50,0.025),mean(s50),quantile(s50,0.975)))
# print(c(quantile(s65,0.025),mean(s65),quantile(s65,0.975)))
# print(c(quantile(s80,0.025),mean(s80),quantile(s80,0.975)))
# print(c(quantile(s95,0.025),mean(s95),quantile(s95,0.975)))


# }

# Indices and quantile labels
indices <- 23:31
sd_vec <- c(sd_ppt,sd_soil,sd_pca,1,sd1,sd2,sd3,sd4,sd5)

quantiles <- c(5, 50, 95)

# Create an empty list to store all results
all_results <- list()

ii <- 1
for(i in indices) {
  sd_fac <- sd_vec[ii]
  for(q in quantiles) {
    # Dynamically access the sample vector for this quantile/component
    samples <- (sd_fac)*get(paste0("samp.theta_", q, "_exAL_synth_DISC"))$samp_theta[i, TT, ]
    result <- data.frame(
      Component = i,
      Quantile = paste0(q, "th"),
      Lower = quantile(samples, 0.025),
      Mean = mean(samples),
      Upper = quantile(samples, 0.975)
    )
    all_results[[length(all_results) + 1]] <- result
  }
}

# Combine all into one tidy data frame
summary_df <- do.call(rbind, all_results)
print(summary_df)

if (posterior_table_exports_enabled) {
  profile_section("figures.export_covariate_effects_table", {
    cov_export <- post_export_covariate_effects_table(
      summary_df = summary_df,
      output_dir = posterior_table_output_dir,
      time_index = TT,
      ci_digits = 5L,
      write_tex = TRUE,
      table_formats = posterior_table_formats,
      keep_na = posterior_table_keep_na
    )
    posterior_table_export_manifest <<- rbind(posterior_table_export_manifest, cov_export$manifest)
    post_write_table_exports_manifest(
      manifest_df = posterior_table_export_manifest,
      output_dir = posterior_table_output_dir
    )
    post_write_table_exports_readme(
      output_dir = posterior_table_output_dir,
      ci_digits = 5L,
      table_formats = posterior_table_formats
    )
  })
}



synthesize_quantiles <- function(y_reps, percentiles, M = 10000) {
  # Dimensions
  n_p0 <- dim(y_reps)[1]
  n_samp <- dim(y_reps)[2]
  n_T <- dim(y_reps)[3]
  
  # Precompute grids (optimized)
  u_grid_dense <- (1:M)/(M+1)            # Dense grid for Q_init
  u_final <- (1:n_samp)/(n_samp+1)       # Probability levels for final sample
  pp <- (1:n_samp)/(n_samp+1)            # Grid for model quantiles
  
  # Output array [n_samp, n_T]
  output <- array(NA, dim = c(n_samp, n_T))
  
  for (t in 1:n_T) {
    # Step 1: Compute empirical τ-quantiles
    v <- vapply(1:n_p0, function(k) {
      quantile(y_reps[k, , t], probs = percentiles[k], type = 7, names = FALSE)
    }, numeric(1))
    
    # Step 2: Isotonic adjustment
    fit <- gpava(percentiles, v, ties = "primary")
    m_adj <- fit$x
    
    # Step 3: Distributional alignment
    adjusted_samples <- matrix(NA, nrow = n_p0, ncol = n_samp)
    for (k in 1:n_p0) {
      shift <- m_adj[k] - v[k]
      adj_vec <- y_reps[k, , t] + shift
      adjusted_samples[k, ] <- sort_to_len(adj_vec, target_len = n_samp, context = sprintf("adjusted_samples[%d,]", k))
    }
    
    # Step 4: Quantile function construction on a dense grid (vectorized).
    # Preserve exact behavior by evaluating approx() at the same u_grid_dense points,
    # but avoid calling approx() 10,000 times per t (one per u).
    q_dense <- vapply(
      1:n_p0,
      function(k) approx(pp, adjusted_samples[k, ], xout = u_grid_dense, rule = 2)$y,
      numeric(M)
    )
    # q_dense is [M x n_p0]; transpose to [n_p0 x M] for row-wise indexing
    q_dense <- t(q_dense)

    # Step 5: Initial synthesis (linear blend between adjacent quantile functions)
    q_init <- numeric(M)

    # Match boundary conditions from the original scalar loop
    mask_low <- u_grid_dense <= percentiles[1]
    mask_high <- u_grid_dense >= percentiles[n_p0]
    mask_mid <- !(mask_low | mask_high)

    if (any(mask_low)) {
      q_init[mask_low] <- q_dense[1, mask_low]
    }
    if (any(mask_high)) {
      q_init[mask_high] <- q_dense[n_p0, mask_high]
    }

    if (any(mask_mid)) {
      pos <- which(mask_mid)
      u_mid <- u_grid_dense[pos]
      i <- findInterval(u_mid, percentiles)
      w <- (u_mid - percentiles[i]) / (percentiles[i + 1] - percentiles[i])
      q_i <- q_dense[cbind(i, pos)]
      q_i1 <- q_dense[cbind(i + 1, pos)]
      q_init[pos] <- (1 - w) * q_i + w * q_i1
    }
    
    # Step 6: Monotone rearrangement
    q_sorted <- sort_to_len(q_init, target_len = M, context = "q_sorted")  # Enforces global monotonicity
    
    # Step 7: Generate synthesized sample
    output[, t] <- approx(
      x = u_grid_dense, 
      y = q_sorted, 
      xout = u_final, 
      rule = 2
    )$y
  }
  
  return(output)
}

# Usage:
output_f <- synthesize_quantiles(y_reps_f, percentiles = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95))

q_estim_output_f <- fast_col_quantiles_t(output_f, probs = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95))
q_estim_synth_f <- fast_col_quantiles_t(log(synth_f), probs = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95))

n_T <- dim(output_f)[2]
plot(rep(0,n_T), ylim = c(-1,3), type = 'line')
# for(s in 1:n.samp){
#     lines(output_f[s,], col = 'pink', lwd = 0.1)
# }

# for(s in 1:n.samp){
#     points(log(synth_f[s,]), col = 'lightblue', lwd = 0.1)
# }

for(i in 1:7){
    lines(q_estim_output_f[i,], col = 'black', lwd = 2, lty = 2)
    lines(q_estim_synth_f[i,], col = 'red', lwd = 2, lty = 2)
}

# Usage:
output <- synthesize_quantiles(y_reps, percentiles = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95))

q_estim_output <- fast_col_quantiles_t(output, probs = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95))
q_estim_synth <- fast_col_quantiles_t(log(synth), probs = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95))

n_T <- dim(output)[2]
plot(rep(0,n_T), ylim = c(-1,3), type = 'line')
# for(s in 1:n.samp){
#     lines(output[s,], col = 'pink', lwd = 0.1)
# }

# for(s in 1:n.samp){
#     points(log(synth[s,]), col = 'lightblue', lwd = 0.1)
# }

for(i in 1:7){
    lines(q_estim_output[i,], col = 'black', lwd = 2, lty = 2)
    lines(q_estim_synth[i,], col = 'red', lwd = 2, lty = 2)
}

# q_s

idx <- idx_sub

output_f_q <- colQuantiles(output_f, probs = q_s, type = 8)
output_f_q <- t(output_f_q)

output_q <- colQuantiles(output, probs = q_s, type = 8)
output_q <- t(output_q)


# 1. Dates for fit and forecast
fit_dates <- as.Date(timestamps[idx])
forecast_dates <- daily_dates_for_matrix_cols(
  output_f,
  start_date = fit_dates[length(fit_dates)] + 1,
  context = "posterior_dates.output_f"
)

# 2. Posterior samples, tidy for ggplot (long format; avoid pivot_longer)
df_post_fit <- fast_long_by_row(
  mat = output,
  row_values = seq_len(nrow(output)),
  col_values = fit_dates,
  row_name = "sample",
  col_name = "Date",
  value_name = "Value"
)
df_post_fit$Type <- "Fit"

df_post_forecast <- fast_long_by_row(
  mat = output_f,
  row_values = seq_len(nrow(output_f)),
  col_values = forecast_dates,
  row_name = "sample",
  col_name = "Date",
  value_name = "Value"
)
df_post_forecast$Type <- "Forecast"

df_post <- bind_rows(df_post_fit, df_post_forecast)

# 3. Quantile curves (avoid pivot_longer)
df_q_fit <- fast_long_by_row(
  mat = output_q,
  row_values = seq_len(nrow(output_q)),
  col_values = fit_dates,
  row_name = "quantile",
  col_name = "Date",
  value_name = "Value"
)
df_q_fit$Type <- "Fit"

df_q_forecast <- fast_long_by_row(
  mat = output_f_q,
  row_values = seq_len(nrow(output_f_q)),
  col_values = forecast_dates,
  row_name = "quantile",
  col_name = "Date",
  value_name = "Value"
)
df_q_forecast$Type <- "Forecast"

df_q <- bind_rows(df_q_fit, df_q_forecast)

# 4. Observed values for USGS
obs_df <- usgs_plot_df %>% 
  mutate(Source = "USGS", colgroup = ifelse(obs_type == "After", "After", "Before"))
obs_label_x <- safe_max_date(obs_df$time, fallback_date = usgs_right_x)
glofas_after_ens_df_valid <- fast_long_ensembles(ensembles[[1]], glofas_dates)
nws_after_ens_df_valid <- fast_long_ensembles(ensembles[[2]], nws_dates)
ylim_post_valid <- compute_adaptive_ylim(
  df_post$Value,
  df_q$Value,
  obs_df$value,
  glofas_before_df$Value,
  nws_before_df$Value,
  glofas_after_ens_df_valid$value,
  nws_after_ens_df_valid$value,
  flood_stages_trans,
  cutoff_label_y,
  jan9_label_y
)

p_post <- ggplot() +
  # Flood stage lines and labels
  geom_hline(
    yintercept = flood_stages_trans,
    linetype = "dashed",
    color = "gray",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = CUTOFF_DATE,
    y = cutoff_label_y,
    label = cutoff_label_short,
    color = "gray40",
    size = 3.5,
    fontface = "bold",
    vjust = 4,
    hjust = -0.1 
  ) +
  annotate(
    "text",
    x = obs_label_x,
    y = flood_stages_trans,
    label = flood_stage_labels,
    hjust = 10.5,
    vjust = -0.5,
    color = "black",
    fontface = "italic",
    size = 3.5
  ) +
  # Vertical lines for forecast init and flood
  geom_vline(
    xintercept = as.numeric(CUTOFF_DATE), 
    color = "gray40", linetype = "dashed", linewidth = 0.5, alpha = 0.8
  ) +
  # Posterior samples ("spaghetti")
  geom_line(
    data = df_post, 
    aes(x = Date, y = (Value), group = interaction(Type, sample)), 
    color = "pink", linewidth = 0.15, alpha = 0.15
  ) +
  # Posterior quantile curves (thinner black lines)
  geom_line(
    data = df_q, 
    aes(x = Date, y = (Value), group = interaction(Type, quantile)), 
    color = "black", linewidth = 0.1
  ) +
  # USGS obs: before forecast
  geom_point(
    data = obs_df %>% filter(colgroup == "Before"), 
    aes(x = time, y = (value)), 
    color = usgs_green, size = 1.5
  ) +
  # USGS obs: after forecast (light green)
  geom_point(
    data = obs_df %>% filter(colgroup == "After"), 
    aes(x = time, y = (value)), 
    color = usgs_after_color, size = 2
  ) +
  geom_line(
    data = obs_df %>% filter(colgroup == "Before"),
    aes(x = time, y = (value)), color = usgs_green, linewidth = 0.5
  ) +
  geom_line(
    data = obs_df %>% filter(colgroup == "After"),
    aes(x = time, y = (value)), color = usgs_after_color, linewidth = 0.5, linetype = "dashed"
  ) +
  ############################
# GloFAS before (gray)
geom_line(
  data = glofas_before_df,
  aes(x = Date, y = Value, linetype = Source),
  color = "gray", linewidth = 0.5, alpha = 0.85
) +
geom_point(
  data = glofas_before_df,
  aes(x = Date, y = Value, shape = Source),
  color = "gray", size = 1.4, alpha = 0.85
) +
# NWS before (gray)
geom_line(
  data = nws_before_df,
  aes(x = Date, y = Value, linetype = Source),
  color = "gray", linewidth = 0.5, alpha = 0.85
) +
geom_point(
  data = nws_before_df,
  aes(x = Date, y = Value, shape = Source),
  color = "gray", size = 1.4, alpha = 0.85
) +
# GloFAS ensembles after (gray)
geom_line(
  data = glofas_after_ens_df_valid,
  aes(x = Date, y = value, group = member),
  color = "gray", alpha = 0.22, linewidth = 0.5, show.legend = FALSE
) +
# NWS ensembles after (gray)
geom_line(
  data = nws_after_ens_df_valid,
  aes(x = Date, y = value, group = member),
  color = "gray", alpha = 0.22, linewidth = 0.5, show.legend = FALSE
) +
  coord_cartesian(ylim = ylim_post_valid) +
  ############################
  scale_x_date(breaks = pretty_breaks(6), date_labels = "%b %d") +
  labs(
    title = "Posterior Predictive Samples and Quantiles\nwith USGS Observed Flow",
    x = "Date (2022-2023)",
    y = expression("Water Flow (Log-Log cm^3/s)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
p_post <- add_jan9_flood_marker(p_post, jan9_label_y)

print(p_post)

ggsave(
  filename = "SOURCE_WORKFLOW_REFERENCE",
  plot = p_post,
  width = 12,
  height = 6,
  units = "in",
  dpi = 900
)


output_uni_f <- synthesize_quantiles(y_reps_uni, percentiles = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95))
output_uni <- synthesize_quantiles(y_reps_hist_uni, percentiles = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95))

idx <- idx_sub

output_f_q <- colQuantiles(output_uni_f, probs = q_s, type = 8)
output_f_q <- t(output_f_q)

output_q <- colQuantiles(output_uni, probs = q_s, type = 8)
output_q <- t(output_q)

# Dates for fit (historical) and forecast
fit_dates <- as.Date(timestamps[idx])
forecast_dates <- daily_dates_for_matrix_cols(
  synth_f2,
  start_date = fit_dates[length(fit_dates)] + 1,
  context = "posterior_dates.synth_f2_second_block"
)

# 1. Posterior samples: historical (fit) and forecast (avoid pivot_longer)
df_post_fit <- fast_long_by_row(
  mat = log(synth_hist_uni),
  row_values = seq_len(nrow(synth_hist_uni)),
  col_values = fit_dates,
  row_name = "sample",
  col_name = "Date",
  value_name = "Value"
)
df_post_fit$Type <- "Fit"

df_post_forecast <- fast_long_by_row(
  mat = log(synth_f2),
  row_values = seq_len(nrow(synth_f2)),
  col_values = forecast_dates,
  row_name = "sample",
  col_name = "Date",
  value_name = "Value"
)
df_post_forecast$Type <- "Forecast"

df_post <- bind_rows(df_post_fit, df_post_forecast)

# 2. Quantile curves: historical (fit) and forecast (avoid pivot_longer)
df_q_fit <- fast_long_by_row(
  mat = log(synth_hist_uni_q),
  row_values = seq_len(nrow(synth_hist_uni_q)),
  col_values = fit_dates,
  row_name = "quantile",
  col_name = "Date",
  value_name = "Value"
)
df_q_fit$Type <- "Fit"

df_q_forecast <- fast_long_by_row(
  mat = log(synth_f2_q),
  row_values = seq_len(nrow(synth_f2_q)),
  col_values = forecast_dates,
  row_name = "quantile",
  col_name = "Date",
  value_name = "Value"
)
df_q_forecast$Type <- "Forecast"

df_q <- bind_rows(df_q_fit, df_q_forecast)

# 3. Observed values for USGS
obs_df <- usgs_plot_df %>% 
  mutate(Source = "USGS", colgroup = ifelse(obs_type == "After", "After", "Before"))
obs_label_x <- safe_max_date(obs_df$time, fallback_date = usgs_right_x)
ylim_post_counter_valid <- compute_adaptive_ylim(
  df_post$Value,
  df_q$Value,
  obs_df$value,
  flood_stages_trans,
  cutoff_label_y,
  jan9_label_y
)

# 4. Plot (as before, no need to change this part except color for 'After' points/lines)
p_post <- ggplot() +
  # Vertical lines for forecast init and flood
  geom_vline(
    xintercept = as.numeric(CUTOFF_DATE), 
    color = "gray40", linetype = "dashed", linewidth = 0.5, alpha = 0.8
  ) +
  # Posterior samples
  geom_line(
    data = df_post, 
    aes(x = Date, y = Value, group = interaction(Type, sample)), 
    color = "pink", linewidth = 0.15, alpha = 0.15
  ) +
  # Posterior quantile curves
  geom_line(
    data = df_q, 
    aes(x = Date, y = Value, group = interaction(Type, quantile)), 
    color = "black", linewidth = 0.1
  ) +
  # USGS obs: before forecast
  geom_point(
    data = obs_df %>% filter(colgroup == "Before"), 
    aes(x = time, y = (value)), 
    color = usgs_green, size = 1.5
  ) +
  # USGS obs: after forecast (DARK RED)
  geom_point(
    data = obs_df %>% filter(colgroup == "After"), 
    aes(x = time, y = (value)), 
    color = usgs_after_color, size = 2
  ) +
  geom_line(
    data = obs_df %>% filter(colgroup == "Before"),
    aes(x = time, y = (value)), color = usgs_green, linewidth = 0.5
  ) +
  geom_line(
    data = obs_df %>% filter(colgroup == "After"),
    aes(x = time, y = (value)), color = usgs_after_color, linewidth = 0.5, linetype = "dashed"
  ) +
  # Flood stage lines and labels
  geom_hline(
    yintercept = flood_stages_trans,
    linetype = "dashed",
    color = "gray",
    linewidth = 0.8
  ) +
    coord_cartesian(ylim = ylim_post_counter_valid) +
  annotate(
    "text",
    x = obs_label_x,
    y = flood_stages_trans,
    label = flood_stage_labels,
    hjust = 10.5,
    vjust = -0.5,
    color = "black",
    fontface = "italic",
    size = 3.5
  ) +
    annotate(
    "text",
    x = CUTOFF_DATE,
    y = cutoff_label_y,
    label = cutoff_label_short,
    color = "gray40",
    size = 3.5,
    fontface = "bold",
    vjust = 4,
    hjust = -0.1 
  ) +
  ############################
  scale_x_date(breaks = pretty_breaks(6), date_labels = "%b %d") +
  labs(
    title = "Posterior Predictive Samples and Quantiles\nwith USGS Observed Flow",
    x = "Date",
    y = expression("Water Flow (Log-Log cm^3/s)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
p_post <- add_jan9_flood_marker(p_post, jan9_label_y)

print(p_post)

ggsave(
  filename = "SOURCE_WORKFLOW_REFERENCE",
  plot = p_post,
  width = 12,
  height = 6,
  units = "in",
  dpi = 900
)
