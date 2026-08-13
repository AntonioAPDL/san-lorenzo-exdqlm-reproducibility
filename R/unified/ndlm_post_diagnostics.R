# unified/ndlm_post_diagnostics.R

unified_ndlm_diag_num <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 0L) return(NA_real_)
  out[[1L]]
}

unified_ndlm_diag_int <- function(x) {
  out <- suppressWarnings(as.integer(x))
  if (length(out) == 0L) return(NA_integer_)
  out[[1L]]
}

unified_ndlm_diag_read_csv <- function(path, label) {
  if (is.null(path) || !nzchar(path)) {
    stop(sprintf("[NDLM_DIAG_INPUT_PATH] Missing %s CSV path.", label), call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("[NDLM_DIAG_INPUT_PATH] %s CSV does not exist: %s", label, path), call. = FALSE)
  }
  out <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) e
  )
  if (inherits(out, "error") || !is.data.frame(out) || nrow(out) < 1L) {
    stop(sprintf("[NDLM_DIAG_INPUT_READ] Unable to read non-empty %s CSV: %s", label, path), call. = FALSE)
  }
  out
}

unified_ndlm_diag_extract_date_column <- function(df) {
  cand <- c("Date", "date", "target_date", "timestamp", "Timestamp")
  for (nm in cand) {
    if (!nm %in% names(df)) next
    vals <- suppressWarnings(as.Date(df[[nm]]))
    if (length(vals) == nrow(df) && sum(is.na(vals)) < length(vals)) {
      return(vals)
    }
  }
  as.Date(rep(NA_character_, nrow(df)))
}

unified_ndlm_diag_pick_numeric_column <- function(df, preferred = character(0)) {
  if (length(preferred) > 0L) {
    for (nm in preferred) {
      if (nm %in% names(df) && is.numeric(df[[nm]])) {
        return(as.numeric(df[[nm]]))
      }
    }
  }
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols) == 0L) return(numeric(0))
  as.numeric(df[[num_cols[[1L]]]])
}

unified_ndlm_diag_parse_progress_log <- function(log_path) {
  cols <- c(
    "iter", "elbo", "crit_elbo", "sigma_exp", "sigma_usgs_exp", "sigma_nws_exp", "sigma_glofas_exp", "gamma_exp", "state_norm_sq",
    "w_hist", "w_fore", "df_t", "df_s1", "df_s2", "df_s67", "df_discrep", "lambda"
  )
  empty <- stats::setNames(data.frame(matrix(ncol = length(cols), nrow = 0L)), cols)
  if (is.null(log_path) || !nzchar(log_path) || !file.exists(log_path)) {
    return(empty)
  }

  lines <- readLines(log_path, warn = FALSE)
  rows <- vector("list", length(lines))
  row_i <- 0L
  for (ln in lines) {
    if (!grepl("\\[gamsig_progress\\]", ln)) next

    extract_token <- function(key) {
      pat <- sprintf(".*%s=([^ ]+).*", key)
      if (!grepl(sprintf("%s=", key), ln, fixed = TRUE)) return(NA_character_)
      sub(pat, "\\1", ln)
    }

    row_i <- row_i + 1L
    rows[[row_i]] <- list(
      iter = unified_ndlm_diag_int(extract_token("iter")),
      elbo = unified_ndlm_diag_num(extract_token("elbo")),
      crit_elbo = unified_ndlm_diag_num(extract_token("crit_elbo")),
      sigma_exp = unified_ndlm_diag_num(extract_token("sigma_exp")),
      sigma_usgs_exp = unified_ndlm_diag_num(extract_token("sigma_usgs_exp")),
      sigma_nws_exp = unified_ndlm_diag_num(extract_token("sigma_nws_exp")),
      sigma_glofas_exp = unified_ndlm_diag_num(extract_token("sigma_glofas_exp")),
      gamma_exp = unified_ndlm_diag_num(extract_token("gamma_exp")),
      state_norm_sq = unified_ndlm_diag_num(extract_token("state_norm_sq")),
      w_hist = unified_ndlm_diag_num(extract_token("w_hist")),
      w_fore = unified_ndlm_diag_num(extract_token("w_fore")),
      df_t = unified_ndlm_diag_num(extract_token("df_t")),
      df_s1 = unified_ndlm_diag_num(extract_token("df_s1")),
      df_s2 = unified_ndlm_diag_num(extract_token("df_s2")),
      df_s67 = unified_ndlm_diag_num(extract_token("df_s67")),
      df_discrep = unified_ndlm_diag_num(extract_token("df_discrep")),
      lambda = unified_ndlm_diag_num(extract_token("lambda"))
    )
  }

  if (row_i == 0L) {
    return(empty)
  }

  rows <- rows[seq_len(row_i)]
  out <- as.data.frame(do.call(rbind, lapply(rows, as.data.frame)), stringsAsFactors = FALSE)
  out$iter <- as.integer(out$iter)
  out
}

unified_ndlm_diag_shape_row <- function(object_name, value) {
  obj_type <- paste(class(value), collapse = "|")
  obj_rank <- if (is.null(dim(value))) {
    if (is.list(value)) NA_integer_ else 1L
  } else {
    length(dim(value))
  }
  dims_txt <- if (is.list(value)) {
    if (length(value) == 0L) {
      ""
    } else {
      paste(vapply(value, function(x) {
        d <- dim(x)
        if (is.null(d)) {
          as.character(length(x))
        } else {
          paste(as.integer(d), collapse = "x")
        }
      }, character(1)), collapse = ";")
    }
  } else {
    d <- dim(value)
    if (is.null(d)) as.character(length(value)) else paste(as.integer(d), collapse = "x")
  }

  data.frame(
    object = object_name,
    type = obj_type,
    rank = obj_rank,
    dims = dims_txt,
    stringsAsFactors = FALSE
  )
}

unified_ndlm_diag_date_span <- function(dates) {
  if (length(dates) == 0L) return(c(t_min = "", t_max = ""))
  ok <- !is.na(dates)
  if (!any(ok)) return(c(t_min = "", t_max = ""))
  c(t_min = as.character(min(dates[ok])), t_max = as.character(max(dates[ok])))
}

unified_ndlm_diag_named_int <- function(x, name, fallback = NA_integer_) {
  if (is.null(x)) return(as.integer(fallback))
  if (!is.null(names(x)) && (name %in% names(x))) {
    return(unified_ndlm_diag_int(x[[name]]))
  }
  unified_ndlm_diag_int(x[[1L]])
}

unified_ndlm_diag_cov_row <- function(object_name, cov_arr) {
  dims <- dim(cov_arr)
  if (is.null(dims) || length(dims) != 3L || dims[1] != dims[2]) {
    return(data.frame(
      object = object_name,
      n_slices = NA_integer_,
      matrix_dim = NA_integer_,
      nonfinite_slices = NA_integer_,
      asymmetry_max = NA_real_,
      min_diag_min = NA_real_,
      min_eig_min = NA_real_,
      min_eig_p01 = NA_real_,
      base_chol_fail_slices = NA_integer_,
      base_chol_fail_rate = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  n_slices <- as.integer(dims[3])
  min_eigs <- rep(NA_real_, n_slices)
  min_diags <- rep(NA_real_, n_slices)
  asym <- rep(NA_real_, n_slices)
  nonfinite <- rep(FALSE, n_slices)
  base_fail <- rep(FALSE, n_slices)
  for (k in seq_len(n_slices)) {
    S <- as.matrix(cov_arr[, , k, drop = TRUE])
    if (!all(is.finite(S))) {
      nonfinite[k] <- TRUE
      next
    }
    S <- (S + t(S)) / 2
    asym[k] <- max(abs(S - t(S)))
    min_diags[k] <- min(diag(S))
    min_eigs[k] <- min(eigen(S, symmetric = TRUE, only.values = TRUE)$values)
    base_fail[k] <- is.null(tryCatch(chol(S + diag(1e-8, nrow(S))), error = function(e) NULL))
  }

  data.frame(
    object = object_name,
    n_slices = n_slices,
    matrix_dim = as.integer(dims[1]),
    nonfinite_slices = as.integer(sum(nonfinite)),
    asymmetry_max = if (all(is.na(asym))) NA_real_ else max(asym, na.rm = TRUE),
    min_diag_min = if (all(is.na(min_diags))) NA_real_ else min(min_diags, na.rm = TRUE),
    min_eig_min = if (all(is.na(min_eigs))) NA_real_ else min(min_eigs, na.rm = TRUE),
    min_eig_p01 = if (all(is.na(min_eigs))) NA_real_ else as.numeric(stats::quantile(min_eigs, probs = 0.01, na.rm = TRUE, names = FALSE)),
    base_chol_fail_slices = as.integer(sum(base_fail, na.rm = TRUE)),
    base_chol_fail_rate = mean(base_fail, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

unified_ndlm_diag_write_trace_plot <- function(df, x_col, y_col, path, main, ylab) {
  if (!is.data.frame(df) || !(x_col %in% names(df)) || !(y_col %in% names(df))) return(FALSE)
  x <- suppressWarnings(as.numeric(df[[x_col]]))
  y <- suppressWarnings(as.numeric(df[[y_col]]))
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2L) return(FALSE)

  grDevices::png(filename = path, width = 1400, height = 800, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(4.2, 4.6, 3.4, 1.2))
  graphics::plot(
    x[ok], y[ok],
    type = "l",
    col = "#004C6D",
    lwd = 2.4,
    xlab = "Iteration",
    ylab = ylab,
    main = main
  )
  graphics::grid(col = "#D6DCE5", lty = "dotted")
  TRUE
}

unified_ndlm_diag_safe_filename <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- "scale"
  x
}

unified_ndlm_diag_extract_sigma_long <- function(iter_trace, env) {
  # Prefer explicit scale-sequence object if present; fallback to sigma/log traces.
  scale_obj <- if (exists("seq.scale_50_NDLM_synth_DISC", envir = env, inherits = FALSE)) {
    get("seq.scale_50_NDLM_synth_DISC", envir = env, inherits = FALSE)
  } else {
    NULL
  }
  sigma_obj <- if (exists("seq.sigma_50_NDLM_synth_DISC", envir = env, inherits = FALSE)) {
    get("seq.sigma_50_NDLM_synth_DISC", envir = env, inherits = FALSE)
  } else {
    NULL
  }

  iter_n <- if (is.data.frame(iter_trace) && nrow(iter_trace) > 0L && "iter" %in% names(iter_trace)) {
    max(suppressWarnings(as.integer(iter_trace$iter)), na.rm = TRUE)
  } else {
    NA_integer_
  }
  if (!is.finite(iter_n) || iter_n < 1L) iter_n <- NA_integer_

  normalize_trace_matrix <- function(x, iter_n) {
    if (is.null(x) || !is.numeric(x)) return(NULL)
    if (is.null(dim(x))) {
      mat <- matrix(as.numeric(x), ncol = 1L)
      colnames(mat) <- "sigma_exp"
      return(mat)
    }
    d <- dim(x)
    if (length(d) == 2L) {
      mat <- as.matrix(x)
      nr <- nrow(mat)
      nc <- ncol(mat)
      if (is.finite(iter_n)) {
        if (nr == iter_n) {
          # keep orientation
        } else if (nc == iter_n) {
          mat <- t(mat)
          nr <- nrow(mat)
          nc <- ncol(mat)
        } else if (nc > nr) {
          mat <- t(mat)
        }
      } else if (nc > nr) {
        mat <- t(mat)
      }
      return(mat)
    }
    iter_dim <- if (is.finite(iter_n) && any(d == iter_n)) {
      which(d == iter_n)[1L]
    } else {
      length(d)
    }
    perm <- c(iter_dim, setdiff(seq_along(d), iter_dim))
    arr <- aperm(array(as.numeric(x), dim = d), perm = perm)
    mat <- matrix(arr, nrow = d[[iter_dim]])
    mat
  }

  append_rows_from_matrix <- function(mat, default_label = "scale") {
    if (is.null(mat) || !is.matrix(mat) || nrow(mat) < 1L || ncol(mat) < 1L) {
      return(list())
    }
    lbls <- colnames(mat)
    if (is.null(lbls) || length(lbls) != ncol(mat)) {
      lbls <- sprintf("%s_%02d", default_label, seq_len(ncol(mat)))
    } else {
      lbls <- ifelse(nzchar(lbls), lbls, sprintf("%s_%02d", default_label, seq_len(ncol(mat))))
    }
    out <- vector("list", ncol(mat))
    for (j in seq_len(ncol(mat))) {
      out[[j]] <- data.frame(
        iter = seq_len(nrow(mat)),
        scale_key = sprintf("scale_%02d", j),
        scale_label = as.character(lbls[[j]]),
        sigma = as.numeric(mat[, j]),
        stringsAsFactors = FALSE
      )
    }
    out
  }

  rows <- list()
  scale_mat <- normalize_trace_matrix(scale_obj, iter_n = iter_n)
  if (!is.null(scale_mat)) {
    rows <- append_rows_from_matrix(scale_mat, default_label = "scale")
  }

  if (length(rows) == 0L) {
    sigma_mat <- normalize_trace_matrix(sigma_obj, iter_n = iter_n)
    if (!is.null(sigma_mat)) {
      if (ncol(sigma_mat) == 1L && (is.null(colnames(sigma_mat)) || !nzchar(colnames(sigma_mat)[[1L]]))) {
        colnames(sigma_mat) <- "sigma_exp"
      }
      rows <- append_rows_from_matrix(sigma_mat, default_label = "sigma")
    }
  }

  if (length(rows) == 0L && is.data.frame(iter_trace) && nrow(iter_trace) > 0L) {
    scale_cols <- c("sigma_usgs_exp", "sigma_nws_exp", "sigma_glofas_exp", "sigma_exp", "w_hist", "w_fore", "df_t", "df_s1", "df_s2", "df_s67", "df_discrep", "lambda", "df_trans", "df_covs")
    scale_cols <- scale_cols[scale_cols %in% names(iter_trace)]
    if (length(scale_cols) > 0L && "iter" %in% names(iter_trace)) {
      iter_vals <- suppressWarnings(as.integer(iter_trace$iter))
      j <- 0L
      for (nm in scale_cols) {
        vals <- suppressWarnings(as.numeric(iter_trace[[nm]]))
        ok <- is.finite(iter_vals) & is.finite(vals)
        if (sum(ok) < 1L) next
        j <- j + 1L
        rows[[length(rows) + 1L]] <- data.frame(
          iter = iter_vals[ok],
          scale_key = sprintf("scale_%02d", j),
          scale_label = nm,
          sigma = vals[ok],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(iter = integer(0), scale_key = character(0), scale_label = character(0), sigma = numeric(0), stringsAsFactors = FALSE))
  }

  out <- do.call(rbind, rows)
  out <- out[is.finite(out$iter) & is.finite(out$sigma), , drop = FALSE]
  out$iter <- as.integer(out$iter)
  rownames(out) <- NULL
  out
}

unified_ndlm_diag_write_sigma_traces <- function(sigma_long, output_dir, primary_path) {
  if (!is.data.frame(sigma_long) || nrow(sigma_long) < 2L) {
    return(character(0))
  }
  paths <- character(0)
  scales <- unique(as.character(sigma_long$scale_key))
  scales <- scales[nzchar(scales)]
  if (length(scales) < 1L) return(paths)

  for (k in seq_along(scales)) {
    sk <- scales[[k]]
    sub <- sigma_long[sigma_long$scale_key == sk, , drop = FALSE]
    lbl <- if (nrow(sub) > 0L) as.character(sub$scale_label[[1L]]) else sk
    if (!nzchar(lbl)) lbl <- sk
    out_path <- if (k == 1L) {
      primary_path
    } else {
      file.path(output_dir, sprintf("ndlm_sigma_trace_%s.png", unified_ndlm_diag_safe_filename(sk)))
    }
    ok <- unified_ndlm_diag_write_trace_plot(
      df = sub,
      x_col = "iter",
      y_col = "sigma",
      path = out_path,
      main = sprintf("NDLM Sigma Trace (%s)", lbl),
      ylab = "Sigma"
    )
    if (isTRUE(ok)) paths <- c(paths, out_path)
  }
  paths
}

unified_ndlm_diag_loglog1p_from_log1p <- function(x) {
  vals <- suppressWarnings(as.numeric(x))
  # Current workflow policy: diagnostics stay on the same log1p scale as
  # retros, observations, and forecast ensembles.
  vals
}

unified_ndlm_diag_pick_usgs_flow_col <- function(df) {
  if (!is.data.frame(df) || nrow(df) < 1L) return("")
  nms <- names(df)
  preferred <- c("X_00060_00003", "X_00060_00003.y", "Flow", "flow", "value")
  for (nm in preferred) {
    if (nm %in% nms && is.numeric(df[[nm]])) return(nm)
  }
  hit <- grep("00060_00003", nms, fixed = TRUE, value = TRUE)
  if (length(hit) > 0L) {
    hit <- hit[vapply(hit, function(nm) is.numeric(df[[nm]]), logical(1))]
    if (length(hit) > 0L) return(hit[[1L]])
  }
  numeric_cols <- nms[vapply(df, is.numeric, logical(1))]
  if (length(numeric_cols) < 1L) return("")
  numeric_cols[[1L]]
}

unified_ndlm_diag_fetch_future_usgs <- function(usgs_site, start_date, end_date) {
  empty <- data.frame(
    date = as.Date(character(0)),
    observed_cfs = numeric(0),
    observed_log1p = numeric(0),
    observed = numeric(0),
    stringsAsFactors = FALSE
  )

  usgs_site <- if (is.null(usgs_site)) "" else as.character(usgs_site)
  if (!nzchar(usgs_site)) return(empty)
  start_date <- suppressWarnings(as.Date(start_date))
  end_date <- suppressWarnings(as.Date(end_date))
  if (length(start_date) < 1L || length(end_date) < 1L || is.na(start_date[[1L]]) || is.na(end_date[[1L]])) {
    return(empty)
  }
  if (end_date[[1L]] < start_date[[1L]]) return(empty)
  if (!requireNamespace("dataRetrieval", quietly = TRUE)) return(empty)

  raw <- tryCatch(
    dataRetrieval::readNWISdv(
      siteNumbers = usgs_site,
      parameterCd = "00060",
      statCd = "00003",
      startDate = as.character(start_date[[1L]]),
      endDate = as.character(end_date[[1L]])
    ),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) < 1L) return(empty)

  date_col <- if ("Date" %in% names(raw)) "Date" else if ("dateTime" %in% names(raw)) "dateTime" else ""
  if (!nzchar(date_col)) return(empty)
  d <- suppressWarnings(as.Date(raw[[date_col]]))

  flow_col <- unified_ndlm_diag_pick_usgs_flow_col(raw)
  if (!nzchar(flow_col)) return(empty)
  flow_cfs <- suppressWarnings(as.numeric(raw[[flow_col]]))
  ok <- !is.na(d) & is.finite(flow_cfs)
  if (sum(ok) < 1L) return(empty)

  cfs_to_cms <- 0.0283168466
  flow_log1p <- log(flow_cfs[ok] * cfs_to_cms + 1)
  flow_loglog1p <- unified_ndlm_diag_loglog1p_from_log1p(flow_log1p)

  out <- data.frame(
    date = d[ok],
    observed_cfs = as.numeric(flow_cfs[ok]),
    observed_log1p = as.numeric(flow_log1p),
    observed = as.numeric(flow_loglog1p),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$date), , drop = FALSE]
  rownames(out) <- NULL
  out
}

unified_ndlm_diag_write_fit_plot <- function(
  dates,
  obs,
  fit,
  path,
  title,
  x_as_date = TRUE
) {
  obs <- suppressWarnings(as.numeric(obs))
  fit <- suppressWarnings(as.numeric(fit))
  n <- min(length(obs), length(fit), length(dates))
  if (n < 2L) return(FALSE)
  obs <- obs[seq_len(n)]
  fit <- fit[seq_len(n)]
  d <- dates[seq_len(n)]
  ok <- is.finite(obs) & is.finite(fit)
  if (x_as_date) {
    ok <- ok & !is.na(d)
    x <- d
    xlab <- "Date"
  } else {
    x <- seq_len(n)
    xlab <- "Index"
  }
  if (sum(ok) < 2L) return(FALSE)

  y_rng <- range(c(obs[ok], fit[ok]), finite = TRUE)
  if (!all(is.finite(y_rng))) return(FALSE)
  pad <- 0.05 * max(diff(y_rng), 1e-8)
  y_lim <- c(y_rng[1] - pad, y_rng[2] + pad)

  grDevices::png(filename = path, width = 1600, height = 900, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(4.2, 4.8, 3.4, 1.4))
  graphics::plot(
    x[ok], obs[ok],
    type = "o",
    pch = 16,
    cex = 0.35,
    lwd = 1.2,
    col = "#171A1F",
    xlab = xlab,
    ylab = "log(log1p(cms))",
    ylim = y_lim,
    main = title
  )
  graphics::lines(x[ok], fit[ok], col = "#D1495B", lwd = 2.2)
  graphics::grid(col = "#D6DCE5", lty = "dotted")
  graphics::legend(
    "topright",
    legend = c("Observed (USGS)", "NDLM dynamic location fit"),
    col = c("#171A1F", "#D1495B"),
    lwd = c(1.2, 2.2),
    pch = c(16, NA),
    pt.cex = c(0.6, NA),
    bty = "n"
  )
  TRUE
}

unified_ndlm_diag_write_fit_modes_plot <- function(df, path, title) {
  req <- c("date", "observed", "one_step_predicted", "filtered_fit", "smoothed_fit")
  if (!is.data.frame(df) || !all(req %in% names(df)) || nrow(df) < 2L) return(FALSE)

  obs <- suppressWarnings(as.numeric(df$observed))
  one_step <- suppressWarnings(as.numeric(df$one_step_predicted))
  filt <- suppressWarnings(as.numeric(df$filtered_fit))
  smooth <- suppressWarnings(as.numeric(df$smoothed_fit))
  d <- suppressWarnings(as.Date(df$date))
  use_date <- any(!is.na(d))
  x <- if (use_date) d else seq_len(nrow(df))
  ok_obs <- is.finite(obs) & if (use_date) !is.na(x) else TRUE
  if (sum(ok_obs) < 2L) return(FALSE)

  y_stack <- c(obs[ok_obs], one_step[is.finite(one_step)], filt[is.finite(filt)], smooth[is.finite(smooth)])
  y_rng <- range(y_stack, finite = TRUE)
  if (!all(is.finite(y_rng))) return(FALSE)
  pad <- 0.05 * max(diff(y_rng), 1e-8)
  y_lim <- c(y_rng[1] - pad, y_rng[2] + pad)

  grDevices::png(filename = path, width = 1800, height = 1000, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(4.2, 4.8, 3.4, 1.4))
  graphics::plot(
    x[ok_obs], obs[ok_obs],
    type = "o",
    pch = 16,
    cex = 0.32,
    lwd = 1.0,
    col = "#171A1F",
    xlab = if (use_date) "Date" else "Index",
    ylab = "log(log1p(cms))",
    ylim = y_lim,
    main = title
  )
  ok_one <- is.finite(one_step) & if (use_date) !is.na(x) else TRUE
  if (sum(ok_one) >= 2L) graphics::lines(x[ok_one], one_step[ok_one], col = "#1E88E5", lwd = 1.8, lty = 2)
  ok_filt <- is.finite(filt) & if (use_date) !is.na(x) else TRUE
  if (sum(ok_filt) >= 2L) graphics::lines(x[ok_filt], filt[ok_filt], col = "#00897B", lwd = 1.8, lty = 3)
  ok_smooth <- is.finite(smooth) & if (use_date) !is.na(x) else TRUE
  if (sum(ok_smooth) >= 2L) graphics::lines(x[ok_smooth], smooth[ok_smooth], col = "#D1495B", lwd = 2.2, lty = 1)
  graphics::grid(col = "#D6DCE5", lty = "dotted")
  graphics::legend(
    "topright",
    legend = c("Observed", "One-step predicted", "Filtered fit", "Smoothed fit"),
    col = c("#171A1F", "#1E88E5", "#00897B", "#D1495B"),
    lwd = c(1.0, 1.8, 1.8, 2.2),
    lty = c(1, 2, 3, 1),
    pch = c(16, NA, NA, NA),
    pt.cex = c(0.55, NA, NA, NA),
    bty = "n"
  )
  TRUE
}

unified_ndlm_diag_build_mu_obs_long <- function(exps, retros_df, retros_dates) {
  if (!is.numeric(exps) || is.null(dim(exps)) || length(dim(exps)) != 2L) {
    return(data.frame())
  }
  exps_mat <- as.matrix(exps)
  n_mu <- nrow(exps_mat)
  n_t <- ncol(exps_mat)
  if (!is.finite(n_mu) || !is.finite(n_t) || n_mu < 1L || n_t < 1L) {
    return(data.frame())
  }

  obs_cols <- names(retros_df)[vapply(retros_df, is.numeric, logical(1))]
  n_obs <- length(obs_cols)
  n_hist <- nrow(retros_df)
  if (!is.finite(n_hist) || n_hist < 0L) n_hist <- 0L

  dates_hist <- suppressWarnings(as.Date(retros_dates))
  if (length(dates_hist) < n_hist) {
    dates_hist <- c(dates_hist, rep(as.Date(NA_character_), n_hist - length(dates_hist)))
  } else if (length(dates_hist) > n_hist) {
    dates_hist <- dates_hist[seq_len(n_hist)]
  }
  if (n_t > n_hist) {
    dates_full <- c(dates_hist, rep(as.Date(NA_character_), n_t - n_hist))
  } else {
    dates_full <- dates_hist[seq_len(n_t)]
  }

  rows <- vector("list", n_mu)
  for (j in seq_len(n_mu)) {
    mu_vals <- suppressWarnings(as.numeric(exps_mat[j, ]))
    obs_vals_log1p <- rep(NA_real_, n_t)
    src_label <- sprintf("series_%02d", j)
    if (j <= n_obs) {
      src_label <- as.character(obs_cols[[j]])
      obs_j <- suppressWarnings(as.numeric(retros_df[[obs_cols[[j]]]]))
      n_copy <- min(length(obs_j), n_t)
      if (n_copy > 0L) {
        obs_vals_log1p[seq_len(n_copy)] <- obs_j[seq_len(n_copy)]
      }
    }
    obs_vals <- unified_ndlm_diag_loglog1p_from_log1p(obs_vals_log1p)

    rows[[j]] <- data.frame(
      source_index = as.integer(j),
      source_label = src_label,
      t_index = as.integer(seq_len(n_t)),
      date = dates_full,
      segment = ifelse(seq_len(n_t) <= n_hist, "historical", "forecast"),
      observed_log1p = obs_vals_log1p,
      observed = obs_vals,
      mu = mu_vals,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

unified_ndlm_diag_write_mu_obs_panels <- function(mu_obs_df, path, title, historical_only = FALSE) {
  req <- c("source_index", "source_label", "t_index", "date", "observed", "mu", "segment")
  if (!is.data.frame(mu_obs_df) || !all(req %in% names(mu_obs_df)) || nrow(mu_obs_df) < 2L) return(FALSE)

  work <- mu_obs_df
  if (isTRUE(historical_only)) {
    work <- work[work$segment == "historical", , drop = FALSE]
    if (nrow(work) < 2L) return(FALSE)
  }

  src_ids <- sort(unique(suppressWarnings(as.integer(work$source_index))))
  src_ids <- src_ids[is.finite(src_ids)]
  if (length(src_ids) < 1L) return(FALSE)

  n_panels <- length(src_ids)
  n_col <- min(3L, max(1L, ceiling(sqrt(n_panels))))
  n_row <- max(1L, ceiling(n_panels / n_col))
  width_px <- max(1400L, 520L * n_col)
  height_px <- max(900L, 320L * n_row)

  grDevices::png(filename = path, width = width_px, height = height_px, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(n_row, n_col), mar = c(3.1, 3.8, 2.2, 1.2), oma = c(0.5, 0.5, 2.0, 0))

  for (sid in src_ids) {
    sub <- work[suppressWarnings(as.integer(work$source_index)) == sid, , drop = FALSE]
    obs <- suppressWarnings(as.numeric(sub$observed))
    mu <- suppressWarnings(as.numeric(sub$mu))
    x_idx <- suppressWarnings(as.integer(sub$t_index))
    x_date <- suppressWarnings(as.Date(sub$date))
    use_date <- isTRUE(historical_only) && any(!is.na(x_date))
    x <- if (use_date) x_date else x_idx
    ok_obs <- is.finite(obs) & if (use_date) !is.na(x) else is.finite(x)
    ok_mu <- is.finite(mu) & if (use_date) !is.na(x) else is.finite(x)
    y_rng <- range(c(obs[ok_obs], mu[ok_mu]), finite = TRUE)
    if (!all(is.finite(y_rng))) {
      graphics::plot.new()
      graphics::title(main = as.character(sub$source_label[[1L]]))
      next
    }
    pad <- 0.05 * max(diff(y_rng), 1e-8)
    y_lim <- c(y_rng[1] - pad, y_rng[2] + pad)

    graphics::plot(x[ok_mu], mu[ok_mu], type = "l", col = "#D1495B", lwd = 2.1,
                   xlab = if (use_date) "Date" else "Time index", ylab = "log(log1p(cms))",
                   ylim = y_lim, main = as.character(sub$source_label[[1L]]))
    if (sum(ok_obs) >= 1L) {
      graphics::lines(x[ok_obs], obs[ok_obs], col = "#1A1A1A", lwd = 1.3)
      graphics::points(x[ok_obs], obs[ok_obs], col = "#1A1A1A", pch = 16, cex = 0.22)
    }
    if (!isTRUE(historical_only)) {
      hist_idx <- suppressWarnings(as.integer(sub$t_index[sub$segment == "historical"]))
      if (length(hist_idx) > 0L && is.finite(max(hist_idx, na.rm = TRUE))) {
        graphics::abline(v = max(hist_idx, na.rm = TRUE), col = "#6B7280", lty = 2)
      }
    }
    graphics::grid(col = "#D6DCE5", lty = "dotted")
    graphics::legend("topright", legend = c("mu_t", "observed"),
                     col = c("#D1495B", "#1A1A1A"), lty = 1, lwd = c(2.1, 1.3),
                     pch = c(NA, 16), pt.cex = c(NA, 0.55), bty = "n", cex = 0.8)
  }
  graphics::mtext(title, outer = TRUE, line = 0.3, cex = 1.0)
  TRUE
}

unified_ndlm_diag_component_label <- function(component_id) {
  component_id <- suppressWarnings(as.integer(component_id[[1L]]))
  if (!is.finite(component_id) || component_id < 1L) {
    return("theta_unknown")
  }
  if (component_id <= 7L) {
    return(sprintf("hist_%02d (theta_%02d)", component_id, component_id))
  }
  if (component_id <= 14L) {
    return(sprintf("discrep_%02d (theta_%02d)", component_id - 7L, component_id))
  }
  sprintf("transfer_%02d (theta_%02d)", component_id - 14L, component_id)
}

unified_ndlm_diag_extract_theta_draws <- function(env, max_draws = NULL) {
  if (!exists("samp.theta_50_NDLM_synth_DISC", envir = env, inherits = FALSE)) {
    return(NULL)
  }
  raw_obj <- get("samp.theta_50_NDLM_synth_DISC", envir = env, inherits = FALSE)
  arr <- if (is.list(raw_obj) && !is.null(raw_obj$samp_theta)) raw_obj$samp_theta else raw_obj
  if (!is.numeric(arr)) return(NULL)
  d <- dim(arr)
  if (is.null(d) || length(d) != 3L) return(NULL)

  max_draws_i <- suppressWarnings(as.integer(max_draws))
  if (!length(max_draws_i)) {
    max_draws_i <- NA_integer_
  } else {
    max_draws_i <- max_draws_i[[1L]]
  }
  draw_dim <- suppressWarnings(as.integer(d[3]))
  if (is.finite(max_draws_i) && max_draws_i > 0L && is.finite(draw_dim) && draw_dim > max_draws_i) {
    idx <- unique(round(seq(1, draw_dim, length.out = max_draws_i)))
    idx <- idx[idx >= 1L & idx <= draw_dim]
    arr <- arr[, , idx, drop = FALSE]
  }
  arr
}

unified_ndlm_diag_summarize_state_draws <- function(theta_draws, dates = as.Date(character(0))) {
  d <- dim(theta_draws)
  if (is.null(d) || length(d) != 3L) {
    stop("[NDLM_STATE_DRAWS_SHAPE] theta_draws must be a numeric 3D array [state, time, draw]", call. = FALSE)
  }
  n_state <- as.integer(d[1])
  n_time <- as.integer(d[2])
  n_draw <- as.integer(d[3])
  if (!is.finite(n_state) || !is.finite(n_time) || !is.finite(n_draw) ||
      n_state < 1L || n_time < 1L || n_draw < 1L) {
    stop("[NDLM_STATE_DRAWS_SHAPE] theta_draws dimensions must be positive", call. = FALSE)
  }

  if (length(dates) < n_time) {
    dates_use <- as.Date(rep(NA_character_, n_time))
  } else {
    dates_use <- suppressWarnings(as.Date(dates[seq_len(n_time)]))
  }
  idx <- seq_len(n_time)

  summary_rows <- vector("list", n_state)
  coverage_rows <- vector("list", n_state)
  for (j in seq_len(n_state)) {
    mat_j <- theta_draws[j, , , drop = TRUE]
    if (is.null(dim(mat_j))) {
      mat_j <- matrix(as.numeric(mat_j), nrow = n_time, ncol = 1L)
    } else if (length(dim(mat_j)) != 2L) {
      mat_j <- matrix(as.numeric(mat_j), nrow = n_time, ncol = n_draw)
    }
    q025 <- apply(mat_j, 1L, stats::quantile, probs = 0.025, na.rm = TRUE, type = 7L, names = FALSE)
    q500 <- apply(mat_j, 1L, stats::quantile, probs = 0.500, na.rm = TRUE, type = 7L, names = FALSE)
    q975 <- apply(mat_j, 1L, stats::quantile, probs = 0.975, na.rm = TRUE, type = 7L, names = FALSE)
    mn <- rowMeans(mat_j, na.rm = TRUE)
    band <- q975 - q025
    ok <- is.finite(q025) & is.finite(q500) & is.finite(q975) & is.finite(mn)

    label_j <- unified_ndlm_diag_component_label(j)
    summary_rows[[j]] <- data.frame(
      component_id = as.integer(j),
      component_label = label_j,
      t_index = as.integer(idx),
      date = dates_use,
      q025 = as.numeric(q025),
      q500 = as.numeric(q500),
      q975 = as.numeric(q975),
      mean = as.numeric(mn),
      band_width = as.numeric(band),
      stringsAsFactors = FALSE
    )
    coverage_rows[[j]] <- data.frame(
      component_id = as.integer(j),
      component_label = label_j,
      n_time = as.integer(n_time),
      finite_points = as.integer(sum(ok)),
      finite_rate = as.numeric(sum(ok) / n_time),
      mean_band_width = if (any(ok)) mean(band[ok]) else NA_real_,
      median_band_width = if (any(ok)) stats::median(band[ok]) else NA_real_,
      q95_band_width = if (any(ok)) as.numeric(stats::quantile(band[ok], probs = 0.95, names = FALSE, na.rm = TRUE)) else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  list(
    summary = do.call(rbind, summary_rows),
    coverage = do.call(rbind, coverage_rows)
  )
}

unified_ndlm_diag_write_state_components_ci_plot <- function(summary_df, path, title, component_ids = NULL) {
  req <- c("component_id", "component_label", "t_index", "q025", "q500", "q975")
  if (!is.data.frame(summary_df) || !all(req %in% names(summary_df)) || nrow(summary_df) < 2L) return(FALSE)
  work <- summary_df
  if (!is.null(component_ids)) {
    keep <- suppressWarnings(as.integer(component_ids))
    keep <- keep[is.finite(keep)]
    work <- work[work$component_id %in% keep, , drop = FALSE]
  }
  comps <- sort(unique(suppressWarnings(as.integer(work$component_id))))
  comps <- comps[is.finite(comps)]
  if (length(comps) < 1L) return(FALSE)

  n_panels <- length(comps)
  n_col <- min(4L, max(1L, ceiling(sqrt(n_panels))))
  n_row <- max(1L, ceiling(n_panels / n_col))
  width_px <- max(1400L, 520L * n_col)
  height_px <- max(900L, 320L * n_row)

  grDevices::png(filename = path, width = width_px, height = height_px, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(n_row, n_col), mar = c(2.9, 3.4, 2.2, 1.1), oma = c(0.5, 0.5, 2.0, 0))

  for (cid in comps) {
    sub <- work[work$component_id == cid, , drop = FALSE]
    x <- suppressWarnings(as.numeric(sub$t_index))
    lo <- suppressWarnings(as.numeric(sub$q025))
    md <- suppressWarnings(as.numeric(sub$q500))
    hi <- suppressWarnings(as.numeric(sub$q975))
    ok <- is.finite(x) & is.finite(lo) & is.finite(md) & is.finite(hi)
    panel_title <- as.character(sub$component_label[[1L]])
    if (sum(ok) < 2L) {
      graphics::plot.new()
      graphics::title(main = panel_title)
      next
    }
    y_rng <- range(c(lo[ok], hi[ok]), finite = TRUE)
    if (!all(is.finite(y_rng))) {
      graphics::plot.new()
      graphics::title(main = panel_title)
      next
    }
    pad <- 0.05 * max(diff(y_rng), 1e-8)
    y_lim <- c(y_rng[1] - pad, y_rng[2] + pad)

    graphics::plot(
      x[ok], md[ok],
      type = "n",
      xlab = "Time index",
      ylab = "State value",
      ylim = y_lim,
      main = panel_title
    )
    graphics::polygon(
      x = c(x[ok], rev(x[ok])),
      y = c(lo[ok], rev(hi[ok])),
      col = grDevices::adjustcolor("#80B1D3", alpha.f = 0.30),
      border = NA
    )
    graphics::lines(x[ok], md[ok], col = "#0B3C5D", lwd = 1.7)
    graphics::grid(col = "#D6DCE5", lty = "dotted")
  }

  graphics::mtext(title, outer = TRUE, line = 0.2, cex = 1.0)
  TRUE
}

unified_ndlm_diag_extract_member_matrix <- function(df) {
  if (!is.data.frame(df) || nrow(df) < 1L) {
    return(matrix(numeric(0), nrow = 0L, ncol = 0L))
  }
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols) < 1L) {
    return(matrix(numeric(0), nrow = nrow(df), ncol = 0L))
  }
  as.matrix(df[, num_cols, drop = FALSE])
}

unified_ndlm_diag_build_ensemble_summary <- function(source_key, source_label, dates, members_mat_loglog) {
  if (!is.numeric(members_mat_loglog) || is.null(dim(members_mat_loglog)) ||
      length(dim(members_mat_loglog)) != 2L || nrow(members_mat_loglog) < 1L ||
      ncol(members_mat_loglog) < 1L) {
    return(data.frame())
  }
  n <- as.integer(nrow(members_mat_loglog))
  if (length(dates) < n) {
    dates <- c(dates, rep(as.Date(NA_character_), n - length(dates)))
  } else if (length(dates) > n) {
    dates <- dates[seq_len(n)]
  }

  qfun <- function(v, p) {
    vv <- v[is.finite(v)]
    if (length(vv) < 1L) return(NA_real_)
    as.numeric(stats::quantile(vv, probs = p, names = FALSE, na.rm = TRUE, type = 7L))
  }

  q05 <- apply(members_mat_loglog, 1L, qfun, p = 0.05)
  q50 <- apply(members_mat_loglog, 1L, qfun, p = 0.50)
  q95 <- apply(members_mat_loglog, 1L, qfun, p = 0.95)
  mn <- rowMeans(members_mat_loglog, na.rm = TRUE)
  n_mem <- apply(members_mat_loglog, 1L, function(v) sum(is.finite(v)))

  data.frame(
    source_key = source_key,
    source_label = source_label,
    lead = seq_len(n),
    date = dates,
    ensemble_q05 = as.numeric(q05),
    ensemble_q50 = as.numeric(q50),
    ensemble_q95 = as.numeric(q95),
    ensemble_mean = as.numeric(mn),
    n_members = as.integer(n_mem),
    stringsAsFactors = FALSE
  )
}

unified_ndlm_diag_build_ensemble_members_long <- function(source_key, source_label, dates, members_mat_loglog) {
  if (!is.numeric(members_mat_loglog) || is.null(dim(members_mat_loglog)) ||
      length(dim(members_mat_loglog)) != 2L || nrow(members_mat_loglog) < 1L ||
      ncol(members_mat_loglog) < 1L) {
    return(data.frame())
  }
  n <- as.integer(nrow(members_mat_loglog))
  if (length(dates) < n) {
    dates <- c(dates, rep(as.Date(NA_character_), n - length(dates)))
  } else if (length(dates) > n) {
    dates <- dates[seq_len(n)]
  }

  rows <- vector("list", as.integer(ncol(members_mat_loglog)))
  for (j in seq_len(ncol(members_mat_loglog))) {
    rows[[j]] <- data.frame(
      source_key = source_key,
      source_label = source_label,
      member = sprintf("member_%03d", as.integer(j)),
      lead = seq_len(n),
      date = dates,
      value = as.numeric(members_mat_loglog[, j]),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

unified_ndlm_diag_match_forecast_sources <- function(row_leads, row_labels, source_leads) {
  if (length(row_leads) < 1L || length(source_leads) < 1L) {
    return(rep(NA_character_, length(row_leads)))
  }
  if (is.null(names(source_leads))) {
    names(source_leads) <- sprintf("source_%02d", seq_along(source_leads))
  }

  row_leads <- suppressWarnings(as.integer(row_leads))
  row_labels <- as.character(row_labels)
  out <- rep(NA_character_, length(row_leads))
  used <- rep(FALSE, length(source_leads))

  ord <- order(row_leads, decreasing = TRUE, na.last = TRUE)
  for (ii in ord) {
    k <- row_leads[[ii]]
    if (!is.finite(k) || k <= 0L) next
    diffs <- abs(as.numeric(source_leads) - k)
    diffs[used] <- Inf
    if (!any(is.finite(diffs))) next
    cand <- which(diffs == min(diffs, na.rm = TRUE))
    if (length(cand) > 1L) {
      lbl <- if (length(row_labels) >= ii) tolower(row_labels[[ii]]) else ""
      cand_keys <- names(source_leads)[cand]
      if (grepl("nws", lbl, fixed = TRUE) && "nws_forecast" %in% cand_keys) {
        cand <- cand[which(cand_keys == "nws_forecast")[1L]]
      } else if (grepl("glofas", lbl, fixed = TRUE) && "glofas_forecast" %in% cand_keys) {
        cand <- cand[which(cand_keys == "glofas_forecast")[1L]]
      } else {
        cand <- cand[[1L]]
      }
    } else {
      cand <- cand[[1L]]
    }
    out[[ii]] <- names(source_leads)[[cand]]
    used[[cand]] <- TRUE
  }

  # Fallback assignment for unassigned rows with forecast content.
  for (ii in seq_along(row_leads)) {
    if (!is.na(out[[ii]])) next
    k <- row_leads[[ii]]
    if (!is.finite(k) || k <= 0L) next
    diffs <- abs(as.numeric(source_leads) - k)
    if (!any(is.finite(diffs))) next
    out[[ii]] <- names(source_leads)[which.min(diffs)][[1L]]
  }

  out
}

unified_ndlm_diag_build_forecast_bundle <- function(exps, retros_df, nws_df, glofas_df, sigma_draws = NULL) {
  empty <- list(
    ensemble_summary = data.frame(),
    ensemble_members = data.frame(),
    ndlm_forecast = data.frame(),
    row_source_map = data.frame()
  )

  if (!is.numeric(exps) || is.null(dim(exps)) || length(dim(exps)) != 2L || ncol(exps) < 1L) {
    return(empty)
  }

  build_source <- function(df, source_key, source_label) {
    dates <- unified_ndlm_diag_extract_date_column(df)
    members_raw <- unified_ndlm_diag_extract_member_matrix(df)
    if (!is.numeric(members_raw) || is.null(dim(members_raw)) ||
        nrow(members_raw) < 1L || ncol(members_raw) < 1L) {
      return(list(
        key = source_key,
        label = source_label,
        dates = as.Date(character(0)),
        n = 0L,
        summary = data.frame(),
        members = data.frame()
      ))
    }
    members_loglog <- matrix(
      unified_ndlm_diag_loglog1p_from_log1p(as.numeric(members_raw)),
      nrow = nrow(members_raw),
      ncol = ncol(members_raw),
      dimnames = dimnames(members_raw)
    )
    summary_df <- unified_ndlm_diag_build_ensemble_summary(
      source_key = source_key,
      source_label = source_label,
      dates = dates,
      members_mat_loglog = members_loglog
    )
    members_long <- unified_ndlm_diag_build_ensemble_members_long(
      source_key = source_key,
      source_label = source_label,
      dates = dates,
      members_mat_loglog = members_loglog
    )
    list(
      key = source_key,
      label = source_label,
      dates = suppressWarnings(as.Date(summary_df$date)),
      n = if (is.data.frame(summary_df)) as.integer(nrow(summary_df)) else 0L,
      summary = summary_df,
      members = members_long
    )
  }

  src_nws <- build_source(nws_df, "nws_forecast", "NWS forecast ensemble")
  src_glofas <- build_source(glofas_df, "glofas_forecast", "GloFAS forecast ensemble")
  source_list <- list(src_nws, src_glofas)
  source_lengths <- vapply(source_list, function(s) as.integer(s$n), integer(1))
  names(source_lengths) <- vapply(source_list, function(s) as.character(s$key), character(1))
  source_dates <- lapply(source_list, function(s) suppressWarnings(as.Date(s$dates)))
  names(source_dates) <- names(source_lengths)

  n_state_rows <- as.integer(nrow(exps))
  retros_n <- as.integer(nrow(retros_df))
  if (!is.finite(retros_n) || retros_n < 0L) retros_n <- 0L
  if (ncol(exps) <= retros_n) {
    return(list(
      ensemble_summary = do.call(rbind, lapply(source_list, function(s) s$summary)),
      ensemble_members = do.call(rbind, lapply(source_list, function(s) s$members)),
      ndlm_forecast = data.frame(),
      row_source_map = data.frame(
        source_index = as.integer(seq_len(n_state_rows)),
        row_label = if (n_state_rows > 0L) sprintf("series_%02d", seq_len(n_state_rows)) else character(0),
        forecast_finite_leads = 0L,
        mapped_source = NA_character_,
        mapped_source_leads = NA_integer_,
        lead_diff = NA_integer_,
        sigma_var = NA_real_,
        sigma_sd = NA_real_,
        stringsAsFactors = FALSE
      )
    ))
  }

  mu_fore <- as.matrix(exps[, (retros_n + 1L):ncol(exps), drop = FALSE])
  row_labels <- names(retros_df)[vapply(retros_df, is.numeric, logical(1))]
  if (length(row_labels) < n_state_rows) {
    row_labels <- c(row_labels, sprintf("series_%02d", (length(row_labels) + 1L):n_state_rows))
  } else if (length(row_labels) > n_state_rows) {
    row_labels <- row_labels[seq_len(n_state_rows)]
  }
  if (length(row_labels) == 0L && n_state_rows > 0L) {
    row_labels <- sprintf("series_%02d", seq_len(n_state_rows))
  }

  row_leads <- apply(mu_fore, 1L, function(v) sum(is.finite(v)))
  mapped_source <- unified_ndlm_diag_match_forecast_sources(
    row_leads = row_leads,
    row_labels = row_labels,
    source_leads = source_lengths
  )

  sigma_var <- rep(NA_real_, n_state_rows)
  if (is.numeric(sigma_draws)) {
    sigma_mat <- as.matrix(sigma_draws)
    if (is.null(dim(sigma_mat))) {
      sigma_mat <- matrix(as.numeric(sigma_mat), ncol = 1L)
    }
    if (ncol(sigma_mat) < n_state_rows && nrow(sigma_mat) == n_state_rows) {
      sigma_mat <- t(sigma_mat)
    }
    if (ncol(sigma_mat) >= n_state_rows) {
      for (j in seq_len(n_state_rows)) {
        vals <- suppressWarnings(as.numeric(sigma_mat[, j]))
        vals <- vals[is.finite(vals)]
        sigma_var[[j]] <- if (length(vals) > 0L) stats::median(vals) else NA_real_
      }
    }
  }
  sigma_sd <- sqrt(pmax(sigma_var, 0))

  row_source_map <- data.frame(
    source_index = as.integer(seq_len(n_state_rows)),
    row_label = as.character(row_labels),
    forecast_finite_leads = as.integer(row_leads),
    mapped_source = as.character(mapped_source),
    mapped_source_leads = as.integer(ifelse(
      !is.na(mapped_source) & mapped_source %in% names(source_lengths),
      source_lengths[mapped_source],
      NA_integer_
    )),
    lead_diff = as.integer(ifelse(
      !is.na(mapped_source) & mapped_source %in% names(source_lengths),
      abs(source_lengths[mapped_source] - as.integer(row_leads)),
      NA_integer_
    )),
    sigma_var = as.numeric(sigma_var),
    sigma_sd = as.numeric(sigma_sd),
    stringsAsFactors = FALSE
  )

  ndlm_rows <- list()
  row_i <- 0L
  for (j in seq_len(n_state_rows)) {
    vals <- suppressWarnings(as.numeric(mu_fore[j, ]))
    idx <- which(is.finite(vals))
    if (length(idx) < 1L) next
    src_key <- mapped_source[[j]]
    src_dates <- if (!is.na(src_key) && src_key %in% names(source_dates)) source_dates[[src_key]] else as.Date(character(0))
    dvals <- rep(as.Date(NA_character_), length(idx))
    if (length(src_dates) > 0L) {
      ok_idx <- idx[idx <= length(src_dates)]
      if (length(ok_idx) > 0L) {
        dvals[match(ok_idx, idx)] <- src_dates[ok_idx]
      }
    }
    sd_j <- sigma_sd[[j]]
    q05 <- rep(NA_real_, length(idx))
    q95 <- rep(NA_real_, length(idx))
    if (is.finite(sd_j)) {
      q05 <- vals[idx] + sd_j * stats::qnorm(0.05)
      q95 <- vals[idx] + sd_j * stats::qnorm(0.95)
    }

    row_i <- row_i + 1L
    ndlm_rows[[row_i]] <- data.frame(
      source_index = as.integer(j),
      row_label = as.character(row_labels[[j]]),
      mapped_source = as.character(src_key),
      lead = as.integer(idx),
      date = dvals,
      mu = as.numeric(vals[idx]),
      q05 = as.numeric(q05),
      q50 = as.numeric(vals[idx]),
      q95 = as.numeric(q95),
      sigma_var = as.numeric(sigma_var[[j]]),
      sigma_sd = as.numeric(sd_j),
      stringsAsFactors = FALSE
    )
  }
  ndlm_forecast <- if (length(ndlm_rows) > 0L) do.call(rbind, ndlm_rows) else data.frame()
  ensemble_summary <- do.call(rbind, lapply(source_list, function(s) s$summary))
  ensemble_members <- do.call(rbind, lapply(source_list, function(s) s$members))
  if (!is.data.frame(ensemble_summary)) ensemble_summary <- data.frame()
  if (!is.data.frame(ensemble_members)) ensemble_members <- data.frame()
  if (!is.data.frame(ndlm_forecast)) ndlm_forecast <- data.frame()

  list(
    ensemble_summary = ensemble_summary,
    ensemble_members = ensemble_members,
    ndlm_forecast = ndlm_forecast,
    row_source_map = row_source_map
  )
}

unified_ndlm_diag_write_forecast_overlay_panels <- function(ensemble_summary, ndlm_forecast, usgs_future_obs, path, title) {
  req_e <- c("source_key", "source_label", "lead", "date", "ensemble_q05", "ensemble_q50", "ensemble_q95")
  if (!is.data.frame(ensemble_summary) || !all(req_e %in% names(ensemble_summary)) || nrow(ensemble_summary) < 2L) {
    return(FALSE)
  }

  src_keys <- unique(as.character(ensemble_summary$source_key))
  src_keys <- src_keys[nzchar(src_keys)]
  if (length(src_keys) < 1L) return(FALSE)

  n_panels <- length(src_keys)
  n_col <- min(2L, max(1L, n_panels))
  n_row <- max(1L, ceiling(n_panels / n_col))
  width_px <- max(1400L, 700L * n_col)
  height_px <- max(900L, 420L * n_row)

  grDevices::png(filename = path, width = width_px, height = height_px, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(n_row, n_col), mar = c(3.5, 4.1, 2.2, 1.1), oma = c(0.5, 0.5, 2.0, 0))

  for (src in src_keys) {
    e <- ensemble_summary[as.character(ensemble_summary$source_key) == src, , drop = FALSE]
    e <- e[order(suppressWarnings(as.integer(e$lead))), , drop = FALSE]
    x_date <- suppressWarnings(as.Date(e$date))
    use_date <- any(!is.na(x_date))
    x <- if (use_date) x_date else suppressWarnings(as.numeric(e$lead))

    q05e <- suppressWarnings(as.numeric(e$ensemble_q05))
    q50e <- suppressWarnings(as.numeric(e$ensemble_q50))
    q95e <- suppressWarnings(as.numeric(e$ensemble_q95))

    n <- ndlm_forecast
    if (is.data.frame(n) && nrow(n) > 0L && ("mapped_source" %in% names(n))) {
      n <- n[as.character(n$mapped_source) == src, , drop = FALSE]
      n <- n[order(suppressWarnings(as.integer(n$lead))), , drop = FALSE]
    } else {
      n <- data.frame()
    }
    x_n <- if (nrow(n) > 0L) {
      if (use_date) suppressWarnings(as.Date(n$date)) else suppressWarnings(as.numeric(n$lead))
    } else {
      numeric(0)
    }
    q05n <- if (nrow(n) > 0L) suppressWarnings(as.numeric(n$q05)) else numeric(0)
    q50n <- if (nrow(n) > 0L) suppressWarnings(as.numeric(n$q50)) else numeric(0)
    q95n <- if (nrow(n) > 0L) suppressWarnings(as.numeric(n$q95)) else numeric(0)

    ok_e <- is.finite(q50e) & (if (use_date) !is.na(x) else is.finite(x))
    y_stack <- c(q05e, q50e, q95e, q05n, q50n, q95n)
    y_rng <- range(y_stack, finite = TRUE)
    if (!all(is.finite(y_rng))) {
      graphics::plot.new()
      graphics::title(main = as.character(e$source_label[[1L]]))
      next
    }
    pad <- 0.07 * max(diff(y_rng), 1e-8)
    y_lim <- c(y_rng[1] - pad, y_rng[2] + pad)

    graphics::plot(
      x[ok_e], q50e[ok_e],
      type = "n",
      xlab = if (use_date) "Date" else "Forecast lead",
      ylab = "log(log1p(cms))",
      ylim = y_lim,
      main = as.character(e$source_label[[1L]]),
      xaxt = if (use_date) "n" else "s"
    )
    if (use_date && sum(ok_e) >= 2L) {
      at_num <- pretty(as.numeric(x[ok_e]), n = 7L)
      at_date <- suppressWarnings(as.Date(at_num, origin = "1970-01-01"))
      at_date <- at_date[!is.na(at_date)]
      if (length(at_date) > 0L) graphics::axis.Date(1, at = at_date, format = "%b %d")
    }

    ok_erib <- is.finite(q05e) & is.finite(q95e) & (if (use_date) !is.na(x) else is.finite(x))
    if (sum(ok_erib) >= 2L) {
      xp <- if (use_date) as.numeric(x[ok_erib]) else x[ok_erib]
      graphics::polygon(
        x = c(xp, rev(xp)),
        y = c(q05e[ok_erib], rev(q95e[ok_erib])),
        col = grDevices::adjustcolor("#9CA3AF", alpha.f = 0.32),
        border = NA
      )
    }
    if (sum(ok_e) >= 2L) {
      graphics::lines(x[ok_e], q50e[ok_e], col = "#374151", lwd = 1.8)
    }

    ok_n <- is.finite(q50n) & (if (use_date) !is.na(x_n) else is.finite(x_n))
    ok_nrib <- is.finite(q05n) & is.finite(q95n) & (if (use_date) !is.na(x_n) else is.finite(x_n))
    if (sum(ok_nrib) >= 2L) {
      xp <- if (use_date) as.numeric(x_n[ok_nrib]) else x_n[ok_nrib]
      graphics::polygon(
        x = c(xp, rev(xp)),
        y = c(q05n[ok_nrib], rev(q95n[ok_nrib])),
        col = grDevices::adjustcolor("#F59E0B", alpha.f = 0.24),
        border = NA
      )
    }
    if (sum(ok_n) >= 2L) {
      graphics::lines(x_n[ok_n], q50n[ok_n], col = "#B45309", lwd = 2.1)
    }

    if (is.data.frame(usgs_future_obs) && nrow(usgs_future_obs) > 0L && use_date) {
      u <- usgs_future_obs
      if (all(c("date", "observed") %in% names(u))) {
        u_date <- suppressWarnings(as.Date(u$date))
        u_val <- suppressWarnings(as.numeric(u$observed))
        date_min <- suppressWarnings(min(as.Date(e$date), na.rm = TRUE))
        date_max <- suppressWarnings(max(as.Date(e$date), na.rm = TRUE))
        ok_u <- !is.na(u_date) & is.finite(u_val) & !is.na(date_min) & !is.na(date_max) &
          u_date >= date_min & u_date <= date_max
        if (sum(ok_u) >= 1L) {
          ord_u <- order(u_date[ok_u])
          graphics::points(u_date[ok_u][ord_u], u_val[ok_u][ord_u], pch = 16, cex = 0.65, col = "#A21CAF")
          if (sum(ok_u) >= 2L) {
            graphics::lines(u_date[ok_u][ord_u], u_val[ok_u][ord_u], lwd = 1.1, lty = 3, col = "#A21CAF")
          }
        }
      }
    }

    x0 <- if (use_date && any(!is.na(x))) {
      min(x, na.rm = TRUE)
    } else if (!use_date && any(is.finite(x))) {
      min(x, na.rm = TRUE)
    } else {
      NA
    }
    if ((use_date && !is.na(x0)) || (!use_date && is.finite(x0))) {
      graphics::abline(v = x0, col = "#6B7280", lty = 3, lwd = 1.0)
    }
    graphics::grid(col = "#E5E7EB", lty = "dotted")
    graphics::legend(
      "topright",
      legend = c("Ensemble median", "Ensemble 5-95%", "NDLM mu_t", "NDLM q05-q95", "USGS realized future (unobserved at fit)"),
      col = c("#374151", "#9CA3AF", "#B45309", "#F59E0B", "#A21CAF"),
      lwd = c(1.8, 5.0, 2.1, 5.0, 1.1),
      lty = c(1, 1, 1, 1, 3),
      pch = c(NA, NA, NA, NA, 16),
      pt.cex = c(NA, NA, NA, NA, 0.65),
      bty = "n",
      cex = 0.78
    )
  }

  graphics::mtext(title, outer = TRUE, line = 0.2, cex = 1.0)
  TRUE
}

unified_ndlm_diag_write_forecast_member_panels <- function(ensemble_members, ndlm_forecast, usgs_future_obs, path, title) {
  req <- c("source_key", "source_label", "member", "lead", "date", "value")
  if (!is.data.frame(ensemble_members) || !all(req %in% names(ensemble_members)) || nrow(ensemble_members) < 2L) {
    return(FALSE)
  }

  src_keys <- unique(as.character(ensemble_members$source_key))
  src_keys <- src_keys[nzchar(src_keys)]
  if (length(src_keys) < 1L) return(FALSE)

  n_panels <- length(src_keys)
  n_col <- min(2L, max(1L, n_panels))
  n_row <- max(1L, ceiling(n_panels / n_col))
  width_px <- max(1400L, 700L * n_col)
  height_px <- max(900L, 420L * n_row)

  grDevices::png(filename = path, width = width_px, height = height_px, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(n_row, n_col), mar = c(3.5, 4.1, 2.2, 1.1), oma = c(0.5, 0.5, 2.0, 0))

  for (src in src_keys) {
    e <- ensemble_members[as.character(ensemble_members$source_key) == src, , drop = FALSE]
    if (nrow(e) < 2L) {
      graphics::plot.new()
      graphics::title(main = src)
      next
    }
    x_date <- suppressWarnings(as.Date(e$date))
    use_date <- any(!is.na(x_date))
    x <- if (use_date) x_date else suppressWarnings(as.numeric(e$lead))
    y <- suppressWarnings(as.numeric(e$value))

    n <- ndlm_forecast
    if (is.data.frame(n) && nrow(n) > 0L && ("mapped_source" %in% names(n))) {
      n <- n[as.character(n$mapped_source) == src, , drop = FALSE]
      n <- n[order(suppressWarnings(as.integer(n$lead))), , drop = FALSE]
    } else {
      n <- data.frame()
    }
    x_n <- if (nrow(n) > 0L) {
      if (use_date) suppressWarnings(as.Date(n$date)) else suppressWarnings(as.numeric(n$lead))
    } else {
      numeric(0)
    }
    q05n <- if (nrow(n) > 0L) suppressWarnings(as.numeric(n$q05)) else numeric(0)
    q50n <- if (nrow(n) > 0L) suppressWarnings(as.numeric(n$q50)) else numeric(0)
    q95n <- if (nrow(n) > 0L) suppressWarnings(as.numeric(n$q95)) else numeric(0)

    ok <- is.finite(y) & (if (use_date) !is.na(x) else is.finite(x))
    y_rng <- range(c(y[ok], q05n, q50n, q95n), finite = TRUE)
    if (!all(is.finite(y_rng))) {
      graphics::plot.new()
      graphics::title(main = as.character(e$source_label[[1L]]))
      next
    }
    pad <- 0.07 * max(diff(y_rng), 1e-8)
    y_lim <- c(y_rng[1] - pad, y_rng[2] + pad)
    plot_x <- if (sum(ok) >= 1L) x[ok] else if (use_date) as.Date(Sys.Date()) else 1
    plot_y <- if (sum(ok) >= 1L) y[ok] else 0

    graphics::plot(
      plot_x, plot_y,
      type = "n",
      xlab = if (use_date) "Date" else "Forecast lead",
      ylab = "log(log1p(cms))",
      ylim = y_lim,
      main = as.character(e$source_label[[1L]]),
      xaxt = if (use_date) "n" else "s"
    )
    if (use_date && sum(ok) >= 2L) {
      at_num <- pretty(as.numeric(x[ok]), n = 7L)
      at_date <- suppressWarnings(as.Date(at_num, origin = "1970-01-01"))
      at_date <- at_date[!is.na(at_date)]
      if (length(at_date) > 0L) graphics::axis.Date(1, at = at_date, format = "%b %d")
    }

    members <- unique(as.character(e$member))
    for (m in members) {
      sub <- e[as.character(e$member) == m, , drop = FALSE]
      sub <- sub[order(suppressWarnings(as.integer(sub$lead))), , drop = FALSE]
      xs <- if (use_date) suppressWarnings(as.Date(sub$date)) else suppressWarnings(as.numeric(sub$lead))
      ys <- suppressWarnings(as.numeric(sub$value))
      okm <- is.finite(ys) & (if (use_date) !is.na(xs) else is.finite(xs))
      if (sum(okm) >= 2L) {
        graphics::lines(xs[okm], ys[okm], col = grDevices::adjustcolor("#6B7280", alpha.f = 0.25), lwd = 0.8)
      }
    }

    ok_n <- is.finite(q50n) & (if (use_date) !is.na(x_n) else is.finite(x_n))
    ok_nrib <- is.finite(q05n) & is.finite(q95n) & (if (use_date) !is.na(x_n) else is.finite(x_n))
    if (sum(ok_nrib) >= 2L) {
      xp <- if (use_date) as.numeric(x_n[ok_nrib]) else x_n[ok_nrib]
      graphics::polygon(
        x = c(xp, rev(xp)),
        y = c(q05n[ok_nrib], rev(q95n[ok_nrib])),
        col = grDevices::adjustcolor("#F59E0B", alpha.f = 0.24),
        border = NA
      )
    }
    if (sum(ok_n) >= 2L) {
      graphics::lines(x_n[ok_n], q50n[ok_n], col = "#B45309", lwd = 2.1)
    }

    if (is.data.frame(usgs_future_obs) && nrow(usgs_future_obs) > 0L && use_date) {
      u <- usgs_future_obs
      if (all(c("date", "observed") %in% names(u))) {
        u_date <- suppressWarnings(as.Date(u$date))
        u_val <- suppressWarnings(as.numeric(u$observed))
        date_min <- suppressWarnings(min(as.Date(e$date), na.rm = TRUE))
        date_max <- suppressWarnings(max(as.Date(e$date), na.rm = TRUE))
        ok_u <- !is.na(u_date) & is.finite(u_val) & !is.na(date_min) & !is.na(date_max) &
          u_date >= date_min & u_date <= date_max
        if (sum(ok_u) >= 1L) {
          ord_u <- order(u_date[ok_u])
          graphics::points(u_date[ok_u][ord_u], u_val[ok_u][ord_u], pch = 16, cex = 0.65, col = "#A21CAF")
          if (sum(ok_u) >= 2L) {
            graphics::lines(u_date[ok_u][ord_u], u_val[ok_u][ord_u], lwd = 1.1, lty = 3, col = "#A21CAF")
          }
        }
      }
    }

    x0 <- if (use_date && any(!is.na(x))) {
      min(x, na.rm = TRUE)
    } else if (!use_date && any(is.finite(x))) {
      min(x, na.rm = TRUE)
    } else {
      NA
    }
    if ((use_date && !is.na(x0)) || (!use_date && is.finite(x0))) {
      graphics::abline(v = x0, col = "#6B7280", lty = 3, lwd = 1.0)
    }
    graphics::grid(col = "#E5E7EB", lty = "dotted")
    graphics::legend(
      "topright",
      legend = c("Ensemble members", "NDLM mu_t", "NDLM q05-q95", "USGS realized future (unobserved at fit)"),
      col = c("#6B7280", "#B45309", "#F59E0B", "#A21CAF"),
      lwd = c(1.2, 2.1, 5.0, 1.1),
      lty = c(1, 1, 1, 3),
      pch = c(NA, NA, NA, 16),
      pt.cex = c(NA, NA, NA, 0.65),
      bty = "n",
      cex = 0.78
    )
  }

  graphics::mtext(title, outer = TRUE, line = 0.2, cex = 1.0)
  TRUE
}

unified_ndlm_diag_write_forecast_quantile_panels <- function(ensemble_summary, ndlm_forecast, path, title) {
  req_e <- c("source_key", "source_label", "lead", "date", "ensemble_q05", "ensemble_q50", "ensemble_q95")
  req_n <- c("mapped_source", "lead", "date", "q05", "q50", "q95")
  if (!is.data.frame(ensemble_summary) || !all(req_e %in% names(ensemble_summary)) || nrow(ensemble_summary) < 2L) {
    return(FALSE)
  }
  if (!is.data.frame(ndlm_forecast) || !all(req_n %in% names(ndlm_forecast)) || nrow(ndlm_forecast) < 2L) {
    return(FALSE)
  }

  src_keys <- intersect(
    unique(as.character(ensemble_summary$source_key)),
    unique(as.character(ndlm_forecast$mapped_source))
  )
  src_keys <- src_keys[nzchar(src_keys)]
  if (length(src_keys) < 1L) return(FALSE)

  n_panels <- length(src_keys)
  n_col <- min(2L, max(1L, n_panels))
  n_row <- max(1L, ceiling(n_panels / n_col))
  width_px <- max(1400L, 700L * n_col)
  height_px <- max(900L, 420L * n_row)

  grDevices::png(filename = path, width = width_px, height = height_px, res = 140)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(n_row, n_col), mar = c(3.5, 4.1, 2.2, 1.1), oma = c(0.5, 0.5, 2.0, 0))

  for (src in src_keys) {
    e <- ensemble_summary[as.character(ensemble_summary$source_key) == src, , drop = FALSE]
    e <- e[order(suppressWarnings(as.integer(e$lead))), , drop = FALSE]
    n <- ndlm_forecast[as.character(ndlm_forecast$mapped_source) == src, , drop = FALSE]
    n <- n[order(suppressWarnings(as.integer(n$lead))), , drop = FALSE]

    x_date <- suppressWarnings(as.Date(e$date))
    use_date <- any(!is.na(x_date))
    x_e <- if (use_date) x_date else suppressWarnings(as.numeric(e$lead))
    x_n <- if (use_date) suppressWarnings(as.Date(n$date)) else suppressWarnings(as.numeric(n$lead))

    eq05 <- suppressWarnings(as.numeric(e$ensemble_q05))
    eq50 <- suppressWarnings(as.numeric(e$ensemble_q50))
    eq95 <- suppressWarnings(as.numeric(e$ensemble_q95))
    nq05 <- suppressWarnings(as.numeric(n$q05))
    nq50 <- suppressWarnings(as.numeric(n$q50))
    nq95 <- suppressWarnings(as.numeric(n$q95))

    y_rng <- range(c(eq05, eq50, eq95, nq05, nq50, nq95), finite = TRUE)
    if (!all(is.finite(y_rng))) {
      graphics::plot.new()
      graphics::title(main = as.character(e$source_label[[1L]]))
      next
    }
    pad <- 0.07 * max(diff(y_rng), 1e-8)
    y_lim <- c(y_rng[1] - pad, y_rng[2] + pad)

    ok_e <- is.finite(eq50) & (if (use_date) !is.na(x_e) else is.finite(x_e))
    graphics::plot(
      x_e[ok_e], eq50[ok_e],
      type = "n",
      xlab = if (use_date) "Date" else "Forecast lead",
      ylab = "log(log1p(cms))",
      ylim = y_lim,
      main = as.character(e$source_label[[1L]]),
      xaxt = if (use_date) "n" else "s"
    )
    if (use_date && sum(ok_e) >= 2L) {
      at_num <- pretty(as.numeric(x_e[ok_e]), n = 7L)
      at_date <- suppressWarnings(as.Date(at_num, origin = "1970-01-01"))
      at_date <- at_date[!is.na(at_date)]
      if (length(at_date) > 0L) graphics::axis.Date(1, at = at_date, format = "%b %d")
    }

    ok_eq <- is.finite(eq05) & is.finite(eq50) & is.finite(eq95) & (if (use_date) !is.na(x_e) else is.finite(x_e))
    if (sum(ok_eq) >= 2L) {
      graphics::lines(x_e[ok_eq], eq05[ok_eq], col = "#6B7280", lwd = 1.2, lty = 2)
      graphics::lines(x_e[ok_eq], eq50[ok_eq], col = "#374151", lwd = 2.0, lty = 1)
      graphics::lines(x_e[ok_eq], eq95[ok_eq], col = "#6B7280", lwd = 1.2, lty = 2)
    }

    ok_nq <- is.finite(nq05) & is.finite(nq50) & is.finite(nq95) & (if (use_date) !is.na(x_n) else is.finite(x_n))
    if (sum(ok_nq) >= 2L) {
      graphics::lines(x_n[ok_nq], nq05[ok_nq], col = "#F59E0B", lwd = 1.3, lty = 2)
      graphics::lines(x_n[ok_nq], nq50[ok_nq], col = "#B45309", lwd = 2.1, lty = 1)
      graphics::lines(x_n[ok_nq], nq95[ok_nq], col = "#F59E0B", lwd = 1.3, lty = 2)
    }

    x0 <- if (use_date && any(!is.na(x_e))) {
      min(x_e, na.rm = TRUE)
    } else if (!use_date && any(is.finite(x_e))) {
      min(x_e, na.rm = TRUE)
    } else {
      NA
    }
    if ((use_date && !is.na(x0)) || (!use_date && is.finite(x0))) {
      graphics::abline(v = x0, col = "#6B7280", lty = 3, lwd = 1.0)
    }
    graphics::grid(col = "#E5E7EB", lty = "dotted")
    graphics::legend(
      "topright",
      legend = c("Ensemble q05/q95", "Ensemble q50", "NDLM q05/q95", "NDLM q50"),
      col = c("#6B7280", "#374151", "#F59E0B", "#B45309"),
      lwd = c(1.2, 2.0, 1.3, 2.1),
      lty = c(2, 1, 2, 1),
      bty = "n",
      cex = 0.8
    )
  }

  graphics::mtext(title, outer = TRUE, line = 0.2, cex = 1.0)
  TRUE
}

unified_ndlm_diag_build_horizon_contract <- function(ndlm_obj, state_obj, retros_n, nws_n, glofas_n) {
  state_k <- NA_integer_
  state_k_overlap <- NA_integer_
  state_k_max <- NA_integer_
  state_k_cap <- NA_integer_
  state_nws <- NA_integer_
  state_glofas <- NA_integer_
  state_k_vec_nws <- NA_integer_
  state_k_vec_glofas <- NA_integer_
  state_seg_overlap <- NA_integer_
  state_seg_extension <- NA_integer_
  if (is.list(state_obj)) {
    state_k <- unified_ndlm_diag_int(state_obj$K)
    state_k_overlap <- unified_ndlm_diag_int(state_obj$K_overlap)
    state_k_max <- unified_ndlm_diag_int(state_obj$K_max)
    state_k_cap <- unified_ndlm_diag_int(state_obj$K_cap)
    state_nws <- unified_ndlm_diag_int(state_obj$nws_len)
    state_glofas <- unified_ndlm_diag_int(state_obj$glofas_len)
    state_k_vec_nws <- unified_ndlm_diag_named_int(state_obj$K_vec, "nws")
    state_k_vec_glofas <- unified_ndlm_diag_named_int(state_obj$K_vec, "glofas")
    state_seg_overlap <- unified_ndlm_diag_named_int(state_obj$segment_lengths, "overlap")
    state_seg_extension <- unified_ndlm_diag_named_int(state_obj$segment_lengths, "extension")
  }

  if (!is.finite(state_k_cap) || state_k_cap <= 0L) state_k_cap <- 14L
  if (!is.finite(state_nws) || state_nws <= 0L) state_nws <- nws_n
  if (!is.finite(state_glofas) || state_glofas <= 0L) state_glofas <- glofas_n

  expected_k_nws <- suppressWarnings(as.integer(min(state_nws, state_k_cap)))
  expected_k_glofas <- suppressWarnings(as.integer(min(state_glofas, state_k_cap)))
  expected_k_overlap <- suppressWarnings(as.integer(min(expected_k_nws, expected_k_glofas)))
  expected_k_max <- suppressWarnings(as.integer(max(expected_k_nws, expected_k_glofas)))
  expected_seg <- c(expected_k_overlap, max(expected_k_max - expected_k_overlap, 0L))
  standard_k <- if (is.list(ndlm_obj) && is.numeric(ndlm_obj$standard_forecast_errors)) {
    d <- dim(ndlm_obj$standard_forecast_errors)
    if (!is.null(d) && length(d) == 2L) as.integer(d[2]) else NA_integer_
  } else {
    NA_integer_
  }

  sm_k <- if (is.list(ndlm_obj) && is.list(ndlm_obj$sm_ens) && length(ndlm_obj$sm_ens) > 0L) {
    vapply(ndlm_obj$sm_ens, function(x) {
      d <- dim(x)
      if (is.null(d) || length(d) != 2L) return(NA_integer_)
      as.integer(d[2])
    }, integer(1))
  } else {
    integer(0)
  }

  sc_k <- if (is.list(ndlm_obj) && is.list(ndlm_obj$sC_ens) && length(ndlm_obj$sC_ens) > 0L) {
    vapply(ndlm_obj$sC_ens, function(x) {
      d <- dim(x)
      if (is.null(d) || length(d) != 3L) return(NA_integer_)
      as.integer(d[3])
    }, integer(1))
  } else {
    integer(0)
  }

  exps_k <- if (is.list(ndlm_obj) && is.numeric(ndlm_obj$exps)) {
    d <- dim(ndlm_obj$exps)
    if (!is.null(d) && length(d) == 2L) max(as.integer(d[2]) - as.integer(retros_n), 0L) else NA_integer_
  } else {
    NA_integer_
  }

  actual_seg <- if (length(sm_k) > 0L) sm_k else integer(0)
  actual_seg_txt <- if (length(actual_seg) == 0L) "[]" else sprintf("[%s]", paste(actual_seg, collapse = ","))
  expected_seg_txt <- sprintf("[%s]", paste(expected_seg, collapse = ","))
  sc_seg_txt <- if (length(sc_k) == 0L) "[]" else sprintf("[%s]", paste(sc_k, collapse = ","))

  rows <- list(
    data.frame(
      figure_or_series = "ndlm_total_forecast_horizon",
      expected_horizon = expected_k_max,
      actual_horizon = standard_k,
      status = if (is.finite(expected_k_max) && is.finite(standard_k) && expected_k_max == standard_k) "pass" else "mismatch",
      contract_rule = "K_max = max(min(nws_len,K_cap), min(glofas_len,K_cap))",
      notes = sprintf("state.K=%s state.K_max=%s state.K_overlap=%s K_cap=%s state.K_vec=(nws=%s,glofas=%s)", as.character(state_k), as.character(state_k_max), as.character(state_k_overlap), as.character(state_k_cap), as.character(state_k_vec_nws), as.character(state_k_vec_glofas)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      figure_or_series = "ndlm_segment_profile_sm_ens",
      expected_horizon = expected_k_overlap,
      actual_horizon = if (length(sm_k) == 0L) NA_integer_ else sum(sm_k),
      status = if (length(sm_k) > 0L && all(is.finite(sm_k)) && all(sm_k >= 0L) && identical(as.integer(sm_k), as.integer(expected_seg))) "pass" else "mismatch",
      contract_rule = "sm_ens segment lengths must match [K_overlap, K_max-K_overlap]",
      notes = sprintf("expected=%s actual=%s", expected_seg_txt, actual_seg_txt),
      stringsAsFactors = FALSE
    ),
    data.frame(
      figure_or_series = "ndlm_segment_profile_sC_ens",
      expected_horizon = expected_k_overlap,
      actual_horizon = if (length(sc_k) == 0L) NA_integer_ else sum(sc_k),
      status = if (length(sc_k) > 0L && all(is.finite(sc_k)) && all(sc_k >= 0L) && identical(as.integer(sc_k), as.integer(expected_seg))) "pass" else "mismatch",
      contract_rule = "sC_ens segment lengths must match [K_overlap, K_max-K_overlap]",
      notes = sprintf("expected=%s actual=%s", expected_seg_txt, sc_seg_txt),
      stringsAsFactors = FALSE
    ),
    data.frame(
      figure_or_series = "ndlm_segment_profile_state_consistency",
      expected_horizon = expected_k_max,
      actual_horizon = if (length(sm_k) == 0L) NA_integer_ else sum(sm_k),
      status = if (length(sm_k) > 0L && length(sc_k) > 0L && all(is.finite(sm_k)) && all(is.finite(sc_k)) && length(sm_k) == length(sc_k) && all(sm_k == sc_k) && sum(sm_k) == standard_k) "pass" else "mismatch",
      contract_rule = "sm_ens and sC_ens segment profiles must match and sum to standard_forecast_errors horizon",
      notes = sprintf("sm_ens=%s sC_ens=%s standard.K=%s", actual_seg_txt, sc_seg_txt, as.character(standard_k)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      figure_or_series = "ndlm_state_metadata_consistency",
      expected_horizon = expected_k_max,
      actual_horizon = state_k_max,
      status = if (is.finite(state_k_max) && is.finite(state_k_overlap) && is.finite(state_seg_overlap) && is.finite(state_seg_extension) &&
                    state_k_max == expected_k_max &&
                    state_k_overlap == expected_k_overlap &&
                    state_seg_overlap == state_k_overlap &&
                    (state_seg_overlap + state_seg_extension) == state_k_max &&
                    state_k_vec_nws == expected_k_nws &&
                    state_k_vec_glofas == expected_k_glofas &&
                    state_k == state_k_max) "pass" else "mismatch",
      contract_rule = "state metadata (K_vec/K_overlap/K_max/segment_lengths) must match expected ragged horizon",
      notes = sprintf("state.seg=[%s,%s] state.K=%s expected.Kmax=%s expected.Koverlap=%s", as.character(state_seg_overlap), as.character(state_seg_extension), as.character(state_k), as.character(expected_k_max), as.character(expected_k_overlap)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      figure_or_series = "ndlm_exps_forecast_extension",
      expected_horizon = 0L,
      actual_horizon = exps_k,
      status = if (is.finite(exps_k) && exps_k == 0L) "pass" else "mismatch",
      contract_rule = "Theory-aligned NDLM stores retrospective exps over T only; forecast component is represented via sm_ens/sC_ens",
      notes = sprintf("retros_n=%d", as.integer(retros_n)),
      stringsAsFactors = FALSE
    )
  )

  do.call(rbind, rows)
}

unified_generate_ndlm_post_diagnostics <- function(
  run_root,
  ndlm_rdata_path,
  retros_csv_path,
  nws_csv_path,
  glofas_csv_path,
  fit_log_path = "",
  output_dir = NULL,
  usgs_site = "11160500",
  forecast_start_date = NULL,
  forecast_end_date = NULL,
  strict_contract = FALSE,
  state_ci_max_draws = NULL
) {
  if (is.null(output_dir) || !nzchar(output_dir)) {
    output_dir <- file.path(run_root, "diagnostics", "ndlm")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  retros_df <- unified_ndlm_diag_read_csv(retros_csv_path, "retros")
  nws_df <- unified_ndlm_diag_read_csv(nws_csv_path, "nws_forecast")
  glofas_df <- unified_ndlm_diag_read_csv(glofas_csv_path, "glofas_forecast")

  env <- new.env(parent = emptyenv())
  load(ndlm_rdata_path, envir = env)
  if (!exists("new.theta.out_50_NDLM_synth_DISC", envir = env, inherits = FALSE)) {
    stop("[NDLM_DIAG_OBJECT_MISSING] Missing new.theta.out_50_NDLM_synth_DISC in NDLM bundle.", call. = FALSE)
  }

  ndlm_obj <- get("new.theta.out_50_NDLM_synth_DISC", envir = env, inherits = FALSE)
  state_obj <- if (exists("ndlm_main_theory_state", envir = env, inherits = FALSE)) {
    get("ndlm_main_theory_state", envir = env, inherits = FALSE)
  } else {
    NULL
  }
  covariance_diagnostics <- if (
    is.list(state_obj) &&
      is.data.frame(state_obj$covariance_diagnostics) &&
      nrow(state_obj$covariance_diagnostics) > 0L
  ) {
    state_obj$covariance_diagnostics
  } else {
    do.call(rbind, list(
      unified_ndlm_diag_cov_row("smooth_cov", ndlm_obj$sC),
      if (is.list(ndlm_obj$sC_ens) && length(ndlm_obj$sC_ens) >= 1L) unified_ndlm_diag_cov_row("forecast_cov_segment_1", ndlm_obj$sC_ens[[1L]]) else NULL,
      if (is.list(ndlm_obj$sC_ens) && length(ndlm_obj$sC_ens) >= 2L) unified_ndlm_diag_cov_row("forecast_cov_segment_2", ndlm_obj$sC_ens[[2L]]) else NULL
    ))
  }

  iter_trace <- unified_ndlm_diag_parse_progress_log(fit_log_path)
  if (nrow(iter_trace) == 0L && exists("seq.elbo_50_NDLM_synth_DISC", envir = env, inherits = FALSE)) {
    elbo <- as.numeric(get("seq.elbo_50_NDLM_synth_DISC", envir = env, inherits = FALSE))
    sigma_usgs <- rep(NA_real_, length(elbo))
    sigma_nws <- rep(NA_real_, length(elbo))
    sigma_glofas <- rep(NA_real_, length(elbo))
    if (exists("seq.sigma_50_NDLM_synth_DISC", envir = env, inherits = FALSE)) {
      sigma_obj <- get("seq.sigma_50_NDLM_synth_DISC", envir = env, inherits = FALSE)
      sigma_mat <- NULL
      if (is.numeric(sigma_obj)) {
        if (is.null(dim(sigma_obj))) {
          sigma_mat <- matrix(as.numeric(sigma_obj), ncol = 1L)
        } else if (length(dim(sigma_obj)) == 2L) {
          sigma_mat <- as.matrix(sigma_obj)
        }
      }
      if (!is.null(sigma_mat)) {
        nr <- nrow(sigma_mat)
        nc <- ncol(sigma_mat)
        if (nr != length(elbo) && nc == length(elbo)) {
          sigma_mat <- t(sigma_mat)
          nr <- nrow(sigma_mat)
          nc <- ncol(sigma_mat)
        }
        if (nr < length(elbo) && nr > 0L) {
          pad <- matrix(NA_real_, nrow = length(elbo) - nr, ncol = nc)
          sigma_mat <- rbind(sigma_mat, pad)
          nr <- nrow(sigma_mat)
        }
        if (nr > length(elbo)) {
          sigma_mat <- sigma_mat[seq_len(length(elbo)), , drop = FALSE]
          nr <- nrow(sigma_mat)
        }
        nm_norm <- if (!is.null(colnames(sigma_mat))) gsub("[^a-z0-9]+", "", tolower(colnames(sigma_mat))) else rep("", ncol(sigma_mat))
        pick_col <- function(keys, fallback_idx) {
          for (k in keys) {
            hit <- which(nm_norm == k)
            if (length(hit) > 0L) return(hit[[1L]])
          }
          if (is.finite(fallback_idx) && fallback_idx >= 1L && fallback_idx <= ncol(sigma_mat)) {
            return(as.integer(fallback_idx))
          }
          NA_integer_
        }
        idx_usgs <- pick_col(c("sigmausgsexp", "sigmausgs", "usgs"), 1L)
        idx_nws <- pick_col(c("sigmanwsexp", "sigmanws", "nws"), if (ncol(sigma_mat) >= 2L) 2L else NA_integer_)
        idx_glofas <- pick_col(c("sigmaglofasexp", "sigmaglofas", "glofas"), if (ncol(sigma_mat) >= 3L) 3L else NA_integer_)
        if (is.finite(idx_usgs)) sigma_usgs <- as.numeric(sigma_mat[, idx_usgs])
        if (is.finite(idx_nws)) sigma_nws <- as.numeric(sigma_mat[, idx_nws])
        if (is.finite(idx_glofas)) sigma_glofas <- as.numeric(sigma_mat[, idx_glofas])
      }
    }
    sigma <- sigma_usgs
    iter_trace <- data.frame(
      iter = seq_along(elbo),
      elbo = elbo,
      crit_elbo = c(NA_real_, abs(diff(elbo))),
      sigma_exp = sigma,
      sigma_usgs_exp = sigma_usgs,
      sigma_nws_exp = sigma_nws,
      sigma_glofas_exp = sigma_glofas,
      gamma_exp = NA_real_,
      state_norm_sq = NA_real_,
      w_hist = NA_real_,
      w_fore = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  retros_dates <- unified_ndlm_diag_extract_date_column(retros_df)
  nws_dates <- unified_ndlm_diag_extract_date_column(nws_df)
  glofas_dates <- unified_ndlm_diag_extract_date_column(glofas_df)

  exps <- ndlm_obj$exps
  sfe <- ndlm_obj$standard_forecast_errors
  sm_ens <- ndlm_obj$sm_ens

  time_rows <- list(
    {
      span <- unified_ndlm_diag_date_span(retros_dates)
      data.frame(source_series = "retros", t_min = span[["t_min"]], t_max = span[["t_max"]], n_points = as.integer(nrow(retros_df)), missing_count = as.integer(sum(!is.finite(unified_ndlm_diag_pick_numeric_column(retros_df, preferred = c("USGS", "y", "obs", "flow", "value")))), na.rm = TRUE), stringsAsFactors = FALSE)
    },
    {
      span <- unified_ndlm_diag_date_span(nws_dates)
      data.frame(source_series = "nws_forecast", t_min = span[["t_min"]], t_max = span[["t_max"]], n_points = as.integer(nrow(nws_df)), missing_count = 0L, stringsAsFactors = FALSE)
    },
    {
      span <- unified_ndlm_diag_date_span(glofas_dates)
      data.frame(source_series = "glofas_forecast", t_min = span[["t_min"]], t_max = span[["t_max"]], n_points = as.integer(nrow(glofas_df)), missing_count = 0L, stringsAsFactors = FALSE)
    },
    {
      exps_dim <- dim(exps)
      n_exps <- if (!is.null(exps_dim) && length(exps_dim) == 2L) as.integer(exps_dim[2]) else 0L
      data.frame(source_series = "ndlm_exps", t_min = if (n_exps > 0L) "1" else "", t_max = as.character(n_exps), n_points = n_exps, missing_count = as.integer(if (is.numeric(exps)) sum(!is.finite(exps)) else NA_integer_), stringsAsFactors = FALSE)
    },
    {
      sfe_dim <- dim(sfe)
      n_sfe <- if (!is.null(sfe_dim) && length(sfe_dim) == 2L) as.integer(sfe_dim[2]) else 0L
      data.frame(source_series = "ndlm_standard_forecast_errors", t_min = if (n_sfe > 0L) "1" else "", t_max = as.character(n_sfe), n_points = n_sfe, missing_count = as.integer(if (is.numeric(sfe)) sum(!is.finite(sfe)) else NA_integer_), stringsAsFactors = FALSE)
    }
  )

  if (is.list(sm_ens) && length(sm_ens) > 0L) {
    for (i in seq_along(sm_ens)) {
      d <- dim(sm_ens[[i]])
      n_seg <- if (!is.null(d) && length(d) == 2L) as.integer(d[2]) else 0L
      time_rows[[length(time_rows) + 1L]] <- data.frame(
        source_series = sprintf("ndlm_sm_ens_seg_%d", as.integer(i)),
        t_min = if (n_seg > 0L) "1" else "",
        t_max = as.character(n_seg),
        n_points = n_seg,
        missing_count = as.integer(if (is.numeric(sm_ens[[i]])) sum(!is.finite(sm_ens[[i]])) else NA_integer_),
        stringsAsFactors = FALSE
      )
    }
  }

  time_coverage <- do.call(rbind, time_rows)

  shape_rows <- do.call(rbind, list(
    unified_ndlm_diag_shape_row("new.theta.out_50_NDLM_synth_DISC$sm", ndlm_obj$sm),
    unified_ndlm_diag_shape_row("new.theta.out_50_NDLM_synth_DISC$sC", ndlm_obj$sC),
    unified_ndlm_diag_shape_row("new.theta.out_50_NDLM_synth_DISC$exps", ndlm_obj$exps),
    unified_ndlm_diag_shape_row("new.theta.out_50_NDLM_synth_DISC$standard_forecast_errors", ndlm_obj$standard_forecast_errors),
    unified_ndlm_diag_shape_row("new.theta.out_50_NDLM_synth_DISC$sm_ens", ndlm_obj$sm_ens),
    unified_ndlm_diag_shape_row("new.theta.out_50_NDLM_synth_DISC$sC_ens", ndlm_obj$sC_ens),
    if (!is.null(state_obj)) unified_ndlm_diag_shape_row("ndlm_main_theory_state", state_obj) else NULL
  ))

  horizon_contract <- unified_ndlm_diag_build_horizon_contract(
    ndlm_obj = ndlm_obj,
    state_obj = state_obj,
    retros_n = as.integer(nrow(retros_df)),
    nws_n = as.integer(nrow(nws_df)),
    glofas_n = as.integer(nrow(glofas_df))
  )

  derive_active_set <- function() {
    if (is.list(state_obj) && is.data.frame(state_obj$active_set_by_lead)) {
      out <- state_obj$active_set_by_lead
      req <- c("lead", "active_nws", "active_glofas", "active_count")
      if (all(req %in% names(out))) {
        return(out[, req, drop = FALSE])
      }
    }
    cap <- if (is.list(state_obj)) unified_ndlm_diag_int(state_obj$K_cap) else NA_integer_
    if (!is.finite(cap) || cap <= 0L) cap <- 14L
    k_nws <- min(as.integer(nrow(nws_df)), cap)
    k_glofas <- min(as.integer(nrow(glofas_df)), cap)
    k_max <- max(k_nws, k_glofas)
    data.frame(
      lead = seq_len(k_max),
      active_nws = as.integer(seq_len(k_max) <= k_nws),
      active_glofas = as.integer(seq_len(k_max) <= k_glofas),
      active_count = as.integer((seq_len(k_max) <= k_nws) + (seq_len(k_max) <= k_glofas)),
      stringsAsFactors = FALSE
    )
  }
  active_set_by_lead <- derive_active_set()

  state_dim_by_lead <- if (is.list(state_obj) && is.data.frame(state_obj$state_dim_by_lead) &&
                           all(c("lead", "state_dim") %in% names(state_obj$state_dim_by_lead))) {
    state_obj$state_dim_by_lead[, c("lead", "state_dim"), drop = FALSE]
  } else {
    data.frame(
      lead = active_set_by_lead$lead,
      state_dim = as.integer(7L * active_set_by_lead$active_count),
      stringsAsFactors = FALSE
    )
  }

  parse_seg_profile <- function(x) {
    out <- gsub("^\\[|\\]$", "", as.character(x))
    if (!nzchar(out)) return(integer(0))
    vals <- strsplit(out, ",", fixed = TRUE)[[1L]]
    suppressWarnings(as.integer(trimws(vals)))
  }
  row_sm <- horizon_contract[horizon_contract$figure_or_series == "ndlm_segment_profile_sm_ens", , drop = FALSE]
  row_total <- horizon_contract[horizon_contract$figure_or_series == "ndlm_total_forecast_horizon", , drop = FALSE]
  sm_profile <- if (nrow(row_sm) == 1L) parse_seg_profile(sub(".*actual=\\[([^]]*)\\].*", "[\\1]", row_sm$notes[[1L]])) else integer(0)
  ragged_coverage_summary <- data.frame(
    metric = c(
      "k_nws_effective", "k_glofas_effective", "k_overlap", "k_max",
      "segment_overlap", "segment_extension", "segment_sum", "standard_forecast_errors_k", "contract_status"
    ),
    value = c(
      as.character(sum(active_set_by_lead$active_nws)),
      as.character(sum(active_set_by_lead$active_glofas)),
      as.character(sum(active_set_by_lead$active_count == 2L)),
      as.character(nrow(active_set_by_lead)),
      as.character(if (length(sm_profile) >= 1L) sm_profile[[1L]] else NA_integer_),
      as.character(if (length(sm_profile) >= 2L) sm_profile[[2L]] else NA_integer_),
      as.character(if (length(sm_profile) > 0L) sum(sm_profile, na.rm = TRUE) else NA_integer_),
      as.character(if (nrow(row_total) == 1L) row_total$actual_horizon[[1L]] else NA_integer_),
      if (all(horizon_contract$status == "pass")) "pass" else "mismatch"
    ),
    stringsAsFactors = FALSE
  )

  obs_series_log1p <- unified_ndlm_diag_pick_numeric_column(retros_df, preferred = c("USGS", "y", "obs", "flow", "value"))
  obs_series <- unified_ndlm_diag_loglog1p_from_log1p(obs_series_log1p)
  smooth_series <- if (is.numeric(exps) && !is.null(dim(exps)) && length(dim(exps)) == 2L && dim(exps)[1] >= 2L) {
    as.numeric(exps[2, ])
  } else {
    numeric(0)
  }

  fit_diag_state <- if (is.list(state_obj) && is.list(state_obj$fit_diagnostics)) state_obj$fit_diagnostics else NULL
  get_diag_vec <- function(name, fallback, n_target) {
    val <- if (!is.null(fit_diag_state)) fit_diag_state[[name]] else NULL
    out <- suppressWarnings(as.numeric(val))
    if (length(out) != n_target) out <- fallback
    if (length(out) != n_target) out <- rep(NA_real_, n_target)
    out
  }

  n_overlap <- max(
    min(length(obs_series), length(smooth_series)),
    if (!is.null(fit_diag_state)) suppressWarnings(as.integer(length(fit_diag_state$y_observed))) else 0L
  )
  if (!is.finite(n_overlap) || n_overlap < 0L) n_overlap <- 0L
  if (n_overlap > 0L) {
    n_overlap <- min(
      n_overlap,
      length(obs_series),
      max(length(smooth_series), if (!is.null(fit_diag_state)) suppressWarnings(as.integer(length(fit_diag_state$y_smoothed))) else 0L)
    )
  }

  obs_use <- if (n_overlap > 0L) as.numeric(obs_series[seq_len(n_overlap)]) else numeric(0)
  smooth_use <- if (n_overlap > 0L) get_diag_vec("y_smoothed", smooth_series[seq_len(min(length(smooth_series), n_overlap))], n_overlap) else numeric(0)
  pred_use <- if (n_overlap > 0L) get_diag_vec("y_predicted_one_step", rep(NA_real_, n_overlap), n_overlap) else numeric(0)
  filt_use <- if (n_overlap > 0L) get_diag_vec("y_filtered", rep(NA_real_, n_overlap), n_overlap) else numeric(0)
  date_use <- if (n_overlap > 0L && length(retros_dates) >= n_overlap) retros_dates[seq_len(n_overlap)] else as.Date(rep(NA_character_, n_overlap))

  mode_series <- data.frame(
    date = date_use,
    observed = obs_use,
    one_step_predicted = pred_use,
    filtered_fit = filt_use,
    smoothed_fit = smooth_use,
    residual_one_step = if (n_overlap > 0L) pred_use - obs_use else numeric(0),
    residual_filtered = if (n_overlap > 0L) filt_use - obs_use else numeric(0),
    residual_smoothed = if (n_overlap > 0L) smooth_use - obs_use else numeric(0),
    stringsAsFactors = FALSE
  )

  summarize_mode <- function(mode_name, fitted_vals, observed_vals) {
    fitted_vals <- suppressWarnings(as.numeric(fitted_vals))
    observed_vals <- suppressWarnings(as.numeric(observed_vals))
    n <- min(length(fitted_vals), length(observed_vals))
    if (n <= 0L) {
      return(data.frame(
        mode = mode_name, n_points = 0L, finite_points = 0L, coverage_rate = NA_real_,
        rmse = NA_real_, mae = NA_real_, corr = NA_real_, mean_residual = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    f <- fitted_vals[seq_len(n)]
    o <- observed_vals[seq_len(n)]
    ok <- is.finite(f) & is.finite(o)
    err <- f - o
    data.frame(
      mode = mode_name,
      n_points = as.integer(n),
      finite_points = as.integer(sum(ok)),
      coverage_rate = if (n > 0L) as.numeric(sum(ok) / n) else NA_real_,
      rmse = if (any(ok)) sqrt(mean(err[ok]^2)) else NA_real_,
      mae = if (any(ok)) mean(abs(err[ok])) else NA_real_,
      corr = if (sum(ok) >= 2L) suppressWarnings(stats::cor(f[ok], o[ok])) else NA_real_,
      mean_residual = if (any(ok)) mean(err[ok]) else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  mode_coverage <- do.call(rbind, list(
    summarize_mode("one_step_predicted", mode_series$one_step_predicted, mode_series$observed),
    summarize_mode("filtered_fit", mode_series$filtered_fit, mode_series$observed),
    summarize_mode("smoothed_fit", mode_series$smoothed_fit, mode_series$observed)
  ))
  row_sm <- mode_coverage[mode_coverage$mode == "smoothed_fit", , drop = FALSE]
  fit_summary <- data.frame(
    metric = c(
      "retros_points", "exps_points", "overlap_points", "finite_overlap_points",
      "coverage_rate", "rmse", "mae", "corr"
    ),
    value = c(
      as.numeric(length(obs_series)),
      as.numeric(length(smooth_series)),
      as.numeric(n_overlap),
      if (nrow(row_sm) == 1L) as.numeric(row_sm$finite_points[[1L]]) else NA_real_,
      if (nrow(row_sm) == 1L) as.numeric(row_sm$coverage_rate[[1L]]) else NA_real_,
      if (nrow(row_sm) == 1L) as.numeric(row_sm$rmse[[1L]]) else NA_real_,
      if (nrow(row_sm) == 1L) as.numeric(row_sm$mae[[1L]]) else NA_real_,
      if (nrow(row_sm) == 1L) as.numeric(row_sm$corr[[1L]]) else NA_real_
    ),
    stringsAsFactors = FALSE
  )

  fit_series <- data.frame(
    date = mode_series$date,
    observed = mode_series$observed,
    ndlm_fit = mode_series$smoothed_fit,
    residual = mode_series$residual_smoothed,
    stringsAsFactors = FALSE
  )

  mu_obs_long <- unified_ndlm_diag_build_mu_obs_long(
    exps = exps,
    retros_df = retros_df,
    retros_dates = retros_dates
  )

  sigma_draws <- if (exists("samp.sigma_50_NDLM_synth_DISC", envir = env, inherits = FALSE)) {
    get("samp.sigma_50_NDLM_synth_DISC", envir = env, inherits = FALSE)
  } else {
    NULL
  }
  forecast_bundle <- unified_ndlm_diag_build_forecast_bundle(
    exps = exps,
    retros_df = retros_df,
    nws_df = nws_df,
    glofas_df = glofas_df,
    sigma_draws = sigma_draws
  )
  ensemble_summary <- forecast_bundle$ensemble_summary
  ensemble_members <- forecast_bundle$ensemble_members
  ndlm_forecast <- forecast_bundle$ndlm_forecast
  row_source_map <- forecast_bundle$row_source_map

  infer_min_date <- function(x) {
    dx <- suppressWarnings(as.Date(x))
    if (length(dx) < 1L || all(is.na(dx))) return(as.Date(NA_character_))
    min(dx, na.rm = TRUE)
  }
  infer_max_date <- function(x) {
    dx <- suppressWarnings(as.Date(x))
    if (length(dx) < 1L || all(is.na(dx))) return(as.Date(NA_character_))
    max(dx, na.rm = TRUE)
  }

  as_date_or_na <- function(x) {
    d <- suppressWarnings(as.Date(x))
    if (length(d) < 1L) return(as.Date(NA_character_))
    d[[1L]]
  }

  usgs_start <- as_date_or_na(forecast_start_date)
  usgs_end <- as_date_or_na(forecast_end_date)
  if (is.na(usgs_start)) {
    if (is.data.frame(ensemble_summary) && nrow(ensemble_summary) > 0L && "date" %in% names(ensemble_summary)) {
      usgs_start <- infer_min_date(ensemble_summary$date)
    } else {
      usgs_start <- infer_min_date(c(nws_dates, glofas_dates))
    }
  }
  if (is.na(usgs_end)) {
    if (is.data.frame(ensemble_summary) && nrow(ensemble_summary) > 0L && "date" %in% names(ensemble_summary)) {
      usgs_end <- infer_max_date(ensemble_summary$date)
    } else {
      usgs_end <- infer_max_date(c(nws_dates, glofas_dates))
    }
  }
  usgs_future_obs <- unified_ndlm_diag_fetch_future_usgs(
    usgs_site = usgs_site,
    start_date = usgs_start,
    end_date = usgs_end
  )

  sigma_long <- unified_ndlm_diag_extract_sigma_long(iter_trace = iter_trace, env = env)

  theta_draws <- unified_ndlm_diag_extract_theta_draws(env, max_draws = state_ci_max_draws)
  state_ci <- NULL
  if (!is.null(theta_draws)) {
    state_ci <- unified_ndlm_diag_summarize_state_draws(theta_draws = theta_draws, dates = retros_dates)
  }

  horizon_note <- c(
    "# NDLM Horizon Contract",
    "",
    "Theory alignment:",
    "1. NDLM Model C uses ragged forecast horizons with active set A_k = {j: k <= K_j}.",
    "2. In this implementation, K_j = min(source_len_j, K_cap), K_overlap=min(K_j), K_max=max(K_j).",
    "3. `exps` is retrospective-only (`T` columns). Forecast discrepancy dynamics are represented by segmented `sm_ens/sC_ens` and `standard_forecast_errors` over K_max.",
    "",
    sprintf("Observed lengths: retros=%d, nws=%d, glofas=%d", as.integer(nrow(retros_df)), as.integer(nrow(nws_df)), as.integer(nrow(glofas_df))),
    sprintf("Contract result: %s", if (all(horizon_contract$status == "pass")) "pass" else "mismatch")
  )

  paths <- list(
    ndlm_iter_trace = file.path(output_dir, "ndlm_iter_trace.csv"),
    ndlm_time_coverage = file.path(output_dir, "ndlm_time_coverage.csv"),
    active_set_by_lead = file.path(output_dir, "active_set_by_lead.csv"),
    state_dim_by_lead = file.path(output_dir, "state_dim_by_lead.csv"),
    horizon_contract_check = file.path(output_dir, "horizon_contract_check.csv"),
    ndlm_plot_contract_check = file.path(output_dir, "ndlm_plot_contract_check.csv"),
    ndlm_object_shapes = file.path(output_dir, "ndlm_object_shapes.csv"),
    ndlm_fit_vs_observed_coverage = file.path(output_dir, "ndlm_fit_vs_observed_coverage.csv"),
    ndlm_fit_series = file.path(output_dir, "ndlm_fit_series.csv"),
    ndlm_fit_modes_coverage = file.path(output_dir, "ndlm_fit_modes_coverage.csv"),
    ndlm_fit_modes_series = file.path(output_dir, "ndlm_fit_modes_series.csv"),
    ndlm_mu_vs_observed_long = file.path(output_dir, "ndlm_mu_vs_observed_long.csv"),
    ndlm_mu_vs_observed_all_sources = file.path(output_dir, "ndlm_mu_vs_observed_all_sources.png"),
    ndlm_mu_vs_observed_historical_sources = file.path(output_dir, "ndlm_mu_vs_observed_historical_sources.png"),
    ndlm_forecast_ensemble_summary = file.path(output_dir, "ndlm_forecast_ensemble_summary.csv"),
    ndlm_forecast_ensemble_members_long = file.path(output_dir, "ndlm_forecast_ensemble_members_long.csv"),
    ndlm_forecast_mu_quantiles_by_source = file.path(output_dir, "ndlm_forecast_mu_quantiles_by_source.csv"),
    ndlm_forecast_row_source_map = file.path(output_dir, "ndlm_forecast_row_source_map.csv"),
    ndlm_forecast_usgs_future_observed = file.path(output_dir, "ndlm_forecast_usgs_future_observed.csv"),
    ndlm_sigma_trace_long = file.path(output_dir, "ndlm_sigma_trace_long.csv"),
    ndlm_state_components_ci_summary = file.path(output_dir, "ndlm_state_components_ci_summary.csv"),
    ndlm_state_components_ci_coverage = file.path(output_dir, "ndlm_state_components_ci_coverage.csv"),
    ndlm_covariance_diagnostics = file.path(output_dir, "ndlm_covariance_diagnostics.csv"),
    ragged_coverage_summary = file.path(output_dir, "ragged_coverage_summary.csv"),
    ndlm_horizon_contract = file.path(output_dir, "ndlm_horizon_contract.md"),
    ndlm_elbo_trace = file.path(output_dir, "ndlm_elbo_trace.png"),
    ndlm_sigma_trace = file.path(output_dir, "ndlm_sigma_trace.png"),
    ndlm_state_norm_trace = file.path(output_dir, "ndlm_state_norm_trace.png"),
    ndlm_dynamic_fit_full = file.path(output_dir, "ndlm_dynamic_fit_full.png"),
    ndlm_dynamic_fit_modes_full = file.path(output_dir, "ndlm_dynamic_fit_modes_full.png"),
    ndlm_state_components_ci_all = file.path(output_dir, "ndlm_state_components_ci_all.png"),
    ndlm_state_components_ci_hist = file.path(output_dir, "ndlm_state_components_ci_hist.png"),
    ndlm_state_components_ci_discrep = file.path(output_dir, "ndlm_state_components_ci_discrep.png"),
    ndlm_state_components_ci_transfer = file.path(output_dir, "ndlm_state_components_ci_transfer.png"),
    ndlm_forecast_window_ndlm_vs_ensembles = file.path(output_dir, "ndlm_forecast_window_ndlm_vs_ensembles.png"),
    ndlm_forecast_window_ensemble_members = file.path(output_dir, "ndlm_forecast_window_ensemble_members.png"),
    ndlm_forecast_window_quantiles = file.path(output_dir, "ndlm_forecast_window_quantiles.png"),
    ndlm_dynamic_fit_2012_2016 = file.path(output_dir, "ndlm_dynamic_fit_2012_2016.png"),
    ndlm_dynamic_fit_2017_2019 = file.path(output_dir, "ndlm_dynamic_fit_2017_2019.png"),
    ndlm_dynamic_fit_2018_2020 = file.path(output_dir, "ndlm_dynamic_fit_2018_2020.png")
  )

  utils::write.csv(iter_trace, paths$ndlm_iter_trace, row.names = FALSE)
  utils::write.csv(time_coverage, paths$ndlm_time_coverage, row.names = FALSE)
  utils::write.csv(active_set_by_lead, paths$active_set_by_lead, row.names = FALSE)
  utils::write.csv(state_dim_by_lead, paths$state_dim_by_lead, row.names = FALSE)
  utils::write.csv(horizon_contract, paths$horizon_contract_check, row.names = FALSE)
  utils::write.csv(horizon_contract, paths$ndlm_plot_contract_check, row.names = FALSE)
  utils::write.csv(shape_rows, paths$ndlm_object_shapes, row.names = FALSE)
  utils::write.csv(fit_summary, paths$ndlm_fit_vs_observed_coverage, row.names = FALSE)
  utils::write.csv(fit_series, paths$ndlm_fit_series, row.names = FALSE)
  utils::write.csv(mode_coverage, paths$ndlm_fit_modes_coverage, row.names = FALSE)
  utils::write.csv(mode_series, paths$ndlm_fit_modes_series, row.names = FALSE)
  if (is.data.frame(mu_obs_long) && nrow(mu_obs_long) > 0L) {
    utils::write.csv(mu_obs_long, paths$ndlm_mu_vs_observed_long, row.names = FALSE)
  }
  if (is.data.frame(ensemble_summary) && nrow(ensemble_summary) > 0L) {
    utils::write.csv(ensemble_summary, paths$ndlm_forecast_ensemble_summary, row.names = FALSE)
  }
  if (is.data.frame(ensemble_members) && nrow(ensemble_members) > 0L) {
    utils::write.csv(ensemble_members, paths$ndlm_forecast_ensemble_members_long, row.names = FALSE)
  }
  if (is.data.frame(ndlm_forecast) && nrow(ndlm_forecast) > 0L) {
    utils::write.csv(ndlm_forecast, paths$ndlm_forecast_mu_quantiles_by_source, row.names = FALSE)
  }
  if (is.data.frame(row_source_map) && nrow(row_source_map) > 0L) {
    utils::write.csv(row_source_map, paths$ndlm_forecast_row_source_map, row.names = FALSE)
  }
  if (is.data.frame(usgs_future_obs) && nrow(usgs_future_obs) > 0L) {
    utils::write.csv(usgs_future_obs, paths$ndlm_forecast_usgs_future_observed, row.names = FALSE)
  }
  if (is.data.frame(sigma_long) && nrow(sigma_long) > 0L) {
    utils::write.csv(sigma_long, paths$ndlm_sigma_trace_long, row.names = FALSE)
  }
  if (!is.null(state_ci) && is.list(state_ci) && is.data.frame(state_ci$summary) && nrow(state_ci$summary) > 0L) {
    utils::write.csv(state_ci$summary, paths$ndlm_state_components_ci_summary, row.names = FALSE)
    utils::write.csv(state_ci$coverage, paths$ndlm_state_components_ci_coverage, row.names = FALSE)
  }
  utils::write.csv(covariance_diagnostics, paths$ndlm_covariance_diagnostics, row.names = FALSE)
  utils::write.csv(ragged_coverage_summary, paths$ragged_coverage_summary, row.names = FALSE)
  writeLines(horizon_note, con = paths$ndlm_horizon_contract)

  invisible(unified_ndlm_diag_write_trace_plot(
    df = iter_trace,
    x_col = "iter",
    y_col = "elbo",
    path = paths$ndlm_elbo_trace,
    main = "NDLM ELBO Trace",
    ylab = "ELBO"
  ))
  sigma_trace_paths <- unified_ndlm_diag_write_sigma_traces(
    sigma_long = sigma_long,
    output_dir = output_dir,
    primary_path = paths$ndlm_sigma_trace
  )
  if (length(sigma_trace_paths) == 0L) {
    invisible(unified_ndlm_diag_write_trace_plot(
      df = iter_trace,
      x_col = "iter",
      y_col = "sigma_exp",
      path = paths$ndlm_sigma_trace,
      main = "NDLM Sigma Trace",
      ylab = "Sigma"
    ))
  }
  invisible(unified_ndlm_diag_write_trace_plot(
    df = iter_trace,
    x_col = "iter",
    y_col = "state_norm_sq",
    path = paths$ndlm_state_norm_trace,
    main = "NDLM State-Norm Trace",
    ylab = "State Norm Sq"
  ))

  if (is.data.frame(mu_obs_long) && nrow(mu_obs_long) > 1L) {
    invisible(unified_ndlm_diag_write_mu_obs_panels(
      mu_obs_df = mu_obs_long,
      path = paths$ndlm_mu_vs_observed_all_sources,
      title = "NDLM Expected Location (all mu_t rows) vs Observed (full span)",
      historical_only = FALSE
    ))
    invisible(unified_ndlm_diag_write_mu_obs_panels(
      mu_obs_df = mu_obs_long,
      path = paths$ndlm_mu_vs_observed_historical_sources,
      title = "NDLM Expected Location (all mu_t rows) vs Observed (historical only)",
      historical_only = TRUE
    ))
  }

  if (is.data.frame(ensemble_summary) && nrow(ensemble_summary) > 1L &&
      is.data.frame(ndlm_forecast) && nrow(ndlm_forecast) > 1L) {
    invisible(unified_ndlm_diag_write_forecast_overlay_panels(
      ensemble_summary = ensemble_summary,
      ndlm_forecast = ndlm_forecast,
      usgs_future_obs = usgs_future_obs,
      path = paths$ndlm_forecast_window_ndlm_vs_ensembles,
      title = "NDLM Forecast Window: NDLM dynamic location vs forecast ensembles"
    ))
    invisible(unified_ndlm_diag_write_forecast_quantile_panels(
      ensemble_summary = ensemble_summary,
      ndlm_forecast = ndlm_forecast,
      path = paths$ndlm_forecast_window_quantiles,
      title = "NDLM Forecast Window: implied quantile shifts (q05/q50/q95) vs ensemble quantiles"
    ))
  }

  if (is.data.frame(ensemble_members) && nrow(ensemble_members) > 1L &&
      is.data.frame(ndlm_forecast) && nrow(ndlm_forecast) > 1L) {
    invisible(unified_ndlm_diag_write_forecast_member_panels(
      ensemble_members = ensemble_members,
      ndlm_forecast = ndlm_forecast,
      usgs_future_obs = usgs_future_obs,
      path = paths$ndlm_forecast_window_ensemble_members,
      title = "NDLM Forecast Window: ensemble members with NDLM overlay"
    ))
  }

  if (n_overlap > 1L) {
    invisible(unified_ndlm_diag_write_fit_modes_plot(
      df = mode_series,
      path = paths$ndlm_dynamic_fit_modes_full,
      title = "NDLM Fit Comparison: One-step vs Filtered vs Smoothed"
    ))

    # Full retrospective fit view.
    invisible(unified_ndlm_diag_write_fit_plot(
      dates = fit_series$date,
      obs = fit_series$observed,
      fit = fit_series$ndlm_fit,
      path = paths$ndlm_dynamic_fit_full,
      title = "NDLM Dynamic Location Fit vs Observed (Full Retrospective)",
      x_as_date = any(!is.na(fit_series$date))
    ))

    # Standard post windows for quick parity checks.
    win_specs <- list(
      list(path = paths$ndlm_dynamic_fit_2012_2016, start = as.Date("2012-01-01"), end = as.Date("2016-12-31"), label = "2012-2016"),
      list(path = paths$ndlm_dynamic_fit_2017_2019, start = as.Date("2017-01-01"), end = as.Date("2019-12-31"), label = "2017-2019"),
      list(path = paths$ndlm_dynamic_fit_2018_2020, start = as.Date("2018-01-01"), end = as.Date("2020-12-31"), label = "2018-2020")
    )
    for (spec in win_specs) {
      idx <- if (any(!is.na(fit_series$date))) {
        which(!is.na(fit_series$date) & fit_series$date >= spec$start & fit_series$date <= spec$end)
      } else {
        integer(0)
      }
      if (length(idx) < 2L) next
      invisible(unified_ndlm_diag_write_fit_plot(
        dates = fit_series$date[idx],
        obs = fit_series$observed[idx],
        fit = fit_series$ndlm_fit[idx],
        path = spec$path,
        title = sprintf("NDLM Dynamic Location Fit vs Observed (%s)", spec$label),
        x_as_date = TRUE
      ))
    }
  }

  if (!is.null(state_ci) && is.list(state_ci) && is.data.frame(state_ci$summary) && nrow(state_ci$summary) > 0L) {
    invisible(unified_ndlm_diag_write_state_components_ci_plot(
      summary_df = state_ci$summary,
      path = paths$ndlm_state_components_ci_all,
      title = "NDLM State Components (Posterior Median + 95% Credible Interval)"
    ))
    invisible(unified_ndlm_diag_write_state_components_ci_plot(
      summary_df = state_ci$summary,
      path = paths$ndlm_state_components_ci_hist,
      title = "NDLM Historical Block States (1-7): Median + 95% CI",
      component_ids = 1:7
    ))
    invisible(unified_ndlm_diag_write_state_components_ci_plot(
      summary_df = state_ci$summary,
      path = paths$ndlm_state_components_ci_discrep,
      title = "NDLM Discrepancy Block States (8-14): Median + 95% CI",
      component_ids = 8:14
    ))
    invisible(unified_ndlm_diag_write_state_components_ci_plot(
      summary_df = state_ci$summary,
      path = paths$ndlm_state_components_ci_transfer,
      title = "NDLM Transfer Block States (15+): Median + 95% CI",
      component_ids = which(sort(unique(as.integer(state_ci$summary$component_id))) >= 15L)
    ))
  }

  if (isTRUE(strict_contract)) {
    mismatches <- horizon_contract$figure_or_series[horizon_contract$status != "pass"]
    if (length(mismatches) > 0L) {
      stop(
        sprintf(
          "[NDLM_HORIZON_CONTRACT] NDLM horizon contract mismatch for: %s",
          paste(mismatches, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  list(
    status = if (all(horizon_contract$status == "pass")) "pass" else "mismatch",
    output_dir = normalizePath(output_dir, mustWork = FALSE),
    paths = unname(vapply(paths, normalizePath, character(1), mustWork = FALSE))
  )
}
