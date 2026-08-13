family_shared_read_csv <- function(path, label) {
  if (is.null(path) || !nzchar(path)) {
    stop(sprintf("input %s path is empty", label), call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("input %s missing: %s", label, path), call. = FALSE)
  }
  out <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) e
  )
  if (inherits(out, "error") || !is.data.frame(out)) {
    stop(sprintf("input %s unreadable CSV: %s", label, path), call. = FALSE)
  }
  if (nrow(out) < 1L || ncol(out) < 1L) {
    stop(sprintf("input %s is empty: %s", label, path), call. = FALSE)
  }
  out
}

family_shared_read_usgs_daily <- function(path, min_date = as.Date("1979-01-01")) {
  df <- family_shared_read_csv(path, "usgs_daily")
  src_dates <- family_shared_pick_date_column(df, cov_name = "USGS")
  if (all(is.na(src_dates))) {
    stop(sprintf("input usgs_daily has no parseable date column: %s", path), call. = FALSE)
  }

  cfs_to_cms <- 0.0283168466
  cfs <- NULL
  cms <- NULL

  if ("discharge_cfs" %in% names(df)) {
    cfs <- suppressWarnings(as.numeric(df$discharge_cfs))
  } else if ("X_00060_00003" %in% names(df)) {
    cfs <- suppressWarnings(as.numeric(df$X_00060_00003))
  }

  if ("discharge_cms" %in% names(df)) {
    cms <- suppressWarnings(as.numeric(df$discharge_cms))
  }

  if (is.null(cfs) && is.null(cms)) {
    fallback <- family_shared_pick_numeric_column(
      df,
      preferred = c("USGS", "usgs", "flow", "value", "discharge")
    )
    if (is.null(fallback)) {
      stop(sprintf("input usgs_daily has no numeric discharge column: %s", path), call. = FALSE)
    }
    cms <- suppressWarnings(as.numeric(fallback))
  }

  if (is.null(cms)) {
    cms <- cfs * cfs_to_cms
  }
  if (is.null(cfs)) {
    cfs <- cms / cfs_to_cms
  }

  keep <- !is.na(src_dates) & is.finite(cfs) & is.finite(cms)
  out <- data.frame(
    Date = as.Date(src_dates[keep]),
    timestamp = as.Date(src_dates[keep]),
    time = as.Date(src_dates[keep]),
    discharge_cfs = as.numeric(cfs[keep]),
    discharge_cms = as.numeric(cms[keep]),
    X_00060_00003 = as.numeric(cfs[keep]),
    data0 = log(as.numeric(cms[keep]) + 1),
    stringsAsFactors = FALSE
  )
  out <- out[out$Date > as.Date(min_date), , drop = FALSE]
  out <- out[order(out$Date), , drop = FALSE]
  rownames(out) <- NULL
  if (nrow(out) < 1L) {
    stop(sprintf("input usgs_daily has no finite rows after filtering: %s", path), call. = FALSE)
  }
  out
}

family_shared_normalize_name <- function(x) {
  gsub("[^a-z0-9]+", "", tolower(as.character(x)))
}

family_shared_pick_numeric_column <- function(df, preferred = character(0), exclude_time_like = TRUE) {
  nm_norm <- family_shared_normalize_name(names(df))
  if (length(preferred) > 0L) {
    pref_norm <- unique(family_shared_normalize_name(preferred))
    for (cand in pref_norm) {
      idx <- which(nm_norm == cand)
      if (length(idx) < 1L) next
      col <- suppressWarnings(as.numeric(df[[idx[[1L]]]]))
      if (any(is.finite(col))) {
        return(col)
      }
    }
  }

  num_idx <- which(vapply(df, is.numeric, logical(1)))
  if (length(num_idx) < 1L) {
    return(NULL)
  }
  if (isTRUE(exclude_time_like)) {
    drop_pat <- "^(date|time|timestamp|index|idx|year|month|day|julian)$"
    keep <- !grepl(drop_pat, nm_norm[num_idx])
    if (any(keep)) {
      num_idx <- num_idx[keep]
    }
  }
  if (length(num_idx) < 1L) {
    return(NULL)
  }
  col <- suppressWarnings(as.numeric(df[[num_idx[[1L]]]]))
  if (!any(is.finite(col))) {
    return(NULL)
  }
  col
}

family_shared_shift_date_years <- function(x, years_back) {
  if (!length(x)) return(as.Date(character(0)))
  if (requireNamespace("lubridate", quietly = TRUE)) {
    return(as.Date(x - lubridate::years(as.integer(years_back))))
  }
  as.Date(as.numeric(x) - round(as.numeric(years_back) * 365.2425), origin = "1970-01-01")
}

family_shared_pick_date_column <- function(df, cov_name = NULL) {
  nms <- names(df)
  if (!length(nms)) return(rep(as.Date(NA), nrow(df)))
  candidates <- unique(c(
    nms[grepl("date|time|timestamp", tolower(nms))],
    nms[[1L]]
  ))
  cov_key <- toupper(trimws(as.character(cov_name[[1L]])))
  min_good <- max(1L, min(10L, floor(0.7 * nrow(df))))

  parse_one <- function(v, shift_eli = FALSE) {
    if (inherits(v, "Date")) {
      return(as.Date(v))
    }
    if (is.numeric(v)) {
      d <- suppressWarnings(as.Date(v, origin = "1970-01-01"))
      if (shift_eli) {
        d <- family_shared_shift_date_years(d, 170L)
      }
      return(d)
    }
    suppressWarnings(as.Date(v))
  }

  for (nm in candidates) {
    vals <- df[[nm]]
    d <- parse_one(vals, shift_eli = FALSE)
    if (sum(!is.na(d)) >= min_good) {
      return(d)
    }
    if (identical(cov_key, "ELI")) {
      d_eli <- parse_one(vals, shift_eli = TRUE)
      if (sum(!is.na(d_eli)) >= min_good) {
        return(d_eli)
      }
    }
  }
  rep(as.Date(NA), nrow(df))
}

family_shared_feature_columns <- function(df) {
  if (!is.data.frame(df) || ncol(df) < 1L) return(character(0))
  nm <- names(df)
  nm_norm <- family_shared_normalize_name(nm)
  num_idx <- which(vapply(df, is.numeric, logical(1)))
  if (length(num_idx) < 1L) return(character(0))
  drop_pat <- "^(date|time|timestamp|index|idx|year|month|day|julian)$"
  keep <- !grepl(drop_pat, nm_norm[num_idx])
  if (any(keep)) {
    num_idx <- num_idx[keep]
  }
  nm[num_idx]
}

family_shared_normalize_selected_feature_names <- function(selected_feature_names) {
  if (is.null(selected_feature_names) || length(selected_feature_names) == 0L) {
    return(character(0))
  }
  vals <- trimws(as.character(unlist(selected_feature_names, use.names = FALSE)))
  vals <- vals[nzchar(vals)]
  unique(vals)
}

family_shared_apply_feature_selection <- function(X, X_f, selected_feature_names, context_label = "feature design") {
  selected <- family_shared_normalize_selected_feature_names(selected_feature_names)
  if (length(selected) < 1L) {
    return(list(X = X, X_f = X_f, feature_names = colnames(X)))
  }
  available <- colnames(X)
  if (is.null(available) || length(available) < 1L) {
    stop(sprintf("%s has no available feature columns to select from", context_label), call. = FALSE)
  }
  missing <- setdiff(selected, available)
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "%s is missing selected feature columns: %s",
        context_label,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  list(
    X = X[, selected, drop = FALSE],
    X_f = X_f[, selected, drop = FALSE],
    feature_names = selected
  )
}

family_shared_transfer_design_diagnostics <- function(
  X,
  X_f = NULL,
  out_dir = "",
  mode = "",
  feature_names = NULL
) {
  if (!is.matrix(X)) X <- as.matrix(X)
  if (!is.null(X_f) && !is.matrix(X_f)) X_f <- as.matrix(X_f)
  if (is.null(feature_names) || length(feature_names) != ncol(X)) {
    feature_names <- colnames(X)
  }
  if (is.null(feature_names) || length(feature_names) != ncol(X)) {
    feature_names <- sprintf("feature_%02d", seq_len(ncol(X)))
  }
  colnames(X) <- feature_names
  if (!is.null(X_f) && ncol(X_f) == length(feature_names)) {
    colnames(X_f) <- feature_names
  }

  empty_summary <- function() {
    data.frame(
      block = character(0),
      feature_index = integer(0),
      feature_name = character(0),
      n = integer(0),
      finite_n = integer(0),
      nonfinite_n = integer(0),
      mean = numeric(0),
      sd = numeric(0),
      min = numeric(0),
      q01 = numeric(0),
      median = numeric(0),
      q99 = numeric(0),
      max = numeric(0),
      max_abs = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  empty_condition <- function(block) {
    data.frame(
      block = block,
      n_rows = 0L,
      n_features = 0L,
      finite_complete_rows = 0L,
      rank = 0L,
      rank_deficient = NA,
      singular_min = NA_real_,
      singular_max = NA_real_,
      condition_number = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  summarize_block <- function(mat, block) {
    if (is.null(mat) || ncol(mat) < 1L) {
      return(empty_summary())
    }
    rows <- lapply(seq_len(ncol(mat)), function(j) {
      vals <- suppressWarnings(as.numeric(mat[, j]))
      finite <- vals[is.finite(vals)]
      q <- function(p) {
        if (!length(finite)) return(NA_real_)
        as.numeric(stats::quantile(finite, probs = p, names = FALSE, na.rm = TRUE, type = 7))
      }
      data.frame(
        block = block,
        feature_index = j,
        feature_name = colnames(mat)[[j]],
        n = length(vals),
        finite_n = length(finite),
        nonfinite_n = length(vals) - length(finite),
        mean = if (length(finite)) mean(finite) else NA_real_,
        sd = if (length(finite) > 1L) stats::sd(finite) else NA_real_,
        min = if (length(finite)) min(finite) else NA_real_,
        q01 = q(0.01),
        median = q(0.50),
        q99 = q(0.99),
        max = if (length(finite)) max(finite) else NA_real_,
        max_abs = if (length(finite)) max(abs(finite)) else NA_real_,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }

  condition_rows <- function(mat, block) {
    if (is.null(mat) || ncol(mat) < 1L || nrow(mat) < 1L) {
      return(empty_condition(block))
    }
    finite_rows <- apply(mat, 1L, function(row) all(is.finite(row)))
    mat_finite <- mat[finite_rows, , drop = FALSE]
    if (nrow(mat_finite) < 1L) {
      svals <- numeric(0)
    } else {
      svals <- tryCatch(svd(mat_finite, nu = 0L, nv = 0L)$d, error = function(e) numeric(0))
    }
    s_all <- svals[is.finite(svals)]
    s_finite <- s_all[s_all > 0]
    rank_val <- if (length(s_finite)) sum(s_finite > max(s_finite) * .Machine$double.eps * max(dim(mat_finite))) else 0L
    rank_deficient <- rank_val < min(nrow(mat_finite), ncol(mat_finite))
    data.frame(
      block = block,
      n_rows = nrow(mat),
      n_features = ncol(mat),
      finite_complete_rows = sum(finite_rows),
      rank = rank_val,
      rank_deficient = isTRUE(rank_deficient),
      singular_min = if (length(s_finite)) min(s_finite) else NA_real_,
      singular_max = if (length(s_finite)) max(s_finite) else NA_real_,
      condition_number = if (isTRUE(rank_deficient)) Inf else if (length(s_finite)) max(s_finite) / min(s_finite) else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  summary <- rbind(
    summarize_block(X, "history"),
    summarize_block(X_f, "forecast")
  )
  condition <- rbind(
    condition_rows(X, "history"),
    condition_rows(X_f, "forecast")
  )
  metadata <- data.frame(
    mode = rep(as.character(mode), length(feature_names)),
    feature_index = seq_along(feature_names),
    feature_name = as.character(feature_names),
    history_n = rep(nrow(X), length(feature_names)),
    forecast_n = rep(if (is.null(X_f)) 0L else nrow(X_f), length(feature_names)),
    stringsAsFactors = FALSE
  )

  if (!is.null(out_dir) && nzchar(as.character(out_dir))) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(summary, file.path(out_dir, "transfer_design_summary.csv"), row.names = FALSE)
    utils::write.csv(condition, file.path(out_dir, "transfer_design_condition.csv"), row.names = FALSE)
    utils::write.csv(metadata, file.path(out_dir, "transfer_feature_metadata.csv"), row.names = FALSE)
  }

  list(summary = summary, condition = condition, metadata = metadata)
}

family_shared_build_feature_matrices <- function(
  path,
  history_dates,
  forecast_dates = NULL,
  fill_value = 0,
  scale_with_history = TRUE,
  scale_mode = "sd"
) {
  history_dates <- as.Date(history_dates)
  forecast_dates <- as.Date(forecast_dates)
  scale_mode <- tolower(trimws(as.character(scale_mode[[1L]])))
  if (!scale_mode %in% c("sd", "zscore")) {
    scale_mode <- "sd"
  }
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(list(
      history = matrix(numeric(0), nrow = length(history_dates), ncol = 0L),
      forecast = matrix(numeric(0), nrow = length(forecast_dates), ncol = 0L),
      feature_names = character(0),
      scales = numeric(0)
    ))
  }

  df <- family_shared_read_csv(path, "covariate_features")
  feature_cols <- family_shared_feature_columns(df)
  if (length(feature_cols) < 1L) {
    stop(sprintf("covariate feature table has no numeric feature columns: %s", path), call. = FALSE)
  }
  src_dates <- family_shared_pick_date_column(df, cov_name = "FEATURES")
  if (all(is.na(src_dates))) {
    stop(sprintf("covariate feature table has no parseable date column: %s", path), call. = FALSE)
  }

  hist_cols <- vector("list", length(feature_cols))
  fore_cols <- vector("list", length(feature_cols))
  scales <- rep(NA_real_, length(feature_cols))
  for (i in seq_along(feature_cols)) {
    vals <- suppressWarnings(as.numeric(df[[feature_cols[[i]]]]))
    hist_vals_raw <- family_shared_align_by_dates(src_dates, vals, history_dates, fill_value = NA_real_)
    fore_vals_raw <- family_shared_align_by_dates(src_dates, vals, forecast_dates, fill_value = NA_real_)
    if (isTRUE(scale_with_history)) {
      scaled <- family_shared_scale_history_future(hist_vals_raw, fore_vals_raw, mode = scale_mode)
      fill_scaled <- as.numeric(fill_value) / scaled$sd
      if (identical(scale_mode, "zscore")) {
        fill_scaled <- (as.numeric(fill_value) - scaled$center) / scaled$sd
      }
      hist_vals <- scaled$history
      fore_vals <- scaled$future
      hist_vals[!is.finite(hist_vals_raw)] <- fill_scaled
      fore_vals[!is.finite(fore_vals_raw)] <- fill_scaled
      hist_cols[[i]] <- hist_vals
      fore_cols[[i]] <- fore_vals
      scales[[i]] <- scaled$sd
    } else {
      hist_vals <- hist_vals_raw
      fore_vals <- fore_vals_raw
      hist_vals[!is.finite(hist_vals)] <- as.numeric(fill_value)
      fore_vals[!is.finite(fore_vals)] <- as.numeric(fill_value)
      hist_cols[[i]] <- hist_vals
      fore_cols[[i]] <- fore_vals
    }
  }

  history_mat <- if (length(hist_cols) > 0L) do.call(cbind, hist_cols) else matrix(numeric(0), nrow = length(history_dates), ncol = 0L)
  forecast_mat <- if (length(fore_cols) > 0L) do.call(cbind, fore_cols) else matrix(numeric(0), nrow = length(forecast_dates), ncol = 0L)
  if (!is.matrix(history_mat)) history_mat <- matrix(history_mat, ncol = length(feature_cols))
  if (!is.matrix(forecast_mat)) forecast_mat <- matrix(forecast_mat, ncol = length(feature_cols))
  colnames(history_mat) <- feature_cols
  colnames(forecast_mat) <- feature_cols

  list(
    history = history_mat,
    forecast = forecast_mat,
    feature_names = feature_cols,
    scales = setNames(scales, feature_cols)
  )
}

family_shared_safe_sd <- function(x, default = 1) {
  sd_val <- suppressWarnings(stats::sd(as.numeric(x), na.rm = TRUE))
  if (!is.finite(sd_val) || sd_val <= 0) {
    return(as.numeric(default))
  }
  as.numeric(sd_val)
}

family_shared_match_column <- function(df, candidates) {
  if (!is.data.frame(df) || ncol(df) < 1L) return(NULL)
  nm_norm <- family_shared_normalize_name(names(df))
  cand_norm <- unique(family_shared_normalize_name(candidates))
  for (cand in cand_norm) {
    idx <- which(nm_norm == cand)
    if (length(idx) < 1L) next
    return(as.numeric(df[[idx[[1L]]]]))
  }
  NULL
}

family_shared_build_featurecov_design_matrices <- function(
  history_df,
  forecast_df,
  history_dates,
  forecast_dates,
  feature_path = "",
  fill_value = 0,
  selected_feature_names = NULL,
  feature_mode = "full",
  scaling_mode = "sd"
) {
  history_dates <- as.Date(history_dates)
  forecast_dates <- as.Date(forecast_dates)
  history_n <- length(history_dates)
  forecast_n <- length(forecast_dates)
  feature_mode <- tolower(trimws(as.character(feature_mode[[1L]])))
  if (!feature_mode %in% c("full", "base_only", "custom", "none")) {
    feature_mode <- "full"
  }
  scaling_mode <- tolower(trimws(as.character(scaling_mode[[1L]])))
  if (!scaling_mode %in% c("sd", "zscore")) {
    scaling_mode <- "sd"
  }

  if (!is.data.frame(history_df)) history_df <- as.data.frame(history_df)
  if (!is.data.frame(forecast_df)) forecast_df <- as.data.frame(forecast_df)

  if (identical(feature_mode, "none")) {
    X <- matrix(numeric(0), nrow = history_n, ncol = 0L)
    X_f <- matrix(numeric(0), nrow = forecast_n, ncol = 0L)
    colnames(X) <- character(0)
    colnames(X_f) <- character(0)
    return(list(
      X = X,
      X_f = X_f,
      mode = paste("transfer_level_only", scaling_mode, sep = "_"),
      feature_names = character(0),
      feature_mode = feature_mode,
      scaling_mode = scaling_mode
    ))
  }

  if (nzchar(feature_path) && file.exists(feature_path)) {
    feature_bundle <- family_shared_build_feature_matrices(
      path = feature_path,
      history_dates = history_dates,
      forecast_dates = forecast_dates,
      fill_value = fill_value,
      scale_with_history = TRUE,
      scale_mode = scaling_mode
    )
    # Keep SD-only scaling from the engineered feature table, but do not
    # append an explicit intercept feature. The transfer block already has its
    # own zeta_t state, so a second constant regressor is redundant.
    X <- data.matrix(feature_bundle$history)
    X_f <- data.matrix(feature_bundle$forecast)
    if (!is.matrix(X)) X <- matrix(X, nrow = history_n)
    if (!is.matrix(X_f)) X_f <- matrix(X_f, nrow = forecast_n)
    colnames(X) <- feature_bundle$feature_names
    colnames(X_f) <- feature_bundle$feature_names
    selected <- family_shared_apply_feature_selection(
      X = X,
      X_f = X_f,
      selected_feature_names = selected_feature_names,
      context_label = "engineered feature table"
    )
    return(list(
      X = selected$X,
      X_f = selected$X_f,
      mode = paste("engineered_feature_table", scaling_mode, sep = "_"),
      feature_names = selected$feature_names,
      feature_mode = feature_mode,
      scaling_mode = scaling_mode
    ))
  }

  hist_ppt <- family_shared_match_column(history_df, c("ppt", "precip"))
  hist_soil <- family_shared_match_column(history_df, c("soil", "soil_moisture"))
  hist_pca <- family_shared_match_column(history_df, c("Static_PCA", "PCA"))
  fore_ppt <- family_shared_match_column(forecast_df, c("ppt", "precip"))
  fore_soil <- family_shared_match_column(forecast_df, c("soil", "soil_moisture"))
  fore_pca <- family_shared_match_column(forecast_df, c("Static_PCA", "PCA"))
  if (is.null(hist_ppt) || is.null(hist_soil) || is.null(hist_pca) ||
      is.null(fore_ppt) || is.null(fore_soil) || is.null(fore_pca)) {
    stop("legacy featurecov fallback requires PPT, SOIL, and PCA columns in history and forecast frames", call. = FALSE)
  }

  base_hist <- cbind(
    PPT = hist_ppt,
    SOIL = hist_soil,
    PCA = hist_pca
  )
  base_fore <- cbind(
    PPT = fore_ppt,
    SOIL = fore_soil,
    PCA = fore_pca
  )

  # Legacy fallback omits an explicit intercept feature from the transfer
  # design. The default scaling is the historical SD-only contract.
  X <- base_hist
  X_f <- base_fore

  x_ext <- matrix(NA_real_, ncol = 5L, nrow = history_n)
  x_ext[, 1L] <- c(0, X[seq_len(history_n - 1L), 1L])
  x_ext[, 2L] <- c(0, 0, X[seq_len(history_n - 2L), 1L])
  x_ext[, 3L] <- X[, 1L]^2
  x_ext[, 4L] <- c(0, X[seq_len(history_n - 1L), 1L])^2
  x_ext[, 5L] <- c(0, 0, X[seq_len(history_n - 2L), 1L])^2

  x_ext_f <- matrix(NA_real_, ncol = 5L, nrow = forecast_n)
  x_ext_f[, 1L] <- c(X[history_n, 1L], X_f[seq_len(forecast_n - 1L), 1L])
  x_ext_f[, 2L] <- c(X[history_n - 1L, 1L], X[history_n, 1L], X_f[seq_len(forecast_n - 2L), 1L])
  x_ext_f[, 3L] <- X_f[, 1L]^2
  x_ext_f[, 4L] <- c(X[history_n, 1L], X_f[seq_len(forecast_n - 1L), 1L])^2
  x_ext_f[, 5L] <- c(X[history_n - 1L, 1L], X[history_n, 1L], X_f[seq_len(forecast_n - 2L), 1L])^2

  scale_pair <- function(hist_mat, fore_mat) {
    for (j in seq_len(ncol(hist_mat))) {
      scaled <- family_shared_scale_history_future(hist_mat[, j], fore_mat[, j], mode = scaling_mode)
      hist_mat[, j] <- scaled$history
      fore_mat[, j] <- scaled$future
    }
    list(history = hist_mat, forecast = fore_mat)
  }
  main_scaled <- scale_pair(X[, 1:3, drop = FALSE], X_f[, 1:3, drop = FALSE])
  X[, 1:3] <- main_scaled$history
  X_f[, 1:3] <- main_scaled$forecast
  ext_scaled <- scale_pair(x_ext, x_ext_f)
  x_ext <- ext_scaled$history
  x_ext_f <- ext_scaled$forecast

  X <- cbind(X, x_ext)
  X_f <- cbind(X_f, x_ext_f)
  colnames(X) <- c(
    "PPT", "SOIL", "PCA",
    "PPT_lag1", "PPT_lag2", "PPT_sq", "PPT_lag1_sq", "PPT_lag2_sq"
  )
  colnames(X_f) <- colnames(X)
  selected <- family_shared_apply_feature_selection(
    X = X,
    X_f = X_f,
    selected_feature_names = selected_feature_names,
    context_label = "legacy featurecov fallback"
  )

  list(
    X = selected$X,
    X_f = selected$X_f,
    mode = paste("legacy_precip_extension", scaling_mode, sep = "_"),
    feature_names = selected$feature_names,
    feature_mode = feature_mode,
    scaling_mode = scaling_mode
  )
}

family_shared_tail_align_series <- function(x, target_len, fill = 0) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  target_len <- suppressWarnings(as.integer(target_len[[1L]]))
  if (!is.finite(target_len) || target_len <= 0L) return(numeric(0))
  if (length(x) == 0L) return(rep(as.numeric(fill), target_len))
  if (length(x) >= target_len) return(tail(x, target_len))
  c(rep(as.numeric(fill), target_len - length(x)), x)
}

family_shared_align_by_dates <- function(dates_src, values_src, target_dates, fill_value = 0) {
  values_src <- as.numeric(values_src)
  target_dates <- as.Date(target_dates)
  if (length(target_dates) == 0L) return(numeric(0))
  if (length(values_src) < length(dates_src)) {
    values_src <- c(values_src, rep(NA_real_, length(dates_src) - length(values_src)))
  }
  if (all(is.na(dates_src))) {
    return(family_shared_tail_align_series(values_src, length(target_dates), fill = fill_value))
  }

  ord <- order(dates_src)
  dates_ord <- as.Date(dates_src[ord])
  vals_ord <- values_src[ord]
  keep <- !duplicated(dates_ord, fromLast = TRUE)
  dates_ord <- dates_ord[keep]
  vals_ord <- vals_ord[keep]

  out <- rep(as.numeric(fill_value), length(target_dates))
  mt <- match(as.Date(target_dates), dates_ord)
  matched <- which(is.finite(mt))
  if (length(matched) > 0L) {
    out[matched] <- vals_ord[mt[matched]]
  }
  out[!is.finite(out)] <- as.numeric(fill_value)
  out
}

family_shared_sd_scale_history_future <- function(history, future = NULL) {
  history <- as.numeric(history)
  future <- as.numeric(future)
  hist_finite <- history[is.finite(history)]
  sd_hist <- suppressWarnings(stats::sd(hist_finite, na.rm = TRUE))
  if (!is.finite(sd_hist) || sd_hist <= 1e-8) {
    sd_hist <- 1
  }
  history_scaled <- history / sd_hist
  history_scaled[!is.finite(history_scaled)] <- 0
  future_scaled <- future / sd_hist
  future_scaled[!is.finite(future_scaled)] <- 0
  list(
    history = history_scaled,
    future = future_scaled,
    sd = sd_hist,
    center = 0
  )
}

family_shared_zscore_history_future <- function(history, future = NULL) {
  history <- as.numeric(history)
  future <- as.numeric(future)
  hist_finite <- history[is.finite(history)]
  center_hist <- if (length(hist_finite)) mean(hist_finite, na.rm = TRUE) else 0
  if (!is.finite(center_hist)) {
    center_hist <- 0
  }
  sd_hist <- suppressWarnings(stats::sd(hist_finite, na.rm = TRUE))
  if (!is.finite(sd_hist) || sd_hist <= 1e-8) {
    sd_hist <- 1
  }
  history_scaled <- (history - center_hist) / sd_hist
  history_scaled[!is.finite(history_scaled)] <- 0
  future_scaled <- (future - center_hist) / sd_hist
  future_scaled[!is.finite(future_scaled)] <- 0
  list(
    history = history_scaled,
    future = future_scaled,
    sd = sd_hist,
    center = center_hist
  )
}

family_shared_scale_history_future <- function(history, future = NULL, mode = "sd") {
  mode <- tolower(trimws(as.character(mode[[1L]])))
  if (identical(mode, "zscore")) {
    return(family_shared_zscore_history_future(history, future))
  }
  family_shared_sd_scale_history_future(history, future)
}

family_shared_covariate_preferences <- function(cov_name) {
  cov_key <- toupper(trimws(as.character(cov_name[[1L]])))
  switch(
    cov_key,
    ELI = c("ELI_lon", "ELI", "eli_lon", "eli", "value"),
    ONI = c("nino34", "oni", "ONI", "nino3.4", "nino_34", "value"),
    PPT = c("PRCP_mm", "ppt", "precip", "prcp", "precipitation", "value"),
    SOIL = c("Daily_Avg_Soil_Moisture", "soil", "soil_moisture", "daily_avg_soil_moisture", "value"),
    PCA = c("Static_PCA", "PCA", "static_pca", "pca", "value"),
    c(as.character(cov_name), "value", "x", "data")
  )
}

family_shared_build_covariate_series <- function(path, cov_name, history_dates, forecast_dates = NULL, fill_value = 0, scale_with_history = TRUE) {
  history_dates <- as.Date(history_dates)
  forecast_dates <- as.Date(forecast_dates)
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    hist <- rep(as.numeric(fill_value), length(history_dates))
    fore <- rep(as.numeric(fill_value), length(forecast_dates))
    if (isTRUE(scale_with_history)) {
      scaled <- family_shared_sd_scale_history_future(hist, fore)
      return(list(history = scaled$history, forecast = scaled$future, sd = scaled$sd))
    }
    return(list(history = hist, forecast = fore, sd = NA_real_))
  }

  df <- family_shared_read_csv(path, sprintf("covariate_%s", cov_name))
  val <- family_shared_pick_numeric_column(
    df,
    preferred = family_shared_covariate_preferences(cov_name),
    exclude_time_like = TRUE
  )
  if (is.null(val)) {
    hist <- rep(as.numeric(fill_value), length(history_dates))
    fore <- rep(as.numeric(fill_value), length(forecast_dates))
    if (isTRUE(scale_with_history)) {
      scaled <- family_shared_sd_scale_history_future(hist, fore)
      return(list(history = scaled$history, forecast = scaled$future, sd = scaled$sd))
    }
    return(list(history = hist, forecast = fore, sd = NA_real_))
  }

  dt <- family_shared_pick_date_column(df, cov_name = cov_name)
  hist_vals <- family_shared_align_by_dates(dt, val, history_dates, fill_value = fill_value)
  finite_vals <- as.numeric(val[is.finite(val)])
  fore_fill <- if (length(finite_vals) > 0L) tail(finite_vals, 1L) else fill_value
  fore_vals <- family_shared_align_by_dates(dt, val, forecast_dates, fill_value = fore_fill)
  if (isTRUE(scale_with_history)) {
    scaled <- family_shared_sd_scale_history_future(hist_vals, fore_vals)
    return(list(history = scaled$history, forecast = scaled$future, sd = scaled$sd))
  }
  list(history = hist_vals, forecast = fore_vals, sd = NA_real_)
}

family_shared_extract_forecast_mean <- function(df, label, transform = c("none", "log1p", "log")) {
  transform <- match.arg(transform)
  if (!is.data.frame(df) || nrow(df) < 1L) {
    stop(sprintf("%s must be a non-empty data.frame", label), call. = FALSE)
  }
  nm_norm <- family_shared_normalize_name(names(df))
  num_idx <- which(vapply(df, is.numeric, logical(1)))
  if (length(num_idx) < 1L) {
    stop(sprintf("%s has no numeric forecast columns", label), call. = FALSE)
  }
  keep <- !grepl("^(date|time|timestamp|index|idx|year|month|day)$", nm_norm[num_idx])
  if (any(keep)) {
    num_idx <- num_idx[keep]
  }
  if (length(num_idx) < 1L) {
    stop(sprintf("%s has no usable ensemble member columns after excluding date/time fields", label), call. = FALSE)
  }
  mat <- data.matrix(df[, num_idx, drop = FALSE])
  if (!all(is.finite(mat))) {
    mat[!is.finite(mat)] <- NA_real_
  }
  if (identical(transform, "log1p")) {
    if (any(mat < 0, na.rm = TRUE)) {
      stop(sprintf("%s contains negative values; cannot apply log1p transform safely", label), call. = FALSE)
    }
    mat <- log1p(mat)
  } else if (identical(transform, "log")) {
    if (any(mat <= 0, na.rm = TRUE)) {
      stop(sprintf("%s contains non-positive values; cannot apply log transform safely", label), call. = FALSE)
    }
    mat <- log(mat)
  }
  out <- rowMeans(mat, na.rm = TRUE)
  out[!is.finite(out)] <- NA_real_
  out
}

family_shared_extract_forecast_ensemble <- function(
  df,
  label,
  transform = c("none", "log1p", "log"),
  target_dates = NULL
) {
  transform <- match.arg(transform)
  if (!is.data.frame(df) || nrow(df) < 1L) {
    stop(sprintf("%s must be a non-empty data.frame", label), call. = FALSE)
  }

  nm_norm <- family_shared_normalize_name(names(df))
  num_idx <- which(vapply(df, is.numeric, logical(1)))
  if (length(num_idx) < 1L) {
    stop(sprintf("%s has no numeric forecast columns", label), call. = FALSE)
  }
  keep <- !grepl("^(date|time|timestamp|index|idx|year|month|day)$", nm_norm[num_idx])
  if (any(keep)) {
    num_idx <- num_idx[keep]
  }
  if (length(num_idx) < 1L) {
    stop(sprintf("%s has no usable ensemble member columns after excluding date/time fields", label), call. = FALSE)
  }

  mat <- data.matrix(df[, num_idx, drop = FALSE])
  if (!all(is.finite(mat))) {
    mat[!is.finite(mat)] <- NA_real_
  }
  if (identical(transform, "log1p")) {
    if (any(mat < 0, na.rm = TRUE)) {
      stop(sprintf("%s contains negative values; cannot apply log1p transform safely", label), call. = FALSE)
    }
    mat <- log1p(mat)
  } else if (identical(transform, "log")) {
    if (any(mat <= 0, na.rm = TRUE)) {
      stop(sprintf("%s contains non-positive values; cannot apply log transform safely", label), call. = FALSE)
    }
    mat <- log(mat)
  }

  src_dates <- family_shared_pick_date_column(df)
  target_dates <- as.Date(target_dates)
  if (length(target_dates) > 0L) {
    aligned <- matrix(NA_real_, nrow = length(target_dates), ncol = ncol(mat))
    if (!all(is.na(src_dates))) {
      ord <- order(src_dates)
      src_dates_ord <- as.Date(src_dates[ord])
      mat_ord <- mat[ord, , drop = FALSE]
      keep_last <- !duplicated(src_dates_ord, fromLast = TRUE)
      src_dates_ord <- src_dates_ord[keep_last]
      mat_ord <- mat_ord[keep_last, , drop = FALSE]
      mt <- match(target_dates, src_dates_ord)
      hit <- which(!is.na(mt))
      if (length(hit) > 0L) {
        aligned[hit, ] <- mat_ord[mt[hit], , drop = FALSE]
      }
    } else {
      take <- seq_len(min(nrow(mat), length(target_dates)))
      aligned[take, ] <- mat[take, , drop = FALSE]
    }
    mat <- aligned
    src_dates <- target_dates
  }

  row_means <- rowMeans(mat, na.rm = TRUE)
  row_means[!is.finite(row_means)] <- NA_real_
  list(
    members = mat,
    row_means = as.numeric(row_means),
    dates = as.Date(src_dates),
    member_cols = names(df)[num_idx]
  )
}
