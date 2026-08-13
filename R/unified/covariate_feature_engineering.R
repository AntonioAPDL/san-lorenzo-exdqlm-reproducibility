# unified/covariate_feature_engineering.R
#
# Shared engineered-covariate layer for unified reruns.
# Builds a run-scoped feature table from the final shared covariate series after
# deterministic climate substitution has already materialized PPT/SOIL/PCA.

unified_covfeat_read_series <- function(path, label, value_candidates = character(0)) {
  df <- unified_read_csv_checked(path, label, "covariate_features/read_series")
  date_info <- unified_detect_date_info(df, label, path, required = TRUE)

  value_col <- ""
  if (length(value_candidates) > 0L) {
    for (cand in value_candidates) {
      if (!(cand %in% names(df))) next
      vals <- suppressWarnings(as.numeric(df[[cand]]))
      if (sum(is.finite(vals)) >= max(1L, floor(0.8 * length(vals)))) {
        value_col <- cand
        break
      }
    }
  }
  if (!nzchar(value_col)) {
    numeric_cols <- setdiff(
      names(df)[vapply(df, function(x) {
        vals <- suppressWarnings(as.numeric(x))
        sum(is.finite(vals)) >= max(1L, floor(0.8 * length(vals)))
      }, logical(1))],
      date_info$col
    )
    if (length(numeric_cols) == 0L) {
      stop(
        sprintf("covariate feature engineering could not identify a numeric value column for %s: %s", label, path),
        call. = FALSE
      )
    }
    value_col <- numeric_cols[[1L]]
  }

  out <- data.frame(
    date = as.Date(date_info$dates),
    value = suppressWarnings(as.numeric(df[[value_col]])),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$date), , drop = FALSE]
  out <- stats::aggregate(value ~ date, data = out, FUN = mean, na.rm = TRUE)
  out <- out[order(out$date), , drop = FALSE]
  if (nrow(out) < 1L) {
    stop(sprintf("covariate feature engineering found no dated rows for %s: %s", label, path), call. = FALSE)
  }

  list(
    data = out,
    source_path = normalizePath(path, mustWork = FALSE),
    value_candidates = value_candidates,
    value_col = value_col
  )
}

unified_covfeat_align_series <- function(series_df, target_dates) {
  target_dates <- as.Date(target_dates)
  out <- rep(NA_real_, length(target_dates))
  if (!is.data.frame(series_df) || nrow(series_df) < 1L || length(target_dates) < 1L) {
    return(out)
  }
  mt <- match(target_dates, as.Date(series_df$date))
  hit <- which(!is.na(mt))
  if (length(hit) > 0L) {
    out[hit] <- as.numeric(series_df$value[mt[hit]])
  }
  out
}

unified_covfeat_apply_lags <- function(x, lag_orders) {
  x <- as.numeric(x)
  out <- list()
  for (lag_k in lag_orders) {
    lag_k <- as.integer(lag_k)
    if (!is.finite(lag_k) || lag_k < 1L) next
    if (length(x) <= lag_k) {
      out[[paste0("lag", lag_k)]] <- rep(NA_real_, length(x))
    } else {
      out[[paste0("lag", lag_k)]] <- c(rep(NA_real_, lag_k), x[seq_len(length(x) - lag_k)])
    }
  }
  out
}

unified_covfeat_build_table <- function(
  ppt_path,
  soil_path,
  pca_path,
  lag_orders = c(1L, 2L, 3L),
  include_squares = TRUE,
  include_interaction = TRUE
) {
  ppt_info <- unified_covfeat_read_series(
    ppt_path,
    label = "feature_ppt",
    value_candidates = c("ppt", "PRCP_mm", "precip", "prcp", "precipitation", "value")
  )
  soil_info <- unified_covfeat_read_series(
    soil_path,
    label = "feature_soil",
    value_candidates = c("soil", "Daily_Avg_Soil_Moisture", "soil_moisture", "daily_avg_soil_moisture", "value")
  )
  pca_info <- unified_covfeat_read_series(
    pca_path,
    label = "feature_pca",
    value_candidates = c("Static_PCA", "PCA", "static_pca", "pca", "value")
  )

  all_dates <- sort(unique(c(ppt_info$data$date, soil_info$data$date, pca_info$data$date)))
  if (length(all_dates) < 1L) {
    stop("covariate feature engineering found no dates across PPT/SOIL/PCA", call. = FALSE)
  }
  full_dates <- seq(min(all_dates), max(all_dates), by = "day")

  out <- data.frame(
    date = as.Date(full_dates),
    PPT = unified_covfeat_align_series(ppt_info$data, full_dates),
    SOIL = unified_covfeat_align_series(soil_info$data, full_dates),
    PCA = unified_covfeat_align_series(pca_info$data, full_dates),
    stringsAsFactors = FALSE
  )

  if (isTRUE(include_squares)) {
    out$PPT_sq <- out$PPT ^ 2
    out$SOIL_sq <- out$SOIL ^ 2
  }
  if (isTRUE(include_interaction)) {
    out$PPT_x_SOIL <- out$PPT * out$SOIL
  }

  lag_orders <- sort(unique(as.integer(lag_orders[is.finite(lag_orders)])))
  lag_orders <- lag_orders[lag_orders >= 1L]
  ppt_lags <- unified_covfeat_apply_lags(out$PPT, lag_orders)
  soil_lags <- unified_covfeat_apply_lags(out$SOIL, lag_orders)
  for (nm in names(ppt_lags)) {
    out[[paste0("PPT_", nm)]] <- ppt_lags[[nm]]
  }
  for (nm in names(soil_lags)) {
    out[[paste0("SOIL_", nm)]] <- soil_lags[[nm]]
  }

  attr(out, "feature_metadata") <- list(
    ppt_source = ppt_info$source_path,
    soil_source = soil_info$source_path,
    pca_source = pca_info$source_path,
    lag_orders = lag_orders,
    include_squares = isTRUE(include_squares),
    include_interaction = isTRUE(include_interaction)
  )
  out
}

unified_covfeat_write_table <- function(df, out_csv, summary_path = NULL) {
  if (!is.data.frame(df) || nrow(df) < 1L) {
    stop("covariate feature engineering requires a non-empty data.frame", call. = FALSE)
  }
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, out_csv, row.names = FALSE)

  if (!is.null(summary_path) && nzchar(as.character(summary_path))) {
    meta <- attr(df, "feature_metadata")
    missing_counts <- vapply(df, function(x) sum(!is.finite(as.numeric(x))), numeric(1))
    missing_counts <- missing_counts[names(missing_counts) != "date"]
    lines <- c(
      sprintf("path=%s", normalizePath(out_csv, mustWork = FALSE)),
      sprintf("rows=%d", nrow(df)),
      sprintf("date_min=%s", as.character(min(as.Date(df$date), na.rm = TRUE))),
      sprintf("date_max=%s", as.character(max(as.Date(df$date), na.rm = TRUE))),
      sprintf("columns=%s", paste(names(df), collapse = ",")),
      sprintf("lag_orders=%s", paste(meta$lag_orders, collapse = ",")),
      sprintf("include_squares=%s", if (isTRUE(meta$include_squares)) "TRUE" else "FALSE"),
      sprintf("include_interaction=%s", if (isTRUE(meta$include_interaction)) "TRUE" else "FALSE"),
      sprintf("ppt_source=%s", meta$ppt_source),
      sprintf("soil_source=%s", meta$soil_source),
      sprintf("pca_source=%s", meta$pca_source)
    )
    if (length(missing_counts) > 0L) {
      lines <- c(lines, sprintf("missing_counts.%s=%d", names(missing_counts), as.integer(missing_counts)))
    }
    writeLines(lines, con = summary_path, useBytes = TRUE)
  }
  invisible(out_csv)
}

unified_materialize_covariate_features <- function(cfg, shared_paths, cov_path_map) {
  enabled <- unified_get(cfg, c("inputs", "covariate_features", "enabled"), default = FALSE)
  if (!isTRUE(enabled)) {
    return(NULL)
  }

  output_filename <- as.character(unified_get(
    cfg,
    c("inputs", "covariate_features", "output_filename"),
    default = "covariate_features.csv"
  )[[1L]])
  if (!nzchar(output_filename)) {
    stop("inputs.covariate_features.output_filename must be non-empty when enabled=true", call. = FALSE)
  }
  lag_orders <- unified_get(cfg, c("inputs", "covariate_features", "lag_orders"), default = c(1L, 2L, 3L))
  if (is.null(lag_orders)) lag_orders <- c(1L, 2L, 3L)
  lag_orders <- as.integer(unlist(lag_orders, use.names = FALSE))
  lag_orders <- lag_orders[is.finite(lag_orders) & lag_orders >= 1L]
  if (length(lag_orders) < 1L) {
    stop("inputs.covariate_features.lag_orders must include at least one positive integer", call. = FALSE)
  }
  include_squares <- isTRUE(unified_get(cfg, c("inputs", "covariate_features", "include_squares"), default = TRUE))
  include_interaction <- isTRUE(unified_get(cfg, c("inputs", "covariate_features", "include_interaction"), default = TRUE))

  missing_inputs <- c(
    if (!nzchar(cov_path_map$ppt)) "PPT" else character(0),
    if (!nzchar(cov_path_map$soil)) "SOIL" else character(0),
    if (!nzchar(cov_path_map$pca)) "PCA" else character(0)
  )
  if (length(missing_inputs) > 0L) {
    stop(
      sprintf(
        "covariate feature engineering requires run-scoped PPT/SOIL/PCA covariates; missing: %s",
        paste(missing_inputs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out_csv <- file.path(shared_paths$covariates_dir, output_filename)
  summary_path <- file.path(shared_paths$covariates_dir, sub("\\.csv$", "_summary.txt", output_filename, ignore.case = TRUE))
  feature_df <- unified_covfeat_build_table(
    ppt_path = cov_path_map$ppt,
    soil_path = cov_path_map$soil,
    pca_path = cov_path_map$pca,
    lag_orders = lag_orders,
    include_squares = include_squares,
    include_interaction = include_interaction
  )
  unified_covfeat_write_table(feature_df, out_csv = out_csv, summary_path = summary_path)

  list(
    csv_path = normalizePath(out_csv, mustWork = FALSE),
    summary_path = normalizePath(summary_path, mustWork = FALSE),
    lag_orders = lag_orders,
    include_squares = include_squares,
    include_interaction = include_interaction,
    column_names = names(feature_df)
  )
}
