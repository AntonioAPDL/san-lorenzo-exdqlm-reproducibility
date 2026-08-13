# unified/deterministic_climate_covariates.R
#
# Run-scoped deterministic climate covariate materialization for unified runs.
# This splices observed history through the cutoff date with forecast-derived
# precipitation and soil covariates for the post-cutoff horizon.

local({
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  candidates <- unique(c(
    if (!is.null(ofile)) file.path(dirname(normalizePath(ofile, mustWork = FALSE)), "deterministic_climate_blend.R") else character(0),
    file.path(getwd(), "R", "unified", "deterministic_climate_blend.R")
  ))
  helper_path <- candidates[file.exists(candidates)][[1L]]
  if (!nzchar(helper_path)) {
    stop("Could not locate R/unified/deterministic_climate_blend.R", call. = FALSE)
  }
  source(helper_path)
})

unified_detclim_detect_date_info <- function(df, label, path) {
  nm <- names(df)
  candidates <- nm[grepl("date|time", tolower(nm))]
  if (length(nm) > 0L) {
    candidates <- unique(c(candidates, nm[[1L]]))
  }
  for (cand in candidates) {
    vals <- suppressWarnings(as.Date(df[[cand]]))
    good <- sum(!is.na(vals))
    if (good >= max(1L, floor(0.8 * length(vals)))) {
      return(list(col = cand, dates = vals))
    }
  }
  stop(
    sprintf(
      "deterministic_climate requires a parseable date column for %s: %s",
      label,
      path
    ),
    call. = FALSE
  )
}

unified_detclim_read_observed_series <- function(path, label, value_candidates = character(0)) {
  df <- unified_read_csv_checked(path, label, "deterministic_climate/read_observed_series")
  date_info <- unified_detclim_detect_date_info(df, label, path)

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
        sprintf("deterministic_climate could not identify a numeric value column for %s: %s", label, path),
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
  if (nrow(out) == 0L) {
    stop(sprintf("deterministic_climate found no dated rows in %s: %s", label, path), call. = FALSE)
  }
  out <- stats::aggregate(value ~ date, data = out, FUN = mean, na.rm = TRUE)
  out <- out[order(out$date), , drop = FALSE]
  if (any(!is.finite(out$value))) {
    stop(sprintf("deterministic_climate found non-finite values in %s: %s", label, path), call. = FALSE)
  }

  list(
    data = out,
    date_col = date_info$col,
    value_col = value_col,
    source_path = normalizePath(path, mustWork = FALSE)
  )
}

unified_detclim_read_member_forecast <- function(path, label) {
  df <- unified_read_csv_checked(path, label, "deterministic_climate/read_member_forecast")
  member_cols <- names(df)[grepl("^member_", names(df))]
  if (length(member_cols) == 0L) {
    stop(sprintf("deterministic_climate forecast file has no member columns: %s", path), call. = FALSE)
  }

  if ("target_date" %in% names(df)) {
    target_date <- suppressWarnings(as.Date(df$target_date))
  } else if ("target_time_utc" %in% names(df)) {
    target_date <- suppressWarnings(as.Date(sub("T.*$", "", as.character(df$target_time_utc))))
  } else {
    stop(sprintf("deterministic_climate forecast file is missing target_date/target_time_utc: %s", path), call. = FALSE)
  }

  if (sum(!is.na(target_date)) == 0L) {
    stop(sprintf("deterministic_climate could not parse forecast dates from %s", path), call. = FALSE)
  }

  df$target_date <- target_date
  if (!("lead_hours" %in% names(df))) {
    df$lead_hours <- NA_real_
  }

  list(
    data = df,
    member_cols = member_cols,
    source_path = normalizePath(path, mustWork = FALSE)
  )
}

unified_detclim_reduce_members <- function(values, reduction) {
  detclim_reduce_values(values, reduction = reduction)
}

unified_detclim_member_daily_by_date <- function(df, member_cols, cutoff_date, end_date, daily_fun, reduction) {
  cutoff_date <- as.Date(cutoff_date)
  end_date <- as.Date(end_date)
  keep <- !is.na(df$target_date) & df$target_date > cutoff_date & df$target_date <= end_date
  df <- df[keep, , drop = FALSE]
  if (nrow(df) == 0L) {
    return(data.frame(
      date = as.Date(character()),
      value = numeric(),
      member_count = integer(),
      stringsAsFactors = FALSE
    ))
  }

  daily_fun <- match.arg(daily_fun, c("sum", "mean"))
  dates <- sort(unique(df$target_date))
  out <- data.frame(
    date = dates,
    value = NA_real_,
    member_count = 0L,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(dates)) {
    sub_df <- df[df$target_date == dates[[i]], member_cols, drop = FALSE]
    member_mat <- as.matrix(sub_df)
    storage.mode(member_mat) <- "numeric"
    if (daily_fun == "sum") {
      member_daily <- colSums(member_mat, na.rm = TRUE)
    } else {
      member_daily <- colMeans(member_mat, na.rm = TRUE)
    }
    member_daily <- member_daily[is.finite(member_daily)]
    out$value[[i]] <- unified_detclim_reduce_members(member_daily, reduction = reduction)
    out$member_count[[i]] <- as.integer(length(member_daily))
  }

  out
}

unified_detclim_forecast_horizon_days <- function(glofas_csv, cutoff_date, override_days = NULL) {
  if (!is.null(override_days)) {
    val <- suppressWarnings(as.integer(override_days))
    if (!is.finite(val) || val < 1L) {
      stop("inputs.deterministic_climate.horizon_days must be null or an integer >= 1.", call. = FALSE)
    }
    return(as.integer(val))
  }

  df <- unified_read_csv_checked(glofas_csv, "shared GloFAS forecast", "deterministic_climate/horizon_days")
  date_info <- unified_detclim_detect_date_info(df, "shared GloFAS forecast", glofas_csv)
  future_dates <- sort(unique(as.Date(date_info$dates[!is.na(date_info$dates) & date_info$dates > as.Date(cutoff_date)])))
  if (length(future_dates) > 0L) {
    return(as.integer(length(future_dates)))
  }
  if (nrow(df) < 1L) {
    stop(sprintf("deterministic_climate could not infer forecast horizon from empty GloFAS file: %s", glofas_csv), call. = FALSE)
  }
  as.integer(nrow(df))
}

unified_detclim_resolve_handoff_root <- function(path) {
  path <- as.character(path[[1L]])
  if (!nzchar(path)) {
    stop("inputs.deterministic_climate.handoff_root is required when deterministic climate substitution is enabled.", call. = FALSE)
  }
  candidate <- normalizePath(path, mustWork = FALSE)
  if (dir.exists(file.path(candidate, "catalogs")) && dir.exists(file.path(candidate, "forecast_cache"))) {
    return(candidate)
  }

  nested <- Sys.glob(file.path(candidate, "handoff_forecasts", "site=*", "run_id=*"))
  nested <- nested[dir.exists(nested)]
  nested <- nested[
    dir.exists(file.path(nested, "catalogs")) &
      dir.exists(file.path(nested, "forecast_cache"))
  ]
  if (length(nested) == 1L) {
    return(normalizePath(nested[[1L]], mustWork = FALSE))
  }
  if (length(nested) > 1L) {
    stop(
      sprintf(
        "inputs.deterministic_climate.handoff_root is ambiguous; multiple handoff roots found under %s",
        candidate
      ),
      call. = FALSE
    )
  }

  stop(sprintf("deterministic_climate handoff root not found or incomplete: %s", candidate), call. = FALSE)
}

unified_detclim_resolve_gefs_apcp_path <- function(handoff_root, cutoff_date) {
  unified_detclim_resolve_gefs_path(
    handoff_root = handoff_root,
    cutoff_date = cutoff_date,
    short_name = "APCP",
    level_descriptor = "surface"
  )
}

unified_detclim_read_gefs_catalog <- function(handoff_root) {
  catalog_path <- file.path(handoff_root, "catalogs", "gefs_catalog.csv")
  if (!file.exists(catalog_path)) {
    stop(sprintf("GEFS catalog missing: %s", catalog_path), call. = FALSE)
  }
  unified_read_csv_checked(catalog_path, "GEFS handoff catalog", "deterministic_climate/gefs_catalog")
}

unified_detclim_resolve_gefs_path <- function(handoff_root, cutoff_date, short_name, level_descriptor) {
  catalog_df <- unified_detclim_read_gefs_catalog(handoff_root)
  hit <- catalog_df[
    catalog_df$init_date == as.character(cutoff_date) &
      catalog_df$short_name == short_name &
      catalog_df$level_descriptor == level_descriptor,
    ,
    drop = FALSE
  ]
  if (nrow(hit) < 1L) {
    stop(
      sprintf(
        "GEFS handoff catalog is missing %s %s for cutoff %s.",
        short_name,
        level_descriptor,
        as.character(cutoff_date)
      ),
      call. = FALSE
    )
  }
  path <- as.character(hit$file_path[[1L]])
  if (!file.exists(path)) {
    stop(sprintf("GEFS forecast cache missing for cutoff %s: %s", as.character(cutoff_date), path), call. = FALSE)
  }
  normalizePath(path, mustWork = FALSE)
}

unified_detclim_resolve_nwm_soilsat_paths <- function(handoff_root, cutoff_date) {
  init_dir <- file.path(handoff_root, "forecast_cache", "nwm", sprintf("init_date=%s", as.character(cutoff_date)))
  paths <- list(
    short_range_land = file.path(init_dir, "product_family=short_range_land", "variable=SOILSAT_TOP_top_soil_saturation_fraction", "nwm_members.csv"),
    medium_range_land = file.path(init_dir, "product_family=medium_range_land", "variable=SOILSAT_TOP_top_soil_saturation_fraction", "nwm_members.csv"),
    long_range_land = file.path(init_dir, "product_family=long_range_land", "variable=SOILSAT_TOP_top_soil_saturation_fraction", "nwm_members.csv")
  )
  found <- vapply(paths, file.exists, logical(1))
  if (!isTRUE(found[["medium_range_land"]])) {
    stop(sprintf("NWM medium-range SOILSAT_TOP forecast cache missing for cutoff %s.", as.character(cutoff_date)), call. = FALSE)
  }
  if (!isTRUE(found[["long_range_land"]])) {
    stop(sprintf("NWM long-range SOILSAT_TOP forecast cache missing for cutoff %s.", as.character(cutoff_date)), call. = FALSE)
  }
  paths[found]
}

unified_detclim_porosity_ratios_from_triplet <- function(top_path, soil_m0_path, soil_m1_path) {
  top_info <- unified_detclim_read_member_forecast(top_path, "NWM SOILSAT_TOP")
  soil_m0_info <- unified_detclim_read_member_forecast(soil_m0_path, "NWM SOIL_M layer 0")
  soil_m1_info <- unified_detclim_read_member_forecast(soil_m1_path, "NWM SOIL_M layer 1")

  key_cols <- intersect(c("init_date", "cycle_hour", "lead_hours", "target_date"), names(top_info$data))
  if (length(key_cols) == 0L) {
    stop("Cannot estimate NWM porosity: no alignment keys were found.", call. = FALSE)
  }

  build_key <- function(df) {
    do.call(paste, c(lapply(key_cols, function(k) as.character(df[[k]])), sep = "|"))
  }
  top_key <- build_key(top_info$data)
  soil_m0_key <- build_key(soil_m0_info$data)
  soil_m1_key <- build_key(soil_m1_info$data)
  common_keys <- Reduce(intersect, list(top_key, soil_m0_key, soil_m1_key))
  if (length(common_keys) == 0L) {
    return(numeric(0))
  }

  common_members <- Reduce(intersect, list(top_info$member_cols, soil_m0_info$member_cols, soil_m1_info$member_cols))
  if (length(common_members) == 0L) {
    return(numeric(0))
  }

  top_idx <- match(common_keys, top_key)
  soil_m0_idx <- match(common_keys, soil_m0_key)
  soil_m1_idx <- match(common_keys, soil_m1_key)

  top_mat <- as.matrix(top_info$data[top_idx, common_members, drop = FALSE])
  soil_m0_mat <- as.matrix(soil_m0_info$data[soil_m0_idx, common_members, drop = FALSE])
  soil_m1_mat <- as.matrix(soil_m1_info$data[soil_m1_idx, common_members, drop = FALSE])
  storage.mode(top_mat) <- "numeric"
  storage.mode(soil_m0_mat) <- "numeric"
  storage.mode(soil_m1_mat) <- "numeric"

  top_two_vwc <- ((0.1 * soil_m0_mat) + (0.3 * soil_m1_mat)) / 0.4
  ratio <- top_two_vwc / top_mat
  keep <- is.finite(ratio) & is.finite(top_mat) & (top_mat > 0.05)
  as.numeric(ratio[keep])
}

unified_detclim_estimate_nwm_top_porosity <- function(handoff_root) {
  catalog_path <- file.path(handoff_root, "catalogs", "nwm_catalog.csv")
  if (!file.exists(catalog_path)) {
    stop(sprintf("NWM catalog missing for porosity estimation: %s", catalog_path), call. = FALSE)
  }
  catalog_df <- unified_read_csv_checked(catalog_path, "NWM handoff catalog", "deterministic_climate/porosity")
  catalog_df <- catalog_df[catalog_df$product_family == "medium_range_land", , drop = FALSE]

  layer_index_num <- suppressWarnings(as.integer(catalog_df$layer_index))
  top_df <- catalog_df[catalog_df$short_name == "SOILSAT_TOP", , drop = FALSE]
  soil_m0_df <- catalog_df[catalog_df$short_name == "SOIL_M" & is.finite(layer_index_num) & layer_index_num == 0L, , drop = FALSE]
  soil_m1_df <- catalog_df[catalog_df$short_name == "SOIL_M" & is.finite(layer_index_num) & layer_index_num == 1L, , drop = FALSE]

  init_dates <- Reduce(intersect, list(unique(top_df$init_date), unique(soil_m0_df$init_date), unique(soil_m1_df$init_date)))
  ratios <- numeric(0)
  for (init_date in sort(init_dates)) {
    top_path <- as.character(top_df$file_path[top_df$init_date == init_date][[1L]])
    soil_m0_path <- as.character(soil_m0_df$file_path[soil_m0_df$init_date == init_date][[1L]])
    soil_m1_path <- as.character(soil_m1_df$file_path[soil_m1_df$init_date == init_date][[1L]])
    ratios <- c(ratios, unified_detclim_porosity_ratios_from_triplet(top_path, soil_m0_path, soil_m1_path))
  }

  ratios <- ratios[is.finite(ratios)]
  if (length(ratios) == 0L) {
    stop("Cannot estimate NWM SOILSAT_TOP porosity: no valid SOILSAT_TOP/SOIL_M overlap rows were found.", call. = FALSE)
  }

  porosity <- stats::median(ratios, na.rm = TRUE)
  if (!is.finite(porosity) || porosity <= 0 || porosity > 1) {
    stop(sprintf("Estimated NWM porosity is not physically plausible: %s", as.character(porosity)), call. = FALSE)
  }

  list(
    porosity = porosity,
    sample_count = length(ratios),
    q10 = unname(stats::quantile(ratios, probs = 0.10, na.rm = TRUE, type = 7)),
    q90 = unname(stats::quantile(ratios, probs = 0.90, na.rm = TRUE, type = 7))
  )
}

unified_detclim_complete_future_dates <- function(series_df, cutoff_date, horizon_days, require_full_horizon, label) {
  future_dates <- seq(as.Date(cutoff_date) + 1L, by = "day", length.out = as.integer(horizon_days))
  merged <- merge(
    data.frame(date = future_dates, stringsAsFactors = FALSE),
    series_df,
    by = "date",
    all.x = TRUE,
    sort = TRUE
  )
  missing_dates <- merged$date[!is.finite(merged$value)]
  if (length(missing_dates) > 0L && isTRUE(require_full_horizon)) {
    stop(
      sprintf(
        "%s is missing %d forecast dates within the required horizon. Examples: %s",
        label,
        length(missing_dates),
        paste(head(as.character(missing_dates), 5L), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (length(missing_dates) > 0L) {
    merged <- merged[is.finite(merged$value), , drop = FALSE]
  }
  merged
}

unified_detclim_extract_observed_future <- function(observed_info, cutoff_date, horizon_days, require_full_horizon, label) {
  future_df <- observed_info$data[observed_info$data$date > as.Date(cutoff_date), c("date", "value"), drop = FALSE]
  unified_detclim_complete_future_dates(
    future_df,
    cutoff_date = cutoff_date,
    horizon_days = horizon_days,
    require_full_horizon = require_full_horizon,
    label = label
  )
}

unified_detclim_build_gefs_series <- function(handoff_root, cutoff_date, horizon_days, short_name, level_descriptor, daily_fun, reduction, require_full_horizon, label, source_label) {
  path <- unified_detclim_resolve_gefs_path(
    handoff_root = handoff_root,
    cutoff_date = cutoff_date,
    short_name = short_name,
    level_descriptor = level_descriptor
  )
  info <- unified_detclim_read_member_forecast(path, label)
  end_date <- as.Date(cutoff_date) + as.integer(horizon_days)
  daily_df <- unified_detclim_member_daily_by_date(
    df = info$data,
    member_cols = info$member_cols,
    cutoff_date = cutoff_date,
    end_date = end_date,
    daily_fun = daily_fun,
    reduction = reduction
  )
  daily_df <- unified_detclim_complete_future_dates(
    daily_df,
    cutoff_date = cutoff_date,
    horizon_days = horizon_days,
    require_full_horizon = require_full_horizon,
    label = label
  )
  daily_df$source <- source_label
  daily_df$reduction <- reduction
  daily_df$source_path <- info$source_path
  daily_df$level_descriptor <- level_descriptor
  daily_df
}

unified_detclim_build_precip_forecast <- function(handoff_root, cutoff_date, horizon_days, source = "gefs_apcp", reduction = "mean", require_full_horizon = TRUE) {
  source <- tolower(as.character(source)[[1L]])
  if (!identical(source, "gefs_apcp")) {
    stop(sprintf("Unsupported deterministic_climate precip source: %s", source), call. = FALSE)
  }
  unified_detclim_build_gefs_series(
    handoff_root = handoff_root,
    cutoff_date = cutoff_date,
    horizon_days = horizon_days,
    short_name = "APCP",
    level_descriptor = "surface",
    daily_fun = "sum",
    reduction = reduction,
    require_full_horizon = require_full_horizon,
    label = "GEFS APCP precipitation",
    source_label = "GEFS_APCP"
  )
}

unified_detclim_build_soil_forecast_nwm <- function(handoff_root, cutoff_date, horizon_days, reduction = "mean", require_full_horizon = TRUE) {
  soilsat_paths <- unified_detclim_resolve_nwm_soilsat_paths(handoff_root, cutoff_date)
  porosity_info <- unified_detclim_estimate_nwm_top_porosity(handoff_root)
  end_date <- as.Date(cutoff_date) + as.integer(horizon_days)
  family_priority <- c(short_range_land = 1L, medium_range_land = 2L, long_range_land = 3L)

  family_rows <- list()
  for (family_name in names(soilsat_paths)) {
    info <- unified_detclim_read_member_forecast(soilsat_paths[[family_name]], sprintf("NWM %s SOILSAT_TOP", family_name))
    daily_df <- unified_detclim_member_daily_by_date(
      df = info$data,
      member_cols = info$member_cols,
      cutoff_date = cutoff_date,
      end_date = end_date,
      daily_fun = "mean",
      reduction = reduction
    )
    if (nrow(daily_df) == 0L) next
    daily_df$value <- daily_df$value * porosity_info$porosity
    daily_df$source_family <- family_name
    daily_df$priority <- family_priority[[family_name]]
    daily_df$source_path <- info$source_path
    family_rows[[family_name]] <- daily_df
  }

  if (length(family_rows) == 0L) {
    stop(sprintf("No NWM SOILSAT_TOP forecast rows were available after cutoff %s.", as.character(cutoff_date)), call. = FALSE)
  }

  combined <- do.call(rbind, family_rows)
  combined <- combined[order(combined$date, combined$priority), , drop = FALSE]
  combined <- combined[!duplicated(combined$date), , drop = FALSE]
  combined <- unified_detclim_complete_future_dates(
    combined,
    cutoff_date = cutoff_date,
    horizon_days = horizon_days,
    require_full_horizon = require_full_horizon,
    label = "NWM SOILSAT_TOP deterministic soil"
  )
  combined$source <- "NWM_SOILSAT_TOP"
  combined$reduction <- reduction
  combined$porosity <- porosity_info$porosity
  combined$porosity_q10 <- porosity_info$q10
  combined$porosity_q90 <- porosity_info$q90
  combined$porosity_sample_count <- porosity_info$sample_count

  list(
    daily = combined,
    porosity_info = porosity_info,
    selected_families = family_rows
  )
}

unified_detclim_build_soil_forecast <- function(handoff_root, cutoff_date, horizon_days, source = "nwm_soilsat_top", reduction = "mean", require_full_horizon = TRUE) {
  source <- tolower(as.character(source)[[1L]])
  if (identical(source, "gefs_soilw_0_0.1m")) {
    daily_df <- unified_detclim_build_gefs_series(
      handoff_root = handoff_root,
      cutoff_date = cutoff_date,
      horizon_days = horizon_days,
      short_name = "SOILW",
      level_descriptor = "0-0.1 m below ground",
      daily_fun = "mean",
      reduction = reduction,
      require_full_horizon = require_full_horizon,
      label = "GEFS SOILW 0-0.1 m below ground soil",
      source_label = "GEFS_SOILW_0_0.1M"
    )
    return(list(
      daily = daily_df,
      porosity_info = NULL,
      selected_families = list(gefs_soilw_0_0.1m = daily_df)
    ))
  }
  if (!identical(source, "nwm_soilsat_top")) {
    stop(sprintf("Unsupported deterministic_climate soil source: %s", source), call. = FALSE)
  }
  unified_detclim_build_soil_forecast_nwm(
    handoff_root = handoff_root,
    cutoff_date = cutoff_date,
    horizon_days = horizon_days,
    reduction = reduction,
    require_full_horizon = require_full_horizon
  )
}

unified_detclim_write_spliced_series <- function(observed_info, future_df, cutoff_date, output_path) {
  history_df <- observed_info$data[observed_info$data$date <= as.Date(cutoff_date), , drop = FALSE]
  if (nrow(history_df) == 0L) {
    stop(sprintf("Observed history for %s is empty at or before cutoff %s.", output_path, as.character(cutoff_date)), call. = FALSE)
  }

  out_df <- rbind(history_df, future_df[, c("date", "value"), drop = FALSE])
  out_df <- out_df[order(out_df$date), , drop = FALSE]
  if (any(duplicated(out_df$date))) {
    stop(sprintf("deterministic_climate produced duplicated dates for %s", output_path), call. = FALSE)
  }

  write_df <- setNames(
    data.frame(
      out_df$date,
      out_df$value,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    c(observed_info$date_col, observed_info$value_col)
  )
  utils::write.csv(write_df, output_path, row.names = FALSE)

  list(
    output_path = normalizePath(output_path, mustWork = FALSE),
    rows_written = nrow(write_df),
    history_rows = nrow(history_df),
    future_rows = nrow(future_df)
  )
}

unified_materialize_deterministic_climate_covariates <- function(cfg, shared_paths, cov_path_map, repo_root) {
  det_cfg <- unified_get(cfg, c("inputs", "deterministic_climate"), default = list())
  if (!isTRUE(unified_get(det_cfg, c("enabled"), default = FALSE))) {
    return(NULL)
  }

  required_covs <- c("ppt", "soil", "pca")
  missing_covs <- required_covs[!nzchar(unlist(cov_path_map[required_covs], use.names = FALSE))]
  if (length(missing_covs) > 0L) {
    stop(
      sprintf(
        "deterministic_climate requires run-scoped covariates for: %s",
        paste(missing_covs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  cutoff_date <- suppressWarnings(as.Date(unified_get(cfg, c("dates", "cutoff_date"), default = NA_character_)))
  if (is.na(cutoff_date)) {
    stop("dates.cutoff_date must be a valid date when deterministic climate substitution is enabled.", call. = FALSE)
  }

  handoff_root <- unified_detclim_resolve_handoff_root(unified_get(det_cfg, c("handoff_root"), default = ""))
  horizon_days <- unified_detclim_forecast_horizon_days(
    glofas_csv = shared_paths$glofas,
    cutoff_date = cutoff_date,
    override_days = unified_get(det_cfg, c("horizon_days"), default = NULL)
  )
  require_full_horizon <- isTRUE(unified_get(det_cfg, c("require_full_horizon"), default = TRUE))
  precip_cfg <- detclim_normalize_series_cfg(det_cfg, "precip")
  soil_cfg <- detclim_normalize_series_cfg(det_cfg, "soil")

  ppt_observed <- unified_detclim_read_observed_series(
    cov_path_map$ppt,
    label = "run-scoped precipitation covariate",
    value_candidates = c("PRCP_mm", "ppt")
  )
  soil_observed <- unified_detclim_read_observed_series(
    cov_path_map$soil,
    label = "run-scoped soil covariate",
    value_candidates = c("Daily_Avg_Soil_Moisture", "soil", "average_soil_moisture")
  )

  ppt_observed_future <- unified_detclim_extract_observed_future(
    observed_info = ppt_observed,
    cutoff_date = cutoff_date,
    horizon_days = horizon_days,
    require_full_horizon = require_full_horizon,
    label = "observed precipitation future"
  )
  soil_observed_future <- unified_detclim_extract_observed_future(
    observed_info = soil_observed,
    cutoff_date = cutoff_date,
    horizon_days = horizon_days,
    require_full_horizon = require_full_horizon,
    label = "observed soil future"
  )

  precip_forecast <- unified_detclim_build_precip_forecast(
    handoff_root = handoff_root,
    cutoff_date = cutoff_date,
    horizon_days = horizon_days,
    source = precip_cfg$source,
    reduction = precip_cfg$reduction,
    require_full_horizon = require_full_horizon
  )
  soil_forecast <- unified_detclim_build_soil_forecast(
    handoff_root = handoff_root,
    cutoff_date = cutoff_date,
    horizon_days = horizon_days,
    source = soil_cfg$source,
    reduction = soil_cfg$reduction,
    require_full_horizon = require_full_horizon
  )

  precip_future_debug <- detclim_compose_future_series(
    observed_df = ppt_observed_future,
    forecast_df = precip_forecast[, c("date", "value"), drop = FALSE],
    observed_weight = if (isTRUE(precip_cfg$observed_blend$enabled)) precip_cfg$observed_blend$observed_weight else 0,
    noise_sd = if (isTRUE(precip_cfg$noisy_blend$enabled)) precip_cfg$noisy_blend$noise_sd else 0,
    noise_seed = precip_cfg$noisy_blend$noise_seed,
    floor_at_zero = isTRUE(precip_cfg$noisy_blend$floor_at_zero),
    noise_distribution = precip_cfg$noisy_blend$noise_distribution,
    observed_zero_stay_prob = if (isTRUE(precip_cfg$observed_blend$enabled)) precip_cfg$observed_blend$observed_zero_stay_prob else NULL,
    observed_zero_stay_seed = precip_cfg$observed_blend$observed_zero_stay_seed,
    label = sprintf("precip|%s|%s", as.character(cutoff_date), precip_cfg$source)
  )
  precip_future_debug$source <- precip_forecast$source
  precip_future_debug$reduction <- precip_forecast$reduction
  precip_future_debug$source_path <- precip_forecast$source_path
  precip_future_debug$level_descriptor <- precip_forecast$level_descriptor
  precip_future_debug$final_value <- if (isTRUE(precip_cfg$observed_blend$enabled)) precip_future_debug$blended_value_effective else precip_future_debug$noisy_forecast_value
  if (isTRUE(precip_cfg$observed_blend$enabled) && !isTRUE(precip_cfg$noisy_blend$enabled)) {
    precip_future_debug$final_value <- precip_future_debug$blended_value_effective
  } else if (!isTRUE(precip_cfg$observed_blend$enabled) && !isTRUE(precip_cfg$noisy_blend$enabled)) {
    precip_future_debug$final_value <- precip_future_debug$forecast_value
  } else if (!isTRUE(precip_cfg$observed_blend$enabled) && isTRUE(precip_cfg$noisy_blend$enabled)) {
    precip_future_debug$final_value <- precip_future_debug$noisy_forecast_value
  }
  dry_day_threshold_mm <- suppressWarnings(as.numeric(precip_cfg$dry_day_threshold_mm %||% 0))
  if (is.finite(dry_day_threshold_mm) && dry_day_threshold_mm > 0) {
    precip_future_debug$final_value <- ifelse(precip_future_debug$final_value < dry_day_threshold_mm, 0, precip_future_debug$final_value)
  }
  precip_future <- precip_future_debug[, c("date", "final_value"), drop = FALSE]
  names(precip_future)[[2L]] <- "value"

  soil_future_debug <- detclim_compose_future_series(
    observed_df = soil_observed_future,
    forecast_df = soil_forecast$daily[, c("date", "value"), drop = FALSE],
    observed_weight = if (isTRUE(soil_cfg$observed_blend$enabled)) soil_cfg$observed_blend$observed_weight else 0,
    noise_sd = if (isTRUE(soil_cfg$noisy_blend$enabled)) soil_cfg$noisy_blend$noise_sd else 0,
    noise_seed = soil_cfg$noisy_blend$noise_seed,
    floor_at_zero = isTRUE(soil_cfg$noisy_blend$floor_at_zero),
    noise_distribution = soil_cfg$noisy_blend$noise_distribution,
    observed_zero_stay_prob = if (isTRUE(soil_cfg$observed_blend$enabled)) soil_cfg$observed_blend$observed_zero_stay_prob else NULL,
    observed_zero_stay_seed = soil_cfg$observed_blend$observed_zero_stay_seed,
    label = sprintf("soil|%s|%s", as.character(cutoff_date), soil_cfg$source)
  )
  soil_future_debug$source <- soil_forecast$daily$source
  soil_future_debug$reduction <- soil_forecast$daily$reduction
  soil_future_debug$source_path <- soil_forecast$daily$source_path
  soil_future_debug$level_descriptor <- soil_forecast$daily$level_descriptor
  soil_future_debug$final_value <- if (isTRUE(soil_cfg$observed_blend$enabled)) soil_future_debug$blended_value_effective else soil_future_debug$noisy_forecast_value
  if (isTRUE(soil_cfg$observed_blend$enabled) && !isTRUE(soil_cfg$noisy_blend$enabled)) {
    soil_future_debug$final_value <- soil_future_debug$blended_value_effective
  } else if (!isTRUE(soil_cfg$observed_blend$enabled) && !isTRUE(soil_cfg$noisy_blend$enabled)) {
    soil_future_debug$final_value <- soil_future_debug$forecast_value
  } else if (!isTRUE(soil_cfg$observed_blend$enabled) && isTRUE(soil_cfg$noisy_blend$enabled)) {
    soil_future_debug$final_value <- soil_future_debug$noisy_forecast_value
  }
  soil_future <- soil_future_debug[, c("date", "final_value"), drop = FALSE]
  names(soil_future)[[2L]] <- "value"

  ppt_write <- unified_detclim_write_spliced_series(
    observed_info = ppt_observed,
    future_df = precip_future,
    cutoff_date = cutoff_date,
    output_path = cov_path_map$ppt
  )
  soil_write <- unified_detclim_write_spliced_series(
    observed_info = soil_observed,
    future_df = soil_future,
    cutoff_date = cutoff_date,
    output_path = cov_path_map$soil
  )

  debug_root <- file.path(shared_paths$root, "deterministic_climate")
  dir.create(debug_root, recursive = TRUE, showWarnings = FALSE)

  precip_debug_path <- file.path(debug_root, "deterministic_precip_future.csv")
  soil_debug_path <- file.path(debug_root, "deterministic_soil_future.csv")
  soil_family_path <- file.path(debug_root, "deterministic_soil_family_support.csv")
  summary_path <- file.path(debug_root, "deterministic_climate_summary.txt")

  utils::write.csv(precip_future_debug, precip_debug_path, row.names = FALSE)
  utils::write.csv(soil_future_debug, soil_debug_path, row.names = FALSE)
  soil_family_df <- do.call(
    rbind,
    lapply(names(soil_forecast$selected_families), function(fam) {
      df <- soil_forecast$selected_families[[fam]]
      data.frame(
        date = df$date,
        value = df$value,
        source_family = fam,
        source_path = df$source_path,
        stringsAsFactors = FALSE
      )
    })
  )
  utils::write.csv(soil_family_df, soil_family_path, row.names = FALSE)

  summary_lines <- c(
    sprintf("enabled=TRUE"),
    sprintf("handoff_root=%s", handoff_root),
    sprintf("cutoff_date=%s", as.character(cutoff_date)),
    sprintf("forecast_start_date=%s", as.character(cutoff_date + 1L)),
    sprintf("horizon_days=%d", as.integer(horizon_days)),
    sprintf("require_full_horizon=%s", if (require_full_horizon) "TRUE" else "FALSE"),
    sprintf("precip_source=%s", precip_cfg$source),
    sprintf("precip_reduction=%s", precip_cfg$reduction),
    sprintf("precip_noisy_blend.enabled=%s", if (isTRUE(precip_cfg$noisy_blend$enabled)) "TRUE" else "FALSE"),
    sprintf("precip_noisy_blend.noise_sd=%s", as.character(precip_cfg$noisy_blend$noise_sd)),
    sprintf("precip_noisy_blend.noise_distribution=%s", as.character(precip_cfg$noisy_blend$noise_distribution)),
    sprintf("precip_noisy_blend.noise_seed_effective=%s", as.character(precip_future_debug$noise_seed_effective[[1L]])),
    sprintf("precip_observed_blend.enabled=%s", if (isTRUE(precip_cfg$observed_blend$enabled)) "TRUE" else "FALSE"),
    sprintf("precip_observed_blend.observed_weight=%s", as.character(precip_cfg$observed_blend$observed_weight)),
    sprintf("precip_observed_blend.observed_zero_stay_prob=%s", as.character(precip_cfg$observed_blend$observed_zero_stay_prob)),
    sprintf("precip_observed_blend.observed_zero_stay_seed_effective=%s", as.character(precip_future_debug$observed_zero_stay_seed_effective[[1L]])),
    sprintf("precip_output=%s", cov_path_map$ppt),
    sprintf("soil_source=%s", soil_cfg$source),
    sprintf("soil_reduction=%s", soil_cfg$reduction),
    sprintf("soil_noisy_blend.enabled=%s", if (isTRUE(soil_cfg$noisy_blend$enabled)) "TRUE" else "FALSE"),
    sprintf("soil_noisy_blend.noise_sd=%s", as.character(soil_cfg$noisy_blend$noise_sd)),
    sprintf("soil_noisy_blend.noise_distribution=%s", as.character(soil_cfg$noisy_blend$noise_distribution)),
    sprintf("soil_noisy_blend.noise_seed_effective=%s", as.character(soil_future_debug$noise_seed_effective[[1L]])),
    sprintf("soil_observed_blend.enabled=%s", if (isTRUE(soil_cfg$observed_blend$enabled)) "TRUE" else "FALSE"),
    sprintf("soil_observed_blend.observed_weight=%s", as.character(soil_cfg$observed_blend$observed_weight)),
    sprintf("soil_observed_blend.observed_zero_stay_prob=%s", as.character(soil_cfg$observed_blend$observed_zero_stay_prob)),
    sprintf("soil_observed_blend.observed_zero_stay_seed_effective=%s", as.character(soil_future_debug$observed_zero_stay_seed_effective[[1L]])),
    sprintf("soil_output=%s", cov_path_map$soil),
    sprintf("nwm_soilsat_top_porosity=%s", if (!is.null(soil_forecast$porosity_info)) sprintf("%.6f", soil_forecast$porosity_info$porosity) else NA_character_),
    sprintf("nwm_soilsat_top_porosity_q10=%s", if (!is.null(soil_forecast$porosity_info)) sprintf("%.6f", soil_forecast$porosity_info$q10) else NA_character_),
    sprintf("nwm_soilsat_top_porosity_q90=%s", if (!is.null(soil_forecast$porosity_info)) sprintf("%.6f", soil_forecast$porosity_info$q90) else NA_character_),
    sprintf("nwm_soilsat_top_porosity_samples=%s", if (!is.null(soil_forecast$porosity_info)) as.character(soil_forecast$porosity_info$sample_count) else NA_character_),
    sprintf("ppt_history_rows=%d", ppt_write$history_rows),
    sprintf("ppt_future_rows=%d", ppt_write$future_rows),
    sprintf("soil_history_rows=%d", soil_write$history_rows),
    sprintf("soil_future_rows=%d", soil_write$future_rows),
    sprintf("pca_passthrough=%s", cov_path_map$pca)
  )
  writeLines(summary_lines, con = summary_path)

  list(
    updated_covariates = c(ppt = cov_path_map$ppt, soil = cov_path_map$soil),
    debug_artifacts = c(precip_debug_path, soil_debug_path, soil_family_path, summary_path),
    debug_artifact_paths = list(
      summary_path = normalizePath(summary_path, mustWork = FALSE),
      precip_future_path = normalizePath(precip_debug_path, mustWork = FALSE),
      soil_future_path = normalizePath(soil_debug_path, mustWork = FALSE),
      soil_family_support_path = normalizePath(soil_family_path, mustWork = FALSE)
    ),
    horizon_days = horizon_days,
    cutoff_date = cutoff_date,
    handoff_root = handoff_root,
    porosity_info = soil_forecast$porosity_info,
    require_full_horizon = require_full_horizon,
    precip_enabled = isTRUE(precip_cfg$enabled),
    soil_enabled = isTRUE(soil_cfg$enabled),
    precip_reduction = precip_cfg$reduction,
    soil_reduction = soil_cfg$reduction,
    precip_source = precip_cfg$source,
    soil_source = soil_cfg$source,
    precip_blend_cfg = precip_cfg,
    soil_blend_cfg = soil_cfg,
    ppt_history_rows = ppt_write$history_rows,
    ppt_future_rows = ppt_write$future_rows,
    soil_history_rows = soil_write$history_rows,
    soil_future_rows = soil_write$future_rows
  )
}
