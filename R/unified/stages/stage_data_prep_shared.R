# unified/stages/stage_data_prep_shared.R

unified_stage_data_prep_shared <- function(cfg, run_root, repo_root, manifest) {
  shared_paths <- unified_shared_input_paths(run_root)
  shared_root <- shared_paths$root
  parameters_dir <- file.path(shared_root, "parameters")
  retros_dir <- file.path(shared_root, "retros")
  forecasts_dir <- file.path(shared_root, "forecasts")
  usgs_dir <- dirname(shared_paths$usgs)
  covariates_dir <- file.path(shared_root, "covariates")

  dir.create(parameters_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(retros_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(forecasts_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(usgs_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(covariates_dir, recursive = TRUE, showWarnings = FALSE)
  shared_storage_scales <- list()
  shared_cov_paths <- list(
    eli = "",
    oni = "",
    ppt = "",
    soil = "",
    pca = ""
  )

  add_shared_file <- function(src_path, dst_path, storage_scale, role = "shared_input") {
    if (is.null(src_path) || !nzchar(src_path) || !file.exists(src_path)) {
      stop(sprintf("data_prep_shared input missing: %s", as.character(src_path)), call. = FALSE)
    }
    dir.create(dirname(dst_path), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(src_path, dst_path, overwrite = TRUE)
    if (!isTRUE(ok) || !file.exists(dst_path)) {
      stop(sprintf("failed to copy shared input: %s -> %s", src_path, dst_path), call. = FALSE)
    }

    manifest <<- unified_manifest_add_artifact(
      manifest,
      normalizePath(dst_path, mustWork = FALSE),
      storage_scale = storage_scale,
      role = role
    )
    shared_storage_scales[[normalizePath(dst_path, mustWork = FALSE)]] <<- storage_scale
    manifest$inputs[[length(manifest$inputs) + 1L]] <<- list(
      path = normalizePath(dst_path, mustWork = FALSE),
      sha256 = unified_sha256(dst_path),
      storage_scale = storage_scale
    )
  }

  refresh_shared_manifest_entry <- function(path, role = "shared_input") {
    npath <- normalizePath(path, mustWork = FALSE)
    storage_scale <- shared_storage_scales[[npath]]
    if (is.null(storage_scale) || !nzchar(storage_scale)) {
      storage_scale <- "table_csv"
    }
    manifest <<- unified_manifest_add_artifact(
      manifest,
      npath,
      storage_scale = storage_scale,
      role = role
    )
    manifest$inputs[[length(manifest$inputs) + 1L]] <<- list(
      path = npath,
      sha256 = unified_sha256(path),
      storage_scale = storage_scale
    )
  }

  assign_cov_path <- function(cov_name, cov_path) {
    key <- tolower(as.character(cov_name))
    if (grepl("eli", key, fixed = TRUE)) shared_cov_paths$eli <<- cov_path
    if (grepl("oni", key, fixed = TRUE)) shared_cov_paths$oni <<- cov_path
    if (grepl("ppt", key, fixed = TRUE) || grepl("precip", key, fixed = TRUE)) shared_cov_paths$ppt <<- cov_path
    if (grepl("soil", key, fixed = TRUE)) shared_cov_paths$soil <<- cov_path
    if (grepl("pca", key, fixed = TRUE)) shared_cov_paths$pca <<- cov_path
  }

  detect_date_info <- function(df, label, path, required) {
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
    if (isTRUE(required)) {
      stop(
        sprintf(
          "data_prep_shared date filtering requires a parseable date column for %s: %s",
          label, path
        ),
        call. = FALSE
      )
    }
    NULL
  }

  assert_csv_daily_continuity <- function(path, label) {
    df <- unified_read_csv_checked(path, label, "data_prep_shared/continuity")
    date_info <- detect_date_info(df, label, path, required = TRUE)
    d <- sort(unique(as.Date(date_info$dates)))
    d <- d[!is.na(d)]
    if (length(d) < 2L) return(invisible(TRUE))
    gaps <- as.integer(diff(d))
    bad <- which(gaps > 1L)
    if (length(bad) == 0L) return(invisible(TRUE))
    max_gap <- max(gaps[bad], na.rm = TRUE)
    samples <- head(
      sprintf("%s->%s (%dd)", as.character(d[bad]), as.character(d[bad + 1L]), gaps[bad]),
      5L
    )
    stop(
      sprintf(
        paste0(
          "data_prep_shared detected non-daily gaps in %s (%s): ",
          "%d gaps, max gap %d days. Examples: %s"
        ),
        label,
        path,
        length(bad),
        max_gap,
        paste(samples, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  guess_storage_scale <- function(path) {
    npath <- normalizePath(path, mustWork = FALSE)
    if (grepl("/parameters/parameters\\.txt$", npath)) {
      return("parameters_text")
    }
    if (grepl("/retros/retros\\.csv$", npath)) {
      return(as.character(cfg$inputs$fit$retros_storage_scale %||% "log1p_cms"))
    }
    if (grepl("/forecasts/nws_forecast\\.csv$", npath)) {
      return(as.character(cfg$inputs$fit$nws_storage_scale %||% "raw_cms"))
    }
    if (grepl("/forecasts/glofas_forecast\\.csv$", npath)) {
      return(as.character(cfg$inputs$fit$glofas_storage_scale %||% "raw_cms"))
    }
    if (grepl("/usgs/usgs_daily\\.csv$", npath)) {
      return("raw_cms")
    }
    if (grepl("\\.csv$", npath, ignore.case = TRUE)) {
      return("table_csv")
    }
    "text"
  }

  parse_detclim_summary_file <- function(summary_path) {
    if (!nzchar(summary_path) || !file.exists(summary_path)) {
      return(list())
    }
    lines <- readLines(summary_path, warn = FALSE)
    lines <- lines[nzchar(lines)]
    out <- list()
    for (line in lines) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1L]]
      if (length(parts) < 2L) next
      key <- trimws(parts[[1L]])
      val <- paste(parts[-1L], collapse = "=")
      out[[key]] <- val
    }
    out
  }

  parse_detclim_flag <- function(x, default = FALSE) {
    if (is.null(x) || !length(x)) return(isTRUE(default))
    val <- tolower(trimws(as.character(x[[1L]])))
    if (!nzchar(val) || identical(val, "na")) return(isTRUE(default))
    val %in% c("true", "t", "1", "yes", "y")
  }

  parse_detclim_int <- function(x, default = NA_integer_) {
    if (is.null(x) || !length(x)) return(as.integer(default))
    out <- suppressWarnings(as.integer(x[[1L]]))
    if (!is.finite(out)) return(as.integer(default))
    out
  }

  parse_detclim_num <- function(x, default = NA_real_) {
    if (is.null(x) || !length(x)) return(as.numeric(default))
    txt <- trimws(as.character(x[[1L]]))
    if (!nzchar(txt) || identical(tolower(txt), "na")) return(as.numeric(default))
    out <- suppressWarnings(as.numeric(txt))
    if (!is.finite(out)) return(as.numeric(default))
    out
  }

  hydrate_exact_snapshot_detclim_manifest <- function(manifest) {
    det_dir <- file.path(shared_root, "deterministic_climate")
    summary_path <- file.path(det_dir, "deterministic_climate_summary.txt")
    precip_future_path <- file.path(det_dir, "deterministic_precip_future.csv")
    soil_future_path <- file.path(det_dir, "deterministic_soil_future.csv")
    soil_family_support_path <- file.path(det_dir, "deterministic_soil_family_support.csv")
    if (!file.exists(summary_path)) {
      return(manifest)
    }

    cov_paths <- list.files(covariates_dir, full.names = TRUE)
    for (cov_path in cov_paths) {
      if (!file.exists(cov_path)) next
      assign_cov_path(basename(cov_path), cov_path)
    }

    det_summary <- parse_detclim_summary_file(summary_path)
    manifest$deterministic_climate <- list(
      enabled = parse_detclim_flag(det_summary$enabled, default = TRUE),
      handoff_root = if (!is.null(det_summary$handoff_root) && nzchar(det_summary$handoff_root)) {
        det_summary$handoff_root
      } else {
        unified_get(cfg, c("inputs", "deterministic_climate", "handoff_root"), default = NULL)
      },
      horizon_days = parse_detclim_int(
        det_summary$horizon_days,
        default = unified_get(cfg, c("inputs", "deterministic_climate", "horizon_days"), default = NA_integer_)
      ),
      require_full_horizon = parse_detclim_flag(
        det_summary$require_full_horizon,
        default = unified_get(cfg, c("inputs", "deterministic_climate", "require_full_horizon"), default = TRUE)
      ),
      cutoff_date = if (!is.null(det_summary$cutoff_date) && nzchar(det_summary$cutoff_date)) det_summary$cutoff_date else NA_character_,
      summary_path = summary_path,
      summary_sha256 = unified_sha256(summary_path),
      precip_future_path = if (file.exists(precip_future_path)) precip_future_path else NA_character_,
      soil_future_path = if (file.exists(soil_future_path)) soil_future_path else NA_character_,
      soil_family_support_path = if (file.exists(soil_family_support_path)) soil_family_support_path else NA_character_,
      precip = list(
        enabled = TRUE,
        source = if (!is.null(det_summary$precip_source) && nzchar(det_summary$precip_source)) {
          det_summary$precip_source
        } else {
          unified_get(cfg, c("inputs", "deterministic_climate", "precip", "source"), default = "gefs_apcp")
        },
        reduction = if (!is.null(det_summary$precip_reduction) && nzchar(det_summary$precip_reduction)) {
          det_summary$precip_reduction
        } else {
          unified_get(cfg, c("inputs", "deterministic_climate", "precip", "reduction"), default = "mean")
        },
        output_path = if (nzchar(shared_cov_paths$ppt)) shared_cov_paths$ppt else NA_character_,
        history_rows = parse_detclim_int(det_summary$ppt_history_rows),
        future_rows = parse_detclim_int(det_summary$ppt_future_rows)
      ),
      soil = list(
        enabled = TRUE,
        source = if (!is.null(det_summary$soil_source) && nzchar(det_summary$soil_source)) {
          det_summary$soil_source
        } else {
          unified_get(cfg, c("inputs", "deterministic_climate", "soil", "source"), default = "nwm_soilsat_top")
        },
        reduction = if (!is.null(det_summary$soil_reduction) && nzchar(det_summary$soil_reduction)) {
          det_summary$soil_reduction
        } else {
          unified_get(cfg, c("inputs", "deterministic_climate", "soil", "reduction"), default = "mean")
        },
        output_path = if (nzchar(shared_cov_paths$soil)) shared_cov_paths$soil else NA_character_,
        history_rows = parse_detclim_int(det_summary$soil_history_rows),
        future_rows = parse_detclim_int(det_summary$soil_future_rows),
        porosity = parse_detclim_num(det_summary$nwm_soilsat_top_porosity),
        porosity_q10 = parse_detclim_num(det_summary$nwm_soilsat_top_porosity_q10),
        porosity_q90 = parse_detclim_num(det_summary$nwm_soilsat_top_porosity_q90),
        porosity_sample_count = parse_detclim_int(det_summary$nwm_soilsat_top_porosity_samples)
      ),
      pca = list(
        mode = "passthrough",
        output_path = if (nzchar(shared_cov_paths$pca)) shared_cov_paths$pca else NA_character_
      ),
      verified_in_data_prep_shared = TRUE
    )
    manifest
  }

  copy_exact_shared_snapshot <- function(source_root, target_root) {
    source_root <- normalizePath(path.expand(as.character(source_root)), mustWork = TRUE)
    if (!dir.exists(source_root)) {
      stop(sprintf("inputs.shared.exact_source_snapshot_root is not a directory: %s", source_root), call. = FALSE)
    }
    if (dir.exists(target_root)) {
      unlink(target_root, recursive = TRUE, force = TRUE)
    }
    dir.create(target_root, recursive = TRUE, showWarnings = FALSE)
    rel_paths <- list.files(
      source_root,
      recursive = TRUE,
      all.files = TRUE,
      no.. = TRUE,
      full.names = FALSE,
      include.dirs = TRUE
    )
    if (length(rel_paths) < 1L) {
      stop(sprintf("exact shared snapshot root is empty: %s", source_root), call. = FALSE)
    }
    src_full <- file.path(source_root, rel_paths)
    info <- file.info(src_full)
    is_dir <- !is.na(info$isdir) & info$isdir
    dir_paths <- rel_paths[is_dir]
    file_paths <- rel_paths[!is_dir]
    for (rel in dir_paths) {
      dir.create(file.path(target_root, rel), recursive = TRUE, showWarnings = FALSE)
    }
    copied_files <- character(0)
    for (rel in file_paths) {
      src <- file.path(source_root, rel)
      dst <- file.path(target_root, rel)
      dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(src, dst, overwrite = TRUE)
      if (!isTRUE(ok) || !file.exists(dst)) {
        stop(sprintf("failed to preserve exact shared snapshot file: %s -> %s", src, dst), call. = FALSE)
      }
      copied_files <- c(copied_files, normalizePath(dst, mustWork = FALSE))
    }
    copied_files
  }

  exact_snapshot_root_raw <- unified_get(
    cfg,
    c("inputs", "shared", "exact_source_snapshot_root"),
    default = ""
  )
  exact_snapshot_root <- if (is.null(exact_snapshot_root_raw) || length(exact_snapshot_root_raw) < 1L) {
    ""
  } else {
    out <- as.character(exact_snapshot_root_raw[[1L]])
    if (!length(out) || is.na(out[[1L]])) "" else out[[1L]]
  }
  if (nzchar(exact_snapshot_root)) {
    copied_files <- copy_exact_shared_snapshot(exact_snapshot_root, shared_root)
    exact_usgs_resolution <- unified_resolve_usgs_daily_path(cfg, snapshot_root = exact_snapshot_root)
    if (!file.exists(shared_paths$usgs) && nzchar(exact_usgs_resolution$path)) {
      dir.create(dirname(shared_paths$usgs), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(exact_usgs_resolution$path, shared_paths$usgs, overwrite = TRUE)
      if (!isTRUE(ok) || !file.exists(shared_paths$usgs)) {
        stop(
          sprintf(
            "failed to supplement exact shared snapshot with local USGS truth: %s -> %s",
            exact_usgs_resolution$path,
            shared_paths$usgs
          ),
          call. = FALSE
        )
      }
      copied_files <- c(copied_files, normalizePath(shared_paths$usgs, mustWork = FALSE))
    }
    manifest$shared_snapshot <- list(
      mode = "exact_copy",
      source_root = normalizePath(path.expand(exact_snapshot_root), mustWork = TRUE),
      copied_files = as.integer(length(copied_files)),
      usgs_origin = if (nzchar(exact_usgs_resolution$path)) exact_usgs_resolution$origin else ""
    )
    for (copied_path in copied_files) {
      storage_scale <- guess_storage_scale(copied_path)
      manifest <- unified_manifest_add_artifact(
        manifest,
        copied_path,
        storage_scale = storage_scale,
        role = "shared_input"
      )
      manifest$inputs[[length(manifest$inputs) + 1L]] <- list(
        path = copied_path,
        sha256 = unified_sha256(copied_path),
        storage_scale = storage_scale
      )
    }

    manifest <- hydrate_exact_snapshot_detclim_manifest(manifest)

    cov_required <- list.files(covariates_dir, full.names = TRUE)
    if (length(cov_required) == 0L) cov_required <- character(0)
    assert_csv_daily_continuity(shared_paths$retros, "retros")
    unified_validate_required_shared_inputs(
      run_root = run_root,
      stage_name = "data_prep_shared",
      manifest = manifest,
      enabled_models = cfg$models,
      required_covariates = cov_required,
      required_usgs = isTRUE(cfg$stages$post)
    )
    message(sprintf(
      "data_prep_shared: preserved exact shared snapshot from %s",
      normalizePath(path.expand(exact_snapshot_root), mustWork = TRUE)
    ))
    return(list(manifest = manifest))
  }

  snapshot_dest_rel <- cfg$inputs$forecats$snapshot$dest_rel
  if (is.null(snapshot_dest_rel) || !nzchar(snapshot_dest_rel)) {
    snapshot_dest_rel <- "inputs/shared/forecats_bundle"
  }
  snapshot_root <- file.path(run_root, snapshot_dest_rel)
  prefer_snapshot <- isTRUE(cfg$inputs$shared$prefer_forecats_snapshot)
  snapshot_retros <- file.path(snapshot_root, "retros.csv")
  snapshot_nws <- file.path(snapshot_root, "nws_forecast.csv")
  snapshot_glofas <- file.path(snapshot_root, "glofas_forecast.csv")
  snapshot_ready <- file.exists(snapshot_retros) && file.exists(snapshot_nws) && file.exists(snapshot_glofas)
  snapshot_valid <- TRUE

  cutoff_date <- suppressWarnings(as.Date(unified_get(cfg, c("dates", "cutoff_date"), default = NA_character_)))
  forecast_start_date <- if (!is.na(cutoff_date)) cutoff_date + 1L else as.Date(NA)

  source_retros <- cfg$inputs$fit$retros_path
  source_nws <- cfg$inputs$fit$nws_forecast_path
  source_glofas <- cfg$inputs$fit$glofas_forecast_path
  source_retros_scale <- cfg$inputs$fit$retros_storage_scale
  source_nws_scale <- cfg$inputs$fit$nws_storage_scale
  source_glofas_scale <- cfg$inputs$fit$glofas_storage_scale
  source_usgs <- ""
  source_mode <- "configured"
  source_retros_origin <- "configured"
  source_nws_origin <- "configured"
  source_glofas_origin <- "configured"
  source_usgs_origin <- "unresolved"

  if (prefer_snapshot && snapshot_ready && !is.na(cutoff_date)) {
    snapshot_valid <- tryCatch(
      {
        unified_validate_forecast_window_csv(
          snapshot_glofas,
          label = "forecats snapshot glofas_forecast",
          stage_name = "data_prep_shared/source_glofas_snapshot",
          cutoff_date = cutoff_date
        )
        unified_validate_forecast_window_csv(
          snapshot_nws,
          label = "forecats snapshot nws_forecast",
          stage_name = "data_prep_shared/source_nws_snapshot",
          cutoff_date = cutoff_date
        )
        TRUE
      },
      error = function(e) {
        warning(
          sprintf(
            "data_prep_shared: forecats snapshot failed cutoff-date validation; falling back to configured paths. Details: %s",
            conditionMessage(e)
          ),
          call. = FALSE
        )
        FALSE
      }
    )
    if (!isTRUE(snapshot_valid)) snapshot_ready <- FALSE
  }

  if (prefer_snapshot && snapshot_ready) {
    source_mode <- "forecats_snapshot_mixed"
    source_retros <- snapshot_retros
    source_retros_origin <- "snapshot"
    if (is.null(source_retros_scale) || !nzchar(as.character(source_retros_scale))) {
      source_retros_scale <- "log1p_cms"
    }
    source_glofas <- snapshot_glofas
    source_glofas_scale <- "raw_cms"
    source_glofas_origin <- "snapshot"

    nws_snapshot_ok <- tryCatch(
      {
        unified_validate_forecast_numeric_csv(
          snapshot_nws,
          label = "forecats snapshot nws_forecast",
          stage_name = "data_prep_shared/source_nws_snapshot",
          min_rows = 5L,
          min_numeric_cols = 2L,
          allow_nonfinite = TRUE,
          min_finite_rows = 5L,
          min_finite_numeric_cols = 2L
        )
        TRUE
      },
      error = function(e) {
        warning(
          sprintf(
            "data_prep_shared: forecats snapshot NWS alias failed schema validation; falling back to configured path. Details: %s",
            conditionMessage(e)
          ),
          call. = FALSE
        )
        FALSE
      }
    )
    if (isTRUE(nws_snapshot_ok)) {
      source_nws <- snapshot_nws
      source_nws_scale <- "raw_cms"
      source_nws_origin <- "snapshot"
    }
  }

  usgs_resolution <- unified_resolve_usgs_daily_path(cfg, snapshot_root = snapshot_root)
  if (nzchar(usgs_resolution$path)) {
    source_usgs <- usgs_resolution$path
    source_usgs_origin <- usgs_resolution$origin
  } else {
    repro_mode <- tolower(trimws(as.character(cfg$run$repro_mode)))
    require_local_usgs <- isTRUE(cfg$stages$post) && identical(repro_mode, "strict")
    fit_usgs_mode <- tolower(trimws(as.character(cfg$inputs$fit$usgs_mode)))
    if (identical(fit_usgs_mode, "cache")) {
      require_local_usgs <- TRUE
    }
    if (isTRUE(require_local_usgs)) {
      stop(
        paste(
          "data_prep_shared could not resolve a local USGS daily truth source.",
          "Checked shared forecats snapshot, inputs.fit.usgs_cache_path, and inputs.forecats.existing_bundle_path/meta.yaml.",
          "Strict post mode requires a run-scoped USGS truth CSV."
        ),
        call. = FALSE
      )
    }
    warning(
      paste(
        "data_prep_shared: local USGS daily truth source not found.",
        "Non-strict mode may still fall back to live NWIS in legacy post code."
      ),
      call. = FALSE
    )
  }

  unified_validate_glofas_members_csv(
    source_glofas,
    stage_name = "data_prep_shared/source_glofas"
  )
  if (grepl("weighted_time_series\\.csv$", tolower(basename(source_glofas)))) {
    unified_validate_weighted_time_series_csv(
      source_glofas,
      stage_name = "data_prep_shared/source_glofas",
      provenance = sprintf("source_mode=%s", source_mode)
    )
  }
  message(sprintf("data_prep_shared: validated GloFAS members schema at %s", source_glofas))

  add_shared_file(
    cfg$inputs$fit$parameters_path,
    shared_paths$parameters,
    storage_scale = "parameters_text"
  )
  add_shared_file(
    source_retros,
    shared_paths$retros,
    storage_scale = source_retros_scale
  )
  add_shared_file(
    source_nws,
    shared_paths$nws,
    storage_scale = source_nws_scale
  )
  add_shared_file(
    source_glofas,
    shared_paths$glofas,
    storage_scale = source_glofas_scale
  )
  if (nzchar(source_usgs)) {
    add_shared_file(
      source_usgs,
      shared_paths$usgs,
      storage_scale = "raw_cms"
    )
  }

  source_map_path <- file.path(shared_root, "source_map.txt")
  writeLines(
      c(
        sprintf("source_mode=%s", source_mode),
        sprintf("source.parameters=%s", cfg$inputs$fit$parameters_path),
        sprintf("source.retros=%s", source_retros),
        sprintf("source.retros_origin=%s", source_retros_origin),
        sprintf("source.nws=%s", source_nws),
      sprintf("source.nws_origin=%s", source_nws_origin),
      sprintf("source.glofas=%s", source_glofas),
      sprintf("source.glofas_origin=%s", source_glofas_origin),
      sprintf("source.usgs=%s", source_usgs),
      sprintf("source.usgs_origin=%s", source_usgs_origin),
      sprintf("snapshot_root=%s", snapshot_root),
      sprintf("snapshot_ready=%s", if (snapshot_ready) "TRUE" else "FALSE"),
      sprintf("snapshot_valid=%s", if (snapshot_valid) "TRUE" else "FALSE"),
      sprintf("cutoff_date=%s", as.character(cutoff_date)),
      sprintf("forecast_start_date=%s", as.character(forecast_start_date))
    ),
    con = source_map_path
  )
  manifest <- unified_manifest_add_artifact(
    manifest,
    normalizePath(source_map_path, mustWork = FALSE),
    storage_scale = "text",
    role = "shared_input"
  )

  fit_covariates <- cfg$inputs$fit$covariates
  if (is.null(fit_covariates)) fit_covariates <- list()
  if (length(fit_covariates) > 0L) {
    for (i in seq_along(fit_covariates)) {
      entry <- fit_covariates[[i]]
      if (!is.list(entry)) next
      cov_name <- if (is.null(entry$name)) sprintf("cov%02d", i) else as.character(entry$name)
      cov_path <- if (is.null(entry$path)) "" else as.character(entry$path)
      if (!nzchar(cov_path)) next
      cov_tag <- gsub("[^A-Za-z0-9]+", "_", cov_name)
      cov_tag <- gsub("^_+|_+$", "", cov_tag)
      if (!nzchar(cov_tag)) cov_tag <- sprintf("cov%02d", i)
      add_shared_file(
        cov_path,
        file.path(covariates_dir, sprintf("cov_%02d_%s.csv", i, cov_tag)),
        storage_scale = "table_csv"
      )
      assign_cov_path(cov_name, file.path(covariates_dir, sprintf("cov_%02d_%s.csv", i, cov_tag)))
    }
  } else {
    shared_covariates <- cfg$inputs$shared_covariates
    if (is.null(shared_covariates)) {
      shared_covariates <- list()
    }
    shared_covariates <- unlist(shared_covariates, use.names = FALSE)
    if (length(shared_covariates) > 0L) {
      for (cov_path in shared_covariates) {
        cov_path <- as.character(cov_path)
        if (!nzchar(cov_path)) next
        add_shared_file(
          cov_path,
          file.path(covariates_dir, basename(cov_path)),
          storage_scale = "table_csv"
        )
        assign_cov_path(basename(cov_path), file.path(covariates_dir, basename(cov_path)))
      }
    }
  }

  detclim_result <- unified_materialize_deterministic_climate_covariates(
    cfg = cfg,
    shared_paths = shared_paths,
    cov_path_map = shared_cov_paths,
    repo_root = repo_root
  )
  if (!is.null(detclim_result)) {
    manifest$deterministic_climate <- list(
      enabled = TRUE,
      handoff_root = detclim_result$handoff_root,
      horizon_days = as.integer(detclim_result$horizon_days),
      require_full_horizon = isTRUE(detclim_result$require_full_horizon),
      cutoff_date = as.character(detclim_result$cutoff_date),
      summary_path = detclim_result$debug_artifact_paths$summary_path,
      summary_sha256 = unified_sha256(detclim_result$debug_artifact_paths$summary_path),
      precip_future_path = detclim_result$debug_artifact_paths$precip_future_path,
      soil_future_path = detclim_result$debug_artifact_paths$soil_future_path,
      soil_family_support_path = detclim_result$debug_artifact_paths$soil_family_support_path,
      precip = list(
        enabled = isTRUE(detclim_result$precip_enabled),
        source = detclim_result$precip_source,
        reduction = detclim_result$precip_reduction,
        blend = detclim_result$precip_blend_cfg,
        output_path = shared_cov_paths$ppt,
        history_rows = as.integer(detclim_result$ppt_history_rows),
        future_rows = as.integer(detclim_result$ppt_future_rows)
      ),
      soil = list(
        enabled = isTRUE(detclim_result$soil_enabled),
        source = detclim_result$soil_source,
        reduction = detclim_result$soil_reduction,
        blend = detclim_result$soil_blend_cfg,
        output_path = shared_cov_paths$soil,
        history_rows = as.integer(detclim_result$soil_history_rows),
        future_rows = as.integer(detclim_result$soil_future_rows),
        porosity = if (!is.null(detclim_result$porosity_info)) unname(detclim_result$porosity_info$porosity) else NA_real_,
        porosity_q10 = if (!is.null(detclim_result$porosity_info)) unname(detclim_result$porosity_info$q10) else NA_real_,
        porosity_q90 = if (!is.null(detclim_result$porosity_info)) unname(detclim_result$porosity_info$q90) else NA_real_,
        porosity_sample_count = if (!is.null(detclim_result$porosity_info)) as.integer(detclim_result$porosity_info$sample_count) else NA_integer_
      ),
      pca = list(
        mode = "passthrough",
        output_path = shared_cov_paths$pca
      ),
      verified_in_data_prep_shared = TRUE
    )
    for (cov_path in unlist(detclim_result$updated_covariates, use.names = FALSE)) {
      if (!nzchar(as.character(cov_path))) next
      refresh_shared_manifest_entry(cov_path, role = "shared_input")
    }
    for (artifact_path in detclim_result$debug_artifacts) {
      if (!file.exists(artifact_path)) next
      if (grepl("\\.csv$", artifact_path, ignore.case = TRUE)) {
        manifest <- unified_manifest_add_artifact(
          manifest,
          normalizePath(artifact_path, mustWork = FALSE),
          storage_scale = "table_csv",
          role = "shared_input"
        )
      } else {
        manifest <- unified_manifest_add_artifact(
          manifest,
          normalizePath(artifact_path, mustWork = FALSE),
          storage_scale = "text",
          role = "shared_input"
        )
      }
    }
  }

  covfeat_result <- unified_materialize_covariate_features(
    cfg = cfg,
    shared_paths = shared_paths,
    cov_path_map = shared_cov_paths
  )
  if (!is.null(covfeat_result)) {
    manifest$covariate_features <- list(
      enabled = TRUE,
      csv_path = covfeat_result$csv_path,
      summary_path = covfeat_result$summary_path,
      lag_orders = as.integer(covfeat_result$lag_orders),
      include_squares = isTRUE(covfeat_result$include_squares),
      include_interaction = isTRUE(covfeat_result$include_interaction),
      column_names = as.character(covfeat_result$column_names)
    )
    refresh_shared_manifest_entry(covfeat_result$csv_path, role = "shared_input")
    if (file.exists(covfeat_result$summary_path)) {
      manifest <- unified_manifest_add_artifact(
        manifest,
        covfeat_result$summary_path,
        storage_scale = "text",
        role = "shared_input"
      )
    }
  }

  data_start <- unified_get(cfg, c("dates", "data_start"), default = NULL)
  if (!is.null(data_start) && nzchar(as.character(data_start))) {
    data_start_date <- suppressWarnings(as.Date(as.character(data_start)))
    if (is.na(data_start_date)) {
      stop("dates.data_start must be a valid YYYY-MM-DD date.", call. = FALSE)
    }

    core_paths <- c(
      retros = normalizePath(shared_paths$retros, mustWork = FALSE),
      usgs = normalizePath(shared_paths$usgs, mustWork = FALSE)
    )
    forecast_paths <- c(
      nws = normalizePath(shared_paths$nws, mustWork = FALSE),
      glofas = normalizePath(shared_paths$glofas, mustWork = FALSE)
    )
    cov_paths <- list.files(covariates_dir, pattern = "\\.csv$", full.names = TRUE)
    cov_paths <- normalizePath(cov_paths, mustWork = FALSE)
    filter_targets <- c(core_paths, forecast_paths, cov_paths)

    entries <- list()
    for (p in filter_targets) {
      is_core <- p %in% unname(core_paths)
      is_forecast <- p %in% unname(forecast_paths)
      label <- if (is_core) {
        names(core_paths)[match(p, unname(core_paths))]
      } else if (is_forecast) {
        names(forecast_paths)[match(p, unname(forecast_paths))]
      } else {
        sprintf("covariate:%s", basename(p))
      }
      df <- unified_read_csv_checked(p, label, "data_prep_shared/filter")
      date_info <- detect_date_info(df, label, p, required = is_core || is_forecast)
      if (is.null(date_info)) {
        warning(sprintf("data_prep_shared: skipping date filter for %s (no parseable date column)", p), call. = FALSE)
        next
      }
      keep <- !is.na(date_info$dates) & date_info$dates >= data_start_date
      if (!any(keep)) {
        stop(
          sprintf("data_prep_shared date filter removed all rows for %s after %s", p, as.character(data_start_date)),
          call. = FALSE
        )
      }
      entries[[p]] <- list(
        label = label,
        is_core = is_core,
        is_forecast = is_forecast,
        data = df[keep, , drop = FALSE],
        dates = as.character(date_info$dates[keep]),
        date_col = date_info$col
      )
    }

    core_entries <- entries[unname(core_paths)]
    if (length(core_entries) != length(core_paths) || any(vapply(core_entries, is.null, logical(1)))) {
      stop("data_prep_shared date filtering failed to prepare all core shared inputs.", call. = FALSE)
    }
    common_dates <- Reduce(intersect, lapply(core_entries, function(x) x$dates))
    common_dates <- sort(unique(common_dates))
    if (length(common_dates) == 0L) {
      stop(
        sprintf("data_prep_shared date filtering produced no common date support across core inputs after %s", as.character(data_start_date)),
        call. = FALSE
      )
    }

    summary_lines <- c(
      sprintf("data_start=%s", as.character(data_start_date)),
      sprintf("common_dates_count=%d", length(common_dates)),
      sprintf("common_date_min=%s", common_dates[[1L]]),
      sprintf("common_date_max=%s", common_dates[[length(common_dates)]])
    )

    for (p in names(entries)) {
      ent <- entries[[p]]
      out_df <- if (isTRUE(ent$is_core)) {
        idx <- ent$dates %in% common_dates
        ent$data[idx, , drop = FALSE]
      } else {
        ent$data
      }
      if (nrow(out_df) == 0L) {
        stop(sprintf("data_prep_shared date filtering left zero rows for %s after common-date alignment", p), call. = FALSE)
      }
      utils::write.csv(out_df, p, row.names = FALSE)
      refresh_shared_manifest_entry(p, role = "shared_input")
      summary_lines <- c(
        summary_lines,
        sprintf(
          "%s rows=%d date_col=%s",
          p, nrow(out_df), ent$date_col
        )
      )
    }

    filter_summary_path <- file.path(shared_root, "data_start_filter_summary.txt")
    writeLines(summary_lines, filter_summary_path)
    manifest <- unified_manifest_add_artifact(
      manifest,
      normalizePath(filter_summary_path, mustWork = FALSE),
      storage_scale = "text",
      role = "shared_input"
    )
  }

  cov_required <- list.files(covariates_dir, full.names = TRUE)
  if (length(cov_required) == 0L) cov_required <- character(0)

  # Hard gate: retros used by legacy bridges must be daily-continuous.
  assert_csv_daily_continuity(shared_paths$retros, "retros")

  unified_validate_required_shared_inputs(
    run_root = run_root,
    stage_name = "data_prep_shared",
    manifest = manifest,
    enabled_models = cfg$models,
    required_covariates = cov_required,
    required_usgs = isTRUE(cfg$stages$post)
  )
  message(sprintf("data_prep_shared: shared input validation passed under %s", shared_root))

  list(manifest = manifest)
}
