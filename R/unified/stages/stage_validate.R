# unified/stages/stage_validate.R

unified_write_sha_for_dir <- function(dir_path, out_path) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dir_path)) {
    writeLines(character(0), out_path, useBytes = TRUE)
    return(invisible(out_path))
  }
  files <- sort(list.files(dir_path, full.names = TRUE))
  files <- files[file.info(files)$isdir %in% FALSE]
  lines <- vapply(files, function(p) sprintf("%s  %s", unified_sha256(p), basename(p)), character(1))
  writeLines(lines, out_path, useBytes = TRUE)
  invisible(out_path)
}

unified_parse_compare_report_txt <- function(path) {
  if (!file.exists(path)) {
    return(list(matched = NA_integer_, missing = NA_integer_, extra = NA_integer_, mismatched = NA_integer_))
  }
  lines <- readLines(path, warn = FALSE)
  parse_count <- function(key) {
    line <- grep(paste0("^", key, ":"), lines, value = TRUE)
    if (length(line) == 0) return(NA_integer_)
    suppressWarnings(as.integer(sub("^.*:\\s*", "", line[[1]])))
  }
  list(
    matched = parse_count("Matched"),
    missing = parse_count("Missing"),
    extra = parse_count("Extra"),
    mismatched = parse_count("Mismatched")
  )
}

unified_parse_key_value_text <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  out <- list()
  for (line in lines) {
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2L) next
    key <- trimws(parts[[1L]])
    val <- paste(parts[-1L], collapse = "=")
    out[[key]] <- val
  }
  out
}

unified_validate_detclim_report <- function(cfg, manifest, run_root) {
  det_manifest <- manifest$deterministic_climate
  det_enabled <- isTRUE(unified_get(cfg, c("inputs", "deterministic_climate", "enabled"), default = FALSE)) ||
    (is.list(det_manifest) && isTRUE(det_manifest$enabled))

  if (!det_enabled) {
    return(list(enabled = FALSE, status = "disabled"))
  }

  if (!is.list(det_manifest)) {
    return(list(
      enabled = TRUE,
      status = "fail",
      error = "deterministic climate enabled but manifest metadata is missing"
    ))
  }

  required_paths <- list(
    summary_path = det_manifest$summary_path,
    precip_future_path = det_manifest$precip_future_path,
    soil_future_path = det_manifest$soil_future_path,
    soil_family_support_path = det_manifest$soil_family_support_path
  )
  path_report <- lapply(required_paths, function(p) {
    p <- if (is.null(p)) "" else as.character(p)
    list(
      path = p,
      exists = nzchar(p) && file.exists(p),
      sha256 = if (nzchar(p) && file.exists(p)) unified_sha256(p) else NA_character_,
      bytes = if (nzchar(p) && file.exists(p)) as.numeric(file.info(p)$size) else NA_real_
    )
  })

  missing_required <- names(path_report)[!vapply(path_report, function(x) isTRUE(x$exists), logical(1))]
  summary_kv <- list()
  if (isTRUE(path_report$summary_path$exists)) {
    summary_kv <- unified_parse_key_value_text(path_report$summary_path$path)
  }

  manifest_sha_ok <- TRUE
  manifest_summary_sha <- if (is.null(det_manifest$summary_sha256)) "" else as.character(det_manifest$summary_sha256)
  if (nzchar(manifest_summary_sha) && isTRUE(path_report$summary_path$exists)) {
    manifest_sha_ok <- identical(manifest_summary_sha, path_report$summary_path$sha256)
  }

  canonical_run_id <- cfg$validation$canonical_run_id
  if (!is.null(canonical_run_id) && nzchar(canonical_run_id) && identical(toupper(canonical_run_id), "__SELF__")) {
    canonical_run_id <- cfg$run$run_id
  }
  canonical_summary_path <- ""
  canonical_summary_exists <- FALSE
  canonical_summary_sha <- NA_character_
  canonical_summary_sha_match <- NA
  if (!is.null(canonical_run_id) && nzchar(canonical_run_id)) {
    canonical_summary_path <- file.path(
      cfg$run$run_root,
      canonical_run_id,
      "inputs",
      "shared",
      "deterministic_climate",
      "deterministic_climate_summary.txt"
    )
    canonical_summary_exists <- file.exists(canonical_summary_path)
    if (canonical_summary_exists) {
      canonical_summary_sha <- unified_sha256(canonical_summary_path)
      if (isTRUE(path_report$summary_path$exists)) {
        canonical_summary_sha_match <- identical(canonical_summary_sha, path_report$summary_path$sha256)
      }
    }
  }

  errors <- character(0)
  if (length(missing_required) > 0L) {
    errors <- c(errors, sprintf("missing deterministic climate artifacts: %s", paste(missing_required, collapse = ", ")))
  }
  if (!isTRUE(manifest_sha_ok)) {
    errors <- c(errors, "deterministic climate summary hash does not match manifest metadata")
  }
  if (!isTRUE(det_manifest$verified_in_data_prep_shared)) {
    errors <- c(errors, "deterministic climate manifest flag verified_in_data_prep_shared is not true")
  }

  list(
    enabled = TRUE,
    status = if (length(errors) == 0L) "pass" else "fail",
    errors = errors,
    handoff_root = if (is.null(det_manifest$handoff_root)) NA_character_ else as.character(det_manifest$handoff_root),
    horizon_days = det_manifest$horizon_days,
    require_full_horizon = det_manifest$require_full_horizon,
    cutoff_date = det_manifest$cutoff_date,
    manifest_summary_sha256 = if (nzchar(manifest_summary_sha)) manifest_summary_sha else NA_character_,
    manifest_summary_sha_match = manifest_sha_ok,
    current = list(
      summary = path_report$summary_path,
      precip_future = path_report$precip_future_path,
      soil_future = path_report$soil_future_path,
      soil_family_support = path_report$soil_family_support_path
    ),
    summary = summary_kv,
    canonical = list(
      run_id = if (is.null(canonical_run_id) || !nzchar(canonical_run_id)) NA_character_ else canonical_run_id,
      summary_path = if (nzchar(canonical_summary_path)) canonical_summary_path else NA_character_,
      summary_exists = canonical_summary_exists,
      summary_sha256 = canonical_summary_sha,
      summary_sha_match = canonical_summary_sha_match
    )
  )
}

unified_stage_validate <- function(cfg, run_root, repo_root, manifest) {
  validate_root <- file.path(run_root, "validate")
  dir.create(validate_root, recursive = TRUE, showWarnings = FALSE)

  run_id <- cfg$run$run_id
  current_dir <- file.path(run_root, "post", "outputs", run_id)

  canonical_run_id <- cfg$validation$canonical_run_id
  if (!is.null(canonical_run_id) && nzchar(canonical_run_id) &&
      identical(toupper(canonical_run_id), "__SELF__")) {
    canonical_run_id <- run_id
  }
  if (!is.null(canonical_run_id) && nzchar(canonical_run_id)) {
    canonical_dir <- file.path(cfg$run$run_root, canonical_run_id, "post", "outputs", canonical_run_id)
  } else {
    canonical_dir <- file.path(repo_root, "Environmetrics_reproduce")
  }

  canonical_sha <- file.path(validate_root, "canonical.sha256")
  current_sha <- file.path(validate_root, "current.sha256")
  report_txt <- file.path(validate_root, "compare_report.txt")
  report_json <- file.path(validate_root, "compare_report.json")
  diff_dir <- file.path(validate_root, "diff")

  unified_write_sha_for_dir(canonical_dir, canonical_sha)
  unified_write_sha_for_dir(current_dir, current_sha)

  compare_script <- file.path(repo_root, "repro", "compare_to_canonical.py")
  compare_mode_raw <- as.character(unified_get(cfg, c("validation", "compare", "mode"), default = "both"))
  compare_mode <- if (length(compare_mode_raw) > 0L) tolower(trimws(compare_mode_raw[[1L]])) else "both"
  if (!nzchar(compare_mode)) compare_mode <- "both"
  cmd_status <- 0L
  cmd_out <- character(0)
  compare_skipped <- identical(compare_mode, "none")
  if (compare_skipped) {
    writeLines("Canonical comparison skipped (validation.compare.mode=none).", report_txt, useBytes = TRUE)
    cmd_out <- "canonical comparison skipped"
  } else if (file.exists(compare_script)) {
    args <- c(
      compare_script,
      "--manifest", file.path(run_root, "run_manifest.yaml"),
      "--canonical-dir", canonical_dir,
      "--current-dir", current_dir,
      "--canonical-sha", canonical_sha,
      "--current-sha", current_sha,
      "--report", report_txt,
      "--diff-dir", diff_dir,
      "--mode", compare_mode
    )
    cmd_out <- system2("python3", args, stdout = TRUE, stderr = TRUE)
    status_attr <- attr(cmd_out, "status")
    if (!is.null(status_attr)) cmd_status <- as.integer(status_attr)
  } else {
    cmd_status <- 1L
    cmd_out <- sprintf("compare tool missing: %s", compare_script)
  }

  metrics <- unified_parse_compare_report_txt(report_txt)
  compare_ok <- if (compare_skipped) {
    TRUE
  } else {
    !is.na(metrics$mismatched) && !is.na(metrics$missing) && !is.na(metrics$extra) &&
      metrics$mismatched == 0 && metrics$missing == 0 && metrics$extra == 0 && cmd_status == 0
  }
  status <- if (isTRUE(compare_ok)) "pass" else "fail"

  report <- list(
    status = status,
    profile = unified_get(cfg, c("validation", "profile"), default = "production"),
    run_id = run_id,
    canonical_run_id = canonical_run_id,
    canonical_dir = canonical_dir,
    current_dir = current_dir,
    mode = compare_mode,
    compare_skipped = compare_skipped,
    metrics = metrics,
    command_status = cmd_status,
    command_output_tail = utils::tail(cmd_out, 20)
  )

  env_drift_path <- file.path(validate_root, "env_drift_report.json")
  if (!is.null(canonical_run_id) && nzchar(canonical_run_id)) {
    current_env_dir <- file.path(run_root, "env")
    canonical_env_dir <- file.path(cfg$run$run_root, canonical_run_id, "env")
    env_report <- unified_env_drift_report(current_env_dir, canonical_env_dir, out_json_path = env_drift_path)
    report$env_drift <- env_report
    if (identical(env_report$status, "fail")) {
      status <- "fail"
      report$status <- "fail"
    }
  }

  detclim_report <- unified_validate_detclim_report(cfg, manifest, run_root)
  report$deterministic_climate <- detclim_report
  if (identical(detclim_report$status, "fail")) {
    status <- "fail"
    report$status <- "fail"
  }

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(report, path = report_json, auto_unbox = TRUE, pretty = TRUE)
  } else {
    writeLines(c("{", sprintf("  \"status\": \"%s\"", status), "}"), report_json, useBytes = TRUE)
  }

  manifest$validation$status <- status
  manifest$validation$compare_report_path <- report_json
  manifest$validation$validator_profile <- unified_get(cfg, c("validation", "profile"), default = "production")
  list(manifest = manifest)
}
