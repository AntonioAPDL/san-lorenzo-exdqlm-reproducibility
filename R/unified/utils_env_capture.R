# unified/utils_env_capture.R

unified_capture_env_artifacts <- function(run_root) {
  env_root <- file.path(run_root, "env")
  dir.create(env_root, recursive = TRUE, showWarnings = FALSE)

  r_session <- file.path(env_root, "R_sessionInfo.txt")
  writeLines(capture.output({
    cat("captured_at_utc=", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "\n", sep = "")
    print(sessionInfo())
  }), r_session, useBytes = TRUE)

  r_pkgs <- file.path(env_root, "R_installed_packages.csv")
  pkgs <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
  cols <- intersect(c("Package", "Version", "LibPath", "Built"), colnames(pkgs))
  utils::write.csv(pkgs[, cols, drop = FALSE], r_pkgs, row.names = FALSE)

  pip_freeze <- file.path(env_root, "python_pip_freeze.txt")
  python_bin <- Sys.which("python3")
  if (python_bin == "") python_bin <- Sys.which("python")
  if (python_bin != "") {
    out <- tryCatch(system2(python_bin, c("-m", "pip", "freeze"), stdout = TRUE, stderr = TRUE), error = function(e) c(conditionMessage(e)))
    writeLines(out, pip_freeze, useBytes = TRUE)
  } else {
    writeLines("python not found", pip_freeze, useBytes = TRUE)
  }

  renviron_snapshot <- file.path(env_root, "renviron_snapshot.txt")
  env_keys <- c("PKG_CXXFLAGS", "PKG_LIBS", "LD_LIBRARY_PATH", "DISC_BASE_SEED")
  vals <- Sys.getenv(env_keys, unset = "")
  lines <- c(
    sprintf("captured_at_utc=%s", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    sprintf("%s=%s", env_keys, unname(vals))
  )
  writeLines(lines, renviron_snapshot, useBytes = TRUE)

  threads_snapshot <- file.path(env_root, "threads_snapshot.txt")
  thread_keys <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS", "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS")
  thread_vals <- Sys.getenv(thread_keys, unset = "")
  writeLines(c(
    sprintf("captured_at_utc=%s", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    sprintf("%s=%s", thread_keys, unname(thread_vals))
  ), threads_snapshot, useBytes = TRUE)

  list(
    R_sessionInfo = r_session,
    R_installed_packages = r_pkgs,
    python_pip_freeze = pip_freeze,
    renviron_snapshot = renviron_snapshot,
    threads_snapshot = threads_snapshot
  )
}

unified_normalize_ld_library_path <- function(value) {
  value <- as.character(value)
  if (!length(value) || is.na(value[[1L]])) return("")
  value <- trimws(value[[1L]])
  if (!nzchar(value)) return("")
  parts <- strsplit(value, ":", fixed = TRUE)[[1L]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  # Canonicalize by removing duplicates while keeping first-occurrence order.
  parts <- parts[!duplicated(parts)]
  paste(parts, collapse = ":")
}

unified_env_drift_report <- function(current_env_dir, canonical_env_dir, out_json_path = NULL) {
  required <- c("R_sessionInfo.txt", "R_installed_packages.csv", "python_pip_freeze.txt", "renviron_snapshot.txt", "threads_snapshot.txt")
  normalize_lines <- function(path, name) {
    if (!file.exists(path)) return(character(0))
    lines <- readLines(path, warn = FALSE)
    if (name %in% c("R_sessionInfo.txt", "renviron_snapshot.txt", "threads_snapshot.txt")) {
      lines <- lines[!grepl("^captured_at_utc=", lines)]
    }
    if (identical(name, "renviron_snapshot.txt") && length(lines) > 0L) {
      lines <- unname(vapply(lines, function(line) {
        if (!grepl("^LD_LIBRARY_PATH=", line)) return(line)
        raw_val <- sub("^LD_LIBRARY_PATH=", "", line)
        sprintf("LD_LIBRARY_PATH=%s", unified_normalize_ld_library_path(raw_val))
      }, character(1)))
    }
    lines
  }

  records <- lapply(required, function(name) {
    current_path <- file.path(current_env_dir, name)
    canonical_path <- file.path(canonical_env_dir, name)
    current_norm <- normalize_lines(current_path, name)
    canonical_norm <- normalize_lines(canonical_path, name)
    same_norm <- identical(current_norm, canonical_norm)
    list(
      file = name,
      current_exists = file.exists(current_path),
      canonical_exists = file.exists(canonical_path),
      current_sha256 = if (file.exists(current_path)) unified_sha256(current_path) else NA_character_,
      canonical_sha256 = if (file.exists(canonical_path)) unified_sha256(canonical_path) else NA_character_
      ,
      normalized_equal = same_norm
    )
  })

  mismatched <- vapply(records, function(r) {
    isTRUE(r$current_exists) && isTRUE(r$canonical_exists) && !isTRUE(r$normalized_equal)
  }, logical(1))

  missing <- vapply(records, function(r) {
    !isTRUE(r$current_exists) || !isTRUE(r$canonical_exists)
  }, logical(1))

  report <- list(
    status = if (!any(mismatched) && !any(missing)) "pass" else "fail",
    compared_files = records,
    mismatched_files = unname(required[mismatched]),
    missing_files = unname(required[missing])
  )

  if (!is.null(out_json_path) && requireNamespace("jsonlite", quietly = TRUE)) {
    dir.create(dirname(out_json_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(report, path = out_json_path, auto_unbox = TRUE, pretty = TRUE)
  }

  report
}
