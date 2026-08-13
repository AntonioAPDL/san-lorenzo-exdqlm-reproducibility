# unified/preflight.R

unified_df_parse_percent <- function(x) {
  suppressWarnings(as.numeric(gsub("%", "", as.character(x), fixed = TRUE)))
}

unified_storage_snapshot <- function(path) {
  target <- if (is.null(path) || !nzchar(as.character(path))) getwd() else as.character(path)
  target <- normalizePath(target, mustWork = FALSE)

  parse_line <- function(lines, expect = c("blocks", "inodes")) {
    expect <- match.arg(expect)
    lines <- lines[nzchar(lines)]
    if (length(lines) < 2L) {
      stop(sprintf("unable to parse df output for %s", target), call. = FALSE)
    }
    line <- lines[[length(lines)]]
    fields <- strsplit(trimws(line), "\\s+")[[1]]
    if (length(fields) < 6L) {
      stop(sprintf("unexpected df output format for %s: %s", target, line), call. = FALSE)
    }
    list(
      filesystem = fields[[1L]],
      total = suppressWarnings(as.numeric(fields[[2L]])),
      used = suppressWarnings(as.numeric(fields[[3L]])),
      avail = suppressWarnings(as.numeric(fields[[4L]])),
      used_pct = unified_df_parse_percent(fields[[5L]]),
      mountpoint = fields[[6L]]
    )
  }

  blocks_out <- system2("df", c("-Pk", target), stdout = TRUE, stderr = TRUE)
  blocks_status <- attr(blocks_out, "status")
  if (!is.null(blocks_status) && blocks_status != 0) {
    stop(sprintf("df -Pk failed for %s: %s", target, paste(blocks_out, collapse = " | ")), call. = FALSE)
  }
  block_info <- parse_line(blocks_out, expect = "blocks")

  inode_out <- system2("df", c("-Pi", target), stdout = TRUE, stderr = TRUE)
  inode_status <- attr(inode_out, "status")
  inode_info <- if (!is.null(inode_status) && inode_status != 0) {
    list(total = NA_real_, used = NA_real_, avail = NA_real_, used_pct = NA_real_, mountpoint = block_info$mountpoint)
  } else {
    parse_line(inode_out, expect = "inodes")
  }

  free_bytes <- as.numeric(block_info$avail) * 1024
  free_inodes_pct <- if (is.finite(inode_info$total) && inode_info$total > 0) {
    (as.numeric(inode_info$avail) / as.numeric(inode_info$total)) * 100
  } else {
    NA_real_
  }

  list(
    path = target,
    filesystem = block_info$filesystem,
    mountpoint = block_info$mountpoint,
    free_bytes = free_bytes,
    free_gb = free_bytes / (1024^3),
    used_pct = as.numeric(block_info$used_pct),
    free_inodes = as.numeric(inode_info$avail),
    total_inodes = as.numeric(inode_info$total),
    free_inodes_pct = free_inodes_pct
  )
}

unified_preflight_num_scalar <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0L) return(default)
  x[[1L]]
}

unified_preflight_sanitize_label <- function(x, fallback = "preflight") {
  label <- gsub("[^A-Za-z0-9]+", "_", as.character(x))
  label <- gsub("^_+|_+$", "", label)
  if (!nzchar(label)) fallback else label
}

unified_preflight_build_report <- function(
  path,
  min_free_bytes,
  min_free_inodes_pct = NULL,
  context = "",
  mode = c("enforce", "warn"),
  critical_free_bytes = 5 * 1024^3
) {
  mode <- match.arg(mode)
  min_free_bytes <- unified_preflight_num_scalar(min_free_bytes, default = NA_real_)
  min_free_inodes_pct <- unified_preflight_num_scalar(min_free_inodes_pct, default = NA_real_)
  critical_free_bytes <- unified_preflight_num_scalar(critical_free_bytes, default = NA_real_)

  thresholds_active <- (is.finite(min_free_bytes) && min_free_bytes > 0) ||
    (is.finite(min_free_inodes_pct) && min_free_inodes_pct > 0)
  if (!thresholds_active) {
    return(list(
      timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      context = context,
      mode = mode,
      status = "skipped",
      message = "thresholds disabled",
      path = normalizePath(path, mustWork = FALSE)
    ))
  }

  snap <- unified_storage_snapshot(path)
  violations <- character(0)

  if (is.finite(min_free_bytes) && min_free_bytes > 0 && (!is.finite(snap$free_bytes) || snap$free_bytes < min_free_bytes)) {
    violations <- c(
      violations,
      sprintf(
        "free space %.2f GB below threshold %.2f GB",
        snap$free_gb,
        min_free_bytes / (1024^3)
      )
    )
  }

  if (is.finite(min_free_inodes_pct) && min_free_inodes_pct > 0 &&
      (!is.finite(snap$free_inodes_pct) || snap$free_inodes_pct < min_free_inodes_pct)) {
    violations <- c(
      violations,
      sprintf("free inode pct %.2f%% below threshold %.2f%%", snap$free_inodes_pct, min_free_inodes_pct)
    )
  }

  critical_low <- is.finite(critical_free_bytes) &&
    critical_free_bytes > 0 &&
    is.finite(snap$free_bytes) &&
    snap$free_bytes < critical_free_bytes

  status <- "pass"
  if (length(violations) > 0L) {
    status <- if (identical(mode, "warn") && !critical_low) "warn" else "fail"
  }

  list(
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    context = context,
    mode = mode,
    status = status,
    path = snap$path,
    mountpoint = snap$mountpoint,
    filesystem = snap$filesystem,
    thresholds = list(
      min_free_gb = if (is.finite(min_free_bytes)) min_free_bytes / (1024^3) else NA_real_,
      min_free_inodes_pct = if (is.finite(min_free_inodes_pct)) min_free_inodes_pct else NA_real_,
      critical_free_gb = if (is.finite(critical_free_bytes)) critical_free_bytes / (1024^3) else NA_real_
    ),
    snapshot = list(
      free_bytes = snap$free_bytes,
      free_gb = snap$free_gb,
      used_pct = snap$used_pct,
      free_inodes = snap$free_inodes,
      total_inodes = snap$total_inodes,
      free_inodes_pct = snap$free_inodes_pct
    ),
    violations = violations,
    critical_low = critical_low
  )
}

unified_preflight_write_report <- function(report, report_dir, stage_label = "preflight") {
  if (is.null(report_dir) || !nzchar(as.character(report_dir))) return(NULL)
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  safe_stage <- unified_preflight_sanitize_label(stage_label, fallback = "preflight")
  ts <- gsub("[-:]", "", gsub("T|Z", "_", as.character(report$timestamp_utc)))
  ts <- gsub("[^A-Za-z0-9_]+", "", ts)
  if (!nzchar(ts)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
  }
  out_path <- file.path(report_dir, sprintf("%s_%s.json", safe_stage, ts))

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(report, path = out_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    return(out_path)
  }

  esc <- function(x) gsub("\"", "\\\\\"", as.character(x), fixed = TRUE)
  lines <- c(
    "{",
    sprintf("  \"timestamp_utc\": \"%s\",", esc(report$timestamp_utc)),
    sprintf("  \"context\": \"%s\",", esc(report$context)),
    sprintf("  \"mode\": \"%s\",", esc(report$mode)),
    sprintf("  \"status\": \"%s\",", esc(report$status)),
    sprintf("  \"path\": \"%s\",", esc(report$path)),
    sprintf("  \"mountpoint\": \"%s\",", esc(report$mountpoint)),
    sprintf("  \"filesystem\": \"%s\",", esc(report$filesystem)),
    sprintf("  \"message\": \"jsonlite unavailable; fallback preflight record\""),
    "}"
  )
  writeLines(lines, con = out_path, useBytes = TRUE)
  out_path
}

unified_preflight_summary_line <- function(report, report_path = NULL, check_point = NULL, scope = NULL) {
  snapshot <- report$snapshot
  free_gb <- if (!is.null(snapshot) && is.finite(snapshot$free_gb)) sprintf("%.2f", snapshot$free_gb) else "NA"
  used_pct <- if (!is.null(snapshot) && is.finite(snapshot$used_pct)) sprintf("%.2f", snapshot$used_pct) else "NA"
  min_gb <- if (!is.null(report$thresholds) && is.finite(report$thresholds$min_free_gb)) sprintf("%.2f", report$thresholds$min_free_gb) else "NA"
  critical_gb <- if (!is.null(report$thresholds) && is.finite(report$thresholds$critical_free_gb)) sprintf("%.2f", report$thresholds$critical_free_gb) else "NA"
  sprintf(
    "PREFLIGHT_CHECK status=%s check_point=%s scope=%s context=\"%s\" free_gb=%s used_pct=%s min_free_gb=%s critical_free_gb=%s report=%s",
    report$status,
    if (is.null(check_point)) "" else as.character(check_point),
    if (is.null(scope)) "" else as.character(scope),
    as.character(report$context),
    free_gb,
    used_pct,
    min_gb,
    critical_gb,
    if (is.null(report_path) || !nzchar(as.character(report_path))) "" else as.character(report_path)
  )
}

unified_preflight_append_log <- function(log_path, line) {
  if (is.null(log_path) || !nzchar(as.character(log_path))) return(invisible(FALSE))
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  cat(sprintf("%s\n", line), file = log_path, append = TRUE)
  invisible(TRUE)
}

unified_preflight_message <- function(report, report_path = NULL, scope = NULL, check_point = NULL) {
  violations <- if (length(report$violations) > 0L) paste(report$violations, collapse = " | ") else "none"
  snapshot <- report$snapshot
  paste(
    c(
      sprintf("[%s] Storage preflight %s.", report$context, report$status),
      sprintf("- scope: %s", if (is.null(scope)) "legacy" else as.character(scope)),
      sprintf("- check_point: %s", if (is.null(check_point)) "" else as.character(check_point)),
      sprintf("- path: %s", report$path),
      sprintf("- mountpoint: %s", report$mountpoint),
      sprintf("- filesystem: %s", report$filesystem),
      sprintf("- free_gb: %s", if (!is.null(snapshot) && is.finite(snapshot$free_gb)) sprintf("%.2f", snapshot$free_gb) else "NA"),
      sprintf("- used_pct: %s", if (!is.null(snapshot) && is.finite(snapshot$used_pct)) sprintf("%.2f%%", snapshot$used_pct) else "NA"),
      sprintf("- free_inodes_pct: %s", if (!is.null(snapshot) && is.finite(snapshot$free_inodes_pct)) sprintf("%.2f%%", snapshot$free_inodes_pct) else "NA"),
      sprintf("- min_free_gb: %s", if (is.finite(report$thresholds$min_free_gb)) sprintf("%.2f", report$thresholds$min_free_gb) else "NA"),
      sprintf("- min_free_inodes_pct: %s", if (is.finite(report$thresholds$min_free_inodes_pct)) sprintf("%.2f", report$thresholds$min_free_inodes_pct) else "NA"),
      sprintf("- critical_free_gb: %s", if (is.finite(report$thresholds$critical_free_gb)) sprintf("%.2f", report$thresholds$critical_free_gb) else "NA"),
      sprintf("- violations: %s", violations),
      sprintf("- report_path: %s", if (is.null(report_path) || !nzchar(report_path)) "none" else report_path),
      "- cleanup_suggestions: thin failed repro/runs first, prune old completed runs next, then optional baseline thinning/root-RData pruning"
    ),
    collapse = "\n"
  )
}

unified_run_io_check_plan <- function(io_settings, check_point = c("run_start", "fit_start", "continue")) {
  check_point <- match.arg(check_point)

  enabled <- isTRUE(io_settings$enabled)
  scope <- io_settings$preflight_scope
  if (is.null(scope) || !nzchar(as.character(scope))) scope <- "legacy"
  scope <- as.character(scope)
  if (!scope %in% c("legacy", "fit_start_and_continue", "fit_start_only")) {
    scope <- "legacy"
  }

  min_legacy <- unified_preflight_num_scalar(io_settings$min_free_bytes, default = 0)
  min_start <- unified_preflight_num_scalar(io_settings$min_free_bytes_start, default = NA_real_)
  min_continue <- unified_preflight_num_scalar(io_settings$min_free_bytes_continue, default = NA_real_)
  min_inode <- unified_preflight_num_scalar(io_settings$min_free_inodes_pct, default = NA_real_)
  critical <- unified_preflight_num_scalar(io_settings$critical_free_bytes, default = 5 * 1024^3)

  if (!is.finite(min_start)) min_start <- min_legacy
  if (!is.finite(min_continue)) min_continue <- min_legacy

  mode <- "enforce"
  min_free_bytes <- min_legacy

  if (identical(scope, "fit_start_and_continue")) {
    if (check_point %in% c("run_start", "fit_start")) {
      min_free_bytes <- min_start
    } else {
      min_free_bytes <- min_continue
    }
  } else if (identical(scope, "fit_start_only")) {
    if (check_point %in% c("run_start", "fit_start")) {
      min_free_bytes <- min_start
      mode <- "enforce"
    } else {
      min_free_bytes <- min_continue
      mode <- "warn"
    }
  }

  list(
    enabled = enabled,
    scope = scope,
    check_point = check_point,
    mode = mode,
    min_free_bytes = min_free_bytes,
    min_free_inodes_pct = min_inode,
    critical_free_bytes = critical
  )
}

unified_run_io_preflight <- function(
  path,
  io_settings,
  check_point = c("run_start", "fit_start", "continue"),
  context = "",
  report_dir = NULL,
  stage_label = "preflight",
  log_path = NULL
) {
  check_point <- match.arg(check_point)
  plan <- unified_run_io_check_plan(io_settings, check_point = check_point)
  if (!isTRUE(plan$enabled)) {
    return(list(status = "disabled", report_path = NULL, report = NULL, plan = plan))
  }

  report <- unified_preflight_build_report(
    path = path,
    min_free_bytes = plan$min_free_bytes,
    min_free_inodes_pct = plan$min_free_inodes_pct,
    context = context,
    mode = plan$mode,
    critical_free_bytes = plan$critical_free_bytes
  )
  report$scope <- plan$scope
  report$check_point <- check_point

  report_path <- unified_preflight_write_report(report, report_dir = report_dir, stage_label = stage_label)
  summary_line <- unified_preflight_summary_line(report, report_path = report_path, check_point = check_point, scope = plan$scope)
  unified_preflight_append_log(log_path, summary_line)

  if (identical(report$status, "fail")) {
    stop(unified_preflight_message(report, report_path = report_path, scope = plan$scope, check_point = check_point), call. = FALSE)
  }
  if (identical(report$status, "warn")) {
    warning(unified_preflight_message(report, report_path = report_path, scope = plan$scope, check_point = check_point), call. = FALSE)
  }

  list(status = report$status, report_path = report_path, report = report, plan = plan)
}

unified_require_free_space <- function(path, min_free_bytes, min_free_inodes_pct = NULL, context = "") {
  result <- unified_run_io_preflight(
    path = path,
    io_settings = list(
      enabled = TRUE,
      preflight_scope = "legacy",
      min_free_bytes = min_free_bytes,
      min_free_bytes_start = min_free_bytes,
      min_free_bytes_continue = min_free_bytes,
      min_free_inodes_pct = min_free_inodes_pct,
      critical_free_bytes = 5 * 1024^3
    ),
    check_point = "run_start",
    context = context,
    report_dir = NULL,
    stage_label = "legacy_preflight",
    log_path = NULL
  )

  if (identical(result$status, "disabled") || is.null(result$report) || is.null(result$report$snapshot)) {
    return(invisible(NULL))
  }
  invisible(result$report$snapshot)
}

unified_get_run_io_settings <- function(cfg) {
  io <- NULL
  if (is.list(cfg) && is.list(cfg$run)) io <- cfg$run$io
  if (is.null(io) || !is.list(io)) io <- list()

  enabled <- isTRUE(io$enabled)
  min_free_gb <- unified_preflight_num_scalar(io$min_free_gb, default = NA_real_)
  min_free_gb_start <- unified_preflight_num_scalar(io$min_free_gb_start, default = NA_real_)
  min_free_gb_continue <- unified_preflight_num_scalar(io$min_free_gb_continue, default = NA_real_)
  min_free_inodes_pct <- unified_preflight_num_scalar(io$min_free_inodes_pct, default = NA_real_)
  preflight_scope <- io$preflight_scope
  if (is.null(preflight_scope) || !nzchar(as.character(preflight_scope))) preflight_scope <- "legacy"
  preflight_scope <- as.character(preflight_scope)
  if (!preflight_scope %in% c("legacy", "fit_start_and_continue", "fit_start_only")) {
    preflight_scope <- "legacy"
  }
  critical_free_gb <- unified_preflight_num_scalar(io$critical_free_gb, default = 5)
  if (!is.finite(critical_free_gb) || critical_free_gb <= 0) critical_free_gb <- 5

  legacy_bytes <- if (is.finite(min_free_gb) && min_free_gb > 0) min_free_gb * 1024^3 else 0
  start_bytes <- if (is.finite(min_free_gb_start) && min_free_gb_start >= 0) min_free_gb_start * 1024^3 else legacy_bytes
  continue_bytes <- if (is.finite(min_free_gb_continue) && min_free_gb_continue >= 0) min_free_gb_continue * 1024^3 else legacy_bytes

  list(
    enabled = enabled,
    preflight_scope = preflight_scope,
    min_free_bytes = legacy_bytes,
    min_free_bytes_start = start_bytes,
    min_free_bytes_continue = continue_bytes,
    min_free_inodes_pct = if (is.finite(min_free_inodes_pct) && min_free_inodes_pct > 0) min_free_inodes_pct else NA_real_,
    critical_free_bytes = critical_free_gb * 1024^3
  )
}

unified_safe_save <- function(save_fun, final_path, tmp_suffix = ".tmp", context = "") {
  if (!is.function(save_fun)) {
    stop("save_fun must be a function(path)", call. = FALSE)
  }
  final_path <- normalizePath(final_path, mustWork = FALSE)
  final_dir <- dirname(final_path)
  dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)

  tmp_path <- sprintf("%s%s.%d", final_path, tmp_suffix, Sys.getpid())
  if (file.exists(tmp_path)) unlink(tmp_path, force = TRUE)

  result <- tryCatch(
    {
      save_fun(tmp_path)
      TRUE
    },
    error = function(e) e
  )

  if (inherits(result, "error")) {
    unlink(tmp_path, force = TRUE)
    storage_msg <- tryCatch(
      {
        snap <- unified_storage_snapshot(final_dir)
        sprintf("mount=%s free_gb=%.2f used_pct=%.2f free_inodes_pct=%s",
                snap$mountpoint, snap$free_gb, snap$used_pct,
                if (is.finite(snap$free_inodes_pct)) sprintf("%.2f", snap$free_inodes_pct) else "NA")
      },
      error = function(e) "storage snapshot unavailable"
    )
    stop(
      sprintf(
        "safe save failed%s: %s | target=%s | %s",
        if (nzchar(context)) sprintf(" (%s)", context) else "",
        conditionMessage(result),
        final_path,
        storage_msg
      ),
      call. = FALSE
    )
  }

  tmp_size <- suppressWarnings(file.info(tmp_path)$size)
  if (!file.exists(tmp_path) || !is.finite(tmp_size) || tmp_size <= 0) {
    unlink(tmp_path, force = TRUE)
    stop(
      sprintf(
        "safe save produced missing/empty temp file%s: %s",
        if (nzchar(context)) sprintf(" (%s)", context) else "",
        tmp_path
      ),
      call. = FALSE
    )
  }

  if (file.exists(final_path)) unlink(final_path, force = TRUE)
  moved <- isTRUE(file.rename(tmp_path, final_path))
  if (!moved) {
    copied <- file.copy(tmp_path, final_path, overwrite = TRUE)
    unlink(tmp_path, force = TRUE)
    if (!isTRUE(copied)) {
      stop(
        sprintf(
          "safe save rename/copy failed%s: %s",
          if (nzchar(context)) sprintf(" (%s)", context) else "",
          final_path
        ),
        call. = FALSE
      )
    }
  }

  final_size <- suppressWarnings(file.info(final_path)$size)
  if (!file.exists(final_path) || !is.finite(final_size) || final_size <= 0) {
    stop(
      sprintf(
        "safe save produced missing/empty final file%s: %s",
        if (nzchar(context)) sprintf(" (%s)", context) else "",
        final_path
      ),
      call. = FALSE
    )
  }

  invisible(final_path)
}
