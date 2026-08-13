#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

usage <- function() {
  cat("Usage: Rscript --vanilla scripts/unified_run.R --config <yaml> [--dry-run]\n")
}

parse_args <- function(args) {
  config_path <- NULL
  dry_run <- FALSE

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--config")) {
      if (i == length(args)) stop("--config requires a value", call. = FALSE)
      config_path <- args[[i + 1L]]
      i <- i + 2L
      next
    }
    if (identical(arg, "--dry-run")) {
      dry_run <- TRUE
      i <- i + 1L
      next
    }
    stop(sprintf("Unknown argument: %s", arg), call. = FALSE)
  }

  if (is.null(config_path)) {
    usage()
    stop("--config is required", call. = FALSE)
  }

  list(config_path = config_path, dry_run = dry_run)
}

opts <- parse_args(args)
repo_root <- normalizePath(getwd(), mustWork = TRUE)

source(file.path(repo_root, "R", "unified", "utils_hash.R"))
source(file.path(repo_root, "R", "unified", "utils_scale.R"))
source(file.path(repo_root, "R", "unified", "utils_env_capture.R"))
source(file.path(repo_root, "R", "unified", "preflight.R"))
source(file.path(repo_root, "R", "unified", "utils_artifact_locator.R"))
source(file.path(repo_root, "R", "unified", "inputs_shared_validate.R"))
source(file.path(repo_root, "R", "unified", "contract_checks.R"))
source(file.path(repo_root, "R", "unified", "diagnostics.R"))
source(file.path(repo_root, "R", "unified", "config.R"))
source(file.path(repo_root, "R", "unified", "determinism.R"))
source(file.path(repo_root, "R", "unified", "deterministic_climate_covariates.R"))
source(file.path(repo_root, "R", "unified", "families", "shared_input_helpers.R"))
source(file.path(repo_root, "R", "unified", "covariate_feature_engineering.R"))
source(file.path(repo_root, "R", "unified", "manifest.R"))
source(file.path(repo_root, "R", "unified", "utils_write_audit.R"))
source(file.path(repo_root, "R", "unified", "stages", "stage_forecats.R"))
source(file.path(repo_root, "R", "unified", "stages", "stage_data_prep_shared.R"))
source(file.path(repo_root, "R", "unified", "stages", "stage_fit.R"))
source(file.path(repo_root, "R", "unified", "stages", "stage_post.R"))
source(file.path(repo_root, "R", "unified", "stages", "stage_validate.R"))
source(file.path(repo_root, "R", "unified", "stages", "stage_report.R"))

cfg <- unified_load_config(opts$config_path, repo_root = repo_root)

# Prevent Python helper calls from writing bytecode caches outside run roots.
Sys.setenv(PYTHONDONTWRITEBYTECODE = "1")

run_id <- cfg$run$run_id
if (is.null(run_id) || !nzchar(run_id)) {
  run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
}
run_root <- file.path(cfg$run$run_root, run_id)

if (dir.exists(run_root) && !isTRUE(cfg$run$overwrite)) {
  if (isTRUE(cfg$run$auto_suffix_on_collision)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_id <- as.character(run_id)
    candidate_id <- sprintf("%s_rerun_%s", base_id, ts)
    candidate_root <- file.path(cfg$run$run_root, candidate_id)
    counter <- 1L
    while (dir.exists(candidate_root) && counter < 1000L) {
      counter <- counter + 1L
      candidate_id <- sprintf("%s_rerun_%s_%03d", base_id, ts, as.integer(counter))
      candidate_root <- file.path(cfg$run$run_root, candidate_id)
    }
    if (dir.exists(candidate_root)) {
      stop(
        sprintf("Run root exists and could not resolve collision automatically: %s", run_root),
        call. = FALSE
      )
    }
    message(sprintf("Run root collision detected; using auto-suffixed run_id: %s", candidate_id))
    run_id <- candidate_id
    run_root <- candidate_root
  } else {
    stop(sprintf("Run root exists and overwrite=false: %s", run_root), call. = FALSE)
  }
}

dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_root, "validate"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_root, "report"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_root, "env"), recursive = TRUE, showWarnings = FALSE)

cfg$run$run_id <- run_id
cfg$run$resolved_run_root <- run_root
cfg$run$resolved_config_path <- normalizePath(opts$config_path, mustWork = FALSE)

io_settings <- unified_get_run_io_settings(cfg)
if (isTRUE(io_settings$enabled)) {
  preflight_dir <- file.path(run_root, "preflight")
  preflight_log <- file.path(preflight_dir, "preflight.log")
  unified_run_io_preflight(
    path = run_root,
    io_settings = io_settings,
    check_point = "run_start",
    context = "unified_run preflight",
    report_dir = preflight_dir,
    stage_label = "run_start",
    log_path = preflight_log
  )
}

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required", call. = FALSE)
}

resolved_config_path <- file.path(run_root, "resolved_config.yaml")
unified_write_text_atomic <- function(text, out_path) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  tmp_path <- tempfile(pattern = paste0(".", basename(out_path), "."), tmpdir = dirname(out_path))
  ok <- FALSE
  on.exit({
    if (!ok && file.exists(tmp_path)) {
      unlink(tmp_path)
    }
  }, add = TRUE)
  writeLines(text, con = tmp_path, useBytes = TRUE)
  ok <- file.rename(tmp_path, out_path)
  if (!isTRUE(ok)) {
    stop(sprintf("Failed atomic write to %s", out_path), call. = FALSE)
  }
  invisible(out_path)
}

unified_write_text_atomic(
  yaml::as.yaml(cfg, indent.mapping.sequence = TRUE, precision = 15),
  resolved_config_path
)

repro_record <- unified_apply_seed(seed = cfg$run$seed, mode = cfg$run$repro_mode)
manifest <- unified_manifest_init(cfg, run_id = run_id, run_root = run_root, repo_root = repo_root, repro_record = repro_record)
preflight_dir <- file.path(run_root, "preflight")
if (dir.exists(preflight_dir)) {
  preflight_artifacts <- list.files(preflight_dir, pattern = "\\.json$|\\.log$", full.names = TRUE, recursive = FALSE)
  preflight_artifacts <- preflight_artifacts[file.exists(preflight_artifacts)]
  for (pf in preflight_artifacts) {
    manifest <- unified_manifest_add_artifact(manifest, pf, storage_scale = "text", role = "preflight")
  }
}
env_artifacts <- unified_capture_env_artifacts(run_root)
for (nm in names(env_artifacts)) {
  manifest <- unified_manifest_add_artifact(manifest, env_artifacts[[nm]], storage_scale = "text")
}
manifest_path <- file.path(run_root, "run_manifest.yaml")
unified_manifest_write(manifest, manifest_path)

enabled_stages <- names(cfg$stages)[vapply(cfg$stages, isTRUE, logical(1))]

cat("Unified run plan\n")
cat(sprintf("- config: %s\n", normalizePath(opts$config_path, mustWork = FALSE)))
cat(sprintf("- run_id: %s\n", run_id))
cat(sprintf("- run_root: %s\n", run_root))
cat(sprintf("- repro_mode: %s\n", cfg$run$repro_mode))
cat(sprintf("- seed: %s\n", cfg$run$seed))
cat(sprintf("- stages: %s\n", paste(enabled_stages, collapse = ", ")))
cat(sprintf("- resolved_config: %s\n", resolved_config_path))
cat(sprintf("- manifest: %s\n", manifest_path))

if (isTRUE(opts$dry_run) || isTRUE(cfg$run$dry_run)) {
  cat("Dry-run complete.\n")
  quit(save = "no", status = 0)
}

stage_order <- c("forecats", "data_prep_shared", "fit", "post", "validate", "report")
stage_index <- c(
  forecats = 1L,
  data_prep_shared = 1L,
  fit = 2L,
  post = 3L,
  validate = 4L,
  report = 5L
)
stage_log_paths <- c(
  forecats = file.path(run_root, "forecats", "forecats_pipeline.log"),
  data_prep_shared = file.path(run_root, "data_prep_shared", "data_prep_shared.log"),
  fit = file.path(run_root, "fit", "logs", "fit_stage.log"),
  post = file.path(run_root, "post", "logs", "post_runner.log"),
  validate = file.path(run_root, "validate", "validate.log"),
  report = file.path(run_root, "report", "summary.md")
)

stage_log_path <- function(stage) {
  path <- stage_log_paths[[stage]]
  if (is.null(path) || !nzchar(path)) return(NULL)
  path
}

cleanup_rdata_after_post_enabled <- {
  v <- tolower(trimws(Sys.getenv("CLEANUP_RDATA_AFTER_POST", "0")))
  v %in% c("1", "true", "yes")
}

cleanup_rdata_on_failure_enabled <- {
  default <- if (isTRUE(cleanup_rdata_after_post_enabled)) "1" else "0"
  v <- tolower(trimws(Sys.getenv("CLEANUP_RDATA_ON_FAILURE", default)))
  v %in% c("1", "true", "yes")
}

cleanup_rdata_under_run <- function(run_root) {
  rdata_paths <- list.files(
    run_root,
    pattern = "\\.[Rr][Dd]ata$",
    recursive = TRUE,
    full.names = TRUE
  )
  rdata_paths <- rdata_paths[file.exists(rdata_paths)]
  before <- length(rdata_paths)
  removed <- 0L
  if (before > 0L) {
    removed <- sum(file.remove(rdata_paths), na.rm = TRUE)
  }
  after <- length(list.files(
    run_root,
    pattern = "\\.[Rr][Dd]ata$",
    recursive = TRUE,
    full.names = TRUE
  ))
  list(before = as.integer(before), removed = as.integer(removed), remaining = as.integer(after))
}

run_stage <- function(stage, manifest) {
  switch(stage,
    forecats = unified_stage_forecats(cfg, run_root, repo_root, manifest),
    data_prep_shared = unified_stage_data_prep_shared(cfg, run_root, repo_root, manifest),
    fit = unified_stage_fit(cfg, run_root, repo_root, manifest),
    post = unified_stage_post(cfg, run_root, repo_root, manifest),
    validate = unified_stage_validate(cfg, run_root, repo_root, manifest),
    report = unified_stage_report(cfg, run_root, repo_root, manifest),
    stop(sprintf("Unknown stage: %s", stage), call. = FALSE)
  )
}

audit_enabled <- isTRUE(cfg$write_audit$enabled)
audit_threshold <- as.integer(cfg$write_audit$enforce_from_stage)
allowlist <- unlist(cfg$write_audit$allowlist_outside_run_root, use.names = FALSE)

for (stage in stage_order) {
  enabled_flag <- isTRUE(cfg$stages[[stage]])
  if (!enabled_flag) {
    manifest <- unified_manifest_stage_mark_skip(manifest, stage, log_path = stage_log_path(stage))
    unified_manifest_write(manifest, manifest_path)
    next
  }

  cat(sprintf("== Running stage: %s ==\n", stage))
  manifest <- unified_manifest_stage_mark_start(manifest, stage, log_path = stage_log_path(stage))
  unified_manifest_write(manifest, manifest_path)

  enforce_audit <- audit_enabled && (stage_index[[stage]] >= audit_threshold)
  stage_audit_dir <- file.path(run_root, "validate", "write_audit", stage)
  before_path <- file.path(stage_audit_dir, "fs_before.tsv")
  after_path <- file.path(stage_audit_dir, "fs_after.tsv")
  diff_path <- file.path(stage_audit_dir, "fs_diff.patch")

  if (enforce_audit) {
    unified_write_audit_snapshot(repo_root, run_root, before_path)
  }

  stage_error <- NULL
  result <- tryCatch(
    run_stage(stage, manifest),
    error = function(e) {
      stage_error <<- e
      NULL
    }
  )
  if (!is.null(stage_error)) {
    manifest <- unified_manifest_stage_mark_fail(manifest, stage, log_path = stage_log_path(stage))
    if (isTRUE(cleanup_rdata_on_failure_enabled)) {
      cleanup_info <- cleanup_rdata_under_run(run_root)
      manifest$rdata_cleanup <- manifest$rdata_cleanup %||% list()
      manifest$rdata_cleanup$on_failure <- cleanup_info
      cat(
        sprintf(
          "Failure-stage .RData cleanup: before=%d removed=%d remaining=%d\n",
          cleanup_info$before,
          cleanup_info$removed,
          cleanup_info$remaining
        )
      )
    }
    try(unified_manifest_write(manifest, manifest_path), silent = TRUE)
    stop(conditionMessage(stage_error), call. = FALSE)
  }

  manifest <- result$manifest
  manifest <- unified_manifest_stage_mark_pass(manifest, stage, log_path = stage_log_path(stage))
  unified_manifest_write(manifest, manifest_path)

  if (enforce_audit) {
    audit_error <- NULL
    tryCatch(
      {
        unified_write_audit_snapshot(repo_root, run_root, after_path)
        unified_write_audit_diff(before_path, after_path, diff_path)
        unified_write_audit_enforce(diff_path, allowlist = allowlist)
      },
      error = function(e) {
        audit_error <<- e
        NULL
      }
    )
    if (!is.null(audit_error)) {
      manifest <- unified_manifest_stage_mark_fail(manifest, stage, log_path = stage_log_path(stage))
      try(unified_manifest_write(manifest, manifest_path), silent = TRUE)
      stop(conditionMessage(audit_error), call. = FALSE)
    }
  }

  if (identical(stage, "post") && isTRUE(cleanup_rdata_after_post_enabled)) {
    cleanup_info <- cleanup_rdata_under_run(run_root)
    manifest$rdata_cleanup <- manifest$rdata_cleanup %||% list()
    manifest$rdata_cleanup$after_post <- cleanup_info
    unified_manifest_write(manifest, manifest_path)
    cat(
      sprintf(
        "Post-stage .RData cleanup: before=%d removed=%d remaining=%d\n",
        cleanup_info$before,
        cleanup_info$removed,
        cleanup_info$remaining
      )
    )
  }
}

manifest$timestamps$finished_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
unified_manifest_write(manifest, manifest_path)

cat("Unified run complete.\n")
quit(save = "no", status = 0)
