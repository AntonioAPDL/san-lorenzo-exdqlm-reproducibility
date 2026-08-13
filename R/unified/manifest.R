# unified/manifest.R

unified_git_info <- function(repo_root) {
  read_cmd <- function(...) {
    out <- tryCatch(system2("git", c("-C", repo_root, ...), stdout = TRUE, stderr = FALSE), error = function(e) character(0))
    if (length(out) == 0) return("unknown")
    out[[1]]
  }

  dirty_lines <- tryCatch(system2("git", c("-C", repo_root, "status", "--porcelain"), stdout = TRUE, stderr = FALSE), error = function(e) character(0))

  list(
    commit = read_cmd("rev-parse", "HEAD"),
    branch = read_cmd("rev-parse", "--abbrev-ref", "HEAD"),
    dirty = length(dirty_lines) > 0
  )
}

unified_collect_input_records <- function(cfg) {
  records <- list()

  add_record <- function(path, storage_scale) {
    if (is.null(path) || !nzchar(path)) return()
    records[[length(records) + 1]] <<- list(
      path = path,
      sha256 = unified_sha256(path),
      storage_scale = storage_scale
    )
  }

  if (isTRUE(cfg$stages$fit)) {
    add_record(cfg$inputs$fit$parameters_path, "parameters_text")
    add_record(cfg$inputs$fit$retros_path, cfg$inputs$fit$retros_storage_scale)
    add_record(cfg$inputs$fit$nws_forecast_path, cfg$inputs$fit$nws_storage_scale)
    add_record(cfg$inputs$fit$glofas_forecast_path, cfg$inputs$fit$glofas_storage_scale)
  }

  if (isTRUE(cfg$stages$forecats)) {
    mode <- cfg$inputs$forecats$mode
    if (identical(mode, "build")) {
      add_record(cfg$inputs$forecats$pipeline_config_path, "yaml_config")
    }
    if (identical(mode, "use_existing")) {
      add_record(cfg$inputs$forecats$existing_bundle_path, "bundle")
    }
  }

  records
}

unified_manifest_stage_order <- function(cfg = NULL) {
  canonical <- c("forecats", "data_prep_shared", "fit", "post", "validate", "report")
  if (is.null(cfg) || is.null(cfg$stages) || is.null(names(cfg$stages))) {
    return(canonical)
  }
  extras <- setdiff(names(cfg$stages), canonical)
  c(canonical, extras)
}

unified_manifest_stage_map_init <- function(cfg = NULL) {
  out <- list()
  for (stage in unified_manifest_stage_order(cfg)) {
    out[[stage]] <- list(
      status = "pending",
      started_at_utc = NULL,
      finished_at_utc = NULL,
      log_path = NULL
    )
  }
  out
}

unified_manifest_stage_mark_start <- function(manifest, stage, log_path = NULL) {
  if (is.null(manifest$stages) || !is.list(manifest$stages)) {
    manifest$stages <- list()
  }
  if (is.null(manifest$stages[[stage]]) || !is.list(manifest$stages[[stage]])) {
    manifest$stages[[stage]] <- list()
  }
  manifest$stages[[stage]]$status <- "pending"
  manifest$stages[[stage]]$started_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  manifest$stages[[stage]]$finished_at_utc <- NULL
  if (!is.null(log_path) && nzchar(as.character(log_path))) {
    manifest$stages[[stage]]$log_path <- log_path
  }
  manifest
}

unified_manifest_stage_mark_pass <- function(manifest, stage, log_path = NULL) {
  if (is.null(manifest$stages) || !is.list(manifest$stages)) {
    manifest$stages <- list()
  }
  if (is.null(manifest$stages[[stage]]) || !is.list(manifest$stages[[stage]])) {
    manifest$stages[[stage]] <- list()
  }
  if (is.null(manifest$stages[[stage]]$started_at_utc)) {
    manifest$stages[[stage]]$started_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  manifest$stages[[stage]]$status <- "pass"
  manifest$stages[[stage]]$finished_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (!is.null(log_path) && nzchar(as.character(log_path))) {
    manifest$stages[[stage]]$log_path <- log_path
  }
  manifest
}

unified_manifest_stage_mark_fail <- function(manifest, stage, log_path = NULL) {
  if (is.null(manifest$stages) || !is.list(manifest$stages)) {
    manifest$stages <- list()
  }
  if (is.null(manifest$stages[[stage]]) || !is.list(manifest$stages[[stage]])) {
    manifest$stages[[stage]] <- list()
  }
  if (is.null(manifest$stages[[stage]]$started_at_utc)) {
    manifest$stages[[stage]]$started_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  manifest$stages[[stage]]$status <- "fail"
  manifest$stages[[stage]]$finished_at_utc <- NULL
  if (!is.null(log_path) && nzchar(as.character(log_path))) {
    manifest$stages[[stage]]$log_path <- log_path
  }
  manifest
}

unified_manifest_stage_mark_skip <- function(manifest, stage, log_path = NULL) {
  if (is.null(manifest$stages) || !is.list(manifest$stages)) {
    manifest$stages <- list()
  }
  manifest$stages[[stage]] <- list(
    status = "skip",
    started_at_utc = NULL,
    finished_at_utc = NULL,
    log_path = if (!is.null(log_path) && nzchar(as.character(log_path))) log_path else NULL
  )
  manifest
}

unified_manifest_init <- function(cfg, run_id, run_root, repo_root, repro_record) {
  git <- unified_git_info(repo_root)
  inputs <- unified_collect_input_records(cfg)
  multivar_mode <- unified_get(cfg, c("models", "exdqlm_multivar", "implementation_mode"), default = "legacy_bridge")
  univar_mode <- unified_get(cfg, c("models", "exdqlm_univar", "implementation_mode"), default = "legacy_bridge")
  ndlm_mode <- unified_get(cfg, c("models", "ndlm_main", "implementation_mode"), default = "theory_aligned")
  ndlm_univar_mode <- unified_get(cfg, c("models", "ndlm_univar", "implementation_mode"), default = "theory_aligned_closed_form")
  multivar_authoritative <- isTRUE(unified_get(cfg, c("models", "exdqlm_multivar", "authoritative"), default = TRUE))
  univar_authoritative <- isTRUE(unified_get(cfg, c("models", "exdqlm_univar", "authoritative"), default = FALSE))
  ndlm_authoritative <- isTRUE(unified_get(cfg, c("models", "ndlm_main", "authoritative"), default = FALSE))
  ndlm_univar_authoritative <- isTRUE(unified_get(cfg, c("models", "ndlm_univar", "authoritative"), default = FALSE))

  list(
    manifest_version = 1L,
    config_version = cfg$config_version,
    run_id = run_id,
    run_root = run_root,
    timestamps = list(
      started_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      finished_at_utc = NULL
    ),
    git = git,
    repro = list(
      mode = cfg$run$repro_mode,
      seed = as.integer(cfg$run$seed),
      thread_env = list(
        OMP_NUM_THREADS = Sys.getenv("OMP_NUM_THREADS", ""),
        OPENBLAS_NUM_THREADS = Sys.getenv("OPENBLAS_NUM_THREADS", ""),
        MKL_NUM_THREADS = Sys.getenv("MKL_NUM_THREADS", ""),
        VECLIB_MAXIMUM_THREADS = Sys.getenv("VECLIB_MAXIMUM_THREADS", ""),
        NUMEXPR_NUM_THREADS = Sys.getenv("NUMEXPR_NUM_THREADS", "")
      ),
      r_rng = list(
        fit = paste(repro_record$fit_rng, collapse = "/"),
        post = paste(repro_record$post_rng, collapse = "/")
      )
    ),
    stages = unified_manifest_stage_map_init(cfg),
    families = list(
      exdqlm_multivar = list(
        enabled = isTRUE(cfg$models$run_exdqlm_multivar),
        implementation_mode = multivar_mode,
        authoritative = multivar_authoritative
      ),
      exdqlm_univar = list(
        enabled = isTRUE(cfg$models$run_exdqlm_univar),
        implementation_mode = univar_mode,
        authoritative = univar_authoritative
      ),
      ndlm_main = list(
        enabled = isTRUE(cfg$models$run_ndlm_main),
        implementation_mode = ndlm_mode,
        kalman_backend = as.character(unified_get(cfg, c("models", "ndlm_main", "kalman_backend"), default = "cpp")),
        authoritative = ndlm_authoritative
      ),
      ndlm_univar = list(
        enabled = isTRUE(cfg$models$run_ndlm_univar),
        implementation_mode = ndlm_univar_mode,
        kalman_backend = as.character(unified_get(cfg, c("models", "ndlm_univar", "kalman_backend"), default = "cpp")),
        authoritative = ndlm_univar_authoritative
      )
    ),
    inputs = inputs,
    artifacts = list(),
    scale_history = list(),
    change_approval = list(
      required = TRUE,
      status = "pending",
      approver = NULL,
      approved_at_utc = NULL,
      rationale = NULL,
      expected_diffs = list(
        allowed_path_patterns = list(),
        disallowed_path_patterns = list()
      ),
      metric_thresholds = list(
        numeric_abs_max = 0,
        numeric_rel_max = 0,
        pixel_max_abs = 0
      ),
      evidence_paths = list(
        compare_report = NULL,
        diff_summary = NULL
      )
    ),
    validation = list(
      compare_report_path = file.path(run_root, "validate", "compare_report.json"),
      write_audit_diff_path = file.path(run_root, "validate", "write_audit", "fs_diff.patch"),
      validator_profile = unified_get(cfg, c("validation", "profile"), default = "production"),
      status = "pending"
    ),
    deterministic_climate = list(
      enabled = isTRUE(unified_get(cfg, c("inputs", "deterministic_climate", "enabled"), default = FALSE)),
      handoff_root = unified_get(cfg, c("inputs", "deterministic_climate", "handoff_root"), default = NULL),
      horizon_days = unified_get(cfg, c("inputs", "deterministic_climate", "horizon_days"), default = NULL),
      require_full_horizon = unified_get(cfg, c("inputs", "deterministic_climate", "require_full_horizon"), default = TRUE),
      precip = list(
        source = unified_get(cfg, c("inputs", "deterministic_climate", "precip", "source"), default = "gefs_apcp"),
        reduction = unified_get(cfg, c("inputs", "deterministic_climate", "precip", "reduction"), default = "mean")
      ),
      soil = list(
        source = unified_get(cfg, c("inputs", "deterministic_climate", "soil", "source"), default = "nwm_soilsat_top"),
        reduction = unified_get(cfg, c("inputs", "deterministic_climate", "soil", "reduction"), default = "mean")
      ),
      summary_path = NULL,
      summary_sha256 = NULL,
      precip_future_path = NULL,
      soil_future_path = NULL,
      soil_family_support_path = NULL,
      verified_in_data_prep_shared = FALSE
    ),
    schema_migration = list(
      previous_manifest_version = NULL,
      migration_notes = NULL
    )
  )
}

unified_manifest_add_artifact <- function(
  manifest,
  path,
  storage_scale,
  analysis_scale = NULL,
  flow_domain = NULL,
  role = NULL
) {
  artifact <- list(
    path = path,
    sha256 = if (file.exists(path)) unified_sha256(path) else NA_character_,
    storage_scale = storage_scale
  )
  if (!is.null(analysis_scale)) artifact$analysis_scale <- analysis_scale
  if (!is.null(flow_domain)) artifact$flow_domain <- flow_domain
  if (!is.null(role)) artifact$role <- role

  manifest$artifacts[[length(manifest$artifacts) + 1]] <- artifact
  manifest
}

unified_manifest_add_scale_history <- function(manifest, artifact, from_scale, to_scale, transform) {
  manifest$scale_history[[length(manifest$scale_history) + 1]] <- list(
    artifact = artifact,
    from_scale = from_scale,
    to_scale = to_scale,
    transform = transform
  )
  manifest
}

unified_manifest_write <- function(manifest, out_path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to write unified manifest")
  }
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  yaml_text <- yaml::as.yaml(manifest, indent.mapping.sequence = TRUE)
  tmp_path <- tempfile(pattern = paste0(".", basename(out_path), "."), tmpdir = dirname(out_path))
  ok <- FALSE
  on.exit({
    if (!ok && file.exists(tmp_path)) {
      unlink(tmp_path)
    }
  }, add = TRUE)
  writeLines(yaml_text, con = tmp_path, useBytes = TRUE)
  ok <- file.rename(tmp_path, out_path)
  if (!isTRUE(ok)) {
    stop(sprintf("Failed atomic manifest write to %s", out_path), call. = FALSE)
  }
  invisible(out_path)
}
