# unified/stages/stage_report.R

unified_extract_artifact_quantiles <- function(paths, family = c("multivar", "univar")) {
  family <- match.arg(family)
  paths <- as.character(paths)
  paths <- paths[nzchar(paths)]
  out <- integer(0)

  for (p in paths) {
    if (family == "multivar") {
      reg <- regmatches(
        p,
        regexec(
          "fit/exdqlm_multivar/([a-z0-9_\\-]+)/q=([0-9]{2})/outputs/DISC_variables_([0-9]+)_exAL_synth_DISC\\.RData$",
          p,
          perl = TRUE
        )
      )[[1]]
      if (length(reg) >= 4L) {
        q_val <- suppressWarnings(as.integer(reg[[4L]]))
        if (!is.finite(q_val)) {
          q_val <- suppressWarnings(as.integer(reg[[3L]]))
        }
        if (is.finite(q_val)) out <- c(out, q_val)
        next
      }
      reg <- regmatches(
        p,
        regexec(
          "fit/q=([0-9]{2})/outputs/DISC_variables_([0-9]+)_exAL_synth_DISC\\.RData$",
          p,
          perl = TRUE
        )
      )[[1]]
      if (length(reg) >= 3L) {
        q_val <- suppressWarnings(as.integer(reg[[3L]]))
        if (!is.finite(q_val)) {
          q_val <- suppressWarnings(as.integer(reg[[2L]]))
        }
        if (is.finite(q_val)) out <- c(out, q_val)
      }
      next
    }

    # Prefer quantile from directory q=<QQ>; fallback to filename numeric token.
    reg <- regmatches(
      p,
      regexec(
        "fit/exdqlm_univar/q=([0-9]{2})/outputs/variables_([0-9]+)_exAL_synth_DISC_uni\\.RData$",
        p,
        perl = TRUE
      )
    )[[1]]
    if (length(reg) >= 3L) {
      q_val <- suppressWarnings(as.integer(reg[[2L]]))
      if (!is.finite(q_val)) {
        q_val <- suppressWarnings(as.integer(reg[[3L]]))
      }
      if (is.finite(q_val)) out <- c(out, q_val)
      next
    }

    reg <- regmatches(
      p,
      regexec(
        "fit/exdqlm_univar/.*/variables_([0-9]+)_exAL_synth_DISC_uni\\.RData$",
        p,
        perl = TRUE
      )
    )[[1]]
    if (length(reg) >= 2L) {
      q_val <- suppressWarnings(as.integer(reg[[2L]]))
      if (is.finite(q_val)) out <- c(out, q_val)
    }
  }

  sort(unique(out))
}

unified_extract_multivar_quantiles_by_mode <- function(paths, primary_mode = "drop") {
  paths <- as.character(paths)
  paths <- paths[nzchar(paths)]
  out <- list()

  append_q <- function(mode, q_val) {
    if (!is.finite(q_val)) return(invisible(NULL))
    mode <- tolower(trimws(as.character(mode)))
    if (!nzchar(mode)) return(invisible(NULL))
    prev <- out[[mode]]
    if (is.null(prev)) prev <- integer(0)
    out[[mode]] <<- sort(unique(c(prev, as.integer(q_val))))
    invisible(NULL)
  }

  for (p in paths) {
    reg_mode <- regmatches(
      p,
      regexec(
        "fit/exdqlm_multivar/([a-z0-9_\\-]+)/q=([0-9]{2})/outputs/DISC_variables_([0-9]+)_exAL_synth_DISC\\.RData$",
        p,
        perl = TRUE
      )
    )[[1]]
    if (length(reg_mode) >= 4L) {
      q_val <- suppressWarnings(as.integer(reg_mode[[4L]]))
      if (!is.finite(q_val)) {
        q_val <- suppressWarnings(as.integer(reg_mode[[3L]]))
      }
      append_q(reg_mode[[2L]], q_val)
      next
    }

    reg_legacy <- regmatches(
      p,
      regexec(
        "fit/q=([0-9]{2})/outputs/DISC_variables_([0-9]+)_exAL_synth_DISC\\.RData$",
        p,
        perl = TRUE
      )
    )[[1]]
    if (length(reg_legacy) >= 3L) {
      q_val <- suppressWarnings(as.integer(reg_legacy[[3L]]))
      if (!is.finite(q_val)) {
        q_val <- suppressWarnings(as.integer(reg_legacy[[2L]]))
      }
      append_q(primary_mode, q_val)
    }
  }

  out
}

unified_merge_quantiles_by_mode <- function(lhs, rhs) {
  out <- list()
  modes <- sort(unique(c(names(lhs), names(rhs))))
  for (mode in modes) {
    vals <- c(lhs[[mode]], rhs[[mode]])
    vals <- suppressWarnings(as.integer(vals))
    vals <- vals[is.finite(vals)]
    out[[mode]] <- sort(unique(vals))
  }
  out
}

unified_detect_multivar_quantiles_by_mode_from_run_root <- function(run_root, primary_mode = "drop") {
  out <- list()
  run_root <- normalizePath(run_root, mustWork = FALSE)
  fit_root <- file.path(run_root, "fit", "exdqlm_multivar")

  append_q <- function(mode, q_val) {
    mode <- tolower(trimws(as.character(mode)))
    if (!nzchar(mode)) return(invisible(NULL))
    q_val <- suppressWarnings(as.integer(q_val))
    if (!is.finite(q_val)) return(invisible(NULL))
    prev <- out[[mode]]
    if (is.null(prev)) prev <- integer(0)
    out[[mode]] <<- sort(unique(c(prev, q_val)))
    invisible(NULL)
  }

  if (dir.exists(fit_root)) {
    q_dirs <- list.dirs(fit_root, recursive = TRUE, full.names = TRUE)
    q_dirs <- q_dirs[grepl("/q=[0-9]{2}$", q_dirs, perl = TRUE)]
    for (q_dir in q_dirs) {
      rel <- sub(paste0("^", gsub("([\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}\\\\])", "\\\\\\1", fit_root), "/?"), "", q_dir, perl = TRUE)
      parts <- strsplit(rel, "/", fixed = TRUE)[[1L]]
      q_token <- parts[[length(parts)]]
      q_val <- suppressWarnings(as.integer(sub("^q=", "", q_token)))
      mode <- if (length(parts) >= 2L) parts[[length(parts) - 1L]] else primary_mode
      append_q(mode, q_val)
    }
  }

  legacy_fit_root <- file.path(run_root, "fit")
  if (dir.exists(legacy_fit_root)) {
    q_dirs <- list.dirs(legacy_fit_root, recursive = FALSE, full.names = TRUE)
    q_dirs <- q_dirs[grepl("/q=[0-9]{2}$", q_dirs, perl = TRUE)]
    for (q_dir in q_dirs) {
      q_val <- suppressWarnings(as.integer(sub("^q=", "", basename(q_dir))))
      append_q(primary_mode, q_val)
    }
  }

  out
}

unified_detect_ndlm_output_present <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[nzchar(paths)]
  any(grepl(
    "fit/ndlm_main/outputs/(DISC_variables_50_NDLM_synth_DISC|ndlm_main_state|ndlm_main[^/]*)\\.RData$",
    paths,
    perl = TRUE
  ))
}

unified_detect_ndlm_univar_output_present <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[nzchar(paths)]
  any(grepl(
    "fit/ndlm_univar/outputs/(DISC_variables_50_NDLM_univar_synth_DISC|ndlm_univar_state|ndlm_univar[^/]*)\\.RData$",
    paths,
    perl = TRUE
  ))
}

unified_stage_report <- function(cfg, run_root, repo_root, manifest) {
  report_root <- file.path(run_root, "report")
  dir.create(report_root, recursive = TRUE, showWarnings = FALSE)

  compare_report_path <- manifest$validation$compare_report_path
  compare_metrics <- list(matched = NA_integer_, missing = NA_integer_, extra = NA_integer_, mismatched = NA_integer_)
  env_drift_status <- NA_character_
  detclim_validation <- NULL
  if (!is.null(compare_report_path) && file.exists(compare_report_path) && requireNamespace("jsonlite", quietly = TRUE)) {
    cmp <- tryCatch(jsonlite::read_json(compare_report_path, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(cmp) && !is.null(cmp$metrics)) {
      compare_metrics <- as.list(cmp$metrics)
    }
    if (!is.null(cmp) && !is.null(cmp$env_drift) && !is.null(cmp$env_drift$status)) {
      env_drift_status <- as.character(cmp$env_drift$status)
    }
    if (!is.null(cmp) && !is.null(cmp$deterministic_climate)) {
      detclim_validation <- cmp$deterministic_climate
    }
  }

  diff_files <- list.files(file.path(run_root, "validate", "write_audit"), pattern = "fs_diff.patch", recursive = TRUE, full.names = TRUE)
  write_audit_clean <- if (length(diff_files) == 0) TRUE else all(vapply(diff_files, function(p) !file.exists(p) || file.info(p)$size == 0, logical(1)))

  profile_summary_path <- NULL
  if (isTRUE(cfg$post$profile)) {
    profile_dir <- file.path(run_root, "post", "profile", cfg$run$run_id)
    run_log_path <- file.path(run_root, "post", "logs", cfg$run$run_id, "run_log.txt")
    profile_summary_path <- file.path(report_root, "profile_summary.md")
    summarize_script <- file.path(repo_root, "scripts", "summarize_profile_run.py")
    if (file.exists(summarize_script) && dir.exists(profile_dir)) {
      cmd_out <- system2(
        "python3",
        c(
          summarize_script,
          "--project-root", repo_root,
          "--run-id", cfg$run$run_id,
          "--profile-dir", profile_dir,
          "--run-log-path", run_log_path,
          "--out", profile_summary_path
        ),
        stdout = TRUE,
        stderr = TRUE
      )
      status <- attr(cmd_out, "status")
      if (!is.null(status) && status != 0) {
        profile_summary_path <- NULL
      }
    } else {
      profile_summary_path <- NULL
    }
  }

  input_hashes <- lapply(manifest$inputs, function(x) list(path = x$path, sha256 = x$sha256, storage_scale = x$storage_scale))

  quantiles_expected <- suppressWarnings(as.integer(round(as.numeric(cfg$fit$quantiles) * 100)))
  quantiles_expected <- sort(unique(quantiles_expected[is.finite(quantiles_expected)]))
  resolve_multivar_modes <- function(cfg_obj) {
    if (exists("unified_resolve_multivar_transfer_modes", mode = "function")) {
      return(unified_resolve_multivar_transfer_modes(cfg_obj))
    }
    mode <- as.character(unified_get(
      cfg_obj,
      c("models", "exdqlm_multivar", "forecast_transfer_mode"),
      default = "drop"
    ))
    if (!length(mode) || is.na(mode[[1L]]) || !nzchar(mode[[1L]])) mode <- "drop" else mode <- mode[[1L]]
    tolower(trimws(mode))
  }
  resolve_primary_multivar_mode <- function(cfg_obj, modes) {
    if (exists("unified_resolve_multivar_primary_transfer_mode", mode = "function")) {
      return(unified_resolve_multivar_primary_transfer_mode(cfg_obj, modes = modes))
    }
    modes <- unique(tolower(trimws(as.character(modes))))
    modes <- modes[nzchar(modes)]
    if (!length(modes)) return("drop")
    mode <- as.character(unified_get(
      cfg_obj,
      c("models", "exdqlm_multivar", "forecast_transfer_mode"),
      default = modes[[1L]]
    ))
    if (!length(mode) || is.na(mode[[1L]]) || !nzchar(mode[[1L]])) {
      return(modes[[1L]])
    }
    mode <- tolower(trimws(mode[[1L]]))
    if (mode %in% modes) mode else modes[[1L]]
  }
  multivar_transfer_modes <- resolve_multivar_modes(cfg)
  primary_multivar_transfer_mode <- resolve_primary_multivar_mode(cfg, multivar_transfer_modes)

  artifact_paths <- unlist(lapply(manifest$artifacts, function(x) {
    val <- x$path
    if (is.null(val)) "" else as.character(val)
  }), use.names = FALSE)
  artifact_paths <- artifact_paths[nzchar(artifact_paths)]

  multivar_found_by_mode_raw <- unified_extract_multivar_quantiles_by_mode(
    artifact_paths,
    primary_mode = primary_multivar_transfer_mode
  )
  multivar_found_by_mode_fs <- unified_detect_multivar_quantiles_by_mode_from_run_root(
    run_root,
    primary_mode = primary_multivar_transfer_mode
  )
  multivar_found_by_mode_raw <- unified_merge_quantiles_by_mode(
    multivar_found_by_mode_raw,
    multivar_found_by_mode_fs
  )
  multivar_found_by_mode <- list()
  if (isTRUE(cfg$models$run_exdqlm_multivar)) {
    if (!length(multivar_transfer_modes)) {
      multivar_transfer_modes <- primary_multivar_transfer_mode
    }
    for (mode in multivar_transfer_modes) {
      q_vals <- multivar_found_by_mode_raw[[mode]]
      if (is.null(q_vals)) q_vals <- integer(0)
      multivar_found_by_mode[[mode]] <- sort(unique(as.integer(q_vals)))
    }
  }
  multivar_found <- if (length(multivar_found_by_mode)) {
    sort(unique(as.integer(unlist(multivar_found_by_mode, use.names = FALSE))))
  } else {
    integer(0)
  }
  univar_found <- unified_extract_artifact_quantiles(artifact_paths, family = "univar")
  ndlm_present <- unified_detect_ndlm_output_present(artifact_paths)
  ndlm_univar_present <- unified_detect_ndlm_univar_output_present(artifact_paths)

  families_summary <- list(
    exdqlm_multivar = list(
      enabled = isTRUE(cfg$models$run_exdqlm_multivar),
      transfer_modes = if (isTRUE(cfg$models$run_exdqlm_multivar)) multivar_transfer_modes else character(0),
      primary_transfer_mode = if (isTRUE(cfg$models$run_exdqlm_multivar)) primary_multivar_transfer_mode else NA_character_,
      quantiles_expected_by_mode = if (isTRUE(cfg$models$run_exdqlm_multivar)) {
        stats::setNames(
          rep(list(quantiles_expected), length(multivar_transfer_modes)),
          multivar_transfer_modes
        )
      } else {
        list()
      },
      quantiles_found_by_mode = if (isTRUE(cfg$models$run_exdqlm_multivar)) multivar_found_by_mode else list(),
      quantiles_expected = if (isTRUE(cfg$models$run_exdqlm_multivar)) quantiles_expected else integer(0),
      quantiles_found = if (isTRUE(cfg$models$run_exdqlm_multivar)) multivar_found else integer(0)
    ),
    exdqlm_univar = list(
      enabled = isTRUE(cfg$models$run_exdqlm_univar),
      quantiles_expected = if (isTRUE(cfg$models$run_exdqlm_univar)) quantiles_expected else integer(0),
      quantiles_found = if (isTRUE(cfg$models$run_exdqlm_univar)) univar_found else integer(0)
    ),
    ndlm_main = list(
      enabled = isTRUE(cfg$models$run_ndlm_main),
      output_present = if (isTRUE(cfg$models$run_ndlm_main)) ndlm_present else FALSE
    ),
    ndlm_univar = list(
      enabled = isTRUE(cfg$models$run_ndlm_univar),
      output_present = if (isTRUE(cfg$models$run_ndlm_univar)) ndlm_univar_present else FALSE
    )
  )

  detclim_manifest <- manifest$deterministic_climate
  detclim_summary <- list(
    enabled = if (is.list(detclim_manifest)) isTRUE(detclim_manifest$enabled) else FALSE,
    status = if (!is.null(detclim_validation) && !is.null(detclim_validation$status)) {
      as.character(detclim_validation$status)
    } else if (is.list(detclim_manifest) && isTRUE(detclim_manifest$enabled)) {
      "configured"
    } else {
      "disabled"
    },
    handoff_root = if (is.list(detclim_manifest) && !is.null(detclim_manifest$handoff_root)) as.character(detclim_manifest$handoff_root) else NA_character_,
    horizon_days = if (is.list(detclim_manifest)) detclim_manifest$horizon_days else NA,
    require_full_horizon = if (is.list(detclim_manifest)) detclim_manifest$require_full_horizon else NA,
    cutoff_date = if (is.list(detclim_manifest) && !is.null(detclim_manifest$cutoff_date)) as.character(detclim_manifest$cutoff_date) else NA_character_,
    summary_path = if (is.list(detclim_manifest) && !is.null(detclim_manifest$summary_path)) as.character(detclim_manifest$summary_path) else NA_character_,
    summary_sha256 = if (is.list(detclim_manifest) && !is.null(detclim_manifest$summary_sha256)) as.character(detclim_manifest$summary_sha256) else NA_character_,
    precip = if (is.list(detclim_manifest) && is.list(detclim_manifest$precip)) detclim_manifest$precip else list(),
    soil = if (is.list(detclim_manifest) && is.list(detclim_manifest$soil)) detclim_manifest$soil else list(),
    pca = if (is.list(detclim_manifest) && is.list(detclim_manifest$pca)) detclim_manifest$pca else list(),
    validation = detclim_validation
  )

  summary_json <- list(
    run_id = cfg$run$run_id,
    run_root = run_root,
    git_commit = manifest$git$commit,
    repro_mode = cfg$run$repro_mode,
    seed = cfg$run$seed,
    stages_enabled = names(cfg$stages)[vapply(cfg$stages, isTRUE, logical(1))],
    input_hashes = input_hashes,
    drift_metrics = compare_metrics,
    env_drift_status = env_drift_status,
    validation_status = manifest$validation$status,
    change_approval_status = manifest$change_approval$status,
    write_audit_clean = write_audit_clean,
    compare_report_path = compare_report_path,
    profile_summary_path = if (is.null(profile_summary_path)) NA_character_ else profile_summary_path,
    artifacts_recorded = length(manifest$artifacts),
    deterministic_climate = detclim_summary,
    rdata_cleanup = manifest$rdata_cleanup,
    report = list(
      families = families_summary
    )
  )

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(summary_json, path = file.path(report_root, "summary.json"), auto_unbox = TRUE, pretty = TRUE)
  }

  summary_lines <- c(
    sprintf("# Unified Run Summary (%s)", cfg$run$run_id),
    "",
    "## Run",
    sprintf("- run_root: `%s`", run_root),
    sprintf("- git_commit: `%s`", manifest$git$commit),
    sprintf("- repro_mode: `%s`", cfg$run$repro_mode),
    sprintf("- seed: `%s`", cfg$run$seed),
    sprintf("- stages_enabled: `%s`", paste(summary_json$stages_enabled, collapse = ", ")),
    "",
    "## Validation",
    sprintf("- validation_status: `%s`", manifest$validation$status),
    sprintf("- compare_report: `%s`", compare_report_path),
    sprintf("- drift metrics: matched=%s missing=%s extra=%s mismatched=%s",
            compare_metrics$matched, compare_metrics$missing, compare_metrics$extra, compare_metrics$mismatched),
    sprintf("- env_drift_status: `%s`", env_drift_status),
    sprintf("- write_audit_clean: `%s`", write_audit_clean),
    sprintf("- change_approval.status: `%s`", manifest$change_approval$status),
    "",
    "## Inputs",
    sprintf("- input artifacts hashed: `%d`", length(input_hashes)),
    sprintf("- deterministic_climate.enabled: `%s`", detclim_summary$enabled),
    sprintf("- deterministic_climate.status: `%s`", detclim_summary$status),
    sprintf("- deterministic_climate.handoff_root: `%s`", detclim_summary$handoff_root),
    sprintf("- deterministic_climate.horizon_days: `%s`", detclim_summary$horizon_days),
    sprintf("- deterministic_climate.require_full_horizon: `%s`", detclim_summary$require_full_horizon),
    sprintf("- deterministic_climate.summary_path: `%s`", detclim_summary$summary_path),
    sprintf("- deterministic_climate.precip: source=`%s` reduction=`%s` future_rows=`%s`",
            if (!is.null(detclim_summary$precip$source)) detclim_summary$precip$source else NA_character_,
            if (!is.null(detclim_summary$precip$reduction)) detclim_summary$precip$reduction else NA_character_,
            if (!is.null(detclim_summary$precip$future_rows)) detclim_summary$precip$future_rows else NA),
    sprintf("- deterministic_climate.soil: source=`%s` reduction=`%s` future_rows=`%s` porosity=`%s`",
            if (!is.null(detclim_summary$soil$source)) detclim_summary$soil$source else NA_character_,
            if (!is.null(detclim_summary$soil$reduction)) detclim_summary$soil$reduction else NA_character_,
            if (!is.null(detclim_summary$soil$future_rows)) detclim_summary$soil$future_rows else NA,
            if (!is.null(detclim_summary$soil$porosity)) detclim_summary$soil$porosity else NA),
    "",
    "## Outputs",
    sprintf("- artifacts_recorded: `%d`", length(manifest$artifacts)),
    sprintf("- summary_json: `%s`", file.path(report_root, "summary.json")),
    sprintf("- families.exdqlm_multivar.enabled: `%s`", families_summary$exdqlm_multivar$enabled),
    sprintf("- families.exdqlm_multivar.transfer_modes: `%s`", paste(families_summary$exdqlm_multivar$transfer_modes, collapse = ", ")),
    sprintf("- families.exdqlm_multivar.primary_transfer_mode: `%s`", families_summary$exdqlm_multivar$primary_transfer_mode),
    sprintf("- families.exdqlm_multivar.quantiles_found: `%s`", paste(families_summary$exdqlm_multivar$quantiles_found, collapse = ", ")),
    sprintf("- families.exdqlm_univar.enabled: `%s`", families_summary$exdqlm_univar$enabled),
    sprintf("- families.exdqlm_univar.quantiles_found: `%s`", paste(families_summary$exdqlm_univar$quantiles_found, collapse = ", ")),
    sprintf("- families.ndlm_main.enabled: `%s`", families_summary$ndlm_main$enabled),
    sprintf("- families.ndlm_main.output_present: `%s`", families_summary$ndlm_main$output_present),
    sprintf("- families.ndlm_univar.enabled: `%s`", families_summary$ndlm_univar$enabled),
    sprintf("- families.ndlm_univar.output_present: `%s`", families_summary$ndlm_univar$output_present),
    sprintf("- rdata_cleanup.after_post.remaining: `%s`",
            if (!is.null(manifest$rdata_cleanup$after_post$remaining)) manifest$rdata_cleanup$after_post$remaining else NA)
  )

  if (isTRUE(families_summary$exdqlm_multivar$enabled) &&
      length(families_summary$exdqlm_multivar$quantiles_found_by_mode) > 0L) {
    for (mode in names(families_summary$exdqlm_multivar$quantiles_found_by_mode)) {
      mode_q <- families_summary$exdqlm_multivar$quantiles_found_by_mode[[mode]]
      summary_lines <- c(
        summary_lines,
        sprintf("- families.exdqlm_multivar.quantiles_found_by_mode.%s: `%s`", mode, paste(mode_q, collapse = ", "))
      )
    }
  }

  if (!is.null(profile_summary_path)) {
    summary_lines <- c(summary_lines, sprintf("- profile_summary: `%s`", profile_summary_path))
  }

  writeLines(summary_lines, file.path(report_root, "summary.md"), useBytes = TRUE)

  list(manifest = manifest)
}
