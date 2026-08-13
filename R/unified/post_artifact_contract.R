# unified/post_artifact_contract.R

unified_iso_utc <- function(x = Sys.time()) {
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

unified_post_artifact_type <- function(path, scope = c("outputs", "cache")) {
  scope <- match.arg(scope)
  base <- basename(path)
  ext <- tolower(tools::file_ext(base))

  if (scope == "cache") {
    if (base %in% c("y_reps_f.rds", "y_reps.rds", "y_reps_f_new.rds", "y_reps_new.rds", "y_hist_uni.rds", "y_forecast_uni.rds")) {
      return("synthesis_cache")
    }
    if (ext == "rds") return("cache_rds")
    return("cache_other")
  }

  if (base == "post_smoke_marker.txt") return("smoke_marker")
  if (base %in% c("post_artifacts_manifest.csv", "post_artifacts_summary.json")) return("post_artifact_meta")

  if (ext %in% c("png", "pdf", "svg", "jpg", "jpeg", "tif", "tiff")) return("figure")

  if (base %in% c(
    "gamma_summary.csv", "sigma_summary.csv", "covariate_effects_summary.csv",
    "gamma_summary.rds", "sigma_summary.rds", "covariate_effects_summary.rds",
    "gamma_summary.tex", "sigma_summary.tex", "covariate_effects_summary.tex",
    "posterior_table_exports_manifest.csv", "posterior_table_exports_README.md"
  )) {
    return("table")
  }
  if (ext %in% c("csv", "rds", "tex", "md") && grepl("(summary|table|manifest)", base, ignore.case = TRUE)) {
    return("table")
  }

  if (ext %in% c("csv", "tsv")) return("tabular")
  if (ext %in% c("txt", "json", "yaml", "yml", "log")) return("text")
  "other"
}

unified_collect_post_artifacts <- function(outputs_dir, cache_dir = NULL) {
  collect_scope <- function(root_dir, scope) {
    if (is.null(root_dir) || !nzchar(root_dir) || !dir.exists(root_dir)) {
      return(data.frame(
        scope = character(0),
        relative_path = character(0),
        artifact_type = character(0),
        extension = character(0),
        bytes = numeric(0),
        modified_at_utc = character(0),
        abs_path = character(0),
        stringsAsFactors = FALSE
      ))
    }

    root_abs <- normalizePath(root_dir, mustWork = TRUE)
    files <- list.files(root_abs, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
    files <- files[file.info(files)$isdir %in% FALSE]
    if (length(files) == 0L) {
      return(data.frame(
        scope = character(0),
        relative_path = character(0),
        artifact_type = character(0),
        extension = character(0),
        bytes = numeric(0),
        modified_at_utc = character(0),
        abs_path = character(0),
        stringsAsFactors = FALSE
      ))
    }

    prefix <- paste0(root_abs, .Platform$file.sep)
    rel <- ifelse(startsWith(files, prefix), substring(files, nchar(prefix) + 1L), basename(files))
    ext <- tolower(tools::file_ext(files))
    info <- file.info(files)
    mtime <- ifelse(is.na(info$mtime), "", unified_iso_utc(info$mtime))

    data.frame(
      scope = rep(scope, length(files)),
      relative_path = rel,
      artifact_type = vapply(files, function(p) unified_post_artifact_type(p, scope = scope), character(1)),
      extension = ext,
      bytes = as.numeric(info$size),
      modified_at_utc = mtime,
      abs_path = files,
      stringsAsFactors = FALSE
    )
  }

  out_df <- collect_scope(outputs_dir, "outputs")
  cache_df <- collect_scope(cache_dir, "cache")
  all_df <- rbind(out_df, cache_df)
  if (nrow(all_df) == 0L) {
    return(all_df)
  }
  ord <- order(all_df$scope, all_df$relative_path, method = "radix", na.last = TRUE)
  rownames(all_df) <- NULL
  all_df[ord, , drop = FALSE]
}

unified_validate_synthesis_cube_file <- function(path, context) {
  if (!file.exists(path)) {
    return(list(ok = FALSE, message = sprintf("%s missing: %s", context, path)))
  }
  obj <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(obj, "error")) {
    return(list(ok = FALSE, message = sprintf("%s unreadable (%s)", context, conditionMessage(obj))))
  }
  d <- dim(obj)
  ok <- is.numeric(obj) && !is.null(d) && length(d) == 3L && all(is.finite(d)) && all(d > 0)
  if (!ok) {
    return(list(ok = FALSE, message = sprintf("%s invalid shape; expected numeric 3D array.", context)))
  }
  list(ok = TRUE, message = "")
}

unified_validate_matrix_rds_file <- function(path, context) {
  if (!file.exists(path)) {
    return(list(ok = FALSE, message = sprintf("%s missing: %s", context, path)))
  }
  obj <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(obj, "error")) {
    return(list(ok = FALSE, message = sprintf("%s unreadable (%s)", context, conditionMessage(obj))))
  }
  d <- dim(obj)
  ok <- is.numeric(obj) && !is.null(d) && length(d) == 2L && all(is.finite(d)) && all(d > 0)
  if (!ok) {
    return(list(ok = FALSE, message = sprintf("%s invalid shape; expected numeric 2D matrix.", context)))
  }
  list(ok = TRUE, message = "")
}

unified_post_contract_check <- function(
  artifacts_df,
  outputs_dir,
  cache_dir = NULL,
  post_figures = TRUE,
  export_tables = TRUE,
  post_smoke_fast = FALSE,
  multivar_component_diagnostics = FALSE,
  multivar_component_fail_fast = TRUE,
  authoritative_selected_model_support = FALSE,
  authoritative_selected_model_support_fail_fast = TRUE,
  multivar_component_transfer_mode = NA_character_,
  model_run_exdqlm_multivar = TRUE,
  model_run_exdqlm_univar = TRUE,
  model_run_ndlm_main = TRUE,
  model_run_ndlm_univar = FALSE
) {
  if (is.null(artifacts_df)) {
    artifacts_df <- unified_collect_post_artifacts(outputs_dir = outputs_dir, cache_dir = cache_dir)
  }

  checks <- list()
  messages <- character(0)
  missing_paths <- character(0)

  outputs_df <- artifacts_df[artifacts_df$scope == "outputs", , drop = FALSE]
  output_basenames <- if (nrow(outputs_df) > 0L) basename(outputs_df$relative_path) else character(0)
  has_output_file <- function(name) {
    if (!nchar(name)) return(FALSE)
    any(output_basenames == name)
  }
  has_any_output_file <- function(names_vec) {
    if (length(names_vec) == 0L) return(FALSE)
    any(vapply(as.character(names_vec), has_output_file, logical(1)))
  }
  requested_component_transfer_mode <- {
    raw <- if (is.null(multivar_component_transfer_mode)) "" else as.character(multivar_component_transfer_mode)
    raw <- tolower(trimws(raw[[1L]]))
    if (!nzchar(raw) || is.na(raw)) "" else raw
  }
  component_required_csv <- c(
    "multivar_trace_summary_q50.csv",
    "multivar_forecast_window_q50_summary.csv",
    "multivar_forecast_window_q50_metrics.csv",
    "multivar_transfer_state_window_q50.csv",
    "multivar_transfer_state_contract_q50.csv",
    "multivar_transfer_identity_check_q50.csv",
    "multivar_transfer_contract_q50.csv",
    "multivar_vb_usgs_location_quantiles_cutoff_window.csv",
    "multivar_vb_usgs_location_quantile_summary.csv"
  )
  component_required_figures <- c(
    "multivar_elbo_trace_q50.png",
    "multivar_sigma_traces_q50.png",
    "multivar_gamma_traces_q50.png",
    "multivar_transfer_zeta_window_q50.png",
    "multivar_transfer_observation_decomposition_q50.png",
    "multivar_transfer_source_mu_window_q50.png",
    "multivar_transfer_discrepancy_identity_q50.png",
    "multivar_vb_usgs_location_quantiles_cutoff_window.png"
  )
  if (!identical(requested_component_transfer_mode, "drop")) {
    component_required_csv <- c(component_required_csv, "multivar_transfer_coefficients_window_q50.csv")
    component_required_figures <- c(component_required_figures, "multivar_transfer_coefficients_window_q50.png")
  }
  authoritative_support_required <- c(
    "authoritative_usgs_quantile_dynamics_summary.csv",
    "authoritative_usgs_quantile_dynamics_summary.rds",
    "authoritative_component_summary.csv",
    "authoritative_component_summary.rds",
    "authoritative_selected_support_lineage.csv",
    "authoritative_selected_support_manifest.json"
  )
  check_authoritative_selected_support_contract <- function() {
    missing_support <- authoritative_support_required[
      !vapply(authoritative_support_required, has_output_file, logical(1))
    ]
    checks$authoritative_selected_model_support_present <<- length(missing_support) == 0L
    if (!checks$authoritative_selected_model_support_present) {
      missing_paths <<- c(missing_paths, file.path(outputs_dir, missing_support))
      messages <<- c(messages, sprintf(
        "missing authoritative selected-model support artifacts: %s",
        paste(missing_support, collapse = ", ")
      ))
    }
    invisible(checks$authoritative_selected_model_support_present)
  }
  check_multivar_component_contract <- function() {
    missing_component <- c(
      component_required_csv[!vapply(component_required_csv, has_output_file, logical(1))],
      component_required_figures[!vapply(component_required_figures, has_output_file, logical(1))]
    )
    checks$multivar_component_diagnostics_present <<- length(missing_component) == 0L
    if (!checks$multivar_component_diagnostics_present) {
      missing_paths <<- c(missing_paths, file.path(outputs_dir, missing_component))
      messages <<- c(messages, sprintf(
        "missing multivariate component diagnostics: %s",
        paste(missing_component, collapse = ", ")
      ))
    }

    contract_path <- file.path(outputs_dir, "multivar_transfer_contract_q50.csv")
    contract_df <- tryCatch(
      if (file.exists(contract_path)) utils::read.csv(contract_path, stringsAsFactors = FALSE, check.names = FALSE) else NULL,
      error = function(e) e
    )
    checks$multivar_component_transfer_contract_ok <<- FALSE
    if (inherits(contract_df, "error") || is.null(contract_df) || !is.data.frame(contract_df) || nrow(contract_df) < 1L) {
      messages <<- c(messages, "multivar_transfer_contract_q50.csv is missing or unreadable.")
      if (!file.exists(contract_path)) missing_paths <<- c(missing_paths, contract_path)
      return(invisible(FALSE))
    }

    row <- contract_df[1L, , drop = FALSE]
    num_field <- function(name) {
      if (!name %in% names(row)) return(NA_real_)
      suppressWarnings(as.numeric(row[[name]][[1L]]))
    }
    chr_field <- function(name) {
      if (!name %in% names(row)) return("")
      as.character(row[[name]][[1L]])
    }
    bool_field <- function(name) {
      val <- tolower(trimws(chr_field(name)))
      val %in% c("true", "t", "1", "yes")
    }

    transfer_mode_raw <- if (is.null(multivar_component_transfer_mode)) "" else multivar_component_transfer_mode
    transfer_mode <- tolower(trimws(as.character(transfer_mode_raw)))
    if (!nzchar(transfer_mode) || is.na(transfer_mode)) {
      transfer_mode <- tolower(trimws(chr_field("transfer_mode")))
    }
    forecast_has_transfer <- bool_field("forecast_has_transfer")
    n_forecast_rows <- num_field("n_forecast_rows")
    finite_zeta <- num_field("finite_zeta_forecast")
    finite_mu_without_transfer <- num_field("finite_mu_without_transfer_forecast")
    max_decomp <- num_field("max_abs_mu_decomp_error")
    max_g <- num_field("max_abs_identity_err_glofas")
    max_n <- num_field("max_abs_identity_err_nws")
    tol_decomp <- num_field("tol_decomp")
    tol_identity <- num_field("tol_identity")
    if (!is.finite(tol_decomp)) tol_decomp <- 1e-8
    if (!is.finite(tol_identity)) tol_identity <- 1e-8

    violations <- character(0)
    if (is.finite(max_decomp) && max_decomp > tol_decomp) {
      violations <- c(violations, sprintf("mu decomposition error %.6e exceeds tolerance %.6e", max_decomp, tol_decomp))
    }
    if (is.finite(max_g) && max_g > tol_identity) {
      violations <- c(violations, sprintf("GLOFAS identity error %.6e exceeds tolerance %.6e", max_g, tol_identity))
    }
    if (is.finite(max_n) && max_n > tol_identity) {
      violations <- c(violations, sprintf("NWS identity error %.6e exceeds tolerance %.6e", max_n, tol_identity))
    }
    if (identical(transfer_mode, "keep")) {
      if (!isTRUE(forecast_has_transfer)) {
        violations <- c(violations, "keep mode expected forecast_has_transfer=true")
      }
      if (!is.finite(n_forecast_rows) || n_forecast_rows <= 0) {
        violations <- c(violations, "keep mode expected positive n_forecast_rows")
      }
      if (!is.finite(finite_zeta) || finite_zeta <= 0) {
        violations <- c(violations, "keep mode expected finite forecast zeta values")
      }
      if (!is.finite(finite_mu_without_transfer) || finite_mu_without_transfer <= 0) {
        violations <- c(violations, "keep mode expected finite mu_without_transfer values")
      }
    }

    checks$multivar_component_transfer_contract_ok <<- length(violations) == 0L
    if (!checks$multivar_component_transfer_contract_ok) {
      messages <<- c(messages, sprintf(
        "multivariate component transfer contract failed: %s",
        paste(violations, collapse = "; ")
      ))
    }
    invisible(checks$multivar_component_transfer_contract_ok)
  }
  checks$outputs_nonempty <- nrow(outputs_df) > 0L
  if (!checks$outputs_nonempty) {
    messages <- c(messages, "post outputs directory has no files.")
  }

  ndlm_any_mode <- isTRUE(model_run_ndlm_main) || isTRUE(model_run_ndlm_univar)
  ndlm_only_mode <- isTRUE(ndlm_any_mode) &&
    !isTRUE(model_run_exdqlm_multivar) &&
    !isTRUE(model_run_exdqlm_univar)
  univar_only_mode <- isTRUE(model_run_exdqlm_univar) &&
    !isTRUE(model_run_exdqlm_multivar) &&
    !isTRUE(ndlm_any_mode)
  multivar_only_mode <- isTRUE(model_run_exdqlm_multivar) &&
    !isTRUE(model_run_exdqlm_univar) &&
    !isTRUE(ndlm_any_mode)

  if (!isTRUE(post_figures)) {
    marker_path <- file.path(outputs_dir, "post_smoke_marker.txt")
    checks$smoke_marker_exists <- file.exists(marker_path)
    if (!checks$smoke_marker_exists) {
      missing_paths <- c(missing_paths, marker_path)
      messages <- c(messages, "smoke marker missing for non-figures post mode.")
    }
  } else {
    checks$has_figure <- any(outputs_df$artifact_type == "figure")
    if (!checks$has_figure) {
      messages <- c(messages, "no figure artifacts found under post outputs.")
    }

    if (isTRUE(post_smoke_fast)) {
      # Smoke-fast figure runs are intentionally minimal and do not emit full
      # synthesis cache cubes or table-export artifacts.
      checks$synthesis_cache_files_present <- TRUE
      checks$synthesis_core_shapes_ok <- TRUE
      checks$table_exports_present <- TRUE
    } else if (univar_only_mode) {
      required_univar_cache <- c(
        "y_hist_uni.rds",
        "y_forecast_uni.rds",
        "synth_univar_hist_log1p.rds",
        "synth_univar_forecast_log1p.rds"
      )
      univar_cache_paths <- file.path(cache_dir, required_univar_cache)
      missing_univar_cache <- univar_cache_paths[!file.exists(univar_cache_paths)]
      checks$synthesis_cache_files_present <- length(missing_univar_cache) == 0L
      if (!checks$synthesis_cache_files_present) {
        missing_paths <- c(missing_paths, missing_univar_cache)
        messages <- c(messages, sprintf("missing univariate-only cache files: %s", paste(basename(missing_univar_cache), collapse = ", ")))
      }

      univar_shape_checks <- list(
        unified_validate_synthesis_cube_file(file.path(cache_dir, "y_hist_uni.rds"), "y_hist_uni.rds"),
        unified_validate_synthesis_cube_file(file.path(cache_dir, "y_forecast_uni.rds"), "y_forecast_uni.rds"),
        unified_validate_matrix_rds_file(file.path(cache_dir, "synth_univar_hist_log1p.rds"), "synth_univar_hist_log1p.rds"),
        unified_validate_matrix_rds_file(file.path(cache_dir, "synth_univar_forecast_log1p.rds"), "synth_univar_forecast_log1p.rds")
      )
      checks$synthesis_core_shapes_ok <- all(vapply(univar_shape_checks, `[[`, logical(1), "ok"))
      if (!isTRUE(checks$synthesis_core_shapes_ok)) {
        bad_msgs <- vapply(univar_shape_checks[!vapply(univar_shape_checks, `[[`, logical(1), "ok")], `[[`, character(1), "message")
        messages <- c(messages, bad_msgs)
      }

      legacy_univar_fit_figures_present <- has_any_output_file(c(
        "univar_fit_mu_vs_observed_log1p.png",
        "univar_fit_mu_vs_observed_recent_log1p.png",
        "univar_fit_mu_vs_observed_loglog.png",
        "univar_fit_mu_vs_observed_recent_loglog.png"
      ))

      checks$univar_forecast_figure_present <- has_any_output_file(c(
        "univar_forecast_window_mu_vs_future_usgs.png",
        "univar_forecast_window_predictive_q50_vs_future_usgs.png",
        "univar_forecast_window_univar_vs_ensembles.png",
        "univar_forecast_window_ensemble_members.png",
        "univar_forecast_window_quantiles_raw_cms.png"
      ))
      if (!checks$univar_forecast_figure_present) {
        messages <- c(messages, "missing univariate forecast-window figure outputs.")
      }

      legacy_univar_trace_figures_present <- has_any_output_file(c(
        "univar_elbo_traces.png",
        "univar_gamma_traces.png",
        "univar_sigma_traces.png",
        "All_ELBOS_DISC.png"
      ))

      required_univar_outputs <- c(
        "univar_forecast_window_quantiles.csv",
        "univar_forecast_quantile_crossing_per_time.csv",
        "univar_forecast_quantile_crossing_summary.csv"
      )
      missing_univar_outputs <- required_univar_outputs[!vapply(required_univar_outputs, has_output_file, logical(1))]
      checks$univar_summary_exports_present <- length(missing_univar_outputs) == 0L
      if (!checks$univar_summary_exports_present) {
        missing_paths <- c(missing_paths, file.path(outputs_dir, missing_univar_outputs))
        messages <- c(messages, sprintf("missing univariate summary exports: %s", paste(basename(missing_univar_outputs), collapse = ", ")))
      }

      # The isolated univariate repair module is intentionally forecast-window
      # centric and does not emit the legacy fit/trace figures. Accept either
      # the legacy figure set or the new dedicated forecast-window diagnostics.
      checks$univar_fit_figure_present <- legacy_univar_fit_figures_present || isTRUE(checks$univar_forecast_figure_present)
      if (!checks$univar_fit_figure_present) {
        messages <- c(messages, "missing univariate fit or dedicated forecast-window figure outputs.")
      }

      checks$univar_trace_figure_present <- legacy_univar_trace_figures_present || isTRUE(checks$univar_summary_exports_present)
      if (!checks$univar_trace_figure_present) {
        messages <- c(messages, "missing univariate trace or dedicated summary exports.")
      }

      if (isTRUE(export_tables)) {
        required_univar_tables <- c(
          "crps_forecast_summary.csv",
          "crps_forecast_per_time.csv",
          "crps_input_health.csv",
          "crps_input_health_per_time.csv",
          "posterior_table_exports_manifest.csv",
          "posterior_table_exports_README.md"
        )
        missing_univar_tables <- required_univar_tables[!vapply(required_univar_tables, has_output_file, logical(1))]
        checks$table_exports_present <- length(missing_univar_tables) == 0L
        if (!checks$table_exports_present) {
          missing_paths <- c(missing_paths, file.path(outputs_dir, "tables", missing_univar_tables))
          messages <- c(messages, sprintf("missing univariate-only table exports: %s", paste(basename(missing_univar_tables), collapse = ", ")))
        }
      } else {
        checks$table_exports_present <- TRUE
      }
    } else if (multivar_only_mode) {
      # Multivariate-only post runs use the dedicated 40_figures_multivar_only
      # module. They intentionally skip full synthesis-cache cubes and
      # cross-family table exports, but must still produce core diagnostics.
      checks$synthesis_cache_files_present <- TRUE
      checks$synthesis_core_shapes_ok <- TRUE

      checks$multivar_fit_figure_present <- has_any_output_file(c(
        "multivar_fit_mu_vs_observed_loglog.png",
        "multivar_fit_mu_vs_observed_recent_loglog.png"
      ))
      if (!checks$multivar_fit_figure_present) {
        messages <- c(messages, "missing multivariate fit figure outputs.")
      }

      checks$multivar_forecast_figure_present <- has_any_output_file(c(
        "multivar_forecast_window_mu_vs_future_usgs.png",
        "multivar_forecast_window_multivar_vs_ensembles.png",
        "multivar_forecast_window_ensemble_members.png"
      ))
      if (!checks$multivar_forecast_figure_present) {
        messages <- c(messages, "missing multivariate forecast-window figure outputs.")
      }

      checks$multivar_trace_figure_present <- has_output_file("multivar_elbo_trace_q50.png")
      if (!checks$multivar_trace_figure_present) {
        messages <- c(messages, "missing multivariate ELBO trace figure output.")
      }

      required_multivar_csv <- c(
        "multivar_trace_summary_q50.csv",
        "multivar_forecast_window_q50_summary.csv",
        "multivar_forecast_window_q50_metrics.csv"
      )
      missing_multivar_csv <- required_multivar_csv[!vapply(required_multivar_csv, has_output_file, logical(1))]
      checks$multivar_summary_csv_present <- length(missing_multivar_csv) == 0L
      if (!checks$multivar_summary_csv_present) {
        missing_paths <- c(missing_paths, file.path(outputs_dir, missing_multivar_csv))
        messages <- c(messages, sprintf("missing multivariate summary exports: %s", paste(basename(missing_multivar_csv), collapse = ", ")))
      }

      # For multivar-only profiles, treat required multivar CSV diagnostics as
      # the table contract when export_tables is enabled.
      checks$table_exports_present <- TRUE
      if (isTRUE(export_tables)) {
        checks$table_exports_present <- isTRUE(checks$multivar_summary_csv_present)
      }
    } else if (ndlm_only_mode) {
      required_ndlm_cache <- c("xbs_ndlm_log1p.rds", "y_reps_ndlm_log1p.rds")
      ndlm_cache_paths <- file.path(cache_dir, required_ndlm_cache)
      missing_ndlm_cache <- ndlm_cache_paths[!file.exists(ndlm_cache_paths)]
      checks$synthesis_cache_files_present <- length(missing_ndlm_cache) == 0L
      if (!checks$synthesis_cache_files_present) {
        missing_paths <- c(missing_paths, missing_ndlm_cache)
        messages <- c(messages, sprintf("missing NDLM cache files: %s", paste(basename(missing_ndlm_cache), collapse = ", ")))
      }

      ndlm_shape_checks <- list(
        unified_validate_matrix_rds_file(file.path(cache_dir, "xbs_ndlm_log1p.rds"), "xbs_ndlm_log1p.rds"),
        unified_validate_matrix_rds_file(file.path(cache_dir, "y_reps_ndlm_log1p.rds"), "y_reps_ndlm_log1p.rds")
      )
      checks$synthesis_core_shapes_ok <- all(vapply(ndlm_shape_checks, `[[`, logical(1), "ok"))
      if (!isTRUE(checks$synthesis_core_shapes_ok)) {
        bad_msgs <- vapply(ndlm_shape_checks[!vapply(ndlm_shape_checks, `[[`, logical(1), "ok")], `[[`, character(1), "message")
        messages <- c(messages, bad_msgs)
      }

      checks$ndlm_forecast_figure_present <- has_any_output_file(c(
        "ndlm_forecast_window_quantiles_raw_cms.png",
        "ndlm_fit_recent_log1p.png",
        "All_ELBOS_DISC.png"
      ))
      if (!checks$ndlm_forecast_figure_present) {
        messages <- c(messages, "missing NDLM-only forecast/fit figure outputs.")
      }

      required_ndlm_tables <- c(
        "crps_forecast_summary.csv",
        "crps_forecast_per_time.csv",
        "crps_input_health.csv",
        "crps_input_health_per_time.csv",
        "ndlm_forecast_window_quantiles.csv",
        "posterior_table_exports_manifest.csv",
        "posterior_table_exports_README.md"
      )
      missing_ndlm_tables <- required_ndlm_tables[!vapply(required_ndlm_tables, has_output_file, logical(1))]
      checks$table_exports_present <- length(missing_ndlm_tables) == 0L
      if (!checks$table_exports_present) {
        missing_paths <- c(missing_paths, file.path(outputs_dir, "tables", missing_ndlm_tables))
        messages <- c(messages, sprintf("missing NDLM-only table exports: %s", paste(basename(missing_ndlm_tables), collapse = ", ")))
      }
    } else {
      required_cache <- c("y_reps_f.rds", "y_reps.rds", "y_reps_f_new.rds", "y_reps_new.rds")
      cache_paths <- file.path(cache_dir, required_cache)
      missing_cache <- cache_paths[!file.exists(cache_paths)]
      checks$synthesis_cache_files_present <- length(missing_cache) == 0L
      if (!checks$synthesis_cache_files_present) {
        missing_paths <- c(missing_paths, missing_cache)
        messages <- c(messages, sprintf("missing synthesis cache files: %s", paste(basename(missing_cache), collapse = ", ")))
      }

      core_shape_checks <- list(
        unified_validate_synthesis_cube_file(file.path(cache_dir, "y_reps_f.rds"), "y_reps_f.rds"),
        unified_validate_synthesis_cube_file(file.path(cache_dir, "y_reps.rds"), "y_reps.rds")
      )
      core_shape_ok <- all(vapply(core_shape_checks, `[[`, logical(1), "ok"))
      checks$synthesis_core_shapes_ok <- core_shape_ok
      if (!core_shape_ok) {
        bad_msgs <- vapply(core_shape_checks[!vapply(core_shape_checks, `[[`, logical(1), "ok")], `[[`, character(1), "message")
        messages <- c(messages, bad_msgs)
      }

      if (isTRUE(export_tables)) {
        required_tables <- c(
          "gamma_summary.csv",
          "sigma_summary.csv",
          "covariate_effects_summary.csv",
          "posterior_table_exports_manifest.csv",
          "posterior_table_exports_README.md"
        )
        missing_tables <- required_tables[!vapply(required_tables, has_output_file, logical(1))]
        checks$table_exports_present <- length(missing_tables) == 0L
        if (!checks$table_exports_present) {
          missing_paths <- c(missing_paths, file.path(outputs_dir, "tables", missing_tables))
          messages <- c(messages, sprintf("missing table exports: %s", paste(basename(missing_tables), collapse = ", ")))
        }
      } else {
        checks$table_exports_present <- TRUE
      }
    }
  }

  if (isTRUE(multivar_component_diagnostics)) {
    if (!isTRUE(model_run_exdqlm_multivar)) {
      checks$multivar_component_diagnostics_present <- FALSE
      messages <- c(messages, "multivar component diagnostics were requested but exdqlm multivar is disabled.")
    } else {
      check_multivar_component_contract()
    }
  }
  if (isTRUE(authoritative_selected_model_support)) {
    if (!isTRUE(model_run_exdqlm_multivar)) {
      checks$authoritative_selected_model_support_present <- FALSE
      messages <- c(messages, "authoritative selected-model support was requested but exdqlm multivar is disabled.")
    } else {
      check_authoritative_selected_support_contract()
    }
  }

  status_checks <- checks
  if (isTRUE(multivar_component_diagnostics) && !isTRUE(multivar_component_fail_fast)) {
    component_check_names <- grep("^multivar_component_", names(status_checks), value = TRUE)
    if (length(component_check_names) > 0L) {
      component_values <- unlist(status_checks[component_check_names], use.names = TRUE)
      if (any(!component_values)) {
        messages <- c(
          messages,
          "multivar component diagnostics failed but multivar_component_fail_fast=false; not failing post contract."
        )
      }
      status_checks[component_check_names] <- as.list(rep(TRUE, length(component_check_names)))
    }
  }
  if (isTRUE(authoritative_selected_model_support) && !isTRUE(authoritative_selected_model_support_fail_fast)) {
    support_check_names <- grep("^authoritative_selected_model_support_", names(status_checks), value = TRUE)
    if (length(support_check_names) > 0L) {
      support_values <- unlist(status_checks[support_check_names], use.names = TRUE)
      if (any(!support_values)) {
        messages <- c(
          messages,
          "authoritative selected-model support failed but authoritative_selected_model_support_fail_fast=false; not failing post contract."
        )
      }
      status_checks[support_check_names] <- as.list(rep(TRUE, length(support_check_names)))
    }
  }

  checks_vec <- unlist(status_checks, use.names = TRUE)
  status <- length(checks_vec) > 0L && all(checks_vec)
  list(
    status = isTRUE(status),
    checks = checks,
    messages = unique(messages),
    missing_paths = unique(normalizePath(missing_paths, mustWork = FALSE))
  )
}

unified_write_post_artifact_reports <- function(
  artifacts_df,
  outputs_dir,
  run_id = "",
  cache_dir = NULL,
  contract = NULL,
  manifest_path = NULL,
  summary_path = NULL
) {
  if (is.null(manifest_path) || !nzchar(manifest_path)) {
    manifest_path <- file.path(outputs_dir, "post_artifacts_manifest.csv")
  }
  if (is.null(summary_path) || !nzchar(summary_path)) {
    summary_path <- file.path(outputs_dir, "post_artifacts_summary.json")
  }

  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(artifacts_df)) {
    artifacts_df <- unified_collect_post_artifacts(outputs_dir = outputs_dir, cache_dir = cache_dir)
  }

  manifest_df <- artifacts_df[, c("scope", "relative_path", "artifact_type", "extension", "bytes", "modified_at_utc"), drop = FALSE]
  utils::write.csv(manifest_df, file = manifest_path, row.names = FALSE)

  counts <- if (nrow(manifest_df) == 0L) {
    data.frame(scope = character(0), artifact_type = character(0), count = integer(0), stringsAsFactors = FALSE)
  } else {
    as.data.frame(table(manifest_df$scope, manifest_df$artifact_type), stringsAsFactors = FALSE)
  }
  names(counts) <- c("scope", "artifact_type", "count")

  summary <- list(
    run_id = as.character(run_id),
    generated_at_utc = unified_iso_utc(),
    outputs_dir = normalizePath(outputs_dir, mustWork = FALSE),
    cache_dir = normalizePath(cache_dir, mustWork = FALSE),
    total_artifact_files = nrow(manifest_df),
    counts = counts,
    contract = contract
  )

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(summary, path = summary_path, auto_unbox = TRUE, pretty = TRUE)
  } else {
    lines <- c(
      "{",
      sprintf("  \"run_id\": \"%s\",", as.character(run_id)),
      sprintf("  \"generated_at_utc\": \"%s\",", unified_iso_utc()),
      sprintf("  \"total_artifact_files\": %d", as.integer(nrow(manifest_df))),
      "}"
    )
    writeLines(lines, con = summary_path, useBytes = TRUE)
  }

  list(
    manifest_path = manifest_path,
    summary_path = summary_path,
    manifest_df = manifest_df
  )
}
