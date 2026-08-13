# unified/stages/stage_post.R

unified_stage_post <- function(cfg, run_root, repo_root, manifest) {
  run_root_abs <- normalizePath(run_root, mustWork = FALSE)
  repo_root_abs <- normalizePath(repo_root, mustWork = FALSE)
  ndlm_diag_helpers <- file.path(repo_root_abs, "R", "unified", "ndlm_post_diagnostics.R")
  if (!file.exists(ndlm_diag_helpers)) {
    stop(sprintf("Missing NDLM post diagnostics helpers: %s", ndlm_diag_helpers), call. = FALSE)
  }
  source(ndlm_diag_helpers, local = environment())
  post_table_helpers <- file.path(repo_root_abs, "R", "environmetrics", "02_helpers_core.R")
  if (!file.exists(post_table_helpers)) {
    stop(sprintf("Missing post table helpers: %s", post_table_helpers), call. = FALSE)
  }
  source(post_table_helpers, local = environment())
  run_id <- cfg$run$run_id
  repro_mode <- cfg$run$repro_mode
  if (is.null(repro_mode) || !nzchar(repro_mode)) repro_mode <- "strict"
  repro_mode <- as.character(repro_mode)
  strict_repro <- identical(tolower(repro_mode), "strict")
  post_use_fit_outputs_from_run <- isTRUE(unified_get(cfg, c("inputs", "post", "use_fit_outputs_from_run"), default = TRUE))
  post_source_run_id <- unified_get(cfg, c("inputs", "post", "source_run_id"), default = NULL)
  if (!is.null(post_source_run_id) && !is.character(post_source_run_id)) {
    post_source_run_id <- as.character(post_source_run_id)
  }
  post_source_run_root <- unified_get(cfg, c("inputs", "post", "source_run_root"), default = NULL)
  if (is.null(post_source_run_root) || !nzchar(post_source_run_root)) {
    post_source_run_root <- cfg$run$run_root
  }
  fit_outputs_root_abs <- run_root_abs
  if (post_use_fit_outputs_from_run &&
      !is.null(post_source_run_id) &&
      nzchar(post_source_run_id) &&
      !identical(post_source_run_id, run_id)) {
    fit_outputs_root_abs <- unified_resolve_source_run_dir(
      source_run_root = post_source_run_root,
      source_run_id = post_source_run_id,
      fallback_run_root = cfg$run$run_root
    )
    if (!dir.exists(fit_outputs_root_abs)) {
      stop(sprintf(
        "inputs.post.source_run_id requested but source run root is missing: %s",
        fit_outputs_root_abs
      ), call. = FALSE)
    }
  }
  post_root <- file.path(run_root, "post")
  post_inputs <- file.path(post_root, "inputs")
  post_logs <- file.path(post_root, "logs")
  post_cache_dir <- file.path(run_root_abs, "post", "cache")
  dir.create(post_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(post_inputs, recursive = TRUE, showWarnings = FALSE)
  dir.create(post_logs, recursive = TRUE, showWarnings = FALSE)
  dir.create(post_cache_dir, recursive = TRUE, showWarnings = FALSE)

  shared_input_run_root <- run_root
  run_scoped_shared_exists <- dir.exists(file.path(run_root_abs, "inputs", "shared"))
  source_scoped_shared_exists <- dir.exists(file.path(fit_outputs_root_abs, "inputs", "shared"))
  if (!run_scoped_shared_exists && source_scoped_shared_exists) {
    shared_input_run_root <- fit_outputs_root_abs
  }
  shared_paths <- unified_shared_input_paths(shared_input_run_root)
  use_shared_inputs <- isTRUE(cfg$stages$data_prep_shared) || dir.exists(shared_paths$root)
  if (use_shared_inputs) {
    shared_validation <- unified_validate_required_shared_inputs(
      run_root = shared_input_run_root,
      stage_name = "post",
      manifest = manifest,
      enabled_models = cfg$models,
      required_usgs = TRUE
    )
    source_retros <- shared_validation$paths$retros
    source_nws <- shared_validation$paths$nws
    source_glofas <- shared_validation$paths$glofas
    source_usgs <- shared_validation$paths$usgs
    source_retros_scale <- shared_validation$scales$retros
    if (is.null(source_retros_scale) || !nzchar(source_retros_scale)) {
      source_retros_scale <- cfg$inputs$fit$retros_storage_scale
    }
    source_nws_scale <- shared_validation$scales$nws
    if (is.null(source_nws_scale) || !nzchar(source_nws_scale)) {
      source_nws_scale <- cfg$inputs$fit$nws_storage_scale
    }
    source_glofas_scale <- shared_validation$scales$glofas
    if (is.null(source_glofas_scale) || !nzchar(source_glofas_scale)) {
      source_glofas_scale <- cfg$inputs$fit$glofas_storage_scale
    }
  } else {
    source_retros <- cfg$inputs$fit$retros_path
    source_nws <- cfg$inputs$fit$nws_forecast_path
    source_glofas <- cfg$inputs$fit$glofas_forecast_path
    snapshot_dest_rel <- unified_get(cfg, c("inputs", "forecats", "snapshot", "dest_rel"), default = "inputs/shared/forecats_bundle")
    if (is.null(snapshot_dest_rel) || !nzchar(as.character(snapshot_dest_rel))) {
      snapshot_dest_rel <- "inputs/shared/forecats_bundle"
    }
    source_usgs <- unified_resolve_usgs_daily_path(
      cfg,
      snapshot_root = file.path(shared_input_run_root, as.character(snapshot_dest_rel))
    )$path
    source_retros_scale <- cfg$inputs$fit$retros_storage_scale
    source_nws_scale <- cfg$inputs$fit$nws_storage_scale
    source_glofas_scale <- cfg$inputs$fit$glofas_storage_scale
  }
  post_usgs_max_date <- function(path) {
    path <- as.character(path %||% "")
    if (!nzchar(path) || !file.exists(path)) {
      return(as.Date(NA))
    }
    df <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.data.frame(df) || nrow(df) < 1L) {
      return(as.Date(NA))
    }
    date_name <- intersect(c("date", "Date", "timestamp", "time", "target_date"), names(df))
    if (length(date_name) < 1L) {
      return(as.Date(NA))
    }
    dates <- suppressWarnings(as.Date(df[[date_name[[1L]]]]))
    if (!any(!is.na(dates))) {
      return(as.Date(NA))
    }
    max(dates, na.rm = TRUE)
  }

  post_truth_usgs <- source_usgs
  post_truth_snapshot_dest_rel <- unified_get(
    cfg,
    c("inputs", "forecats", "snapshot", "dest_rel"),
    default = "inputs/shared/forecats_bundle"
  )
  if (is.null(post_truth_snapshot_dest_rel) || !nzchar(as.character(post_truth_snapshot_dest_rel))) {
    post_truth_snapshot_dest_rel <- "inputs/shared/forecats_bundle"
  }
  post_truth_resolution <- tryCatch(
    unified_resolve_usgs_daily_path(
      cfg,
      snapshot_root = file.path(shared_input_run_root, as.character(post_truth_snapshot_dest_rel))
    ),
    error = function(e) NULL
  )
  post_truth_candidate <- if (is.list(post_truth_resolution)) as.character(post_truth_resolution$path %||% "") else ""
  if (nzchar(post_truth_candidate) && file.exists(post_truth_candidate)) {
    source_usgs_max <- post_usgs_max_date(source_usgs)
    candidate_usgs_max <- post_usgs_max_date(post_truth_candidate)
    if (!is.na(candidate_usgs_max) && (is.na(source_usgs_max) || candidate_usgs_max > source_usgs_max)) {
      post_truth_usgs <- post_truth_candidate
    }
  }

  if (strict_repro && (!nzchar(post_truth_usgs) || !file.exists(post_truth_usgs))) {
    stop(
      paste(
        "post stage requires a run-scoped or cached USGS daily truth CSV in strict mode.",
        "Enable data_prep_shared materialization or provide inputs.fit.usgs_cache_path / inputs.forecats.existing_bundle_path."
      ),
      call. = FALSE
    )
  }

  fit_covariates <- cfg$inputs$fit$covariates
  if (is.null(fit_covariates)) fit_covariates <- list()
  shared_cov_paths <- list(
    eli = "",
    oni = "",
    ppt = "",
    soil = "",
    pca = ""
  )
  assign_cov_path <- function(cov_name, cov_path) {
    key <- tolower(as.character(cov_name))
    if (grepl("eli", key, fixed = TRUE)) shared_cov_paths$eli <<- cov_path
    if (grepl("oni", key, fixed = TRUE)) shared_cov_paths$oni <<- cov_path
    if (grepl("ppt", key, fixed = TRUE) || grepl("precip", key, fixed = TRUE)) shared_cov_paths$ppt <<- cov_path
    if (grepl("soil", key, fixed = TRUE)) shared_cov_paths$soil <<- cov_path
    if (grepl("pca", key, fixed = TRUE)) shared_cov_paths$pca <<- cov_path
  }

  if (use_shared_inputs) {
    sanitize_cov_tag <- function(x) {
      tag <- gsub("[^A-Za-z0-9]+", "_", as.character(x))
      tag <- gsub("^_+|_+$", "", tag)
      if (!nzchar(tag)) "cov" else tag
    }
    if (length(fit_covariates) > 0L) {
      for (i in seq_along(fit_covariates)) {
        entry <- fit_covariates[[i]]
        if (!is.list(entry)) next
        cov_name <- if (is.null(entry$name)) "" else as.character(entry$name)
        if (!nzchar(cov_name)) next
        cov_path <- file.path(shared_paths$covariates_dir, sprintf("cov_%02d_%s.csv", i, sanitize_cov_tag(cov_name)))
        if (!file.exists(cov_path)) next
        assign_cov_path(cov_name, cov_path)
      }
    }
  } else if (length(fit_covariates) > 0L) {
    for (entry in fit_covariates) {
      if (!is.list(entry)) next
      cov_name <- if (is.null(entry$name)) "" else as.character(entry$name)
      cov_path <- if (is.null(entry$path)) "" else as.character(entry$path)
      if (!nzchar(cov_name) || !nzchar(cov_path) || !file.exists(cov_path)) next
      assign_cov_path(cov_name, cov_path)
    }
  }

  covariate_feature_filename <- as.character(unified_get(
    cfg,
    c("inputs", "covariate_features", "output_filename"),
    default = "covariate_features.csv"
  )[[1L]])
  if (!nzchar(covariate_feature_filename)) {
    covariate_feature_filename <- "covariate_features.csv"
  }
  shared_feature_csv <- if (isTRUE(use_shared_inputs)) {
    cand <- file.path(shared_paths$covariates_dir, covariate_feature_filename)
    if (file.exists(cand)) normalizePath(cand, mustWork = FALSE) else ""
  } else {
    feature_env <- Sys.getenv("UNIFIED_COVARIATE_FEATURES_CSV", "")
    if (nzchar(feature_env) && file.exists(feature_env)) normalizePath(feature_env, mustWork = FALSE) else ""
  }

  legacy_scale <- cfg$scale_contract$legacy_post_input_scale
  unified_assert_known_scale(legacy_scale, "scale_contract.legacy_post_input_scale")

  adapted_retros <- file.path(post_inputs, "retros_post_adapter.csv")
  adapted_nws <- file.path(post_inputs, "nws_post_adapter.csv")
  adapted_glofas <- file.path(post_inputs, "glofas_post_adapter.csv")

  unified_adapt_csv_scale(
    input_path = source_retros,
    output_path = adapted_retros,
    from_scale = source_retros_scale,
    to_scale = legacy_scale,
    positive_required = FALSE
  )
  manifest <- unified_manifest_add_scale_history(
    manifest,
    artifact = "post_input/retros",
    from_scale = source_retros_scale,
    to_scale = legacy_scale,
    transform = sprintf("adapter_%s_to_%s", source_retros_scale, legacy_scale)
  )

  unified_adapt_csv_scale(
    input_path = source_nws,
    output_path = adapted_nws,
    from_scale = source_nws_scale,
    to_scale = legacy_scale,
    positive_required = FALSE
  )
  manifest <- unified_manifest_add_scale_history(
    manifest,
    artifact = "post_input/nws_forecast",
    from_scale = source_nws_scale,
    to_scale = legacy_scale,
    transform = sprintf("adapter_%s_to_%s", source_nws_scale, legacy_scale)
  )

  unified_adapt_csv_scale(
    input_path = source_glofas,
    output_path = adapted_glofas,
    from_scale = source_glofas_scale,
    to_scale = legacy_scale,
    positive_required = FALSE
  )
  manifest <- unified_manifest_add_scale_history(
    manifest,
    artifact = "post_input/glofas_forecast",
    from_scale = source_glofas_scale,
    to_scale = legacy_scale,
    transform = sprintf("adapter_%s_to_%s", source_glofas_scale, legacy_scale)
  )

  manifest <- unified_manifest_add_artifact(manifest, adapted_retros, storage_scale = legacy_scale)
  manifest <- unified_manifest_add_artifact(manifest, adapted_nws, storage_scale = legacy_scale)
  manifest <- unified_manifest_add_artifact(manifest, adapted_glofas, storage_scale = legacy_scale)

  legacy_fallback_requested <- isTRUE(cfg$post$allow_legacy_root_fallback)
  if (legacy_fallback_requested) {
    warning(
      "post.allow_legacy_root_fallback is deprecated and should remain false; this compatibility path will be removed in a future cutover.",
      call. = FALSE
    )
  }
  allow_legacy_root_fallback <- legacy_fallback_requested && !strict_repro
  quantiles <- as.numeric(cfg$fit$quantiles)
  q_num <- as.integer(round(quantiles * 100))
  q_labels <- sprintf("%02d", q_num)
  multivar_transfer_modes <- unified_resolve_multivar_transfer_modes(cfg)
  primary_multivar_transfer_mode <- unified_resolve_multivar_primary_transfer_mode(
    cfg,
    modes = multivar_transfer_modes
  )
  multivar_output_suffix <- unified_resolve_exdqlm_multivar_legacy_output_suffix(cfg, default = "DISC")
  univar_likelihood_mode <- unified_resolve_univar_likelihood_mode(cfg, default = "exal")
  multivar_likelihood_mode <- unified_resolve_multivar_likelihood_mode(cfg, default = "exal")
  ndlm_forecast_transfer_mode <- unified_resolve_ndlm_forecast_transfer_mode(cfg, default = "keep")
  ndlm_univar_forecast_transfer_mode <- unified_resolve_ndlm_univar_forecast_transfer_mode(cfg, default = "keep")
  multivar_dual_mode <- isTRUE(cfg$models$run_exdqlm_multivar) && length(multivar_transfer_modes) > 1L

  resolve_manifest_paths <- function(patterns, family_name, fallback_rel_paths = NULL) {
    allowed_roots <- unique(c(run_root_abs, fit_outputs_root_abs))
    is_under_allowed_roots <- function(path) {
      abs_path <- path.expand(path)
      any(vapply(allowed_roots, function(root) {
        startsWith(abs_path, paste0(root, .Platform$file.sep)) || identical(abs_path, root)
      }, logical(1)))
    }

    paths <- vapply(patterns, function(pattern) {
      unified_first_artifact_path(manifest, pattern = pattern, must_exist = FALSE)
    }, character(1))
    names(paths) <- names(patterns)

    if (!is.null(fallback_rel_paths)) {
      for (nm in names(paths)) {
        rel <- fallback_rel_paths[[nm]]
        rel <- if (is.null(rel)) "" else as.character(rel)
        if (!nzchar(rel)) next
        candidate <- file.path(fit_outputs_root_abs, rel)
        if (!file.exists(candidate)) next

        if (!nzchar(paths[[nm]])) {
          paths[[nm]] <- candidate
          next
        }

        if (strict_repro && !is_under_allowed_roots(paths[[nm]])) {
          paths[[nm]] <- candidate
        }
      }
    }

    missing <- names(paths)[!nzchar(paths)]
    if (length(missing) > 0L) {
      msg <- sprintf(
        "post stage missing run-scoped %s artifacts for keys: %s",
        family_name,
        paste(missing, collapse = ", ")
      )
      if (strict_repro) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    }
    existing <- paths[nzchar(paths)]
    if (length(existing) == 0L) {
      return(character(0))
    }
    unified_artifact_paths_to_absolute(existing, run_root = fit_outputs_root_abs, repo_root = repo_root_abs, must_exist = strict_repro)
  }

  disc_w_paths_by_mode <- list()
  if (isTRUE(cfg$models$run_exdqlm_multivar)) {
    resolve_disc_w_mode_paths <- function(mode) {
      mode <- tolower(trimws(as.character(mode)))
      use_legacy_primary <- identical(mode, "drop") &&
        identical(mode, primary_multivar_transfer_mode)
      if (use_legacy_primary) {
        patterns <- setNames(
          sprintf("fit/q=%s/outputs/DISC_variables_%d_exAL_synth_%s\\.RData$", q_labels, q_num, multivar_output_suffix),
          q_labels
        )
        fallback_rel <- setNames(
          sprintf("fit/q=%s/outputs/DISC_variables_%d_exAL_synth_%s.RData", q_labels, q_num, multivar_output_suffix),
          q_labels
        )
      } else {
        patterns <- setNames(
          sprintf(
            "fit/exdqlm_multivar/%s/q=%s/outputs/DISC_variables_%d_exAL_synth_%s\\.RData$",
            mode, q_labels, q_num, multivar_output_suffix
          ),
          q_labels
        )
        fallback_rel <- setNames(
          sprintf(
            "fit/exdqlm_multivar/%s/q=%s/outputs/DISC_variables_%d_exAL_synth_%s.RData",
            mode, q_labels, q_num, multivar_output_suffix
          ),
          q_labels
        )
      }
      resolve_manifest_paths(
        patterns = patterns,
        family_name = sprintf("DISC-W (%s)", mode),
        fallback_rel_paths = fallback_rel
      )
    }

    for (mode in multivar_transfer_modes) {
      disc_w_paths_by_mode[[mode]] <- resolve_disc_w_mode_paths(mode)
    }
  }
  post_primary_multivar_mode <- if (
    isTRUE(cfg$models$run_exdqlm_multivar) &&
    "drop" %in% names(disc_w_paths_by_mode) &&
    length(disc_w_paths_by_mode[["drop"]]) > 0L
  ) {
    "drop"
  } else {
    primary_multivar_transfer_mode
  }

  disc_w_paths_abs <- if (length(disc_w_paths_by_mode) > 0L) {
    disc_w_paths_by_mode[[post_primary_multivar_mode]]
  } else {
    character(0)
  }

  univ_paths_abs <- character(0)
  if (isTRUE(cfg$models$run_exdqlm_univar)) {
    patterns <- setNames(
      sprintf("fit/exdqlm_univar/q=%s/outputs/variables_%s_exAL_synth_DISC_uni\\.RData$", q_labels, q_labels),
      q_labels
    )
    fallback_rel <- setNames(
      sprintf("fit/exdqlm_univar/q=%s/outputs/variables_%s_exAL_synth_DISC_uni.RData", q_labels, q_labels),
      q_labels
    )
    univ_paths_abs <- resolve_manifest_paths(patterns, "univariate", fallback_rel_paths = fallback_rel)
  }

  ndlm_is_run_scoped <- function(path) {
    abs_path <- path.expand(path)
    startsWith(abs_path, paste0(run_root_abs, .Platform$file.sep)) ||
      identical(abs_path, run_root_abs) ||
      startsWith(abs_path, paste0(fit_outputs_root_abs, .Platform$file.sep)) ||
      identical(abs_path, fit_outputs_root_abs)
  }

  ndlm_main_path_abs <- ""
  if (isTRUE(cfg$models$run_ndlm_main)) {
    ndlm_rel <- unified_first_artifact_path(
      manifest,
      pattern = "fit/ndlm_main/outputs/DISC_variables_50_NDLM_synth_DISC\\.RData$",
      must_exist = FALSE
    )
    ndlm_fallback <- file.path(fit_outputs_root_abs, "fit", "ndlm_main", "outputs", "DISC_variables_50_NDLM_synth_DISC.RData")
    if (file.exists(ndlm_fallback)) {
      if (!nzchar(ndlm_rel) || (strict_repro && !ndlm_is_run_scoped(ndlm_rel))) {
        ndlm_rel <- ndlm_fallback
      }
    }
    if (!nzchar(ndlm_rel)) {
      msg <- "post stage missing run-scoped NDLM main artifact"
      if (strict_repro) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    } else {
      ndlm_main_path_abs <- unified_artifact_path_to_absolute(
        ndlm_rel,
        run_root = fit_outputs_root_abs,
        repo_root = repo_root_abs,
        must_exist = strict_repro
      )
    }
  }

  ndlm_univar_path_abs <- ""
  if (isTRUE(cfg$models$run_ndlm_univar)) {
    ndlm_univar_rel <- unified_first_artifact_path(
      manifest,
      pattern = "fit/ndlm_univar/outputs/DISC_variables_50_NDLM_univar_synth_DISC\\.RData$",
      must_exist = FALSE
    )
    ndlm_univar_fallback <- file.path(
      fit_outputs_root_abs, "fit", "ndlm_univar", "outputs", "DISC_variables_50_NDLM_univar_synth_DISC.RData"
    )
    if (file.exists(ndlm_univar_fallback)) {
      if (!nzchar(ndlm_univar_rel) || (strict_repro && !ndlm_is_run_scoped(ndlm_univar_rel))) {
        ndlm_univar_rel <- ndlm_univar_fallback
      }
    }
    if (!nzchar(ndlm_univar_rel)) {
      msg <- "post stage missing run-scoped NDLM univar artifact"
      if (strict_repro) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    } else {
      ndlm_univar_path_abs <- unified_artifact_path_to_absolute(
        ndlm_univar_rel,
        run_root = fit_outputs_root_abs,
        repo_root = repo_root_abs,
        must_exist = strict_repro
      )
    }
  }
  ndlm_path_abs <- if (nzchar(ndlm_main_path_abs)) ndlm_main_path_abs else ndlm_univar_path_abs

  encode_env_list <- function(x) {
    x <- as.character(x)
    if (length(x) == 0L) return("")
    paste(x, collapse = ",")
  }

  ndlm_any_mode <- isTRUE(cfg$models$run_ndlm_main) || isTRUE(cfg$models$run_ndlm_univar)
  univar_only_mode <- isTRUE(cfg$models$run_exdqlm_univar) &&
    !isTRUE(cfg$models$run_exdqlm_multivar) &&
    !isTRUE(ndlm_any_mode)
  multivar_only_mode <- isTRUE(cfg$models$run_exdqlm_multivar) &&
    !isTRUE(cfg$models$run_exdqlm_univar) &&
    !isTRUE(ndlm_any_mode)
  force_isolation_smoke_fast <- isTRUE(unified_get(
    cfg,
    c("post", "force_isolation_smoke_fast"),
    default = TRUE
  ))
  post_smoke_fast_effective <- isTRUE(cfg$post$smoke_fast) ||
    (isTRUE(force_isolation_smoke_fast) && (univar_only_mode || multivar_only_mode))
  if (univar_only_mode && !isTRUE(cfg$post$smoke_fast) && isTRUE(force_isolation_smoke_fast)) {
    warning(
      "stage_post: enabling smoke-fast post artifact contract for univariate-only mode to avoid cross-family artifact requirements.",
      call. = FALSE
    )
  }
  if (multivar_only_mode && !isTRUE(cfg$post$smoke_fast) && isTRUE(force_isolation_smoke_fast)) {
    warning(
      "stage_post: enabling smoke-fast post artifact contract for multivariate-only mode to avoid cross-family artifact requirements.",
      call. = FALSE
    )
  }

  sort_keep_na <- cfg$post$sort_keep_na
  if (is.null(sort_keep_na)) sort_keep_na <- TRUE
  export_tables <- cfg$post$export_tables
  if (is.null(export_tables)) export_tables <- TRUE
  multivar_component_diagnostics_enabled <- isTRUE(unified_get(
    cfg,
    c("post", "multivar_component_diagnostics", "enabled"),
    default = FALSE
  ))
  multivar_component_pre_days <- suppressWarnings(as.integer(unified_get(
    cfg,
    c("post", "multivar_component_diagnostics", "pre_days"),
    default = 30L
  )))
  if (!is.finite(multivar_component_pre_days) || multivar_component_pre_days < 0L) {
    multivar_component_pre_days <- 30L
  }
  multivar_component_quantile <- suppressWarnings(as.numeric(unified_get(
    cfg,
    c("post", "multivar_component_diagnostics", "quantile"),
    default = 0.50
  )))
  if (!is.finite(multivar_component_quantile)) {
    multivar_component_quantile <- 0.50
  }
  multivar_component_fail_fast <- isTRUE(unified_get(
    cfg,
    c("post", "multivar_component_diagnostics", "fail_fast"),
    default = TRUE
  ))
  authoritative_selected_support_enabled <- isTRUE(unified_get(
    cfg,
    c("post", "authoritative_selected_model_support", "enabled"),
    default = FALSE
  ))
  authoritative_selected_support_fail_fast <- isTRUE(unified_get(
    cfg,
    c("post", "authoritative_selected_model_support", "fail_fast"),
    default = TRUE
  ))
  table_formats <- cfg$post$table_formats
  if (is.null(table_formats) || length(table_formats) == 0L) {
    table_formats <- "csv"
  } else {
    table_formats <- tolower(as.character(table_formats))
    table_formats <- table_formats[nzchar(table_formats)]
    if (length(table_formats) == 0L) table_formats <- "csv"
  }

  cfg_date <- function(path, default) {
    raw <- as.character(unified_get(cfg, path, default = default))
    if (!length(raw) || is.na(raw[[1L]]) || !nzchar(raw[[1L]])) {
      raw <- default
    } else {
      raw <- raw[[1L]]
    }
    parsed <- suppressWarnings(as.Date(raw))
    if (is.na(parsed)) parsed <- as.Date(default)
    parsed
  }

  post_cutoff_date <- cfg_date(c("dates", "cutoff_date"), "2022-12-25")
  post_forecast_start_date <- post_cutoff_date + 1L
  post_plot_start_date <- cfg_date(c("dates", "plot_start"), as.character(post_cutoff_date - 18L))
  post_plot_end_date <- cfg_date(c("dates", "plot_end"), as.character(post_cutoff_date + 28L))
  ndlm_crps_primary_family <- if (isTRUE(cfg$models$run_ndlm_main)) {
    "ndlm_main"
  } else if (isTRUE(cfg$models$run_ndlm_univar)) {
    "ndlm_univar"
  } else {
    ""
  }
  exdqlm_univar_impl_mode <- as.character(unified_get(
    cfg,
    c("models", "exdqlm_univar", "implementation_mode"),
    default = "legacy_bridge"
  ))
  if (!length(exdqlm_univar_impl_mode) ||
      is.na(exdqlm_univar_impl_mode[[1L]]) ||
      !nzchar(exdqlm_univar_impl_mode[[1L]])) {
    exdqlm_univar_impl_mode <- "legacy_bridge"
  } else {
    exdqlm_univar_impl_mode <- exdqlm_univar_impl_mode[[1L]]
  }
  exdqlm_structure_include_trend <- if (isTRUE(unified_get(
    cfg, c("models", "exdqlm_multivar", "structure", "include_trend"), default = TRUE
  ))) "TRUE" else "FALSE"
  exdqlm_structure_harmonics <- as.character(unified_get(
    cfg, c("models", "exdqlm_multivar", "structure", "enabled_harmonic_indices"), default = c(1L, 2L, 3L)
  ))
  exdqlm_structure_harmonics <- paste(exdqlm_structure_harmonics, collapse = ",")
  exdqlm_transfer_feature_columns <- paste(
    unified_resolve_transfer_feature_columns(cfg),
    collapse = ","
  )
  out_dir <- file.path(run_root, "post", "outputs", run_id)

  base_env_overrides <- c(
    UNIFIED_RUN_ROOT = run_root_abs,
    UNIFIED_RUN_ID = run_id,
    UNIFIED_FIT_OUTPUTS_SOURCE_ROOT = fit_outputs_root_abs,
    UNIFIED_POST_CACHE_DIR = normalizePath(post_cache_dir, mustWork = FALSE),
    UNIFIED_LEGACY_FIT_INPUT_SCALE = as.character(cfg$scale_contract$legacy_fit_input_scale),
    UNIFIED_ANALYSIS_SCALE_FIT_INTERNAL = as.character(cfg$scale_contract$analysis_scale_fit_internal),
    UNIFIED_LEGACY_POST_INPUT_SCALE = as.character(cfg$scale_contract$legacy_post_input_scale),
    UNIFIED_ANALYSIS_SCALE_POST_INTERNAL = as.character(cfg$scale_contract$analysis_scale_post_internal),
    UNIFIED_TRANSFORM_POLICY = as.character(unified_get(
      cfg,
      c("scale_contract", "transform_policy"),
      default = ""
    )),
    UNIFIED_REPRO_MODE = repro_mode,
    UNIFIED_REQUIRE_RUNSCOPED_POST = if (strict_repro) "TRUE" else "FALSE",
    UNIFIED_ALLOW_LEGACY_POST_FALLBACK = if (allow_legacy_root_fallback) "TRUE" else "FALSE",
    UNIFIED_MODEL_RUN_EXDQLM_MULTIVAR = if (isTRUE(cfg$models$run_exdqlm_multivar)) "TRUE" else "FALSE",
    UNIFIED_MODEL_RUN_EXDQLM_UNIVAR = if (isTRUE(cfg$models$run_exdqlm_univar)) "TRUE" else "FALSE",
    UNIFIED_MODEL_RUN_NDLM_MAIN = if (isTRUE(cfg$models$run_ndlm_main)) "TRUE" else "FALSE",
    UNIFIED_MODEL_RUN_NDLM_UNIVAR = if (isTRUE(cfg$models$run_ndlm_univar)) "TRUE" else "FALSE",
    UNIFIED_EXDQLM_MULTIVAR_LIKELIHOOD_MODE = as.character(multivar_likelihood_mode),
    UNIFIED_EXDQLM_MULTIVAR_OUTPUT_SUFFIX = as.character(multivar_output_suffix),
    UNIFIED_EXDQLM_MULTIVAR_INCLUDE_TREND = exdqlm_structure_include_trend,
    UNIFIED_EXDQLM_MULTIVAR_ENABLED_HARMONIC_INDICES = exdqlm_structure_harmonics,
    UNIFIED_TRANSFER_FEATURE_COLUMNS = exdqlm_transfer_feature_columns,
    UNIFIED_EXDQLM_UNIVAR_LIKELIHOOD_MODE = as.character(univar_likelihood_mode),
    UNIFIED_EXDQLM_UNIVAR_IMPLEMENTATION_MODE = as.character(exdqlm_univar_impl_mode),
    UNIFIED_NDLM_FORECAST_TRANSFER_MODE = as.character(ndlm_forecast_transfer_mode),
    UNIFIED_NDLM_UNIVAR_FORECAST_TRANSFER_MODE = as.character(ndlm_univar_forecast_transfer_mode),
    UNIFIED_NDLM_CRPS_PRIMARY_FAMILY = as.character(ndlm_crps_primary_family),
    UNIFIED_POST_SMOKE_FAST = if (isTRUE(post_smoke_fast_effective)) "TRUE" else "FALSE",
    UNIFIED_POST_MULTIVAR_COMPONENT_DIAGNOSTICS = if (
      isTRUE(multivar_component_diagnostics_enabled) &&
        isTRUE(cfg$models$run_exdqlm_multivar)
    ) "TRUE" else "FALSE",
    UNIFIED_POST_MULTIVAR_COMPONENT_PRE_DAYS = as.character(multivar_component_pre_days),
    UNIFIED_POST_MULTIVAR_COMPONENT_QUANTILE = as.character(multivar_component_quantile),
    UNIFIED_POST_MULTIVAR_COMPONENT_FAIL_FAST = if (isTRUE(multivar_component_fail_fast)) "TRUE" else "FALSE",
    UNIFIED_POST_AUTHORITATIVE_SELECTED_SUPPORT = if (
      isTRUE(authoritative_selected_support_enabled) &&
        isTRUE(cfg$models$run_exdqlm_multivar)
    ) "TRUE" else "FALSE",
    UNIFIED_POST_AUTHORITATIVE_SELECTED_SUPPORT_FAIL_FAST = if (isTRUE(authoritative_selected_support_fail_fast)) "TRUE" else "FALSE",
    UNIFIED_FIT_QUANTILE_LABELS = encode_env_list(q_labels),
    UNIFIED_DISC_W_RDATA_PATHS = encode_env_list(disc_w_paths_abs),
    UNIFIED_POST_OUTPUT_SUBDIR = "",
    UNIFIED_MULTIVAR_FORECAST_TRANSFER_MODE = post_primary_multivar_mode,
    UNIFIED_POST_OUTPUT_SUFFIX = "",
    UNIFIED_POST_PRESERVE_OUT_DIR = "FALSE",
    UNIFIED_POST_CRPS_INPUT_HEALTH_ENABLED = if (isTRUE(unified_get(
      cfg, c("post", "crps_input_health", "enabled"), default = TRUE
    ))) "TRUE" else "FALSE",
    UNIFIED_POST_CRPS_INPUT_HEALTH_FAIL_FAST = if (isTRUE(unified_get(
      cfg, c("post", "crps_input_health", "fail_fast"), default = FALSE
    ))) "TRUE" else "FALSE",
    UNIFIED_POST_CRPS_INPUT_HEALTH_MIN_FINITE_SHARE = as.character(unified_get(
      cfg, c("post", "crps_input_health", "min_finite_share"), default = 1
    )),
    UNIFIED_POST_CRPS_INPUT_HEALTH_MAX_ABS = as.character(unified_get(
      cfg, c("post", "crps_input_health", "max_abs"), default = NA_real_
    )),
    UNIFIED_CUTOFF_DATE = as.character(post_cutoff_date),
    UNIFIED_FORECAST_START_DATE = as.character(post_forecast_start_date),
    UNIFIED_PLOT_START = as.character(post_plot_start_date),
    UNIFIED_PLOT_END = as.character(post_plot_end_date),
    UNIFIED_UNIV_RDATA_PATHS = encode_env_list(univ_paths_abs),
    UNIFIED_NDLM_RDATA_PATH = ndlm_path_abs,
    UNIFIED_NDLM_UNIVAR_RDATA_PATH = ndlm_univar_path_abs,
    RUN_ID = run_id,
    PROFILE = if (isTRUE(cfg$post$profile)) "TRUE" else "FALSE",
    PROFILE_DETAIL = if (isTRUE(cfg$post$profile_detail)) "TRUE" else "FALSE",
    UNIFIED_POST_FIGURES = if (isTRUE(cfg$post$figures)) "TRUE" else "FALSE",
    UNIFIED_POST_PUBLICATION_FIGURES = if (isTRUE(unified_get(
      cfg, c("post", "publication_figures", "enabled"), default = TRUE
    ))) "TRUE" else "FALSE",
    UNIFIED_POST_PUBLICATION_REWRITE_CANONICAL = if (isTRUE(unified_get(
      cfg, c("post", "publication_figures", "rewrite_canonical_png"), default = TRUE
    ))) "TRUE" else "FALSE",
    UNIFIED_POST_PUBLICATION_EXPORT_PDF = if (isTRUE(unified_get(
      cfg, c("post", "publication_figures", "export_pdf"), default = TRUE
    ))) "TRUE" else "FALSE",
    UNIFIED_POST_PUBLICATION_FAIL_FAST = if (isTRUE(unified_get(
      cfg, c("post", "publication_figures", "fail_fast"), default = TRUE
    ))) "TRUE" else "FALSE",
    UNIFIED_POST_PUBLICATION_STYLE_PATH = normalizePath(
      as.character(unified_get(
        cfg,
        c("post", "publication_figures", "style_config_path"),
        default = file.path(repo_root_abs, "config", "post_publication_figures.yaml")
      )),
      mustWork = FALSE
    ),
    ENV_SORT_KEEP_NA = if (isTRUE(sort_keep_na)) "TRUE" else "FALSE",
    EXPORT_TABLES = if (isTRUE(export_tables)) "TRUE" else "FALSE",
    EXPORT_TABLE_FORMATS = paste(unique(table_formats), collapse = ","),
    ENV_PROJECT_ROOT = repo_root_abs,
    ENV_RETROS_PATH = normalizePath(adapted_retros, mustWork = FALSE),
    ENV_NWS_FORECAST_PATH = normalizePath(adapted_nws, mustWork = FALSE),
    ENV_GLOFAS_FORECAST_PATH = normalizePath(adapted_glofas, mustWork = FALSE),
    if (nzchar(post_truth_usgs) && file.exists(post_truth_usgs)) c(ENV_USGS_DAILY_PATH = normalizePath(post_truth_usgs, mustWork = FALSE)) else character(0),
    if (nzchar(shared_feature_csv)) c(UNIFIED_COVARIATE_FEATURES_CSV = shared_feature_csv) else character(0),
    if (nzchar(shared_feature_csv)) c(ENV_COVARIATE_FEATURES_PATH = shared_feature_csv) else character(0),
    if (nzchar(shared_cov_paths$eli)) c(ENV_COV_ELI_PATH = normalizePath(shared_cov_paths$eli, mustWork = FALSE)) else character(0),
    if (nzchar(shared_cov_paths$oni)) c(ENV_COV_ONI_PATH = normalizePath(shared_cov_paths$oni, mustWork = FALSE)) else character(0),
    if (nzchar(shared_cov_paths$ppt)) c(ENV_PPT_PATH = normalizePath(shared_cov_paths$ppt, mustWork = FALSE)) else character(0),
    if (nzchar(shared_cov_paths$soil)) c(ENV_SOIL_PATH = normalizePath(shared_cov_paths$soil, mustWork = FALSE)) else character(0),
    if (nzchar(shared_cov_paths$pca)) c(ENV_PCA_PATH = normalizePath(shared_cov_paths$pca, mustWork = FALSE)) else character(0)
  )
  run_post_runner <- function(env_overrides, log_path) {
    env_kv <- sprintf("%s=%s", names(env_overrides), unname(env_overrides))
    rscript_bin <- file.path(R.home("bin"), "Rscript")
    cmd_out <- system2(
      rscript_bin,
      c("--vanilla", file.path("scripts", "run_environmetrics_figures.R")),
      stdout = TRUE,
      stderr = TRUE,
      env = env_kv
    )
    writeLines(cmd_out, log_path, useBytes = TRUE)
    status <- attr(cmd_out, "status")
    if (!is.null(status) && status != 0) {
      stop(sprintf("post stage failed; see %s", log_path), call. = FALSE)
    }
  }

  merge_dual_mode_crps_exports <- function(output_dir, table_formats, keep_na) {
    tables_dir <- file.path(output_dir, "tables")
    if (!dir.exists(tables_dir)) {
      return(invisible(FALSE))
    }

    read_optional_csv <- function(path) {
      if (!file.exists(path)) return(NULL)
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    }

    bind_union_rows <- function(dfs) {
      dfs <- Filter(Negate(is.null), dfs)
      if (length(dfs) == 0L) return(NULL)
      cols <- unique(unlist(lapply(dfs, names), use.names = FALSE))
      aligned <- lapply(dfs, function(df) {
        missing_cols <- setdiff(cols, names(df))
        for (nm in missing_cols) df[[nm]] <- NA
        df <- df[, cols, drop = FALSE]
        for (nm in names(df)) {
          if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
        }
        df
      })
      out <- do.call(rbind, aligned)
      dup_key <- do.call(
        paste,
        c(lapply(out, function(col) ifelse(is.na(col), "<NA>", as.character(col))), sep = "\r")
      )
      out <- out[!duplicated(dup_key), , drop = FALSE]
      rownames(out) <- NULL
      out
    }

    order_by_keys <- function(df, keys) {
      if (is.null(df) || nrow(df) == 0L) return(df)
      keys <- intersect(keys, names(df))
      if (length(keys) == 0L) return(df)
      ord_args <- lapply(keys, function(nm) {
        col <- df[[nm]]
        if (inherits(col, "Date")) return(col)
        if (is.numeric(col) || is.integer(col)) return(col)
        as.character(col)
      })
      ord <- do.call(order, c(ord_args, list(method = "radix", na.last = TRUE)))
      df <- df[ord, , drop = FALSE]
      rownames(df) <- NULL
      df
    }

    merge_pair <- function(base_name, keep_name, sort_keys) {
      merged <- bind_union_rows(list(
        read_optional_csv(file.path(tables_dir, base_name)),
        read_optional_csv(file.path(tables_dir, keep_name))
      ))
      order_by_keys(merged, sort_keys)
    }

    merged_summary <- merge_pair(
      "crps_forecast_summary.csv",
      "crps_forecast_summary_keep.csv",
      c("model_id", "transfer_mode", "cutoff_date", "forecast_start_date")
    )
    merged_per_time <- merge_pair(
      "crps_forecast_per_time.csv",
      "crps_forecast_per_time_keep.csv",
      c("model_id", "transfer_mode", "forecast_date", "lead_day")
    )
    merged_health_summary <- merge_pair(
      "crps_input_health.csv",
      "crps_input_health_keep.csv",
      c("model_id", "transfer_mode", "cutoff_date", "forecast_start_date")
    )
    merged_health_per_time <- merge_pair(
      "crps_input_health_per_time.csv",
      "crps_input_health_per_time_keep.csv",
      c("model_id", "transfer_mode", "forecast_date", "lead_day")
    )

    rewrote_any <- FALSE
    manifest_rows <- list()
    existing_manifest_path <- file.path(tables_dir, "posterior_table_exports_manifest.csv")
    existing_manifest <- if (file.exists(existing_manifest_path)) {
      utils::read.csv(existing_manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      data.frame(
        table_name = character(0),
        file_path = character(0),
        nrow = integer(0),
        ncol = integer(0),
        sha256 = character(0),
        stringsAsFactors = FALSE
      )
    }

    if (!is.null(merged_summary) && !is.null(merged_per_time)) {
      crps_export <- post_export_crps_tables(
        per_time_df = merged_per_time,
        summary_df = merged_summary,
        output_dir = tables_dir,
        table_formats = table_formats,
        keep_na = keep_na,
        numeric_digits = 17L,
        file_suffix = ""
      )
      manifest_rows[[length(manifest_rows) + 1L]] <- crps_export$manifest
      rewrote_any <- TRUE
    }

    if (!is.null(merged_health_summary) && !is.null(merged_health_per_time)) {
      health_export <- post_export_crps_input_health_tables(
        summary_df = merged_health_summary,
        per_time_df = merged_health_per_time,
        output_dir = tables_dir,
        table_formats = table_formats,
        keep_na = keep_na,
        numeric_digits = 17L,
        file_suffix = ""
      )
      manifest_rows[[length(manifest_rows) + 1L]] <- health_export$manifest
      rewrote_any <- TRUE
    }

    if (!isTRUE(rewrote_any)) {
      return(invisible(FALSE))
    }

    merged_manifest <- bind_union_rows(c(list(existing_manifest), manifest_rows))
    merged_manifest <- order_by_keys(merged_manifest, c("table_name", "file_path"))
    post_write_table_exports_manifest(merged_manifest, tables_dir)
    invisible(TRUE)
  }

  run_post_runner(
    env_overrides = base_env_overrides,
    log_path = file.path(post_logs, "post_runner.log")
  )

  if (
    isTRUE(cfg$models$run_exdqlm_multivar) &&
    multivar_dual_mode &&
    "keep" %in% names(disc_w_paths_by_mode) &&
    length(disc_w_paths_by_mode[["keep"]]) > 0L &&
    !identical(post_primary_multivar_mode, "keep")
  ) {
    keep_env <- base_env_overrides
    keep_env["UNIFIED_DISC_W_RDATA_PATHS"] <- encode_env_list(disc_w_paths_by_mode[["keep"]])
    keep_env["UNIFIED_MULTIVAR_FORECAST_TRANSFER_MODE"] <- "keep"
    keep_env["UNIFIED_POST_OUTPUT_SUFFIX"] <- "_keep"
    keep_env["UNIFIED_POST_PRESERVE_OUT_DIR"] <- "TRUE"
    run_post_runner(
      env_overrides = keep_env,
      log_path = file.path(post_logs, "post_runner_keep.log")
    )
    merge_dual_mode_crps_exports(
      output_dir = out_dir,
      table_formats = table_formats,
      keep_na = isTRUE(sort_keep_na)
    )
  }

  ndlm_only_mode <- isTRUE(ndlm_any_mode) &&
    !isTRUE(cfg$models$run_exdqlm_multivar) &&
    !isTRUE(cfg$models$run_exdqlm_univar)
  if (isTRUE(cfg$models$run_ndlm_main) && nzchar(ndlm_main_path_abs)) {
    ndlm_impl_mode <- as.character(unified_get(
      cfg,
      c("models", "ndlm_main", "implementation_mode"),
      default = "theory_aligned"
    ))
    if (!length(ndlm_impl_mode) || is.na(ndlm_impl_mode[[1L]]) || !nzchar(ndlm_impl_mode[[1L]])) {
      ndlm_impl_mode <- "theory_aligned"
    } else {
      ndlm_impl_mode <- ndlm_impl_mode[[1L]]
    }

    fit_logs_root <- file.path(fit_outputs_root_abs, "fit", "ndlm_main", "logs")
    ndlm_fit_log_primary <- if (identical(ndlm_impl_mode, "legacy_bridge")) {
      file.path(fit_logs_root, "ndlm_legacy.log")
    } else {
      file.path(fit_logs_root, "ndlm_theory.log")
    }
    ndlm_fit_log_alt <- if (identical(ndlm_impl_mode, "legacy_bridge")) {
      file.path(fit_logs_root, "ndlm_theory.log")
    } else {
      file.path(fit_logs_root, "ndlm_legacy.log")
    }
    ndlm_fit_log <- if (file.exists(ndlm_fit_log_primary)) ndlm_fit_log_primary else ndlm_fit_log_alt

    ndlm_usgs_site <- as.character(unified_get(cfg, c("site", "usgs_site"), default = "11160500"))
    ndlm_cutoff_date <- suppressWarnings(as.Date(unified_get(cfg, c("dates", "cutoff_date"), default = NA_character_)))
    ndlm_forecast_start <- if (!is.na(ndlm_cutoff_date)) as.character(ndlm_cutoff_date + 1L) else NA_character_
    ndlm_forecast_end <- as.character(unified_get(cfg, c("dates", "plot_end"), default = NA_character_))

    ndlm_diag_result <- unified_generate_ndlm_post_diagnostics(
      run_root = run_root_abs,
      ndlm_rdata_path = ndlm_main_path_abs,
      retros_csv_path = normalizePath(adapted_retros, mustWork = FALSE),
      nws_csv_path = normalizePath(adapted_nws, mustWork = FALSE),
      glofas_csv_path = normalizePath(adapted_glofas, mustWork = FALSE),
      fit_log_path = ndlm_fit_log,
      output_dir = file.path(run_root_abs, "diagnostics", "ndlm"),
      usgs_site = ndlm_usgs_site,
      forecast_start_date = ndlm_forecast_start,
      forecast_end_date = ndlm_forecast_end,
      strict_contract = ndlm_only_mode && !identical(ndlm_impl_mode, "legacy_bridge"),
      state_ci_max_draws = if (identical(ndlm_impl_mode, "legacy_bridge")) 250L else NULL
    )

    ndlm_diag_paths <- unlist(ndlm_diag_result$paths, use.names = FALSE)
    ndlm_diag_paths <- ndlm_diag_paths[nzchar(ndlm_diag_paths)]
    for (diag_path in ndlm_diag_paths) {
      if (!file.exists(diag_path)) next
      if (grepl("\\.png$", diag_path, ignore.case = TRUE)) {
        manifest <- unified_manifest_add_artifact(manifest, diag_path, storage_scale = "image_png", analysis_scale = "n/a", role = "diagnostics")
      } else if (grepl("\\.csv$", diag_path, ignore.case = TRUE)) {
        manifest <- unified_manifest_add_artifact(manifest, diag_path, storage_scale = "table_csv", analysis_scale = "n/a", role = "diagnostics")
      } else if (grepl("\\.(md|txt)$", diag_path, ignore.case = TRUE)) {
        manifest <- unified_manifest_add_artifact(manifest, diag_path, storage_scale = "text_plain", analysis_scale = "n/a", role = "diagnostics")
      } else {
        manifest <- unified_manifest_add_artifact(manifest, diag_path, storage_scale = "text_plain", analysis_scale = "n/a", role = "diagnostics")
      }
    }
  }

  add_stage_post_artifacts <- function(manifest_obj, root_dir, role_tag) {
    if (!dir.exists(root_dir)) {
      return(manifest_obj)
    }
    generated <- list.files(root_dir, full.names = TRUE, recursive = TRUE)
    if (length(generated) == 0L) {
      return(manifest_obj)
    }
    allowed_ext <- "\\.(png|pdf|csv|tsv|txt|json|yaml|yml|rds)$"
    root_abs <- normalizePath(root_dir, mustWork = TRUE)
    root_prefix <- paste0(root_abs, .Platform$file.sep)

    for (f in generated) {
      if (isTRUE(file.info(f)$isdir)) next
      if (!grepl(allowed_ext, f, ignore.case = TRUE)) next
      f_abs <- normalizePath(f, mustWork = FALSE)
      if (!startsWith(f_abs, root_prefix) && !identical(f_abs, root_abs)) next

      if (grepl("\\.png$", f, ignore.case = TRUE)) {
        manifest_obj <- unified_manifest_add_artifact(manifest_obj, f, storage_scale = "image_png", analysis_scale = "n/a", role = role_tag)
      } else if (grepl("\\.pdf$", f, ignore.case = TRUE)) {
        manifest_obj <- unified_manifest_add_artifact(manifest_obj, f, storage_scale = "text_binary", analysis_scale = "n/a", role = role_tag)
      } else if (grepl("\\.rds$", f, ignore.case = TRUE)) {
        manifest_obj <- unified_manifest_add_artifact(manifest_obj, f, storage_scale = "model_state", role = role_tag)
      } else if (grepl("\\.csv$", f, ignore.case = TRUE)) {
        manifest_obj <- unified_manifest_add_artifact(manifest_obj, f, storage_scale = "table_csv", analysis_scale = "n/a", role = role_tag)
      } else if (grepl("\\.tsv$", f, ignore.case = TRUE)) {
        manifest_obj <- unified_manifest_add_artifact(manifest_obj, f, storage_scale = "table_tsv", analysis_scale = "n/a", role = role_tag)
      } else if (grepl("\\.json$", f, ignore.case = TRUE)) {
        manifest_obj <- unified_manifest_add_artifact(manifest_obj, f, storage_scale = "text_json", analysis_scale = "n/a", role = role_tag)
      } else if (grepl("\\.(yaml|yml)$", f, ignore.case = TRUE)) {
        manifest_obj <- unified_manifest_add_artifact(manifest_obj, f, storage_scale = "text_yaml", analysis_scale = "n/a", role = role_tag)
      } else if (grepl("\\.txt$", f, ignore.case = TRUE)) {
        manifest_obj <- unified_manifest_add_artifact(manifest_obj, f, storage_scale = "text_plain", analysis_scale = "n/a", role = role_tag)
      }
    }
    manifest_obj
  }

  manifest <- add_stage_post_artifacts(manifest, out_dir, role_tag = "post_output")
  manifest <- add_stage_post_artifacts(manifest, post_cache_dir, role_tag = "post_cache")

  list(manifest = manifest)
}
