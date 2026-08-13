# unified/stages/stage_fit.R

unified_normalize_fit_worker_result <- function(res, context_label = "fit stage worker") {
  required <- c("quantile", "output_path", "log_path", "status")
  if (is.list(res) && all(required %in% names(res))) {
    return(res)
  }

  res_preview <- tryCatch(paste(utils::head(as.character(res), 5L), collapse = " | "), error = function(e) "")
  if (!nzchar(res_preview)) {
    res_preview <- sprintf("<class=%s>", paste(class(res), collapse = "/"))
  }
  stop(
    sprintf("%s returned invalid result: %s", context_label, res_preview),
    call. = FALSE
  )
}

unified_multivar_fit_health_check <- function(
  rdata_path,
  quantile,
  transfer_mode,
  report_path = NULL,
  terminal_report_path = NULL,
  terminal_csv_path = NULL,
  latent_limit = 650,
  sigma_limit = 100,
  state_limit = 1000,
  history_latent_limit = 25,
  state_norm_sq_per_T_limit = 1e4,
  transfer_level_limit = 25,
  transfer_coef_limit = 100
) {
  if (!file.exists(rdata_path)) {
    stop(sprintf("[FIT_FORECAST_HEALTH_MISSING] missing RData: %s", rdata_path), call. = FALSE)
  }

  env <- new.env(parent = emptyenv())
  load(rdata_path, envir = env)
  theta_name <- grep("^new\\.theta\\.out", ls(env), value = TRUE)
  gamsig_name <- grep("^new\\.gamsig\\.out", ls(env), value = TRUE)
  if (length(theta_name) != 1L) {
    stop(
      sprintf(
        "[FIT_FORECAST_HEALTH_OBJECT] expected exactly one new.theta.out object in %s; found %d",
        rdata_path,
        length(theta_name)
      ),
      call. = FALSE
    )
  }

  theta <- get(theta_name[[1L]], envir = env)
  gamsig <- if (length(gamsig_name) == 1L) get(gamsig_name[[1L]], envir = env) else NULL

  max_abs_sm_ens <- NA_real_
  nonfinite_sm_ens <- 0L
  if (is.list(theta$sm_ens) && length(theta$sm_ens) > 0L) {
    sm_vals <- unlist(lapply(theta$sm_ens, function(x) as.numeric(as.matrix(x))), use.names = FALSE)
    if (length(sm_vals) > 0L) {
      nonfinite_sm_ens <- sum(!is.finite(sm_vals))
      sm_vals <- sm_vals[is.finite(sm_vals)]
      if (length(sm_vals) > 0L) {
        max_abs_sm_ens <- max(abs(sm_vals), na.rm = TRUE)
      }
    }
  }

  TT <- NA_integer_
  if (is.numeric(theta$sm) && !is.null(dim(theta$sm)) && length(dim(theta$sm)) == 2L) {
    TT <- as.integer(ncol(theta$sm))
  }

  sm_mat <- if (is.numeric(theta$sm) && !is.null(dim(theta$sm)) && length(dim(theta$sm)) == 2L) {
    as.matrix(theta$sm)
  } else {
    matrix(numeric(0), nrow = 0L, ncol = 0L)
  }
  sm_vals_all <- as.numeric(sm_mat)
  nonfinite_sm <- sum(!is.finite(sm_vals_all))
  sm_vals_finite <- sm_vals_all[is.finite(sm_vals_all)]
  state_norm_sq <- if (length(sm_vals_finite) > 0L) sum(sm_vals_finite^2) else NA_real_
  state_norm_sq_per_T <- if (is.finite(state_norm_sq) && is.finite(TT) && TT > 0L) {
    state_norm_sq / as.numeric(TT)
  } else {
    NA_real_
  }

  transfer_level_max_abs <- NA_real_
  transfer_level_median_abs <- NA_real_
  if (nrow(sm_mat) >= 22L && ncol(sm_mat) > 0L) {
    transfer_level_vals <- as.numeric(sm_mat[22L, , drop = TRUE])
    transfer_level_finite <- transfer_level_vals[is.finite(transfer_level_vals)]
    if (length(transfer_level_finite) > 0L) {
      transfer_level_max_abs <- max(abs(transfer_level_finite), na.rm = TRUE)
      transfer_level_median_abs <- stats::median(abs(transfer_level_finite), na.rm = TRUE)
    }
  }

  transfer_coef_max_abs <- NA_real_
  if (nrow(sm_mat) >= 23L && ncol(sm_mat) > 0L) {
    transfer_coef_vals <- as.numeric(sm_mat[23L:nrow(sm_mat), , drop = FALSE])
    transfer_coef_vals <- transfer_coef_vals[is.finite(transfer_coef_vals)]
    if (length(transfer_coef_vals) > 0L) {
      transfer_coef_max_abs <- max(abs(transfer_coef_vals), na.rm = TRUE)
    }
  }

  max_abs_history_exps <- NA_real_
  nonfinite_history_exps <- 0L
  finite_history_exps <- 0L
  if (is.numeric(theta$exps) && !is.null(dim(theta$exps)) && length(dim(theta$exps)) == 2L &&
      ncol(theta$exps) > 0L) {
    hist_end <- if (is.finite(TT) && TT > 0L) min(as.integer(TT), ncol(theta$exps)) else ncol(theta$exps)
    exps_hist <- theta$exps[, seq_len(hist_end), drop = FALSE]
    nonfinite_history_exps <- sum(!is.finite(exps_hist))
    finite_vals <- exps_hist[is.finite(exps_hist)]
    finite_history_exps <- length(finite_vals)
    if (length(finite_vals) > 0L) {
      max_abs_history_exps <- max(abs(finite_vals), na.rm = TRUE)
    }
  }

  max_abs_forecast_exps <- NA_real_
  nonfinite_forecast_exps <- 0L
  finite_forecast_exps <- 0L
  if (is.numeric(theta$exps) && !is.null(dim(theta$exps)) && length(dim(theta$exps)) == 2L &&
      is.finite(TT) && TT >= 0L && ncol(theta$exps) > TT) {
    exps_fore <- theta$exps[, (TT + 1L):ncol(theta$exps), drop = FALSE]
    nonfinite_forecast_exps <- sum(!is.finite(exps_fore))
    finite_vals <- exps_fore[is.finite(exps_fore)]
    finite_forecast_exps <- length(finite_vals)
    if (length(finite_vals) > 0L) {
      max_abs_forecast_exps <- max(abs(finite_vals), na.rm = TRUE)
    }
  }

  max_E_sigma <- NA_real_
  if (is.list(gamsig) && !is.null(gamsig$E.sigma)) {
    sigma_vals <- as.numeric(gamsig$E.sigma)
    sigma_vals <- sigma_vals[is.finite(sigma_vals)]
    if (length(sigma_vals) > 0L) {
      max_E_sigma <- max(sigma_vals, na.rm = TRUE)
    }
  }

  summary_lines <- c(
    sprintf("rdata_path=%s", normalizePath(rdata_path, mustWork = FALSE)),
    sprintf("transfer_mode=%s", as.character(transfer_mode)),
    sprintf("quantile=%s", as.character(quantile)),
    sprintf("theta_object=%s", theta_name[[1L]]),
    sprintf("gamsig_object=%s", if (length(gamsig_name) == 1L) gamsig_name[[1L]] else ""),
    sprintf("TT=%s", if (is.finite(TT)) as.character(TT) else "NA"),
    sprintf("max_abs_sm_ens=%s", if (is.finite(max_abs_sm_ens)) format(max_abs_sm_ens, digits = 10) else "NA"),
    sprintf("nonfinite_sm_ens=%d", as.integer(nonfinite_sm_ens)),
    sprintf("state_norm_sq=%s", if (is.finite(state_norm_sq)) format(state_norm_sq, digits = 10) else "NA"),
    sprintf("state_norm_sq_per_T=%s", if (is.finite(state_norm_sq_per_T)) format(state_norm_sq_per_T, digits = 10) else "NA"),
    sprintf("nonfinite_sm=%d", as.integer(nonfinite_sm)),
    sprintf("transfer_level_max_abs=%s", if (is.finite(transfer_level_max_abs)) format(transfer_level_max_abs, digits = 10) else "NA"),
    sprintf("transfer_level_median_abs=%s", if (is.finite(transfer_level_median_abs)) format(transfer_level_median_abs, digits = 10) else "NA"),
    sprintf("transfer_coef_max_abs=%s", if (is.finite(transfer_coef_max_abs)) format(transfer_coef_max_abs, digits = 10) else "NA"),
    sprintf("max_abs_history_exps=%s", if (is.finite(max_abs_history_exps)) format(max_abs_history_exps, digits = 10) else "NA"),
    sprintf("finite_history_exps=%d", as.integer(finite_history_exps)),
    sprintf("nonfinite_history_exps=%d", as.integer(nonfinite_history_exps)),
    sprintf("max_abs_forecast_exps=%s", if (is.finite(max_abs_forecast_exps)) format(max_abs_forecast_exps, digits = 10) else "NA"),
    sprintf("finite_forecast_exps=%d", as.integer(finite_forecast_exps)),
    sprintf("nonfinite_forecast_exps=%d", as.integer(nonfinite_forecast_exps)),
    sprintf("max_E_sigma=%s", if (is.finite(max_E_sigma)) format(max_E_sigma, digits = 10) else "NA"),
    sprintf("latent_limit=%s", format(as.numeric(latent_limit), digits = 10)),
    sprintf("sigma_limit=%s", format(as.numeric(sigma_limit), digits = 10)),
    sprintf("state_limit=%s", format(as.numeric(state_limit), digits = 10)),
    sprintf("history_latent_limit=%s", format(as.numeric(history_latent_limit), digits = 10)),
    sprintf("state_norm_sq_per_T_limit=%s", format(as.numeric(state_norm_sq_per_T_limit), digits = 10)),
    sprintf("transfer_level_limit=%s", format(as.numeric(transfer_level_limit), digits = 10)),
    sprintf("transfer_coef_limit=%s", format(as.numeric(transfer_coef_limit), digits = 10))
  )
  if (!is.null(report_path) && nzchar(report_path)) {
    writeLines(summary_lines, con = report_path)
  }

  health_row <- function(metric, value, limit, direction = "max", severity = "hard") {
    value <- suppressWarnings(as.numeric(value)[1L])
    limit <- suppressWarnings(as.numeric(limit)[1L])
    severity <- as.character(severity)[1L]
    if (!nzchar(severity) || !(severity %in% c("hard", "warning"))) {
      severity <- "hard"
    }
    failed <- FALSE
    if (identical(direction, "required_positive_count")) {
      failed <- !is.finite(value) || value <= 0
    } else if (identical(direction, "required_zero_count")) {
      failed <- is.finite(value) && value > 0
    } else if (identical(direction, "max")) {
      failed <- is.finite(value) && is.finite(limit) && value > limit
    }
    status <- if (isTRUE(failed)) {
      if (identical(severity, "warning")) "warn" else "fail"
    } else {
      "ok"
    }
    data.frame(
      metric = metric,
      value = value,
      limit = limit,
      direction = direction,
      severity = severity,
      status = status,
      stringsAsFactors = FALSE
    )
  }
  terminal_rows <- rbind(
    health_row("finite_history_exps", finite_history_exps, 0, "required_positive_count"),
    health_row("nonfinite_history_exps", nonfinite_history_exps, 0, "required_zero_count"),
    health_row("nonfinite_sm", nonfinite_sm, 0, "required_zero_count"),
    health_row("max_abs_history_exps", max_abs_history_exps, history_latent_limit, severity = "warning"),
    health_row("state_norm_sq_per_T", state_norm_sq_per_T, state_norm_sq_per_T_limit),
    health_row("transfer_level_max_abs", transfer_level_max_abs, transfer_level_limit),
    health_row("transfer_coef_max_abs", transfer_coef_max_abs, transfer_coef_limit)
  )
  if (!is.null(terminal_csv_path) && nzchar(terminal_csv_path)) {
    utils::write.csv(terminal_rows, file = terminal_csv_path, row.names = FALSE)
  }
  if (!is.null(terminal_report_path) && nzchar(terminal_report_path)) {
    hard_violations <- terminal_rows$metric[
      terminal_rows$status == "fail" & terminal_rows$severity == "hard"
    ]
    warning_violations <- terminal_rows$metric[terminal_rows$status == "warn"]
    terminal_status <- if (length(hard_violations) > 0L) {
      "fail"
    } else if (length(warning_violations) > 0L) {
      "warn"
    } else {
      "ok"
    }
    terminal_lines <- c(
      sprintf("rdata_path=%s", normalizePath(rdata_path, mustWork = FALSE)),
      sprintf("transfer_mode=%s", as.character(transfer_mode)),
      sprintf("quantile=%s", as.character(quantile)),
      sprintf("theta_object=%s", theta_name[[1L]]),
      sprintf("TT=%s", if (is.finite(TT)) as.character(TT) else "NA"),
      sprintf("terminal_status=%s", terminal_status),
      sprintf("hard_violations=%s", paste(hard_violations, collapse = "|")),
      sprintf("warnings=%s", paste(warning_violations, collapse = "|")),
      sprintf("violations=%s", paste(terminal_rows$metric[terminal_rows$status != "ok"], collapse = "|"))
    )
    writeLines(terminal_lines, con = terminal_report_path)
  }

  violations <- character(0)
  if (nonfinite_sm_ens > 0L) {
    violations <- c(violations, sprintf("nonfinite_sm_ens=%d", as.integer(nonfinite_sm_ens)))
  }
  if (finite_forecast_exps <= 0L) {
    violations <- c(violations, "finite_forecast_exps=0")
  }
  if (finite_history_exps <= 0L) {
    violations <- c(violations, "finite_history_exps=0")
  }
  if (nonfinite_history_exps > 0L) {
    violations <- c(violations, sprintf("nonfinite_history_exps=%d", as.integer(nonfinite_history_exps)))
  }
  if (nonfinite_sm > 0L) {
    violations <- c(violations, sprintf("nonfinite_sm=%d", as.integer(nonfinite_sm)))
  }
  if (is.finite(max_abs_sm_ens) && max_abs_sm_ens > state_limit) {
    violations <- c(violations, sprintf("max_abs_sm_ens=%.6f > %.6f", max_abs_sm_ens, as.numeric(state_limit)))
  }
  if (is.finite(max_abs_history_exps) && max_abs_history_exps > history_latent_limit) {
    violations <- c(violations, sprintf("max_abs_history_exps=%.6f > %.6f", max_abs_history_exps, as.numeric(history_latent_limit)))
  }
  if (is.finite(max_abs_forecast_exps) && max_abs_forecast_exps > latent_limit) {
    violations <- c(violations, sprintf("max_abs_forecast_exps=%.6f > %.6f", max_abs_forecast_exps, as.numeric(latent_limit)))
  }
  if (is.finite(state_norm_sq_per_T) && state_norm_sq_per_T > state_norm_sq_per_T_limit) {
    violations <- c(violations, sprintf("state_norm_sq_per_T=%.6f > %.6f", state_norm_sq_per_T, as.numeric(state_norm_sq_per_T_limit)))
  }
  if (is.finite(transfer_level_max_abs) && transfer_level_max_abs > transfer_level_limit) {
    violations <- c(violations, sprintf("transfer_level_max_abs=%.6f > %.6f", transfer_level_max_abs, as.numeric(transfer_level_limit)))
  }
  if (is.finite(transfer_coef_max_abs) && transfer_coef_max_abs > transfer_coef_limit) {
    violations <- c(violations, sprintf("transfer_coef_max_abs=%.6f > %.6f", transfer_coef_max_abs, as.numeric(transfer_coef_limit)))
  }
  if (is.finite(max_E_sigma) && max_E_sigma > sigma_limit) {
    violations <- c(violations, sprintf("max_E_sigma=%.6f > %.6f", max_E_sigma, as.numeric(sigma_limit)))
  }

  list(
    violations = violations,
    report_path = report_path,
    terminal_report_path = terminal_report_path,
    terminal_csv_path = terminal_csv_path,
    max_abs_sm_ens = max_abs_sm_ens,
    nonfinite_sm_ens = nonfinite_sm_ens,
    state_norm_sq = state_norm_sq,
    state_norm_sq_per_T = state_norm_sq_per_T,
    nonfinite_sm = nonfinite_sm,
    max_abs_history_exps = max_abs_history_exps,
    finite_history_exps = finite_history_exps,
    nonfinite_history_exps = nonfinite_history_exps,
    transfer_level_max_abs = transfer_level_max_abs,
    transfer_level_median_abs = transfer_level_median_abs,
    transfer_coef_max_abs = transfer_coef_max_abs,
    terminal_rows = terminal_rows,
    hard_violations = terminal_rows$metric[
      terminal_rows$status == "fail" & terminal_rows$severity == "hard"
    ],
    warning_violations = terminal_rows$metric[terminal_rows$status == "warn"],
    max_abs_forecast_exps = max_abs_forecast_exps,
    finite_forecast_exps = finite_forecast_exps,
    nonfinite_forecast_exps = nonfinite_forecast_exps,
    max_E_sigma = max_E_sigma
  )
}

unified_resolve_fit_parallel_mode <- function(cfg) {
  mode <- as.character(unified_get(cfg, c("fit", "parallel", "mode"), default = "one_core_per_model"))
  if (!length(mode) || is.na(mode[[1]]) || !nzchar(mode[[1]])) {
    return("one_core_per_model")
  }
  mode <- gsub("[^A-Za-z0-9]+", "_", tolower(mode[[1]]))
  if (!(mode %in% c("by_family", "global_models", "one_core_per_model"))) {
    warning(
      sprintf("unknown fit.parallel.mode=%s; falling back to one_core_per_model", mode),
      call. = FALSE
    )
    return("one_core_per_model")
  }
  mode
}

unified_resolve_fit_parallel_workers <- function(cfg, n_jobs, default_workers = 1L) {
  workers <- unified_get(cfg, c("fit", "parallel", "workers"), default = default_workers)
  workers <- suppressWarnings(as.integer(workers))
  if (!is.finite(workers) || workers < 1L) workers <- 1L
  if (n_jobs < 1L) return(1L)
  min(workers, as.integer(n_jobs))
}

unified_inspect_ndlm_retros_history <- function(retros_path) {
  retros_path <- normalizePath(retros_path, mustWork = FALSE)
  if (!file.exists(retros_path)) {
    stop(sprintf("NDLM retros path does not exist: %s", retros_path), call. = FALSE)
  }

  retros <- tryCatch(
    utils::read.csv(retros_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      stop(sprintf("unable to read NDLM retros CSV %s: %s", retros_path, conditionMessage(e)), call. = FALSE)
    }
  )
  if (!is.data.frame(retros) || nrow(retros) < 1L) {
    stop(sprintf("NDLM retros CSV is empty or invalid: %s", retros_path), call. = FALSE)
  }

  resolve_history_column <- function(df) {
    preferred <- c("USGS", "usgs", "Observed", "observed", "retros", "Retros", "retrospective", "Retrospective")
    for (nm in preferred) {
      if (!(nm %in% names(df))) next
      vals <- suppressWarnings(as.numeric(df[[nm]]))
      if (any(is.finite(vals), na.rm = TRUE)) {
        return(list(name = nm, values = vals))
      }
    }

    fallback_names <- names(df)[!grepl("date|time|nws|glofas", names(df), ignore.case = TRUE)]
    for (nm in fallback_names) {
      vals <- suppressWarnings(as.numeric(df[[nm]]))
      if (any(is.finite(vals), na.rm = TRUE)) {
        return(list(name = nm, values = vals))
      }
    }

    for (nm in names(df)) {
      if (grepl("date|time", nm, ignore.case = TRUE)) next
      vals <- suppressWarnings(as.numeric(df[[nm]]))
      if (any(is.finite(vals), na.rm = TRUE)) {
        return(list(name = nm, values = vals))
      }
    }

    stop(
      sprintf("unable to resolve NDLM retrospective history column from %s", retros_path),
      call. = FALSE
    )
  }

  history_col <- resolve_history_column(retros)
  finite_count <- sum(is.finite(history_col$values), na.rm = TRUE)
  list(
    retros_path = retros_path,
    history_column = history_col$name,
    finite_count = as.integer(finite_count),
    total_rows = as.integer(nrow(retros))
  )
}

unified_assert_ndlm_retros_history <- function(retros_path, min_required = 30L, report_path = NULL) {
  min_required <- suppressWarnings(as.integer(min_required))
  if (!is.finite(min_required) || min_required < 1L) {
    min_required <- 30L
  }

  inspection <- unified_inspect_ndlm_retros_history(retros_path)
  lines <- c(
    sprintf("retros_path=%s", inspection$retros_path),
    sprintf("history_column=%s", inspection$history_column),
    sprintf("finite_count=%d", inspection$finite_count),
    sprintf("total_rows=%d", inspection$total_rows),
    sprintf("minimum_required=%d", min_required)
  )
  if (!is.null(report_path) && nzchar(report_path)) {
    writeLines(lines, con = report_path, useBytes = TRUE)
  }

  if (inspection$finite_count < min_required) {
    stop(
      sprintf(
        paste0(
          "[NDLM_RETROS_HISTORY] insufficient retrospective history for NDLM-enabled lane: ",
          "retros_path=%s history_column=%s finite_count=%d minimum_required=%d"
        ),
        inspection$retros_path,
        inspection$history_column,
        inspection$finite_count,
        min_required
      ),
      call. = FALSE
    )
  }

  inspection
}



unified_deep_merge_list <- function(base, patch) {
  if (!is.list(base)) base <- list()
  if (!is.list(patch)) return(patch)
  out <- base
  for (nm in names(patch)) {
    lhs <- out[[nm]]
    rhs <- patch[[nm]]
    if (is.list(lhs) && is.list(rhs)) {
      out[[nm]] <- unified_deep_merge_list(lhs, rhs)
    } else {
      out[[nm]] <- rhs
    }
  }
  out
}

unified_quantile_override_keys <- function(q) {
  q_num <- suppressWarnings(as.numeric(q))
  if (!is.finite(q_num)) return(character(0))
  pct <- as.integer(round(q_num * 100))
  fmt2 <- sprintf('%.2f', q_num)
  fmt_trim <- sub("\\.?0+$", "", fmt2)
  unique(c(
    sprintf('q=%02d', pct),
    sprintf('q%d', pct),
    fmt2,
    fmt_trim,
    as.character(q_num),
    as.character(pct)
  ))
}

unified_resolve_gamma_sigma_policy <- function(cfg, family_key, q = NULL) {
  policy <- unified_get(cfg, c('fit', family_key, 'gamma_sigma'), default = list())
  if (!is.list(policy)) policy <- list()
  overrides <- policy[['quantile_overrides']]
  if (is.list(overrides) && length(overrides) > 0L && !is.null(q)) {
    keys <- unified_quantile_override_keys(q)
    for (key in keys) {
      patch <- overrides[[key]]
      if (is.list(patch)) {
        policy <- unified_deep_merge_list(policy, patch)
        break
      }
    }
  }
  policy[['quantile_overrides']] <- NULL
  policy
}

unified_stage_fit <- function(cfg, run_root, repo_root, manifest) {
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(repo_root)

  run_root_abs <- normalizePath(run_root, mustWork = FALSE)
  io_settings <- unified_get_run_io_settings(cfg)
  fit_root <- file.path(run_root_abs, "fit")
  fit_inputs <- file.path(fit_root, "inputs")
  fit_logs_root <- file.path(fit_root, "logs")
  preflight_dir <- file.path(run_root_abs, "preflight")
  fit_preflight_log <- file.path(fit_logs_root, "preflight.log")
  fit_stage_log <- file.path(fit_logs_root, "fit_stage.log")
  fit_worker_error_log <- file.path(fit_logs_root, "fit_worker_errors.log")
  dir.create(fit_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(fit_inputs, recursive = TRUE, showWarnings = FALSE)
  dir.create(fit_logs_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(preflight_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(fit_stage_log)) {
    file.create(fit_stage_log)
  }
  append_fit_stage_log <- function(msg) {
    line <- sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), as.character(msg))
    try(write(line, file = fit_stage_log, append = TRUE), silent = TRUE)
    invisible(NULL)
  }
  append_fit_preflight_log <- function(msg) {
    line <- sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), as.character(msg))
    try(write(line, file = fit_preflight_log, append = TRUE), silent = TRUE)
    invisible(NULL)
  }
  append_fit_stage_log(sprintf("stage_fit start run_root=%s", run_root_abs))

  run_exdqlm_multivar <- isTRUE(cfg$models$run_exdqlm_multivar)
  run_exdqlm_univar <- isTRUE(cfg$models$run_exdqlm_univar)
  run_ndlm_main <- isTRUE(cfg$models$run_ndlm_main)
  run_ndlm_univar <- isTRUE(cfg$models$run_ndlm_univar)
  if (!run_exdqlm_multivar && !run_exdqlm_univar && !run_ndlm_main && !run_ndlm_univar) {
    no_models_msg <- "stage_fit no-op: all model families disabled"
    message(no_models_msg)
    append_fit_stage_log(no_models_msg)
    append_fit_stage_log("stage_fit complete")
    if (file.exists(fit_stage_log)) {
      manifest <- unified_manifest_add_artifact(manifest, fit_stage_log, storage_scale = "text")
    }
    if (file.exists(fit_preflight_log)) {
      manifest <- unified_manifest_add_artifact(
        manifest,
        fit_preflight_log,
        storage_scale = "text",
        role = "preflight"
      )
    }
    return(list(manifest = manifest))
  }

  shared_paths <- unified_shared_input_paths(run_root)
  use_shared_inputs <- isTRUE(cfg$stages$data_prep_shared) || dir.exists(shared_paths$root)
  if (use_shared_inputs) {
    shared_validation <- unified_validate_required_shared_inputs(
      run_root = run_root,
      stage_name = "fit",
      manifest = manifest,
      enabled_models = cfg$models
    )
    source_parameters <- shared_validation$paths$parameters
    source_retros <- shared_validation$paths$retros
    source_nws <- shared_validation$paths$nws
    source_glofas <- shared_validation$paths$glofas
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
    source_parameters <- cfg$inputs$fit$parameters_path
    source_retros <- cfg$inputs$fit$retros_path
    source_nws <- cfg$inputs$fit$nws_forecast_path
    source_glofas <- cfg$inputs$fit$glofas_forecast_path
    source_retros_scale <- cfg$inputs$fit$retros_storage_scale
    source_nws_scale <- cfg$inputs$fit$nws_storage_scale
    source_glofas_scale <- cfg$inputs$fit$glofas_storage_scale
  }

  cutoff_raw <- unified_get(cfg, c("dates", "cutoff_date"), default = NA_character_)
  cutoff_date <- suppressWarnings(as.Date(cutoff_raw))
  if (!is.na(cutoff_date) && isTRUE(use_shared_inputs)) {
    unified_validate_forecast_window_csv(
      source_glofas,
      label = "shared glofas_forecast",
      stage_name = "fit/shared_inputs",
      cutoff_date = cutoff_date
    )
    unified_validate_forecast_window_csv(
      source_nws,
      label = "shared nws_forecast",
      stage_name = "fit/shared_inputs",
      cutoff_date = cutoff_date
    )
  }

  legacy_scale <- cfg$scale_contract$legacy_fit_input_scale
  unified_assert_known_scale(legacy_scale, "scale_contract.legacy_fit_input_scale")

  parameters_copy <- file.path(fit_inputs, basename(source_parameters))
  file.copy(source_parameters, parameters_copy, overwrite = TRUE)

  adapted_retros <- file.path(fit_inputs, "retros_fit_adapter.csv")
  adapted_nws <- file.path(fit_inputs, "nws_fit_adapter.csv")
  adapted_glofas <- file.path(fit_inputs, "glofas_fit_adapter.csv")

  unified_adapt_csv_scale(
    input_path = source_retros,
    output_path = adapted_retros,
    from_scale = source_retros_scale,
    to_scale = legacy_scale,
    positive_required = FALSE
  )
  manifest <- unified_manifest_add_scale_history(
    manifest,
    artifact = "fit_input/retros",
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
    artifact = "fit_input/nws_forecast",
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
    artifact = "fit_input/glofas_forecast",
    from_scale = source_glofas_scale,
    to_scale = legacy_scale,
    transform = sprintf("adapter_%s_to_%s", source_glofas_scale, legacy_scale)
  )

  manifest <- unified_manifest_add_artifact(manifest, adapted_retros, storage_scale = legacy_scale)
  manifest <- unified_manifest_add_artifact(manifest, adapted_nws, storage_scale = legacy_scale)
  manifest <- unified_manifest_add_artifact(manifest, adapted_glofas, storage_scale = legacy_scale)

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
  } else {
    if (length(fit_covariates) > 0L) {
      for (entry in fit_covariates) {
        if (!is.list(entry)) next
        cov_name <- if (is.null(entry$name)) "" else as.character(entry$name)
        cov_path <- if (is.null(entry$path)) "" else as.character(entry$path)
        if (!nzchar(cov_name) || !nzchar(cov_path) || !file.exists(cov_path)) next
        assign_cov_path(cov_name, cov_path)
      }
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
  using_engineered_covariates <- nzchar(shared_feature_csv)

  quantiles <- as.numeric(cfg$fit$quantiles)
  univar_likelihood_mode <- unified_resolve_univar_likelihood_mode(cfg, default = "exal")
  multivar_likelihood_mode <- unified_resolve_multivar_likelihood_mode(cfg, default = "exal")
  ndlm_forecast_transfer_mode <- unified_resolve_ndlm_forecast_transfer_mode(cfg, default = "keep")
  ndlm_univar_forecast_transfer_mode <- unified_resolve_ndlm_univar_forecast_transfer_mode(cfg, default = "keep")
  univar_impl_mode <- unified_get(
    cfg,
    c("models", "exdqlm_univar", "implementation_mode"),
    default = "legacy_bridge"
  )
  ndlm_impl_mode <- unified_get(
    cfg,
    c("models", "ndlm_main", "implementation_mode"),
    default = "theory_aligned"
  )
  ndlm_univar_impl_mode <- unified_get(
    cfg,
    c("models", "ndlm_univar", "implementation_mode"),
    default = "theory_aligned_closed_form"
  )
  append_fit_stage_log(sprintf(
    paste0(
      "fit model_modes multivar_likelihood=%s univar_likelihood=%s ",
      "ndlm_forecast_transfer_mode=%s ndlm_univar_forecast_transfer_mode=%s"
    ),
    as.character(multivar_likelihood_mode),
    as.character(univar_likelihood_mode),
    as.character(ndlm_forecast_transfer_mode),
    as.character(ndlm_univar_forecast_transfer_mode)
  ))
  if (isTRUE(cfg$models$run_exdqlm_univar) &&
      identical(univar_impl_mode, "theory_aligned") &&
      identical(univar_likelihood_mode, "al")) {
    warning(
      paste(
        "models.exdqlm_univar.implementation_mode=theory_aligned with likelihood_mode=al",
        "is experimental and is not the accepted comparison workflow.",
        "Prefer legacy_bridge for dqlm_univar_al_synth."
      ),
      call. = FALSE
    )
  }
  if (isTRUE(cfg$models$run_ndlm_main) && identical(ndlm_impl_mode, "legacy_bridge")) {
    warning(
      "models.ndlm_main.implementation_mode=legacy_bridge is supported but deprecated; prefer theory_aligned.",
      call. = FALSE
    )
  }
  if (isTRUE(cfg$models$run_ndlm_univar) &&
      !identical(ndlm_univar_impl_mode, "theory_aligned_closed_form") &&
      !identical(ndlm_univar_impl_mode, "theory_aligned")) {
    stop(
      sprintf(
        "unsupported models.ndlm_univar.implementation_mode=%s; expected theory_aligned_closed_form/theory_aligned",
        as.character(ndlm_univar_impl_mode)
      ),
      call. = FALSE
    )
  }
  contract_checks_enabled <- isTRUE(unified_get(cfg, c("fit", "contract_checks", "enabled"), default = FALSE))
  contract_checks_fail_fast <- isTRUE(unified_get(cfg, c("fit", "contract_checks", "fail_fast"), default = TRUE))
  contract_checks_write_reports <- isTRUE(unified_get(cfg, c("fit", "contract_checks", "write_reports"), default = TRUE))
  diagnostics_enabled <- isTRUE(unified_get(cfg, c("fit", "diagnostics", "enabled"), default = FALSE))
  diagnostics_fail_fast <- isTRUE(unified_get(cfg, c("fit", "diagnostics", "fail_fast"), default = TRUE))
  diagnostics_write_reports <- isTRUE(unified_get(cfg, c("fit", "diagnostics", "write_reports"), default = TRUE))
  diagnostics_settings <- list(
    max_time_checks = as.integer(unified_get(cfg, c("fit", "diagnostics", "max_time_checks"), default = 25L)),
    seed = as.integer(unified_get(cfg, c("fit", "diagnostics", "seed"), default = cfg$run$seed)),
    psd_tol = as.numeric(unified_get(cfg, c("fit", "diagnostics", "psd_tol"), default = -1e-10)),
    full_slice_psd = isTRUE(unified_get(cfg, c("fit", "diagnostics", "full_slice_psd"), default = FALSE)),
    psd_warn_tol = as.numeric(unified_get(
      cfg,
      c("fit", "diagnostics", "psd_warn_tol"),
      default = unified_get(cfg, c("fit", "diagnostics", "psd_tol"), default = -1e-10)
    )),
    psd_fail_tol = as.numeric(unified_get(
      cfg,
      c("fit", "diagnostics", "psd_fail_tol"),
      default = unified_get(cfg, c("fit", "diagnostics", "psd_tol"), default = -1e-10)
    ))
  )
  multivar_forecast_health_enabled <- isTRUE(unified_get(
    cfg,
    c("fit", "exdqlm_multivar", "forecast_health", "enabled"),
    default = TRUE
  ))
  multivar_forecast_health_fail_fast <- isTRUE(unified_get(
    cfg,
    c("fit", "exdqlm_multivar", "forecast_health", "fail_fast"),
    default = TRUE
  ))
  multivar_forecast_health_write_reports <- isTRUE(unified_get(
    cfg,
    c("fit", "exdqlm_multivar", "forecast_health", "write_reports"),
    default = TRUE
  ))
  multivar_forecast_health_limits <- list(
    latent = as.numeric(unified_get(
      cfg,
      c("fit", "exdqlm_multivar", "forecast_health", "latent_limit"),
      default = 650
    )),
    sigma = as.numeric(unified_get(
      cfg,
      c("fit", "exdqlm_multivar", "forecast_health", "sigma_limit"),
      default = 100
    )),
    state = as.numeric(unified_get(
      cfg,
      c("fit", "exdqlm_multivar", "forecast_health", "state_limit"),
      default = 1000
    )),
    history_latent = as.numeric(unified_get(
      cfg,
      c("fit", "exdqlm_multivar", "forecast_health", "history_latent_limit"),
      default = 25
    )),
    state_norm_sq_per_T = as.numeric(unified_get(
      cfg,
      c("fit", "exdqlm_multivar", "forecast_health", "state_norm_sq_per_T_limit"),
      default = 1e4
    )),
    transfer_level = as.numeric(unified_get(
      cfg,
      c("fit", "exdqlm_multivar", "forecast_health", "transfer_level_limit"),
      default = 25
    )),
    transfer_coef = as.numeric(unified_get(
      cfg,
      c("fit", "exdqlm_multivar", "forecast_health", "transfer_coef_limit"),
      default = 100
    ))
  )

  add_report_artifacts <- function(manifest, report_paths, role) {
    report_paths <- unlist(report_paths, use.names = FALSE)
    report_paths <- report_paths[nzchar(report_paths)]
    if (length(report_paths) > 0L) {
      for (rp in report_paths) {
        if (file.exists(rp)) {
          manifest <- unified_manifest_add_artifact(
            manifest,
            rp,
            storage_scale = "text",
            role = role
          )
        }
      }
    }
    manifest
  }

  run_preflight_check <- function(path, check_point, context, stage_label) {
    if (!isTRUE(io_settings$enabled)) {
      return(list(status = "disabled", report_path = NULL))
    }
    unified_run_io_preflight(
      path = path,
      io_settings = io_settings,
      check_point = check_point,
      context = context,
      report_dir = preflight_dir,
      stage_label = stage_label,
      log_path = fit_preflight_log
    )
  }

  add_preflight_artifact <- function(manifest, preflight_result) {
    rp <- preflight_result$report_path
    if (!is.null(rp) && nzchar(rp) && file.exists(rp)) {
      manifest <- unified_manifest_add_artifact(
        manifest,
        rp,
        storage_scale = "text",
        role = "preflight"
      )
    }
    manifest
  }

  fit_preflight <- run_preflight_check(
    path = fit_root,
    check_point = "fit_start",
    context = "stage_fit preflight",
    stage_label = "fit_start"
  )
  manifest <- add_preflight_artifact(manifest, fit_preflight)

  fit_parallel_mode <- unified_resolve_fit_parallel_mode(cfg)
  run_exdqlm_multivar <- isTRUE(cfg$models$run_exdqlm_multivar)
  run_exdqlm_univar <- isTRUE(cfg$models$run_exdqlm_univar)
  run_ndlm_main <- isTRUE(cfg$models$run_ndlm_main)
  run_ndlm_univar <- isTRUE(cfg$models$run_ndlm_univar)
  if (run_ndlm_main || run_ndlm_univar) {
    ndlm_history_report <- file.path(preflight_dir, "ndlm_retros_history_check.txt")
    ndlm_history <- tryCatch(
      unified_assert_ndlm_retros_history(
        retros_path = source_retros,
        min_required = 30L,
        report_path = ndlm_history_report
      ),
      error = function(e) {
        msg <- conditionMessage(e)
        append_fit_preflight_log(msg)
        append_fit_stage_log(msg)
        stop(msg, call. = FALSE)
      }
    )
    ndlm_history_msg <- sprintf(
      "ndlm retros preflight pass retros_path=%s history_column=%s finite_count=%d minimum_required=%d",
      ndlm_history$retros_path,
      ndlm_history$history_column,
      ndlm_history$finite_count,
      30L
    )
    append_fit_preflight_log(ndlm_history_msg)
    append_fit_stage_log(ndlm_history_msg)
    if (file.exists(ndlm_history_report)) {
      manifest <- unified_manifest_add_artifact(
        manifest,
        ndlm_history_report,
        storage_scale = "text",
        role = "preflight"
      )
    }
  }
  multivar_transfer_modes <- unified_resolve_multivar_transfer_modes(cfg)
  primary_multivar_transfer_mode <- unified_resolve_multivar_primary_transfer_mode(
    cfg,
    modes = multivar_transfer_modes
  )
  multivar_dual_mode <- isTRUE(run_exdqlm_multivar) && length(multivar_transfer_modes) > 1L

  default_workers <- suppressWarnings(as.integer(cfg$run$threads$mc_cores))
  if (!is.finite(default_workers) || default_workers < 1L) {
    default_workers <- 1L
  }

  run_one_quantile <- function(q, transfer_mode = primary_multivar_transfer_mode) {
    q_num <- as.integer(round(q * 100))
    q_label <- sprintf("%02d", q_num)
    forecast_transfer_mode <- tolower(trimws(as.character(transfer_mode)))
    if (!nzchar(forecast_transfer_mode) || !(forecast_transfer_mode %in% c("drop", "keep"))) {
      forecast_transfer_mode <- primary_multivar_transfer_mode
    }
    is_primary_multivar_mode <- identical(forecast_transfer_mode, primary_multivar_transfer_mode)
    # Reserve the legacy root fit/q=* layout for drop-mode outputs only. A keep-only
    # rerun must still write into fit/exdqlm_multivar/keep/... so it cannot clobber
    # the persisted drop artifacts that post expects at the legacy path.
    use_legacy_multivar_layout <- identical(forecast_transfer_mode, "drop") &&
      (!multivar_dual_mode || is_primary_multivar_mode)
    if (use_legacy_multivar_layout) {
      q_root <- file.path(fit_root, sprintf("q=%s", q_label))
    } else {
      q_root <- file.path(fit_root, "exdqlm_multivar", forecast_transfer_mode, sprintf("q=%s", q_label))
    }
    q_outputs <- file.path(q_root, "outputs")
    q_logs <- file.path(q_root, "logs")
    dir.create(q_outputs, recursive = TRUE, showWarnings = FALSE)
    dir.create(q_logs, recursive = TRUE, showWarnings = FALSE)
    if (isTRUE(io_settings$enabled)) {
      run_preflight_check(
        path = q_outputs,
        check_point = "continue",
        context = sprintf("stage_fit multivar mode=%s quantile q=%s", forecast_transfer_mode, q_label),
        stage_label = sprintf("fit_multivar_%s_q%s", forecast_transfer_mode, q_label)
      )
    }

    cutoff_raw <- as.character(unified_get(
      cfg,
      c("dates", "cutoff_date"),
      default = "2022-12-25"
    ))
    if (!length(cutoff_raw) || is.na(cutoff_raw[[1]]) || !nzchar(cutoff_raw[[1]])) {
      cutoff_raw <- "2022-12-25"
    } else {
      cutoff_raw <- cutoff_raw[[1]]
    }
    cutoff_date <- suppressWarnings(as.Date(cutoff_raw))
    if (is.na(cutoff_date)) cutoff_date <- as.Date("2022-12-25")
    forecast_start_date <- cutoff_date + 1
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
  exdqlm_transfer_feature_mode <- unified_resolve_transfer_feature_mode(cfg)
  exdqlm_transfer_feature_scaling <- unified_resolve_transfer_feature_scaling(cfg)
    output_suffix <- unified_resolve_exdqlm_multivar_legacy_output_suffix(cfg, default = "DISC")
    expected_output_path <- file.path(
      q_outputs,
      sprintf("DISC_variables_%d_exAL_synth_%s.RData", q_num, output_suffix)
    )

    warm_start_enabled <- isTRUE(unified_get(cfg, c("fit", "warm_start", "enabled"), default = FALSE))
    warm_start_source_run_root <- unified_get(cfg, c("fit", "warm_start", "source_run_root"), default = NULL)
    warm_start_source_run_id <- unified_get(cfg, c("fit", "warm_start", "source_run_id"), default = NULL)
    if (!is.null(warm_start_source_run_root)) {
      warm_start_source_run_root <- as.character(warm_start_source_run_root[[1L]])
      if (!nzchar(warm_start_source_run_root)) warm_start_source_run_root <- NULL
    }
    if (!is.null(warm_start_source_run_id)) {
      warm_start_source_run_id <- as.character(warm_start_source_run_id[[1L]])
      if (!nzchar(warm_start_source_run_id)) warm_start_source_run_id <- NULL
    }
    warm_start_rdata_path <- ""
    if (warm_start_enabled) {
      warm_start_source_dir <- if (!is.null(warm_start_source_run_id)) {
        unified_resolve_source_run_dir(
          source_run_root = warm_start_source_run_root,
          source_run_id = warm_start_source_run_id,
          fallback_run_root = warm_start_source_run_root
        )
      } else if (!is.null(warm_start_source_run_root)) {
        normalizePath(path.expand(warm_start_source_run_root), mustWork = FALSE)
      } else {
        NULL
      }
      if (is.null(warm_start_source_dir) || !dir.exists(warm_start_source_dir)) {
        stop(sprintf(
          "Warm start enabled for q=%s but source run directory is missing: %s",
          q_label,
          if (is.null(warm_start_source_dir)) "<null>" else warm_start_source_dir
        ), call. = FALSE)
      }
      warm_start_file_name <- sprintf("DISC_variables_%d_exAL_synth_%s.RData", q_num, output_suffix)
      warm_start_candidates <- c(
        file.path(
          warm_start_source_dir,
          "fit", "exdqlm_multivar", forecast_transfer_mode,
          sprintf("q=%s", q_label),
          "outputs",
          warm_start_file_name
        ),
        file.path(
          warm_start_source_dir,
          "fit",
          sprintf("q=%s", q_label),
          "outputs",
          warm_start_file_name
        )
      )
      warm_start_hits <- warm_start_candidates[file.exists(warm_start_candidates)]
      if (length(warm_start_hits) < 1L) {
        recursive_hits <- list.files(
          warm_start_source_dir,
          pattern = sprintf("^%s$", warm_start_file_name),
          recursive = TRUE,
          full.names = TRUE
        )
        if (length(recursive_hits) > 0L) {
          q_fragment <- sprintf("q=%s", q_label)
          recursive_hits <- recursive_hits[grepl(q_fragment, recursive_hits, fixed = TRUE)]
        }
        warm_start_hits <- unique(c(warm_start_hits, recursive_hits[file.exists(recursive_hits)]))
      }
      warm_start_hits <- unique(normalizePath(warm_start_hits, mustWork = FALSE))
      if (length(warm_start_hits) != 1L) {
        stop(sprintf(
          "Warm start enabled for q=%s but expected exactly one seed RData; found %d under %s",
          q_label,
          length(warm_start_hits),
          warm_start_source_dir
        ), call. = FALSE)
      }
      warm_start_rdata_path <- warm_start_hits[[1L]]
      message(sprintf("exdqlm_multivar warm start q=%s <- %s", q_label, warm_start_rdata_path))
    }

    gamsig_policy <- unified_resolve_gamma_sigma_policy(cfg, "exdqlm_multivar", q = q)

    gamsig_freeze_iters <- as.character(unified_get(
      gamsig_policy, c("warmup_freeze_iters"), default = 5L
    ))
    gamsig_min_update_iters <- as.character(unified_get(
      gamsig_policy, c("min_update_iters"), default = 50L
    ))
    gamsig_min_total_iters <- as.character(unified_get(
      gamsig_policy, c("min_total_iters"), default = 50L
    ))
    gamsig_max_iter <- as.character(unified_get(
      gamsig_policy, c("max_iter"), default = 100L
    ))

    latent_ablation_policy <- unified_get(
      cfg, c("fit", "exdqlm_multivar", "latent_ablation"), default = list()
    )
    if (!is.list(latent_ablation_policy)) latent_ablation_policy <- list()
    pseudodata_guard_policy <- unified_get(
      cfg, c("fit", "exdqlm_multivar", "pseudodata_guard"), default = list()
    )
    if (!is.list(pseudodata_guard_policy)) pseudodata_guard_policy <- list()
    latent_diagnostics_policy <- unified_get(
      cfg, c("fit", "exdqlm_multivar", "diagnostics", "latent"), default = list()
    )
    if (!is.list(latent_diagnostics_policy)) latent_diagnostics_policy <- list()
    pseudodata_guard_report_dir <- as.character(unified_get(
      pseudodata_guard_policy, c("report_dir"), default = ""
    ))
    if (!length(pseudodata_guard_report_dir) || is.na(pseudodata_guard_report_dir[[1L]])) {
      pseudodata_guard_report_dir <- ""
    } else {
      pseudodata_guard_report_dir <- pseudodata_guard_report_dir[[1L]]
    }
    if (!nzchar(pseudodata_guard_report_dir)) {
      pseudodata_guard_report_dir <- file.path(q_logs, "pseudodata_guard")
    }
    latent_diagnostics_report_dir <- as.character(unified_get(
      latent_diagnostics_policy, c("report_dir"), default = ""
    ))
    if (!length(latent_diagnostics_report_dir) || is.na(latent_diagnostics_report_dir[[1L]])) {
      latent_diagnostics_report_dir <- ""
    } else {
      latent_diagnostics_report_dir <- latent_diagnostics_report_dir[[1L]]
    }
    if (!nzchar(latent_diagnostics_report_dir)) {
      latent_diagnostics_report_dir <- file.path(q_outputs, "diagnostics", "vb_iteration")
    }

    transfer_compare_fast_enabled <- isTRUE(unified_get(
      gamsig_policy,
      c("transfer_compare_fast", "enabled"),
      default = FALSE
    ))
    if (transfer_compare_fast_enabled) {
      gamsig_freeze_iters <- as.character(unified_get(
        gamsig_policy, c("transfer_compare_fast", "warmup_freeze_iters"), default = 5L
      ))
      gamsig_min_update_iters <- as.character(unified_get(
        gamsig_policy, c("transfer_compare_fast", "min_update_iters"), default = 15L
      ))
      gamsig_min_total_iters <- as.character(unified_get(
        gamsig_policy, c("transfer_compare_fast", "min_total_iters"), default = 20L
      ))
      gamsig_max_iter <- as.character(unified_get(
        gamsig_policy, c("transfer_compare_fast", "max_iter"), default = 20L
      ))
      message(
        sprintf(
          "exdqlm_multivar transfer_compare_fast enabled for q=%s (freeze=%s, min_update=%s, min_total=%s, max_iter=%s)",
          q_label,
          gamsig_freeze_iters,
          gamsig_min_update_iters,
          gamsig_min_total_iters,
          gamsig_max_iter
        )
      )
    }

    env_overrides <- c(
      DISC_BASE_SEED = as.character(cfg$run$seed),
      UNIFIED_LEGACY_FIT_INPUT_SCALE = as.character(cfg$scale_contract$legacy_fit_input_scale),
      UNIFIED_ANALYSIS_SCALE_FIT_INTERNAL = as.character(cfg$scale_contract$analysis_scale_fit_internal),
      UNIFIED_TRANSFORM_POLICY = as.character(unified_get(
        cfg,
        c("scale_contract", "transform_policy"),
        default = ""
      )),
      DISC_USE_PREV = if (isTRUE(cfg$fit$warm_start$enabled)) "TRUE" else "FALSE",
      DISC_PREV_RDATA = warm_start_rdata_path,
      DISC_W_LIKELIHOOD_MODE = as.character(multivar_likelihood_mode),
      DISC_W_FORECAST_TRANSFER_MODE = forecast_transfer_mode,
      DISC_W_INCLUDE_TREND = exdqlm_structure_include_trend,
      DISC_W_ENABLED_HARMONIC_INDICES = exdqlm_structure_harmonics,
      DISC_W_TRANSFER_FEATURE_COLUMNS = exdqlm_transfer_feature_columns,
      UNIFIED_TRANSFER_FEATURE_COLUMNS = exdqlm_transfer_feature_columns,
      DISC_W_TRANSFER_FEATURE_MODE = exdqlm_transfer_feature_mode,
      UNIFIED_TRANSFER_FEATURE_MODE = exdqlm_transfer_feature_mode,
      DISC_W_TRANSFER_FEATURE_SCALING = exdqlm_transfer_feature_scaling,
      UNIFIED_TRANSFER_FEATURE_SCALING = exdqlm_transfer_feature_scaling,
      DISC_W_CUTOFF_DATE = as.character(cutoff_date),
      DISC_W_FORECAST_START_DATE = as.character(forecast_start_date),
      DISC_W_OUTPUT_DIR = q_outputs,
      DISC_W_EXPECTED_RDATA_PATH = expected_output_path,
      DISC_W_PARAMETERS_PATH = parameters_copy,
      DISC_W_RETROS_PATH = adapted_retros,
      DISC_W_NWS_PATH = adapted_nws,
      DISC_W_GLOFAS_PATH = adapted_glofas,
      DISC_W_DF_T = as.character(unified_get(
        cfg, c("models", "exdqlm_multivar", "state_evolution", "df_t"), default = 0.9999995
      )),
      DISC_W_DF_S1 = as.character(unified_get(
        cfg, c("models", "exdqlm_multivar", "state_evolution", "df_s1"), default = 0.9997
      )),
      DISC_W_DF_S2 = as.character(unified_get(
        cfg, c("models", "exdqlm_multivar", "state_evolution", "df_s2"), default = 0.9997
      )),
      DISC_W_DF_S67 = as.character(unified_get(
        cfg, c("models", "exdqlm_multivar", "state_evolution", "df_s67"), default = 0.9997
      )),
      DISC_W_DF_DISCREP = as.character(unified_get(
        cfg, c("models", "exdqlm_multivar", "state_evolution", "df_discrep"), default = 0.999
      )),
      DISC_W_LAMBDA = as.character(unified_get(
        cfg, c("models", "exdqlm_multivar", "state_evolution", "lambda"), default = 0.8995
      )),
      DISC_W_DF_TRANS = as.character(unified_get(
        cfg, c("models", "exdqlm_multivar", "state_evolution", "df_trans"), default = 0.99999999
      )),
      DISC_W_DF_COVS = as.character(unified_get(
        cfg, c("models", "exdqlm_multivar", "state_evolution", "df_covs"), default = 0.99999
      )),
      DISC_W_LAM1 = as.character(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "lam1"), default = 1 - 1e-6
      )),
      DISC_W_LAM2 = as.character(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "lam2"), default = 1 - 1e-6
      )),
      DISC_W_N_SAMP = as.character(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "n_samp"), default = 2000L
      )),
      DISC_W_SAMPLING_HEARTBEAT_ENABLED = if (isTRUE(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "sampling_diagnostics", "heartbeat_enabled"), default = FALSE
      ))) "TRUE" else "FALSE",
      DISC_W_SAMPLING_HEARTBEAT_SECONDS = as.character(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "sampling_diagnostics", "heartbeat_seconds"), default = 60L
      )),
      DISC_W_SAMPLING_PHASE_MARKERS_ENABLED = if (isTRUE(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "sampling_diagnostics", "phase_markers_enabled"), default = FALSE
      ))) "TRUE" else "FALSE",
      DISC_W_SAMPLING_WALLTIME_SECONDS = as.character(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "sampling_diagnostics", "walltime_seconds"), default = 0L
      )),
      DISC_W_SAMPLING_MEMBER_WALLTIME_SECONDS = as.character(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "sampling_diagnostics", "member_walltime_seconds"), default = 0L
      )),
      DISC_W_SAMPLING_DIAG_PATH = file.path(q_logs, "sampling_diagnostics.log"),
      DISC_W_SAMPLING_DIAG_STDERR_ENABLED = "TRUE",
      DISC_W_SIMS_ENABLED = if (isTRUE(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "sims_enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_W_USE_COVARIATES = if (isTRUE(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "use_covariates"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_W_POST_SAVE_OBJECTIVE_ENABLED = if (isTRUE(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "post_save_objective_enabled"), default = FALSE
      ))) "TRUE" else "FALSE",
      DISC_W_POST_SAVE_JSD_ENABLED = if (isTRUE(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "post_save_jsd_enabled"), default = FALSE
      ))) "TRUE" else "FALSE",
      DISC_W_POST_SAVE_JSD_GRIDSIZE = as.character(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "post_save_jsd_gridsize"), default = 100L
      )),
      DISC_W_TRANSFER_DESIGN_DIAG_DIR = file.path(q_outputs, "diagnostics", "transfer_design"),
      DISC_W_C_FACTOR = as.character(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "forecast_cov", "c_factor"), default = 1e2
      )),
      DISC_W_FORECAST_COV_EPSILON = as.character(unified_get(
        cfg, c("fit", "exdqlm_multivar", "legacy", "forecast_cov", "epsilon"), default = NA_real_
      )),
      DISC_LATENT_ABLATION_MODE = as.character(unified_get(
        latent_ablation_policy, c("mode"), default = "free"
      )),
      DISC_LATENT_E_INV_U_CAP = as.character(unified_get(
        latent_ablation_policy, c("e_inv_u_cap"), default = 5000
      )),
      DISC_LATENT_E_U_CAP = as.character(unified_get(
        latent_ablation_policy, c("e_u_cap"), default = 1e6
      )),
      DISC_W_LATENT_DIAG_ENABLED = if (isTRUE(unified_get(
        latent_diagnostics_policy, c("enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_W_LATENT_DIAG_REPORT_DIR = latent_diagnostics_report_dir,
      DISC_W_LATENT_DIAG_TOP_K = as.character(unified_get(
        latent_diagnostics_policy, c("top_k"), default = 20L
      )),
      DISC_W_LATENT_DIAG_WRITE_ITERATION_SUMMARY = if (isTRUE(unified_get(
        latent_diagnostics_policy, c("write_iteration_summary"), default = FALSE
      ))) "TRUE" else "FALSE",
      DISC_W_LATENT_DIAG_WRITE_HEALTH_SUMMARY = if (isTRUE(unified_get(
        latent_diagnostics_policy, c("write_health_summary"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_W_LATENT_DIAG_WRITE_TOP_CELLS = if (isTRUE(unified_get(
        latent_diagnostics_policy, c("write_top_cells"), default = FALSE
      ))) "TRUE" else "FALSE",
      DISC_PSEUDODATA_GUARD_ENABLED = if (isTRUE(unified_get(
        pseudodata_guard_policy, c("enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_PSEUDODATA_GUARD_MODE = as.character(unified_get(
        pseudodata_guard_policy, c("mode"), default = "fail"
      )),
      DISC_PSEUDODATA_GUARD_REPORT_DIR = pseudodata_guard_report_dir,
      DISC_PSEUDODATA_FFF_ABS_CAP = as.character(unified_get(
        pseudodata_guard_policy, c("caps", "fff_abs_cap"), default = 1000
      )),
      DISC_PSEUDODATA_QQQ_DIAG_ABS_CAP = as.character(unified_get(
        pseudodata_guard_policy, c("caps", "qqq_diag_abs_cap"), default = 10000
      )),
      DISC_PSEUDODATA_E_S_ABS_CAP = as.character(unified_get(
        pseudodata_guard_policy, c("caps", "e_s_abs_cap"), default = 1000
      )),
      DISC_PSEUDODATA_E_S2_ABS_CAP = as.character(unified_get(
        pseudodata_guard_policy, c("caps", "e_s2_abs_cap"), default = 1e6
      )),
      DISC_PSEUDODATA_E_U_ABS_CAP = as.character(unified_get(
        pseudodata_guard_policy, c("caps", "e_u_abs_cap"), default = 1e6
      )),
      DISC_PSEUDODATA_E_INV_U_ABS_CAP = as.character(unified_get(
        pseudodata_guard_policy, c("caps", "e_inv_u_abs_cap"), default = 5000
      )),
      DISC_PSEUDODATA_E_INV_U_FLOOR = as.character(unified_get(
        pseudodata_guard_policy, c("caps", "e_inv_u_floor"), default = 1e-9
      )),
      DISC_PSEUDODATA_E_INV_U_FLOOR_FRAC_CAP = as.character(unified_get(
        pseudodata_guard_policy, c("caps", "e_inv_u_floor_frac_cap"), default = 0.25
      )),
      DISC_GAMSIG_FREEZE_ITERS = gamsig_freeze_iters,
      DISC_GAMSIG_MIN_UPDATE_ITERS = gamsig_min_update_iters,
      DISC_GAMSIG_MIN_TOTAL_ITERS = gamsig_min_total_iters,
      DISC_GAMSIG_MAX_ITER = gamsig_max_iter,
      DISC_GAMSIG_CONVERGENCE_TOL = as.character(unified_get(
        gamsig_policy, c("convergence_tol"), default = 1e-6
      )),
      DISC_GAMSIG_ELBO_TOL = as.character(unified_get(
        gamsig_policy, c("convergence", "elbo_tol"), default = 1e-6
      )),
      DISC_GAMSIG_STATE_NORM_TOL = as.character(unified_get(
        gamsig_policy, c("convergence", "state_norm_sq_tol"), default = 1e-6
      )),
      DISC_GAMSIG_SIGMA_EXP_TOL = as.character(unified_get(
        gamsig_policy, c("convergence", "sigma_exp_tol"), default = 1e-6
      )),
      DISC_GAMSIG_GAMMA_EXP_TOL = as.character(unified_get(
        gamsig_policy, c("convergence", "gamma_exp_tol"), default = 1e-6
      )),
      DISC_GAMSIG_FREEZE_TARGET = as.character(unified_get(
        gamsig_policy, c("freeze_target"), default = "gamma_sigma"
      )),
      DISC_GAMSIG_STATE_REFRESH_SCHEDULE_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("state_refresh_schedule", "enabled"), default = FALSE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_STATE_REFRESH_SCHEDULE_START_ITER = as.character(unified_get(
        gamsig_policy, c("state_refresh_schedule", "start_iter"), default = 11L
      )),
      DISC_GAMSIG_STATE_REFRESH_SCHEDULE_END_ITER = as.character(unified_get(
        gamsig_policy, c("state_refresh_schedule", "end_iter"), default = 200L
      )),
      DISC_GAMSIG_STATE_REFRESH_SCHEDULE_HOLD_ITERS = as.character(unified_get(
        gamsig_policy, c("state_refresh_schedule", "hold_iters"), default = 10L
      )),
      DISC_GAMSIG_STATE_REFRESH_SCHEDULE_REFRESH_ITERS = as.character(unified_get(
        gamsig_policy, c("state_refresh_schedule", "refresh_iters"), default = 1L
      )),
      DISC_GAMSIG_GUARD_REFREEZE_ITERS = as.character(unified_get(
        gamsig_policy, c("guard_refreeze_iters"), default = 10L
      )),
      DISC_GAMSIG_INIT_MODE = as.character(unified_get(
        gamsig_policy, c("init", "mode"), default = "robust"
      )),
      DISC_GAMSIG_INIT_GAMMA = as.character(unified_get(
        gamsig_policy, c("init", "gamma"), default = 0.0
      )),
      DISC_GAMSIG_INIT_SIGMA_FLOOR = as.character(unified_get(
        gamsig_policy, c("init", "sigma_floor"), default = 1e-3
      )),
      DISC_GAMSIG_INIT_SIGMA_SCALE = as.character(unified_get(
        gamsig_policy, c("init", "sigma_scale"), default = 1.0
      )),
      DISC_GAMSIG_PRIOR_SIGMA_MEAN = as.character(unified_get(
        gamsig_policy, c("priors", "sigma", "mean"), default = 1.0
      )),
      DISC_GAMSIG_PRIOR_SIGMA_VARIANCE = as.character(unified_get(
        gamsig_policy, c("priors", "sigma", "variance"), default = 1e10
      )),
      DISC_GAMSIG_PRIOR_GAMMA_LOCATION = as.character(unified_get(
        gamsig_policy, c("priors", "gamma", "location"), default = 0.0
      )),
      DISC_GAMSIG_PRIOR_GAMMA_SCALE = as.character(unified_get(
        gamsig_policy, c("priors", "gamma", "scale"), default = 1e10
      )),
      DISC_GAMSIG_PRIOR_GAMMA_DF = as.character(unified_get(
        gamsig_policy, c("priors", "gamma", "df"), default = 1.0
      )),
      DISC_GAMSIG_OBJECTIVE_GUARD_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("objective_guard", "enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_OBJECTIVE_GUARD_FAIL_FAST = if (isTRUE(unified_get(
        gamsig_policy, c("objective_guard", "fail_fast"), default = FALSE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_OBJECTIVE_GUARD_LOG_FAILURES = if (isTRUE(unified_get(
        gamsig_policy, c("objective_guard", "log_failures"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_OBJECTIVE_GUARD_MODE = as.character(unified_get(
        gamsig_policy, c("objective_guard", "mode"), default = "adaptive_freeze"
      )),
      DISC_GAMSIG_OBJECTIVE_GUARD_PENALTY = as.character(unified_get(
        gamsig_policy, c("objective_guard", "penalty"), default = 1e12
      )),
      DISC_GAMSIG_LAPLACE_SPLIT_NEAR_ZERO_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("laplace_split_near_zero", "enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_LAPLACE_SPLIT_NEAR_ZERO_ABS_GAMMA = as.character(unified_get(
        gamsig_policy, c("laplace_split_near_zero", "abs_gamma_threshold"), default = 0.05
      )),
      DISC_GAMSIG_LAPLACE_SPLIT_NEAR_ZERO_REL_SUPPORT = as.character(unified_get(
        gamsig_policy, c("laplace_split_near_zero", "rel_support_threshold"), default = 0.02
      )),
      DISC_GAMSIG_LAPLACE_SPLIT_ZERO_MARGIN_ABS_GAMMA = as.character(unified_get(
        gamsig_policy, c("laplace_split_near_zero", "zero_margin_abs_gamma"), default = 1e-6
      )),
      DISC_GAMSIG_LAPLACE_SPLIT_ON_GUARD = if (isTRUE(unified_get(
        gamsig_policy, c("laplace_split_near_zero", "split_on_guard"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_NEAR_ZERO_FALLBACK_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("near_zero_fallback", "enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_NEAR_ZERO_FALLBACK_MODE = as.character(unified_get(
        gamsig_policy, c("near_zero_fallback", "mode"), default = "sigma_only"
      )),
      DISC_GAMSIG_NEAR_ZERO_GAMMA_ANCHOR = as.character(unified_get(
        gamsig_policy, c("near_zero_fallback", "gamma_anchor"), default = "full_candidate"
      )),
      DISC_GAMSIG_COHERENCE_GUARD_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("coherence_guard", "enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_COHERENCE_ROLLBACK_ON_GUARD = if (isTRUE(unified_get(
        gamsig_policy, c("coherence_guard", "rollback_on_guard"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_COHERENCE_MIN_UTS_PSI = as.character(unified_get(
        gamsig_policy, c("coherence_guard", "min_uts_psi"), default = 1e-8
      )),
      DISC_GAMSIG_COHERENCE_NONNEGATIVE_TOL = as.character(unified_get(
        gamsig_policy, c("coherence_guard", "nonnegative_tol"), default = 1e-10
      )),
      DISC_GAMSIG_TERMINAL_SAMPLING_GUARD_MODE = as.character(unified_get(
        gamsig_policy, c("terminal_sampling_guard", "mode"), default = "off"
      )),
      DISC_GAMSIG_TERMINAL_SAMPLING_GUARD_MIN_GUARD_COUNT = as.character(unified_get(
        gamsig_policy, c("terminal_sampling_guard", "min_guard_count"), default = 1L
      )),
      DISC_GAMSIG_TERMINAL_SAMPLING_GUARD_MAX_GUARD_LAG_ITERS = as.character(unified_get(
        gamsig_policy, c("terminal_sampling_guard", "max_guard_lag_iters"), default = 0L
      )),
      DISC_GAMSIG_TERMINAL_SAMPLING_GUARD_REQUIRE_FROZEN = if (isTRUE(unified_get(
        gamsig_policy, c("terminal_sampling_guard", "require_frozen"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_THETA_SIGMA_LOWER = as.character(unified_get(
        gamsig_policy, c("stabilization", "theta_sigma_lower"), default = log(1e-4)
      )),
      DISC_GAMSIG_THETA_SIGMA_UPPER = as.character(unified_get(
        gamsig_policy, c("stabilization", "theta_sigma_upper"), default = log(1e3)
      )),
      DISC_GAMSIG_THETA_GAMMA_LOWER = as.character(unified_get(
        gamsig_policy, c("stabilization", "theta_gamma_lower"), default = qlogis(1e-6)
      )),
      DISC_GAMSIG_THETA_GAMMA_UPPER = as.character(unified_get(
        gamsig_policy, c("stabilization", "theta_gamma_upper"), default = qlogis(1 - 1e-6)
      )),
      DISC_GAMSIG_HESSIAN_RIDGE_INIT = as.character(unified_get(
        gamsig_policy, c("stabilization", "hessian_ridge_init"), default = 1e-6
      )),
      DISC_GAMSIG_HESSIAN_RIDGE_MULTIPLIER = as.character(unified_get(
        gamsig_policy, c("stabilization", "hessian_ridge_multiplier"), default = 10
      )),
      DISC_GAMSIG_HESSIAN_RIDGE_MAX_TRIES = as.character(unified_get(
        gamsig_policy, c("stabilization", "hessian_ridge_max_tries"), default = 8L
      )),
      DISC_GAMSIG_MEDIAN_SIGMA_ONLY_FALLBACK_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("stabilization", "median_sigma_only_fallback_enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_MEDIAN_SIGMA_ONLY_FALLBACK_TOL = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_sigma_only_fallback_tol"), default = 1e-8
      )),
      DISC_GAMSIG_MEDIAN_STATE_GUARD_SIGMA_ONLY_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("stabilization", "median_state_guard_sigma_only_enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_MEDIAN_STATE_GUARD_SIGMA_ONLY_AFTER = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_state_guard_sigma_only_after"), default = 1L
      )),
      DISC_GAMSIG_MEDIAN_STATE_GUARD_SIGMA_ONLY_ANCHOR = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_state_guard_sigma_only_anchor"), default = "zero"
      )),
      DISC_GAMSIG_MEDIAN_STEP_DAMPING_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("stabilization", "median_step_damping_enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_MEDIAN_MAX_ABS_GAMMA_STEP = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_max_abs_gamma_step"), default = 0.25
      )),
      DISC_GAMSIG_MEDIAN_MAX_ABS_LOG_SIGMA_STEP = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_max_abs_log_sigma_step"), default = 0.5
      )),
      DISC_GAMSIG_STATE_GUARD_STEP_BACKOFF_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("stabilization", "state_guard_step_backoff_enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_STATE_GUARD_STEP_BACKOFF_FACTOR = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_guard_step_backoff_factor"), default = 0.2
      )),
      DISC_GAMSIG_STATE_GUARD_MIN_STEP_SCALE = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_guard_min_step_scale"), default = 0.005
      )),
      DISC_GAMSIG_STATE_HOLD_FREEZE_LATENTS_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("stabilization", "state_hold_freeze_latents_enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_STATE_GUARD_HOLD_STEP_SCALE_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("stabilization", "state_guard_hold_step_scale_enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_STATE_GUARD_MIN_REFREEZE_ITERS = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_guard_min_refreeze_iters"), default = 1L
      )),
      DISC_GAMSIG_STATE_GUARD_MIN_HOLD_ITERS = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_guard_min_hold_iters"), default = 1L
      )),
      DISC_GAMSIG_MEDIAN_STATE_GUARD_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("stabilization", "median_state_guard_enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      DISC_GAMSIG_MEDIAN_STATE_NORM_MAX_RATIO = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_state_norm_max_ratio"), default = 25
      )),
      DISC_GAMSIG_MEDIAN_STATE_NORM_ABS_CAP = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_state_norm_abs_cap"), default = 1e8
      )),
      DISC_GAMSIG_STATE_NORM_ABS_CAP_SCALE = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_norm_abs_cap_scale"), default = "per_time"
      )),
      if (!is.null(unified_get(
        gamsig_policy, c("stabilization", "state_norm_ratio_ref_floor"), default = NULL
      ))) c(DISC_GAMSIG_STATE_NORM_RATIO_REF_FLOOR = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_norm_ratio_ref_floor"), default = NULL
      ))) else character(0),
      DISC_GAMSIG_MEDIAN_STATE_GUARD_REFREEZE_ITERS = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_state_guard_refreeze_iters"), default = unified_get(
          gamsig_policy, c("guard_refreeze_iters"), default = 10L
        )
      )),
      DISC_GAMSIG_MEDIAN_STATE_HOLD_AFTER_GUARD_ITERS = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_state_hold_after_guard_iters"), default = 0L
      )),
      DISC_GAMSIG_MEDIAN_STATE_BLEND_ALPHA = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_state_blend_alpha"), default = 1.0
      )),
      DISC_GAMSIG_MEDIAN_COV_BLEND_ALPHA = as.character(unified_get(
        gamsig_policy, c("stabilization", "median_cov_blend_alpha"), default = 1.0
      )),
      if (!is.null(unified_get(
        gamsig_policy, c("stabilization", "state_guard_enabled"), default = NULL
      ))) c(DISC_GAMSIG_STATE_GUARD_ENABLED = if (isTRUE(unified_get(
        gamsig_policy, c("stabilization", "state_guard_enabled"), default = NULL
      ))) "TRUE" else "FALSE") else character(0),
      if (!is.null(unified_get(
        gamsig_policy, c("stabilization", "state_norm_max_ratio"), default = NULL
      ))) c(DISC_GAMSIG_STATE_NORM_MAX_RATIO = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_norm_max_ratio"), default = NULL
      ))) else character(0),
      if (!is.null(unified_get(
        gamsig_policy, c("stabilization", "state_norm_abs_cap"), default = NULL
      ))) c(DISC_GAMSIG_STATE_NORM_ABS_CAP = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_norm_abs_cap"), default = NULL
      ))) else character(0),
      if (!is.null(unified_get(
        gamsig_policy, c("stabilization", "state_guard_refreeze_iters"), default = NULL
      ))) c(DISC_GAMSIG_STATE_GUARD_REFREEZE_ITERS = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_guard_refreeze_iters"), default = NULL
      ))) else character(0),
      if (!is.null(unified_get(
        gamsig_policy, c("stabilization", "state_guard_start_iter"), default = NULL
      ))) c(DISC_GAMSIG_STATE_GUARD_START_ITER = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_guard_start_iter"), default = NULL
      ))) else character(0),
      if (!is.null(unified_get(
        gamsig_policy, c("stabilization", "state_hold_after_guard_iters"), default = NULL
      ))) c(DISC_GAMSIG_STATE_HOLD_AFTER_GUARD_ITERS = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_hold_after_guard_iters"), default = NULL
      ))) else character(0),
      if (!is.null(unified_get(
        gamsig_policy, c("stabilization", "state_blend_alpha"), default = NULL
      ))) c(DISC_GAMSIG_STATE_BLEND_ALPHA = as.character(unified_get(
        gamsig_policy, c("stabilization", "state_blend_alpha"), default = NULL
      ))) else character(0),
      if (!is.null(unified_get(
        gamsig_policy, c("stabilization", "cov_blend_alpha"), default = NULL
      ))) c(DISC_GAMSIG_COV_BLEND_ALPHA = as.character(unified_get(
        gamsig_policy, c("stabilization", "cov_blend_alpha"), default = NULL
      ))) else character(0)
    )
    cov_env_overrides <- c(
      if (using_engineered_covariates) c(UNIFIED_COVARIATE_FEATURES_CSV = shared_feature_csv) else character(0),
      if (nzchar(shared_cov_paths$eli)) c(DISC_W_COV1_PATH = shared_cov_paths$eli) else character(0),
      if (nzchar(shared_cov_paths$oni)) c(DISC_W_COV2_PATH = shared_cov_paths$oni) else character(0),
      if (nzchar(shared_cov_paths$ppt)) c(DISC_W_PRISM_PATH = shared_cov_paths$ppt) else character(0),
      if (nzchar(shared_cov_paths$soil)) c(DISC_W_SOIL_PATH = shared_cov_paths$soil) else character(0),
      if (nzchar(shared_cov_paths$pca)) c(DISC_W_PCA_PATH = shared_cov_paths$pca) else character(0)
    )
    if (length(cov_env_overrides) > 0L) {
      env_overrides <- c(env_overrides, cov_env_overrides)
    }
    env_kv <- sprintf("%s=%s", names(env_overrides), unname(env_overrides))

    log_path <- file.path(q_logs, "fit.log")
    cmd_status <- suppressWarnings(system2(
      "Rscript",
      c("--vanilla", file.path("scripts", "run_DISC_Optimal_Synth_Ranges_W.R"), as.character(q), as.character(cfg$run$seed)),
      stdout = log_path,
      stderr = log_path,
      env = env_kv
    ))
    if (!is.finite(cmd_status)) cmd_status <- 0L

    output_path <- expected_output_path
    forecast_health_path <- file.path(q_outputs, "multivar_forecast_health.txt")
    terminal_health_path <- file.path(q_outputs, "multivar_terminal_state_health.txt")
    terminal_health_csv_path <- file.path(q_outputs, "multivar_terminal_state_health.csv")
    forecast_health <- NULL
    if (!is.null(cmd_status) && is.finite(cmd_status) && as.integer(cmd_status) == 0L &&
        multivar_forecast_health_enabled) {
      report_target <- if (isTRUE(multivar_forecast_health_write_reports)) forecast_health_path else NULL
      terminal_report_target <- if (isTRUE(multivar_forecast_health_write_reports)) terminal_health_path else NULL
      terminal_csv_target <- if (isTRUE(multivar_forecast_health_write_reports)) terminal_health_csv_path else NULL
      forecast_health <- unified_multivar_fit_health_check(
        rdata_path = output_path,
        quantile = q,
        transfer_mode = forecast_transfer_mode,
        report_path = report_target,
        terminal_report_path = terminal_report_target,
        terminal_csv_path = terminal_csv_target,
        latent_limit = multivar_forecast_health_limits$latent,
        sigma_limit = multivar_forecast_health_limits$sigma,
        state_limit = multivar_forecast_health_limits$state,
        history_latent_limit = multivar_forecast_health_limits$history_latent,
        state_norm_sq_per_T_limit = multivar_forecast_health_limits$state_norm_sq_per_T,
        transfer_level_limit = multivar_forecast_health_limits$transfer_level,
        transfer_coef_limit = multivar_forecast_health_limits$transfer_coef
      )
      if (length(forecast_health$violations) > 0L) {
        hard_violations <- forecast_health$hard_violations
        if (is.null(hard_violations)) hard_violations <- character(0)
        warning_violations <- forecast_health$warning_violations
        if (is.null(warning_violations)) warning_violations <- character(0)
        health_tag <- if (length(hard_violations) > 0L || isTRUE(multivar_forecast_health_fail_fast)) {
          "FIT_FORECAST_HEALTH_FAIL"
        } else {
          "FIT_FORECAST_HEALTH_WARN"
        }
        msg <- sprintf(
          paste0(
            "[%s] multivar %s q=%s violated forecast-health limits: %s. ",
            "hard_violations=%s warnings=%s. ",
            "See %s"
          ),
          health_tag,
          forecast_transfer_mode,
          q_label,
          paste(forecast_health$violations, collapse = " | "),
          paste(hard_violations, collapse = "|"),
          paste(warning_violations, collapse = "|"),
          if (!is.null(forecast_health$report_path) && nzchar(forecast_health$report_path)) {
            forecast_health$report_path
          } else {
            output_path
          }
        )
        if (isTRUE(multivar_forecast_health_fail_fast)) {
          stop(msg, call. = FALSE)
        } else {
          warning(msg, call. = FALSE)
        }
      }
    }
    list(
      model_family = "exdqlm_multivar",
      transfer_mode = forecast_transfer_mode,
      quantile = q,
      output_path = output_path,
      log_path = log_path,
      forecast_health_path = if (file.exists(forecast_health_path)) forecast_health_path else "",
      terminal_health_path = if (file.exists(terminal_health_path)) terminal_health_path else "",
      terminal_health_csv_path = if (file.exists(terminal_health_csv_path)) terminal_health_csv_path else "",
      transfer_design_diag_dir = file.path(q_outputs, "diagnostics", "transfer_design"),
      status = as.integer(cmd_status)
    )
  }

  process_multivar_result <- function(manifest, res_raw) {
    res <- unified_normalize_fit_worker_result(res_raw, context_label = "fit stage parallel worker")
    if (!is.null(res$status) && res$status != 0) {
      stop(sprintf("fit stage failed for quantile %s; see %s", res$quantile, res$log_path), call. = FALSE)
    }
    file_size <- suppressWarnings(file.info(res$output_path)$size)
    if (!file.exists(res$output_path) || !is.finite(file_size) || file_size <= 0) {
      stop(
        sprintf("fit stage output missing or empty for quantile %s: %s", res$quantile, res$output_path),
        call. = FALSE
      )
    }
    unified_manifest_add_artifact(
      manifest,
      res$output_path,
      storage_scale = "model_state",
      flow_domain = cfg$scale_contract$analysis_scale_fit_internal
    )
    if (!is.null(res$forecast_health_path) &&
        nzchar(as.character(res$forecast_health_path)) &&
        file.exists(as.character(res$forecast_health_path))) {
      manifest <- unified_manifest_add_artifact(
        manifest,
        as.character(res$forecast_health_path),
        storage_scale = "text",
        role = "diagnostics"
      )
    }
    terminal_paths <- c(res$terminal_health_path, res$terminal_health_csv_path)
    terminal_paths <- terminal_paths[nzchar(as.character(terminal_paths))]
    for (terminal_path in terminal_paths) {
      if (!is.null(terminal_path) && file.exists(as.character(terminal_path))) {
        manifest <- unified_manifest_add_artifact(
          manifest,
          as.character(terminal_path),
          storage_scale = "text",
          role = "diagnostics"
        )
      }
    }
    transfer_diag_dir <- as.character(res$transfer_design_diag_dir)
    if (length(transfer_diag_dir) == 1L && nzchar(transfer_diag_dir) && dir.exists(transfer_diag_dir)) {
      transfer_diag_paths <- file.path(
        transfer_diag_dir,
        c("transfer_design_summary.csv", "transfer_design_condition.csv", "transfer_feature_metadata.csv")
      )
      for (transfer_diag_path in transfer_diag_paths[file.exists(transfer_diag_paths)]) {
        manifest <- unified_manifest_add_artifact(
          manifest,
          as.character(transfer_diag_path),
          storage_scale = "text",
          role = "diagnostics"
        )
      }
    }
    if (!is.null(res$log_path) && file.exists(as.character(res$log_path))) {
      manifest <- unified_manifest_add_artifact(
        manifest,
        as.character(res$log_path),
        storage_scale = "text"
      )
    }
    manifest
  }

  univar_script <- NULL
  if (run_exdqlm_univar) {
    if (!use_shared_inputs) {
      stop(
        "legacy univariate bridge requires run-scoped shared inputs. Enable stages.data_prep_shared and provide shared bundle inputs.",
        call. = FALSE
      )
    }
    required_cov_keys <- if (using_engineered_covariates) character(0) else c("eli", "oni", "ppt", "soil", "pca")
    missing_cov <- required_cov_keys[!nzchar(unlist(shared_cov_paths[required_cov_keys], use.names = FALSE))]
    if (length(missing_cov) > 0L) {
      stop(
        sprintf(
          "legacy univariate bridge missing shared covariates in run bundle: %s",
          paste(missing_cov, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    univar_script <- if (identical(univar_impl_mode, "theory_aligned")) {
      file.path(repo_root, "scripts", "run_exdqlm_univar.R")
    } else {
      file.path(repo_root, "scripts", "run_OptimalModelSLexAL.R")
    }
    if (!file.exists(univar_script)) {
      stop(
        sprintf("univariate script not found for implementation_mode=%s: %s", univar_impl_mode, univar_script),
        call. = FALSE
      )
    }

    if (isTRUE(io_settings$enabled)) {
      for (q in quantiles) {
        q_num <- as.integer(round(q * 100))
        q_lab <- sprintf("%02d", q_num)
        q_outputs <- file.path(fit_root, "exdqlm_univar", sprintf("q=%s", q_lab), "outputs")
        dir.create(q_outputs, recursive = TRUE, showWarnings = FALSE)
        preflight_univar <- run_preflight_check(
          path = q_outputs,
          check_point = "continue",
          context = sprintf("stage_fit univar q=%s", q_lab),
          stage_label = sprintf("fit_univar_q%s", q_lab)
        )
        manifest <- add_preflight_artifact(manifest, preflight_univar)
      }
    }
  }

  run_one_univar_quantile <- function(q) {
    q_num <- as.integer(round(q * 100))
    q_lab <- sprintf("%02d", q_num)
    q_root <- file.path(fit_root, "exdqlm_univar", sprintf("q=%s", q_lab))
    q_outputs <- file.path(q_root, "outputs")
    q_logs <- file.path(q_root, "logs")
    dir.create(q_outputs, recursive = TRUE, showWarnings = FALSE)
    dir.create(q_logs, recursive = TRUE, showWarnings = FALSE)
    univar_gamsig_policy <- unified_resolve_gamma_sigma_policy(cfg, "exdqlm_univar", q = q)

    output_path <- file.path(q_outputs, sprintf("variables_%s_exAL_synth_DISC_uni.RData", q_lab))
    log_name <- if (identical(univar_impl_mode, "theory_aligned")) {
      sprintf("univar_theory_%s.log", as.character(univar_likelihood_mode))
    } else {
      "univar_legacy.log"
    }
    log_path <- file.path(q_logs, log_name)

    env_overrides <- c(
      ENVIRONMETRICS_LIBS_ONLY = "1",
      UNIFIED_UNIV_RDATA_OUT = output_path,
      UNIV_RUN_ROOT = run_root_abs,
      UNIV_OUT_DIR = q_outputs,
      UNIV_SHARED_INPUT_ROOT = shared_paths$root,
      UNIV_USGS_DAILY_CSV = file.path(shared_paths$root, "usgs", "usgs_daily.csv"),
      UNIV_PARAMETERS_TXT = source_parameters,
      UNIV_RETROS_CSV = source_retros,
      UNIV_NWS_FORECAST_CSV = source_nws,
      UNIV_GLOFAS_FORECAST_CSV = source_glofas,
      UNIFIED_LEGACY_FIT_INPUT_SCALE = as.character(cfg$scale_contract$legacy_fit_input_scale),
      UNIFIED_ANALYSIS_SCALE_FIT_INTERNAL = as.character(cfg$scale_contract$analysis_scale_fit_internal),
      UNIFIED_TRANSFORM_POLICY = as.character(unified_get(
        cfg,
        c("scale_contract", "transform_policy"),
        default = ""
      )),
      UNIFIED_COVARIATE_FEATURES_CSV = shared_feature_csv,
      UNIV_COVARIATES_DIR = shared_paths$covariates_dir,
      UNIV_COV1_ELI_CSV = shared_cov_paths$eli,
      UNIV_COV2_ONI_CSV = shared_cov_paths$oni,
      UNIV_PPT_CSV = shared_cov_paths$ppt,
      UNIV_SOIL_CSV = shared_cov_paths$soil,
      UNIV_PCA_CSV = shared_cov_paths$pca,
      UNIV_USE_PREV = if (isTRUE(cfg$fit$warm_start$enabled)) "TRUE" else "FALSE",
      UNIV_PREV_RDATA = output_path,
      UNIV_LIKELIHOOD_MODE = as.character(univar_likelihood_mode),
      UNIV_GAMSIG_FREEZE_ITERS = as.character(unified_get(
        univar_gamsig_policy, c("warmup_freeze_iters"), default = 5L
      )),
      UNIV_GAMSIG_MIN_UPDATE_ITERS = as.character(unified_get(
        univar_gamsig_policy, c("min_update_iters"), default = 50L
      )),
      UNIV_GAMSIG_MIN_TOTAL_ITERS = as.character(unified_get(
        univar_gamsig_policy, c("min_total_iters"), default = 50L
      )),
      UNIV_GAMSIG_MAX_ITER = as.character(unified_get(
        univar_gamsig_policy, c("max_iter"), default = 100L
      )),
      UNIV_GAMSIG_CONVERGENCE_TOL = as.character(unified_get(
        univar_gamsig_policy, c("convergence_tol"), default = 1e-6
      )),
      UNIV_GAMSIG_ELBO_TOL = as.character(unified_get(
        univar_gamsig_policy, c("convergence", "elbo_tol"), default = 1e-6
      )),
      UNIV_GAMSIG_ELBO_REL_TOL = as.character(unified_get(
        univar_gamsig_policy, c("convergence", "elbo_rel_tol"), default = 2.5e-4
      )),
      UNIV_GAMSIG_STATE_NORM_TOL = as.character(unified_get(
        univar_gamsig_policy, c("convergence", "state_norm_sq_tol"), default = 1e-6
      )),
      UNIV_GAMSIG_STATE_NORM_REL_TOL = as.character(unified_get(
        univar_gamsig_policy, c("convergence", "state_norm_sq_rel_tol"), default = 2.5e-4
      )),
      UNIV_GAMSIG_SIGMA_EXP_TOL = as.character(unified_get(
        univar_gamsig_policy, c("convergence", "sigma_exp_tol"), default = 1e-6
      )),
      UNIV_GAMSIG_SIGMA_EXP_REL_TOL = as.character(unified_get(
        univar_gamsig_policy, c("convergence", "sigma_exp_rel_tol"), default = 5e-5
      )),
      UNIV_GAMSIG_GAMMA_EXP_TOL = as.character(unified_get(
        univar_gamsig_policy, c("convergence", "gamma_exp_tol"), default = 1e-6
      )),
      UNIV_GAMSIG_GAMMA_EXP_REL_TOL = as.character(unified_get(
        univar_gamsig_policy, c("convergence", "gamma_exp_rel_tol"), default = 5e-5
      )),
      UNIV_GAMSIG_FREEZE_TARGET = as.character(unified_get(
        univar_gamsig_policy, c("freeze_target"), default = "gamma_sigma"
      )),
      UNIV_GAMSIG_GUARD_REFREEZE_ITERS = as.character(unified_get(
        univar_gamsig_policy, c("guard_refreeze_iters"), default = 10L
      )),
      UNIV_GAMSIG_INIT_MODE = as.character(unified_get(
        univar_gamsig_policy, c("init", "mode"), default = "robust"
      )),
      UNIV_GAMSIG_INIT_GAMMA = as.character(unified_get(
        univar_gamsig_policy, c("init", "gamma"), default = 0.0
      )),
      UNIV_GAMSIG_INIT_SIGMA_FLOOR = as.character(unified_get(
        univar_gamsig_policy, c("init", "sigma_floor"), default = 1e-3
      )),
      UNIV_GAMSIG_INIT_SIGMA_SCALE = as.character(unified_get(
        univar_gamsig_policy, c("init", "sigma_scale"), default = 1.0
      )),
      UNIV_GAMSIG_OBJECTIVE_GUARD_ENABLED = if (isTRUE(unified_get(
        univar_gamsig_policy, c("objective_guard", "enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      UNIV_GAMSIG_OBJECTIVE_GUARD_FAIL_FAST = if (isTRUE(unified_get(
        univar_gamsig_policy, c("objective_guard", "fail_fast"), default = FALSE
      ))) "TRUE" else "FALSE",
      UNIV_GAMSIG_OBJECTIVE_GUARD_LOG_FAILURES = if (isTRUE(unified_get(
        univar_gamsig_policy, c("objective_guard", "log_failures"), default = TRUE
      ))) "TRUE" else "FALSE",
      UNIV_GAMSIG_OBJECTIVE_GUARD_MODE = as.character(unified_get(
        univar_gamsig_policy, c("objective_guard", "mode"), default = "adaptive_freeze"
      )),
      UNIV_GAMSIG_OBJECTIVE_GUARD_PENALTY = as.character(unified_get(
        univar_gamsig_policy, c("objective_guard", "penalty"), default = 1e12
      )),
      UNIV_DF_T = as.character(unified_get(
        cfg, c("models", "exdqlm_univar", "state_evolution", "df_t"), default = 0.9999995
      )),
      UNIV_DF_S1 = as.character(unified_get(
        cfg, c("models", "exdqlm_univar", "state_evolution", "df_s1"), default = 0.9997
      )),
      UNIV_DF_S2 = as.character(unified_get(
        cfg, c("models", "exdqlm_univar", "state_evolution", "df_s2"), default = 0.9997
      )),
      UNIV_DF_S67 = as.character(unified_get(
        cfg, c("models", "exdqlm_univar", "state_evolution", "df_s67"), default = 0.9997
      )),
      UNIV_LAMBDA = as.character(unified_get(
        cfg, c("models", "exdqlm_univar", "state_evolution", "lambda"), default = 0.8995
      )),
      UNIV_DF_TRANS = as.character(unified_get(
        cfg, c("models", "exdqlm_univar", "state_evolution", "df_trans"), default = 0.99999999
      )),
      UNIV_DF_COVS = as.character(unified_get(
        cfg, c("models", "exdqlm_univar", "state_evolution", "df_covs"), default = 0.99999
      )),
      UNIV_LAM1 = as.character(unified_get(
        cfg, c("fit", "exdqlm_univar", "legacy", "lam1"), default = 1 - 1e-16
      )),
      UNIV_LAM2 = as.character(unified_get(
        cfg, c("fit", "exdqlm_univar", "legacy", "lam2"), default = 1 - 1e-16
      )),
      UNIV_N_SAMP = as.character(unified_get(
        cfg, c("fit", "exdqlm_univar", "legacy", "n_samp"), default = 2000L
      )),
      UNIV_SIMS_ENABLED = if (isTRUE(unified_get(
        cfg, c("fit", "exdqlm_univar", "legacy", "sims_enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      UNIV_USE_COVARIATES = if (isTRUE(unified_get(
        cfg, c("fit", "exdqlm_univar", "legacy", "use_covariates"), default = TRUE
      ))) "TRUE" else "FALSE",
      UNIV_THEORY_SUMMARY_LOG = file.path(q_logs, "univar_theory_summary.log")
    )
    env_kv <- sprintf("%s=%s", names(env_overrides), unname(env_overrides))

    script_args <- c("--vanilla", univar_script, as.character(q), as.character(cfg$run$seed))
    cmd_status <- suppressWarnings(system2(
      "Rscript",
      script_args,
      stdout = log_path,
      stderr = log_path,
      env = env_kv
    ))
    if (!is.finite(cmd_status)) {
      cmd_status <- 0L
    }
    list(
      model_family = "exdqlm_univar",
      quantile = q,
      q_num = q_num,
      q_lab = q_lab,
      output_path = output_path,
      log_path = log_path,
      status = as.integer(cmd_status)
    )
  }

  process_univar_result <- function(manifest, res_raw) {
    res <- unified_normalize_fit_worker_result(res_raw, context_label = "univariate fit worker")
    if (!is.null(res$status) && res$status != 0) {
      stop(
        sprintf(
          "univariate fit failed for quantile %s (implementation_mode=%s); see %s",
          res$quantile,
          univar_impl_mode,
          res$log_path
        ),
        call. = FALSE
      )
    }
    file_size <- suppressWarnings(file.info(res$output_path)$size)
    if (!file.exists(res$output_path) || !is.finite(file_size) || file_size <= 0) {
      stop(
        sprintf(
          "univariate output missing or empty for quantile %s (implementation_mode=%s): %s",
          res$quantile,
          univar_impl_mode,
          res$output_path
        ),
        call. = FALSE
      )
    }

    q_num <- suppressWarnings(as.integer(res$q_num))
    if (!is.finite(q_num)) q_num <- as.integer(round(as.numeric(res$quantile) * 100))
    q_lab <- as.character(res$q_lab)
    if (!length(q_lab) || is.na(q_lab[[1]]) || !nzchar(q_lab[[1]])) {
      q_lab <- sprintf("%02d", q_num)
    } else {
      q_lab <- q_lab[[1]]
    }
    q_logs <- file.path(fit_root, "exdqlm_univar", sprintf("q=%s", q_lab), "logs")

    manifest <- unified_manifest_add_artifact(
      manifest,
      res$output_path,
      storage_scale = "model_state",
      flow_domain = cfg$scale_contract$analysis_scale_fit_internal
    )
    if (file.exists(res$log_path)) {
      manifest <- unified_manifest_add_artifact(manifest, res$log_path, storage_scale = "text")
    }

    if (contract_checks_enabled && identical(univar_impl_mode, "theory_aligned")) {
      check_dir <- file.path(fit_root, "contract_checks", "exdqlm_univar", sprintf("q=%s", q_lab))
      check_result <- unified_contract_check_exdqlm_univar(
        rdata_path = res$output_path,
        q_num = q_num,
        report_dir = check_dir,
        write_reports = contract_checks_write_reports
      )
      manifest <- add_report_artifacts(manifest, check_result$report_paths, role = "contract_check")
      if (!identical(check_result$status, "pass")) {
        err_msg <- sprintf(
          "univariate contract check failed for q=%s: %s",
          q_lab,
          paste(check_result$errors, collapse = " | ")
        )
        if (contract_checks_fail_fast) {
          stop(err_msg, call. = FALSE)
        } else {
          warning(err_msg, call. = FALSE)
        }
      }
    }

    if (diagnostics_enabled && identical(univar_impl_mode, "theory_aligned")) {
      diag_dir <- file.path(fit_root, "diagnostics", "exdqlm_univar", sprintf("q=%s", q_lab))
      summary_log_path <- file.path(q_logs, "univar_theory_summary.log")
      diag_result <- unified_diag_exdqlm_univar_theory(
        rdata_path = res$output_path,
        q_num = q_num,
        report_dir = diag_dir,
        summary_log_path = summary_log_path,
        settings = diagnostics_settings,
        write_reports = diagnostics_write_reports
      )
      manifest <- add_report_artifacts(manifest, diag_result$report_paths, role = "diagnostics")
      if (!identical(diag_result$status, "pass")) {
        report_pointer <- unlist(diag_result$report_paths, use.names = FALSE)
        report_pointer <- report_pointer[nzchar(report_pointer)]
        pointer_msg <- if (length(report_pointer) > 0L) {
          sprintf(" (see %s)", report_pointer[[1]])
        } else {
          ""
        }
        err_msg <- sprintf(
          "univariate diagnostics failed for q=%s%s: %s",
          q_lab,
          pointer_msg,
          paste(diag_result$errors, collapse = " | ")
        )
        if (diagnostics_fail_fast) {
          stop(err_msg, call. = FALSE)
        } else {
          warning(err_msg, call. = FALSE)
        }
      }
    }

    manifest
  }

  ndlm_script <- NULL
  ndlm_outputs <- NULL
  ndlm_logs <- NULL
  if (run_ndlm_main) {
    if (!use_shared_inputs) {
      stop(
        "legacy NDLM bridge requires run-scoped shared inputs. Enable stages.data_prep_shared and provide shared bundle inputs.",
        call. = FALSE
      )
    }
    required_cov_keys <- if (using_engineered_covariates) character(0) else c("eli", "oni", "ppt", "soil", "pca")
    missing_cov <- required_cov_keys[!nzchar(unlist(shared_cov_paths[required_cov_keys], use.names = FALSE))]
    if (length(missing_cov) > 0L) {
      stop(
        sprintf(
          "legacy NDLM bridge missing shared covariates in run bundle: %s",
          paste(missing_cov, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    ndlm_script <- if (identical(ndlm_impl_mode, "theory_aligned")) {
      file.path(repo_root, "scripts", "run_ndlm_main.R")
    } else {
      file.path(repo_root, "scripts", "run_DISC_Optimal_Synth_Ranges_NDLM.R")
    }
    if (!file.exists(ndlm_script)) {
      stop(
        sprintf("NDLM script not found for implementation_mode=%s: %s", ndlm_impl_mode, ndlm_script),
        call. = FALSE
      )
    }

    ndlm_root <- file.path(fit_root, "ndlm_main")
    ndlm_outputs <- file.path(ndlm_root, "outputs")
    ndlm_logs <- file.path(ndlm_root, "logs")
    dir.create(ndlm_outputs, recursive = TRUE, showWarnings = FALSE)
    dir.create(ndlm_logs, recursive = TRUE, showWarnings = FALSE)
    if (isTRUE(io_settings$enabled)) {
      ndlm_preflight <- run_preflight_check(
        path = ndlm_outputs,
        check_point = "continue",
        context = "stage_fit ndlm_main",
        stage_label = "fit_ndlm_main"
      )
      manifest <- add_preflight_artifact(manifest, ndlm_preflight)
    }
  }

  run_ndlm_fit <- function() {
    output_path <- file.path(ndlm_outputs, "DISC_variables_50_NDLM_synth_DISC.RData")
    log_name <- if (identical(ndlm_impl_mode, "theory_aligned")) "ndlm_theory.log" else "ndlm_legacy.log"
    log_path <- file.path(ndlm_logs, log_name)
    ndlm_forecast_iw_epsilon <- unified_get(
      cfg,
      c("models", "ndlm_main", "prior", "forecast_cov", "epsilon"),
      default = NULL
    )
    env_overrides <- c(
      UNIFIED_NDLM_RDATA_OUT = output_path,
      NDLM_RUN_ROOT = run_root_abs,
      NDLM_OUT_DIR = ndlm_outputs,
      NDLM_SHARED_INPUT_ROOT = shared_paths$root,
      NDLM_PARAMETERS_TXT = source_parameters,
      NDLM_RETROS_CSV = source_retros,
      NDLM_NWS_FORECAST_CSV = source_nws,
      NDLM_GLOFAS_FORECAST_CSV = source_glofas,
      UNIFIED_COVARIATE_FEATURES_CSV = shared_feature_csv,
      NDLM_COVARIATES_DIR = shared_paths$covariates_dir,
      NDLM_COV1_ELI_CSV = shared_cov_paths$eli,
      NDLM_COV2_ONI_CSV = shared_cov_paths$oni,
      NDLM_PPT_CSV = shared_cov_paths$ppt,
      NDLM_SOIL_CSV = shared_cov_paths$soil,
      NDLM_PCA_CSV = shared_cov_paths$pca,
      NDLM_USE_PREV = if (isTRUE(cfg$fit$warm_start$enabled)) "TRUE" else "FALSE",
      NDLM_PREV_RDATA = output_path,
      NDLM_FORECAST_TRANSFER_MODE = as.character(ndlm_forecast_transfer_mode),
      NDLM_GAMSIG_MIN_TOTAL_ITERS = as.character(unified_get(
        cfg, c("fit", "ndlm_main", "gamma_sigma", "min_total_iters"), default = 50L
      )),
      NDLM_GAMSIG_MAX_ITER = as.character(unified_get(
        cfg, c("fit", "ndlm_main", "gamma_sigma", "max_iter"), default = 100L
      )),
      NDLM_GAMSIG_CONVERGENCE_TOL = as.character(unified_get(
        cfg, c("fit", "ndlm_main", "gamma_sigma", "convergence_tol"), default = 1e-6
      )),
      NDLM_GAMSIG_ELBO_TOL = as.character(unified_get(
        cfg, c("fit", "ndlm_main", "gamma_sigma", "convergence", "elbo_tol"), default = 1e-6
      )),
      NDLM_GAMSIG_ELBO_REL_TOL = as.character(unified_get(
        cfg, c("fit", "ndlm_main", "gamma_sigma", "convergence", "elbo_rel_tol"), default = 2.5e-4
      )),
      NDLM_KALMAN_BACKEND = as.character(unified_get(
        cfg, c("models", "ndlm_main", "kalman_backend"), default = "cpp"
      )),
      NDLM_DF_T = as.character(unified_get(
        cfg, c("models", "ndlm_main", "state_evolution", "df_t"), default = 0.95
      )),
      NDLM_DF_S1 = as.character(unified_get(
        cfg, c("models", "ndlm_main", "state_evolution", "df_s1"), default = 0.98
      )),
      NDLM_DF_S2 = as.character(unified_get(
        cfg, c("models", "ndlm_main", "state_evolution", "df_s2"), default = 0.98
      )),
      NDLM_DF_S67 = as.character(unified_get(
        cfg, c("models", "ndlm_main", "state_evolution", "df_s67"), default = 0.98
      )),
      NDLM_DF_DISCREP = as.character(unified_get(
        cfg, c("models", "ndlm_main", "state_evolution", "df_discrep"), default = 0.98
      )),
      NDLM_LAMBDA = as.character(unified_get(
        cfg, c("models", "ndlm_main", "state_evolution", "lambda"), default = 0.99
      )),
      NDLM_DF_TRANS = as.character(unified_get(
        cfg, c("models", "ndlm_main", "state_evolution", "df_trans"), default = 0.99999999
      )),
      NDLM_DF_COVS = as.character(unified_get(
        cfg, c("models", "ndlm_main", "state_evolution", "df_covs"), default = 0.99999
      )),
      NDLM_SEASONAL_PERIOD = as.character(unified_get(
        cfg, c("models", "ndlm_main", "seasonality", "period"), default = 363.5854
      )),
      NDLM_SEASONAL_HARMONICS = paste(
        as.character(unified_get(
          cfg, c("models", "ndlm_main", "seasonality", "harmonics"),
          default = c(1, 2, 1 / 6.8068493)
        )),
        collapse = ","
      ),
      NDLM_FORECAST_IW_C_FACTOR = as.character(unified_get(
        cfg, c("models", "ndlm_main", "prior", "forecast_cov", "c_factor"), default = 1.0
      )),
      NDLM_FORECAST_IW_EPSILON0 = if (is.null(ndlm_forecast_iw_epsilon) || length(ndlm_forecast_iw_epsilon) < 1L || all(is.na(ndlm_forecast_iw_epsilon))) {
        ""
      } else {
        as.character(ndlm_forecast_iw_epsilon[[1L]])
      },
      NDLM_FORECAST_IW_DOF_OFFSET = as.character(unified_get(
        cfg, c("models", "ndlm_main", "prior", "forecast_cov", "dof_offset"), default = 4L
      )),
      NDLM_FORECAST_IW_SCALE_MULT = as.character(unified_get(
        cfg, c("models", "ndlm_main", "prior", "forecast_cov", "scale_mult"), default = 1.0
      )),
      NDLM_FORECAST_IW_JITTER = as.character(unified_get(
        cfg, c("models", "ndlm_main", "prior", "forecast_cov", "jitter"), default = 1e-8
      )),
      NDLM_COV_EIG_FLOOR = as.character(unified_get(
        cfg, c("models", "ndlm_main", "stabilization", "cov_eig_floor"), default = 1e-8
      )),
      NDLM_COV_EIG_CAP = as.character(unified_get(
        cfg, c("models", "ndlm_main", "stabilization", "cov_eig_cap"), default = 1e8
      )),
      NDLM_COV_DIAG_JITTER = as.character(unified_get(
        cfg, c("models", "ndlm_main", "stabilization", "cov_diag_jitter"), default = 1e-10
      )),
      NDLM_SIGMA_UPPER_CAP = as.character(unified_get(
        cfg, c("models", "ndlm_main", "stabilization", "sigma_upper_cap"), default = 1e12
      )),
      NDLM_SIGMA_UPDATE_DAMPING = as.character(unified_get(
        cfg, c("models", "ndlm_main", "stabilization", "sigma_update_damping"), default = 1.0
      )),
      NDLM_LATENT_VAR_CAP_MULT = as.character(unified_get(
        cfg, c("models", "ndlm_main", "stabilization", "latent_var_cap_mult"), default = 1e4
      )),
      NDLM_LATENT_VAR_CAP_ABS = as.character(unified_get(
        cfg, c("models", "ndlm_main", "stabilization", "latent_var_cap_abs"), default = 1e8
      )),
      NDLM_LAM1 = as.character(unified_get(
        cfg, c("fit", "ndlm_main", "legacy", "lam1"), default = 1 - 1e-6
      )),
      NDLM_LAM2 = as.character(unified_get(
        cfg, c("fit", "ndlm_main", "legacy", "lam2"), default = 0.9
      )),
      NDLM_N_SAMP = as.character(unified_get(
        cfg, c("fit", "ndlm_main", "legacy", "n_samp"), default = 2000L
      )),
      NDLM_SIMS_ENABLED = if (isTRUE(unified_get(
        cfg, c("fit", "ndlm_main", "legacy", "sims_enabled"), default = TRUE
      ))) "TRUE" else "FALSE",
      NDLM_USE_COVARIATES = if (isTRUE(unified_get(
        cfg, c("fit", "ndlm_main", "legacy", "use_covariates"), default = TRUE
      ))) "TRUE" else "FALSE",
      NDLM_THEORY_SUMMARY_LOG = file.path(ndlm_logs, "ndlm_theory_summary.log")
    )
    env_kv <- sprintf("%s=%s", names(env_overrides), unname(env_overrides))

    script_args <- c("--vanilla", ndlm_script, as.character(cfg$run$seed))
    cmd_out <- system2(
      "Rscript",
      script_args,
      stdout = TRUE,
      stderr = TRUE,
      env = env_kv
    )
    writeLines(cmd_out, log_path, useBytes = TRUE)
    status <- attr(cmd_out, "status")
    if (is.null(status) || !is.finite(status)) status <- 0L
    list(
      model_family = "ndlm_main",
      quantile = NA_real_,
      output_path = output_path,
      log_path = log_path,
      status = as.integer(status)
    )
  }

  ndlm_cov_stabilize_matrix_for_export <- function(Sigma, floor_val, cap_val, jitter_val) {
    Sigma <- as.matrix(Sigma)
    if (!is.numeric(Sigma) || nrow(Sigma) != ncol(Sigma)) {
      stop("ndlm covariance hardening expects a numeric square matrix", call. = FALSE)
    }
    d <- nrow(Sigma)
    Sigma[!is.finite(Sigma)] <- 0
    Sigma <- (Sigma + t(Sigma)) / 2

    eig_vals <- tryCatch(
      suppressWarnings(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values),
      error = function(e) rep(NA_real_, d)
    )
    has_nonfinite_eigs <- any(!is.finite(eig_vals))
    floor_hit <- has_nonfinite_eigs || min(eig_vals, na.rm = TRUE) < floor_val
    cap_hit <- has_nonfinite_eigs || max(eig_vals, na.rm = TRUE) > cap_val
    if (isTRUE(floor_hit) || isTRUE(cap_hit)) {
      eig <- tryCatch(
        suppressWarnings(eigen(Sigma, symmetric = TRUE)),
        error = function(e) NULL
      )
      if (is.null(eig) || any(!is.finite(eig$values)) || any(!is.finite(eig$vectors))) {
        Sigma <- diag(floor_val, d)
      } else {
        vals <- as.numeric(eig$values)
        vals <- pmin(pmax(vals, floor_val), cap_val)
        Sigma <- eig$vectors %*% diag(vals, length(vals)) %*% t(eig$vectors)
      }
    }
    Sigma <- (Sigma + t(Sigma)) / 2
    if (jitter_val > 0) {
      Sigma <- Sigma + diag(jitter_val, d)
    }

    for (ii in seq_len(3L)) {
      final_min <- tryCatch(
        suppressWarnings(min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values)),
        error = function(e) NA_real_
      )
      if (is.finite(final_min) && final_min >= floor_val) {
        break
      }
      shift <- if (!is.finite(final_min)) floor_val else (floor_val - final_min)
      shift <- max(shift + jitter_val, jitter_val)
      Sigma <- Sigma + diag(shift, d)
      Sigma <- (Sigma + t(Sigma)) / 2
    }

    chol_pad <- max(floor_val, jitter_val, 1e-12)
    has_chol_contract <- !is.null(tryCatch(
      chol(Sigma + diag(chol_pad, d)),
      error = function(e) NULL
    ))
    if (!isTRUE(has_chol_contract)) {
      eig <- tryCatch(suppressWarnings(eigen(Sigma, symmetric = TRUE)), error = function(e) NULL)
      if (!is.null(eig) && all(is.finite(eig$values)) && all(is.finite(eig$vectors))) {
        vals <- as.numeric(eig$values)
        vals <- pmin(pmax(vals, floor_val), cap_val)
        Sigma <- eig$vectors %*% diag(vals, length(vals)) %*% t(eig$vectors)
        Sigma <- (Sigma + t(Sigma)) / 2
      }
      for (ii in seq_len(6L)) {
        has_chol_contract <- !is.null(tryCatch(
          chol(Sigma + diag(chol_pad, d)),
          error = function(e) NULL
        ))
        if (isTRUE(has_chol_contract)) break
        bump <- max(chol_pad * (2 ^ (ii - 1L)), jitter_val)
        Sigma <- Sigma + diag(bump, d)
        Sigma <- (Sigma + t(Sigma)) / 2
      }
    }

    final_min <- tryCatch(
      suppressWarnings(min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values)),
      error = function(e) NA_real_
    )
    if (!is.finite(final_min) || final_min < floor_val) {
      shift <- if (!is.finite(final_min)) floor_val else (floor_val - final_min + jitter_val)
      Sigma <- Sigma + diag(shift, d)
      Sigma <- (Sigma + t(Sigma)) / 2
    }
    Sigma
  }

  ndlm_cov_harden_array_for_export <- function(cov_arr, floor_val, cap_val, jitter_val) {
    dims <- dim(cov_arr)
    if (is.null(dims) || length(dims) != 3L || dims[1] != dims[2]) {
      stop("ndlm covariance hardening expects a square 3D covariance array", call. = FALSE)
    }
    out <- cov_arr
    repaired <- 0L
    before_mins <- rep(NA_real_, dims[3])
    after_mins <- rep(NA_real_, dims[3])
    for (k in seq_len(dims[3])) {
      before <- tryCatch(
        suppressWarnings(min(eigen((out[, , k, drop = TRUE] + t(out[, , k, drop = TRUE])) / 2, symmetric = TRUE, only.values = TRUE)$values)),
        error = function(e) NA_real_
      )
      out[, , k] <- ndlm_cov_stabilize_matrix_for_export(out[, , k, drop = TRUE], floor_val, cap_val, jitter_val)
      after <- tryCatch(
        suppressWarnings(min(eigen((out[, , k, drop = TRUE] + t(out[, , k, drop = TRUE])) / 2, symmetric = TRUE, only.values = TRUE)$values)),
        error = function(e) NA_real_
      )
      before_mins[[k]] <- before
      after_mins[[k]] <- after
      if (!is.finite(before) || !is.finite(after) || abs(after - before) > 1e-12) {
        repaired <- repaired + 1L
      }
    }
    list(
      cov = out,
      repaired_slices = as.integer(repaired),
      min_eig_before = if (all(!is.finite(before_mins))) NA_real_ else min(before_mins, na.rm = TRUE),
      min_eig_after = if (all(!is.finite(after_mins))) NA_real_ else min(after_mins, na.rm = TRUE),
      below_floor_before = as.integer(sum(is.finite(before_mins) & before_mins < floor_val)),
      below_floor_after = as.integer(sum(is.finite(after_mins) & after_mins < floor_val))
    )
  }

  ndlm_cov_diag_one <- function(object_name, cov_arr) {
    dims <- dim(cov_arr)
    if (is.null(dims) || length(dims) != 3L || dims[1] != dims[2]) {
      stop(sprintf("[NDLM_COV_SHAPE] %s must be a square 3D covariance array", object_name), call. = FALSE)
    }
    n_slices <- as.integer(dims[3])
    min_eigs <- rep(NA_real_, n_slices)
    min_diags <- rep(NA_real_, n_slices)
    max_asym <- rep(NA_real_, n_slices)
    nonfinite <- rep(FALSE, n_slices)
    base_chol_fail <- rep(FALSE, n_slices)
    for (k in seq_len(n_slices)) {
      S <- as.matrix(cov_arr[, , k, drop = TRUE])
      if (!all(is.finite(S))) {
        nonfinite[k] <- TRUE
        next
      }
      S <- (S + t(S)) / 2
      max_asym[k] <- max(abs(S - t(S)))
      min_diags[k] <- min(diag(S))
      min_eigs[k] <- min(eigen(S, symmetric = TRUE, only.values = TRUE)$values)
      base_try <- tryCatch(chol(S + diag(1e-8, nrow(S))), error = function(e) NULL)
      base_chol_fail[k] <- is.null(base_try)
    }
    data.frame(
      object = object_name,
      n_slices = n_slices,
      matrix_dim = as.integer(dims[1]),
      nonfinite_slices = as.integer(sum(nonfinite)),
      asymmetry_max = if (all(is.na(max_asym))) NA_real_ else max(max_asym, na.rm = TRUE),
      min_diag_min = if (all(is.na(min_diags))) NA_real_ else min(min_diags, na.rm = TRUE),
      min_eig_min = if (all(is.na(min_eigs))) NA_real_ else min(min_eigs, na.rm = TRUE),
      min_eig_p01 = if (all(is.na(min_eigs))) NA_real_ else as.numeric(stats::quantile(min_eigs, probs = 0.01, na.rm = TRUE, names = FALSE)),
      base_chol_fail_slices = as.integer(sum(base_chol_fail, na.rm = TRUE)),
      base_chol_fail_rate = mean(base_chol_fail, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  ndlm_cov_harden_rdata <- function(rdata_path, floor_val, cap_val, jitter_val, report_path = NULL) {
    if (!file.exists(rdata_path)) {
      stop(sprintf("ndlm covariance hardening target does not exist: %s", rdata_path), call. = FALSE)
    }
    env <- new.env(parent = emptyenv())
    load(rdata_path, envir = env)
    theta_name <- "new.theta.out_50_NDLM_synth_DISC"
    if (!exists(theta_name, envir = env, inherits = FALSE)) {
      return(list(applied = FALSE, repaired_slices_total = 0L, report_path = report_path))
    }
    theta <- get(theta_name, envir = env, inherits = FALSE)
    if (!is.list(theta) || !("sC" %in% names(theta)) || !("sC_ens" %in% names(theta))) {
      return(list(applied = FALSE, repaired_slices_total = 0L, report_path = report_path))
    }

    smooth_fix <- ndlm_cov_harden_array_for_export(theta$sC, floor_val = floor_val, cap_val = cap_val, jitter_val = jitter_val)
    theta$sC <- smooth_fix$cov

    seg_fix_total <- 0L
    seg_min_before <- numeric(0)
    seg_min_after <- numeric(0)
    seg_below_floor_before <- integer(0)
    seg_below_floor_after <- integer(0)
    if (is.list(theta$sC_ens) && length(theta$sC_ens) > 0L) {
      for (ii in seq_along(theta$sC_ens)) {
        seg_fix <- ndlm_cov_harden_array_for_export(theta$sC_ens[[ii]], floor_val = floor_val, cap_val = cap_val, jitter_val = jitter_val)
        theta$sC_ens[[ii]] <- seg_fix$cov
        seg_fix_total <- seg_fix_total + seg_fix$repaired_slices
        seg_min_before <- c(seg_min_before, seg_fix$min_eig_before)
        seg_min_after <- c(seg_min_after, seg_fix$min_eig_after)
        seg_below_floor_before <- c(seg_below_floor_before, seg_fix$below_floor_before)
        seg_below_floor_after <- c(seg_below_floor_after, seg_fix$below_floor_after)
      }
    }

    assign(theta_name, theta, envir = env)

    state_name <- "ndlm_main_theory_state"
    if (exists(state_name, envir = env, inherits = FALSE)) {
      st <- get(state_name, envir = env, inherits = FALSE)
      if (is.list(st)) {
        sC1 <- if (is.list(theta$sC_ens) && length(theta$sC_ens) >= 1L) theta$sC_ens[[1L]] else array(0, dim = c(7L, 7L, 0L))
        sC2 <- if (is.list(theta$sC_ens) && length(theta$sC_ens) >= 2L) theta$sC_ens[[2L]] else array(0, dim = c(7L, 7L, 0L))
        st$covariance_diagnostics <- do.call(rbind, list(
          ndlm_cov_diag_one("smooth_cov", theta$sC),
          ndlm_cov_diag_one("forecast_cov_segment_1", sC1),
          ndlm_cov_diag_one("forecast_cov_segment_2", sC2)
        ))
        rownames(st$covariance_diagnostics) <- NULL
        assign(state_name, st, envir = env)
      }
    }

    save(list = ls(env), file = rdata_path, envir = env)

    repaired_total <- as.integer(smooth_fix$repaired_slices + seg_fix_total)
    if (!is.null(report_path) && nzchar(report_path)) {
      lines <- c(
        sprintf("rdata_path=%s", normalizePath(rdata_path, mustWork = FALSE)),
        sprintf("cov_eig_floor=%s", format(floor_val, digits = 12)),
        sprintf("cov_eig_cap=%s", format(cap_val, digits = 12)),
        sprintf("cov_diag_jitter=%s", format(jitter_val, digits = 12)),
        sprintf("smooth_repaired_slices=%d", as.integer(smooth_fix$repaired_slices)),
        sprintf("segment_repaired_slices=%d", as.integer(seg_fix_total)),
        sprintf("repaired_slices_total=%d", repaired_total),
        sprintf("smooth_min_eig_before=%s", if (is.finite(smooth_fix$min_eig_before)) format(smooth_fix$min_eig_before, digits = 12) else "NA"),
        sprintf("smooth_min_eig_after=%s", if (is.finite(smooth_fix$min_eig_after)) format(smooth_fix$min_eig_after, digits = 12) else "NA"),
        sprintf("smooth_below_floor_before=%d", as.integer(smooth_fix$below_floor_before)),
        sprintf("smooth_below_floor_after=%d", as.integer(smooth_fix$below_floor_after)),
        sprintf(
          "segments_min_eig_before=%s",
          if (length(seg_min_before) > 0L) paste(ifelse(is.finite(seg_min_before), format(seg_min_before, digits = 12), "NA"), collapse = ",") else "NA"
        ),
        sprintf(
          "segments_min_eig_after=%s",
          if (length(seg_min_after) > 0L) paste(ifelse(is.finite(seg_min_after), format(seg_min_after, digits = 12), "NA"), collapse = ",") else "NA"
        ),
        sprintf(
          "segments_below_floor_before=%s",
          if (length(seg_below_floor_before) > 0L) paste(as.integer(seg_below_floor_before), collapse = ",") else "NA"
        ),
        sprintf(
          "segments_below_floor_after=%s",
          if (length(seg_below_floor_after) > 0L) paste(as.integer(seg_below_floor_after), collapse = ",") else "NA"
        )
      )
      writeLines(lines, con = report_path, useBytes = TRUE)
    }
    list(applied = TRUE, repaired_slices_total = repaired_total, report_path = report_path)
  }

  process_ndlm_result <- function(manifest, res_raw) {
    res <- unified_normalize_fit_worker_result(res_raw, context_label = "NDLM fit worker")
    if (!is.null(res$status) && res$status != 0) {
      err_msg <- sprintf("NDLM fit failed (implementation_mode=%s); see %s", ndlm_impl_mode, res$log_path)
      append_fit_stage_log(err_msg)
      stop(err_msg, call. = FALSE)
    }
    if (!file.exists(res$output_path)) {
      err_msg <- sprintf("NDLM output missing (implementation_mode=%s): %s", ndlm_impl_mode, res$output_path)
      append_fit_stage_log(err_msg)
      stop(err_msg, call. = FALSE)
    }

    manifest <- unified_manifest_add_artifact(
      manifest,
      res$output_path,
      storage_scale = "model_state",
      flow_domain = cfg$scale_contract$analysis_scale_fit_internal
    )
    if (file.exists(res$log_path)) {
      manifest <- unified_manifest_add_artifact(manifest, res$log_path, storage_scale = "text")
    }

    if (identical(ndlm_impl_mode, "theory_aligned")) {
      cov_harden_log <- file.path(ndlm_logs, "ndlm_covariance_hardening.log")
      cov_floor <- as.numeric(unified_get(
        cfg, c("models", "ndlm_main", "stabilization", "cov_eig_floor"), default = 1e-8
      ))
      cov_cap <- as.numeric(unified_get(
        cfg, c("models", "ndlm_main", "stabilization", "cov_eig_cap"), default = 1e8
      ))
      cov_jitter <- as.numeric(unified_get(
        cfg, c("models", "ndlm_main", "stabilization", "cov_diag_jitter"), default = 1e-10
      ))
      if (!is.finite(cov_floor) || cov_floor <= 0) cov_floor <- 1e-8
      if (!is.finite(cov_cap) || cov_cap <= cov_floor) cov_cap <- max(1e8, cov_floor * 10)
      if (!is.finite(cov_jitter) || cov_jitter < 0) cov_jitter <- 1e-10
      harden_res <- ndlm_cov_harden_rdata(
        rdata_path = res$output_path,
        floor_val = cov_floor,
        cap_val = cov_cap,
        jitter_val = cov_jitter,
        report_path = cov_harden_log
      )
      if (isTRUE(harden_res$applied) && file.exists(cov_harden_log)) {
        manifest <- unified_manifest_add_artifact(manifest, cov_harden_log, storage_scale = "text")
      }
    }

    if (contract_checks_enabled && identical(ndlm_impl_mode, "theory_aligned")) {
      check_dir <- file.path(fit_root, "contract_checks", "ndlm_main")
      summary_log_path <- file.path(ndlm_logs, "ndlm_theory_summary.log")
      check_result <- unified_contract_check_ndlm_main(
        rdata_path = res$output_path,
        report_dir = check_dir,
        summary_log_path = summary_log_path,
        write_reports = contract_checks_write_reports
      )
      manifest <- add_report_artifacts(manifest, check_result$report_paths, role = "contract_check")
      if (!identical(check_result$status, "pass")) {
        err_msg <- sprintf(
          "NDLM contract check failed: %s",
          paste(check_result$errors, collapse = " | ")
        )
        if (contract_checks_fail_fast) {
          stop(err_msg, call. = FALSE)
        } else {
          warning(err_msg, call. = FALSE)
        }
      }
    }

    if (diagnostics_enabled && identical(ndlm_impl_mode, "theory_aligned")) {
      diag_dir <- file.path(fit_root, "diagnostics", "ndlm_main")
      summary_log_path <- file.path(ndlm_logs, "ndlm_theory_summary.log")
      diag_result <- unified_diag_ndlm_main_theory(
        rdata_path = res$output_path,
        report_dir = diag_dir,
        summary_log_path = summary_log_path,
        settings = diagnostics_settings,
        write_reports = diagnostics_write_reports
      )
      manifest <- add_report_artifacts(manifest, diag_result$report_paths, role = "diagnostics")
      if (!identical(diag_result$status, "pass")) {
        report_pointer <- unlist(diag_result$report_paths, use.names = FALSE)
        report_pointer <- report_pointer[nzchar(report_pointer)]
        pointer_msg <- if (length(report_pointer) > 0L) {
          sprintf(" (see %s)", report_pointer[[1]])
        } else {
          ""
        }
        err_msg <- sprintf(
          "NDLM diagnostics failed%s: %s",
          pointer_msg,
          paste(diag_result$errors, collapse = " | ")
        )
        if (diagnostics_fail_fast) {
          stop(err_msg, call. = FALSE)
        } else {
          warning(err_msg, call. = FALSE)
        }
      }
    }

    manifest
  }

  ndlm_univar_script <- NULL
  ndlm_univar_outputs <- NULL
  ndlm_univar_logs <- NULL
  if (run_ndlm_univar) {
    if (!use_shared_inputs) {
      stop(
        "ndlm_univar requires run-scoped shared inputs. Enable stages.data_prep_shared and provide shared bundle inputs.",
        call. = FALSE
      )
    }
    required_cov_keys <- if (using_engineered_covariates) character(0) else c("eli", "oni", "ppt", "soil", "pca")
    missing_cov <- required_cov_keys[!nzchar(unlist(shared_cov_paths[required_cov_keys], use.names = FALSE))]
    if (length(missing_cov) > 0L) {
      stop(
        sprintf(
          "ndlm_univar missing shared covariates in run bundle: %s",
          paste(missing_cov, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    ndlm_univar_script <- file.path(repo_root, "scripts", "run_ndlm_univar.R")
    if (!file.exists(ndlm_univar_script)) {
      stop(sprintf("ndlm_univar script not found: %s", ndlm_univar_script), call. = FALSE)
    }

    ndlm_univar_root <- file.path(fit_root, "ndlm_univar")
    ndlm_univar_outputs <- file.path(ndlm_univar_root, "outputs")
    ndlm_univar_logs <- file.path(ndlm_univar_root, "logs")
    dir.create(ndlm_univar_outputs, recursive = TRUE, showWarnings = FALSE)
    dir.create(ndlm_univar_logs, recursive = TRUE, showWarnings = FALSE)
    if (isTRUE(io_settings$enabled)) {
      ndlm_univar_preflight <- run_preflight_check(
        path = ndlm_univar_outputs,
        check_point = "continue",
        context = "stage_fit ndlm_univar",
        stage_label = "fit_ndlm_univar"
      )
      manifest <- add_preflight_artifact(manifest, ndlm_univar_preflight)
    }
  }

  run_ndlm_univar_fit <- function() {
    output_path <- file.path(ndlm_univar_outputs, "DISC_variables_50_NDLM_univar_synth_DISC.RData")
    log_path <- file.path(ndlm_univar_logs, "ndlm_univar_theory.log")
    summary_log_path <- file.path(ndlm_univar_logs, "ndlm_univar_theory_summary.log")
    env_overrides <- c(
      UNIFIED_NDLM_UNIVAR_RDATA_OUT = output_path,
      NDLM_UNIV_RUN_ROOT = run_root_abs,
      NDLM_UNIV_OUT_DIR = ndlm_univar_outputs,
      NDLM_SHARED_INPUT_ROOT = shared_paths$root,
      NDLM_RETROS_CSV = source_retros,
      NDLM_NWS_FORECAST_CSV = source_nws,
      NDLM_GLOFAS_FORECAST_CSV = source_glofas,
      UNIFIED_COVARIATE_FEATURES_CSV = shared_feature_csv,
      NDLM_COV1_ELI_CSV = shared_cov_paths$eli,
      NDLM_COV2_ONI_CSV = shared_cov_paths$oni,
      NDLM_PPT_CSV = shared_cov_paths$ppt,
      NDLM_SOIL_CSV = shared_cov_paths$soil,
      NDLM_PCA_CSV = shared_cov_paths$pca,
      NDLM_UNIV_KALMAN_BACKEND = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "kalman_backend"), default = "cpp"
      )),
      NDLM_UNIV_FORECAST_TRANSFER_MODE = as.character(ndlm_univar_forecast_transfer_mode),
      NDLM_UNIV_DF_T = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "state_evolution", "df_t"), default = 0.95
      )),
      NDLM_UNIV_DF_S1 = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "state_evolution", "df_s1"), default = 0.98
      )),
      NDLM_UNIV_DF_S2 = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "state_evolution", "df_s2"), default = 0.98
      )),
      NDLM_UNIV_DF_S67 = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "state_evolution", "df_s67"), default = 0.98
      )),
      NDLM_UNIV_DF_TRANS = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "state_evolution", "df_trans"), default = 0.99999999
      )),
      NDLM_UNIV_DF_COVS = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "state_evolution", "df_covs"), default = 0.99999
      )),
      NDLM_UNIV_LAMBDA = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "state_evolution", "lambda"), default = 0.99
      )),
      NDLM_UNIV_SEASONAL_PERIOD = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "seasonality", "period"), default = 363.5854
      )),
      NDLM_UNIV_SEASONAL_HARMONICS = paste(
        as.character(unified_get(
          cfg, c("models", "ndlm_univar", "seasonality", "harmonics"),
          default = c(1, 2, 1 / 6.8068493)
        )),
        collapse = ","
      ),
      NDLM_UNIV_N0 = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "prior", "n0"), default = 20
      )),
      NDLM_UNIV_S0 = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "prior", "S0"), default = 1
      )),
      NDLM_UNIV_FORECAST_HORIZON_CAP = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "horizon_cap"), default = 1080L
      )),
      NDLM_UNIV_POSTERIOR_DRAWS = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "posterior_draws"), default = 64L
      )),
      NDLM_UNIV_COV_EIG_FLOOR = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "stabilization", "cov_eig_floor"), default = 1e-8
      )),
      NDLM_UNIV_COV_EIG_CAP = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "stabilization", "cov_eig_cap"), default = 1e8
      )),
      NDLM_UNIV_COV_DIAG_JITTER = as.character(unified_get(
        cfg, c("models", "ndlm_univar", "stabilization", "cov_diag_jitter"), default = 1e-10
      )),
      NDLM_UNIV_THEORY_SUMMARY_LOG = summary_log_path
    )
    env_kv <- sprintf("%s=%s", names(env_overrides), unname(env_overrides))

    script_args <- c("--vanilla", ndlm_univar_script, as.character(cfg$run$seed))
    cmd_out <- system2(
      "Rscript",
      script_args,
      stdout = TRUE,
      stderr = TRUE,
      env = env_kv
    )
    writeLines(cmd_out, log_path, useBytes = TRUE)
    status <- attr(cmd_out, "status")
    if (is.null(status) || !is.finite(status)) status <- 0L
    list(
      model_family = "ndlm_univar",
      quantile = NA_real_,
      output_path = output_path,
      log_path = log_path,
      status = as.integer(status)
    )
  }

  process_ndlm_univar_result <- function(manifest, res_raw) {
    res <- unified_normalize_fit_worker_result(res_raw, context_label = "NDLM univar fit worker")
    if (!is.null(res$status) && res$status != 0) {
      err_msg <- sprintf("NDLM univar fit failed; see %s", res$log_path)
      append_fit_stage_log(err_msg)
      stop(err_msg, call. = FALSE)
    }
    if (!file.exists(res$output_path)) {
      err_msg <- sprintf("NDLM univar output missing: %s", res$output_path)
      append_fit_stage_log(err_msg)
      stop(err_msg, call. = FALSE)
    }
    file_size <- suppressWarnings(file.info(res$output_path)$size)
    if (!is.finite(file_size) || file_size <= 0) {
      err_msg <- sprintf("NDLM univar output is empty: %s", res$output_path)
      append_fit_stage_log(err_msg)
      stop(err_msg, call. = FALSE)
    }

    manifest <- unified_manifest_add_artifact(
      manifest,
      res$output_path,
      storage_scale = "model_state",
      flow_domain = cfg$scale_contract$analysis_scale_fit_internal
    )
    if (file.exists(res$log_path)) {
      manifest <- unified_manifest_add_artifact(manifest, res$log_path, storage_scale = "text")
    }

    if (contract_checks_enabled) {
      check_dir <- file.path(fit_root, "contract_checks", "ndlm_univar")
      summary_log_path <- file.path(ndlm_univar_logs, "ndlm_univar_theory_summary.log")
      check_result <- unified_contract_check_ndlm_univar(
        rdata_path = res$output_path,
        report_dir = check_dir,
        summary_log_path = summary_log_path,
        write_reports = contract_checks_write_reports
      )
      manifest <- add_report_artifacts(manifest, check_result$report_paths, role = "contract_check")
      if (!identical(check_result$status, "pass")) {
        err_msg <- sprintf("NDLM univar contract check failed: %s", paste(check_result$errors, collapse = " | "))
        if (contract_checks_fail_fast) {
          stop(err_msg, call. = FALSE)
        } else {
          warning(err_msg, call. = FALSE)
        }
      }
    }

    if (diagnostics_enabled && ndlm_univar_impl_mode %in% c("theory_aligned_closed_form", "theory_aligned")) {
      diag_dir <- file.path(fit_root, "diagnostics", "ndlm_univar")
      summary_log_path <- file.path(ndlm_univar_logs, "ndlm_univar_theory_summary.log")
      diag_result <- unified_diag_ndlm_univar_theory(
        rdata_path = res$output_path,
        report_dir = diag_dir,
        summary_log_path = summary_log_path,
        settings = diagnostics_settings,
        write_reports = diagnostics_write_reports
      )
      manifest <- add_report_artifacts(manifest, diag_result$report_paths, role = "diagnostics")
      if (!identical(diag_result$status, "pass")) {
        report_pointer <- unlist(diag_result$report_paths, use.names = FALSE)
        report_pointer <- report_pointer[nzchar(report_pointer)]
        pointer_msg <- if (length(report_pointer) > 0L) {
          sprintf(" (see %s)", report_pointer[[1]])
        } else {
          ""
        }
        err_msg <- sprintf(
          "NDLM univar diagnostics failed%s: %s",
          pointer_msg,
          paste(diag_result$errors, collapse = " | ")
        )
        if (diagnostics_fail_fast) {
          stop(err_msg, call. = FALSE)
        } else {
          warning(err_msg, call. = FALSE)
        }
      }
    }

    manifest
  }

  execute_fit_jobs <- function(fit_jobs, workers) {
    if (length(fit_jobs) == 0L) return(list())
    safe_run <- function(job) {
      tryCatch(
        job$runner(),
        error = function(e) {
          job_family <- if (!is.null(job$family)) as.character(job$family) else ""
          job_label <- if (!is.null(job$label)) as.character(job$label) else job_family
          if (!length(job_label) || is.na(job_label[[1L]]) || !nzchar(job_label[[1L]])) {
            job_label <- job_family
          }
          msg <- conditionMessage(e)
          if (!is.null(fit_worker_error_log) && nzchar(fit_worker_error_log)) {
            write(
              sprintf("[%s] family=%s label=%s error=%s", Sys.time(), job_family, job_label, msg),
              file = fit_worker_error_log,
              append = TRUE
            )
          }
          list(
            model_family = job_family,
            job_label = job_label,
            status = 1L,
            error = msg
          )
        }
      )
    }
    if (workers > 1L && .Platform$OS.type != "windows") {
      parallel::mclapply(fit_jobs, function(job) safe_run(job), mc.cores = workers)
    } else {
      if (workers > 1L && .Platform$OS.type == "windows") {
        warning(
          "fit parallel workers > 1 requested on Windows; falling back to sequential execution",
          call. = FALSE
        )
      }
      lapply(fit_jobs, function(job) safe_run(job))
    }
  }

  check_fit_worker_results <- function(results, context_label) {
    if (length(results) == 0L) return(invisible(NULL))
    failures <- character(0)
    for (idx in seq_along(results)) {
      res <- results[[idx]]
      if (inherits(res, "try-error")) {
        failures <- c(
          failures,
          sprintf("%s worker #%d returned try-error: %s", context_label, idx, as.character(res))
        )
        next
      }
      if (!is.list(res)) {
        failures <- c(
          failures,
          sprintf(
            "%s worker #%d returned invalid type: %s",
            context_label,
            idx,
            paste(class(res), collapse = "/")
          )
        )
        next
      }
      if (!is.null(res$error)) {
        label <- res$job_label
        if (is.null(label) || !length(label) || is.na(label[[1L]]) || !nzchar(label[[1L]])) {
          label <- res$model_family
        }
        if (is.null(label) || !length(label) || is.na(label[[1L]]) || !nzchar(label[[1L]])) {
          label <- sprintf("worker_%d", idx)
        }
        failures <- c(failures, sprintf("%s %s: %s", context_label, label, res$error))
      }
    }
    if (length(failures) > 0L) {
      for (msg in failures) {
        append_fit_stage_log(msg)
      }
      stop(
        paste0("fit stage worker errors:\n- ", paste(failures, collapse = "\n- ")),
        call. = FALSE
      )
    }
  }

  build_fit_jobs <- function() {
    fit_jobs <- list()

    if (run_exdqlm_multivar) {
      for (mode in multivar_transfer_modes) {
        for (q in quantiles) {
          fit_jobs[[length(fit_jobs) + 1L]] <- local({
            q_local <- q
            q_num <- as.integer(round(q_local * 100))
            q_label <- sprintf("%02d", q_num)
            mode_local <- mode
            list(
              family = sprintf("exdqlm_multivar_%s", mode_local),
              label = sprintf("exdqlm_multivar_%s q=%s", mode_local, q_label),
              runner = function() run_one_quantile(q_local, mode_local)
            )
          })
        }
      }
    }
    if (run_exdqlm_univar) {
      for (q in quantiles) {
        fit_jobs[[length(fit_jobs) + 1L]] <- local({
          q_local <- q
          q_num <- as.integer(round(q_local * 100))
          q_label <- sprintf("%02d", q_num)
          list(
            family = "exdqlm_univar",
            label = sprintf("exdqlm_univar q=%s", q_label),
            runner = function() run_one_univar_quantile(q_local)
          )
        })
      }
    }
    if (run_ndlm_main) {
      fit_jobs[[length(fit_jobs) + 1L]] <- list(
        family = "ndlm_main",
        label = "ndlm_main",
        runner = function() run_ndlm_fit()
      )
    }
    if (run_ndlm_univar) {
      fit_jobs[[length(fit_jobs) + 1L]] <- list(
        family = "ndlm_univar",
        label = "ndlm_univar",
        runner = function() run_ndlm_univar_fit()
      )
    }
    fit_jobs
  }

  if (identical(fit_parallel_mode, "one_core_per_model")) {
    # In one_core_per_model mode, launch at most one worker per enabled family.
    # Quantiles for each family are executed sequentially inside that family worker.
    family_jobs <- list()
    task_count <- 0L
    if (run_exdqlm_multivar) {
      task_count <- task_count + length(quantiles) * length(multivar_transfer_modes)
      for (mode in multivar_transfer_modes) {
        family_jobs[[length(family_jobs) + 1L]] <- local({
          mode_local <- mode
          list(
            family = sprintf("exdqlm_multivar_%s", mode_local),
            runner = function() lapply(quantiles, function(q) run_one_quantile(q, mode_local))
          )
        })
      }
    }
    if (run_exdqlm_univar) {
      task_count <- task_count + length(quantiles)
      family_jobs[[length(family_jobs) + 1L]] <- list(
        family = "exdqlm_univar",
        runner = function() lapply(quantiles, run_one_univar_quantile)
      )
    }
    if (run_ndlm_main) {
      task_count <- task_count + 1L
      family_jobs[[length(family_jobs) + 1L]] <- list(
        family = "ndlm_main",
        runner = function() list(run_ndlm_fit())
      )
    }
    if (run_ndlm_univar) {
      task_count <- task_count + 1L
      family_jobs[[length(family_jobs) + 1L]] <- list(
        family = "ndlm_univar",
        runner = function() list(run_ndlm_univar_fit())
      )
    }

    workers <- as.integer(length(family_jobs))
    detected_cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
    if (is.finite(detected_cores) && detected_cores > 0L && workers > detected_cores) {
      warning(
        sprintf(
          "one_core_per_model requested %d workers but only %d cores detected; oversubscription may slow fit stage",
          workers,
          detected_cores
        ),
        call. = FALSE
      )
    }
    if (length(family_jobs) > 0L) {
      scheduler_msg <- sprintf(
        "fit scheduler mode=%s workers=%d model_jobs=%d task_jobs=%d",
        fit_parallel_mode, workers, length(family_jobs), as.integer(task_count)
      )
      message(
        sprintf(
          "%s",
          scheduler_msg
        )
      )
      append_fit_stage_log(scheduler_msg)
    }
    nested_results <- execute_fit_jobs(family_jobs, workers = workers)
    results <- unlist(nested_results, recursive = FALSE)
    check_fit_worker_results(results, fit_parallel_mode)
    for (res in results) {
      family <- as.character(res$model_family)
      if (!length(family) || is.na(family[[1]]) || !nzchar(family[[1]])) {
        stop(sprintf("fit stage %s worker returned empty model_family", fit_parallel_mode), call. = FALSE)
      }
      family <- family[[1]]
      manifest <- switch(
        family,
        exdqlm_multivar = process_multivar_result(manifest, res),
        exdqlm_univar = process_univar_result(manifest, res),
        ndlm_main = process_ndlm_result(manifest, res),
        ndlm_univar = process_ndlm_univar_result(manifest, res),
        stop(sprintf("unknown fit stage family result in %s mode: %s", fit_parallel_mode, family), call. = FALSE)
      )
    }
  } else if (identical(fit_parallel_mode, "global_models")) {
    fit_jobs <- build_fit_jobs()
    workers <- unified_resolve_fit_parallel_workers(cfg, length(fit_jobs), default_workers = default_workers)
    if (length(fit_jobs) > 0L) {
      scheduler_msg <- sprintf("fit scheduler mode=%s workers=%d jobs=%d", fit_parallel_mode, workers, length(fit_jobs))
      message(scheduler_msg)
      append_fit_stage_log(scheduler_msg)
    }
    results <- execute_fit_jobs(fit_jobs, workers = workers)
    check_fit_worker_results(results, fit_parallel_mode)
    for (res in results) {
      family <- as.character(res$model_family)
      if (!length(family) || is.na(family[[1]]) || !nzchar(family[[1]])) {
        stop(sprintf("fit stage %s worker returned empty model_family", fit_parallel_mode), call. = FALSE)
      }
      family <- family[[1]]
      manifest <- switch(
        family,
        exdqlm_multivar = process_multivar_result(manifest, res),
        exdqlm_univar = process_univar_result(manifest, res),
        ndlm_main = process_ndlm_result(manifest, res),
        ndlm_univar = process_ndlm_univar_result(manifest, res),
        stop(sprintf("unknown fit stage family result in %s mode: %s", fit_parallel_mode, family), call. = FALSE)
      )
    }
  } else {
    if (run_exdqlm_multivar) {
      for (mode in multivar_transfer_modes) {
        mode_local <- mode
        workers <- min(default_workers, length(quantiles))
        results <- execute_fit_jobs(
          lapply(quantiles, function(q) {
            q_local <- q
            q_num <- as.integer(round(q_local * 100))
            q_label <- sprintf("%02d", q_num)
            list(
              family = sprintf("exdqlm_multivar_%s", mode_local),
              label = sprintf("exdqlm_multivar_%s q=%s", mode_local, q_label),
              runner = function() run_one_quantile(q_local, mode_local)
            )
          }),
          workers = workers
        )
        check_fit_worker_results(results, fit_parallel_mode)
        for (res in results) {
          manifest <- process_multivar_result(manifest, res)
        }
      }
    }

    if (run_exdqlm_univar) {
      workers <- min(default_workers, length(quantiles))
      results <- execute_fit_jobs(
        lapply(quantiles, function(q) {
          q_local <- q
          q_num <- as.integer(round(q_local * 100))
          q_label <- sprintf("%02d", q_num)
          list(
            family = "exdqlm_univar",
            label = sprintf("exdqlm_univar q=%s", q_label),
            runner = function() run_one_univar_quantile(q_local)
          )
        }),
        workers = workers
      )
      check_fit_worker_results(results, fit_parallel_mode)
      for (res in results) {
        manifest <- process_univar_result(manifest, res)
      }
    }

    if (run_ndlm_main) {
      manifest <- process_ndlm_result(manifest, run_ndlm_fit())
    }
    if (run_ndlm_univar) {
      manifest <- process_ndlm_univar_result(manifest, run_ndlm_univar_fit())
    }
  }

  append_fit_stage_log("stage_fit complete")
  if (file.exists(fit_stage_log)) {
    manifest <- unified_manifest_add_artifact(manifest, fit_stage_log, storage_scale = "text")
  }

  if (file.exists(fit_preflight_log)) {
    manifest <- unified_manifest_add_artifact(
      manifest,
      fit_preflight_log,
      storage_scale = "text",
      role = "preflight"
    )
  }
  if (isTRUE(io_settings$enabled) && dir.exists(preflight_dir)) {
    preflight_reports <- list.files(preflight_dir, pattern = "\\.json$", full.names = TRUE, recursive = FALSE)
    preflight_reports <- preflight_reports[file.exists(preflight_reports)]
    if (length(preflight_reports) > 0L) {
      existing_paths <- unlist(lapply(manifest$artifacts, function(x) {
        val <- x$path
        if (is.null(val)) "" else as.character(val)
      }), use.names = FALSE)
      for (rp in preflight_reports) {
        if (!(rp %in% existing_paths)) {
          manifest <- unified_manifest_add_artifact(
            manifest,
            rp,
            storage_scale = "text",
            role = "preflight"
          )
        }
      }
    }
  }

  list(manifest = manifest)
}
