# unified/diagnostics.R

diag_result <- function(id, pass, detail, metrics = NULL) {
  out <- list(
    id = as.character(id),
    status = if (isTRUE(pass)) "pass" else "fail",
    detail = as.character(detail)
  )
  if (!is.null(metrics)) {
    out$metrics <- metrics
  }
  out
}

diag_default <- function(x, y) {
  if (is.null(x) || (is.character(x) && length(x) == 1L && !nzchar(x))) y else x
}

diag_collect_result <- function(store, result, severity = "error") {
  store$checks[[length(store$checks) + 1L]] <- result
  if (!identical(result$status, "pass")) {
    msg <- sprintf("%s: %s", result$id, result$detail)
    if (identical(severity, "warning")) {
      store$warnings <- c(store$warnings, msg)
    } else {
      store$errors <- c(store$errors, msg)
    }
  }
  store
}

diag_all_finite <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.list(x)) {
    if (length(x) == 0L) return(TRUE)
    return(all(vapply(x, diag_all_finite, logical(1))))
  }
  if (is.numeric(x)) {
    return(all(is.finite(x)))
  }
  TRUE
}

diag_count_non_finite <- function(x) {
  if (is.null(x) || !is.numeric(x)) return(0L)
  sum(!is.finite(x))
}

diag_check_finite <- function(x, name) {
  non_finite <- diag_count_non_finite(x)
  diag_result(
    id = sprintf("%s.finite", name),
    pass = non_finite == 0L,
    detail = if (non_finite == 0L) "all finite" else sprintf("non-finite count=%d", non_finite),
    metrics = list(non_finite = as.integer(non_finite))
  )
}

diag_check_dims <- function(x, expected, name) {
  if (is.null(expected)) expected <- list()
  dims <- dim(x)
  if (is.null(dims)) dims <- c(length(x))
  dims <- as.integer(dims)

  errs <- character(0)
  if (!is.null(expected$rank)) {
    rank_exp <- as.integer(expected$rank)
    if (length(dims) != rank_exp) {
      errs <- c(errs, sprintf("rank expected=%d got=%d", rank_exp, length(dims)))
    }
  }
  if (!is.null(expected$nrow)) {
    nrow_exp <- as.integer(expected$nrow)
    if (length(dims) < 1L || dims[1] != nrow_exp) {
      errs <- c(errs, sprintf("nrow expected=%d got=%s", nrow_exp, if (length(dims) >= 1L) dims[1] else "NA"))
    }
  }
  if (!is.null(expected$ncol)) {
    ncol_exp <- as.integer(expected$ncol)
    if (length(dims) < 2L || dims[2] != ncol_exp) {
      errs <- c(errs, sprintf("ncol expected=%d got=%s", ncol_exp, if (length(dims) >= 2L) dims[2] else "NA"))
    }
  }
  if (!is.null(expected$min)) {
    min_req <- as.integer(unlist(expected$min, use.names = FALSE))
    if (length(min_req) > length(dims)) {
      errs <- c(errs, sprintf("min dims length=%d exceeds actual rank=%d", length(min_req), length(dims)))
    } else {
      for (i in seq_along(min_req)) {
        if (dims[i] < min_req[i]) {
          errs <- c(errs, sprintf("dim[%d] expected >= %d got %d", i, min_req[i], dims[i]))
        }
      }
    }
  }
  if (!is.null(expected$contains)) {
    contains <- as.integer(unlist(expected$contains, use.names = FALSE))
    for (val in contains) {
      if (!(val %in% dims)) {
        errs <- c(errs, sprintf("expected dimension value %d not found in [%s]", val, paste(dims, collapse = "x")))
      }
    }
  }
  if (!is.null(expected$len_min)) {
    len_min <- as.integer(expected$len_min)
    if (length(x) < len_min) {
      errs <- c(errs, sprintf("length expected >= %d got %d", len_min, length(x)))
    }
  }

  diag_result(
    id = sprintf("%s.dims", name),
    pass = length(errs) == 0L,
    detail = if (length(errs) == 0L) sprintf("dims=%s", paste(dims, collapse = "x")) else paste(errs, collapse = "; "),
    metrics = list(dims = as.list(dims))
  )
}

diag_sample_time_idx <- function(Tn, max_checks, seed) {
  Tn <- as.integer(Tn)
  max_checks <- as.integer(max_checks)
  seed <- as.integer(seed)

  if (!is.finite(Tn) || Tn <= 0L) return(integer(0))
  if (!is.finite(max_checks) || max_checks <= 0L) return(integer(0))
  if (max_checks >= Tn) return(seq_len(Tn))

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  sort(sample.int(Tn, size = max_checks, replace = FALSE))
}

diag_resolve_time_idx <- function(total_slices, sample_idx, full_scan = FALSE) {
  total_slices <- as.integer(total_slices)
  if (!is.finite(total_slices) || total_slices <= 0L) return(integer(0))
  if (isTRUE(full_scan)) return(seq_len(total_slices))
  idx <- as.integer(sample_idx)
  idx <- idx[idx >= 1L & idx <= total_slices]
  unique(idx)
}

diag_check_symmetry_3d <- function(A, sample_idx, name, tol = 1e-8) {
  tol <- as.numeric(tol)
  dims <- dim(A)
  if (!is.numeric(A) || is.null(dims) || length(dims) != 3L) {
    return(diag_result(sprintf("%s.symmetry", name), FALSE, "expected numeric 3D array"))
  }
  if (dims[1] != dims[2]) {
    return(diag_result(sprintf("%s.symmetry", name), FALSE, sprintf("non-square slices: %dx%d", dims[1], dims[2])))
  }

  idx <- as.integer(sample_idx)
  idx <- idx[idx >= 1L & idx <= dims[3]]
  idx <- unique(idx)
  if (length(idx) == 0L) {
    return(diag_result(sprintf("%s.symmetry", name), TRUE, "no sampled slices"))
  }

  max_asym <- 0
  bad_idx <- integer(0)
  for (ti in idx) {
    sl <- A[, , ti, drop = TRUE]
    asym <- max(abs(sl - t(sl)))
    if (!is.finite(asym)) {
      bad_idx <- c(bad_idx, ti)
      next
    }
    if (asym > max_asym) max_asym <- asym
    if (asym > tol) bad_idx <- c(bad_idx, ti)
  }

  diag_result(
    id = sprintf("%s.symmetry", name),
    pass = length(bad_idx) == 0L,
    detail = if (length(bad_idx) == 0L) sprintf("max|A-A'|=%0.3e", max_asym) else sprintf("asymmetry above tol at slices: %s", paste(head(bad_idx, 8), collapse = ",")),
    metrics = list(max_abs_asym = max_asym, sampled_slices = as.list(idx), violating_slices = as.list(unique(bad_idx)))
  )
}

diag_check_psd_3d <- function(A, sample_idx, name, psd_tol = -1e-10, full_scan = FALSE, id_suffix = "psd") {
  psd_tol <- as.numeric(psd_tol)
  dims <- dim(A)
  if (!is.numeric(A) || is.null(dims) || length(dims) != 3L) {
    return(diag_result(sprintf("%s.%s", name, id_suffix), FALSE, "expected numeric 3D array"))
  }
  if (dims[1] != dims[2]) {
    return(diag_result(sprintf("%s.%s", name, id_suffix), FALSE, sprintf("non-square slices: %dx%d", dims[1], dims[2])))
  }

  idx <- diag_resolve_time_idx(total_slices = dims[3], sample_idx = sample_idx, full_scan = full_scan)
  if (length(idx) == 0L) {
    return(diag_result(sprintf("%s.%s", name, id_suffix), TRUE, "no sampled slices"))
  }

  min_eig <- Inf
  bad_idx <- integer(0)
  nonfinite_idx <- integer(0)
  for (ti in idx) {
    sl <- A[, , ti, drop = TRUE]
    sl <- (sl + t(sl)) / 2
    vals <- tryCatch(eigen(sl, symmetric = TRUE, only.values = TRUE)$values, error = function(e) NA_real_)
    if (any(!is.finite(vals))) {
      bad_idx <- c(bad_idx, ti)
      nonfinite_idx <- c(nonfinite_idx, ti)
      next
    }
    cur_min <- min(vals)
    if (cur_min < min_eig) min_eig <- cur_min
    if (cur_min < psd_tol) bad_idx <- c(bad_idx, ti)
  }

  if (!is.finite(min_eig)) min_eig <- NA_real_
  scan_mode <- if (isTRUE(full_scan)) "full" else "sampled"
  diag_result(
    id = sprintf("%s.%s", name, id_suffix),
    pass = length(bad_idx) == 0L,
    detail = if (length(bad_idx) == 0L) {
      sprintf("%s scan min_eig=%0.3e (tol=%0.3e)", scan_mode, min_eig, psd_tol)
    } else {
      sprintf("%s scan eigenvalue below tol at slices: %s", scan_mode, paste(head(bad_idx, 8), collapse = ","))
    },
    metrics = list(
      min_eigenvalue = min_eig,
      psd_tol = psd_tol,
      scan_mode = scan_mode,
      checked_slices_count = as.integer(length(idx)),
      total_slices = as.integer(dims[3]),
      sampled_slices = as.list(idx),
      violating_slices = as.list(unique(bad_idx)),
      nonfinite_slices = as.list(unique(nonfinite_idx))
    )
  )
}

diag_parse_summary_log <- function(log_path) {
  out <- list()
  if (is.null(log_path) || !nzchar(log_path) || !file.exists(log_path)) return(out)
  lines <- readLines(log_path, warn = FALSE)
  for (ln in lines) {
    if (!grepl("=", ln, fixed = TRUE)) next
    parts <- strsplit(ln, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2L) next
    key <- trimws(parts[[1]])
    val <- trimws(paste(parts[-1], collapse = "="))
    if (!nzchar(key)) next
    num <- suppressWarnings(as.numeric(val))
    out[[key]] <- if (is.finite(num)) num else val
  }
  out
}

diag_write_reports <- function(report, out_dir, base_name) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for diagnostics reports", call. = FALSE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  yaml_path <- file.path(out_dir, sprintf("%s_diagnostics.yaml", base_name))
  writeLines(yaml::as.yaml(report, indent.mapping.sequence = TRUE), con = yaml_path, useBytes = TRUE)

  json_path <- file.path(out_dir, sprintf("%s_diagnostics.json", base_name))
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(report, path = json_path, auto_unbox = TRUE, pretty = TRUE)
  } else {
    writeLines("{}", con = json_path, useBytes = TRUE)
  }

  list(
    yaml_path = normalizePath(yaml_path, mustWork = FALSE),
    json_path = normalizePath(json_path, mustWork = FALSE)
  )
}

diag_env_load <- function(rdata_path) {
  env <- new.env(parent = emptyenv())
  load(rdata_path, envir = env)
  env
}

diag_env_get <- function(env, name) {
  if (!exists(name, envir = env, inherits = FALSE)) return(NULL)
  get(name, envir = env, inherits = FALSE)
}

unified_diag_exdqlm_univar_theory <- function(
  rdata_path,
  q_num,
  report_dir,
  summary_log_path = NULL,
  settings = list(),
  write_reports = TRUE
) {
  q_num <- as.integer(q_num)
  max_checks <- as.integer(diag_default(settings$max_time_checks, 25L))
  seed <- as.integer(diag_default(settings$seed, 777L))
  psd_tol <- as.numeric(diag_default(settings$psd_tol, -1e-10))
  psd_warn_tol <- as.numeric(diag_default(settings$psd_warn_tol, psd_tol))
  psd_fail_tol <- as.numeric(diag_default(settings$psd_fail_tol, psd_tol))
  full_slice_psd <- isTRUE(diag_default(settings$full_slice_psd, FALSE))
  if (!is.finite(psd_warn_tol)) psd_warn_tol <- psd_tol
  if (!is.finite(psd_fail_tol)) psd_fail_tol <- psd_tol

  store <- list(checks = list(), errors = character(0), warnings = character(0))
  add <- function(res, severity = "error") {
    store <<- diag_collect_result(store, res, severity = severity)
  }

  suffix <- sprintf("%d_exAL_synth_DISC_uni", q_num)
  required <- c(
    sprintf("new.theta.out_%s", suffix),
    sprintf("samp.theta_%s", suffix),
    sprintf("samp.sigma_%s", suffix),
    sprintf("seq.elbo_%s", suffix)
  )

  env <- diag_env_load(rdata_path)
  for (nm in required) {
    add(diag_result(sprintf("univar.%s.exists", nm), exists(nm, envir = env, inherits = FALSE), if (exists(nm, envir = env, inherits = FALSE)) "present" else "missing"))
  }

  obj_new <- diag_env_get(env, required[[1]])
  obj_theta <- diag_env_get(env, required[[2]])
  obj_sigma <- diag_env_get(env, required[[3]])
  obj_elbo <- diag_env_get(env, required[[4]])

  Tn <- NA_integer_
  if (!is.null(obj_new)) {
    add(diag_result("univar.new_theta.is_list", is.list(obj_new), "expected list"))
    if (is.list(obj_new)) {
      req_fields <- c("sm", "sC", "exps")
      miss <- req_fields[!req_fields %in% names(obj_new)]
      add(diag_result("univar.new_theta.fields", length(miss) == 0L, if (length(miss) == 0L) "all required fields present" else paste("missing:", paste(miss, collapse = ","))))
      if (length(miss) == 0L) {
        sm <- obj_new$sm
        sC <- obj_new$sC
        exps <- obj_new$exps

        add(diag_check_dims(sm, expected = list(rank = 2L), name = "univar.new_theta.sm"))
        add(diag_check_finite(sm, "univar.new_theta.sm"))
        if (!is.null(dim(sm)) && length(dim(sm)) == 2L) {
          Tn <- as.integer(dim(sm)[2])
          add(diag_result("univar.new_theta.sm.T_gt_1000", is.finite(Tn) && Tn > 1000L, sprintf("T=%s", as.character(Tn))))
        }

        add(diag_check_dims(sC, expected = list(rank = 3L), name = "univar.new_theta.sC"))
        add(diag_check_finite(sC, "univar.new_theta.sC"))
        if (!is.null(dim(sC)) && length(dim(sC)) == 3L && is.finite(Tn)) {
          add(diag_result("univar.new_theta.sC.T_match", as.integer(dim(sC)[3]) == as.integer(Tn), sprintf("sC_T=%d T=%d", as.integer(dim(sC)[3]), as.integer(Tn))))
          idx <- diag_sample_time_idx(Tn, max_checks = max_checks, seed = seed)
          add(diag_check_symmetry_3d(sC, sample_idx = idx, name = "univar.new_theta.sC", tol = 1e-8))
          add(diag_check_psd_3d(
            sC,
            sample_idx = idx,
            name = "univar.new_theta.sC",
            psd_tol = psd_fail_tol,
            full_scan = full_slice_psd,
            id_suffix = "psd"
          ))
          add(diag_check_psd_3d(
            sC,
            sample_idx = idx,
            name = "univar.new_theta.sC",
            psd_tol = psd_warn_tol,
            full_scan = full_slice_psd,
            id_suffix = "psd_warn"
          ), severity = "warning")
          min_diag <- Inf
          bad_diag <- integer(0)
          for (ti in idx) {
            dvals <- diag(sC[, , ti, drop = TRUE])
            cur_min <- min(dvals)
            if (cur_min < min_diag) min_diag <- cur_min
            if (!all(dvals >= -1e-12)) bad_diag <- c(bad_diag, ti)
          }
          add(diag_result(
            "univar.new_theta.sC.diag_nonnegative",
            length(bad_diag) == 0L,
            if (length(bad_diag) == 0L) sprintf("min_diag=%0.3e", min_diag) else sprintf("negative diagonal at slices: %s", paste(head(unique(bad_diag), 8), collapse = ","))
          ))
        }

        add(diag_check_dims(exps, expected = list(rank = 2L, ncol = Tn), name = "univar.new_theta.exps"))
        add(diag_check_finite(exps, "univar.new_theta.exps"))
      }
    }
  }

  if (!is.null(obj_theta)) {
    add(diag_check_finite(obj_theta, "univar.samp.theta"))
    if (is.finite(Tn)) {
      add(diag_check_dims(obj_theta, expected = list(contains = c(Tn)), name = "univar.samp.theta"))
    }
  }
  if (!is.null(obj_sigma)) {
    add(diag_check_finite(obj_sigma, "univar.samp.sigma"))
    add(diag_result("univar.samp.sigma.positive", all(as.numeric(obj_sigma) > 0), "samp.sigma must be strictly positive"))
  }
  if (!is.null(obj_elbo)) {
    add(diag_check_finite(obj_elbo, "univar.seq.elbo"))
  }

  summary_vals <- diag_parse_summary_log(summary_log_path)
  if (length(summary_vals) > 0L) {
    sigma <- suppressWarnings(as.numeric(summary_vals$sigma))
    gamma <- suppressWarnings(as.numeric(summary_vals$gamma))
    add(diag_result("univar.summary.sigma_positive", is.finite(sigma) && sigma > 0, sprintf("sigma=%s", as.character(summary_vals$sigma))))
    add(diag_result("univar.summary.gamma_finite", is.finite(gamma), sprintf("gamma=%s", as.character(summary_vals$gamma))))
  } else {
    add(diag_result("univar.summary.log_present", FALSE, "summary log missing or unreadable"), severity = "warning")
  }

  status <- if (length(store$errors) == 0L) "pass" else "fail"
  report <- list(
    family = "exdqlm_univar",
    implementation_mode = "theory_aligned",
    quantile = q_num,
    rdata_path = normalizePath(rdata_path, mustWork = FALSE),
    summary_log_path = if (!is.null(summary_log_path) && nzchar(summary_log_path)) normalizePath(summary_log_path, mustWork = FALSE) else NULL,
    settings = list(
      max_time_checks = max_checks,
      seed = seed,
      psd_tol = psd_tol,
      psd_warn_tol = psd_warn_tol,
      psd_fail_tol = psd_fail_tol,
      full_slice_psd = full_slice_psd
    ),
    status = status,
    errors = unname(store$errors),
    warnings = unname(store$warnings),
    checks = store$checks,
    summary_values = summary_vals,
    checked_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  report_paths <- if (isTRUE(write_reports)) {
    diag_write_reports(
      report = report,
      out_dir = report_dir,
      base_name = sprintf("q%02d_exdqlm_univar", q_num)
    )
  } else {
    list(yaml_path = NULL, json_path = NULL)
  }
  report$report_paths <- report_paths
  report
}

unified_diag_ndlm_main_theory <- function(
  rdata_path,
  report_dir,
  summary_log_path = NULL,
  settings = list(),
  write_reports = TRUE
) {
  max_checks <- as.integer(diag_default(settings$max_time_checks, 25L))
  seed <- as.integer(diag_default(settings$seed, 777L))
  psd_tol <- as.numeric(diag_default(settings$psd_tol, -1e-10))
  psd_warn_tol <- as.numeric(diag_default(settings$psd_warn_tol, psd_tol))
  psd_fail_tol <- as.numeric(diag_default(settings$psd_fail_tol, psd_tol))
  full_slice_psd <- isTRUE(diag_default(settings$full_slice_psd, FALSE))
  if (!is.finite(psd_warn_tol)) psd_warn_tol <- psd_tol
  if (!is.finite(psd_fail_tol)) psd_fail_tol <- psd_tol

  store <- list(checks = list(), errors = character(0), warnings = character(0))
  add <- function(res, severity = "error") {
    store <<- diag_collect_result(store, res, severity = severity)
  }

  required <- c(
    "new.theta.out_50_NDLM_synth_DISC",
    "samp.theta_50_NDLM_synth_DISC",
    "samp.sigma_50_NDLM_synth_DISC",
    "samp.theta_ens_50_NDLM_synth_DISC",
    "seq.elbo_50_NDLM_synth_DISC",
    "seq.sigma_50_NDLM_synth_DISC",
    "delta_50_NDLM_synth_DISC",
    "ndlm_main_theory_state"
  )

  env <- diag_env_load(rdata_path)
  for (nm in required) {
    add(diag_result(sprintf("ndlm.%s.exists", nm), exists(nm, envir = env, inherits = FALSE), if (exists(nm, envir = env, inherits = FALSE)) "present" else "missing"))
  }

  obj_new <- diag_env_get(env, required[[1]])
  obj_theta <- diag_env_get(env, required[[2]])
  obj_sigma <- diag_env_get(env, required[[3]])
  obj_theta_ens <- diag_env_get(env, required[[4]])
  obj_elbo <- diag_env_get(env, required[[5]])
  obj_seq_sigma <- diag_env_get(env, required[[6]])
  obj_seq_scale <- diag_env_get(env, "seq.scale_50_NDLM_synth_DISC")
  obj_delta <- diag_env_get(env, required[[7]])
  obj_state <- diag_env_get(env, required[[8]])

  Tn <- NA_integer_
  Kn <- NA_integer_
  sm_k <- integer(0)
  sc_k <- integer(0)
  if (!is.null(obj_new)) {
    add(diag_result("ndlm.new_theta.is_list", is.list(obj_new), "expected list"))
    if (is.list(obj_new)) {
      req_fields <- c("sm", "sC", "sm_ens", "sC_ens", "exps", "standard_forecast_errors")
      miss <- req_fields[!req_fields %in% names(obj_new)]
      add(diag_result("ndlm.new_theta.fields", length(miss) == 0L, if (length(miss) == 0L) "all required fields present" else paste("missing:", paste(miss, collapse = ","))))
      if (length(miss) == 0L) {
        sm <- obj_new$sm
        sC <- obj_new$sC
        sm_ens <- obj_new$sm_ens
        sC_ens <- obj_new$sC_ens
        exps <- obj_new$exps
        standard_forecast_errors <- obj_new$standard_forecast_errors

        add(diag_check_dims(sm, expected = list(rank = 2L), name = "ndlm.new_theta.sm"))
        add(diag_check_finite(sm, "ndlm.new_theta.sm"))
        if (!is.null(dim(sm)) && length(dim(sm)) == 2L) {
          Tn <- as.integer(dim(sm)[2])
          add(diag_result("ndlm.new_theta.sm.T_gt_1000", is.finite(Tn) && Tn > 1000L, sprintf("T=%s", as.character(Tn))))
        }

        add(diag_check_dims(sC, expected = list(rank = 3L), name = "ndlm.new_theta.sC"))
        add(diag_check_finite(sC, "ndlm.new_theta.sC"))
        if (!is.null(dim(sC)) && length(dim(sC)) == 3L && is.finite(Tn)) {
          add(diag_result("ndlm.new_theta.sC.T_match", as.integer(dim(sC)[3]) == as.integer(Tn), sprintf("sC_T=%d T=%d", as.integer(dim(sC)[3]), as.integer(Tn))))
          idx <- diag_sample_time_idx(Tn, max_checks = max_checks, seed = seed)
          add(diag_check_symmetry_3d(sC, sample_idx = idx, name = "ndlm.new_theta.sC", tol = 1e-8))
          add(diag_check_psd_3d(
            sC,
            sample_idx = idx,
            name = "ndlm.new_theta.sC",
            psd_tol = psd_fail_tol,
            full_scan = full_slice_psd,
            id_suffix = "psd"
          ))
          add(diag_check_psd_3d(
            sC,
            sample_idx = idx,
            name = "ndlm.new_theta.sC",
            psd_tol = psd_warn_tol,
            full_scan = full_slice_psd,
            id_suffix = "psd_warn"
          ), severity = "warning")
        }

        add(diag_result("ndlm.new_theta.sm_ens.is_list", is.list(sm_ens), "sm_ens must be list"))
        add(diag_result("ndlm.new_theta.sC_ens.is_list", is.list(sC_ens), "sC_ens must be list"))
        if (is.list(sm_ens) && is.list(sC_ens)) {
          add(diag_result("ndlm.new_theta.ens_list_length_match", length(sm_ens) == length(sC_ens), sprintf("sm_ens=%d sC_ens=%d", length(sm_ens), length(sC_ens))))
          if (length(sm_ens) > 0L) {
            keep_idx <- unique(c(seq_len(min(2L, length(sm_ens))), length(sm_ens)))
            keep_idx <- keep_idx[keep_idx >= 1L & keep_idx <= length(sm_ens)]
            for (i in keep_idx) {
              sm_i <- sm_ens[[i]]
              sC_i <- sC_ens[[i]]
              add(diag_check_dims(sm_i, expected = list(rank = 2L), name = sprintf("ndlm.new_theta.sm_ens[%d]", i)))
              add(diag_check_finite(sm_i, sprintf("ndlm.new_theta.sm_ens[%d]", i)))
              add(diag_check_dims(sC_i, expected = list(rank = 3L), name = sprintf("ndlm.new_theta.sC_ens[%d]", i)))
              add(diag_check_finite(sC_i, sprintf("ndlm.new_theta.sC_ens[%d]", i)))
              if (!is.null(dim(sC_i)) && length(dim(sC_i)) == 3L) {
                Kc <- as.integer(dim(sC_i)[3])
                idx_k <- diag_sample_time_idx(Kc, max_checks = min(max_checks, 5L), seed = seed + i)
                add(diag_check_symmetry_3d(sC_i, sample_idx = idx_k, name = sprintf("ndlm.new_theta.sC_ens[%d]", i), tol = 1e-8))
                add(diag_check_psd_3d(
                  sC_i,
                  sample_idx = idx_k,
                  name = sprintf("ndlm.new_theta.sC_ens[%d]", i),
                  psd_tol = psd_fail_tol,
                  full_scan = full_slice_psd,
                  id_suffix = "psd"
                ))
                add(diag_check_psd_3d(
                  sC_i,
                  sample_idx = idx_k,
                  name = sprintf("ndlm.new_theta.sC_ens[%d]", i),
                  psd_tol = psd_warn_tol,
                  full_scan = full_slice_psd,
                  id_suffix = "psd_warn"
                ), severity = "warning")
              }
            }
          }
        }

        add(diag_check_dims(exps, expected = list(rank = 2L, ncol = Tn), name = "ndlm.new_theta.exps"))
        add(diag_check_finite(exps, "ndlm.new_theta.exps"))

        add(diag_check_dims(standard_forecast_errors, expected = list(rank = 2L), name = "ndlm.new_theta.standard_forecast_errors"))
        add(diag_check_finite(standard_forecast_errors, "ndlm.new_theta.standard_forecast_errors"))
        sfe_dim <- dim(standard_forecast_errors)
        if (!is.null(sfe_dim) && length(sfe_dim) == 2L) {
          Kn <- as.integer(sfe_dim[2])
          add(diag_result("ndlm.new_theta.standard_forecast_errors.rows_ge_1", as.integer(sfe_dim[1]) >= 1L, sprintf("rows=%d", as.integer(sfe_dim[1]))))
          add(diag_result("ndlm.new_theta.standard_forecast_errors.K_ge_1", is.finite(Kn) && Kn >= 1L, sprintf("K=%s", as.character(Kn))))
        }

        if (is.list(sm_ens) && length(sm_ens) > 0L) {
          sm_k <- vapply(sm_ens, function(x) {
            d <- dim(x)
            if (is.null(d) || length(d) != 2L) return(NA_integer_)
            as.integer(d[2])
          }, integer(1))
          add(diag_result(
            "ndlm.new_theta.sm_ens.segment_lengths_valid",
            all(is.finite(sm_k)) && all(sm_k >= 0L),
            sprintf("sm_ens K=[%s]", paste(sm_k, collapse = ","))
          ))
        }

        if (is.list(sC_ens) && length(sC_ens) > 0L) {
          sc_k <- vapply(sC_ens, function(x) {
            d <- dim(x)
            if (is.null(d) || length(d) != 3L) return(NA_integer_)
            as.integer(d[3])
          }, integer(1))
          add(diag_result(
            "ndlm.new_theta.sC_ens.segment_lengths_valid",
            all(is.finite(sc_k)) && all(sc_k >= 0L),
            sprintf("sC_ens K=[%s]", paste(sc_k, collapse = ","))
          ))
        }
        if (length(sm_k) > 0L && length(sc_k) > 0L) {
          add(diag_result(
            "ndlm.new_theta.sm_sC_segment_profile_match",
            length(sm_k) == length(sc_k) && all(sm_k == sc_k),
            sprintf("sm_ens K=[%s], sC_ens K=[%s]", paste(sm_k, collapse = ","), paste(sc_k, collapse = ","))
          ))
        }
        if (is.finite(Kn) && length(sm_k) > 0L) {
          add(diag_result(
            "ndlm.new_theta.sm_ens.segment_sum_matches_standard_errors",
            all(is.finite(sm_k)) && sum(sm_k) == Kn,
            sprintf("sum(sm_ens K)=%s, standard_forecast_errors K=%s", as.character(sum(sm_k)), as.character(Kn))
          ))
        }
        if (is.finite(Kn) && length(sc_k) > 0L) {
          add(diag_result(
            "ndlm.new_theta.sC_ens.segment_sum_matches_standard_errors",
            all(is.finite(sc_k)) && sum(sc_k) == Kn,
            sprintf("sum(sC_ens K)=%s, standard_forecast_errors K=%s", as.character(sum(sc_k)), as.character(Kn))
          ))
        }
      }
    }
  }

  if (!is.null(obj_state)) {
    add(diag_result("ndlm.theory_state.is_list", is.list(obj_state), "ndlm_main_theory_state must be a list"))
    if (is.list(obj_state)) {
      req_state <- c("K", "K_overlap", "K_max", "K_vec", "segment_lengths", "K_cap", "nws_len", "glofas_len")
      miss_state <- req_state[!req_state %in% names(obj_state)]
      add(diag_result(
        "ndlm.theory_state.required_fields",
        length(miss_state) == 0L,
        if (length(miss_state) == 0L) "all required fields present" else paste("missing:", paste(miss_state, collapse = ","))
      ))
      if (length(miss_state) == 0L) {
        read_named_int <- function(x, name) {
          if (is.null(x)) return(NA_integer_)
          if (length(x) == 0L) return(NA_integer_)
          if (!is.null(names(x)) && (name %in% names(x))) {
            return(suppressWarnings(as.integer(x[[name]])))
          }
          suppressWarnings(as.integer(x[[1L]]))
        }
        K_state <- suppressWarnings(as.integer(obj_state$K[[1L]]))
        K_overlap <- suppressWarnings(as.integer(obj_state$K_overlap[[1L]]))
        K_max <- suppressWarnings(as.integer(obj_state$K_max[[1L]]))
        K_cap <- suppressWarnings(as.integer(obj_state$K_cap[[1L]]))
        nws_len <- suppressWarnings(as.integer(obj_state$nws_len[[1L]]))
        glofas_len <- suppressWarnings(as.integer(obj_state$glofas_len[[1L]]))
        k_nws_cap <- suppressWarnings(as.integer(min(nws_len, K_cap)))
        k_glofas_cap <- suppressWarnings(as.integer(min(glofas_len, K_cap)))
        K_expected_max <- suppressWarnings(as.integer(max(k_nws_cap, k_glofas_cap)))
        K_expected_overlap <- suppressWarnings(as.integer(min(k_nws_cap, k_glofas_cap)))
        K_vec <- obj_state$K_vec
        segment_lengths <- obj_state$segment_lengths
        k_vec_nws <- read_named_int(K_vec, "nws")
        k_vec_glofas <- read_named_int(K_vec, "glofas")
        seg_overlap <- read_named_int(segment_lengths, "overlap")
        seg_extension <- read_named_int(segment_lengths, "extension")
        add(diag_result("ndlm.theory_state.K_finite", is.finite(K_state) && K_state >= 1L, sprintf("K=%s", as.character(obj_state$K))))
        add(diag_result("ndlm.theory_state.K_overlap_finite", is.finite(K_overlap) && K_overlap >= 1L, sprintf("K_overlap=%s", as.character(obj_state$K_overlap))))
        add(diag_result("ndlm.theory_state.K_max_finite", is.finite(K_max) && K_max >= 1L, sprintf("K_max=%s", as.character(obj_state$K_max))))
        add(diag_result("ndlm.theory_state.K_cap_positive", is.finite(K_cap) && K_cap >= 1L, sprintf("K_cap=%s", as.character(obj_state$K_cap))))
        add(diag_result("ndlm.theory_state.nws_len_positive", is.finite(nws_len) && nws_len >= 1L, sprintf("nws_len=%s", as.character(obj_state$nws_len))))
        add(diag_result("ndlm.theory_state.glofas_len_positive", is.finite(glofas_len) && glofas_len >= 1L, sprintf("glofas_len=%s", as.character(obj_state$glofas_len))))
        add(diag_result(
          "ndlm.theory_state.K_vec_expected_match",
          is.finite(k_vec_nws) && is.finite(k_vec_glofas) &&
            k_vec_nws == k_nws_cap && k_vec_glofas == k_glofas_cap,
          sprintf("K_vec=(nws=%s,glofas=%s) expected=(%s,%s)", as.character(k_vec_nws), as.character(k_vec_glofas), as.character(k_nws_cap), as.character(k_glofas_cap))
        ))
        add(diag_result(
          "ndlm.theory_state.K_overlap_expected_match",
          is.finite(K_overlap) && is.finite(K_expected_overlap) && K_overlap == K_expected_overlap,
          sprintf("K_overlap=%s expected=min(%s,%s)=%s", as.character(K_overlap), as.character(k_nws_cap), as.character(k_glofas_cap), as.character(K_expected_overlap))
        ))
        add(diag_result(
          "ndlm.theory_state.K_max_expected_match",
          is.finite(K_max) && is.finite(K_expected_max) && K_max == K_expected_max,
          sprintf("K_max=%s expected=max(%s,%s)=%s", as.character(K_max), as.character(k_nws_cap), as.character(k_glofas_cap), as.character(K_expected_max))
        ))
        add(diag_result(
          "ndlm.theory_state.segment_lengths_consistent",
          is.finite(seg_overlap) && is.finite(seg_extension) &&
            seg_overlap == K_overlap && seg_overlap + seg_extension == K_max,
          sprintf("segment_lengths=(overlap=%s,extension=%s), K_overlap=%s, K_max=%s", as.character(seg_overlap), as.character(seg_extension), as.character(K_overlap), as.character(K_max))
        ))
        add(diag_result(
          "ndlm.theory_state.K_alias_matches_K_max",
          is.finite(K_state) && is.finite(K_max) && K_state == K_max,
          sprintf("K=%s K_max=%s", as.character(K_state), as.character(K_max))
        ))
        if (is.finite(Kn)) {
          add(diag_result(
            "ndlm.theory_state.K_matches_standard_errors",
            K_max == Kn,
            sprintf("K_max=%s standard_forecast_errors.K=%s", as.character(K_max), as.character(Kn))
          ))
        }
        if (is.finite(Kn) && length(sm_k) > 0L) {
          add(diag_result(
            "ndlm.theory_state.segment_sum_matches_standard_errors",
            sum(sm_k) == Kn,
            sprintf("sum(sm_ens K)=%s standard_forecast_errors.K=%s", as.character(sum(sm_k)), as.character(Kn))
          ))
        }
      }
      if ("sigma_by_source" %in% names(obj_state)) {
        sbs <- suppressWarnings(as.numeric(obj_state$sigma_by_source))
        add(diag_result("ndlm.theory_state.sigma_by_source.len_ge_3", length(sbs) >= 3L, sprintf("length=%d", length(sbs))))
        add(diag_result("ndlm.theory_state.sigma_by_source.finite", all(is.finite(sbs)), "sigma_by_source must be finite"))
        add(diag_result("ndlm.theory_state.sigma_by_source.positive", all(sbs > 0), "sigma_by_source must be strictly positive"))
      }
    }
  }

  if (!is.null(obj_theta)) {
    target <- if (is.list(obj_theta) && "samp_theta" %in% names(obj_theta)) obj_theta$samp_theta else obj_theta
    add(diag_check_finite(target, "ndlm.samp.theta"))
    if (is.finite(Tn)) {
      add(diag_check_dims(target, expected = list(contains = c(Tn)), name = "ndlm.samp.theta"))
    }
  }
  if (!is.null(obj_sigma)) {
    target <- if (is.list(obj_sigma) && "samp_sigma" %in% names(obj_sigma)) obj_sigma$samp_sigma else obj_sigma
    add(diag_check_finite(target, "ndlm.samp.sigma"))
    add(diag_result("ndlm.samp.sigma.positive", all(as.numeric(target) > 0), "samp.sigma must be strictly positive"))
  }
  if (!is.null(obj_theta_ens)) {
    leaves <- list()
    walk <- function(x) {
      if (is.list(x)) {
        for (y in x) walk(y)
      } else if (is.numeric(x)) {
        leaves[[length(leaves) + 1L]] <<- x
      }
    }
    walk(obj_theta_ens)
    add(diag_result("ndlm.samp.theta.ens.numeric_leaves", length(leaves) > 0L, sprintf("leaf_count=%d", length(leaves))))
    if (length(leaves) > 0L) {
      all_finite <- all(vapply(leaves, diag_all_finite, logical(1)))
      add(diag_result("ndlm.samp.theta.ens.finite", all_finite, if (all_finite) "all finite" else "contains non-finite leaves"))
    }
  }

  if (!is.null(obj_elbo)) add(diag_check_finite(obj_elbo, "ndlm.seq.elbo"))
  if (!is.null(obj_seq_sigma)) add(diag_check_finite(obj_seq_sigma, "ndlm.seq.sigma"))
  if (!is.null(obj_seq_scale)) add(diag_check_finite(obj_seq_scale, "ndlm.seq.scale"))
  if (!is.null(obj_delta)) {
    add(diag_check_finite(obj_delta, "ndlm.delta"))
    delta_vals <- as.numeric(obj_delta)
    delta_vals <- delta_vals[is.finite(delta_vals)]
    if (length(delta_vals) > 0L) {
      neg_share <- mean(delta_vals < 0)
      pos_share <- mean(delta_vals > 0)
      zero_share <- mean(delta_vals == 0)
      add(diag_result(
        "ndlm.delta.sign_balance",
        (neg_share > 0) && (pos_share > 0),
        sprintf("neg_share=%.3f pos_share=%.3f zero_share=%.3f", neg_share, pos_share, zero_share)
      ), severity = "warning")
    }
  }

  summary_vals <- diag_parse_summary_log(summary_log_path)
  if (length(summary_vals) > 0L) {
    sigma <- suppressWarnings(as.numeric(summary_vals$sigma))
    w_hist <- suppressWarnings(as.numeric(summary_vals$w_hist))
    w_fore <- suppressWarnings(as.numeric(summary_vals$w_fore))
    add(diag_result("ndlm.summary.sigma_positive", is.finite(sigma) && sigma > 0, sprintf("sigma=%s", as.character(summary_vals$sigma))))
    if (is.finite(w_hist) && is.finite(w_fore)) {
      add(diag_result("ndlm.summary.w_hist_nonnegative", w_hist >= 0, sprintf("w_hist=%s", as.character(summary_vals$w_hist))))
      add(diag_result("ndlm.summary.w_fore_nonnegative", w_fore >= 0, sprintf("w_fore=%s", as.character(summary_vals$w_fore))))
    } else {
      add(diag_result("ndlm.summary.w_hist_nonnegative", TRUE, "w_hist not emitted; using discount-factor contract"))
      add(diag_result("ndlm.summary.w_fore_nonnegative", TRUE, "w_fore not emitted; using discount-factor contract"))
    }
    for (nm in c("df_t", "df_s1", "df_s2", "df_s67", "df_discrep", "lambda", "df_trans", "df_covs")) {
      cur <- suppressWarnings(as.numeric(summary_vals[[nm]]))
      add(diag_result(
        sprintf("ndlm.summary.%s_in_unit_interval", nm),
        is.finite(cur) && cur > 0 && cur < 1,
        sprintf("%s=%s", nm, as.character(summary_vals[[nm]]))
      ))
    }
    for (nm in c("sigma_usgs", "sigma_nws", "sigma_glofas", "sigma_mean")) {
      cur <- suppressWarnings(as.numeric(summary_vals[[nm]]))
      if (is.finite(cur)) {
        add(diag_result(
          sprintf("ndlm.summary.%s_positive", nm),
          cur > 0,
          sprintf("%s=%s", nm, as.character(summary_vals[[nm]]))
        ))
      }
    }
  } else {
    add(diag_result("ndlm.summary.log_present", FALSE, "summary log missing or unreadable"))
  }

  status <- if (length(store$errors) == 0L) "pass" else "fail"
  report <- list(
    family = "ndlm_main",
    implementation_mode = "theory_aligned",
    rdata_path = normalizePath(rdata_path, mustWork = FALSE),
    summary_log_path = if (!is.null(summary_log_path) && nzchar(summary_log_path)) normalizePath(summary_log_path, mustWork = FALSE) else NULL,
    settings = list(
      max_time_checks = max_checks,
      seed = seed,
      psd_tol = psd_tol,
      psd_warn_tol = psd_warn_tol,
      psd_fail_tol = psd_fail_tol,
      full_slice_psd = full_slice_psd
    ),
    status = status,
    errors = unname(store$errors),
    warnings = unname(store$warnings),
    checks = store$checks,
    summary_values = summary_vals,
    checked_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  report_paths <- if (isTRUE(write_reports)) {
    diag_write_reports(
      report = report,
      out_dir = report_dir,
      base_name = "ndlm_main"
    )
  } else {
    list(yaml_path = NULL, json_path = NULL)
  }
  report$report_paths <- report_paths
  report
}

unified_diag_ndlm_univar_theory <- function(
  rdata_path,
  report_dir,
  summary_log_path = NULL,
  settings = list(),
  write_reports = TRUE
) {
  max_checks <- as.integer(diag_default(settings$max_time_checks, 25L))
  seed <- as.integer(diag_default(settings$seed, 777L))
  psd_tol <- as.numeric(diag_default(settings$psd_tol, -1e-10))
  psd_warn_tol <- as.numeric(diag_default(settings$psd_warn_tol, psd_tol))
  psd_fail_tol <- as.numeric(diag_default(settings$psd_fail_tol, psd_tol))
  full_slice_psd <- isTRUE(diag_default(settings$full_slice_psd, FALSE))
  if (!is.finite(psd_warn_tol)) psd_warn_tol <- psd_tol
  if (!is.finite(psd_fail_tol)) psd_fail_tol <- psd_tol

  store <- list(checks = list(), errors = character(0), warnings = character(0))
  add <- function(res, severity = "error") {
    store <<- diag_collect_result(store, res, severity = severity)
  }

  required <- c(
    "new.theta.out_50_NDLM_univar_synth_DISC",
    "samp.theta_50_NDLM_univar_synth_DISC",
    "samp.sigma_50_NDLM_univar_synth_DISC",
    "samp.theta.ens_50_NDLM_univar_synth_DISC",
    "seq.elbo_50_NDLM_univar_synth_DISC",
    "seq.sigma_50_NDLM_univar_synth_DISC",
    "seq.scale_50_NDLM_univar_synth_DISC",
    "delta_50_NDLM_univar_synth_DISC",
    "y.fore.draws_50_NDLM_univar_synth_DISC",
    "ndlm_univar_theory_state"
  )

  env <- diag_env_load(rdata_path)
  for (nm in required) {
    add(diag_result(
      sprintf("ndlm_univar.%s.exists", nm),
      exists(nm, envir = env, inherits = FALSE),
      if (exists(nm, envir = env, inherits = FALSE)) "present" else "missing"
    ))
  }

  obj_new <- diag_env_get(env, required[[1]])
  obj_theta <- diag_env_get(env, required[[2]])
  obj_sigma <- diag_env_get(env, required[[3]])
  obj_theta_ens <- diag_env_get(env, required[[4]])
  obj_elbo <- diag_env_get(env, required[[5]])
  obj_seq_sigma <- diag_env_get(env, required[[6]])
  obj_seq_scale <- diag_env_get(env, required[[7]])
  obj_delta <- diag_env_get(env, required[[8]])
  obj_y_fore <- diag_env_get(env, required[[9]])
  obj_state <- diag_env_get(env, required[[10]])

  Tn <- NA_integer_
  Kn <- NA_integer_
  if (!is.null(obj_new)) {
    add(diag_result("ndlm_univar.new_theta.is_list", is.list(obj_new), "expected list"))
    if (is.list(obj_new)) {
      req_fields <- c("sm", "sC", "sm_ens", "sC_ens", "exps", "standard_forecast_errors")
      miss <- req_fields[!req_fields %in% names(obj_new)]
      add(diag_result(
        "ndlm_univar.new_theta.fields",
        length(miss) == 0L,
        if (length(miss) == 0L) "all required fields present" else paste("missing:", paste(miss, collapse = ","))
      ))
      if (length(miss) == 0L) {
        sm <- obj_new$sm
        sC <- obj_new$sC
        sm_ens <- obj_new$sm_ens
        sC_ens <- obj_new$sC_ens
        exps <- obj_new$exps
        sfe <- obj_new$standard_forecast_errors

        add(diag_check_dims(sm, expected = list(rank = 2L), name = "ndlm_univar.new_theta.sm"))
        add(diag_check_finite(sm, "ndlm_univar.new_theta.sm"))
        if (!is.null(dim(sm)) && length(dim(sm)) == 2L) {
          Tn <- as.integer(dim(sm)[2])
        }

        add(diag_check_dims(sC, expected = list(rank = 3L), name = "ndlm_univar.new_theta.sC"))
        add(diag_check_finite(sC, "ndlm_univar.new_theta.sC"))
        if (!is.null(dim(sC)) && length(dim(sC)) == 3L && is.finite(Tn)) {
          add(diag_result(
            "ndlm_univar.new_theta.sC.T_match",
            as.integer(dim(sC)[3]) == as.integer(Tn),
            sprintf("sC_T=%d T=%d", as.integer(dim(sC)[3]), as.integer(Tn))
          ))
          idx <- diag_sample_time_idx(Tn, max_checks = max_checks, seed = seed)
          add(diag_check_symmetry_3d(sC, sample_idx = idx, name = "ndlm_univar.new_theta.sC", tol = 1e-8))
          add(diag_check_psd_3d(
            sC,
            sample_idx = idx,
            name = "ndlm_univar.new_theta.sC",
            psd_tol = psd_fail_tol,
            full_scan = full_slice_psd,
            id_suffix = "psd"
          ))
          add(diag_check_psd_3d(
            sC,
            sample_idx = idx,
            name = "ndlm_univar.new_theta.sC",
            psd_tol = psd_warn_tol,
            full_scan = full_slice_psd,
            id_suffix = "psd_warn"
          ), severity = "warning")
        }

        add(diag_result("ndlm_univar.new_theta.sm_ens.is_list", is.list(sm_ens), "sm_ens must be list"))
        add(diag_result("ndlm_univar.new_theta.sC_ens.is_list", is.list(sC_ens), "sC_ens must be list"))
        if (is.list(sm_ens) && is.list(sC_ens)) {
          add(diag_result(
            "ndlm_univar.new_theta.ens_list_length_match",
            length(sm_ens) == length(sC_ens),
            sprintf("sm_ens=%d sC_ens=%d", length(sm_ens), length(sC_ens))
          ))
          for (i in seq_len(min(length(sm_ens), length(sC_ens)))) {
            sm_i <- sm_ens[[i]]
            sC_i <- sC_ens[[i]]
            add(diag_check_dims(sm_i, expected = list(rank = 2L), name = sprintf("ndlm_univar.new_theta.sm_ens[%d]", i)))
            add(diag_check_finite(sm_i, sprintf("ndlm_univar.new_theta.sm_ens[%d]", i)))
            add(diag_check_dims(sC_i, expected = list(rank = 3L), name = sprintf("ndlm_univar.new_theta.sC_ens[%d]", i)))
            add(diag_check_finite(sC_i, sprintf("ndlm_univar.new_theta.sC_ens[%d]", i)))
            if (!is.null(dim(sC_i)) && length(dim(sC_i)) == 3L) {
              Kc <- as.integer(dim(sC_i)[3])
              idx_k <- diag_sample_time_idx(Kc, max_checks = min(max_checks, 10L), seed = seed + i)
              add(diag_check_symmetry_3d(sC_i, sample_idx = idx_k, name = sprintf("ndlm_univar.new_theta.sC_ens[%d]", i), tol = 1e-8))
              add(diag_check_psd_3d(
                sC_i,
                sample_idx = idx_k,
                name = sprintf("ndlm_univar.new_theta.sC_ens[%d]", i),
                psd_tol = psd_fail_tol,
                full_scan = full_slice_psd,
                id_suffix = "psd"
              ))
              add(diag_check_psd_3d(
                sC_i,
                sample_idx = idx_k,
                name = sprintf("ndlm_univar.new_theta.sC_ens[%d]", i),
                psd_tol = psd_warn_tol,
                full_scan = full_slice_psd,
                id_suffix = "psd_warn"
              ), severity = "warning")
            }
          }
        }

        add(diag_check_dims(exps, expected = list(rank = 2L, ncol = Tn), name = "ndlm_univar.new_theta.exps"))
        add(diag_check_finite(exps, "ndlm_univar.new_theta.exps"))

        add(diag_check_dims(sfe, expected = list(rank = 2L), name = "ndlm_univar.new_theta.standard_forecast_errors"))
        add(diag_check_finite(sfe, "ndlm_univar.new_theta.standard_forecast_errors"))
        sfe_dim <- dim(sfe)
        if (!is.null(sfe_dim) && length(sfe_dim) == 2L) {
          Kn <- as.integer(sfe_dim[2])
          add(diag_result("ndlm_univar.new_theta.standard_forecast_errors.K_ge_1", is.finite(Kn) && Kn >= 1L, sprintf("K=%s", as.character(Kn))))
        }
      }
    }
  }

  if (!is.null(obj_theta)) {
    target <- if (is.list(obj_theta) && ("samp_theta" %in% names(obj_theta))) obj_theta$samp_theta else obj_theta
    add(diag_check_finite(target, "ndlm_univar.samp.theta"))
    if (is.finite(Tn)) {
      add(diag_check_dims(target, expected = list(contains = c(Tn)), name = "ndlm_univar.samp.theta"))
    }
  }
  if (!is.null(obj_sigma)) {
    target <- if (is.list(obj_sigma) && ("samp_sigma" %in% names(obj_sigma))) obj_sigma$samp_sigma else obj_sigma
    add(diag_check_finite(target, "ndlm_univar.samp.sigma"))
    add(diag_result("ndlm_univar.samp.sigma.positive", all(as.numeric(target) > 0), "samp.sigma must be strictly positive"))
  }
  if (!is.null(obj_theta_ens)) {
    leaves <- list()
    walk <- function(x) {
      if (is.list(x)) {
        if (length(x) == 0L) return(invisible(NULL))
        for (y in x) walk(y)
      } else if (is.numeric(x)) {
        leaves[[length(leaves) + 1L]] <<- x
      }
      invisible(NULL)
    }
    walk(obj_theta_ens)
    add(diag_result("ndlm_univar.samp.theta.ens.numeric_leaves", length(leaves) > 0L, sprintf("leaf_count=%d", length(leaves))))
    if (length(leaves) > 0L) {
      add(diag_result(
        "ndlm_univar.samp.theta.ens.finite",
        all(vapply(leaves, diag_all_finite, logical(1))),
        "samp.theta.ens leaves must be finite"
      ))
    }
  }

  for (nm in c("seq_elbo", "seq_sigma", "seq_scale", "delta")) {
    obj <- switch(
      nm,
      seq_elbo = obj_elbo,
      seq_sigma = obj_seq_sigma,
      seq_scale = obj_seq_scale,
      delta = obj_delta
    )
    if (!is.null(obj)) {
      add(diag_check_finite(obj, sprintf("ndlm_univar.%s", nm)))
    }
  }
  if (!is.null(obj_y_fore)) {
    add(diag_check_dims(obj_y_fore, expected = list(rank = 2L), name = "ndlm_univar.y_fore_draws"))
    add(diag_check_finite(obj_y_fore, "ndlm_univar.y_fore_draws"))
    if (!is.null(dim(obj_y_fore)) && length(dim(obj_y_fore)) == 2L && is.finite(Kn)) {
      add(diag_result(
        "ndlm_univar.y_fore_draws.K_match",
        as.integer(dim(obj_y_fore)[2]) == as.integer(Kn),
        sprintf("y.fore.draws K=%d expected K=%d", as.integer(dim(obj_y_fore)[2]), as.integer(Kn))
      ))
    }
  }

  if (!is.null(obj_state)) {
    add(diag_result("ndlm_univar.theory_state.is_list", is.list(obj_state), "ndlm_univar_theory_state must be a list"))
    if (is.list(obj_state)) {
      add(diag_result(
        "ndlm_univar.theory_state.transfer_flag",
        "transfer_active_forecast_window" %in% names(obj_state),
        "missing transfer_active_forecast_window"
      ))
      K_state <- suppressWarnings(as.integer(obj_state$K))
      add(diag_result("ndlm_univar.theory_state.K_positive", is.finite(K_state) && K_state >= 1L, sprintf("K=%s", as.character(obj_state$K))))
    }
  }

  summary_vals <- diag_parse_summary_log(summary_log_path)
  if (length(summary_vals) > 0L) {
    sigma <- suppressWarnings(as.numeric(summary_vals$sigma))
    add(diag_result("ndlm_univar.summary.sigma_positive", is.finite(sigma) && sigma > 0, sprintf("sigma=%s", as.character(summary_vals$sigma))))
    for (nm in c("df_t", "df_s1", "df_s2", "df_s67", "lambda", "df_trans", "df_covs")) {
      cur <- suppressWarnings(as.numeric(summary_vals[[nm]]))
      if (is.finite(cur)) {
        add(diag_result(
          sprintf("ndlm_univar.summary.%s_in_unit_interval", nm),
          cur > 0 && cur < 1,
          sprintf("%s=%s", nm, as.character(summary_vals[[nm]]))
        ))
      }
    }
  } else {
    add(diag_result("ndlm_univar.summary.log_present", FALSE, "summary log missing or unreadable"), severity = "warning")
  }

  status <- if (length(store$errors) == 0L) "pass" else "fail"
  report <- list(
    family = "ndlm_univar",
    implementation_mode = "theory_aligned_closed_form",
    rdata_path = normalizePath(rdata_path, mustWork = FALSE),
    summary_log_path = if (!is.null(summary_log_path) && nzchar(summary_log_path)) normalizePath(summary_log_path, mustWork = FALSE) else NULL,
    settings = list(
      max_time_checks = max_checks,
      seed = seed,
      psd_tol = psd_tol,
      psd_warn_tol = psd_warn_tol,
      psd_fail_tol = psd_fail_tol,
      full_slice_psd = full_slice_psd
    ),
    status = status,
    errors = unname(store$errors),
    warnings = unname(store$warnings),
    checks = store$checks,
    summary_values = summary_vals,
    checked_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  report_paths <- if (isTRUE(write_reports)) {
    diag_write_reports(
      report = report,
      out_dir = report_dir,
      base_name = "ndlm_univar"
    )
  } else {
    list(yaml_path = NULL, json_path = NULL)
  }
  report$report_paths <- report_paths
  report
}
