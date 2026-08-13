# unified/contract_checks.R

unified_contract_env_load <- function(rdata_path) {
  env <- new.env(parent = emptyenv())
  load(rdata_path, envir = env)
  env
}

unified_contract_object <- function(env, name) {
  if (!exists(name, envir = env, inherits = FALSE)) return(NULL)
  get(name, envir = env, inherits = FALSE)
}

unified_contract_all_finite <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.list(x)) {
    if (length(x) == 0L) return(TRUE)
    return(all(vapply(x, unified_contract_all_finite, logical(1))))
  }
  if (is.numeric(x)) {
    return(all(is.finite(x)))
  }
  TRUE
}

unified_contract_ndims <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    if (length(x) == 0L) return(integer(0))
    return(length(x))
  }
  d
}

unified_contract_write_report <- function(report, report_dir, stem, write_reports = TRUE) {
  out <- list(yaml_path = NULL, json_path = NULL)
  if (!isTRUE(write_reports)) {
    return(out)
  }
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

  yaml_path <- file.path(report_dir, sprintf("%s_contract_check.yaml", stem))
  writeLines(yaml::as.yaml(report, indent.mapping.sequence = TRUE), con = yaml_path, useBytes = TRUE)
  out$yaml_path <- normalizePath(yaml_path, mustWork = FALSE)

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    json_path <- file.path(report_dir, sprintf("%s_contract_check.json", stem))
    jsonlite::write_json(report, path = json_path, auto_unbox = TRUE, pretty = TRUE)
    out$json_path <- normalizePath(json_path, mustWork = FALSE)
  }
  out
}

unified_contract_check_exdqlm_univar <- function(
  rdata_path,
  q_num,
  report_dir,
  write_reports = TRUE
) {
  checks <- list()
  errors <- character(0)
  warnings <- character(0)

  add_check <- function(id, ok, detail) {
    checks[[length(checks) + 1L]] <<- list(
      id = id,
      status = if (isTRUE(ok)) "pass" else "fail",
      detail = as.character(detail)
    )
    if (!isTRUE(ok)) {
      errors <<- c(errors, sprintf("%s: %s", id, detail))
    }
  }
  add_warning <- function(msg) {
    warnings <<- c(warnings, as.character(msg))
  }

  env <- unified_contract_env_load(rdata_path)
  suffix <- sprintf("%d_exAL_synth_DISC_uni", as.integer(q_num))

  obj_new_theta <- unified_contract_object(env, sprintf("new.theta.out_%s", suffix))
  obj_samp_theta <- unified_contract_object(env, sprintf("samp.theta_%s", suffix))
  obj_samp_sigma <- unified_contract_object(env, sprintf("samp.sigma_%s", suffix))
  obj_seq_elbo <- unified_contract_object(env, sprintf("seq.elbo_%s", suffix))

  add_check("univar.new_theta.exists", !is.null(obj_new_theta), "required object missing")
  add_check("univar.samp_theta.exists", !is.null(obj_samp_theta), "required object missing")
  add_check("univar.samp_sigma.exists", !is.null(obj_samp_sigma), "required object missing")
  add_check("univar.seq_elbo.exists", !is.null(obj_seq_elbo), "required object missing")

  Tn <- NA_integer_
  if (!is.null(obj_new_theta)) {
    is_list <- is.list(obj_new_theta)
    add_check("univar.new_theta.is_list", is_list, "expected list with sm/sC/exps fields")
    if (isTRUE(is_list)) {
      req_fields <- c("sm", "sC", "exps")
      missing_fields <- req_fields[!req_fields %in% names(obj_new_theta)]
      add_check(
        "univar.new_theta.required_fields",
        length(missing_fields) == 0L,
        if (length(missing_fields) == 0L) "ok" else sprintf("missing fields: %s", paste(missing_fields, collapse = ", "))
      )
      if (length(missing_fields) == 0L) {
        sm <- obj_new_theta$sm
        sC <- obj_new_theta$sC
        exps <- obj_new_theta$exps

        add_check("univar.new_theta.sm.numeric", is.numeric(sm), "sm must be numeric")
        add_check("univar.new_theta.sm.has_dim", !is.null(dim(sm)) && length(dim(sm)) == 2L, "sm must be a matrix")
        if (!is.null(dim(sm)) && length(dim(sm)) == 2L) {
          Tn <- as.integer(dim(sm)[2])
          add_check("univar.new_theta.sm.T_gt_1000", is.finite(Tn) && Tn > 1000L, sprintf("T=%s", as.character(Tn)))
        } else {
          add_warning("new.theta.sm did not provide a 2D shape; downstream T checks skipped.")
        }
        add_check("univar.new_theta.sm.finite", unified_contract_all_finite(sm), "sm contains non-finite values")

        sC_dim <- dim(sC)
        add_check("univar.new_theta.sC.numeric", is.numeric(sC), "sC must be numeric")
        add_check("univar.new_theta.sC.shape", !is.null(sC_dim) && length(sC_dim) == 3L, "sC must be a 3D array")
        if (!is.null(sC_dim) && length(sC_dim) == 3L && is.finite(Tn)) {
          add_check("univar.new_theta.sC.T_matches_sm", as.integer(sC_dim[3]) == as.integer(Tn), sprintf("sC third dim=%d, sm T=%d", as.integer(sC_dim[3]), as.integer(Tn)))
        }
        add_check("univar.new_theta.sC.finite", unified_contract_all_finite(sC), "sC contains non-finite values")

        add_check("univar.new_theta.exps.numeric", is.numeric(exps), "exps must be numeric")
        exps_dim <- dim(exps)
        add_check("univar.new_theta.exps.shape", !is.null(exps_dim) && length(exps_dim) == 2L, "exps must be a 2D matrix")
        if (!is.null(exps_dim) && length(exps_dim) == 2L && is.finite(Tn)) {
          add_check("univar.new_theta.exps.T_matches_sm", as.integer(exps_dim[2]) == as.integer(Tn), sprintf("exps ncol=%d, sm T=%d", as.integer(exps_dim[2]), as.integer(Tn)))
        }
        add_check("univar.new_theta.exps.finite", unified_contract_all_finite(exps), "exps contains non-finite values")
      }
    }
  }

  if (!is.null(obj_samp_theta)) {
    if (is.list(obj_samp_theta) && ("samp_theta" %in% names(obj_samp_theta))) {
      obj_samp_theta <- obj_samp_theta$samp_theta
    }
    add_check("univar.samp_theta.numeric", is.numeric(obj_samp_theta), "samp.theta must be numeric")
    st_dim <- dim(obj_samp_theta)
    add_check("univar.samp_theta.has_dim", !is.null(st_dim), "samp.theta must have dimensions")
    if (!is.null(st_dim) && is.finite(Tn)) {
      add_check("univar.samp_theta.contains_T", any(as.integer(st_dim) == as.integer(Tn)), sprintf("dims=%s, expected one dim == T=%d", paste(st_dim, collapse = "x"), as.integer(Tn)))
    }
    add_check("univar.samp_theta.finite", unified_contract_all_finite(obj_samp_theta), "samp.theta contains non-finite values")
  }

  if (!is.null(obj_samp_sigma)) {
    if (is.list(obj_samp_sigma) && ("samp_sigma" %in% names(obj_samp_sigma))) {
      obj_samp_sigma <- obj_samp_sigma$samp_sigma
    }
    add_check("univar.samp_sigma.numeric", is.numeric(obj_samp_sigma), "samp.sigma must be numeric")
    add_check("univar.samp_sigma.len_ge_1", length(obj_samp_sigma) >= 1L, sprintf("length=%d", length(obj_samp_sigma)))
    add_check("univar.samp_sigma.finite", unified_contract_all_finite(obj_samp_sigma), "samp.sigma contains non-finite values")
  }

  if (!is.null(obj_seq_elbo)) {
    add_check("univar.seq_elbo.numeric", is.numeric(obj_seq_elbo), "seq.elbo must be numeric")
    add_check("univar.seq_elbo.len_ge_1", length(obj_seq_elbo) >= 1L, sprintf("length=%d", length(obj_seq_elbo)))
    add_check("univar.seq_elbo.finite", unified_contract_all_finite(obj_seq_elbo), "seq.elbo contains non-finite values")
  }

  status <- if (length(errors) == 0L) "pass" else "fail"
  report <- list(
    family = "exdqlm_univar",
    implementation_mode = "theory_aligned",
    quantile = as.integer(q_num),
    rdata_path = normalizePath(rdata_path, mustWork = FALSE),
    status = status,
    errors = unname(errors),
    warnings = unname(warnings),
    checks = checks,
    checked_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  report_paths <- unified_contract_write_report(
    report = report,
    report_dir = report_dir,
    stem = sprintf("q%s_exdqlm_univar", sprintf("%02d", as.integer(q_num))),
    write_reports = write_reports
  )
  report$report_paths <- report_paths
  report
}

unified_contract_check_ndlm_main <- function(
  rdata_path,
  report_dir,
  summary_log_path = NULL,
  write_reports = TRUE
) {
  checks <- list()
  errors <- character(0)
  warnings <- character(0)

  add_check <- function(id, ok, detail) {
    checks[[length(checks) + 1L]] <<- list(
      id = id,
      status = if (isTRUE(ok)) "pass" else "fail",
      detail = as.character(detail)
    )
    if (!isTRUE(ok)) {
      errors <<- c(errors, sprintf("%s: %s", id, detail))
    }
  }
  add_warning <- function(msg) {
    warnings <<- c(warnings, as.character(msg))
  }

  env <- unified_contract_env_load(rdata_path)
  required_names <- c(
    "new.theta.out_50_NDLM_synth_DISC",
    "samp.theta_50_NDLM_synth_DISC",
    "samp.sigma_50_NDLM_synth_DISC",
    "samp.theta_ens_50_NDLM_synth_DISC",
    "seq.elbo_50_NDLM_synth_DISC",
    "seq.sigma_50_NDLM_synth_DISC",
    "delta_50_NDLM_synth_DISC",
    "ndlm_main_theory_state"
  )
  for (nm in required_names) {
    add_check(
      sprintf("ndlm.%s.exists", nm),
      exists(nm, envir = env, inherits = FALSE),
      "required object missing"
    )
  }

  new_theta <- unified_contract_object(env, "new.theta.out_50_NDLM_synth_DISC")
  samp_theta <- unified_contract_object(env, "samp.theta_50_NDLM_synth_DISC")
  samp_sigma <- unified_contract_object(env, "samp.sigma_50_NDLM_synth_DISC")
  samp_theta_ens <- unified_contract_object(env, "samp.theta_ens_50_NDLM_synth_DISC")
  seq_elbo <- unified_contract_object(env, "seq.elbo_50_NDLM_synth_DISC")
  seq_sigma <- unified_contract_object(env, "seq.sigma_50_NDLM_synth_DISC")
  seq_scale <- unified_contract_object(env, "seq.scale_50_NDLM_synth_DISC")
  delta <- unified_contract_object(env, "delta_50_NDLM_synth_DISC")
  theory_state <- unified_contract_object(env, "ndlm_main_theory_state")

  Tn <- NA_integer_
  Kn <- NA_integer_
  sm_k <- integer(0)
  sc_k <- integer(0)
  if (!is.null(new_theta)) {
    add_check("ndlm.new_theta.is_list", is.list(new_theta), "new.theta must be a list")
    if (is.list(new_theta)) {
      req_fields <- c("sm", "sC", "sm_ens", "sC_ens", "exps", "standard_forecast_errors")
      missing_fields <- req_fields[!req_fields %in% names(new_theta)]
      add_check(
        "ndlm.new_theta.required_fields",
        length(missing_fields) == 0L,
        if (length(missing_fields) == 0L) "ok" else sprintf("missing fields: %s", paste(missing_fields, collapse = ", "))
      )
      if (length(missing_fields) == 0L) {
        sm <- new_theta$sm
        sC <- new_theta$sC
        exps <- new_theta$exps
        sm_ens <- new_theta$sm_ens
        sC_ens <- new_theta$sC_ens
        standard_forecast_errors <- new_theta$standard_forecast_errors

        add_check("ndlm.new_theta.sm.numeric", is.numeric(sm), "sm must be numeric")
        add_check("ndlm.new_theta.sm.shape", !is.null(dim(sm)) && length(dim(sm)) == 2L, "sm must be a matrix")
        if (!is.null(dim(sm)) && length(dim(sm)) == 2L) {
          Tn <- as.integer(dim(sm)[2])
          add_check("ndlm.new_theta.sm.T_gt_1000", is.finite(Tn) && Tn > 1000L, sprintf("T=%s", as.character(Tn)))
        }
        add_check("ndlm.new_theta.sm.finite", unified_contract_all_finite(sm), "sm contains non-finite values")

        sC_dim <- dim(sC)
        add_check("ndlm.new_theta.sC.numeric", is.numeric(sC), "sC must be numeric")
        add_check("ndlm.new_theta.sC.shape", !is.null(sC_dim) && length(sC_dim) == 3L, "sC must be a 3D array")
        if (!is.null(sC_dim) && length(sC_dim) == 3L && is.finite(Tn)) {
          add_check("ndlm.new_theta.sC.T_matches_sm", as.integer(sC_dim[3]) == as.integer(Tn), sprintf("sC third dim=%d, sm T=%d", as.integer(sC_dim[3]), as.integer(Tn)))
        }
        add_check("ndlm.new_theta.sC.finite", unified_contract_all_finite(sC), "sC contains non-finite values")

        add_check("ndlm.new_theta.sm_ens.is_list", is.list(sm_ens), "sm_ens must be a list")
        if (is.list(sm_ens) && length(sm_ens) > 0L) {
          sm_ens_ok <- vapply(sm_ens, function(x) is.numeric(x) && !is.null(dim(x)) && length(dim(x)) == 2L, logical(1))
          add_check("ndlm.new_theta.sm_ens.shape", all(sm_ens_ok), "sm_ens entries must be numeric matrices")
          add_check("ndlm.new_theta.sm_ens.finite", unified_contract_all_finite(sm_ens), "sm_ens contains non-finite values")
        } else {
          add_warning("new.theta.sm_ens is empty; downstream ensemble diagnostics may be limited.")
        }

        add_check("ndlm.new_theta.sC_ens.is_list", is.list(sC_ens), "sC_ens must be a list")
        if (is.list(sC_ens) && length(sC_ens) > 0L) {
          sC_ens_ok <- vapply(sC_ens, function(x) is.numeric(x) && !is.null(dim(x)) && length(dim(x)) == 3L, logical(1))
          add_check("ndlm.new_theta.sC_ens.shape", all(sC_ens_ok), "sC_ens entries must be numeric 3D arrays")
          add_check("ndlm.new_theta.sC_ens.finite", unified_contract_all_finite(sC_ens), "sC_ens contains non-finite values")
        }

        exps_dim <- dim(exps)
        add_check("ndlm.new_theta.exps.numeric", is.numeric(exps), "exps must be numeric")
        add_check("ndlm.new_theta.exps.shape", !is.null(exps_dim) && length(exps_dim) == 2L, "exps must be a 2D matrix")
        if (!is.null(exps_dim) && length(exps_dim) == 2L && is.finite(Tn)) {
          add_check("ndlm.new_theta.exps.T_matches_sm", as.integer(exps_dim[2]) == as.integer(Tn), sprintf("exps ncol=%d, sm T=%d", as.integer(exps_dim[2]), as.integer(Tn)))
        }
        add_check("ndlm.new_theta.exps.finite", unified_contract_all_finite(exps), "exps contains non-finite values")

        sfe_dim <- dim(standard_forecast_errors)
        add_check("ndlm.new_theta.standard_forecast_errors.numeric", is.numeric(standard_forecast_errors), "standard_forecast_errors must be numeric")
        add_check("ndlm.new_theta.standard_forecast_errors.shape", !is.null(sfe_dim) && length(sfe_dim) == 2L, "standard_forecast_errors must be a 2D matrix")
        if (!is.null(sfe_dim) && length(sfe_dim) == 2L) {
          Kn <- as.integer(sfe_dim[2])
          add_check("ndlm.new_theta.standard_forecast_errors.rows_ge_1", as.integer(sfe_dim[1]) >= 1L, sprintf("rows=%d", as.integer(sfe_dim[1])))
          add_check("ndlm.new_theta.standard_forecast_errors.K_ge_1", is.finite(Kn) && Kn >= 1L, sprintf("K=%s", as.character(Kn)))
        }
        add_check("ndlm.new_theta.standard_forecast_errors.finite", unified_contract_all_finite(standard_forecast_errors), "standard_forecast_errors contains non-finite values")

        sm_k <- integer(0)
        if (is.list(sm_ens) && length(sm_ens) > 0L) {
          sm_k <- vapply(sm_ens, function(x) {
            d <- dim(x)
            if (is.null(d) || length(d) != 2L) return(NA_integer_)
            as.integer(d[2])
          }, integer(1))
          add_check(
            "ndlm.new_theta.sm_ens.segment_lengths_valid",
            all(is.finite(sm_k)) && all(sm_k >= 0L),
            sprintf("sm_ens K=[%s]", paste(sm_k, collapse = ","))
          )
        }
        sc_k <- integer(0)
        if (is.list(sC_ens) && length(sC_ens) > 0L) {
          sc_k <- vapply(sC_ens, function(x) {
            d <- dim(x)
            if (is.null(d) || length(d) != 3L) return(NA_integer_)
            as.integer(d[3])
          }, integer(1))
          add_check(
            "ndlm.new_theta.sC_ens.segment_lengths_valid",
            all(is.finite(sc_k)) && all(sc_k >= 0L),
            sprintf("sC_ens K=[%s]", paste(sc_k, collapse = ","))
          )
        }
        if (length(sm_k) > 0L && length(sc_k) > 0L) {
          add_check(
            "ndlm.new_theta.sm_sC_segment_profile_match",
            length(sm_k) == length(sc_k) && all(sm_k == sc_k),
            sprintf("sm_ens K=[%s], sC_ens K=[%s]", paste(sm_k, collapse = ","), paste(sc_k, collapse = ","))
          )
        }
        if (is.finite(Kn) && length(sm_k) > 0L) {
          add_check(
            "ndlm.new_theta.sm_ens.segment_sum_matches_standard_errors",
            all(is.finite(sm_k)) && sum(sm_k) == Kn,
            sprintf("sum(sm_ens K)=%s, standard_forecast_errors K=%s", as.character(sum(sm_k)), as.character(Kn))
          )
        }
        if (is.finite(Kn) && length(sc_k) > 0L) {
          add_check(
            "ndlm.new_theta.sC_ens.segment_sum_matches_standard_errors",
            all(is.finite(sc_k)) && sum(sc_k) == Kn,
            sprintf("sum(sC_ens K)=%s, standard_forecast_errors K=%s", as.character(sum(sc_k)), as.character(Kn))
          )
        }
      }
    }
  }

  if (!is.null(theory_state)) {
    add_check("ndlm.theory_state.is_list", is.list(theory_state), "ndlm_main_theory_state must be a list")
    if (is.list(theory_state)) {
      req_state <- c("K", "K_overlap", "K_max", "K_vec", "segment_lengths", "K_cap", "nws_len", "glofas_len")
      missing_state <- req_state[!req_state %in% names(theory_state)]
      add_check(
        "ndlm.theory_state.required_fields",
        length(missing_state) == 0L,
        if (length(missing_state) == 0L) "ok" else sprintf("missing fields: %s", paste(missing_state, collapse = ", "))
      )
      if (length(missing_state) == 0L) {
        read_named_int <- function(x, name) {
          if (is.null(x)) return(NA_integer_)
          if (length(x) == 0L) return(NA_integer_)
          if (!is.null(names(x)) && (name %in% names(x))) {
            return(suppressWarnings(as.integer(x[[name]])))
          }
          suppressWarnings(as.integer(x[[1L]]))
        }

        K_state <- suppressWarnings(as.integer(theory_state$K[[1L]]))
        K_overlap <- suppressWarnings(as.integer(theory_state$K_overlap[[1L]]))
        K_max <- suppressWarnings(as.integer(theory_state$K_max[[1L]]))
        K_cap <- suppressWarnings(as.integer(theory_state$K_cap[[1L]]))
        nws_len <- suppressWarnings(as.integer(theory_state$nws_len[[1L]]))
        glofas_len <- suppressWarnings(as.integer(theory_state$glofas_len[[1L]]))
        k_nws_cap <- suppressWarnings(as.integer(min(nws_len, K_cap)))
        k_glofas_cap <- suppressWarnings(as.integer(min(glofas_len, K_cap)))
        K_expected_max <- suppressWarnings(as.integer(max(k_nws_cap, k_glofas_cap)))
        K_expected_overlap <- suppressWarnings(as.integer(min(k_nws_cap, k_glofas_cap)))
        K_vec <- theory_state$K_vec
        segment_lengths <- theory_state$segment_lengths
        k_vec_nws <- read_named_int(K_vec, "nws")
        k_vec_glofas <- read_named_int(K_vec, "glofas")
        seg_overlap <- read_named_int(segment_lengths, "overlap")
        seg_extension <- read_named_int(segment_lengths, "extension")

        add_check("ndlm.theory_state.K_finite", is.finite(K_state) && K_state >= 1L, sprintf("K=%s", as.character(theory_state$K)))
        add_check("ndlm.theory_state.K_overlap_finite", is.finite(K_overlap) && K_overlap >= 1L, sprintf("K_overlap=%s", as.character(theory_state$K_overlap)))
        add_check("ndlm.theory_state.K_max_finite", is.finite(K_max) && K_max >= 1L, sprintf("K_max=%s", as.character(theory_state$K_max)))
        add_check("ndlm.theory_state.K_cap_positive", is.finite(K_cap) && K_cap >= 1L, sprintf("K_cap=%s", as.character(theory_state$K_cap)))
        add_check("ndlm.theory_state.nws_len_positive", is.finite(nws_len) && nws_len >= 1L, sprintf("nws_len=%s", as.character(theory_state$nws_len)))
        add_check("ndlm.theory_state.glofas_len_positive", is.finite(glofas_len) && glofas_len >= 1L, sprintf("glofas_len=%s", as.character(theory_state$glofas_len)))
        add_check(
          "ndlm.theory_state.K_vec_expected_match",
          is.finite(k_vec_nws) && is.finite(k_vec_glofas) &&
            k_vec_nws == k_nws_cap && k_vec_glofas == k_glofas_cap,
          sprintf("K_vec=(nws=%s,glofas=%s) expected=(%s,%s)", as.character(k_vec_nws), as.character(k_vec_glofas), as.character(k_nws_cap), as.character(k_glofas_cap))
        )
        add_check(
          "ndlm.theory_state.K_overlap_expected_match",
          is.finite(K_overlap) && is.finite(K_expected_overlap) && K_overlap == K_expected_overlap,
          sprintf("K_overlap=%s expected=min(%s,%s)=%s", as.character(K_overlap), as.character(k_nws_cap), as.character(k_glofas_cap), as.character(K_expected_overlap))
        )
        add_check(
          "ndlm.theory_state.K_max_expected_match",
          is.finite(K_max) && is.finite(K_expected_max) && K_max == K_expected_max,
          sprintf("K_max=%s expected=max(%s,%s)=%s", as.character(K_max), as.character(k_nws_cap), as.character(k_glofas_cap), as.character(K_expected_max))
        )
        add_check(
          "ndlm.theory_state.segment_lengths_consistent",
          is.finite(seg_overlap) && is.finite(seg_extension) &&
            seg_overlap == K_overlap && seg_overlap + seg_extension == K_max,
          sprintf("segment_lengths=(overlap=%s,extension=%s), K_overlap=%s, K_max=%s", as.character(seg_overlap), as.character(seg_extension), as.character(K_overlap), as.character(K_max))
        )
        add_check(
          "ndlm.theory_state.K_alias_matches_K_max",
          is.finite(K_state) && is.finite(K_max) && K_state == K_max,
          sprintf("K=%s K_max=%s", as.character(K_state), as.character(K_max))
        )
        if (is.finite(Kn)) {
          add_check(
            "ndlm.theory_state.K_matches_standard_errors",
            K_max == Kn,
            sprintf("K_max=%s standard_forecast_errors.K=%s", as.character(K_max), as.character(Kn))
          )
        }
        if (is.finite(Kn) && length(sm_k) > 0L) {
          add_check(
            "ndlm.theory_state.segment_sum_matches_standard_errors",
            sum(sm_k) == Kn,
            sprintf("sum(sm_ens K)=%s standard_forecast_errors.K=%s", as.character(sum(sm_k)), as.character(Kn))
          )
        }
      }
      if ("sigma_by_source" %in% names(theory_state)) {
        sbs <- theory_state$sigma_by_source
        sbs_num <- suppressWarnings(as.numeric(sbs))
        add_check("ndlm.theory_state.sigma_by_source.numeric", is.numeric(sbs), "sigma_by_source must be numeric")
        add_check("ndlm.theory_state.sigma_by_source.len_ge_3", length(sbs_num) >= 3L, sprintf("length=%d", length(sbs_num)))
        add_check("ndlm.theory_state.sigma_by_source.finite", unified_contract_all_finite(sbs_num), "sigma_by_source contains non-finite values")
        add_check("ndlm.theory_state.sigma_by_source.positive", all(sbs_num > 0), "sigma_by_source must be strictly positive")
      }
    }
  }

  if (!is.null(samp_theta)) {
    if (is.list(samp_theta) && ("samp_theta" %in% names(samp_theta))) {
      samp_theta <- samp_theta$samp_theta
    }
    add_check("ndlm.samp_theta.numeric", is.numeric(samp_theta), "samp.theta must be numeric")
    st_dim <- dim(samp_theta)
    add_check("ndlm.samp_theta.has_dim", !is.null(st_dim), "samp.theta must have dimensions")
    if (!is.null(st_dim) && is.finite(Tn)) {
      add_check("ndlm.samp_theta.contains_T", any(as.integer(st_dim) == as.integer(Tn)), sprintf("dims=%s, expected one dim == T=%d", paste(st_dim, collapse = "x"), as.integer(Tn)))
    }
    add_check("ndlm.samp_theta.finite", unified_contract_all_finite(samp_theta), "samp.theta contains non-finite values")
  }

  if (!is.null(samp_sigma)) {
    if (is.list(samp_sigma) && ("samp_sigma" %in% names(samp_sigma))) {
      samp_sigma <- samp_sigma$samp_sigma
    }
    add_check("ndlm.samp_sigma.numeric", is.numeric(samp_sigma), "samp.sigma must be numeric")
    add_check("ndlm.samp_sigma.len_ge_1", length(samp_sigma) >= 1L, sprintf("length=%d", length(samp_sigma)))
    add_check("ndlm.samp_sigma.finite", unified_contract_all_finite(samp_sigma), "samp.sigma contains non-finite values")
  }

  if (!is.null(samp_theta_ens)) {
    if (is.list(samp_theta_ens) && ("samp_theta_ens" %in% names(samp_theta_ens))) {
      samp_theta_ens <- samp_theta_ens$samp_theta_ens
    }
    leaf_arrays <- list()
    collect_numeric_leaves <- function(x) {
      if (is.list(x)) {
        if (length(x) == 0L) return(invisible(NULL))
        for (item in x) collect_numeric_leaves(item)
        return(invisible(NULL))
      }
      if (is.numeric(x)) {
        leaf_arrays[[length(leaf_arrays) + 1L]] <<- x
      }
      invisible(NULL)
    }
    collect_numeric_leaves(samp_theta_ens)
    add_check("ndlm.samp_theta_ens.numeric_leaves", length(leaf_arrays) > 0L, "samp.theta_ens must contain numeric leaves")
    if (length(leaf_arrays) > 0L) {
      add_check("ndlm.samp_theta_ens.finite", all(vapply(leaf_arrays, unified_contract_all_finite, logical(1))), "samp.theta_ens contains non-finite values")
      dims_ok <- vapply(leaf_arrays, function(x) {
        d <- dim(x)
        !is.null(d) && length(d) >= 2L
      }, logical(1))
      add_check("ndlm.samp_theta_ens.dim_rank_ge_2", all(dims_ok), "samp.theta_ens numeric leaves must have rank >= 2")
    }
  }

  for (nm in c("seq_elbo", "seq_sigma", "delta")) {
    obj <- switch(
      nm,
      seq_elbo = seq_elbo,
      seq_sigma = seq_sigma,
      delta = delta
    )
    if (!is.null(obj)) {
      add_check(sprintf("ndlm.%s.numeric", nm), is.numeric(obj), sprintf("%s must be numeric", nm))
      add_check(sprintf("ndlm.%s.len_ge_1", nm), length(obj) >= 1L, sprintf("length=%d", length(obj)))
      add_check(sprintf("ndlm.%s.finite", nm), unified_contract_all_finite(obj), sprintf("%s contains non-finite values", nm))
    }
  }
  if (!is.null(seq_scale)) {
    add_check("ndlm.seq_scale.numeric", is.numeric(seq_scale), "seq_scale must be numeric")
    add_check("ndlm.seq_scale.len_ge_1", length(seq_scale) >= 1L, sprintf("length=%d", length(seq_scale)))
    add_check("ndlm.seq_scale.finite", unified_contract_all_finite(seq_scale), "seq_scale contains non-finite values")
  }

  if (!is.null(summary_log_path) && nzchar(summary_log_path)) {
    add_check("ndlm.summary_log.exists", file.exists(summary_log_path), "summary log file missing")
    if (file.exists(summary_log_path)) {
      add_check("ndlm.summary_log.nonempty", file.info(summary_log_path)$size > 0L, sprintf("summary log size=%d", as.integer(file.info(summary_log_path)$size)))
    }
  } else {
    add_warning("summary log path was not provided to NDLM contract check.")
  }

  status <- if (length(errors) == 0L) "pass" else "fail"
  report <- list(
    family = "ndlm_main",
    implementation_mode = "theory_aligned",
    rdata_path = normalizePath(rdata_path, mustWork = FALSE),
    summary_log_path = if (is.null(summary_log_path)) NULL else normalizePath(summary_log_path, mustWork = FALSE),
    status = status,
    errors = unname(errors),
    warnings = unname(warnings),
    checks = checks,
    checked_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  report_paths <- unified_contract_write_report(
    report = report,
    report_dir = report_dir,
    stem = "ndlm_main",
    write_reports = write_reports
  )
  report$report_paths <- report_paths
  report
}

unified_contract_check_ndlm_univar <- function(
  rdata_path,
  report_dir,
  summary_log_path = NULL,
  write_reports = TRUE
) {
  checks <- list()
  errors <- character(0)
  warnings <- character(0)

  add_check <- function(id, ok, detail) {
    checks[[length(checks) + 1L]] <<- list(
      id = id,
      status = if (isTRUE(ok)) "pass" else "fail",
      detail = as.character(detail)
    )
    if (!isTRUE(ok)) {
      errors <<- c(errors, sprintf("%s: %s", id, detail))
    }
  }
  add_warning <- function(msg) {
    warnings <<- c(warnings, as.character(msg))
  }

  env <- unified_contract_env_load(rdata_path)
  required_names <- c(
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
  for (nm in required_names) {
    add_check(
      sprintf("ndlm_univar.%s.exists", nm),
      exists(nm, envir = env, inherits = FALSE),
      "required object missing"
    )
  }

  new_theta <- unified_contract_object(env, "new.theta.out_50_NDLM_univar_synth_DISC")
  samp_theta <- unified_contract_object(env, "samp.theta_50_NDLM_univar_synth_DISC")
  samp_sigma <- unified_contract_object(env, "samp.sigma_50_NDLM_univar_synth_DISC")
  samp_theta_ens <- unified_contract_object(env, "samp.theta.ens_50_NDLM_univar_synth_DISC")
  seq_elbo <- unified_contract_object(env, "seq.elbo_50_NDLM_univar_synth_DISC")
  seq_sigma <- unified_contract_object(env, "seq.sigma_50_NDLM_univar_synth_DISC")
  seq_scale <- unified_contract_object(env, "seq.scale_50_NDLM_univar_synth_DISC")
  delta <- unified_contract_object(env, "delta_50_NDLM_univar_synth_DISC")
  y_fore_draws <- unified_contract_object(env, "y.fore.draws_50_NDLM_univar_synth_DISC")
  theory_state <- unified_contract_object(env, "ndlm_univar_theory_state")

  Tn <- NA_integer_
  Kn <- NA_integer_
  if (!is.null(new_theta)) {
    add_check("ndlm_univar.new_theta.is_list", is.list(new_theta), "new.theta must be a list")
    if (is.list(new_theta)) {
      req_fields <- c("sm", "sC", "sm_ens", "sC_ens", "exps", "standard_forecast_errors")
      missing_fields <- req_fields[!req_fields %in% names(new_theta)]
      add_check(
        "ndlm_univar.new_theta.required_fields",
        length(missing_fields) == 0L,
        if (length(missing_fields) == 0L) "ok" else sprintf("missing fields: %s", paste(missing_fields, collapse = ", "))
      )
      if (length(missing_fields) == 0L) {
        sm <- new_theta$sm
        sC <- new_theta$sC
        exps <- new_theta$exps
        sm_ens <- new_theta$sm_ens
        sC_ens <- new_theta$sC_ens
        sfe <- new_theta$standard_forecast_errors

        add_check("ndlm_univar.new_theta.sm.matrix", is.numeric(sm) && !is.null(dim(sm)) && length(dim(sm)) == 2L, "sm must be numeric matrix")
        if (!is.null(dim(sm)) && length(dim(sm)) == 2L) {
          Tn <- as.integer(dim(sm)[2])
        }
        add_check("ndlm_univar.new_theta.sm.finite", unified_contract_all_finite(sm), "sm contains non-finite values")

        add_check("ndlm_univar.new_theta.sC.array3", is.numeric(sC) && !is.null(dim(sC)) && length(dim(sC)) == 3L, "sC must be numeric 3D array")
        if (!is.null(dim(sC)) && length(dim(sC)) == 3L && is.finite(Tn)) {
          add_check(
            "ndlm_univar.new_theta.sC.T_matches",
            as.integer(dim(sC)[3]) == as.integer(Tn),
            sprintf("sC third dim=%d, sm T=%d", as.integer(dim(sC)[3]), as.integer(Tn))
          )
        }
        add_check("ndlm_univar.new_theta.sC.finite", unified_contract_all_finite(sC), "sC contains non-finite values")

        add_check("ndlm_univar.new_theta.exps.matrix", is.numeric(exps) && !is.null(dim(exps)) && length(dim(exps)) == 2L, "exps must be numeric matrix")
        add_check("ndlm_univar.new_theta.exps.finite", unified_contract_all_finite(exps), "exps contains non-finite values")

        add_check(
          "ndlm_univar.new_theta.sm_ens.list_matrix",
          is.list(sm_ens) && length(sm_ens) > 0L &&
            all(vapply(sm_ens, function(x) is.numeric(x) && !is.null(dim(x)) && length(dim(x)) == 2L, logical(1))),
          "sm_ens must be non-empty list of numeric matrices"
        )
        add_check(
          "ndlm_univar.new_theta.sC_ens.list_array3",
          is.list(sC_ens) && length(sC_ens) > 0L &&
            all(vapply(sC_ens, function(x) is.numeric(x) && !is.null(dim(x)) && length(dim(x)) == 3L, logical(1))),
          "sC_ens must be non-empty list of numeric 3D arrays"
        )
        add_check("ndlm_univar.new_theta.sm_ens.finite", unified_contract_all_finite(sm_ens), "sm_ens contains non-finite values")
        add_check("ndlm_univar.new_theta.sC_ens.finite", unified_contract_all_finite(sC_ens), "sC_ens contains non-finite values")

        add_check("ndlm_univar.new_theta.standard_forecast_errors.matrix", is.numeric(sfe) && !is.null(dim(sfe)) && length(dim(sfe)) == 2L, "standard_forecast_errors must be numeric matrix")
        if (!is.null(dim(sfe)) && length(dim(sfe)) == 2L) {
          Kn <- as.integer(dim(sfe)[2])
          add_check("ndlm_univar.new_theta.standard_forecast_errors.K_ge_1", is.finite(Kn) && Kn >= 1L, sprintf("K=%s", as.character(Kn)))
        }
        add_check("ndlm_univar.new_theta.standard_forecast_errors.finite", unified_contract_all_finite(sfe), "standard_forecast_errors contains non-finite values")
      }
    }
  }

  if (!is.null(samp_theta)) {
    if (is.list(samp_theta) && ("samp_theta" %in% names(samp_theta))) {
      samp_theta <- samp_theta$samp_theta
    }
    add_check("ndlm_univar.samp_theta.numeric", is.numeric(samp_theta), "samp.theta must be numeric")
    st_dim <- dim(samp_theta)
    add_check("ndlm_univar.samp_theta.has_dim", !is.null(st_dim), "samp.theta must have dimensions")
    if (!is.null(st_dim) && is.finite(Tn)) {
      add_check(
        "ndlm_univar.samp_theta.contains_T",
        any(as.integer(st_dim) == as.integer(Tn)),
        sprintf("dims=%s, expected one dim == T=%d", paste(st_dim, collapse = "x"), as.integer(Tn))
      )
    }
    add_check("ndlm_univar.samp_theta.finite", unified_contract_all_finite(samp_theta), "samp.theta contains non-finite values")
  }
  if (!is.null(samp_sigma)) {
    if (is.list(samp_sigma) && ("samp_sigma" %in% names(samp_sigma))) {
      samp_sigma <- samp_sigma$samp_sigma
    }
    add_check("ndlm_univar.samp_sigma.numeric", is.numeric(samp_sigma), "samp.sigma must be numeric")
    add_check("ndlm_univar.samp_sigma.len_ge_1", length(samp_sigma) >= 1L, sprintf("length=%d", length(samp_sigma)))
    add_check("ndlm_univar.samp_sigma.finite", unified_contract_all_finite(samp_sigma), "samp.sigma contains non-finite values")
  }
  if (!is.null(samp_theta_ens)) {
    if (is.list(samp_theta_ens) && ("samp_theta_ens" %in% names(samp_theta_ens))) {
      samp_theta_ens <- samp_theta_ens$samp_theta_ens
    }
    collect_numeric_leaves <- function(x) {
      if (is.null(x)) return(list())
      if (is.numeric(x)) return(list(x))
      if (!is.list(x) || length(x) == 0L) return(list())
      out <- list()
      for (ii in seq_along(x)) {
        out <- c(out, collect_numeric_leaves(x[[ii]]))
      }
      out
    }
    leaf_arrays <- collect_numeric_leaves(samp_theta_ens)
    add_check(
      "ndlm_univar.samp_theta_ens.numeric_leaves",
      length(leaf_arrays) > 0L,
      "samp.theta.ens must contain numeric leaves"
    )
    if (length(leaf_arrays) > 0L) {
      add_check(
        "ndlm_univar.samp_theta_ens.finite",
        all(vapply(leaf_arrays, unified_contract_all_finite, logical(1))),
        "samp.theta.ens contains non-finite values"
      )
      dims_ok <- vapply(leaf_arrays, function(x) {
        d <- dim(x)
        !is.null(d) && length(d) >= 2L
      }, logical(1))
      add_check(
        "ndlm_univar.samp_theta_ens.dim_rank_ge_2",
        all(dims_ok),
        "samp.theta.ens numeric leaves must have rank >= 2"
      )
    }
  }
  for (nm in c("seq_elbo", "seq_sigma", "seq_scale", "delta")) {
    obj <- switch(
      nm,
      seq_elbo = seq_elbo,
      seq_sigma = seq_sigma,
      seq_scale = seq_scale,
      delta = delta
    )
    if (!is.null(obj)) {
      add_check(sprintf("ndlm_univar.%s.numeric", nm), is.numeric(obj), sprintf("%s must be numeric", nm))
      add_check(sprintf("ndlm_univar.%s.finite", nm), unified_contract_all_finite(obj), sprintf("%s contains non-finite values", nm))
    }
  }
  if (!is.null(y_fore_draws)) {
    add_check(
      "ndlm_univar.y_fore_draws.matrix",
      is.numeric(y_fore_draws) && !is.null(dim(y_fore_draws)) && length(dim(y_fore_draws)) == 2L,
      "y.fore.draws must be numeric matrix"
    )
    add_check("ndlm_univar.y_fore_draws.finite", unified_contract_all_finite(y_fore_draws), "y.fore.draws contains non-finite values")
    if (!is.null(dim(y_fore_draws)) && length(dim(y_fore_draws)) == 2L && is.finite(Kn)) {
      add_check(
        "ndlm_univar.y_fore_draws.K_matches",
        as.integer(dim(y_fore_draws)[2]) == as.integer(Kn),
        sprintf("y.fore.draws ncol=%d, expected K=%d", as.integer(dim(y_fore_draws)[2]), as.integer(Kn))
      )
    }
  }
  if (!is.null(theory_state)) {
    add_check("ndlm_univar.theory_state.is_list", is.list(theory_state), "ndlm_univar_theory_state must be a list")
    if (is.list(theory_state)) {
      add_check(
        "ndlm_univar.theory_state.transfer_flag",
        "transfer_active_forecast_window" %in% names(theory_state),
        "missing transfer_active_forecast_window"
      )
      add_check(
        "ndlm_univar.theory_state.K_positive",
        is.finite(suppressWarnings(as.integer(theory_state$K))) &&
          suppressWarnings(as.integer(theory_state$K)) >= 1L,
        sprintf("K=%s", as.character(theory_state$K))
      )
    }
  }

  if (!is.null(summary_log_path) && nzchar(summary_log_path)) {
    add_check("ndlm_univar.summary_log.exists", file.exists(summary_log_path), "summary log file missing")
    if (file.exists(summary_log_path)) {
      add_check(
        "ndlm_univar.summary_log.nonempty",
        file.info(summary_log_path)$size > 0L,
        sprintf("summary log size=%d", as.integer(file.info(summary_log_path)$size))
      )
    }
  } else {
    add_warning("summary log path was not provided to NDLM univar contract check.")
  }

  status <- if (length(errors) == 0L) "pass" else "fail"
  report <- list(
    family = "ndlm_univar",
    implementation_mode = "theory_aligned_closed_form",
    rdata_path = normalizePath(rdata_path, mustWork = FALSE),
    summary_log_path = if (is.null(summary_log_path)) NULL else normalizePath(summary_log_path, mustWork = FALSE),
    status = status,
    errors = unname(errors),
    warnings = unname(warnings),
    checks = checks,
    checked_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  report_paths <- unified_contract_write_report(
    report = report,
    report_dir = report_dir,
    stem = "ndlm_univar",
    write_reports = write_reports
  )
  report$report_paths <- report_paths
  report
}
