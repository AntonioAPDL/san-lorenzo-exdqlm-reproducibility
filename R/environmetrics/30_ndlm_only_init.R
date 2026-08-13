###############################################################################
# NDLM-only post initialization module
# Purpose:
#   - In NDLM-only runs, load and validate only NDLM post artifacts.
#   - Explicitly avoid univariate/multivariate exDQLM bundle loading.
###############################################################################

if (!isTRUE(MODEL_RUN_NDLM_MAIN) && !isTRUE(MODEL_RUN_NDLM_UNIVAR)) {
  stop(
    "[POST_NDLM_ONLY_INIT] either MODEL_RUN_NDLM_MAIN or MODEL_RUN_NDLM_UNIVAR must be TRUE for NDLM-only init module.",
    call. = FALSE
  )
}
if (isTRUE(MODEL_RUN_EXDQLM_MULTIVAR) || isTRUE(MODEL_RUN_EXDQLM_UNIVAR)) {
  stop("[POST_NDLM_ONLY_INIT] NDLM-only init module was selected while exDQLM families are enabled.", call. = FALSE)
}

load_rdata_with_retry <- function(path, attempts = 3L, sleep_sec = 0.5, envir = parent.frame()) {
  stopifnot(is.character(path), length(path) == 1L, attempts >= 1L)
  if (!nzchar(path)) {
    stop("[POST_NDLM_ONLY_INIT] NDLM artifact path is empty (UNIFIED_NDLM_RDATA_PATH).", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("[POST_NDLM_ONLY_INIT] NDLM artifact path does not exist: %s", path), call. = FALSE)
  }
  last_err <- NULL
  for (i in seq_len(attempts)) {
    ok <- tryCatch({
      load(path, envir = envir)
      TRUE
    }, error = function(e) {
      last_err <<- e
      FALSE
    })
    if (ok) return(invisible(TRUE))
    Sys.sleep(sleep_sec)
  }
  stop(last_err)
}

load_ndlm_bundle_into <- function(path, assign_env = parent.frame(), attempts = 3L, sleep_sec = 0.5) {
  bundle_env <- new.env(parent = emptyenv())
  load_rdata_with_retry(path, attempts = attempts, sleep_sec = sleep_sec, envir = bundle_env)
  obj_names <- ls(bundle_env, all.names = TRUE)
  for (nm in obj_names) {
    assign(nm, get(nm, envir = bundle_env, inherits = FALSE), envir = assign_env)
  }
  invisible(obj_names)
}

is_numeric_matrix <- function(x) {
  is.numeric(x) && !is.null(dim(x)) && length(dim(x)) == 2L
}

is_numeric_array3 <- function(x) {
  is.numeric(x) && !is.null(dim(x)) && length(dim(x)) == 3L
}

is_numeric_matrix_list <- function(x) {
  is.list(x) && length(x) > 0L && all(vapply(x, is_numeric_matrix, logical(1)))
}

is_numeric_array3_list <- function(x) {
  is.list(x) && length(x) > 0L && all(vapply(x, is_numeric_array3, logical(1)))
}

extract_nested_list_field <- function(x, field_name, validator) {
  if (validator(x)) {
    return(x)
  }
  if (!is.list(x)) {
    return(NULL)
  }
  nested <- x[[field_name]]
  if (!is.null(nested) && validator(nested)) {
    return(nested)
  }
  idx <- which(vapply(x, validator, logical(1)))
  if (length(idx) > 0L) {
    return(x[[idx[[1L]]]])
  }
  NULL
}

normalize_ndlm_ensemble_fields <- function(
  obj_name = "new.theta.out_50_NDLM_synth_DISC",
  assign_env = parent.frame()
) {
  if (!exists(obj_name, envir = assign_env, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  obj <- get(obj_name, envir = assign_env, inherits = FALSE)
  if (!is.list(obj)) {
    return(invisible(FALSE))
  }

  sm_raw <- obj$sm_ens
  sc_raw <- obj$sC_ens
  sm_fixed <- extract_nested_list_field(sm_raw, "sm_ens", is_numeric_matrix_list)
  sc_fixed <- extract_nested_list_field(sc_raw, "sC_ens", is_numeric_array3_list)

  normalized <- FALSE
  if (!is.null(sm_fixed) && !identical(sm_raw, sm_fixed)) {
    obj$sm_ens <- sm_fixed
    normalized <- TRUE
  }
  if (!is.null(sc_fixed) && !identical(sc_raw, sc_fixed)) {
    obj$sC_ens <- sc_fixed
    normalized <- TRUE
  }

  if (normalized) {
    assign(obj_name, obj, envir = assign_env)
    warning(
      sprintf(
        "Normalized %s ensemble fields to canonical structure (sm_ens=%d, sC_ens=%d).",
        obj_name,
        length(obj$sm_ens),
        length(obj$sC_ens)
      ),
      call. = FALSE
    )
  }
  invisible(normalized)
}

profile_section("ndlm_only.load_bundle", {
  load_ndlm_bundle_into(NDLM_VAR_50, assign_env = .GlobalEnv)
  normalize_ndlm_ensemble_fields(obj_name = "new.theta.out_50_NDLM_synth_DISC", assign_env = .GlobalEnv)
})

required_objects <- c(
  "new.theta.out_50_NDLM_synth_DISC",
  "samp.theta_50_NDLM_synth_DISC",
  "samp.sigma_50_NDLM_synth_DISC",
  "seq.elbo_50_NDLM_synth_DISC"
)
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)]
if (length(missing_objects) > 0L) {
  stop(
    sprintf(
      "[POST_NDLM_ONLY_INIT] Missing required NDLM objects after load: %s",
      paste(missing_objects, collapse = ", ")
    ),
    call. = FALSE
  )
}

ndlm_obj <- get("new.theta.out_50_NDLM_synth_DISC", envir = .GlobalEnv, inherits = FALSE)
if (!is.list(ndlm_obj)) {
  stop("[POST_NDLM_ONLY_INIT] new.theta.out_50_NDLM_synth_DISC must be a list.", call. = FALSE)
}
required_fields <- c("sm", "sC", "exps", "sm_ens", "sC_ens", "standard_forecast_errors")
missing_fields <- required_fields[!required_fields %in% names(ndlm_obj)]
if (length(missing_fields) > 0L) {
  stop(
    sprintf(
      "[POST_NDLM_ONLY_INIT] NDLM bundle missing required fields: %s",
      paste(missing_fields, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (!is_numeric_matrix(ndlm_obj$sm)) {
  stop("[POST_NDLM_ONLY_INIT] NDLM field 'sm' must be a numeric matrix.", call. = FALSE)
}
if (!is_numeric_array3(ndlm_obj$sC)) {
  stop("[POST_NDLM_ONLY_INIT] NDLM field 'sC' must be a numeric 3D array.", call. = FALSE)
}
if (!is_numeric_matrix(ndlm_obj$exps)) {
  stop("[POST_NDLM_ONLY_INIT] NDLM field 'exps' must be a numeric matrix.", call. = FALSE)
}
if (!is_numeric_matrix_list(ndlm_obj$sm_ens)) {
  stop("[POST_NDLM_ONLY_INIT] NDLM field 'sm_ens' must be a list of numeric matrices.", call. = FALSE)
}
if (!is_numeric_array3_list(ndlm_obj$sC_ens)) {
  stop("[POST_NDLM_ONLY_INIT] NDLM field 'sC_ens' must be a list of numeric 3D arrays.", call. = FALSE)
}
if (!is_numeric_matrix(ndlm_obj$standard_forecast_errors)) {
  stop("[POST_NDLM_ONLY_INIT] NDLM field 'standard_forecast_errors' must be a numeric matrix.", call. = FALSE)
}

message(
  sprintf(
    "[POST_NDLM_ONLY_INIT] loaded NDLM bundle: sm=%sx%s, sC=%sx%sx%s, exps=%sx%s, segments=%d",
    nrow(ndlm_obj$sm), ncol(ndlm_obj$sm),
    dim(ndlm_obj$sC)[1], dim(ndlm_obj$sC)[2], dim(ndlm_obj$sC)[3],
    nrow(ndlm_obj$exps), ncol(ndlm_obj$exps),
    length(ndlm_obj$sm_ens)
  )
)
