# Scale bridge for the legacy univariate exDQLM runner.
#
# The unified workflow writes legacy fit adapters on
# scale_contract$legacy_fit_input_scale and declares the model internal scale in
# scale_contract$analysis_scale_fit_internal.  The legacy script must therefore
# use this explicit conversion bridge instead of applying ad hoc log() calls.

univar_legacy_source_utils_scale <- function() {
  if (exists("unified_convert_scale", mode = "function")) {
    return(invisible(TRUE))
  }

  ancestors <- character()
  current <- normalizePath(getwd(), mustWork = FALSE)
  for (ii in seq_len(8L)) {
    ancestors <- c(ancestors, current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  candidates <- file.path(ancestors, "R", "unified", "utils_scale.R")
  candidates <- unique(normalizePath(candidates[file.exists(candidates)], mustWork = FALSE))
  if (length(candidates) < 1L) {
    stop("Could not locate R/unified/utils_scale.R for legacy univariate scale conversion.", call. = FALSE)
  }
  source(candidates[[1L]])
  if (!exists("unified_convert_scale", mode = "function")) {
    stop("R/unified/utils_scale.R did not define unified_convert_scale().", call. = FALSE)
  }
  invisible(TRUE)
}

univar_legacy_env_or_default <- function(key, default) {
  value <- Sys.getenv(key, "")
  if (!nzchar(value)) return(default)
  value
}

univar_legacy_resolve_scale_contract <- function(
  legacy_fit_input_scale = univar_legacy_env_or_default("UNIFIED_LEGACY_FIT_INPUT_SCALE", "log1p_cms"),
  analysis_scale_fit_internal = univar_legacy_env_or_default("UNIFIED_ANALYSIS_SCALE_FIT_INTERNAL", legacy_fit_input_scale),
  transform_policy = univar_legacy_env_or_default("UNIFIED_TRANSFORM_POLICY", "log1p_only")
) {
  univar_legacy_source_utils_scale()
  unified_assert_known_scale(legacy_fit_input_scale, "UNIFIED_LEGACY_FIT_INPUT_SCALE")
  unified_assert_known_scale(analysis_scale_fit_internal, "UNIFIED_ANALYSIS_SCALE_FIT_INTERNAL")

  if (identical(transform_policy, "log1p_only") && !identical(analysis_scale_fit_internal, "log1p_cms")) {
    stop(
      sprintf(
        "UNIFIED_TRANSFORM_POLICY=log1p_only forbids legacy univariate internal scale %s; use log1p_cms.",
        analysis_scale_fit_internal
      ),
      call. = FALSE
    )
  }

  list(
    legacy_fit_input_scale = legacy_fit_input_scale,
    analysis_scale_fit_internal = analysis_scale_fit_internal,
    transform_policy = transform_policy
  )
}

univar_legacy_transform_flow_values_to_internal_scale <- function(
  x,
  context_label,
  scale_contract = univar_legacy_resolve_scale_contract()
) {
  univar_legacy_source_utils_scale()
  x_dim <- dim(x)
  x_dimnames <- dimnames(x)
  out <- unified_convert_scale(
    as.numeric(x),
    from_scale = scale_contract$legacy_fit_input_scale,
    to_scale = scale_contract$analysis_scale_fit_internal
  )
  if (length(out) != length(x)) {
    stop(sprintf("%s scale conversion changed vector length unexpectedly.", context_label), call. = FALSE)
  }
  if (any(!is.finite(out), na.rm = TRUE)) {
    stop(sprintf("%s contains non-finite values after legacy univariate scale conversion.", context_label), call. = FALSE)
  }
  if (!is.null(x_dim)) {
    dim(out) <- x_dim
    dimnames(out) <- x_dimnames
  }
  out
}

univar_legacy_forecast_value_cols <- function(df) {
  setdiff(names(df), c("target_date", "Date", "date", "time"))
}

univar_legacy_transform_flow_frame_cols <- function(
  df,
  cols = univar_legacy_forecast_value_cols(df),
  context_label,
  scale_contract = univar_legacy_resolve_scale_contract()
) {
  if (length(cols) < 1L) {
    return(df)
  }
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L) {
    stop(sprintf("%s missing expected flow columns: %s", context_label, paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  for (nm in cols) {
    if (!is.numeric(df[[nm]])) {
      stop(sprintf("%s[%s] must be numeric before scale conversion.", context_label, nm), call. = FALSE)
    }
    df[[nm]] <- univar_legacy_transform_flow_values_to_internal_scale(
      df[[nm]],
      context_label = sprintf("%s[%s]", context_label, nm),
      scale_contract = scale_contract
    )
  }
  df
}
