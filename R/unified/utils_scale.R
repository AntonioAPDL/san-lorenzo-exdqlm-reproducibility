# unified/utils_scale.R

unified_scale_enum <- c("raw_cms", "log_cms", "log1p_cms", "log_log_cms", "log_log1p_cms")

unified_assert_known_scale <- function(scale, key = "scale") {
  if (!(scale %in% unified_scale_enum)) {
    stop(sprintf("%s must be one of [%s], got: %s", key, paste(unified_scale_enum, collapse = ", "), scale), call. = FALSE)
  }
}

unified_to_raw_cms <- function(x, from_scale) {
  unified_assert_known_scale(from_scale, "from_scale")
  if (from_scale == "raw_cms") return(x)
  if (from_scale == "log_cms") return(exp(x))
  if (from_scale == "log1p_cms") return(expm1(x))
  if (from_scale == "log_log_cms") return(exp(exp(x)))
  if (from_scale == "log_log1p_cms") return(expm1(exp(x)))
  stop("Unhandled from_scale")
}

unified_from_raw_cms <- function(x, to_scale) {
  unified_assert_known_scale(to_scale, "to_scale")
  if (to_scale == "raw_cms") return(x)
  if (to_scale == "log_cms") return(log(x))
  if (to_scale == "log1p_cms") return(log1p(x))
  if (to_scale == "log_log_cms") return(log(log(x)))
  if (to_scale == "log_log1p_cms") return(log(log1p(x)))
  stop("Unhandled to_scale")
}

unified_convert_scale <- function(x, from_scale, to_scale) {
  unified_assert_known_scale(from_scale, "from_scale")
  unified_assert_known_scale(to_scale, "to_scale")
  if (identical(from_scale, to_scale)) return(x)

  raw <- unified_to_raw_cms(x, from_scale)
  out <- unified_from_raw_cms(raw, to_scale)

  if (any(!is.finite(out), na.rm = TRUE)) {
    stop(sprintf("Non-finite values produced during scale conversion (%s -> %s)", from_scale, to_scale), call. = FALSE)
  }

  out
}

unified_assert_legacy_log_ready <- function(x, key = "adapter_output") {
  if (any(!is.finite(x), na.rm = TRUE)) {
    stop(sprintf("%s has non-finite values after conversion", key), call. = FALSE)
  }
  if (any(x <= 0, na.rm = TRUE)) {
    stop(sprintf("%s must be > 0 before legacy log() paths", key), call. = FALSE)
  }
}

unified_adapt_csv_scale <- function(input_path, output_path, from_scale, to_scale, positive_required = FALSE) {
  if (!file.exists(input_path)) {
    stop(sprintf("Input CSV missing: %s", input_path), call. = FALSE)
  }

  dat <- utils::read.csv(input_path, check.names = FALSE, stringsAsFactors = FALSE)
  num_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
  if (length(num_cols) == 0) {
    stop(sprintf("No numeric columns found for scale conversion: %s", input_path), call. = FALSE)
  }

  for (nm in num_cols) {
    dat[[nm]] <- unified_convert_scale(dat[[nm]], from_scale = from_scale, to_scale = to_scale)
  }

  if (positive_required) {
    for (nm in num_cols) {
      unified_assert_legacy_log_ready(dat[[nm]], key = sprintf("%s:%s", basename(output_path), nm))
    }
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(dat, output_path, row.names = FALSE)

  list(path = output_path, numeric_columns = num_cols)
}
