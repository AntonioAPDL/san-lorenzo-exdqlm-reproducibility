###############################################################################
# Paths (centralized)
# Inputs:
#   - Environment variables provided by unified stage_post
# Outputs:
#   - Path variables for all inputs/outputs used downstream
# Dependencies:
#   - Run-scoped artifacts are required in strict mode
###############################################################################

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

env_or_default <- function(key, default) {
  val <- Sys.getenv(key, unset = "")
  if (nzchar(val)) val else default
}

parse_date_env <- function(key, default) {
  raw <- env_or_default(key, default)
  parsed <- suppressWarnings(as.Date(raw))
  if (is.na(parsed)) {
    parsed <- as.Date(default)
  }
  parsed
}

env_flag <- function(key, default = "FALSE") {
  isTRUE(as.logical(Sys.getenv(key, default)))
}

split_env_paths <- function(key) {
  raw <- Sys.getenv(key, "")
  if (!nzchar(raw)) {
    return(character(0))
  }
  delim <- if (grepl("\n", raw, fixed = TRUE)) {
    "\n"
  } else if (grepl("|", raw, fixed = TRUE)) {
    "|"
  } else {
    ","
  }
  vals <- unlist(strsplit(raw, delim, fixed = TRUE), use.names = FALSE)
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0L) character(0) else path.expand(vals)
}

to_quantile_label <- function(x) {
  sprintf("%02d", as.integer(round(as.numeric(x))))
}

index_by_labels <- function(paths, labels) {
  labels <- as.character(labels)
  out <- setNames(rep("", length(labels)), labels)
  if (length(paths) == 0L || length(labels) == 0L) {
    return(out)
  }
  n <- min(length(paths), length(labels))
  out[seq_len(n)] <- paths[seq_len(n)]
  out
}

map_get <- function(map, key) {
  if (!(key %in% names(map))) "" else as.character(map[[key]])
}

nearest_available_label <- function(requested_label, path_map) {
  requested_label <- to_quantile_label(requested_label)
  available <- names(path_map)[nzchar(as.character(path_map))]
  if (length(available) == 0L) return("")
  req <- suppressWarnings(as.integer(requested_label))
  avail_int <- suppressWarnings(as.integer(available))
  if (!is.finite(req) || any(!is.finite(avail_int))) return(available[[1]])
  available[[which.min(abs(avail_int - req))]]
}

PROJECT_ROOT <- env_or_default("ENV_PROJECT_ROOT", "SOURCE_WORKFLOW_ROOT")
PROJECT_ROOT <- normalizePath(PROJECT_ROOT, mustWork = FALSE)
shared_helpers_path <- file.path(PROJECT_ROOT, "R", "unified", "families", "shared_input_helpers.R")
if (file.exists(shared_helpers_path)) {
  source(shared_helpers_path)
}
RUN_ROOT <- env_or_default("UNIFIED_RUN_ROOT", as.character(getOption("unified.run_root", "")))
RUN_ROOT <- if (nzchar(RUN_ROOT)) normalizePath(RUN_ROOT, mustWork = FALSE) else ""
RUN_ID <- env_or_default("UNIFIED_RUN_ID", as.character(getOption("unified.run_id", "")))
REPRO_MODE <- tolower(env_or_default("UNIFIED_REPRO_MODE", ""))
STRICT_RUNSCOPED_POST <- env_flag("UNIFIED_REQUIRE_RUNSCOPED_POST", "FALSE") || identical(REPRO_MODE, "strict")
ALLOW_LEGACY_ROOT_FALLBACK <- env_flag("UNIFIED_ALLOW_LEGACY_POST_FALLBACK", "FALSE")

MODEL_RUN_EXDQLM_MULTIVAR <- env_flag(
  "UNIFIED_MODEL_RUN_EXDQLM_MULTIVAR",
  if (isTRUE(getOption("unified.model_run_exdqlm_multivar", TRUE))) "TRUE" else "FALSE"
)
MODEL_RUN_EXDQLM_UNIVAR <- env_flag(
  "UNIFIED_MODEL_RUN_EXDQLM_UNIVAR",
  if (isTRUE(getOption("unified.model_run_exdqlm_univar", FALSE))) "TRUE" else "FALSE"
)
MODEL_RUN_NDLM_MAIN <- env_flag(
  "UNIFIED_MODEL_RUN_NDLM_MAIN",
  if (isTRUE(getOption("unified.model_run_ndlm_main", FALSE))) "TRUE" else "FALSE"
)
MODEL_RUN_NDLM_UNIVAR <- env_flag(
  "UNIFIED_MODEL_RUN_NDLM_UNIVAR",
  if (isTRUE(getOption("unified.model_run_ndlm_univar", FALSE))) "TRUE" else "FALSE"
)

POST_CACHE_DIR <- env_or_default("UNIFIED_POST_CACHE_DIR", as.character(getOption("unified.post_cache_dir", "")))
if (!nzchar(POST_CACHE_DIR)) {
  if (STRICT_RUNSCOPED_POST) {
    stop("UNIFIED_POST_CACHE_DIR is required in strict run-scoped post mode.", call. = FALSE)
  }
  POST_CACHE_DIR <- file.path(PROJECT_ROOT, "post", "cache")
  warning(
    sprintf("UNIFIED_POST_CACHE_DIR missing; using fallback cache path: %s", POST_CACHE_DIR),
    call. = FALSE
  )
}
POST_CACHE_DIR <- normalizePath(POST_CACHE_DIR, mustWork = FALSE)
dir.create(POST_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

post_cache_path <- function(name) {
  file.path(POST_CACHE_DIR, name)
}

legacy_root_path <- function(name) {
  file.path(PROJECT_ROOT, name)
}

require_runscoped_path <- function(path_value, what, fallback_path = NULL) {
  path_value <- as.character(path_value %||% "")
  if (nzchar(path_value) && file.exists(path_value)) {
    return(normalizePath(path_value, mustWork = FALSE))
  }
  if (STRICT_RUNSCOPED_POST) {
    stop(
      sprintf("Strict run-scoped mode: missing required %s path: %s", what, if (nzchar(path_value)) path_value else "<empty>"),
      call. = FALSE
    )
  }
  if (isTRUE(ALLOW_LEGACY_ROOT_FALLBACK) && !is.null(fallback_path) && nzchar(fallback_path)) {
    if (file.exists(fallback_path)) {
      warning(sprintf("Using legacy root fallback for %s: %s", what, fallback_path), call. = FALSE)
      return(normalizePath(fallback_path, mustWork = FALSE))
    }
    warning(sprintf("Legacy root fallback path missing for %s: %s", what, fallback_path), call. = FALSE)
  }
  if (nzchar(path_value)) {
    warning(sprintf("Run-scoped %s path not found: %s", what, path_value), call. = FALSE)
  } else {
    warning(sprintf("Run-scoped %s path is empty.", what), call. = FALSE)
  }
  ""
}

resolve_covariate_path <- function(env_key, fallback_path, what, allow_missing = FALSE) {
  env_path <- Sys.getenv(env_key, "")
  if (nzchar(env_path) && file.exists(env_path)) {
    return(normalizePath(env_path, mustWork = FALSE))
  }
  if (STRICT_RUNSCOPED_POST && !isTRUE(allow_missing)) {
    stop(
      sprintf(
        "Strict run-scoped mode: missing required %s path from %s",
        what,
        env_key
      ),
      call. = FALSE
    )
  }
  if (file.exists(fallback_path)) {
    return(normalizePath(fallback_path, mustWork = FALSE))
  }
  if (isTRUE(allow_missing)) {
    return("")
  }
  warning(sprintf("Missing %s path. env=%s fallback=%s", what, env_key, fallback_path), call. = FALSE)
  ""
}

# Canonical/reference output folder (do not write to directly in runs)
CANONICAL_FIG_DIR <- file.path(PROJECT_ROOT, "Environmetrics_reproduce")

# Date anchors for cutoff-dependent plots/data splits.
CUTOFF_DATE <- parse_date_env("UNIFIED_CUTOFF_DATE", "2022-12-25")
FORECAST_START_DATE <- parse_date_env("UNIFIED_FORECAST_START_DATE", as.character(CUTOFF_DATE + 1L))
PLOT_START_DATE <- parse_date_env("UNIFIED_PLOT_START", as.character(CUTOFF_DATE - 18L))
PLOT_END_DATE <- parse_date_env("UNIFIED_PLOT_END", as.character(CUTOFF_DATE + 28L))

COVARIATE_FEATURES_PATH <- env_or_default("ENV_COVARIATE_FEATURES_PATH", env_or_default("UNIFIED_COVARIATE_FEATURES_CSV", ""))
if (nzchar(COVARIATE_FEATURES_PATH)) {
  COVARIATE_FEATURES_PATH <- normalizePath(path.expand(COVARIATE_FEATURES_PATH), mustWork = FALSE)
}
FEATURE_TABLE_PRESENT <- nzchar(COVARIATE_FEATURES_PATH) && file.exists(COVARIATE_FEATURES_PATH)

# Core inputs
COV_ELI_PATH <- resolve_covariate_path(
  "ENV_COV_ELI_PATH",
  "LEGACY_EXAL_INPUT_REFERENCE",
  "ELI covariate",
  allow_missing = FEATURE_TABLE_PRESENT
)
COV_ONI_PATH <- resolve_covariate_path(
  "ENV_COV_ONI_PATH",
  "LEGACY_EXAL_INPUT_REFERENCE",
  "ONI covariate",
  allow_missing = FEATURE_TABLE_PRESENT
)

NWS_FORECAST_PATH <- env_or_default("ENV_NWS_FORECAST_PATH", file.path(PROJECT_ROOT, "nws_forecast.csv"))
GLOFAS_FORECAST_PATH <- env_or_default("ENV_GLOFAS_FORECAST_PATH", file.path(PROJECT_ROOT, "weighted_time_series.csv"))
USGS_DAILY_PATH <- require_runscoped_path(
  env_or_default("ENV_USGS_DAILY_PATH", env_or_default("UNIFIED_USGS_DAILY_CSV", "")),
  "USGS daily truth"
)

PPT_PATH <- resolve_covariate_path(
  "ENV_PPT_PATH",
  file.path(PROJECT_ROOT, "prism_precipitation_santa_cruz_1987_2023.csv"),
  "precipitation covariate"
)
SOIL_PATH <- resolve_covariate_path(
  "ENV_SOIL_PATH",
  file.path(PROJECT_ROOT, "soil_moisture_data", "soil_moisture_big_trees_daily_avg_1987_2023.csv"),
  "soil covariate"
)
PCA_PATH <- resolve_covariate_path(
  "ENV_PCA_PATH",
  file.path(PROJECT_ROOT, "pca.csv"),
  "PCA covariate"
)
retros_default_cutoff <- file.path(PROJECT_ROOT, sprintf("retros_%s.csv", format(CUTOFF_DATE, "%Y-%m-%d")))
retros_default_legacy <- file.path(PROJECT_ROOT, "retros_2022-12-25.csv")
RETROS_PATH <- env_or_default(
  "ENV_RETROS_PATH",
  if (file.exists(retros_default_cutoff)) retros_default_cutoff else retros_default_legacy
)

DATA_CBIND_RDS <- file.path(PROJECT_ROOT, "data_cbind_tY_X.rds")
DATA_CBIND_CSV <- file.path(PROJECT_ROOT, "data_cbind_tY_X.csv")

TIMESTAMPS_CSV <- file.path(PROJECT_ROOT, "timestamps.csv")

quantile_labels <- split_env_paths("UNIFIED_FIT_QUANTILE_LABELS")
if (length(quantile_labels) == 0L) {
  quantile_labels <- c("05", "20", "35", "50", "65", "80", "95")
}
quantile_labels <- to_quantile_label(quantile_labels)

UNIV_RDATA_PATHS <- split_env_paths("UNIFIED_UNIV_RDATA_PATHS")
DISC_W_RDATA_PATHS <- split_env_paths("UNIFIED_DISC_W_RDATA_PATHS")
NDLM_RDATA_PATH <- env_or_default("UNIFIED_NDLM_RDATA_PATH", "")
if (nzchar(NDLM_RDATA_PATH)) {
  NDLM_RDATA_PATH <- path.expand(NDLM_RDATA_PATH)
}
NDLM_UNIVAR_RDATA_PATH <- env_or_default("UNIFIED_NDLM_UNIVAR_RDATA_PATH", "")
if (nzchar(NDLM_UNIVAR_RDATA_PATH)) {
  NDLM_UNIVAR_RDATA_PATH <- path.expand(NDLM_UNIVAR_RDATA_PATH)
}

UNIV_RDATA_MAP <- index_by_labels(UNIV_RDATA_PATHS, quantile_labels)
DISC_W_RDATA_MAP <- index_by_labels(DISC_W_RDATA_PATHS, quantile_labels)

detect_disc_w_object_suffix <- function(paths, default = "DISC") {
  env_suffix <- trimws(Sys.getenv("UNIFIED_EXDQLM_MULTIVAR_OUTPUT_SUFFIX", ""))
  if (nzchar(env_suffix)) {
    env_suffix_lower <- tolower(env_suffix)
    if (env_suffix_lower %in% c("disc", "simp")) {
      return(if (identical(env_suffix_lower, "disc")) "DISC" else "simp")
    }
  }
  if (length(paths) > 0L) {
    for (path in as.character(paths)) {
      base <- basename(path)
      if (grepl("_exAL_synth_simp\\.RData$", base)) {
        return("simp")
      }
      if (grepl("_exAL_synth_DISC\\.RData$", base)) {
        return("DISC")
      }
    }
  }
  default
}

DISC_W_OBJECT_SUFFIX <- detect_disc_w_object_suffix(DISC_W_RDATA_PATHS, default = "DISC")

resolve_univar_path <- function(label) {
  label <- to_quantile_label(label)
  fallback <- legacy_root_path(sprintf("variables_%d_exAL_synth_DISC_uni.RData", as.integer(label)))
  require_runscoped_path(map_get(UNIV_RDATA_MAP, label), sprintf("univariate artifact q=%s", label), fallback)
}

resolve_disc_w_path <- function(label) {
  label <- to_quantile_label(label)
  fallback <- legacy_root_path(sprintf("DISC_variables_%d_exAL_synth_%s.RData", as.integer(label), DISC_W_OBJECT_SUFFIX))
  require_runscoped_path(map_get(DISC_W_RDATA_MAP, label), sprintf("DISC-W artifact q=%s", label), fallback)
}

resolve_univar_path_if_present <- function(label) {
  label <- to_quantile_label(label)
  has_label <- label %in% quantile_labels
  has_path <- nzchar(map_get(UNIV_RDATA_MAP, label))
  if (!(has_label || has_path)) {
    return("")
  }
  resolve_univar_path(label)
}

resolve_disc_w_path_if_present <- function(label) {
  label <- to_quantile_label(label)
  has_label <- label %in% quantile_labels
  has_path <- nzchar(map_get(DISC_W_RDATA_MAP, label))
  if (!(has_label || has_path)) {
    return("")
  }
  resolve_disc_w_path(label)
}

resolve_ndlm_path <- function() {
  fallback <- legacy_root_path("DISC_variables_50_NDLM_synth_DISC.RData")
  require_runscoped_path(NDLM_RDATA_PATH, "NDLM artifact q=50", fallback)
}

resolve_ndlm_univar_path <- function() {
  fallback <- legacy_root_path("DISC_variables_50_NDLM_univar_synth_DISC.RData")
  require_runscoped_path(NDLM_UNIVAR_RDATA_PATH, "NDLM univar artifact q=50", fallback)
}

resolve_univar_path_with_source <- function(label) {
  label <- to_quantile_label(label)
  src <- label
  path <- resolve_univar_path_if_present(label)
  if (!nzchar(path)) {
    src <- nearest_available_label(label, UNIV_RDATA_MAP)
    if (nzchar(src)) {
      path <- resolve_univar_path_if_present(src)
    }
  }
  list(path = path, source_label = src)
}

resolve_disc_w_path_with_source <- function(label) {
  label <- to_quantile_label(label)
  src <- label
  path <- resolve_disc_w_path_if_present(label)
  if (!nzchar(path)) {
    src <- nearest_available_label(label, DISC_W_RDATA_MAP)
    if (nzchar(src)) {
      path <- resolve_disc_w_path_if_present(src)
    }
  }
  list(path = path, source_label = src)
}

# Univariate outputs
if (isTRUE(MODEL_RUN_EXDQLM_UNIVAR) || length(UNIV_RDATA_PATHS) > 0L) {
  uni_05 <- resolve_univar_path_with_source("05")
  uni_20 <- resolve_univar_path_with_source("20")
  uni_35 <- resolve_univar_path_with_source("35")
  uni_50 <- resolve_univar_path_with_source("50")
  uni_65 <- resolve_univar_path_with_source("65")
  uni_80 <- resolve_univar_path_with_source("80")
  uni_95 <- resolve_univar_path_with_source("95")

  UNI_VAR_05 <- uni_05$path
  UNI_VAR_20 <- uni_20$path
  UNI_VAR_35 <- uni_35$path
  UNI_VAR_50 <- uni_50$path
  UNI_VAR_65 <- uni_65$path
  UNI_VAR_80 <- uni_80$path
  UNI_VAR_95 <- uni_95$path

  UNI_VAR_SRC_05 <- uni_05$source_label
  UNI_VAR_SRC_20 <- uni_20$source_label
  UNI_VAR_SRC_35 <- uni_35$source_label
  UNI_VAR_SRC_50 <- uni_50$source_label
  UNI_VAR_SRC_65 <- uni_65$source_label
  UNI_VAR_SRC_80 <- uni_80$source_label
  UNI_VAR_SRC_95 <- uni_95$source_label
} else {
  UNI_VAR_05 <- ""
  UNI_VAR_20 <- ""
  UNI_VAR_35 <- ""
  UNI_VAR_50 <- ""
  UNI_VAR_65 <- ""
  UNI_VAR_80 <- ""
  UNI_VAR_95 <- ""
  UNI_VAR_SRC_05 <- ""
  UNI_VAR_SRC_20 <- ""
  UNI_VAR_SRC_35 <- ""
  UNI_VAR_SRC_50 <- ""
  UNI_VAR_SRC_65 <- ""
  UNI_VAR_SRC_80 <- ""
  UNI_VAR_SRC_95 <- ""
}

# DISC-W and NDLM outputs
if (isTRUE(MODEL_RUN_EXDQLM_MULTIVAR) || length(DISC_W_RDATA_PATHS) > 0L) {
  disc_05 <- resolve_disc_w_path_with_source("05")
  disc_20 <- resolve_disc_w_path_with_source("20")
  disc_35 <- resolve_disc_w_path_with_source("35")
  disc_50 <- resolve_disc_w_path_with_source("50")
  disc_65 <- resolve_disc_w_path_with_source("65")
  disc_80 <- resolve_disc_w_path_with_source("80")
  disc_95 <- resolve_disc_w_path_with_source("95")

  DISC_W_VAR_05 <- disc_05$path
  DISC_W_VAR_20 <- disc_20$path
  DISC_W_VAR_35 <- disc_35$path
  DISC_W_VAR_50 <- disc_50$path
  DISC_W_VAR_65 <- disc_65$path
  DISC_W_VAR_80 <- disc_80$path
  DISC_W_VAR_95 <- disc_95$path

  DISC_W_VAR_SRC_05 <- disc_05$source_label
  DISC_W_VAR_SRC_20 <- disc_20$source_label
  DISC_W_VAR_SRC_35 <- disc_35$source_label
  DISC_W_VAR_SRC_50 <- disc_50$source_label
  DISC_W_VAR_SRC_65 <- disc_65$source_label
  DISC_W_VAR_SRC_80 <- disc_80$source_label
  DISC_W_VAR_SRC_95 <- disc_95$source_label
} else {
  DISC_W_VAR_05 <- ""
  DISC_W_VAR_20 <- ""
  DISC_W_VAR_35 <- ""
  DISC_W_VAR_50 <- ""
  DISC_W_VAR_65 <- ""
  DISC_W_VAR_80 <- ""
  DISC_W_VAR_95 <- ""
  DISC_W_VAR_SRC_05 <- ""
  DISC_W_VAR_SRC_20 <- ""
  DISC_W_VAR_SRC_35 <- ""
  DISC_W_VAR_SRC_50 <- ""
  DISC_W_VAR_SRC_65 <- ""
  DISC_W_VAR_SRC_80 <- ""
  DISC_W_VAR_SRC_95 <- ""
}

if (isTRUE(MODEL_RUN_NDLM_MAIN) || nzchar(NDLM_RDATA_PATH)) {
  NDLM_VAR_50 <- resolve_ndlm_path()
} else if (isTRUE(MODEL_RUN_NDLM_UNIVAR) || nzchar(NDLM_UNIVAR_RDATA_PATH)) {
  NDLM_VAR_50 <- resolve_ndlm_univar_path()
} else {
  NDLM_VAR_50 <- ""
}

if (isTRUE(MODEL_RUN_NDLM_UNIVAR) || nzchar(NDLM_UNIVAR_RDATA_PATH)) {
  NDLM_UNIVAR_VAR_50 <- resolve_ndlm_univar_path()
} else {
  NDLM_UNIVAR_VAR_50 <- ""
}
