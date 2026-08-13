#!/usr/bin/env Rscript

message("DEPRECATED entrypoint for unified workflow orchestration. Prefer: scripts/unified_run.R")

# Headless runner for Environmetrics figures (no comparisons, no nbconvert).

# -------------------------
# Config
# -------------------------
env_flag <- function(key, default = "FALSE") {
  isTRUE(as.logical(Sys.getenv(key, default)))
}

split_env_paths <- function(key) {
  raw <- Sys.getenv(key, "")
  if (!nzchar(raw)) return(character(0))
  delim <- if (grepl("\n", raw, fixed = TRUE)) {
    "\n"
  } else if (grepl("|", raw, fixed = TRUE)) {
    "|"
  } else {
    ","
  }
  parts <- unlist(strsplit(raw, delim, fixed = TRUE), use.names = FALSE)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) character(0) else unique(parts)
}

PROJECT_ROOT <- Sys.getenv("ENV_PROJECT_ROOT", "SOURCE_WORKFLOW_ROOT")
PROJECT_ROOT <- normalizePath(PROJECT_ROOT, mustWork = FALSE)
POST_CONTRACT_HELPERS <- file.path(PROJECT_ROOT, "R", "unified", "post_artifact_contract.R")
if (!file.exists(POST_CONTRACT_HELPERS)) {
  stop(sprintf("Missing post artifact contract helpers: %s", POST_CONTRACT_HELPERS), call. = FALSE)
}
source(POST_CONTRACT_HELPERS)
POST_MODULE_PLAN_HELPERS <- file.path(PROJECT_ROOT, "R", "unified", "post_module_plan.R")
if (!file.exists(POST_MODULE_PLAN_HELPERS)) {
  stop(sprintf("Missing post module-plan helpers: %s", POST_MODULE_PLAN_HELPERS), call. = FALSE)
}
source(POST_MODULE_PLAN_HELPERS)

RUN_ROOT <- Sys.getenv("UNIFIED_RUN_ROOT", "")
RUN_ID <- Sys.getenv("UNIFIED_RUN_ID", Sys.getenv("RUN_ID", ""))
POST_CACHE_DIR <- Sys.getenv("UNIFIED_POST_CACHE_DIR", "")
POST_OUTPUT_SUBDIR <- Sys.getenv("UNIFIED_POST_OUTPUT_SUBDIR", "")
POST_OUTPUT_SUBDIR <- gsub("[^A-Za-z0-9._-]+", "_", POST_OUTPUT_SUBDIR)
POST_OUTPUT_SUBDIR <- gsub("^_+|_+$", "", POST_OUTPUT_SUBDIR)
POST_OUTPUT_SUFFIX <- Sys.getenv("UNIFIED_POST_OUTPUT_SUFFIX", "")
POST_OUTPUT_SUFFIX <- gsub("[^A-Za-z0-9._-]+", "_", POST_OUTPUT_SUFFIX)
POST_PRESERVE_OUT_DIR <- env_flag("UNIFIED_POST_PRESERVE_OUT_DIR", "FALSE")
UNIFIED_REPRO_MODE <- tolower(Sys.getenv("UNIFIED_REPRO_MODE", ""))
STRICT_RUNSCOPED_POST <- env_flag("UNIFIED_REQUIRE_RUNSCOPED_POST", "FALSE") || identical(UNIFIED_REPRO_MODE, "strict")
ALLOW_LEGACY_ROOT_FALLBACK <- env_flag("UNIFIED_ALLOW_LEGACY_POST_FALLBACK", "FALSE")
MODEL_RUN_EXDQLM_MULTIVAR <- env_flag("UNIFIED_MODEL_RUN_EXDQLM_MULTIVAR", "TRUE")
MODEL_RUN_EXDQLM_UNIVAR <- env_flag("UNIFIED_MODEL_RUN_EXDQLM_UNIVAR", "FALSE")
MODEL_RUN_NDLM_MAIN <- env_flag("UNIFIED_MODEL_RUN_NDLM_MAIN", "FALSE")
MODEL_RUN_NDLM_UNIVAR <- env_flag("UNIFIED_MODEL_RUN_NDLM_UNIVAR", "FALSE")
POST_FIGURES <- env_flag("UNIFIED_POST_FIGURES", "TRUE")
POST_SMOKE_FAST <- env_flag("UNIFIED_POST_SMOKE_FAST", "FALSE")
POST_MULTIVAR_COMPONENT_DIAGNOSTICS <- env_flag("UNIFIED_POST_MULTIVAR_COMPONENT_DIAGNOSTICS", "FALSE")
POST_MULTIVAR_COMPONENT_FAIL_FAST <- env_flag("UNIFIED_POST_MULTIVAR_COMPONENT_FAIL_FAST", "TRUE")
POST_AUTHORITATIVE_SELECTED_SUPPORT <- env_flag("UNIFIED_POST_AUTHORITATIVE_SELECTED_SUPPORT", "FALSE")
POST_AUTHORITATIVE_SELECTED_SUPPORT_FAIL_FAST <- env_flag("UNIFIED_POST_AUTHORITATIVE_SELECTED_SUPPORT_FAIL_FAST", "TRUE")
POST_PUBLICATION_FIGURES <- env_flag("UNIFIED_POST_PUBLICATION_FIGURES", "TRUE")
POST_PUBLICATION_REWRITE_CANONICAL <- env_flag("UNIFIED_POST_PUBLICATION_REWRITE_CANONICAL", "TRUE")
POST_PUBLICATION_EXPORT_PDF <- env_flag("UNIFIED_POST_PUBLICATION_EXPORT_PDF", "TRUE")
POST_PUBLICATION_FAIL_FAST <- env_flag("UNIFIED_POST_PUBLICATION_FAIL_FAST", "TRUE")
POST_PUBLICATION_STYLE_PATH <- Sys.getenv("UNIFIED_POST_PUBLICATION_STYLE_PATH", "")

if (STRICT_RUNSCOPED_POST) {
  required <- c("UNIFIED_RUN_ROOT", "UNIFIED_RUN_ID", "UNIFIED_POST_CACHE_DIR")
  required_vals <- vapply(required, function(k) Sys.getenv(k, ""), character(1))
  missing <- required[!nzchar(required_vals)]
  if (length(missing) > 0L) {
    stop(sprintf("Strict run-scoped post requires env vars: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
}

if (nzchar(RUN_ROOT)) RUN_ROOT <- normalizePath(RUN_ROOT, mustWork = FALSE)
if (nzchar(POST_CACHE_DIR)) {
  dir.create(POST_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  POST_CACHE_DIR <- normalizePath(POST_CACHE_DIR, mustWork = FALSE)
}

DISC_W_RDATA_PATHS <- split_env_paths("UNIFIED_DISC_W_RDATA_PATHS")
UNIV_RDATA_PATHS <- split_env_paths("UNIFIED_UNIV_RDATA_PATHS")
NDLM_RDATA_PATH <- Sys.getenv("UNIFIED_NDLM_RDATA_PATH", "")
if (nzchar(NDLM_RDATA_PATH)) NDLM_RDATA_PATH <- path.expand(NDLM_RDATA_PATH)
NDLM_UNIVAR_RDATA_PATH <- Sys.getenv("UNIFIED_NDLM_UNIVAR_RDATA_PATH", "")
if (nzchar(NDLM_UNIVAR_RDATA_PATH)) NDLM_UNIVAR_RDATA_PATH <- path.expand(NDLM_UNIVAR_RDATA_PATH)

options(
  unified.run_root = RUN_ROOT,
  unified.run_id = RUN_ID,
  unified.post_cache_dir = POST_CACHE_DIR,
  unified.strict_runscoped_post = STRICT_RUNSCOPED_POST,
  unified.allow_legacy_root_fallback = ALLOW_LEGACY_ROOT_FALLBACK,
  unified.disc_w_rdata_paths = DISC_W_RDATA_PATHS,
  unified.univ_rdata_paths = UNIV_RDATA_PATHS,
  unified.ndlm_rdata_path = NDLM_RDATA_PATH,
  unified.ndlm_univar_rdata_path = NDLM_UNIVAR_RDATA_PATH,
  unified.model_run_exdqlm_multivar = MODEL_RUN_EXDQLM_MULTIVAR,
  unified.model_run_exdqlm_univar = MODEL_RUN_EXDQLM_UNIVAR,
  unified.model_run_ndlm_main = MODEL_RUN_NDLM_MAIN,
  unified.model_run_ndlm_univar = MODEL_RUN_NDLM_UNIVAR,
  unified.post_figures = POST_FIGURES
)

NDLM_ANY_MODE <- isTRUE(MODEL_RUN_NDLM_MAIN) || isTRUE(MODEL_RUN_NDLM_UNIVAR)
NDLM_ONLY_MODE <- isTRUE(NDLM_ANY_MODE) &&
  !isTRUE(MODEL_RUN_EXDQLM_MULTIVAR) &&
  !isTRUE(MODEL_RUN_EXDQLM_UNIVAR)
POST_SMOKE_FAST_EFFECTIVE <- isTRUE(POST_SMOKE_FAST)

OUT_PARENT <- if (nzchar(RUN_ROOT)) {
  file.path(RUN_ROOT, "post", "outputs")
} else {
  file.path(PROJECT_ROOT, "Environmetrics_reproduce_script_runs")
}
if (!nzchar(RUN_ID)) {
  RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S")
}
OUT_DIR <- file.path(OUT_PARENT, RUN_ID)
if (nzchar(POST_OUTPUT_SUBDIR)) {
  OUT_DIR <- file.path(OUT_DIR, POST_OUTPUT_SUBDIR)
}
SEED <- 777
PROFILE <- env_flag("PROFILE", "FALSE")
PROFILE_DETAIL <- env_flag("PROFILE_DETAIL", "FALSE")
ENV_SORT_KEEP_NA <- env_flag("ENV_SORT_KEEP_NA", "TRUE")
EXPORT_TABLES <- env_flag("EXPORT_TABLES", "TRUE")

# Deterministic settings (match notebook)
set.seed(SEED)
RNGkind("Mersenne-Twister", "Inversion", "Rounding")
options(stringsAsFactors = FALSE)

# -------------------------
# Logging
# -------------------------
log_dir <- if (nzchar(RUN_ROOT)) {
  if (nzchar(POST_OUTPUT_SUBDIR)) {
    file.path(RUN_ROOT, "post", "logs", RUN_ID, POST_OUTPUT_SUBDIR)
  } else {
    file.path(RUN_ROOT, "post", "logs", RUN_ID)
  }
} else {
  if (nzchar(POST_OUTPUT_SUBDIR)) {
    file.path(PROJECT_ROOT, "repro", "logs", "script_runs", RUN_ID, POST_OUTPUT_SUBDIR)
  } else {
    file.path(PROJECT_ROOT, "repro", "logs", "script_runs", RUN_ID)
  }
}
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
log_path <- file.path(log_dir, "run_log.txt")
con <- file(log_path, open = "wt")
sink(con, split = TRUE)
cat(sprintf("START: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
git_hash <- tryCatch(system("git rev-parse HEAD", intern = TRUE), error = function(e) "UNKNOWN")
cat(sprintf("GIT_COMMIT: %s\n", git_hash))
cat(sprintf("OUT_DIR: %s\n", OUT_DIR))
cat(sprintf("POST_OUTPUT_SUBDIR: %s\n", if (nzchar(POST_OUTPUT_SUBDIR)) POST_OUTPUT_SUBDIR else "<root>"))
cat(sprintf("POST_OUTPUT_SUFFIX: %s\n", if (nzchar(POST_OUTPUT_SUFFIX)) POST_OUTPUT_SUFFIX else "<none>"))
cat(sprintf("POST_PRESERVE_OUT_DIR: %s\n", POST_PRESERVE_OUT_DIR))
cat(sprintf("SEED: %s\n", SEED))
cat(sprintf("PROFILE: %s\n", PROFILE))
cat(sprintf("PROFILE_DETAIL: %s\n", PROFILE_DETAIL))
cat(sprintf("ENV_SORT_KEEP_NA: %s\n", ENV_SORT_KEEP_NA))
cat(sprintf("EXPORT_TABLES: %s\n", EXPORT_TABLES))
cat(sprintf("RUN_ROOT: %s\n", RUN_ROOT))
cat(sprintf("POST_CACHE_DIR: %s\n", POST_CACHE_DIR))
cat(sprintf("STRICT_RUNSCOPED_POST: %s\n", STRICT_RUNSCOPED_POST))
cat(sprintf("ALLOW_LEGACY_ROOT_FALLBACK: %s\n", ALLOW_LEGACY_ROOT_FALLBACK))
cat(sprintf("MODEL_RUN_EXDQLM_MULTIVAR: %s\n", MODEL_RUN_EXDQLM_MULTIVAR))
cat(sprintf("MODEL_RUN_EXDQLM_UNIVAR: %s\n", MODEL_RUN_EXDQLM_UNIVAR))
cat(sprintf("MODEL_RUN_NDLM_MAIN: %s\n", MODEL_RUN_NDLM_MAIN))
cat(sprintf("MODEL_RUN_NDLM_UNIVAR: %s\n", MODEL_RUN_NDLM_UNIVAR))
cat(sprintf("POST_FIGURES: %s\n", POST_FIGURES))
cat(sprintf("POST_SMOKE_FAST: %s\n", POST_SMOKE_FAST))
cat(sprintf("POST_MULTIVAR_COMPONENT_DIAGNOSTICS: %s\n", POST_MULTIVAR_COMPONENT_DIAGNOSTICS))
cat(sprintf("POST_MULTIVAR_COMPONENT_FAIL_FAST: %s\n", POST_MULTIVAR_COMPONENT_FAIL_FAST))
cat(sprintf("POST_AUTHORITATIVE_SELECTED_SUPPORT: %s\n", POST_AUTHORITATIVE_SELECTED_SUPPORT))
cat(sprintf("POST_AUTHORITATIVE_SELECTED_SUPPORT_FAIL_FAST: %s\n", POST_AUTHORITATIVE_SELECTED_SUPPORT_FAIL_FAST))
cat(sprintf("POST_PUBLICATION_FIGURES: %s\n", POST_PUBLICATION_FIGURES))
cat(sprintf("POST_PUBLICATION_REWRITE_CANONICAL: %s\n", POST_PUBLICATION_REWRITE_CANONICAL))
cat(sprintf("POST_PUBLICATION_EXPORT_PDF: %s\n", POST_PUBLICATION_EXPORT_PDF))
cat(sprintf("POST_PUBLICATION_FAIL_FAST: %s\n", POST_PUBLICATION_FAIL_FAST))
cat(sprintf("POST_PUBLICATION_STYLE_PATH: %s\n", if (nzchar(POST_PUBLICATION_STYLE_PATH)) POST_PUBLICATION_STYLE_PATH else "<default>"))
cat(sprintf("UNIFIED_LEGACY_FIT_INPUT_SCALE: %s\n", Sys.getenv("UNIFIED_LEGACY_FIT_INPUT_SCALE", "<unset>")))
cat(sprintf("UNIFIED_ANALYSIS_SCALE_FIT_INTERNAL: %s\n", Sys.getenv("UNIFIED_ANALYSIS_SCALE_FIT_INTERNAL", "<unset>")))
cat(sprintf("UNIFIED_LEGACY_POST_INPUT_SCALE: %s\n", Sys.getenv("UNIFIED_LEGACY_POST_INPUT_SCALE", "<unset>")))
cat(sprintf("UNIFIED_ANALYSIS_SCALE_POST_INTERNAL: %s\n", Sys.getenv("UNIFIED_ANALYSIS_SCALE_POST_INTERNAL", "<unset>")))
cat(sprintf("UNIFIED_TRANSFORM_POLICY: %s\n", Sys.getenv("UNIFIED_TRANSFORM_POLICY", "<unset>")))
if (length(DISC_W_RDATA_PATHS) > 0L) {
  cat("DISC_W_RDATA_PATHS:\n")
  cat(paste0(" - ", DISC_W_RDATA_PATHS, collapse = "\n"), "\n")
} else {
  cat("DISC_W_RDATA_PATHS: <none>\n")
}
if (length(UNIV_RDATA_PATHS) > 0L) {
  cat("UNIV_RDATA_PATHS:\n")
  cat(paste0(" - ", UNIV_RDATA_PATHS, collapse = "\n"), "\n")
} else {
  cat("UNIV_RDATA_PATHS: <none>\n")
}
cat(sprintf("NDLM_RDATA_PATH: %s\n", if (nzchar(NDLM_RDATA_PATH)) NDLM_RDATA_PATH else "<none>"))
cat(sprintf("NDLM_UNIVAR_RDATA_PATH: %s\n", if (nzchar(NDLM_UNIVAR_RDATA_PATH)) NDLM_UNIVAR_RDATA_PATH else "<none>"))

check_required_paths <- function(paths, label) {
  if (length(paths) == 0L) {
    stop(sprintf("No run-scoped %s path(s) were provided.", label), call. = FALSE)
  }
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop(
      sprintf("Missing run-scoped %s path(s): %s", label, paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
}

if (STRICT_RUNSCOPED_POST) {
  if (MODEL_RUN_EXDQLM_MULTIVAR) check_required_paths(DISC_W_RDATA_PATHS, "DISC-W artifact")
  if (MODEL_RUN_EXDQLM_UNIVAR) check_required_paths(UNIV_RDATA_PATHS, "univariate artifact")
  if (MODEL_RUN_NDLM_MAIN) check_required_paths(NDLM_RDATA_PATH, "NDLM artifact")
  if (MODEL_RUN_NDLM_UNIVAR) check_required_paths(NDLM_UNIVAR_RDATA_PATH, "NDLM univar artifact")
}

# capture session info
session_path <- file.path(log_dir, "sessionInfo.txt")
writeLines(capture.output(sessionInfo()), session_path)

# -------------------------
# Path redirection helper (force outputs into OUT_DIR)
# -------------------------
is_runscoped_target <- function(path) {
  if (!nzchar(path) || !nzchar(RUN_ROOT)) return(FALSE)
  path_norm <- normalizePath(path, mustWork = FALSE)
  run_root_norm <- normalizePath(RUN_ROOT, mustWork = FALSE)
  cache_norm <- if (nzchar(POST_CACHE_DIR)) normalizePath(POST_CACHE_DIR, mustWork = FALSE) else ""

  startsWith(path_norm, paste0(run_root_norm, .Platform$file.sep)) ||
    identical(path_norm, run_root_norm) ||
    (nzchar(cache_norm) && (startsWith(path_norm, paste0(cache_norm, .Platform$file.sep)) || identical(path_norm, cache_norm)))
}

redirect_path <- function(filename) {
  append_suffix <- function(path_value, suffix_value) {
    path_chr <- as.character(path_value)
    if (!nzchar(path_chr) || !nzchar(suffix_value)) return(path_chr)
    ext <- tools::file_ext(path_chr)
    stem <- if (nzchar(ext)) {
      substr(path_chr, 1L, nchar(path_chr) - nchar(ext) - 1L)
    } else {
      path_chr
    }
    if (endsWith(stem, suffix_value)) return(path_chr)
    if (nzchar(ext)) {
      sprintf("%s%s.%s", stem, suffix_value, ext)
    } else {
      paste0(stem, suffix_value)
    }
  }

  dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
  if (is.null(filename)) return(filename)
  filename_chr <- as.character(filename)
  if (!nzchar(filename_chr)) return(filename_chr)

  # Preserve explicit run-scoped/cache paths so later readRDS/read.csv calls can resolve them.
  if (is_runscoped_target(filename_chr)) {
    dir.create(dirname(filename_chr), showWarnings = FALSE, recursive = TRUE)
    return(normalizePath(filename_chr, mustWork = FALSE))
  }

  file.path(OUT_DIR, append_suffix(basename(filename_chr), POST_OUTPUT_SUFFIX))
}

# Clean only OUT_DIR
if (dir.exists(OUT_DIR) && !isTRUE(POST_PRESERVE_OUT_DIR)) {
  unlink(OUT_DIR, recursive = TRUE, force = TRUE)
}
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Override ggsave and graphic devices to prevent overwriting canonical outputs
if (!exists("ggsave_original", inherits = FALSE)) {
  ggsave_original <- ggplot2::ggsave
}

ggsave <- function(filename, plot = ggplot2::last_plot(), ...) {
  out_path <- redirect_path(filename)
  t0 <- Sys.time()
  on.exit({
    t1 <- Sys.time()
    log_io_timing("ggsave", out_path, t0, t1)
  }, add = TRUE)
  do.call(ggsave_original, c(list(filename = out_path, plot = plot), list(...)))
}

last_device_file <- NULL
last_device_kind <- NULL

png <- function(filename, ...) {
  out_path <- redirect_path(filename)
  last_device_file <<- out_path
  last_device_kind <<- "png"
  do.call(grDevices::png, c(list(filename = out_path), list(...)))
}

pdf <- function(file, ...) {
  out_path <- redirect_path(file)
  last_device_file <<- out_path
  last_device_kind <<- "pdf"
  do.call(grDevices::pdf, c(list(file = out_path), list(...)))
}

jpeg <- function(filename, ...) {
  out_path <- redirect_path(filename)
  last_device_file <<- out_path
  last_device_kind <<- "jpeg"
  do.call(grDevices::jpeg, c(list(filename = out_path), list(...)))
}

tiff <- function(filename, ...) {
  out_path <- redirect_path(filename)
  last_device_file <<- out_path
  last_device_kind <<- "tiff"
  do.call(grDevices::tiff, c(list(filename = out_path), list(...)))
}

svg <- function(filename, ...) {
  out_path <- redirect_path(filename)
  last_device_file <<- out_path
  last_device_kind <<- "svg"
  do.call(grDevices::svg, c(list(filename = out_path), list(...)))
}

dev_off_original <- grDevices::dev.off
dev.off <- function(...) {
  t0 <- Sys.time()
  on.exit({
    t1 <- Sys.time()
    kind <- if (!is.null(last_device_kind)) paste0(last_device_kind, ".dev.off") else "dev.off"
    log_io_timing(kind, last_device_file, t0, t1)
    last_device_file <<- NULL
    last_device_kind <<- NULL
  }, add = TRUE)
  dev_off_original(...)
}

if (!exists("saveRDS_original", inherits = FALSE)) {
  saveRDS_original <- base::saveRDS
}
saveRDS <- function(object, file = "", ...) {
  out_path <- redirect_path(file)
  t0 <- Sys.time()
  on.exit({
    t1 <- Sys.time()
    log_io_timing("saveRDS", out_path, t0, t1)
  }, add = TRUE)
  saveRDS_original(object = object, file = out_path, ...)
}

if (!exists("write.csv_original", inherits = FALSE)) {
  write.csv_original <- utils::write.csv
}
write.csv <- function(x, file = "", ...) {
  out_path <- redirect_path(file)
  t0 <- Sys.time()
  on.exit({
    t1 <- Sys.time()
    log_io_timing("write.csv", out_path, t0, t1)
  }, add = TRUE)
  write.csv_original(x = x, file = out_path, ...)
}

# -------------------------
# Execute modularized notebook export (preserve order)
# -------------------------
modules_dir <- file.path(PROJECT_ROOT, "R", "environmetrics")
core_modules <- c(
  "00_paths.R",
  "00_setup.R",
  "00_constants.R",
  "01_config.R",
  "02_helpers_core.R",
  "utils_data.R",
  "utils_plot.R"
)
modules <- unified_post_select_modules(
  post_figures = POST_FIGURES,
  post_smoke_fast = POST_SMOKE_FAST_EFFECTIVE,
  model_run_exdqlm_multivar = MODEL_RUN_EXDQLM_MULTIVAR,
  model_run_exdqlm_univar = MODEL_RUN_EXDQLM_UNIVAR,
  model_run_ndlm_main = MODEL_RUN_NDLM_MAIN,
  model_run_ndlm_univar = MODEL_RUN_NDLM_UNIVAR,
  core_modules = core_modules,
  multivar_component_diagnostics = POST_MULTIVAR_COMPONENT_DIAGNOSTICS
)

missing <- modules[!file.exists(file.path(modules_dir, modules))]
if (length(missing) > 0) {
  stop(sprintf(
    "Missing modular files in %s: %s\nRecreate modules from Environmetrics_Figures__OLDEST_linearized.R.",
    modules_dir,
    paste(missing, collapse = ", ")
  ))
}

log_step <- function(msg) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

profile_dir <- NULL
timings_path <- NULL
io_timings_path <- NULL
if (PROFILE) {
  profile_dir <- if (nzchar(RUN_ROOT)) {
    file.path(RUN_ROOT, "post", "profile", RUN_ID)
  } else {
    file.path(PROJECT_ROOT, "repro", "logs", "profile", RUN_ID)
  }
  dir.create(profile_dir, showWarnings = FALSE, recursive = TRUE)
  timings_path <- file.path(profile_dir, "timings.csv")
  writeLines("section,start,end,elapsed_sec", timings_path)
  io_timings_path <- file.path(profile_dir, "io_timings.csv")
  writeLines("kind,file,start,end,elapsed_sec,file_bytes", io_timings_path)
}

log_timing <- function(section, start_time, end_time) {
  if (!PROFILE) return(invisible(NULL))
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  line <- sprintf("%s,%s,%s,%.6f", section, start_time, end_time, elapsed)
  write(line, file = timings_path, append = TRUE)
}

log_io_timing <- function(kind, file, start_time, end_time) {
  if (!PROFILE) return(invisible(NULL))
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  file_bytes <- NA_integer_
  if (!is.null(file) && !is.na(file) && nzchar(file) && file.exists(file)) {
    file_bytes <- as.integer(file.info(file)$size)
  }
  safe_file <- if (is.null(file) || is.na(file)) "" else file
  line <- sprintf("%s,%s,%s,%s,%.6f,%s", kind, safe_file, start_time, end_time, elapsed, file_bytes)
  write(line, file = io_timings_path, append = TRUE)
}

## Pre-check: paths and inputs (fast, no parsing)
paths_file <- file.path(modules_dir, "00_paths.R")
if (file.exists(paths_file)) {
  source(paths_file)
  check_script <- file.path(PROJECT_ROOT, "scripts", "check_inputs.R")
  if (POST_FIGURES && file.exists(check_script)) {
    if (STRICT_RUNSCOPED_POST) {
      log_step("SKIP check_inputs (STRICT_RUNSCOPED_POST=TRUE)")
    } else {
      source(check_script)
      log_step("START check_inputs")
      check_inputs()
      log_step("END check_inputs")
    }
  } else if (!POST_FIGURES) {
    log_step("SKIP check_inputs (POST_FIGURES=FALSE)")
  }
}

for (mod in modules) {
  log_step(paste("START", mod))
  t0 <- Sys.time()
  source(file.path(modules_dir, mod))
  t1 <- Sys.time()
  log_step(paste("END", mod))
  log_timing(mod, t0, t1)
}

publication_helpers <- file.path(PROJECT_ROOT, "R", "unified", "post_publication_figures.R")
if (POST_FIGURES && POST_PUBLICATION_FIGURES) {
  if (!file.exists(publication_helpers)) {
    stop(sprintf("Missing publication figure helpers: %s", publication_helpers), call. = FALSE)
  }
  log_step("START publication_figure_rewrite")
  t0 <- Sys.time()
  source(publication_helpers)
  pub_result <- unified_render_publication_figures(
    outputs_dir = OUT_DIR,
    run_id = RUN_ID,
    project_root = PROJECT_ROOT,
    enabled = TRUE,
    rewrite_canonical_png = POST_PUBLICATION_REWRITE_CANONICAL,
    export_pdf = POST_PUBLICATION_EXPORT_PDF,
    fail_fast = POST_PUBLICATION_FAIL_FAST,
    style_config_path = if (nzchar(POST_PUBLICATION_STYLE_PATH)) POST_PUBLICATION_STYLE_PATH else NULL
  )
  t1 <- Sys.time()
  log_step(sprintf(
    "END publication_figure_rewrite rendered=%d skipped=%d",
    as.integer(pub_result$rendered %||% 0L),
    as.integer(pub_result$skipped %||% 0L)
  ))
  log_timing("publication_figure_rewrite", t0, t1)
}

if (!POST_FIGURES) {
  marker <- file.path(OUT_DIR, "post_smoke_marker.txt")
  writeLines(
    c(
      sprintf("run_id=%s", RUN_ID),
      sprintf("run_root=%s", RUN_ROOT),
      sprintf("post_cache_dir=%s", POST_CACHE_DIR),
      sprintf("strict_runscoped_post=%s", STRICT_RUNSCOPED_POST),
      sprintf("disc_w_paths=%d", length(DISC_W_RDATA_PATHS)),
      sprintf("univ_paths=%d", length(UNIV_RDATA_PATHS)),
      sprintf("ndlm_path_present=%s", nzchar(NDLM_RDATA_PATH)),
      sprintf("ndlm_univar_path_present=%s", nzchar(NDLM_UNIVAR_RDATA_PATH))
    ),
    marker
  )
  log_step(sprintf("WROTE %s", marker))
}

post_artifacts <- unified_collect_post_artifacts(
  outputs_dir = OUT_DIR,
  cache_dir = if (nzchar(POST_CACHE_DIR)) POST_CACHE_DIR else NULL
)
post_contract <- unified_post_contract_check(
  artifacts_df = post_artifacts,
  outputs_dir = OUT_DIR,
  cache_dir = if (nzchar(POST_CACHE_DIR)) POST_CACHE_DIR else NULL,
  post_figures = isTRUE(POST_FIGURES),
  export_tables = isTRUE(EXPORT_TABLES),
  post_smoke_fast = isTRUE(POST_SMOKE_FAST_EFFECTIVE),
  multivar_component_diagnostics = isTRUE(POST_MULTIVAR_COMPONENT_DIAGNOSTICS),
  multivar_component_fail_fast = isTRUE(POST_MULTIVAR_COMPONENT_FAIL_FAST),
  authoritative_selected_model_support = isTRUE(POST_AUTHORITATIVE_SELECTED_SUPPORT),
  authoritative_selected_model_support_fail_fast = isTRUE(POST_AUTHORITATIVE_SELECTED_SUPPORT_FAIL_FAST),
  multivar_component_transfer_mode = Sys.getenv("UNIFIED_MULTIVAR_FORECAST_TRANSFER_MODE", ""),
  model_run_exdqlm_multivar = isTRUE(MODEL_RUN_EXDQLM_MULTIVAR),
  model_run_exdqlm_univar = isTRUE(MODEL_RUN_EXDQLM_UNIVAR),
  model_run_ndlm_main = isTRUE(MODEL_RUN_NDLM_MAIN),
  model_run_ndlm_univar = isTRUE(MODEL_RUN_NDLM_UNIVAR)
)
post_artifact_reports <- unified_write_post_artifact_reports(
  artifacts_df = post_artifacts,
  outputs_dir = OUT_DIR,
  run_id = RUN_ID,
  cache_dir = if (nzchar(POST_CACHE_DIR)) POST_CACHE_DIR else NULL,
  contract = post_contract
)
log_step(sprintf("WROTE %s", post_artifact_reports$manifest_path))
log_step(sprintf("WROTE %s", post_artifact_reports$summary_path))

if (!isTRUE(post_contract$status)) {
  fail_parts <- c(
    "[POST_ARTIFACT_CONTRACT_FAIL] post artifact contract check failed.",
    post_contract$messages
  )
  stop(paste(fail_parts[nzchar(fail_parts)], collapse = " "), call. = FALSE)
}

cat(sprintf("END: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

sink()
close(con)
