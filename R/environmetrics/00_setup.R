#!/usr/bin/env Rscript

# =============================================================================
# Setup: library paths and package imports
# Inputs:
#   - None
# Outputs:
#   - Loads required packages into session
# Dependencies:
#   - R packages listed below must be installed
# =============================================================================
libs_only <- identical(Sys.getenv("ENVIRONMETRICS_LIBS_ONLY", "0"), "1")
if (!libs_only) {
  .libPaths(unique(c(.libPaths(), path.expand("~/R/libs"))))
}
print(.libPaths())

# Prefer Cairo-backed bitmap devices so post-stage PNG rendering stays
# headless-safe during replay and queue runs.
if (isTRUE(capabilities("cairo"))) {
  options(bitmapType = "cairo")
}

load_required_pkg <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(sprintf("Required package '%s' is not installed.", pkg), call. = FALSE)
  }
}

load_optional_pkg <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(sprintf("Optional package '%s' is not installed; continuing without it.", pkg))
    return(FALSE)
  }
  TRUE
}

# Core analysis + model dependencies
invisible(lapply(c(
  "parallel", "dlm", "exdqlm", "mvtnorm", "jmuOutlier", "sn", "Matrix",
  "numDeriv", "foreach", "doParallel", "zoo", "patchwork", "expint",
  "nimble", "nloptr", "expm", "RcppArmadillo", "RcppEigen", "ks", "MASS",
  "FNN", "matrixStats", "truncnorm", "isotone", "ggplot2", "dplyr", "readr",
  "tidyr", "scales", "lubridate"
), load_required_pkg))

invisible(lapply(c("dataRetrieval", "tseries", "rvest", "tidyverse"), load_optional_pkg))

# Tidyverse (explicit + meta)
# Already loaded above; keep the section for readability.

# Legacy state-space helpers expected by older exAL scripts.
if (!exists("combineMods", mode = "function")) {
  combineMods <- function(mod1, mod2) {
    if (inherits(mod1, "exdqlm") && inherits(mod2, "exdqlm")) {
      return(mod1 + mod2)
    }
    if (inherits(mod1, "dlm") && inherits(mod2, "dlm")) {
      return(get("%+%", envir = asNamespace("dlm"))(mod1, mod2))
    }
    stop("combineMods requires matching 'exdqlm' or 'dlm' objects.", call. = FALSE)
  }
}

# =============================================================================
# Lightweight profiling helpers (used by modules; controlled by PROFILE flag)
# =============================================================================
is_profile_enabled <- function() {
  exists("PROFILE", inherits = TRUE) && isTRUE(get("PROFILE", inherits = TRUE))
}

profile_section <- function(section, expr) {
  expr <- substitute(expr)
  if (!is_profile_enabled()) {
    return(eval(expr, envir = parent.frame()))
  }
  t0 <- Sys.time()
  on.exit({
    t1 <- Sys.time()
    if (exists("log_timing", inherits = TRUE)) {
      # Avoid commas in section labels (CSV output in runner)
      safe_section <- gsub(",", ";", section, fixed = TRUE)
      log_timing(safe_section, t0, t1)
    }
  }, add = TRUE)
  eval(expr, envir = parent.frame())
}

# =============================================================================
# Optional detailed profiling (sampling profiler via Rprof)
# - Enabled only when PROFILE_DETAIL=TRUE in the runner env.
# - Designed to wrap a few heavy blocks (do not use for many short sections).
# - Writes outputs under repro/logs/profile/<RUN_ID>/ when available.
# =============================================================================
is_profile_detail_enabled <- function() {
  exists("PROFILE_DETAIL", inherits = TRUE) && isTRUE(get("PROFILE_DETAIL", inherits = TRUE))
}

get_profile_detail_section_filter <- function() {
  if (exists("PROFILE_DETAIL_SECTION", inherits = TRUE)) {
    val <- get("PROFILE_DETAIL_SECTION", inherits = TRUE)
    if (is.character(val) && length(val) == 1L && nzchar(val)) {
      return(val)
    }
  }
  Sys.getenv("PROFILE_DETAIL_SECTION", "")
}

profile_detail_section <- function(section, expr) {
  expr <- substitute(expr)
  if (!is_profile_detail_enabled()) {
    return(eval(expr, envir = parent.frame()))
  }

  filter <- get_profile_detail_section_filter()
  if (nzchar(filter)) {
    allowed <- trimws(strsplit(filter, ",", fixed = TRUE)[[1]])
    if (!(section %in% allowed)) {
      return(eval(expr, envir = parent.frame()))
    }
  }

  profile_dir <- if (exists("profile_dir", inherits = TRUE)) get("profile_dir", inherits = TRUE) else tempdir()
  dir.create(profile_dir, showWarnings = FALSE, recursive = TRUE)
  safe <- gsub("[^A-Za-z0-9_.-]+", "_", section)
  rprof_path <- file.path(profile_dir, paste0("rprof_", safe, ".out"))
  summary_path <- file.path(profile_dir, paste0("rprof_", safe, "_summary.txt"))

  Rprof(rprof_path, interval = 0.01)
  on.exit({
    Rprof(NULL)
    summ <- tryCatch(summaryRprof(rprof_path), error = function(e) NULL)
    if (!is.null(summ)) {
      lines <- c(
        paste0("section: ", section),
        paste0("generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "",
        "== by.self (top 50) ==",
        capture.output(utils::head(summ$by.self, 50)),
        "",
        "== by.total (top 50) ==",
        capture.output(utils::head(summ$by.total, 50))
      )
      writeLines(lines, summary_path)
    }
  }, add = TRUE)

  eval(expr, envir = parent.frame())
}
