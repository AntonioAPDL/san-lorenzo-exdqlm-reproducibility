#!/usr/bin/env Rscript

# Reproduce Environmetrics figures from the notebook logic (headless).
# Usage:
#   Rscript scripts/make_environmetrics_figures.R

args <- commandArgs(trailingOnly = FALSE)
file_flag <- "--file="
script_path <- sub(file_flag, "", args[grep(file_flag, args)])
if (length(script_path) == 0 || script_path == "") {
  script_path <- "scripts/make_environmetrics_figures.R"
}
script_dir <- dirname(normalizePath(script_path))
root_dir <- normalizePath(file.path(script_dir, ".."))
setwd(root_dir)

# -------------------------
# Config
# -------------------------
SKIP_UNIVARIATE <- TRUE
WRITE_FIGURES <- TRUE
FIG_DIR <- "Environmetrics"

COV_1_ELI_PATH <- "LEGACY_EXAL_INPUT_ROOT/covariates/cov_1_ELI.csv"
COV_2_ONI_PATH <- "LEGACY_EXAL_INPUT_ROOT/covariates/cov_2_ONI.csv"

log_msg <- function(msg) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_msg("Starting Environmetrics figure generation")

source("R/environmetrics_utils.R")
source("scripts/_notebook_linearized.R")

log_msg("Completed Environmetrics figure generation")
