# Public reproducibility stub.
#
# The public repository starts from model-ready staged covariates. Raw
# GEFS/NWM retrieval and intermediate forecast-covariate construction
# are intentionally outside this release.

unified_materialize_deterministic_climate_covariates <- function(cfg, shared_paths, cov_path_map, repo_root) {
  stop(
    paste(
      "Raw forecast-covariate materialization is not bundled in the public reproducibility release.",
      "Use the model-ready staged covariates under data/staged/covariates and the cutoff-specific",
      "forecast-origin bundles under data/staged/forecast_origins."
    ),
    call. = FALSE
  )
}
