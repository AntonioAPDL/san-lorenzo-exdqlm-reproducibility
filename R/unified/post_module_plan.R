unified_post_select_modules <- function(
  post_figures,
  post_smoke_fast,
  model_run_exdqlm_multivar,
  model_run_exdqlm_univar,
  model_run_ndlm_main,
  model_run_ndlm_univar,
  core_modules,
  multivar_component_diagnostics = FALSE
) {
  stopifnot(is.logical(post_figures), length(post_figures) == 1L)
  stopifnot(is.logical(post_smoke_fast), length(post_smoke_fast) == 1L)
  stopifnot(is.logical(model_run_exdqlm_multivar), length(model_run_exdqlm_multivar) == 1L)
  stopifnot(is.logical(model_run_exdqlm_univar), length(model_run_exdqlm_univar) == 1L)
  stopifnot(is.logical(model_run_ndlm_main), length(model_run_ndlm_main) == 1L)
  stopifnot(is.logical(model_run_ndlm_univar), length(model_run_ndlm_univar) == 1L)
  stopifnot(is.character(core_modules), length(core_modules) > 0L)
  stopifnot(is.logical(multivar_component_diagnostics), length(multivar_component_diagnostics) == 1L)

  append_multivar_components <- function(modules) {
    if (isTRUE(multivar_component_diagnostics) &&
        isTRUE(model_run_exdqlm_multivar) &&
        !("40_figures_multivar_only.R" %in% modules)) {
      modules <- c(modules, "40_figures_multivar_only.R")
    }
    modules
  }

  if (!isTRUE(post_figures)) {
    return(core_modules)
  }

  ndlm_any_mode <- isTRUE(model_run_ndlm_main) || isTRUE(model_run_ndlm_univar)
  ndlm_only_mode <- isTRUE(ndlm_any_mode) &&
    !isTRUE(model_run_exdqlm_multivar) &&
    !isTRUE(model_run_exdqlm_univar)

  univar_only_mode <- isTRUE(model_run_exdqlm_univar) &&
    !isTRUE(model_run_exdqlm_multivar) &&
    !isTRUE(ndlm_any_mode)

  multivar_only_mode <- isTRUE(model_run_exdqlm_multivar) &&
    !isTRUE(model_run_exdqlm_univar) &&
    !isTRUE(ndlm_any_mode)

  if (ndlm_only_mode) {
    # NDLM isolation lane: run the dedicated NDLM-only full post module so
    # forecast diagnostics/CRPS do not depend on exDQLM notebook objects.
    return(c(core_modules, "10_data_inputs.R", "20_model_setup.R", "30_ndlm_only_init.R", "40_figures_ndlm_only.R"))
  }

  if (univar_only_mode) {
    if (isTRUE(post_smoke_fast)) {
      return(c(core_modules, "10_data_inputs.R", "20_model_setup.R", "30_univariate_and_misc.R", "40_figures_smoke_fast.R"))
    }
    # Univariate isolation lane: go directly to the dedicated full post module.
    # The legacy smoke-fast figures assume broader notebook objects and can fail
    # on targeted repair runs with only a subset of fitted quantiles.
    return(c(
      core_modules,
      "10_data_inputs.R",
      "20_model_setup.R",
      "30_univariate_and_misc.R",
      "40_figures_univar_only.R"
    ))
  }

  if (multivar_only_mode) {
    if (isTRUE(post_smoke_fast)) {
      # Multivariate comparison lanes in smoke-fast mode should use the same
      # lightweight comparison exporter as the mixed v7 workflow so CRPS,
      # input-health, and figure manifests stay on one contract.
      return(append_multivar_components(c(
        core_modules,
        "10_data_inputs.R",
        "20_model_setup.R",
        "30_univariate_and_misc.R",
        "40_figures_smoke_fast.R"
      )))
    }
    # Multivariate isolation lane: generate DISC-only diagnostics/forecast plots
    # without touching NDLM-specific figure sections.
    return(c(core_modules, "10_data_inputs.R", "20_model_setup.R", "30_univariate_and_misc.R", "40_figures_multivar_only.R"))
  }

  if (isTRUE(post_smoke_fast)) {
    # Mixed smoke-fast lane still needs the lightweight synthesis/post objects
    # from 30_univariate_and_misc.R so CRPS/input-health tables can export
    # without running the full legacy figure stack.
    return(append_multivar_components(c(core_modules, "10_data_inputs.R", "20_model_setup.R", "30_univariate_and_misc.R", "40_figures_smoke_fast.R")))
  }

  c(core_modules, "10_data_inputs.R", "20_model_setup.R", "30_univariate_and_misc.R", "40_figures.R")
}
