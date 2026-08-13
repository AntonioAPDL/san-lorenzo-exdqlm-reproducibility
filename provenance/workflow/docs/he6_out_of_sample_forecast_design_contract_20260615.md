# HE-6 Out-of-Sample Forecast Design Contract

Date: 2026-06-15  
Scope: Handling Editor comment HE-6, Reviewer 1 Major comment R1-M5, cross-repo forecast-validation wording, and publication-freeze validation.

## Purpose

This contract locks the revised article and corrections response to the actual
rolling-origin forecast design used by the current publication workflow. It is
intended to prevent three common ambiguities:

1. treating the forecast validation as in-sample;
2. confusing forecast-origin inputs with held-out USGS verification data;
3. describing the canonical GDPC climate factor as if it were an operational
   forecast product like NWS or GloFAS.
4. treating the rolling-origin forecast folds as if they were a random
   cross-validation split or a dense continuous hindcast.

## Authoritative Design

The forecast validation is a five-cutoff rolling-origin exercise. For each
cutoff \(c\):

- the model is fit using USGS observations through \(c\);
- retrospective/historical external products are used through \(c\) to learn
  source-specific discrepancies;
- forecast products are restricted to the latest products issued at or before
  \(c\) and are used for forecast generation, not for fitting;
- forecast-window precipitation and soil-moisture covariates enter as staged
  forecast-window transfer covariates from the cutoff-specific support bundle;
- the workflow-facing `PCA` slot is the canonical GDPC1 compatibility alias and
  is treated as a deterministic climate-index covariate, not as an operational
  forecast product or a verification target;
- post-cutoff USGS observations are used only to score the forecasts and are not
  used to fit, update, or select the predictive distributions.

For fair forecast assessment, the forecast origin is the evaluation unit. Each
retained cutoff defines a version-consistent staged dataset: only information
available at that origin is used to fit seven quantile-specific models and
synthesize the posterior predictive distribution, while future USGS
observations are held out over the forecast window for scoring. A broader grid
of origins could be used in future applications if version-consistent forecast
archives are available, but the current publication does not claim a continuous
daily post-2022 hindcast or a dense grid of heavily overlapping forecast
windows. Constructing each retained origin requires more than shifting the
cutoff date in a fixed table. The forecast-validation inputs come from evolving
operational systems with different release histories, product versions, update
frequencies, horizons, ensemble-member structures, spatial supports, and access
interfaces. For each retained origin the workflow rebuilds a version-consistent
bundle of observations, retrospective products, issued forecast products, and
forecast-window covariates. This archive-reconstruction step is the main
practical constraint on a denser rolling-origin design. A dense origin grid
would require substantially more data recovery, version matching, spatial
extraction, covariate staging, seven quantile-specific model fits, and
posterior predictive synthesis, while nearby forecast windows would repeatedly
evaluate similar hydrological episodes.

## Code Evidence

- `R/disc_w/03_covariates_standardize.R` builds historical and forecast-window
  transfer matrices separately. The post-cutoff forecast matrix `X_f` is
  constructed from precipitation, soil moisture, and `Static_PCA`.
- `scripts/render_exal_m_t1_setup_support_by_cutoff_v2.py` records the
  manuscript-facing GDPC panel as the canonical GDPC1 master factor sliced to the
  cutoff-specific support window, while preserving the workflow-facing `PCA`
  compatibility alias.
- `tests/python/test_canonical_gdpc_master_builder.py` verifies that the GDPC
  factor and the `cov_03_PCA.csv` / `cov_05_PCA.csv` aliases are numerically
  identical on the compatibility column.

## Article-Side Evidence

- Revised manuscript:
  `Evironmetrics---REVISED-DOC-Corrected-2/wileyNJD-APA.tex`
- Article manifest:
  `Evironmetrics---REVISED-DOC-Corrected-2/artifacts/forecast_design/forecast_design_manifest.json`
- Article documentation:
  `Evironmetrics---REVISED-DOC-Corrected-2/docs/forecast_design_contract.md`
- Cutoff setup/support metadata:
  `Evironmetrics---REVISED-DOC-Corrected-2/artifacts/five_cutoff_setup_support/*/metadata/scale_contract.yaml`

## Validation Gates

The following checks enforce this contract:

- `scripts/forecast_design_contract.py` validates the compact article manifest.
- `tests/python/test_he6_forecast_design_contract.py` verifies the manifest,
  docs, article wording, and corrections wording.
- `scripts/validate_publication_freeze.py` includes the HE-6 contract in the
  publication-freeze check family.
- `scripts/validate_revision_cross_repo_wiring.py` writes a
  `forecast_design_audit.csv` file and includes this contract in the cross-repo
  pass/fail summary.

## Wording Policy

Use "out-of-sample" only for the held-out USGS verification target. Use
"forecast-origin bundle" or "forecast-window support covariates" for staged
post-cutoff transfer inputs. Reader-facing prose should call the climate-index
covariate the canonical GDPC factor; the historical `PCA` slot name is only a
workflow compatibility alias. Do not call GDPC a forecast product. Do not imply
that post-cutoff USGS observations enter fitting, updating, model selection, or
posterior predictive construction. Do not imply that forecast products
themselves are fit-stage inputs; they are forecast-generation inputs after the
cutoff. When discussing cross-validation, use rolling-origin folds or
time-ordered cross-validation analogue language; do not describe the publication
exercise as random K-fold cross-validation or as a continuous dense hindcast.
