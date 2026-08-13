# Canonical GDPC Master Pipeline Runbook

Date: 2026-05-09
Scope: full canonical GDPC master-covariate build, including source acquisition, daily interpolation, diagnostics, stationarity audit, canonical `GDPC1`, and workflow compatibility aliases.

## Purpose

This runbook documents the one-command path for rebuilding the canonical large-scale climate covariate used by the workflow.

The full pipeline now has a clean, reproducible shape:
1. download the canonical 17 monthly climate-index source files,
2. parse and preserve them as normalized monthly CSVs,
3. build the canonical daily interpolated matrix on `1987-05-29 -> 2023-01-22`,
4. standardize the daily matrix,
5. render per-index diagnostic plots,
6. audit stationarity/trend compatibility,
7. optionally run the bounded simple lag screen over `{1, 2, 3}`,
8. fit canonical `GDPC1` with screened fixed lag `k = 2`,
9. orient the sign deterministically,
10. emit workflow-facing compatibility aliases that preserve the legacy `PCA` slot contract.

## Canonical config

- `config/canonical_gdpc_master_covariate.yaml`

This config freezes:
- the 17-index source set,
- the canonical daily window,
- the interpolation contract,
- the standardization contract,
- the fixed GDPC lag,
- the sign rule,
- and the compatibility alias filenames.

## Entry points

Full one-shot wrapper:
- `scripts/run_canonical_gdpc_master_pipeline.py`

Component steps:
- `scripts/download_canonical_climate_indices.py`
- `scripts/build_canonical_climate_daily_matrices.py`
- `scripts/render_canonical_climate_index_diagnostics.py`
- `scripts/build_canonical_climate_stationarity_audit.R`
- `scripts/screen_canonical_gdpc_lags.py`
- `scripts/build_canonical_gdpc_master_covariate.py`
- `scripts/build_canonical_gdpc_factor.R`

## Recommended command

```bash
python3 scripts/run_canonical_gdpc_master_pipeline.py \
  --config config/canonical_gdpc_master_covariate.yaml
```

To force redownload of the raw monthly source files first:

```bash
python3 scripts/run_canonical_gdpc_master_pipeline.py \
  --config config/canonical_gdpc_master_covariate.yaml \
  --force-download
```

To rerun the bounded simple lag screening as part of the full pipeline:

```bash
python3 scripts/run_canonical_gdpc_master_pipeline.py \
  --config config/canonical_gdpc_master_covariate.yaml \
  --run-screening
```

To force recomputation of the screening candidates as well:

```bash
python3 scripts/run_canonical_gdpc_master_pipeline.py \
  --config config/canonical_gdpc_master_covariate.yaml \
  --run-screening \
  --force-screening
```

## Canonical output root

- `data/canonical_gdpc_master/v20260509/`

Key inputs and intermediate outputs:
- `inputs/raw_psl_text/`
- `inputs/monthly_csv/`
- `intermediate/combined_climate_indices_daily_19870529_20230122.csv`
- `intermediate/combined_climate_indices_daily_standardized_19870529_20230122.csv`

Key review outputs:
- `review/CANONICAL_CLIMATE_INDEX_DOWNLOAD_REVIEW.md`
- `review/CANONICAL_CLIMATE_INDEX_POSTPROCESS_REVIEW.md`
- `review/CANONICAL_CLIMATE_INDEX_DIAGNOSTIC_PLOTS.md`
- `review/stationarity/CANONICAL_GDPC_STATIONARITY_AUDIT.md`
- `review/lag_screening/CANONICAL_GDPC_K_SCREENING_REVIEW.md`
- `review/CANONICAL_GDPC_BUILD_REVIEW.md`

Key canonical GDPC outputs:
- `outputs/gdpc_master_component_01_19870529_20230122.csv`
- `outputs/gdpc_master_component_01_alpha_19870529_20230122.csv`
- `outputs/gdpc_master_component_01_beta_19870529_20230122.csv`
- `outputs/gdpc_master_component_01_initial_f_19870529_20230122.csv`
- `outputs/compat/cov_03_PCA.csv`
- `outputs/compat/cov_05_PCA.csv`

Key metadata:
- `metadata/source_manifest.csv`
- `metadata/validation_summary.json`
- `metadata/gdpc_build_metadata.json`
- `metadata/compatibility_alias_manifest.csv`

## Decision contract frozen in this lineage

- keep the full 17 daily climate indices
- standardize each index over `1987-05-29 -> 2023-01-22`
- fit GDPC on the standardized series in levels
- do not difference or detrend before GDPC
- use the bounded simple screen over `k in {1, 2, 3}` when reconfirmation is needed
- freeze the canonical lag at `k = 2`
- use tolerance `1e-3` and `niter_max = 200`
- use `BIC` as the recorded reconstruction criterion
- orient the final component so it has positive correlation with `oni`

Canonical build snapshot currently frozen in this lineage:
- converged: `TRUE`
- iterations used: `13`
- explained variance: `0.4407`
- reconstruction MSE: `0.5593`
- runtime: about `301` seconds

Screening snapshot currently frozen in this lineage:
- `k = 1`: converged, `BIC = 30277.8839`, runtime about `272` seconds
- `k = 2`: converged, `BIC = 29973.3221`, runtime about `316` seconds
- `k = 3`: timed out at the `900` second screening cap

## Validation expectation

After a successful full run:
- all 17 monthly source files exist,
- the standardized daily matrix covers `1987-05-29 -> 2023-01-22`,
- the stationarity audit is present and supports keeping the full 17-series block in levels,
- `GDPC1` converged under the configured tolerance and iteration budget,
- the compatibility aliases exist and are hash-tracked,
- and the review markdown files summarize the lineage clearly enough for advisor-facing inspection.

## Workflow wiring note

The canonical source-of-truth artifact is the GDPC master file:
- `outputs/gdpc_master_component_01_19870529_20230122.csv`

The legacy workflow still consumes a `PCA` slot. For that reason, the pipeline also emits:
- `outputs/compat/cov_03_PCA.csv`
- `outputs/compat/cov_05_PCA.csv`

These are compatibility shims derived directly from the canonical GDPC master factor. Downstream code should treat them as aliases, not as independent sources of truth.
