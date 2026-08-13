# GloFAS Harmonization and QA Specification

Last updated: 2026-02-17
Scope: Workstream E preparation and execution contract.

## 1) Objective

Define one normalized point-series schema and reproducible QA outputs to compare:

1. GloFAS forecast-derived point values.
2. GloFAS historical consolidated point values.
3. Legacy JRC reanalysis point values.

## 2) Canonical Harmonized Schema

| field | type | required | description |
|---|---|---|---|
| `date` | `YYYY-MM-DD` | yes | Target valid date of the hydrologic value. |
| `value_cms` | float | yes | River discharge in `m^3/s` (raw cms). |
| `center` | string | yes | Fixed as `GLOFAS` for this workflow. |
| `product_family` | string | yes | `forecast` / `historical` / `legacy_reanalysis`. |
| `system_version_label` | string | yes | e.g., `operational`, `version_3_1`, `version_4_0`, `jrc_v3_0`. |
| `hydrological_model` | string | no | `lisflood` / `htessel_lisflood` when available. |
| `product_type` | string | no | e.g., `consolidated`, `control_forecast`, etc. |
| `issue_date` | `YYYY-MM-DD` | conditional | Required for forecast rows. |
| `lead_time_h` | int | conditional | Required for forecast rows. |
| `member` | int | conditional | Required for forecast ensemble rows. |
| `source_path` | string | yes | Source file path used to generate row. |
| `source_hash_sha256` | string | yes | SHA256 of source file bytes. |
| `record_hash` | string | yes | SHA256 of canonical row key (provenance guard). |
| `extraction_run_id` | string | yes | Run identifier for reproducibility. |
| `notes` | string | no | Optional caveat/annotation. |

## 3) Source-to-Schema Mapping

### 3.1 Forecast cache (`scripts/forecats_extract_glofas_batch.py`)

Input columns:
- `issue_date`, `target_date`, `member`, `lead_time_h`, `discharge_cms`

Mapping:
- `date <- target_date`
- `value_cms <- discharge_cms`
- `product_family <- forecast`
- `system_version_label <- operational` (or explicit selector if available in request provenance)

### 3.2 Historical consolidated downloads

Upstream source:
- EWDS zip payloads from `cems-glofas-historical` campaign runs.

Required extraction step:
- Decode GRIB/NetCDF payloads to point CSV with `date, discharge_cms`.

Mapping:
- `product_family <- historical`
- `system_version_label <- version_2_1/version_3_1/version_4_0`
- `product_type <- consolidated`

### 3.3 Legacy JRC reanalysis (`scripts/forecats_extract_legacy_glofas_point.py`)

Input columns:
- `date`, `discharge_cms`

Mapping:
- `product_family <- legacy_reanalysis`
- `system_version_label <- jrc_v3_0` / `jrc_v4_0`

## 4) QA Artifacts

## 4.1 Overlap comparison report (pairwise)

For each pair of series over common dates:

1. `n_overlap`
2. `max_abs_diff`
3. `mae`
4. `rmse`
5. `mean_diff`
6. `corr_pearson`
7. `diff` quantiles (`q05`, `q50`, `q95`)

Outputs:
- row-level diff CSV
- summary JSON

## 4.2 Transition diagnostics

At each known version transition date (configured):

1. pre-window and post-window sample counts
2. pre/post means and medians
3. `delta_mean` and `delta_median`
4. pre/post spread (`std`, `q05`, `q95`)

Outputs:
- per-transition CSV
- summary JSON

## 5) Handoff Package (Model-Fit Ready)

Target bundle:

1. `harmonized_point_series.csv`
2. `harmonized_point_series.schema.json`
3. `overlap_reports/*.csv` and `overlap_reports/*.json`
4. `transition_diagnostics/*.csv` and `transition_diagnostics/*.json`
5. `provenance_manifest.csv` (file paths + checksums + run IDs)
6. `README.md` (exact commands and assumptions)

## 6) Execution Readiness Status

1. Schema definition: complete.
2. Comparison tooling: implemented (scripts in `scripts/`).
3. Transition diagnostics tooling: implemented (scripts in `scripts/`).
4. Full execution blocked until:
   - historical campaign completion, and
   - at least one legacy v3/v4 point extraction CSV available.
