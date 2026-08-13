# Canonical GDPC Master Covariate Report

Date: 2026-05-09

Scope:
- Repo audited: `/data/muscat_data/jaguir26/project1_ucsc_phd`
- This report documents the current PCA/GDPC state, the evidence behind it, and the agreed direction for the next reproducibility-hardening phase.
- This is a design and provenance report only. No GDPC regeneration or model reruns are implemented in this report.

## 1. Executive decision

The project should move to one canonical master large-scale climate covariate built as the **first Generalized Dynamic Principal Component (GDPC1)** from the full daily climate-index window:
- start: `1987-05-29`
- end: `2023-01-22`

Agreed design choices for the future implementation phase:
- use the daily climate-index matrix on the fixed window above
- use the full 17-index climate set already identified in the notebook build path
- use the `gdpc` package rather than the current frozen static PCA artifact
- retain only the first dynamic component `GDPC1`
- accept the resulting shared-master leakage across cutoffs by design
- do **not** use expensive lag cross-validation or automatic lag search
- use one simple fixed lag choice, documented explicitly and frozen in metadata

This future GDPC artifact will become the canonical master climate covariate used across cutoffs and then sliced by date inside the current workflow structure.

Companion implementation tracker for the next phase:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/repro/run/CANONICAL_GDPC_IMPLEMENTATION_TRACKER_20260509.md`

Implemented source-pipeline runbook:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/repro/run/CANONICAL_GDPC_SOURCE_PIPELINE_RUNBOOK_20260509.md`

Implemented source-pipeline entry points:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/download_canonical_climate_indices.py`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/build_canonical_climate_daily_matrices.py`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/run_canonical_climate_index_pipeline.py`

## 2. What the current workflow actually does

### 2.1 Active unified pipeline behavior

The active unified workflow does **not** compute GDPC or static PCA in-pipeline.

Instead, it treats the climate factor as a precomputed input artifact and copies it into the run-scoped shared-input tree.

Evidence:
- `R/unified/stages/stage_data_prep_shared.R` copies shared-input covariate CSVs into `inputs/shared/covariates/` and preserves exact snapshots when configured.
- The shared covariate slots include `pca` alongside `eli`, `oni`, `ppt`, and `soil`.
- No active `R/unified` or current workflow `scripts/` path calls `gdpc` or `auto.gdpc`.

Relevant files:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/R/unified/stages/stage_data_prep_shared.R`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/R/disc_w/03_covariates_standardize.R`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/R/environmetrics/10_data_inputs.R`

### 2.2 Current model-facing artifact

The current model-facing climate factor is stored under names such as:
- `cov_05_PCA.csv`
- `cov_03_PCA.csv`
- column name `Static_PCA`

The modeling code reads that file and uses it as a passthrough covariate.

### 2.3 Current history/future handling

The current code builds both:
- historical PCA window `time <= cutoff_date`
- future PCA window `time >= cutoff_date + 1`

So the current contract is not merely “historical factor used during fit.” It also passes the post-cutoff factor values into the forecast-window covariate construction.

That behavior is already wired in the current code paths and must be acknowledged explicitly in the future GDPC design.

## 3. What I verified about the frozen PCA artifact

### 3.1 One dominant master series is reused across cutoffs

The five authoritative frozen shared-input snapshots all use the same PCA file content byte-for-byte.

Authoritative snapshot root:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/repro/frozen_shared_inputs/exalm_t1_authoritative_20260505`

For the representative `2022-12-25` cutoff, the preserved PCA file is:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/repro/frozen_shared_inputs/exalm_t1_authoritative_20260505/cutoff_date=2022-12-25/covariates/cov_05_PCA.csv`

Observed properties:
- rows: `13023`
- columns: `time`, `Static_PCA`
- min date: `1987-05-29`
- max date: `2023-01-22`

This confirms that the current factor is already being treated as a shared master series that is sliced by date rather than recomputed per cutoff.

### 3.2 The “two PCA lineages” on disk are not two different factors

Two file hashes exist on disk for `cov_05_PCA.csv`.

What they actually mean:
- dominant hash: full master series `1987-05-29 -> 2023-01-22`
- second hash: shorter slices such as `2010-01-01 -> 2023-01-22`

I checked the overlap directly. The shorter lineage is numerically identical to the matching suffix of the dominant master series.

Conclusion:
- there is one dominant current factor definition
- some older runtime trees store truncated copies of it
- the main ambiguity is provenance of the master series, not disagreement among current values

### 3.3 The current master series is not standardized over the active full window

For the dominant current master series:
- mean: about `-0.024`
- standard deviation: about `1.373`

So the current stored factor is not a simple z-scored series over the active `1987-05-29 -> 2023-01-22` window. It looks more like a preserved PC score series from an earlier offline computation.

## 4. What the historical notebooks show

The historical notebook record shows several exploratory paths, not one clean provenance chain.

### 4.1 Daily climate-index source matrix build

`Indexes_Notebook.ipynb` contains an explicit build path for a daily standardized climate-index matrix using the following 17 indices:
- `Niño 3`
- `NAO`
- `Niño 1+2`
- `WHWP`
- `GMT`
- `ONI`
- `PNA`
- `NOI`
- `WP`
- `Niño 3.4`
- `Solar Flux`
- `AMO`
- `ESPI`
- `TSA`
- `Niño 4`
- `TNA`
- `SOI`

This notebook first builds a standardized daily combined file through a broad fixed window, and later contains a variant extending farther in time.

### 4.2 GDPC experiments existed

The repo contains genuine GDPC experiments:
- `Dynamic_PCA.ipynb`
- `gdpc_fit.ipynb`
- `gdpc_analysis.R`
- `auto_gdpc_analysis.r`

These show real usage of the `gdpc` package, including leave-one-out criteria in exploratory notebook work.

### 4.3 Static PCA experiments also existed

The same historical record also contains multiple static PCA experiments.

Important example:
- `gdpc_fit.ipynb` contains a static PCA block that uses a smaller index subset and writes out a `pca.csv`-style export.
- One notebook path even writes `PC2`, not `PC1`, to `pca.csv`.

Conclusion:
- notebook evidence proves that both GDPC and static PCA were explored
- notebook evidence does **not** prove that the current frozen model-facing artifact came from the GDPC path
- the current active workflow and artifact naming strongly suggest the present production lineage is static-PCA-derived rather than GDPC-derived

## 5. Current manuscript mismatch

The revised manuscript currently still says:
- the climate factor is the first GDPC
- leads/lags were selected by leave-one-out cross-validation
- the first component explains `53.42%`

Relevant files:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/Evironmetrics---REVISED-DOC-Corrected-2/wileyNJD-APA.tex`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/article.txt`

At present, those claims are not backed by the active reproducible workflow.

So the current mismatch is real:
- workflow artifact says `Static_PCA`
- manuscript says `GDPC`
- no current wired generation step explains the active artifact from first principles

## 6. Leakage policy and forecast-window policy

The user explicitly accepts the cross-cutoff leakage that comes from using one master canonical climate factor across all cutoffs.

This means the future reproducible contract should explicitly state:
- the master GDPC1 is fit on the full fixed daily climate window `1987-05-29 -> 2023-01-22`
- all cutoffs then reuse that same master factor by date slicing
- this is a deliberate design choice, not an oversight

A second point also needs to stay explicit:
- the current workflow passes the climate factor after the cutoff as well
- this factor is therefore not being forecasted operationally in the same way as NWS/GloFAS products
- it is a fixed shared master covariate that remains available over the master window by design

This is acceptable if documented honestly.

## 7. Recommended future canonical GDPC contract

### 7.1 Source data contract

Canonical source set:
- the 17 daily climate-index series identified in `Indexes_Notebook.ipynb`

Canonical fitting window:
- `1987-05-29 -> 2023-01-22`

Canonical source preprocessing:
1. start from the monthly climate-index source files
2. interpolate to daily resolution using one documented method
3. restrict to the exact canonical window above
4. standardize each of the 17 series over that exact canonical window
5. build the final `T x 17` matrix for GDPC

This should replace the current under-documented notebook-only source path.

### 7.2 GDPC method contract

Recommended future method:
- package: `gdpc`
- function: `gdpc()`
- number of components retained: `1`
- fixed lag count: one explicit frozen value
- no `auto.gdpc()` lag search
- no leave-one-out lag selection

Package-status note from this audit:
- the canonical implementation environment now has `gdpc` installed
- the canonical build was completed with CRAN `gdpc` version `1.1.4`

Why use `gdpc()` rather than `auto.gdpc()`:
- simpler
- much cheaper
- easier to explain and rerun
- avoids the current user concern that automatic lag selection is too expensive and opaque

### 7.3 Recommended fixed lag choice

Canonical implementation now frozen at:
- `k = 2`
- `tol = 1e-3`
- `niter_max = 200`
- `crit = 'BIC'`

Reasoning:
- we ran a bounded simple lag screen over `k in {1, 2, 3}` on the full standardized matrix
- `k = 2` achieved the best converged `BIC` value among the screened candidates
- `k = 2` improved on `k = 1` materially while keeping runtime in the same practical range
- `k = 3` did not finish within the screening runtime cap, so it was excluded from the final selection set as not practical for the canonical contract

Important note:
- the `crit` argument in `gdpc()` is only used to evaluate the fitted reconstruction; it does not choose `k` when `k` is fixed by the user
- so the canonical build should not pretend that the criterion selected the lag count
- `BIC` is the pragmatic default for the canonical build because it is simple, deterministic, and lighter than leave-one-out evaluation for this full-window batch artifact

Simple screening policy now used:
- compare `k in {1, 2, 3}`
- keep only converged fits
- minimize `BIC`
- break ties by lower runtime, then smaller `k`
- enforce a practical runtime cap of `900` seconds per candidate

If future reconfirmation is needed, reuse that same bounded screening rule rather than broadening the lag search silently.

Canonical build result:
- converged: `TRUE`
- iterations used: `13`
- explained variance (`expart`): `0.4407`
- reconstruction MSE: `0.5593`
- runtime: about `301` seconds

Simple screening result:
- `k = 1`: converged, `BIC = 30277.8839`, runtime about `272` seconds
- `k = 2`: converged, `BIC = 29973.3221`, runtime about `316` seconds
- `k = 3`: timed out at `900` seconds under the bounded screening rule

### 7.4 Standardization contract

Recommended future standardization policy:
- standardize each of the 17 climate-index series over the exact canonical window `1987-05-29 -> 2023-01-22`
- then run GDPC on that standardized matrix

This keeps the climate indices on a comparable scale before the dynamic factor is estimated.

### 7.4a Stationarity decision

Current decision after the reproducible audit:
- keep the full 17-index source set
- use the standardized daily series in levels
- do **not** difference, detrend, or pre-filter the inputs before fitting `GDPC1`

Rationale:
- the `gdpc` method does not require stationary inputs
- the 17-series audit shows family-wide low-frequency persistence rather than one small obviously pathological subset
- forcing stationarity by differencing would discard part of the large-scale climate signal we are explicitly trying to summarize in the master factor

Reproducible audit outputs:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/data/canonical_gdpc_master/v20260509/review/stationarity/CANONICAL_GDPC_STATIONARITY_AUDIT.md`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/data/canonical_gdpc_master/v20260509/review/stationarity/stationarity_audit.csv`

### 7.5 Output contract

The future canonical outputs should include all of the following.

#### Canonical source table
- one daily climate-index matrix used for the GDPC fit
- example name: `combined_climate_indices_daily_standardized_19870529_20230122.csv`

#### Canonical factor artifact
- one master factor file
- example name: `gdpc_master_component_01_19870529_20230122.csv`
- preferred columns:
  - `time`
  - `GDPC1`

#### Canonical metadata
- one metadata JSON or YAML recording:
  - date range
  - 17 index names
  - interpolation method
  - standardization policy
  - package and version
  - fixed lag `k`
  - convergence flag
  - iterations used
  - explained-variance quantity reported by `gdpc`
  - sign convention
  - output hashes

#### Compatibility export
For legacy consumers we will likely still need a compatibility export that matches current workflow expectations.

That compatibility layer should be generated from the canonical artifact, not the other way around.

Possible compatibility files:
- `cov_05_PCA.csv`
- `cov_03_PCA.csv`

But these should be treated as compatibility shims only. The real source of truth should be the clearly named GDPC master artifact.

### 7.6 Sign convention

Because principal-component signs are arbitrary, the canonical GDPC1 artifact needs one deterministic sign rule.

Recommended future policy:
- fix the sign so that GDPC1 has positive correlation with a named anchor series over the canonical window
- document that anchor explicitly in metadata

Without this, regenerations may flip sign while remaining mathematically equivalent, which would be confusing for downstream validation and article figures.

## 8. Reproducibility gaps that still need to be fixed in implementation

These are the concrete gaps that remain.

1. The repo does not currently contain a wired source-of-truth generation path from raw climate indices to the active master factor.
2. The repo does not currently carry the old `combined_indices_daily_standardized.csv` export used in exploratory notebooks.
3. The repo does not currently have the `gdpc` package installed in the local environment used for this audit.
4. The manuscript and the workflow currently describe different factor-generation stories.
5. The future factor usage after the cutoff is implicit in code and should become explicit in metadata and prose.

## 9. What we need to do next

This is the recommended order for the next implementation phase.

### Step 1. Freeze the canonical design before coding

Before implementing, explicitly freeze:
- the 17 index list
- the exact canonical window `1987-05-29 -> 2023-01-22`
- the daily interpolation method
- the standardization rule
- the fixed lag choice `k`
- the sign convention
- the future post-cutoff availability policy for GDPC1

### Step 2. Build one dedicated GDPC generation script

The future script should:
- build or read the canonical daily 17-index matrix
- standardize it deterministically
- run `gdpc()` with the frozen fixed lag
- write the canonical `GDPC1` artifact
- write metadata
- write compatibility aliases for downstream fit code

### Step 3. Add one validation check

The future validation should prove that the rerun-consumed PCA/GDPC compatibility file matches the canonical master artifact slice intended for that lineage.

### Step 4. Update the workflow wiring

After the canonical artifact exists, the workflow can be rewired so the shared-input bundles consume the new master GDPC artifact rather than the legacy frozen `Static_PCA` lineage.

### Step 5. Update article and runbook language

Once the new artifact is wired and validated, the manuscript and runbooks can honestly say:
- the large-scale climate factor is GDPC1
- the fitting window is the fixed master daily window
- the lag choice is fixed by design rather than selected by expensive cross-validation

## 10. Recommended report language for the future paper/workflow state

If the future implementation follows this design, the clean description should be:
- the large-scale climate factor is the first generalized dynamic principal component computed from 17 daily climate indices over the fixed canonical window `1987-05-29 -> 2023-01-22`
- the component is built once as a master shared covariate and then sliced by cutoff date for downstream workflow use
- a fixed lag count is used by design for reproducibility and computational practicality, rather than reselected separately for each run

This is the most coherent future story that matches the user's current preferences.

## 11. Bottom line

The current project is not yet on a reproducible GDPC workflow.

It is on a reproducible **frozen factor artifact** workflow, where the active factor is a preserved `Static_PCA`-style series that is reused across cutoffs.

The agreed future direction is now clearer:
- move intentionally to a **canonical master GDPC1**
- use the full 17-index daily matrix over `1987-05-29 -> 2023-01-22`
- accept the shared-master leakage across cutoffs
- avoid automatic lag cross-validation
- freeze one simple lag choice and document it carefully
- wire the resulting master artifact into the current workflow as the new source-of-truth climate covariate

That gives us a much cleaner and more defensible reproducibility story than the current PCA/GDPC hybrid lineage.
