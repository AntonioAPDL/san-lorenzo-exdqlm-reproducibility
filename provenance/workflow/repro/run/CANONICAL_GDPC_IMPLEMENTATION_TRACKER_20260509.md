# Canonical GDPC Implementation Tracker

Date: 2026-05-09
Owner: Codex + Antonio
Status: Source acquisition and daily post-processing implemented; GDPC fit and workflow rewiring still pending.

## Goal

Build one canonical, reproducible, well-wired large-scale climate covariate for the workflow as the **first Generalized Dynamic Principal Component (`GDPC1`)** and make it the single master factor reused across cutoffs.

This tracker covers the **GDPC phase only**.
It does **not** yet:
- rerun any models,
- rebuild the affected full-history retrospective bundles,
- refresh article figures/tables from corrected reruns,
- or change the current publication-state results.

The point of this phase is to replace the current under-documented frozen `Static_PCA` lineage with a documented, reproducible, inspectable master `GDPC1` lineage that the rest of the workflow can consume safely.

## Why This Phase Exists

The current active workflow does **not** compute GDPC in-pipeline. It consumes a frozen precomputed climate-factor CSV under names such as:
- `cov_05_PCA.csv`
- `cov_03_PCA.csv`
- column `Static_PCA`

That artifact is shared across cutoffs and sliced by date, but its generation path is not currently reproducible from the wired workflow.

At the same time, the manuscript intent is now to use a **Generalized Dynamic Principal Component**, not a static PCA.

This phase therefore exists to:
- make the climate-factor contract honest,
- make the generation path reproducible,
- keep the master-factor reuse across cutoffs explicit,
- and prepare the workflow for the later corrected rerun phase.

## Repos In Scope

Workflow repo root:
- `/data/muscat_data/jaguir26/project1_ucsc_phd`

Article repo root:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/Evironmetrics---REVISED-DOC-Corrected-2`

Primary planning documents already in place:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/repro/run/CANONICAL_GDPC_MASTER_COVARIATE_REPORT_20260509.md`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/repro/run/HE2_FULL_HISTORY_REPAIR_FORWARD_PLAN.md`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/Evironmetrics---REVISED-DOC-Corrected-2/docs/manuscript_revision_checklist.md`

## Locked Decisions

These decisions are locked for the GDPC implementation phase unless Antonio explicitly changes them.

1. **Method**
- Use the R package `gdpc`.
- Use `gdpc()` with a fixed lag choice.
- Do **not** use `auto.gdpc()`.
- Do **not** use lag cross-validation or automatic lag search.

2. **Component retained**
- Retain only the first component: `GDPC1`.

3. **Master fitting window**
- Use the full fixed daily climate-index window:
  - start: `1987-05-29`
  - end: `2023-01-22`

4. **Source covariate set**
- Use the 17 daily climate indices identified in the historical notebook build path:
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

5. **Cross-cutoff policy**
- One master canonical `GDPC1` will be fit on the full fixed window above.
- All cutoffs will reuse that same master factor by date slicing.
- This cross-cutoff leakage is accepted by design and will be documented explicitly.

6. **Default lag choice**
- Canonical lag frozen in the implemented build: `k = 2`.
- The bounded simple screening now uses `{1, 2, 3}` with a `900` second per-candidate cap.
- Screening result frozen in the lineage: `k = 2` beat `k = 1` on converged `BIC`, while `k = 3` timed out under the cap.
- The final chosen lag must be written into metadata and frozen.

7. **Workflow-compatibility policy**
- The new canonical artifact should remain consumable by current workflow surfaces that expect a `PCA` covariate slot.
- The master factor may be called `GDPC1` canonically, but compatibility exports can still populate filenames like `cov_05_PCA.csv` or `cov_03_PCA.csv` if needed.

8. **Future-window policy**
- The master factor remains available throughout the full canonical window and may continue to be sliced into post-cutoff forecast windows as the current workflow already does.
- This should be documented honestly as a shared master covariate, not an operationally forecasted product.

## Current Evidence Base

### 1. Current workflow consumes a frozen factor artifact

Evidence paths:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/R/unified/stages/stage_data_prep_shared.R`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/R/disc_w/03_covariates_standardize.R`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/R/environmetrics/10_data_inputs.R`

Observed behavior:
- shared-input stage copies a precomputed climate-factor CSV into the run tree;
- legacy and unified modeling bridges then read that CSV as `Static_PCA`;
- the factor is used in both the historical and post-cutoff windows.

### 2. Current frozen master factor is shared across cutoffs

Authoritative frozen path family:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/repro/frozen_shared_inputs/exalm_t1_authoritative_20260505/cutoff_date=*/covariates/cov_05_PCA.csv`

Observed properties for the representative frozen file:
- rows: `13023`
- date range: `1987-05-29 -> 2023-01-22`
- columns: `time`, `Static_PCA`
- sample mean: `-0.023808`
- sample standard deviation: `1.372903`

Implication:
- the current factor is already structurally a shared master series, even though its generation path is under-documented.

### 3. Historical GDPC and static PCA experiments both existed

Relevant exploratory files:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/Indexes_Notebook.ipynb`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/Dynamic_PCA.ipynb`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/gdpc_fit.ipynb`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/gdpc_analysis.R`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/auto_gdpc_analysis.r`

Important notebook evidence already verified:
- `Indexes_Notebook.ipynb` builds a 17-index daily standardized matrix.
- `gdpc_fit.ipynb` contains real `gdpc()` experimentation with `k = 3`.
- historical notebook work is exploratory and not a current authoritative generation path.

### 4. Historical raw climate-index directory is currently missing

Notebook and script references point to:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/climate_indices/`

Current audit result:
- that directory does **not** exist in the current checkout;
- the old combined file `combined_indices_daily_standardized.csv` is also not present in the current repo tree.

Implication:
- a proper GDPC implementation must include a **fresh, documented climate-index acquisition/reconstruction stage**, not just a factor-fitting stage.

### 5. A reusable downloader starting point already exists

Legacy source-acquisition script:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/indxex_download.py`

Implication:
- we do not need to invent index acquisition from nothing;
- we can either adapt this legacy downloader or replace it with a cleaner canonical acquisition script,
- but the tracker should preserve its role as evidence of past source acquisition.

## Canonical Future Contract

The implementation phase should produce one canonical lineage with four layers:

1. **raw monthly source files**
2. **daily interpolated source matrix**
3. **daily standardized source matrix**
4. **canonical `GDPC1` master factor**

That lineage must be versioned, documented, and reproducible end to end.


## Environment And Reproducibility Preconditions

The future implementation should not rely on an informal notebook environment.

Minimum reproducibility requirements:
- capture the R version used for the build;
- capture the exact `gdpc` package version;
- capture the versions of any helper packages used for CSV/config handling;
- write `sessionInfo()` or equivalent into build metadata or the review report;
- make the build runnable from one documented command rather than a notebook session.

Current audit note:
- the local audit environment used for this planning pass did **not** have the `gdpc` package installed.

Implementation implication:
- the implementation phase should include one explicit preflight check for required packages;
- if the repo does not adopt a full environment manager for this phase, the build metadata must still record the exact package/runtime versions that produced the canonical artifact.

Recommended preflight outputs:
- package-version block in `build_metadata.json`
- package/version summary in `CANONICAL_GDPC_BUILD_REVIEW.md`
- a hard failure if `gdpc` is unavailable when the build script is run

## Lineage Versioning And Change-Control Rules

The canonical GDPC lineage should be treated as a versioned artifact family, not a mutable scratch file.

Rules:
- changing any of the following must create a new lineage version directory rather than overwriting the current one:
  - source index set,
  - canonical date window,
  - interpolation method,
  - standardization rule,
  - lag `k`,
  - sign convention,
  - output scaling convention.
- compatibility aliases may be regenerated from the canonical source within a lineage, but only if the canonical source itself has not changed.
- the workflow should consume a clearly named canonical lineage, not an ambiguous “latest” file with undocumented history.

Recommended practice:
- keep the lineage root versioned, e.g. `v20260509`;
- snapshot the exact config used inside the lineage metadata;
- require the review report to state whether the build is a first creation, rebuild-verification, or contract-changing rebuild.

## Proposed Canonical Artifact Layout

This tracker recommends the following new workflow-side layout.

### A. Source data and outputs

New lineage root:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/data/canonical_gdpc_master/v20260509/`

Recommended substructure:
- `inputs/raw_psl_text/`
- `inputs/monthly_csv/`
- `intermediate/`
- `outputs/`
- `metadata/`
- `review/`

Recommended contents:

`inputs/raw_psl_text/`
- the upstream NOAA/PSL text payloads preserved exactly as downloaded

`inputs/monthly_csv/`
- the parsed monthly CSVs derived from the raw text payloads
- one CSV per index, normalized to `Year` plus `Month_1..Month_12`

`intermediate/`
- `combined_climate_indices_daily_19870529_20230122.csv`
- `combined_climate_indices_daily_standardized_19870529_20230122.csv`

`outputs/`
- `gdpc_master_component_01_19870529_20230122.csv`
- columns should be at minimum:
  - `time`
  - `GDPC1`

`metadata/`
- `canonical_gdpc_build_config.yaml`
- `source_manifest.csv`
- `build_metadata.json`
- `validation_summary.json`
- `compatibility_alias_manifest.csv`

`review/`
- `CANONICAL_GDPC_BUILD_REVIEW.md`
- optional diagnostic plots and tables for inspection only

### B. Compatibility exports

The current workflow still expects a `PCA`-named covariate slot.

So the canonical build should also emit compatibility files derived from the master `GDPC1` series, for example:
- `compat/cov_05_PCA.csv`
- `compat/cov_03_PCA.csv`

Those compatibility files should be generated automatically from the master artifact and documented as **aliases**, not treated as the source of truth.

### C. Article-side snapshot surface

Later, after implementation is validated, the article repo should receive only frozen review/snapshot outputs that matter for manuscript provenance, not the whole source lineage.

That later article-side snapshot should be documented under:
- `Evironmetrics---REVISED-DOC-Corrected-2/docs/manuscript_revision_checklist.md`
- article-side asset manifests / provenance docs as needed

But article syncing is not the primary focus of this phase.

## Proposed New Scripts And Configs

This tracker recommends introducing a small, explicit script surface rather than relying on notebooks.

### New canonical config

Recommended new config file:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/config/canonical_gdpc_master_covariate.yaml`

Purpose:
- freeze the canonical index list,
- freeze the fitting window,
- freeze interpolation settings,
- freeze standardization settings,
- freeze the chosen lag,
- freeze sign and scaling conventions,
- and make future contract changes explicit rather than hidden in notebooks.

### New canonical build script

Implemented builder surface:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/build_canonical_gdpc_master_covariate.py`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/build_canonical_gdpc_factor.R`

Why split Python + R:
- the `gdpc` package lives in R, so the factor fit itself remains in R
- the surrounding workflow, manifests, alias generation, and file/path orchestration are easier to keep consistent in Python
- this keeps the statistical core close to the package call while still fitting cleanly into the rest of the repo’s tooling

Implemented responsibilities:
- Python wrapper reads canonical config and resolves the lineage paths
- R fitter runs `gdpc()` on the canonical standardized matrix with fixed lag `k`
- the fitter emits the canonical `GDPC1`, loadings, intercepts, initial-factor history, and metadata
- the Python wrapper writes compatibility aliases, manifests, and the human-readable review note

### Optional helper scripts

Implemented source-pipeline entry points in this pass:
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/download_canonical_climate_indices.py`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/build_canonical_climate_daily_matrices.py`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/run_canonical_climate_index_pipeline.py`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/render_canonical_climate_index_diagnostics.py`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/build_canonical_climate_stationarity_audit.R`
- `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/run_canonical_gdpc_master_pipeline.py`
- shared helper library:
  - `/data/muscat_data/jaguir26/project1_ucsc_phd/scripts/canonical_climate_indices_lib.py`
- source-pipeline runbook:
  - `/data/muscat_data/jaguir26/project1_ucsc_phd/repro/run/CANONICAL_GDPC_SOURCE_PIPELINE_RUNBOOK_20260509.md`
- full-pipeline runbook:
  - `/data/muscat_data/jaguir26/project1_ucsc_phd/repro/run/CANONICAL_GDPC_MASTER_PIPELINE_RUNBOOK_20260509.md`

## Proposed Sign And Scaling Convention

Because principal-component-style factors are sign-indeterminate, the implementation must freeze one deterministic sign rule.

Recommended sign anchor:
- after fitting `GDPC1`, orient the sign so that the correlation between `GDPC1` and one fixed anchor index is positive.

Recommended default anchor:
- `ONI`

Reason:
- `ONI` is stable, familiar, and already part of the retained 17-index set.

Alternative acceptable anchor:
- choose the index with the largest absolute loading at lag 0 and force that loading positive.

The sign rule must be written explicitly into metadata and validation summaries.

Scaling recommendation:
- store the raw `GDPC1` score series exactly as produced after sign alignment.
- do not re-standardize the final factor silently after fitting.
- if any downstream path requires a standardized variant later, it should be written as a separate documented derivative.

## Flexible Knobs That Must Remain Explicit

This phase should be rigid by default but not brittle. These items may change later if needed, but only through explicit config/metadata updates.

### Knob 1. Lag choice `k`

Canonical implemented value:
- `k = 2`
- `tol = 1e-3`
- `niter_max = 200`
- `crit = 'BIC'`

Allowed future changes:
- `k = 1`
- `k = 3`
- `k = 7`
- another explicit fixed lag if justified

Rule:
- changing `k` must create a new canonical lineage/version, not silently overwrite the current one.

### Knob 2. Interpolation tail handling

Historical notebook behavior used:
- cubic spline for most of the path,
- linear interpolation for the last 30 days.

This is acceptable as the initial canonical contract if we want continuity with the exploratory path.

Rule:
- the chosen interpolation/tail rule must be frozen in config and restated in metadata.

### Knob 3. Source acquisition method

We may:
- adapt `indxex_download.py`, or
- replace it with a cleaner canonical downloader.

Rule:
- the source manifest must record exact URLs, hashes, and retrieval timestamps regardless of which acquisition path we choose.

### Knob 4. Naming of compatibility files

We may keep using:
- `cov_05_PCA.csv`
- `cov_03_PCA.csv`

for downstream compatibility.

Rule:
- the canonical source-of-truth file must still be named as a `GDPC1` artifact,
- and the compatibility aliases must be documented clearly as aliases.

## Work Packages

### WP1. Canonical source acquisition and source manifest

Purpose:
- recreate the missing raw climate-index source layer in a documented way.

Tasks:
- [x] confirm the authoritative 17-index list in config
- [x] define the canonical raw-source directory
- [x] implement or adapt a source downloader
- [x] record source URLs and retrieval metadata
- [x] write a machine-readable source manifest with hashes
- [x] verify that all 17 files exist locally and parse successfully

Likely files touched:
- `config/canonical_gdpc_master_covariate.yaml`
- `scripts/download_canonical_climate_indices.py` or equivalent
- `data/canonical_gdpc_master/v20260509/inputs/raw_psl_text/*`
- `data/canonical_gdpc_master/v20260509/inputs/monthly_csv/*`
- `data/canonical_gdpc_master/v20260509/metadata/source_manifest.csv`

Acceptance:
- all 17 monthly source files exist,
- the source manifest is complete,
- and every file is hash-tracked.

### WP2. Daily interpolation and standardization build

Purpose:
- turn the monthly raw sources into the exact daily `T x 17` matrix used for GDPC fitting.

Tasks:
- [x] implement a reproducible monthly-to-daily interpolation step
- [x] restrict to `1987-05-29 -> 2023-01-22`
- [x] standardize each index over that exact restricted window
- [x] write both the non-standardized and standardized daily matrices
- [x] write build metadata describing date coverage and missingness

Likely files touched:
- `scripts/build_canonical_climate_daily_matrices.py`
- `data/canonical_gdpc_master/v20260509/intermediate/combined_climate_indices_daily_19870529_20230122.csv`
- `data/canonical_gdpc_master/v20260509/intermediate/combined_climate_indices_daily_standardized_19870529_20230122.csv`
- `data/canonical_gdpc_master/v20260509/metadata/build_metadata.json`

Acceptance:
- the standardized daily matrix has exactly 17 covariate columns plus date,
- the date coverage matches the canonical window exactly,
- and the build metadata records interpolation and standardization details explicitly.

### WP2.5 Stationarity and trend audit

Purpose:
- decide whether the canonical GDPC build should use the full 17 standardized series in levels or prune/pre-filter a subset.

Tasks:
- [x] run a reproducible trend and stationarity audit on the standardized daily matrix
- [x] document whether stationarity is required for the chosen GDPC method
- [x] record a clear keep/drop/pre-filter decision for the canonical source set

Likely files touched:
- `scripts/build_canonical_climate_stationarity_audit.R`
- `tests/python/test_canonical_climate_stationarity_audit.py`
- `data/canonical_gdpc_master/v20260509/review/stationarity/CANONICAL_GDPC_STATIONARITY_AUDIT.md`
- `data/canonical_gdpc_master/v20260509/review/stationarity/stationarity_audit.csv`

Decision frozen now:
- keep all 17 standardized daily climate indices in levels
- do not difference or detrend the inputs before fitting `GDPC1`

Acceptance:
- the stationarity audit is reproducible from the standardized daily matrix,
- the decision is written down explicitly,
- and the next GDPC build stage can consume the 17-series matrix without further pre-filtering ambiguity.

### WP3. Canonical GDPC1 fit

Purpose:
- fit the master `GDPC1` from the standardized daily matrix.

Tasks:
- [x] require the `gdpc` package explicitly in the build script
- [x] fit `gdpc()` with fixed lag `k = 2` by default
- [x] retain only the first component
- [x] compute and record explained-variance / reconstruction diagnostics exposed by the package
- [x] apply the deterministic sign rule
- [x] write the canonical `GDPC1` CSV

Likely files touched:
- `scripts/build_canonical_gdpc_master_covariate.py`
- `scripts/build_canonical_gdpc_factor.R`
- `data/canonical_gdpc_master/v20260509/outputs/gdpc_master_component_01_19870529_20230122.csv`
- `data/canonical_gdpc_master/v20260509/metadata/gdpc_build_metadata.json`

Acceptance:
- the build completes without notebooks,
- `GDPC1` is emitted with stable sign orientation,
- and diagnostics are preserved in metadata.

### WP4. Compatibility aliases for downstream workflow consumers

Purpose:
- let the rest of the workflow keep consuming a `PCA` slot while the canonical source becomes `GDPC1`.

Tasks:
- [ ] define the compatibility export naming contract
- [ ] generate compatibility aliases from the canonical `GDPC1` output
- [ ] ensure alias files carry the expected date column and one value column
- [ ] record alias-to-source mapping in metadata

Likely files touched:
- `scripts/build_canonical_gdpc_master_covariate.py`
- `data/canonical_gdpc_master/v20260509/outputs/compat/cov_05_PCA.csv`
- `data/canonical_gdpc_master/v20260509/outputs/compat/cov_03_PCA.csv`
- `data/canonical_gdpc_master/v20260509/metadata/compatibility_alias_manifest.csv`

Acceptance:
- every alias is reproducibly derived from the canonical `GDPC1` file,
- and the alias manifest documents that relationship explicitly.

### WP5. Validation and QA

Purpose:
- prove the canonical build is coherent before any workflow rewiring.

Tasks:
- [x] validate raw-source completeness
- [x] validate daily-matrix date continuity
- [x] validate per-index standardization summary
- [ ] validate GDPC output date coverage
- [ ] validate sign orientation rule
- [ ] validate compatibility alias equality against the canonical source series
- [ ] produce a short human-readable review report
- [x] produce a short human-readable review report

Recommended new outputs:
- `data/canonical_gdpc_master/v20260509/metadata/validation_summary.json`
- `data/canonical_gdpc_master/v20260509/review/CANONICAL_GDPC_BUILD_REVIEW.md`

Acceptance:
- one command can rebuild and validate the canonical lineage,
- and the review report is strong enough for advisor-facing inspection.

### WP6. Workflow rewiring

Purpose:
- make the workflow consume the canonical GDPC lineage rather than the legacy frozen static-PCA lineage.

Tasks:
- [ ] identify all config builders and staging paths that currently point to legacy `cov_*_PCA.csv`
- [ ] define the new canonical source path contract
- [ ] rewire shared-input staging so the `PCA` slot receives the canonical alias output
- [ ] keep downstream covariate names stable unless we intentionally rename them later
- [ ] add a provenance field so manifests can say that the `PCA` slot now comes from canonical `GDPC1`

Confirmed rewiring surfaces to revisit:
- `R/unified/stages/stage_data_prep_shared.R`
- `scripts/build_multimodel_v8_histfix_matrix.py`
- `scripts/build_multimodel_v8_featurecov_cf1_eps_matrix_configs.py`
- config builders that write `PCA` covariate paths
- validation scripts that assert `PPT|SOIL|PCA`

Acceptance:
- new runs can consume the GDPC-derived alias files without changing downstream family semantics,
- and manifests/reports record the new provenance clearly.

### WP7. Article-side snapshot and planning sync

Purpose:
- keep manuscript planning docs aligned once the canonical GDPC lineage exists.

Tasks:
- [ ] refresh the forward-plan doc to reference the implemented canonical lineage
- [ ] refresh article-side checklist/provenance notes as needed
- [ ] decide what minimal GDPC provenance snapshot belongs article-side

Acceptance:
- workflow-side canonical lineage is the source of truth,
- and article-side docs point to it cleanly without duplicating the full build machinery.

## Validation Gates Before Any Model Rerun

These are hard gates. If any fail, we should stop before rerunning anything.

### Gate A. Source completeness
- all 17 raw monthly source files exist
- source manifest is present
- every source file hash is recorded

### Gate B. Daily build integrity
- exact canonical date window is present:
  - `1987-05-29 -> 2023-01-22`
- no missing daily dates after interpolation
- standardized matrix has 17 index columns and one date column

### Gate C. GDPC fit integrity
- `gdpc()` build completes successfully
- chosen lag `k` is recorded
- sign rule is recorded and applied
- first component output is written deterministically

### Gate D. Compatibility integrity
- compatibility alias files match the canonical `GDPC1` series exactly up to column naming
- current staging code can ingest the alias files without further manual edits

### Gate E. Documentation integrity
- runbook/tracker/report agree on:
  - source set,
  - date window,
  - lag choice,
  - sign rule,
  - alias policy,
  - leakage policy.

## Smoke Tests And Dry Runs To Require During Implementation

These should be run during the future implementation pass.

1. **Config parse smoke test**
- builder reads `config/canonical_gdpc_master_covariate.yaml` cleanly

2. **Source acquisition smoke test**
- downloader or source-loader resolves all 17 source files

3. **Daily build smoke test**
- intermediate daily matrices are written with expected row count and coverage

4. **GDPC fit smoke test**
- `gdpc()` completes on the canonical matrix with the fixed lag

5. **Alias smoke test**
- alias files are written and pass equality checks against the source factor

6. **Workflow staging smoke test**
- a dry shared-input stage can point at the new alias files without breaking path expectations
- recommended representative dry target:
  - `2022-12-25 exAL-M-T1`
- the dry target should stage the canonical alias into the current `PCA` slot without running a full model fit

7. **Manifest smoke test**
- run manifest or validation report explicitly records canonical GDPC provenance

8. **Rebuild-repeatability smoke test**
- rerunning the canonical build with unchanged inputs/config should reproduce the same output hashes or explain any intentional non-determinism explicitly

## Risks And Mitigations

### Risk 1. Missing raw climate-index files

Current state:
- the old `climate_indices/` directory is missing.

Mitigation:
- treat raw-source acquisition as a first-class work package, not an assumption.

### Risk 2. Hidden dependence on legacy column names

Current state:
- many downstream files still expect the slot name `PCA` or the value name `Static_PCA`.

Mitigation:
- preserve compatibility aliases during this phase;
- do not rename the downstream covariate slot yet.

### Risk 3. Silent sign flips across rebuilds

Current state:
- sign is not constrained unless we constrain it.

Mitigation:
- freeze a deterministic sign anchor and validate it.

### Risk 4. Over-engineering the method selection

Current state:
- user explicitly does not want long lag-selection runs.

Mitigation:
- keep fixed lag `k = 2` as the canonical default;
- allow only a tiny non-binding sanity comparison if helpful.

### Risk 5. Premature model reruns before canonical factor is stable

Mitigation:
- keep this entire phase separate from rerun execution;
- require the validation gates above before any rerun plan advances.

## Out Of Scope For This Phase

The following are explicitly out of scope for the GDPC implementation phase itself:
- rebuilding the full-history retrospective bundles for `2021-01-23`, `2021-11-12`, and `2022-12-25`
- rerunning the 27 affected Bayesian rows
- changing the article’s final scientific claims
- changing the figure family contracts again
- replacing the `PCA` covariate slot name in all downstream code with `GDPC`

Those belong to later phases.

## Remaining Work Checklist

The source acquisition, diagnostics, stationarity audit, canonical GDPC fit, compatibility aliases, and initial workflow rewiring are now implemented.
The checklist below now serves as the completion record for the broader GDPC phase.

Preconditions already satisfied:
- [x] current workflow behavior has been audited
- [x] historical notebook evidence has been inspected
- [x] canonical source window has been chosen
- [x] canonical 17-index set has been chosen
- [x] leakage policy has been accepted explicitly
- [x] fixed-lag strategy has been chosen in principle
- [x] compatibility-alias strategy has been defined in principle
- [x] standardized-level stationarity decision has been frozen for the 17-series matrix

Still to do to complete the GDPC phase:
- [x] create canonical config
- [x] create canonical source acquisition step
- [x] create reproducible stationarity audit
- [x] create canonical GDPC builder
- [x] create validation outputs
- [x] rewire workflow consumers to canonical aliases
- [x] refresh planning/provenance docs after successful build

## Recommended Next Step After Approval

If Antonio approves this tracker, the next pass should implement **only** the GDPC lineage itself in this order:

1. canonical source acquisition + source manifest
2. daily interpolation + standardization build
3. canonical `gdpc()` fit with fixed `k = 2`
4. compatibility aliases
5. validation and review outputs
6. workflow rewiring to the canonical alias outputs

Only after that should we move into the separate full-history bundle reconstruction and corrected rerun work.
