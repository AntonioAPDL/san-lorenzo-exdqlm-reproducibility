# GEFS + NWM Forecast Audit Tracker

## 1. Goal
Audit the current repo for reusable GEFS and NWM forecast retrieval/extraction logic, confirm actual public source/file/variable availability for the requested 2021-01-23, 2021-11-12, 2021-12-21, 2022-05-11, and 2022-12-25 initializations, and define the smallest safe implementation step for soil-moisture and precipitation forecast point extraction at the existing Big Trees / San Lorenzo location.

Working constraints for this run:
- Audit first, implement second.
- No bulk download or full backfill until smoke tests pass.
- Reuse the existing location and nearest-valid-cell logic where possible.
- Keep unrelated local edits untouched.

Git / safety snapshot:
- Branch: `feature/export_posterior_tables`
- Repo status at start: `ahead 2`
- Unrelated modified files present before this task:
  - `R/unified/stages/stage_data_prep_shared.R`
  - `R/unified/stages/stage_forecats.R`
  - `scripts/forecats_batch.R`

## 2. Target forecast dates
- `2021-01-23`
- `2021-11-12`
- `2021-12-21`
- `2022-05-11`
- `2022-12-25`

## 3. Target variables
- GEFS:
  - precipitation: `APCP`
    - confirmed in sampled `pgrb2a` `.idx` inventories
  - soil moisture: `SOILW`
    - all available layers are in scope
    - top layer `0-0.1 m below ground` confirmed in `pgrb2a`
    - deeper layers `0.1-0.4 m`, `0.4-1 m`, and `1-2 m below ground` confirmed in `pgrb2b`
  - ensemble scope: full ensemble
    - control `gec00` plus all listed `gepNN` members in the dry-run manifest
- NWM:
  - precipitation: `RAINRATE`
    - selected from operational forcing files after smoke tests
    - `QRAIN` was not present in the sampled operational land/forcing files
  - soil moisture: `SOILSAT_TOP`
    - selected as the cross-range operational land variable because it was present in sampled short-, medium-, and long-range land files
  - medium-range-only soil supplement: `SOIL_M`
    - all four `soil_layers_stag=0..3` layers are in scope where operational medium-range land files expose them
    - `SOIL_M` was present in the sampled medium-range land file, but not in the sampled short- or long-range land files

## 4. Location-of-interest logic
- Existing location is already standardized in config and scripts as Big Trees / San Lorenzo:
  - `config/forecats_pipeline.template.yaml`
  - `scripts/build_nwm_retro_soil_point_series.py`
  - `scripts/build_era5_soil_moisture_point_series.py`
  - `forecast_download.py`
- Current canonical lat/lon observed in multiple files:
  - `lat=37.0443931`
  - `lon=-122.072464`
- Existing nearest-point / nearest-valid logic found:
  - Channel / feature-based:
    - `forecast_download.py`: selects nearest NWM hydrofabric reach centroid (`feature_id`) to the target point.
    - `scripts/nwm_retrospective_extract_point_zarr.py`: nearest `feature_id` by latitude/longitude for retrospective channel Zarr.
    - `scripts/nwm_retrospective_extract_point_v12_comp.py`: fixed or nearest `feature_id` for v1.2 `.comp`.
  - Gridded nearest-valid:
    - `scripts/build_nwm_retro_soil_point_series.py`: projects target lat/lon into NWM Lambert grid and runs `choose_cell(...)`, which falls back to nearest non-NaN cell within a configurable radius.
    - `scripts/forecats_extract_glofas_batch.py`: `pick_cell(..., cell_policy="nearest_valid")` chooses the closest finite GRIB cell and persists the cell choice for reuse across issue dates.
    - `scripts/forecats_extract_legacy_glofas_point.py`: sampled-time validity mask for nearest non-NaN NetCDF cell.
- Reuse assessment:
  - NWM gridded soil forecast work should reuse `scripts/build_nwm_retro_soil_point_series.py` cell-selection logic with minimal adaptation.
  - NWM forcing extraction can reuse the same selected x/y cell because sampled operational land and forcing files expose the same `x`, `y`, and `crs` grid coordinates.
  - NWM channel forecast work should reuse existing `feature_id` logic if channel products are needed.
  - GEFS gridded work can likely reuse the GloFAS-style nearest-valid-cell pattern rather than inventing a new selection scheme.

## 5. Existing repo assets
Current audit result: no confirmed GEFS-specific implementation has been found yet; the reusable pieces are mostly NWM/NWS plus generic gridded nearest-valid extraction logic.

| Path | Type | Source relevance | Status | Reuse plan |
| --- | --- | --- | --- | --- |
| `config/forecats_pipeline.template.yaml` | config | location / GloFAS / NWS | usable | Reuse `site.lat`, `site.lon`, and the existing `nearest_valid_cell` pattern as the config home for any future GEFS/NWM forecast point extraction settings. |
| `forecast_download.py` | script | NWM / cloud / location | legacy but informative | Reuse only as reference for historical operational NWM path patterns and nearest-hydrofabric-feature logic; do not extend directly. |
| `scripts/forecats_extract_nws_batch.py` | script | NWM/NWS operational forecast / pickle parsing | usable | Reuse for existing `results.pkl` batch extraction semantics and date/member/lead parsing; likely not the home for new public-cloud GEFS/NWM gridded retrievals. |
| `scripts/forecats_build_nws_weighted.py` | script | NWM/NWS operational forecast post-processing | usable | Reuse downstream weighting/aggregation conventions if new forecast series need to integrate with the current forecast bundle pipeline. |
| `scripts/nws_operational_latest_update.py` | script | NWM operational latest / cloud / HEAD probing | usable | Reuse URL/key builders and lightweight HEAD validation style for smoke tests against operational NWM files. |
| `scripts/nwm_retrospective_build_manifest.py` | script | NWM retrospective / cloud / metadata-first audit | usable | Reuse source registry patterns and bucket probing style; good template for a GEFS manifest or dry-run inventory helper if one becomes necessary. |
| `scripts/nwm_retrospective_extract_point_zarr.py` | script | NWM retrospective / NetCDF-Zarr / location | usable | Reuse for feature-based retrospective channel extraction; informative for public Zarr access patterns. |
| `scripts/nwm_retrospective_extract_point_v12_comp.py` | script | NWM retrospective / NetCDF / S3 | usable | Reuse only if v1.2-style `.comp` access matters; otherwise reference for tiny S3 fetch-and-extract workflow. |
| `scripts/build_nwm_retro_soil_point_series.py` | script | NWM retrospective / soil / gridded nearest-valid / Zarr | directly reusable | Primary reusable gridded cell-selection logic for NWM soil. Extend or mirror this logic for operational forecast land/forcing files rather than rebuilding from scratch. |
| `scripts/update_nwm_soil_retro_full.sh` | helper | NWM retrospective / soil | usable | Reuse as evidence of the existing maintained NWM soil pipeline and output locations; probably no direct code changes needed here for forecast work. |
| `scripts/build_era5_soil_moisture_point_series.py` | script | soil / NetCDF / nearest grid point | usable | Reuse the repo’s established climate-covariate pattern and file-placement conventions; point selection is nearest grid point, not nearest-valid. |
| `scripts/forecats_extract_glofas_batch.py` | script | GRIB / cfgrib / nearest-valid-cell | directly reusable pattern | Best existing GRIB nearest-valid-cell implementation to adapt for GEFS gridded products. |
| `scripts/forecats_extract_legacy_glofas_point.py` | script | NetCDF / nearest-valid-cell | usable | Secondary reusable nearest-valid-cell pattern for chunked NetCDF datasets. |
| `scripts/inventory_forecasts.py` | script | forecast file inventory | usable | Reuse if a local dry-run manifest or inventory summary is needed after smoke tests. |
| `scripts/build_gefs_nwm_forecast_manifest.py` | script | GEFS / NWM / cloud / metadata-only dry-run inventory | newly added and usable | Primary new manifest-first helper. Lists exact public objects for the five target dates and writes run-scoped GEFS/NWM manifests without downloading bulk forecast data. |
| `scripts/gefs_nwm_point_smoke_extract.py` | script | GEFS / NWM / point smoke extraction / nearest-valid reuse | newly added and usable | Minimal one-date point smoke extractor. Reuses `pick_cell(...)` for GEFS and `_forward_lcc(...)` + `choose_cell(...)` for NWM; uses byte-range GRIB fetches for GEFS to keep the smoke test small. |
| `scripts/extract_gefs_nwm_forecast_points.py` | script | GEFS / NWM / heavy point extraction / resumable output | newly added and usable | Primary manifest-consumer extractor. Reads the dry-run manifests, writes point-series CSVs plus per-file status/failure ledgers, uses indexed GEFS byte-range GRIB reads, and keeps NWM remote via `h5py`/HTTP range access. Post-audit correction: NWM packed soil variables now decode `scale_factor`/`add_offset` before writing point values, so `SOILSAT_TOP` and `SOIL_M` are stored in physical units rather than raw packed integers. |
| `scripts/check_gefs_nwm_forecast_extract_health.py` | script | GEFS / NWM / extraction integrity check | newly added and usable | Validates finished extraction outputs against the manifests, file-status ledgers, and per-date row counts. Writes a run-scoped JSON health summary and returns non-zero if any rows/files are missing, duplicated, or failed. |
| `scripts/build_gefs_failed_retry_bundle.py` | script | GEFS / retry manifest builder | newly added and usable | Builds a compact retry-only manifest bundle from failed GEFS status rows so transient throttling failures can be retried without touching successful files. |
| `scripts/reconcile_gefs_retry_outputs.py` | script | GEFS / retry reconciliation | newly added and usable | Merges a retry pass into a new non-destructive canonical output root, preserving the original extract for audit while replacing only the recovered failed URLs. |
| `scripts/run_gefs_failed_retry_pass.sh` | script | GEFS / retry orchestration | newly added and usable | End-to-end wrapper for building a retry bundle, running a low-concurrency GEFS-only retry pass, reconciling outputs, and writing final GEFS-only health summaries. |
| `scripts/consolidate_gefs_nwm_forecast_handoff.py` | script | GEFS / NWM / handoff cache builder | newly added and usable | Builds a non-destructive downstream handoff cache from the finished extraction CSVs. Mirrors the repo’s `forecast_cache/<source>/<date-partition>/...` pattern, preserves native lead times, and writes per-variable member matrices plus catalogs. |
| `scripts/plot_gefs_nwm_forecast_cutoff.R` | script | GEFS / NWM / cutoff plotting | newly added and usable | Reads the handoff cache for a chosen init/cutoff date and produces separate high-resolution soil and precipitation comparison figures. Supported modes now include full overlap with member spread summaries, a cleaner `mean_only` overlap mode, a covariate-overlay variant using the realized PRISM and ERA5 model covariates, and a `mean_only_same_units` mode that converts series into harmonized physical units (`m3/m3` for soil, mm per forecast day for precipitation). |
| `repro/NWS_NWM_GLOFAS_DATA_AUDIT_PLAN.md` | doc | NWM source chronology / bucket mapping | informative | Reuse as prior audit evidence for NWM retrospective buckets and version windows. |
| `repro/NWM_RETROSPECTIVE_EXTRACTION_WORKSTREAM_TRACKER.md` | doc | NWM extraction history / feature reuse | informative | Reuse to avoid rediscovering already verified NWM retrospective paths and feature IDs. |
| `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/` | run artifact | GEFS / NWM / manifests / smoke outputs | newly created | Current run-scoped output directory for the metadata-only dry-run manifest and point smoke extracts. Reuse as the starting point for the next broad retrieval implementation step. |

Environment notes relevant to implementation:
- `cfgrib 0.9.15.1` and `eccodes 2.43.0` are available in the current Python environment.
- `ncdump`, `h5dump`, and `nccopy` are available for NetCDF/HDF inspection.
- `wgrib2` and `pygrib` are not currently available; GEFS work should stay on the existing `cfgrib` path used elsewhere in the repo.

Checklist:
- [x] GEFS repo assets identified
- [x] NWM repo assets identified
- [x] existing location logic identified
- [x] existing closest-valid-grid logic identified
- [x] GEFS historical archive path confirmed
- [x] GEFS precipitation variable confirmed
- [x] GEFS soil-moisture variable confirmed
- [x] NWM historical operational archive path confirmed
- [x] NWM soil-moisture variable confirmed
- [x] NWM precipitation representation selected and justified
- [x] one GEFS smoke test passed
- [x] one NWM smoke test passed
- [x] dry-run retrieval plan drafted
- [x] recommendation for implementation written

Notes on checked items:
- `GEFS repo assets identified` means: no direct GEFS-specific downloader/extractor was found in this repo, but the reusable GRIB and nearest-valid-cell patterns are identified.
- `NWM soil-moisture variable confirmed` refers to operational forecast files in this run: `SOILSAT_TOP` is the only sampled land variable that was present across short-, medium-, and long-range products.
- `dry-run retrieval plan drafted` is documented in Section 10 and does not perform a backfill.
- User decision update in implementation phase:
  - GEFS should include all available `SOILW` layers and the full ensemble.
  - NWM should use cross-range `SOILSAT_TOP` and include medium-range `SOIL_M` layers when available.

## 6. Data-source assumptions to verify
- [x] Assumption 1: GEFS historical forecasts are publicly accessible from the archive.
  - Confirmed for all five requested initialization dates via `s3://noaa-gefs-pds/gefs.YYYYMMDD/00/atmos/`.
- [x] Assumption 2: GEFS precipitation/soil variables are `APCP` and `SOILW`, with useful content split across `pgrb2a` / `pgrb2b`.
  - Confirmed in sampled `.idx` inventories: `APCP` and top-layer `SOILW` in `pgrb2a`; deeper `SOILW` layers in `pgrb2b`.
- [x] Assumption 3: historical operational NWM forecasts for 2021-2022 are not primarily a short rolling AWS archive problem and may require a different public host/path.
  - Refuted for the requested dates in this run. Historical operational files were confirmed directly in Google Cloud bucket `national-water-model` for all five target dates.
- [x] Assumption 4: NWM precipitation target should be chosen explicitly between land-file rainfall variables and forcing-file precipitation variables.
  - Confirmed. Sampled operational forcing files exposed `RAINRATE`; sampled land files did not expose `QRAIN`.
- [x] Assumption 5: existing repo code already contains reusable location logic.
  - Confirmed by audit.

## 7. Smoke tests
Running log. All cloud checks were limited to listings, HEAD requests, `.idx` inventories, or single representative sample-file pulls to `/tmp`. No multi-date or full-horizon bulk download/backfill was performed.

| Test ID | Purpose | Exact command/script used | Date/source/cycle/file tested | Result | Interpretation |
| --- | --- | --- | --- | --- | --- |
| `AUDIT-001` | Confirm workspace safety state before edits | `git status --short --branch` | local repo | pass | Existing unrelated edits were detected and preserved. |
| `AUDIT-002` | Identify forecast/location/cloud assets before any download | targeted `rg` scans and file inspection | local repo | pass | Reusable NWM/NWS and nearest-valid-grid logic exists; no confirmed GEFS-specific code found yet. |
| `GEFS-001` | Confirm public GEFS archive path layout | `aws s3 ls --no-sign-request s3://noaa-gefs-pds/gefs.20210123/00/atmos/` | `2021-01-23`, GEFS, `00z` | pass | `pgrb2ap5`, `pgrb2bp5`, and related atmos prefixes exist as expected. |
| `GEFS-002` | Confirm member/file-family naming and practical lead structure | `aws s3 ls --no-sign-request s3://noaa-gefs-pds/gefs.20210123/00/atmos/pgrb2ap5/ | rg 'gec00\\.t00z\\.pgrb2a\\.0p50\\.(f003|f240|f384)|gep01\\.t00z\\.pgrb2a\\.0p50\\.f003'` | `2021-01-23`, GEFS, `00z`, `pgrb2ap5` | pass | Control and perturbed member names are confirmed; sampled leads extend through `f384`. |
| `GEFS-003` | Confirm lead-step transition | `aws s3 ls --no-sign-request s3://noaa-gefs-pds/gefs.20210123/00/atmos/pgrb2ap5/ | rg 'gec00\\.t00z\\.pgrb2a\\.0p50\\.(f237|f240|f243|f246|f249|f252|f378|f384)'` | `2021-01-23`, GEFS, `00z`, `pgrb2ap5` | pass | Observed 3-hour steps through `f240` and 6-hour steps after that. |
| `GEFS-004` | Confirm GEFS precipitation and top-layer soil moisture variables | `curl -s https://noaa-gefs-pds.s3.amazonaws.com/gefs.20210123/00/atmos/pgrb2ap5/gec00.t00z.pgrb2a.0p50.f003.idx | rg 'APCP|SOILW|TSOIL'` | `2021-01-23`, GEFS, control, `pgrb2a.f003.idx` | pass | `APCP` and top-layer `SOILW` are present in `pgrb2a`. |
| `GEFS-005` | Confirm deeper GEFS soil-moisture layers | `curl -s https://noaa-gefs-pds.s3.amazonaws.com/gefs.20210123/00/atmos/pgrb2bp5/gec00.t00z.pgrb2b.0p50.f003.idx | rg 'SOILW|TSOIL'` | `2021-01-23`, GEFS, control, `pgrb2b.f003.idx` | pass | Deeper `SOILW` layers are present in `pgrb2b`. |
| `GEFS-006` | Confirm pattern generalizes to a second requested date | `curl -s https://noaa-gefs-pds.s3.amazonaws.com/gefs.20221225/00/atmos/pgrb2ap5/gec00.t00z.pgrb2a.0p50.f003.idx | rg 'APCP|SOILW'` | `2022-12-25`, GEFS, control, `pgrb2a.f003.idx` | pass | Same `APCP` + `SOILW` pattern appears on a second target date. |
| `GEFS-007` | Confirm all requested GEFS dates exist | `python3 - <<'PY' ... aws s3 ls s3://noaa-gefs-pds/gefs.DATE/00/atmos/ ... PY` | all five requested GEFS dates | pass | All five target dates expose the expected `atmos/pgrb2a*` and `pgrb2b*` prefixes. |
| `NWM-001` | Confirm historical operational NWM prefixes on public cloud | `curl -s 'https://storage.googleapis.com/storage/v1/b/national-water-model/o?prefix=nwm.20211112/&delimiter=/' | python3 -m json.tool` | `2021-11-12`, NWM | pass | Public bucket exposes `short_range`, `medium_range_mem*`, `long_range_mem*`, `forcing_short_range`, and `forcing_medium_range`. |
| `NWM-002` | Confirm object availability with no download | `curl -I -s https://storage.googleapis.com/national-water-model/nwm.20211112/medium_range_mem1/nwm.t12z.medium_range.channel_rt_1.f003.conus.nc` | `2021-11-12`, NWM, medium-range channel sample | pass | HTTP `200` confirmed real historical object availability. |
| `NWM-003` | Confirm short-range land variable inventory | `curl -L -o /tmp/nwm_short_land_*.nc https://storage.googleapis.com/national-water-model/nwm.20211112/short_range/nwm.t00z.short_range.land.f001.conus.nc && python3 netCDF4 variable listing` | `2021-11-12`, NWM, short-range land `f001` | pass | Sampled vars include `SOILSAT_TOP`; no `SOIL_M` or `QRAIN` observed. |
| `NWM-004` | Confirm medium-range land variable inventory | `curl -L -o /tmp/nwm_medium_land_f003.nc https://storage.googleapis.com/national-water-model/nwm.20211112/medium_range_mem1/nwm.t00z.medium_range.land_1.f003.conus.nc && python3 netCDF4 variable listing` | `2021-11-12`, NWM, medium-range land `f003` | pass | Sampled vars include `SOIL_M` and `SOILSAT_TOP`. |
| `NWM-005` | Confirm long-range land variable inventory | `curl -L -o /tmp/nwm_long_land_f024.nc https://storage.googleapis.com/national-water-model/nwm.20211112/long_range_mem1/nwm.t00z.long_range.land_1.f024.conus.nc && python3 netCDF4 variable listing` | `2021-11-12`, NWM, long-range land `f024` | pass | Sampled vars include `SOILSAT_TOP` and `SOILSAT`; `SOIL_M` was not observed. |
| `NWM-006` | Confirm short-range forcing precipitation variable | `curl -L -o /tmp/nwm_forcing_full_f001.nc https://storage.googleapis.com/national-water-model/nwm.20211112/forcing_short_range/nwm.t00z.short_range.forcing.f001.conus.nc && python3 netCDF4 variable listing` | `2021-11-12`, NWM, short-range forcing `f001` | pass | Sampled vars include `RAINRATE`. |
| `NWM-007` | Confirm medium-range forcing precipitation variable | `curl -L -o /tmp/nwm_medium_forcing_f001.nc https://storage.googleapis.com/national-water-model/nwm.20211112/forcing_medium_range/nwm.t00z.medium_range.forcing.f001.conus.nc && python3 netCDF4 variable listing` | `2021-11-12`, NWM, medium-range forcing `f001` | pass | Sampled vars include `RAINRATE`. |
| `NWM-008` | Confirm all requested NWM dates exist on public GCS | `python3 - <<'PY' ... storage.googleapis.com/storage/v1/b/national-water-model/o?prefix=nwm.DATE/&delimiter=/ ... PY` | all five requested NWM dates | pass | All five target dates expose the expected operational forecast prefixes on GCS. |
| `NWM-009` | Confirm forcing families available across requested dates | `python3 - <<'PY' ... print([p for p in prefixes if 'forcing' in p]) ... PY` | all five requested NWM dates | pass | `forcing_short_range` and `forcing_medium_range` are present; no `forcing_long_range` prefix was observed. |
| `IMPL-001` | Build exact dry-run GEFS + NWM retrieval manifests for the five target dates | `python3 scripts/build_gefs_nwm_forecast_manifest.py --site-config config/forecats_pipeline.template.yaml` | run `gefs_nwm_forecast_manifest_20260307T023425Z` | pass | Wrote run-scoped manifests with exact listed object keys/URLs: GEFS `140120` rows over `56110` files; NWM `14180` rows over `4420` files. |
| `IMPL-002` | Smoke-test one-date GEFS point extraction using only indexed GRIB message subsets | `python3 scripts/gefs_nwm_point_smoke_extract.py --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --site-config config/forecats_pipeline.template.yaml --gefs-init-date 2021-01-23 --nwm-init-date 2021-11-12 --gefs-cycle 0 --nwm-cycle 0` | `2021-01-23`, GEFS, `00z`, control `f003` from `pgrb2a` + `pgrb2b` | pass | Extracted `APCP` plus all four GEFS `SOILW` layers at the nearest-valid grid cell while downloading only the indexed message byte ranges needed for the smoke test. |
| `IMPL-003` | Smoke-test one-date NWM point extraction across available forecast ranges | `python3 scripts/gefs_nwm_point_smoke_extract.py --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --site-config config/forecats_pipeline.template.yaml --gefs-init-date 2021-01-23 --nwm-init-date 2021-11-12 --gefs-cycle 0 --nwm-cycle 0` | `2021-11-12`, NWM, `00z`, short/medium/long land + short/medium forcing samples | pass | Extracted `SOILSAT_TOP` across short-, medium-, and long-range land, all four medium-range `SOIL_M` layers, and `RAINRATE` from both short- and medium-range forcing at the Big Trees target cell. |
| `IMPL-004` | Validate manifest-consumer extractor on real files before the broad run | `python3 scripts/extract_gefs_nwm_forecast_points.py --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --out-subdir extract_devtest --gefs-workers 2 --nwm-workers 2 --batch-size 4 --max-gefs-files 4 --max-nwm-files 4 --overwrite` | GEFS + NWM, first few manifest files | pass | Confirmed the heavy extractor writes point-series CSVs, per-file status ledgers, and source summaries with the same selected cells used in the smoke tests. |
| `IMPL-005` | Identify and resolve broad-run throughput bottlenecks before full extraction | `python3 scripts/extract_gefs_nwm_forecast_points.py ...` plus code inspection/benchmarks | local implementation | pass | Solved three root issues: process pools are now reused across batches, GEFS task generation now streams from manifest groups instead of pre-materializing all `56110` files, and GEFS/NWM workers now support per-file retries for transient network errors. |
| `IMPL-006` | Run broad NWM extraction from the manifest into run-scoped outputs | `python3 scripts/extract_gefs_nwm_forecast_points.py --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --out-subdir extract_full --sources nwm --nwm-workers 16 --batch-size 256 --overwrite` | full NWM broad run | pass | Completed with `4420/4420` files processed, `14180/14180` expected point rows written, and `0` failure rows. Output root: `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/extract_full/nwm/`. |
| `IMPL-007` | Run broad GEFS full-ensemble extraction from the manifest into run-scoped outputs | `python3 scripts/extract_gefs_nwm_forecast_points.py --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --out-subdir extract_gefs_full --sources gefs --gefs-workers 32 --batch-size 1024 --gefs-file-retries 3 --overwrite` | full GEFS broad run | pass | Completed with `56110/56110` files processed, `140120/140120` expected point rows written, and `0` failure rows. Output root: `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/extract_gefs_full/gefs/`. |
| `IMPL-008` | Run post-extraction integrity health check against manifests and outputs | `python3 scripts/check_gefs_nwm_forecast_extract_health.py --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z` | finished GEFS + NWM outputs | pass | Both sources passed all health checks: expected files/rows matched actual outputs exactly, failure ledgers were empty, no duplicate output rows or status rows were found, and no per-date row-count mismatches remained. Health summary written to `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/health_checks/forecast_extract_health.json`. |
| `IMPL-009` | Build a non-destructive downstream handoff cache from the finished extraction outputs | `python3 scripts/consolidate_gefs_nwm_forecast_handoff.py --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --overwrite` | finished GEFS + NWM outputs | pass | Wrote a run-scoped handoff cache under `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/handoff_forecasts/site=11160500/run_id=gefs_nwm_forecast_manifest_20260307T023425Z/`, including `forecast_cache/gefs/.../gefs_members.csv`, `forecast_cache/nwm/.../nwm_members.csv`, source catalogs, and `handoff_meta.json`. |
| `IMPL-010` | Build high-resolution cutoff plots from the finished handoff cache | `Rscript scripts/plot_gefs_nwm_forecast_cutoff.R --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --cutoff-date 2022-12-25` | `2022-12-25`, soil + precipitation | pass | Wrote `soil_forecasts.{png,pdf}` and `precip_forecasts.{png,pdf}` under `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/plots/cutoff_date=2022-12-25/`. The figures read directly from the handoff cache and use source/type-aware color mapping plus ensemble median/spread summaries. |
| `IMPL-011` | Rework the cutoff plots into single-panel overlap comparisons on a lead-days axis | `Rscript scripts/plot_gefs_nwm_forecast_cutoff.R --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --cutoff-date 2022-12-25` | `2022-12-25`, soil + precipitation | pass | Rewrote the same `soil_forecasts.{png,pdf}` and `precip_forecasts.{png,pdf}` as overlapped all-family comparisons. X-axis is now lead time in days from initialization. Because GEFS and NWM soil/precip products use different native units, each source/product/layer family is normalized to its own robust 5th-95th percentile range before plotting so shape, spread, and horizon differences can be compared honestly on one shared panel. |
| `IMPL-012` | Add and render an ensemble-mean-only overlap plot mode | `Rscript scripts/plot_gefs_nwm_forecast_cutoff.R --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --cutoff-date 2022-12-25 --plot-style mean_only` | `2022-12-25`, soil + precipitation | pass | Wrote `soil_forecasts_mean.{png,pdf}` and `precip_forecasts_mean.{png,pdf}` under `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/plots/cutoff_date=2022-12-25/`. These figures keep the same all-family overlap design and lead-in-days axis, but plot only the ensemble mean for each forecast family. Deterministic NWM products remain as their single available trajectory. |
| `IMPL-013` | Overlay the retrospective PRISM and ERA5 model covariates on the mean-forecast comparison plots | `Rscript scripts/plot_gefs_nwm_forecast_cutoff.R --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --cutoff-date 2022-12-25 --plot-style mean_only --overlay-covariates` | `2022-12-25`, soil + precipitation | pass | Wrote `soil_forecasts_mean_with_covariates.{png,pdf}` and `precip_forecasts_mean_with_covariates.{png,pdf}` under `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/plots/cutoff_date=2022-12-25/`. The black dashed overlay is the realized post-cutoff covariate trajectory drawn from `soil_moisture_data/soil_moisture_big_trees_daily_avg_1987_2023.csv` for soil and `prism_precipitation_santa_cruz_1987_2023.csv` for precipitation, matching the retrospective covariate sources referenced in the legacy model/figure scripts. |
| `IMPL-014` | Fix NWM packed soil decoding, rebuild the NWM handoff, and render same-unit forecast-vs-covariate plots | `python3 scripts/extract_gefs_nwm_forecast_points.py --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --out-subdir extract_full --sources nwm --nwm-workers 16 --batch-size 256 --overwrite` plus `python3 scripts/consolidate_gefs_nwm_forecast_handoff.py --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --overwrite` plus `Rscript scripts/plot_gefs_nwm_forecast_cutoff.R --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --cutoff-date 2022-12-25 --plot-style mean_only_same_units --overlay-covariates` | `2022-12-25`, soil + precipitation | pass | Confirmed that the original NWM extraction was writing packed `SOILSAT_TOP`/`SOIL_M` integers instead of decoded physical values, fixed the decoder in `scripts/extract_gefs_nwm_forecast_points.py`, reran the full NWM extraction cleanly, rebuilt the handoff cache, and wrote `soil_forecasts_mean_same_units_with_covariates.{png,pdf}` plus `precip_forecasts_mean_same_units_with_covariates.{png,pdf}`. In the new same-unit soil plot, GEFS `SOILW`, NWM `SOIL_M`, and ERA5 are all plotted in `m3/m3`, while NWM `SOILSAT_TOP` is converted to estimated volumetric water content using a site-level porosity estimate of `0.4305` (10-90% range `0.4236-0.4376`) derived from medium-range `SOIL_M` layers 0-1. In the new same-unit precipitation plot, GEFS `APCP`, NWM `RAINRATE`, and PRISM are all expressed as daily accumulated precipitation in mm per forecast day. |
| `IMPL-015` | Render the same-unit mean forecast-vs-covariate plots for all requested cutoff dates | `for d in 2021-01-23 2021-11-12 2021-12-21 2022-05-11 2022-12-25; do Rscript scripts/plot_gefs_nwm_forecast_cutoff.R --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --cutoff-date \"$d\" --plot-style mean_only_same_units --overlay-covariates; done` | all five requested cutoff dates, soil + precipitation | pass | Wrote `soil_forecasts_mean_same_units_with_covariates.{png,pdf}`, `precip_forecasts_mean_same_units_with_covariates.{png,pdf}`, and `plot_summary_mean_same_units_with_covariates.json` under each `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/plots/cutoff_date=YYYY-MM-DD/` directory for `2021-01-23`, `2021-11-12`, `2021-12-21`, `2022-05-11`, and `2022-12-25`. |
| `IMPL-016` | Add bias-matched soil and ensemble-quantile precipitation plot mode, then render it for all requested cutoff dates | `Rscript scripts/plot_gefs_nwm_forecast_cutoff.R --manifest-run-dir repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z --cutoff-date 2022-12-25 --plot-style mean_only_same_units_bias_quantiles --overlay-covariates` plus the same command for `2021-01-23`, `2021-11-12`, `2021-12-21`, and `2022-05-11` | all five requested cutoff dates, soil + precipitation | pass | Extended `scripts/plot_gefs_nwm_forecast_cutoff.R` with a new `mean_only_same_units_bias_quantiles` mode. Soil output now applies an additive per-family bias shift so each forecast family starts at the first realized retrospective soil value on day 1, while precipitation remains in same physical units and overlays ensemble uncertainty where available: solid line = mean, dashed = median, dotted bounds = 5th/95th percentiles, shown only for products with more than one member. For the current handoff, GEFS precipitation is the ensemble product with quantile structure; NWM forcing precipitation remains deterministic. The new outputs are `soil_forecasts_mean_same_units_bias_matched_with_covariates.{png,pdf}`, `precip_forecasts_mean_same_units_quantiles_with_covariates.{png,pdf}`, and `plot_summary_mean_same_units_bias_quantiles_with_covariates.json` under each cutoff-date plot directory. |
| `IMPL-017` | Recover transient NOAA GEFS throttling failures with a targeted retry bundle and non-destructive reconciliation | `bash scripts/run_gefs_failed_retry_pass.sh SOURCE_RUNTIME_ROOT/data_recovery/site=11160500/recovery_run=site11160500_recovery_20260406T185022Z/family=gefs_forecasts/full_runs/source_native_tranche1_20260406T194500Z/gefs_nwm_forecast_manifest_source_native_tranche1_20260406T194500Z gefs_retry_20260406T224500Z` | recovery runtime GEFS full run | pass | The base GEFS extract finished with `24` transient `HTTP 503: Slow Down` file failures and `62` missing point rows. The targeted retry bundle recovered all `24/24` failed object URLs, wrote `62/62` missing rows, preserved the original extract, and promoted a reconciled canonical output under `extract_gefs_full_reconciled_gefs_retry_20260406T224500Z/`. Reconciled health summary: `health_checks/gefs_reconciled_health_gefs_retry_20260406T224500Z.json`. |

## 8. Confirmed file/path patterns
### GEFS
- Public AWS archive is confirmed for all requested dates:
  - `s3://noaa-gefs-pds/gefs.YYYYMMDD/00/atmos/pgrb2ap5/`
  - `s3://noaa-gefs-pds/gefs.YYYYMMDD/00/atmos/pgrb2bp5/`
- Sampled control/member file naming:
  - `gec00.t00z.pgrb2a.0p50.fNNN`
  - `gepNN.t00z.pgrb2a.0p50.fNNN`
  - matching `.idx` sidecars are available and were sufficient for variable confirmation
- Variable placement confirmed from inventories:
  - `pgrb2a`: `APCP`, `SOILW:0-0.1 m below ground`
  - `pgrb2b`: deeper `SOILW` layers (`0.1-0.4`, `0.4-1`, `1-2 m below ground`)
- Practical lead pattern observed on sampled control member:
  - 3-hour steps through `f240`
  - 6-hour steps after `f240` through at least `f384`
- Manifested retrieval scope in run `gefs_nwm_forecast_manifest_20260307T023425Z`:
  - `00z` cycle only
  - full ensemble (`gec00` + all listed `gepNN` members)
  - `APCP` rows begin at `f003`
  - all available `SOILW` layers are represented
- Current run artifact paths:
  - `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/manifests/gefs_manifest.csv`
  - `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/smoke/gefs/gefs_point_smoke.csv`
- Repo-side status:
  - no GEFS-specific downloader/extractor was found during the audit
  - nearest-valid GRIB logic can be reused from `scripts/forecats_extract_glofas_batch.py`

### NWM
- Existing repo-local operational `results.pkl` key pattern:
  - `nwm.YYYYMMDD/.../nwm.t??z.medium_range.channel_rt_*.fNNN.conus.nc`
  - Parsed in `scripts/forecats_extract_nws_batch.py`
- Existing repo-local public URL pattern for current operational NWM:
  - `https://storage.googleapis.com/national-water-model/nwm.YYYYMMDD/medium_range_memM/nwm.t??z.medium_range.channel_rt_M.fNNN.conus.nc`
  - Built in `scripts/nws_operational_latest_update.py`
- Existing repo-local retrospective source registry:
  - `s3://noaa-nwm-retrospective-3-0-pds/CONUS/zarr/chrtout.zarr`
  - `s3://noaa-nwm-retrospective-2-1-zarr-pds/chrtout.zarr`
  - `s3://noaa-nwm-retro-v2-zarr-pds`
  - `s3://nwm-archive/YYYY/YYYYMMDDHH00.CHRTOUT_DOMAIN1.comp`
  - Defined in `scripts/nwm_retrospective_build_manifest.py`
- Existing repo-local retrospective soil source:
  - `s3://noaa-nwm-retrospective-3-0-pds/CONUS/zarr/ldasout.zarr`
  - Defined in `scripts/build_nwm_retro_soil_point_series.py`
- Historical operational public-cloud patterns confirmed for all requested dates:
  - Short-range land:
    - `https://storage.googleapis.com/national-water-model/nwm.YYYYMMDD/short_range/nwm.t00z.short_range.land.fNNN.conus.nc`
  - Short-range forcing:
    - `https://storage.googleapis.com/national-water-model/nwm.YYYYMMDD/forcing_short_range/nwm.t00z.short_range.forcing.fNNN.conus.nc`
  - Medium-range land:
    - `https://storage.googleapis.com/national-water-model/nwm.YYYYMMDD/medium_range_memM/nwm.t00z.medium_range.land_M.fNNN.conus.nc`
  - Medium-range forcing:
    - `https://storage.googleapis.com/national-water-model/nwm.YYYYMMDD/forcing_medium_range/nwm.t00z.medium_range.forcing.fNNN.conus.nc`
  - Medium-range channel:
    - `https://storage.googleapis.com/national-water-model/nwm.YYYYMMDD/medium_range_memM/nwm.t00z.medium_range.channel_rt_M.fNNN.conus.nc`
  - Long-range land:
    - `https://storage.googleapis.com/national-water-model/nwm.YYYYMMDD/long_range_memM/nwm.t00z.long_range.land_M.fNNN.conus.nc`
  - Long-range channel:
    - `https://storage.googleapis.com/national-water-model/nwm.YYYYMMDD/long_range_memM/nwm.t00z.long_range.channel_rt_M.fNNN.conus.nc`
- Sampled operational NWM variable findings:
  - short-range land: `SOILSAT_TOP`
  - medium-range land: `SOIL_M`, `SOILSAT_TOP`
  - long-range land: `SOILSAT_TOP`, `SOILSAT`
  - short-range forcing: `RAINRATE`
  - medium-range forcing: `RAINRATE`
- Sampled practical lead structures from file naming and global attrs:
  - short-range land/forcing: hourly files, `model_total_valid_times = 18`
  - medium-range land: sampled at 3-hour output spacing, `model_total_valid_times = 80`
  - medium-range forcing: hourly output, `model_total_valid_times = 240`
  - long-range land: sampled at daily spacing, `model_total_valid_times = 30`
- Important absence observed:
  - no `forcing_long_range` prefix was observed on the tested dates, so precipitation lead time appears to stop at the medium-range forcing horizon unless another product/archive is introduced later
- Manifested retrieval scope in run `gefs_nwm_forecast_manifest_20260307T023425Z`:
  - `SOILSAT_TOP` rows across short-, medium-, and long-range land
  - `SOIL_M` layer rows (`soil_layers_stag=0..3`) for medium-range land only
  - `RAINRATE` rows across short- and medium-range forcing
- Current run artifact paths:
  - `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/manifests/nwm_manifest.csv`
  - `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/smoke/nwm/nwm_point_smoke.csv`

## 9. Open questions / ambiguities
- For NWM precipitation, is it acceptable that practical precipitation lead time currently appears limited to short- and medium-range forcing because no long-range forcing prefix was observed?
- Should the first broad retrieval implementation write standalone run-scoped point-series CSVs first, or should it wire directly into the existing `forecats` bundle/weighting pipeline in the same change?
- If standalone point-series CSVs are preferred first, what should the target output layout be for GEFS full-ensemble point series versus NWM multi-range point series?
- Should the finished outputs remain in separate run-scoped subdirectories (`extract_full/nwm` and `extract_gefs_full/gefs`) or be consolidated into a single shared extraction directory for downstream pipeline wiring?

## 10. Next actions
Completed in this run:
1. Added `scripts/build_gefs_nwm_forecast_manifest.py`.
2. Added `scripts/gefs_nwm_point_smoke_extract.py`.
3. Added `scripts/extract_gefs_nwm_forecast_points.py`.
4. Built run-scoped manifests under `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/`.
5. Completed one-date point smoke extraction for:
   - GEFS `2021-01-23` `00z` control `f003`, including `APCP` plus all `SOILW` layers
   - NWM `2021-11-12` `00z` short/medium/long land plus short/medium forcing
6. Solved the main broad-run implementation issues:
   - avoided full local NWM downloads by using remote `h5py`/HTTP range access
   - removed GEFS upfront task-materialization bottlenecks by streaming manifest groups per batch
   - reused process pools across batches instead of recreating them
   - added per-file retry handling for transient GEFS/NWM network failures
7. Started broad extraction runs:
   - NWM broad run under `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/extract_full/nwm/`
   - GEFS broad run under `repro/gefs_nwm_forecast_runs/gefs_nwm_forecast_manifest_20260307T023425Z/extract_gefs_full/gefs/`
   - final broad-extraction outcome:
     - NWM complete: `4420/4420` files processed, `14180` output rows, `0` failure rows
     - GEFS complete: `56110/56110` files processed, `140120` output rows, `0` failure rows
8. Ran post-extraction health checks and confirmed both sources match their manifests exactly.
9. Built a non-destructive handoff cache that mirrors the repo’s forecast-cache layout while preserving native GEFS/NWM lead-time resolution.
10. Built reusable cutoff-plotting outputs for `2022-12-25`, with separate soil and precipitation figures sourced from the handoff cache.
11. Harmonized forecast and retrospective units for comparison plots and rendered same-unit, covariate-overlay mean plots for all five requested cutoff dates.
12. Added a bias-matched soil comparison plot and an ensemble-quantile precipitation comparison plot in same units, then rendered both for all five requested cutoff dates.

Recommended next implementation step:
11. After the broad point-series CSVs are complete and health-checked, decide whether to:
   - keep them as standalone run-scoped forecast point products, or
   - consume the new handoff cache under `handoff_forecasts/.../forecast_cache/` from a narrow downstream plotting/modeling step.

Safe execution order after that:
12. Preserve the finished extraction outputs, the health summary JSON, the handoff cache, and the example cutoff plots as the canonical record for this run.
13. If a downstream family wants daily or bundle-specific aliases, derive them from the handoff cache rather than rerunning extraction.
14. Only after the point-series outputs look correct should the repo decide whether to wire those outputs into the existing `forecats` plotting/bundle pipeline.
