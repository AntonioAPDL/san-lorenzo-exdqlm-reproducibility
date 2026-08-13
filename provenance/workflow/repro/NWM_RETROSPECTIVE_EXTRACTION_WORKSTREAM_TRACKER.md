# NWM Retrospective Extraction Workstream Tracker

## 1) Purpose
Track implementation progress for extracting NWS/NWM retrospective streamflow point series at USGS Big Trees (site `11160500`) across versions `v1.2`, `v2.0`, `v2.1`, and `v3.0`, with reproducible manifests, audits, and issue tracking.

This tracker is the operational source of truth for:
- Phase status
- Commands executed
- Artifacts produced
- Open issues and resolutions

---

## 2) Scope and Target
- Target location (fixed): `lat=37.0443931`, `lon=-122.072464`
- Preferred extraction target: nearest valid NWM feature to target location
- Streamflow variable: channel `streamflow` (cms)
- Coverage intent:
  - `v1.2`: `1993-01-01` to `2017-12-31`
  - `v2.0`: `1993-01-01` to `2018-12-31`
  - `v2.1`: `1979-02-01` to `2020-12-31`
  - `v3.0`: `1979-02-01` to `2023-01-31`

---

## 3) Phase Checklist

### Phase 0: Preflight and Manifest
- [x] Create version-source manifest for all four retrospective versions
- [x] Probe bucket structure and confirm extraction paths
- [x] Confirm Python dependency stack for Zarr and NetCDF workflows

### Phase 1: Reuse Existing Local Assets
- [x] Inventory existing local NWM retrospective files
- [x] Audit daily continuity and NaNs for existing local series
- [x] Register reusable artifacts in manifest

### Phase 2: Version-Specific Extraction Implementations
- [x] Implement Zarr point-extraction script for `v3.0/v2.1/v2.0`
- [x] Implement v1.2 NetCDF `.comp` point-extraction script (`nwm-archive`)
- [x] Implement continuity audit script for produced outputs

### Phase 3: Pilot Extractions
- [x] Pilot `v3.0` Zarr point extraction
- [x] Pilot `v2.1` Zarr point extraction
- [x] Pilot `v2.0` Zarr point extraction
- [x] Pilot `v1.2` `.comp` point extraction
- [x] Run pilot continuity/NA audit

### Phase 4: Full Extractions
- [x] Full `v2.0` point extraction
- [ ] Full `v1.2` point extraction (in progress; year-sharded tmux workers running)
- [x] Re-validate local `v2.1` and `v3.0` series against same schema/audit
- [x] Full `v2.1`/`v3.0` point extractions from Zarr (schema-aligned with `v2.0`)

### Phase 5: Integration Readiness
- [ ] Build unified NWM retrospective table with columns `NWS1.2/NWS2.0/NWS2.1/NWS3.0`
- [ ] Validate readiness for `forecats.png` integration
  - Provisional artifact created: `repro/nwm_retrospective_runs/nwm_retrospective_campaign_20260218T024352Z/point_series/nwm_unified_daily.csv`
  - Current non-NA counts: `NWS1.2=2` (pilot only), `NWS2.0=9496`, `NWS2.1=15310`, `NWS3.0=16071`
  - Full compare rerender completed for all baseline cutoffs using multiple NWS retrospective overlays:
    - run id: `20260218_paper_default_nws_multiretro_compare_r02`
    - figures generated: `1176/1176` (`2019-11-05` through `2023-01-31`)
    - note: `NWS v2.0` is configured but not visible in these windows because v2.0 coverage ends `2018-12-31`.

---

## 4) Run Ledger

### Run: `nwm_retrospective_campaign_20260218T024352Z`
- Status: in progress (main extraction logic validated; `v1.2` full run pending)
- Artifacts:
  - `repro/nwm_retrospective_runs/nwm_retrospective_campaign_20260218T024352Z/manifests/version_source_manifest.csv`
  - `repro/nwm_retrospective_runs/nwm_retrospective_campaign_20260218T024352Z/manifests/local_inventory.csv`
  - `repro/nwm_retrospective_runs/nwm_retrospective_campaign_20260218T024352Z/logs/bucket_probes.json`
  - `repro/nwm_retrospective_runs/nwm_retrospective_campaign_20260218T024352Z/point_series/v20_full_daily.csv`
  - `repro/nwm_retrospective_runs/nwm_retrospective_campaign_20260218T024352Z/point_series/v21_full_daily_from_zarr.csv`
  - `repro/nwm_retrospective_runs/nwm_retrospective_campaign_20260218T024352Z/point_series/v30_full_daily_from_zarr.csv`
  - `repro/nwm_retrospective_runs/nwm_retrospective_campaign_20260218T024352Z/point_series/pilot_v12_daily.csv`
  - `repro/nwm_retrospective_runs/nwm_retrospective_campaign_20260218T024352Z/point_series/nwm_unified_daily.csv`
  - `repro/nwm_retrospective_runs/nwm_retrospective_campaign_20260218T024352Z/audits/combined_audit_summary.csv`
- Key results:
  - Zarr full daily continuity is complete for `v2.0/v2.1/v3.0` with zero missing days in expected ranges.
  - `feature_id=17682474` is valid in all tested versions (`v1.2/v2.0/v2.1/v3.0` pilots).
  - `v1.2` pilot (`1993-01-01` to `1993-01-02`) succeeded with 48/48 hourly files and no missing-hour fetches.
  - Repository scan found no pre-existing full `v1.2` point-series artifact to reuse; `.comp` extraction remains required for full coverage.
  - Legacy-local vs re-extracted equivalence check for NWS point series:
    - `v3.0`: overlap `16071` days, correlation `1.0`, max abs diff `2.84e-14`.
    - `v2.1`: overlap `15310` days, correlation `1.0`, max abs diff `5.68e-14`.
    - conclusion: legacy-local and re-extracted `v2.1/v3.0` are numerically identical up to floating-point epsilon.
  - Full-run extraction scripts now available for all versions:
    - `scripts/nwm_retrospective_extract_point_zarr.py`
    - `scripts/nwm_retrospective_extract_point_v12_comp.py`
    - `scripts/nwm_retrospective_audit_point_series.py`
    - `scripts/nwm_retrospective_build_unified_table.py`

---

## 5) Issues and Decisions

### Open Issues
1. `v1.2` is NetCDF `.comp` in `s3://nwm-archive/` (no Zarr path in reviewed metadata), requiring separate extraction logic from Zarr versions.
2. `v1.2` CHRTOUT files do not expose `latitude/longitude` in-file (only `feature_id` + variables in tested sample), so nearest-feature lookup cannot be derived directly from `v1.2` files.
3. Full `v1.2` extraction is an operationally long run (hourly file loop over 25 years), so it should be executed as resumable shards (by year or interval list).
4. Existing local `v3.0` hourly CSV (`11160500_nws_retro.csv`) includes a trailing timestamp at `2023-02-01 00:00`, while extracted daily range from Zarr bounded at `2023-01-31` matches expected coverage framing.

### Decisions
1. Use metadata-first probes before any heavy extraction.
2. Use Zarr direct point extraction for `v3.0/v2.1/v2.0` to avoid bulk downloads.
3. Use `.comp` hourly file reads for `v1.2`, with resumable/year-sharded execution for full coverage.
4. Pin `feature_id=17682474` for `v1.2` full extraction to ensure consistent site mapping with Zarr-era series.

---

## 6) Standard Paths
- Run root: `repro/nwm_retrospective_runs/`
- Scripts:
  - `scripts/nwm_retrospective_build_manifest.py`
  - `scripts/nwm_retrospective_extract_point_zarr.py`
  - `scripts/nwm_retrospective_extract_point_v12_comp.py`
  - `scripts/nwm_retrospective_audit_point_series.py`
- Outputs (current run):
  - `repro/nwm_retrospective_runs/<run_id>/manifests/`
  - `repro/nwm_retrospective_runs/<run_id>/point_series/`
  - `repro/nwm_retrospective_runs/<run_id>/audits/`
  - `repro/nwm_retrospective_runs/<run_id>/logs/`

---

## 7) Next Execution Block (v1.2 Full Run)

Recommended shard strategy: run one year per shard and merge daily outputs after success.

Example command pattern:

```bash
RUN_ID=nwm_retrospective_campaign_20260218T024352Z
ROOT=repro/nwm_retrospective_runs/${RUN_ID}
mkdir -p "${ROOT}/point_series/v12_yearly" "${ROOT}/logs/v12_yearly"

for Y in $(seq 1993 2017); do
  python3 scripts/nwm_retrospective_extract_point_v12_comp.py \
    --bucket nwm-archive \
    --version 1.2 \
    --lat 37.0443931 \
    --lon -122.072464 \
    --feature-id 17682474 \
    --start-date "${Y}-01-01" \
    --end-date "${Y}-12-31" \
    --aggregate daily \
    --out-csv "${ROOT}/point_series/v12_yearly/v12_${Y}_daily.csv" \
    --out-meta "${ROOT}/logs/v12_yearly/v12_${Y}_meta.json" \
    --missing-hours-csv "${ROOT}/logs/v12_yearly/v12_${Y}_missing_hours.csv"
done
```

Post-merge (after all yearly shards complete):

```bash
python3 scripts/nwm_retrospective_build_unified_table.py \
  --v12 "${ROOT}/point_series/v12_full_daily.csv" \
  --v20 "${ROOT}/point_series/v20_full_daily.csv" \
  --v21 "${ROOT}/point_series/v21_full_daily_from_zarr.csv" \
  --v30 "${ROOT}/point_series/v30_full_daily_from_zarr.csv" \
  --out-csv "${ROOT}/point_series/nwm_unified_daily.csv"
```
