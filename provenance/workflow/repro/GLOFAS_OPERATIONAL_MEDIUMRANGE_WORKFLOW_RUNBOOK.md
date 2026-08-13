# GloFAS Operational Medium-Range Downloader Runbook

## 1) Executive Summary

This workflow downloads GloFAS medium-range forecast GRIB files from EWDS for issue dates, stores them in a partitioned `issue_date=` layout, records each request in a manifest, and feeds downstream batch extraction/render pipelines.

Core script: `glofas_operational_mediumrange_download_point.py:1`.

It exists to support reproducible, restartable, point-focused forecast extraction at Big Trees / San Lorenzo defaults (`lat=37.0443931`, `lon=-122.072464`) with tmux-parallel interval splits (`config/glofas_operational_medium_range_tmux_splits.yaml:15`).

## 2) File-by-File Map

| File | Role | Key Inputs | Key Outputs | Connections |
|---|---|---|---|---|
| `glofas_operational_mediumrange_download_point.py` | EWDS downloader + manifest writer | CLI args, interval list, `cdsapi` config | GRIBs under `grib/issue_date=...`, request JSON sidecars, `download_manifest.csv`, `download.log` | Feeds `scripts/forecats_extract_glofas_batch.py` through `grib_root` |
| `config/glofas_operational_medium_range_tmux_splits.yaml` | Declares tmux session interval splits | Session names + date ranges | Operational run partition plan | Used when launching split downloader sessions |
| `data/glofas_operational_medium_range/logs/download.log` | Structured downloader runtime log | Downloader execution stream | Errors, GET/OK events, status transitions | Primary troubleshooting source |
| `data/glofas_operational_medium_range/logs/optA_stdout.log` | tmux split A stdout | Session `glofas_optA` intervals | EWDS lifecycle lines + per-day OK lines | Validates split execution for A |
| `data/glofas_operational_medium_range/logs/optB_stdout.log` | tmux split B stdout | Session `glofas_optB` intervals | EWDS lifecycle lines + per-day OK lines | Validates split execution for B |
| `data/glofas_operational_medium_range/logs/optC_stdout.log` | tmux split C stdout | Session `glofas_optC` intervals | EWDS lifecycle lines + per-day OK lines | Validates split execution for C |
| `data/glofas_operational_medium_range/logs/optD_stdout.log` | tmux split D stdout | Session `glofas_optD` intervals | EWDS lifecycle lines + per-day OK lines | Validates split execution for D |
| `data/glofas_operational_medium_range/logs/stage1_stdout.log` | Earlier staging run stdout | Single interval run | Skip/OK progression | Historical evidence of restart/idempotency |
| `data/glofas_operational_medium_range/logs/sess1_stdout.log` | Earlier split session 1 | Legacy split interval | EWDS lifecycle + OK rows | Earlier run family evidence |
| `data/glofas_operational_medium_range/logs/sess2_stdout.log` | Earlier split session 2 | Legacy split interval | EWDS lifecycle + OK rows | Earlier run family evidence |
| `data/glofas_operational_medium_range/logs/sess3_stdout.log` | Earlier split session 3 | Legacy split interval | EWDS lifecycle + OK rows | Earlier run family evidence |
| `data/glofas_operational_medium_range/logs/sess4_stdout.log` | Earlier split session 4 | Legacy split intervals | EWDS lifecycle + OK rows | Earlier run family evidence |
| `data/glofas_operational_medium_range/manifests/download_manifest.csv` | Download ledger | Downloader row appends | status per issue-date + req_id + model + notes | Primary completion/audit table |
| `scripts/forecats_batch.R` | Batch orchestrator (`audit`, `build-forecasts`, `render`) | Config YAML + cutoff dates | audit CSVs, forecast caches, per-cutoff bundles/figures | Calls `scripts/forecats_extract_glofas_batch.py` |
| `scripts/forecats_extract_glofas_batch.py` | GRIB -> per-issue forecast cache converter | `grib_root`, dates list, point, dtype mapping | `forecast_cache/glofas/issue_date=.../glofas_members.csv` + `cell.json` | Upstream: downloader GRIBs; Downstream: `forecats_batch.R` render |
| `config/forecats_batch.site=11160500.default.yaml` | Batch/render config for site 11160500 | Paths, intervals, transforms, weighting | Configuration used by `forecats_batch.R` | Binds operational GRIB root and plotting semantics |

## 3) End-to-End Data Flow

1. Issue-date intervals are selected either from built-in defaults (`glofas_operational_mediumrange_download_point.py:47`) or `--intervals-file` (`glofas_operational_mediumrange_download_point.py:346`).
2. For each date, downloader builds EWDS request (`glofas_operational_mediumrange_download_point.py:111`) with `system_version="operational"` (`glofas_operational_mediumrange_download_point.py:56`) and model selected by date switch (`glofas_operational_mediumrange_download_point.py:82`).
3. Downloader saves request JSON sidecar then retrieves GRIB (`glofas_operational_mediumrange_download_point.py:260`, `glofas_operational_mediumrange_download_point.py:270`).
4. Downloader appends manifest rows with status and notes (`glofas_operational_mediumrange_download_point.py:289`).
5. `scripts/forecats_batch.R` in `build-forecasts` mode calls `scripts/forecats_extract_glofas_batch.py` (`scripts/forecats_batch.R:154`).
6. Extractor decodes control+perturbed data (`scripts/forecats_extract_glofas_batch.py:143`), converts to per-target-date member matrix (`scripts/forecats_extract_glofas_batch.py:287`), and writes `glofas_members.csv` per issue-date (`scripts/forecats_extract_glofas_batch.py:299`).
7. In render mode, batch script copies cached GloFAS/NWS CSVs into per-cutoff bundle inputs (`scripts/forecats_batch.R:453`) and renders `forecats.png` (`scripts/forecats_batch.R:500`).

## 4) Downloader I/O Specification (`glofas_operational_mediumrange_download_point.py`)

### 4.1 CLI Arguments

- `--lat` default `37.0443931` (`glofas_operational_mediumrange_download_point.py:334`, `glofas_operational_mediumrange_download_point.py:40`)
- `--lon` default `-122.072464` (`glofas_operational_mediumrange_download_point.py:335`, `glofas_operational_mediumrange_download_point.py:41`)
- `--buffer-deg` default `1.0` (`glofas_operational_mediumrange_download_point.py:336`, `glofas_operational_mediumrange_download_point.py:42`)
- `--out-root` default `/data/.../data/glofas_operational_medium_range` (`glofas_operational_mediumrange_download_point.py:337`, `glofas_operational_mediumrange_download_point.py:44`)
- `--dry-run` or `--run` (mutually exclusive, default behavior is dry-run when `--run` is omitted) (`glofas_operational_mediumrange_download_point.py:339`, `glofas_operational_mediumrange_download_point.py:355`)
- `--overwrite` (`glofas_operational_mediumrange_download_point.py:343`)
- `--verbose` (`glofas_operational_mediumrange_download_point.py:344`)
- `--intervals-file` optional external intervals (`glofas_operational_mediumrange_download_point.py:346`)

### 4.2 Request Fields Sent to EWDS

Constructed in `build_request(...)`:
- `system_version` (constant `operational`) (`glofas_operational_mediumrange_download_point.py:115`)
- `hydrological_model` (`htessel_lisflood` before 2021-05-26, else `lisflood`) (`glofas_operational_mediumrange_download_point.py:85`, `glofas_operational_mediumrange_download_point.py:116`)
- `product_type=[control_forecast, ensemble_perturbed_forecasts]` (`glofas_operational_mediumrange_download_point.py:58`, `glofas_operational_mediumrange_download_point.py:117`)
- `variable=river_discharge_in_the_last_24_hours` (`glofas_operational_mediumrange_download_point.py:57`, `glofas_operational_mediumrange_download_point.py:118`)
- date parts `year/month/day` from issue date (`glofas_operational_mediumrange_download_point.py:119`)
- `leadtime_hour=24..720 step 24` (`glofas_operational_mediumrange_download_point.py:59`, `glofas_operational_mediumrange_download_point.py:122`)
- `format=grib` (`glofas_operational_mediumrange_download_point.py:123`)
- `area=[north,west,south,east]` from `area_bbox(...)` (`glofas_operational_mediumrange_download_point.py:89`, `glofas_operational_mediumrange_download_point.py:124`)

Saved sidecars confirm `system_version: operational` in historical runs (`data/glofas_operational_medium_range/grib/issue_date=2019-11-05/operational_htessel_lisflood_2019-11-05_36c589c77f.request.json:48`, `data/glofas_operational_medium_range/grib/issue_date=2021-05-26/operational_lisflood_2021-05-26_d186c0e862.request.json:48`).

### 4.3 Output Layout

- GRIB files: `.../grib/issue_date=YYYY-MM-DD/<req_id>.grib` (`glofas_operational_mediumrange_download_point.py:131`)
- Request sidecar: same base name, `.request.json` (`glofas_operational_mediumrange_download_point.py:261`)
- Manifest: `.../manifests/download_manifest.csv` (`glofas_operational_mediumrange_download_point.py:139`)
- Log: `.../logs/download.log` (`glofas_operational_mediumrange_download_point.py:144`)

### 4.4 Manifest Schema

Header observed: `issue_date,status,path,req_id,hydrological_model,notes,timestamp_utc` (`data/glofas_operational_medium_range/manifests/download_manifest.csv:1`).

### 4.5 Status Lifecycle

Implementation states:
- `planned` initial (`glofas_operational_mediumrange_download_point.py:223`)
- `skipped_exists` when file already present (`glofas_operational_mediumrange_download_point.py:227`)
- `downloaded` on non-empty success (`glofas_operational_mediumrange_download_point.py:278`)
- `error_empty` on zero-byte result (`glofas_operational_mediumrange_download_point.py:274`)
- `error_exception` on retrieve failure (`glofas_operational_mediumrange_download_point.py:283`)

Manifest examples include `error_exception`, `downloaded`, `skipped_exists` (`data/glofas_operational_medium_range/manifests/download_manifest.csv:4`, `data/glofas_operational_medium_range/manifests/download_manifest.csv:10`, `data/glofas_operational_medium_range/manifests/download_manifest.csv:12`).

## 5) tmux Split Strategy (`config/glofas_operational_medium_range_tmux_splits.yaml`)

### 5.1 Declared split sessions

- `glofas_optA`: `2021-03-19..2021-05-25`, `2022-07-15..2022-09-23` (`config/glofas_operational_medium_range_tmux_splits.yaml:16`)
- `glofas_optB`: `2021-05-28..2021-08-04`, `2022-05-05..2022-07-13` (`config/glofas_operational_medium_range_tmux_splits.yaml:24`)
- `glofas_optC`: `2021-08-05..2021-12-31` (`config/glofas_operational_medium_range_tmux_splits.yaml:32`)
- `glofas_optD`: `2022-09-24..2023-01-31` (`config/glofas_operational_medium_range_tmux_splits.yaml:38`)

### 5.2 Why this split is useful

- Reduces single-session wall-clock time by dividing date intervals.
- Improves restartability (rerun only failed split).
- Keeps manifest/idempotent behavior centralized in one output tree.

### 5.3 Mapping to logs

- `glofas_optA` -> `optA_stdout.log` (`config/glofas_operational_medium_range_tmux_splits.yaml:22`)
- `glofas_optB` -> `optB_stdout.log` (`config/glofas_operational_medium_range_tmux_splits.yaml:30`)
- `glofas_optC` -> `optC_stdout.log` (`config/glofas_operational_medium_range_tmux_splits.yaml:36`)
- `glofas_optD` -> `optD_stdout.log` (`config/glofas_operational_medium_range_tmux_splits.yaml:42`)

Observed session evidence:
- planned counts and interval declarations (`data/glofas_operational_medium_range/logs/optA_stdout.log:1`, `data/glofas_operational_medium_range/logs/optA_stdout.log:5`)
- EWDS lifecycle `accepted -> running -> successful` (`data/glofas_operational_medium_range/logs/optB_stdout.log:12`, `data/glofas_operational_medium_range/logs/optB_stdout.log:13`, `data/glofas_operational_medium_range/logs/optB_stdout.log:14`)
- successful file saves (`data/glofas_operational_medium_range/logs/optC_stdout.log:15`)

## 6) Practical Runbook

### 6.1 Dry run (no download)

```bash
python3 glofas_operational_mediumrange_download_point.py --dry-run --verbose
```

### 6.2 Single interval run

```bash
cat > /tmp/glofas_interval_one.txt <<'TXT'
2022-12-01 2022-12-07
TXT

python3 glofas_operational_mediumrange_download_point.py \
  --run \
  --intervals-file /tmp/glofas_interval_one.txt \
  --verbose
```

### 6.3 CSV interval-file run

```bash
cat > /tmp/glofas_intervals.csv <<'CSV'
start,end
2021-03-19,2021-05-25
2022-07-15,2022-09-23
CSV

python3 glofas_operational_mediumrange_download_point.py \
  --run \
  --intervals-file /tmp/glofas_intervals.csv \
  --verbose
```

### 6.4 tmux split launch pattern

```bash
# Example: one split session (repeat for optB/optC/optD with own interval file)
tmux new -d -s glofas_optA \
  "cd /data/muscat_data/jaguir26/project1_ucsc_phd && \
   python3 glofas_operational_mediumrange_download_point.py --run --intervals-file /tmp/optA_intervals.txt --verbose \
   |& tee data/glofas_operational_medium_range/logs/optA_stdout.log"
```

### 6.5 Verification commands

```bash
# 1) Manifest status summary
python3 - <<'PY'
import csv, collections
p='data/glofas_operational_medium_range/manifests/download_manifest.csv'
c=collections.Counter()
with open(p,newline='') as f:
    for r in csv.DictReader(f): c[r['status']]+=1
print(c)
PY

# 2) Count issue_date partitions and gribs
find data/glofas_operational_medium_range/grib -maxdepth 1 -type d -name 'issue_date=*' | wc -l
find data/glofas_operational_medium_range/grib -type f -name '*.grib' | wc -l

# 3) Spot-check latest failures in log
rg -n "\[ERR\]|404|error" data/glofas_operational_medium_range/logs/download.log | tail -n 20
```

## 7) Implementation Details and Caveats

1. Hydrological model switch is hard-coded at `2021-05-26` (`glofas_operational_mediumrange_download_point.py:85`).
2. Bbox geometry is asymmetric by design: latitude span is `4*buffer`, longitude span is `2*buffer` (`glofas_operational_mediumrange_download_point.py:90`).
3. Idempotency: existing non-empty files are skipped unless `--overwrite` (`glofas_operational_mediumrange_download_point.py:226`).
4. Endpoint pitfall: historical logs show CDS endpoint 404 failures before correct EWDS configuration (`data/glofas_operational_medium_range/logs/download.log:14`, `data/glofas_operational_medium_range/logs/download.log:63`).
5. Naming/docstring mismatch: module doc example names `...download_grib.py` while actual file is `glofas_operational_mediumrange_download_point.py` (`glofas_operational_mediumrange_download_point.py:13`).
6. `--dry-run` still appends manifest rows with status `planned` and note `dry-run` (`glofas_operational_mediumrange_download_point.py:244`).

## 8) Downstream Integration Notes

### 8.1 How `scripts/forecats_batch.R` consumes GRIB root

- Reads `inputs.glofas.grib_root` from config (`config/forecats_batch.site=11160500.default.yaml:80`).
- In `build-forecasts` mode, invokes extractor with that root (`scripts/forecats_batch.R:157`).
- Stores extracted GloFAS cache under `forecast_cache/glofas` (`scripts/forecats_batch.R:150`).

### 8.2 How `scripts/forecats_extract_glofas_batch.py` transforms GRIB to cache CSV

- Opens control (`cf`) and perturbed (`pf`) fields with cfgrib (`scripts/forecats_extract_glofas_batch.py:61`, `scripts/forecats_extract_glofas_batch.py:66`).
- Locks nearest target cell once and persists `cell.json` (`scripts/forecats_extract_glofas_batch.py:216`, `scripts/forecats_extract_glofas_batch.py:248`).
- Filters to forecast window `[cutoff+1, cutoff+post_days]` (`scripts/forecats_extract_glofas_batch.py:280`).
- Writes per-issue `glofas_members.csv` (`scripts/forecats_extract_glofas_batch.py:258`, `scripts/forecats_extract_glofas_batch.py:299`).

## 9) Troubleshooting

| Symptom | Likely Cause | First Checks | Action |
|---|---|---|---|
| `404 ... process not found` | Wrong endpoint/client config | `download.log` error lines (`data/glofas_operational_medium_range/logs/download.log:14`) | Ensure EWDS URL/token in runtime config and retry |
| Many `skipped_exists` rows | Re-run without overwrite | Manifest (`data/glofas_operational_medium_range/manifests/download_manifest.csv:12`) | Expected for idempotent reruns; use `--overwrite` only when needed |
| Missing forecast cache at render | build step not completed for some dates | `scripts/forecats_batch.R:418` | Run `--mode build-forecasts` for missing shard/date |
| Empty/missing GRIB for a date | Retrieval failure or absent issue_date partition | `download.log`, `manifest` status, grib path count | Retry targeted interval/date |
| Cell mismatch concerns | different nearest valid cell due to geometry/masks | `cell.json` written by extractor (`scripts/forecats_extract_glofas_batch.py:248`) | Compare stored cell metadata across runs |

## 10) Operator Checklist

1. Verify EWDS credentials are active in current environment.
2. Confirm target intervals and whether run is dry-run or full run.
3. Launch split sessions (or single run) and tee stdout to split logs.
4. Monitor `download.log` and split stdout for `accepted/running/successful` and `[ERR]`.
5. Summarize manifest statuses; investigate any `error_*` rows.
6. Run `forecats_batch.R --mode build-forecasts` to build cache.
7. Run `forecats_batch.R --mode render` and verify `forecats.png` outputs.
8. Archive command lines/config hash with each batch run.

## Quick Start

```bash
cd /data/muscat_data/jaguir26/project1_ucsc_phd

# 1) Audit current batch readiness
Rscript scripts/forecats_batch.R \
  --config config/forecats_batch.site=11160500.default.yaml \
  --mode audit

# 2) Build GloFAS + NWS forecast caches
Rscript scripts/forecats_batch.R \
  --config config/forecats_batch.site=11160500.default.yaml \
  --mode build-forecasts

# 3) Render per-cutoff figures
Rscript scripts/forecats_batch.R \
  --config config/forecats_batch.site=11160500.default.yaml \
  --mode render
```
