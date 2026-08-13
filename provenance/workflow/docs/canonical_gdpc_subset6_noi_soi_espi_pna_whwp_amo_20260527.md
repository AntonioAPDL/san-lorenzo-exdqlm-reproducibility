# Canonical GDPC Subset-6 Covariate

Date: 2026-05-27

## Purpose

Build an isolated candidate replacement for the current 17-index `GDPC1` climate covariate, using the same canonical GDPC pipeline and fit contract but only the following six indices:

- `NOI`
- `SOI`
- `ESPI`
- `PNA`
- `WHWP`
- `AMO`

This artifact is **not** wired into any active input bundle or model run. It is stored separately so it can be reviewed first and optionally substituted into future bundles later.

## Reproducibility

Config:

```text
config/canonical_gdpc_subset6_noi_soi_espi_pna_whwp_amo_covariate.yaml
```

Command:

```bash
python3 scripts/run_canonical_gdpc_master_pipeline.py \
  --config config/canonical_gdpc_subset6_noi_soi_espi_pna_whwp_amo_covariate.yaml
```

The subset config keeps the same canonical daily window and GDPC settings as the 17-index covariate:

- canonical daily window: `1987-05-29` to `2023-01-22`
- monthly source window: `1987-01-01` to `2023-01-01`
- interpolation: cubic spline with 30-day linear tail
- standardization: z-score, `ddof=1`
- GDPC lag: `k=2`
- tolerance: `1e-3`
- maximum iterations: `200`
- criterion label: `BIC`
- required convergence: `true`

One intentional difference is the sign anchor. The 17-index build uses `ONI`, which is not part of this six-index subset. This subset uses `SOI` only to orient the arbitrary factor sign via positive correlation.

## Output Root

```text
data/canonical_gdpc_subset6_noi_soi_espi_pna_whwp_amo/v20260527
```

Key outputs:

```text
data/canonical_gdpc_subset6_noi_soi_espi_pna_whwp_amo/v20260527/outputs/gdpc_master_component_01_19870529_20230122.csv
data/canonical_gdpc_subset6_noi_soi_espi_pna_whwp_amo/v20260527/outputs/compat/cov_05_PCA.csv
data/canonical_gdpc_subset6_noi_soi_espi_pna_whwp_amo/v20260527/outputs/compat/cov_03_PCA.csv
data/canonical_gdpc_subset6_noi_soi_espi_pna_whwp_amo/v20260527/metadata/gdpc_build_metadata.json
data/canonical_gdpc_subset6_noi_soi_espi_pna_whwp_amo/v20260527/review/CANONICAL_GDPC_BUILD_REVIEW.md
data/canonical_gdpc_subset6_noi_soi_espi_pna_whwp_amo/v20260527/review/stationarity/CANONICAL_GDPC_STATIONARITY_AUDIT.md
data/canonical_gdpc_subset6_noi_soi_espi_pna_whwp_amo/v20260527/review/figures/all_indices_raw_daily_overview.png
```

The compatibility aliases intentionally use the workflow-facing `Static_PCA` column so they can later replace the existing `PCA`/`cov_05_PCA.csv` slot if we explicitly choose to do that.

## Fit Results

From `metadata/gdpc_build_metadata.json` and `review/CANONICAL_GDPC_BUILD_REVIEW.md`:

| field | value |
| --- | ---: |
| series count | 6 |
| rows | 13023 |
| converged | true |
| iterations used | 26 |
| elapsed seconds | 467.4831 |
| criterion value | 13242.9816 |
| explained variance | 0.5472 |
| reconstruction MSE | 0.4528 |
| factor mean | 0.0003 |
| factor sd | 0.9999 |
| factor min | -2.4832 |
| factor max | 2.2340 |
| sign anchor | SOI |
| anchor correlation after orientation | 0.7377 |
| sign flipped | false |

## Verification

Checks run after the pipeline completed:

| artifact | rows | columns | missing values |
| --- | ---: | --- | ---: |
| raw daily matrix | 13023 | `time, noi, soi, espi, pna, whwp, amo` | 0 |
| standardized daily matrix | 13023 | `time, noi, soi, espi, pna, whwp, amo` | 0 |
| GDPC factor | 13023 | `time, GDPC1` | 0 |
| `cov_05_PCA.csv` alias | 13023 | `time, Static_PCA` | 0 |
| alpha loadings | 6 | `series, alpha` | 0 |
| beta loadings | 6 | `series, lag_0, lag_1, lag_2` | 0 |
| initial factor | 2 | `initial_offset, value` | 0 |

The standardized source columns were verified to have mean `0` and standard deviation `1` to six decimal places.

Checksums:

| artifact | sha256 |
| --- | --- |
| subset GDPC factor | `d11610910fa729c10eea5e13df5219790ca8a5bcdf0778ba7035ac3738917de7` |
| subset `cov_05_PCA.csv` alias | `6727fb272db2399d66349dbe4fb4bfc16ee573240218f587f41588ca32f34eac` |
| subset `cov_03_PCA.csv` alias | `6727fb272db2399d66349dbe4fb4bfc16ee573240218f587f41588ca32f34eac` |
| active 17-index runtime `cov_05_PCA.csv` | `f67b2db6410eefb60df12e868ab216579c085e2b8fd2ccd694b86a24e9b7a2e2` |

The checksum difference confirms the active 17-index runtime covariate was not overwritten.

## Future Substitution Plan

If we later decide to test this subset factor in exDQLM input bundles, the safe path is:

1. Copy or symlink the subset alias `outputs/compat/cov_05_PCA.csv` into a new isolated shared-input bundle root.
2. Do not modify the current `multimodel_v8_he2_publication_shared_inputs_20260510` bundle in place.
3. Regenerate or patch model configs to point `PCA` to the new bundle-local alias.
4. Run a prelaunch validator that checks `PPT|SOIL|PCA` paths, date spans, hashes, and forecast-window coverage.
5. Launch a small smoke run before any full cutoff/spec grid.

No bundle substitution was performed in this build.
