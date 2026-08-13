# HE-7 Latest-Forecast-Issue Contract

Status: active publication contract, June 15, 2026.

This document closes the Handling Editor HE-7 point about replacing the old
multi-issuance forecast combination with a latest-forecast-only protocol.

## Contract

For each rolling-origin cutoff \(c\), the publication workflow uses forecast
products that are available at or before \(c\), without averaging older
issuances into the publication forecast matrix.

- GloFAS: the forecast matrix is the daily issue associated with the cutoff,
  stored by the forecast cache as
  `forecast_cache/glofas/issue_date=<cutoff>/glofas_members.csv`.
- NWS/NWM: the forecast cache first restricts to issue datetimes available by
  the cutoff day, retains the most recent available issue for each
  `(target_date, target_hour, ensemble)` tuple, and then averages the retained
  hourly values to daily resolution.
- The legacy `nws_weighted_daily.csv` and `glofas_weighted_daily.csv` file names
  are workflow compatibility aliases. Under the publication protocol they refer
  to latest-issue member matrices and must not be interpreted as active
  cross-issue weighting.

## Current Code Evidence

- `scripts/forecats_extract_nws_batch.py` defaults to
  `--weighting-scheme latest` and, in latest mode, keeps the maximum
  `issue_dt` within each target/member/hour group before daily aggregation.
- `scripts/forecats_extract_glofas_batch.py` writes raw ensemble members under
  `issue_date=<cutoff>/glofas_members.csv`; it does not perform cross-issue
  weighting.
- `scripts/forecats_batch.R` passes the configured NWS weighting scheme, whose
  publication configs set `latest`, and copies cache outputs into legacy
  `*_weighted_daily.csv` names for downstream compatibility.
- `R/unified/stages/stage_forecats.R` and
  `scripts/build_multimodel_v8_histfix_bundles.py` preserve those legacy aliases
  while also carrying explicit member-level `nws_members.csv` and
  `glofas_members.csv` paths.
- Publication configs
  `config/forecats_batch.site=11160500.single_retro_policy_pre1080.yaml` and
  `config/forecats_batch.site=11160500.default.yaml` use
  `weighting.scheme: latest`.

## Validation

Tracked validation is split into two layers.

1. The article-side machine-readable manifest
   `artifacts/latest_forecast_issue/latest_forecast_issue_manifest.json` records
   the publication protocol, source-specific selection rules, legacy-alias
   policy, and code evidence.
2. `scripts/latest_forecast_issue_contract.py`,
   `tests/python/test_he7_latest_forecast_issue_contract.py`,
   `scripts/validate_publication_freeze.py`, and
   `scripts/validate_revision_cross_repo_wiring.py` enforce the same contract
   across the workflow repo, revised article, and corrections response.

When the frozen runtime bundles are mounted, the optional helper
`scripts/audit_latest_forecast_issue_runtime_bundles.py` verifies that the
legacy `*_weighted_daily.csv` aliases are byte-identical to the corresponding
member-level forecast matrices for each publication cutoff.

The latest local runtime alias audit was run as:

```bash
python3 scripts/audit_latest_forecast_issue_runtime_bundles.py \
  --output-dir reports/latest_forecast_issue_runtime_bundle_audit_20260615_final
```

That audit covered the five publication cutoffs and both forecast sources
(`5 x 2 = 10` alias checks) with zero failures. The report remains under
`reports/` because it is runtime evidence rather than a lightweight
manuscript-facing artifact; the tracked script and command above reproduce it
whenever the frozen shared-input bundles are mounted.

## Non-Claims

This contract does not relaunch any model, change any posterior output, or
claim that older forecast issuances were beneficial. It records the current
publication protocol and prevents future regressions where legacy file names
could be mistaken for active lagged-issuance weighting.
