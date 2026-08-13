# Project1 Forecasting Workflow

This repository contains the study-specific workflow for the revised article
``Bayesian Quantile-Based Correction and Synthesis of River Flow Forecasts``.
It is the public workflow layer that connects the reusable `exdqlm` estimation
package to the Santa Cruz River forecasting analysis, generated tables, figures,
and cross-repository validation checks.

## Reproducibility Contract

The current revision uses a three-layer software and reproducibility contract:

1. Reusable estimation routines are provided by the CRAN R package `exdqlm`
   version 1.1.0: `https://CRAN.R-project.org/package=exdqlm`. The package
   methods and software interface are described in the accompanying arXiv paper:
   `https://arxiv.org/abs/2607.22760`.
2. This repository, `https://github.com/AntonioAPDL/Project1`, contains the
   study-specific workflow, orchestration, post-processing, and validators.
3. The revised article repository freezes manuscript-facing figures, tables,
   generated TeX fragments, and compact provenance manifests.

The permanent archive DOI for this workflow is pending final revision freeze.
The archive should be minted only after the article, corrections response,
generated assets, and validation gates are final.

Canonical documentation:

- `repro/run/REVISION_SOFTWARE_REPRODUCIBILITY_CONTRACT_20260615.md`
- `docs/current_authority_refresh_runbook.md`
- `docs/software_reproducibility_release_plan_20260615.md`
- `docs/workflow_archive_readiness_20260615.md`
- `repro/run/CANONICAL_REVISED_ARTICLE_WORKFLOW.md`
- `repro/ENV_LOCK_STRATEGY.md`

## Data And Artifact Scope

Large runtime outputs, raw forecast archives, `.RData` objects, generated
reports, and basin-specific local data products are intentionally not tracked in
this repository. Applying the workflow to another basin requires staging the
corresponding observations, current forecast products, and covariates.
Reproducing the retrospective validation design additionally requires
constructing a basin-specific, version-consistent archive that aligns
historical observations, retrospective products, issued forecast products,
forecast-window covariates, product versions, spatial extraction rules, and
source-specific forecast horizons.

Run-scoped environment captures are the active reproducibility mechanism until
the planned formal `renv` migration is complete.

## Validation

Core cross-repository gates from the workflow repository are:

```bash
scripts/validate_current_authority_sync.sh
python3 scripts/validate_publication_freeze.py
python3 scripts/validate_revision_cross_repo_wiring.py --after-patch
```

Use `scripts/validate_current_authority_sync.sh --compile` before committing a
deliberate authority refresh that changes rendered PDFs. The revised article
and corrections response have their own compile/test gates; see the current
authority refresh runbook and revision software reproducibility contract for
the full checklist.

## Legacy Figure Helper

The old headless figure script is retained for compatibility only. For current
revision assets, use the canonical workflow runbook and article-side refresh
helpers instead of direct notebook or one-off copying.

```bash
Rscript scripts/check_inputs.R
scripts/run_environmetrics_figures_bg.sh
tail -f repro/logs/script_runs/<RUN_ID>/run_log.txt
```
