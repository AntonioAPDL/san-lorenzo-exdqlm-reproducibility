# San Lorenzo exDQLM Reproducibility

This repository contains the clean reproducibility bundle for the
Environmetrics manuscript *Bayesian Quantile-Based Correction and
Synthesis of Hydrologic Products*.

The repository is intentionally narrower than the live research
workspace. It contains compact staged inputs, current manuscript-facing
outputs, provenance, and the selected workflow code needed to inspect
or rerun the reported case-study analysis from model-ready inputs. It
does not contain raw climate-center archives, raw covariate-retrieval
workflows, active runtime campaigns, local notebooks, poster drafts, or
internal exploratory outputs.

## Quick Validation

```bash
make validate
```

This checks required files, manifests, hashes, file sizes, and the
absence of forbidden heavy runtime formats.

## Reproducibility Levels

1. **Fast artifact verification.** Regenerate or verify
   manuscript-facing tables, figures, hashes, and provenance from the
   compact staged outputs in `outputs/expected/`.
2. **Selected-model rerun support.** Use the selected R workflow,
   configuration files, and model-ready staged input bundles in
   `data/staged/` to rerun the reported exDQLM/DQLM case-study fits.
   This requires the public CRAN package `exdqlm` and sufficient local
   compute.
3. **Raw archive reconstruction.** Not bundled. Reconstructing a new
   retrospective validation archive requires agency-specific historical
   products, version matching, spatial extraction, forecast-window
   covariate staging, and source-specific horizon handling.

## Main Directories

- `R/`: selected model, post-processing, and figure-generation code.
- `scripts/`: orchestration, manifest, and validation scripts.
- `config/`: selected public model-specification files.
- `data/staged/`: compact model-ready inputs used by the five cutoff cases.
- `outputs/expected/`: current manuscript-facing expected outputs and
  compact artifact bundles.
- `figures/`: frozen manuscript figure files.
- `tables/`: generated manuscript table fragments and summaries.
- `provenance/`: source crosswalks, data/version notes, and hashes.
- `manuscript/`: article-side manifest and source pointers.

## Software

The reusable estimation routines are provided by the CRAN package
`exdqlm`, version 1.1.0:

- <https://CRAN.R-project.org/package=exdqlm>
- <https://doi.org/10.32614/CRAN.package.exdqlm>

The accompanying software paper is:

- <https://arxiv.org/abs/2607.22760>
- <https://doi.org/10.48550/arXiv.2607.22760>

## Repository URL

https://github.com/AntonioAPDL/san-lorenzo-exdqlm-reproducibility

## Source-Path Placeholders

Copied text artifacts replace local machine paths with stable
placeholders so the public tree can be read outside the original
workspace:

- `SOURCE_WORKFLOW_ROOT`: private live workflow repository used for export.
- `SOURCE_ARTICLE_ROOT`: private revised manuscript repository used for export.
- `SOURCE_CORRECTIONS_ROOT`: private response-letter repository used
  for cross-repo validation.
- `SOURCE_RUNTIME_ROOT`: private runtime root used for staged artifacts.
- `STAGED_INPUT_BUNDLE_ROOT`: private staged-input bundle used for export.
- `PUBLIC_REPRO_ROOT`: local checkout of this public reproducibility repo.
- `LEGACY_EXAL_INPUT_ROOT`: legacy local input root referenced by older
  workflow scripts.
- `LEGACY_PROJECT_ROOT`: legacy local project root referenced by older
  workflow scripts.
- `LOCAL_RCPP_LIB_ROOT`: local compiled-library root referenced by
  commented Rcpp setup notes.
- `EXTERNAL_RUNTIME_SOURCE_ROOT`: external local runtime source tree used
  in legacy provenance notes.

Export-time commits and source roles are preserved in
`provenance/source_file_crosswalk.csv` and
`provenance/runtime_source_crosswalk.csv`; machine-specific paths are
replaced by public placeholders.

## License Status

The final reuse license must be confirmed by the authors before
archival release. See `LICENSE` for the current review-stage notice.
