# San Lorenzo exDQLM Reproducibility

This repository contains the clean reproducibility bundle for the
Environmetrics manuscript *Bayesian Quantile-Based Correction and
Synthesis of Hydrologic Products*.

The repository is intentionally narrower than the live research
workspace. It contains compact staged inputs, current manuscript-facing
outputs, provenance, and the selected workflow code needed to inspect
or rerun the reported case-study analysis. It does not contain raw
climate-center archives, active runtime campaigns, local notebooks, or
generated screening outputs.

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
   configuration files, and staged input bundles in `data/staged/` to
   rerun the reported exDQLM/DQLM case-study fits. This requires the
   public CRAN package `exdqlm` and sufficient local compute.
3. **Raw archive reconstruction.** Not bundled. Reconstructing a new
   retrospective validation archive requires agency-specific historical
   products, version matching, spatial extraction, forecast-window
   covariate staging, and source-specific horizon handling.

## Main Directories

- `R/`: selected model, post-processing, and figure-generation code.
- `scripts/`: orchestration, manifest, and validation scripts.
- `config/`: selected publication and authority configuration files.
- `data/staged/`: compact staged inputs used by the five cutoff cases.
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

## License Status

The final reuse license must be confirmed by the authors before
archival release. See `LICENSE` for the current review-stage notice.
