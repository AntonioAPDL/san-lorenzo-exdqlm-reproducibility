# Revision Software Reproducibility Contract

Date: 2026-06-15

## Purpose

This document is the current HE-5 software and reproducibility contract for the
revised article. It separates three related but different objects:

1. reusable model-estimation software,
2. the study-specific forecasting workflow,
3. the manuscript-local figure/table freeze.

The revised article should not claim that the article repository alone can
rerun the full forecasting study. The article repository is a publication
freeze of figures, tables, generated TeX fragments, and provenance manifests.
The generation workflow and model orchestration remain in this repository.

## Public Software Layers

### 1. Reusable estimation package

- Package: `exdqlm`
- Public package page: `https://CRAN.R-project.org/package=exdqlm`
- Package DOI: `https://doi.org/10.32614/CRAN.package.exdqlm`
- Public source repository: `https://github.com/AntonioAPDL/exdqlm`
- Verified current CRAN version for this contract: `1.1.0`
- Verified CRAN publication date for this contract: `2026-07-09`
- Software paper: `https://arxiv.org/abs/2607.22760`
- Software paper DOI: `https://doi.org/10.48550/arXiv.2607.22760`

This package is the reusable estimation layer for exDQLM/exAL model fitting.
It is the right public citation for readers who want the general method rather
than the full Santa Cruz basin workflow.

### 2. Study-specific workflow

- Workflow repository: `https://github.com/AntonioAPDL/Project1`
- Local workflow repository: `/data/muscat_data/jaguir26/project1_ucsc_phd`
- Canonical runbook:
  `repro/run/CANONICAL_REVISED_ARTICLE_WORKFLOW.md`
- Environment strategy:
  `repro/ENV_LOCK_STRATEGY.md`
- Public workflow overview:
  `README.md`
- Citation metadata:
  `CITATION.cff`
- Pending-final-archive release notes:
  `RELEASE_NOTES_PENDING_FINAL_ARCHIVE.md`
- Final archive readiness checklist:
  `docs/workflow_archive_readiness_20260615.md`

The workflow repository contains the unified run entrypoint, model-family
configuration, post-processing, table generation, figure generation, and
cross-repo validators. It is the correct location for reproducible workflow
documentation and release archival metadata.

### 3. Revised article freeze

- Article repository:
  `/data/muscat_data/jaguir26/project1_ucsc_phd/Evironmetrics---REVISED-DOC-Corrected-2`
- Article-side manifest:
  `MANUSCRIPT_ASSET_MANIFEST.json`
- Software availability manifest:
  `artifacts/software_availability/software_availability_manifest.json`
- Article provenance document:
  `docs/figure_table_provenance.md`

The article repository intentionally carries only publication-facing assets and
compact support manifests. Large runtime objects, `.RData` files, and
intermediate support payloads stay outside the Overleaf-facing article repo.

## Current Archive Status

The current public availability status is:

- CRAN package: available now.
- Workflow GitHub repository: available now.
- Permanent workflow archive DOI: pending final revision freeze.

The final workflow DOI should be minted only after the revised article,
corrections response, generated tables, generated figures, and cross-repo
validators are final. Minting the DOI too early would permanently archive a
moving target.

## Final Release Checklist

Before final resubmission:

1. Confirm the workflow repository has no unintended local changes.
2. Confirm the revised article repository has no unintended local changes.
3. Confirm the corrections repository has no unintended local changes.
4. Run the workflow-side publication freeze validator.
5. Run the workflow-side cross-repo wiring validator.
6. Compile the revised article.
7. Compile the corrections response.
8. Confirm the workflow repository has an appropriate author-approved license.
9. Confirm root README, citation metadata, pending-final-archive release notes,
   and archive checklist are current.
10. Create a GitHub release for the workflow repository.
11. Archive that release with Zenodo, OSF, or an equivalent permanent archive.
12. Replace the manuscript and manifest `pending` archive DOI fields with the
    final DOI.
13. Re-run all validators and compiles after the DOI update.

## Validation Gates

The following checks must remain automated:

- The revised manuscript `Code availability` section must cite the CRAN package,
  the package DOI, and the workflow repository.
- The corrections HE-5 response must mirror the same public availability
  contract.
- The article-side software availability manifest must exist and must record
  the CRAN package, workflow repository, article repository, corrections
  repository, archive status, and final-DOI policy.
- While the workflow archive DOI is pending, neither the manuscript nor the
  corrections response may state that the workflow has already been archived.
- The workflow-side validators must emit current local commit metadata at
  validation time. Static manifests should not store their own repository's
  current commit SHA, because that SHA would become stale as soon as the manifest
  is committed.
- While the final workflow DOI is pending, root citation metadata and release
  notes must describe the archive as pending and must not include a workflow DOI.

## Canonical Commands

From the workflow repository:

```bash
python3 scripts/validate_publication_freeze.py
python3 scripts/validate_revision_cross_repo_wiring.py --after-patch
```

From the revised article repository:

```bash
python3 -m unittest discover -s tests
pdflatex -interaction=nonstopmode -halt-on-error -jobname=output wileyNJD-APA.tex
bibtex output
pdflatex -interaction=nonstopmode -halt-on-error -jobname=output wileyNJD-APA.tex
pdflatex -interaction=nonstopmode -halt-on-error -jobname=output wileyNJD-APA.tex
```

From the corrections repository:

```bash
make
```
