# Software Reproducibility Release Plan

Date: 2026-06-15

This document summarizes the implementation-side release plan for HE-5. The
full contract lives in:

- `repro/run/REVISION_SOFTWARE_REPRODUCIBILITY_CONTRACT_20260615.md`

## Decision

Use a layered reproducibility contract:

1. CRAN `exdqlm` version 1.1.0 is the public reusable estimation package, with
   the accompanying software paper at `https://arxiv.org/abs/2607.22760`.
2. GitHub `AntonioAPDL/san-lorenzo-exdqlm-reproducibility` is the public
   study-specific reproducibility bundle exported from the live workflow
   workspace.
3. The revised article repository is the publication freeze of figures, tables,
   generated TeX fragments, and compact provenance manifests.
4. A permanent workflow archive DOI will be minted after final revision freeze,
   not during active correction drafting.

## Why This Is The Correct Path

- The article repository is intentionally not a full compute environment. It
  carries the manuscript-facing artifacts and compact manifests.
- The live workflow repository is the source of model orchestration, runtime
  configuration, post-stage generation, and validator logic. The public
  reproducibility repository is the curated reviewer-facing export from
  model-ready staged inputs; it is not a raw climate-archive reconstruction
  repository.
- The old `repro/REPRODUCE_PAPER.md` is a legacy record and should not be used
  as the current reproduction contract.
- The package-level method is already public through CRAN and the accompanying
  arXiv software paper. The workflow-level DOI should freeze the final workflow
  state, not an intermediate patch set.

## Required Implementation State

- Manuscript `Code availability` text names the CRAN package, package DOI,
  arXiv software paper, and public reproducibility repository.
- Corrections HE-5 response mirrors the manuscript wording.
- Article repo contains:
  `artifacts/software_availability/software_availability_manifest.json`
- Workflow repo contains the public-repo exporter:
  `scripts/export_san_lorenzo_exdqlm_reproducibility.py`.
- Public reproducibility repo contains root reader-facing files:
  `README.md`, `CITATION.cff`, `LICENSE`, `Makefile`, and validation scripts.
- Public reproducibility repo contains model-ready staged inputs for the five
  forecast origins, compact climate-product versioning notes, and public
  metadata with low-level covariate-retrieval and forecast-covariate internals removed.
- Workflow repo contains final archive checklist:
  `docs/workflow_archive_readiness_20260615.md`.
- Workflow validators check the manifest and prose.
- Validation reports record current commit metadata at validation time.

## Archive-Readiness Patch

The current implementation deliberately stops at archive readiness rather than
claiming a final archive. The root citation metadata uses
`version: "pending-final-archive"` and does not contain a workflow DOI. The
release notes and checklist describe the exact remaining final-freeze steps:
confirm the license, create the release tag, archive the release, replace all
`pending` DOI fields, and rerun every validator and compile.

The root license is still an author decision. Do not infer a license from
dependencies or from the CRAN `exdqlm` package; confirm the intended workflow
repository license before minting the final archived release.

## Follow-Up After Final Article Freeze

After the final correction pass:

1. Push all three repositories.
2. Run publication freeze and cross-repo wiring validators.
3. Confirm article and corrections compile.
4. Create a workflow release.
5. Archive the workflow release.
6. Replace `pending` DOI fields in the manuscript, corrections response, and
   article-side manifest.
7. Re-run all validators and compiles.
