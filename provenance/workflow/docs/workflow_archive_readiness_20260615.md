# Workflow Archive Readiness Checklist

Date: 2026-06-15

## Purpose

This checklist records what must be true before the public
`san-lorenzo-exdqlm-reproducibility` repository is converted from the current
review-stage export into a permanent archived release. It supports the HE-5
software-availability response without claiming that a final archive already
exists.

## Current State

- CRAN `exdqlm` package: public.
- Public study workflow repository: `https://github.com/AntonioAPDL/san-lorenzo-exdqlm-reproducibility`.
- Revised article repository: manuscript-facing freeze of figures, tables,
  generated TeX fragments, and compact provenance manifests.
- Permanent workflow archive DOI: pending final revision freeze.

## Readiness Items

- [x] Public workflow repository is identified in the manuscript and response.
- [x] CRAN package URL, CRAN package DOI, and arXiv software paper are
  identified in the manuscript and response.
- [x] Article-side software availability manifest records the package,
  workflow repository, article repository, corrections repository, and archive
  status.
- [x] Workflow-side validators check the article text, corrections text, and
  software availability manifest together.
- [x] Root `README.md` explains the public workflow scope and exclusions.
- [x] Root `CITATION.cff` exists without a workflow DOI until final archive.
- [x] Pending-final-archive release notes exist.
- [ ] Workflow repository license is confirmed by the authors.
- [ ] Final workflow release tag is created.
- [ ] Final workflow release is archived with a permanent DOI.
- [ ] Manuscript, response, article manifest, and citation metadata are updated
  with the final archive DOI.
- [ ] Validators and compiles are rerun after the DOI update.

## Non-Negotiable Release Gates

The final archive must not include local-only runtime payloads by accident. In
particular, do not add `.RData`, `.rda`, `.rds`, large generated `reports/`
payloads, raw forecast archives, or machine-specific logs unless a specific
artifact has been reviewed and explicitly selected for publication.

The final archive must not be minted from a dirty working tree. Validation
reports should record current commit metadata at validation time rather than
storing self-referential commit hashes in tracked manifests.

## Final DOI Update Procedure

After the final archive service returns a DOI:

1. change the software availability manifest archive status from
   `pending_final_release` to `archived_final_release`;
2. replace `workflow_archive_doi`, `workflow_archive_service`, and
   `workflow_archive_release_tag` with final values;
3. update the manuscript Code availability section;
4. update the corrections HE-5 response;
5. update `CITATION.cff` if the final workflow DOI should be exposed there;
6. rerun all workflow, article, and corrections validation gates.
