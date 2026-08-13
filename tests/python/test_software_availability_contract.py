#!/usr/bin/env python3
from __future__ import annotations

import json
import os
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
ARTICLE_ROOT = ROOT / "Evironmetrics---REVISED-DOC-Corrected-2"
CORRECTIONS_ROOT = Path("/data/muscat_data/jaguir26/Corrections---Project-1")
os.sys.path.insert(0, str(ROOT / "scripts"))

from software_availability_contract import (  # noqa: E402
    ARTICLE_SOFTWARE_DOC_REL,
    CRAN_EXDQLM_DOI_URL,
    CRAN_EXDQLM_PUBLICATION_DATE,
    CRAN_EXDQLM_URL,
    CRAN_EXDQLM_VERSION,
    EXDQLM_SOFTWARE_PAPER_BIBTEX_KEY,
    EXDQLM_SOFTWARE_PAPER_DOI_URL,
    EXDQLM_SOFTWARE_PAPER_URL,
    PROJECT1_URL,
    SOFTWARE_CONTRACT_REL,
    SOFTWARE_MANIFEST_REL,
    WORKFLOW_ARCHIVE_READINESS_REL,
    WORKFLOW_CITATION_REL,
    WORKFLOW_README_REL,
    WORKFLOW_RELEASE_NOTES_REL,
    WORKFLOW_RELEASE_READINESS_RELS,
    check_archive_status,
)


class SoftwareAvailabilityContractTests(unittest.TestCase):
    def test_article_software_manifest_records_public_contract(self) -> None:
        manifest_path = ARTICLE_ROOT / SOFTWARE_MANIFEST_REL
        self.assertTrue(manifest_path.exists(), f"missing software manifest: {manifest_path}")
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        self.assertEqual(manifest["schema_version"], "revision_software_availability_v1")
        self.assertEqual(manifest["public_estimation_package"]["cran_package_url"], CRAN_EXDQLM_URL)
        self.assertEqual(manifest["public_estimation_package"]["package_doi"], CRAN_EXDQLM_DOI_URL)
        self.assertEqual(manifest["public_estimation_package"]["cran_version_verified_for_contract"], CRAN_EXDQLM_VERSION)
        self.assertEqual(manifest["public_estimation_package"]["cran_publication_date_verified_for_contract"], CRAN_EXDQLM_PUBLICATION_DATE)
        self.assertEqual(manifest["public_estimation_package"]["software_paper_arxiv_url"], EXDQLM_SOFTWARE_PAPER_URL)
        self.assertEqual(manifest["public_estimation_package"]["software_paper_arxiv_doi"], EXDQLM_SOFTWARE_PAPER_DOI_URL)
        self.assertEqual(manifest["public_estimation_package"]["software_paper_bibtex_key"], EXDQLM_SOFTWARE_PAPER_BIBTEX_KEY)
        self.assertEqual(manifest["study_workflow_repository"]["public_url"], PROJECT1_URL)
        self.assertEqual(
            manifest["study_workflow_repository"]["release_readiness_files"],
            {
                "readme": WORKFLOW_README_REL,
                "citation": WORKFLOW_CITATION_REL,
                "pending_release_notes": WORKFLOW_RELEASE_NOTES_REL,
                "archive_readiness_checklist": WORKFLOW_ARCHIVE_READINESS_REL,
            },
        )
        self.assertEqual(manifest["archive_status"]["workflow_archive_status"], "pending_final_release")
        self.assertEqual(manifest["archive_status"]["workflow_archive_doi"], "pending")
        self.assertIn("reason_static_commits_are_not_recorded", manifest["validation_policy"])
        archive_check = check_archive_status(manifest["archive_status"])
        self.assertTrue(archive_check.ok)
        self.assertTrue(archive_check.is_pending)

    def test_contract_docs_exist_in_correct_repositories(self) -> None:
        self.assertTrue((ROOT / SOFTWARE_CONTRACT_REL).exists())
        self.assertTrue((ROOT / "docs" / "software_reproducibility_release_plan_20260615.md").exists())
        self.assertTrue((ARTICLE_ROOT / ARTICLE_SOFTWARE_DOC_REL).exists())
        for rel in WORKFLOW_RELEASE_READINESS_RELS:
            self.assertTrue((ROOT / rel).exists(), f"missing workflow release-readiness file: {rel}")

    def test_workflow_archive_readiness_files_do_not_overclaim(self) -> None:
        texts = {rel: (ROOT / rel).read_text(encoding="utf-8") for rel in WORKFLOW_RELEASE_READINESS_RELS}
        self.assertIn(PROJECT1_URL, texts[WORKFLOW_README_REL])
        self.assertIn(CRAN_EXDQLM_URL, texts[WORKFLOW_README_REL])
        self.assertIn("pending final revision freeze", texts[WORKFLOW_README_REL])
        self.assertIn(SOFTWARE_CONTRACT_REL, texts[WORKFLOW_README_REL])
        self.assertIn("version: \"pending-final-archive\"", texts[WORKFLOW_CITATION_REL])
        self.assertNotIn("\ndoi:", texts[WORKFLOW_CITATION_REL])
        self.assertIn("pending final revision freeze", texts[WORKFLOW_RELEASE_NOTES_REL])
        self.assertIn("Workflow repository license is confirmed by the authors", texts[WORKFLOW_ARCHIVE_READINESS_REL])
        for rel, text in texts.items():
            self.assertNotIn("workflow repository has been archived", text, rel)
            self.assertNotIn("workflow has been archived", text, rel)
            self.assertNotIn("archived workflow DOI", text, rel)

    def test_manuscript_and_corrections_share_he5_contract(self) -> None:
        article = (ARTICLE_ROOT / "wileyNJD-APA.tex").read_text(encoding="utf-8")
        corrections = (CORRECTIONS_ROOT / "main.tex").read_text(encoding="utf-8")
        for text in [article, corrections]:
            self.assertIn(r"CRAN R package \texttt{exdqlm}", text)
            self.assertIn(f"version {CRAN_EXDQLM_VERSION}", text)
            self.assertIn(CRAN_EXDQLM_URL, text)
            self.assertIn(CRAN_EXDQLM_DOI_URL, text)
            self.assertIn(PROJECT1_URL, text)
            self.assertIn("compact provenance manifests", text)
            self.assertNotIn("workflow repository has been archived", text)
            self.assertNotIn("archived workflow DOI", text)
        self.assertIn(EXDQLM_SOFTWARE_PAPER_BIBTEX_KEY, article)
        self.assertIn(EXDQLM_SOFTWARE_PAPER_DOI_URL, corrections)
        self.assertIn("permanent archival release of the workflow repository will be created", article)
        self.assertIn("Before final resubmission", corrections)

    def test_canonical_runbook_keeps_selected_diagnostics_tied_to_selected_output(self) -> None:
        runbook = (ROOT / "repro" / "run" / "CANONICAL_REVISED_ARTICLE_WORKFLOW.md").read_text(encoding="utf-8")
        self.assertIn("### B. Selected-output support assets", runbook)
        self.assertIn("artifacts/representative_selected_model_2022_12_25/", runbook)
        self.assertIn("same posterior-output authority as the selected synthesis figures", runbook)
        self.assertIn("### C. Historical-only appendix reference assets", runbook)
        self.assertNotIn("### B. Historical-summary support assets", runbook)

    def test_archive_status_helper_accepts_only_coherent_states(self) -> None:
        pending = {
            "workflow_archive_status": "pending_final_release",
            "workflow_archive_doi": "pending",
            "workflow_archive_service": "pending",
            "workflow_archive_release_tag": "pending",
        }
        final = {
            "workflow_archive_status": "archived_final_release",
            "workflow_archive_doi": "https://doi.org/10.5281/zenodo.1234567",
            "workflow_archive_service": "Zenodo",
            "workflow_archive_release_tag": "v2026.06.15",
        }
        mixed = {
            "workflow_archive_status": "archived_final_release",
            "workflow_archive_doi": "pending",
            "workflow_archive_service": "Zenodo",
            "workflow_archive_release_tag": "v2026.06.15",
        }
        self.assertTrue(check_archive_status(pending).is_pending)
        self.assertTrue(check_archive_status(final).is_final)
        self.assertFalse(check_archive_status(mixed).ok)


if __name__ == "__main__":
    unittest.main()
