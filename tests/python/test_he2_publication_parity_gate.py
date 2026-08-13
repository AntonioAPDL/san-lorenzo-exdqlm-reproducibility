#!/usr/bin/env python3
from __future__ import annotations

import os
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
os.sys.path.insert(0, str(ROOT / "scripts"))

from build_he2_bayesian_publication_manifest import ALLOWED_EXAL_KEEP_REPLACEMENT_LINEAGE_PREFIXES  # noqa: E402
from build_he2_publication_parity_gate import CANONICAL_LINEAGE, PROMOTED_LABEL, PROMOTED_LABELS, build_gate  # noqa: E402
from he2_exdqlm_keep_authoritative import load_authoritative_spec  # noqa: E402


class He2PublicationParityGateTests(unittest.TestCase):
    def test_gate_exposes_all_nine_promoted_families(self) -> None:
        rows, summary = build_gate()
        self.assertEqual(len(rows), 45)
        self.assertEqual(summary["promoted_rows"], 45)
        self.assertEqual(summary["pending_rows"], 0)
        self.assertEqual(summary["blocked_rows"], 0)
        self.assertEqual(set(summary["promoted_labels"]), PROMOTED_LABELS)
        self.assertEqual(summary["remaining_model_families_pending"], 0)
        self.assertEqual(summary["remaining_submodels_pending"], 0)
        self.assertTrue(summary["final_9_model_benchmark_ready"])
        self.assertEqual(summary["pending_labels"], [])

    def test_promoted_exal_keep_rows_match_authority_or_allowed_replacement(self) -> None:
        rows, _summary = build_gate()
        authoritative = load_authoritative_spec()
        by_cutoff = authoritative.winner_by_cutoff()
        promoted = [row for row in rows if row["manuscript_label"] == PROMOTED_LABEL]
        self.assertEqual(len(promoted), 5)
        for row in promoted:
            winner = by_cutoff[row["cutoff"]]
            lineage = row["current_campaign_lineage"]
            if lineage == CANONICAL_LINEAGE:
                self.assertEqual(row["current_run_id"], winner.run_id)
            else:
                self.assertTrue(
                    any(lineage.startswith(prefix) for prefix in ALLOWED_EXAL_KEEP_REPLACEMENT_LINEAGE_PREFIXES),
                    msg=f"unexpected replacement lineage for {row['cutoff']}: {lineage}",
                )
                self.assertNotEqual(row["current_run_id"], "")
            self.assertEqual(row["target_status"], "authoritative_promoted")
            self.assertEqual(row["required_action"], "none")
            self.assertEqual(row["current_score_scale"], "log_cms_plus1")
            self.assertEqual(row["current_within_cutoff_shared_inputs_aligned"], "True")

    def test_no_rows_remain_pending_after_full_promotion(self) -> None:
        rows, summary = build_gate()
        pending = [row for row in rows if row["target_status"] != "authoritative_promoted"]
        self.assertEqual(len(pending), 0)
        self.assertEqual(sorted({row["manuscript_label"] for row in pending}), summary["pending_labels"])
        for row in rows:
            self.assertIn(row["manuscript_label"], PROMOTED_LABELS)
            self.assertEqual(row["paper_table_gate"], "ready_final_9_model_table")
            self.assertEqual(row["target_status"], "authoritative_promoted")
            self.assertEqual(row["required_action"], "none")


if __name__ == "__main__":
    unittest.main()
