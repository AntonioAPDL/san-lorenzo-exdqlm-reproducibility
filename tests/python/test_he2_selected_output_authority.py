from __future__ import annotations

import shutil
import sys
import tempfile
import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts"))

from validate_he2_selected_output_authority import DEFAULT_AUTHORITY, validate_authority  # noqa: E402


class HE2SelectedOutputAuthorityTests(unittest.TestCase):
    def test_current_authority_matches_winner_manifest_and_article_bundle(self) -> None:
        rows = validate_authority(DEFAULT_AUTHORITY, ROOT / "Evironmetrics---REVISED-DOC-Corrected-2")
        failures = [row for row in rows if row["status"] != "PASS"]
        self.assertEqual(failures, [])

    def test_run_id_drift_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as td:
            tmp = Path(td) / "authority.yaml"
            shutil.copy2(DEFAULT_AUTHORITY, tmp)
            payload = yaml.safe_load(tmp.read_text(encoding="utf-8"))
            payload["authority"]["run_id"] = "wrong_run"
            tmp.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")

            rows = validate_authority(tmp, article_root=None, require_article_bundle=False)
            failures = {row["check"]: row for row in rows if row["status"] != "PASS"}
            self.assertIn("run_id_matches", failures)


if __name__ == "__main__":
    unittest.main()
