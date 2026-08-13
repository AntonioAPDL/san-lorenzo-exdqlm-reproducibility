#!/usr/bin/env python3
from __future__ import annotations

import os
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
os.sys.path.insert(0, str(ROOT / "scripts"))

from build_he2_bayesian_publication_manifest import (  # noqa: E402
    ARTIFACT_SPECS,
    ALLOWED_EXAL_KEEP_REPLACEMENT_LINEAGE_PREFIXES,
    CUTOFFS,
    FAMILY_TO_LABEL,
    PROMOTED_AL_DROP_ROOT,
    PROMOTED_AL_KEEP_ROOT,
    PROMOTED_EXAL_DROP_ROOT,
    PROMOTED_NDLM_ROOT,
    PROMOTED_UNIVAR_AL_EXAL_ROOT,
    PROMOTED_FAMILY_LINEAGES,
    REQUIRED_ALIGNMENT_ARTIFACTS,
    apply_replacement_overlay,
    build_outputs,
    load_replacement_overlay,
)
from he2_exdqlm_keep_authoritative import load_authoritative_spec  # noqa: E402


def active_overlay_by_key() -> tuple[dict[tuple[str, str], dict[str, object]], dict[str, object]]:
    overlay = load_replacement_overlay()
    replacements = {
        (str(row["cutoff"]), str(row["family"])): row
        for row in overlay.get("replacements", [])
    }
    return replacements, overlay


def assert_promoted_or_repaired(
    test: unittest.TestCase,
    row: dict[str, str],
    *,
    expected_run_id: str,
    expected_root: Path,
    expected_lineage: str,
    overlay_by_key: dict[tuple[str, str], dict[str, object]],
    overlay: dict[str, object],
) -> None:
    key = (row["cutoff"], row["family"])
    replacement = overlay_by_key.get(key)
    if replacement is None:
        test.assertEqual(row["run_id"], expected_run_id)
        test.assertTrue(row["run_root"].startswith(str(expected_root)))
        test.assertEqual(row["campaign_lineage"], expected_lineage)
        return

    test.assertEqual(row["run_id"], replacement["run_id"])
    expected_replacement_root = replacement.get("run_root") or f'{overlay["artifact_root"]}/runs/{replacement["run_id"]}'
    test.assertEqual(row["run_root"], expected_replacement_root)
    test.assertEqual(row["campaign_lineage"], replacement.get("campaign_lineage", overlay["campaign_lineage"]))
    test.assertEqual(row["replacement_reason"], replacement.get("replacement_reason", overlay["replacement_reason"]))
    test.assertEqual(row["expected_input_bundle_id"], replacement.get("expected_input_bundle_id", overlay["expected_input_bundle_id"]))
    test.assertEqual(row["replaced_source_run_id"], expected_run_id)


class He2BayesianPublicationManifestTests(unittest.TestCase):
    def test_build_outputs_resolves_full_publication_matrix(self) -> None:
        manifest_rows, input_rows, alignment_rows = build_outputs()
        self.assertEqual(len(manifest_rows), 45)
        self.assertEqual(len(input_rows), 45 * len(ARTIFACT_SPECS))
        self.assertEqual(len(alignment_rows), len(CUTOFFS) * len(ARTIFACT_SPECS))
        self.assertEqual(sorted({row["manuscript_label"] for row in manifest_rows}), sorted(FAMILY_TO_LABEL.values()))

    def test_exal_keep_rows_point_to_authoritative_grid_winners(self) -> None:
        manifest_rows, _input_rows, _alignment_rows = build_outputs()
        authoritative = load_authoritative_spec()
        overlay_by_key, overlay = active_overlay_by_key()
        for winner in authoritative.winners:
            row = next(
                row
                for row in manifest_rows
                if row["cutoff"] == winner.cutoff and row["manuscript_label"] == "exAL-M-T1"
            )
            assert_promoted_or_repaired(
                self,
                row,
                expected_run_id=winner.run_id,
                expected_root=authoritative.runtime_root,
                expected_lineage="exdqlm_multivar_keep_canonical_grid_20260524:authoritative_winner",
                overlay_by_key=overlay_by_key,
                overlay=overlay,
            )
            replacement = overlay_by_key.get((winner.cutoff, "exdqlm_multivar_keep"))
            if replacement is None:
                self.assertAlmostEqual(float(row["crps_exact"]), winner.mean_crps, places=12)
            else:
                self.assertLess(float(row["crps_exact"]), winner.mean_crps)
                self.assertTrue(
                    any(
                        row["campaign_lineage"].startswith(prefix)
                        for prefix in ALLOWED_EXAL_KEEP_REPLACEMENT_LINEAGE_PREFIXES
                    )
                )

    def test_multivar_quantile_rows_point_to_canonical_promoted_roots(self) -> None:
        manifest_rows, _input_rows, _alignment_rows = build_outputs()
        authoritative = load_authoritative_spec()
        winners = authoritative.winner_by_cutoff()
        overlay_by_key, overlay = active_overlay_by_key()
        for cutoff in CUTOFFS:
            winner = winners[cutoff]
            al_keep = next(row for row in manifest_rows if row["cutoff"] == cutoff and row["manuscript_label"] == "AL-M-T1")
            assert_promoted_or_repaired(
                self,
                al_keep,
                expected_run_id=f"multimodel_{cutoff}_v8_he2grid_{winner.grid_spec_id}_dqlm_multivar_al_keep",
                expected_root=PROMOTED_AL_KEEP_ROOT,
                expected_lineage=PROMOTED_FAMILY_LINEAGES["dqlm_multivar_al_keep"],
                overlay_by_key=overlay_by_key,
                overlay=overlay,
            )
            self.assertEqual(al_keep["likelihood_mode"], "al")
            self.assertEqual(al_keep["forecast_transfer_mode"], "keep")
            self.assertEqual(al_keep["reused_external_pass"], "False")

            exal_drop = next(row for row in manifest_rows if row["cutoff"] == cutoff and row["manuscript_label"] == "exAL-M-T0")
            assert_promoted_or_repaired(
                self,
                exal_drop,
                expected_run_id=f"multimodel_{cutoff}_v8_he2pubgdpc1r1_exdqlm_multivar_drop",
                expected_root=PROMOTED_EXAL_DROP_ROOT,
                expected_lineage=PROMOTED_FAMILY_LINEAGES["exdqlm_multivar_drop"],
                overlay_by_key=overlay_by_key,
                overlay=overlay,
            )
            self.assertEqual(exal_drop["likelihood_mode"], "exal")
            self.assertEqual(exal_drop["forecast_transfer_mode"], "drop")
            self.assertEqual(exal_drop["reused_external_pass"], "False")

            al_drop = next(row for row in manifest_rows if row["cutoff"] == cutoff and row["manuscript_label"] == "AL-M-T0")
            assert_promoted_or_repaired(
                self,
                al_drop,
                expected_run_id=f"multimodel_{cutoff}_v8_he2pubgdpc1r1_dqlm_multivar_al_drop",
                expected_root=PROMOTED_AL_DROP_ROOT,
                expected_lineage=PROMOTED_FAMILY_LINEAGES["dqlm_multivar_al_drop"],
                overlay_by_key=overlay_by_key,
                overlay=overlay,
            )
            self.assertEqual(al_drop["likelihood_mode"], "al")
            self.assertEqual(al_drop["forecast_transfer_mode"], "drop")
            self.assertEqual(al_drop["reused_external_pass"], "False")

    def test_univar_al_exal_rows_point_to_20260603_promoted_root(self) -> None:
        manifest_rows, _input_rows, _alignment_rows = build_outputs()
        overlay_by_key, overlay = active_overlay_by_key()
        for cutoff in CUTOFFS:
            al = next(row for row in manifest_rows if row["cutoff"] == cutoff and row["manuscript_label"] == "AL-U-T1")
            assert_promoted_or_repaired(
                self,
                al,
                expected_run_id=f"multimodel_{cutoff}_v8_he2pubgdpc1r1_dqlm_univar_al",
                expected_root=PROMOTED_UNIVAR_AL_EXAL_ROOT,
                expected_lineage=PROMOTED_FAMILY_LINEAGES["dqlm_univar_al"],
                overlay_by_key=overlay_by_key,
                overlay=overlay,
            )
            self.assertEqual(al["likelihood_mode"], "al")
            self.assertEqual(al["reused_external_pass"], "False")

            exal = next(row for row in manifest_rows if row["cutoff"] == cutoff and row["manuscript_label"] == "exAL-U-T1")
            assert_promoted_or_repaired(
                self,
                exal,
                expected_run_id=f"multimodel_{cutoff}_v8_he2pubgdpc1r1_exdqlm_univar",
                expected_root=PROMOTED_UNIVAR_AL_EXAL_ROOT,
                expected_lineage=PROMOTED_FAMILY_LINEAGES["exdqlm_univar"],
                overlay_by_key=overlay_by_key,
                overlay=overlay,
            )
            self.assertEqual(exal["likelihood_mode"], "exal")
            self.assertEqual(exal["reused_external_pass"], "False")

    def test_ndlm_rows_point_to_20260607_promoted_root(self) -> None:
        manifest_rows, _input_rows, _alignment_rows = build_outputs()
        overlay_by_key, overlay = active_overlay_by_key()
        expected = {
            "N-U-T1": ("ndlm_univar_keep", "keep"),
            "N-M-T0": ("ndlm_main_drop", "drop"),
            "N-M-T1": ("ndlm_main_keep", "keep"),
        }
        for cutoff in CUTOFFS:
            for label, (family, transfer_mode) in expected.items():
                row = next(row for row in manifest_rows if row["cutoff"] == cutoff and row["manuscript_label"] == label)
                assert_promoted_or_repaired(
                    self,
                    row,
                    expected_run_id=f"multimodel_{cutoff}_v8_he2pubgdpc1r1_{family}",
                    expected_root=PROMOTED_NDLM_ROOT,
                    expected_lineage=PROMOTED_FAMILY_LINEAGES[family],
                    overlay_by_key=overlay_by_key,
                    overlay=overlay,
                )
                self.assertEqual(row["likelihood_mode"], "normal")
                self.assertEqual(row["forecast_transfer_mode"], transfer_mode)
                self.assertEqual(row["reused_external_pass"], "False")
                if (cutoff, family) not in overlay_by_key:
                    self.assertIn("20260607", row["campaign_lineage"])

    def test_all_rows_share_current_featurecov_contract(self) -> None:
        manifest_rows, _input_rows, alignment_rows = build_outputs()
        for row in manifest_rows:
            self.assertEqual(row["fit_covariate_names"], "PPT|SOIL|PCA")
            self.assertEqual(row["deterministic_climate_enabled"], "True")
            self.assertEqual(row["covariate_features_enabled"], "True")
            self.assertEqual(row["lag_orders"], "1|2|3")
            self.assertEqual(row["include_squares"], "True")
            self.assertEqual(row["include_interaction"], "True")
        required = [row for row in alignment_rows if row["artifact"] in REQUIRED_ALIGNMENT_ARTIFACTS]
        self.assertEqual(sum(row["all_equal"] == "True" for row in required), len(CUTOFFS) * len(REQUIRED_ALIGNMENT_ARTIFACTS))
        self.assertEqual(len(required), len(CUTOFFS) * len(REQUIRED_ALIGNMENT_ARTIFACTS))
        self.assertTrue(all(row["within_cutoff_shared_inputs_aligned"] == "True" for row in manifest_rows))

    def test_replacement_overlay_replaces_only_exact_target_cells(self) -> None:
        base = [
            {
                "cutoff": "20220511",
                "family": "dqlm_multivar_al_drop",
                "run_id": "old_al_drop",
                "run_root": "/old/al_drop",
                "compare_dir": "",
                "campaign_lineage": PROMOTED_FAMILY_LINEAGES["dqlm_multivar_al_drop"],
                "publication_note": "old",
                "replaced_source_run_id": "",
            },
            {
                "cutoff": "20220511",
                "family": "exdqlm_multivar_keep",
                "run_id": "keep_winner",
                "run_root": "/old/keep",
                "compare_dir": "",
                "campaign_lineage": PROMOTED_FAMILY_LINEAGES["exdqlm_multivar_keep"],
                "publication_note": "keep",
                "replaced_source_run_id": "",
            },
        ]
        overlay = {
            "active": True,
            "artifact_root": "/tmp/he2_table1_overlay_test",
            "expected_input_bundle_id": "20260510_publication_shared_r01",
            "campaign_lineage": "he2_table1_targeted_repair_20260612:canonical_bundle_targeted_repair",
            "replacement_reason": "unit_test_replacement",
            "publication_note": "new",
            "replacements": [
                {
                    "cutoff": "20220511",
                    "family": "dqlm_multivar_al_drop",
                    "manuscript_label": "AL-M-T0",
                    "run_id": "new_al_drop",
                }
            ],
        }
        out = apply_replacement_overlay(base, overlay)
        replaced = next(row for row in out if row["family"] == "dqlm_multivar_al_drop")
        untouched = next(row for row in out if row["family"] == "exdqlm_multivar_keep")

        self.assertEqual(replaced["run_id"], "new_al_drop")
        self.assertEqual(replaced["run_root"], "/tmp/he2_table1_overlay_test/runs/new_al_drop")
        self.assertEqual(replaced["replaced_source_run_id"], "old_al_drop")
        self.assertEqual(replaced["replacement_reason"], "unit_test_replacement")
        self.assertEqual(replaced["expected_input_bundle_id"], "20260510_publication_shared_r01")
        self.assertEqual(untouched["run_id"], "keep_winner")


if __name__ == "__main__":
    unittest.main()
