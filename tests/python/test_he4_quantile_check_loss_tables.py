#!/usr/bin/env python3
from __future__ import annotations

import os
import shutil
import tempfile
import unittest
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
os.sys.path.insert(0, str(ROOT / "scripts"))

from build_he4_quantile_check_loss_tables import (  # noqa: E402
    HE4_LABEL_TO_FAMILY,
    TAU_SPECS,
    cutoff_to_display,
    load_he4_targets_from_publication_manifest,
    load_forecast_quantile_frame,
    pinball_loss,
    render_he4_latex_main_table,
    render_he4_latex_rows,
    resolve_baseline_source_run,
    resolve_run_dir,
    resolve_tuned_selected_source_run,
    summarize_quantile_check_losses,
)


class He4QuantileCheckLossTableTests(unittest.TestCase):
    def test_pinball_loss_matches_standard_definition(self) -> None:
        observed = pd.Series([5.0, 2.0], dtype=float).to_numpy()
        quantile = pd.Series([3.0, 4.0], dtype=float).to_numpy()
        losses = pinball_loss(observed, quantile, 0.2)
        self.assertAlmostEqual(float(losses[0]), 0.4)
        self.assertAlmostEqual(float(losses[1]), 1.6)

    def test_resolve_baseline_source_run_requires_unique_provenance(self) -> None:
        td = Path(tempfile.mkdtemp(prefix="he4_provenance_"))
        try:
            reports_root = td / "reports"
            for eps, run_name in [("eps1cf1", "run_a"), ("eps30cf1", "run_a")]:
                compare_dir = reports_root / f"multimodel_20210123_v8_{eps}_compare"
                compare_dir.mkdir(parents=True, exist_ok=True)
                pd.DataFrame(
                    [{"model_id": "exdqlm_univar_synth", "source_run": run_name}]
                ).to_csv(compare_dir / "source_provenance.csv", index=False)
            resolved = resolve_baseline_source_run(reports_root, "20210123", "exdqlm_univar_synth")
            self.assertEqual(resolved, "run_a")

            bad_dir = reports_root / "multimodel_20210123_v8_eps60cf1_compare"
            bad_dir.mkdir(parents=True, exist_ok=True)
            pd.DataFrame(
                [{"model_id": "exdqlm_univar_synth", "source_run": "run_b"}]
            ).to_csv(bad_dir / "source_provenance.csv", index=False)
            with self.assertRaises(ValueError):
                resolve_baseline_source_run(reports_root, "20210123", "exdqlm_univar_synth")
        finally:
            shutil.rmtree(td, ignore_errors=True)

    def test_resolve_tuned_selected_source_run_prefers_selected_source_run(self) -> None:
        td = Path(tempfile.mkdtemp(prefix="he4_tuned_provenance_"))
        try:
            reports_root = td / "reports"
            compare_dir = reports_root / "multimodel_20210123_v8_eps180cf1_compare"
            compare_dir.mkdir(parents=True, exist_ok=True)
            pd.DataFrame(
                [
                    {
                        "model_id": "dqlm_multivar_al_synth_keep",
                        "source_run": "current_featurecov_run",
                        "selected_source_run": "selected_long_history_support_run",
                        "reuse_source_run_id": "reuse_featurecov_run",
                        "reused": True,
                        "source_type": "featurecov_cf1_eps_sweep",
                    }
                ]
            ).to_csv(compare_dir / "source_provenance.csv", index=False)
            run_name, metadata = resolve_tuned_selected_source_run(
                reports_root,
                "20210123",
                "eps180cf1",
                "dqlm_multivar_al_synth_keep",
            )
            self.assertEqual(run_name, "reuse_featurecov_run")
            self.assertEqual(metadata["provenance_source_run"], "current_featurecov_run")
            self.assertEqual(metadata["provenance_selected_source_run"], "selected_long_history_support_run")
            self.assertEqual(metadata["provenance_reuse_source_run_id"], "reuse_featurecov_run")
            self.assertTrue(metadata["provenance_reused"])

            pd.DataFrame(
                [
                    {
                        "model_id": "dqlm_multivar_al_synth_keep",
                        "source_run": "current_featurecov_run",
                        "selected_source_run": "selected_long_history_support_run",
                        "reuse_source_run_id": "",
                        "reused": False,
                        "source_type": "featurecov_cf1_eps_sweep",
                    }
                ]
            ).to_csv(compare_dir / "source_provenance.csv", index=False)
            run_name, metadata = resolve_tuned_selected_source_run(
                reports_root,
                "20210123",
                "eps180cf1",
                "dqlm_multivar_al_synth_keep",
            )
            self.assertEqual(run_name, "current_featurecov_run")
            self.assertEqual(metadata["provenance_selected_source_run"], "selected_long_history_support_run")
        finally:
            shutil.rmtree(td, ignore_errors=True)

    def test_resolve_run_dir_uses_expected_crps_to_break_duplicates(self) -> None:
        td = Path(tempfile.mkdtemp(prefix="he4_resolve_"))
        try:
            runtime_root = td / "runtime"
            run_name = "multimodel_20210123_v8_epsTT_l1"
            for campaign, mean_crps in [("campaign_a", 0.250000), ("campaign_b", 0.294603)]:
                tables_dir = runtime_root / campaign / "runs" / run_name / "post" / "outputs" / run_name / "tables"
                tables_dir.mkdir(parents=True, exist_ok=True)
                pd.DataFrame(
                    [{"model_id": "dqlm_univar_al_synth", "mean_crps": mean_crps}]
                ).to_csv(tables_dir / "crps_forecast_summary.csv", index=False)
            resolved_dir, resolved_crps = resolve_run_dir(
                runtime_root=runtime_root,
                run_name=run_name,
                internal_model_id="dqlm_univar_al_synth",
                expected_mean_crps=0.294603,
                tolerance=1e-6,
            )
            self.assertTrue(str(resolved_dir).endswith("campaign_b/runs/multimodel_20210123_v8_epsTT_l1"))
            self.assertAlmostEqual(resolved_crps, 0.294603)
        finally:
            shutil.rmtree(td, ignore_errors=True)

    def test_load_forecast_quantile_frame_filters_forecast_and_validates_monotonicity(self) -> None:
        td = Path(tempfile.mkdtemp(prefix="he4_quantiles_"))
        try:
            quantile_csv = td / "quantiles.csv"
            rows = [
                {
                    "model_id": "exdqlm_univar_synth",
                    "date": "2022-01-01",
                    "segment": "history",
                    "observed": 1.0,
                    "q05": 0.1,
                    "q20": 0.2,
                    "q35": 0.3,
                    "q50": 0.4,
                    "q65": 0.5,
                    "q80": 0.6,
                    "q95": 0.7,
                },
                {
                    "model_id": "exdqlm_univar_synth",
                    "date": "2022-01-02",
                    "segment": "forecast",
                    "observed": 2.0,
                    "q05": 1.0,
                    "q20": 1.2,
                    "q35": 1.4,
                    "q50": 1.6,
                    "q65": 1.8,
                    "q80": 2.0,
                    "q95": 2.2,
                },
                {
                    "model_id": "exdqlm_univar_synth",
                    "date": "2022-01-03",
                    "segment": "forecast",
                    "observed": 3.0,
                    "q05": 2.0,
                    "q20": 2.2,
                    "q35": 2.4,
                    "q50": 2.6,
                    "q65": 2.8,
                    "q80": 3.0,
                    "q95": 3.2,
                },
            ]
            pd.DataFrame(rows).to_csv(quantile_csv, index=False)
            forecast = load_forecast_quantile_frame(
                quantile_csv=quantile_csv,
                internal_model_id="exdqlm_univar_synth",
                expected_horizon_days=2,
            )
            self.assertEqual(len(forecast), 2)
            self.assertTrue((forecast["segment"] == "forecast").all())
        finally:
            shutil.rmtree(td, ignore_errors=True)

    def test_summarize_quantile_check_losses_and_render_latex(self) -> None:
        forecast = pd.DataFrame(
            {
                "model_id": ["exdqlm_univar_synth", "exdqlm_univar_synth"],
                "date": pd.to_datetime(["2022-01-02", "2022-01-03"]),
                "segment": ["forecast", "forecast"],
                "observed": [2.0, 4.0],
                "q05": [1.0, 3.0],
                "q20": [1.2, 3.2],
                "q35": [1.4, 3.4],
                "q50": [1.6, 3.6],
                "q65": [1.8, 3.8],
                "q80": [2.0, 4.0],
                "q95": [2.2, 4.2],
            }
        )
        per_day_rows, summary_rows = summarize_quantile_check_losses(
            forecast,
            cutoff="20210123",
            cutoff_display=cutoff_to_display("20210123"),
            manuscript_label="exAL-U-T1",
            model_variant="exdqlm_univar",
            internal_model_id="exdqlm_univar_synth",
            resolved_run_name="run_a",
            resolved_run_dir=Path("/tmp/run_a"),
            expected_mean_crps=0.296919,
            resolved_mean_crps=0.296919,
        )
        self.assertEqual(len(per_day_rows), len(TAU_SPECS) * 2)
        self.assertEqual(len(summary_rows), len(TAU_SPECS))
        wide_df = pd.DataFrame(summary_rows).pivot(
            index=["cutoff", "cutoff_display", "manuscript_label"],
            columns="tau_label",
            values="mean_check_loss",
        ).reset_index()
        base_row = wide_df.iloc[0].to_dict()
        for label in ["exAL-M-T1", "AL-M-T1", "AL-U-T1"]:
            cloned = dict(base_row)
            cloned["manuscript_label"] = label
            wide_df = pd.concat([wide_df, pd.DataFrame([cloned])], ignore_index=True)
        latex = render_he4_latex_rows(wide_df)
        self.assertIn(r"\multicolumn{8}{l}{\textit{Cutoff 01/23/2021}} \\", latex)
        self.assertIn("exAL-U-T1", latex)
        self.assertIn(r"\textbf{", latex)
        main_table = render_he4_latex_main_table(wide_df)
        self.assertIn(r"\label{tab:he4_quantile_check_loss}", main_table)
        self.assertIn("forecast-window quantile check loss", main_table)

    def test_load_he4_targets_from_publication_manifest_maps_current_contract(self) -> None:
        td = Path(tempfile.mkdtemp(prefix="he4_publication_manifest_"))
        try:
            rows = []
            for cutoff in ["20210123", "20211112"]:
                for label, family in HE4_LABEL_TO_FAMILY.items():
                    rows.append(
                        {
                            "cutoff": cutoff,
                            "cutoff_display": cutoff_to_display(cutoff),
                            "manuscript_label": label,
                            "family": family,
                            "run_id": f"run_{cutoff}_{family}",
                            "run_root": str(td / "runs" / f"run_{cutoff}_{family}"),
                            "crps_exact": 0.1,
                            "horizon_days": 28,
                            "score_scale": "log_cms_plus1",
                        }
                    )
            manifest = td / "he2_bayesian_publication_manifest.csv"
            pd.DataFrame(rows).to_csv(manifest, index=False)
            targets = load_he4_targets_from_publication_manifest(manifest)
            self.assertEqual(len(targets), 8)
            self.assertEqual(set(targets["selection_mode"]), {"he2-publication-manifest"})
            self.assertEqual(set(targets["internal_model_id"]), {
                "exdqlm_multivar_synth_keep",
                "dqlm_multivar_al_synth_keep",
                "exdqlm_univar_synth",
                "dqlm_univar_al_synth",
            })
            self.assertTrue((targets["expected_mean_crps"] == 0.1).all())
        finally:
            shutil.rmtree(td, ignore_errors=True)


if __name__ == "__main__":
    unittest.main()
