#!/usr/bin/env python3
from __future__ import annotations

import argparse
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd

from multimodel_v8_lib import ROOT, ensure_dir

RUNTIME_ROOT_DEFAULT = ROOT.parent / "project1_ucsc_phd_runtime"
CF1_SWEEP_ROOT_DEFAULT = RUNTIME_ROOT_DEFAULT / "multimodel_v8_featurecov_cf1_eps_sweep_20260416"
BEST_BY_CUTOFF_CSV_DEFAULT = (
    CF1_SWEEP_ROOT_DEFAULT
    / "reports"
    / "final_featurecov_cf1_eps_analysis"
    / "best_by_cutoff_long.csv"
)
COMPARE_REPORTS_ROOT_DEFAULT = CF1_SWEEP_ROOT_DEFAULT / "reports"
OUTPUT_DIR_DEFAULT = (
    CF1_SWEEP_ROOT_DEFAULT
    / "reports"
    / "final_featurecov_cf1_eps_analysis"
    / "he4_quantile_check_loss"
)
HE2_PUBLICATION_MANIFEST_DEFAULT = (
    ROOT
    / "Evironmetrics---REVISED-DOC-Corrected-2"
    / "artifacts"
    / "he2_publication_freeze"
    / "he2_bayesian_publication_manifest.csv"
)

SOURCE_MODE_CF1_SWEEP = "cf1-sweep"
SOURCE_MODE_HE2_PUBLICATION_MANIFEST = "he2-publication-manifest"
SOURCE_MODE_CHOICES = (SOURCE_MODE_CF1_SWEEP, SOURCE_MODE_HE2_PUBLICATION_MANIFEST)

TAU_SPECS: tuple[tuple[str, str, float], ...] = (
    ("q0.05", "q05", 0.05),
    ("q0.20", "q20", 0.20),
    ("q0.35", "q35", 0.35),
    ("q0.50", "q50", 0.50),
    ("q0.65", "q65", 0.65),
    ("q0.80", "q80", 0.80),
    ("q0.95", "q95", 0.95),
)
DISPLAY_DIGITS = 5
DISPLAY_TOLERANCE = 0.5 * 10 ** (-DISPLAY_DIGITS)


@dataclass(frozen=True)
class He4TargetSpec:
    manuscript_label: str
    best_by_cutoff_variant: str
    internal_model_id: str
    quantile_filename: str
    selection_mode: str


HE4_TARGET_SPECS: dict[str, He4TargetSpec] = {
    "exdqlm_multivar_keep": He4TargetSpec(
        manuscript_label="exAL-M-T1",
        best_by_cutoff_variant="exdqlm_multivar_keep",
        internal_model_id="exdqlm_multivar_synth_keep",
        quantile_filename="exdqlm_multivar_synth_keep_cutoff_window_quantiles.csv",
        selection_mode="tuned_current_cf1",
    ),
    "dqlm_multivar_al_keep": He4TargetSpec(
        manuscript_label="AL-M-T1",
        best_by_cutoff_variant="dqlm_multivar_al_keep",
        internal_model_id="dqlm_multivar_al_synth_keep",
        quantile_filename="dqlm_multivar_al_synth_keep_cutoff_window_quantiles.csv",
        selection_mode="tuned_current_cf1",
    ),
    "exdqlm_univar": He4TargetSpec(
        manuscript_label="exAL-U-T1",
        best_by_cutoff_variant="exdqlm_univar",
        internal_model_id="exdqlm_univar_synth",
        quantile_filename="exdqlm_univar_synth_cutoff_window_quantiles.csv",
        selection_mode="fixed_baseline",
    ),
    "dqlm_univar_al": He4TargetSpec(
        manuscript_label="AL-U-T1",
        best_by_cutoff_variant="dqlm_univar_al",
        internal_model_id="dqlm_univar_al_synth",
        quantile_filename="dqlm_univar_al_synth_cutoff_window_quantiles.csv",
        selection_mode="fixed_baseline",
    ),
}

HE4_LABEL_TO_FAMILY = {
    spec.manuscript_label: family
    for family, spec in HE4_TARGET_SPECS.items()
}

MANUSCRIPT_MODEL_ORDER = [
    "exAL-M-T1",
    "AL-M-T1",
    "exAL-U-T1",
    "AL-U-T1",
]


def _clean_str(value: object) -> str:
    if value is None:
        return ""
    try:
        if pd.isna(value):
            return ""
    except Exception:
        pass
    return str(value).strip()


def cutoff_to_display(cutoff: str) -> str:
    return datetime.strptime(str(cutoff), "%Y%m%d").strftime("%m/%d/%Y")


def pinball_loss(observed: np.ndarray, quantile: np.ndarray, tau: float) -> np.ndarray:
    residual = observed - quantile
    return np.where(residual >= 0.0, tau * residual, (1.0 - tau) * (-residual))


def _load_he4_targets(best_by_cutoff_csv: Path) -> pd.DataFrame:
    df = pd.read_csv(best_by_cutoff_csv)
    wanted = set(HE4_TARGET_SPECS)
    df = df[df["model_variant"].isin(wanted)].copy()
    if df.empty:
        raise ValueError(f"No HE4 targets found in {best_by_cutoff_csv}")
    df["cutoff"] = df["cutoff"].astype(str)
    df["spec"] = df["model_variant"].map(HE4_TARGET_SPECS)
    if df["spec"].isna().any():
        missing = sorted(df.loc[df["spec"].isna(), "model_variant"].unique())
        raise ValueError(f"Missing HE4 target specs for: {missing}")
    df["manuscript_label"] = df["spec"].map(lambda spec: spec.manuscript_label)
    df["internal_model_id"] = df["spec"].map(lambda spec: spec.internal_model_id)
    df["quantile_filename"] = df["spec"].map(lambda spec: spec.quantile_filename)
    df["selection_mode"] = df["spec"].map(lambda spec: spec.selection_mode)
    df["cutoff_display"] = df["cutoff"].map(cutoff_to_display)
    df["expected_mean_crps"] = pd.to_numeric(df["forecast_window_crps"])
    return df.sort_values(["cutoff", "manuscript_label"]).reset_index(drop=True)


def load_he4_targets_from_publication_manifest(he2_publication_manifest: Path) -> pd.DataFrame:
    df = pd.read_csv(he2_publication_manifest)
    required = {
        "cutoff",
        "cutoff_display",
        "manuscript_label",
        "family",
        "run_id",
        "run_root",
        "crps_exact",
        "horizon_days",
        "score_scale",
    }
    missing = required.difference(df.columns)
    if missing:
        raise ValueError(f"{he2_publication_manifest} is missing required columns: {sorted(missing)}")

    targets = df[df["manuscript_label"].isin(HE4_LABEL_TO_FAMILY)].copy()
    if targets.empty:
        raise ValueError(f"No HE4 publication targets found in {he2_publication_manifest}")

    expected_pairs = {
        label: HE4_LABEL_TO_FAMILY[label]
        for label in MANUSCRIPT_MODEL_ORDER
        if label in HE4_LABEL_TO_FAMILY
    }
    observed_pairs = dict(zip(targets["manuscript_label"], targets["family"]))
    for label, family in expected_pairs.items():
        if label not in observed_pairs:
            raise ValueError(f"Missing HE4 publication target for manuscript label {label}")
        if observed_pairs[label] != family and not (
            (targets["manuscript_label"] == label) & (targets["family"] == family)
        ).any():
            raise ValueError(
                f"HE4 publication target {label} should map to family {family}, "
                f"found {sorted(targets.loc[targets['manuscript_label'] == label, 'family'].unique())}"
            )

    targets["cutoff"] = targets["cutoff"].astype(str)
    targets["model_variant"] = targets["family"].astype(str)
    targets["spec"] = targets["model_variant"].map(HE4_TARGET_SPECS)
    if targets["spec"].isna().any():
        missing_families = sorted(targets.loc[targets["spec"].isna(), "model_variant"].unique())
        raise ValueError(f"Missing HE4 target specs for publication families: {missing_families}")
    targets["internal_model_id"] = targets["spec"].map(lambda spec: spec.internal_model_id)
    targets["quantile_filename"] = targets["spec"].map(lambda spec: spec.quantile_filename)
    targets["selection_mode"] = SOURCE_MODE_HE2_PUBLICATION_MANIFEST
    targets["expected_mean_crps"] = pd.to_numeric(targets["crps_exact"], errors="raise")
    targets["horizon_days"] = pd.to_numeric(targets["horizon_days"], errors="raise").astype(int)
    targets["run_root"] = targets["run_root"].astype(str)
    targets["run_id"] = targets["run_id"].astype(str)

    duplicate_keys = targets.duplicated(["cutoff", "manuscript_label"], keep=False)
    if duplicate_keys.any():
        duplicates = targets.loc[duplicate_keys, ["cutoff", "manuscript_label", "run_id"]]
        raise ValueError(f"Duplicate HE4 publication targets detected:\n{duplicates.to_string(index=False)}")

    expected_n = len(set(targets["cutoff"])) * len(expected_pairs)
    if len(targets) != expected_n:
        raise ValueError(
            f"Expected {expected_n} HE4 publication rows "
            f"({len(set(targets['cutoff']))} cutoffs x {len(expected_pairs)} models), found {len(targets)}"
        )

    return targets.sort_values(["cutoff", "manuscript_label"]).reset_index(drop=True)


def _candidate_source_provenance_paths(compare_reports_root: Path, cutoff: str) -> list[Path]:
    return sorted(compare_reports_root.glob(f"multimodel_{cutoff}_v8_*_compare/source_provenance.csv"))


def resolve_baseline_source_run(
    compare_reports_root: Path,
    cutoff: str,
    internal_model_id: str,
) -> str:
    source_runs: set[str] = set()
    for provenance_path in _candidate_source_provenance_paths(compare_reports_root, cutoff):
        df = pd.read_csv(provenance_path)
        rows = df[df["model_id"] == internal_model_id]
        if rows.empty:
            continue
        for value in rows["source_run"].dropna().astype(str):
            if value.strip():
                source_runs.add(value.strip())
    if not source_runs:
        raise FileNotFoundError(
            f"Could not resolve baseline source run for cutoff={cutoff}, model_id={internal_model_id}"
        )
    if len(source_runs) != 1:
        raise ValueError(
            f"Expected one baseline source run for cutoff={cutoff}, model_id={internal_model_id}; "
            f"found {sorted(source_runs)}"
        )
    return next(iter(source_runs))


def resolve_tuned_selected_source_run(
    compare_reports_root: Path,
    cutoff: str,
    epsilon_label: str,
    internal_model_id: str,
) -> tuple[str, dict[str, object]]:
    provenance_path = (
        compare_reports_root
        / f"multimodel_{cutoff}_v8_{epsilon_label}_compare"
        / "source_provenance.csv"
    )
    if not provenance_path.exists():
        raise FileNotFoundError(f"Missing tuned source provenance file: {provenance_path}")
    df = pd.read_csv(provenance_path)
    rows = df[df["model_id"] == internal_model_id]
    if rows.empty:
        raise ValueError(f"{internal_model_id} not found in {provenance_path}")
    if len(rows) != 1:
        raise ValueError(f"Expected one row for {internal_model_id} in {provenance_path}, found {len(rows)}")
    row = rows.iloc[0]
    reuse_source_run_id = _clean_str(row.get("reuse_source_run_id", ""))
    selected_source_run = _clean_str(row.get("selected_source_run", ""))
    source_run = _clean_str(row.get("source_run", ""))
    chosen = reuse_source_run_id or source_run
    if not chosen:
        raise ValueError(f"No source run recorded for {internal_model_id} in {provenance_path}")
    metadata = {
        "provenance_path": str(provenance_path),
        "provenance_source_run": source_run,
        "provenance_reuse_source_run_id": reuse_source_run_id,
        "provenance_selected_source_run": selected_source_run,
        "provenance_reused": bool(row.get("reused", False)),
        "provenance_source_type": _clean_str(row.get("source_type", "")),
        "provenance_selected_epsilon": row.get("selected_epsilon", np.nan),
        "provenance_selected_c_factor": row.get("selected_c_factor", np.nan),
    }
    return chosen, metadata


def discover_run_candidates(runtime_root: Path, run_name: str) -> list[Path]:
    candidates = sorted(path.resolve() for path in runtime_root.glob(f"*/runs/{run_name}") if path.is_dir())
    if not candidates:
        raise FileNotFoundError(f"Could not find any run directories named {run_name} under {runtime_root}")
    return candidates


def resolve_unique_run_candidate(runtime_root: Path, run_name: str) -> Path:
    candidates = discover_run_candidates(runtime_root, run_name)
    if len(candidates) != 1:
        raise ValueError(
            f"Expected exactly one run directory for {run_name}, found {len(candidates)}: "
            f"{[str(path) for path in candidates]}"
        )
    return candidates[0]


def run_output_root(run_dir: Path) -> Path:
    return run_dir / "post" / "outputs" / run_dir.name


def crps_summary_path_for_run(run_dir: Path) -> Path:
    return run_output_root(run_dir) / "tables" / "crps_forecast_summary.csv"


def quantile_path_for_run(run_dir: Path, quantile_filename: str) -> Path:
    return run_output_root(run_dir) / quantile_filename


def read_compare_bundle_mean_crps(
    compare_reports_root: Path,
    cutoff: str,
    epsilon_label: str,
    internal_model_id: str,
) -> float:
    compare_csv = (
        compare_reports_root
        / f"multimodel_{cutoff}_v8_{epsilon_label}_compare"
        / "crps_forecast_summary_all_models.csv"
    )
    if not compare_csv.exists():
        raise FileNotFoundError(f"Missing compare-bundle CRPS summary: {compare_csv}")
    df = pd.read_csv(compare_csv)
    rows = df[df["model_id"] == internal_model_id]
    if rows.empty:
        raise ValueError(f"{internal_model_id} not found in {compare_csv}")
    if len(rows) != 1:
        raise ValueError(f"Expected one row for {internal_model_id} in {compare_csv}, found {len(rows)}")
    return float(rows["mean_crps"].iloc[0])


def read_model_mean_crps(crps_summary_path: Path, internal_model_id: str) -> float:
    if not crps_summary_path.exists():
        raise FileNotFoundError(f"Missing CRPS summary: {crps_summary_path}")
    df = pd.read_csv(crps_summary_path)
    rows = df[df["model_id"] == internal_model_id]
    if rows.empty:
        raise ValueError(f"{internal_model_id} not found in {crps_summary_path}")
    if len(rows) != 1:
        raise ValueError(f"Expected one row for {internal_model_id} in {crps_summary_path}, found {len(rows)}")
    return float(rows["mean_crps"].iloc[0])


def resolve_run_dir(
    runtime_root: Path,
    run_name: str,
    internal_model_id: str,
    expected_mean_crps: float,
    tolerance: float,
) -> tuple[Path, float]:
    diagnostics: list[tuple[Path, float, float]] = []
    for run_dir in discover_run_candidates(runtime_root, run_name):
        observed = read_model_mean_crps(crps_summary_path_for_run(run_dir), internal_model_id)
        diagnostics.append((run_dir, observed, abs(observed - expected_mean_crps)))
    exact = [row for row in diagnostics if row[2] <= tolerance]
    if len(exact) == 1:
        run_dir, observed, _diff = exact[0]
        return run_dir, observed
    if len(exact) > 1:
        raise ValueError(
            f"Ambiguous run resolution for {run_name} / {internal_model_id}: "
            f"multiple candidates match expected CRPS {expected_mean_crps:.6f}: "
            f"{[(str(path), value) for path, value, _diff in exact]}"
        )
    raise ValueError(
        f"No run candidate matched expected CRPS {expected_mean_crps:.6f} for {run_name} / {internal_model_id}. "
        f"Observed candidates: {[(str(path), value, diff) for path, value, diff in diagnostics]}"
    )


def load_forecast_quantile_frame(
    quantile_csv: Path,
    internal_model_id: str,
    expected_horizon_days: int,
) -> pd.DataFrame:
    if not quantile_csv.exists():
        raise FileNotFoundError(f"Missing quantile artifact: {quantile_csv}")
    df = pd.read_csv(quantile_csv)
    required_columns = {"model_id", "date", "segment", "observed"} | {col for _label, col, _tau in TAU_SPECS}
    missing = required_columns.difference(df.columns)
    if missing:
        raise ValueError(f"{quantile_csv} is missing required columns: {sorted(missing)}")
    model_ids = set(df["model_id"].dropna().astype(str))
    if model_ids != {internal_model_id}:
        raise ValueError(f"Expected {quantile_csv} to contain only {internal_model_id}, found {sorted(model_ids)}")
    forecast = df[df["segment"].astype(str) == "forecast"].copy()
    if len(forecast) != int(expected_horizon_days):
        raise ValueError(
            f"Expected {expected_horizon_days} forecast rows in {quantile_csv}, found {len(forecast)}"
        )
    forecast["date"] = pd.to_datetime(forecast["date"], utc=False)
    numeric_columns = ["observed"] + [col for _label, col, _tau in TAU_SPECS]
    for column in numeric_columns:
        forecast[column] = pd.to_numeric(forecast[column], errors="raise")
    quantile_cols = [col for _label, col, _tau in TAU_SPECS]
    quantile_matrix = forecast[quantile_cols].to_numpy(dtype=float)
    if np.isnan(quantile_matrix).any():
        raise ValueError(f"NaN quantiles detected in {quantile_csv}")
    monotone = np.all(np.diff(quantile_matrix, axis=1) >= -1e-10)
    if not monotone:
        raise ValueError(f"Quantile crossing detected in {quantile_csv}")
    return forecast.sort_values("date").reset_index(drop=True)


def summarize_quantile_check_losses(
    forecast_df: pd.DataFrame,
    *,
    cutoff: str,
    cutoff_display: str,
    manuscript_label: str,
    model_variant: str,
    internal_model_id: str,
    resolved_run_name: str,
    resolved_run_dir: Path,
    expected_mean_crps: float,
    resolved_mean_crps: float,
    score_scale: str = "log_cms_plus1",
) -> tuple[list[dict[str, object]], list[dict[str, object]]]:
    per_day_rows: list[dict[str, object]] = []
    summary_rows: list[dict[str, object]] = []
    observed = forecast_df["observed"].to_numpy(dtype=float)
    dates = forecast_df["date"].dt.strftime("%Y-%m-%d")
    for tau_label, quantile_column, tau in TAU_SPECS:
        predicted = forecast_df[quantile_column].to_numpy(dtype=float)
        losses = pinball_loss(observed, predicted, tau)
        summary_rows.append(
            {
                "cutoff": cutoff,
                "cutoff_display": cutoff_display,
                "manuscript_label": manuscript_label,
                "model_variant": model_variant,
                "internal_model_id": internal_model_id,
                "resolved_run_name": resolved_run_name,
                "resolved_run_dir": str(resolved_run_dir),
                "tau_label": tau_label,
                "tau": tau,
                "quantile_column": quantile_column,
                "mean_check_loss": float(losses.mean()),
                "n_forecast_days": int(len(losses)),
                "expected_mean_crps": float(expected_mean_crps),
                "resolved_mean_crps": float(resolved_mean_crps),
                "crps_abs_diff": abs(float(expected_mean_crps) - float(resolved_mean_crps)),
                "score_scale": score_scale,
            }
        )
        for forecast_date, obs_value, pred_value, loss_value in zip(dates, observed, predicted, losses):
            per_day_rows.append(
                {
                    "cutoff": cutoff,
                    "cutoff_display": cutoff_display,
                    "forecast_date": forecast_date,
                    "manuscript_label": manuscript_label,
                    "model_variant": model_variant,
                    "internal_model_id": internal_model_id,
                    "resolved_run_name": resolved_run_name,
                    "resolved_run_dir": str(resolved_run_dir),
                    "tau_label": tau_label,
                    "tau": tau,
                    "quantile_column": quantile_column,
                    "observed": float(obs_value),
                    "predicted_quantile": float(pred_value),
                    "check_loss": float(loss_value),
                    "score_scale": score_scale,
                }
            )
    return per_day_rows, summary_rows


def build_he4_outputs(
    *,
    best_by_cutoff_csv: Path,
    he2_publication_manifest: Path | None = None,
    compare_reports_root: Path,
    runtime_root: Path,
    output_dir: Path,
    tolerance: float,
    source_mode: str = SOURCE_MODE_CF1_SWEEP,
) -> dict[str, Path]:
    ensure_dir(output_dir)
    if source_mode == SOURCE_MODE_CF1_SWEEP:
        targets = _load_he4_targets(best_by_cutoff_csv)
        provenance_source = str(best_by_cutoff_csv)
    elif source_mode == SOURCE_MODE_HE2_PUBLICATION_MANIFEST:
        if he2_publication_manifest is None:
            raise ValueError("--he2-publication-manifest is required for he2-publication-manifest mode")
        targets = load_he4_targets_from_publication_manifest(he2_publication_manifest)
        provenance_source = str(he2_publication_manifest)
    else:
        raise ValueError(f"Unsupported HE4 source mode: {source_mode}")

    selection_rows: list[dict[str, object]] = []
    per_day_rows: list[dict[str, object]] = []
    summary_rows: list[dict[str, object]] = []

    for row in targets.to_dict("records"):
        if row["selection_mode"] == SOURCE_MODE_HE2_PUBLICATION_MANIFEST:
            expected_run_name = str(row["run_id"])
            resolved_run_dir = Path(str(row["run_root"])).resolve()
            if not resolved_run_dir.is_dir():
                raise FileNotFoundError(f"HE2 manifest run_root does not exist: {resolved_run_dir}")
            if resolved_run_dir.name != expected_run_name:
                raise ValueError(
                    f"HE2 manifest run_root/run_id mismatch: run_root={resolved_run_dir}, run_id={expected_run_name}"
                )
            resolved_mean_crps = read_model_mean_crps(
                crps_summary_path_for_run(resolved_run_dir),
                str(row["internal_model_id"]),
            )
            if abs(float(row["expected_mean_crps"]) - float(resolved_mean_crps)) > tolerance:
                raise ValueError(
                    f"HE2 manifest CRPS mismatch for {expected_run_name}: expected "
                    f"{float(row['expected_mean_crps']):.6f}, observed {float(resolved_mean_crps):.6f}"
                )
            provenance_metadata = {
                "provenance_path": provenance_source,
                "provenance_source_run": expected_run_name,
                "provenance_selected_source_run": expected_run_name,
                "provenance_reuse_source_run_id": "",
                "provenance_reused": False,
                "provenance_source_type": SOURCE_MODE_HE2_PUBLICATION_MANIFEST,
                "provenance_selected_epsilon": np.nan,
                "provenance_selected_c_factor": np.nan,
            }
        elif row["selection_mode"] == "tuned_current_cf1":
            expected_run_name, provenance_metadata = resolve_tuned_selected_source_run(
                compare_reports_root=compare_reports_root,
                cutoff=str(row["cutoff"]),
                epsilon_label=str(row["best_epsilon_label"]),
                internal_model_id=str(row["internal_model_id"]),
            )
            resolved_mean_crps = read_compare_bundle_mean_crps(
                compare_reports_root=compare_reports_root,
                cutoff=str(row["cutoff"]),
                epsilon_label=str(row["best_epsilon_label"]),
                internal_model_id=str(row["internal_model_id"]),
            )
            if abs(float(row["expected_mean_crps"]) - float(resolved_mean_crps)) > tolerance:
                raise ValueError(
                    f"Compare-bundle CRPS mismatch for {expected_run_name}: expected "
                    f"{float(row['expected_mean_crps']):.6f}, observed {float(resolved_mean_crps):.6f}"
                )
        else:
            expected_run_name = resolve_baseline_source_run(
                compare_reports_root,
                str(row["cutoff"]),
                str(row["internal_model_id"]),
            )
            provenance_metadata = {
                "provenance_path": "",
                "provenance_source_run": expected_run_name,
                "provenance_selected_source_run": "",
                "provenance_reused": False,
                "provenance_source_type": "baseline_tt",
                "provenance_selected_epsilon": np.nan,
                "provenance_selected_c_factor": np.nan,
            }
        if row["selection_mode"] != SOURCE_MODE_HE2_PUBLICATION_MANIFEST:
            resolved_run_dir, resolved_mean_crps = resolve_run_dir(
                runtime_root=runtime_root,
                run_name=expected_run_name,
                internal_model_id=str(row["internal_model_id"]),
                expected_mean_crps=float(row["expected_mean_crps"]),
                tolerance=tolerance,
            )
        quantile_csv = quantile_path_for_run(resolved_run_dir, str(row["quantile_filename"]))
        forecast_df = load_forecast_quantile_frame(
            quantile_csv=quantile_csv,
            internal_model_id=str(row["internal_model_id"]),
            expected_horizon_days=int(row["horizon_days"]),
        )
        row_per_day, row_summary = summarize_quantile_check_losses(
            forecast_df,
            cutoff=str(row["cutoff"]),
            cutoff_display=str(row["cutoff_display"]),
            manuscript_label=str(row["manuscript_label"]),
            model_variant=str(row["model_variant"]),
            internal_model_id=str(row["internal_model_id"]),
            resolved_run_name=expected_run_name,
            resolved_run_dir=resolved_run_dir,
            expected_mean_crps=float(row["expected_mean_crps"]),
            resolved_mean_crps=float(resolved_mean_crps),
        )
        per_day_rows.extend(row_per_day)
        summary_rows.extend(row_summary)
        selection_rows.append(
            {
                "cutoff": row["cutoff"],
                "cutoff_display": row["cutoff_display"],
                "manuscript_label": row["manuscript_label"],
                "model_variant": row["model_variant"],
                "selection_mode": row["selection_mode"],
                "source_mode": source_mode,
                "best_epsilon_label": row.get("best_epsilon_label", ""),
                "best_epsilon_value": row.get("best_epsilon_value", np.nan),
                "best_c_factor": row.get("best_c_factor", np.nan),
                "selected_run_name": expected_run_name,
                "resolved_run_dir": str(resolved_run_dir),
                "quantile_csv": str(quantile_csv),
                "expected_mean_crps": float(row["expected_mean_crps"]),
                "resolved_mean_crps": float(resolved_mean_crps),
                "crps_abs_diff": abs(float(row["expected_mean_crps"]) - float(resolved_mean_crps)),
                "horizon_days": int(row["horizon_days"]),
                **provenance_metadata,
            }
        )

    selection_df = pd.DataFrame(selection_rows).sort_values(["cutoff", "manuscript_label"]).reset_index(drop=True)
    per_day_df = pd.DataFrame(per_day_rows).sort_values(
        ["cutoff", "manuscript_label", "tau", "forecast_date"]
    ).reset_index(drop=True)
    summary_df = pd.DataFrame(summary_rows).sort_values(
        ["cutoff", "manuscript_label", "tau"]
    ).reset_index(drop=True)
    wide_df = (
        summary_df.pivot(index=["cutoff", "cutoff_display", "manuscript_label"], columns="tau_label", values="mean_check_loss")
        .reset_index()
        .rename_axis(columns=None)
    )
    wide_df["model_order"] = wide_df["manuscript_label"].map({label: idx for idx, label in enumerate(MANUSCRIPT_MODEL_ORDER)})
    wide_df = wide_df.sort_values(["cutoff", "model_order"]).drop(columns=["model_order"]).reset_index(drop=True)

    selection_path = output_dir / "he4_selection_audit.csv"
    per_day_path = output_dir / "he4_quantile_check_loss_per_day.csv"
    long_path = output_dir / "he4_quantile_check_loss_long.csv"
    wide_path = output_dir / "he4_quantile_check_loss_wide.csv"
    summary_md_path = output_dir / "he4_quantile_check_loss_summary.md"
    latex_path = output_dir / "he4_table_rows.tex"
    latex_main_path = output_dir / "he4_main_table.tex"

    selection_df.to_csv(selection_path, index=False)
    per_day_df.to_csv(per_day_path, index=False)
    summary_df.to_csv(long_path, index=False)
    wide_df.to_csv(wide_path, index=False)
    summary_md_path.write_text(render_he4_markdown(selection_df, wide_df), encoding="utf-8")
    latex_path.write_text(render_he4_latex_rows(wide_df), encoding="utf-8")
    latex_main_path.write_text(render_he4_latex_main_table(wide_df), encoding="utf-8")

    return {
        "selection_audit": selection_path,
        "per_day": per_day_path,
        "long": long_path,
        "wide": wide_path,
        "summary_md": summary_md_path,
        "latex_rows": latex_path,
        "latex_main": latex_main_path,
    }


def render_he4_markdown(selection_df: pd.DataFrame, wide_df: pd.DataFrame) -> str:
    def frame_to_markdown(df: pd.DataFrame, *, floatfmt: str | None = None) -> str:
        try:
            return df.to_markdown(index=False, floatfmt=floatfmt)  # type: ignore[call-arg]
        except Exception:
            return df.to_csv(index=False)

    lines = [
        "# HE4 Quantile Check-Loss Summary",
        "",
        "- Scope: mean forecast-window quantile check loss for the four HE4 synthesis models.",
        "- Verification target: observed USGS series embedded in the selected run quantile artifacts.",
        "- Score scale: `log_cms_plus1`, matching the CRPS summaries used in HE-2.",
        "- Forecast rows only: `segment == forecast`.",
        "",
        "## Selection Audit",
        "",
        frame_to_markdown(selection_df),
        "",
        "## HE4 Table",
        "",
    ]
    for cutoff, cutoff_panel in wide_df.groupby("cutoff", sort=True):
        display = cutoff_panel["cutoff_display"].iloc[0]
        lines.extend(
            [
                f"### Cutoff {display}",
                "",
                frame_to_markdown(cutoff_panel.drop(columns=["cutoff"]), floatfmt=f".{DISPLAY_DIGITS}f"),
                "",
            ]
        )
    return "\n".join(lines)


def _format_he4_value(
    value: float,
    *,
    best_value: float | None,
    bold_best: bool,
    tolerance: float = DISPLAY_TOLERANCE,
) -> str:
    rendered = f"{float(value):.{DISPLAY_DIGITS}f}"
    if (
        bold_best
        and best_value is not None
        and abs(round(float(value), DISPLAY_DIGITS) - round(float(best_value), DISPLAY_DIGITS)) <= tolerance
    ):
        return rf"\textbf{{{rendered}}}"
    return rendered


def render_he4_latex_rows(wide_df: pd.DataFrame, *, bold_best: bool = True) -> str:
    lines: list[str] = []
    tau_labels = [label for label, _col, _tau in TAU_SPECS]
    for cutoff, cutoff_panel in wide_df.groupby("cutoff", sort=True):
        display = cutoff_panel["cutoff_display"].iloc[0]
        lines.append(rf"\multicolumn{{8}}{{l}}{{\textit{{Cutoff {display}}}}} \\")
        best_by_tau = {
            tau_label: float(cutoff_panel[tau_label].astype(float).min())
            for tau_label in tau_labels
        }
        for manuscript_label in MANUSCRIPT_MODEL_ORDER:
            row = cutoff_panel[cutoff_panel["manuscript_label"] == manuscript_label]
            if row.empty:
                raise ValueError(f"Missing HE4 row for cutoff={cutoff}, manuscript_label={manuscript_label}")
            record = row.iloc[0]
            values = " & ".join(
                _format_he4_value(
                    float(record[tau_label]),
                    best_value=best_by_tau[tau_label],
                    bold_best=bold_best,
                )
                for tau_label in tau_labels
            )
            lines.append(f"{manuscript_label} & {values} \\\\")
        lines.append(r"\addlinespace[1pt]")
    if lines and lines[-1] == r"\addlinespace[1pt]":
        lines.pop()
    return "\n".join(lines) + "\n"


def render_he4_latex_main_table(wide_df: pd.DataFrame) -> str:
    body = render_he4_latex_rows(wide_df).rstrip("\n").splitlines()
    lines = [
        r"\begin{table*}[htbp]",
        r"\centering",
        r"\renewcommand{\arraystretch}{1.08}",
        r"\begin{threeparttable}",
        r"\caption{Mean forecast-window quantile check loss by synthesis model, cutoff, and target quantile. Lower values are better; bold indicates the lowest check loss within each cutoff and quantile column.}",
        r"\label{tab:he4_quantile_check_loss}",
        r"\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}} >{\ttfamily}l r r r r r r r}",
        r"\toprule",
        r"Model & q0.05 & q0.20 & q0.35 & q0.50 & q0.65 & q0.80 & q0.95 \\",
        r"\midrule",
        *body,
        r"\bottomrule",
        r"\end{tabular*}",
        r"\begin{tablenotes}",
        r"\item \textit{Note:} Check loss is computed on forecast-window rows only, using the held-out USGS observation as the verification target on the same $\log(1+Q)$ scale used for CRPS. The four synthesis competitors are resolved directly from the frozen HE-2 publication manifest.",
        r"\end{tablenotes}",
        r"\end{threeparttable}",
        r"\end{table*}",
    ]
    return "\n".join(lines) + "\n"


def parse_args(argv: Iterable[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Build HE4 forecast-window quantile check-loss tables from the same selected runs used in HE-2."
        )
    )
    parser.add_argument(
        "--source-mode",
        choices=SOURCE_MODE_CHOICES,
        default=SOURCE_MODE_CF1_SWEEP,
        help=(
            "Target source contract. Use cf1-sweep for the legacy April sweep or "
            "he2-publication-manifest for the current publication HE2 manifest."
        ),
    )
    parser.add_argument(
        "--best-by-cutoff-csv",
        type=Path,
        default=BEST_BY_CUTOFF_CSV_DEFAULT,
        help="Path to best_by_cutoff_long.csv from the finalized cf1 sweep analysis.",
    )
    parser.add_argument(
        "--he2-publication-manifest",
        type=Path,
        default=HE2_PUBLICATION_MANIFEST_DEFAULT,
        help="Path to the frozen HE2 publication manifest used by he2-publication-manifest mode.",
    )
    parser.add_argument(
        "--compare-reports-root",
        type=Path,
        default=COMPARE_REPORTS_ROOT_DEFAULT,
        help="Root directory containing multimodel_*_compare/source_provenance.csv files.",
    )
    parser.add_argument(
        "--runtime-root",
        type=Path,
        default=RUNTIME_ROOT_DEFAULT,
        help="Root directory containing campaign runtime folders with run outputs.",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=OUTPUT_DIR_DEFAULT,
        help="Directory where HE4 CSV/Markdown/LaTeX outputs will be written.",
    )
    parser.add_argument(
        "--crps-match-tolerance",
        type=float,
        default=1e-6,
        help="Absolute tolerance used to match duplicate source runs against the HE-2 CRPS target.",
    )
    return parser.parse_args(argv)


def main(argv: Iterable[str] | None = None) -> int:
    args = parse_args(argv)
    outputs = build_he4_outputs(
        best_by_cutoff_csv=args.best_by_cutoff_csv.resolve(),
        he2_publication_manifest=args.he2_publication_manifest.resolve(),
        compare_reports_root=args.compare_reports_root.resolve(),
        runtime_root=args.runtime_root.resolve(),
        output_dir=args.output_dir.resolve(),
        tolerance=float(args.crps_match_tolerance),
        source_mode=str(args.source_mode),
    )
    print("HE4 outputs written:")
    for key, path in outputs.items():
        print(f"- {key}: {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
