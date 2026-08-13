#!/usr/bin/env python3
from __future__ import annotations

import csv
import hashlib
import json
import os
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Any

import yaml

ROOT = Path("/data/muscat_data/jaguir26/project1_ucsc_phd")
RUNTIME_ROOT = ROOT.parent / "project1_ucsc_phd_runtime"
OUT_DIR = ROOT / "reports" / "he2_publication_manifest"
import sys

if str(ROOT / "scripts") not in sys.path:
    sys.path.insert(0, str(ROOT / "scripts"))

from he2_exdqlm_keep_authoritative import load_authoritative_spec  # noqa: E402

CUTOFFS = ["20210123", "20211112", "20211221", "20220511", "20221225"]
CUTOFF_TO_DATE = {
    "20210123": "2021-01-23",
    "20211112": "2021-11-12",
    "20211221": "2021-12-21",
    "20220511": "2022-05-11",
    "20221225": "2022-12-25",
}
FAMILY_ORDER = [
    "ndlm_univar_keep",
    "ndlm_main_drop",
    "ndlm_main_keep",
    "dqlm_univar_al",
    "dqlm_multivar_al_drop",
    "dqlm_multivar_al_keep",
    "exdqlm_univar",
    "exdqlm_multivar_drop",
    "exdqlm_multivar_keep",
]
FAMILY_TO_LABEL = {
    "ndlm_univar_keep": "N-U-T1",
    "ndlm_main_drop": "N-M-T0",
    "ndlm_main_keep": "N-M-T1",
    "dqlm_univar_al": "AL-U-T1",
    "dqlm_multivar_al_drop": "AL-M-T0",
    "dqlm_multivar_al_keep": "AL-M-T1",
    "exdqlm_univar": "exAL-U-T1",
    "exdqlm_multivar_drop": "exAL-M-T0",
    "exdqlm_multivar_keep": "exAL-M-T1",
}
FAMILY_TO_MODEL_KEY = {
    "ndlm_univar_keep": "ndlm_univar",
    "ndlm_main_drop": "ndlm_main",
    "ndlm_main_keep": "ndlm_main",
    "dqlm_univar_al": "exdqlm_univar",
    "dqlm_multivar_al_drop": "exdqlm_multivar",
    "dqlm_multivar_al_keep": "exdqlm_multivar",
    "exdqlm_univar": "exdqlm_univar",
    "exdqlm_multivar_drop": "exdqlm_multivar",
    "exdqlm_multivar_keep": "exdqlm_multivar",
}
MODEL_ID_TO_FAMILY = {
    "exdqlm_multivar_synth_keep": "exdqlm_multivar_keep",
    "dqlm_multivar_al_synth_keep": "dqlm_multivar_al_keep",
    "exdqlm_multivar_synth_drop": "exdqlm_multivar_drop",
    "dqlm_multivar_al_synth_drop": "dqlm_multivar_al_drop",
    "exdqlm_univar_synth": "exdqlm_univar",
    "dqlm_univar_al_synth": "dqlm_univar_al",
    "ndlm_main_synth_keep": "ndlm_main_keep",
    "ndlm_main_synth_drop": "ndlm_main_drop",
    "ndlm_univar_synth_keep": "ndlm_univar_keep",
}
FAMILY_TO_MODEL_ID = {family: model_id for model_id, family in MODEL_ID_TO_FAMILY.items()}
ARTIFACT_SPECS = [
    ("parameters", "inputs/shared/parameters/parameters.txt"),
    ("retros", "inputs/shared/retros/retros.csv"),
    ("nws_forecast", "inputs/shared/forecasts/nws_forecast.csv"),
    ("glofas_forecast", "inputs/shared/forecasts/glofas_forecast.csv"),
    ("usgs_daily", "inputs/shared/usgs/usgs_daily.csv"),
    ("cov_01_PPT", "inputs/shared/covariates/cov_01_PPT.csv"),
    ("cov_02_SOIL", "inputs/shared/covariates/cov_02_SOIL.csv"),
    ("cov_03_PCA", "inputs/shared/covariates/cov_03_PCA.csv"),
    ("covariate_features", "inputs/shared/covariates/covariate_features.csv"),
    ("deterministic_precip_future", "inputs/shared/deterministic_climate/deterministic_precip_future.csv"),
    ("deterministic_soil_future", "inputs/shared/deterministic_climate/deterministic_soil_future.csv"),
]
REQUIRED_ALIGNMENT_ARTIFACTS = [
    name for name, _rel in ARTIFACT_SPECS if name != "usgs_daily"
]
CSV_FIELDS = [
    "cutoff",
    "cutoff_display",
    "manuscript_label",
    "family",
    "run_id",
    "run_root",
    "compare_dir",
    "artifact_run_id",
    "artifact_run_root",
    "resolved_config_path",
    "artifact_resolved_config_path",
    "reused_external_pass",
    "campaign_lineage",
    "publication_note",
    "replaced_source_run_id",
    "replacement_reason",
    "expected_input_bundle_id",
    "crps_exact",
    "crps_display4",
    "score_source",
    "score_scale",
    "horizon_days",
    "n_valid",
    "implementation_mode",
    "likelihood_mode",
    "forecast_transfer_mode",
    "fit_covariate_names",
    "fit_covariate_paths_json",
    "deterministic_climate_enabled",
    "deterministic_climate_json",
    "covariate_features_enabled",
    "lag_orders",
    "include_squares",
    "include_interaction",
    "covariate_features_json",
    "state_evolution_json",
    "prior_json",
    "seasonality_json",
    "within_cutoff_shared_inputs_aligned",
]
INPUT_FIELDS = [
    "cutoff",
    "manuscript_label",
    "family",
    "run_id",
    "artifact",
    "path",
    "exists",
    "sha256_16",
]
ALIGNMENT_FIELDS = [
    "cutoff",
    "artifact",
    "comparison_basis",
    "all_equal",
    "distinct_hashes",
    "missing_count",
    "hash_groups_json",
    "missing_labels",
]
AUTHORITATIVE_EXAL_KEEP_MANIFEST = ROOT / "docs" / "exdqlm_multivar_keep_authoritative_specs_20260601.yaml"
AUTHORITATIVE_EXAL_KEEP_LINEAGE = "exdqlm_multivar_keep_canonical_grid_20260524:authoritative_winner"
TRANSITION_PUBLICATION_NOTE = (
    "Authoritative canonical-grid exAL-M-T1 winner. All nine HE2 Bayesian benchmark families are now promoted "
    "onto the canonical 20260510 input-bundle contract; the full 9-model benchmark table is final for the "
    "current publication snapshot."
)
PROMOTED_AL_KEEP_ROOT = RUNTIME_ROOT / "multimodel_v8_he2_dqlm_multivar_al_keep_from_exal_winners_20260602"
PROMOTED_EXAL_DROP_ROOT = RUNTIME_ROOT / "multimodel_v8_he2_exdqlm_multivar_drop_current_relaunch_q50repair_20260602"
PROMOTED_AL_DROP_ROOT = RUNTIME_ROOT / "multimodel_v8_he2_dqlm_multivar_al_drop_p5_production_20260606"
PROMOTED_UNIVAR_AL_EXAL_ROOT = RUNTIME_ROOT / "multimodel_v8_he2_univar_al_exal_publication_relaunch_20260603"
PROMOTED_NDLM_ROOT = RUNTIME_ROOT / "multimodel_v8_he2_bayesian_publication_relaunch_wave_a_ndlm_promotion_20260607"
DEFAULT_REPLACEMENT_OVERLAY = (
    ROOT / "config" / "he2_publication_manifest_replacement_overlay_current_authority_20260623.yaml"
)
PROMOTED_FAMILY_LINEAGES = {
    "exdqlm_multivar_keep": AUTHORITATIVE_EXAL_KEEP_LINEAGE,
    "dqlm_multivar_al_keep": "dqlm_multivar_al_keep_from_exal_winners_20260602:canonical_bundle_promoted",
    "exdqlm_multivar_drop": "exdqlm_multivar_drop_current_relaunch_q50repair_20260602:canonical_bundle_promoted",
    "dqlm_multivar_al_drop": "dqlm_multivar_al_drop_p5_production_20260606:canonical_bundle_promoted",
    "dqlm_univar_al": "univar_al_exal_publication_relaunch_20260603:canonical_bundle_promoted",
    "exdqlm_univar": "univar_al_exal_publication_relaunch_20260603:canonical_bundle_promoted",
    "ndlm_univar_keep": "ndlm_publication_promotion_20260607:canonical_bundle_promoted",
    "ndlm_main_drop": "ndlm_publication_promotion_20260607:canonical_bundle_promoted",
    "ndlm_main_keep": "ndlm_publication_promotion_20260607:canonical_bundle_promoted",
}
ALLOWED_REPLACEMENT_LINEAGE_PREFIXES = {
    "he2_table1_targeted_repair_20260612:",
    "he2_univar_al_exal_scale_repair_20260629:",
    "exdqlm_multivar_keep_partial_screen_20260623:",
    "exdqlm_multivar_keep_partial_authority_refresh_20260623:",
}
ALLOWED_EXAL_KEEP_REPLACEMENT_LINEAGE_PREFIXES = {
    "exdqlm_multivar_keep_partial_screen_20260623:",
    "exdqlm_multivar_keep_partial_authority_refresh_20260623:",
}
PROMOTED_FAMILY_NOTES = {
    "exdqlm_multivar_keep": TRANSITION_PUBLICATION_NOTE,
    "dqlm_multivar_al_keep": (
        "Promoted AL-M-T1 clone of the authoritative exAL-M-T1 winner configs on the canonical 20260510 bundle. "
        "Only the likelihood mode is changed to AL; fit/post/report passed and heavy RData cleanup is complete."
    ),
    "exdqlm_multivar_drop": (
        "Promoted current-code exAL-M-T0 relaunch on the canonical 20260510 bundle with the q50 repair policy; "
        "fit/post/validate/report passed and heavy RData cleanup is complete."
    ),
    "dqlm_multivar_al_drop": (
        "Promoted P5 AL-M-T0 clone of the current-code exAL-M-T0 drop configs on the canonical 20260510 bundle. "
        "The P5 warmup/post-save policy passed fit/post/validate/report for all five cutoffs and heavy RData "
        "cleanup is complete."
    ),
    "dqlm_univar_al": (
        "Promoted AL-U-T1 canonical-bundle relaunch from 2026-06-03; fit/post/validate/report passed and "
        "heavy RData cleanup is complete."
    ),
    "exdqlm_univar": (
        "Promoted exAL-U-T1 canonical-bundle relaunch from 2026-06-03; fit/post/validate/report passed and "
        "heavy RData cleanup is complete."
    ),
    "ndlm_univar_keep": (
        "Promoted N-U-T1 NDLM canonical-bundle relaunch from 2026-06-07; fit/post/validate/report passed, "
        "the corrected noninteger seasonal harmonic is wired, merged compare-bundle rows are available, and "
        "heavy RData cleanup is complete."
    ),
    "ndlm_main_drop": (
        "Promoted N-M-T0 NDLM canonical-bundle relaunch from 2026-06-07; fit/post/validate/report passed, "
        "the corrected noninteger seasonal harmonic is wired, merged compare-bundle rows are available, and "
        "heavy RData cleanup is complete."
    ),
    "ndlm_main_keep": (
        "Promoted N-M-T1 NDLM canonical-bundle relaunch from 2026-06-07; fit/post/validate/report passed, "
        "the corrected noninteger seasonal harmonic is wired, merged compare-bundle rows are available, and "
        "heavy RData cleanup is complete."
    ),
}


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def read_yaml(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as handle:
        return yaml.safe_load(handle) or {}


def replacement_overlay_path() -> Path | None:
    raw = os.environ.get("HE2_PUBLICATION_MANIFEST_REPLACEMENT_OVERLAY")
    if raw is None:
        return DEFAULT_REPLACEMENT_OVERLAY
    value = raw.strip()
    if value.lower() in {"", "none", "disabled", "false", "0"}:
        return None
    return Path(value)


def load_replacement_overlay(path: Path | None = None) -> dict[str, Any]:
    overlay_path = replacement_overlay_path() if path is None else path
    if overlay_path is None or not overlay_path.exists():
        return {"active": False, "replacements": []}
    overlay = read_yaml(overlay_path)
    if not bool(overlay.get("active", False)):
        return {"active": False, "replacements": []}
    replacements = overlay.get("replacements") or []
    if not isinstance(replacements, list):
        raise RuntimeError(f"Replacement overlay has non-list replacements: {overlay_path}")
    overlay["replacements"] = replacements
    overlay["overlay_path"] = str(overlay_path)
    return overlay


def sha16(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()[:16]


def csv_semantically_equal(left: Path, right: Path, tol: float = 1e-12) -> tuple[bool, str]:
    with left.open("r", encoding="utf-8", newline="") as handle:
        left_rows = list(csv.DictReader(handle))
        left_fields = list(left_rows[0].keys()) if left_rows else []
    with right.open("r", encoding="utf-8", newline="") as handle:
        right_rows = list(csv.DictReader(handle))
        right_fields = list(right_rows[0].keys()) if right_rows else []
    if left_fields != right_fields:
        return False, f"field mismatch: {left_fields} != {right_fields}"
    if len(left_rows) != len(right_rows):
        return False, f"row count mismatch: {len(left_rows)} != {len(right_rows)}"
    for idx, (lrow, rrow) in enumerate(zip(left_rows, right_rows), start=1):
        for field in left_fields:
            lval = str(lrow.get(field, ""))
            rval = str(rrow.get(field, ""))
            try:
                lf = float(lval)
                rf = float(rval)
            except ValueError:
                if lval != rval:
                    return False, f"row {idx} field {field} text mismatch: {lval!r} != {rval!r}"
            else:
                if abs(lf - rf) > tol:
                    return False, f"row {idx} field {field} numeric mismatch: {lf} != {rf}"
    return True, "ok"


def canonical_artifact_path(bundle_root: Path, bundle_run_id: str, cutoff: str, artifact: str) -> Path:
    stable = bundle_root / "stable_inputs" / "site=11160500" / f"cutoff_date={CUTOFF_TO_DATE[cutoff]}" / f"run_id={bundle_run_id}"
    support = bundle_root / "supporting_inputs"
    mapping = {
        "parameters": support / "parameters" / "parameters.txt",
        "retros": stable / "retros.csv",
        "nws_forecast": stable / "nws_forecast.csv",
        "glofas_forecast": stable / "glofas_forecast.csv",
        "cov_01_PPT": support / "covariates" / "cov_03_PPT.csv",
        "cov_02_SOIL": support / "covariates" / "cov_04_SOIL.csv",
        "cov_03_PCA": support / "covariates" / "cov_05_PCA.csv",
    }
    try:
        return mapping[artifact]
    except KeyError as exc:
        raise ValueError(f"no canonical artifact mapping for {artifact}") from exc


def cutoff_display(cutoff: str) -> str:
    return datetime.strptime(cutoff, "%Y%m%d").strftime("%m/%d/%Y")


def display4(value: float) -> str:
    return f"{value:.4f}"


def json_compact(obj: Any) -> str:
    return json.dumps(obj, sort_keys=True, separators=(",", ":"))


def model_prior_payload(cfg: dict[str, Any], model_key: str) -> dict[str, Any]:
    """Expose the fitted prior knobs that are otherwise nested under legacy fit config."""
    model_cfg = (cfg.get("models") or {}).get(model_key) or {}
    payload: dict[str, Any] = dict(model_cfg.get("prior") or {})
    legacy = ((cfg.get("fit") or {}).get(model_key) or {}).get("legacy") or {}
    forecast_cov = legacy.get("forecast_cov")
    if isinstance(forecast_cov, dict) and forecast_cov:
        payload["forecast_cov"] = forecast_cov
    return payload


def as_existing_path(value: object) -> Path | None:
    text = str(value or "").strip()
    if not text:
        return None
    path = Path(text)
    if path.exists() and path.is_file():
        return path
    return None


def resolve_artifact(run_root: Path, rel: str, cfg: dict[str, Any]) -> Path | None:
    direct = run_root / rel
    if direct.exists() and direct.is_file():
        return direct

    fit = (cfg.get("inputs") or {}).get("fit") or {}
    if rel == "inputs/shared/parameters/parameters.txt":
        return as_existing_path(fit.get("parameters_path", ""))
    if rel == "inputs/shared/retros/retros.csv":
        return as_existing_path(fit.get("retros_path", ""))
    if rel == "inputs/shared/forecasts/nws_forecast.csv":
        return as_existing_path(fit.get("nws_forecast_path", ""))
    if rel == "inputs/shared/forecasts/glofas_forecast.csv":
        return as_existing_path(fit.get("glofas_forecast_path", ""))
    if rel == "inputs/shared/usgs/usgs_daily.csv":
        return as_existing_path(fit.get("usgs_cache_path", ""))
    if rel.startswith("inputs/shared/covariates/"):
        name_map = {
            "inputs/shared/covariates/cov_01_PPT.csv": "PPT",
            "inputs/shared/covariates/cov_02_SOIL.csv": "SOIL",
            "inputs/shared/covariates/cov_03_PCA.csv": "PCA",
        }
        wanted = name_map.get(rel)
        if wanted:
            for cov in fit.get("covariates") or []:
                if cov.get("name") == wanted:
                    return as_existing_path(cov.get("path", ""))
    return None


def manifest_stage_statuses(run_root: Path) -> dict[str, str]:
    manifest_path = run_root / "run_manifest.yaml"
    if not manifest_path.exists():
        raise FileNotFoundError(f"Missing promoted run manifest: {manifest_path}")
    manifest = read_yaml(manifest_path)
    stages = manifest.get("stages") or {}
    return {
        stage: str((stages.get(stage) or {}).get("status", "")).strip().lower()
        for stage in ["fit", "post", "validate", "report"]
    }


def retained_heavy_artifacts(run_root: Path) -> list[Path]:
    patterns = ["*.RData", "*.rda", "*.Rda"]
    out: list[Path] = []
    for pattern in patterns:
        out.extend(path for path in run_root.rglob(pattern) if path.is_file())
    return sorted(out)


def artifact_context(pointer_run_root: Path) -> tuple[Path, Path, bool]:
    reuse_pointer = pointer_run_root / "reuse_pointer.yaml"
    if reuse_pointer.exists():
        pointer_cfg = read_yaml(reuse_pointer)
        artifact_root = Path(str(pointer_cfg["reuse_source_run_root"]))
        artifact_cfg = Path(str(pointer_cfg["reuse_source_config"]))
        return artifact_root, artifact_cfg, True
    return pointer_run_root, pointer_run_root / "resolved_config.yaml", False


def score_row_for_family(run_root: Path, family: str) -> dict[str, str]:
    table = run_root / "post" / "outputs" / run_root.name / "tables" / "crps_forecast_summary.csv"
    rows = read_csv(table)
    for row in rows:
        if row.get("model_variant") == family:
            return row
    raise ValueError(f"Missing model_variant={family} in {table}")


def compare_score_row(compare_dir: Path, family: str) -> dict[str, str]:
    table = compare_dir / "crps_forecast_summary_all_models.csv"
    rows = read_csv(table)
    model_id = FAMILY_TO_MODEL_ID[family]
    for row in rows:
        if row.get("model_id") == model_id:
            return row
    raise ValueError(f"Missing model_id={model_id} in {table}")


def resolve_multivar_rows() -> list[dict[str, str]]:
    authoritative = load_authoritative_spec(AUTHORITATIVE_EXAL_KEEP_MANIFEST)
    authoritative_by_cutoff = authoritative.winner_by_cutoff()
    rows = []
    for cutoff in CUTOFFS:
        winner = authoritative_by_cutoff[cutoff]
        rows.append(
            {
                "cutoff": cutoff,
                "family": "exdqlm_multivar_keep",
                "run_id": winner.run_id,
                "run_root": str(authoritative.run_root(winner)),
                "compare_dir": "",
                "campaign_lineage": PROMOTED_FAMILY_LINEAGES["exdqlm_multivar_keep"],
                "publication_note": PROMOTED_FAMILY_NOTES["exdqlm_multivar_keep"],
                "replaced_source_run_id": "",
            }
        )
        al_keep_run_id = f"multimodel_{cutoff}_v8_he2grid_{winner.grid_spec_id}_dqlm_multivar_al_keep"
        rows.append(
            {
                "cutoff": cutoff,
                "family": "dqlm_multivar_al_keep",
                "run_id": al_keep_run_id,
                "run_root": str(PROMOTED_AL_KEEP_ROOT / "runs" / al_keep_run_id),
                "compare_dir": "",
                "campaign_lineage": PROMOTED_FAMILY_LINEAGES["dqlm_multivar_al_keep"],
                "publication_note": PROMOTED_FAMILY_NOTES["dqlm_multivar_al_keep"],
                "replaced_source_run_id": "",
            }
        )
        exal_drop_run_id = f"multimodel_{cutoff}_v8_he2pubgdpc1r1_exdqlm_multivar_drop"
        rows.append(
            {
                "cutoff": cutoff,
                "family": "exdqlm_multivar_drop",
                "run_id": exal_drop_run_id,
                "run_root": str(PROMOTED_EXAL_DROP_ROOT / "runs" / exal_drop_run_id),
                "compare_dir": "",
                "campaign_lineage": PROMOTED_FAMILY_LINEAGES["exdqlm_multivar_drop"],
                "publication_note": PROMOTED_FAMILY_NOTES["exdqlm_multivar_drop"],
                "replaced_source_run_id": "",
            }
        )
        al_drop_run_id = f"multimodel_{cutoff}_v8_he2pubgdpc1r1_dqlm_multivar_al_drop"
        rows.append(
            {
                "cutoff": cutoff,
                "family": "dqlm_multivar_al_drop",
                "run_id": al_drop_run_id,
                "run_root": str(PROMOTED_AL_DROP_ROOT / "runs" / al_drop_run_id),
                "compare_dir": "",
                "campaign_lineage": PROMOTED_FAMILY_LINEAGES["dqlm_multivar_al_drop"],
                "publication_note": PROMOTED_FAMILY_NOTES["dqlm_multivar_al_drop"],
                "replaced_source_run_id": "",
            }
        )
    return rows


def resolve_univar_rows() -> list[dict[str, str]]:
    rows = []
    for cutoff in CUTOFFS:
        for family in ["dqlm_univar_al", "exdqlm_univar"]:
            run_id = f"multimodel_{cutoff}_v8_he2pubgdpc1r1_{family}"
            rows.append(
                {
                    "cutoff": cutoff,
                    "family": family,
                    "run_id": run_id,
                    "run_root": str(PROMOTED_UNIVAR_AL_EXAL_ROOT / "runs" / run_id),
                    "compare_dir": "",
                    "campaign_lineage": PROMOTED_FAMILY_LINEAGES[family],
                    "publication_note": PROMOTED_FAMILY_NOTES[family],
                    "replaced_source_run_id": "",
                }
            )
    return rows


def resolve_ndlm_rows() -> list[dict[str, str]]:
    rows = []
    for cutoff in CUTOFFS:
        compare_dir = PROMOTED_NDLM_ROOT / "reports" / f"multimodel_{cutoff}_v8_he2pubgdpc1r1_compare"
        if not compare_dir.exists():
            raise FileNotFoundError(f"Missing promoted NDLM compare bundle: {compare_dir}")
        for family in ["ndlm_univar_keep", "ndlm_main_drop", "ndlm_main_keep"]:
            run_id = f"multimodel_{cutoff}_v8_he2pubgdpc1r1_{family}"
            rows.append(
                {
                    "cutoff": cutoff,
                    "family": family,
                    "run_id": run_id,
                    "run_root": str(PROMOTED_NDLM_ROOT / "runs" / run_id),
                    "compare_dir": str(compare_dir),
                    "campaign_lineage": PROMOTED_FAMILY_LINEAGES[family],
                    "publication_note": PROMOTED_FAMILY_NOTES[family],
                    "replaced_source_run_id": "",
                }
            )
    return rows


def apply_replacement_overlay(rows: list[dict[str, str]], overlay: dict[str, Any]) -> list[dict[str, str]]:
    if not bool(overlay.get("active", False)):
        return rows
    artifact_root = Path(str(overlay.get("artifact_root", ""))).expanduser()
    campaign_lineage = str(overlay.get("campaign_lineage", "")).strip()
    publication_note = str(overlay.get("publication_note", "")).strip()
    replacement_reason = str(overlay.get("replacement_reason", "")).strip()
    expected_input_bundle_id = str(overlay.get("expected_input_bundle_id", "")).strip()
    if not artifact_root.is_absolute():
        raise RuntimeError(f"Replacement overlay artifact_root must be absolute: {artifact_root}")
    if not campaign_lineage:
        raise RuntimeError("Replacement overlay must define campaign_lineage")
    if not replacement_reason:
        raise RuntimeError("Replacement overlay must define replacement_reason")
    if not expected_input_bundle_id:
        raise RuntimeError("Replacement overlay must define expected_input_bundle_id")

    by_key = {(row["cutoff"], row["family"]): dict(row) for row in rows}
    seen: set[tuple[str, str]] = set()
    label_to_family = {label: family for family, label in FAMILY_TO_LABEL.items()}
    for repl in overlay["replacements"]:
        if not isinstance(repl, dict):
            raise RuntimeError(f"Replacement overlay row is not a mapping: {repl!r}")
        cutoff = str(repl.get("cutoff", "")).strip()
        family = str(repl.get("family", "")).strip()
        manuscript_label = str(repl.get("manuscript_label", "")).strip()
        run_id = str(repl.get("run_id", "")).strip()
        if not family and manuscript_label:
            family = label_to_family.get(manuscript_label, "")
        if not cutoff or not family or not run_id:
            raise RuntimeError(f"Replacement overlay row must define cutoff, family/label, and run_id: {repl!r}")
        if manuscript_label and FAMILY_TO_LABEL.get(family) != manuscript_label:
            raise RuntimeError(
                f"Replacement overlay label/family mismatch for {run_id}: "
                f"{manuscript_label} != {FAMILY_TO_LABEL.get(family)}"
            )
        key = (cutoff, family)
        if key in seen:
            raise RuntimeError(f"Duplicate replacement overlay key: cutoff={cutoff} family={family}")
        if key not in by_key:
            raise RuntimeError(f"Replacement overlay key not present in manifest base rows: cutoff={cutoff} family={family}")
        seen.add(key)

        old = by_key[key]
        run_root = str(repl.get("run_root", "")).strip()
        if not run_root:
            run_root = str(artifact_root / "runs" / run_id)
        compare_dir = str(repl.get("compare_dir", "")).strip()
        by_key[key] = {
            **old,
            "run_id": run_id,
            "run_root": run_root,
            "compare_dir": compare_dir,
            "campaign_lineage": str(repl.get("campaign_lineage", campaign_lineage)).strip(),
            "publication_note": str(repl.get("publication_note", publication_note)).strip(),
            "replaced_source_run_id": str(repl.get("replaced_source_run_id", old["run_id"])).strip(),
            "replacement_reason": str(repl.get("replacement_reason", replacement_reason)).strip(),
            "expected_input_bundle_id": str(repl.get("expected_input_bundle_id", expected_input_bundle_id)).strip(),
        }
    return sorted(by_key.values(), key=lambda row: (row["cutoff"], FAMILY_ORDER.index(row["family"])))


def build_resolved_rows() -> list[dict[str, str]]:
    rows = resolve_multivar_rows() + resolve_univar_rows() + resolve_ndlm_rows()
    rows = apply_replacement_overlay(rows, load_replacement_overlay())
    rows = sorted(rows, key=lambda row: (row["cutoff"], FAMILY_ORDER.index(row["family"])))
    if len(rows) != 45:
        raise RuntimeError(f"Expected 45 publication rows, found {len(rows)}")
    return rows


def build_outputs() -> tuple[list[dict[str, str]], list[dict[str, str]], list[dict[str, str]]]:
    manifest_rows: list[dict[str, str]] = []
    input_rows: list[dict[str, str]] = []

    resolved_rows = build_resolved_rows()
    for row in resolved_rows:
        run_root = Path(row["run_root"])
        cfg_path = run_root / "resolved_config.yaml"
        artifact_root, artifact_cfg_path, reused = artifact_context(run_root)
        cfg = read_yaml(artifact_cfg_path)
        fit = (cfg.get("inputs") or {}).get("fit") or {}
        det = (cfg.get("inputs") or {}).get("deterministic_climate") or {}
        covfeat = (cfg.get("inputs") or {}).get("covariate_features") or {}
        fit_covariates = fit.get("covariates") or []
        cov_names = [cov.get("name", "") for cov in fit_covariates if cov.get("name")]
        cov_paths = {cov.get("name", ""): cov.get("path", "") for cov in fit_covariates if cov.get("name")}
        model_cfg = (cfg.get("models") or {}).get(FAMILY_TO_MODEL_KEY[row["family"]]) or {}
        local_score_table = artifact_root / "post" / "outputs" / artifact_root.name / "tables" / "crps_forecast_summary.csv"
        if local_score_table.exists():
            score = score_row_for_family(artifact_root, row["family"])
            score_source = str(local_score_table)
        else:
            score = compare_score_row(Path(row["compare_dir"]), row["family"])
            score_source = str(Path(row["compare_dir"]) / "crps_forecast_summary_all_models.csv")

        for artifact, rel in ARTIFACT_SPECS:
            path = resolve_artifact(artifact_root, rel, cfg)
            input_rows.append(
                {
                    "cutoff": row["cutoff"],
                    "manuscript_label": FAMILY_TO_LABEL[row["family"]],
                    "family": row["family"],
                    "run_id": row["run_id"],
                    "artifact": artifact,
                    "path": str(path) if path else "",
                    "exists": "True" if path and path.exists() else "False",
                    "sha256_16": sha16(path) if path and path.exists() else "",
                }
            )

        manifest_rows.append(
            {
                "cutoff": row["cutoff"],
                "cutoff_display": cutoff_display(row["cutoff"]),
                "manuscript_label": FAMILY_TO_LABEL[row["family"]],
                "family": row["family"],
                "run_id": row["run_id"],
                "run_root": str(run_root),
                "compare_dir": row["compare_dir"],
                "artifact_run_id": artifact_root.name,
                "artifact_run_root": str(artifact_root),
                "resolved_config_path": str(cfg_path),
                "artifact_resolved_config_path": str(artifact_cfg_path),
                "reused_external_pass": str(reused),
                "campaign_lineage": row["campaign_lineage"],
                "publication_note": row["publication_note"],
                "replaced_source_run_id": row["replaced_source_run_id"],
                "replacement_reason": row.get("replacement_reason", ""),
                "expected_input_bundle_id": row.get("expected_input_bundle_id", ""),
                "crps_exact": str(float(score["mean_crps"])),
                "crps_display4": display4(float(score["mean_crps"])),
                "score_source": score_source,
                "score_scale": score["score_scale"],
                "horizon_days": score["horizon_days"],
                "n_valid": score["n_valid"],
                "implementation_mode": str(model_cfg.get("implementation_mode", "")),
                "likelihood_mode": str(model_cfg.get("likelihood_mode", "normal")),
                "forecast_transfer_mode": str(model_cfg.get("forecast_transfer_mode", "")),
                "fit_covariate_names": "|".join(cov_names),
                "fit_covariate_paths_json": json_compact(cov_paths),
                "deterministic_climate_enabled": str(bool(det.get("enabled", False))),
                "deterministic_climate_json": json_compact(det),
                "covariate_features_enabled": str(bool(covfeat.get("enabled", False))),
                "lag_orders": "|".join(str(x) for x in (covfeat.get("lag_orders") or [])),
                "include_squares": str(bool(covfeat.get("include_squares", False))),
                "include_interaction": str(bool(covfeat.get("include_interaction", False))),
                "covariate_features_json": json_compact(covfeat),
                "state_evolution_json": json_compact(model_cfg.get("state_evolution") or {}),
                "prior_json": json_compact(model_prior_payload(cfg, FAMILY_TO_MODEL_KEY[row["family"]])),
                "seasonality_json": json_compact(model_cfg.get("seasonality") or {}),
                "within_cutoff_shared_inputs_aligned": "",
            }
        )

    authoritative = load_authoritative_spec(AUTHORITATIVE_EXAL_KEEP_MANIFEST)
    canonical_bundle_root = Path(str(authoritative.metadata.get("bundle_artifact_root", "")))
    canonical_bundle_run_id = str(authoritative.metadata.get("bundle_run_id", ""))

    alignment_rows: list[dict[str, str]] = []
    for cutoff in CUTOFFS:
        cutoff_inputs = [row for row in input_rows if row["cutoff"] == cutoff]
        for artifact, _rel in ARTIFACT_SPECS:
            subset = [row for row in cutoff_inputs if row["artifact"] == artifact]
            groups: dict[str, list[str]] = defaultdict(list)
            for item in subset:
                if item["exists"] == "True":
                    groups[item["sha256_16"]].append(item["manuscript_label"])
            missing = [item["manuscript_label"] for item in subset if item["exists"] != "True"]
            comparison_basis = "sha256_16"
            all_equal = len(groups) == 1 and not missing
            if artifact == "retros" and not missing:
                comparison_basis = "semantic_csv_against_canonical_bundle"
                canonical_path = canonical_artifact_path(canonical_bundle_root, canonical_bundle_run_id, cutoff, artifact)
                semantic_results = [
                    csv_semantically_equal(Path(item["path"]), canonical_path, tol=1e-10)[0]
                    for item in subset
                ]
                all_equal = all(semantic_results)
            alignment_rows.append(
                {
                    "cutoff": cutoff,
                    "artifact": artifact,
                    "comparison_basis": comparison_basis,
                    "all_equal": "True" if all_equal else "False",
                    "distinct_hashes": str(len(groups)),
                    "missing_count": str(len(missing)),
                    "hash_groups_json": json_compact({k: sorted(v) for k, v in groups.items()}),
                    "missing_labels": "|".join(sorted(missing)),
                }
            )

    aligned_cutoffs = {
        cutoff: all(
            row["all_equal"] == "True"
            for row in alignment_rows
            if row["cutoff"] == cutoff and row["artifact"] in REQUIRED_ALIGNMENT_ARTIFACTS
        )
        for cutoff in CUTOFFS
    }
    for row in manifest_rows:
        row["within_cutoff_shared_inputs_aligned"] = str(aligned_cutoffs[row["cutoff"]])

    return manifest_rows, input_rows, alignment_rows


def validate(
    manifest_rows: list[dict[str, str]],
    input_rows: list[dict[str, str]],
    alignment_rows: list[dict[str, str]],
) -> None:
    authoritative = load_authoritative_spec(AUTHORITATIVE_EXAL_KEEP_MANIFEST)
    authoritative_by_cutoff = authoritative.winner_by_cutoff()
    canonical_bundle_root = Path(str(authoritative.metadata.get("bundle_artifact_root", "")))
    canonical_bundle_run_id = str(authoritative.metadata.get("bundle_run_id", ""))
    if len(manifest_rows) != 45:
        raise RuntimeError(f"Expected 45 manifest rows, found {len(manifest_rows)}")
    observed_labels = sorted({row["manuscript_label"] for row in manifest_rows})
    if observed_labels != sorted(FAMILY_TO_LABEL.values()):
        raise RuntimeError(f"Unexpected manuscript labels: {observed_labels}")
    for row in manifest_rows:
        if row["fit_covariate_names"] != "PPT|SOIL|PCA":
            raise RuntimeError(f"Unexpected covariate contract in {row['run_id']}: {row['fit_covariate_names']}")
        if row["deterministic_climate_enabled"] != "True":
            raise RuntimeError(f"Deterministic climate disabled in {row['run_id']}")
        if row["covariate_features_enabled"] != "True":
            raise RuntimeError(f"Covariate features disabled in {row['run_id']}")
        if row["lag_orders"] != "1|2|3":
            raise RuntimeError(f"Unexpected lag orders in {row['run_id']}: {row['lag_orders']}")
        if row["include_squares"] != "True" or row["include_interaction"] != "True":
            raise RuntimeError(f"Feature transform contract failed in {row['run_id']}")
    for cutoff, winner in authoritative_by_cutoff.items():
        row = next(item for item in manifest_rows if item["cutoff"] == cutoff and item["manuscript_label"] == "exAL-M-T1")
        is_replacement = bool(row.get("replacement_reason", ""))
        if not is_replacement:
            if row["run_id"] != winner.run_id:
                raise RuntimeError(f"Unexpected authoritative exAL-M-T1 run for {cutoff}: {row['run_id']} != {winner.run_id}")
            if abs(float(row["crps_exact"]) - float(winner.mean_crps)) > 1e-12:
                raise RuntimeError(f"Unexpected authoritative exAL-M-T1 CRPS for {cutoff}: {row['crps_exact']} != {winner.mean_crps}")
            if row["campaign_lineage"] != AUTHORITATIVE_EXAL_KEEP_LINEAGE:
                raise RuntimeError(f"Unexpected authoritative exAL-M-T1 lineage for {cutoff}: {row['campaign_lineage']}")
        else:
            if not any(
                row["campaign_lineage"].startswith(prefix)
                for prefix in ALLOWED_EXAL_KEEP_REPLACEMENT_LINEAGE_PREFIXES
            ):
                raise RuntimeError(f"Unexpected exAL-M-T1 replacement lineage for {row['run_id']}: {row['campaign_lineage']}")
            if not row.get("replaced_source_run_id", ""):
                raise RuntimeError(f"Replacement exAL-M-T1 row missing replaced source run id: {row['run_id']}")
            if row.get("expected_input_bundle_id", "") != canonical_bundle_run_id:
                raise RuntimeError(
                    f"Replacement exAL-M-T1 expected bundle mismatch for {row['run_id']}: "
                    f"{row.get('expected_input_bundle_id', '')} != {canonical_bundle_run_id}"
                )
            if float(row["crps_exact"]) > float(winner.mean_crps):
                raise RuntimeError(
                    f"Replacement exAL-M-T1 CRPS is worse than 20260601 authority for {cutoff}: "
                    f"{row['crps_exact']} > {winner.mean_crps}"
                )
        prior = json.loads(row["prior_json"] or "{}")
        forecast_cov = prior.get("forecast_cov") or {}
        if "epsilon" not in forecast_cov or "c_factor" not in forecast_cov:
            raise RuntimeError(f"Authoritative exAL-M-T1 row is missing forecast_cov prior fields: {row['run_id']}")
        if abs(float(forecast_cov["c_factor"]) - 1.0) > 1e-12:
            raise RuntimeError(f"Unexpected exAL-M-T1 c_factor for {cutoff}: {forecast_cov['c_factor']}")

    expected_promoted = {
        "ndlm_univar_keep": {"label": "N-U-T1", "likelihood": "normal", "transfer": "keep"},
        "ndlm_main_drop": {"label": "N-M-T0", "likelihood": "normal", "transfer": "drop"},
        "ndlm_main_keep": {"label": "N-M-T1", "likelihood": "normal", "transfer": "keep"},
        "exdqlm_multivar_keep": {"label": "exAL-M-T1", "likelihood": "exal", "transfer": "keep"},
        "dqlm_multivar_al_keep": {"label": "AL-M-T1", "likelihood": "al", "transfer": "keep"},
        "exdqlm_multivar_drop": {"label": "exAL-M-T0", "likelihood": "exal", "transfer": "drop"},
        "dqlm_multivar_al_drop": {"label": "AL-M-T0", "likelihood": "al", "transfer": "drop"},
        "dqlm_univar_al": {"label": "AL-U-T1", "likelihood": "al", "transfer": ""},
        "exdqlm_univar": {"label": "exAL-U-T1", "likelihood": "exal", "transfer": ""},
    }
    canonical_source_hash_artifacts = {
        "parameters",
        "nws_forecast",
        "glofas_forecast",
    }
    canonical_semantic_csv_artifacts = {"retros"}
    canonical_covariate_artifacts = {
        "cov_01_PPT",
        "cov_02_SOIL",
        "cov_03_PCA",
    }
    input_by_run_artifact = {(row["run_id"], row["artifact"]): row for row in input_rows}
    promoted_labels = {expected["label"] for expected in expected_promoted.values()}
    for cutoff in CUTOFFS:
        for artifact in REQUIRED_ALIGNMENT_ARTIFACTS:
            subset = [
                row
                for row in input_rows
                if row["cutoff"] == cutoff
                and row["manuscript_label"] in promoted_labels
                and row["artifact"] == artifact
            ]
            hashes = {row["sha256_16"] for row in subset if row["exists"] == "True"}
            missing = [row["run_id"] for row in subset if row["exists"] != "True"]
            if len(subset) != len(promoted_labels) or missing:
                raise RuntimeError(
                    f"Promoted rows have missing materialized input for cutoff={cutoff} "
                    f"artifact={artifact}: missing={missing}"
                )
            if artifact in canonical_semantic_csv_artifacts:
                canonical_path = canonical_artifact_path(canonical_bundle_root, canonical_bundle_run_id, cutoff, artifact)
                semantic_failures = []
                for item in subset:
                    ok, detail = csv_semantically_equal(Path(item["path"]), canonical_path, tol=1e-10)
                    if not ok:
                        semantic_failures.append(f"{item['run_id']}:{detail}")
                if semantic_failures:
                    raise RuntimeError(
                        f"Promoted semantic CSV inputs do not match canonical bundle for cutoff={cutoff} "
                        f"artifact={artifact}: {semantic_failures}"
                    )
            elif len(hashes) != 1:
                raise RuntimeError(
                    f"Promoted rows do not share one materialized input for cutoff={cutoff} "
                    f"artifact={artifact}: hashes={sorted(hashes)} missing={missing}"
                )
    for family, expected in expected_promoted.items():
        rows = [row for row in manifest_rows if row["family"] == family]
        if len(rows) != len(CUTOFFS):
            raise RuntimeError(f"Expected {len(CUTOFFS)} promoted rows for {family}, found {len(rows)}")
        for row in rows:
            if row["manuscript_label"] != expected["label"]:
                raise RuntimeError(f"Unexpected promoted label for {row['run_id']}: {row['manuscript_label']}")
            is_replacement = bool(row.get("replacement_reason", ""))
            if not is_replacement and row["campaign_lineage"] != PROMOTED_FAMILY_LINEAGES[family]:
                raise RuntimeError(f"Unexpected promoted lineage for {row['run_id']}: {row['campaign_lineage']}")
            if is_replacement:
                if not any(row["campaign_lineage"].startswith(prefix) for prefix in ALLOWED_REPLACEMENT_LINEAGE_PREFIXES):
                    raise RuntimeError(f"Unexpected replacement lineage for {row['run_id']}: {row['campaign_lineage']}")
                if row.get("expected_input_bundle_id", "") != canonical_bundle_run_id:
                    raise RuntimeError(
                        f"Replacement row expected bundle mismatch for {row['run_id']}: "
                        f"{row.get('expected_input_bundle_id', '')} != {canonical_bundle_run_id}"
                    )
                if not row.get("replaced_source_run_id", ""):
                    raise RuntimeError(f"Replacement row missing replaced source run id: {row['run_id']}")
            if row["likelihood_mode"] != expected["likelihood"]:
                raise RuntimeError(f"Unexpected likelihood for {row['run_id']}: {row['likelihood_mode']}")
            if row["forecast_transfer_mode"] != expected["transfer"]:
                raise RuntimeError(f"Unexpected transfer mode for {row['run_id']}: {row['forecast_transfer_mode']}")
            artifact_root = Path(row["artifact_run_root"])
            stage_statuses = manifest_stage_statuses(artifact_root)
            if any(stage_statuses[stage] != "pass" for stage in ["fit", "post", "validate", "report"]):
                raise RuntimeError(f"Promoted run has non-pass stages: {row['run_id']} {stage_statuses}")
            heavy = retained_heavy_artifacts(artifact_root)
            if heavy:
                raise RuntimeError(f"Promoted run retained heavy RData artifacts: {row['run_id']} {heavy[:3]}")
            score_source = Path(row["score_source"])
            if not score_source.exists():
                raise RuntimeError(f"Promoted CRPS table missing: {score_source}")
            if family.startswith("ndlm_"):
                output_root = artifact_root / "post" / "outputs" / artifact_root.name
                required_ndlm_post = [
                    output_root / "All_ELBOS_DISC.png",
                    output_root / "ndlm_fit_recent_log1p.png",
                    output_root / "ndlm_forecast_window_quantiles_raw_cms.png",
                    output_root / "post_artifacts_manifest.csv",
                    output_root / "post_artifacts_summary.json",
                    output_root / "tables" / "crps_forecast_summary.csv",
                    output_root / "tables" / "crps_input_health.csv",
                    output_root / "tables" / "posterior_table_exports_manifest.csv",
                ]
                missing_ndlm_post = [str(path) for path in required_ndlm_post if not path.exists()]
                if missing_ndlm_post:
                    raise RuntimeError(f"Promoted NDLM post artifacts missing: {row['run_id']} {missing_ndlm_post}")
                if row["compare_dir"]:
                    compare_dir = Path(row["compare_dir"])
                    required_compare = [
                        compare_dir / "crps_forecast_summary_all_models.csv",
                        compare_dir / "crps_input_health_all_models.csv",
                        compare_dir / "figure_manifest.csv",
                        compare_dir / "model_coverage.csv",
                        compare_dir / "source_provenance.csv",
                        compare_dir / "summary.md",
                    ]
                    missing_compare = [str(path) for path in required_compare if not path.exists()]
                    if missing_compare:
                        raise RuntimeError(f"Promoted NDLM compare bundle missing artifacts: {row['run_id']} {missing_compare}")
                    compare_score = compare_score_row(compare_dir, family)
                    if abs(float(compare_score["mean_crps"]) - float(row["crps_exact"])) > 1e-12:
                        raise RuntimeError(
                            f"Promoted NDLM compare CRPS does not match run-local CRPS: "
                            f"{row['run_id']} compare={compare_score['mean_crps']} local={row['crps_exact']}"
                        )
            else:
                figure_manifest = artifact_root / "post" / "outputs" / artifact_root.name / "publication_figure_manifest.csv"
                if not figure_manifest.exists():
                    raise RuntimeError(f"Promoted figure manifest missing: {figure_manifest}")
            for artifact in canonical_source_hash_artifacts:
                input_row = input_by_run_artifact.get((row["run_id"], artifact))
                if not input_row or input_row["exists"] != "True":
                    raise RuntimeError(f"Promoted canonical artifact missing: {row['run_id']} {artifact}")
                canonical_path = canonical_artifact_path(canonical_bundle_root, canonical_bundle_run_id, row["cutoff"], artifact)
                if not canonical_path.exists():
                    raise RuntimeError(f"Canonical bundle artifact missing: {canonical_path}")
                canonical_hash = sha16(canonical_path)
                if input_row["sha256_16"] != canonical_hash:
                    raise RuntimeError(
                        f"Promoted artifact content does not match canonical bundle: "
                        f"{row['run_id']} {artifact} observed={input_row['sha256_16']} "
                        f"canonical={canonical_hash} observed_path={input_row['path']} canonical_path={canonical_path}"
                    )
            for artifact in canonical_semantic_csv_artifacts:
                input_row = input_by_run_artifact.get((row["run_id"], artifact))
                if not input_row or input_row["exists"] != "True":
                    raise RuntimeError(f"Promoted canonical artifact missing: {row['run_id']} {artifact}")
                canonical_path = canonical_artifact_path(canonical_bundle_root, canonical_bundle_run_id, row["cutoff"], artifact)
                if not canonical_path.exists():
                    raise RuntimeError(f"Canonical bundle artifact missing: {canonical_path}")
                ok, detail = csv_semantically_equal(Path(input_row["path"]), canonical_path, tol=1e-10)
                if not ok:
                    raise RuntimeError(
                        f"Promoted semantic CSV artifact does not match canonical bundle: "
                        f"{row['run_id']} {artifact} detail={detail} observed_path={input_row['path']} "
                        f"canonical_path={canonical_path}"
                    )
            cfg = read_yaml(Path(row["artifact_resolved_config_path"]))
            fit_covariates = {
                cov.get("name"): str(cov.get("path", ""))
                for cov in ((cfg.get("inputs") or {}).get("fit") or {}).get("covariates") or []
            }
            cov_name_by_artifact = {
                "cov_01_PPT": "PPT",
                "cov_02_SOIL": "SOIL",
                "cov_03_PCA": "PCA",
            }
            for artifact in canonical_covariate_artifacts:
                cov_name = cov_name_by_artifact[artifact]
                canonical_path = canonical_artifact_path(canonical_bundle_root, canonical_bundle_run_id, row["cutoff"], artifact)
                if fit_covariates.get(cov_name) != str(canonical_path):
                    raise RuntimeError(
                        f"Promoted config covariate source is not canonical: "
                        f"{row['run_id']} {cov_name} observed={fit_covariates.get(cov_name)} canonical={canonical_path}"
                    )


def write_csv(path: Path, rows: list[dict[str, str]], fieldnames: list[str]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def markdown_table(headers: list[str], rows: list[list[str]]) -> str:
    head = "| " + " | ".join(headers) + " |"
    sep = "|" + "|".join(["---"] * len(headers)) + "|"
    body = ["| " + " | ".join(row) + " |" for row in rows]
    return "\n".join([head, sep, *body])


def write_markdown(manifest_rows: list[dict[str, str]], alignment_rows: list[dict[str, str]]) -> None:
    cutoff_rows = []
    for cutoff in CUTOFFS:
        aligned = sum(
            1
            for row in alignment_rows
            if row["cutoff"] == cutoff
            and row["artifact"] in REQUIRED_ALIGNMENT_ARTIFACTS
            and row["all_equal"] == "True"
        )
        result = "Aligned" if aligned == len(REQUIRED_ALIGNMENT_ARTIFACTS) else "Transition mismatch"
        cutoff_rows.append([cutoff_display(cutoff), f"{aligned} / {len(REQUIRED_ALIGNMENT_ARTIFACTS)}", result])

    current_rows = []
    promoted_rows = []
    for row in manifest_rows:
        promoted = row["campaign_lineage"] in set(PROMOTED_FAMILY_LINEAGES.values()) or bool(row.get("replacement_reason", ""))
        note = "targeted repair replacement" if row.get("replacement_reason", "") else ("canonical-bundle promoted" if promoted else "")
        if promoted:
            promoted_rows.append([row["cutoff_display"], row["manuscript_label"], row["crps_display4"], row["run_id"]])
        current_rows.append(
            [
                row["cutoff_display"],
                row["manuscript_label"],
                row["crps_display4"],
                row["run_id"],
                row["campaign_lineage"],
                note,
            ]
        )

    unique_likelihoods = sorted({row["likelihood_mode"] for row in manifest_rows})
    aligned_full = sum(
        1
        for row in alignment_rows
        if row["artifact"] in REQUIRED_ALIGNMENT_ARTIFACTS and row["all_equal"] == "True"
    )
    total_full = len(CUTOFFS) * len(REQUIRED_ALIGNMENT_ARTIFACTS)

    md = f"""# HE2 Bayesian Publication Manifest

This report freezes the **current manuscript-facing HE2 Bayesian table** at the run level for all `9 x 5 = 45` cells.

Headline checks:
- published Bayesian HE2 cells documented: `{len(manifest_rows)}`
- cutoffs documented: `{len(CUTOFFS)}`
- canonical-bundle promoted cells: `{len(promoted_rows)}`
- remaining transition cells: `0`
- required shared-input artifacts checked within each cutoff: `{len(REQUIRED_ALIGNMENT_ARTIFACTS)}`
- fit covariate contract observed: `PPT|SOIL|PCA`
- deterministic-climate enabled flags observed: `True`
- covariate-features enabled flags observed: `True`
- lag orders observed: `1|2|3`
- square terms observed: `True`
- interaction term observed: `True`
- likelihood modes observed: `{', '.join(unique_likelihoods)}`
- full within-cutoff shared-input alignment checks passing: `{aligned_full} / {total_full}`

Special publication update:
- all nine benchmark families now resolve to canonical-bundle promoted roots.
- Final gate: the full 9-model manuscript benchmark table is ready for the current publication snapshot.

## Canonical-Bundle Promoted Rows

{markdown_table(["Cutoff", "Label", "Mean CRPS", "Run ID"], promoted_rows)}

## Within-Cutoff Input Congruence

{markdown_table(["Cutoff", "Artifact Checks Passing", "Result"], cutoff_rows)}

Archival caveat:
- `usgs_daily.csv` was not preserved inside some older multivariate quantile run roots, so the strict within-cutoff congruence table is evaluated on the **10 fit/forecast/blended-covariate artifacts** rather than on the auxiliary USGS cache file.
- Input congruence is now a final-pass claim across the 10 fit/forecast/blended-covariate artifacts required for the Bayesian benchmark rows.

## Publication Rows

{markdown_table(["Cutoff", "Label", "CRPS", "Run ID", "Campaign", "Note"], current_rows)}

## Outputs

- manifest: `{OUT_DIR / 'he2_bayesian_publication_manifest.csv'}`
- inputs: `{OUT_DIR / 'he2_bayesian_publication_inputs.csv'}`
- alignment: `{OUT_DIR / 'he2_bayesian_publication_alignment.csv'}`
"""
    (OUT_DIR / "he2_bayesian_publication_manifest.md").write_text(md + "\n", encoding="utf-8")


def main() -> int:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    manifest_rows, input_rows, alignment_rows = build_outputs()
    validate(manifest_rows, input_rows, alignment_rows)
    write_csv(OUT_DIR / "he2_bayesian_publication_manifest.csv", manifest_rows, CSV_FIELDS)
    write_csv(OUT_DIR / "he2_bayesian_publication_inputs.csv", input_rows, INPUT_FIELDS)
    write_csv(OUT_DIR / "he2_bayesian_publication_alignment.csv", alignment_rows, ALIGNMENT_FIELDS)
    write_markdown(manifest_rows, alignment_rows)
    print(f"manifest={OUT_DIR / 'he2_bayesian_publication_manifest.csv'}")
    print(f"inputs={OUT_DIR / 'he2_bayesian_publication_inputs.csv'}")
    print(f"alignment={OUT_DIR / 'he2_bayesian_publication_alignment.csv'}")
    print(f"markdown={OUT_DIR / 'he2_bayesian_publication_manifest.md'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
