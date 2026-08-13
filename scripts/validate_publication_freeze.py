#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import re
import subprocess
import sys
from collections import Counter
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable

from multimodel_v8_lib import ROOT, load_yaml
from forecast_design_contract import (
    ARTICLE_FORECAST_DESIGN_DOC_REL,
    FORBIDDEN_FORECAST_DESIGN_CLAIMS,
    FORECAST_DESIGN_CONTRACT_REL,
    FORECAST_DESIGN_MANIFEST_REL,
    REQUIRED_FORECAST_DESIGN_ARTICLE_CLAIMS,
    REQUIRED_FORECAST_DESIGN_CORRECTIONS_CLAIMS,
    check_forecast_design_manifest,
)
from latest_forecast_issue_contract import (
    ARTICLE_LATEST_FORECAST_ISSUE_DOC_REL,
    FORBIDDEN_LATEST_FORECAST_ARTICLE_CLAIMS,
    LATEST_FORECAST_ISSUE_CONTRACT_REL,
    LATEST_FORECAST_ISSUE_MANIFEST_REL,
    REQUIRED_LATEST_FORECAST_ARTICLE_CLAIMS,
    REQUIRED_LATEST_FORECAST_CORRECTIONS_CLAIMS,
    check_latest_forecast_issue_manifest,
)
from runtime_feasibility_contract import (
    ARTICLE_RUNTIME_DOC_REL,
    FORBIDDEN_RUNTIME_DECOMPOSITION_CLAIMS,
    REQUIRED_RUNTIME_ARTICLE_CLAIMS,
    REQUIRED_RUNTIME_CORRECTIONS_CLAIMS,
    RUNTIME_CONTRACT_REL,
    RUNTIME_MANIFEST_REL,
    check_runtime_manifest,
)
from reviewer1_overview_contract import (
    ARTICLE_R1_OVERVIEW_DOC_REL,
    R1_OVERVIEW_CONTRACT_REL,
    check_r1_overview_text,
)
from reviewer1_uncertainty_contract import (
    ARTICLE_R1_UNCERTAINTY_DOC_REL,
    R1_UNCERTAINTY_CONTRACT_REL,
    check_r1_uncertainty_text,
)
from reviewer1_remaining_contracts import (
    ARTICLE_R1_REMAINING_DOC_REL,
    R1_REMAINING_CONTRACT_REL,
    check_reviewer1_remaining_text,
)
from software_availability_contract import (
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


CUTOFF_ORDER = ["20210123", "20211112", "20211221", "20220511", "20221225"]
CUTOFF_DISPLAY = {
    "20210123": "01/23/2021",
    "20211112": "11/12/2021",
    "20211221": "12/21/2021",
    "20220511": "05/11/2022",
    "20221225": "12/25/2022",
}
HE3_VARIANTS = ["full", "noTrend", "noTF", "noH1", "noH2", "noH3"]
HE3_LABEL_BY_VARIANT = {
    "full": "exAL-M-T1 (full)",
    "noTrend": "exAL-M-T1-noTrend",
    "noTF": "exAL-M-noTF",
    "noH1": "exAL-M-T1-noH1",
    "noH2": "exAL-M-T1-noH2",
    "noH3": "exAL-M-T1-noH3",
}
TARGETED_REPAIR_LINEAGE = "he2_table1_targeted_repair_20260612:canonical_bundle_targeted_repair"
UNIVAR_SCALE_REPAIR_LINEAGE_PREFIX = "he2_univar_al_exal_scale_repair_20260629:"
SELECTED_EXAL_KEEP_REPLACEMENT_LINEAGE_PREFIXES = (
    "exdqlm_multivar_keep_partial_screen_20260623:",
    "exdqlm_multivar_keep_partial_authority_refresh_20260623:",
)
AUTHORITATIVE_KEEP_LINEAGE = "exdqlm_multivar_keep_canonical_grid_20260524:authoritative_winner"
HE3_CURRENT_SOURCE_TABLE_REL = Path("config/he3_exdqlm_ablation_current_authority_20260625_best_by_cutoff_long.csv")
HE3_CURRENT_RUNTIME_ROOT = ROOT.parent / "project1_ucsc_phd_runtime/multimodel_v8_he3_exdqlm_ablation_current_authority_20260625"
DISPLAY_DIGITS = 5
DISPLAY_TOL = 0.5 * 10 ** (-DISPLAY_DIGITS)

NON_PROMOTED_WORSE_REPAIRS = {
    ("20210123", "N-M-T1"): 3.2149023502665646,
    ("20221225", "exAL-M-T0"): 1.2113191493945392,
    ("20221225", "N-M-T1"): 3.8886290887278108,
}

SELECTED_MODEL_FIGURES = {
    "fig:dry_quantile",
    "fig:rainy_quantile",
    "fig:synth1",
    "fig:80_components",
}
SELECTED_MODEL_SOURCE_CLASS = "current_selected_model_representative"
RETAINED_CURRENT_RDATA_SUFFIX = "_authoritative_rdata_retained_current_20260623"

@dataclass
class Check:
    family: str
    item: str
    status: str
    detail: str


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def write_csv(path: Path, rows: list[dict[str, object]], fields: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def git_value(repo: Path, *args: str) -> str:
    try:
        return subprocess.check_output(["git", "-C", str(repo), *args], text=True).strip()
    except Exception as exc:
        return f"ERROR: {exc}"


def add(checks: list[Check], family: str, item: str, ok: bool, detail: str) -> None:
    checks.append(Check(family, item, "pass" if ok else "fail", detail))


def strip_retained_current_rdata_suffix(run_id: str) -> str:
    if run_id.endswith(RETAINED_CURRENT_RDATA_SUFFIX):
        return run_id[: -len(RETAINED_CURRENT_RDATA_SUFFIX)]
    return run_id


def same_or_retained_current_rdata_run(observed: str, expected: str) -> bool:
    return observed == expected or strip_retained_current_rdata_suffix(observed) == expected


def as_float(value: object) -> float:
    return float(str(value).strip())


def parse_json_field(value: str | None) -> dict[str, object]:
    if not value:
        return {}
    parsed = json.loads(value)
    if not isinstance(parsed, dict):
        return {}
    return parsed


def same_display(expected: float, observed: float) -> bool:
    return abs(round(expected, DISPLAY_DIGITS) - observed) <= DISPLAY_TOL


def strip_tex(cell: str) -> str:
    text = cell.strip()
    previous = None
    while previous != text:
        previous = text
        text = re.sub(r"\\textbf\{([^{}]+)\}", r"\1", text)
        text = re.sub(r"\\textit\{([^{}]+)\}", r"\1", text)
        text = re.sub(r"\\texttt\{([^{}]+)\}", r"\1", text)
    return text.replace("$", "").replace("\\", "").strip()


def parse_numeric(cell: str) -> float:
    match = re.search(r"-?\d+(?:\.\d+)?", strip_tex(cell))
    if not match:
        raise ValueError(f"Could not parse numeric cell: {cell!r}")
    return float(match.group(0))


def parse_flat_tex(path: Path, expected_numeric_cells: int = 5) -> dict[str, list[float]]:
    rows: dict[str, list[float]] = {}
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        if "&" not in line or line.startswith("\\") or "\\multicolumn" in line:
            continue
        parts = [part.strip() for part in line.rstrip("\\").split("&")]
        if len(parts) != expected_numeric_cells + 1:
            continue
        label = strip_tex(parts[0])
        if label in {"Ablation model", "Model", "Model label"}:
            continue
        try:
            rows[label] = [parse_numeric(cell) for cell in parts[1:]]
        except ValueError:
            continue
    return rows


def mean_crps_for_leads(path: Path, *, model_id: str, horizon_days: int) -> float:
    rows = [
        row for row in read_csv(path)
        if row.get("model_id") == model_id and 1 <= int(row["lead_day"]) <= horizon_days
    ]
    leads = sorted(int(row["lead_day"]) for row in rows)
    expected = list(range(1, horizon_days + 1))
    if leads != expected:
        raise ValueError(f"Expected leads {expected} for model_id={model_id} in {path}; got {leads}")
    return sum(as_float(row["crps"]) for row in rows) / len(rows)


def keyed(rows: list[dict[str, str]], *cols: str) -> dict[tuple[str, ...], dict[str, str]]:
    out: dict[tuple[str, ...], dict[str, str]] = {}
    for row in rows:
        out[tuple(str(row[col]).strip() for col in cols)] = row
    return out


def load_he3_source_authority(workflow_root: Path) -> dict[str, dict[str, object]]:
    legacy_winners = load_yaml(workflow_root / "docs/exdqlm_multivar_keep_authoritative_specs_20260601.yaml")["winners"]
    legacy_by_cutoff = {str(row["cutoff"]): row for row in legacy_winners}
    source_table = workflow_root / HE3_CURRENT_SOURCE_TABLE_REL
    if not source_table.exists():
        return {
            cutoff: {
                "run_id": row["run_id"],
                "mean_crps": float(row["mean_crps"]),
                "he3_source_label": row["grid_spec_id"],
                "synthesis_grid_spec_id": row["grid_spec_id"],
                "c_factor": float(row["c_factor"]),
                "epsilon": float(row["epsilon_value"]),
            }
            for cutoff, row in legacy_by_cutoff.items()
        }

    rows = read_csv(source_table)
    out: dict[str, dict[str, object]] = {}
    for row in rows:
        if row.get("model_variant") != "exdqlm_multivar_keep":
            continue
        if str(row.get("rank_within_cutoff", "")).strip() != "1":
            continue
        cutoff = str(row["cutoff"])
        legacy = legacy_by_cutoff.get(cutoff, {})
        out[cutoff] = {
            "run_id": row["source_run_id"],
            "mean_crps": float(row["forecast_window_crps"]),
            "he3_source_label": row["best_epsilon_label"],
            "synthesis_grid_spec_id": legacy.get("grid_spec_id", row["best_epsilon_label"]),
            "c_factor": float(row["best_c_factor"]),
            "epsilon": float(row["best_epsilon_value"]),
        }
    return out


def resolve_he3_matrix_dir(he3_runtime_root: Path) -> Path:
    candidates = [
        he3_runtime_root / "control/he3_exdqlm_ablation_current_authority_v1",
        he3_runtime_root / "control/he3_exdqlm_ablation_authoritative_winners_v1",
    ]
    for candidate in candidates:
        if (candidate / "matrix_status.csv").exists() and (candidate / "selection_manifest.csv").exists():
            return candidate
    return candidates[0]


def check_he2_selective_manifest(
    *,
    workflow_root: Path,
    article_root: Path,
    checks: list[Check],
) -> None:
    overlay = load_yaml(workflow_root / "config/he2_publication_manifest_replacement_overlay_current_authority_20260623.yaml")
    replacements = overlay.get("replacements", [])
    replacement_type_counts: Counter[str] = Counter()
    for repl in replacements:
        lineage = str(repl.get("campaign_lineage", overlay.get("campaign_lineage", "")))
        if lineage == TARGETED_REPAIR_LINEAGE:
            replacement_type_counts["table1_targeted_repair"] += 1
        elif lineage.startswith(UNIVAR_SCALE_REPAIR_LINEAGE_PREFIX):
            replacement_type_counts["univariate_scale_repair"] += 1
        elif any(lineage.startswith(prefix) for prefix in SELECTED_EXAL_KEEP_REPLACEMENT_LINEAGE_PREFIXES):
            replacement_type_counts["selected_exal_keep_refresh"] += 1
        else:
            replacement_type_counts["unexpected"] += 1
    overlay_keys = {(str(r["cutoff"]), str(r["manuscript_label"])) for r in replacements}
    table1_overlay_keys = {
        (str(r["cutoff"]), str(r["manuscript_label"]))
        for r in replacements
        if str(r.get("campaign_lineage", overlay.get("campaign_lineage", ""))).startswith("he2_table1_targeted_repair_20260612:")
    }
    scale_repair_overlay_keys = {
        (str(r["cutoff"]), str(r["manuscript_label"]))
        for r in replacements
        if str(r.get("campaign_lineage", overlay.get("campaign_lineage", ""))).startswith(UNIVAR_SCALE_REPAIR_LINEAGE_PREFIX)
    }
    selected_exal_overlay_keys = {
        (str(r["cutoff"]), str(r["manuscript_label"]))
        for r in replacements
        if any(
            str(r.get("campaign_lineage", overlay.get("campaign_lineage", ""))).startswith(prefix)
            for prefix in SELECTED_EXAL_KEEP_REPLACEMENT_LINEAGE_PREFIXES
        )
    }
    expected_replacement_type_counts = {
        "table1_targeted_repair": 11,
        "univariate_scale_repair": 10,
        "selected_exal_keep_refresh": 3,
    }
    add(checks, "he2_selective", "overlay_active", bool(overlay.get("active")), "overlay active flag")
    add(checks, "he2_selective", "overlay_replacement_count", len(replacements) == 24, f"{len(replacements)} replacements")
    add(
        checks,
        "he2_selective",
        "overlay_replacement_type_counts",
        dict(replacement_type_counts) == expected_replacement_type_counts,
        json.dumps(dict(replacement_type_counts), sort_keys=True),
    )
    add(checks, "he2_selective", "overlay_table1_repair_count", len(table1_overlay_keys) == 11, f"{len(table1_overlay_keys)} Table 1 repairs")
    add(checks, "he2_selective", "overlay_univar_scale_repair_count", len(scale_repair_overlay_keys) == 10, f"{len(scale_repair_overlay_keys)} univariate scale repairs")
    add(checks, "he2_selective", "overlay_selected_exal_keep_count", len(selected_exal_overlay_keys) == 3, f"{len(selected_exal_overlay_keys)} selected exAL-M-T1 rows")
    add(
        checks,
        "he2_selective",
        "overlay_mentions_selective_policy",
        "promoted selectively" in str(overlay.get("publication_note", "")),
        "publication_note records selective policy",
    )

    manifest_path = article_root / "artifacts/he2_publication_freeze/he2_bayesian_publication_manifest.csv"
    rows = read_csv(manifest_path)
    by_key = keyed(rows, "cutoff", "manuscript_label")
    targeted = {
        (r["cutoff"], r["manuscript_label"])
        for r in rows
        if r.get("campaign_lineage") == TARGETED_REPAIR_LINEAGE
    }
    scale_repair = {
        (r["cutoff"], r["manuscript_label"])
        for r in rows
        if str(r.get("campaign_lineage", "")).startswith(UNIVAR_SCALE_REPAIR_LINEAGE_PREFIX)
    }
    selected_exal = {
        (r["cutoff"], r["manuscript_label"])
        for r in rows
        if any(str(r.get("campaign_lineage", "")).startswith(prefix) for prefix in SELECTED_EXAL_KEEP_REPLACEMENT_LINEAGE_PREFIXES)
    }
    add(checks, "he2_selective", "manifest_row_count", len(rows) == 45, f"{len(rows)} rows")
    add(checks, "he2_selective", "targeted_repair_count", len(targeted) == 11, f"{len(targeted)} targeted rows")
    add(checks, "he2_selective", "univar_scale_repair_count", len(scale_repair) == 10, f"{len(scale_repair)} repaired univariate rows")
    add(checks, "he2_selective", "selected_exal_keep_count", len(selected_exal) == 3, f"{len(selected_exal)} selected exAL-M-T1 rows")
    add(
        checks,
        "he2_selective",
        "targeted_rows_match_overlay",
        targeted == table1_overlay_keys,
        f"manifest={len(targeted)} overlay={len(table1_overlay_keys)}",
    )
    add(
        checks,
        "he2_selective",
        "univar_scale_repair_rows_match_overlay",
        scale_repair == scale_repair_overlay_keys,
        f"manifest={len(scale_repair)} overlay={len(scale_repair_overlay_keys)}",
    )
    add(
        checks,
        "he2_selective",
        "selected_exal_rows_match_overlay",
        selected_exal == selected_exal_overlay_keys,
        f"manifest={len(selected_exal)} overlay={len(selected_exal_overlay_keys)}",
    )

    for key, expected_crps in sorted(NON_PROMOTED_WORSE_REPAIRS.items()):
        row = by_key.get(key)
        label = f"{key[0]}:{key[1]}"
        add(checks, "he2_selective", f"{label}:present", row is not None, "fallback row present")
        if row is None:
            continue
        add(
            checks,
            "he2_selective",
            f"{label}:not_targeted_repair",
            row.get("campaign_lineage") != TARGETED_REPAIR_LINEAGE,
            row.get("campaign_lineage", ""),
        )
        add(
            checks,
            "he2_selective",
            f"{label}:fallback_crps",
            abs(as_float(row["crps_exact"]) - expected_crps) <= 1e-10,
            f"{as_float(row['crps_exact']):.12f}",
        )


def check_he3_authoritative(
    *,
    workflow_root: Path,
    article_root: Path,
    corrections_root: Path,
    he3_runtime_root: Path,
    checks: list[Check],
) -> None:
    winner_by_cutoff = load_he3_source_authority(workflow_root)
    add(checks, "he3_authority", "winner_cutoff_set", list(winner_by_cutoff) == CUTOFF_ORDER, ",".join(winner_by_cutoff))
    he2_manifest_rows = read_csv(article_root / "artifacts/he2_publication_freeze/he2_bayesian_publication_manifest.csv")
    he2_current_exal = {
        row["cutoff"]: row
        for row in he2_manifest_rows
        if row.get("manuscript_label") == "exAL-M-T1" and row.get("family") == "exdqlm_multivar_keep"
    }

    matrix_dir = resolve_he3_matrix_dir(he3_runtime_root)
    add(checks, "he3_authority", "matrix_dir_exists", matrix_dir.exists(), str(matrix_dir))
    status_rows = read_csv(matrix_dir / "matrix_status.csv")
    selection_rows = read_csv(matrix_dir / "selection_manifest.csv")
    status_counts: dict[str, int] = {}
    variant_counts: dict[str, int] = {}
    for row in status_rows:
        status_counts[row["status"]] = status_counts.get(row["status"], 0) + 1
        variant_counts[row["variant"]] = variant_counts.get(row["variant"], 0) + 1
    add(checks, "he3_authority", "matrix_row_count", len(status_rows) == 30, f"{len(status_rows)} rows")
    add(checks, "he3_authority", "matrix_all_pass", status_counts == {"pass": 30}, json.dumps(status_counts, sort_keys=True))
    add(
        checks,
        "he3_authority",
        "variant_balance",
        variant_counts == {variant: 5 for variant in HE3_VARIANTS},
        json.dumps(variant_counts, sort_keys=True),
    )

    selection_by_key = keyed(selection_rows, "cutoff", "variant")
    wide_runtime = read_csv(he3_runtime_root / "reports/he3_exdqlm_ablation/he3_ablation_wide.csv")
    wide_article = read_csv(article_root / "artifacts/he3_exdqlm_ablation_authoritative/he3_ablation_wide.csv")
    runtime_key = keyed(wide_runtime, "cutoff", "variant")
    article_key = keyed(wide_article, "cutoff", "variant")
    add(checks, "he3_authority", "runtime_wide_row_count", len(wide_runtime) == 30, f"{len(wide_runtime)} rows")
    add(checks, "he3_authority", "article_wide_row_count", len(wide_article) == 30, f"{len(wide_article)} rows")

    for cutoff in CUTOFF_ORDER:
        winner = winner_by_cutoff[cutoff]
        full_sel = selection_by_key.get((cutoff, "full"))
        full_row = runtime_key.get((cutoff, "full"))
        label = CUTOFF_DISPLAY[cutoff]
        add(
            checks,
            "he3_authority",
            f"{cutoff}:full_source_run",
            full_sel is not None and full_sel.get("source_run_id") == winner["run_id"],
            full_sel.get("source_run_id", "missing") if full_sel else "missing",
        )
        add(
            checks,
            "he3_authority",
            f"{cutoff}:full_grid_spec",
            full_sel is not None and full_sel.get("best_epsilon_label") == winner["he3_source_label"],
            full_sel.get("best_epsilon_label", "missing") if full_sel else "missing",
        )
        add(
            checks,
            "he3_authority",
            f"{cutoff}:full_crps_matches_winner",
            full_row is not None and abs(as_float(full_row["mean_crps"]) - float(winner["mean_crps"])) <= 1e-12,
            f"{label} full={as_float(full_row['mean_crps']):.12f}" if full_row else "missing",
        )
        meta_path = article_root / f"artifacts/five_cutoff_main_model_synthesis/{cutoff}_exal_m_t1/source_metadata.json"
        meta = json.loads(meta_path.read_text(encoding="utf-8"))
        current = he2_current_exal.get(cutoff)
        add(checks, "he3_authority", f"{cutoff}:synthesis_current_manifest_row", current is not None, "current HE2 exAL-M-T1 row")
        if current is not None:
            observed_run = str(meta.get("multivar_run_id", ""))
            observed_lineage = str(meta.get("source_lineage", ""))
            allowed_lineages = {str(current.get("campaign_lineage", "")), AUTHORITATIVE_KEEP_LINEAGE}
            add(
                checks,
                "he3_authority",
                f"{cutoff}:synthesis_run_id",
                same_or_retained_current_rdata_run(observed_run, current["run_id"]),
                observed_run,
            )
            add(
                checks,
                "he3_authority",
                f"{cutoff}:synthesis_grid",
                str(meta.get("grid_spec_id", ""))
                in {
                    value
                    for value in [
                        str(winner["synthesis_grid_spec_id"]),
                        str(winner["he3_source_label"]),
                        str(current.get("campaign_lineage", "")),
                        str(current.get("expected_input_bundle_id", "")),
                    ]
                    if value
                },
                str(meta.get("grid_spec_id", "")),
            )
            add(
                checks,
                "he3_authority",
                f"{cutoff}:synthesis_lineage",
                observed_lineage in allowed_lineages,
                observed_lineage,
            )

    for key, runtime_row in runtime_key.items():
        article_row = article_key.get(key)
        add(
            checks,
            "he3_authority",
            f"{key[0]}:{key[1]}:article_artifact_matches_runtime",
            article_row is not None and abs(as_float(article_row["mean_crps"]) - as_float(runtime_row["mean_crps"])) <= 1e-12,
            f"runtime={runtime_row.get('mean_crps')} article={article_row.get('mean_crps') if article_row else 'missing'}",
        )

    article_table = parse_flat_tex(article_root / "tables/generated_tex/he3_ablation_crps_main_table.tex")
    corrections_table = parse_flat_tex(corrections_root / "tables/generated_tex/he3_ablation_crps_response_table.tex")
    article_nws_table = parse_flat_tex(article_root / "tables/generated_tex/he3_ablation_crps_nws_horizon_table.tex")
    corrections_nws_table = parse_flat_tex(corrections_root / "tables/generated_tex/he3_ablation_crps_nws_horizon_response_table.tex")
    add(checks, "he3_authority", "article_table_excludes_nws_28day", "RAW-NWS" not in article_table, "NWS omitted from 28-day HE3 table")
    add(checks, "he3_authority", "corrections_table_excludes_nws_28day", "RAW-NWS" not in corrections_table, "NWS omitted from 28-day HE3 response table")
    add(checks, "he3_authority", "article_nws_horizon_includes_nws", "RAW-NWS" in article_nws_table, "NWS included in eight-day HE3 table")
    add(checks, "he3_authority", "corrections_nws_horizon_includes_nws", "RAW-NWS" in corrections_nws_table, "NWS included in eight-day HE3 response table")
    for variant in HE3_VARIANTS:
        label = HE3_LABEL_BY_VARIANT[variant]
        expected = [as_float(runtime_key[(cutoff, variant)]["mean_crps"]) for cutoff in CUTOFF_ORDER]
        observed = article_table.get(label)
        add(
            checks,
            "he3_authority",
            f"article_table:{label}",
            observed is not None and all(same_display(e, o) for e, o in zip(expected, observed)),
            "five-decimal rendered values",
        )
        corrections_observed = corrections_table.get(label)
        add(
            checks,
            "he3_authority",
            f"corrections_table:{label}",
            corrections_observed is not None and all(same_display(e, o) for e, o in zip(expected, corrections_observed)),
            "five-decimal rendered values",
        )
        expected_nws_horizon = []
        for cutoff in CUTOFF_ORDER:
            row = runtime_key[(cutoff, variant)]
            per_time_path = (
                Path(row["resolved_run_dir"])
                / "post"
                / "outputs"
                / row["resolved_run_id"]
                / "tables"
                / "crps_forecast_per_time.csv"
            )
            expected_nws_horizon.append(mean_crps_for_leads(per_time_path, model_id=row["target_model_id"], horizon_days=8))
        observed_nws = article_nws_table.get(label)
        add(
            checks,
            "he3_authority",
            f"article_nws_horizon_table:{label}",
            observed_nws is not None and all(same_display(e, o) for e, o in zip(expected_nws_horizon, observed_nws)),
            "eight-day rendered values",
        )
        corrections_observed_nws = corrections_nws_table.get(label)
        add(
            checks,
            "he3_authority",
            f"corrections_nws_horizon_table:{label}",
            corrections_observed_nws is not None and all(same_display(e, o) for e, o in zip(expected_nws_horizon, corrections_observed_nws)),
            "eight-day rendered values",
        )


def check_he4_sync(article_root: Path, corrections_root: Path, checks: list[Check]) -> None:
    audit_rows = read_csv(article_root / "artifacts/he4_quantile_check_loss_current_publication/he4_selection_audit.csv")
    source_modes = sorted({row.get("source_mode", "") for row in audit_rows})
    max_crps_diff = max(as_float(row.get("crps_abs_diff", "0")) for row in audit_rows)
    add(checks, "he4_sync", "selection_row_count", len(audit_rows) == 20, f"{len(audit_rows)} rows")
    add(checks, "he4_sync", "selection_source_mode", source_modes == ["he2-publication-manifest"], ",".join(source_modes))
    add(checks, "he4_sync", "selection_crps_crosscheck", max_crps_diff <= 1e-6, f"max={max_crps_diff:.3g}")
    article_rows = (article_root / "tables/generated_tex/he4_quantile_check_loss_rows.tex").read_text(encoding="utf-8").strip()
    corrections = (corrections_root / "tables/generated_tex/he4_quantile_check_loss_response_table.tex").read_text(encoding="utf-8")
    add(checks, "he4_sync", "corrections_contains_article_rows", article_rows in corrections, "HE4 rows synced into corrections wrapper")


def check_selected_figures(article_root: Path, checks: list[Check]) -> None:
    def rel_exists(rel: str) -> bool:
        path = article_root / rel
        if path.exists():
            return True
        parts = Path(rel).parts
        if parts and parts[0] == "figures":
            return (article_root / "Figures" / Path(*parts[1:])).exists()
        return False

    manifest = json.loads((article_root / "MANUSCRIPT_ASSET_MANIFEST.json").read_text(encoding="utf-8"))
    figures = {row["label"]: row for row in manifest.get("figures", [])}
    for label in SELECTED_MODEL_FIGURES:
        row = figures.get(label)
        add(checks, "figure_lineage", f"{label}:manifest_entry", row is not None, "manifest entry")
        if row is None:
            continue
        add(
            checks,
            "figure_lineage",
            f"{label}:current_model_flag",
            bool(row.get("current_model_output_wired")),
            str(row.get("current_model_output_wired")),
        )
        add(
            checks,
            "figure_lineage",
            f"{label}:source_class",
            row.get("source_class") == SELECTED_MODEL_SOURCE_CLASS,
            str(row.get("source_class", "")),
        )
        add(checks, "figure_lineage", f"{label}:source_exists", rel_exists(row["source_path"]), row["source_path"])
        add(checks, "figure_lineage", f"{label}:manuscript_exists", rel_exists(row["manuscript_path"]), row["manuscript_path"])
        add(
            checks,
            "figure_lineage",
            f"{label}:selected_authority_note",
            "selected" in str(row.get("note", "")).lower() or "authoritative" in str(row.get("note", "")).lower() or "support" in str(row.get("note", "")).lower(),
            str(row.get("note", "")),
        )
    he2_manifest_rows = read_csv(article_root / "artifacts/he2_publication_freeze/he2_bayesian_publication_manifest.csv")
    expected_representative = next(
        row for row in he2_manifest_rows
        if row.get("cutoff") == "20221225"
        and row.get("manuscript_label") == "exAL-M-T1"
        and row.get("family") == "exdqlm_multivar_keep"
    )
    bundle = json.loads((article_root / "artifacts/representative_selected_model_2022_12_25/bundle_metadata.json").read_text(encoding="utf-8"))
    add(
        checks,
        "figure_lineage",
        "representative_bundle_run",
        same_or_retained_current_rdata_run(str(bundle.get("run_id", "")), expected_representative["run_id"]),
        str(bundle.get("run_id", "")),
    )
    support_readme = (article_root / "artifacts/representative_selected_model_2022_12_25/authoritative_support/README.md").read_text(encoding="utf-8")
    add(
        checks,
        "figure_lineage",
        "support_readme_same_authority",
        "same current `2022-12-25` selected `exAL-M-T1` output authority" in support_readme,
        "authoritative support README",
    )


def check_prior_claim_contract(article_root: Path, corrections_root: Path, checks: list[Check]) -> None:
    manifest_path = article_root / "artifacts/he2_publication_freeze/he2_bayesian_publication_manifest.csv"
    rows = [
        row for row in read_csv(manifest_path)
        if row.get("manuscript_label") == "exAL-M-T1"
        and row.get("family") == "exdqlm_multivar_keep"
    ]
    add(checks, "prior_claims", "selected_exal_row_count", len(rows) == 5, f"{len(rows)} rows")
    epsilon_values: list[float] = []
    for row in rows:
        prior = parse_json_field(row.get("prior_json"))
        forecast_cov = prior.get("forecast_cov", {}) if isinstance(prior.get("forecast_cov"), dict) else {}
        run_id = row.get("run_id", "")
        has_epsilon = "epsilon" in forecast_cov
        has_c_factor = "c_factor" in forecast_cov
        add(checks, "prior_claims", f"{row.get('cutoff')}:forecast_cov_has_epsilon", has_epsilon, run_id)
        add(checks, "prior_claims", f"{row.get('cutoff')}:forecast_cov_has_c_factor", has_c_factor, run_id)
        if has_epsilon:
            try:
                epsilon_values.append(float(forecast_cov["epsilon"]))
                add(checks, "prior_claims", f"{row.get('cutoff')}:epsilon_positive", float(forecast_cov["epsilon"]) > 0, str(forecast_cov["epsilon"]))
            except (TypeError, ValueError):
                add(checks, "prior_claims", f"{row.get('cutoff')}:epsilon_parseable", False, str(forecast_cov.get("epsilon")))
        if has_c_factor:
            try:
                add(checks, "prior_claims", f"{row.get('cutoff')}:c_factor_is_one", abs(float(forecast_cov["c_factor"]) - 1.0) <= 1e-12, str(forecast_cov["c_factor"]))
            except (TypeError, ValueError):
                add(checks, "prior_claims", f"{row.get('cutoff')}:c_factor_parseable", False, str(forecast_cov.get("c_factor")))

    add(
        checks,
        "prior_claims",
        "epsilon_values_manifest_backed",
        len(epsilon_values) == len(rows),
        ",".join(f"{value:g}" for value in epsilon_values),
    )

    article_text = (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8")
    corrections_text = (corrections_root / "main.tex").read_text(encoding="utf-8")
    stale_c_patterns = [
        r"c\s*=\s*10\^2",
        r"c\s*=\s*10\^\{2\}",
        r"c\s*=\s*10\$\^\{2\}\$",
    ]
    for pattern in stale_c_patterns:
        add(checks, "prior_claims", f"article_forbidden:{pattern}", re.search(pattern, article_text) is None, pattern)
        add(checks, "prior_claims", f"corrections_forbidden:{pattern}", re.search(pattern, corrections_text) is None, pattern)
    add(
        checks,
        "prior_claims",
        "article_explains_c_scale",
        "The scalar \\(c\\) controls the scale of the carried-forward covariance anchor" in article_text,
        "conceptual c-scale wording",
    )
    add(
        checks,
        "prior_claims",
        "article_explains_epsilon_strength",
        "\\(\\epsilon\\) controls how strongly that anchor is retained" in article_text,
        "conceptual epsilon-strength wording",
    )


def check_prose(article_root: Path, corrections_root: Path, checks: list[Check]) -> None:
    generated_tables = "\n".join(
        path.read_text(encoding="utf-8")
        for path in sorted((article_root / "tables" / "generated_tex").glob("*.tex"))
    )
    article = (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8") + "\n" + generated_tables
    corrections = (corrections_root / "main.tex").read_text(encoding="utf-8")
    required_article = [
        "The exAL-M-T1 specification attains the lowest 28-day CRPS at all five rolling-origin cutoffs",
        "A separate eight-day NWS-horizon table preserves the direct operational comparison to NWS",
        r"Appendix~\ref{app:he3ablation} reports a component-removal sensitivity analysis",
        r"noH3} refers to the retained noninteger frequency \(1/6.8068493\)",
        "representative selected-model illustration",
        "conceptual or physically based models",
        "Conceptual formulations remain especially practical for prediction",
        "easier to specify, calibrate, and deploy operationally",
        "The empirical focus is forecasting performance and uncertainty quantification",
        r"Section~\ref{sec:forecastvalidation} reports the out-of-sample forecast validation results",
        r"\section{FORECAST VALIDATION RESULTS}",
        r"\section{INTERPRETATION OF THE SELECTED SPECIFICATION}",
        "five-cutoff rolling-origin forecast comparison",
        "These figures are interpretation diagnostics",
        "decomposes fitted historical quantile behavior across contrasting regimes",
        "five cutoff-specific, version-consistent staged datasets that span contrasting hydrological conditions",
        "relatively low-flow windows as well as winter high-flow episodes",
        "not a continuous daily hindcast over the full post-2022 period",
        "Post-cutoff USGS observations are reserved strictly for verification",
        "five cutoff-specific, version-consistent staged datasets",
        "uses only information available at that origin to fit seven quantile-specific models",
        "scores that distribution against future USGS observations held out over the forecast window",
        "archive-feasible, version-consistent origins that span contrasting hydrological settings",
        "This archive-reconstruction step is the main practical constraint on a denser rolling-origin design",
        "data recovery, version matching, spatial extraction, covariate staging, and computation",
        "avoid dense overlaps that would overrepresent the same episode",
        "These two uncertainty sources are related but distinct",
        "Hydrological uncertainty arises from model structure, parameters, states, and observations",
        "meteorological uncertainty enters through imperfect precipitation and related atmospheric forcing fields",
        "local precipitation from the PRISM Climate Group",
        "local soil moisture from ECMWF ERA5-Land",
        "historical gridded covariates extracted at the Big Trees location",
        "The GDPC factor is treated as a climate-index covariate, not as an operational forecast product or verification target",
        r"\section{APPLICATION DATA AND FORECASTING DESIGN}",
        r"\subsection{Study Setting and Observations}",
        "Our target series is",
        "USGS target series",
        "three additional information sources",
        "Each source plays a different role",
        "retrospective products are used to learn source-specific discrepancies",
        "relative to the USGS target series",
        "The transfer component takes three inputs",
        "These local hydrometeorological covariates enter as deterministic summaries in the present implementation",
        r"\subsection{Extended Asymmetric Laplace Likelihood}",
        r"\(L\in\{\mathrm{N},\mathrm{AL},\mathrm{exAL}\}\) denotes a Gaussian, asymmetric Laplace, or extended asymmetric Laplace observation likelihood",
        r"\(S\in\{\mathrm{U},\mathrm{M}\}\) indicates whether the synthesis is univariate or multivariate",
        r"\(T\in\{\mathrm{T0},\mathrm{T1}\}\) indicates whether the transfer component is suppressed or retained during the forecast window",
        "nine Bayesian variants of the common state-space framework",
        "We focus on exAL-M-T1 because it has the lowest 28-day forecast-window CRPS",
        "provide the strongest performance in the 28-day synthesis comparison",
        "Selected Posterior Means and 95\\% Credible Intervals for Transfer-Function Covariates",
        "Posterior Medians and 95\\% Credible Intervals for the Source-Specific Skewness Parameters",
        "Posterior Medians and 95\\% Credible Intervals for the Source-Specific Scale Parameters",
        "CRPS is negatively oriented",
        "For reproducibility, implementation pseudocode for the VB algorithm is provided",
        "Table~\\ref{tab:he4_quantile_check_loss} complements the CRPS comparisons with targeted quantile diagnostics",
        "uncertainty around fitted quantile-location curves",
        "synthesized posterior predictive distribution",
        "fitted quantile-specific forecasts combine into one predictive distribution",
        "posterior predictive envelope can vary across the forecast window",
        "quantile-specific posterior predictions into a single predictive distribution",
    ]
    required_corrections = [
        "exAL-M-T1} has the lowest 28-day CRPS in all five rolling-origin cutoffs",
        "separate eight-day NWS-horizon comparison",
        "Because this is a sensitivity analysis rather than a primary benchmark comparison",
        "The appendix sensitivity analysis uses the same selected specification",
        r"noH3} refers to the retained noninteger frequency \(1/6.8068493\)",
        "while removing one structural component at a time",
        "This fixed sensitivity design assesses whether the retained trend",
        "centering the forecasting analysis on multiple rolling-origin cutoffs",
        "supported by rolling-origin forecast evaluation and selected-model interpretation",
        "rather than treating dynamic discrepancy correction alone as the central novelty",
        "forecasting evaluation is expanded to five rolling-origin out-of-sample cutoffs",
        "evidence is no longer tied to a single moderate-flood episode",
        "not presented as a continuous 2023-present hindcast",
        "representative selected-model illustration at one forecast origin",
        "validation evidence is the multi-cutoff CRPS and check-loss tables",
        "selected-model diagnostics",
        "organized around forecast origins rather than a conventional random split",
        "five cutoff-specific forecast-window evaluations",
        "post-cutoff USGS observations are used only for verification",
        "archive reconstruction step before any model can be fit",
        "rather than a dense set of overlapping windows that would repeatedly score the same hydrological episode",
        "pre-cutoff observational window",
        "fixed calibrated specification",
        "The revised introduction now broadens this statement",
        "uses both conceptual and physically based models",
        "simpler to specify, calibrate, and deploy in forecasting applications",
        "typographical error rather than intended terminology",
        "The revised manuscript no longer uses this term",
        "reanalysis-based model products rather than direct observations or uncertainty-free measurements",
        "ERA5/ERA5-Land variables may include short forecast components",
        "precipitation is from PRISM and ERA5-Land enters as the soil-moisture covariate",
        "external covariates rather than verification observations",
        "now separates the general methodology from the application data and forecasting design",
        "USGS daily flow series as the observed target",
        "distinguishes forecast covariates, retrospective products, and operational forecast products",
        "external historical inputs used to learn source-specific discrepancies relative to the USGS target",
        "now states explicitly that precipitation is not handled through censoring",
        "zero-inflation, or a separate occurrence/intensity model",
        "zero-precipitation days are retained in the supplied covariate path",
        "precipitation intermittency enters through the transfer component",
        "no longer uses the vague ``General Results'' organization",
        "separates the material by inferential role",
        "Forecast Validation Results",
        "Interpretation of the Selected Specification",
        "selected-model diagnostics are separated from the forecast-validation tables",
        "representative transfer-function covariate table reports posterior means",
        "tables report posterior medians with 95\\% credible intervals",
        "table-specific export contract",
        "The revised introduction now separates these concepts before introducing the Bayesian framework",
        "hydrological uncertainty with river-system structure, parameters, states, and observations",
        "meteorological uncertainty with precipitation and atmospheric forcing fields",
        "using available forecast and retrospective products to produce calibrated predictive distributions",
        "no longer organizes the methodology around models A, B, and C",
        "presents a common state-space formulation",
        r"Section 4 identifies the models in terms of \(L\)-\(S\)-\(T\) labels",
        "likelihood family, source set, and forecast-window transfer treatment",
        r"selected \texttt{exAL-M-T1} specification",
        "PIT-centered development has been removed from the main text",
        "final forecast comparison uses CRPS as the primary full-distribution score",
        "targeted quantile check loss as the quantile-level diagnostic",
        "retained a compact posterior predictive synthesis subsection",
        "posterior uncertainty around fitted quantile-location or component summaries",
        "full forecast predictive distribution at each date",
        "representative single-cutoff posterior predictive distribution",
        "forecast-window inputs change",
        "Quantile crossing is no longer developed as a separate procedure in the main text",
        "Details about the MCMC and VB algorithms are provided in the Appendix",
    ]
    forbidden = [
        "best-performing model in all five cutoffs",
        "lowest forecast-window CRPS in every case",
        "do uniformly dominate the operational baseline",
        "raw NWS forecast product has the lowest CRPS overall",
        "raw NWS forecast product has the lowest CRPS in the table",
        "the full model remains best across all five ablation comparisons",
        "full model remains the best ablation configuration",
        "the main contribution will be presented",
        "does not currently distinguish meteorological and hydrological uncertainty",
        "we will reorganize the introduction",
        "we will broaden it",
        "we will correct it",
        "we will replace the deterministic language",
        "we will state earlier and more explicitly",
        "we will reorganize the application material",
        "We will make this explicit in the revised manuscript",
        "We agree that the current organization of Section 3 is not clear enough",
        "we will replace vague headings",
        "This will separate setup, historical behavior, and forecasting evidence more clearly",
        "The original labeling of Tables 1 and 2 was inconsistent: the entries reported there are posterior medians",
        "Posterior Means and 95\\% Credible Intervals for the Source-Specific",
        "local hydrological covariates",
        "current A/B/C presentation does not make the connection",
        "we will present the final forecasting specification",
        "We will also revise the opening of the results section",
        "we will simplify the presentation substantially",
        "we will remove the detailed PIT development",
        "we will reduce intermediate derivational detail",
        "will be mentioned briefly as a robustness device",
        "random K-fold cross-validation",
    ]
    for claim in required_article:
        add(checks, "prose", f"article_required:{claim}", claim in article, claim)
    for claim in required_corrections:
        add(checks, "prose", f"corrections_required:{claim}", claim in corrections, claim)
    for claim in forbidden:
        add(checks, "prose", f"article_forbidden:{claim}", claim not in article, claim)
        add(checks, "prose", f"corrections_forbidden:{claim}", claim not in corrections, claim)
    add(checks, "prose", "article_forbidden:flexile", "flexile" not in article.lower(), "flexile")


def check_software_availability(workflow_root: Path, article_root: Path, corrections_root: Path, checks: list[Check]) -> None:
    manifest_path = article_root / SOFTWARE_MANIFEST_REL
    add(checks, "software_availability", "manifest_exists", manifest_path.exists(), SOFTWARE_MANIFEST_REL)
    if not manifest_path.exists():
        return

    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    package = manifest.get("public_estimation_package", {})
    workflow = manifest.get("study_workflow_repository", {})
    article_repo = manifest.get("revised_article_repository", {})
    corrections_repo = manifest.get("corrections_repository", {})
    archive = manifest.get("archive_status", {})
    validation_policy = manifest.get("validation_policy", {})
    release_readiness_files = workflow.get("release_readiness_files", {})

    add(
        checks,
        "software_availability",
        "schema_version",
        manifest.get("schema_version") == "revision_software_availability_v1",
        str(manifest.get("schema_version", "")),
    )
    add(checks, "software_availability", "cran_package_url", package.get("cran_package_url") == CRAN_EXDQLM_URL, str(package.get("cran_package_url", "")))
    add(checks, "software_availability", "cran_package_doi", package.get("package_doi") == CRAN_EXDQLM_DOI_URL, str(package.get("package_doi", "")))
    add(checks, "software_availability", "cran_version_recorded", package.get("cran_version_verified_for_contract") == CRAN_EXDQLM_VERSION, str(package.get("cran_version_verified_for_contract", "")))
    add(checks, "software_availability", "cran_publication_date_recorded", package.get("cran_publication_date_verified_for_contract") == CRAN_EXDQLM_PUBLICATION_DATE, str(package.get("cran_publication_date_verified_for_contract", "")))
    add(checks, "software_availability", "software_paper_arxiv_url", package.get("software_paper_arxiv_url") == EXDQLM_SOFTWARE_PAPER_URL, str(package.get("software_paper_arxiv_url", "")))
    add(checks, "software_availability", "software_paper_arxiv_doi", package.get("software_paper_arxiv_doi") == EXDQLM_SOFTWARE_PAPER_DOI_URL, str(package.get("software_paper_arxiv_doi", "")))
    add(checks, "software_availability", "software_paper_bibtex_key", package.get("software_paper_bibtex_key") == EXDQLM_SOFTWARE_PAPER_BIBTEX_KEY, str(package.get("software_paper_bibtex_key", "")))
    add(checks, "software_availability", "workflow_url", workflow.get("public_url") == PROJECT1_URL, str(workflow.get("public_url", "")))
    expected_release_files = {
        "readme": WORKFLOW_README_REL,
        "citation": WORKFLOW_CITATION_REL,
        "pending_release_notes": WORKFLOW_RELEASE_NOTES_REL,
        "archive_readiness_checklist": WORKFLOW_ARCHIVE_READINESS_REL,
    }
    add(
        checks,
        "software_availability",
        "workflow_release_readiness_manifest",
        release_readiness_files == expected_release_files,
        json.dumps(release_readiness_files, sort_keys=True),
    )
    add(
        checks,
        "software_availability",
        "workflow_contract_doc",
        (workflow_root / SOFTWARE_CONTRACT_REL).exists(),
        SOFTWARE_CONTRACT_REL,
    )
    add(
        checks,
        "software_availability",
        "article_contract_doc",
        (article_root / ARTICLE_SOFTWARE_DOC_REL).exists(),
        ARTICLE_SOFTWARE_DOC_REL,
    )
    add(
        checks,
        "software_availability",
        "article_repo_url",
        "Evironmetrics---REVISED-DOC-Corrected-2" in str(article_repo.get("public_url", "")),
        str(article_repo.get("public_url", "")),
    )
    add(
        checks,
        "software_availability",
        "corrections_repo_url",
        "Corrections---Project-1" in str(corrections_repo.get("public_url", "")),
        str(corrections_repo.get("public_url", "")),
    )
    archive_check = check_archive_status(archive)
    add(checks, "software_availability", "archive_status_coherent", archive_check.ok, archive_check.detail)
    add(
        checks,
        "software_availability",
        "static_commit_policy",
        "reason_static_commits_are_not_recorded" in validation_policy,
        str(validation_policy.get("reason_static_commits_are_not_recorded", "")),
    )
    for rel in WORKFLOW_RELEASE_READINESS_RELS:
        add(checks, "software_availability", f"workflow_release_readiness_exists:{rel}", (workflow_root / rel).exists(), rel)
    remote_url = git_value(workflow_root, "remote", "get-url", "origin")
    add(checks, "software_availability", "workflow_remote_matches_project1", "AntonioAPDL/Project1" in remote_url, remote_url)

    readme_path = workflow_root / WORKFLOW_README_REL
    citation_path = workflow_root / WORKFLOW_CITATION_REL
    release_notes_path = workflow_root / WORKFLOW_RELEASE_NOTES_REL
    checklist_path = workflow_root / WORKFLOW_ARCHIVE_READINESS_REL
    if readme_path.exists():
        readme_text = readme_path.read_text(encoding="utf-8")
        add(checks, "software_availability", "readme_names_project1", PROJECT1_URL in readme_text, WORKFLOW_README_REL)
        add(checks, "software_availability", "readme_names_cran_package", CRAN_EXDQLM_URL in readme_text, WORKFLOW_README_REL)
        add(checks, "software_availability", "readme_names_contract", SOFTWARE_CONTRACT_REL in readme_text, WORKFLOW_README_REL)
        add(checks, "software_availability", "readme_archive_pending", "pending final revision freeze" in readme_text, WORKFLOW_README_REL)
    if citation_path.exists():
        citation_text = citation_path.read_text(encoding="utf-8")
        add(checks, "software_availability", "citation_pending_version", 'version: "pending-final-archive"' in citation_text, WORKFLOW_CITATION_REL)
        add(checks, "software_availability", "citation_no_workflow_doi_field", "\ndoi:" not in citation_text, WORKFLOW_CITATION_REL)
        add(checks, "software_availability", "citation_names_project1", PROJECT1_URL in citation_text, WORKFLOW_CITATION_REL)
    if release_notes_path.exists():
        release_notes_text = release_notes_path.read_text(encoding="utf-8")
        add(checks, "software_availability", "release_notes_archive_pending", "pending final revision freeze" in release_notes_text, WORKFLOW_RELEASE_NOTES_REL)
    if checklist_path.exists():
        checklist_text = checklist_path.read_text(encoding="utf-8")
        add(checks, "software_availability", "archive_checklist_license_gate", "Workflow repository license is confirmed by the authors" in checklist_text, WORKFLOW_ARCHIVE_READINESS_REL)
        add(checks, "software_availability", "archive_checklist_final_doi_gate", "Final workflow release is archived with a permanent DOI" in checklist_text, WORKFLOW_ARCHIVE_READINESS_REL)

    article_text = (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8")
    corrections_text = (corrections_root / "main.tex").read_text(encoding="utf-8")
    required_article = [
        r"CRAN R package \texttt{exdqlm}",
        f"version {CRAN_EXDQLM_VERSION}",
        CRAN_EXDQLM_URL,
        CRAN_EXDQLM_DOI_URL,
        EXDQLM_SOFTWARE_PAPER_BIBTEX_KEY,
        PROJECT1_URL,
        "compact provenance manifests",
    ]
    required_corrections = [
        r"CRAN R package \texttt{exdqlm}",
        f"version {CRAN_EXDQLM_VERSION}",
        CRAN_EXDQLM_URL,
        CRAN_EXDQLM_DOI_URL,
        EXDQLM_SOFTWARE_PAPER_DOI_URL,
        PROJECT1_URL,
        "compact provenance manifests",
    ]
    if archive_check.is_pending:
        required_article.append("permanent archival release of the workflow repository will be created")
        required_corrections.append("Before final resubmission")
    elif archive_check.is_final:
        required_article.append(archive_check.doi)
        required_corrections.append(archive_check.doi)
    for claim in required_article:
        add(checks, "software_availability", f"article_required:{claim}", claim in article_text, claim)
    for claim in required_corrections:
        add(checks, "software_availability", f"corrections_required:{claim}", claim in corrections_text, claim)
    if archive_check.is_pending:
        premature_archive_claims = [
            "workflow repository has been archived",
            "workflow has been archived",
            "archived workflow DOI",
        ]
        for claim in premature_archive_claims:
            add(checks, "software_availability", f"article_no_premature_archive_claim:{claim}", claim not in article_text, claim)
            add(checks, "software_availability", f"corrections_no_premature_archive_claim:{claim}", claim not in corrections_text, claim)
            for rel in WORKFLOW_RELEASE_READINESS_RELS:
                readiness_path = workflow_root / rel
                if readiness_path.exists():
                    readiness_text = readiness_path.read_text(encoding="utf-8")
                    add(checks, "software_availability", f"workflow_no_premature_archive_claim:{rel}:{claim}", claim not in readiness_text, claim)
    elif archive_check.is_final:
        stale_pending_claims = [
            "permanent archival release of the workflow repository will be created",
            "Before final resubmission, we will archive",
            "workflow archive DOI: pending",
        ]
        for claim in stale_pending_claims:
            add(checks, "software_availability", f"article_no_stale_pending_claim:{claim}", claim not in article_text, claim)
            add(checks, "software_availability", f"corrections_no_stale_pending_claim:{claim}", claim not in corrections_text, claim)


def check_runtime_feasibility(workflow_root: Path, article_root: Path, corrections_root: Path, checks: list[Check]) -> None:
    manifest_path = article_root / RUNTIME_MANIFEST_REL
    add(checks, "runtime_feasibility", "manifest_exists", manifest_path.exists(), RUNTIME_MANIFEST_REL)
    if not manifest_path.exists():
        return

    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    for row in check_runtime_manifest(manifest):
        add(checks, "runtime_feasibility", row.item, row.ok, row.detail)

    workflow_doc = workflow_root / RUNTIME_CONTRACT_REL
    article_doc = article_root / ARTICLE_RUNTIME_DOC_REL
    add(checks, "runtime_feasibility", "workflow_contract_doc", workflow_doc.exists(), RUNTIME_CONTRACT_REL)
    add(checks, "runtime_feasibility", "article_contract_doc", article_doc.exists(), ARTICLE_RUNTIME_DOC_REL)

    article_text = (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8")
    corrections_text = (corrections_root / "main.tex").read_text(encoding="utf-8")
    for claim in REQUIRED_RUNTIME_ARTICLE_CLAIMS:
        add(checks, "runtime_feasibility", f"article_required:{claim}", claim in article_text, claim)
    for claim in REQUIRED_RUNTIME_CORRECTIONS_CLAIMS:
        add(checks, "runtime_feasibility", f"corrections_required:{claim}", claim in corrections_text, claim)
    for claim in FORBIDDEN_RUNTIME_DECOMPOSITION_CLAIMS:
        add(checks, "runtime_feasibility", f"article_forbidden:{claim}", claim not in article_text, claim)
        add(checks, "runtime_feasibility", f"corrections_forbidden:{claim}", claim not in corrections_text, claim)


def check_forecast_design(workflow_root: Path, article_root: Path, corrections_root: Path, checks: list[Check]) -> None:
    manifest_path = article_root / FORECAST_DESIGN_MANIFEST_REL
    add(checks, "forecast_design", "manifest_exists", manifest_path.exists(), FORECAST_DESIGN_MANIFEST_REL)
    if not manifest_path.exists():
        return

    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    for row in check_forecast_design_manifest(manifest):
        add(checks, "forecast_design", row.item, row.ok, row.detail)

    workflow_doc = workflow_root / FORECAST_DESIGN_CONTRACT_REL
    article_doc = article_root / ARTICLE_FORECAST_DESIGN_DOC_REL
    add(checks, "forecast_design", "workflow_contract_doc", workflow_doc.exists(), FORECAST_DESIGN_CONTRACT_REL)
    add(checks, "forecast_design", "article_contract_doc", article_doc.exists(), ARTICLE_FORECAST_DESIGN_DOC_REL)

    article_text = (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8")
    corrections_text = (corrections_root / "main.tex").read_text(encoding="utf-8")
    for claim in REQUIRED_FORECAST_DESIGN_ARTICLE_CLAIMS:
        add(checks, "forecast_design", f"article_required:{claim}", claim in article_text, claim)
    for claim in REQUIRED_FORECAST_DESIGN_CORRECTIONS_CLAIMS:
        add(checks, "forecast_design", f"corrections_required:{claim}", claim in corrections_text, claim)
    for claim in FORBIDDEN_FORECAST_DESIGN_CLAIMS:
        add(checks, "forecast_design", f"article_forbidden:{claim}", claim not in article_text, claim)
        add(checks, "forecast_design", f"corrections_forbidden:{claim}", claim not in corrections_text, claim)


def check_latest_forecast_issue(workflow_root: Path, article_root: Path, corrections_root: Path, checks: list[Check]) -> None:
    manifest_path = article_root / LATEST_FORECAST_ISSUE_MANIFEST_REL
    add(checks, "latest_forecast_issue", "manifest_exists", manifest_path.exists(), LATEST_FORECAST_ISSUE_MANIFEST_REL)
    if not manifest_path.exists():
        return

    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    for row in check_latest_forecast_issue_manifest(manifest, workflow_root=workflow_root):
        add(checks, "latest_forecast_issue", row.item, row.ok, row.detail)

    workflow_doc = workflow_root / LATEST_FORECAST_ISSUE_CONTRACT_REL
    article_doc = article_root / ARTICLE_LATEST_FORECAST_ISSUE_DOC_REL
    add(checks, "latest_forecast_issue", "workflow_contract_doc", workflow_doc.exists(), LATEST_FORECAST_ISSUE_CONTRACT_REL)
    add(checks, "latest_forecast_issue", "article_contract_doc", article_doc.exists(), ARTICLE_LATEST_FORECAST_ISSUE_DOC_REL)

    article_text = (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8")
    corrections_text = (corrections_root / "main.tex").read_text(encoding="utf-8")
    for claim in REQUIRED_LATEST_FORECAST_ARTICLE_CLAIMS:
        add(checks, "latest_forecast_issue", f"article_required:{claim}", claim in article_text, claim)
    for claim in REQUIRED_LATEST_FORECAST_CORRECTIONS_CLAIMS:
        add(checks, "latest_forecast_issue", f"corrections_required:{claim}", claim in corrections_text, claim)
    for claim in FORBIDDEN_LATEST_FORECAST_ARTICLE_CLAIMS:
        add(checks, "latest_forecast_issue", f"article_forbidden:{claim}", claim not in article_text, claim)


def check_reviewer1_overview(workflow_root: Path, article_root: Path, corrections_root: Path, checks: list[Check]) -> None:
    workflow_doc = workflow_root / R1_OVERVIEW_CONTRACT_REL
    article_doc = article_root / ARTICLE_R1_OVERVIEW_DOC_REL
    add(checks, "reviewer1_overview", "workflow_contract_doc", workflow_doc.exists(), R1_OVERVIEW_CONTRACT_REL)
    add(checks, "reviewer1_overview", "article_contract_doc", article_doc.exists(), ARTICLE_R1_OVERVIEW_DOC_REL)

    article_text = (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8")
    corrections_text = (corrections_root / "main.tex").read_text(encoding="utf-8")
    for row in check_r1_overview_text(article_text, corrections_text):
        add(checks, "reviewer1_overview", row.item, row.ok, row.detail)


def check_reviewer1_uncertainty(workflow_root: Path, article_root: Path, corrections_root: Path, checks: list[Check]) -> None:
    workflow_doc = workflow_root / R1_UNCERTAINTY_CONTRACT_REL
    article_doc = article_root / ARTICLE_R1_UNCERTAINTY_DOC_REL
    add(checks, "reviewer1_uncertainty", "workflow_contract_doc", workflow_doc.exists(), R1_UNCERTAINTY_CONTRACT_REL)
    add(checks, "reviewer1_uncertainty", "article_contract_doc", article_doc.exists(), ARTICLE_R1_UNCERTAINTY_DOC_REL)

    article_text = (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8")
    corrections_text = (corrections_root / "main.tex").read_text(encoding="utf-8")
    for row in check_r1_uncertainty_text(article_text, corrections_text):
        add(checks, "reviewer1_uncertainty", row.item, row.ok, row.detail)


def check_reviewer1_remaining(workflow_root: Path, article_root: Path, corrections_root: Path, checks: list[Check]) -> None:
    workflow_doc = workflow_root / R1_REMAINING_CONTRACT_REL
    article_doc = article_root / ARTICLE_R1_REMAINING_DOC_REL
    add(checks, "reviewer1_remaining", "workflow_contract_doc", workflow_doc.exists(), R1_REMAINING_CONTRACT_REL)
    add(checks, "reviewer1_remaining", "article_contract_doc", article_doc.exists(), ARTICLE_R1_REMAINING_DOC_REL)

    generated_tables = "\n".join(
        path.read_text(encoding="utf-8")
        for path in sorted((article_root / "tables" / "generated_tex").glob("*.tex"))
    )
    article_text = (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8") + "\n" + generated_tables
    corrections_text = (corrections_root / "main.tex").read_text(encoding="utf-8")
    for row in check_reviewer1_remaining_text(article_text, corrections_text):
        add(checks, "reviewer1_remaining", row.item, row.ok, row.detail)


def repo_metadata(repo: Path) -> dict[str, str]:
    return {
        "path": str(repo),
        "branch": git_value(repo, "rev-parse", "--abbrev-ref", "HEAD"),
        "head": git_value(repo, "rev-parse", "--short", "HEAD"),
        "status_short": git_value(repo, "status", "--short"),
    }


def render_summary(checks: list[Check], metadata: dict[str, object]) -> str:
    failed = [check for check in checks if check.status != "pass"]
    counts: dict[str, dict[str, int]] = {}
    for check in checks:
        counts.setdefault(check.family, {"pass": 0, "fail": 0})
        counts[check.family]["pass" if check.status == "pass" else "fail"] += 1
    lines = [
        "# Publication Freeze Validation",
        "",
        f"- Timestamp UTC: `{metadata['timestamp_utc']}`",
        f"- Overall status: `{'pass' if not failed else 'fail'}`",
        f"- Failed checks: `{len(failed)}`",
        "",
        "## Repositories",
        "",
        "| repo | branch | head | dirty |",
        "|---|---|---|---|",
    ]
    for name, meta in metadata["repos"].items():
        dirty = "yes" if str(meta["status_short"]).strip() else "no"
        lines.append(f"| {name} | `{meta['branch']}` | `{meta['head']}` | {dirty} |")
    lines.extend(["", "## Check Families", "", "| family | pass | fail |", "|---|---:|---:|"])
    for family in sorted(counts):
        row = counts[family]
        lines.append(f"| {family} | {row['pass']} | {row['fail']} |")
    if failed:
        lines.extend(["", "## Failures", "", "| family | item | detail |", "|---|---|---|"])
        for check in failed:
            lines.append(f"| {check.family} | `{check.item}` | {check.detail} |")
    return "\n".join(lines) + "\n"


def parse_args(argv: Iterable[str] | None = None) -> argparse.Namespace:
    ap = argparse.ArgumentParser(description="Validate the current cross-repo publication freeze.")
    ap.add_argument("--workflow-root", type=Path, default=ROOT)
    ap.add_argument("--article-root", type=Path, default=ROOT / "Evironmetrics---REVISED-DOC-Corrected-2")
    ap.add_argument("--corrections-root", type=Path, default=Path("/data/muscat_data/jaguir26/Corrections---Project-1"))
    ap.add_argument(
        "--he3-runtime-root",
        type=Path,
        default=HE3_CURRENT_RUNTIME_ROOT,
    )
    ap.add_argument("--report-dir", type=Path, default=ROOT / "reports/publication_freeze_validation_20260614")
    ap.add_argument("--require-clean", action="store_true")
    return ap.parse_args(argv)


def main(argv: Iterable[str] | None = None) -> int:
    args = parse_args(argv)
    workflow_root = args.workflow_root.resolve()
    article_root = args.article_root.resolve()
    corrections_root = args.corrections_root.resolve()
    he3_runtime_root = args.he3_runtime_root.resolve()
    report_dir = args.report_dir.resolve()
    report_dir.mkdir(parents=True, exist_ok=True)

    checks: list[Check] = []
    metadata: dict[str, object] = {
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "argv": list(argv or sys.argv[1:]),
        "repos": {
            "workflow": repo_metadata(workflow_root),
            "article": repo_metadata(article_root),
            "corrections": repo_metadata(corrections_root),
        },
        "he3_runtime_root": str(he3_runtime_root),
    }
    if args.require_clean:
        for name, meta in metadata["repos"].items():
            add(checks, "repo_clean", name, not str(meta["status_short"]).strip(), str(meta["status_short"]))

    check_he2_selective_manifest(workflow_root=workflow_root, article_root=article_root, checks=checks)
    check_he3_authoritative(
        workflow_root=workflow_root,
        article_root=article_root,
        corrections_root=corrections_root,
        he3_runtime_root=he3_runtime_root,
        checks=checks,
    )
    check_he4_sync(article_root, corrections_root, checks)
    check_selected_figures(article_root, checks)
    check_prior_claim_contract(article_root, corrections_root, checks)
    check_prose(article_root, corrections_root, checks)
    check_forecast_design(workflow_root, article_root, corrections_root, checks)
    check_latest_forecast_issue(workflow_root, article_root, corrections_root, checks)
    check_reviewer1_overview(workflow_root, article_root, corrections_root, checks)
    check_reviewer1_uncertainty(workflow_root, article_root, corrections_root, checks)
    check_reviewer1_remaining(workflow_root, article_root, corrections_root, checks)
    check_runtime_feasibility(workflow_root, article_root, corrections_root, checks)
    check_software_availability(workflow_root, article_root, corrections_root, checks)

    rows = [{"family": c.family, "item": c.item, "status": c.status, "detail": c.detail} for c in checks]
    write_csv(report_dir / "publication_freeze_validation_checks.csv", rows, ["family", "item", "status", "detail"])
    payload = {
        "metadata": metadata,
        "total_checks": len(checks),
        "failed_checks": sum(1 for c in checks if c.status != "pass"),
        "status": "pass" if all(c.status == "pass" for c in checks) else "fail",
    }
    (report_dir / "publication_freeze_validation_summary.json").write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    (report_dir / "PUBLICATION_FREEZE_VALIDATION.md").write_text(render_summary(checks, metadata), encoding="utf-8")

    if payload["status"] != "pass":
        print(f"Publication freeze validation failed: {report_dir}")
        return 1
    print(f"Publication freeze validation passed: {report_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
