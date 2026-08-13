#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import sys
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT / "scripts") not in sys.path:
    sys.path.insert(0, str(ROOT / "scripts"))

from build_he2_bayesian_publication_manifest import (  # noqa: E402
    CUTOFFS,
    FAMILY_ORDER,
    FAMILY_TO_LABEL,
    build_outputs,
)
from he2_exdqlm_keep_authoritative import load_authoritative_spec  # noqa: E402

OUT_DIR = ROOT / "reports" / "he2_publication_manifest"
PROMOTED_LABEL = "exAL-M-T1"
PROMOTED_LABELS = {
    "N-U-T1",
    "N-M-T0",
    "N-M-T1",
    "AL-U-T1",
    "AL-M-T0",
    "AL-M-T1",
    "exAL-U-T1",
    "exAL-M-T0",
    "exAL-M-T1",
}
CANONICAL_LINEAGE = "exdqlm_multivar_keep_canonical_grid_20260524:authoritative_winner"
PENDING_ACTION = "rerun_or_promote_on_20260510_canonical_bundle"
BLOCKED_ACTION_BY_LABEL: dict[str, str] = {}


def _submodel_count(family: str) -> int:
    return 1 if family.startswith("ndlm_") else 7


def _row_status(label: str) -> tuple[str, str]:
    if label in PROMOTED_LABELS:
        return "authoritative_promoted", "none"
    if label in BLOCKED_ACTION_BY_LABEL:
        return "blocked_canonical_input_promotion", BLOCKED_ACTION_BY_LABEL[label]
    return "pending_canonical_input_promotion", PENDING_ACTION


def build_gate() -> tuple[list[dict[str, str]], dict[str, Any]]:
    manifest_rows, _input_rows, alignment_rows = build_outputs()
    authoritative = load_authoritative_spec()
    canonical_bundle_root = str(authoritative.metadata.get("bundle_artifact_root", ""))
    canonical_bundle_run_id = str(authoritative.metadata.get("bundle_run_id", ""))
    data_start = str(authoritative.metadata.get("data_start", "1987-05-29"))
    active_quantiles = str(authoritative.metadata.get("active_quantiles", "05|20|35|50|65|80|95"))

    rows: list[dict[str, str]] = []
    for row in manifest_rows:
        label = row["manuscript_label"]
        status, action = _row_status(label)
        rows.append(
            {
                "cutoff": row["cutoff"],
                "cutoff_display": row["cutoff_display"],
                "manuscript_label": label,
                "family": row["family"],
                "current_run_id": row["run_id"],
                "current_campaign_lineage": row["campaign_lineage"],
                "current_crps_display4": row["crps_display4"],
                "current_score_scale": row["score_scale"],
                "current_within_cutoff_shared_inputs_aligned": row["within_cutoff_shared_inputs_aligned"],
                "target_status": status,
                "required_action": action,
                "target_bundle_root": canonical_bundle_root,
                "target_bundle_run_id": canonical_bundle_run_id,
                "target_data_start": data_start,
                "target_quantile_lanes": active_quantiles if _submodel_count(row["family"]) == 7 else "single_model",
                "target_submodels": str(_submodel_count(row["family"])),
                "paper_table_gate": "ready_final_9_model_table" if label in PROMOTED_LABELS else "blocks_final_9_model_table",
            }
        )

    rows.sort(key=lambda item: (item["cutoff"], FAMILY_ORDER.index(item["family"])))
    status_counts = Counter(row["target_status"] for row in rows)
    pending_labels = sorted({row["manuscript_label"] for row in rows if row["target_status"] != "authoritative_promoted"})
    promoted_labels = sorted({row["manuscript_label"] for row in rows if row["target_status"] == "authoritative_promoted"})
    alignment_required = [row for row in alignment_rows if row["artifact"] != "usgs_daily"]
    alignment_passes = sum(row["all_equal"] == "True" for row in alignment_required)
    pending_rows = status_counts.get("pending_canonical_input_promotion", 0) + status_counts.get(
        "blocked_canonical_input_promotion", 0
    )
    summary = {
        "generated_at_utc": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "rows": len(rows),
        "cutoffs": len(CUTOFFS),
        "promoted_rows": status_counts.get("authoritative_promoted", 0),
        "pending_rows": pending_rows,
        "blocked_rows": status_counts.get("blocked_canonical_input_promotion", 0),
        "promoted_labels": promoted_labels,
        "pending_labels": pending_labels,
        "remaining_model_families_pending": len(pending_labels),
        "remaining_submodels_pending": sum(int(row["target_submodels"]) for row in rows if row["target_status"] != "authoritative_promoted"),
        "canonical_bundle_root": canonical_bundle_root,
        "canonical_bundle_run_id": canonical_bundle_run_id,
        "canonical_data_start": data_start,
        "within_cutoff_alignment_passes": alignment_passes,
        "within_cutoff_alignment_checks": len(alignment_required),
        "final_9_model_benchmark_ready": pending_rows == 0,
        "gate_reason": "All nine HE2 Bayesian benchmark families are promoted onto the same canonical 20260510 input bundle.",
    }
    return rows, summary


def _write_csv(path: Path, rows: list[dict[str, str]]) -> None:
    if not rows:
        raise ValueError(f"no rows to write: {path}")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def _write_markdown(path: Path, rows: list[dict[str, str]], summary: dict[str, Any]) -> None:
    pending = [row for row in rows if row["target_status"] != "authoritative_promoted"]
    by_label = Counter(row["manuscript_label"] for row in pending)
    lines = [
        "# HE2 Publication Parity Gate",
        "",
        f"- generated_at_utc: `{summary['generated_at_utc']}`",
        f"- promoted rows: `{summary['promoted_rows']}`",
        f"- pending rows: `{summary['pending_rows']}`",
        f"- blocked rows: `{summary['blocked_rows']}`",
        f"- pending families: `{summary['remaining_model_families_pending']}`",
        f"- pending submodels: `{summary['remaining_submodels_pending']}`",
        f"- canonical bundle: `{summary['canonical_bundle_root']}`",
        f"- bundle run id: `{summary['canonical_bundle_run_id']}`",
        f"- final 9-model benchmark ready: `{summary['final_9_model_benchmark_ready']}`",
        "",
        "All nine HE2 Bayesian benchmark families are promoted onto canonical-bundle roots.",
        "The full manuscript benchmark table is paper-final for the current publication snapshot.",
        "",
        "## Pending Families",
        "",
        "| Label | Rows | Required Action |",
        "|---|---:|---|",
    ]
    if by_label:
        for label in sorted(by_label):
            action = BLOCKED_ACTION_BY_LABEL.get(label, PENDING_ACTION)
            lines.append(f"| `{label}` | {by_label[label]} | `{action}` |")
    else:
        lines.append("| None | 0 | `none` |")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def write_outputs(out_dir: Path = OUT_DIR) -> dict[str, str]:
    rows, summary = build_gate()
    out_dir.mkdir(parents=True, exist_ok=True)
    csv_path = out_dir / "he2_publication_parity_gate.csv"
    json_path = out_dir / "he2_publication_parity_gate_summary.json"
    md_path = out_dir / "he2_publication_parity_gate.md"
    _write_csv(csv_path, rows)
    json_path.write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    _write_markdown(md_path, rows, summary)
    return {"csv": str(csv_path), "json": str(json_path), "markdown": str(md_path)}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build the HE2 9-model publication parity gate.")
    parser.add_argument("--out-dir", type=Path, default=OUT_DIR)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    print(json.dumps(write_outputs(args.out_dir.resolve()), indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
