#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any

import yaml

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_AUTHORITY = ROOT / "docs" / "authoritative_selected_outputs" / "he2_exal_m_t1_representative_20221225.yaml"
DEFAULT_ARTICLE_ROOT = ROOT / "Evironmetrics---REVISED-DOC-Corrected-2"

sys.path.insert(0, str(ROOT / "scripts"))
from he2_exdqlm_keep_authoritative import load_authoritative_spec  # noqa: E402


def load_yaml(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as handle:
        payload = yaml.safe_load(handle) or {}
    if not isinstance(payload, dict):
        raise ValueError(f"YAML root must be a mapping: {path}")
    return payload


def load_json(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as handle:
        payload = json.load(handle)
    if not isinstance(payload, dict):
        raise ValueError(f"JSON root must be a mapping: {path}")
    return payload


def normalize_cutoff(value: Any) -> str:
    raw = str(value).strip()
    return raw.replace("-", "")


def as_float(value: Any) -> float:
    return float(value)


def add_check(rows: list[dict[str, str]], check: str, ok: bool, detail: str) -> None:
    rows.append({"check": check, "status": "PASS" if ok else "FAIL", "detail": detail})


def validate_authority(
    authority_path: Path = DEFAULT_AUTHORITY,
    article_root: Path | None = DEFAULT_ARTICLE_ROOT,
    require_article_bundle: bool = True,
) -> list[dict[str, str]]:
    payload = load_yaml(authority_path)
    authority = payload.get("authority", {})
    if not isinstance(authority, dict):
        raise ValueError(f"authority must be a mapping: {authority_path}")

    winner_manifest = (ROOT / str(authority["authoritative_winner_manifest"])).resolve()
    spec = load_authoritative_spec(winner_manifest)
    winners = spec.winner_by_cutoff()
    cutoff = normalize_cutoff(authority["selected_cutoff"])
    winner = winners.get(cutoff)

    rows: list[dict[str, str]] = []
    add_check(rows, "selected_cutoff_in_winner_manifest", winner is not None, cutoff)
    if winner is None:
        return rows

    add_check(rows, "model_family_matches", authority.get("model_family") == spec.model_family, f"{authority.get('model_family')} vs {spec.model_family}")
    add_check(rows, "model_label_matches", authority.get("model_label") == spec.manuscript_label, f"{authority.get('model_label')} vs {spec.manuscript_label}")
    add_check(rows, "model_id_matches", authority.get("model_id") == spec.model_id, f"{authority.get('model_id')} vs {spec.model_id}")
    add_check(rows, "spec_id_matches", authority.get("spec_id") == winner.grid_spec_id, f"{authority.get('spec_id')} vs {winner.grid_spec_id}")
    add_check(rows, "discount_case_matches", authority.get("discount_case_id") == winner.discount_case_id, f"{authority.get('discount_case_id')} vs {winner.discount_case_id}")
    add_check(rows, "epsilon_matches", as_float(authority.get("epsilon_value")) == winner.epsilon_value, f"{authority.get('epsilon_value')} vs {winner.epsilon_value}")
    add_check(rows, "c_factor_matches", as_float(authority.get("c_factor")) == winner.c_factor, f"{authority.get('c_factor')} vs {winner.c_factor}")
    add_check(rows, "run_id_matches", authority.get("run_id") == winner.run_id, f"{authority.get('run_id')} vs {winner.run_id}")
    add_check(rows, "runtime_root_matches", Path(str(authority.get("runtime_root"))).resolve() == spec.runtime_root, f"{authority.get('runtime_root')} vs {spec.runtime_root}")
    add_check(rows, "runtime_run_root_matches", Path(str(authority.get("runtime_run_root"))).resolve() == spec.run_root(winner), f"{authority.get('runtime_run_root')} vs {spec.run_root(winner)}")
    add_check(rows, "runtime_output_root_matches", Path(str(authority.get("runtime_output_root"))).resolve() == spec.output_root(winner), f"{authority.get('runtime_output_root')} vs {spec.output_root(winner)}")
    add_check(rows, "score_scale_matches", authority.get("score_scale") == spec.score_scale, f"{authority.get('score_scale')} vs {spec.score_scale}")
    add_check(rows, "figure_scale_matches", authority.get("figure_scale") == spec.metadata.get("figure_scale"), f"{authority.get('figure_scale')} vs {spec.metadata.get('figure_scale')}")
    add_check(rows, "active_quantiles_matches", authority.get("active_quantiles") == spec.metadata.get("active_quantiles"), f"{authority.get('active_quantiles')} vs {spec.metadata.get('active_quantiles')}")
    add_check(rows, "max_iter_matches", int(authority.get("max_iter")) == int(spec.metadata.get("max_iter")), f"{authority.get('max_iter')} vs {spec.metadata.get('max_iter')}")
    add_check(rows, "data_start_matches", authority.get("data_start") == spec.metadata.get("data_start"), f"{authority.get('data_start')} vs {spec.metadata.get('data_start')}")

    discount = authority.get("discount_factors", {})
    for key, winner_attr in [
        ("df_t", winner.df_t),
        ("df_s1", winner.df_s1),
        ("df_s2", winner.df_s2),
        ("df_s67", winner.df_s67),
        ("df_discrep", winner.df_discrep),
        ("lambda", winner.lambda_value),
        ("df_trans", winner.df_trans),
        ("df_covs", winner.df_covs),
    ]:
        add_check(rows, f"{key}_matches", abs(as_float(discount.get(key)) - winner_attr) <= 1e-15, f"{discount.get(key)} vs {winner_attr}")

    selected_score = authority.get("selected_score", {})
    for key in ["mean_crps", "median_crps", "max_crps", "runner_up_mean_crps", "winner_runner_abs_diff"]:
        add_check(rows, f"{key}_matches", abs(as_float(selected_score.get(key)) - as_float(getattr(winner, key))) <= 1e-12, f"{selected_score.get(key)} vs {getattr(winner, key)}")
    add_check(rows, "runner_up_spec_matches", selected_score.get("runner_up_grid_spec_id") == winner.runner_up_grid_spec_id, f"{selected_score.get('runner_up_grid_spec_id')} vs {winner.runner_up_grid_spec_id}")

    missing_outputs = [str(path) for path in spec.required_output_paths(winner) if not path.exists()]
    add_check(rows, "runtime_required_outputs_present", not missing_outputs, ";".join(missing_outputs))
    retained = spec.rdata_files(winner)
    expected_rdata = bool(authority.get("cleanup_policy", {}).get("current_selected_run_rdata_expected", False))
    add_check(rows, "runtime_rdata_cleanup_state_matches", bool(retained) == expected_rdata, f"retained_rdata_count={len(retained)} expected_present={expected_rdata}")

    if article_root is not None:
        article_root = article_root.resolve()
        bundle_rel = str(authority.get("article_sync_contract", {}).get("representative_bundle_relpath", ""))
        bundle_meta = article_root / bundle_rel / "bundle_metadata.json"
        if not bundle_meta.exists():
            add_check(rows, "article_representative_bundle_metadata_present", not require_article_bundle, str(bundle_meta))
        else:
            add_check(rows, "article_representative_bundle_metadata_present", True, str(bundle_meta))
            bundle = load_json(bundle_meta)
            add_check(rows, "article_bundle_cutoff_matches", normalize_cutoff(bundle.get("cutoff")) == cutoff, f"{bundle.get('cutoff')} vs {cutoff}")
            add_check(rows, "article_bundle_run_id_matches", bundle.get("run_id") == winner.run_id, f"{bundle.get('run_id')} vs {winner.run_id}")
            add_check(rows, "article_bundle_run_root_matches", Path(str(bundle.get("runtime_run_root"))).resolve() == spec.run_root(winner), f"{bundle.get('runtime_run_root')} vs {spec.run_root(winner)}")
            add_check(rows, "article_bundle_output_root_matches", Path(str(bundle.get("runtime_output_root"))).resolve() == spec.output_root(winner), f"{bundle.get('runtime_output_root')} vs {spec.output_root(winner)}")

    return rows


def write_report(rows: list[dict[str, str]], report_path: Path) -> None:
    report_path.parent.mkdir(parents=True, exist_ok=True)
    status = "PASS" if all(row["status"] == "PASS" for row in rows) else "FAIL"
    lines = [
        "# HE2 Selected Output Authority Validation",
        "",
        f"Result: `{status}`",
        "",
        "| Check | Status | Detail |",
        "|---|---|---|",
    ]
    for row in rows:
        detail = row["detail"].replace("|", "\\|")
        lines.append(f"| `{row['check']}` | `{row['status']}` | {detail} |")
    lines.append("")
    report_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate the HE2 representative selected-output authority manifest.")
    parser.add_argument("--authority", type=Path, default=DEFAULT_AUTHORITY)
    parser.add_argument("--article-root", type=Path, default=DEFAULT_ARTICLE_ROOT)
    parser.add_argument("--no-article-bundle", action="store_true")
    parser.add_argument("--report", type=Path, default=ROOT / "reports" / "selected_output_lineage" / "HE2_SELECTED_OUTPUT_AUTHORITY_VALIDATION.md")
    args = parser.parse_args()

    rows = validate_authority(
        authority_path=args.authority.resolve(),
        article_root=None if args.no_article_bundle else args.article_root.resolve(),
        require_article_bundle=not args.no_article_bundle,
    )
    write_report(rows, args.report.resolve())
    print(args.report.resolve())
    return 0 if all(row["status"] == "PASS" for row in rows) else 1


if __name__ == "__main__":
    raise SystemExit(main())
