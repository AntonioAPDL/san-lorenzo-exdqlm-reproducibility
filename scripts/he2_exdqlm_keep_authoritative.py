#!/usr/bin/env python3
from __future__ import annotations

import csv
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any, Iterable

import yaml

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_MANIFEST = ROOT / "docs" / "exdqlm_multivar_keep_authoritative_specs_20260601.yaml"
DEFAULT_RUNTIME_ROOT = (
    ROOT.parent / "project1_ucsc_phd_runtime" / "multimodel_v8_he2_exdqlm_multivar_keep_epsilon_discount_grid_20260524"
)
EXPECTED_CUTOFFS = ["20210123", "20211112", "20211221", "20220511", "20221225"]
EXPECTED_QUANTILES = [0.05, 0.20, 0.35, 0.50, 0.65, 0.80, 0.95]
EXPECTED_QUANTILE_LABELS = "05|20|35|50|65|80|95"
TARGET_MODEL_ID = "exdqlm_multivar_synth_keep"
TARGET_FAMILY = "exdqlm_multivar_keep"
TARGET_LABEL = "exAL-M-T1"
TARGET_SCORE_SCALE = "log_cms_plus1"

REQUIRED_OUTPUT_FILES = [
    "figure_manifest.csv",
    "publication_figure_manifest.csv",
    "publication_style_used.yaml",
    "exdqlm_multivar_synth_keep_cutoff_window_posterior_samples.png",
    "exdqlm_multivar_synth_keep_cutoff_window_posterior_samples.pdf",
    "exdqlm_multivar_synth_keep_cutoff_window_posterior_samples_with_raw_ensembles.png",
    "exdqlm_multivar_synth_keep_cutoff_window_posterior_samples_with_raw_ensembles.pdf",
    "exdqlm_multivar_synth_keep_cutoff_window_quantiles.csv",
    "exdqlm_multivar_synth_keep_cutoff_window_sample_subset.csv",
    "tables/crps_forecast_summary.csv",
    "tables/crps_forecast_per_time.csv",
    "tables/crps_input_health.csv",
    "tables/covariate_effects_summary.csv",
    "tables/gamma_summary.csv",
    "tables/sigma_summary.csv",
    "tables/posterior_table_exports_manifest.csv",
    "tables/posterior_table_exports_README.md",
]


def load_yaml(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as handle:
        payload = yaml.safe_load(handle) or {}
    if not isinstance(payload, dict):
        raise ValueError(f"YAML root must be a mapping: {path}")
    return payload


def read_csv_rows(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def cutoff_display(cutoff: str) -> str:
    cutoff = str(cutoff).zfill(8)
    return datetime.strptime(cutoff, "%Y%m%d").strftime("%m/%d/%Y")


def cutoff_dash(cutoff: str) -> str:
    cutoff = str(cutoff).zfill(8)
    return f"{cutoff[:4]}-{cutoff[4:6]}-{cutoff[6:8]}"


def _as_float(value: Any) -> float:
    return float(value)


@dataclass(frozen=True)
class AuthoritativeWinner:
    cutoff: str
    grid_spec_id: str
    discount_case_id: str
    epsilon_value: float
    c_factor: float
    df_t: float
    df_s1: float
    df_s2: float
    df_s67: float
    df_discrep: float
    lambda_value: float
    df_trans: float
    df_covs: float
    run_id: str
    mean_crps: float
    median_crps: float
    max_crps: float
    runner_up_grid_spec_id: str
    runner_up_mean_crps: float
    winner_runner_abs_diff: float

    @classmethod
    def from_mapping(cls, row: dict[str, Any]) -> "AuthoritativeWinner":
        return cls(
            cutoff=str(row["cutoff"]).zfill(8),
            grid_spec_id=str(row["grid_spec_id"]),
            discount_case_id=str(row["discount_case_id"]),
            epsilon_value=_as_float(row["epsilon_value"]),
            c_factor=_as_float(row["c_factor"]),
            df_t=_as_float(row["df_t"]),
            df_s1=_as_float(row["df_s1"]),
            df_s2=_as_float(row["df_s2"]),
            df_s67=_as_float(row["df_s67"]),
            df_discrep=_as_float(row["df_discrep"]),
            lambda_value=_as_float(row["lambda"]),
            df_trans=_as_float(row["df_trans"]),
            df_covs=_as_float(row["df_covs"]),
            run_id=str(row["run_id"]),
            mean_crps=_as_float(row["mean_crps"]),
            median_crps=_as_float(row["median_crps"]),
            max_crps=_as_float(row["max_crps"]),
            runner_up_grid_spec_id=str(row["runner_up_grid_spec_id"]),
            runner_up_mean_crps=_as_float(row["runner_up_mean_crps"]),
            winner_runner_abs_diff=_as_float(row["winner_runner_abs_diff"]),
        )

    def as_row(self) -> dict[str, Any]:
        return {
            "cutoff": self.cutoff,
            "cutoff_display": cutoff_display(self.cutoff),
            "grid_spec_id": self.grid_spec_id,
            "discount_case_id": self.discount_case_id,
            "epsilon_value": self.epsilon_value,
            "c_factor": self.c_factor,
            "df_t": self.df_t,
            "df_s1": self.df_s1,
            "df_s2": self.df_s2,
            "df_s67": self.df_s67,
            "df_discrep": self.df_discrep,
            "lambda": self.lambda_value,
            "df_trans": self.df_trans,
            "df_covs": self.df_covs,
            "run_id": self.run_id,
            "mean_crps": self.mean_crps,
            "median_crps": self.median_crps,
            "max_crps": self.max_crps,
            "runner_up_grid_spec_id": self.runner_up_grid_spec_id,
            "runner_up_mean_crps": self.runner_up_mean_crps,
            "winner_runner_abs_diff": self.winner_runner_abs_diff,
        }


class AuthoritativeSpec:
    def __init__(self, manifest_path: Path = DEFAULT_MANIFEST) -> None:
        self.manifest_path = manifest_path.resolve()
        self.payload = load_yaml(self.manifest_path)
        self.metadata = self.payload.get("metadata", {}) if isinstance(self.payload.get("metadata"), dict) else {}
        winners_raw = self.payload.get("winners", [])
        if not isinstance(winners_raw, list):
            raise ValueError(f"winners must be a list: {self.manifest_path}")
        self.winners = [AuthoritativeWinner.from_mapping(row) for row in winners_raw]
        self._validate_static_contract()

    @property
    def runtime_root(self) -> Path:
        return Path(str(self.metadata.get("runtime_root") or DEFAULT_RUNTIME_ROOT)).expanduser().resolve()

    @property
    def model_family(self) -> str:
        return str(self.metadata.get("model_family") or TARGET_FAMILY)

    @property
    def manuscript_label(self) -> str:
        return str(self.metadata.get("manuscript_label") or TARGET_LABEL)

    @property
    def model_id(self) -> str:
        return str(self.metadata.get("model_id") or TARGET_MODEL_ID)

    @property
    def score_scale(self) -> str:
        return str(self.metadata.get("score_scale") or TARGET_SCORE_SCALE)

    def _validate_static_contract(self) -> None:
        cutoffs = [winner.cutoff for winner in self.winners]
        if cutoffs != EXPECTED_CUTOFFS:
            raise ValueError(f"authoritative winners must cover {EXPECTED_CUTOFFS} in order; observed={cutoffs}")
        if len(set(cutoffs)) != len(cutoffs):
            raise ValueError(f"duplicate authoritative cutoffs: {cutoffs}")
        if self.metadata.get("active_quantiles") not in (None, EXPECTED_QUANTILE_LABELS):
            raise ValueError(f"unexpected active_quantiles={self.metadata.get('active_quantiles')}")
        if int(self.metadata.get("max_iter", 100)) != 100:
            raise ValueError(f"authoritative max_iter must be 100; observed={self.metadata.get('max_iter')}")
        for winner in self.winners:
            if not winner.run_id.startswith(f"multimodel_{winner.cutoff}_v8_he2grid_{winner.grid_spec_id}_"):
                raise ValueError(f"run_id does not match cutoff/spec: {winner.run_id}")

    def winner_by_cutoff(self) -> dict[str, AuthoritativeWinner]:
        return {winner.cutoff: winner for winner in self.winners}

    def run_root(self, winner: AuthoritativeWinner) -> Path:
        return self.runtime_root / "runs" / winner.run_id

    def output_root(self, winner: AuthoritativeWinner) -> Path:
        return self.run_root(winner) / "post" / "outputs" / winner.run_id

    def generated_config_path(self, winner: AuthoritativeWinner) -> Path:
        return self.runtime_root / "control" / "generated_configs" / f"{winner.run_id}.yaml"

    def crps_summary_path(self, winner: AuthoritativeWinner) -> Path:
        return self.output_root(winner) / "tables" / "crps_forecast_summary.csv"

    def selected_crps_row(self, winner: AuthoritativeWinner) -> dict[str, str]:
        rows = read_csv_rows(self.crps_summary_path(winner))
        for row in rows:
            if row.get("model_id") == self.model_id or row.get("model_variant") == self.model_family:
                return row
        raise ValueError(f"missing {self.model_id}/{self.model_family} row in {self.crps_summary_path(winner)}")

    def rdata_files(self, winner: AuthoritativeWinner) -> list[Path]:
        root = self.run_root(winner)
        if not root.exists():
            return []
        return sorted(path for path in root.rglob("*") if path.suffix.lower() in {".rdata", ".rda"})

    def required_output_paths(self, winner: AuthoritativeWinner) -> list[Path]:
        out = self.output_root(winner)
        return [out / rel for rel in REQUIRED_OUTPUT_FILES]


def load_authoritative_spec(path: Path | None = None) -> AuthoritativeSpec:
    return AuthoritativeSpec(path or DEFAULT_MANIFEST)


def winners_as_rows(spec: AuthoritativeSpec) -> list[dict[str, Any]]:
    return [winner.as_row() for winner in spec.winners]


def assert_close(left: float, right: float, tol: float = 1e-12) -> bool:
    return abs(float(left) - float(right)) <= tol


def iter_runtime_checks(spec: AuthoritativeSpec) -> Iterable[dict[str, Any]]:
    for winner in spec.winners:
        run_root = spec.run_root(winner)
        output_root = spec.output_root(winner)
        cfg_path = spec.generated_config_path(winner)
        yield {
            "cutoff": winner.cutoff,
            "run_id": winner.run_id,
            "check": "run_root_exists",
            "status": "pass" if run_root.exists() else "fail",
            "detail": str(run_root),
        }
        yield {
            "cutoff": winner.cutoff,
            "run_id": winner.run_id,
            "check": "output_root_exists",
            "status": "pass" if output_root.exists() else "fail",
            "detail": str(output_root),
        }
        yield {
            "cutoff": winner.cutoff,
            "run_id": winner.run_id,
            "check": "generated_config_exists",
            "status": "pass" if cfg_path.exists() else "fail",
            "detail": str(cfg_path),
        }
        missing = [str(path.relative_to(output_root)) for path in spec.required_output_paths(winner) if not path.exists()]
        yield {
            "cutoff": winner.cutoff,
            "run_id": winner.run_id,
            "check": "required_post_outputs",
            "status": "pass" if not missing else "fail",
            "detail": "|".join(missing),
        }
        rdata = spec.rdata_files(winner)
        yield {
            "cutoff": winner.cutoff,
            "run_id": winner.run_id,
            "check": "no_retained_rdata",
            "status": "pass" if not rdata else "fail",
            "detail": str(len(rdata)),
        }
        if spec.crps_summary_path(winner).exists():
            row = spec.selected_crps_row(winner)
            table_crps = float(row["mean_crps"])
            scale = row.get("score_scale", "")
            yield {
                "cutoff": winner.cutoff,
                "run_id": winner.run_id,
                "check": "mean_crps_matches_manifest",
                "status": "pass" if assert_close(table_crps, winner.mean_crps, 1e-12) else "fail",
                "detail": f"manifest={winner.mean_crps} table={table_crps}",
            }
            yield {
                "cutoff": winner.cutoff,
                "run_id": winner.run_id,
                "check": "score_scale_matches_manifest",
                "status": "pass" if scale == spec.score_scale else "fail",
                "detail": f"manifest={spec.score_scale} table={scale}",
            }


if __name__ == "__main__":
    import json

    loaded = load_authoritative_spec()
    print(json.dumps({"manifest": str(loaded.manifest_path), "runtime_root": str(loaded.runtime_root), "winners": winners_as_rows(loaded)}, indent=2))
