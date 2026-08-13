#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import re
import subprocess
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable

import pandas as pd

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
    R1_OVERVIEW_CONTRACT_REL,
    check_r1_overview_text,
)
from reviewer1_uncertainty_contract import (
    R1_UNCERTAINTY_CONTRACT_REL,
    check_r1_uncertainty_text,
)
from reviewer1_remaining_contracts import (
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
    PUBLIC_REPRO_URL,
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
MODEL_ORDER = [
    "N-U-T1",
    "N-M-T0",
    "N-M-T1",
    "AL-U-T1",
    "AL-M-T0",
    "AL-M-T1",
    "exAL-U-T1",
    "exAL-M-T0",
    "exAL-M-T1",
]
HE3_ORDER = [
    "exAL-M-T1 (full)",
    "exAL-M-T1-noTrend",
    "exAL-M-noTF",
    "exAL-M-T1-noH1",
    "exAL-M-T1-noH2",
    "exAL-M-T1-noH3",
    "RAW-GLOFAS",
    "RAW-NWS",
]
HE2_LONG_ORDER = ["RAW-GLOFAS"] + MODEL_ORDER
HE2_SHORT_ORDER = ["RAW-GLOFAS", "RAW-NWS"] + MODEL_ORDER
HE3_ABLATION_ORDER = [
    "exAL-M-T1 (full)",
    "exAL-M-T1-noTrend",
    "exAL-M-noTF",
    "exAL-M-T1-noH1",
    "exAL-M-T1-noH2",
    "exAL-M-T1-noH3",
]
HE3_LONG_ORDER = HE3_ABLATION_ORDER + ["RAW-GLOFAS"]
HE3_SHORT_ORDER = HE3_ABLATION_ORDER + ["RAW-GLOFAS", "RAW-NWS"]
HE4_ORDER = ["exAL-M-T1", "AL-M-T1", "exAL-U-T1", "AL-U-T1"]
HE4_TAU_COLUMNS = ["q0.05", "q0.20", "q0.35", "q0.50", "q0.65", "q0.80", "q0.95"]
RAW_MODEL_MAP = {"RAW-GLOFAS": "glofas_ensemble", "RAW-NWS": "nws_nwm_ensemble"}
RUN_SLUG_MAP = {
    "20210123": "20210123_exal_m_t1",
    "20211112": "20211112_exal_m_t1",
    "20211221": "20211221_exal_m_t1",
    "20220511": "20220511_exal_m_t1",
    "20221225": "20221225_exal_m_t1",
}
DISPLAY_DIGITS = 5
DISPLAY_TOLERANCE = 0.5 * 10 ** (-DISPLAY_DIGITS)

FORBIDDEN_CLAIMS = [
    "lowest forecast-window CRPS in every case",
    "best-performing model in all five cutoffs",
    "outperforms the best raw forecast baseline across the panel",
    "lower forecast-window CRPS than \\texttt{AL-M-T1} at all five",
    "better than \\texttt{AL-M-T1} in all five",
    "raw NWS forecast product has the lowest CRPS overall",
    "raw NWS forecast product is best overall",
    "the main contribution will be presented",
    "does not currently distinguish meteorological and hydrological uncertainty",
    "we will reorganize the introduction",
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
    "TODO[",
]
REQUIRED_CLAIMS = [
    ("article", "Table~\\ref{tab:benchmark_crps_models_nws_horizon}"),
    ("article", "exAL-M-T1 specification attains the lowest 28-day forecast-window CRPS in all five cutoffs"),
    ("article", "conceptual or physically based models"),
    ("article", "Conceptual formulations remain especially practical for prediction"),
    ("article", "easier to specify, calibrate, and deploy operationally"),
    ("article", "The empirical focus is forecasting performance and uncertainty quantification"),
    ("article", "Section~\\ref{sec:forecastvalidation} reports the out-of-sample forecast validation results"),
    ("article", "\\section{FORECAST VALIDATION RESULTS}"),
    ("article", "\\section{INTERPRETATION OF THE SELECTED SPECIFICATION}"),
    ("article", "five-cutoff rolling-origin forecast comparison"),
    ("article", "These figures are interpretation diagnostics"),
    ("article", "It illustrates how the fitted quantile-specific forecasts combine into one predictive distribution"),
    ("article", "five cutoff-specific, version-consistent staged datasets that span contrasting hydrological conditions"),
    ("article", "relatively low-flow windows as well as winter high-flow episodes"),
    ("article", "not a continuous daily hindcast over the full post-2022 period"),
    ("article", "Post-cutoff USGS observations are reserved strictly for verification"),
    ("article", "five cutoff-specific, version-consistent staged datasets"),
    ("article", "uses only information available at that origin to fit seven quantile-specific models"),
    ("article", "scores that distribution against future USGS observations held out over the forecast window"),
    ("article", "archive-feasible, version-consistent origins that span contrasting hydrological settings"),
    ("article", "This archive-reconstruction step is the main practical constraint on a denser rolling-origin design"),
    ("article", "data recovery, version matching, spatial extraction, covariate staging, and computation"),
    ("article", "avoid dense overlaps that would overrepresent the same episode"),
    ("article", "These two uncertainty sources are related but distinct"),
    ("article", "Hydrological uncertainty arises from model structure, parameters, states, and observations"),
    ("article", "meteorological uncertainty enters through imperfect precipitation and related atmospheric forcing fields"),
    ("article", "local hydrometeorological covariates"),
    ("article", "reanalysis-based model inputs"),
    ("article", "rather than direct observations or uncertainty-free measurements"),
    ("article", "ERA5/ERA5-Land variables may include short forecast components"),
    ("article", "not verification observations"),
    ("article", "\\section{APPLICATION DATA AND FORECASTING DESIGN}"),
    ("article", "\\subsection{Study Setting and Observations}"),
    ("article", "Our target series is"),
    ("article", "USGS target series"),
    ("article", "three additional information sources"),
    ("article", "Each source plays a different role"),
    ("article", "retrospective products are used to learn source-specific discrepancies"),
    ("article", "relative to the USGS target series"),
    ("article", "Precipitation is not modeled through a separate censoring"),
    ("article", "zero-inflation, or occurrence/intensity layer"),
    ("article", "dry days are retained in the supplied covariate path"),
    ("article", "deterministic engineered terms"),
    ("article", "\\subsection{Extended Asymmetric Laplace Likelihood}"),
    ("article", "\\(L\\in\\{\\mathrm{N},\\mathrm{AL},\\mathrm{exAL}\\}\\) denotes a Gaussian, asymmetric Laplace, or extended asymmetric Laplace observation likelihood"),
    ("article", "\\(S\\in\\{\\mathrm{U},\\mathrm{M}\\}\\) indicates whether the synthesis is univariate or multivariate"),
    ("article", "\\(T\\in\\{\\mathrm{T0},\\mathrm{T1}\\}\\) indicates whether the transfer component is suppressed or retained during the forecast window"),
    ("article", "nine Bayesian variants of the common state-space framework"),
    ("article", "Because exAL-M-T1 is the selected extended-likelihood multivariate specification"),
    ("article", "provide the strongest performance in the 28-day synthesis comparison"),
    ("article", "Selected Posterior Means and 95\\% Credible Intervals for Transfer-Function Covariates"),
    ("article", "Posterior Medians and 95\\% Credible Intervals for the Source-Specific Skewness Parameters"),
    ("article", "Posterior Medians and 95\\% Credible Intervals for the Source-Specific Scale Parameters"),
    ("article", "primary score for forecast-window predictive distributions"),
    ("article", "For reproducibility, implementation pseudocode for the VB algorithm is provided"),
    ("article", "Its role is illustrative"),
    ("article", "comparative forecast evaluation remains the main empirical evidence"),
    ("article", "uncertainty around fitted quantile-location curves"),
    ("article", "synthesized posterior predictive distribution"),
    ("article", "fitted quantile-specific forecasts combine into one predictive distribution"),
    ("article", "posterior predictive envelope can vary across the forecast window"),
    ("article", "risk of quantile crossing after independently fitting the quantile-specific models"),
    ("corrections", "separate eight-day NWS-horizon comparison"),
    ("corrections", "NWS is omitted from the 28-day table"),
    ("corrections", "centering the forecasting analysis on multiple rolling-origin cutoffs"),
    ("corrections", "supported by rolling-origin forecast evaluation and selected-model interpretation"),
    ("corrections", "rather than treating dynamic discrepancy correction alone as the central novelty"),
    ("corrections", "forecasting evaluation is expanded to five rolling-origin out-of-sample cutoffs"),
    ("corrections", "evidence is no longer tied to a single moderate-flood episode"),
    ("corrections", "not presented as a continuous 2023-present hindcast"),
    ("corrections", "representative selected-model illustration at one forecast origin"),
    ("corrections", "validation evidence is the multi-cutoff CRPS and check-loss tables"),
    ("corrections", "selected-model diagnostics"),
    ("corrections", "organized around forecast origins rather than a conventional random split"),
    ("corrections", "five cutoff-specific forecast-window evaluations"),
    ("corrections", "post-cutoff USGS observations are used only for verification"),
    ("corrections", "archive reconstruction step before any model can be fit"),
    ("corrections", "rather than a dense set of overlapping windows that would repeatedly score the same hydrological episode"),
    ("corrections", "pre-cutoff observational window"),
    ("corrections", "fixed calibrated specification"),
    ("corrections", "The revised introduction now broadens this statement"),
    ("corrections", "uses both conceptual and physically based models"),
    ("corrections", "simpler to specify, calibrate, and deploy in forecasting applications"),
    ("corrections", "typographical error rather than intended terminology"),
    ("corrections", "The revised manuscript no longer uses this term"),
    ("corrections", "reanalysis-based model products rather than direct observations or uncertainty-free measurements"),
    ("corrections", "ERA5/ERA5-Land variables may include short forecast components"),
    ("corrections", "precipitation is from PRISM and ERA5-Land enters as the soil-moisture covariate"),
    ("corrections", "external covariates rather than verification observations"),
    ("corrections", "now separates the general methodology from the application data and forecasting design"),
    ("corrections", "USGS daily flow series as the observed target"),
    ("corrections", "distinguishes forecast covariates, retrospective products, and operational forecast products"),
    ("corrections", "external historical inputs used to learn source-specific discrepancies relative to the USGS target"),
    ("corrections", "now states explicitly that precipitation is not handled through censoring"),
    ("corrections", "zero-inflation, or a separate occurrence/intensity model"),
    ("corrections", "zero-precipitation days are retained in the supplied covariate path"),
    ("corrections", "precipitation intermittency enters through the transfer component"),
    ("corrections", "no longer uses the vague ``General Results'' organization"),
    ("corrections", "separates the material by inferential role"),
    ("corrections", "Forecast Validation Results"),
    ("corrections", "Interpretation of the Selected Specification"),
    ("corrections", "selected-model diagnostics are separated from the forecast-validation tables"),
    ("corrections", "representative transfer-function covariate table reports posterior means"),
    ("corrections", "tables report posterior medians with 95\\% credible intervals"),
    ("corrections", "table-specific export contract"),
    ("corrections", "The revised introduction now separates these concepts before introducing the Bayesian framework"),
    ("corrections", "hydrological uncertainty with river-system structure, parameters, states, and observations"),
    ("corrections", "meteorological uncertainty with precipitation and atmospheric forcing fields"),
    ("corrections", "using available forecast and retrospective products to produce calibrated predictive distributions"),
    ("corrections", "no longer organizes the methodology around models A, B, and C"),
    ("corrections", "presents a common state-space formulation"),
    ("corrections", "Section 4 identifies the models in terms of \\(L\\)-\\(S\\)-\\(T\\) labels"),
    ("corrections", "likelihood family, source set, and forecast-window transfer treatment"),
    ("corrections", "selected \\texttt{exAL-M-T1} specification"),
    ("corrections", "PIT-centered development has been removed from the main text"),
    ("corrections", "final forecast comparison uses CRPS as the primary full-distribution score"),
    ("corrections", "targeted quantile check loss as the quantile-level diagnostic"),
    ("corrections", "retained a compact posterior predictive synthesis subsection"),
    ("corrections", "posterior uncertainty around fitted quantile-location or component summaries"),
    ("corrections", "full forecast predictive distribution at each date"),
    ("corrections", "representative single-cutoff posterior predictive distribution"),
    ("corrections", "forecast-window inputs change"),
    ("corrections", "Quantile crossing is no longer developed as a separate procedure in the main text"),
    ("corrections", "Details about the MCMC and VB algorithms are provided in the Appendix"),
]

@dataclass
class CheckRow:
    family: str
    item: str
    status: str
    detail: str


def read_csv_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="") as f:
        return list(csv.DictReader(f))


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def git_metadata(repo: Path) -> dict[str, object]:
    def run_git(*args: str) -> str:
        try:
            return subprocess.check_output(["git", "-C", str(repo), *args], text=True).strip()
        except Exception as exc:
            return f"ERROR: {exc}"

    return {
        "path": str(repo),
        "branch": run_git("rev-parse", "--abbrev-ref", "HEAD"),
        "commit": run_git("rev-parse", "HEAD"),
        "status_short": run_git("status", "--short"),
    }


def write_csv(path: Path, rows: list[dict[str, object]], fieldnames: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def strip_tex(cell: str) -> str:
    text = cell.strip()
    previous = None
    while previous != text:
        previous = text
        text = re.sub(r"\\textbf\{([^{}]+)\}", r"\1", text)
        text = re.sub(r"\\textit\{([^{}]+)\}", r"\1", text)
    text = text.replace("$", "").replace("\\,", " ").replace("\\", "")
    return text.strip()


def parse_numeric_cell(cell: str) -> float:
    cleaned = strip_tex(cell)
    match = re.search(r"-?\d+(?:\.\d+)?", cleaned)
    if not match:
        raise ValueError(f"Could not parse numeric table cell: {cell!r}")
    return float(match.group(0))


def parse_flat_table(path: Path, expected_numeric_cells: int) -> dict[str, list[float]]:
    rows: dict[str, list[float]] = {}
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if "&" not in line or line.startswith("\\") or "\\multicolumn" in line:
            continue
        parts = [part.strip() for part in line.rstrip("\\").split("&")]
        if len(parts) != expected_numeric_cells + 1:
            continue
        label = strip_tex(parts[0])
        if label in {"Model", "Model label", "Ablation model"}:
            continue
        try:
            values = [parse_numeric_cell(cell) for cell in parts[1:]]
        except ValueError:
            continue
        rows[label] = values
    return rows


def parse_panel_table(path: Path, expected_numeric_cells: int) -> dict[tuple[str, str], list[float]]:
    rows: dict[tuple[str, str], list[float]] = {}
    current_panel = ""
    panel_re = re.compile(r"Cutoff\s+(\d{2}/\d{2}/\d{4})")
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        panel_match = panel_re.search(line)
        if panel_match:
            current_panel = panel_match.group(1)
            continue
        if not current_panel or "&" not in line or line.startswith("\\") or "\\multicolumn" in line:
            continue
        parts = [part.strip() for part in line.rstrip("\\").split("&")]
        if len(parts) != expected_numeric_cells + 1:
            continue
        label = strip_tex(parts[0])
        if label in {"Model", "Model label", "Ablation model"}:
            continue
        try:
            values = [parse_numeric_cell(cell) for cell in parts[1:]]
        except ValueError:
            continue
        rows[(current_panel, label)] = values
    return rows


def load_manifest(article_root: Path) -> dict:
    return json.loads((article_root / "MANUSCRIPT_ASSET_MANIFEST.json").read_text(encoding="utf-8"))


def mean_crps_for_leads(df: pd.DataFrame, *, selector_col: str, selector_value: str, horizon_days: int, source: Path) -> float:
    rows = df[(df[selector_col] == selector_value) & (df["lead_day"].astype(int).between(1, horizon_days))]
    leads = sorted(rows["lead_day"].astype(int).tolist())
    expected = list(range(1, horizon_days + 1))
    if leads != expected:
        raise ValueError(f"Expected leads {expected} for {selector_col}={selector_value} in {source}; got {leads}")
    return float(rows.sort_values("lead_day")["crps"].astype(float).mean())


def build_he2_expected(
    article_root: Path,
    manifest: dict,
    *,
    horizon_days: int,
    include_nws: bool,
) -> dict[str, list[float]]:
    cfg = manifest["tables"]["tab:benchmark_crps_models"]
    manifest_rows = pd.read_csv(article_root / cfg["sources"]["bayesian_manifest_csv"])
    bayes = {(str(row.cutoff), row.manuscript_label): row for row in manifest_rows.itertuples()}
    expected: dict[str, list[float]] = {}

    five_root = article_root / cfg["sources"]["five_run_source_root"]
    raw_order = ["RAW-GLOFAS", "RAW-NWS"] if include_nws else ["RAW-GLOFAS"]
    for raw_label in raw_order:
        model_id = RAW_MODEL_MAP[raw_label]
        values = []
        for cutoff in CUTOFF_ORDER:
            source = five_root / RUN_SLUG_MAP[cutoff] / "crps_forecast_per_time.csv"
            crps = pd.read_csv(source)
            values.append(mean_crps_for_leads(crps, selector_col="model_id", selector_value=model_id, horizon_days=horizon_days, source=source))
        expected[raw_label] = values
    for label in MODEL_ORDER:
        values = []
        for cutoff in CUTOFF_ORDER:
            row = bayes[(cutoff, label)]
            source = Path(row.score_source).with_name("crps_forecast_per_time.csv")
            crps = pd.read_csv(source)
            values.append(mean_crps_for_leads(crps, selector_col="model_variant", selector_value=row.family, horizon_days=horizon_days, source=source))
        expected[label] = values
    order = HE2_SHORT_ORDER if include_nws else HE2_LONG_ORDER
    return {label: expected[label] for label in order}


def build_he3_expected(
    article_root: Path,
    manifest: dict,
    *,
    horizon_days: int,
    include_nws: bool,
) -> dict[str, list[float]]:
    cfg = manifest["tables"]["tab:he3_ablation_crps"]
    rows = pd.read_csv(article_root / cfg["sources"]["he3_ablation_long_csv"])
    label_map = {"exAL-M-T1": "exAL-M-T1 (full)"}
    expected: dict[str, list[float]] = {}
    raw_order = ["RAW-GLOFAS", "RAW-NWS"] if include_nws else ["RAW-GLOFAS"]
    for raw_label in raw_order:
        model_id = RAW_MODEL_MAP[raw_label]
        values = []
        five_root = article_root / cfg["sources"]["five_run_source_root"]
        for cutoff in CUTOFF_ORDER:
            source = five_root / RUN_SLUG_MAP[cutoff] / "crps_forecast_per_time.csv"
            crps = pd.read_csv(source)
            values.append(mean_crps_for_leads(crps, selector_col="model_id", selector_value=model_id, horizon_days=horizon_days, source=source))
        expected[raw_label] = values
    for manuscript_label in sorted(rows["manuscript_label"].unique()):
        out_label = label_map.get(manuscript_label, manuscript_label)
        values = []
        for cutoff in CUTOFF_ORDER:
            row = rows[(rows["cutoff"].astype(str) == cutoff) & (rows["manuscript_label"] == manuscript_label)]
            if len(row) != 1:
                raise ValueError(f"Expected one HE3 row for {manuscript_label} / {cutoff}")
            source = (
                Path(str(row["resolved_run_dir"].iloc[0]))
                / "post"
                / "outputs"
                / str(row["resolved_run_id"].iloc[0])
                / "tables"
                / "crps_forecast_per_time.csv"
            )
            crps = pd.read_csv(source)
            values.append(
                mean_crps_for_leads(
                    crps,
                    selector_col="model_id",
                    selector_value=str(row["target_model_id"].iloc[0]),
                    horizon_days=horizon_days,
                    source=source,
                )
            )
        expected[out_label] = values
    order = HE3_SHORT_ORDER if include_nws else HE3_LONG_ORDER
    return {label: expected[label] for label in order}


def build_he4_expected(article_root: Path, manifest: dict) -> dict[tuple[str, str], list[float]]:
    cfg = manifest["tables"]["tab:he4_quantile_check_loss"]
    rows = pd.read_csv(article_root / cfg["sources"]["he4_quantile_check_loss_wide_csv"])
    expected: dict[tuple[str, str], list[float]] = {}
    for cutoff in CUTOFF_ORDER:
        display = CUTOFF_DISPLAY[cutoff]
        for label in HE4_ORDER:
            row = rows[(rows["cutoff"].astype(str) == cutoff) & (rows["manuscript_label"] == label)]
            if len(row) != 1:
                raise ValueError(f"Expected one HE4 row for {label} / {cutoff}")
            expected[(display, label)] = [float(row[col].iloc[0]) for col in HE4_TAU_COLUMNS]
    return expected


def compare_table(
    *,
    table_name: str,
    expected: dict,
    observed: dict,
    columns: list[str],
    out_rows: list[dict[str, object]],
    tolerance: float = DISPLAY_TOLERANCE,
) -> list[CheckRow]:
    checks: list[CheckRow] = []
    for key, expected_values in expected.items():
        observed_values = observed.get(key)
        if observed_values is None:
            checks.append(CheckRow("table_values", f"{table_name}:{key}", "fail", "missing rendered row"))
            continue
        if len(observed_values) != len(expected_values):
            checks.append(CheckRow("table_values", f"{table_name}:{key}", "fail", "wrong numeric cell count"))
            continue
        for column, expected_value, observed_value in zip(columns, expected_values, observed_values):
            expected_display = round(float(expected_value), DISPLAY_DIGITS)
            observed_display = float(observed_value)
            diff = abs(expected_display - observed_display)
            status = "pass" if diff <= tolerance else "fail"
            out_rows.append(
                {
                    "table": table_name,
                    "row_key": str(key),
                    "column": column,
                    "expected_rounded5": f"{float(expected_value):.{DISPLAY_DIGITS}f}",
                    "observed": f"{float(observed_value):.{DISPLAY_DIGITS}f}",
                    "abs_diff": f"{diff:.8f}",
                    "status": status,
                }
            )
            if status == "fail":
                checks.append(
                    CheckRow(
                        "table_values",
                        f"{table_name}:{key}:{column}",
                        "fail",
                        f"expected {expected_value:.{DISPLAY_DIGITS}f}, observed {observed_value:.{DISPLAY_DIGITS}f}",
                    )
                )
    extra = sorted(set(observed).difference(expected))
    for key in extra:
        checks.append(CheckRow("table_values", f"{table_name}:{key}", "fail", "unexpected rendered row"))
    if not checks:
        checks.append(CheckRow("table_values", table_name, "pass", f"{len(expected)} rendered rows match sources"))
    return checks


def audit_manifest_paths(article_root: Path, manifest: dict) -> tuple[list[CheckRow], list[dict[str, object]], list[Path]]:
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    source_paths: list[Path] = [article_root / "MANUSCRIPT_ASSET_MANIFEST.json"]

    def add_row(kind: str, label: str, rel_path: str) -> None:
        path = article_root / rel_path
        exists = path.exists()
        rows.append(
            {
                "kind": kind,
                "label": label,
                "relative_path": rel_path,
                "absolute_path": str(path),
                "exists": exists,
            }
        )
        checks.append(
            CheckRow("manifest_paths", f"{kind}:{label}:{rel_path}", "pass" if exists else "fail", "exists" if exists else "missing")
        )
        if exists and path.is_file():
            source_paths.append(path)

    for fig in manifest.get("figures", []):
        add_row("figure_manuscript_path", fig["label"], fig["manuscript_path"])
        add_row("figure_source_path", fig["label"], fig["source_path"])
    for label, table in manifest.get("tables", {}).items():
        add_row("table_tex_path", label, table["table_tex_path"])
        for source_name, source_rel in table.get("sources", {}).items():
            add_row(f"table_source:{source_name}", label, source_rel)
    for section_name in ["runtime_benchmarks", "forecast_protocols"]:
        for label, item in manifest.get(section_name, {}).items():
            for key in ["manifest_path", "doc_path"]:
                if key in item:
                    add_row(f"{section_name}:{key}", label, item[key])

    for tex_path in [article_root / "wileyNJD-APA.tex"]:
        if tex_path.exists():
            source_paths.append(tex_path)
            for rel_input in re.findall(r"\\input\{([^}]+)\}", tex_path.read_text(encoding="utf-8")):
                rel = rel_input if rel_input.endswith(".tex") else f"{rel_input}.tex"
                add_row("article_input", tex_path.name, rel)

    return checks, rows, source_paths


def audit_corrections_inputs(corrections_root: Path) -> tuple[list[CheckRow], list[dict[str, object]], list[Path]]:
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    sources = [corrections_root / "main.tex"]
    text = (corrections_root / "main.tex").read_text(encoding="utf-8")
    for rel_input in re.findall(r"\\input\{([^}]+)\}", text):
        rel = rel_input if rel_input.endswith(".tex") else f"{rel_input}.tex"
        path = corrections_root / rel
        exists = path.exists()
        rows.append(
            {
                "kind": "corrections_input",
                "label": "main.tex",
                "relative_path": rel,
                "absolute_path": str(path),
                "exists": exists,
            }
        )
        checks.append(CheckRow("manifest_paths", f"corrections_input:{rel}", "pass" if exists else "fail", "exists" if exists else "missing"))
        if exists and path.is_file():
            sources.append(path)
    return checks, rows, sources


def audit_prose_claims(article_root: Path, corrections_root: Path) -> tuple[list[CheckRow], list[dict[str, object]]]:
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    generated_tables = "\n".join(
        path.read_text(encoding="utf-8")
        for path in sorted((article_root / "tables" / "generated_tex").glob("*.tex"))
    )
    texts = {
        "article": (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8") + "\n" + generated_tables,
        "corrections": (corrections_root / "main.tex").read_text(encoding="utf-8"),
    }
    for repo_name, text in texts.items():
        for claim in FORBIDDEN_CLAIMS:
            present = claim in text
            status = "fail" if present else "pass"
            rows.append({"repo": repo_name, "claim_type": "forbidden", "claim": claim, "status": status})
            checks.append(CheckRow("prose_claims", f"{repo_name}:forbidden:{claim}", status, "present" if present else "absent"))
    article_typo_present = "flexile" in texts["article"].lower()
    rows.append({
        "repo": "article",
        "claim_type": "forbidden",
        "claim": "flexile",
        "status": "fail" if article_typo_present else "pass",
    })
    checks.append(
        CheckRow(
            "prose_claims",
            "article:forbidden:flexile",
            "fail" if article_typo_present else "pass",
            "present" if article_typo_present else "absent",
        )
    )
    for repo_name, claim in REQUIRED_CLAIMS:
        present = claim in texts[repo_name]
        status = "pass" if present else "fail"
        rows.append({"repo": repo_name, "claim_type": "required", "claim": claim, "status": status})
        checks.append(CheckRow("prose_claims", f"{repo_name}:required:{claim}", status, "present" if present else "missing"))
    return checks, rows


def audit_reviewer1_overview(
    workflow_root: Path,
    article_root: Path,
    corrections_root: Path,
) -> tuple[list[CheckRow], list[dict[str, object]], list[Path]]:
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    sources: list[Path] = []

    def record(item: str, ok: bool, detail: str) -> None:
        status = "pass" if ok else "fail"
        checks.append(CheckRow("reviewer1_overview", item, status, detail))
        rows.append({"item": item, "status": status, "detail": detail})

    workflow_doc = workflow_root / R1_OVERVIEW_CONTRACT_REL
    record("workflow_contract_doc_exists", workflow_doc.exists(), R1_OVERVIEW_CONTRACT_REL)
    for path in [workflow_doc]:
        if path.exists():
            sources.append(path)

    article_path = article_root / "wileyNJD-APA.tex"
    corrections_path = corrections_root / "main.tex"
    article_text = article_path.read_text(encoding="utf-8")
    corrections_text = corrections_path.read_text(encoding="utf-8")
    sources.extend([article_path, corrections_path])
    for row in check_r1_overview_text(article_text, corrections_text):
        record(row.item, row.ok, row.detail)

    return checks, rows, sources


def audit_reviewer1_uncertainty(
    workflow_root: Path,
    article_root: Path,
    corrections_root: Path,
) -> tuple[list[CheckRow], list[dict[str, object]], list[Path]]:
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    sources: list[Path] = []

    def record(item: str, ok: bool, detail: str) -> None:
        status = "pass" if ok else "fail"
        checks.append(CheckRow("reviewer1_uncertainty", item, status, detail))
        rows.append({"item": item, "status": status, "detail": detail})

    workflow_doc = workflow_root / R1_UNCERTAINTY_CONTRACT_REL
    record("workflow_contract_doc_exists", workflow_doc.exists(), R1_UNCERTAINTY_CONTRACT_REL)
    for path in [workflow_doc]:
        if path.exists():
            sources.append(path)

    article_path = article_root / "wileyNJD-APA.tex"
    corrections_path = corrections_root / "main.tex"
    article_text = article_path.read_text(encoding="utf-8")
    corrections_text = corrections_path.read_text(encoding="utf-8")
    sources.extend([article_path, corrections_path])
    for row in check_r1_uncertainty_text(article_text, corrections_text):
        record(row.item, row.ok, row.detail)

    return checks, rows, sources


def audit_reviewer1_remaining(
    workflow_root: Path,
    article_root: Path,
    corrections_root: Path,
) -> tuple[list[CheckRow], list[dict[str, object]], list[Path]]:
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    sources: list[Path] = []

    def record(item: str, ok: bool, detail: str) -> None:
        status = "pass" if ok else "fail"
        checks.append(CheckRow("reviewer1_remaining", item, status, detail))
        rows.append({"item": item, "status": status, "detail": detail})

    workflow_doc = workflow_root / R1_REMAINING_CONTRACT_REL
    record("workflow_contract_doc_exists", workflow_doc.exists(), R1_REMAINING_CONTRACT_REL)
    for path in [workflow_doc]:
        if path.exists():
            sources.append(path)

    article_path = article_root / "wileyNJD-APA.tex"
    corrections_path = corrections_root / "main.tex"
    generated_table_paths = sorted((article_root / "tables" / "generated_tex").glob("*.tex"))
    generated_tables = "\n".join(path.read_text(encoding="utf-8") for path in generated_table_paths)
    article_text = article_path.read_text(encoding="utf-8") + "\n" + generated_tables
    corrections_text = corrections_path.read_text(encoding="utf-8")
    sources.extend([article_path, corrections_path, *generated_table_paths])
    for row in check_reviewer1_remaining_text(article_text, corrections_text):
        record(row.item, row.ok, row.detail)

    return checks, rows, sources


def audit_software_availability(
    workflow_root: Path,
    article_root: Path,
    corrections_root: Path,
) -> tuple[list[CheckRow], list[dict[str, object]], list[Path]]:
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    sources: list[Path] = []
    manifest_path = article_root / SOFTWARE_MANIFEST_REL

    def record(item: str, ok: bool, detail: str) -> None:
        status = "pass" if ok else "fail"
        checks.append(CheckRow("software_availability", item, status, detail))
        rows.append({"item": item, "status": status, "detail": detail})

    record("manifest_exists", manifest_path.exists(), SOFTWARE_MANIFEST_REL)
    if not manifest_path.exists():
        return checks, rows, sources

    sources.append(manifest_path)
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
    package = data.get("public_estimation_package", {})
    workflow = data.get("study_workflow_repository", {})
    article_repo = data.get("revised_article_repository", {})
    corrections_repo = data.get("corrections_repository", {})
    archive = data.get("archive_status", {})
    validation_policy = data.get("validation_policy", {})
    release_readiness_files = workflow.get("release_readiness_files", {})

    record("schema_version", data.get("schema_version") == "revision_software_availability_v1", str(data.get("schema_version", "")))
    record("cran_package_url", package.get("cran_package_url") == CRAN_EXDQLM_URL, str(package.get("cran_package_url", "")))
    record("cran_package_doi", package.get("package_doi") == CRAN_EXDQLM_DOI_URL, str(package.get("package_doi", "")))
    record("cran_version_recorded", package.get("cran_version_verified_for_contract") == CRAN_EXDQLM_VERSION, str(package.get("cran_version_verified_for_contract", "")))
    record("cran_publication_date_recorded", package.get("cran_publication_date_verified_for_contract") == CRAN_EXDQLM_PUBLICATION_DATE, str(package.get("cran_publication_date_verified_for_contract", "")))
    record("software_paper_arxiv_url", package.get("software_paper_arxiv_url") == EXDQLM_SOFTWARE_PAPER_URL, str(package.get("software_paper_arxiv_url", "")))
    record("software_paper_arxiv_doi", package.get("software_paper_arxiv_doi") == EXDQLM_SOFTWARE_PAPER_DOI_URL, str(package.get("software_paper_arxiv_doi", "")))
    record("software_paper_bibtex_key", package.get("software_paper_bibtex_key") == EXDQLM_SOFTWARE_PAPER_BIBTEX_KEY, str(package.get("software_paper_bibtex_key", "")))
    record("workflow_public_url", workflow.get("public_url") == PUBLIC_REPRO_URL, str(workflow.get("public_url", "")))
    expected_release_files = {
        "readme": WORKFLOW_README_REL,
        "citation": WORKFLOW_CITATION_REL,
        "license_notice": "LICENSE",
        "validation_script": "scripts/validate_public_repository.py",
    }
    record(
        "workflow_release_readiness_manifest",
        release_readiness_files == expected_release_files,
        json.dumps(release_readiness_files, sort_keys=True),
    )
    record("article_public_url", "Evironmetrics---REVISED-DOC-Corrected-2" in str(article_repo.get("public_url", "")), str(article_repo.get("public_url", "")))
    record("corrections_public_url", "Corrections---Project-1" in str(corrections_repo.get("public_url", "")), str(corrections_repo.get("public_url", "")))
    archive_check = check_archive_status(archive)
    record("archive_status_coherent", archive_check.ok, archive_check.detail)
    record(
        "static_commit_policy",
        "reason_static_commits_are_not_recorded" in validation_policy,
        str(validation_policy.get("reason_static_commits_are_not_recorded", "")),
    )

    workflow_contract = workflow_root / SOFTWARE_CONTRACT_REL
    article_contract = article_root / ARTICLE_SOFTWARE_DOC_REL
    record("workflow_contract_doc_exists", workflow_contract.exists(), SOFTWARE_CONTRACT_REL)
    record("article_contract_doc_exists", article_contract.exists(), ARTICLE_SOFTWARE_DOC_REL)
    for path in [workflow_contract, article_contract]:
        if path.exists():
            sources.append(path)
    readiness_paths = [workflow_root / rel for rel in WORKFLOW_RELEASE_READINESS_RELS]
    for readiness_path in readiness_paths:
        record(f"workflow_release_readiness_exists:{readiness_path.relative_to(workflow_root)}", readiness_path.exists(), str(readiness_path.relative_to(workflow_root)))
        if readiness_path.exists():
            sources.append(readiness_path)

    readme_path = workflow_root / WORKFLOW_README_REL
    citation_path = workflow_root / WORKFLOW_CITATION_REL
    release_notes_path = workflow_root / WORKFLOW_RELEASE_NOTES_REL
    checklist_path = workflow_root / WORKFLOW_ARCHIVE_READINESS_REL
    if readme_path.exists():
        readme_text = readme_path.read_text(encoding="utf-8")
        record("readme_names_public_repro_repo", PUBLIC_REPRO_URL in readme_text, WORKFLOW_README_REL)
        record("readme_names_cran_package", CRAN_EXDQLM_URL in readme_text, WORKFLOW_README_REL)
        record("readme_names_contract", SOFTWARE_CONTRACT_REL in readme_text, WORKFLOW_README_REL)
        record("readme_archive_pending", "pending final revision freeze" in readme_text, WORKFLOW_README_REL)
    if citation_path.exists():
        citation_text = citation_path.read_text(encoding="utf-8")
        record("citation_pending_version", 'version: "pending-final-archive"' in citation_text, WORKFLOW_CITATION_REL)
        record("citation_no_workflow_doi_field", "\ndoi:" not in citation_text, WORKFLOW_CITATION_REL)
        record("citation_names_public_repro_repo", PUBLIC_REPRO_URL in citation_text, WORKFLOW_CITATION_REL)
    if release_notes_path.exists():
        release_notes_text = release_notes_path.read_text(encoding="utf-8")
        record("release_notes_archive_pending", "pending final revision freeze" in release_notes_text, WORKFLOW_RELEASE_NOTES_REL)
    if checklist_path.exists():
        checklist_text = checklist_path.read_text(encoding="utf-8")
        record("archive_checklist_license_gate", "Workflow repository license is confirmed by the authors" in checklist_text, WORKFLOW_ARCHIVE_READINESS_REL)
        record("archive_checklist_final_doi_gate", "Final workflow release is archived with a permanent DOI" in checklist_text, WORKFLOW_ARCHIVE_READINESS_REL)

    article_text = (article_root / "wileyNJD-APA.tex").read_text(encoding="utf-8")
    corrections_text = (corrections_root / "main.tex").read_text(encoding="utf-8")
    for path in [article_root / "wileyNJD-APA.tex", corrections_root / "main.tex"]:
        sources.append(path)
    required_article = [
        r"CRAN R package \texttt{exdqlm}",
        f"version {CRAN_EXDQLM_VERSION}",
        CRAN_EXDQLM_URL,
        CRAN_EXDQLM_DOI_URL,
        EXDQLM_SOFTWARE_PAPER_BIBTEX_KEY,
        PUBLIC_REPRO_URL,
        "compact provenance manifests",
    ]
    required_corrections = [
        r"CRAN R package \texttt{exdqlm}",
        f"version {CRAN_EXDQLM_VERSION}",
        CRAN_EXDQLM_URL,
        CRAN_EXDQLM_DOI_URL,
        EXDQLM_SOFTWARE_PAPER_DOI_URL,
        PUBLIC_REPRO_URL,
        "compact provenance manifests",
    ]
    if archive_check.is_pending:
        required_article.append("clean reproducibility repository for this study")
        required_corrections.append("A permanent archive DOI will be minted from the final clean reproducibility release")
    elif archive_check.is_final:
        required_article.append(archive_check.doi)
        required_corrections.append(archive_check.doi)
    for claim in required_article:
        record(f"article_required:{claim}", claim in article_text, claim)
    for claim in required_corrections:
        record(f"corrections_required:{claim}", claim in corrections_text, claim)
    if archive_check.is_pending:
        for claim in ["workflow repository has been archived", "workflow has been archived", "archived workflow DOI"]:
            record(f"article_no_premature_archive_claim:{claim}", claim not in article_text, claim)
            record(f"corrections_no_premature_archive_claim:{claim}", claim not in corrections_text, claim)
            for readiness_path in readiness_paths:
                if readiness_path.exists():
                    readiness_text = readiness_path.read_text(encoding="utf-8")
                    record(
                        f"workflow_no_premature_archive_claim:{readiness_path.relative_to(workflow_root)}:{claim}",
                        claim not in readiness_text,
                        claim,
                    )
    elif archive_check.is_final:
        for claim in [
            "A permanent archive DOI will be minted from the final clean reproducibility release",
            "workflow archive DOI: pending",
        ]:
            record(f"article_no_stale_pending_claim:{claim}", claim not in article_text, claim)
            record(f"corrections_no_stale_pending_claim:{claim}", claim not in corrections_text, claim)

    return checks, rows, sources


def audit_runtime_feasibility(
    workflow_root: Path,
    article_root: Path,
    corrections_root: Path,
) -> tuple[list[CheckRow], list[dict[str, object]], list[Path]]:
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    sources: list[Path] = []
    manifest_path = article_root / RUNTIME_MANIFEST_REL

    def record(item: str, ok: bool, detail: str) -> None:
        status = "pass" if ok else "fail"
        checks.append(CheckRow("runtime_feasibility", item, status, detail))
        rows.append({"item": item, "status": status, "detail": detail})

    record("manifest_exists", manifest_path.exists(), RUNTIME_MANIFEST_REL)
    if not manifest_path.exists():
        return checks, rows, sources

    sources.append(manifest_path)
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
    for row in check_runtime_manifest(data):
        record(row.item, row.ok, row.detail)

    workflow_doc = workflow_root / RUNTIME_CONTRACT_REL
    article_doc = article_root / ARTICLE_RUNTIME_DOC_REL
    record("workflow_contract_doc_exists", workflow_doc.exists(), RUNTIME_CONTRACT_REL)
    record("article_contract_doc_exists", article_doc.exists(), ARTICLE_RUNTIME_DOC_REL)
    for path in [workflow_doc, article_doc]:
        if path.exists():
            sources.append(path)

    article_path = article_root / "wileyNJD-APA.tex"
    corrections_path = corrections_root / "main.tex"
    article_text = article_path.read_text(encoding="utf-8")
    corrections_text = corrections_path.read_text(encoding="utf-8")
    sources.extend([article_path, corrections_path])
    for claim in REQUIRED_RUNTIME_ARTICLE_CLAIMS:
        record(f"article_required:{claim}", claim in article_text, claim)
    for claim in REQUIRED_RUNTIME_CORRECTIONS_CLAIMS:
        record(f"corrections_required:{claim}", claim in corrections_text, claim)
    for claim in FORBIDDEN_RUNTIME_DECOMPOSITION_CLAIMS:
        record(f"article_forbidden:{claim}", claim not in article_text, claim)
        record(f"corrections_forbidden:{claim}", claim not in corrections_text, claim)

    return checks, rows, sources


def audit_forecast_design(
    workflow_root: Path,
    article_root: Path,
    corrections_root: Path,
) -> tuple[list[CheckRow], list[dict[str, object]], list[Path]]:
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    sources: list[Path] = []
    manifest_path = article_root / FORECAST_DESIGN_MANIFEST_REL

    def record(item: str, ok: bool, detail: str) -> None:
        status = "pass" if ok else "fail"
        checks.append(CheckRow("forecast_design", item, status, detail))
        rows.append({"item": item, "status": status, "detail": detail})

    record("manifest_exists", manifest_path.exists(), FORECAST_DESIGN_MANIFEST_REL)
    if not manifest_path.exists():
        return checks, rows, sources

    sources.append(manifest_path)
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
    for row in check_forecast_design_manifest(data):
        record(row.item, row.ok, row.detail)

    workflow_doc = workflow_root / FORECAST_DESIGN_CONTRACT_REL
    article_doc = article_root / ARTICLE_FORECAST_DESIGN_DOC_REL
    record("workflow_contract_doc_exists", workflow_doc.exists(), FORECAST_DESIGN_CONTRACT_REL)
    record("article_contract_doc_exists", article_doc.exists(), ARTICLE_FORECAST_DESIGN_DOC_REL)
    for path in [workflow_doc, article_doc]:
        if path.exists():
            sources.append(path)

    article_path = article_root / "wileyNJD-APA.tex"
    corrections_path = corrections_root / "main.tex"
    article_text = article_path.read_text(encoding="utf-8")
    corrections_text = corrections_path.read_text(encoding="utf-8")
    sources.extend([article_path, corrections_path])
    for claim in REQUIRED_FORECAST_DESIGN_ARTICLE_CLAIMS:
        record(f"article_required:{claim}", claim in article_text, claim)
    for claim in REQUIRED_FORECAST_DESIGN_CORRECTIONS_CLAIMS:
        record(f"corrections_required:{claim}", claim in corrections_text, claim)
    for claim in FORBIDDEN_FORECAST_DESIGN_CLAIMS:
        record(f"article_forbidden:{claim}", claim not in article_text, claim)
        record(f"corrections_forbidden:{claim}", claim not in corrections_text, claim)

    return checks, rows, sources


def audit_latest_forecast_issue(
    workflow_root: Path,
    article_root: Path,
    corrections_root: Path,
) -> tuple[list[CheckRow], list[dict[str, object]], list[Path]]:
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    sources: list[Path] = []
    manifest_path = article_root / LATEST_FORECAST_ISSUE_MANIFEST_REL

    def record(item: str, ok: bool, detail: str) -> None:
        status = "pass" if ok else "fail"
        checks.append(CheckRow("latest_forecast_issue", item, status, detail))
        rows.append({"item": item, "status": status, "detail": detail})

    record("manifest_exists", manifest_path.exists(), LATEST_FORECAST_ISSUE_MANIFEST_REL)
    if not manifest_path.exists():
        return checks, rows, sources

    sources.append(manifest_path)
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
    for row in check_latest_forecast_issue_manifest(data, workflow_root=workflow_root):
        record(row.item, row.ok, row.detail)

    workflow_doc = workflow_root / LATEST_FORECAST_ISSUE_CONTRACT_REL
    article_doc = article_root / ARTICLE_LATEST_FORECAST_ISSUE_DOC_REL
    record("workflow_contract_doc_exists", workflow_doc.exists(), LATEST_FORECAST_ISSUE_CONTRACT_REL)
    record("article_contract_doc_exists", article_doc.exists(), ARTICLE_LATEST_FORECAST_ISSUE_DOC_REL)
    for path in [workflow_doc, article_doc]:
        if path.exists():
            sources.append(path)

    article_path = article_root / "wileyNJD-APA.tex"
    corrections_path = corrections_root / "main.tex"
    article_text = article_path.read_text(encoding="utf-8")
    corrections_text = corrections_path.read_text(encoding="utf-8")
    sources.extend([article_path, corrections_path])
    for claim in REQUIRED_LATEST_FORECAST_ARTICLE_CLAIMS:
        record(f"article_required:{claim}", claim in article_text, claim)
    for claim in REQUIRED_LATEST_FORECAST_CORRECTIONS_CLAIMS:
        record(f"corrections_required:{claim}", claim in corrections_text, claim)
    for claim in FORBIDDEN_LATEST_FORECAST_ARTICLE_CLAIMS:
        record(f"article_forbidden:{claim}", claim not in article_text, claim)

    return checks, rows, sources


def audit_he4_selection(article_root: Path, manifest: dict) -> tuple[list[CheckRow], list[Path]]:
    cfg = manifest["tables"]["tab:he4_quantile_check_loss"]
    selection_path = article_root / cfg["sources"]["he4_selection_audit_csv"]
    source_paths = [selection_path]
    df = pd.read_csv(selection_path)
    checks: list[CheckRow] = []
    expected_n = len(CUTOFF_ORDER) * len(HE4_ORDER)
    checks.append(CheckRow("he4_selection", "row_count", "pass" if len(df) == expected_n else "fail", f"{len(df)} rows"))
    max_diff = float(df["crps_abs_diff"].max())
    checks.append(CheckRow("he4_selection", "crps_abs_diff", "pass" if max_diff <= 1e-6 else "fail", f"max={max_diff:.3g}"))
    source_modes = sorted(set(df["source_mode"].astype(str)))
    checks.append(
        CheckRow(
            "he4_selection",
            "source_mode",
            "pass" if source_modes == ["he2-publication-manifest"] else "fail",
            ",".join(source_modes),
        )
    )
    return checks, source_paths


def audit_compile_logs(article_root: Path, corrections_root: Path, enabled: bool) -> tuple[list[CheckRow], list[dict[str, object]], list[Path]]:
    if not enabled:
        return [CheckRow("compile_logs", "skipped", "pass", "compile-log checks are only required in --after-patch mode")], [], []
    logs = {
        "article": article_root / "output.log",
        "corrections": corrections_root / "main.log",
    }
    bad_patterns = [
        "LaTeX Error",
        "Emergency stop",
        "Fatal error",
        "Undefined control sequence",
        "Citation.*undefined",
        "Reference.*undefined",
        "No file",
    ]
    checks: list[CheckRow] = []
    rows: list[dict[str, object]] = []
    sources: list[Path] = []
    for name, path in logs.items():
        exists = path.exists()
        text = path.read_text(errors="replace") if exists else ""
        if exists:
            sources.append(path)
        matched = [pattern for pattern in bad_patterns if re.search(pattern, text)]
        status = "pass" if exists and not matched else "fail"
        detail = "clean" if status == "pass" else ("missing" if not exists else ",".join(matched))
        checks.append(CheckRow("compile_logs", name, status, detail))
        rows.append({"document": name, "log_path": str(path), "exists": exists, "status": status, "detail": detail})
    return checks, rows, sources


def build_metadata(workflow_root: Path, article_root: Path, corrections_root: Path, argv: list[str]) -> dict[str, object]:
    return {
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "argv": argv,
        "cwd": str(Path.cwd()),
        "python": sys.version,
        "repos": {
            "workflow": git_metadata(workflow_root),
            "article": git_metadata(article_root),
            "corrections": git_metadata(corrections_root),
        },
    }


def render_summary_md(checks: list[CheckRow], metadata: dict[str, object]) -> str:
    fail_count = sum(1 for check in checks if check.status != "pass")
    family_counts: dict[str, dict[str, int]] = {}
    for check in checks:
        family_counts.setdefault(check.family, {"pass": 0, "fail": 0})
        family_counts[check.family]["pass" if check.status == "pass" else "fail"] += 1
    lines = [
        "# Cross-Repo Validation Summary",
        "",
        f"- Timestamp UTC: `{metadata['timestamp_utc']}`",
        f"- Overall status: `{'pass' if fail_count == 0 else 'fail'}`",
        f"- Failed checks: `{fail_count}`",
        "",
        "## By Family",
        "",
        "| family | pass | fail |",
        "|---|---:|---:|",
    ]
    for family in sorted(family_counts):
        counts = family_counts[family]
        lines.append(f"| {family} | {counts['pass']} | {counts['fail']} |")
    failures = [check for check in checks if check.status != "pass"]
    if failures:
        lines.extend(["", "## Failures", "", "| family | item | detail |", "|---|---|---|"])
        for check in failures:
            lines.append(f"| {check.family} | `{check.item}` | {check.detail} |")
    return "\n".join(lines) + "\n"


def parse_args(argv: Iterable[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Validate cross-repo revised-article and corrections wiring.")
    parser.add_argument("--workflow-root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--article-root", type=Path, default=Path(__file__).resolve().parents[1] / "Evironmetrics---REVISED-DOC-Corrected-2")
    parser.add_argument("--corrections-root", type=Path, default=Path("SOURCE_CORRECTIONS_ROOT"))
    parser.add_argument("--output-dir", type=Path, default=Path("reports/revision_cross_repo_validation_20260609"))
    parser.add_argument("--check-only", action="store_true")
    parser.add_argument("--after-patch", action="store_true")
    parser.add_argument("--allow-known-failures", action="store_true")
    parser.add_argument("--strict", action="store_true", default=True)
    return parser.parse_args(argv)


def main(argv: Iterable[str] | None = None) -> int:
    args = parse_args(argv)
    workflow_root = args.workflow_root.resolve()
    article_root = args.article_root.resolve()
    corrections_root = args.corrections_root.resolve()
    output_dir = (workflow_root / args.output_dir).resolve() if not args.output_dir.is_absolute() else args.output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    checks: list[CheckRow] = []
    source_paths: list[Path] = []
    table_value_rows: list[dict[str, object]] = []

    manifest = load_manifest(article_root)
    metadata = build_metadata(workflow_root, article_root, corrections_root, list(argv or sys.argv[1:]))

    manifest_checks, manifest_rows, manifest_sources = audit_manifest_paths(article_root, manifest)
    correction_input_checks, correction_input_rows, correction_input_sources = audit_corrections_inputs(corrections_root)
    checks.extend(manifest_checks)
    checks.extend(correction_input_checks)
    source_paths.extend(manifest_sources)
    source_paths.extend(correction_input_sources)

    he2_expected = build_he2_expected(article_root, manifest, horizon_days=28, include_nws=False)
    he2_nws_horizon_expected = build_he2_expected(article_root, manifest, horizon_days=8, include_nws=True)
    he3_expected = build_he3_expected(article_root, manifest, horizon_days=28, include_nws=False)
    he3_nws_horizon_expected = build_he3_expected(article_root, manifest, horizon_days=8, include_nws=True)
    he4_expected = build_he4_expected(article_root, manifest)

    he2_article = parse_flat_table(article_root / "tables/generated_tex/benchmark_crps_main_table.tex", 5)
    he2_nws_horizon_article = parse_flat_table(article_root / "tables/generated_tex/benchmark_crps_nws_horizon_table.tex", 5)
    he2_corrections = parse_flat_table(corrections_root / "tables/generated_tex/he2_benchmark_crps_response_table.tex", 5)
    he3_article = parse_flat_table(article_root / "tables/generated_tex/he3_ablation_crps_main_table.tex", 5)
    he3_nws_horizon_article = parse_flat_table(article_root / "tables/generated_tex/he3_ablation_crps_nws_horizon_table.tex", 5)
    he3_corrections = parse_flat_table(corrections_root / "tables/generated_tex/he3_ablation_crps_response_table.tex", 5)
    he3_nws_horizon_corrections = parse_flat_table(corrections_root / "tables/generated_tex/he3_ablation_crps_nws_horizon_response_table.tex", 5)
    he4_article = parse_panel_table(article_root / "tables/generated_tex/he4_quantile_check_loss_main_table.tex", 7)
    he4_corrections = parse_panel_table(corrections_root / "tables/generated_tex/he4_quantile_check_loss_response_table.tex", 7)

    cutoff_cols = [CUTOFF_DISPLAY[cutoff] for cutoff in CUTOFF_ORDER]
    checks.extend(compare_table(table_name="article_he2", expected=he2_expected, observed=he2_article, columns=cutoff_cols, out_rows=table_value_rows))
    checks.extend(compare_table(table_name="article_he2_nws_horizon", expected=he2_nws_horizon_expected, observed=he2_nws_horizon_article, columns=cutoff_cols, out_rows=table_value_rows))
    checks.extend(compare_table(table_name="corrections_he2", expected=he2_expected, observed=he2_corrections, columns=cutoff_cols, out_rows=table_value_rows))
    checks.extend(compare_table(table_name="article_he3", expected=he3_expected, observed=he3_article, columns=cutoff_cols, out_rows=table_value_rows))
    checks.extend(compare_table(table_name="article_he3_nws_horizon", expected=he3_nws_horizon_expected, observed=he3_nws_horizon_article, columns=cutoff_cols, out_rows=table_value_rows))
    checks.extend(compare_table(table_name="corrections_he3", expected=he3_expected, observed=he3_corrections, columns=cutoff_cols, out_rows=table_value_rows))
    checks.extend(compare_table(table_name="corrections_he3_nws_horizon", expected=he3_nws_horizon_expected, observed=he3_nws_horizon_corrections, columns=cutoff_cols, out_rows=table_value_rows))
    checks.extend(compare_table(table_name="article_he4", expected=he4_expected, observed=he4_article, columns=HE4_TAU_COLUMNS, out_rows=table_value_rows))
    checks.extend(compare_table(table_name="corrections_he4", expected=he4_expected, observed=he4_corrections, columns=HE4_TAU_COLUMNS, out_rows=table_value_rows))

    he4_selection_checks, he4_selection_sources = audit_he4_selection(article_root, manifest)
    checks.extend(he4_selection_checks)
    source_paths.extend(he4_selection_sources)

    prose_checks, prose_rows = audit_prose_claims(article_root, corrections_root)
    checks.extend(prose_checks)

    r1_overview_checks, r1_overview_rows, r1_overview_sources = audit_reviewer1_overview(workflow_root, article_root, corrections_root)
    checks.extend(r1_overview_checks)
    source_paths.extend(r1_overview_sources)

    r1_uncertainty_checks, r1_uncertainty_rows, r1_uncertainty_sources = audit_reviewer1_uncertainty(workflow_root, article_root, corrections_root)
    checks.extend(r1_uncertainty_checks)
    source_paths.extend(r1_uncertainty_sources)

    r1_remaining_checks, r1_remaining_rows, r1_remaining_sources = audit_reviewer1_remaining(workflow_root, article_root, corrections_root)
    checks.extend(r1_remaining_checks)
    source_paths.extend(r1_remaining_sources)

    forecast_checks, forecast_rows, forecast_sources = audit_forecast_design(workflow_root, article_root, corrections_root)
    checks.extend(forecast_checks)
    source_paths.extend(forecast_sources)

    latest_issue_checks, latest_issue_rows, latest_issue_sources = audit_latest_forecast_issue(workflow_root, article_root, corrections_root)
    checks.extend(latest_issue_checks)
    source_paths.extend(latest_issue_sources)

    software_checks, software_rows, software_sources = audit_software_availability(workflow_root, article_root, corrections_root)
    checks.extend(software_checks)
    source_paths.extend(software_sources)

    runtime_checks, runtime_rows, runtime_sources = audit_runtime_feasibility(workflow_root, article_root, corrections_root)
    checks.extend(runtime_checks)
    source_paths.extend(runtime_sources)

    compile_checks, compile_rows, compile_sources = audit_compile_logs(article_root, corrections_root, enabled=bool(args.after_patch))
    checks.extend(compile_checks)
    source_paths.extend(compile_sources)

    check_rows = [
        {"family": check.family, "item": check.item, "status": check.status, "detail": check.detail}
        for check in checks
    ]
    write_csv(output_dir / "check_summary.csv", check_rows, ["family", "item", "status", "detail"])
    write_csv(output_dir / "manifest_path_audit.csv", manifest_rows + correction_input_rows, ["kind", "label", "relative_path", "absolute_path", "exists"])
    write_csv(output_dir / "table_value_audit.csv", table_value_rows, ["table", "row_key", "column", "expected_rounded5", "observed", "abs_diff", "status"])
    write_csv(output_dir / "prose_claim_audit.csv", prose_rows, ["repo", "claim_type", "claim", "status"])
    write_csv(output_dir / "reviewer1_overview_audit.csv", r1_overview_rows, ["item", "status", "detail"])
    write_csv(output_dir / "reviewer1_uncertainty_audit.csv", r1_uncertainty_rows, ["item", "status", "detail"])
    write_csv(output_dir / "reviewer1_remaining_audit.csv", r1_remaining_rows, ["item", "status", "detail"])
    write_csv(output_dir / "forecast_design_audit.csv", forecast_rows, ["item", "status", "detail"])
    write_csv(output_dir / "latest_forecast_issue_audit.csv", latest_issue_rows, ["item", "status", "detail"])
    write_csv(output_dir / "software_availability_audit.csv", software_rows, ["item", "status", "detail"])
    write_csv(output_dir / "runtime_feasibility_audit.csv", runtime_rows, ["item", "status", "detail"])
    write_csv(output_dir / "compile_log_audit.csv", compile_rows, ["document", "log_path", "exists", "status", "detail"])

    unique_sources = sorted({path.resolve() for path in source_paths if path.exists() and path.is_file()})
    sha_rows = [{"path": str(path), "sha256": sha256_file(path)} for path in unique_sources]
    write_csv(output_dir / "source_sha256_manifest.csv", sha_rows, ["path", "sha256"])

    metadata["source_file_count"] = len(unique_sources)
    metadata["checks"] = {
        "total": len(checks),
        "failed": sum(1 for check in checks if check.status != "pass"),
    }
    (output_dir / "environment_metadata.json").write_text(json.dumps(metadata, indent=2) + "\n", encoding="utf-8")
    (output_dir / "cross_repo_validation_summary.json").write_text(json.dumps(metadata["checks"], indent=2) + "\n", encoding="utf-8")
    (output_dir / "cross_repo_validation_summary.md").write_text(render_summary_md(checks, metadata), encoding="utf-8")

    failed = [check for check in checks if check.status != "pass"]
    if failed and not args.allow_known_failures:
        print(f"Cross-repo validation failed with {len(failed)} failed checks. See {output_dir}")
        return 1
    print(f"Cross-repo validation passed. See {output_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
