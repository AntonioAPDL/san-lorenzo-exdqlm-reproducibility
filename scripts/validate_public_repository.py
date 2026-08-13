#!/usr/bin/env python3
from __future__ import annotations

import csv
import hashlib
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FORBIDDEN_SUFFIXES = {
    ".RData",
    ".rda",
    ".rdata",
    ".nc",
    ".grib",
    ".grib2",
    ".zarr",
    ".pkl",
    ".pickle",
    ".parquet",
    ".feather",
    ".h5",
    ".hdf5",
}
REQUIRED = [
    "README.md",
    "CITATION.cff",
    "LICENSE",
    "Makefile",
    "data/staged/source_series/glofas_lisflood_retrospective_daily.csv",
    "data/staged/source_series/nws_nwm_retrospective_v30_daily.csv",
    "data/staged/covariates/local_precipitation_daily.csv",
    "data/staged/covariates/local_shallow_soil_water_daily.csv",
    "data/staged/covariates/gdpc_climate_index_pc1_daily.csv",
    "tables/generated_tex/benchmark_crps_main_table.tex",
    "figures/manuscript_context/site_context_usgs.png",
    "outputs/expected/artifacts/he2_publication_freeze/he2_bayesian_publication_manifest.csv",
    "provenance/source_file_crosswalk.csv",
    "provenance/runtime_source_crosswalk.csv",
    "data/SHA256SUMS.txt",
]
TEXT_SUFFIXES = {
    ".R",
    ".Rmd",
    ".bib",
    ".bst",
    ".cff",
    ".cls",
    ".csv",
    ".json",
    ".md",
    ".py",
    ".sh",
    ".sty",
    ".tex",
    ".txt",
    ".yaml",
    ".yml",
}
TEXT_FILENAMES = {"Makefile", "LICENSE", ".gitignore"}
ALLOWED_LOCAL_PATH_FILES = {
    "provenance/source_file_crosswalk.csv",
    "provenance/runtime_source_crosswalk.csv",
}
LOCAL_PATH_MARKERS = ("/" + "data/muscat_data/", "/" + "data/jaguir26/")
STALE_TEXT_MARKERS = (
    "https://github.com/AntonioAPDL/" + "Project1",
    "PROJECT1" + "_URL",
    "chat" + "gpt",
    "co" + "dex",
    "ai" + "-generated",
    "ai " + "wording",
)


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as fh:
        for chunk in iter(lambda: fh.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def is_text_like(path: Path) -> bool:
    return path.suffix in TEXT_SUFFIXES or path.name in TEXT_FILENAMES


def main() -> int:
    errors = []
    for rel in REQUIRED:
        if not (ROOT / rel).exists():
            errors.append(f"missing required file: {rel}")

    for path in ROOT.rglob("*"):
        if not path.is_file() or ".git" in path.parts:
            continue
        rel = path.relative_to(ROOT).as_posix()
        if path.suffix in FORBIDDEN_SUFFIXES:
            errors.append(f"forbidden heavy/runtime file type: {rel}")
        if path.stat().st_size > 100 * 1024 * 1024:
            errors.append(f"oversized file >100MB: {rel}")
        if is_text_like(path):
            try:
                text = path.read_text(encoding="utf-8")
            except UnicodeDecodeError:
                continue
            lower_text = text.lower()
            if rel not in ALLOWED_LOCAL_PATH_FILES and any(marker in text for marker in LOCAL_PATH_MARKERS):
                errors.append(f"local absolute path outside provenance crosswalk: {rel}")
            for marker in STALE_TEXT_MARKERS:
                if marker.lower() in lower_text:
                    errors.append(f"stale/internal marker {marker!r} in {rel}")

    manifest = ROOT / "data/SHA256SUMS.txt"
    if manifest.exists():
        for line in manifest.read_text(encoding="utf-8").splitlines():
            if not line.strip():
                continue
            expected, relpath = line.split("  ", 1)
            path = ROOT / relpath
            if not path.exists():
                errors.append(f"hash manifest missing file: {relpath}")
            elif sha256(path) != expected:
                errors.append(f"hash mismatch: {relpath}")

    crosswalk = ROOT / "provenance/source_file_crosswalk.csv"
    if crosswalk.exists():
        with crosswalk.open(newline="", encoding="utf-8") as fh:
            rows = list(csv.DictReader(fh))
        if len(rows) < 50:
            errors.append("source_file_crosswalk.csv has unexpectedly few rows")

    if errors:
        print("Public repository validation failed:")
        for item in errors:
            print(f"- {item}")
        return 1

    print("Public repository validation passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
