#!/usr/bin/env bash
set -euo pipefail

ROOT="${WORKFLOW_ROOT:-/data/muscat_data/jaguir26/project1_ucsc_phd}"
ARTICLE_ROOT="${ARTICLE_ROOT:-$ROOT/Evironmetrics---REVISED-DOC-Corrected-2}"
CORRECTIONS_ROOT="${CORRECTIONS_ROOT:-/data/muscat_data/jaguir26/Corrections---Project-1}"

run_compile=0
if [[ "${1:-}" == "--compile" ]]; then
  run_compile=1
elif [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  cat <<'USAGE'
Usage:
  scripts/validate_current_authority_sync.sh [--compile]

Checks the current HE2 authority wiring across the workflow repo, revised
article repo, corrections response repo, and tracked poster PDF.

Without --compile, this is a non-mutating validation pass. With --compile, it
also rebuilds the article PDF, corrections PDF, and poster PDF; use that before
committing a deliberate authority refresh because LaTeX embeds timestamps in
tracked PDFs.
USAGE
  exit 0
elif [[ -n "${1:-}" ]]; then
  echo "Unknown argument: $1" >&2
  exit 2
fi

require_file() {
  local path="$1"
  [[ -f "$path" ]] || {
    echo "Missing required file: $path" >&2
    exit 1
  }
}

section() {
  printf '\n== %s ==\n' "$1"
}

section "Repository Roots"
printf 'workflow:    %s\n' "$ROOT"
printf 'article:     %s\n' "$ARTICLE_ROOT"
printf 'corrections: %s\n' "$CORRECTIONS_ROOT"
require_file "$ARTICLE_ROOT/artifacts/he2_publication_freeze/he2_bayesian_publication_manifest.csv"
require_file "$ARTICLE_ROOT/isba2026_poster/poster.pdf"
require_file "$CORRECTIONS_ROOT/tables/generated_tex/he2_benchmark_crps_response_table.tex"

section "Workflow Validators"
cd "$ROOT"
python3 scripts/validate_he2_exal_keep_partial_screen_promotion.py
python3 scripts/validate_publication_freeze.py
python3 scripts/validate_revision_cross_repo_wiring.py
pytest -q \
  tests/python/test_publication_freeze_validation.py \
  tests/python/test_revised_article_stage1_refresh_contract.py \
  tests/python/test_he4_quantile_check_loss_tables.py

section "Article Authority Snapshot"
cd "$ARTICLE_ROOT"
python3 scripts/exal_m_t1_authoritative.py >/tmp/current_authority_snapshot.json
rg -n "0\\.26045|0\\.02273|0\\.53806|lowest 28-day CRPS in all five|lowest 1--28-step-ahead CRPS at all five" \
  wileyNJD-APA.tex \
  isba2026_poster/poster.tex \
  isba2026_poster/README.md \
  tables/generated_tex/benchmark_crps_body.tex \
  artifacts/he2_publication_freeze/he2_bayesian_publication_manifest.csv \
  artifacts/he4_quantile_check_loss_current_publication/he4_selection_audit.csv

if rg -n --hidden -S \
  "four of five|first four|DQLM leads|AL-M-T1 leads|best corrected Bayesian row|Selected exDQLM is lowest at four|2022-12-25 exception" \
  wileyNJD-APA.tex \
  isba2026_poster/poster.tex \
  isba2026_poster/README.md \
  isba2026_poster/notes \
  docs/figure_table_provenance.md \
  docs/exal_m_t1_artifact_run_map.md \
  artifacts/five_cutoff_crps_validation_sources \
  artifacts/five_cutoff_main_model_synthesis \
  artifacts/he4_quantile_check_loss_current_publication \
  artifacts/representative_selected_model_2022_12_25 \
  reports/manuscript_figure_selection \
  -g '!*.png' -g '!*.pdf'; then
  echo "Found stale active-facing authority wording." >&2
  exit 1
fi

section "Poster PDF Smoke Test"
pdfinfo isba2026_poster/poster.pdf | rg '^Pages:\s+1$'
pdftotext isba2026_poster/poster.pdf - | rg "Selected exDQLM has the lowest 1.28.step.ahead CRPS|at all five origins"

section "Corrections Response Snapshot"
cd "$CORRECTIONS_ROOT"
rg -n "0\\.26045|0\\.02273|0\\.53806|lowest 28-day CRPS in all five" \
  main.tex \
  tables/generated_tex/he2_benchmark_crps_response_table.tex \
  tables/generated_tex/he4_quantile_check_loss_response_table.tex

if rg -n --hidden -S \
  "four of five|first four|DQLM leads|AL-M-T1 leads|best corrected Bayesian row|Selected exDQLM is lowest at four|2022-12-25 exception" \
  main.tex \
  tables/generated_tex/he2_benchmark_crps_response_table.tex \
  tables/generated_tex/he4_quantile_check_loss_response_table.tex; then
  echo "Found stale active-facing corrections wording." >&2
  exit 1
fi

if [[ "$run_compile" -eq 1 ]]; then
  section "Compile Article"
  cd "$ARTICLE_ROOT"
  pdflatex -interaction=nonstopmode -halt-on-error wileyNJD-APA.tex
  pdflatex -interaction=nonstopmode -halt-on-error wileyNJD-APA.tex

  section "Compile Poster"
  Rscript isba2026_poster/scripts/build_poster_figures.R
  pdflatex -interaction=nonstopmode -halt-on-error -output-directory=isba2026_poster isba2026_poster/poster.tex
  pdflatex -interaction=nonstopmode -halt-on-error -output-directory=isba2026_poster isba2026_poster/poster.tex

  section "Compile Corrections"
  cd "$CORRECTIONS_ROOT"
  make
fi

section "Git Status"
git -C "$ROOT" status --short --branch
git -C "$ARTICLE_ROOT" status --short --branch
git -C "$CORRECTIONS_ROOT" status --short --branch

section "Current Authority Sync Validation Passed"
