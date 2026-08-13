# Current Authority Refresh Runbook

Last updated: 2026-06-23

This runbook is the forward-facing contract for updating the publication
authority again. It replaces date-specific memory from earlier promotion notes
with one ordered path across the workflow repo, revised article repo, corrections
repo, and poster.

## Scope

Use this runbook when a new HE2 publication-authority row is formally promoted.
The current source of truth is:

- workflow overlay:
  `config/he2_publication_manifest_replacement_overlay_current_authority_20260623.yaml`
- workflow manifest builder:
  `scripts/build_he2_bayesian_publication_manifest.py`
- article freeze:
  `Evironmetrics---REVISED-DOC-Corrected-2/artifacts/he2_publication_freeze/he2_bayesian_publication_manifest.csv`
- article run map:
  `Evironmetrics---REVISED-DOC-Corrected-2/docs/exal_m_t1_artifact_run_map.md`
- corrections response tables:
  `SOURCE_CORRECTIONS_ROOT/tables/generated_tex/`

Do not treat exploratory screening output as publication authority until it has
passed the promotion gates below and has been copied into the article/corrections
freeze.

## Promotion Gates

A replacement run can become publication authority only when all of these are
true:

- `fit`, `post`, `validate`, and `report` stages passed.
- The row uses the intended publication input bundle and horizon contract.
- The CRPS is lower than, or tied with, the current authority under the same
  scoring contract.
- Required post outputs exist: CRPS summaries, per-time CRPS, synthesis figures,
  quantile CSVs, posterior sample subset, source-parameter summaries, covariate
  summaries, and figure manifests where relevant.
- Heavy `.RData`, `.rda`, and `.rdata` files are not retained in the promoted
  publication campaign.
- The replacement is written into the workflow overlay/manifest and then
  snapshotted into the revised article repo.
- HE4 quantile check-loss tables are regenerated from the refreshed HE2
  publication manifest.
- The corrections response tables are regenerated from the revised article
  generated table bodies.
- Current-facing article/poster/corrections prose no longer contains stale
  authority claims.

## Benchmark Authority Refresh

Start in the workflow repo:

```bash
cd SOURCE_WORKFLOW_ROOT
```

If the replacement follows the current exDQLM multivariate-keep clean replay
path, use the existing promotion helper and inspect its report:

```bash
python3 scripts/promote_he2_exal_keep_clean_authority.py \
  --out-dir reports/he2_exal_keep_clean_authority_promotion_YYYYMMDD \
  --apply

python3 scripts/validate_he2_exal_keep_partial_screen_promotion.py \
  --out-dir reports/he2_exal_keep_clean_authority_promotion_YYYYMMDD/selected_overlay_validation
```

For a different model family or campaign, update the replacement overlay with
the same fields used by current rows: `cutoff`, `family`, `manuscript_label`,
`run_id`, optional `run_root`, `campaign_lineage`, `replacement_reason`,
`publication_note`, and `replaced_source_run_id`.

Rebuild the workflow-side HE2 manifest:

```bash
python3 scripts/build_he2_bayesian_publication_manifest.py
python3 scripts/build_he2_publication_parity_gate.py
```

Refresh the article-side HE2 freeze:

```bash
ARTICLE_ROOT=SOURCE_ARTICLE_ROOT

python3 "$ARTICLE_ROOT/scripts/refresh_he2_manifest_snapshot.py" \
  --article-root "$ARTICLE_ROOT" \
  --workflow-root SOURCE_WORKFLOW_ROOT
```

Regenerate HE4 from the refreshed HE2 article manifest:

```bash
HE4_OUT=/tmp/he4_current_authority_refresh
rm -rf "$HE4_OUT"

python3 scripts/build_he4_quantile_check_loss_tables.py \
  --source-mode he2-publication-manifest \
  --he2-publication-manifest "$ARTICLE_ROOT/artifacts/he2_publication_freeze/he2_bayesian_publication_manifest.csv" \
  --output-dir "$HE4_OUT"

rsync -a --delete "$HE4_OUT"/ \
  "$ARTICLE_ROOT/artifacts/he4_quantile_check_loss_current_publication"/
```

Refresh article-side selected-model bundles, synthesis families, generated
tables, and poster artifacts:

```bash
cd "$ARTICLE_ROOT"

python3 scripts/refresh_exal_m_t1_generated_assets.py --article-root .
python3 scripts/refresh_cutoff_synthesis_families.py --article-root .
python3 scripts/build_generated_table_includes.py --article-root .
python3 scripts/promote_generated_figures_to_disc.py --article-root .
python3 scripts/sync_legacy_uppercase_figures.py --article-root .

Rscript isba2026_poster/scripts/build_poster_figures.R
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=isba2026_poster isba2026_poster/poster.tex
pdflatex -interaction=nonstopmode -halt-on-error -output-directory=isba2026_poster isba2026_poster/poster.tex
```

If manuscript setup/support assets also changed, run the broader article
refresh path after confirming `config/runtime_bindings.json` points to the new
runtime roots:

```bash
python3 scripts/refresh_all_generated_assets.py --article-root .
```

Do not use the broad refresh to imply that dry/wet or component diagnostics are
refreshed unless their full-history support replay has actually been rebuilt.

## Full-History Support Diagnostics

The dry/wet and 80-month component diagnostics have a different provenance role
from the HE2 benchmark table. They must be current relative to the representative
selected-output authority used for the synthesis figure, while remaining
documented as interpretation diagnostics rather than forecast-validation
evidence. If a new authority changes the representative selected model, refresh
the selected-model support bundle and then update:

- article `config/runtime_bindings.json`
- `artifacts/representative_selected_model_2022_12_25/authoritative_support/`
- `MANUSCRIPT_ASSET_MANIFEST.json`
- `docs/exal_m_t1_artifact_run_map.md`
- `docs/figure_table_provenance.md`

The correct documented state for the current manuscript pass is
`current_selected_model_representative`; do not describe those diagnostics as
refreshed HE2 forecast evidence.

## Corrections Response Sync

Regenerate the corrections response tables from the article generated table
bodies:

```bash
CORRECTIONS_ROOT=SOURCE_CORRECTIONS_ROOT

python3 "$ARTICLE_ROOT/scripts/sync_corrections_generated_table_includes.py" \
  --article-root "$ARTICLE_ROOT" \
  --corrections-root "$CORRECTIONS_ROOT"

cd "$CORRECTIONS_ROOT"
make
```

Update `main.tex` only for prose claims that must change; the table fragments
should remain generated from the article repo.

## Validation

The one-command current-authority validation wrapper is:

```bash
cd SOURCE_WORKFLOW_ROOT
scripts/validate_current_authority_sync.sh
```

Before committing a refresh that changed rendered PDFs, run the compile mode:

```bash
scripts/validate_current_authority_sync.sh --compile
```

The wrapper runs the workflow validators, targeted pytest suite, stale-claim
sweeps, poster-PDF smoke test, and corrections table spot checks. Compile mode
also rebuilds the article, poster, and corrections PDFs.

## Commit And Push

Use three commits unless the change is trivial:

1. workflow repo: overlay/manifest/validators/tests/runbook updates.
2. revised article repo: refreshed freezes, generated figures/tables, poster
   source/PDF, and provenance docs.
3. corrections repo: synced generated response tables and response prose.

Before push:

```bash
git status --short --branch
git -C "$ARTICLE_ROOT" status --short --branch
git -C "$CORRECTIONS_ROOT" status --short --branch
```

After push, confirm each remote SHA matches local:

```bash
git rev-parse main
git ls-remote origin refs/heads/main
```

The article repo is Overleaf-facing. Keep it lightweight: do not add runtime
directories, `.RData`, `.rda`, `.rdata`, or large uncurated support dumps.
