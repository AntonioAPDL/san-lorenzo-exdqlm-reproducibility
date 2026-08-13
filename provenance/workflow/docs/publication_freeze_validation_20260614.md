# Publication Freeze Validation Closeout

Date: 2026-06-14

## Decision

The HE3 ablation study does **not** need to be rerun for the current
publication freeze.

The current authoritative HE3 ablation already uses the selected
`exAL-M-T1` / `exdqlm_multivar_keep` winners from
`docs/exdqlm_multivar_keep_authoritative_specs_20260601.yaml`. The full HE3
row matches those winners to numerical precision for all five cutoffs, and the
article/corrections table values are rendered from the same passed ablation
artifact.

## Frozen Repository Heads

- workflow repo: this commit;
- revised article repo: `38e1230`;
- corrections repo: `7fd2fb0`.

## Authoritative Inputs And Outputs

Current selected `exAL-M-T1` winner manifest:

- `docs/exdqlm_multivar_keep_authoritative_specs_20260601.yaml`

Current HE3 ablation runtime root:

- `/data/muscat_data/jaguir26/project1_ucsc_phd_runtime/multimodel_v8_he3_exdqlm_ablation_authoritative_winners_20260608`

Current HE3 report bundle:

- `/data/muscat_data/jaguir26/project1_ucsc_phd_runtime/multimodel_v8_he3_exdqlm_ablation_authoritative_winners_20260608/reports/he3_exdqlm_ablation/he3_ablation_summary.md`
- `/data/muscat_data/jaguir26/project1_ucsc_phd_runtime/multimodel_v8_he3_exdqlm_ablation_authoritative_winners_20260608/reports/he3_exdqlm_ablation/he3_ablation_wide.csv`

Current frozen article artifacts:

- `Evironmetrics---REVISED-DOC-Corrected-2/artifacts/he2_publication_freeze/he2_bayesian_publication_manifest.csv`
- `Evironmetrics---REVISED-DOC-Corrected-2/artifacts/he3_exdqlm_ablation_authoritative/he3_ablation_wide.csv`
- `Evironmetrics---REVISED-DOC-Corrected-2/artifacts/he4_quantile_check_loss_current_publication/he4_quantile_check_loss_wide.csv`
- `Evironmetrics---REVISED-DOC-Corrected-2/artifacts/representative_selected_model_2022_12_25/bundle_metadata.json`

## New Guard

The new validator is:

```bash
python3 scripts/validate_publication_freeze.py
```

It checks:

1. the selective HE2 Table 1 repair overlay has exactly 16 active replacement
   rows;
2. the eight repaired rows with worse CRPS remain non-promoted fallbacks;
3. the article frozen HE2 manifest matches the selective overlay;
4. the HE3 ablation matrix has 30 passing rows over five cutoffs and six
   variants;
5. the HE3 `full` rows match the authoritative `exAL-M-T1` winners exactly;
6. the article-side HE3 artifact matches the workflow runtime HE3 artifact;
7. the article and corrections HE3 tables render the runtime values to five
   decimals;
8. HE4 selection is sourced from the frozen HE2 publication manifest;
9. selected-model figures are wired to the current representative
   `2022-12-25 exAL-M-T1` output authority;
10. article and corrections prose retain the final-cutoff nuance: `AL-M-T1` is
    the best corrected Bayesian row at `2022-12-25`, while raw NWS is the best
    overall reference there.

The latest untracked runtime validation report is:

- `reports/publication_freeze_validation_20260614/PUBLICATION_FREEZE_VALIDATION.md`

Latest result:

- status: `pass`;
- failed checks: `0`;
- check families: `he2_selective`, `he3_authority`, `he4_sync`,
  `figure_lineage`, and `prose`.

## Validation Commands Run

HE2 manifest/parity/readiness and unit tests:

```bash
python3 scripts/build_he2_bayesian_publication_manifest.py
python3 scripts/build_he2_publication_parity_gate.py
python3 scripts/build_he2_crps_table_readiness_audit.py
python3 -m unittest \
  tests.python.test_he2_bayesian_publication_manifest \
  tests.python.test_he2_crps_table_readiness_audit \
  tests.python.test_he2_publication_parity_gate \
  tests.python.test_publication_freeze_validation -v
```

Result: `14` tests passed.

HE4 table tests:

```bash
python3 -m pytest tests/python/test_he4_quantile_check_loss_tables.py -q
```

Result: `7` tests passed.

Article asset, figure path, and authoritative-output lineage checks:

```bash
python3 Evironmetrics---REVISED-DOC-Corrected-2/scripts/build_article_asset_review_report.py \
  --article-root Evironmetrics---REVISED-DOC-Corrected-2
python3 Evironmetrics---REVISED-DOC-Corrected-2/scripts/validate_manuscript_figure_paths.py \
  --article-root Evironmetrics---REVISED-DOC-Corrected-2
python3 Evironmetrics---REVISED-DOC-Corrected-2/scripts/validate_authoritative_output_lineage.py \
  --article-root Evironmetrics---REVISED-DOC-Corrected-2 \
  --corrections-root /data/muscat_data/jaguir26/Corrections---Project-1 \
  --report-dir /tmp/authoritative_output_lineage_check_20260614
```

Result: all passed.

LaTeX compiles:

```bash
(cd Evironmetrics---REVISED-DOC-Corrected-2 && \
  pdflatex -interaction=nonstopmode -halt-on-error main.tex && \
  pdflatex -interaction=nonstopmode -halt-on-error main.tex)

(cd /data/muscat_data/jaguir26/Corrections---Project-1 && \
  pdflatex -interaction=nonstopmode -halt-on-error main.tex && \
  pdflatex -interaction=nonstopmode -halt-on-error main.tex)
```

Result:

- revised article compiled to `main.pdf`, 29 pages;
- corrections document compiled to `main.pdf`, 13 pages.

Strict cross-repo validation:

```bash
python3 scripts/validate_revision_cross_repo_wiring.py \
  --workflow-root /data/muscat_data/jaguir26/project1_ucsc_phd \
  --article-root /data/muscat_data/jaguir26/project1_ucsc_phd/Evironmetrics---REVISED-DOC-Corrected-2 \
  --corrections-root /data/muscat_data/jaguir26/Corrections---Project-1 \
  --output-dir /tmp/revision_cross_repo_wiring_check_20260614 \
  --check-only --after-patch --strict
```

Result: pass.

Publication freeze validation:

```bash
python3 scripts/validate_publication_freeze.py
```

Result: pass.

## Rerun Policy

Do not rerun the HE3 ablation study unless one of these changes:

1. the authoritative `exAL-M-T1` winner manifest changes;
2. the shared input bundle or covariate contract changes;
3. the forecast-window CRPS calculation changes;
4. the selected-model synthesis/post-processing logic changes;
5. the HE3 table is intentionally redefined to target a different selected
   model family.

For the current freeze, the next work is article/corrections polishing and
final submission packaging, not another expensive ablation launch.
