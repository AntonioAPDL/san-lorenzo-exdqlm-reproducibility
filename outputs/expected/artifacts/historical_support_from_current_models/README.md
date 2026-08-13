# Historical Support From Current Models

This article-side artifact bundle regenerates the historical-support manuscript figures from corrected current model outputs.

Sources:
- Canonical completed multivariate run: `SOURCE_RUNTIME_ROOT/multimodel_v8_he2_exdqlm_multivar_keep_epsilon_discount_grid_20260524/runs/multimodel_20220511_v8_he2grid_c02_eps060_exdqlm_multivar_keep`
- Historical-support render run: `SOURCE_RUNTIME_ROOT/multimodel_v8_he2_exdqlm_multivar_keep_epsilon_discount_grid_20260524/runs/multimodel_20220511_v8_he2grid_c02_eps060_exdqlm_multivar_keep`
- Univariate transfer-active reference figure: `SOURCE_RUNTIME_ROOT/multimodel_v8_he2_univar_al_exal_scale_repair_20260629/runs/multimodel_20221225_v8_he2pubgdpc1r1_exdqlm_univar/post/outputs/multimodel_20221225_v8_he2pubgdpc1r1_exdqlm_univar/exdqlm_univar_synth_cutoff_window_posterior_samples.png`

Retained support contract:
- `SOURCE_ARTICLE_ROOT/artifacts/historical_support_from_current_models/figures/cache/historical_support_state_summaries.rds` preserves the corrected multivariate state summary needed by the renderer after ephemeral fit caches are cleaned from the canonical workflow root.

Refresh entrypoint:
- `scripts/refresh_current_model_output_support_figures.py`
- For the univariate reference alone, run `scripts/refresh_current_model_output_support_figures.py --univar-only`; this refreshes the frozen support artifact and both manuscript-facing figure aliases.
