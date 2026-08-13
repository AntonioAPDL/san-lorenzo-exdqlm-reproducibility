# Historical Support From Current Models

This article-side artifact bundle regenerates the historical-support manuscript figures from corrected current model outputs.

Sources:
- Canonical completed multivariate run: `SOURCE_RUNTIME_REFERENCE
- Historical-support render run: `SOURCE_RUNTIME_REFERENCE
- Univariate transfer-active reference figure: `SOURCE_RUNTIME_REFERENCE

Retained support contract:
- `SOURCE_ARTICLE_REFERENCE preserves the corrected multivariate state summary needed by the renderer after ephemeral fit caches are cleaned from the canonical workflow root.

Refresh entrypoint:
- `scripts/refresh_current_model_output_support_figures.py`
- For the univariate reference alone, run `scripts/refresh_current_model_output_support_figures.py --univar-only`; this refreshes the frozen support artifact and both manuscript-facing figure aliases.
