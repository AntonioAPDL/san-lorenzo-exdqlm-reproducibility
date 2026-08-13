# Model Inputs by Forecast Origin

The public data bundle starts from model-ready inputs for the five
forecast origins used in the manuscript:

- `cutoff_2021_01_23`
- `cutoff_2021_11_12`
- `cutoff_2021_12_21`
- `cutoff_2022_05_11`
- `cutoff_2022_12_25`

Each folder under `data/staged/forecast_origins/` contains:

- `retrospective_products_daily.csv`: USGS observations and aligned
  retrospective hydrologic product inputs available before the cutoff.
- `glofas_ensemble_forecast_daily.csv`: GloFAS ensemble forecast matrix
  issued at the cutoff.
- `nws_ensemble_forecast_daily.csv`: NWS/NWM ensemble forecast matrix
  issued at the cutoff.
- `retrospective_source_lineage.csv`: compact source labels for the
  retrospective products.
- `origin_metadata.yaml` and `bundle_health.json`: compact checks and
  metadata for the staged origin bundle.

Shared covariates live under `data/staged/covariates/`:

- `local_precipitation_daily.csv`
- `local_shallow_soil_water_daily.csv`
- `gdpc_climate_index_pc1_daily.csv`

The forecast-window precipitation and shallow soil-water covariates are
included as deterministic, model-ready summaries derived from
post-processed GEFS forecast products. The public repository does not
bundle raw GEFS retrievals or intermediate covariate-construction
workflows. The GDPC series is a climate-index summary covariate, not an
operational forecast product.
