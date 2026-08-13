# Climate Product Versioning and Public Scope

The validation study uses forecast-origin bundles that align four
information streams at each cutoff:

- USGS observed daily river flow for the San Lorenzo River at Big Trees.
- ECMWF/GloFAS retrospective hydrologic products before the cutoff and
  issued GloFAS ensemble forecasts after the cutoff.
- NOAA/NWS/National Water Model retrospective hydrologic products before
  the cutoff and issued NWS ensemble forecasts after the cutoff.
- Exogenous covariates: local precipitation, local shallow soil-water,
  and a GDPC climate-index summary.

The public repository includes the staged inputs used by the manuscript.
It does not attempt to reproduce the raw historical archive recovery.
That reconstruction requires product-version matching, spatial
extraction rules, source-specific forecast horizons, and cutoff-specific
issued forecast bundles for each climate-center product family.

This versioning detail is most important for the hydrologic product
families. GloFAS/ECMWF and NWS/NWM each contribute retrospective
information and issued forecast information, and those products differ
in version history, spatial support, ensemble structure, update cycle,
and forecast horizon. The model inputs exported here are the aligned
result of that recovery and harmonization step.
