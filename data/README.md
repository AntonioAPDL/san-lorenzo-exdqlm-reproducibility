# Staged Data

`data/staged/` contains compact, cutoff-specific inputs used by the
publication workflow. These are model-ready inputs, not raw
climate-center retrieval archives. Filenames are intentionally
descriptive; source roles and hashes are recorded in
`provenance/source_file_crosswalk.csv`.

The five forecast-origin folders are:

- `cutoff_2021_01_23`
- `cutoff_2021_11_12`
- `cutoff_2021_12_21`
- `cutoff_2022_05_11`
- `cutoff_2022_12_25`

Each contains retrospective products, issued GloFAS and NWS ensemble
forecast matrices, source-lineage metadata, and bundle health metadata.
