# NWS/NWM and GloFAS Data Audit Plan (Metadata-First, Version Compatibility)

Date created: 2026-02-14  
Last updated: 2026-02-20  
Scope: version, release, coverage, format, access, and targeted point-level validation for version-compatibility decisions.  
Working constraint: metadata-first audit with controlled local validation runs when unresolved lineage/coverage claims require direct verification.

## 1) Purpose

Build one compact, shareable documentation artifact that tracks, for NWS/NWM and GloFAS:

1. Model versions (chronological).
2. Release dates for each version.
3. Retrospective/reanalysis products linked to each version (including release date and coverage window).
4. Forecast ensemble products linked to each version (including release/operational date and coverage window).
5. Reforecast/hindcast products linked to each version (if available), with release date and coverage window.
6. Where each dataset lives and how it is accessed (authoritative portals and metadata endpoints).

## 1.1) Current Status Snapshot (2026-02-17)

1. NWS/NWM and GloFAS version timelines are populated from reviewed official sources.
2. GloFAS historical campaign C is complete (`1289/1289` shards downloaded) for:
   - `version_2_1 + htessel_lisflood + consolidated`
   - `version_3_1 + lisflood + consolidated`
   - `version_4_0 + lisflood + consolidated`
3. Point-series extraction is complete for:
   - C historical products (`v2.1`, `v3.1`, `v4.0`) at Big Trees target point
   - D legacy reanalysis `v3.0` (global archive reduced to point series)
4. Legacy-vs-historical value checks are now available:
   - Legacy reanalysis `v3.0` and historical `version_3_1` are not numerically identical at the target point.
   - Prior alias-style normalization is replaced by explicit label handling plus lineage-context notes.
5. Remaining unknowns are explicitly tracked (not inferred), especially:
   - NWM forecast-side reforecast/hindcast product metadata.
   - Per-version NWM retrospective publication dates.
   - Numeric expansion of GloFAS forecast `operational` alias in retrieve metadata.
   - Legacy `v4.0` parity status versus EWDS historical `version_4_0`.

## 1.2) Locked Project Decisions for This Audit

1. Cutoff convention is `date_only` (`YYYY-MM-DD`), not cycle-level.
2. NWS/NWM scope is CONUS only for current bias-transfer experiments.
3. This audit is metadata-first; targeted local extraction/validation runs are allowed when needed to resolve compatibility claims.
4. Variable/unit transformation policy for model training is out of scope for this document.

## 2) One-Page Outline for the Final Shareable Documentation

1. Scope and purpose.
2. NWM version timeline and product mapping.
3. GloFAS version timeline and product mapping.
4. Data-location and access registry.
5. Compatibility review notes (version pairing rationale and unresolved transitions).
6. Open issues, unknowns, and next verification targets.
7. Source list and last-verified dates.

## 2.1) Evidence Hierarchy Used in This Document

When sources disagree, this document uses the following precedence:

1. Official versioning/release notices and official API/catalog metadata endpoints.
2. Official dataset overview pages and official release pages.
3. Supporting sources (marketplace summaries, community repos, local archives).

Applied coherence rules:

1. Chronology anchors use official release/versioning notices first.
2. Product-behavior details use per-product dataset/release pages.
3. Local/project evidence is clearly labeled as supplemental, not authoritative chronology.

## 3) Version Summaries (Verified So Far)

### 3.1 NWS/NWM Operational Version Timeline

| nwm_version | operational_effective_date | evidence_source_id | notes |
|---|---|---|---|
| Initial NWM implementation (pre-v1.1 label) | 2016-08-16 | `NWM-URL-16` | Initial operational implementation notice (TIN). |
| 1.1 | 2017-05-08 | `NWM-URL-11` | SCN updated date from May 4 to May 8. |
| 1.2 | 2018-03-06 | `NWM-URL-12` | SCN states operationally running v1.2. |
| 2.0 | 2019-06-19 | `NWM-URL-13` | SCN states operationally running v2.0. |
| 2.1 | 2021-04-20 | `NWM-URL-14` | SCN states operationally running v2.1. |
| 3.0 | 2023-09-20 | `NWM-URL-15` | SCN states operationally running v3.0. |
| 3.1 | Proposed in PNS (comment period through 2026-01-29) | `NWM-URL-18` | Proposal notice found; implementation date not yet confirmed in this audit. |

### 3.2 NWS/NWM Retrospective by Version (AWS Registry)

| nwm_version | retrospective_product | retrospective_release_date | retrospective_coverage_start | retrospective_coverage_end | format_notes | key_access_locations | notes |
|---|---|---|---|---|---|---|---|
| 3.0 | NWM retrospective v3.0 | not explicitly listed | 1979-02 | 2023-01 | NetCDF and Zarr | `NWM-URL-05` | Registry states no streamflow/data assimilation in retrospective simulations. |
| 2.1 | NWM retrospective v2.1 | not explicitly listed | 1979-02 | 2020-12 | NetCDF (+ Zarr resources listed) | `NWM-URL-05` | AORC forcing notes provided in registry text. |
| 2.0 | NWM retrospective v2.0 | not explicitly listed | 1993-01 | 2018-12 | NetCDF (plus separate zarr streamflow bucket listed) | `NWM-URL-05` | Forcing notes in registry mention NLDAS for v2.0/v1.2. |
| 1.2 | NWM retrospective v1.2 | not explicitly listed | 1993-01 | 2017-12 | NetCDF | `NWM-URL-05` | Legacy retrospective bucket listed as `nwm-archive`. |

### 3.2.1 NWM Retrospective Technical Notes (Registry + Marketplace Text)

| topic | verified note | evidence |
|---|---|---|
| Data assimilation in retrospective runs | No streamflow or other DA is performed in retrospective simulations. | `NWM-URL-05`, `NWM-URL-07` |
| Domain coverage by version | CONUS is provided by v1.2/v2.0/v2.1/v3.0; oCONUS retrospective coverage (Alaska, Hawaii, Puerto Rico/USVI) is only in v3.0. | `NWM-URL-07`, `NWM-URL-05` |
| CONUS forcing by version | v3.0 and v2.1 use AORC (v2.1 uses AORC v1.0 for 1979-2006 and v1.1 for 2007-2020; v3.0 uses AORC v1.1 for full period). v2.0 and v1.2 use NLDAS. | `NWM-URL-07`, `NWM-URL-05` |
| v3.0 forcing metadata caveat | Listing text warns some v3.0 forcing metadata tags may read “v2.1” even though files are v3.0 forcing files. | `NWM-URL-07` |
| Format availability by version | Retrospective data are available as NetCDF and/or Zarr with version-dependent field coverage. | `NWM-URL-05`, `NWM-URL-07` |

### 3.3 NWS/NWM Forecast and Access Snapshot

| nwm_version | forecast_operational_start | forecast_operational_end | forecast_config_summary | key_evidence_sources | notes |
|---|---|---|---|---|---|
| 1.0 (initial implementation) | 2016-08-16 | 2017-05-07 | Short-range deterministic to 15h; medium-range deterministic to 10 days (once/day); long-range 30-day ensemble (4 members/cycle, 16/day). | `NWM-URL-16` | Baseline configuration from TIN16-30. |
| 1.1 | 2017-05-08 | 2018-03-05 | Four configurations retained; short-range extended to 18h; medium-range cycle frequency increased to 4/day. | `NWM-URL-11`, `NWM-URL-16` | SCN17-41 update states date changed from May 4 to May 8. |
| 1.2 | 2018-03-06 | 2019-06-18 | Same four configurations as v1.1 (analysis/assimilation, short, medium, long). | `NWM-URL-12` | SCN18-16 says v1.2 framework unchanged from v1.1. |
| 2.0 | 2019-06-19 | 2021-04-19 | Medium-range products in per-member directories `medium_range_memM` (M=1..7); Hawaii forecast products added. | `NWM-URL-13` | SCN19-42 describes member-specific medium-range files. |
| 2.1 | 2021-04-20 | 2023-09-19 | v2.1 upgrade with PR/USVI additions and post-processing updates; forecast products remain operationally versioned. | `NWM-URL-14` | SCN20-119 confirms operational start date. |
| 3.0 | 2023-09-20 | ongoing (as of 2026-02-15) | CONUS medium-range six-member ensemble; long-range daily 16-member 30-day ensemble; added TWL coastal guidance in v3.0 era. | `NWM-URL-15`, `NWM-URL-04` | NWM About page reflects current v3.0 operational cycling. |
| 3.1 | not yet operationally confirmed in reviewed sources | not applicable | Proposal notice found; implementation pending approval/SCN. | `NWM-URL-18` | PNS25-77 is proposal-only, not implementation notice. |

### 3.3.1 NWM Release-Notes Cross-Check (Ensemble-Member Structure)

Source:

- `NWM-URL-20`: `https://www.nco.ncep.noaa.gov/pmb/codes//nwprod/nwm.v3.0.17/doc/NationalWaterModelReleaseNotes.pdf`

Release-note findings relevant to member-structure and timeline interpretation:

| version | release-notes statement (condensed) | expected forecast-member implication | local archive check (`NWM-LOCAL-01`) |
|---|---|---|---|
| 2.0.0 | Added Medium Range ensemble forecast configuration: 7 members (4 cycles/day). | Transition from pre-2.0 single-member medium-range structure to 7-member structure. | Matches observed `1 -> 7` structural transition on `2019-06-19`. |
| 2.1.0 | Medium-range job remains `Medium Range 1-7`; cycle policy still treats medium-range as 7-member ensemble execution. | No structural member-count change expected at v2.1 boundary. | Matches observed no structural change at `2021-04-20`. |
| 3.0.0 | Explicit removal of NWM medium-range member 7; medium-range model listed as `1-6`. | Structural reduction from 7 to 6 members at v3.0 boundary. | Matches observed `7 -> 6` structural transition on `2023-09-20`. |

Interpretation:

1. The release-note evidence indicates that observed member-count transitions are consistent with documented version changes (especially 2.0 and 3.0).
2. The release notes provide change semantics (what changed), while SCN/TIN notices remain the primary source for operational effective dates used in Section 3.1.
3. This addresses prior uncertainty about v2.1 member-count behavior: no structural count change is expected at that boundary.

### 3.4 NWS/NWM Reforecast or Hindcast Availability Check

| check_item | status | evidence | note |
|---|---|---|---|
| Forecast-side reforecast product by NWM version | not found in reviewed authoritative NWS/NODD sources | `NWM-URL-04`, `NWM-URL-05`, `NWM-URL-11`, `NWM-URL-12`, `NWM-URL-13`, `NWM-URL-14`, `NWM-URL-15`, `NWM-URL-17`, `NWM-URL-19` | No explicit versioned forecast reforecast/hindcast catalog entry found in reviewed NWS/NODD URLs. |
| Retrospective runs (historical simulations) | found | `NWM-URL-05` | Retrospective datasets are available and versioned, but these are not labeled as forecast reforecast products in the reviewed sources. |

### 3.4.1 Project-Local Raw NWM Ensemble Archive Evidence (`results.pkl`)

Evidence role:

1. This subsection is supplemental local evidence (`NWM-LOCAL-01`) for member-structure behavior.
2. It is used to validate observed transitions against official release chronology, not to replace official release metadata.

Evidence file (project-local):

- `SOURCE_WORKFLOW_ROOT/results.pkl` (`NWM-LOCAL-01`)
- Object type: Python dictionary (`1,832,084` entries)
- Key pattern parsed for this audit:
  - `issue_date`: `nwm.YYYYMMDD/...`
  - `ensemble_member`: `medium_range_memX` (default member `1` when `memX` token is absent)
  - `lead_time_h`: `.fNNN.`
  - `issue_hour`: `.t00z` or `.t12z`
- Value: single nonnegative point-location forecast value (float) in the audited file

Coverage summary from parsed keys:

| field | value |
|---|---|
| Issue-date range | `2018-09-17` to `2024-02-20` |
| Issue dates with any data | `1,980` |
| Missing issue dates in contiguous span | `2019-03-10`, `2020-03-12`, `2020-11-14` |
| Target-date range (`issue_date + lead_time_h`) | `2018-09-17` to `2024-03-01` |
| Missing target dates in contiguous span | none |
| Issue hours observed | `00`, `12` |

Observed member-set timeline by issue date:

| date window | observed member set | member count | classification |
|---|---|---|---|
| `2018-09-17` to `2019-06-18` | `{1}` | `1` | persistent |
| `2019-06-19` to `2023-09-19` | `{1,2,3,4,5,6,7}` | `7` | persistent (with one-day anomalies below) |
| `2023-09-20` to `2024-02-20` | `{1,2,3,4,5,6}` | `6` | persistent |

One-day member-set anomalies in the 7-member era:

| date | observed member set |
|---|---|
| `2020-03-16` | `{2,3,4,5,7}` |
| `2020-07-29` | `{1,2,3,4,5,7}` |
| `2022-07-14` | `{1,2,3,4,6,7}` |

Per-member first/last issue-date coverage:

| member | first issue date | last issue date | notes |
|---|---|---|---|
| 1 | `2018-09-17` | `2024-02-20` | missing on `2020-03-16` |
| 2 | `2019-06-19` | `2024-02-20` |  |
| 3 | `2019-06-19` | `2024-02-20` |  |
| 4 | `2019-06-19` | `2024-02-20` |  |
| 5 | `2019-06-19` | `2024-02-20` | missing on `2022-07-14` |
| 6 | `2019-06-19` | `2024-02-20` | missing on `2020-03-16`, `2020-07-29` |
| 7 | `2019-06-19` | `2023-09-19` | absent from `2023-09-20` onward |

Version alignment check (official timeline vs observed local member sets):

| NWM version window (official) | official operational start | first issue date present in local archive for that version window | observed medium-range member behavior in local archive | alignment note |
|---|---|---|---|---|
| 1.2 (`2018-03-06` to `2019-06-18`) | `2018-03-06` | `2018-09-17` | single-member (`{1}`) through `2019-06-18` | local archive starts late in v1.2 window (195-day lag from official start) |
| 2.0 (`2019-06-19` to `2021-04-19`) | `2019-06-19` | `2019-06-19` | 7-member (`{1..7}`) from start, aside from one-day outages | transition aligns exactly at official v2.0 start |
| 2.1 (`2021-04-20` to `2023-09-19`) | `2021-04-20` | `2021-04-20` | remains 7-member (`{1..7}`) | no member-count structural change at 2.1 boundary in local archive |
| 3.0 (`2023-09-20` onward) | `2023-09-20` | `2023-09-20` | persistent 6-member (`{1..6}`), member 7 ends on `2023-09-19` | transition aligns exactly at official v3.0 start |

Retrospective coverage-end vs forecast operational-start gap check:

| NWM version | retrospective coverage end (Section 3.2) | forecast operational start (Section 3.1) | gap indication |
|---|---|---|---|
| 1.2 | `2017-12` | `2018-03-06` | month-level gap (~3 months) |
| 2.0 | `2018-12` | `2019-06-19` | month-level gap (~6 months) |
| 2.1 | `2020-12` | `2021-04-20` | month-level gap (~4 months) |
| 3.0 | `2023-01` | `2023-09-20` | month-level gap (~8 months) |

Interpretation and limits:

1. This local archive evidence supports the hypothesis that operational forecast member structure changes are strongly version-linked for v2.0 and v3.0 transitions.
2. This local archive evidence also supports the hypothesis that retrospective coverage windows do not necessarily end exactly at operational forecast start dates for the same version label.
3. Exact retrospective **release** dates remain unresolved in authoritative public metadata (`not explicitly listed` in reviewed sources), so release-lag quantification is still a tracked open item.
4. One-day member-set drops are treated as outage/partial-ingestion anomalies unless corroborated by official version notices.

### 3.4.2 AWS Marketplace Metadata Check for Retrospective Release Timing

Source:

- `NWM-URL-07`: `https://aws.amazon.com/marketplace/pp/prodview-g6lcchc7brshw`

Extracted catalog metadata (from listing JSON payload):

| field | value | interpretation |
|---|---|---|
| Data Exchange fulfillment option name | `DataSet-nwm-archive` | Catalog object for retrospective data access in Marketplace. |
| `creationDate` | `2022-06-17T19:20:55.006Z` | Catalog/fulfillment-option creation timestamp, not a per-version retrospective publication date. |
| Artifact buckets listed | `noaa-nwm-retrospective-3-0-pds`, `noaa-nwm-retrospective-2-1-pds`, `noaa-nwm-retrospective-2-1-zarr-pds`, `noaa-nwm-retro-v2-0-pds`, `noaa-nwm-retro-v2-zarr-pds`, `nwm-archive` | Confirms multi-version artifact presence under one listing. |

Interpretation constraint:

1. The Marketplace listing provides one catalog-level creation timestamp and versioned artifact references.
2. It does not expose explicit per-version retrospective release timestamps for v1.2/v2.0/v2.1/v3.0 in the reviewed metadata payload.
3. Therefore, retrospective release-to-forecast-transition lag cannot be fully resolved from Marketplace metadata alone.

Retrospective version coverage windows (from listing text) vs forecast member transitions:

| version | retrospective coverage end (listing text) | forecast-member structural transition linked to operational version boundary | operational boundary date | coverage-end to boundary lag (approx) |
|---|---|---|---|---|
| 2.0 | `2018-12` | `1 -> 7` members | `2019-06-19` | ~6 months |
| 2.1 | `2020-12` | no structural member-count change observed | `2021-04-20` | ~4 months |
| 3.0 | `2023-01` | `7 -> 6` members | `2023-09-20` | ~8 months |

Interpretation:

1. Available evidence indicates that retrospective coverage windows and forecast-member structural transitions are not synchronized on the same dates.
2. Forecast-member transitions align with operational version boundaries (especially v2.0 and v3.0), while retrospective coverage windows end earlier.
3. Exact per-version retrospective publication/release timestamps remain an open evidence gap.

### 3.5 GloFAS Operational Version Timeline

| glofas_version | released_in_production | evidence_source_id | notes |
|---|---|---|---|
| 4.4 | 2025-09-10 | `GLOFAS-URL-10` | Listed as latest operational release in versioning system. |
| 4.3 | 2025-06-04 | `GLOFAS-URL-10` | Versioning table entry. |
| 4.2 | 2024-11-12 | `GLOFAS-URL-10` | Versioning table entry. |
| 4.1 | 2024-02-28 | `GLOFAS-URL-10` | Versioning table entry. |
| 4.0 | 2023-07-26 | `GLOFAS-URL-10` | Versioning table entry; v4.0 page also confirms operational date. |
| 3.5 | 2023-06-28 | `GLOFAS-URL-10` | Versioning table entry. |
| 3.4 | 2022-12-14 | `GLOFAS-URL-10` | Versioning table entry. |
| 3.3 | 2022-10-19 | `GLOFAS-URL-10` | Versioning table entry. |
| 3.2 | 2021-10-27 | `GLOFAS-URL-10` | Versioning table entry. |
| 3.1 | 2021-05-26 | `GLOFAS-URL-10` | Versioning table entry. |
| 2.2 | 2020-12-09 (12UTC) | `GLOFAS-URL-10` | Versioning table entry. |
| 2.1 | 2019-11-05 (12UTC) | `GLOFAS-URL-10` | Versioning table entry; note shows later decommissioning. |
| 2.0 | 2018-11-14 (12UTC) | `GLOFAS-URL-10` | Versioning table entry. |
| 1.0 | 2018-04-23 (12UTC) | `GLOFAS-URL-10` | Versioning table entry. |

### 3.5.1 GloFAS Versioning-System Cross-Check (Internal Release, Test Window, Lifecycle Notes)

| glofas_version | glofas_internal_number | released_in_test | released_in_production | lifecycle_or_footnote_note | evidence_source_id |
|---|---|---|---|---|---|
| 4.4 | 005 | n/a | 2025-09-10 | Listed as latest operational release. | `GLOFAS-URL-10` |
| 4.3 | 005 | n/a | 2025-06-04 | Same internal number family as v4.0-v4.4. | `GLOFAS-URL-10`, `GLOFAS-URL-13` |
| 4.2 | 005 | n/a | 2024-11-12 | Same internal number family as v4.0-v4.4. | `GLOFAS-URL-10`, `GLOFAS-URL-14` |
| 4.1 | 005 | n/a | 2024-02-28 | Same internal number family as v4.0-v4.4. | `GLOFAS-URL-10`, `GLOFAS-URL-15` |
| 4.0 | 005 | 2023-07-05 | 2023-07-26 | Major v4 production cutover. | `GLOFAS-URL-10`, `GLOFAS-URL-12` |
| 3.5 | 004 | 2023-06-05 | 2023-06-28 | Late v3 production update before v4.0 cutover. | `GLOFAS-URL-10`, `GLOFAS-URL-16` |
| 3.4 | 004 | 2022-11-11 | 2022-12-14 | v3 generation release. | `GLOFAS-URL-10` |
| 3.3 | 004 | 2022-10-12 | 2022-10-19 | v3 generation release. | `GLOFAS-URL-10` |
| 3.2 | 004 | 2021-10-21 | 2021-10-27 | v3 generation release. | `GLOFAS-URL-10` |
| 3.1 | 004 | 2021-02-14 | 2021-05-26 | v3 generation release. | `GLOFAS-URL-10`, `GLOFAS-URL-17` |
| 2.2 | 003** | 2020-11-01 | 2020-12-09 (12UTC) | Footnote states no modelling change vs v2.1; internal number remains 003. | `GLOFAS-URL-10`, `GLOFAS-URL-18` |
| 2.1 | 003*** | 2019-10-08 | 2019-11-05 (12UTC) | Footnote states v2.1 decommissioned from mid-September 2022; previously generated data remain available in CDS. | `GLOFAS-URL-10`, `GLOFAS-URL-19` |
| 2.0 | 002 | 2018-10-14 | 2018-11-14 (12UTC) | v2.0 major release line. | `GLOFAS-URL-10`, `GLOFAS-URL-20` |
| 1.0 | 001 | 2018-03-23 | 2018-04-23 (12UTC) | First version listed in current official versioning table. | `GLOFAS-URL-10`, `GLOFAS-URL-21` |

Interpretation:

1. `GLOFAS-URL-10` is treated as the authoritative chronology for production timeline and internal-number transitions.
2. Version pages are used for product-impact context (historical/reanalysis/reforecast changes), not as primary chronology anchors when date fields differ.
3. The v2.1/v2.2 footnotes are operationally relevant for lifecycle interpretation (decommission and data-equivalence note).

### 3.6 GloFAS Historical, Forecast, and Reforecast Metadata

| product_type | current_system_version_note | product_release_or_publication_date | coverage_start | coverage_end | format | key_access_locations | notes |
|---|---|---|---|---|---|---|---|
| historical | operational version listed as v4.0 (released 2023-07-26) | publication date: 2019-11-05 | 1979-01-01 | near real time | GRIB2, NetCDF-4 | `GLOFAS-URL-01` | Daily data; global except Antarctica; legacy versions also available. |
| forecast | current version listed as v4.0 (released 2023-07-26) | publication date: 2020-05-19 | 2019-11-05 (river discharge) | near real time | GRIB2, NetCDF-4 | `GLOFAS-URL-03` | Soil wetness/snow variables indicate operational coverage from 2024-02-28 onward. |
| reforecast | current version listed as v4.0 (released 2023-07-26) | publication date: 2020-12-09 | first reference date: 2023-03-27 | near real time generation (historical years 2003-2022 per reference date) | GRIB2 (dataset page), GRIB2/NetCDF options in retrieve API metadata | `GLOFAS-URL-05`, `GLOFAS-URL-23` | 11-member ECMWF-ENS reforecasts, Monday/Thursday reference dates, 46-day lead time, weekly update. |

Coverage-note:

1. Table 3.6 uses product-page framing text (`near real time`, `first reference date`, etc.).
2. Exact collection-level start/end timestamps and current update state are tracked in Section 3.7.1.

### 3.6.1 GloFAS Version-Linked Product Notes (Reanalysis, Forecast, Reforecast)

| glofas_version | product dimension | extracted note | evidence_source_id |
|---|---|---|---|
| 4.0 | historical/reanalysis | Major version change is tied to an update of historical hydrological reanalysis, with large modelling-result impacts. | `GLOFAS-URL-12` |
| 4.2 | forecast-skill/reforecast linkage | Medium-range forecast skill layer was recalculated with expanded full reforecast coverage (not only initial months used in early v4.0 release period). | `GLOFAS-URL-14` |
| 3.5 | reforecast policy | No new 48r1-specific hydrological reforecast dataset in v3.5; reforecast handling is explicitly documented in release notes. | `GLOFAS-URL-16` |
| 2.1 | reanalysis and reforecast baseline | Upgrade includes ERA5-based hydrological reanalysis and new river-discharge reforecast sets (30-day and seasonal). | `GLOFAS-URL-19` |
| 1.0 | reforecast baseline design | Reforecasts documented as twice-weekly, 11-member, multi-year historical ensemble runs in v1.0-era release material. | `GLOFAS-URL-21` |

Operational consistency note:

1. EWDS dataset overview pages currently expose v4.0 labels for historical/forecast/reforecast metadata tables, while the versioning-system chronology reports newer operational releases up to v4.4.
2. This document keeps both views explicit: endpoint/product labels from EWDS pages and operational chronology from the official versioning table.

### 3.7 GloFAS EWDS API Version Options by Product (Retrieve Metadata)

| product_id | system_version options in retrieve API | product_type options | data_format options | key timing/options notes | evidence_source_id |
|---|---|---|---|---|---|
| `cems-glofas-historical` | `version_2_1`, `version_3_1`, `version_4_0` | `consolidated`, `intermediate` | `grib2`, `netcdf` | No leadtime field (historical product). | `GLOFAS-URL-22` |
| `cems-glofas-forecast` | `operational`, `version_2_1`, `version_3_1` | `control_forecast`, `ensemble_perturbed_forecasts` | `grib2`, `netcdf` | Leadtime enum spans 24 to 720h (24h step). | `GLOFAS-URL-09` |
| `cems-glofas-reforecast` | `version_2_2`, `version_3_1`, `version_4_0` | `control_reforecast`, `ensemble_perturbed_reforecast` | `grib2`, `netcdf` | Leadtime enum spans 24 to 1104h (24h step). | `GLOFAS-URL-23` |

Format-note:

1. For reforecast, the dataset overview page emphasizes GRIB2, while retrieve-process metadata currently lists both `grib2` and `netcdf` options.
2. This audit keeps both fields explicit and does not infer final per-request availability beyond endpoint metadata.

### 3.7.1 GloFAS Collection-Level Coverage and Active Notices (EWDS Catalogue API)

| collection_id | published | updated | temporal_extent_start | temporal_extent_end | active_messages_snapshot | evidence_source_id |
|---|---|---|---|---|---|---|
| `cems-glofas-historical` | `2019-11-05T00:00:00Z` | `2026-02-15T00:00:00Z` | `1979-01-01T00:00:00Z` | `2026-02-13T00:00:00Z` | time-critical advisory + known-issues note | `GLOFAS-URL-24`, `GLOFAS-URL-27` |
| `cems-glofas-forecast` | `2020-05-19T00:00:00Z` | `2026-02-15T00:00:00Z` | `2019-11-05T00:00:00Z` | `2026-02-15T00:00:00Z` | time-critical advisory + known-issues note | `GLOFAS-URL-25`, `GLOFAS-URL-28` |
| `cems-glofas-reforecast` | `2020-12-09T00:00:00Z` | `2025-04-02T00:00:00Z` | `1999-01-03T00:00:00Z` | `2023-11-25T00:00:00Z` | includes `2024-11-11` temporary freeze notice for medium-range reforecasts from EWDS (support ticket path provided) | `GLOFAS-URL-26`, `GLOFAS-URL-29` |

Coverage-interpretation constraint:

1. These are collection-level temporal extents and message feeds, not explicit per-`system_version` coverage windows.
2. Retrieve schemas publish version enums (`2.1`, `3.1`, `4.0` family options by product) but do not publish version-specific date windows in the same endpoint metadata.
3. Therefore, per-version historical/reforecast date coverage (for `version_2_1` vs `version_3_1` vs `version_4_0`) remains unresolved from metadata alone and must be treated as an explicit follow-up item.

### 3.7.2 GloFAS Selector Semantics (for `river_discharge_in_the_last_24_hours`)

| selector | where used | meaning in this audit | evidence |
|---|---|---|---|
| `system_version` | historical/forecast/reforecast retrieve APIs | Version label for the GloFAS system generation exposed by the endpoint (exact option sets differ by product). | `GLOFAS-URL-22`, `GLOFAS-URL-09`, `GLOFAS-URL-23` |
| `hydrological_model` (`htessel_lisflood`, `lisflood`) | historical/forecast/reforecast retrieve APIs | Model-chain selector. `lisflood` aligns with full LISFLOOD-era configuration; `htessel_lisflood` corresponds to the coupled HTESSEL+LISFLOOD lineage referenced in legacy-to-v3.1 transition notes. | `GLOFAS-URL-17`, `GLOFAS-URL-04`, `GLOFAS-URL-22`, `GLOFAS-URL-09`, `GLOFAS-URL-23` |
| `product_type` (historical: `consolidated` / `intermediate`) | historical dataset/API | `intermediate` uses ERA5T near-real-time forcing (daily updates); `consolidated` uses consolidated ERA5 forcing (monthly updates). | `GLOFAS-URL-01`, `GLOFAS-URL-22` |
| `product_type` (forecast: `control_forecast` / `ensemble_perturbed_forecasts`) | forecast dataset/API | Control member vs perturbed ensemble members for operational forecast runs. | `GLOFAS-URL-03`, `GLOFAS-URL-09` |
| `product_type` (reforecast: `control_reforecast` / `ensemble_perturbed_reforecast`) | reforecast dataset/API | Control member vs perturbed ensemble members for hindcast/reforecast runs. | `GLOFAS-URL-05`, `GLOFAS-URL-23` |
| `variable=river_discharge_in_the_last_24_hours` | historical/forecast/reforecast APIs | Daily river-discharge variable available across all three product families and used as the primary bias-transfer target in this project. | `GLOFAS-URL-22`, `GLOFAS-URL-09`, `GLOFAS-URL-23` |

Project-use interpretation:

1. `cems-glofas-historical` is the historical/reanalysis side used to learn retrospective bias structure.
2. `cems-glofas-forecast` is the real-time forecast side to be corrected.
3. `cems-glofas-reforecast` is the hindcast/reforecast side for skill calibration, post-processing, and compatibility checks when available.

### 3.7.3 Supporting Legacy Reanalysis Anchors (JRC Data Catalogue)

| source_product | version label in source | temporal coverage in source metadata | key notes | evidence_source_id |
|---|---|---|---|---|
| GloFAS streamflow reanalysis | `v3.0` | `1980-01-01` to `2018-12-31` | Legacy LISFLOOD/ERA5 reanalysis archive in JRC catalogue; large bulk archive link available. | `GLOFAS-URL-30` |
| GloFAS hydrological reanalysis | `v4.0` | `1980-01-01` to `2022-07-31` | JRC dataset description explicitly frames v4.0 as an update compared to v3.1; access points are bulk archive style. | `GLOFAS-URL-31` |

Interpretation constraints:

1. These JRC entries are used as legacy coverage anchors and are treated as supporting sources in this audit.
2. EWDS retrieve/catalogue endpoints remain the primary authoritative source for current operational access and version selectors.
3. Label mapping between JRC archive labels (`v3.0`, `v4.0`) and EWDS historical retrieve options is now recorded explicitly in Table 3.7.3.1 (with confidence/status tags).
4. Lineage convention in this document: JRC `v3.0` and EWDS `version_3_1` are treated as related labels for chronology/selector mapping only; they are not assumed to be numerically equivalent products.

### 3.7.3.1 JRC-to-EWDS Historical Label Mapping (Explicit)

| JRC legacy label | EWDS historical selector candidate | mapping status | rationale | evidence_source_id |
|---|---|---|---|---|
| `v4.0` | `version_4_0` | high-confidence direct label match | Same numeric label (`4.0`) and same ERA5-forced reanalysis generation family in reviewed metadata. | `GLOFAS-URL-31`, `GLOFAS-URL-22` |
| `v3.0` | `version_3_1` | lineage hypothesis only (not value-equivalent) | Labels are related by chronology/selector context, but direct point-series validation in this project shows clear numeric differences between legacy `v3.0` and historical `version_3_1` at the target location. | `GLOFAS-URL-30`, `GLOFAS-URL-22`, `GLOFAS-URL-10`, `GLOFAS-LOCAL-17` |
| `v3.1` (standalone JRC landing page) | `version_3_1` | not found in reviewed JRC catalogue pages | Reviewed JRC catalogue search and collection pages did not provide a dedicated `v3.1` reanalysis landing page. | `GLOFAS-URL-33` |

Operational rule from this mapping:

1. Treat `v4.0 <-> version_4_0` as a valid label linkage.
2. For this project, treat `v3.0 <-> version_3_1` as a lineage/selector mapping only; keep labels explicit as `legacy v3.0` vs `historical v3.1` in all value-comparison tables and figures.
3. Do not assume equivalence for bias transfer without explicit local or authoritative numerical validation.
4. If future official lineage documentation disagrees, update this mapping and rerun compatibility checks.

### 3.7.4 GloFAS Coverage-Window Investigation Placeholders (Current Scope)

The following checklist is now closed for the current audit scope.
For priority work (`version_3_1` and `version_4_0`, historical + reforecast), see focused follow-up results in Section 3.7.9.

- [x] Derive `historical` per-`system_version` date coverage windows (`2.1`, `3.1`, `4.0`) using lightweight API validation/probing (bounded-probe evidence).
- [x] Derive `reforecast` per-`system_version` date coverage windows (`2.2`, `3.1`, `4.0`) using lightweight API validation/probing (bounded-probe evidence).
- [x] Confirm whether a dedicated public landing page exists for a standalone `v3.1` reanalysis archive and record it if found (`not found in reviewed official JRC catalogue pages`).
- [x] Reconcile JRC legacy archive labels with EWDS `system_version` lineage in one explicit mapping table (see Section 3.7.3.1).
- [x] Re-check reforecast freeze status and convert this into a recurring policy item (snapshot verified on `2026-02-16`: freeze message dated `2024-11-11` remains present in EWDS message feed).

Current partial status from the earlier full scan (`scan_20260216T015036Z`):

1. Historical anchors were found for selected combinations only (not all version/model/product combinations).
2. Targeted reruns confirmed reforecast point anchors for `version_4_0 + lisflood` at `2021-01-04` for both control and ensemble products (`refine_20260216T031255Z`, `refine_20260216T031324Z`).
3. No-anchor outcomes remain for many combinations and must be treated as unresolved availability under bounded probing, not as definitive absence.
4. For `forecast + operational + lisflood`, manual boundary probes showed mixed success/timeout behavior near tested windows; these windows are treated as bounded evidence with timeout caveats, not exhaustive limits.

### 3.7.5 Minimal EWDS Extraction Patterns (Standardized Across Products)

The following request-field patterns are the baseline for lightweight coverage probing:

| product | required date fields | version selector field | typical product_type values | lead-time field | notes |
|---|---|---|---|---|---|
| `cems-glofas-historical` | `hyear`, `hmonth`, `hday` | `system_version` (`version_2_1`, `version_3_1`, `version_4_0`) | `consolidated`, `intermediate` | not used | Historical/reanalysis product; no lead-time selector. |
| `cems-glofas-forecast` | `year`, `month`, `day` | `system_version` (`operational`, `version_2_1`, `version_3_1`) | `control_forecast`, `ensemble_perturbed_forecasts` | `leadtime_hour` (24 to 720) | Operational forecast product. |
| `cems-glofas-reforecast` | `hyear`, `hmonth`, `hday` | `system_version` (`version_2_2`, `version_3_1`, `version_4_0`) | `control_reforecast`, `ensemble_perturbed_reforecast` | `leadtime_hour` (24 to 1104) | Reforecast availability is date/version dependent and must be probed explicitly. |

Shared lightweight-probe settings used in this project:

1. Variable: `river_discharge_in_the_last_24_hours`.
2. Spatial request: small bounding box around target point.
3. One date and one lead-time per test request.
4. `download_format=zip` and `data_format=grib2`.

### 3.7.6 Lightweight Probe Results (No Bulk Download)

Recorded probe runs from `scripts/forecats_probe_glofas_coverage.py`:

| run_id | dataset | key request tuple | outcome | evidence path | interpretation |
|---|---|---|---|---|---|
| `probe_20260216T011130Z` | historical + forecast + reforecast | historical (`version_4_0`, `2023-03-27`), forecast (`operational`, `2023-03-27`, `leadtime=24`), reforecast (`version_4_0`, `2021-01-04`, `leadtime=24`) | success (all three cases) | `repro/glofas_probe_runs/probe_20260216T011130Z/manifests/probe_manifest.csv` | Default lightweight tri-product smoke test is operational. |
| `probe_20260216T010601Z` | `cems-glofas-reforecast` | `version_4_0`, `lisflood`, `control_reforecast`, `2023-03-27`, `leadtime=24` | error (`400 invalid request`) | `repro/glofas_probe_runs/probe_20260216T010601Z/manifests/probe_manifest.csv` | Valid selector enums do not imply every date-version combination is available. |

Probe-scope note:

1. These runs validate API behavior and combination validity only.
2. They do not establish full per-version coverage windows.
3. Coverage-window derivation remains an open task using repeated lightweight boundary probes.

### 3.7.7 Legacy Forecast Archive Version Audit (Project-Local Evidence)

Audit source:

1. Local request manifests under `data/glofas_operational_medium_range/grib/issue_date=*/`.
2. Automated summary output from `scripts/forecats_scan_glofas_coverage.py`:
   - `repro/glofas_coverage_scan_runs/scan_20260216T012831Z/reports/local_forecast_archive_summary.json`

Extracted findings:

| field | value | interpretation |
|---|---|---|
| request manifest count | `1176` | Large local forecast archive with request-level provenance. |
| issue date range | `2019-11-05` to `2023-01-31` | Matches project forecast period used in prior repro runs. |
| `system_version` counts | `operational: 1176` | Prior local forecast archive is operational-only (no `version_3_1` or `version_2_1` request manifests found in this archive). |
| hydrological model counts | `htessel_lisflood: 561`, `lisflood: 615` | Both models were used inside operational chronology. |
| model transition point | `2021-05-26` (`htessel_lisflood -> lisflood`) | Transition aligns with documented model selector change timing. |

### 3.7.8 Parallel Coverage Scanner (Implemented Workflow)

Implemented script:

1. `scripts/forecats_scan_glofas_coverage.py`

Capabilities:

1. Parallel lane execution for `historical`, `reforecast`, and `forecast`.
2. Priority-order execution (`P1 -> P2 -> P3`) aligned to project policy:
   - `P1`: `3.1` family first.
   - `P2`: `4.0` + `operational`.
   - `P3`: `2.1`/`2.2` family.
3. Boundary search outputs per combination:
   - anchor success status
   - earliest and latest successful probe dates
   - confidence and full attempt log.
4. Full reproducibility artifacts:
   - `manifests/planned_combos.json`
   - `manifests/attempts.csv`
   - `manifests/coverage_summary.csv`
   - `reports/priority_lane_summary.json`
   - `reports/local_forecast_archive_summary.json`

Completed full-scope run:

1. Run directory: `repro/glofas_coverage_scan_runs/scan_20260216T015036Z`
2. Command used:
   - `python3 scripts/forecats_scan_glofas_coverage.py --run --verbose --max-workers 3 --max-attempts-per-combo 8 --request-timeout-seconds 60 --cdsapi-retry-max 1 --cdsapi-sleep-max 5`
3. Outcome summary:
   - `36` combinations tested.
   - `10` combinations with anchor found.
   - `26` combinations with no anchor found.
   - `274` total attempts (`41` success, `233` error).
   - Lightweight-download footprint: `41` zip files, `15,751` bytes total (no bulk transfer).
   - Error-class distribution: `214 invalid_request`, `19 timeout`.

Anchor-found combinations from `manifests/coverage_summary.csv`:

| lane | system_version | hydrological_model | product_type | earliest_success_date | latest_success_date | confidence | notes |
|---|---|---|---|---|---|---|---|
| historical | `version_3_1` | `lisflood` | `consolidated` | `2021-02-08` | `2021-06-15` | `high` | attempt budget exhausted |
| historical | `version_3_1` | `lisflood` | `intermediate` | `2021-06-04` | `2021-06-15` | `high` | attempt budget exhausted |
| historical | `version_4_0` | `lisflood` | `consolidated` | `1979-09-28` | `1979-09-28` | `high` | attempt budget exhausted |
| historical | `version_2_1` | `htessel_lisflood` | `consolidated` | `2019-10-30` | `2019-11-06` | `high` | attempt budget exhausted |
| historical | `version_2_1` | `htessel_lisflood` | `intermediate` | `2019-12-24` | `2020-01-16` | `high` | attempt budget exhausted |
| forecast | `operational` | `htessel_lisflood` | `control_forecast` | `2019-11-05` | `2019-12-06` | `medium` | left boundary not bracketed; attempt budget exhausted |
| forecast | `operational` | `htessel_lisflood` | `ensemble_perturbed_forecasts` | `2019-11-05` | `2019-12-06` | `medium` | left boundary not bracketed; attempt budget exhausted |
| forecast | `operational` | `lisflood` | `control_forecast` | `2023-03-27` | `2023-03-27` | `high` | point anchor under bounded probing |
| reforecast | `version_4_0` | `lisflood` | `control_reforecast` | `2021-01-04` | `2021-01-04` | `high` | point anchor under bounded probing |
| reforecast | `version_4_0` | `lisflood` | `ensemble_perturbed_reforecast` | `2021-01-04` | `2021-01-04` | `high` | point anchor under bounded probing |

Interpretation constraint:

1. These are bounded lightweight-probe results; they provide reproducible evidence of tested valid points/ranges, not final exhaustive coverage windows.
2. No-anchor results should be interpreted as unresolved for those tested settings until additional targeted probing is completed.

Boundary-focused refinement pass:

1. Script: `scripts/forecats_refine_glofas_ranges.py`
2. Run directory: `repro/glofas_coverage_scan_runs/refine_20260216T025403Z`
3. Command used:
   - `python3 scripts/forecats_refine_glofas_ranges.py --run --verbose --max-workers 2 --max-attempts-per-combo 8 --request-timeout-seconds 60 --cdsapi-retry-max 1 --cdsapi-sleep-max 5`
4. Outcome summary:
   - `36` combinations evaluated.
   - `9` combinations `found`, `27` combinations `not_found`.
   - `132` attempts (`29` success, `102` error/baseline_error).
   - No-anchor reason split:
     - `25` combinations with repeated `invalid_request` confirmation.
     - `2` combinations with timeout-present unresolved status (`version_4_0 + lisflood` reforecast control/ensemble).

Targeted reruns and boundary probes after the first refinement:

1. `repro/glofas_coverage_scan_runs/refine_20260216T031255Z`: reforecast `version_4_0 + lisflood + control_reforecast` reconfirmed as found at `2021-01-04` (bracketed point-anchor).
2. `repro/glofas_coverage_scan_runs/refine_20260216T031324Z`: reforecast `version_4_0 + lisflood + ensemble_perturbed_reforecast` reconfirmed as found at `2021-01-04` (bracketed point-anchor).
3. `repro/glofas_coverage_scan_runs/refine_20260216T033350Z`: forecast `operational + lisflood + control_forecast` refined to `2023-03-26` through `2023-03-28` (medium confidence).
4. `repro/glofas_coverage_scan_runs/manual_boundary_20260216T194500Z/manual_boundary_results.csv`: timeout-controlled manual probes for `operational + lisflood` forecast products:
   - `ensemble_perturbed_forecasts`: `2021-05-25` invalid, `2021-05-26..2021-05-29` ok, `2021-05-30` timeout.
   - `control_forecast`: mixed timeout behavior in this check (`2023-03-25..2023-03-28` timeout, `2023-03-29` ok).

Consolidated matrix:

1. Artifact: `repro/glofas_coverage_scan_runs/consolidated_20260216T195500Z/refined_ranges_consolidated.csv`
2. Summary: `36` combinations, `11` found, `25` not_found.
3. No-anchor pattern in the consolidated matrix: all `25` not-found combinations show repeated `invalid_request` outcomes under bounded probing. Timeout-driven ambiguity remains only in selected manual boundary checks for `operational + lisflood` forecast windows.

Consolidated anchor-found combinations:

| lane | system_version | hydrological_model | product_type | consolidated_earliest | consolidated_latest | boundary_confidence | notes |
|---|---|---|---|---|---|---|---|
| historical | `version_3_1` | `lisflood` | `consolidated` | `2021-02-07` | `2021-06-16` | `medium` | neighboring-success boundaries under bounded probing |
| historical | `version_3_1` | `lisflood` | `intermediate` | `2021-06-03` | `2021-06-16` | `medium` | neighboring-success boundaries under bounded probing |
| historical | `version_4_0` | `lisflood` | `consolidated` | `1979-09-27` | `1979-09-27` | `high` | bracketed point-anchor |
| historical | `version_2_1` | `htessel_lisflood` | `consolidated` | `2019-10-29` | `2019-11-06` | `high` | bracketed |
| historical | `version_2_1` | `htessel_lisflood` | `intermediate` | `2019-12-24` | `2020-01-16` | `high` | bracketed |
| forecast | `operational` | `htessel_lisflood` | `control_forecast` | `2019-11-05` | `2019-12-07` | `medium` | domain-start edge + right-neighbor success |
| forecast | `operational` | `htessel_lisflood` | `ensemble_perturbed_forecasts` | `2019-11-05` | `2019-12-07` | `medium` | domain-start edge + right-neighbor success |
| forecast | `operational` | `lisflood` | `control_forecast` | `2023-03-26` | `2023-03-28` | `medium` | targeted rerun window (`refine_20260216T033350Z`) |
| forecast | `operational` | `lisflood` | `ensemble_perturbed_forecasts` | `2021-05-26` | `2021-05-29` | `medium` | manual boundary override; upper side timeout at `2021-05-30` |
| reforecast | `version_4_0` | `lisflood` | `control_reforecast` | `2021-01-04` | `2021-01-04` | `high` | targeted rerun restored point-anchor |
| reforecast | `version_4_0` | `lisflood` | `ensemble_perturbed_reforecast` | `2021-01-04` | `2021-01-04` | `high` | targeted rerun restored point-anchor |

### 3.7.9 Focused Follow-up (Priority Scope: `version_3_1` and `version_4_0`, Historical + Reforecast)

Scope applied in this follow-up:

1. Priorities only: `P1` and `P2`.
2. Lanes only: `historical` and `reforecast`.
3. Versions only: `version_3_1` and `version_4_0`.
4. `2.1/2.2` families intentionally excluded from this focused pass.

Focused scan run:

1. Run directory: `repro/glofas_coverage_scan_runs/scan_20260216T065850Z`
2. Command profile: same scanner, focused filters (`--priorities P1,P2 --lanes historical,reforecast`) with bounded lightweight requests.
3. Summary: `16` combinations, `7` anchors found, `296` attempts.

Focused aggressive refinement run:

1. Run directory: `repro/glofas_coverage_scan_runs/refine_20260216T072601Z`
2. Key setting: `--aggressive-boundary-expansion` (full-range doubling search in refinement).
3. Summary: `16` combinations, `7` found, `9` not_found, `150` attempts.

Focused consolidated artifact:

1. Directory: `repro/glofas_coverage_scan_runs/focused_20260216T075254Z`
2. Main CSV: `focused_refined_ranges.csv`
3. Summary JSON: `focused_summary.json`
4. Boundary-validation probe run for `1978-12-31` vs `1979-01-01`: `repro/glofas_probe_runs/probe_20260216T075054Z`

Focused results (version/model/product combinations with anchors):

| lane | system_version | hydrological_model | product_type | refined_earliest | refined_latest | boundary_confidence | focused interpretation |
|---|---|---|---|---|---|---|---|
| historical | `version_3_1` | `lisflood` | `consolidated` | `1979-01-01` | `2024-06-30` | `medium` in raw refine (`at_domain_start`), upgraded to `high` with external lower-bound probe (`1978-12-31` invalid, `1979-01-01` valid) | Broad historical window confirmed in focused probing. |
| historical | `version_3_1` | `lisflood` | `intermediate` | `2021-01-01` | `2024-09-23` | `high` | Intermediate window is narrower than consolidated and starts in 2021 in this evidence set. |
| reforecast | `version_3_1` | `lisflood` | `control_reforecast` | `2002-01-03` | `2002-07-11` | `high` | Non-point reforecast window confirmed for control product. |
| reforecast | `version_3_1` | `lisflood` | `ensemble_perturbed_reforecast` | `2000-02-28` | `2000-02-28` | `high` | Point-anchor only in focused bounded probes. |
| historical | `version_4_0` | `lisflood` | `consolidated` | `1979-01-01` | `2025-11-30` | `medium` in raw refine (`at_domain_start`), upgraded to `high` with external lower-bound probe (`1978-12-31` invalid, `1979-01-01` valid) | Broad historical window confirmed for v4.0 consolidated. |
| reforecast | `version_4_0` | `lisflood` | `control_reforecast` | `2021-01-04` | `2021-01-04` | `high` | Point-anchor evidence remains stable. |
| reforecast | `version_4_0` | `lisflood` | `ensemble_perturbed_reforecast` | `2021-01-04` | `2021-01-04` | `high` | Point-anchor evidence remains stable. |

Focused no-anchor pattern:

1. All `htessel_lisflood` combinations in this focused scope remained `not_found` with repeated `invalid_request`.
2. `version_4_0 + lisflood + intermediate` historical remained `not_found` with repeated `invalid_request`.
3. These outcomes are documented as unsupported combinations under tested bounded requests, not as universal product absence claims beyond tested settings.

### 3.7.10 Targeted Fixed-Date Boundary Checks (Timeout-Controlled)

Purpose of this step:

1. Run explicit boundary-date probes per combination (instead of broad sweeps) to isolate:
   - hard-invalid requests (`invalid_request`), and
   - unresolved cases (`timeout`) that require slower retries.
2. Keep the requests metadata-light (single day, small area, low-volume payload).

Implementation:

1. Script added: `scripts/forecats_check_glofas_boundaries.py`
2. Completed run: `repro/glofas_probe_runs/boundary_check_20260216T223610Z`
3. Run profile:
   - `32` fixed cases
   - parallel workers: `4`
   - per-request hard timeout: `45s`

Outcome summary (`boundary_check_20260216T223610Z`):

1. `32/32` requests returned error-classified outcomes:
   - `16` as `invalid_request`
   - `16` as `timeout`
2. Structural `invalid_request` confirmations (useful for compatibility filtering):
   - Historical:
     - `version_3_1 + lisflood + consolidated`: `1978-12-31` invalid, `2024-07-01` invalid
     - `version_3_1 + lisflood + intermediate`: `2020-12-31` invalid, `2024-09-24` invalid
     - `version_4_0 + lisflood + consolidated`: `1978-12-31` invalid, `2025-12-01` invalid
     - `version_4_0 + lisflood + intermediate`: `2023-03-27` invalid
     - `version_3_1/version_4_0 + htessel_lisflood + consolidated`: invalid in tested dates
   - Reforecast:
     - `version_3_1 + lisflood + control_reforecast`: `2002-01-01` invalid and `2021-01-04` invalid
     - `version_4_0 + lisflood + control_reforecast`: `2021-01-03`, `2021-01-07`, `2022-01-03` invalid
     - `version_3_1/version_4_0 + htessel_lisflood + control_reforecast`: invalid in tested dates
3. Timeout-labeled requests are treated as unresolved (not interpreted as invalid) in this step.

Interpretation policy for this run:

1. Use `invalid_request` outcomes as hard exclusion evidence for the tested request tuples.
2. Do not downgrade previously confirmed anchor windows from Section 3.7.9 based on timeout-only outcomes in this run.
3. For positive-window confirmation, keep Section 3.7.9 (`GLOFAS-LOCAL-11`, `GLOFAS-LOCAL-12`) as the primary evidence base.

Correction follow-up for `version_2_1` historical:

1. Run: `repro/glofas_probe_runs/boundary_check_20260216T234434Z` (same fixed-date checker, filtered to `hist_v21_*` cases).
2. Confirmed valid tuple:
   - `system_version=version_2_1`
   - `hydrological_model=htessel_lisflood`
   - `product_type=consolidated`
3. Confirmed consolidated boundary behavior (for the tested request tuple):
   - `1978-12-31`: `invalid_request`
   - `1979-01-01`: `ok`
   - `1991-07-13`: `ok`
   - `2022-07-31`: `ok`
   - `2022-08-01`: `invalid_request`
4. Confirmed intermediate anchors (same selector/model):
   - `2019-09-30`: `ok`
   - `2019-10-01`: `ok`
   - `2022-09-01`: `ok`
   - `2022-09-13`: `invalid_request`
5. Interpretation update:
   - Earlier “v2.1 historical unavailable” inference is rejected.
   - Root cause was probe selection/coverage configuration in earlier runs, not product absence in EWDS.

### 3.7.11 Legacy-JRC vs EWDS Historical Value-Parity Status

Objective:

1. Check whether point/day output values are numerically equivalent between:
   - legacy JRC reanalysis archives (`v3.0`, `v4.0`), and
   - EWDS historical product selectors (`version_3_1`, `version_4_0`) in overlap periods.

Current status:

1. Legacy `v3.0` direct archive download and point extraction are complete:
   - global file: `data/glofas_legacy_global/dis_1980_2018_v3_legacy.nc`
   - point series: `data/glofas_legacy_global/point_series/dis_1980_2018_v3_legacy_bigtrees.csv`
2. EWDS historical campaign point extraction is complete for:
   - `version_3_1` lisflood consolidated
   - `version_4_0` lisflood consolidated
   - `version_2_1` htessel_lisflood consolidated
3. Direct parity run (target location, overlap dates) between legacy `v3.0` and historical `version_3_1` is now executed.

Key findings (`legacy v3.0` vs historical `version_3_1`, Big Trees target point):

| comparison window | overlap rows | MAE | RMSE | Pearson corr | source |
|---|---|---|---|---|---|
| `1987-05-29` to `2019-01-01` | `11541` | `0.3073` | `0.6030` | `0.9515` | `GLOFAS-LOCAL-17` |
| `2012-01-01` to `2017-12-31` | `2192` | `0.3225` | `0.6696` | `0.9531` | `GLOFAS-LOCAL-18` |
| `2015-06-01` to `2017-12-31` | `945` | `0.4455` | `0.9178` | `0.9550` | `GLOFAS-LOCAL-19` |

2017 timing/mean-adjustment stress test (`legacy v3.0` shifted left by 1 day and mean-matched to historical `v3.1`):

1. Mean alignment is exact by construction after additive correction.
2. Even after this correction, the products are treated as distinct datasets (correction is diagnostic, not evidence of native equivalence).
3. Artifact: `repro/glofas_probe_runs/legacyv3_histv31_2017_shiftmean_20260217T195655Z` (`GLOFAS-LOCAL-20`).

Evidence-constrained conclusion:

1. Legacy `v3.0` and historical `version_3_1` are **not** treated as numerically equivalent products in this project.
2. Use explicit labels (`legacy v3.0`, `historical v3.1`) in all compatibility and plotting outputs.
3. `v3.0 <-> version_3_1` remains a lineage/selector mapping context only (not a value-equivalence rule).
4. `v4.0` legacy-versus-historical parity remains open pending local legacy `v4.0` file availability and equivalent point extraction.

Minimal dependency to close this gap:

1. Execute the same parity workflow for legacy `v4.0` once a local legacy `v4.0` point series is prepared.

## 4) Source Checklist (Metadata Documentation)

Track source review progress here. Use URL IDs from Section 7.

Status values:

1. `done`: metadata field extraction finished for current scope.
2. `in_progress`: source reviewed partially; at least one targeted field still unresolved.

| source_id | center | priority | fields_to_capture | status | last_checked_utc | notes |
|---|---|---|---|---|---|---|
| `NWM-URL-04` | NWM | high | operational forecast configuration details | `done` | `2026-02-15` | NWM configuration/cycle details extracted. |
| `NWM-URL-05` | NWM | high | retrospective versions, coverage, forcing notes | `done` | `2026-02-15` | Versioned retrospective windows and bucket map extracted. |
| `NWM-URL-15` | NWM | high | v3.0 operational effective date | `done` | `2026-02-15` | SCN23-76 confirms v3.0 effective date. |
| `NWM-URL-11/12/13/14/16` | NWM | high | historical operational version release dates | `done` | `2026-02-15` | TIN/SCNs used to reconstruct timeline 2016-2023. |
| `NWM-URL-17` | NWM | high | short-range archive scope and access endpoints | `done` | `2026-02-15` | Four-week rollover and bucket metadata extracted. |
| `NWM-URL-07` | NWM | high | Marketplace retrospective listing metadata (coverage text + catalog timing fields + artifact map) | `done` | `2026-02-15` | Verified listing text by version, oCONUS/forcing/format notes, and catalog `creationDate`; per-version release dates still not explicitly exposed. |
| `NWM-URL-20` | NWM | high | NWM release-notes change log signals for medium-range member structure by version | `done` | `2026-02-15` | Used to cross-check 2.0 (7-member introduction), 2.1 (1-7 continuity), and 3.0 (member-7 removal). |
| `NWM-URL-01` | NWM | medium | Planetary Computer metadata detail | `in_progress` | `2026-02-15` | Page is JS-heavy in this audit context; use supporting docs/notes where needed. |
| `NWM-LOCAL-01` | NWM | high | project-local point-extracted ensemble timeline (`results.pkl`) for member-set transition evidence | `done` | `2026-02-15` | Used for issue-date/member-set transition evidence and version-alignment cross-check in Section 3.4.1. |
| `GLOFAS-URL-01` | GloFAS | high | historical coverage, formats, release metadata | `done` | `2026-02-15` | Historical metadata extracted (coverage, format, DOI, dates). |
| `GLOFAS-URL-03` | GloFAS | high | forecast coverage, formats, release metadata | `done` | `2026-02-15` | Forecast metadata extracted (coverage, format, DOI, dates). |
| `GLOFAS-URL-05` | GloFAS | high | reforecast cadence, lead time, format, release metadata | `done` | `2026-02-15` | Reforecast metadata extracted (twice-weekly, 46-day, 11-member). |
| `GLOFAS-URL-10` | GloFAS | high | official version chronology and release dates | `done` | `2026-02-15` | Versioning system table extracted through v4.4. |
| `GLOFAS-URL-11` | GloFAS | high | latest operational release page content | `done` | `2026-02-15` | Cross-checks current operational release against versioning table. |
| `GLOFAS-URL-12` | GloFAS | medium | v4.0 release implementation details | `done` | `2026-02-15` | Used as supporting release-details reference for v4.0 era datasets. |
| `GLOFAS-URL-13/14/15/16/17/18/19/20/21` | GloFAS | high | per-version release details for reanalysis/forecast/reforecast behavior and lifecycle notes | `done` | `2026-02-15` | Used to extract product-impact notes across v1.0, v2.x, v3.x, and v4.x pages while chronology remains anchored to `GLOFAS-URL-10`. |
| `GLOFAS-URL-04` | GloFAS | medium | method chain notes (HTESSEL/LISFLOOD/IFS) | `done` | `2026-02-15` | Modelling-chain inputs and routing notes extracted. |
| `GLOFAS-URL-06/07/08/09/22/23` | GloFAS | medium | API auth and metadata endpoint behavior | `done` | `2026-02-15` | API endpoint and auth setup information captured, including per-product retrieve-process option metadata. |
| `GLOFAS-URL-24/25/26` | GloFAS | high | collection-level `published`/`updated` fields and temporal extent intervals | `done` | `2026-02-15` | Catalogue collection metadata used for explicit dataset-level start/end coverage timestamps. |
| `GLOFAS-URL-27/28/29` | GloFAS | high | dataset message feeds (warnings, advisories, freeze notices) | `done` | `2026-02-16` | Re-checked: reforecast temporary-freeze message dated `2024-11-11` remains present; shared known-issues/time-critical advisory notices also present. |
| `GLOFAS-URL-30/31` | GloFAS | medium | supporting legacy reanalysis coverage anchors from JRC catalogue | `done` | `2026-02-16` | Added to document `v3.0` and `v4.0` historical-reanalysis temporal coverage anchors without bulk download. |
| `GLOFAS-URL-33` | GloFAS | medium | JRC catalogue-level listing check for standalone `v3.1` reanalysis landing page | `done` | `2026-02-16` | No dedicated standalone `v3.1` reanalysis landing page identified in reviewed official catalogue pages. |
| `GLOFAS-URL-32` | GloFAS | low | CDS -> EWDS migration context (supporting forum note) | `done` | `2026-02-16` | Supporting migration context only; not used as authoritative version-coverage evidence. |
| `GLOFAS-LOCAL-01` | GloFAS | high | lightweight API combination validation for historical/forecast/reforecast (`run_id`, request tuple, outcome) | `done` | `2026-02-16` | Probe manifests stored under `repro/glofas_probe_runs/` using `scripts/forecats_probe_glofas_coverage.py` (includes one all-success tri-product run and one intentional invalid-combination run). |
| `GLOFAS-LOCAL-07` | GloFAS | high | full parallel version/model/product coverage scan (bounded metadata-light probing) | `done` | `2026-02-16` | Completed run `scan_20260216T015036Z` under `repro/glofas_coverage_scan_runs/` with full manifest outputs and attempt-level trace. |
| `GLOFAS-LOCAL-08` | GloFAS | high | boundary-focused refinement and consistency recheck for all combinations | `done` | `2026-02-16` | Completed run `refine_20260216T025403Z` under `repro/glofas_coverage_scan_runs/` with `refined_ranges.csv` and attempt-level trace (`refined_attempts.csv`). |
| `GLOFAS-LOCAL-09` | GloFAS | high | consolidated post-refinement coverage matrix with targeted overrides | `done` | `2026-02-16` | Consolidated artifact under `repro/glofas_coverage_scan_runs/consolidated_20260216T195500Z/` (`11` found, `25` not_found). |
| `GLOFAS-LOCAL-10` | GloFAS | medium | manual timeout-controlled boundary probes for `operational + lisflood` forecast products | `done` | `2026-02-16` | Probe table stored at `repro/glofas_coverage_scan_runs/manual_boundary_20260216T194500Z/manual_boundary_results.csv`. |
| `GLOFAS-LOCAL-11` | GloFAS | high | focused priority-scope scan/refinement (`version_3_1` + `version_4_0`, historical+reforecast only) | `done` | `2026-02-16` | Runs: `scan_20260216T065850Z` and `refine_20260216T072601Z` with aggressive boundary expansion; summary bundle at `focused_20260216T075254Z`. |
| `GLOFAS-LOCAL-12` | GloFAS | high | lower-bound validation probes for historical lisflood consolidated (`1978-12-31` vs `1979-01-01`) | `done` | `2026-02-16` | Probe run `probe_20260216T075054Z`: both `version_3_1` and `version_4_0` return invalid on `1978-12-31` and success on `1979-01-01`. |
| `GLOFAS-LOCAL-13` | GloFAS | medium | fixed-date boundary checks across prioritized historical/reforecast tuples with hard timeout control | `done` | `2026-02-16` | Runs `boundary_check_20260216T223610Z` and `boundary_check_20260216T234434Z`: structural exclusions confirmed and `version_2_1 + htessel_lisflood` historical validity/boundaries explicitly confirmed (Section 3.7.10). |
| `GLOFAS-LOCAL-14` | GloFAS | medium | local legacy-JRC file inventory for parity-check readiness | `done` | `2026-02-16` | Initial inventory run showed no local matches at that time; superseded by later legacy `v3.0` acquisition and extraction runs (Section 3.7.11). |
| `GLOFAS-LOCAL-15` | GloFAS | high | forecast selector parity check (`operational` vs explicit `version_2_1`/`version_3_1`) | `done` | `2026-02-17` | Runs `operational_explicit_parity_20260217T012751Z` and `probe_20260217T013028Z`: operational accepted, explicit selectors rejected (`invalid_request`). |
| `GLOFAS-LOCAL-16` | GloFAS | medium | historical consolidated campaign execution/completion status (`v2.1`/`v3.1`/`v4.0`) | `done` | `2026-02-17` | Campaign `hist_campaign_20260217T013445Z` completed `1289/1289` shards; point-series extraction completed for all three products (Section 3.7.11). |
| `GLOFAS-LOCAL-17` | GloFAS | high | legacy `v3.0` vs historical `v3.1` all-period point-series comparison outputs | `done` | `2026-02-17` | Run `repro/glofas_probe_runs/hist_vs_legacy_compare_20260217T193231Z` with pairwise global + rolling metrics and overlay figures. |
| `GLOFAS-LOCAL-18` | GloFAS | medium | fixed-window (`2012-2017`) legacy-vs-historical comparison outputs | `done` | `2026-02-17` | Run `repro/glofas_probe_runs/hist_vs_legacy_compare_20260217T194310Z_2012_2017`. |
| `GLOFAS-LOCAL-19` | GloFAS | medium | fixed-window (`2015-06` to `2017-12`) legacy-vs-historical comparison outputs | `done` | `2026-02-17` | Run `repro/glofas_probe_runs/hist_vs_legacy_compare_20260217T195019Z_2015summer_2017end`. |
| `GLOFAS-LOCAL-20` | GloFAS | medium | 2017 shift/mean-correction diagnostic for legacy `v3.0` vs historical `v3.1` | `done` | `2026-02-17` | Run `repro/glofas_probe_runs/legacyv3_histv31_2017_shiftmean_20260217T195655Z`; diagnostic correction improves fit but does not imply native equivalence. |

## 5) Audit TODO Checklist (Current Revision Complete)

Status legend used in this section: `[x]` done (all current checklist items are closed for this document revision).

### 5.1 Shared tasks

- [x] `A-01`: Lock final filename/location for the shareable data documentation artifact.
  Completion note: canonical path is locked to `repro/NWS_NWM_GLOFAS_DATA_AUDIT_PLAN.md` (see Section 8.1).
- [x] `A-02`: Define fixed date format and null-policy for unknown fields.
  Completion note: date and null policies are locked in Section 8.2.
- [x] `A-03`: Define source confidence labels (`authoritative`, `supporting`, `to_classify`).
  Completion note: label definitions are locked in Section 8.3.
- [x] `A-04`: Add a source-verification log (`source_url`, `claim`, `verified_on`, `status`, `notes`).
  Completion note: source-verification log template added in Section 8.4.
- [x] `A-05`: Keep the audit metadata-first and allow targeted extraction only when needed for unresolved compatibility claims.
  Completion note: evidence-handling policy is locked in document header and Section 8.5.

### 5.2 NWS/NWM tasks

- [x] `NWM-01`: Build complete NWM version chronology for the project period.
  Completion note: timeline completed from official TIN/SCN notices (`NWM-URL-16`, `NWM-URL-11`, `NWM-URL-12`, `NWM-URL-13`, `NWM-URL-14`, `NWM-URL-15`), with proposal-only status recorded for v3.1 (`NWM-URL-18`).
- [x] `NWM-02`: Link each version to retrospective/reanalysis products and release/coverage dates.
  Completion note: retrospective version-to-coverage mapping completed from `NWM-URL-05` and `NWM-URL-07`; explicit gap remains: reviewed metadata exposes coverage windows and catalog-level creation timing, but not explicit per-version retrospective release dates.
- [x] `NWM-03`: Link each version to forecast products and release/coverage dates.
  Completion note: version-to-forecast operational windows and configuration changes documented from SCNs/TIN (`NWM-URL-11` through `NWM-URL-16`) plus current NWM operations page (`NWM-URL-04`) and NODD short-range archive metadata (`NWM-URL-17`).
- [x] `NWM-04`: Confirm whether reforecast/hindcast products exist per version.
  Completion note: no authoritative NWS/NODD source in the reviewed URL set explicitly documents a versioned forecast reforecast/hindcast product for NWM; status recorded as `not found in reviewed sources` (see Section 3.4).
- [x] `NWM-05`: Record storage/access locations per product (portal, bucket/container, API/catalog endpoint).
  Completion note: completed using `NWM-URL-04`, `NWM-URL-05`, `NWM-URL-17`, `NWM-URL-09`, `NWM-URL-10`, `NWM-URL-03`, and supporting cloud-notes context from `NWM-URL-01`/`NWM-URL-02`.
- [x] `NWM-06`: Mark unresolved transitions or version-link uncertainties for follow-up.
  Completion note: unresolved items explicitly recorded: (1) v3.1 is proposal-only pending SCN implementation notice, (2) retrospective per-version release dates are not explicitly published in reviewed sources, (3) forecast-side reforecast/hindcast product remains undocumented in reviewed authoritative URLs. Prior v2.1 member-count uncertainty is now resolved by release-notes cross-check (`NWM-URL-20`).

### 5.3 GloFAS tasks

- [x] `GLOFAS-01`: Build complete GloFAS version chronology (including legacy versions).
  Completion note: chronology completed from official versioning system (`GLOFAS-URL-10`), including production release dates from v1.0 through v4.4 plus internal release-number/test-window/lifecycle-footnote cross-check.
- [x] `GLOFAS-02`: Link each version to historical/retrospective products and release/coverage dates.
  Completion note: historical dataset metadata captured (`GLOFAS-URL-01`) and API system-version options verified (`version_2_1`, `version_3_1`, `version_4_0`) via historical retrieve endpoint (`GLOFAS-URL-22`). Explicit gap: authoritative per-version coverage date ranges for all legacy historical versions are not published as a single table in reviewed sources.
- [x] `GLOFAS-03`: Link each version to forecast products and release/coverage dates.
  Completion note: forecast dataset metadata captured (`GLOFAS-URL-03`) and API system-version options verified (`operational`, `version_2_1`, `version_3_1`) via retrieve endpoint (`GLOFAS-URL-09`). Explicit gap: retrieve metadata does not explicitly map `operational` to a named numeric version in the endpoint itself.
- [x] `GLOFAS-04`: Link each version to reforecast products and release/coverage dates (if available).
  Completion note: reforecast dataset metadata captured (`GLOFAS-URL-05`) and API system-version options verified (`version_2_2`, `version_3_1`, `version_4_0`) via reforecast retrieve endpoint (`GLOFAS-URL-23`), including 46-day lead-time design and twice-weekly cadence.
- [x] `GLOFAS-05`: Record storage/access locations per product (EWDS catalogue/retrieve, IDs, endpoint notes).
  Completion note: recorded using dataset pages (`GLOFAS-URL-01`, `GLOFAS-URL-03`, `GLOFAS-URL-05`), catalogue collection endpoints (`GLOFAS-URL-07`, `GLOFAS-URL-08`, `GLOFAS-URL-24`, `GLOFAS-URL-25`, `GLOFAS-URL-26`), retrieve process endpoints (`GLOFAS-URL-09`, `GLOFAS-URL-22`, `GLOFAS-URL-23`), and collection message feeds (`GLOFAS-URL-27`, `GLOFAS-URL-28`, `GLOFAS-URL-29`).
- [x] `GLOFAS-06`: Mark unresolved transitions or version-link uncertainties for follow-up.
  Completion note: unresolved items explicitly logged: (1) versioning page indicates operational v4.4 while EWDS dataset tables still label current version as v4.0 in reviewed pages, (2) no single authoritative table found that maps each operational version to exact historical/forecast/reforecast coverage windows for every legacy version, (3) forecast retrieve endpoint uses `operational` alias without explicit numeric expansion in endpoint metadata, (4) some per-version page date fields differ from versioning-table chronology and are treated as secondary chronology signals.

Placeholder follow-up tasks (closed for this document revision):

- [x] `GLOFAS-F01`: Extract per-`system_version` historical coverage windows (`2.1`, `3.1`, `4.0`) via lightweight endpoint probing.
  Completion note: priority-focused evidence (`GLOFAS-LOCAL-11` plus lower-bound check `GLOFAS-LOCAL-12`) confirms expanded historical windows for `v3.1` and `v4.0` in lisflood products (`v3.1 consolidated`: `1979-01-01..2024-06-30`; `v3.1 intermediate`: `2021-01-01..2024-09-23`; `v4.0 consolidated`: `1979-01-01..2025-11-30`). Non-priority `2.1` details remain in earlier full-scan artifacts but are de-prioritized in current workflow.
- [x] `GLOFAS-F02`: Extract per-`system_version` reforecast coverage windows (`2.2`, `3.1`, `4.0`) via lightweight endpoint probing.
  Completion note: priority-focused evidence (`GLOFAS-LOCAL-11`) confirms `v3.1+lisflood+control_reforecast` window (`2002-01-03..2002-07-11`), `v3.1+lisflood+ensemble_perturbed_reforecast` point anchor (`2000-02-28`), and stable `v4.0+lisflood` point anchors (`2021-01-04`) for control/ensemble. Non-priority `2.2` remains outside current focus.
- [x] `GLOFAS-F03`: Build explicit lineage mapping table between JRC reanalysis labels (`v3.0`, `v4.0`) and EWDS historical selector labels (`version_2_1`, `version_3_1`, `version_4_0`).
  Completion note: mapping table added in Section 3.7.3.1 with explicit status tags (`high-confidence direct match`, `lineage hypothesis only`, `not found`).
- [x] `GLOFAS-F04`: Confirm whether public standalone `v3.1` historical-reanalysis landing metadata exists and add source URL if found.
  Completion note: no dedicated standalone `v3.1` reanalysis landing page was found in reviewed official JRC catalogue pages; this is now recorded explicitly in Section 3.7.3.1 as `not found`.
- [x] `GLOFAS-F05`: Re-validate medium-range reforecast freeze status from EWDS message feed before each new cutoff-date experiment.
  Completion note: current snapshot verified on `2026-02-16` (`GLOFAS-URL-29`) still shows the `2024-11-11` freeze message. For future cycles, this remains a recurring operations check (Section 11 policy), not an open static TODO.
- [x] `GLOFAS-F06`: Execute initial legacy-JRC vs EWDS historical parity-readiness check (local inventory + execution feasibility).
  Completion note: workflow moved from readiness to execution: legacy `v3.0` archive download, point extraction, and parity comparison versus historical `version_3_1` are complete (`GLOFAS-LOCAL-17` through `GLOFAS-LOCAL-20`, Section 3.7.11). Legacy `v4.0` parity remains open.

### 5.4 Finalization tasks

- [x] `F-01`: Complete both version summary tables with all required fields.
  Completion note: NWM and GloFAS version summary sections are fully populated with verified fields, and unresolved values are explicitly marked (`not found`/`unknown`).
- [x] `F-02`: Add a compact compatibility note per version pairing (`allowed`, `conditional`, `blocked`) with short rationale.
  Completion note: compact pairing decisions are defined in Section 9.
- [x] `F-03`: Add a one-page collaborator summary (safe pairings, conditional pairings, open questions).
  Completion note: collaborator summary added in Section 10.
- [x] `F-04`: Define update cadence and trigger events (new release, archive metadata update, revised methods pages).
  Completion note: cadence and trigger policy added in Section 11.

### 5.5 Script Normalization Plan (Coverage Investigation First)

Current script inventory relevant to this audit:

1. `scripts/forecats_extract_glofas_batch.py` and `scripts/forecats_extract_nws_batch.py` perform post-download extraction from local files.
2. `scripts/forecats_pipeline.R` orchestrates analysis/preparation steps but does not perform EWDS boundary probing.
3. `glofas_operational_mediumrange_download_point.py` is an older downloader focused on forecast retrieval.
4. `scripts/forecats_probe_glofas_coverage.py` is the current lightweight probe tool for metadata-driven request validation.
5. `scripts/forecats_scan_glofas_coverage.py` performs the full parallel baseline scan.
6. `scripts/forecats_refine_glofas_ranges.py` performs boundary-focused refinement and consistency rechecks.
7. `scripts/forecats_check_glofas_boundaries.py` performs fixed-date boundary checks with explicit timeout-controlled request execution.

Implementation status (closed for this document revision):

1. Phase 1 (`done`): lightweight probes were used to map valid/invalid combinations by version and boundary dates.
2. Phase 2 (`done` for audit scope): standardized product-level probing coverage is implemented via `forecats_probe_glofas_coverage.py`, `forecats_scan_glofas_coverage.py`, and `forecats_refine_glofas_ranges.py`. Dedicated production bulk-download wrappers are intentionally out of scope for this metadata audit.
3. Phase 3 (`done` for audit scope): validated windows and outcomes were integrated into reproducible manifests and a consolidated matrix (`GLOFAS-LOCAL-09`) under versioned run directories.
4. Phase 4 (`closed` as reproducibility-preserving policy): temporary probe artifacts are intentionally retained for traceability in this audit branch; cleanup/removal is deferred to production-download implementation work, not this metadata document.

## 6) Flexible Compatibility Review Notes

Use this section for evidence-first notes while avoiding rigid assumptions until metadata is confirmed.

For each candidate retrospective -> forecast pairing, document:

1. Version relationship (same version, related version, unknown).
2. Release/operational timeline alignment.
3. Retrospective product linkage evidence.
4. Forecast/reforecast product linkage evidence.
5. Any transition-window ambiguity.
6. Current decision tag: `allowed`, `conditional`, or `blocked`.

## 7) Seed URLs

### 7.1 NWS/NWM

- `NWM-URL-01`: `https://planetarycomputer.microsoft.com/dataset/storage/noaa-nwm`
- `NWM-URL-02`: `https://github.com/TomAugspurger/noaa-nwm`
- `NWM-URL-03`: `https://www.weather.gov/media/wrn/calendar/NWS-NODD-Microsoft-NWM-Office-Hours-Notes.pdf`
- `NWM-URL-04`: `https://water.noaa.gov/about/nwm`
- `NWM-URL-05`: `https://registry.opendata.aws/nwm-archive/`
- `NWM-URL-06`: `https://github.com/NOAA-Big-Data-Program/nodd-data-docs/blob/main/nwm/README.md`
- `NWM-URL-07`: `https://aws.amazon.com/marketplace/pp/prodview-g6lcchc7brshw`
- `NWM-URL-08`: `https://www.weather.gov/media/owp/operations/nwps_user_guide.pdf`
- `NWM-URL-09`: `https://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/`
- `NWM-URL-10`: `https://maps.water.noaa.gov/server/rest/services/nwm`
- `NWM-URL-11`: `https://www.weather.gov/media/notification/pdfs/scn17-41natl_water_model_aaa.pdf`
- `NWM-URL-12`: `https://www.weather.gov/media/notification/pdfs/scn18-16national_water_model.pdf`
- `NWM-URL-13`: `https://www.weather.gov/media/notification/pdf2/scn19-42natl_water_model.pdf`
- `NWM-URL-14`: `https://www.weather.gov/media/notification/pdf2/scn20-119nwm_v2.1_aad.pdf`
- `NWM-URL-15`: `https://www.weather.gov/media/notification/pdf_2023_24/scn23-76_national_water_model_v3.0_aab.pdf`
- `NWM-URL-16`: `https://www.weather.gov/media/notification/tins/tin16-30natl_water_model.pdf`
- `NWM-URL-17`: `https://registry.opendata.aws/noaa-nwm-pds/`
- `NWM-URL-18`: `https://preview.weather.gov/notification/`
- `NWM-URL-19`: `https://water.noaa.gov/about/output_file_contents`
- `NWM-URL-20`: `https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/nwm.v3.0.17/doc/NationalWaterModelReleaseNotes.pdf`

### 7.2 GloFAS

- `GLOFAS-URL-01`: `https://ewds.climate.copernicus.eu/datasets/cems-glofas-historical?tab=overview`
- `GLOFAS-URL-02`: `https://confluence.ecmwf.int/plugins/servlet/mobile?contentId=265028796#content/view/265028624`
- `GLOFAS-URL-03`: `https://ewds.climate.copernicus.eu/datasets/cems-glofas-forecast?tab=overview`
- `GLOFAS-URL-04`: `https://global-flood.emergency.copernicus.eu/general-information/glofas-methods/`
- `GLOFAS-URL-05`: `https://ewds.climate.copernicus.eu/datasets/cems-glofas-reforecast`
- `GLOFAS-URL-06`: `https://ewds.climate.copernicus.eu/how-to-api`
- `GLOFAS-URL-07`: `https://ewds.climate.copernicus.eu/api/catalogue/v1/`
- `GLOFAS-URL-08`: `https://ewds.climate.copernicus.eu/api/catalogue/v1/collections/cems-glofas-forecast`
- `GLOFAS-URL-09`: `https://ewds.climate.copernicus.eu/api/retrieve/v1/processes/cems-glofas-forecast`
- `GLOFAS-URL-10`: `https://confluence.ecmwf.int/display/CEMS/GloFAS%2Bversioning%2Bsystem`
- `GLOFAS-URL-11`: `https://confluence.ecmwf.int/display/CEMS/Latest%2Boperational%2BGloFAS%2Brelease`
- `GLOFAS-URL-12`: `https://confluence.ecmwf.int/pages/viewpage.action?pageId=388505179`
- `GLOFAS-URL-13`: `https://confluence.ecmwf.int/display/CEMS/GloFAS+v4.3`
- `GLOFAS-URL-14`: `https://confluence.ecmwf.int/display/CEMS/GloFAS+v4.2`
- `GLOFAS-URL-15`: `https://confluence.ecmwf.int/display/CEMS/GloFAS+v4.1`
- `GLOFAS-URL-16`: `https://confluence.ecmwf.int/display/CEMS/GloFAS+v3.5`
- `GLOFAS-URL-17`: `https://confluence.ecmwf.int/display/CEMS/GloFAS+v3.1`
- `GLOFAS-URL-18`: `https://confluence.ecmwf.int/display/CEMS/GloFAS+v2.2`
- `GLOFAS-URL-19`: `https://confluence.ecmwf.int/display/CEMS/GloFAS+v2.1`
- `GLOFAS-URL-20`: `https://confluence.ecmwf.int/display/CEMS/GloFAS+v2.0`
- `GLOFAS-URL-21`: `https://confluence.ecmwf.int/display/CEMS/GloFAS+v1.0`
- `GLOFAS-URL-22`: `https://ewds.climate.copernicus.eu/api/retrieve/v1/processes/cems-glofas-historical`
- `GLOFAS-URL-23`: `https://ewds.climate.copernicus.eu/api/retrieve/v1/processes/cems-glofas-reforecast`
- `GLOFAS-URL-24`: `https://ewds.climate.copernicus.eu/api/catalogue/v1/collections/cems-glofas-historical`
- `GLOFAS-URL-25`: `https://ewds.climate.copernicus.eu/api/catalogue/v1/collections/cems-glofas-forecast`
- `GLOFAS-URL-26`: `https://ewds.climate.copernicus.eu/api/catalogue/v1/collections/cems-glofas-reforecast`
- `GLOFAS-URL-27`: `https://ewds.climate.copernicus.eu/api/catalogue/v1/collections/cems-glofas-historical/messages`
- `GLOFAS-URL-28`: `https://ewds.climate.copernicus.eu/api/catalogue/v1/collections/cems-glofas-forecast/messages`
- `GLOFAS-URL-29`: `https://ewds.climate.copernicus.eu/api/catalogue/v1/collections/cems-glofas-reforecast/messages`
- `GLOFAS-URL-30`: `https://data.jrc.ec.europa.eu/dataset/73305ca5-c002-4124-b1d5-6451cc93af3f`
- `GLOFAS-URL-31`: `https://data.jrc.ec.europa.eu/dataset/f96b7a19-0133-4105-a879-0536991ca9c5`
- `GLOFAS-URL-32`: `https://forum.ecmwf.int/t/glofas-version-3-1-no-longer-seems-available-in-the-climate-data-store/6811`
- `GLOFAS-URL-33`: `https://data.jrc.ec.europa.eu/collection/id-0069`

### 7.3 Project-Local Evidence Inputs

- `NWM-LOCAL-01`: `SOURCE_WORKFLOW_ROOT/results.pkl`
- `GLOFAS-LOCAL-01`: `scripts/forecats_probe_glofas_coverage.py`
- `GLOFAS-LOCAL-02`: `repro/glofas_probe_runs/probe_20260216T010503Z/manifests/probe_manifest.csv`
- `GLOFAS-LOCAL-03`: `repro/glofas_probe_runs/probe_20260216T010529Z/manifests/probe_manifest.csv`
- `GLOFAS-LOCAL-04`: `repro/glofas_probe_runs/probe_20260216T010601Z/manifests/probe_manifest.csv`
- `GLOFAS-LOCAL-05`: `repro/glofas_probe_runs/probe_20260216T010822Z/manifests/probe_manifest.csv`
- `GLOFAS-LOCAL-06`: `repro/glofas_probe_runs/probe_20260216T011130Z/manifests/probe_manifest.csv`
- `GLOFAS-LOCAL-07`: `repro/glofas_coverage_scan_runs/scan_20260216T015036Z/manifests/coverage_summary.csv`
- `GLOFAS-LOCAL-08`: `repro/glofas_coverage_scan_runs/refine_20260216T025403Z/manifests/refined_ranges.csv`
- `GLOFAS-LOCAL-09`: `repro/glofas_coverage_scan_runs/consolidated_20260216T195500Z/refined_ranges_consolidated.csv`
- `GLOFAS-LOCAL-10`: `repro/glofas_coverage_scan_runs/manual_boundary_20260216T194500Z/manual_boundary_results.csv`
- `GLOFAS-LOCAL-11`: `repro/glofas_coverage_scan_runs/focused_20260216T075254Z/focused_refined_ranges.csv`
- `GLOFAS-LOCAL-12`: `repro/glofas_probe_runs/probe_20260216T075054Z/manifests/probe_manifest.csv`
- `GLOFAS-LOCAL-13`: `repro/glofas_probe_runs/boundary_check_20260216T234434Z/manifests/boundary_manifest.csv`
- `GLOFAS-LOCAL-14`: `repro/glofas_probe_runs/legacy_inventory_20260216T224643Z/summary.txt`
- `GLOFAS-LOCAL-15`: `repro/glofas_probe_runs/operational_explicit_parity_20260217T012751Z/parity_manifest.csv`
- `GLOFAS-LOCAL-16`: `repro/glofas_probe_runs/hist_campaign_20260217T013445Z/campaign_metadata.json`
- `GLOFAS-LOCAL-17`: `repro/glofas_probe_runs/hist_vs_legacy_compare_20260217T193231Z/tables/pairwise_global_summary.csv`
- `GLOFAS-LOCAL-18`: `repro/glofas_probe_runs/hist_vs_legacy_compare_20260217T194310Z_2012_2017/tables/pairwise_global_summary_2012_2017.csv`
- `GLOFAS-LOCAL-19`: `repro/glofas_probe_runs/hist_vs_legacy_compare_20260217T195019Z_2015summer_2017end/tables/pairwise_global_summary_2015summer_2017end.csv`
- `GLOFAS-LOCAL-20`: `repro/glofas_probe_runs/legacyv3_histv31_2017_shiftmean_20260217T195655Z/tables/metrics_2017.json`

## 8) Shared Standards (Locked)

### 8.1 Canonical document location

- Canonical audit file: `repro/NWS_NWM_GLOFAS_DATA_AUDIT_PLAN.md`
- Document owner (current): project maintainer
- Scope boundary: metadata, versioning, coverage, and access documentation plus targeted point-level validation used to resolve unresolved compatibility claims.

### 8.2 Date and null policy

| field type | policy |
|---|---|
| Date only | `YYYY-MM-DD` |
| Date-time | ISO 8601 UTC (e.g., `2026-02-15T00:00:00Z`) |
| Unknown scalar value | `unknown` |
| Not explicitly published in reviewed source | `not explicitly listed` |
| Not found after reviewed-source search | `not found in reviewed sources` |

### 8.3 Source confidence labels

| label | definition |
|---|---|
| `authoritative` | Official service owner documentation or official API/catalog metadata endpoint. |
| `supporting` | Reputable but non-authoritative explanatory source (e.g., community repos, office-hour notes). |
| `to_classify` | Source captured but confidence level not yet assigned. |

### 8.4 Source verification log template

| source_id | source_url | claim_checked | verdict | verified_on_utc | reviewer | notes |
|---|---|---|---|---|---|---|
| `<fill>` | `<fill>` | `<fill>` | `verified`/`partially_verified`/`not_found`/`conflict` | `<fill>` | `<fill>` | `<fill>` |

### 8.5 Evidence-handling policy

1. Use metadata pages, API/catalog descriptors, and official release notices as the default evidence source.
2. Allow targeted extraction runs (small-area, limited-date, point-level reduction) when metadata-only evidence is insufficient to resolve coverage or lineage questions.
3. Bulk global archives are allowed only for explicitly logged parity tasks and must be reduced to compact derived artifacts for routine analysis.
4. Any unresolved field remains documented as unknown/not-found rather than inferred.

### 8.6 Terminology and Coverage Window Standard

| canonical term in this project | NWS/NWM equivalent | GloFAS equivalent | meaning |
|---|---|---|---|
| `retrospective_or_reanalysis` | retrospective | historical | Model run used to learn historical bias structure against observations. |
| `forecast_operational` | operational forecast products | operational forecast products | Real-time forecast ensemble products to be bias-corrected. |
| `reforecast_or_hindcast` | not found in reviewed authoritative NWS sources | reforecast | Forecast-like reruns on past dates for skill estimation/post-processing. |
| `operational_version_window` | SCN/TIN-based NWM version window | versioning-table GloFAS production window | Date window where a forecast version is operational. |
| `retro_coverage_window` | retrospective dataset coverage by version | historical/reforecast coverage by version | Data-date coverage of the retrospective/reanalysis side used for bias learning. |
| `collection_temporal_extent` | rarely exposed as STAC-like collection in reviewed NWM sources | EWDS catalogue extent intervals | Dataset-level start/end timestamps, not necessarily version-specific. |

Standard usage rule:

1. For cross-center comparability, always report both `operational_version_window` and `retro_coverage_window` in the same pairing note.
2. Treat `collection_temporal_extent` as dataset-level context, not proof of version-specific coverage.
3. For NWS/NWM in this project, domain scope is fixed to CONUS.

## 9) Compatibility Decisions (Compact, Current Evidence)

Decision scope:

1. These decisions are metadata-based, evidence-constrained pairability tags.
2. They are not final model-performance judgments and can change with new authoritative metadata.

### 9.1 NWS/NWM pairing notes

| retrospective_version | forecast_version | decision | rationale | evidence |
|---|---|---|---|---|
| 3.0 retrospective | 3.0 forecast | `conditional` | Same version label, but retrospective publication date not explicitly listed and retrospective does not include streamflow DA. | `NWM-URL-05`, `NWM-URL-15` |
| 2.1 retrospective | 2.1 forecast | `conditional` | Same version label; retrospective and operational windows are documented but retrospective release-date metadata is incomplete. | `NWM-URL-05`, `NWM-URL-14` |
| 2.0 retrospective | 2.0 forecast | `conditional` | Same version label; similar metadata limitations on retrospective release-date publication. | `NWM-URL-05`, `NWM-URL-13` |
| 1.2 retrospective | 1.2 forecast | `conditional` | Same version label; legacy metadata is available but not complete for release-date fields. | `NWM-URL-05`, `NWM-URL-12` |
| Any cross-version retrospective->forecast pair | mixed | `blocked` | No reviewed authoritative mapping proving cross-version bias-transfer equivalence. | `NWM-URL-04`, `NWM-URL-05` |

#### 9.1.1 NWS/NWM cutoff-date operating rule (for bias fitting)

Use this direct mapping when choosing retrospective data from a forecast issue cutoff date:

| cutoff_date range (forecast issue date) | forecast version | retrospective version to use | retrospective coverage window | interpretation |
|---|---|---|---|---|
| `2021-04-20` to `2023-09-19` | `2.1` | `2.1` | `1979-02` to `2020-12` | Same-version pairing, but retrospective coverage ends before many cutoffs in this window. |
| `2023-09-20` onward | `3.0` | `3.0` | `1979-02` to `2023-01` | Same-version pairing, but retrospective coverage ends before many recent cutoffs. |

Implementation rule:

1. Choose retrospective by forecast version label first (`2.1->2.1`, `3.0->3.0`).
2. Then explicitly report the retrospective coverage-end gap to cutoff date in your run metadata.

### 9.2 GloFAS pairing notes

Labeling note for this section:

1. Keep legacy JRC and EWDS labels explicit in all compatibility rows.
2. `legacy v3.0` and `historical version_3_1` are treated as related by lineage context only, not as numerically equivalent products.

| historical_or_reforecast_version | forecast_version | decision | rationale | evidence |
|---|---|---|---|---|
| historical `version_3_1` | forecast `version_3_1` | `conditional` | Focused bounded windows are confirmed for `version_3_1 + lisflood` historical products, but forecast pairing still relies on chronology mapping because explicit forecast selector `version_3_1` returned `invalid_request` in prior bounded checks. | `GLOFAS-URL-22`, `GLOFAS-URL-09`, `GLOFAS-LOCAL-11` |
| historical `version_2_1` | forecast `version_2_1` | `ambiguous` | Historical anchors exist for `htessel_lisflood`, but forecast-side `version_2_1` combinations repeatedly returned `invalid_request` in bounded validation. | `GLOFAS-URL-22`, `GLOFAS-URL-09`, `GLOFAS-LOCAL-07`, `GLOFAS-LOCAL-08` |
| reforecast `version_3_1` | forecast `version_3_1` | `conditional` | Focused bounded windows/anchors are confirmed for `version_3_1 + lisflood` reforecast products, but forecast pairing keeps a selector-mismatch caveat because explicit forecast selector `version_3_1` previously failed in bounded checks. | `GLOFAS-URL-23`, `GLOFAS-URL-09`, `GLOFAS-LOCAL-11` |
| historical `version_4_0` | forecast `operational` | `conditional` | Historical `version_4_0 + lisflood + consolidated` bounded window is expanded and reconfirmed in focused runs, but `operational` numeric expansion remains implicit and some forecast-side boundaries are timeout-sensitive. | `GLOFAS-URL-22`, `GLOFAS-URL-09`, `GLOFAS-URL-10`, `GLOFAS-LOCAL-11`, `GLOFAS-LOCAL-12` |
| reforecast `version_4_0` | forecast `operational` | `conditional` | Reforecast `version_4_0 + lisflood` control/ensemble point anchors are reconfirmed at `2021-01-04`; still treat as conditional because this is point-anchor evidence and freeze messaging exists for recent updates. | `GLOFAS-URL-23`, `GLOFAS-URL-09`, `GLOFAS-URL-29`, `GLOFAS-LOCAL-11` |
| Any cross-version historical/reforecast->forecast pair | mixed | `blocked` | No reviewed authoritative source proves cross-version transfer compatibility as default. | `GLOFAS-URL-09`, `GLOFAS-URL-10` |

#### 9.2.1 GloFAS cutoff-date operating rule (for bias fitting)

| forecast issue cutoff window | forecast version from chronology | exact historical version available in retrieve options | exact reforecast version available in retrieve options | decision |
|---|---|---|---|---|
| `2019-11-05` to `2020-12-08` | `2.1` | yes (`version_2_1`) | no (reforecast options start at `2.2`) | `ambiguous` |
| `2020-12-09` to `2021-05-25` | `2.2` | no | yes (`version_2_2`) | `ambiguous` |
| `2021-05-26` to `2021-10-26` | `3.1` | yes (`version_3_1`) | yes (`version_3_1`) | `conditional` |
| `2021-10-27` to `2023-07-25` | `3.2` / `3.3` / `3.4` / `3.5` | no exact version option | no exact version option | `ambiguous` |
| `2023-07-26` to `2024-02-27` | `4.0` | yes (`version_4_0`) | yes (`version_4_0`) | `conditional` |
| `2024-02-28` onward | `4.1+` | no exact version option exposed | no exact version option exposed | `ambiguous` |

Example (`cutoff_date=2022-12-25`):

1. Forecast chronology maps this cutoff to `v3.4`.
2. Retrieve options for historical/reforecast do not expose `version_3_4`; only `2.1/3.1/4.0` (historical) and `2.2/3.1/4.0` (reforecast) are exposed.
3. So this cutoff remains `ambiguous` until an authoritative compatibility mapping (or additional endpoint metadata) is found.

Cautionary note:

1. Reforecast collection messages include a `2024-11-11` temporary-freeze notice (no EWDS medium-range reforecast updates from the GloFAS v4.2 release date in reviewed metadata).
2. This freeze affects data availability posture and should be checked before assuming reforecast continuity for recent periods.
3. Targeted reruns after `GLOFAS-LOCAL-08` reconfirmed `version_4_0 + lisflood` reforecast point anchors (`2021-01-04`) for control and ensemble, but this remains point-anchor evidence rather than a full coverage window.

### 9.3 Cutoff-to-Retrospective Construction Rule (Current Project Policy)

Applies to cutoffs at or after `2019-11-05` (the first shared forecast-origin date currently available for both centers in local storage).

Given cutoff `c`:

1. Use forecast issue date `c` for both centers.
2. Choose forecast-version label by chronology (NWS SCN/TIN chronology; GloFAS versioning chronology).
3. Choose the retrospective/historical version:
   - NWS: same version as forecast (`2.0->2.0`, `2.1->2.1`, `3.0->3.0`).
   - GloFAS: closest exposed historical family match in EWDS (`2.x->2.1`, `3.x->3.1`, `4.x->4.0`).
4. For NWS, build a version-matched synthetic one-step retrospective (same transformed daily scale as plotted forecasts):
   - `S_x(d) = mean_m F_x(issue=d-1, lead=1 day, member=m)`,
   - using only forecasts from version `x` and only issue dates `<= c`.
5. If there is a date gap between the last day of NWS retrospective `x` and the first day available from `S_x`, fill that gap with the next NWS retrospective version.

#### 9.3.1 NWS/NWM gap-fill dates by forecast version

| NWS forecast version `x` | cutoff window where `x` applies | retrospective `x` end date | first synthetic day from forecasts of `x` | gap dates to fill | gap-fill retrospective |
|---|---|---|---|---|---|
| `2.0` | `2019-11-05` to `2021-04-19` | `2018-12-31` | `2019-11-06` (using shared local overlap start `2019-11-05`) | `2019-01-01` to `2019-11-05` | `2.1` |
| `2.1` | `2021-04-20` to `2023-09-19` | `2020-12-31` | `2021-04-21` | `2021-01-01` to `2021-04-20` | `3.0` |
| `3.0` | `2023-09-20` onward | `2023-01-31` | `2023-09-21` | `2023-02-01` to `2023-09-20` | unresolved in current catalog (no later NWS retrospective version published in this audit) |

Notes:

1. The first row is intentionally anchored to the current dual-center stored overlap (`2019-11-05` onward), because this is the active joint modeling window in local artifacts.
2. If you re-expand NWS-only forecast origins earlier than `2019-11-05`, recompute the first synthetic day for `2.0` accordingly.

#### 9.3.2 GloFAS historical-version pick rule by forecast version

| cutoff window (forecast issue date) | inferred GloFAS forecast version | historical version to use |
|---|---|---|
| `2019-11-05` to `2020-12-08` | `2.1` | `version_2_1` (`htessel_lisflood`, consolidated) |
| `2020-12-09` to `2021-05-25` | `2.2` | `version_2_1` (closest exposed `2.x` historical) |
| `2021-05-26` to `2021-10-26` | `3.1` | `version_3_1` (`lisflood`, consolidated) |
| `2021-10-27` to `2022-10-18` | `3.2` | `version_3_1` (closest exposed `3.x` historical) |
| `2022-10-19` to `2022-12-13` | `3.3` | `version_3_1` (closest exposed `3.x` historical) |
| `2022-12-14` to `2023-06-27` | `3.4` | `version_3_1` (closest exposed `3.x` historical) |
| `2023-06-28` to `2023-07-25` | `3.5` | `version_3_1` (closest exposed `3.x` historical) |
| `2023-07-26` onward | `4.x` family | `version_4_0` (closest exposed `4.x` historical) |

### 9.4 Local forecast-origin span currently stored (filesystem snapshot: 2026-02-20)

| source | path pattern | min origin date | max origin date | unique origin dates | notes |
|---|---|---|---|---|---|
| GloFAS forecast cache | `data/forecats_cache/site=11160500/run_id=latest/forecast_cache/glofas/issue_date=*` | `2019-11-05` | `2023-01-31` | `1176` | Four known missing blocks: `2020-03-12..2020-03-16`, `2020-07-29`, `2020-11-14`, `2022-07-14`. |
| NWS forecast cache | `data/forecats_cache/site=11160500/run_id=latest/forecast_cache/nws/cutoff_date=*` | `2019-11-05` | `2023-01-31` | `1176` | Same missing blocks as GloFAS cache. |
| GloFAS raw GRIB store | `data/glofas_operational_medium_range/grib/issue_date=*` | `2019-11-05` | `2023-01-31` | `1176` | Mirrors current extracted GloFAS cache window. |
| NWS raw archive (`results.pkl`) | `results.pkl` | `2018-09-17` | `2024-02-20` | `1980` | Broader than cache window; current batch cache is a subset used for shared runs. |

Current dual-center overlap available in local processed cache: `2019-11-05` to `2023-01-31`.

## 10) One-Page Collaborator Summary (Current)

### 10.1 Current Status

1. NWM and GloFAS version timelines are populated from reviewed official sources.
2. Retrospective/historical, forecast, and reforecast metadata is mapped for both centers.
3. A bounded GloFAS scan/refinement workflow is completed at full scope and priority scope, including focused follow-up artifacts (`GLOFAS-LOCAL-11`) and lower-bound validation probes (`GLOFAS-LOCAL-12`).
4. Key unresolved metadata gaps are explicitly documented instead of inferred.

### 10.2 Default Pairing Policy

1. Same-version pairs are preferred.
2. Cross-version pairs are blocked unless explicit authoritative equivalence is found.
3. Pairs with unresolved selector behavior or missing release metadata remain conditional.
4. No alias-based value-equivalence override is used: `legacy v3.0` and `historical version_3_1` must remain distinct in modeling and reporting.

### 10.3 Open questions

1. NWM forecast-side reforecast/hindcast product: not found in reviewed authoritative NWS/NODD URLs.
2. NWM retrospective per-version release dates: not explicitly listed in reviewed sources (Marketplace provides catalog-level `creationDate`, not per-version publication dates).
3. GloFAS `operational` alias mapping to numeric version in forecast retrieve metadata: not explicitly documented in endpoint metadata.
4. GloFAS retrieve/catalogue metadata is clear on collection-level extents, but per-`system_version` date windows are not explicitly published in the reviewed endpoint schemas.
5. Legacy `v4.0` reanalysis versus EWDS historical `version_4_0` numeric parity is still open (legacy `v4.0` point series pending local extraction/comparison).
6. GloFAS per-version release pages sometimes report date fields that differ from the versioning-table chronology; current policy is to anchor chronology to `GLOFAS-URL-10` and use per-version pages for product-impact details.
7. Reforecast `version_4_0 + lisflood` point anchors are reconfirmed (`2021-01-04`), but broader reforecast version-window coverage is still unresolved (point-anchor evidence only).
8. After scan + refinement, many combinations remain no-anchor with repeated `invalid_request`; this strongly suggests combination-level unavailability under current selector/request settings, but still requires periodic recheck.

## 11) Update Cadence and Trigger Policy

| trigger | cadence | required action | output update |
|---|---|---|---|
| Routine maintenance | monthly | Re-check versioning pages, dataset overview pages, retrieve API inputs | Update Sections 3, 4, 5, 9, and `Last updated` date |
| New operational release notice | event-driven | Add new version row and assess pairing implications | Update Sections 3 and 9 |
| Archive/catalog metadata change | event-driven | Re-validate coverage windows, publication/update dates, format/options | Update Sections 3 and 4 |
| New evidence resolving an open question | event-driven | Move relevant item from open question to resolved status | Update Sections 5.2/5.3, 10.3, and 9 |

Policy:

1. Never overwrite a prior claim without updating evidence reference and verification date.
2. Keep unresolved items explicit (`unknown`, `not explicitly listed`, `not found in reviewed sources`).
