<p align="center">
  <img src="https://native-resilience.com/wp-content/uploads/2025/02/NR-dark-bluebk-300x300.png" alt="Native Resilience Logo" width="200" height="200" />
</p>

## Native Resilience CMIP6 Agricultural Climate Projections

<p align="center">
<strong>Data are available via a searchable web map at<br>
https://native-resilience.com/projections/</strong>
<br><br>
<strong>Browse the archive here:<br>
https://data.native-resilience.com/projections/</strong>
</p>

[*Native Resilience*](https://native-resilience.com) (formerly Native Climate) is a USDA NIFA-funded project to support climate
adaptation efforts in Native American communities by building new
connections between Native wisdom and Western scientific data. Through
two-way information-sharing and relationship-building, Native Resilience
aims to make climate data more accessible and useful to Tribes, and to
build awareness nationally about climate impacts and resilience on
Native lands. The project is led by the Desert Research Institute in
partnership with University of Nevada, Reno Tribal Extension, the
Montana Climate Office at the University of Montana, University of
Arizona Tribal Extension, and the USDA Climate Hubs. The project team
includes Tribal natural resource managers, agriculture producers, and
climate leaders along with 1994 and 1862 Tribal Extension, researchers
working in Indian Country, other federal and state climate service
organizations, and a group of Native American advisors. *Native Resilience*
is funded by the U.S. Department of Agriculture, National Institute of
Food and Agriculture (USDA NIFA) and builds on the work of the [Native
Waters on Arid Lands](https://nativewaters-aridlands.com/) project
(2015–2022).

Native Resilience partners at the Montana Climate Office have extracted
place-based climate data for Native American, Alaska Native, and Native
Hawaiian lands located in the United States. Climate data and
projections for temperature, precipitation, and other metrics related to
crop, livestock and forestry agriculture are shown in the accompanying
graphs. The data derive from eight Coupled Model Intercomparison Project
Phase 6 (CMIP6) global climate models and four socioeconomic scenarios
for the period from 2015 to 2100, as well as the historical simulation
for each model for the period 1950 to 2014. Raw data are area-weighted
averages over each land, extracted from version 2.0 of the NASA Earth
Exchange (NEX) Global Daily Downscaled Projections (GDDP) dataset
(NEX-GDDP-CMIP6 v2.0, 2025). Further information on the NASA NEX
downscaled product, including descriptions of the projected climate
variables, can be found at
<https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6>.
NASA cautions that downscaled values over smaller islands may not be
realistic, because the observational data used for bias correction are
not validated over oceans.

Spatial data on Native lands were derived from the 2025 US Census
TIGER/Line database (via the
[census-aiannh](https://data.sustainable-fsa.com/census-aiannh/)
archive), which includes all tribally controlled lands in the United
States, as well as Alaska Native Village Statistical Areas (ANVSA) and
State Designated Tribal Statistical Areas (SDTSA). Reservations and
off-reservation trust lands (including Hawaiian Home Lands) are kept
separate: daily data are extracted for both, and annual and smoothed
projections, workbooks, and graphs are provided for reservations and
statistical areas. We divided the Navajo
Nation into its five agencies to better represent climate differences
across the Nation. We used the recently defined climate divisions for
the State of Hawai‘i ([Luo et al
2024](https://journals.ametsoc.org/view/journals/bams/105/6/BAMS-D-23-0236.1.xml))
to represent climate difference on the Hawaiian Islands.

Tabular data for all graphs included here are available in the
accompanying Microsoft Excel workbook. We aggregated daily data from
each model and scenario into seasonal or annual statistics (the **Annual
Projections** worksheet), and then we combined the results from the
eight models using a generalized additive model for location, scale and
shape (GAMLSS, fit with the [gamlss2](https://github.com/gamlss-dev/gamlss2)
R package; the **Smoothed Projections** worksheet). For each scenario and
variable, the annual values of all eight models are fit with a
distribution whose parameters change smoothly over time, and we report
the median and the 5th, 25th, 75th and 95th percentiles of that
distribution in each year, so the ranges reflect both differences among
models and year-to-year variability. We provide raw daily data for each
scenario in the **Raw** worksheets.

The CMIP6 data include projections for four scenarios for how global
society, demographics and economics might change over the next century,
collectively called “Shared Socioeconomic Pathways” (SSPs). The SSPs
offer pathways that the world could take in the future and are used in
conjunction with the “Representative Concentration Pathways” (RCPs) for
greenhouse gas emissions that were included in previous CMIP
projections.

The following scenarios are included in these data:

> **SSP1-2.6** This is a best-case scenario. Global CO<sub>2</sub>
> emissions are cut severely, reaching net-zero after 2050. Societies
> switch to more sustainable practices, with focus shifting from
> economic growth to overall well-being. Investments in education and
> health go up, and inequality falls. Temperatures stabilize around 1.8
> ºC higher by the end of the century.
>
> **SSP2-4.5** This is a “middle of the road” scenario. CO<sub>2</sub>
> emissions hover around current levels before starting to fall
> mid-century, but do not reach net-zero by 2100. Socioeconomic factors
> follow their historic trends, with no notable shifts. Progress toward
> sustainability is slow, with development and income growing unevenly.
> In this scenario, temperatures rise 2.7 ºC by the end of the century.
>
> **SSP3-7.0** On this path, emissions and temperatures rise steadily
> and CO<sub>2</sub> emissions roughly double from current levels by
> 2100. Countries become more competitive with one another, shifting
> toward national security and ensuring their own food supplies. By the
> end of the century, average temperatures have risen by 3.6 ºC.
>
> **SSP5-8.5** This can be considered a worst-case scenario. Current
> CO<sub>2</sub> emissions levels roughly double by 2050. The global
> economy grows quickly, but this growth is fueled by exploiting fossil
> fuels and energy-intensive lifestyles. By 2100, the average global
> temperature is 4.4 ºC higher.

**We derived the following agricultural climate variables from the raw
data:**

<table>
<colgroup>
<col style="width: 29%" />
<col style="width: 14%" />
<col style="width: 55%" />
</colgroup>
<thead>
<tr>
<th>Variable</th>
<th>Units</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td>Average Temperature</td>
<td>ºF</td>
<td>Annual average daily temperature</td>
</tr>
<tr>
<td>Growing Degree Days</td>
<td>Fahrenheit GDDs</td>
<td>Heat accumulation (warmth) above 50 ºF, in units of Fahrenheit
Growing Degree Days. Appropriate for corn agriculture.</td>
</tr>
<tr>
<td>Frost Free Days</td>
<td>count</td>
<td>Number of days per year with minimum daily temperatures greater than
32 ºF</td>
</tr>
<tr>
<td>Annual Precipitation</td>
<td>inches</td>
<td>Total annual precipitation</td>
</tr>
<tr>
<td>Annual Frozen Precipitation</td>
<td>inches</td>
<td>Total annual precipitation on days with a minimum temperature of at
most 32 ºF</td>
</tr>
<tr>
<td>Spring Precipitation</td>
<td>inches</td>
<td>March–May total precipitation</td>
</tr>
<tr>
<td>Summer Precipitation</td>
<td>inches</td>
<td>June–August total precipitation</td>
</tr>
<tr>
<td>Fall Precipitation</td>
<td>inches</td>
<td>September–November total precipitation</td>
</tr>
<tr>
<td>Winter Precipitation</td>
<td>inches</td>
<td>December–February total precipitation</td>
</tr>
<tr>
<td>Maximum 3-Day Precipitation</td>
<td>inches</td>
<td>Maximum total precipitation over a three-day period each year</td>
</tr>
<tr>
<td>Average Precipitation on Wet Days</td>
<td>inches</td>
<td>Average daily precipitation on days with precipitation</td>
</tr>
<tr>
<td>Average Precipitation on Wet Days (trace)</td>
<td>inches</td>
<td>Average daily precipitation on days with more than 0.08 inches (2
mm) of precipitation</td>
</tr>
<tr>
<td>Number of Wet Days</td>
<td>count</td>
<td>Number of days per year with precipitation</td>
</tr>
<tr>
<td>Number of Wet Days (trace)</td>
<td>count</td>
<td>Number of days per year with more than 0.08 inches (2 mm) of
precipitation</td>
</tr>
<tr>
<td>Number of Dry Days</td>
<td>count</td>
<td>Number of days per year with no precipitation</td>
</tr>
<tr>
<td>Number of Dry Days (trace)</td>
<td>count</td>
<td>Number of days per year with at most 0.08 inches (2 mm) of
precipitation</td>
</tr>
<tr>
<td>Number of Days &gt;= 100 ºF</td>
<td>count</td>
<td>Number of days per year where the daily high temperature is at least
100 ºF</td>
</tr>
<tr>
<td>Number of Days with Heat Index Hazard</td>
<td>count</td>
<td><p>Number of days per year at each of the National Atmospheric and
Oceanographic Organization’s (NOAA) heat index hazard levels.</p>
<p><em>Caution</em>: fatigue is possible with prolonged exposure and
activity; continuing activity could result in heat cramps.</p>
<p><em>Extreme caution</em>: heat cramps and heat exhaustion are
possible; continuing activity could result in heat stroke.</p>
<p><em>Danger</em>: heat cramps and heat exhaustion are likely; heat
stroke is probable with continued activity.</p>
<p><em>Extreme danger</em>: heat stroke is imminent.</p></td>
</tr>
<tr>
<td>Average Surface Wind Speed</td>
<td>miles per hour</td>
<td>Annual average wind speed at ten meters above the ground
surface</td>
</tr>
<tr>
<td>Normal First Day of Growing Season</td>
<td>date</td>
<td>First day of the year, prior to the normal hottest day of the year,
having six consecutive days with a normal average daily temperature
above 50 ºF</td>
</tr>
<tr>
<td>Normal Last Day of Growing Season</td>
<td>date</td>
<td>First day of the year, following the normal hottest day of the year,
having six consecutive days with a normal average daily temperature
below 50 ºF</td>
</tr>
<tr>
<td>Normal Length of Growing Season</td>
<td>days</td>
<td>Number of days between the first and last days of the growing
season; zero in years when the normal temperature never stays above
50 ºF for six days</td>
</tr>
<tr>
<td>Day of First Snow</td>
<td>date</td>
<td>First day of the year, following the normal hottest day of the year,
with frozen precipitation</td>
</tr>
</tbody>
</table>

## Data archive

The per-land zips, PDF reports and dashboard data are archived in the
public S3 bucket `s3://native-resilience` (us-west-2) under
`projections/`, served at <https://data.native-resilience.com/projections/>:

- `<Native_Land_Name>.zip` — the XLSX workbook, PDF report and PNG graphs
  for one land (spaces in the name become underscores).
- `pdf/<Native Land Name>_CMIP6-climate-projections.pdf` — the PDF report alone.
- `native-land.parquet` — GeoParquet boundaries of every unit: 633 modeled
  lands plus 250 off-reservation trust lands and Hawaiian Home Lands.
  `land_id` is the Census GEOID (`2430R-<agency>` for the Navajo agencies,
  `HI-<nn>` for the Hawai‘i climate divisions); `modeled` marks the units
  with projections.
- `data/` — hive-partitioned parquet for dashboards:
  - `daily/land_id=…/` — raw daily NEX-GDDP-CMIP6 values in NASA's native
    units (K, kg m⁻² s⁻¹, m s⁻¹, %, kg/kg, W m⁻²), every unit.
  - `annual/land_id=…/` and `smoothed/land_id=…/` — annual metrics per
    model, and the GAMLSS percentiles (`p05`…`p95`), in US units, one land
    per partition.
  - `annual-by-variable/variable=…/scenario=…/` and
    `smoothed-by-variable/…` — the same tables partitioned for maps of one
    variable across all lands. Every partition is a single
    `part-0.parquet` (at most ~6 MB).
  - `variables.parquet` (variable id → label, family, type) and
    `lands.parquet` (land id → name).
- `projections-manifest.json` — every file with its size (and sha256 for
  the zips and PDFs); `_manifest.txt` — one download URL per line.

No credentials are needed:

``` bash
aws s3 ls s3://native-resilience/projections/ --no-sign-request
aws s3 cp "s3://native-resilience/projections/pdf/Acoma Pueblo_CMIP6-climate-projections.pdf" . --no-sign-request
```

``` sql
-- DuckDB: one land, every variable
SELECT * FROM read_parquet(
  'https://data.native-resilience.com/projections/data/smoothed/land_id=0305R/part-0.parquet');

-- DuckDB: one variable across all lands
SELECT land_id, Year, p50 FROM read_parquet(
  'https://data.native-resilience.com/projections/data/smoothed-by-variable/variable=temp_mean/scenario=ssp245/part-0.parquet');
```

## Running the pipeline

| Script | What it does |
|----|----|
| `projections-boundaries.R` | Builds `native-land.parquet` from the latest census-aiannh vintage, the Navajo agencies and the Hawai‘i climate divisions |
| `projections-extract.R` | Extracts NEX-GDDP-CMIP6 for every unit and assembles `data/daily/`. Run on EC2 in us-west-2 next to `s3://nex-gddp-cmip6` |
| `projections.R` | Annual metrics, gamlss2 smooths, workbooks, graphs, zips and the dashboard tables |
| `projections-families.R` | Compares candidate distribution families per variable on a sample of lands (`_state/family-selection.csv`) |
| `projections-publish.R` | Mirrors `upload/` and `data/` to S3, verifies, writes the manifests and invalidates the CDN |
| `projections-map.R` | Writes the web map's GeoJSON and search index |

Every stage is incremental. A ledger of fingerprints
(`_state/ledger.parquet`, mirrored to S3) records what each output was
built from: NASA's file checksums and each land's geometry for the
extraction, the metric definition and the smoothing settings for the
annual and smoothed tables. A rerun does only what changed. Environment
variables steer a run:

- `NR_STAGES` — e.g. `annual,smooth` (default: every stage of the script)
- `NR_LANDS` — comma list of `land_id`s
- `NR_FORCE` — stages to rebuild regardless of the ledger
- `NR_WORKERS` — parallel workers ([mirai](https://mirai.r-lib.org)
  daemons driven by `purrr::in_parallel()`)
- `PUBLISH=0` — build locally without reading or writing S3

**Running on EC2.** The extraction reads ~29,000 yearly global NetCDF
files (~7 TB), so it and the modeling run in us-west-2, where reading
`s3://nex-gddp-cmip6` is free and fast. `ec2/` holds everything needed:

``` bash
aws sso login --profile mco
ec2/launch.sh extract       # projections-extract.R: NEX extraction + daily files (~4 h)
ec2/launch.sh projections   # projections.R: metrics, smooths, outputs, dashboard (~2.5 h)
NR_EC2_DRY_RUN=1 ec2/launch.sh projections   # validate the request only
```

`launch.sh` starts a self-terminating instance that clones the current
branch (it must be pushed), runs the script in `rocker/geospatial` with one
worker per core, and streams its log to `projections/_logs/`. Check on a run
with `aws s3 cp s3://native-resilience/projections/_logs/<stage>-status.txt -
--profile mco`. A successful run terminates its instance; a failed one stops
it (terminate it once you have looked). Both scripts are resumable: relaunch
after any failure and the ledger skips finished work. Zips and PDFs from
`projections` go to `projections/_staging/` for review before publishing.

The defaults fit the MCO landing-zone account: private subnets with egress
through the transit gateway, the S3 gateway endpoint, and
`EC2-Default-SSM-AD-Role` carrying the inline policy in
`ec2/iam-policy.json` (write access to `native-resilience/projections/*`
only). The scripts set `S3_AMBIENT_CREDENTIALS=1` so the S3 helpers use the
instance role instead of the local `mco` SSO profile.
`NR_TASKS="GFDL-ESM4/ssp245/2097"` limits `projections-extract.R` to
matching model/scenario/year tasks for a smoke test.

**Adding a metric.** Append one `metric()` entry to `R/metrics.R` (an id,
label, an expression over the day's data in US units, a distribution
family and type) and rerun `projections.R`. Only the new metric is
computed, smoothed, added to every workbook and graph set, and written to
the dashboard tables. Bump a metric's `version` when its definition
changes to rebuild it everywhere.

## Web map

The searchable map at <https://native-resilience.github.io/projections/>
(embedded at <https://native-resilience.com/projections/>) is a static
page in `docs/`, served by GitHub Pages from the `main` branch. It follows
the Montana Climate Office web style
([mt-climate-office/mco-web-style](https://github.com/mt-climate-office/mco-web-style),
pinned at `@0.6.0`) and uses MapLibre GL 5 with the CARTO vector basemaps.

- `docs/index.html`, `docs/app.js`, `docs/app.css` — hand-written page;
  not generated.
- `docs/data/native-land.geojson` and `docs/data/native-land-index.json` —
  simplified boundaries and the search index, written by
  `projections-map.R` from `native-land.parquet` (itself built by
  `projections-boundaries.R`). Re-run `Rscript projections-map.R` whenever the
  boundaries change; add `--check-links` to HEAD-check every
  PDF and zip URL.
- Preview locally with `python3 -m http.server 8000 --directory docs`.
- Bumping the kit or MapLibre version means new SRI hashes (kit README) and
  a recomputed anti-flash `sha256` in the page's CSP (kit `MIGRATING.md`).
- Pushing to `main` deploys to production.

**Please contact Dr. Kyle Bocinsky, Director of Climate Extension for
the Montana Climate Office with any technical questions about these
data:
[kyle.bocinsky@umontana.edu](mailto:kyle.bocinsky@umontana.edu?subject=CMIP6%20Tribal%20Projections).
Code for producing all data supplied here is freely available on Github:
<https://github.com/native-resilience/projections>.**
