# Can Adolescent-Friendly Health Services Lower Teen Birth Rates? Evidence from a Staggered Roll-Out in the Dominican Republic

Replication code for the paper by **Marie C. Montás** (Department of Global Health and Population, Harvard T.H. Chan School of Public Health). Working paper; comments welcome at mariemontas@fas.harvard.edu.

## Abstract

Adolescent-friendly health services are the World Health Organization's standard for adolescent care, yet causal evidence on fertility at national scale is scarce. This paper evaluates the staggered opening of dedicated Adolescent Units (Unidades de Atención Integral para Adolescentes) inside public hospitals in the Dominican Republic. Combining a newly assembled record of unit openings with 1.44 million registered births (2016–2025), census-consistent municipality population denominators, geolocated public facilities, and hospital and school administrative records, heterogeneity-robust difference-in-differences estimators compare 20 municipalities that received their first unit in 2020–2025 with not-yet-treated and never-treated municipalities (146 municipalities). A first unit reduces the adolescent birth rate by 6.43 births per 1,000 women aged 15–19 (12 percent of the pre-opening mean), with no pre-trends, a nine-month gestational lag, effects concentrated at ages 18–19, a parallel decline in independently recorded pregnancy events, and larger effects where adolescents live closer to public hospitals.

## Repository layout

```
code/R/                 numbered R pipeline (00_config -> 10_tables); 00_run_all.R runs the core path
docs/                   codebooks and public reference data (see below)
replication_package/    municipality population denominators, Series A and B (public, derived from NSO data)
```

Restricted data (`data/`) and generated exhibits (`output/`) are excluded from this repository by `.gitignore`; see *Data access*.

## Pipeline

Every script starts with `source("code/R/00_config.R")`, which sets paths relative to the project root (via the `here` package) and shared constants; `00_theme.R` holds the figure theme. Scripts are numbered by stage and run in order:

| Stage | Scripts | What they do |
|---|---|---|
| 01 | `01_crosswalk`, `01b_*` | Canonical municipality key (ADM3 pcode) and name-matching helpers |
| 02 | `02_population_projection`, `02b`–`02f` | Municipality population denominators: Hamilton–Perry cohort-change ratios launched from the 2022 census, raked to NSO province projections (Series A); intercensal interpolation (Series B); validation and appendix volume |
| 03 | `03_clean_births` | Cleans the birth registry microdata (restricted) to municipality × age × year counts |
| 04 | `04_treatment`, `04b`–`04i` | Unit opening dates, first-opening vs. modernization treatment, allocation and adoption-timing tests (Table 2) |
| 05 | `05_covariates`, `05b`–`05x2` | Baseline covariates: census wealth index, health-facility registry and distance, DHS/MICS benchmarks, JEE school-day expansion, SNS Form 67-A pregnancy registries, MoE enrollment and Pruebas Nacionales |
| 06 | `06_build_panel`, `06b_monthly_panel` | Municipality × year (and monthly) estimation panels |
| 07 | `07_eventstudy`, `07s_triplediff_ddd`, `07t_headline`, `07a`–`07z7` | Callaway–Sant'Anna event studies, triple difference (`triplediff`), placebo ages, gestational-lag, descriptives |
| 08 | `08_estimators`, `08l_het_master`, `08n_byage`, `08x_bridge_cs`, `08b`–`08z13` | Alternative estimators, heterogeneity (proximity), age decomposition, pregnancy/abortion bridge, schooling outcomes, mechanisms |
| 09 | `09_robustness`, `09b`–`09m` | Robustness battery, cost-effectiveness thresholds, matching, COVID, denominators |
| 10 | `10_tables`, `10_*` | Typesets all paper tables from the numeric outputs |
| 99 | `99_validate_pipeline` | Independent end-to-end audit of the clean datasets |

`code/R/00_run_all.R` sources the core path (crosswalk → denominators → births → treatment → covariates → panel → main estimates → tables). Standalone `fig_*.R` scripts draw the motivation and appendix figures.

## Software

R 4.3.3. Key packages and the versions used: `did` 2.1.2, `triplediff` 0.2.4, `fixest` 0.12.1, `did2s` 1.0.2, `didimputation` 0.3.0, `data.table` 1.17.8, `tidyverse` 2.0.0, `sf` 1.0.21, `sandwich` 3.1.1, `ggplot2` 3.5.2, `patchwork` 1.3.0, `readxl` 1.4.3, `here` 1.0.1. Scripts that read Excel files with accented names (`03`, `04`) must run under a UTF-8 locale (`LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8`). Seeds are set before every bootstrap call.

## Data access

The outcome data are the individual-level live-birth registry (BDNV) of the Dominican Republic's Ministry of Public Health (Ministerio de Salud Pública, MSP), provided under a data-use agreement with the Dirección de Análisis de Situación de Salud (DASIS). The microdata cannot be redistributed, and neither the raw files nor the cleaned datasets derived from them are in this repository. Researchers can request access by writing to DASIS at the Ministry of Public Health (Santo Domingo), describing the research purpose, variables, and years needed; requests for aggregate public statistics can also be filed through the Dominican Republic's public-information portal (Sistema de Acceso a la Información Pública, SAIP, under Law 200-04).

Public inputs and their sources: National Statistics Office (ONE) population estimates, projections, and the 2010 and 2022 censuses (one.gob.do); National Health Service (SNS) facility registry and hospital-production statistics (sns.gob.do); Ministry of Education (MINERD) per-center enrollment, Jornada Escolar Extendida records, and Pruebas Nacionales results (minerd.gob.do, some via public-information requests); ENDESA 2013 (DHS Program) and ENHOGAR-MICS 2019 (ONE/UNICEF); World Bank WDI.

Included here: `replication_package/pop_municipio_2016_2025_A.csv` and `_B.csv` (the full municipality × sex × age × year denominators, 52,700 rows each; see `replication_package/README.md`), `docs/au_opening_dates.csv` (the 39 Adolescent Units with opening and modernization dates), and codebooks in `docs/`.

## Citation

Montás, M. C. (2026). *Can Adolescent-Friendly Health Services Lower Teen Birth Rates? Evidence from a Staggered Roll-Out in the Dominican Republic.* Working paper, Harvard T.H. Chan School of Public Health.
