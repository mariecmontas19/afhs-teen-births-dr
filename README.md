# Can Adolescent-Friendly Health Services Lower Teen Birth Rates? Evidence from a Staggered Roll-Out in the Dominican Republic

Replication code for the paper by **Marie C. Montás** (Department of Global Health and Population, Harvard T.H. Chan School of Public Health). Working paper; comments welcome at mariemontas@fas.harvard.edu.

## Abstract

The Dominican Republic has long had one of the highest adolescent birth rates in Latin America, and UNFPA estimates the opportunity cost of adolescent pregnancy at US$245 million annually, about 0.3 percent of GDP. The registered rate has fallen sharply in recent years, from 62 births per 1,000 women aged 15–19 in 2019 to 36 in 2025, but the decline coincided with a national expansion of adolescent services, and how much of it the services caused is unknown. Adolescent-friendly health services are the World Health Organization's standard for adolescent care, yet existing evidence concerns utilization rather than fertility. This paper estimates the causal effect of opening dedicated Adolescent Units inside public hospitals, confidential youth-oriented services providing contraception, reproductive-health counseling, and psychological and gynecological care, on adolescent fertility. It combines a newly assembled record of unit openings with 1.44 million registered births (2016–2025), census-consistent municipality population denominators, geolocated public facilities, and hospital-production and school administrative records. Identification exploits the staggered opening of the first unit in 20 previously unserved municipalities between 2020 and 2025; documented allocation rules targeted municipalities by adolescent population size, not by fertility rates. Heterogeneity-robust difference-in-differences estimators compare treated municipalities with not-yet-treated and never-treated municipalities across 146 municipalities, with event studies for pre-trends and dynamics and a within-municipality triple difference against women aged 30–34. A municipality's first unit reduces the adolescent birth rate by 6.43 births per 1,000 women (p = .002), 12 percent of the pre-opening mean, with no differential pre-trends. Births are unchanged for nine months after opening and then decline, reaching 17 percent below baseline by the second year. Reductions concentrate at ages 18 and 19 and extend to ages 20 and 21 as exposed cohorts age out; women aged 30–34 show no change. Independent hospital registries show a 17 percent decline in adolescent pregnancy events, and abortion-related attendances fall in proportion to births. Effects are larger where adolescents live closer to public hospitals (10.2 versus 2.7 births per 1,000). Secondary-school enrollment rises by 1.5 percentage points. The estimates imply roughly 3,333 averted adolescent births and break-even operating costs of about US$48,000 per municipality-year using health-system savings alone and US$560,000 using broader social costs.

## What is in this repository

```
code/R/                 114 R scripts, numbered by stage; 00_run_all.R sources them in order
docs/                   codebooks and public reference data
replication_package/    municipality population denominators, Series A and B (public, derived from NSO data)
```

The scripts included are exactly those on the dependency path of the paper's exhibits: the 8 main-text tables, 21 appendix tables, and 17 generated figures (the photographs in Figures 2 and B6 are not script output), plus every upstream script that builds an input those exhibits use. Exploratory analyses that were run during the project but do not enter the paper are not included. Restricted data (`data/`) and generated exhibits (`output/`) are excluded by `.gitignore`; see *Data access*.

## Pipeline

Every script starts with `source("code/R/00_config.R")`, which sets paths relative to the project root (via the `here` package) and shared constants; `00_theme.R` holds the figure theme. Scripts are numbered by stage and run in order; `code/R/00_run_all.R` sources all of them.

| Stage | Scripts | What they do |
|---|---|---|
| 01 | `01_crosswalk`, `01b_*` | Canonical municipality key (ADM3 pcode) and name-matching helpers |
| 02 | `02_population_projection`, `02b`–`02d`, `02f` | Municipality population denominators: Hamilton–Perry cohort-change ratios launched from the 2022 census and raked to NSO province projections (Series A); intercensal interpolation (Series B); the supplementary projections volume and Table A20 |
| 03 | `03_clean_births` | Cleans the birth-registry microdata (restricted) to municipality × age × year counts |
| 04 | `04_treatment`, `04b`, `04c`, `04g`–`04i` | Unit opening dates and their provenance (Table A21), first-opening vs. modernization treatment, allocation and adoption-timing tests (Table 2) |
| 05 | `05_covariates`, `05b`–`05x2` | Baseline covariates and auxiliary data: census wealth index, health-facility registry and distance, DHS/MICS benchmarks, SISALRIL/SeNaSa and SIUBEN, JEE school-day expansion, SNS Form 67-A pregnancy registries, MoE enrollment and Pruebas Nacionales, family-questionnaire moderators |
| 06 | `06_build_panel`, `06b_monthly_panel` | Municipality × year and municipality × month estimation panels |
| 07 | `07t_headline`, `07s_triplediff_ddd`, `07t2`, `07a4`, `07b`, `07p`, `07y`, `07z5`, `07z6` | Callaway–Sant'Anna headline and event studies (Figure 4), triple difference (`triplediff`), Table 1, HonestDiD sensitivity (Table A4), first-access vs. upgrade decomposition (Table A17), distance panel, gestational lag (Figure 5) |
| 08 | `08_estimators`, `08l_het_master`, `08n_byage`, `08x_bridge_cs`, `08b`–`08z9` | Alternative estimators (Table A1, Figure B3), heterogeneity and proximity (Figure 6, Table A6), age decomposition (Table 4, A7, A8), pregnancy/abortion bridge (Table 5), newborn health (A5), cohorts (A16), schooling outcomes (Table 6), mechanisms |
| 09 | `09_robustness`, `09b`–`09m` | Robustness battery, registration-ramp and undercount checks (A12), denominators (A19), functional form (A18), cost-effectiveness thresholds (Table 8), matched controls (A10–A11), COVID, alternative treatment dates, DDD variants |
| 10 | `10_tables`, `10_*` | Typesets the paper tables from the numeric outputs and writes a manifest of all table files |
| 99 | `99_validate_pipeline` | Independent end-to-end audit of the clean datasets (90 checks) |

Standalone `fig3b_motivation.R`, `fig_lac_ranking_2018.R`, `fig_pcua_rollout.R`, and `fig_proximity_coef.R` draw Figures 1, B1, B2, and 6b.

## Software

R 4.3.3. Key packages and the versions used: `did` 2.1.2, `triplediff` 0.2.4, `fixest` 0.12.1, `did2s` 1.0.2, `didimputation` 0.3.0, `data.table` 1.17.8, `tidyverse` 2.0.0, `sf` 1.0.21, `sandwich` 3.1.1, `ggplot2` 3.5.2, `patchwork` 1.3.0, `readxl` 1.4.3, `here` 1.0.1. Scripts that read Excel files with accented names (`03`, `04`) must run under a UTF-8 locale (`LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8`). Seeds are set before every bootstrap call.

## Data access

The outcome data are the individual-level live-birth registry (BDNV) of the Dominican Republic's Ministry of Public Health (Ministerio de Salud Pública, MSP), provided under a data-use agreement with the Dirección de Análisis de Situación de Salud (DASIS). The microdata cannot be redistributed, and neither the raw files nor the cleaned datasets derived from them are in this repository. Researchers can request access by writing to DASIS at the Ministry of Public Health (Santo Domingo), describing the research purpose, variables, and years needed; requests for aggregate public statistics can also be filed through the Dominican Republic's public-information portal (Sistema de Acceso a la Información Pública, SAIP, under Law 200-04).

Public inputs and their sources: National Statistics Office (ONE) population estimates, projections, and the 2010 and 2022 censuses (one.gob.do); National Health Service (SNS) facility registry and hospital-production statistics (sns.gob.do); Ministry of Education (MINERD) per-center enrollment, Jornada Escolar Extendida records, and Pruebas Nacionales results (minerd.gob.do, some via public-information requests); ENDESA 2013 (DHS Program) and ENHOGAR-MICS 2019 (ONE/UNICEF); World Bank WDI.

Included here: `replication_package/pop_municipio_2016_2025_A.csv` and `_B.csv` (the full municipality × sex × age × year denominators, 52,700 rows each; see `replication_package/README.md`), `docs/au_opening_dates.csv` (the 39 Adolescent Units with opening and modernization dates), and codebooks in `docs/`.

## Citation

Montás, M. C. (2026). *Can Adolescent-Friendly Health Services Lower Teen Birth Rates? Evidence from a Staggered Roll-Out in the Dominican Republic.* Working paper, Harvard T.H. Chan School of Public Health.
