# Can Adolescent-Friendly Health Services Lower Teen Birth Rates? Evidence from a Staggered Roll-Out in the Dominican Republic

Replication code for the paper by Marie C. Montás, Department of Global Health and Population, Harvard T.H. Chan School of Public Health (mariemontas@fas.harvard.edu).

## Abstract

Adolescent-friendly health services are the World Health Organization's global standard for adolescent care, yet national-scale causal evidence on their effects on fertility remains scarce. I evaluate the Dominican Republic's staggered expansion of dedicated adolescent units inside public hospitals using a newly assembled record of opening dates, 1.44 million registered births from 2016 to 2025, and heterogeneity-robust difference-in-differences models across 146 municipalities. Opening a municipality's first adolescent unit reduces the birth rate among women aged 15–19 by 6.4 births per 1,000, or 12 percent of the pre-opening mean. The effect appears only after a nine-month gestational lag, reaches 17 percent by the second year of exposure, is concentrated in conceptions from age 17, and extends into ages 20 and 21 as treated cohorts age out of adolescence. Independent hospital-production registries show a 17 percent decline in adolescent pregnancy events, while abortion-related attendances also fall, consistent with fewer pregnancies rather than a change in reporting or substitution toward termination. Effects are larger where adolescents live closer to public health facilities, and the estimates remain negative across alternative estimators, samples, treatment dates, and population denominators. Across treated municipalities, the estimates imply around 3,300 averted adolescent births, and benchmark break-even calculations show how much the program could cost before these benefits are exhausted.

*JEL:* I12, J13, I15, I18

## Contents

```
code/R/                 117 R scripts, numbered by stage; 00_run_all.R runs them in order
docs/                   codebooks and the adolescent-unit opening dates
replication_package/    municipality population denominators, Series A and B
```

Restricted data (`data/`) and generated exhibits (`output/`) are not included.

## Pipeline

Each script sources `00_config.R`, which sets paths relative to the project root and shared constants. `00_theme.R` holds the figure theme.

| Stage | What it does |
|---|---|
| 01 | Municipality key (ADM3 pcode) and name matching |
| 02 | Population denominators: Hamilton–Perry projections raked to NSO province totals (Series A) and intercensal interpolation (Series B); Table C1 |
| 03 | Birth-registry microdata to municipality × age × year counts (restricted input) |
| 04 | Unit opening dates and provenance (Table C2), first-opening and modernization treatment, allocation tests (Table 2) |
| 05 | Covariates and auxiliary data: census, facility registry and distances, DHS and MICS, SeNaSa and SIUBEN, extended school day, hospital pregnancy registries, school records |
| 06 | Municipality-year and municipality-month panels |
| 07 | Headline estimates and event studies (Table 3, Figure 4), triple difference, Table 1, placebo and sensitivity tests (Table A4), first-opening vs. upgrade decomposition (Table A17), gestational lag (Figure 5) |
| 08 | Alternative estimators, heterogeneity and proximity (Figure 6), age profile (Table 4), pregnancy and abortion registries (Table 5), schooling (Table 6), newborn health, mechanisms |
| 09 | Robustness, registration and denominator checks, matched controls, break-even thresholds (Table 8) |
| 10 | Typesets the tables from the numeric outputs |
| 99 | End-to-end audit of the clean datasets |

The `fig_*.R` scripts draw Figures 1, 3a, 6b, B1 and B2. Every paper figure is saved as PNG and vector PDF.

## Software

R 4.3.3 with `did` 2.1.2, `triplediff` 0.2.4, `fixest` 0.12.1, `did2s` 1.0.2, `didimputation` 0.3.0, `data.table` 1.17.8, `sf` 1.0.21, `ggplot2` 3.5.2 and `here` 1.0.1. Run under a UTF-8 locale (`LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8`). Seeds are set before every bootstrap call.

## Data access

Births come from the live-birth registry (BDNV) of the Dominican Republic's Ministry of Health, provided by its Dirección de Análisis de Situación de Salud (DASIS) under a data use agreement; it cannot be redistributed. Other inputs are public: National Statistics Office (ONE) estimates, projections and the 2010 and 2022 censuses; National Health Service (SNS) facility and hospital-production records; Ministry of Education (MINERD) school records; ENDESA-DHS 2013; ENHOGAR-MICS 2019 and 2025; and the World Bank's WDI.

This repository includes the municipality × sex × age × year population denominators (`replication_package/`, described in its README), the opening and modernization dates of the 39 adolescent units (`docs/au_opening_dates.csv`), and codebooks.

## Citation

Montás, M. C. (2026). *Can Adolescent-Friendly Health Services Lower Teen Birth Rates? Evidence from a Staggered Roll-Out in the Dominican Republic.* Working paper, Harvard T.H. Chan School of Public Health.
