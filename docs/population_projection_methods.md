# Municipio Population Denominators — Methods Note

Documentation for the paper's data appendix. Describes how we build municipio ×
age-group × sex × year (2016–2025) population denominators. Status flags:
**[DONE]** = implemented & validated in `code/R/02_population_projection.R`;
**[PLANNED]** = designed, not yet coded.

## Why we must build this
DR's national statistics office (ONE) publishes municipio population only through
**2020** (file `cuadro_poblacion_municipio_2015 a 2020.xlsx`, ONE estimates by
5-year age group and sex). For the event study we need denominators through 2025
at the municipio level. We therefore project 2021 and 2023–2025, anchored on the
**2022 census** (microdata, `BD PERSONAS XCNPV.csv`), and rake to ONE's official
**province** projections (2000–2030).

## Inputs and identifier reconciliation [DONE]
Three identifier systems are reconciled onto one canonical key, the shapefile
**ADM3 pcode** (158 municipios), via `data/clean/muni_crosswalk.rds`:
- municipio xlsx (ONE estimates, by sheet name) — 155 municipios (3 created after 2020);
- 2022 census microdata (ONE numeric province/municipio codes, no names);
- shapefile ADM3 (OCHA pcodes + names).
The census carries only codes. We map census (prov_code, muni_code) → adm3_pcode using
ONE's **sheet order** as the authoritative municipio numbering (validated for 153/155),
with an **optimal population re-match** for the one province where sheet order and census
code disagree (Santo Domingo: Pedro Brand ↔ Los Alcarrizos swapped). See "matching
diagnostic" below.

## The 2022-vs-2020 ratio: a matching diagnostic (not a data-quality target)
To confirm each 2015–2020 xlsx municipio is paired with the *correct* 2022 census
municipio, we compute `ratio = census2022_pop / ONE_estimate_2020_pop` per municipio.
This ratio is **not expected to equal 1**, because it compares two different quantities:
- numerator = an actual **census head-count (2022)**;
- denominator = an **ONE projection (2020)** extrapolated from the **2010** census.
It therefore reflects (i) two years of real population change, (ii) projection-vs-census
error, and (iii) differential change across municipios (internal migration). The role of
the ratio is diagnostic: a **correct** municipio pairing yields ratio ≈ 1 (here all 155
land in **[0.78, 1.49]**, median 1.06); a **wrong** pairing yields a wild value (the
Santo Domingo swap produced 0.28 and 3.71), which is how we detected and fixed it.
National anchor (verified): ONE 2020 estimate 10,448,499 → 2022 census 10,773,879
(ratio 1.031, pop-weighted). The unweighted median municipio ratio (1.061) is higher
because the largest municipio, Santo Domingo Este, came in 11% below its 2020 projection
(1,161,393 → 1,029,116) — consistent with out-migration to the peri-urban ring and/or
the 2010-based projection over-extrapolating it.

## Denominator decision: build BOTH (A and B), decide main/robustness later
The 2022 census total is publicly contested (Dauhajre, El Caribe 2025, argues a ~14%
undercount). We therefore do NOT treat any single source as truth. We build the event
study under MULTIPLE denominator series and report robustness:
- **A — ONE official:** ONE municipio estimates 2016–2020 + ONE province projections
  2000–2030 distributed to municipios 2021–2025 via Hamilton–Perry (raked to ONE province
  projections). Does NOT use the 2022 census. 155 municipios (ONE doesn't separate the 3 new).
- **B — intercensal:** 2010↔2022 censuses interpolated to municipio×age×sex for 2016–2021,
  census 2022, Hamilton–Perry extrapolation 2023–2025. Uses both ACTUAL censuses.
- (Plus the 2022-census-anchored `02b` as a third variant.)

### Cohort diagnostic (02_diag_census_cohorts.R) — VERIFIED findings
- Total population: 2010 census 9,445,281; 2022 census 10,773,983; ONE-2020 estimate
  (10,448,499) ≈ 2010→2022 interpolation (10,552,533), ratio 0.990. All consistent on total.
- **Female cohort retention 2010→2022** (same birth cohort across the two real censuses):
  15-19=0.979, 20-24=0.972, 25-29=0.946, 30-34=0.932, 35-39=0.930. Highest (≈mortality only)
  for teens, lower for emigration-prone mid-adult cohorts → real attrition, NOT uniform
  undercount. The teen cohort tracks cleanly → the two raw censuses AGREE on teens.
- **Why ONE over-projects teens (VERIFIED mechanism):** ONE's reconciled 2010 base inflates
  young children vs the raw census (F 0-4 +8.5%, 5-9 +6.0% in 2010; total preserved) — a
  standard correction for young-child census undercount. Those reconciled children age into
  ONE's inflated 2015-2020 teen estimates. ONE-2020 F15-19 = 472,682 vs census-2022 = 426,598.
- Caveat: R≈0.98 shows the two censuses agree with EACH OTHER on teens; it cannot rule out a
  chronic undercount in both. For rates, a common/proportional denominator error is absorbed
  by year FE in the event study (affects descriptive LEVELS, not the treatment effect).

## Projection method [Part E — A: 02c (built); B: 02d (planned)]
**Hamilton–Perry cohort-change-ratio (CCR) method**, launched from the 2022 census and
**raked (IPF) to ONE's official province projections** each year (the standard NSO small-
area recipe; full cohort-component is infeasible without municipio fertility/mortality/
migration schedules). Refs to verify before citing: Smith, Tayman & Swanson, *A
Practitioner's Guide to State and Local Population Projections*; Hamilton & Perry (1962).
- 2016–2020 = ONE estimates; 2022 = census anchor; 2021 = interpolated; 2023–2025 =
  CCR-projected forward from 2022; all years raked to province totals.
- **Back-test:** launch from 2015–2018, predict 2020 & 2022, report MAPE/ALPE by age-sex.

## Declining-population municipios
- **Projection:** Hamilton–Perry handles decline naturally — a shrinking cohort has CCR < 1,
  which propagates continued decline. CAVEAT: CCRs can be unstable/extreme for small or
  volatile municipios; we bound/smooth extreme CCRs and the province-raking corrects
  municipio-level drift. [to implement & check in Part E]
- **Event study:** the outcome is a *rate* (teen births ÷ female population), with municipio
  fixed effects, so a declining denominator does not by itself bias estimates as long as the
  denominator is measured well. The **age triple-difference** is protective here: the
  comparison age group (30–34) in the *same* municipio shares that municipio's population
  trend and any denominator/projection error, so differencing nets it out. Genuine residual
  risks to state as limitations: (i) denominator measurement error is larger for volatile
  municipios (→ attenuation/noise; mitigated by population-weighted specs and the DDD);
  (ii) migration selection can change *who* is at risk of a teen birth; (iii) confounding if
  AU openings correlate with population growth/decline (→ checked via event-time
  pre-trends and HonestDiD). [general econometric reasoning; verify empirically in analysis]
