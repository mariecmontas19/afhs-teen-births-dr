# Master Data Dictionary & Variable Plan — DR AU Adolescent Births

Every variable we plan to build, its coding/recode, and its **role**, across ALL databases — plus a
"candidate additions / gaps" hunt. **Agree on this before scripts are written.** Detailed BDNV coding
rules also in `births_variable_coding.md`; this is the master across all sources.

**Verification status:** ✔=value coding verified against the data · ◑=column/label verified, value
coding still to confirm against codebook · ☐=from prior inspection/agent, re-verify on use.
**Roles:** ID · DENOM(inator) · TREATMENT · OUTCOME · CONTROL(baseline, time-invariant) ·
HET(erogeneity split) · MECHANISM(outcome showing the channel).

---

## 1. BDNV births microdata (`03_clean_births.R`) — one row per live-born baby, 2016–2025
| Raw column | Clean var | Coding | Role | Status |
|---|---|---|---|---|
| Vivo_Edad_Madre | `age_mom`, `age_grp` | num. ERROR FIX (verified counts): ages **0,1,9 → recode 10** (43 recs, into 10–14); ages **55,68 → into 45+** (17 recs). Groups: **10–14, 15–19, 20–24, 25–29, 30–34, 35–39, 40–44, 45+** where **45+ = 45–54** (denom = women 45–49 + 50–54). Pre-existing NA age (27,257, mostly 2016–17) stays NA. Outcome=15–19 & 10–14; comparison=30–34 (alt 25–29/20–24) | DENOM key / sample / full ASFR schedule | ✔ |
| Vivo_Fecha_nacimiento_Hijo | `birth_date`,`birth_year`,`birth_month` | parse date | ID/time | ✔ |
| Provincia/Municipio madre | `adm3_pcode` (residence) | crosswalk; fold the 3 children for A/B | ID/geography | ✔ 100% match |
| Vivo_Fecha_Nac_Madre | (age check) | cross-check age = birth−DOB | QC | ◑ |
| **OUTCOMES (next generation, all years):** | | | | |
| peso en gramo | `weight_g`,`low_bw`(<2500) | keep 300–6500g else NA | OUTCOME | ✔ |
| Edad gestacional | `gestage_wks`,`preterm`(<37) | parse "## Semanas"; keep 20–44 else NA | OUTCOME (preterm) + input to conception & SGA; **NOT a control** (on causal path to weight) | ✔ |
| (weight+gestage+sex) | `sga` (<10th pctile weight-for-GA & sex) | sample-internal percentiles (or INTERGROWTH-21st if pkg verified) | OUTCOME (refined: growth restriction vs prematurity) | build in 03 |
| Riesgo neonatal | `neo_risk`(ord),`neo_high`(=Alto) | Se Ignora/No Evaluado→NA | OUTCOME | ✔ |
| TIPO_PARTO_DESCRIPCION | `csection` | 1=Cesárea | OUTCOME (care) | ✔ 66% |
| Talla al nacer | `length_cm` | outlier rule | OUTCOME (2ndary) | ◑ |
| PERIMETRO_CEFALICO_DESC | `head_circ` | outlier rule | OUTCOME (2ndary) | ◑ |
| Sexo del bebe | `sex_baby` | M/F | **CONTROL in newborn-health outcome models** (boys heavier) + sex-ratio descriptive | ✔ |
| **DERIVED (all years):** | | | | |
| (birth_date − gestage) | `conception_date`,`conception_ym` | birth_date − gestage_wks×7 | TIMING (9-mo lag test) | ✔ feasible |
| **MECHANISM / composition (limited years):** | | | | |
| ESCOLARIDAD | `educ` None/Primary/Secondary/University | collapse 2 secundario→Secondary; SE IGNORA→NA | MECHANISM/HET | ✔ 2020–25 |
| Estado Conyugal Madre | `in_union`(UNIÓN LIBRE/CASADO=1) | | MECHANISM/HET | ✔ 2021–25 |
| Ars_Madre | `insurance`(PUBLIC/AUTOGESTIÓN/PRIVATE/NONE),`has_insurance`,`subsidized`(SENASA) | per do-file; carry 2018→2016–17 | CONTROL(SES)/MECH | ✔ 2018–25 |
| Nacionalidad de la madre | `nationality` Dominican/Haitian/Other | | HET / sample (robustness Dominican-only) | ✔ |
| Vivo_Cantidad_Chequeos_Prenatales | `prenatal_checks` | >20→NA | MECHANISM (care) | ✔ 2021–25 |
| Mes último chequeo embarazo | `last_check_month`(1–12); `gest_at_last_check` | month→num; gestational timing (±1 mo) | MECHANISM (care) | ✔ 2021–25 |
| (prenatal_checks + timing + gestage) | `prenatal_adequacy` (Kotelchuck-style) | | MECHANISM (care) | 2021–25 |
| Ocupación Habitual Madre | — | **DROP** (930 values, 65–70% missing post-2021) | — | ✔ dropped |
| Lugar ocurre parto | `facility` public/private/home/other | public-vs-private | **MECHANISM outcome** (does AU shift teens to public?) + descriptive; NOT a control (mediator) | ✔ |
| Parto atendido por | `doctor_attended` | Obstetra/General=1 | descriptive only (99% → ceiling, no usable variation) | ✔ |
| Tipo de producto | `multiple` | 1=Múltiple | QC / robustness | ✔ ~2% |
| Municipio atendió parto | `adm3_pcode_delivery` | (keep for spillover/travel analysis) | spillover check | ✔ |

## 2. Population denominators (`02*`, DONE) — DENOM
Series A (ONE official), B (intercensal 2010↔2022), census-anchored — municipio×age×sex×year 2016–2025.
Teen-birth rate = births(age grp) ÷ female pop(age grp). Already built & audited (44/44).

## 3. 2022 census Persona (`05_covariates`) — contextual covariates & heterogeneity
Aggregate to municipio (and province) — **time-invariant; for CONTROL (CS baseline) + HET + descriptives.**
| Raw | Built (municipio %) | Role | Status |
|---|---|---|---|
| ZONA | `pct_urban` | CONTROL/HET (urban vs rural) | ◑ |
| P41 | `literacy_rate` | CONTROL(SES) | ◑ |
| P43/P44 | `educ_secondary_plus` (women 20–49) | **CONTROL (the education control you want)** | ◑ |
| P48/P49 | `internet_access`,`computer` | CONTROL(SES/info) / HET | ◑ |
| P53–P63 | `lfp_female`,`employment` | CONTROL(SES) | ◑ |
| **P64** | `pct_afrodescendant` (race self-ID) | **HET — Galárraga-analog (like indigenous concentration)** | ◑ confirm categories |
| P65 | `children_ever_born`,`child_woman_ratio` | fertility context / HET | ◑ |
| P65–P66 | `child_survival` (P66/P65) | child-mortality proxy / context | ◑ |

## 4. 2010 census Persona+Hogares (`05`) — PRE-TREATMENT baseline (2010) covariates
Same constructs as 2022 (literacy P35, education P37, employment P45–P54, civil status P55, **fertility
P56/P57 children ever born**, child survival P59, migration P33/P42–44). **2010 = clean pre-treatment
baseline** for CS conditional parallel trends (before all in-window openings). Role: CONTROL/HET. ◑

## 5. Vivienda/Hogar census 2022 (+2010) (`05`) — WEALTH INDEX
Wall(3)/roof(4)/floor(5) material, sanitation(11), garbage(14), bedrooms(17→crowding), cooking fuel(18),
water, electricity, assets. → **PCA asset/wealth index per municipio** (DHS-style). Role: CONTROL(SES)/HET. ◑

## 6. Income survey 2018 (`05`) — SES
QUINTIL (income quintile), ESTRATO, GRUPO_EDUCACION, employment, FACTOR_EXPANSION (weight). Aggregate
to **province (robust) and municipio (noisy)**, weighted. Role: CONTROL(SES)/HET. ✔ exists

## 7. Mortality (`08`/stretch) — OUTCOMES
MI infant-death files (2019–23, municipio, no mother age→needs linkage) → neonatal/infant mortality;
MM maternal-death files (incl. ages 10–19, municipio) → adolescent maternal mortality. Role: OUTCOME (stretch). ☐ re-verify

## 8. AU treatment (`04`) — TREATMENT
`Visits to PCUAS.xlsx`: opening year/month, municipio, # units, health region, services (verify services
detail). → first-opening cohort (binary absorbing), event-time, #units, distance-to-nearest. Role: TREATMENT. ✔ timing

## 9. Shapefile ADM3 (`04`) — geography
centroids→distance-to-nearest-AU; AREA_SQKM→density; maps. Role: TREATMENT(intensity)/descriptive. ✔

---

## 10. CANDIDATE ADDITIONS — am I missing anything important? (recommendations)
**Strongly recommend adding** (verified to exist, high value):
1. **% urban (ZONA)** — teen fertility differs sharply urban/rural; strong control + HET.
2. **Race/ethnicity (P64, 2022)** — a **direct Galárraga analog** (he used indigenous concentration). Enables an
   ethnicity-based DDD/heterogeneity — potentially a headline contribution. *(Confirm category coding.)*
3. **Census wealth index** (vivienda) — robust, full-coverage municipio SES (better than survey quintile alone).
4. **Internet/ICT access (P48/P49)** — SES + information-access proxy (relevant to SRH knowledge).
5. **Fertility context (P65 child-woman ratio)** + **child-survival proxy (P65–P66)** — context & a mortality angle.
6. **Birth length & head circumference (BDNV)** — extra newborn-health outcomes at ~zero extra cost.
7. **Delivery municipio (BDNV)** — lets us measure cross-municipio travel for delivery (spillover evidence).

**Consider / lower priority:**
8. Literacy (P41), female LFP (P53–63) — SES context (may be collinear with education/wealth).
9. Disability (P40) — likely not relevant to teen fertility; skip.
10. Mortality outcomes — stretch (linkage risk).

**Confirmed NOT available / dropped:** parity/birth-order (not in BDNV); occupation (dropped).

## 11. AGREED decisions (2026-06-19)
- **Race/ethnicity (P64):** build municipio % Afro-descendant; use as **covariate + heterogeneity split
  + descriptive** (NOT a DDD dimension — our DDD is already age×location; a 4th difference is unwieldy).
- **Census contextual covariates to build (05):** % urban (ZONA), census wealth index (PCA), internet/ICT
  (P48/P49), fertility context (child-woman ratio, P65), education (P43/P44) — all aggregated to municipio
  (+province), time-invariant baselines (2010 pre-treatment + 2022).
- **Birth length + head circumference (BDNV):** YES, secondary newborn-health outcomes (with outlier rules).
- **Delivery municipio (BDNV):** keep, to measure cross-municipio travel-to-deliver (spillover evidence).
- **SES approach:** **census wealth index = PRIMARY municipio SES** (full-coverage, municipio-representative);
  **income-survey quintile = PROVINCE-level robustness/cross-check** (survey isn't municipio-representative).
  Do NOT blend into one composite at municipio level (would dilute the reliable census measure with survey
  noise). Validate municipio-wealth ↔ province-income correlation. [pending final user confirm]
- **Education control:** municipio % women 20–49 secondary+ from census (2010 baseline + 2022); births-
  education kept as MECHANISM outcome (2020+).
- **Dropped/absent:** parity (not in BDNV), occupation (930 values, heavy missing).

## 12. Build order
03_clean_births (BDNV, this includes births-education/insurance/union/prenatal/conception/length/head-circ)
→ 04_treatment (AU timing + distance) → 05_covariates (census contextual: education, % urban, wealth
index, internet, fertility, race; income-survey province SES; insurance carry-back) → 06_build_panel.
Before building 05, CONFIRM value codings (◑): P43/P44 education levels, P64 race categories, ZONA urban
code, vivienda asset codes — verify against codebooks, don't assume.
