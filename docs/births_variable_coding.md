# BDNV births — proposed variable coding (for `03_clean_births.R`)

All proposals verified against the **pooled actual data** (all years, text-read) and cross-checked
against the old `analysis/dofile/cleaning_births.do`. Counts are pooled 2016–2025. **OPEN** = needs
your decision. **⚠️** = caveat. Normalize every categorical (uppercase + collapse spaces + strip
accents) BEFORE recoding — many categories appear in multiple cases/spellings across years (e.g.
occupation "Cocineros" vs "COCINEROS"), which the do-file's exact-string matching would miss.

## A. Can we use education/insurance/civil/occupation/prenatal as COVARIATES? (3 honest constraints)
1. **Year coverage** (verified): insurance 2018–25; education 2020–25; civil status / prenatal /
   occupation 2021–25. Most AU openings are 2020–24 → little/no pre-period for treated municipios.
2. **Bad-control risk:** these are *maternal characteristics that the AU itself could change*
   (selection into who gives birth — the Cruz "who becomes a parent" mechanism). Controlling for a
   post-treatment mediator **biases** the treatment effect. Your own notes say lag (t−1)/pre-period.
   → Don't use them as contemporaneous controls; if used, use **baseline/pre-period** values only.
3. **Municipio FE absorbs time-invariant covariates:** a 2018 municipio SES value is constant over
   time → dropped by municipio FE. So SES enters only via **heterogeneity (interactions)** or
   **descriptives/balance**, not as a within-municipio control.
→ **Recommendation:** capture all; use the composition vars as **outcomes** (where coverage allows)
   and for **descriptives/heterogeneity**; avoid them as time-varying controls.

## B. Income survey (2018) for SES — and "can we combine databases?"
- File = 2018 labor-force survey (ENCFT-type), 28,394 records, 1,132 vars. SES-ready columns:
  **QUINTIL** (income quintile 1–5), **ESTRATO**, GRUPO_EDUCACION, OCUPADO/DESOCUPADO,
  **FACTOR_EXPANSION** (survey weight), ID_MUNICIPIO/DES_MUNICIPIO, ID_PROVINCIA. (Raw income amount
  is in a coded D-series var; QUINTIL/ESTRATO suffice.)
- **Combine = municipio/province-level AGGREGATION, NOT individual linkage** (no common person ID;
  different samples/years). Build weighted SES per area: e.g. % in bottom quintile, mean stratum,
  share employed. Merge to the panel by area.
- **⚠️ Representativeness:** national surveys are usually representative at **province/region**, not
  municipio (sparse per municipio) → aggregate to **province (or region)** for stable SES; municipio
  estimates likely too noisy (verify n per municipio in `05`).
- **⚠️ Time-invariant (2018)** → absorbed by municipio FE (use for heterogeneity/descriptives).
- **OPEN:** SES geographic level — province (robust) vs municipio (granular but noisy)? Need to map
  ID_MUNICIPIO → adm3_pcode in `05` (verify it uses ONE codes).

## C. Per-variable coding (the cleaned birth-level file = one row per live-born baby)

| Births column | Clean var | Coding | Coverage / notes |
|---|---|---|---|
| `Vivo_Edad_Madre` | `age_mom` (num) → `age_grp` | groups 10–14,15–19,20–24,25–29,30–34,35–39,…; **teen=15–19**, secondary 10–14 | 9–50; **12% missing 2016–17** (those births can't enter an age group) |
| `Vivo_Fecha_nacimiento_Hijo` | `birth_date`,`birth_year`,`birth_month` | extract Y/M | complete, proper date |
| `Municipio madre`+`Provincia madre` | `adm3_pcode` (residence) | via `muni_match_helpers.rds`; fold San Víctor→Moca etc. for A/B (158 for census variant) | residence (decided), 100% matched |
| `Nacionalidad de la madre` | `nationality` | DOMINICANA→Dominican, HAITIANA→Haitian, else→Other; N/A→NA | 14% Haitian; 117 raw values |
| `peso en gramo` | `weight_g`; `low_bw`=weight<2500 | **outlier rule: keep 300–6500g; else NA** | mean 3,012; max 7,256 (error) |
| `Edad gestacional` | `gestage_wks` (parse `"39 Semanas"`→39); `preterm`=<37 | **keep 20–44 wks; else NA** | 44–46 wks likely errors; one "39Semanas" no-space |
| `Riesgo neonatal` | `neo_risk` ord (Bajo<Medio<Alto); `neo_high`=Alto | **Se Ignora / No Evaluado → NA** | composite clinical (asphyxia/LBW/preterm) |
| `TIPO_PARTO_DESCRIPCION` | `csection`=1 if Cesárea | clean | 66% c-section (real for DR) |
| `Tipo de producto` | `multiple`=1 if Múltiple | clean | ~2%; count each baby (unit decided) |
| `Sexo del bebe` | `sex_baby` (M/F) | clean | — |
| `Lugar ocurre parto` | `facility` (public/private/home/other) | clean | public 62%, private 38% |
| `Parto atendido por` | `doctor_attended`=1 if Médico Obstetra/General | clean | 99% doctor → low variation |
| `Ars_Madre` | `insurance` (PUBLIC / AUTOGESTION / PRIVATE / NONE); `has_insurance`; `subsidized`=SENASA | per do-file; **all 25 ARS values covered** (verified) | 2018–25; NO TIENE+N/D = none |
| `ESCOLARIDAD` | `educ` (ordinal, see below) | **OPEN ordering** | 2020–25; **SE IGNORA 23% → NA** |
| `Estado Conyugal Madre` | `in_union`=1 if UNION LIBRE/CASADO, 0 if SOLTERO/DIVORCIADO/VIUDO | matches do-file (inverted) | 2021–25; 76% unión libre |
| `Ocupación Habitual Madre` | `occ_grp` (see below) | **OPEN** | 2021–25; **930 values, case-split, 65–70% missing 2022+** |
| `Vivo_Cantidad_Chequeos_Prenatales_Embarazo` | `prenatal_checks` (num) | **outlier: >20 → NA** | 2021–25; max 750 (error) |
| `Mes último chequeo embarazo` | `last_check_month` (1–12) | month name → number (do-file map) | 2021–25; for adequacy/timing |

### Education (ESCOLARIDAD) — proposed ordinal, **needs your confirmation**
Raw (pooled): NINGUNO 65k · PRIMARIO(1-3) 20k · PRIMARIO(4-7) 51k · PRIMARIO COMPLETA(8vo) 38k ·
SECUNDARIO INCOMPLETO(1ro-3ero) 134k · SECUNDARIO INCOMPLETO(Bachiller) 230k · ESTUDIOS
UNIVERSITARIO 171k · SE IGNORA 201k (unknown).
Proposed 5-level ordinal: **0 None · 1 Primary (the 3 primary rows) · 2 Lower secondary (1ro-3ero) ·
3 Upper secondary / Bachiller · 4 University**; SE IGNORA → NA. Coarse binary alt: "≥ secondary".
**OPEN:** is "SECUNDARIO INCOMPLETO (Bachiller)" *above* "1ro-3ero" (my assumption) — the label is
internally contradictory ("incompleto" + "Bachiller"). You know the DR system; please confirm.

### Occupation — **OPEN (recommend not using as a control)**
930 free-text values, case-split, 65–70% missing after 2021. Options: (a) coarse keyword groups
(unskilled / domestic / skilled-manual / clerical / professional / student / homemaker / none);
(b) **drop** and rely on QUINTIL/education for SES (my lean); (c) crosswalk to the income survey's
GRUPO_OCUPACION for income imputation (labor-intensive, weak given missingness). Recommend (b),
with (a) only if you want a rough labor-type descriptor.

## D. Derived calculations you asked for
1. **Conception date:** `conception_date = birth_date − gestage_wks × 7`. Needs valid `gestage_wks`.
   Enables the 9-month gestational-lag falsification and conception-timing relative to unit opening.
2. **Gestational timing of last prenatal check ("pregnancy month"):** we only have the *calendar
   month* of the last check (no day/year). Infer its year so it falls in the ~9-month window before
   birth (e.g., a DICIEMBRE check before a FEB birth = prior year), then
   `gest_week_at_last_check = (last_check_date − conception_date)/7` → month of pregnancy at last
   check. **⚠️ ±~1 month imprecision** (month-only); 2021–25 only.
3. **Prenatal-care adequacy (Kotelchuck-style):** combine `prenatal_checks` + timing of last check +
   `gestage_wks` (enough visits for the gestation reached + reasonably late last check). 2021–25 only.

## E. Open decisions to confirm
1. Education ordering (Bachiller placement) — above.
2. Occupation handling — drop (recommended) vs coarse groups vs income-crosswalk.
3. SES geographic level — province (robust) vs municipio (noisy).
4. Covariate role — confirm: composition vars used as outcomes/descriptives/heterogeneity, NOT as
   time-varying controls (bad-control). SES from 2018 → heterogeneity/descriptives (FE-absorbed).
