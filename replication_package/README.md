# Replication deposit — municipality population projections

Supplementary data for **"Can Adolescent-Friendly Health Services Lower Teen Birth Rates?
Evidence from a Staggered Roll-Out in the Dominican Republic"** (Marie C. Montás, Harvard
T.H. Chan School of Public Health).

These files contain the complete municipality-level population estimates used as
denominators in the paper, referenced in Appendix C and typeset in full in the
supplementary volume *Population Projections for the Municipalities of the Dominican
Republic, 2016–2025*.

## Files

| File | Rows | Description |
|---|---|---|
| `pop_municipio_2016_2025_A.csv` | 52,700 | **Series A** (main): official NSO (Oficina Nacional de Estadística) municipal estimates 2016–2020, extended 2021–2025 with Hamilton–Perry cohort-change ratios raked to NSO province projections |
| `pop_municipio_2016_2025_B.csv` | 52,700 | **Series B** (robustness): intercensal interpolation between the 2010 and 2022 census counts |

## Columns

| Column | Description |
|---|---|
| `adm3_pcode` | Municipality code (OCHA ADM3 pcode, e.g. `DOM010901`; encodes NSO province/municipio codes in positions 6–7/8–9) |
| `prov_name` | Province name |
| `adm3_name` | Municipality name |
| `sex` | `F` / `M` |
| `age_group` | Five-year bands `0-4` … `75-79`, `80+` |
| `year` | 2016–2025 |
| `pop` | Estimated population (persons) |

## Notes

- Geography is the 2010 municipal division (155 municipalities): the three
  municipalities created after 2010 (San Víctor, Matanzas, Baitoa) are folded into
  their parent municipalities.
- A back-test predicting the held-out 2022 census from the 2015–2020 series yields
  mean absolute percentage errors of 8.1 percent (national age–sex cells) and
  14.7 percent (municipality cells); construction is documented in Appendix C of the
  paper and is reproducible end-to-end from raw census inputs via the paper's code
  (`02_population_projection.R` → `02f_projection_appendix.R`).
- Sources: National Statistics Office (NSO; Oficina Nacional de Estadística) municipal estimates and province
  projections; 2010 and 2022 national censuses.

Contact: mariemontas@fas.harvard.edu
