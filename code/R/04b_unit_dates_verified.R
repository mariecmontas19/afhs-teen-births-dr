# ============================================================================
# 04b_unit_dates_verified.R
# Verified AU/UAIA unit dates: hospital founding, modern-unit inauguration
# (in-window units) and modern-service UPGRADE date (pre-2016 "always-treated"
# units), with PROVENANCE and SOURCE for every cell. Built 2026-06-24 from
# (a) web verification (presidencia.gob.do / sns.gob.do / hospital sites; see
# notes/pcua_unit_dates_verified.md for the source key) and (b) Marie's field
# work (incl. a photographed KOICA plaque for San Vicente de Paúl).
#
# *** This file records EVIDENCE, not a final treatment coding. *** The
# municipio-level re-coding (first-modern-unit rule; controls vs drop for the
# "old, never-modernized" units) is a separate, downstream decision.
#
# Anti-hallucination: every date here is either web-sourced (source URL in the
# .md key), field-confirmed, or photo-confirmed. Cells we could NOT verify are
# left NA with provenance = "recorded_only" or "NOT_FOUND" — never guessed.
# ============================================================================
source(here::here("code", "R", "00_config.R"))

u <- as.data.frame(readRDS(file.path(DIR_CLEAN, "pcua_units_mapped.rds")))
# 39 units since the Boca Chica dedup (§59.38, 2026-07-17)
stopifnot(nrow(u) == 39L, !anyDuplicated(u$unit_id))

# ---- verified extras, keyed by unit_id ------------------------------------
# modern_event_date : YYYY, YYYY-MM, or YYYY-MM-DD (the date the MODERN
#   comprehensive unit arrived: new opening, or upgrade of an old unit). NA if
#   unknown or if the unit was never modernized.
# modern_event_type : new_opening | upgrade | upgrade_unconfirmed |
#   old_never_modernized | unknown
# never_modernized  : TRUE only where field evidence says an old (1990s) unit
#   was never modernized.
# provenance        : field | field_photo | user_confirmed | web |
#   recorded_only | NOT_FOUND
# source_key        : S# (see .md), or fieldwork / field_photo / user
v <- function(unit_id, hosp_founded = NA_character_, modern_event_date = NA_character_,
              modern_event_type = NA_character_, never_modernized = NA,
              unit_number = NA_integer_, provenance = NA_character_,
              source_key = NA_character_, notes = NA_character_)
  data.frame(unit_id, hosp_founded, modern_event_date, modern_event_type,
             never_modernized, unit_number, provenance, source_key, notes,
             stringsAsFactors = FALSE)

ver <- rbind(
  # ---- 12 pre-2016 "always-treated" units ----
  v(23, NA,        "2022",      "new_opening",          FALSE, NA, "interim",
    "fieldwork", "Villa Duarte (SD Este): consults-only (1990s, no real unit per field). Date 2022 = INTERIM placeholder (MM 2026-06-25, like Las Caobas/Villa Mella, will update). Consults-only regardless"),
  v( 6, "1974",    "2018-05",   "old_never_modernized", TRUE,  NA, "field",
    "S2;user", "San Lorenzo de Los Mina (SD Este): comprehensive program documented by May 2018 (already 2017, pre-Abinader); field says 90s unit; no modern remodel"),
  v(15, "1969",    "2021",      "upgrade",              FALSE, NA, "field",
    "fieldwork", "Nuestra Sra. de Regla (Bani): field-confirmed unit upgrade 2021; web shows whole-hospital remozamiento Mar 19 2025 [S5]"),
  v( 4, "1978",    "2022",      "new_opening",          FALSE, NA, "interim",
    "S3", "Las Caobas (SD Oeste): consults-only (1990s, 'falling apart', no remodel per field); founded 1978 [S3]. Date 2022 = INTERIM placeholder (MM 2026-06-25, will update). Consults-only regardless"),
  v( 7, "1965",    "2024-03",   "upgrade_unconfirmed",  FALSE, NA, "web",
    "S7", "Evangelina Rodriguez (DN): whole-hospital remozado Mar 18 2024 [S7]; adolescent-unit-specific modernization UNCONFIRMED"),
  v(11, "1916/1917","2022",     "upgrade",              FALSE, NA, "field",
    "fieldwork", "Ricardo Limardo (Puerto Plata): field-confirmed update 2022; web shows building reinaug 2021 [S9]"),
  v(19, "1997",    "2017",      "upgrade",              FALSE, NA, "field",
    "fieldwork;S10", "El Almirante (SD Este): unit opened 2005 (field notes); MODERNIZED 2017 by Medina (field-confirmed; whole-hospital remodel Nov 1 2017 [S10]). => SD Este first-modern-unit = 2017 -> SD Este is a TREATED municipio (not a control)"),
  v(29, NA,        "2025",      "upgrade",              FALSE, NA, "field",
    "fieldwork", "Felipe J. Achecar (Pimentel): remodel 2025 (field-confirmed MM); adolescents seen in emergency during remodelacion; web remozamiento Dec 5 2025 [S11]"),
  v( 9, "1947/1961","2023",     "upgrade",              FALSE, NA, "field",
    "S12", "Arturo Grullon (Santiago): field-confirmed update 2023; founded 1947/1961 [S12]"),
  v( 5, "1949/1950","2024-07-17","upgrade",             FALSE, NA, "web",
    "S13;S14", "Maternidad La Altagracia / HUMNSA (DN): 'Esther Portes' service 1993-2017 [S14]; reinaugurated Jul 17 2024 [S14]; founded 1949/50 [S13]"),
  v(31, "1974",    "2024",      "upgrade",              FALSE, NA, "field",
    "fieldwork;S15", "Antonio Yapor Heded (Nagua): unit remodel 2024 (field-confirmed MM 2026-06-25); earlier whole-hospital reinaug Dec 2018 [S15]"),
  v(18, "1926",    "2019",      "upgrade",              FALSE, NA, "field",
    "field_photo;S16", "San Vicente de Paul (SFM): remodeled TWICE — 2013 (KOICA plaque, photo) and 2019; use 2019 as latest remodel (field-confirmed MM 2026-06-25). => SFM modern cohort 2019, now IN-WINDOW (not always-treated). Founded 1926 [S16]"),

  # ---- in-window new openings (verified) ----
  v( 1, NA, "2020-09-25", "new_opening", FALSE, NA, "web", "S19",
    "Hugo Mendoza (SD Norte): 'consulta de adolescentes' ~Sep 25 2020 'este viernes' [S19]"),
  v( 2, NA, "2020-11",    "new_opening", FALSE, NA, "web", "S18",
    "Robert Reid Cabral (DN): UAIA reported Nov 25 2020 (month-level) [S18]"),
  v( 3, NA, "2025-11-19", "new_opening", FALSE, NA, "web", "S31", "Engombe (SD Oeste) [S31]"),
  v(14, NA, "2021-03-17", "new_opening", FALSE, NA, "web", "S21", "Juan Pablo Pina (San Cristobal): press inauguration Mar 17 2021 [S21]; SNS-validated doc says 2020 (year only). MM 2026-09-30: press date in BOTH designs (notes 59.102)"),
  v(22, NA, "2023-01-27", "new_opening", FALSE, NA, "web", "S26", "Jaime Mota (Barahona); 'mas de 30' [S26]"),
  v(26, NA, "2022-06-30", "new_opening", FALSE, NA, "web", "S24", "Imbert (Puerto Plata): unit put into operation Jun 30 2022 [S24]; SNS-validated doc says 2021 (year only). MM 2026-09-30: press date in BOTH designs (notes 59.102)"),
  v(27, NA, "2022-06-14", "new_opening", FALSE, NA, "web", "S23", "Vinicio Calventi (Los Alcarrizos): press inauguration Jun 14 2022 [S23]; SNS-validated doc says 2023 (year only). MM 2026-09-30: press date in BOTH designs (notes 59.102)"),
  v(28, NA, "2021-12-06", "new_opening", FALSE, NA, "web", "S20", "CENOVI / Mario Fdez. Mena (SFM) [S20]"),
  v(32, NA, "2022-07",    "new_opening", FALSE, NA, "web", "S22", "Pedro Antonio Cespedes (Constanza): habilitada by Jul 27 2022; '2nd in region' after Morillo King [S22]"),
  v(33, NA, "2023-09-21", "new_opening", FALSE, 35L, "web", "S27", "Inmaculada Concepcion (Cotui): labeled #35 (numbering conflicts w/ Boca Chica) [S27]"),
  v(34, NA, "2023-06-06", "new_opening", FALSE, 34L, "web", "S25", "Ciudad Sanitaria Aybar (DN): #34 [S25]"),
  v(35, NA, "2024-04-23", "new_opening", FALSE, 35L, "web", "S28", "Boca Chica / F.V. Castro Sandoval (NAME FIX, not 'Cabrera'): #35 [S28]"),
  v(37, NA, "2024-07-10", "new_opening", FALSE, 37L, "web", "S29", "Marcelino Velez Santana (DN): #37 [S29]"),
  v(39, NA, "2024-10-16", "new_opening", FALSE, 39L, "web", "S30", "Sigifredo Alba (Fantino): #39 [S30]"),
  v(12, NA, "2024-07",    "new_opening", FALSE, NA, "user_confirmed", "user",
    "Maternidad Higuey (La Altagracia prov): user-confirmed REAL & distinct from DN HUMNSA"),
  v(40, NA, "2024-12",    "new_opening", FALSE, NA, "field", NA,
    "Las Terrenas (Hospital Municipal Lic. Pablo Antonio Paulino, Samana DOM032003): services since Dec 2024, NOT formally inaugurated (field-confirmed MM 2026-06-25); 40th unit, added in 04"),

  # ---- in-window, UAIA date NOT FOUND online (recorded date stands) ----
  v(10, NA, NA, "unknown", FALSE, NA, "recorded_only", "S35",
    "Estrella Urena (Santiago): recorded 2017-02; UAIA inaug not found online; remozamiento underway Oct 2025 [S35]"),
  v(13, NA, "2023-09-01", "upgrade", FALSE, NA, "SNS_doc", "S22",
    "Morillo King (La Vega): unit EXISTED by Jul 2022 (S22 Constanza article: 'esta se une a la del Morillo King'), true first opening UNVERIFIABLE. FINAL (MM 2026-07-10, notes 57): coded at the documented event = SNS-validated 'Reinauguracion 1-9-23' (2023-09, the source-recorded date); a 2022 bound would itself be constructed. Type upgrade (reinauguration of an existing unit). Sensitivity: La Vega-2022 headline -5.90 (notes 56.1)"),
  v(25, NA, "2021-01", "new_opening", FALSE, NA, "field", NA, "Bajos de Nizao (Peravia): unit arrived 20 Nov 2020, OPENED TO PATIENTS Jan 2021 per official hospital report (Dir. Dra. Dominga Tavarez, 20 Oct 2025). Consults-only."),
  v(30, NA, NA, "unknown", FALSE, NA, "recorded_only", NA, "Castillo (Hospital Municipal de Castillo, DUARTE): recorded 2021-12; UAIA date not found; REMAPPED to Castillo municipio DOM030603 in 04 (was mis-mapped to SFM), field-confirmed MM 2026-06-24"),
  v(21, NA, NA, "unknown", FALSE, NA, "recorded_only", "S36",
    "Santiago Rodriguez (Sabaneta): recorded 2025-04; hospital built Apr 2018 [S36]; UAIA date not found"),
  v(36, NA, NA, "unknown", FALSE, NA, "recorded_only", NA, "Rodolfo de la Cruz (Pedro Brand): recorded 2024-07; UAIA date not found"),
  v(38, NA, NA, "unknown", FALSE, NA, "recorded_only", "S38",
    "Federico Armando Aybar (Las Matas de Farfan): recorded 2024-04; UAIA not found; hosp remozado Jul 24 2018 [S38]"),

  # ---- imputed-year units (no recorded date), UAIA NOT FOUND ----
  v( 8, NA, "2022", "new_opening", FALSE, NA, "interim", NA, "Villa Mella (SD Norte): consults-only; hospital being demolished/rebuilt as of Feb 2026 (no formal unit; field: no one found). Date 2022 = INTERIM placeholder (MM 2026-06-25, will update). SD Norte also treated via Hugo Mendoza 2020"),
  v(16, NA, "2019", "new_opening", FALSE, NA, "web", "S33", "Francisco Gonzalvo (La Romana): adolescent 'modulo' since 2019 remodel [S33]; field (Oct 2024) unit space in remodelacion; 2025 surgical-block remodel in progress (no date) -> use 2019"),
  v(17, NA, "2020", "new_opening", FALSE, NA, "web", "S32", "Yamasa (Monte Plata): hospital remodel Aug 15 2020 [S32]; field: adolescents seen but NO space (consults-only)"),
  v(20, NA, "2025", "new_opening", FALSE, NA, "web", "S34", "Antonio Musa (SPM): unit space remodel completed Apr 2025 [S34]; field (Oct 2024) it was in remodelacion -> use 2025"),

  # ---- former duplicate row (unit 24, generic "Hospital Boca Chica") ----
  # RESOLVED 2026-07-17 (§59.38, MM-confirmed): SAME hospital as unit 35
  # (Castro Sandoval, the rebuilt Boca Chica municipal hospital; the SNS
  # establishments registry lists exactly ONE level-II hospital in the
  # municipality). Row dropped at the source read in 04_treatment.R.
  NULL
)

stopifnot(!anyDuplicated(ver$unit_id), nrow(ver) == 39L)

# ---- consults_only flag (MM 2026-06-25): municipal hospitals that offer adolescent
# CONSULTS but have NO real integral unit/space (vs a full inaugurated unit). Field-
# determined. Las Terrenas (40) is EXCLUDED here: it has a full operating unit (just
# not formally inaugurated). Used in 04c to build a "real-unit-only" treatment variant.
ver$consults_only <- ver$unit_id %in% c(25, 28, 30, 32, 26, 3, 29, 8, 17, 23, 4)

# ---- merge onto the 39 data rows; verify row count ------------------------
out <- merge(u[, c("unit_id", "hosp", "prov", "muni", "adm3_pcode",
                    "year", "month", "year_imputed")],
             ver, by = "unit_id", all.x = TRUE)
cat("rows in:", nrow(u), "| rows out:", nrow(out),
    "| unmatched:", sum(is.na(out$provenance)), "\n")
stopifnot(nrow(out) == 39L, sum(is.na(out$provenance)) == 0L)

names(out)[names(out) == "year"]  <- "recorded_year"
names(out)[names(out) == "month"] <- "recorded_month"
out <- out[order(out$recorded_year, out$muni, out$unit_id), ]

# classification used elsewhere in the pipeline
out$class <- ifelse(!is.na(out$recorded_year) & out$recorded_year < 2016,
                    "always_treated_pre2016", "in_window")

dir.create(here::here("notes"), showWarnings = FALSE)
csv_path <- here::here("notes", "pcua_unit_dates_verified.csv")
write.csv(out, csv_path, row.names = FALSE, fileEncoding = "UTF-8")

cat("\nwrote", csv_path, "\n")
cat("class counts:\n"); print(table(out$class))
cat("modern_event_type:\n"); print(table(out$modern_event_type, useNA = "ifany"))
cat("never_modernized = TRUE:\n")
print(out[which(out$never_modernized), c("unit_id", "hosp", "muni")])
cat("provenance:\n"); print(table(out$provenance))
