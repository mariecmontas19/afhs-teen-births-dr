# ============================================================================
# 04_treatment.R  —  AU treatment: first-opening cohort, event-time inputs,
#                    always-treated flags, # units, distance-to-nearest-AU.
#
# Treatment = BINARY, FIRST-OPENING, ABSORBING (Callaway-Sant'Anna framework):
# a municipio turns on the YEAR its FIRST AU opens and stays on; later units
# ignored for the binary spec (captured by n_units for the intensity extension).
# Missing opening year (4 units) -> 2016 (per plan). Always-treated = first
# opening < 2016 (no in-sample pre-period -> excluded from the binary event study;
# retained via intensity/dose + descriptives). Treatment modeled as HOMOGENEOUS
# (standardized SNS unit model; see notes/unit_characteristics.md); unit_type +
# contraception flags attached for ROBUSTNESS het splits only.
# Output: data/clean/treatment_municipio.rds (158 muni) + pcua_units_mapped.rds.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi); library(sf)})

# ---- helpers + crosswalk + matcher (identical to 03) ----
H <- readRDS(file.path(DIR_CLEAN, "muni_match_helpers.rds"))
norm_name <- H$norm_name; prov_alias <- H$prov_alias; muni_alias <- as.data.table(H$muni_alias)
xw <- as.data.table(readRDS(file.path(DIR_CLEAN, "muni_crosswalk.rds")))
xw[, muni_norm := norm_name(adm3_name)]
key <- xw[, .(prov_norm, muni_norm, adm3_pcode)]
match_pcode <- function(prov_raw, muni_raw){
  d <- data.table(i=seq_along(prov_raw), prov_norm=norm_name(prov_raw), muni_norm=norm_name(muni_raw))
  d[prov_norm %in% names(prov_alias), prov_norm := prov_alias[prov_norm]]
  d[muni_norm=="DISTRITO NACIONAL", `:=`(prov_norm="DISTRITO NACIONAL", muni_norm="SANTO DOMINGO DE GUZMAN")]
  d <- merge(d, muni_alias, by.x=c("prov_norm","muni_norm"), by.y=c("prov_norm","muni_src"), all.x=TRUE, sort=FALSE)
  d[!is.na(muni_canon), muni_norm := muni_canon]
  d <- merge(d, key, by=c("prov_norm","muni_norm"), all.x=TRUE, sort=FALSE)
  d[order(i)]$adm3_pcode
}
MES <- c(ENERO=1,FEBRERO=2,MARZO=3,ABRIL=4,MAYO=5,JUNIO=6,JULIO=7,AGOSTO=8,SEPTIEMBRE=9,OCTUBRE=10,NOVIEMBRE=11,DICIEMBRE=12)
parse_month <- function(x){ y <- toupper(trimws(stri_trans_general(as.character(x),"Latin-ASCII")))
  out <- MES[y]; bad <- is.na(out); out[bad] <- suppressWarnings(as.integer(y[bad])); as.integer(out) }

# ---- read units ----
u <- as.data.table(read_excel(RAW$pcua_xlsx, sheet="Unidades"))
setnames(u, 1:9, c("hosp","prov","muni","region","regnum","anio","mes","links","notas"))
u <- u[!is.na(hosp)]
u[, hosp := trimws(gsub("\xef\xbb\xbf","",hosp, useBytes=TRUE))]
u[, unit_id := .I]
u[, year  := as.integer(stri_extract_first_regex(as.character(anio), "(19|20)[0-9]{2}"))]
u[, month := parse_month(mes)]
u[, year_imputed := is.na(year)]              # undated unit (kept NA; NOT pre-imputed — see cohort logic)
cat("units:", nrow(u), "| undated:", sum(u$year_imputed), "| month present:", sum(!is.na(u$month)), "\n")

# ---- map each unit -> adm3_pcode (named muni via matcher; blank muni via 01b resolved table) ----
u[, adm3_pcode := match_pcode(prov, muni)]
bl <- fread(file.path(DIR_CLEAN, "pcua_blank_muni_resolved.csv"))
bl[, hn := norm_name(hosp_name)]; u[, hn := norm_name(hosp)]
u[is.na(adm3_pcode), adm3_pcode := bl[.SD, on="hn", x.adm3_pcode], .SDcols="hn"]
# ---- field-confirmed location correction (source municipio mislabeled) ----
# Hospital Municipal de Castillo: the source lists its municipio as "San Francisco de
# Macorís" but the hospital is in CASTILLO municipio (DOM030603). Confirmed by MM
# (2026-06-24). Remap so Castillo becomes its own treated municipio (was mis-credited to SFM).
u[grepl("Municipal de Castillo", hosp), adm3_pcode := "DOM030603"]
# Hospital Marcelino Vélez Santana is in Santo Domingo OESTE (Herrera), not Distrito
# Nacional as the source lists it. Field/SNS-confirmed (MM 2026-06-25). Remap to SD Oeste.
u[grepl("Marcelino V", hosp), adm3_pcode := "DOM103202"]
# Hospital in Constanza: official name is "Dr. PEDRO Antonio Céspedes" (§55.3);
# the source sheet omits "Pedro". Name-only fix (no coding impact).
u[grepl("Antonio Céspedes", hosp), hosp := "Hospital Municipal Dr. Pedro Antonio Céspedes"]
# ---- Boca Chica duplicate (RESOLVED 2026-07-17, MM-confirmed, notes §59.38) ----
# The SNS "unidades con fechas" source double-lists ONE hospital: generic row
# "Hospital Boca Chica" (undated, no unit number) AND "Hospital Boca Chica Dr.
# Francisco Vicente Castro Sandoval" (#35, 2024-04-23). The SNS establishments
# registry has exactly ONE level-II hospital in Boca Chica municipality (Castro
# Sandoval, the rebuilt "nuevo hospital"); everything else there is primary care.
# Drop the generic duplicate; keep the dated, numbered Castro Sandoval row.
# Treatment coding unaffected (same municipio DOM103204, same 2024 cohort);
# unit COUNT changes 40 -> 39 and n_units for Boca Chica 2 -> 1.
ndup <- u[hosp == "Hospital Boca Chica", .N]
stopifnot(ndup == 1)                       # exact generic name only; Castro Sandoval row untouched
u <- u[hosp != "Hospital Boca Chica"]
cat("Boca Chica duplicate dropped:", ndup, "row (SNS double-listing; §59.38)\n")
# ---- Morillo King (La Vega) dating note (FINAL: MM decision 2026-07-10, notes §57) ----
# The unit's true first opening is UNVERIFIABLE: SNS's July-2022 Constanza article [S22]
# proves it existed by Jul-2022 ("esta se une a la del Morillo King"), but no opening date
# exists in any source. Coded at the DOCUMENTED event: the SNS-validated "Reinauguracion
# 1-9-23" (2023-09, the source-recorded date) — a 2022 "upper bound" coding would itself
# be a constructed date. Sensitivity (computed, §56.1): La Vega at 2022 gives headline
# -5.90 vs -6.43; DDD ~unchanged. No mutation needed (source records 2023-09).
# ---- 40th unit, NOT in the source xlsx (added from field/SNS data collection) ----
# Hospital Municipal Lic. Pablo Antonio Paulino, Las Terrenas (Samaná, DOM032003): the
# adolescent unit has provided services since Dec 2024 but was NOT formally inaugurated.
# Added by MM (2026-06-25). adm3_pcode set directly (Las Terrenas).
u <- rbind(u, data.table(hosp="Hospital Municipal Licenciado Pablo Antonio Paulino",
           prov="Samaná", muni="Las Terrenas", region=NA_character_, regnum=NA,
           anio="2024", mes="DICIEMBRE", links=NA_character_,
           notas="services since Dec 2024; not formally inaugurated",
           unit_id=40L, year=2024L, month=12L, year_imputed=FALSE,
           adm3_pcode="DOM032003", hn=norm_name("Hospital Municipal Licenciado Pablo Antonio Paulino")),
           fill=TRUE)
nmiss <- u[is.na(adm3_pcode), .N]
cat("units mapped to adm3_pcode:", u[!is.na(adm3_pcode),.N], "/", nrow(u), if(nmiss) " — UNMAPPED:" else "", "\n")
if (nmiss) print(u[is.na(adm3_pcode), .(unit_id, hosp, prov, muni)])
stopifnot(nmiss == 0)
stopifnot(all(u$adm3_pcode %in% xw$adm3_pcode))

# ---- attach unit_characteristics flags (homogeneous main; flags for robustness) ----
uc <- fread(file.path(PROJ, "notes", "unit_characteristics.csv"))[, .(unit_id=id, unit_type, contraception_onsite)]
u <- merge(u, uc, by="unit_id", all.x=TRUE, sort=FALSE)

# ---- municipio-level treatment (first-opening absorbing) ----
# Cohort = earliest DATED unit in the municipio. Fall back to IMPUTE_YEAR ONLY when ALL of a municipio's
# units are undated (avoids an undated unit overriding a documented one — e.g. SD Norte's dated Hugo
# Mendoza 2020 must not be pulled by the undated Villa Mella). cohort_imputed flags the all-undated case.
# IMPUTATION = 2023 (center of the national SNS adolescent-unit rollout; the bulk opened 2023-24; MM
# decision 2026-06-22) for the 3 units (La Romana/SPM/Yamasá) whose dates are unverifiable online.
# Conservative vs earlier years (avoids over-counting treated years). cohort_imputed flags them ->
# ROBUSTNESS in 07 MUST drop them + vary the year; headline must not hinge on these 3.
IMPUTE_YEAR <- 2023L
tm <- u[, {
  dated <- year[!is.na(year)]; mo_d <- month[!is.na(year)]
  has_dated <- length(dated) > 0
  .(first_year   = if (has_dated) min(dated) else IMPUTE_YEAR,
    first_month  = if (has_dated) mo_d[which.min(dated)] else NA_integer_,
    last_year    = if (has_dated) max(dated) else IMPUTE_YEAR,
    n_units      = .N,
    n_dated      = length(dated),
    cohort_imputed   = !has_dated,                 # ALL units undated -> cohort imputed to 2016
    has_undated_unit = any(year_imputed),          # municipio has >=1 undated unit (transparency)
    any_contraception  = any(contraception_onsite=="yes"),
    any_maternity_unit = any(unit_type=="maternity_femaleonly"))
}, by=adm3_pcode]
tm[, ever_treated   := 1L]
tm[, always_treated := as.integer(first_year < 2016)]       # no in-sample pre-period for binary CS

# ---- full 158-municipio frame (treated + never-treated) ----
trt <- merge(xw[, .(adm3_pcode, prov_norm, adm3_name)], tm, by="adm3_pcode", all.x=TRUE)
trt[is.na(ever_treated), `:=`(ever_treated=0L, n_units=0L, n_dated=0L, always_treated=0L,
                              cohort_imputed=FALSE, has_undated_unit=FALSE,
                              any_contraception=FALSE, any_maternity_unit=FALSE)]
# cohort G for CS: first_year for in-window treated; NA for never-treated; always-treated flagged separately
trt[, cohort := first_year]

# ---- distance to nearest AU municipio (centroid-to-centroid, km; 0 if treated) ----
# GEOMETRY CACHE: the ADM3 shapefile lives in Dropbox CloudStorage (online-only, can be evicted).
# On first availability we cache muni geometry (adm3_pcode + polygons, UTM 19N) to data/clean so that
# distance AND maps survive future eviction. Skip cleanly if neither cache nor shapefile is present.
trt[, dist_km_nearest_pcua := NA_real_]
geom_cache <- file.path(DIR_CLEAN, "muni_geom.rds")
s <- NULL
if (file.exists(geom_cache)) {
  s <- readRDS(geom_cache)
} else if (file.exists(RAW$shp_muni)) {
  s <- st_transform(st_read(RAW$shp_muni, quiet=TRUE)["adm3_pcode"], 32619)  # DR UTM 19N (metric)
  s$adm3_pcode <- toupper(s$adm3_pcode)
  saveRDS(s, geom_cache); cat("cached municipio geometry -> data/clean/muni_geom.rds\n")
}
shp_ok <- !is.null(s)
if (shp_ok) {
  cen <- st_centroid(s)
  ord <- match(trt$adm3_pcode, cen$adm3_pcode)
  if (!anyNA(ord)) {
    cen <- cen[ord, ]; treated_idx <- which(trt$ever_treated == 1L)
    D <- st_distance(cen, cen[treated_idx, ])                    # meters
    trt[, dist_km_nearest_pcua := round(as.numeric(apply(D, 1, min)) / 1000, 2)]
  } else shp_ok <- FALSE
}

saveRDS(u[,  .(unit_id, hosp, prov, muni, adm3_pcode, year, month, year_imputed, unit_type, contraception_onsite)],
        file.path(DIR_CLEAN, "pcua_units_mapped.rds"))
saveRDS(trt, file.path(DIR_CLEAN, "treatment_municipio.rds"))

# ---- validation report ----
cat("\n==================== 04 TREATMENT VALIDATION ====================\n")
cat("municipios:", nrow(trt), " | ever-treated:", trt[ever_treated==1,.N],
    " | never-treated:", trt[ever_treated==0,.N], "\n")
cat("always-treated (first<2016, excluded from binary CS):", trt[always_treated==1,.N], "\n")
cat("in-window treated (2016-2025, identifying variation):", trt[ever_treated==1 & always_treated==0,.N], "\n")
cat("\ncohort (first-opening year) distribution, treated municipios:\n")
print(trt[ever_treated==1, .N, by=cohort][order(cohort)])
cat("\nALWAYS-TREATED municipios (first unit pre-2016):\n")
print(trt[always_treated==1, .(adm3_name, prov_norm, first_year, n_units)][order(first_year)])
cat("\nIMPUTED-cohort municipios (ALL units undated -> cohort=2016; robustness: drop/vary):\n")
print(trt[cohort_imputed==TRUE, .(adm3_name, prov_norm, first_year, n_units)])
cat("treated municipios with >=1 undated unit but a DATED cohort (undated unit ignored for timing):\n")
print(trt[ever_treated==1 & has_undated_unit==TRUE & cohort_imputed==FALSE, .(adm3_name, first_year, n_units, n_dated)])
if (shp_ok) {
  cat("\ndistance-to-nearest-AU (km): summary over all 158 municipios:\n")
  print(summary(trt$dist_km_nearest_pcua))
  cat("never-treated with an AU within 10km (spillover-risk controls):",
      trt[ever_treated==0 & dist_km_nearest_pcua<=10, .N], "\n")
} else {
  cat("\n** distance SKIPPED (NA): no muni geometry (cache or shapefile) available — re-run 04 when present. **\n")
}
cat("\nsaved -> data/clean/treatment_municipio.rds (158) + pcua_units_mapped.rds (", nrow(u), "units)\n", sep="")
