# ============================================================================
# 03_clean_births.R  —  Clean BDNV live births 2016-2025 (one row per baby)
#
# Reads each year sheet as TEXT (avoids readxl type-guessing dropping sparse columns),
# harmonizes the year-to-year column-name differences via a canonical matcher, recodes
# every variable per notes/DATA_DICTIONARY_and_variable_plan.md (agreed with MM), maps
# municipio (residence + delivery) to adm3_pcode, and validates. NO aggregation/rates
# here (that is 06_build_panel). Output: data/clean/births_clean_2016_2025.rds.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})

# ---- helpers from 01 (norm_name, province/municipio aliases) + crosswalk ----
H  <- readRDS(file.path(DIR_CLEAN, "muni_match_helpers.rds"))
norm_name  <- H$norm_name; prov_alias <- H$prov_alias; muni_alias <- as.data.table(H$muni_alias)
xw <- as.data.table(readRDS(file.path(DIR_CLEAN, "muni_crosswalk.rds")))
xw[, muni_norm := norm_name(adm3_name)]
key <- xw[, .(prov_norm, muni_norm, adm3_pcode)]
# map (province name, municipio name) -> adm3_pcode  (replicates 01's matcher; births = 204/204)
match_pcode <- function(prov_raw, muni_raw){
  d <- data.table(i = seq_along(prov_raw), prov_norm = norm_name(prov_raw), muni_norm = norm_name(muni_raw))
  d[prov_norm %in% names(prov_alias), prov_norm := prov_alias[prov_norm]]
  d[muni_norm == "DISTRITO NACIONAL", `:=`(prov_norm="DISTRITO NACIONAL", muni_norm="SANTO DOMINGO DE GUZMAN")]
  d <- merge(d, muni_alias, by.x=c("prov_norm","muni_norm"), by.y=c("prov_norm","muni_src"), all.x=TRUE, sort=FALSE)
  d[!is.na(muni_canon), muni_norm := muni_canon]
  d <- merge(d, key, by=c("prov_norm","muni_norm"), all.x=TRUE, sort=FALSE)
  d[order(i)]$adm3_pcode
}
ncol_norm <- function(x) gsub("[^a-z0-9]","", tolower(stri_trans_general(x,"Latin-ASCII")))

# ---- canonical column targets (normalized exact name) ----
TARGETS <- c(age="vivoedadmadre", bdate="vivofechanacimientohijo", mdob="vivofechanacmadre",
  prov_res="provinciamadre", muni_res="municipiomadre", prov_del="provinciaatendioparto",
  muni_del="municipioatendioparto", nat="nacionalidaddelamadre", weight="pesoengramo",
  gestage="edadgestacional", risk="riesgoneonatal", parto="tipopartodescripcion",
  sex="sexodelbebe", length="tallaalnacer", headc="perimetrocefalicodesc",
  facility="lugarocurreparto", attend="partoatendidopor", prod="tipodeproducto",
  ars="arsmadre", educ="escolaridad", civil="estadoconyugalmadre",
  prenatal="vivocantidadchequeosprenatalesembarazo", lastmo="mesultimochequeoembarazo")

yrs <- setdiff(excel_sheets(RAW$births_xlsx), "INDEX")
read_year <- function(s){
  d <- as.data.table(read_excel(RAW$births_xlsx, sheet=s, col_types="text"))
  nn <- ncol_norm(names(d))
  out <- data.table(src_year = rep.int(as.integer(s), nrow(d)))   # init with nrow rows
  for (t in names(TARGETS)){ j <- which(nn == TARGETS[[t]]); out[[t]] <- if(length(j)) d[[names(d)[j[1]]]] else NA_character_ }
  out[]
}
cat("Reading", length(yrs), "year sheets ...\n")
b <- rbindlist(lapply(yrs, read_year), fill=TRUE)
cat("raw rows:", format(nrow(b),big.mark=","), "\n")

# ---- parse dates (Excel serials when read as text) ----
xl2date <- function(x){ v<-suppressWarnings(as.numeric(x)); as.Date(v, origin="1899-12-30") }
b[, birth_date := xl2date(bdate)]
b[, mother_dob := xl2date(mdob)]
b[, birth_year := data.table::year(birth_date)]
b[, birth_month := data.table::month(birth_date)]

# ---- age + age groups (error fixes per MM: 0,1,9->10 ; 55,68->45+) ----
b[, age_mom := suppressWarnings(as.numeric(age))]
b[age_mom %in% c(0,1,9), age_mom := 10L]            # impossible/edge -> youngest group
AGE_LV <- c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45+")
b[, age_grp := fcase(
  age_mom>=10 & age_mom<=14, "10-14", age_mom>=15 & age_mom<=19, "15-19",
  age_mom>=20 & age_mom<=24, "20-24", age_mom>=25 & age_mom<=29, "25-29",
  age_mom>=30 & age_mom<=34, "30-34", age_mom>=35 & age_mom<=39, "35-39",
  age_mom>=40 & age_mom<=44, "40-44", age_mom>=45, "45+", default=NA_character_)]
b[, age_grp := factor(age_grp, levels=AGE_LV)]

# ---- geography -> adm3_pcode (residence = analysis; delivery = spillover) ----
b[, adm3_pcode := match_pcode(prov_res, muni_res)]
b[, adm3_pcode_delivery := match_pcode(prov_del, muni_del)]

# ---- newborn health outcomes ----
b[, weight_g := suppressWarnings(as.numeric(weight))]
b[weight_g < 300 | weight_g > 6500, weight_g := NA]          # outlier rule
b[, low_bw := fifelse(!is.na(weight_g), as.integer(weight_g < 2500), NA_integer_)]
b[, gestage_wks := suppressWarnings(as.numeric(stri_extract_first_regex(gestage, "[0-9]+")))]
b[gestage_wks < 20 | gestage_wks > 44, gestage_wks := NA]    # outlier rule
b[, preterm := fifelse(!is.na(gestage_wks), as.integer(gestage_wks < 37), NA_integer_)]
b[, length_cm := suppressWarnings(as.numeric(length))]; b[length_cm<20 | length_cm>60, length_cm := NA]
b[, head_circ := suppressWarnings(as.numeric(headc))];  b[head_circ<20 | head_circ>45, head_circ := NA]
rn <- norm_name(b$risk)
b[, neo_risk := fcase(rn=="BAJO","Bajo", rn=="MEDIO","Medio", rn=="ALTO","Alto", default=NA_character_)]
b[, neo_risk := factor(neo_risk, levels=c("Bajo","Medio","Alto"), ordered=TRUE)]
b[, neo_high := fifelse(!is.na(neo_risk), as.integer(neo_risk=="Alto"), NA_integer_)]
# neo_any = "any neonatal risk" (Medio OR Alto) — the PRIMARY neo-risk outcome (clinician-assigned;
# empirically tracks asphyxia/Apgar/etc, NOT the nominal LBW/preterm formula). neo_high = severity robustness.
b[, neo_any := fifelse(!is.na(neo_risk), as.integer(neo_risk %in% c("Medio","Alto")), NA_integer_)]
b[, sex_baby := fcase(norm_name(sex)=="MASCULINO","M", norm_name(sex)=="FEMENINO","F", default=NA_character_)]
b[, csection := fcase(grepl("CESAREA", norm_name(parto)),1L, norm_name(parto)=="VAGINAL",0L, default=NA_integer_)]
# SGA: sample-internal <10th pctile of weight within (gestage_wks, sex); needs all three valid
b[, sga := NA_integer_]
b[!is.na(weight_g) & !is.na(gestage_wks) & !is.na(sex_baby),
  sga := as.integer(weight_g < quantile(weight_g, 0.10, na.rm=TRUE)), by=.(gestage_wks, sex_baby)]

# ---- composition / mechanism ----
b[, nationality := fcase(norm_name(nat)=="DOMINICANA","Dominican", norm_name(nat)=="HAITIANA","Haitian",
                         norm_name(nat) %in% c("N A","N D","ND","SIN CODIFICAR","") | is.na(nat), NA_character_,
                         default="Other")]
en <- norm_name(b$educ)
b[, educ := fcase(en=="NINGUNO","None",
  grepl("^PRIMARIO", en),"Primary", grepl("^SECUNDARIO", en),"Secondary",
  grepl("UNIVERSITARIO", en),"University", default=NA_character_)]   # SE IGNORA -> NA
b[, educ := factor(educ, levels=c("None","Primary","Secondary","University"), ordered=TRUE)]
cv <- norm_name(b$civil)
b[, in_union := fcase(cv %in% c("UNION LIBRE","CASADO"),1L, cv %in% c("SOLTERO","DIVORCIADO","VIUDO"),0L, default=NA_integer_)]
# insurance (per do-file lists; verified all 25 ARS covered) using normalized match
an <- norm_name(b$ars)
PUB <- norm_name(c("ADMINISTRADORA DE RIESGO DE SALUD SENASA","ADMINISTRADORA DE RIESGOS DE SALUD (APS)",
  "ADMINISTRADORA DE RIESGOS DE SALUD DE LAS FUERZAS ARMADAS","ADMNISTRADORA DE RIESGO DE SALUD SALUD SEGURA"))
AUTO <- norm_name(c("ADMINISTRADORA DE RIESGO DE SALUD SEMMA","ADMINISTRADORA DE RIESGO DE SALUD SEMUNASED",
  "ADMINISTRADORA DE RIESGO DE SALUD UASD","ADMINISTRADORA DE RIESGOS DE SALUD RESERVAS","MERCASID S A",
  "ADM. DE R. PLAN SALUD BANCO CENTRAL"))
PRIV <- norm_name(c("HUMANO SEGUROS, S.A.","ADMINISTRADORA DE RIESGO DE SALUD HUMANO, S. A.",
  "ADMINISTRADORA DE RIESGO DE SALUD YUNEN","ADMINISTRADORA DE RIESGO DE SALUD UNIVERSAL S A",
  "ADMINISTRADORA DE RIESGO DE SALUD SIMAG","ADMINISTRADORA DE RIESGO DE SALUD RENACER",
  "ADMINISTRADORA DE RIESGO DE SALUD PALIC SALUD","ADMINISTRADORA DE RIESGO DE SALUD MONUMENTAL",
  "ADMINISTRADORA DE RIESGO DE SALUD META SALUD","ADMINISTRADORA DE RIESGO DE SALUD GMA",
  "ADMINISTRADORA DE RIESGO DE SALUD FUTURO","ADMINISTRADORA DE RIESGO DE SALUD CONSTITUCION",
  "ADMINISTRADORA DE RIESGO DE SALUD ASEMAP"))
NONE1 <- norm_name("NO TIENE")               # explicit "no insurance"; N/D, N/A, blank, absent -> NA
b[, insurance := fcase(an %in% PUB,"Public", an %in% AUTO,"Autogestion", an %in% PRIV,"Private",
                       an == NONE1,"None", default=NA_character_)]
b[, has_insurance := fcase(insurance %in% c("Public","Autogestion","Private"),1L, insurance=="None",0L, default=NA_integer_)]
b[, subsidized := fcase(an==norm_name("ADMINISTRADORA DE RIESGO DE SALUD SENASA"),1L, !is.na(insurance),0L, default=NA_integer_)]
# QC: any ARS value not covered by the four lists or the intentional-NA codes? (expect none)
.ars_na <- norm_name(c("N/D","N/A","ND"))
ars_uncovered <- b[!is.na(ars) & an!="" & !(an %in% c(PUB,AUTO,PRIV,NONE1,.ars_na)), .N, by=ars][order(-N)]
# prenatal — COUNT of checks (reliable, 2021+): adequacy >=4 (focused-ANC) / >=8 (WHO-2016)
b[, prenatal_checks := suppressWarnings(as.numeric(prenatal))]; b[prenatal_checks>20, prenatal_checks := NA]
b[, anc4 := fifelse(!is.na(prenatal_checks), as.integer(prenatal_checks>=4), NA_integer_)]
b[, anc8 := fifelse(!is.na(prenatal_checks), as.integer(prenatal_checks>=8), NA_integer_)]
# FIRST prenatal check (Darwin CONFIRMED 2026-06-24): the "mes ... chequeo embarazo" field is the
# CALENDAR MONTH (Jan-Dec) of the FIRST check, NOT a gestational month (verified: values ENERO..DICIEMBRE).
# first_check_month = calendar month 1-12; gestational timing at first visit reconstructed below.
MO <- c("ENERO"=1,"FEBRERO"=2,"MARZO"=3,"ABRIL"=4,"MAYO"=5,"JUNIO"=6,"JULIO"=7,"AGOSTO"=8,
        "SEPTIEMBRE"=9,"OCTUBRE"=10,"NOVIEMBRE"=11,"DICIEMBRE"=12)
b[, first_check_month := MO[norm_name(lastmo)]]
# facility (mechanism) + doctor (descriptive) + multiple
fn <- norm_name(b$facility)
b[, facility := fcase(grepl("PUBLICA",fn),"Public", grepl("PRIVADO",fn),"Private",
                      grepl("VIVIENDA",fn),"Home", grepl("OTRO",fn),"Other", default=NA_character_)]
b[, doctor_attended := fcase(grepl("OBSTETRA|GENERAL", norm_name(attend)),1L,
                             norm_name(attend) %in% c("COMADRONA","ENFERMERA","OTRO"),0L, default=NA_integer_)]
b[, multiple := fcase(grepl("MULTIPLE", norm_name(prod)),1L, grepl("UNICO", norm_name(prod)),0L, default=NA_integer_)]
# conception date (birth - gestational age = approx LMP); for the 9-month-lag test
b[, conception_date := birth_date - gestage_wks*7]
# gestational age at FIRST ANC visit, reconstructed from its CALENDAR month + LMP (approximate):
# assume mid-month (day 15); choose the year so the visit lies within [LMP, birth]; first-trimester = early.
b[, .fcd := as.Date(paste(birth_year, first_check_month, 15, sep="-"))]
b[!is.na(.fcd) & .fcd > birth_date, .fcd := as.Date(paste(birth_year-1L, first_check_month, 15, sep="-"))]
b[, anc_first_gestwk := as.numeric(.fcd - conception_date)/7]
b[anc_first_gestwk < 0 | anc_first_gestwk > 45, anc_first_gestwk := NA]   # drop impossible reconstructions
b[, anc_early := fifelse(!is.na(anc_first_gestwk), as.integer(anc_first_gestwk <= 13), NA_integer_)]  # first trimester
b[, .fcd := NULL]

# ---- final clean table ----
clean <- b[, .(src_year, birth_date, birth_year, birth_month, conception_date, mother_dob,
  adm3_pcode, adm3_pcode_delivery, prov_res, muni_res, age_mom, age_grp, nationality,
  weight_g, low_bw, gestage_wks, preterm, sga, neo_risk, neo_high, neo_any, length_cm, head_circ,
  sex_baby, csection, facility, doctor_attended, multiple,
  educ, in_union, insurance, has_insurance, subsidized, prenatal_checks, anc4, anc8,
  first_check_month, anc_first_gestwk, anc_early)]
saveRDS(clean, file.path(DIR_CLEAN, "births_clean_2016_2025.rds"))

# ============================ VALIDATION ============================
cat("\n==================== 03 VALIDATION ====================\n")
cat("rows:", format(nrow(clean),big.mark=","), " years:", paste(range(clean$birth_year,na.rm=TRUE),collapse="-"), "\n")
cat("birth_year vs sheet-year mismatch:", sum(clean$birth_year != clean$src_year, na.rm=TRUE),
    " | birth_date NA:", sum(is.na(clean$birth_date)), "\n")
cat("rows per year:\n"); print(clean[, .N, by=birth_year][order(birth_year)])
cat("\nadm3_pcode matched:", sum(!is.na(clean$adm3_pcode)), "/", nrow(clean),
    " (", round(100*mean(is.na(clean$adm3_pcode)),3), "% unmatched)\n")
cat("  unmatched (prov_res, muni_res) pairs (top 12):\n")
print(b[is.na(adm3_pcode), .N, by=.(prov_res, muni_res)][order(-N)][1:12])
cat("  of unmatched rows, blank/NA muni_res:",
    b[is.na(adm3_pcode) & (is.na(muni_res) | trimws(muni_res)==""), .N], "\n")
cat("  uncovered ARS values (expect 0):", nrow(ars_uncovered), "\n")
if (nrow(ars_uncovered)) print(head(ars_uncovered, 10))
cat("age: NA", sum(is.na(clean$age_mom)), " | age_grp NA", sum(is.na(clean$age_grp)),
    " | range", paste(range(clean$age_mom,na.rm=TRUE),collapse="-"), "\n")
cat("age_grp counts:\n"); print(clean[, .N, by=age_grp][order(age_grp)])
cat("\noutcome ranges/means (valid only):\n")
cat("  weight_g:", paste(range(clean$weight_g,na.rm=TRUE),collapse="-"), "mean", round(mean(clean$weight_g,na.rm=TRUE)),
    "| low_bw%", round(100*mean(clean$low_bw,na.rm=TRUE),1), "\n")
cat("  gestage:", paste(range(clean$gestage_wks,na.rm=TRUE),collapse="-"), "| preterm%", round(100*mean(clean$preterm,na.rm=TRUE),1),
    "| sga% (should be ~10):", round(100*mean(clean$sga,na.rm=TRUE),1), "\n")
cat("  csection%", round(100*mean(clean$csection,na.rm=TRUE),1), "| neo_high%", round(100*mean(clean$neo_high,na.rm=TRUE),1),
    "| neo_any%", round(100*mean(clean$neo_any,na.rm=TRUE),1),
    "| anc4%", round(100*mean(clean$anc4,na.rm=TRUE),1), "| anc8%", round(100*mean(clean$anc8,na.rm=TRUE),1), "\n")
cat("  first-ANC reconstr: median gest wk", round(median(clean$anc_first_gestwk,na.rm=TRUE),1),
    "| IQR", paste(round(quantile(clean$anc_first_gestwk,c(.25,.75),na.rm=TRUE),1),collapse="-"),
    "| %early(<=13wk)", round(100*mean(clean$anc_early,na.rm=TRUE),1),
    "| %NA among prenatal-covered", round(100*mean(is.na(clean$anc_first_gestwk[!is.na(clean$prenatal_checks)])),1), "\n")
cat("\ncomposition non-missing by year (key vars):\n")
print(clean[, .(educ=round(100*mean(!is.na(educ))), in_union=round(100*mean(!is.na(in_union))),
                insurance=round(100*mean(!is.na(insurance))), prenatal=round(100*mean(!is.na(prenatal_checks)))),
            by=birth_year][order(birth_year)])
cat("\ndistinct values: nationality"); print(table(clean$nationality, useNA="ifany"))
cat("insurance:"); print(table(clean$insurance, useNA="ifany"))
cat("educ:"); print(table(clean$educ, useNA="ifany"))
cat("\nsaved -> data/clean/births_clean_2016_2025.rds\n")
