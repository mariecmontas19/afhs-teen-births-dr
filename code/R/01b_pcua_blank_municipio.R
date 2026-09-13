# ============================================================================
# 01b_pcua_blank_municipio.R  —  Resolve the 10 AU units with BLANK municipio
#
# The `Visits to PCUAS.xlsx` file leaves the municipio blank for 10 (mostly
# regional/provincial) hospitals. Rather than ASSUME they sit in the provincial
# cabecera, each hospital's actual municipio was looked up online and VERIFIED
# (official .gob.do site / SNS / Waze). 9 turned out to be the cabecera; 1 did
# NOT: Hospital Municipal de Imbert is in IMBERT, not San Felipe de Puerto Plata.
#
# Output: data/clean/pcua_blank_muni_resolved.csv  (hospital -> adm3_pcode + source)
# Verified 2026-06-18.
# ============================================================================

source(here::here("code", "R", "00_config.R"))
suppressPackageStartupMessages({ library(readxl); library(data.table); library(stringi) })
norm_name <- function(x){ x<-stringi::stri_trans_general(as.character(x),"Latin-ASCII"); x<-toupper(x)
  x<-gsub("[^A-Z0-9 ]"," ",x); x<-gsub("\\s+"," ",x); trimws(x) }

xw <- as.data.table(readRDS(file.path(DIR_CLEAN, "muni_crosswalk.rds")))
xw[, muni_norm := norm_name(adm3_name)]

# Verified hospital -> (province, municipio name) with source URL.
# Keyed on a DISTINCTIVE token from the hospital name (robust to BOM/accents).
resolved <- tribble(
  ~hosp_token,            ~prov_norm,             ~muni_norm,              ~source_url,                                  ~is_cabecera,
  "ARTURO GRULLON",       "SANTIAGO",             "SANTIAGO",              "https://hospitalarturogrullon.gob.do/contacto/", TRUE,
  "ESTRELLA URENA",       "SANTIAGO",             "SANTIAGO",              "https://hospitalestrellaurena.gob.do/contacto/", TRUE,
  "RICARDO LIMARDO",      "PUERTO PLATA",         "PUERTO PLATA",          "https://hospitalricardolimardo.gob.do/",         TRUE,
  "MORILLO KING",         "LA VEGA",              "LA VEGA",               "https://hospitalmorilloking.gob.do/",            TRUE,
  "INMACULADA CONCEPCION","SANCHEZ RAMIREZ",      "COTUI",                 "https://sns.gob.do/publicaciones/inauguran-hospital-inmaculada-concepcion-cotui/", TRUE,
  "JUAN PABLO PINA",      "SAN CRISTOBAL",        "SAN CRISTOBAL",         "https://www.waze.com/live-map/directions/do/san-cristobal/san-cristobal/hospital-juan-pablo-pina", TRUE,
  "FRANCISCO GONZALVO",   "LA ROMANA",            "LA ROMANA",             "https://hospitalgonzalvo.gob.do/contacto/",      TRUE,
  "ANTONIO MUSA",         "SAN PEDRO DE MACORIS", "SAN PEDRO DE MACORIS",  "https://hospitalantoniomusa.gob.do/contacto/",   TRUE,
  "JAIME MOTA",           "BARAHONA",             "BARAHONA",              "https://hospitaljaimemota.gob.do/contacto/",     TRUE,
  "MUNICIPAL DE IMBERT",  "PUERTO PLATA",         "IMBERT",                "https://hospitaldeimbert.gob.do/",               FALSE
) |> as.data.table()

# attach adm3_pcode from the authoritative crosswalk (no hardcoded codes)
resolved <- merge(resolved, xw[, .(prov_norm, muni_norm, adm3_pcode, adm3_name)],
                  by = c("prov_norm","muni_norm"), all.x = TRUE)
stopifnot(sum(is.na(resolved$adm3_pcode)) == 0)   # every resolution maps to a real municipio

# attach the actual hospital name + opening year from the AU file (match by token)
pc <- as.data.table(read_excel(RAW$pcua_xlsx, sheet = "Unidades"))
hn <- names(pc)[grepl("Hospital", names(pc))][1]; yn <- names(pc)[grepl("A.o en funcion", names(pc))][1]
setnames(pc, c(hn, yn), c("hosp","year"))
pc[, hosp_norm := norm_name(hosp)]
resolved[, hosp_name := sapply(hosp_token, function(tok) {
  hit <- pc$hosp[grepl(tok, pc$hosp_norm, fixed = TRUE)]
  if (length(hit) >= 1) hit[1] else NA_character_
})]
resolved[, open_year := sapply(hosp_token, function(tok) {
  y <- pc$year[grepl(tok, pc$hosp_norm, fixed = TRUE)]
  if (length(y) >= 1) y[1] else NA_real_
})]
stopifnot(sum(is.na(resolved$hosp_name)) == 0)    # every token matched exactly one hospital

setcolorder(resolved, c("hosp_name","hosp_token","prov_norm","muni_norm","adm3_name",
                        "adm3_pcode","is_cabecera","open_year","source_url"))
cat("Resolved", nrow(resolved), "blank-municipio AU units (", sum(!resolved$is_cabecera),
    "NOT in cabecera):\n")
print(resolved[, .(hosp_token, muni_norm, adm3_pcode, is_cabecera, open_year)])

fwrite(resolved, file.path(DIR_CLEAN, "pcua_blank_muni_resolved.csv"))
cat("\nSaved -> data/clean/pcua_blank_muni_resolved.csv\n")
