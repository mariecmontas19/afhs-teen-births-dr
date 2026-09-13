# ============================================================================
# 01_crosswalk.R  —  Authoritative municipio crosswalk (the spine)
#
# Reconciles THREE identifier systems onto one canonical municipio key:
#   (a) ONE numeric codes  (census + population xlsx)   prov 1-32, muni 1..N
#   (b) OCHA pcodes         (shapefile)                  adm3_pcode e.g. DOM010901
#   (c) names               (births, AU)
#
# Strategy: shapefile ADM3 (158, 2025) is the reference geography. The population
# xlsx (ONE, province-ordered) provides the municipio-name spine in ONE order;
# we VALIDATE its per-province municipio counts against the census before trusting it.
#
# This script (Part A-D here): parse province codes, classify xlsx sheets, assign
# provinces, validate counts. Name-matching to shapefile/births/AU follows once
# the spine is validated.
# ============================================================================

source(here::here("code", "R", "00_config.R"))
suppressPackageStartupMessages({ library(readxl); library(data.table); library(sf); library(stringr); library(stringi) })

# ---- name normalizer: strip accents (Latin-ASCII), uppercase, drop punct, squish ----
# Uses stringi::stri_trans_general; iconv ASCII//TRANSLIT is unreliable on macOS
# (mangles accented uppercase, e.g. DAJABON -> "DAJAB ON").
norm_name <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")   # á->a, ñ->n, í->i, Ó->O ...
  x <- toupper(x)
  x <- gsub("[^A-Z0-9 ]", " ", x)            # drop punctuation/apostrophes
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# ---- Part A: official province codes 1-32 (from verified Persona codebook) ----
# Transcribed from Libro_de_códigos...Persona_XCNPV.htm (verified 2026-06-18),
# then CROSS-VALIDATED against the shapefile's 32 ADM2 province names below.
prov_codebook <- tribble(
  ~prov_code, ~prov_name,
  1,"DISTRITO NACIONAL", 2,"AZUA", 3,"BAORUCO", 4,"BARAHONA", 5,"DAJABON",
  6,"DUARTE", 7,"ELIAS PINA", 8,"EL SEIBO", 9,"ESPAILLAT", 10,"INDEPENDENCIA",
  11,"LA ALTAGRACIA", 12,"LA ROMANA", 13,"LA VEGA", 14,"MARIA TRINIDAD SANCHEZ",
  15,"MONTE CRISTI", 16,"PEDERNALES", 17,"PERAVIA", 18,"PUERTO PLATA",
  19,"HERMANAS MIRABAL", 20,"SAMANA", 21,"SAN CRISTOBAL", 22,"SAN JUAN",
  23,"SAN PEDRO DE MACORIS", 24,"SANCHEZ RAMIREZ", 25,"SANTIAGO",
  26,"SANTIAGO RODRIGUEZ", 27,"VALVERDE", 28,"MONSENOR NOUEL", 29,"MONTE PLATA",
  30,"HATO MAYOR", 31,"SAN JOSE DE OCOA", 32,"SANTO DOMINGO"
) |> mutate(prov_norm = norm_name(prov_name))

# ---- shapefile ADM3 (reference geography, 158 municipios) ----
shp <- st_read(RAW$shp_muni, quiet = TRUE) |> st_drop_geometry()
shp <- as.data.table(shp)[, .(adm3_pcode, adm3_name = adm3_es, adm2_pcode, prov_shp = adm2_es)]
shp[, `:=`(muni_norm = norm_name(adm3_name), prov_norm = norm_name(prov_shp))]

# Cross-validate province names: codebook vs shapefile (independent sources)
shp_provs <- sort(unique(shp$prov_norm)); cb_provs <- sort(prov_codebook$prov_norm)
cat("Province names: codebook vs shapefile identical set? ",
    setequal(shp_provs, cb_provs), "\n")
miss <- setdiff(cb_provs, shp_provs); extra <- setdiff(shp_provs, cb_provs)
if (length(miss))  cat("  in codebook not shapefile:", paste(miss, collapse=" | "), "\n")
if (length(extra)) cat("  in shapefile not codebook:", paste(extra, collapse=" | "), "\n")

# ---- Part B: classify population-xlsx sheets ----
sh <- excel_sheets(RAW$pop_muni_xlsx)
xs <- data.table(idx = seq_along(sh), sheet = sh)
xs[, sheet_norm := norm_name(sheet)]
xs[, sheet_norm := gsub("^PROVINCIA ", "", sheet_norm)]   # strip "Provincia " prefix
prov_set <- prov_codebook$prov_norm
xs[, had_prefix := grepl("^PROVINCIA ", norm_name(sheet))]
xs[, is_total_country := sheet_norm %in% c("TOTAL PAIS")]
xs[, is_index := sheet_norm %in% c("INDICE")]
# Provinces that HAVE a "Provincia X" header sheet (their total is the prefixed one;
# the bare same-named sheet is the cabecera MUNICIPIO, not a total).
provs_with_prefix <- xs[had_prefix == TRUE, unique(sheet_norm)]
# A sheet is a PROVINCE TOTAL if it had the prefix, OR it is a bare province name for
# a province that has NO prefixed header. Distrito Nacional is its own municipio (prov 1).
xs[, is_prov_total := !is_index & !is_total_country & sheet_norm != "DISTRITO NACIONAL" &
       (had_prefix | (sheet_norm %in% prov_set & !(sheet_norm %in% provs_with_prefix)))]
xs[, is_muni := !is_index & !is_total_country & !is_prov_total]

# Assign running province to each municipio sheet (province set by last header seen)
xs[, cur_prov := NA_character_]
running <- NA_character_
for (i in seq_len(nrow(xs))) {
  if (xs$is_prov_total[i]) running <- xs$sheet_norm[i]
  if (xs$sheet_norm[i] == "DISTRITO NACIONAL") running <- "DISTRITO NACIONAL"
  if (xs$is_muni[i]) xs$cur_prov[i] <- running
}

cat("\nclassification counts: index", sum(xs$is_index), " country", sum(xs$is_total_country),
    " prov_totals", sum(xs$is_prov_total), " municipios", sum(xs$is_muni), "\n")

# ---- Part C: census municipio counts per province ----
cen <- fread(RAW$census_csv, select = c("PROVINCIA","MUNICIPIO"), showProgress = FALSE)
cen_pp <- cen[, .(n_census = uniqueN(MUNICIPIO)), by = .(prov_code = PROVINCIA)]
cen_pp <- merge(cen_pp, prov_codebook[, c("prov_code","prov_norm")], by = "prov_code")

# ---- Part D: validate xlsx municipio counts per province vs census ----
xls_pp <- xs[is_muni == TRUE, .(n_xlsx = .N), by = .(prov_norm = cur_prov)]
cmp <- merge(cen_pp, xls_pp, by = "prov_norm", all = TRUE)[order(prov_code)]
cmp[, diff := n_xlsx - n_census]
cat("\n--- municipios per province: census vs xlsx ---\n")
print(cmp[, .(prov_code, prov_norm, n_census, n_xlsx, diff)])
cat("\nTOT:  census =", sum(cmp$n_census, na.rm=TRUE), " xlsx =", sum(cmp$n_xlsx, na.rm=TRUE),
    " | provinces with mismatch:", sum(cmp$diff != 0, na.rm=TRUE), "\n")

# ---- Part E: match xlsx municipios to shapefile ADM3 (within province) ----
xmun <- xs[is_muni == TRUE, .(sheet, idx, prov_norm = cur_prov,
                              muni_norm = norm_name(sub("^Provincia ", "", sheet)))]
# join on (province, municipio) normalized names
m <- merge(xmun, shp[, .(prov_norm, muni_norm, adm3_pcode, adm3_name)],
           by = c("prov_norm","muni_norm"), all.x = TRUE)
cat("\n--- Part E: xlsx municipio -> shapefile match ---\n")
cat("xlsx municipios:", nrow(xmun), " matched to pcode:", sum(!is.na(m$adm3_pcode)),
    " UNMATCHED:", sum(is.na(m$adm3_pcode)), "\n")
if (any(is.na(m$adm3_pcode))) {
  cat("\nUNMATCHED xlsx municipios (need name reconciliation):\n")
  print(m[is.na(adm3_pcode), .(prov_norm, sheet, muni_norm)])
}
# shapefile municipios with NO xlsx sheet (the 3 post-2020 + any name mismatches)
shp_unm <- shp[!adm3_pcode %in% m$adm3_pcode, .(prov_norm, adm3_name, adm3_pcode)]
cat("\nShapefile municipios NOT matched by any xlsx sheet (", nrow(shp_unm), "):\n", sep="")
print(shp_unm[order(prov_norm)])

# ---- Part F: manual spelling overrides (verified by eye, same province) ----
# Maps xlsx normalized name -> shapefile normalized name. Each pair is an obvious
# orthographic variant within the SAME province (plural/accent/Villa-prefix/typo).
override <- tribble(
  ~prov_norm,            ~muni_norm,                ~shp_norm,
  "BARAHONA",            "LA CIENEGA",              "LA CIENAGA",
  "DISTRITO NACIONAL",   "DISTRITO NACIONAL",       "SANTO DOMINGO DE GUZMAN",
  "DUARTE",              "EUGENIO MARIA DE HOSTO",  "EUGENIO MARIA DE HOSTOS",
  "INDEPENDENCIA",       "POSTRE RIO",              "POSTRER RIO",
  "MONTE CRISTI",        "CASTANUELA",              "CASTANUELAS",
  "MONTE CRISTI",        "GUAYABIN",                "GUAYUBIN",
  "MONTE CRISTI",        "VILLA VAZQUEZ",           "VILLA VASQUEZ",
  "SAN CRISTOBAL",       "BAJOS DE HAINAS",         "BAJOS DE HAINA",
  "SAN CRISTOBAL",       "CAMBITA GARABITO",        "CAMBITA GARABITOS",
  "SAN CRISTOBAL",       "LOS CACAO",               "LOS CACAOS",
  "SANCHEZ RAMIREZ",     "CEVICO",                  "CEVICOS",
  "SANCHEZ RAMIREZ",     "LA MATA",                 "VILLA LA MATA",
  "SANTIAGO",            "VILLA BISONO",            "BISONO",
  "SANTIAGO",            "VILLA GONZALES",          "VILLA GONZALEZ",
  "SANTIAGO RODRIGUEZ",  "LOS ALMACIGOS",           "VILLA LOS ALMACIGOS"
)
xmun2 <- merge(xmun, override, by = c("prov_norm","muni_norm"), all.x = TRUE)
xmun2[, match_norm := fifelse(is.na(shp_norm), muni_norm, shp_norm)]
m2 <- merge(xmun2, shp[, .(prov_norm, match_norm = muni_norm, adm3_pcode, adm3_name)],
            by = c("prov_norm","match_norm"), all.x = TRUE)
stopifnot(sum(is.na(m2$adm3_pcode)) == 0)   # every xlsx municipio now mapped
cat("\n--- Part F: after overrides, xlsx municipios mapped:",
    sum(!is.na(m2$adm3_pcode)), "/", nrow(m2), "---\n")

# ---- Build final crosswalk: all 158 municipios ----
xw <- shp[, .(adm3_pcode, adm3_name, prov_norm)]
xw <- merge(xw, as.data.table(prov_codebook)[, .(prov_norm, prov_code, prov_name)], by = "prov_norm", all.x = TRUE)
xw <- merge(xw, m2[, .(adm3_pcode, xlsx_sheet = sheet, xlsx_idx = idx)], by = "adm3_pcode", all.x = TRUE)
xw[, in_xlsx_2015_2020 := !is.na(xlsx_sheet)]
xw <- xw[order(prov_code, adm3_pcode)]
cat("\nFinal crosswalk:", nrow(xw), "municipios |", sum(xw$in_xlsx_2015_2020),
    "in 2015-2020 xlsx | NEW (census/shp only):", sum(!xw$in_xlsx_2015_2020), "\n")
cat("The 3 post-2020 municipios (no 2015-2020 population):\n")
print(xw[in_xlsx_2015_2020 == FALSE, .(prov_name, adm3_name, adm3_pcode)])

# lookup key for matching source names -> pcode
xw[, muni_norm := norm_name(adm3_name)]
key <- xw[, .(prov_norm, muni_norm, adm3_pcode, adm3_name)]

# ---- Part G: match BIRTHS municipio names to crosswalk ----
cat("\n--- Part G: BIRTHS municipio match ---\n")
byr <- setdiff(excel_sheets(RAW$births_xlsx), "INDEX")
# find prov/muni columns PER SHEET (year sheets have different naming, e.g. 2025 no-spaces)
ncn <- function(x) gsub("[^a-z0-9]", "", tolower(stringi::stri_trans_general(x, "Latin-ASCII")))
bdist <- rbindlist(lapply(byr, function(s) {
  d <- read_excel(RAW$births_xlsx, sheet = s, col_types = "text"); nn <- ncn(names(d))
  pc <- which(nn == "provinciamadre"); mc <- which(nn == "municipiomadre")
  unique(data.table(prov_raw = if(length(pc)) d[[names(d)[pc[1]]]] else NA_character_,
                    muni_raw = if(length(mc)) d[[names(d)[mc[1]]]] else NA_character_))
}))
bdist <- unique(bdist)
bdist[, `:=`(prov_norm = norm_name(prov_raw), muni_norm = norm_name(muni_raw))]
bm <- merge(bdist, key, by = c("prov_norm","muni_norm"), all.x = TRUE)
cat("distinct births (prov,muni) pairs:", nrow(bdist),
    " matched:", sum(!is.na(bm$adm3_pcode)), " UNMATCHED:", sum(is.na(bm$adm3_pcode)), "\n")
if (any(is.na(bm$adm3_pcode))) {
  cat("UNMATCHED births pairs (raw):\n")
  print(bm[is.na(adm3_pcode), .(prov_raw, muni_raw, prov_norm, muni_norm)][order(prov_norm)])
}

# ---- Part G2: match AU municipio names to crosswalk ----
cat("\n--- Part G2: AU municipio match ---\n")
pc <- as.data.table(read_excel(RAW$pcua_xlsx, sheet = "Unidades"))
setnames(pc, old = names(pc)[grep("Provincia", names(pc))][1], new = "prov_raw")
setnames(pc, old = names(pc)[grep("Municipio", names(pc))][1], new = "muni_raw")
pc[, `:=`(prov_norm = norm_name(prov_raw), muni_norm = norm_name(muni_raw))]
pcm <- merge(pc[, .(prov_raw, muni_raw, prov_norm, muni_norm)], key,
             by = c("prov_norm","muni_norm"), all.x = TRUE)
cat("AU units:", nrow(pc), " matched:", sum(!is.na(pcm$adm3_pcode)),
    " UNMATCHED/blank muni:", sum(is.na(pcm$adm3_pcode)), "\n")
print(pcm[is.na(adm3_pcode), .(prov_raw, muni_raw)][order(prov_raw)])

# ---- Part H: verified alias layer + re-match (applied to births & AU) ----
# Province-name aliases (source spelling -> canonical codebook spelling).
# (AZUA DE COMPOSTELA / SAN JUAN DE LA MAGUANA = full official province names used in some
#  birth-year sheets; verified via 03 unmatched-pairs diagnostic 2026-06-19.)
prov_alias <- c("BAHORUCO" = "BAORUCO", "SANJUAN" = "SAN JUAN",
                "AZUA DE COMPOSTELA" = "AZUA", "SAN JUAN DE LA MAGUANA" = "SAN JUAN")
# Municipio aliases, keyed (canonical prov_norm, source muni_norm) -> canonical muni_norm.
muni_alias <- tribble(
  ~prov_norm,           ~muni_src,                 ~muni_canon,
  "SAN JUAN",           "SAN JUAN DE LA MAGUANA",   "SAN JUAN",
  "SAN JUAN",           "SANJUANDELAMAGUANA",       "SAN JUAN",
  "AZUA",               "AZUA DE COMPOSTELA",       "AZUA",
  "HATO MAYOR",         "HATO MAYOR DEL REY",       "HATO MAYOR",
  "SANCHEZ RAMIREZ",    "LA MATA",                  "VILLA LA MATA",
  "SANTIAGO RODRIGUEZ", "SABANETA",                 "SAN IGNACIO DE SABANETA"
) |> as.data.table()

# generic matcher: raw (prov,muni) -> adm3_pcode using aliases + DN special-case
match_to_xw <- function(prov_raw, muni_raw) {
  d <- data.table(prov_norm = norm_name(prov_raw), muni_norm = norm_name(muni_raw))
  d[prov_norm %in% names(prov_alias), prov_norm := prov_alias[prov_norm]]
  # DN hospitals are coded under prov "Santo Domingo" in some sources -> DN is prov 1
  d[muni_norm == "DISTRITO NACIONAL", prov_norm := "DISTRITO NACIONAL"]
  d[muni_norm == "DISTRITO NACIONAL", muni_norm := "SANTO DOMINGO DE GUZMAN"]
  d <- merge(d, muni_alias, by.x = c("prov_norm","muni_norm"),
             by.y = c("prov_norm","muni_src"), all.x = TRUE, sort = FALSE)
  d[!is.na(muni_canon), muni_norm := muni_canon]
  merge(d, key, by = c("prov_norm","muni_norm"), all.x = TRUE, sort = FALSE)$adm3_pcode
}

bdist[, adm3_pcode := match_to_xw(prov_raw, muni_raw)]
cat("\n--- Part H: re-match with aliases ---\n")
cat("BIRTHS distinct pairs:", nrow(bdist), " matched:", sum(!is.na(bdist$adm3_pcode)),
    " UNMATCHED:", sum(is.na(bdist$adm3_pcode)), "\n")
if (any(is.na(bdist$adm3_pcode))) print(bdist[is.na(adm3_pcode), .(prov_raw, muni_raw)])

pc[, adm3_pcode := match_to_xw(prov_raw, muni_raw)]
pc[, blank_muni := is.na(muni_raw) | norm_name(muni_raw) == ""]
cat("\nPCUA units:", nrow(pc), " matched:", sum(!is.na(pc$adm3_pcode)),
    " | blank municipio (resolved by web-verified table 01b, NOT cabecera assumption):", sum(pc$blank_muni),
    " | non-blank still unmatched:", sum(is.na(pc$adm3_pcode) & !pc$blank_muni), "\n")
if (any(is.na(pc$adm3_pcode) & !pc$blank_muni))
  print(pc[is.na(adm3_pcode) & !blank_muni, .(prov_raw, muni_raw)])

# save alias tables for reuse downstream
saveRDS(list(prov_alias = prov_alias, muni_alias = muni_alias, norm_name = norm_name),
        file.path(DIR_CLEAN, "muni_match_helpers.rds"))

saveRDS(xw, file.path(DIR_CLEAN, "muni_crosswalk.rds"))
fwrite(xw, file.path(DIR_CLEAN, "muni_crosswalk.csv"))
cat("\nSaved crosswalk -> data/clean/muni_crosswalk.{rds,csv} + muni_match_helpers.rds\n")
