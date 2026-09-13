# ============================================================================
# 05p_educ_build.R — MINERD OAI-0877-2026 (SIP-502F3B01, received 2026-07-29):
# center x nivel x grado x sexo enrollment WITH final condition (Promovido /
# Reprobado / Abandono) and Sobreedad, school years 2015-16..2024-25.
# Builds the municipio x school-year x nivel x sexo panel for the education
# margin (§59.28: joins THIS paper). Matching machinery mirrors 05h2 (exact
# name within province + LA MATA alias + fuzzy<=3), fold-3 to 155 geography.
# ALL SECTORS aggregated (population-relevant outcome; Sector not conditioned
# on, so the OAI-0790 sector/planta column-swap gotcha cannot bite).
# Inspection notes (2026-07-29, verified in python before writing this):
#   - header row = sheet row 4 ("Centro...Municipio...Nivel,Grado,Sexo,...");
#     detected dynamically per sheet anyway.
#   - identity Total general = Promovido + Reprobado + Abandono holds EXACTLY
#     (share 1.0000) in 2024-25; asserted >= 99.5% per sheet here.
#   - Sobreedad columns VARY by sheet (one column in early years; two rezago
#     tiers later) -> summed by name pattern "Sobreedad".
# Output: data/clean/educ_condicion_muni.rds
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})

norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155<- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)
F1 <- file.path(DRPAPER,"analysis/datasets/SAIP",
                "OAI-0877-2026 Matricula Desagregada 2015-2025 Condicion Final y Sobreedad.xlsx")
stopifnot(file.exists(F1))

mc <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[
        , .(prov_nm=norm(prov_name), muni_nm=norm(adm3_name), adm3_pcode)]

pieces <- list()
for(s in excel_sheets(F1)){
  peek <- suppressMessages(as.matrix(read_excel(F1, sheet=s, col_names=FALSE, n_max=6)))
  hr <- which(apply(peek, 1, function(r) any(r=="Municipio", na.rm=TRUE) & any(r=="Nivel", na.rm=TRUE)))[1]
  stopifnot(!is.na(hr))
  d <- suppressMessages(as.data.table(read_excel(F1, sheet=s, skip=hr-1)))
  need <- c("Provincia","Municipio","Nivel","Sexo","Total general","Promovido","Reprobado","Abandono")
  stopifnot(all(need %in% names(d)))
  socols <- grep("^Sobreedad", names(d), value=TRUE)
  for(cn in c("Total general","Promovido","Reprobado","Abandono", socols))
    d[, (cn) := fifelse(is.na(suppressWarnings(as.numeric(get(cn)))), 0, suppressWarnings(as.numeric(get(cn))))]
  # accounting identity (Total = final conditions) — the SAIP "Total general" answer
  idok <- d[, mean(abs(`Total general` - (Promovido + Reprobado + Abandono)) <= 0.5)]
  stopifnot(idok >= 0.995)
  # Sobreedad: absent entirely in the 2018-19 sheet (delivery gap, verified
  # 2026-07-29) -> NA that year, NOT zero. Two rezago tiers in 2024-25 summed.
  if (length(socols)) d[, sobre := Reduce(`+`, .SD), .SDcols = socols] else d[, sobre := NA_real_]
  d <- d[, .(prov_nm=norm(Provincia), muni_nm=norm(Municipio), nivel=norm(Nivel),
             sexo=fifelse(norm(Sexo)=="FEMENINO","F","M"),
             mat=`Total general`, prom=Promovido, rep=Reprobado, aband=Abandono, sobre)]
  d <- d[nivel %in% c("SECUNDARIO","PRIMARIO")]              # regular levels only (adults/PREPARA excluded)
  d[, school_year := s]
  pieces[[s]] <- d
  cat(sprintf("  [%s] rows %s | identity ok %.4f | sobre cols: %d\n",
              s, format(nrow(d), big.mark=","), idok, length(socols)))
}
ed <- rbindlist(pieces)
ed[, year_end := as.integer(substr(school_year, 6, 9))]

# municipio -> pcode (05h2 pattern)
ed[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
ed <- merge(ed, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
un <- ed[is.na(adm3_pcode), .N, by=.(prov_nm, muni_nm)][order(-N)]
if(nrow(un)){cat("unmapped labels:\n"); print(un[1:min(8,nrow(un))], class=FALSE)}
for(i in seq_len(nrow(un))){
  cand <- mc[prov_nm==un$prov_nm[i]]
  if(!nrow(cand)) next
  dd <- drop(adist(un$muni_nm[i], cand$muni_nm))
  if(min(dd) <= 3 && sum(dd==min(dd))==1)
    ed[prov_nm==un$prov_nm[i] & muni_nm==un$muni_nm[i], adm3_pcode := cand$adm3_pcode[which.min(dd)]]
}
nb <- nrow(ed); ed <- ed[!is.na(adm3_pcode)]
cat(sprintf("mapped: %.3f%% of rows (%s of %s)\n", 100*nrow(ed)/nb,
            format(nrow(ed),big.mark=","), format(nb,big.mark=",")))
stopifnot(nrow(ed)/nb > 0.995)
ed[, adm3_pcode := to155(adm3_pcode)]

pan <- ed[, .(mat=sum(mat), prom=sum(prom), rep=sum(rep), aband=sum(aband), sobre=sum(sobre)),
          by=.(adm3_pcode, school_year, year_end, nivel, sexo)]
cat("\npanel rows:", nrow(pan), "| munis:", pan[,uniqueN(adm3_pcode)],
    "| years:", pan[,uniqueN(year_end)], "\n")
cat("national SECUNDARIO F enrollment + dropout rate by year:\n")
print(pan[nivel=="SECUNDARIO" & sexo=="F",
          .(mat=sum(mat), dropout_pct=round(100*sum(aband)/sum(mat),2)), keyby=year_end], class=FALSE)
saveRDS(pan, file.path(DIR_CLEAN, "educ_condicion_muni.rds"))
cat("saved -> data/clean/educ_condicion_muni.rds\n")
