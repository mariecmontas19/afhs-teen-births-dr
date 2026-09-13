# ============================================================================
# 05q_pruebas_build.R — Pruebas Nacionales (datos.gob.do, MINERD, ODbL) ->
# municipality x year panel of test performance and test-taking. §59.48.
#
# Source file (archived with provenance README, downloaded 2026-08-03):
#   analysis/datasets/Pruebas Nacionales/pruebas_nacionales_centro_2016_2024.csv
#   SEMICOLON-delimited, latin-1; center-level MEAN scores + counts.
# Verified gotchas honored here:
#   - years 2016-2020, 2022-2024; NO 2021; 2020 has counts but ZERO scores
#     (COVID) -> 2020 EXCLUDED from both scores and takers (auto-promotion
#     counts are not test-takers).
#   - 2024 scale change (means ~18 -> ~62) -> z-standardize center means
#     WITHIN year x subject x modality (student-weighted), never raw levels.
#   - convocatoria 1 only (retakes are selected).
#   - modalities: MEDIA general/academica + tecnico-profesional (BASICA is
#     2016-only; ADULTOS is a different population; ARTES tiny).
# Municipality assignment: center code -> adm3 via the OAI-0790 center list
# (codes embedded in its "Centro" column as "########## - NAME"), name-mapped
# to adm3 with the proven 05h2 logic; fallback = 6-digit code prefix when the
# prefix maps to a unique municipality. Match rates PRINTED and saved.
# Output: data/clean/pruebas_muni.rds + output/tables/pruebas_build_diag.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold155 <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold155), unname(fold155[p]), p)

## ---- 1. center code -> adm3 from OAI-0790 (all sheets) ---------------------
F1 <- file.path(DRPAPER,"analysis/datasets/SAIP/OAI-0790-2026 Lista de centros con datos de niveles y tandas 2015-2025.xlsx")
mc <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[
        , .(prov_nm=norm(prov_name), muni_nm=norm(adm3_name), adm3_pcode)]
# The 8-digit NATIONAL center code lives in the "Planta Fisica" column
# ("16000218 - NAME"); "Centro" carries a 5-digit internal id. And per the
# 05h2 gotcha, Planta Fisica and Sector DATA are swapped in some sheets.
# Robust rule: per sheet, use whichever column mostly matches "########## - ".
pieces <- list()
for(s in excel_sheets(F1)){
  peek <- suppressMessages(as.matrix(read_excel(F1, sheet=s, col_names=FALSE, n_max=6)))
  hr <- which(apply(peek,1,function(r) any(r=="Regional",na.rm=TRUE)&any(r=="Municipio",na.rm=TRUE)))[1]
  d <- suppressMessages(as.data.table(read_excel(F1, sheet=s, skip=hr-1)))
  pv <- names(d)[toupper(names(d))=="PROVINCIA"][1]; mn <- names(d)[toupper(names(d))=="MUNICIPIO"][1]
  hit8 <- function(v) mean(grepl("^\\s*\\d{7,8}\\s*-", as.character(v)), na.rm=TRUE)
  cc <- names(d)[vapply(d, hit8, 0) > 0.5][1]
  stopifnot(!is.na(cc), !is.na(pv), !is.na(mn))
  dt <- names(d)[toupper(names(d))=="DISTRITO"][1]
  x <- d[, .(centro=as.character(get(cc)), prov_nm=norm(get(pv)), muni_nm=norm(get(mn)),
             distrito=sub("^\\s*(\\d{4}).*$","\\1",as.character(get(dt))))]
  x[, code := sub("^\\s*(\\d{7,8})\\s*-.*$", "\\1", centro)]
  x <- x[grepl("^\\d{7,8}$", code)][, code := formatC(as.integer(code), width=8, flag="0")]
  pieces[[s]] <- unique(x[, .(code, prov_nm, muni_nm, distrito)])
}
cw <- unique(rbindlist(pieces))
cw[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
cw <- merge(cw, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
# fuzzy pass within province for unmapped names (05h2 logic, agrep)
un <- unique(cw[is.na(adm3_pcode), .(prov_nm, muni_nm)])
for(i in seq_len(nrow(un))){
  cand <- mc[prov_nm==un$prov_nm[i]]
  hit <- cand[agrepl(un$muni_nm[i], muni_nm, max.distance=0.15)]
  if(nrow(hit)==1) cw[prov_nm==un$prov_nm[i] & muni_nm==un$muni_nm[i], adm3_pcode := hit$adm3_pcode]
}
cw <- cw[!is.na(adm3_pcode)]
# one code -> one municipality? (codes can repeat across sheets)
amb <- cw[, .(n=uniqueN(adm3_pcode)), by=code][n>1]
cat("OAI codes:", cw[,uniqueN(code)], "| ambiguous (multi-muni):", nrow(amb), "-> dropped\n")
cw <- cw[!code %in% amb$code]
code2adm <- unique(cw[, .(code, adm3_pcode)])
# distrito -> muni where unique (tier-3 fallback; distritos educativos usually nest in municipios)
d2m <- unique(cw[grepl("^\\d{4}$", distrito), .(distrito, adm3_pcode)])
d2m <- d2m[, if(.N==1) .SD, by=distrito]
cat("unique distrito->muni mappings:", nrow(d2m), "\n")
# 6-digit prefix -> muni where unique (fallback)
pre <- unique(code2adm[, .(pre6=substr(code,1,6), adm3_pcode)])
pre <- pre[, if(.N==1) .SD, by=pre6]
cat("unique 6-digit prefixes usable as fallback:", nrow(pre), "\n")

## ---- 2. parse pruebas file --------------------------------------------------
PF <- file.path(DRPAPER,"analysis/datasets/Pruebas Nacionales/pruebas_nacionales_centro_2016_2024.csv")
pn <- fread(PF, sep=";", encoding="Latin-1")
setnames(pn, c("periodo","conv","regional","distrito","modalidad","code","centro",
               "esp","mat","soc","nat","n_est","n_f","n_m","n_prom","n_apl"))
pn[, code := formatC(as.integer(code), width=8, flag="0")]
pn[, modalidad := norm(modalidad)]
pn[, grp := fifelse(grepl("GENERAL|ACADEMIC", modalidad), "GEN",
            fifelse(grepl("TECNICO", modalidad), "TEC", NA_character_))]
for (v in c("esp","mat","soc","nat","n_est","n_f","n_m","n_prom")) pn[, (v) := suppressWarnings(as.numeric(get(v)))]
pn <- pn[!is.na(grp) & conv=="1" & periodo %in% c(2016:2019, 2022:2024) &
         !is.na(esp) & esp>0 & !is.na(n_est) & n_est>0]
## PN2024 fix (2026-08-12, §59.83): the public file's 2024 slice labeled conv 1
## carries ~3.3x the true first-convocatoria examinee counts (368,032 students;
## more than terminal-grade enrollment — the §59.50 provisional flag). Replaced
## with MINERD's authoritative conv-1 student microdata, aggregated to the same
## center x modality-group schema by 05q0_pn2024_centro.R (subject columns
## verified = the resp_ test score, corr .89, means 56/57).
pn <- pn[periodo != 2024L]
p24 <- as.data.table(readRDS(file.path(DIR_CLEAN,"pn2024_centro.rds")))
pn <- rbind(pn, p24[!is.na(esp) & esp>0 & n_est>0], fill=TRUE)
cat("\n2024 slice replaced by PN2024 conv-1 microdata:", nrow(p24), "center-grp rows |",
    format(sum(p24$n_est), big.mark=","), "students\n")
cat("pruebas rows kept (conv 1, GEN+TEC, score years):", nrow(pn),
    "| students:", format(sum(pn$n_est), big.mark=","), "\n")

## ---- 3. municipality assignment + match rate -------------------------------
pn <- merge(pn, code2adm, by="code", all.x=TRUE)
n0 <- pn[!is.na(adm3_pcode), sum(n_est)]
pn[, pre6 := substr(code,1,6)]
pn <- merge(pn, setnames(copy(pre), "adm3_pcode", "adm3_pre"), by="pre6", all.x=TRUE)
pn[is.na(adm3_pcode), adm3_pcode := adm3_pre]
n1 <- pn[!is.na(adm3_pcode), sum(n_est)]
pn[, distrito := formatC(suppressWarnings(as.integer(distrito)), width=4, flag="0")]
pn <- merge(pn, setnames(copy(d2m), "adm3_pcode", "adm3_dst"), by="distrito", all.x=TRUE)
pn[is.na(adm3_pcode), adm3_pcode := adm3_dst]
match_exact <- 100*n0/sum(pn$n_est)
match_pre   <- 100*n1/sum(pn$n_est)
match_total <- 100*pn[!is.na(adm3_pcode), sum(n_est)]/sum(pn$n_est)
cat(sprintf("student-weighted match: exact %.1f%% | +prefix %.1f%% | +distrito %.1f%%\n",
    match_exact, match_pre, match_total))
cat("match rate by year (%):\n")
print(pn[, .(pct=round(100*sum(n_est[!is.na(adm3_pcode)])/sum(n_est),1)), by=periodo][order(periodo)], class=FALSE)
pn <- pn[!is.na(adm3_pcode)][, adm3_pcode := to155(adm3_pcode)]

## ---- 4. z-standardize within year x subject x modality x SCORING REGIME ----
# The test moved from a 30-point to a 100-point scale. 2016-2022 are entirely
# old-scale (no center mean exceeds 30 in 43,623 rows) and 2024 is entirely
# new-scale (99.9% above 30), where a within-cell z is invariant to the scale.
# 2023 is TRANSITIONAL: both scales coexist within modality (56% of GEN centers
# above 30), so a plain within-cell z mixes regimes. Fix: classify each
# center-year by its MAX score across the four subjects (bimodal: old cluster
# tops out ~29, new cluster starts ~40; 27 of 3,599 centers in [25,35], <1%);
# max>30 = new scale (certain: 30 is the old ceiling), else old. Then
# standardize within year x subject x modality x regime.
pn[, maxsc := pmax(esp, mat, soc, nat, na.rm=TRUE)]
pn[, regime := fifelse(maxsc > 30, "new", "old")]
cat("\nscoring regime by year (centers):\n")
print(dcast(pn[, .N, by=.(periodo, regime)], periodo~regime, value.var="N", fill=0L), class=FALSE)
long <- melt(pn, id.vars=c("periodo","grp","adm3_pcode","n_est","regime"),
             measure.vars=c("esp","mat","soc","nat"), variable.name="subj", value.name="score")
long <- long[!is.na(score) & score>0]
long[, `:=`(mu = weighted.mean(score, n_est), sg = sqrt(sum(n_est*(score-weighted.mean(score,n_est))^2)/sum(n_est))),
     by=.(periodo, subj, grp, regime)]
long[, z := (score-mu)/sg]
muni <- long[, .(z = weighted.mean(z, n_est)), by=.(adm3_pcode, year_end=periodo, subj)]
muniW <- dcast(muni, adm3_pcode + year_end ~ subj, value.var="z")
setnames(muniW, c("esp","mat","soc","nat"), paste0("z_", c("esp","mat","soc","nat")))
muniW[, z_all := rowMeans(.SD, na.rm=TRUE), .SDcols=patterns("^z_")]
takers <- pn[, .(takers = sum(n_est), takers_f = sum(n_f, na.rm=TRUE),
                 takers_m = sum(n_m, na.rm=TRUE), passers = sum(n_prom, na.rm=TRUE)),
             by=.(adm3_pcode, year_end=periodo)]
out <- merge(muniW, takers, by=c("adm3_pcode","year_end"))
cat("\nmunicipality-year cells:", nrow(out), "| municipalities:", out[,uniqueN(adm3_pcode)],
    "| years:", paste(sort(unique(out$year_end)), collapse=","), "\n")
saveRDS(out, file.path(DIR_CLEAN,"pruebas_muni.rds"))
fwrite(data.table(match_exact=match_exact, match_total=match_total,
                  cells=nrow(out), munis=out[,uniqueN(adm3_pcode)]),
       file.path(DIR_TABLES,"pruebas_build_diag.csv"))
cat("saved -> data/clean/pruebas_muni.rds\n")
