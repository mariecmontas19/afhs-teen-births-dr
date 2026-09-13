# ============================================================================
# 05t_familia_moderators.R — municipality moderators from the Evaluación
# Diagnóstica 2019 3S FAMILY QUESTIONNAIRE (MINERD, received 2026-08-12) for
# Table A6's "Adolescent vulnerability and school attachment" panel (§59.92).
# Wave = school year 2018-19 -> PRE-treatment for every in-window opener.
#
# Four indicators (family-level, then family-weighted municipality means over
# valid responses; 0=Vacío and '*'=Multimarca dropped; codings verified against
# the workbook's Diccionario sheet, §59.86/59.89):
#   mom_secplus = share mother/tutora completed secondary+ (p25 in 5..7; 8=
#                 "No lo recuerdo" dropped)
#   expect_uni  = share family expects the student to complete university
#                 (p9_nivelesperado == 3)
#   parent_part = parental school-participation battery mean, 1=never..4=always
#                 (p7a reuniones, p7b/p7c extraescolares, p7d comités)
#   absent      = share student misses class a couple times a month or more
#                 (p15_ausencia in 3..5; 6="No sé" dropped)
# Center -> municipality via the OAI-0790 center list (05q logic); ~45% of the
# 122,269 families match (the questionnaire CENTRO_CODIGO is the evaluation-
# office code) — all 20 treated municipalities covered (§59.88).
# Output: data/clean/familia_moderators_2019.rds
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold155 <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold155), unname(fold155[p]), p)

## center -> adm3 (same OAI-0790 logic as 05q_pruebas_build.R)
F1 <- file.path(DRPAPER,"analysis/datasets/SAIP/OAI-0790-2026 Lista de centros con datos de niveles y tandas 2015-2025.xlsx")
mc <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[
        , .(prov_nm=norm(prov_name), muni_nm=norm(adm3_name), adm3_pcode)]
pieces <- list()
for(s in excel_sheets(F1)){
  peek <- suppressMessages(as.matrix(read_excel(F1, sheet=s, col_names=FALSE, n_max=6)))
  hr <- which(apply(peek,1,function(r) any(r=="Regional",na.rm=TRUE)&any(r=="Municipio",na.rm=TRUE)))[1]
  d <- suppressMessages(as.data.table(read_excel(F1, sheet=s, skip=hr-1)))
  pv <- names(d)[toupper(names(d))=="PROVINCIA"][1]; mn <- names(d)[toupper(names(d))=="MUNICIPIO"][1]
  hit8 <- function(v) mean(grepl("^\\s*\\d{7,8}\\s*-", as.character(v)), na.rm=TRUE)
  cc <- names(d)[vapply(d, hit8, 0) > 0.5][1]; stopifnot(!is.na(cc), !is.na(pv), !is.na(mn))
  x <- d[, .(centro=as.character(get(cc)), prov_nm=norm(get(pv)), muni_nm=norm(get(mn)))]
  x[, code := sub("^\\s*(\\d{7,8})\\s*-.*$", "\\1", centro)]
  x <- x[grepl("^\\d{7,8}$", code)][, code := formatC(as.integer(code), width=8, flag="0")]
  pieces[[s]] <- unique(x[, .(code, prov_nm, muni_nm)])
}
cw <- unique(rbindlist(pieces)); cw[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
cw <- merge(cw, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
un <- unique(cw[is.na(adm3_pcode), .(prov_nm, muni_nm)])
for(i in seq_len(nrow(un))){
  cand <- mc[prov_nm==un$prov_nm[i]]
  hit <- cand[agrepl(un$muni_nm[i], muni_nm, max.distance=0.15)]
  if(nrow(hit)==1) cw[prov_nm==un$prov_nm[i] & muni_nm==un$muni_nm[i], adm3_pcode := hit$adm3_pcode]
}
cw <- cw[!is.na(adm3_pcode)]
amb <- cw[, .(n=uniqueN(adm3_pcode)), by=code][n>1]
code2adm <- unique(cw[!code %in% amb$code, .(code, adm3_pcode)])

## questionnaire indicators
FQ <- file.path(DRPAPER,"analysis/datasets/Evaluacion Diagnostica/CuestionarioFamilia2019.xlsx")
q <- as.data.table(read_excel(FQ, sheet="Familia_EVA3roSec_2019_REVISADA", col_types="text"))
q[, code := formatC(suppressWarnings(as.integer(CENTRO_CODIGO)), width=8, flag="0")]
q <- q[grepl("^\\d{8}$", code)]
nv <- function(v){ y <- suppressWarnings(as.numeric(q[[v]])); y[y==0] <- NA_real_; y }
me <- nv("p25_educ_madre"); me[me==8] <- NA
ab <- nv("p15_ausencia");   ab[ab==6] <- NA
pp <- sapply(c("p7a_reuniones","p7b_part_extra","p7c_ayuda_extra","p7d_comites"), nv)
q[, `:=`(mom_secplus = as.integer(me>=5),
         expect_uni  = as.integer(nv("p9_nivelesperado")==3),
         parent_part = rowMeans(pp, na.rm=TRUE),
         absent      = as.integer(ab>=3))]
q[is.nan(parent_part), parent_part := NA_real_]
q <- merge(q, code2adm, by="code")[, adm3_pcode := to155(adm3_pcode)]
out <- q[, .(mom_secplus = mean(mom_secplus, na.rm=TRUE),
             expect_uni  = mean(expect_uni,  na.rm=TRUE),
             parent_part = mean(parent_part, na.rm=TRUE),
             absent      = mean(absent,      na.rm=TRUE),
             n_fam = .N), by=adm3_pcode]
cat("familia moderators: municipalities", nrow(out),
    "| families matched", format(sum(out$n_fam), big.mark=","), "\n")
stopifnot(nrow(out) > 120, !anyNA(out[, .(mom_secplus, expect_uni, parent_part, absent)]))
saveRDS(out, file.path(DIR_CLEAN,"familia_moderators_2019.rds"))
cat("saved -> data/clean/familia_moderators_2019.rds\n")
