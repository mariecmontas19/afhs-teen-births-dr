# ============================================================================
# 08z12_familia_het.R — heterogeneity of the MAIN fertility effect by
# CuestionarioFamilia (EDN 2019 3S wave) baseline moderators (§59.88).
# MM 2026-08-12: "Do a het table (similar to tab a06) but with these variables
# ... show me here only" — REPORT-ONLY, no paper exhibit.
#
# Moderators (municipality level, family-weighted means over valid responses;
# 0 = Vacio and '*' = Multimarca dropped; source = CuestionarioFamilia2019,
# 122,269 families, CENTRO_CODIGO onboard; wave = school year 2018-19, i.e.
# PRE-treatment for every in-window opener g_edu >= 2020):
#   mom_secplus = share mother completed secondary+  (p25_educ_madre in 5..7;
#                 8 = "No lo recuerdo" also dropped)
#   inc_20k     = share household income >= RD$20,001/mo (p36_ingreso in 5..6)
#   expect_uni  = share family expects the student to complete university
#                 (p9_nivelesperado == 3)
#   violence    = mean of six barrio-violence items (p6a drogas, p6c vandalismo,
#                 p6e peleas, p6g peleas con armas, p6j agresiones graves,
#                 p6k robos; each 1=Nunca..4=Siempre), family item-mean then
#                 weighted muni mean (HIGH = more violent surroundings)
# Estimation clones 08l_het_master.R exactly: median split of the 20 treated,
# cs_sub() = unconditional CS on rateA_15_19 (notyettreated, universal, reg,
# bstrap biters=2000, cluster id), halves share the never-treated pool,
# gap SE = sqrt(se_u^2 + se_s^2) (conservative). set.seed(20260722) as in 08l.
# Output: output/tables/familia_het.csv (record only).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi); library(did)})
set.seed(20260722)
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold155 <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold155), unname(fold155[p]), p)

## ---- 1. center -> adm3 (same OAI-0790 block as 05q/08z10/08z11) ------------
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
  cc <- names(d)[vapply(d, hit8, 0) > 0.5][1]
  stopifnot(!is.na(cc), !is.na(pv), !is.na(mn))
  x <- d[, .(centro=as.character(get(cc)), prov_nm=norm(get(pv)), muni_nm=norm(get(mn)))]
  x[, code := sub("^\\s*(\\d{7,8})\\s*-.*$", "\\1", centro)]
  x <- x[grepl("^\\d{7,8}$", code)][, code := formatC(as.integer(code), width=8, flag="0")]
  pieces[[s]] <- unique(x[, .(code, prov_nm, muni_nm)])
}
cw <- unique(rbindlist(pieces))
cw[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
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

## ---- 2. family-questionnaire moderators (2019 3S wave) ----------------------
FQ <- file.path(DRPAPER,"analysis/datasets/Evaluacion Diagnostica/CuestionarioFamilia2019.xlsx")
q <- as.data.table(read_excel(FQ, sheet="Familia_EVA3roSec_2019_REVISADA", col_types="text"))
stopifnot(all(c("CENTRO_CODIGO","p25_educ_madre","p36_ingreso","p9_nivelesperado",
                "p6a_drogas","p6c_vandalismo","p6e_peleas","p6g_peleas_armas",
                "p6j_agresiones","p6k_robos") %in% names(q)))
nv <- function(x){ y <- suppressWarnings(as.numeric(x)); fifelse(y==0, NA_real_, y) }  # 0=Vacio, '*'->NA
q[, code := formatC(suppressWarnings(as.integer(CENTRO_CODIGO)), width=8, flag="0")]
q <- q[!is.na(code) & grepl("^\\d{8}$", code)]
q[, `:=`(edm = nv(p25_educ_madre), inc = nv(p36_ingreso), exp = nv(p9_nivelesperado))]
q[edm==8, edm := NA_real_]                                    # "No lo recuerdo"
for(v in c("p6a_drogas","p6c_vandalismo","p6e_peleas","p6g_peleas_armas","p6j_agresiones","p6k_robos"))
  q[, (v) := nv(get(v))]
q[, viol := rowMeans(.SD, na.rm=TRUE), .SDcols=c("p6a_drogas","p6c_vandalismo","p6e_peleas",
                                                 "p6g_peleas_armas","p6j_agresiones","p6k_robos")]
q[is.nan(viol), viol := NA_real_]
cat("families read:", format(nrow(q),big.mark=","), "| valid: edm", sum(!is.na(q$edm)),
    "| inc", sum(!is.na(q$inc)), "| exp", sum(!is.na(q$exp)), "| viol", sum(!is.na(q$viol)), "\n")
q <- merge(q, code2adm, by="code")
q[, adm3_pcode := to155(adm3_pcode)]
cat("families matched to municipality:", format(nrow(q),big.mark=","), "\n")
mod <- q[, .(mom_secplus = mean(edm>=5, na.rm=TRUE),
             inc_20k     = mean(inc>=5, na.rm=TRUE),
             expect_uni  = mean(exp==3, na.rm=TRUE),
             violence    = mean(viol,   na.rm=TRUE),
             n_fam = .N), by=adm3_pcode]
cat("municipalities with moderators:", nrow(mod), "| median families/muni:", median(mod$n_fam), "\n")

## ---- 3. het machinery (clone of 08l) ----------------------------------------
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% names(fold155)]
treated <- tr[ever_treated==1L & always_treated==0L, .(adm3_pcode, g=as.integer(first_year))]
never   <- tr[ever_treated==0L, adm3_pcode]
M <- merge(treated, mod, by="adm3_pcode", all.x=TRUE)
cat("\ntreated munis with questionnaire data:", M[!is.na(mom_secplus), .N], "of", nrow(M), "\n")
stopifnot(M[is.na(mom_secplus), .N] == 0)   # all 20 must have data for the 10/10 split
cs_sub <- function(pcodes){
  est <- merge(p[, .(adm3_pcode, year, rateA_15_19)],
               rbind(treated[adm3_pcode %in% pcodes], data.table(adm3_pcode=never, g=0L)), by="adm3_pcode")
  est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id",
        gname="g", xformla=~1, data=est, control_group="notyettreated", base_period="universal",
        est_method="reg", bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE)); list(att=o$overall.att, se=o$overall.se)
}
spec <- data.table(v=c("mom_secplus","inc_20k","expect_uni","violence"),
                   pretty=c("Mother completed secondary (share)","Household income $\\geq$ RD\\$20{,}000 (share)",
                            "Family expects university (share)","Neighborhood violence index (1--4)"),
                   under_high=c(FALSE,FALSE,FALSE,TRUE))
res <- rbindlist(lapply(seq_len(nrow(spec)), function(i){
  v <- spec$v[i]; med <- median(M[[v]])
  und <- if(spec$under_high[i]) M[get(v)>=med, adm3_pcode] else M[get(v)<med, adm3_pcode]
  srv <- setdiff(M$adm3_pcode, und)
  ru <- cs_sub(und); rs <- cs_sub(srv)
  gap <- ru$att - rs$att
  data.table(label=spec$pretty[i], median=round(med,3),
             att_u=round(ru$att,2), se_u=round(ru$se,2), p_u=round(2*pnorm(-abs(ru$att/ru$se)),3),
             att_s=round(rs$att,2), se_s=round(rs$se,2), p_s=round(2*pnorm(-abs(rs$att/rs$se)),3),
             gap=round(gap,2), p_diff=round(2*pnorm(-abs(gap/sqrt(ru$se^2+rs$se^2))),3),
             n_u=length(und), n_s=length(srv))
}))
print(res, class=FALSE)
fwrite(res, file.path(DIR_TABLES,"familia_het.csv"))
cat("saved -> familia_het.csv (REPORT-ONLY; no paper exhibit)\n")
