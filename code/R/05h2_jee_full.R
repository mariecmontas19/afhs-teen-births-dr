# ============================================================================
# 05h2_jee_full.R — FULL JEE panel from MINERD SAIP OAI-0790-2026 (received
# 2026-07-14): per-center niveles+tandas, ALL 10 school years 2015-16..2024-25.
# Replaces the 3-snapshot 05h build (2019-20/2020-21/2024-25; imputation had
# been rejected as fabrication, §23.1) with zero-imputation annual data.
# SAME definition as 05h (comparability): among PUBLIC+SEMIOFICIAL centers,
#   sec_school = center offers SECUNDARIO (Nivel contains "SECUNDARIO")
#   jee_school = center operates any JORNADA EXTENDIDA tanda
#   pct_jee_sch = 100 * #(sec & jee) / #(sec)   per municipio x school year
# Center-level dedupe: a center counts once per year (any row secondary / any
# row extended). Year convention: school year YYYY-(YYYY+1) -> year_end =
# YYYY+1 (majority of instruction months fall in the spring calendar year).
# CHECKS (rule 1b predictions):
#   V1 vs old jee_rollout_muni.rds on the 3 overlapping years: corr HIGH (>.85)
#      but not identical (different source vintages: "datos preliminares").
#   V2 national JEE share rises steeply 2015-2019 (program scale-up era) then
#      plateaus (near-saturation) — the known program history.
#   C1 CS headline + baseline-JEE control: ~ -5.5 as in 08g (JEE not confound).
#   C2 NEW (full panel): event study of pct_jee itself around AU opening —
#      predict FLAT (parallel rollout): AU munis' JEE didn't move differently
#      at openings.
# Output: data/clean/jee_full_muni.rds + output/tables/jee_full_checks.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi); library(did)})
TAB <- file.path(PROJ,"output","tables")
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155<- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)
F1 <- file.path(DRPAPER,"analysis/datasets/SAIP/OAI-0790-2026 Lista de centros con datos de niveles y tandas 2015-2025.xlsx")

mc <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[
        , .(prov_nm=norm(prov_name), muni_nm=norm(adm3_name), adm3_pcode)]

pieces <- list()
for(s in excel_sheets(F1)){
  peek <- suppressMessages(as.matrix(read_excel(F1, sheet=s, col_names=FALSE, n_max=6)))
  hr <- which(apply(peek, 1, function(r) any(r=="Regional", na.rm=TRUE) & any(r=="Municipio", na.rm=TRUE)))[1]
  stopifnot(!is.na(hr))
  d <- suppressMessages(as.data.table(read_excel(F1, sheet=s, skip=hr-1)))
  for(cn in c("Centro","Sector","Provincia","Municipio","Nivel","Tanda","Total general")){
    hit <- names(d)[toupper(names(d))==toupper(cn)][1]
    if(!is.na(hit) && hit != cn) setnames(d, hit, cn)
  }
  stopifnot(all(c("Centro","Sector","Provincia","Municipio","Nivel","Tanda") %in% names(d)))
  # 2018-19+ sheets: Sector/Planta-Fisica DATA columns are swapped vs the header —
  # detect by which column actually holds sector labels
  SECV <- c("PUBLICO","PRIVADO","SEMIOFICIAL")
  if(mean(norm(d$Sector) %in% SECV) < 0.5 && "Planta Fisica" %in% names(d) &&
     mean(norm(d$`Planta Fisica`) %in% SECV) > 0.5){
    setnames(d, c("Sector","Planta Fisica"), c("Planta Fisica","Sector"))
    cat("  [", s, "] sector/planta columns swapped in data - corrected\n")
  }
  stopifnot(mean(norm(d$Sector) %in% SECV) > 0.9)
  d <- d[norm(Sector) %in% c("PUBLICO","SEMIOFICIAL")]
  tgc <- names(d)[grepl("^total", tolower(names(d)))][1]
  d[, `:=`(prov_nm=norm(Provincia), muni_nm=norm(Municipio),
           sec = grepl("SECUNDARIO", norm(Nivel)),
           jee = grepl("JORNADA EXTENDIDA", norm(Tanda)),
           mat = if(is.na(tgc)) NA_real_ else suppressWarnings(as.numeric(get(tgc))))]
  cen <- d[, .(sec=any(sec), jee=any(jee), mat=sum(mat, na.rm=TRUE)), by=.(prov_nm, muni_nm, Centro)]
  cen[, school_year := s]
  pieces[[s]] <- cen
}
cen <- rbindlist(pieces)
cen[, year_end := as.integer(substr(school_year,6,9))]
cat("center-years:", nrow(cen), "| years:", cen[,uniqueN(school_year)], "\n")

# municipio mapping: exact normalized name within province + alias pass
cen[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
cen <- merge(cen, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
un <- cen[is.na(adm3_pcode), .N, by=.(prov_nm, muni_nm)][order(-N)]
if(nrow(un)){cat("unmapped muni labels (centers):\n"); print(un[1:min(8,nrow(un))], class=FALSE)}
for(i in seq_len(nrow(un))){                                  # fuzzy within province
  cand <- mc[prov_nm==un$prov_nm[i]]
  if(!nrow(cand)) next
  dd <- drop(adist(un$muni_nm[i], cand$muni_nm))
  if(min(dd) <= 3 && sum(dd==min(dd))==1)
    cen[prov_nm==un$prov_nm[i] & muni_nm==un$muni_nm[i], adm3_pcode := cand$adm3_pcode[which.min(dd)]]
}
cat(sprintf("mapped: %.2f%% of centers\n", 100*cen[!is.na(adm3_pcode),.N]/nrow(cen)))
cen[, adm3_pcode := to155(adm3_pcode)]

jee <- cen[!is.na(adm3_pcode) & sec==TRUE,
           .(n_sec=.N, n_jee=sum(jee), pct_jee_sch=100*mean(jee)), by=.(adm3_pcode, school_year, year_end)]
enr <- cen[!is.na(adm3_pcode), .(matricula_total=sum(mat, na.rm=TRUE)), by=.(adm3_pcode, school_year, year_end)]
jee <- merge(jee, enr, by=c("adm3_pcode","school_year","year_end"), all=TRUE)
cat("muni x school-year rows:", nrow(jee), "| national %JEE (sec schools) by year:\n")
print(jee[!is.na(pct_jee_sch), .(pct=round(100*sum(n_jee)/sum(n_sec),1)), keyby=school_year], class=FALSE)
saveRDS(jee, file.path(DIR_CLEAN,"jee_full_muni.rds"))

# ---- V1: vs old 3-snapshot build ----
old <- as.data.table(readRDS(file.path(DIR_CLEAN,"jee_rollout_muni.rds")))
v1 <- merge(old[, .(adm3_pcode, school_year, old_pct=pct_jee_sch)],
            jee[, .(adm3_pcode, school_year, new_pct=pct_jee_sch)], by=c("adm3_pcode","school_year"))
cat(sprintf("\nV1 vs old build: %d overlapping muni-years | corr %.3f | mean |diff| %.1fpp\n",
    nrow(v1), v1[,cor(old_pct,new_pct,use="complete.obs")], v1[,mean(abs(old_pct-new_pct),na.rm=TRUE)]))

# ---- C2: event study of JEE itself around AU opening (parallel-rollout test) ----
fold3 <- c("DOM010905","DOM051703","DOM012510")
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold3]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                  fifelse(ever_treated==0L, 0L, NA_integer_))]
est <- merge(jee[!is.na(pct_jee_sch), .(adm3_pcode, year=year_end, pct_jee_sch)],
             tr[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est[, id := as.integer(factor(adm3_pcode))]
a <- att_gt(yname="pct_jee_sch", tname="year", idname="id", gname="g", xformla=~1, data=est,
            control_group="notyettreated", base_period="universal", est_method="reg",
            bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE,
            allow_unbalanced_panel=TRUE)
gA <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
cat(sprintf("\nC2 JEE-on-AU event study: group ATT %+.2fpp (SE %.2f, p=%.3f) [predict ~0 = parallel rollout]\n",
    gA$overall.att, gA$overall.se, 2*pnorm(-abs(gA$overall.att/gA$overall.se))))
dd <- data.table(e=dy$egt, att=round(dy$att.egt,2), se=round(dy$se.egt,2))
print(dd[e %between% c(-5,4)], class=FALSE)
res <- data.table(check=c("V1_corr_old","C2_jee_on_AU_att","C2_se","C2_p"),
                  value=round(c(v1[,cor(old_pct,new_pct,use="complete.obs")], gA$overall.att, gA$overall.se,
                                2*pnorm(-abs(gA$overall.att/gA$overall.se))),4))
fwrite(res, file.path(TAB,"jee_full_checks.csv"))
cat("saved -> jee_full_muni.rds + jee_full_checks.csv\n")
