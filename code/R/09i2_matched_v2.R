# ============================================================================
# 09i2_matched_v2.R — REFINED matching (MM 2026-07-10): better control groups
# without the mean-reversion trap. Two new specs on top of 09i:
#  M3 "ELIGIBLE POOL": controls RESTRICTED to never-treated municipalities with
#     an SNS hospital in 2019 (the placement precondition: all 20 treated have
#     one) + Mahalanobis match on the structural set (wealth, urban, educ,
#     sns_per10k, senasa, log pop).
#  M4 "FERTILITY-NEED": M3 + cwr_2010_baseline (2010-census child-woman ratio)
#     + dhs_teenasfr_2013 (DHS 2013 teen fertility) + near_any_km (2019
#     proximity, the robust moderator). These capture "high-fertility, health-
#     system-embedded place" from INDEPENDENT, 6-10-years-pre instruments —
#     fertility matching without Daw-Hatfield (the 2019 rate itself stays OUT
#     of the match; its balance is REPORTED).
# CS spec = 07t. Output: appended to matched_balance.csv / matched_cs.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(MatchIt)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
fm <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
fl <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_facility_levels_panel.rds")))[year==2019, .(adm3_pcode, pop_tot, n_hosp)]
fa <- as.data.table(readRDS(file.path(DIR_CLEAN,"facility_access_panel.rds")))[year==2019, .(adm3_pcode, near_any_km)]
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))[!is.na(adm3_pcode) & age_mom %in% 15:19 & birth_year==2019]
b[adm3_pcode %in% names(fm), adm3_pcode := fm[adm3_pcode]]
comp19 <- b[, .(age_mean = mean(age_mom), sh_haitian = 100*mean(nationality=="Haitian", na.rm=TRUE),
                sh_noins = 100*mean((insurance=="None")[!is.na(insurance)]),
                sh_csec  = 100*mean((csection==TRUE | csection==1)[!is.na(csection)])), by=adm3_pcode]
base <- Reduce(function(a,b2) merge(a,b2,by="adm3_pcode",all.x=TRUE),
  list(p[year==2019, .(adm3_pcode, rate2019=rateA_15_19, wealth_index, pct_urban, pct_educ_secplus,
                       pct_senasa_2013, cwr_2010_baseline, dhs_teenasfr_2013)],
       hc, fl, fa, comp19))
base[, `:=`(log_pop = log(pop_tot), has_hosp = as.integer(n_hosp>0))]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
base <- merge(base, tr[!is.na(g), .(adm3_pcode, g, treated=as.integer(g>0))], by="adm3_pcode")
cat("sample:", nrow(base), "| treated:", base[,sum(treated)],
    "| hospital controls:", base[treated==0 & has_hosp==1, .N], "\n")

V3 <- c("wealth_index","pct_urban","pct_educ_secplus","sns_per10k","pct_senasa_2013","log_pop")
V4 <- c(V3, "cwr_2010_baseline","dhs_teenasfr_2013","near_any_km")
REPORT <- unique(c(V4,"age_mean","sh_haitian","sh_noins","sh_csec","rate2019"))
smd <- function(d, v) sapply(v, function(x){
  mt <- d[treated==1][[x]]; mc <- d[treated==0][[x]]
  (mean(mt,na.rm=TRUE)-mean(mc,na.rm=TRUE))/sqrt((var(mt,na.rm=TRUE)+var(mc,na.rm=TRUE))/2) })

run_match <- function(vars, lab, pool){
  dd <- pool[complete.cases(pool[, ..vars])]
  mo <- matchit(as.formula(paste("treated ~", paste(vars, collapse="+"))), data=dd,
                method="nearest", distance="mahalanobis", ratio=3, replace=TRUE)
  keep <- unique(as.data.table(match.data(mo, drop.unmatched=TRUE))$adm3_pcode)
  cat(sprintf("\n== %s: %d munis (%d treated + %d controls)\n", lab, length(keep),
      pool[adm3_pcode %in% keep, sum(treated)], pool[adm3_pcode %in% keep, sum(1-treated)]))
  balpost <- smd(base[adm3_pcode %in% keep], REPORT)
  est <- merge(p[, .(adm3_pcode, year, rateA_15_19)], base[adm3_pcode %in% keep, .(adm3_pcode, g)], by="adm3_pcode")
  est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  bs <- est[g>0][, .SD[year==g-1L], by=adm3_pcode][, mean(rateA_15_19)]
  cat(sprintf("   CS: ATT %.2f (SE %.2f, p=%.3f) | base %.1f (%.0f%%)\n",
      o$overall.att, o$overall.se, 2*pnorm(-abs(o$overall.att/o$overall.se)), bs, 100*o$overall.att/bs))
  list(bal=data.table(variable=REPORT, matched_on=REPORT %in% vars, smd_pre=round(smd(base,REPORT),3),
                      smd_post=round(balpost,3), spec=lab),
       cs=data.table(spec=lab, n_treated=base[adm3_pcode %in% keep, sum(treated)],
                     n_controls=base[adm3_pcode %in% keep, sum(1-treated)],
                     ATT=round(o$overall.att,2), SE=round(o$overall.se,2),
                     p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3),
                     baseline=round(bs,2), pct=round(100*o$overall.att/bs,1)))
}
poolE <- base[treated==1 | has_hosp==1]                       # eligible pool
r3 <- run_match(V3, "M3: eligible pool (hospital) + structural", poolE)
r4 <- run_match(V4, "M4: + cwr2010 + DHS2013 fertility + proximity", poolE)
bal <- rbind(r3$bal, r4$bal); cs <- rbind(r3$cs, r4$cs)
cat("\n=== M4 balance ===\n"); print(bal[spec %like% "M4"], class=FALSE)
old_b <- fread(file.path(TAB,"matched_balance.csv")); old_c <- fread(file.path(TAB,"matched_cs.csv"))
fwrite(rbind(old_b, bal, fill=TRUE), file.path(TAB,"matched_balance.csv"))
fwrite(rbind(old_c, cs, fill=TRUE), file.path(TAB,"matched_cs.csv"))
cat("\nappended -> matched_balance.csv + matched_cs.csv\n")
