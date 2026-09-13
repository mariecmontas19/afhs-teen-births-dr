# ============================================================================
# 08g3_jee_baseline_control.R — Baseline-JEE conditioning check (§59.25),
# SCRIPTED 2026-07-17 (the 07-14 numbers were an ad-hoc chunk; this makes them
# reproducible and refreshes them under wealth-index v2, §59.30).
# Spec mirrors 07t_headline EXACTLY (S1, CS att_gt, not-yet, universal base,
# est="reg", muni-clustered bootstrap), on the COMMON SAMPLE of estimation
# municipalities with a 2018-19 JEE baseline (school_year 2018-2019, fully
# pre-rollout). Four rows: no covs | headline covs | +JEE | JEE only.
# Output: output/tables/jee_baseline_control.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260717)

fold <- c("DOM010905","DOM051703","DOM012510")
pw <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
  , .(adm3_pcode, year, rateA_15_19, wealth_index, pct_urban, pct_educ_secplus)]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
pw <- merge(pw, hc, by="adm3_pcode", all.x=TRUE)
sn <- as.data.table(readRDS(file.path(DIR_CLEAN,"dhs_province_baseline.rds")))[, .(adm3_pcode, pct_senasa_2013)]
pw <- merge(pw, sn, by="adm3_pcode", all.x=TRUE)

jee <- as.data.table(readRDS(file.path(DIR_CLEAN,"jee_full_muni.rds")))[
  school_year=="2018-2019", .(adm3_pcode, jee_1819 = pct_jee_sch)]
cat("municipalities with 2018-19 JEE baseline:", nrow(jee), "\n")

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]

est <- merge(pw, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est <- merge(est, jee, by="adm3_pcode")                          # common sample
est[, id := as.integer(factor(adm3_pcode))]
cat("common sample:", uniqueN(est$adm3_pcode), "municipalities /",
    est[g>0, uniqueN(adm3_pcode)], "treated\n")

run <- function(xf, lab){
  a <- att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g", xformla=xf,
              data=est, control_group="notyettreated", base_period="universal",
              est_method="reg", bstrap=TRUE, cband=TRUE, biters=2000,
              clustervars="id", print_details=FALSE)
  g <- aggte(a, type="group", na.rm=TRUE)
  data.table(spec=lab, ATT=round(g$overall.att,2), SE=round(g$overall.se,2),
             p=round(2*pnorm(-abs(g$overall.att/g$overall.se)),3))
}

res <- rbindlist(list(
  run(~1,                                                             "no covariates (common sample)"),
  run(~ wealth_index + sns_per10k + pct_urban + pct_educ_secplus + pct_senasa_2013, "headline covariates"),
  run(~ wealth_index + sns_per10k + pct_urban + pct_educ_secplus + pct_senasa_2013 + jee_1819, "headline covariates + baseline JEE 2018-19"),
  run(~ jee_1819,                                                     "baseline JEE 2018-19 only")))
print(res, class=FALSE)
fwrite(res, file.path(DIR_TABLES, "jee_baseline_control.csv"))
cat("saved -> output/tables/jee_baseline_control.csv\n")
