# ============================================================================
# 09b_ramp_robustness.R — robustness to the 2016-2018 birth-REGISTRATION RAMP
# (national undercount). Re-estimate the S1 headline on the POST-RAMP subsample
# (year >= 2019) and compare to the full 2016-2025 sample. All 20 in-window
# treated cohorts open 2020-2025, so 2019+ keeps every treated unit (g-1>=2019);
# it just drops the ramp years from the pre-period. If the estimate is stable,
# the effect is not an artifact of incomplete early-period registration.
# Denominator A; S1; not-yet controls; universal base; municipio-clustered.
# Output: output/tables/ramp_robustness.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(triplediff)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
# 2026-08-05: + pct_senasa_2013 so the +covs row matches the headline 5-covariate
# spec (Table 3, -5.66); the old 4-covariate version read -5.73 and would have
# contradicted the main table once this exhibit entered the appendix.
XF <- ~ wealth_index + sns_per10k + pct_urban + pct_educ_secplus + pct_senasa_2013
pw <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19, wealth_index, pct_urban, pct_educ_secplus, pct_senasa_2013)]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
pw <- merge(pw, hc, by="adm3_pcode", all.x=TRUE)
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
La <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_age_long_155.rds")))[ag %in% c("15-19","30-34"), .(adm3_pcode=id, year, ag, y=rateA)]
La[, partition := as.integer(ag=="15-19")]

cs_one <- function(xf, ymin){
  est <- merge(pw[year>=ymin], bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode"); est[, id:=as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g", xformla=xf, data=est,
        control_group="notyettreated", base_period="universal", est_method="reg", bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE)); c(att=o$overall.att, se=o$overall.se)
}
ddd_one <- function(ymin){
  d <- merge(La[year>=ymin], bin[!is.na(g), .(adm3_pcode, state=g)], by="adm3_pcode")
  d[, `:=`(id=as.integer(factor(paste(adm3_pcode,partition))), cluster=as.integer(factor(adm3_pcode)), time=as.integer(year))]
  o <- tryCatch(suppressWarnings(suppressMessages(ddd(yname="y",tname="time",idname="id",gname="state",pname="partition",xformla=~1,data=d,
        control_group="notyettreated", base_period="universal", est_method="dr", panel=TRUE, cluster="cluster", boot=TRUE, nboot=999))), error=function(e) NULL)
  if(is.null(o)) return(c(att=NA, se=NA))
  a <- suppressMessages(agg_ddd(o, type="group", boot=TRUE, nboot=999)$aggte_ddd); c(att=a$overall.att, se=a$overall.se)
}
mk <- function(lab, full, post){
  pv <- function(x) if(is.na(x["se"])||x["se"]==0) NA else 2*pnorm(-abs(x["att"]/x["se"]))
  data.table(spec=lab,
    full_ATT=round(full["att"],2), full_SE=round(full["se"],2), full_p=round(pv(full),3),
    post2019_ATT=round(post["att"],2), post2019_SE=round(post["se"],2), post2019_p=round(pv(post),3))
}
res <- rbindlist(list(
  mk("S1 CS level, no covs", cs_one(~1, 2016L), cs_one(~1, 2019L)),
  mk("S1 CS level, +covs",   cs_one(XF, 2016L), cs_one(XF, 2019L)),
  mk("S1 DDD (no covs)",     ddd_one(2016L),    ddd_one(2019L))))
cat("=== RAMP ROBUSTNESS: full 2016-2025 vs post-ramp 2019-2025 (S1) ===\n"); print(res, class=FALSE)
fwrite(res, file.path(PROJ,"output","tables","ramp_robustness.csv"))
cat("\nsaved -> output/tables/ramp_robustness.csv\n")
