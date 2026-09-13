# ============================================================================
# 07u_checks.R — two diagnostics requested by MM:
# (A) DDD comparison group: never-treated vs not-yet-treated (triplediff's valid
#     not-yet implementation) — does not-yet give TIGHTER inference? (the paper's
#     "use more comparison groups" idea; v0.1.0 has no separate GMM flag).
# (B) 30-34 PLACEBO: directly estimate the CS ATT on the 30-34 rate (the DDD's
#     ineligible comparison group). DDD ATT = teen-level ATT - 30-34 ATT, so:
#       30-34 ATT ~ 0  -> clean placebo (DDD = teen effect)            [WANT THIS]
#       30-34 ATT < 0  -> both ages fell = common municipio shock the DDD removes (ok)
#       30-34 ATT > 0  -> 30-34 ROSE in treated -> non-parallel age groups; DDD INFLATED (concern)
# S1 = binary any-unit ; S2 = modernization. Denom A.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(triplediff)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")

pw  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19, rateA_30_34)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]
La <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_age_long_155.rds")))[ag %in% c("15-19","30-34"), .(adm3_pcode=id, year, ag, y=rateA)]
La[, partition := as.integer(ag=="15-19")]

# (A) DDD with each control group
ddd_cg <- function(gtab, cg){
  d <- merge(La, gtab[!is.na(g), .(adm3_pcode, state=g)], by="adm3_pcode")
  d[, `:=`(id=as.integer(factor(paste(adm3_pcode,partition))), cluster=as.integer(factor(adm3_pcode)), time=as.integer(year))]
  o <- ddd(yname="y",tname="time",idname="id",gname="state",pname="partition",xformla=~1,data=d,
           control_group=cg, base_period="universal", est_method="dr", panel=TRUE, cluster="cluster", boot=TRUE, nboot=999)
  g <- agg_ddd(o, type="group", boot=TRUE, nboot=999)$aggte_ddd
  c(att=g$overall.att, se=g$overall.se)
}
# (B) CS ATT on a given outcome (teen or 30-34)
cs_y <- function(gtab, yname){
  est <- merge(pw, gtab[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode"); est[, id:=as.integer(factor(adm3_pcode))]
  a <- att_gt(yname=yname, tname="year", idname="id", gname="g", xformla=~1, data=est,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  o <- aggte(a, type="group", na.rm=TRUE); c(att=o$overall.att, se=o$overall.se)
}

cat("================= (A) DDD: never-treated vs not-yet-treated comparison =================\n")
for(s in list(list(bin,"S1 binary"), list(mod,"S2 modernization"))){
  nt <- ddd_cg(s[[1]],"nevertreated"); ny <- ddd_cg(s[[1]],"notyettreated")
  cat(sprintf("%-18s  nevertreated ATT=%.2f se=%.2f | notyettreated ATT=%.2f se=%.2f  (SE %s)\n",
      s[[2]], nt["att"],nt["se"], ny["att"],ny["se"],
      ifelse(ny["se"]<nt["se"],"TIGHTER","wider")))
}
cat("\n================= (B) 30-34 PLACEBO (CS ATT on each age outcome) =================\n")
for(s in list(list(bin,"S1 binary"), list(mod,"S2 modernization"))){
  t15 <- cs_y(s[[1]],"rateA_15_19"); t30 <- cs_y(s[[1]],"rateA_30_34")
  cat(sprintf("%-18s  teen(15-19) ATT=%+.2f (se %.2f) | 30-34 ATT=%+.2f (se %.2f, p=%.3f)  -> implied DDD=%+.2f\n",
      s[[2]], t15["att"],t15["se"], t30["att"],t30["se"], 2*pnorm(-abs(t30["att"]/t30["se"])), t15["att"]-t30["att"]))
}
cat("\n(30-34 ATT ~0 = clean placebo; <0 = common shock removed by DDD; >0 = inflates DDD)\n")
