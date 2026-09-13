# ============================================================================
# 07w_eight.R — the 8-cell decision grid: {S1 binary, S2 modernization} x
# {CS, DDD} x {uncond, +covariates}. Covariates = wealth + urban + education +
# health-centers (all baseline). Denom A.
#   CS  = att_gt on rateA_15_19, control="notyettreated", est="reg".
#   DDD = triplediff, control="notyettreated", est="dr" (paper's multiple-comparison-
#         group estimator = tighter inference); never-treated reported as robustness.
#   DDD+covs via triplediff is expected INFEASIBLE (cohorts of 1-3 -> singular);
#   the feasible stand-in is the PRE-DIFFERENCED CS-DDD (att_gt on ddd_A) with covs,
#   which the paper flags as the imperfect covariate route — labeled as such.
# baseline = teen rate at g-1. Output: output/tables/eight_grid.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(triplediff)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
COVS <- ~ wealth_index + pct_urban + pct_educ_secplus + sns_per10k

p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19, ddd_A, wealth_index, pct_urban, pct_educ_secplus)]
p  <- merge(p, as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)], by="adm3_pcode")
covdt <- p[, .(wealth_index=wealth_index[1], pct_urban=pct_urban[1], pct_educ_secplus=pct_educ_secplus[1], sns_per10k=sns_per10k[1]), by=adm3_pcode]
La <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_age_long_155.rds")))[ag %in% c("15-19","30-34"), .(adm3_pcode=id, year, ag, y=rateA)]
La[, partition := as.integer(ag=="15-19")]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]
base_g1 <- function(gt){ tr<-gt[!is.na(g)&g>0,.(adm3_pcode,g)]; d<-merge(p[,.(adm3_pcode,year,rateA_15_19)],tr,by="adm3_pcode"); round(d[year==g-1L,mean(rateA_15_19)],2) }
B <- c(S1=base_g1(bin), S2=base_g1(mod))

fmt <- function(att,se) if(is.na(att)) "INFEASIBLE" else sprintf("%+.2f (se %.2f, p %.3f)", att, se, 2*pnorm(-abs(att/se)))
csA <- function(gt, yname, xf){ est<-merge(p, gt[!is.na(g),.(adm3_pcode,g)],by="adm3_pcode"); est[,id:=as.integer(factor(adm3_pcode))]
  a<-att_gt(yname=yname,tname="year",idname="id",gname="g",xformla=xf,data=est,control_group="notyettreated",
            base_period="universal",est_method="reg",bstrap=TRUE,biters=2000,clustervars="id",print_details=FALSE)
  o<-aggte(a,type="group",na.rm=TRUE); c(att=o$overall.att, se=o$overall.se) }
dddT <- function(gt, xf){ tryCatch({ d<-merge(La, gt[!is.na(g),.(adm3_pcode,state=g)],by="adm3_pcode"); d<-merge(d,covdt,by="adm3_pcode")
  d[,`:=`(id=as.integer(factor(paste(adm3_pcode,partition))),cluster=as.integer(factor(adm3_pcode)),time=as.integer(year))]
  o<-ddd(yname="y",tname="time",idname="id",gname="state",pname="partition",xformla=xf,data=d,control_group="notyettreated",
         base_period="universal",est_method="dr",panel=TRUE,cluster="cluster",boot=TRUE,nboot=999)
  g<-agg_ddd(o,type="group",boot=TRUE,nboot=999)$aggte_ddd; c(att=g$overall.att, se=g$overall.se)
  }, error=function(e) c(att=NA_real_, se=NA_real_)) }

cat("================== 8-CELL DECISION GRID (denom A) ==================\n")
rows <- list()
for(sc in list(list("S1","S1 binary any-unit",bin), list("S2","S2 modernization",mod))){
  tag<-sc[[1]]; lab<-sc[[2]]; gt<-sc[[3]]; bb<-B[[tag]]
  c1<-csA(gt,"rateA_15_19",~1);  c2<-csA(gt,"rateA_15_19",COVS)
  d1<-dddT(gt,~1);               d2<-dddT(gt,COVS)
  pd2<-csA(gt,"ddd_A",COVS)      # feasible stand-in for conditioned DDD (pre-differenced, paper-imperfect)
  cat(sprintf("\n--- %s (baseline g-1 = %.1f) ---\n", lab, bb))
  cat("  CS  uncond :", fmt(c1["att"],c1["se"]), sprintf("(%.0f%%)",100*c1["att"]/bb), "\n")
  cat("  CS  +covs  :", fmt(c2["att"],c2["se"]), sprintf("(%.0f%%)",100*c2["att"]/bb), "\n")
  cat("  DDD uncond :", fmt(d1["att"],d1["se"]), sprintf("(%.0f%%)",100*d1["att"]/bb), "\n")
  cat("  DDD +covs  :", fmt(d2["att"],d2["se"]),
      if(is.na(d2["att"])) paste0("  [triplediff infeasible; pre-diff CS-DDD+covs = ", fmt(pd2["att"],pd2["se"]),"]") else "", "\n")
  rows[[tag]] <- data.table(scenario=tag, baseline=bb,
     CS_uncond=fmt(c1["att"],c1["se"]), CS_covs=fmt(c2["att"],c2["se"]),
     DDD_uncond=fmt(d1["att"],d1["se"]), DDD_covs=fmt(d2["att"],d2["se"]),
     DDD_covs_predit=fmt(pd2["att"],pd2["se"]))
}
fwrite(rbindlist(rows), file.path(TAB <- file.path(PROJ,"output","tables"), "eight_grid.csv"))
cat("\nsaved -> output/tables/eight_grid.csv\n")
