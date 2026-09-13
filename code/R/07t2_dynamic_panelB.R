# ============================================================================
# 07t2_dynamic_panelB.R — DYNAMIC effect by year-since-opening, for Appendix A2
# (H1) and A2b (H2). Two estimators on the LEVEL teen rate (15-19):
#   CS   = did::att_gt -> aggte(type="dynamic")   [clean, heterogeneity-robust]
#   TWFE = fixest distributed-lag i(relyr, ref=-1), never-treated = control
#          [the naive Packham-style contrast]
# Records #municipios identifying each horizon (H1 thins fast: 20/19/13/7/4 at
# e=0..4) -> tables cap at Year 2; e=3,4 saved but flagged thin. Runs BOTH
# H1 (first opening, bin) and H2 (incl. modernization, mod); scenario column.
# Same fold(3) + treatment construction as 07t. Denominator A.
# Output: output/tables/dynamic_panelB.csv (cols scenario,yr,est,att,se,p,n_muni,thin)
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(fixest)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables")

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold, .(adm3_pcode,year,rateA_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]

HOR <- 0:4   # horizons to save (tables use 0:2; 3:4 saved + flagged thin)

run_sc <- function(gt, sc){
  d <- merge(p, gt[!is.na(g), .(adm3_pcode,g)], by="adm3_pcode")
  d[, id := as.integer(factor(adm3_pcode))]
  d[, event_time := fifelse(g>0, year-g, -1000L)]
  nmuni <- sapply(HOR, function(e) d[g>0 & event_time==e, uniqueN(id)])
  # CS dynamic
  a  <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
          xformla=~1, data=d, control_group="notyettreated", base_period="universal",
          est_method="reg", bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  dy <- suppressMessages(aggte(a, type="dynamic", na.rm=TRUE))
  cs <- data.table(yr=dy$egt, est="CS", att=dy$att.egt, se=dy$se.egt)[yr %in% HOR]
  # TWFE distributed-lag
  d[, relyr := fifelse(g>0, pmin(pmax(year-g, -5L), 5L), -1L)]
  m  <- feols(rateA_15_19 ~ i(relyr, ref=-1) | id + year, data=d, cluster=~id)
  ct <- as.data.table(coeftable(m), keep.rownames="term")
  ct[, yr := suppressWarnings(as.integer(sub(".*::","",term)))]
  tw <- ct[yr %in% HOR, .(yr, est="TWFE", att=Estimate, se=`Std. Error`)]
  out <- rbindlist(list(cs, tw), use.names=TRUE)
  out[, p := round(2*pnorm(-abs(att/se)), 3)]
  out[, `:=`(att=round(att,2), se=round(se,2))]
  out <- merge(out, data.table(yr=HOR, n_muni=nmuni), by="yr")
  out[, `:=`(thin = n_muni < 10L, scenario = sc)]
  setorder(out, est, yr)
  stopifnot(nrow(out)==length(HOR)*2L, sum(is.na(out$att))==0)
  out[]
}

res <- rbind(run_sc(bin,"S1"), run_sc(mod,"S2"))
setcolorder(res, c("scenario","yr","est","att","se","p","n_muni","thin"))
fwrite(res, file.path(TAB,"dynamic_panelB.csv"))
cat("=== dynamic panel (S1 + S2) ===\n"); print(res, class=FALSE)
cat("saved -> output/tables/dynamic_panelB.csv\n")
