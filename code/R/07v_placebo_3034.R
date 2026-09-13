# ============================================================================
# 07v_placebo_3034.R — diagnose & try to fix the S2 30-34 contamination.
# 30-34 is the DDD's "ineligible" comparison; we want its ATT ~ 0. S1 is clean
# (~0), S2 is +3.3 (30-34 rose in treated -> urban postponement -> inflates DDD).
# Tests, all CS (att_gt) on rateA_30_34 and rateA_15_19, denom A:
#   (1) 30-34 & teen ATT under control_group = never-treated AND not-yet-treated
#   (2) does conditioning on baseline URBAN (or wealth+urban+educ) shrink S2's 30-34 toward 0?
#       (= the DDD paper's covariate-adjusted parallel-trends logic, done in CS where feasible)
#   (3) does DROPPING the 3 recovered urban municipalities (DN/Santiago/SD Oeste) clean S2's 30-34?
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19, rateA_30_34, wealth_index, pct_urban, pct_educ_secplus)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]
# 3 recovered urban municipalities (binary always-treated -> modern in-window, type new): DN, Santiago, SD Oeste
recovered_urban <- mod[always_treated_mod==0L & mod_ever_treated==1L][
  as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[always_treated==1L], on="adm3_pcode", nomatch=0L]$adm3_pcode

cs <- function(gtab, yname, cg, xf=~1, drop=NULL){
  est <- merge(p, gtab[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  if(!is.null(drop)) est <- est[!adm3_pcode %in% drop]
  est[, id:=as.integer(factor(adm3_pcode))]
  a <- att_gt(yname=yname, tname="year", idname="id", gname="g", xformla=xf, data=est,
              control_group=cg, base_period="universal", est_method="reg", bstrap=TRUE, biters=2000,
              clustervars="id", print_details=FALSE)
  o <- aggte(a, type="group", na.rm=TRUE); sprintf("%+.2f (se %.2f, p %.3f)", o$overall.att, o$overall.se, 2*pnorm(-abs(o$overall.att/o$overall.se)))
}

cat("====== (1) teen & 30-34 ATT under each comparison group ======\n")
for(s in list(list(bin,"S1 binary"), list(mod,"S2 modernization"))){
  for(cg in c("nevertreated","notyettreated"))
    cat(sprintf("%-16s %-14s teen=%s | 30-34=%s\n", s[[2]], cg,
        cs(s[[1]],"rateA_15_19",cg), cs(s[[1]],"rateA_30_34",cg)))
}
cat("\n====== (2) S2 30-34 ATT conditioning on baseline covariates (not-yet-treated) ======\n")
cat("  uncond            :", cs(mod,"rateA_30_34","notyettreated", ~1), "\n")
cat("  + pct_urban       :", cs(mod,"rateA_30_34","notyettreated", ~pct_urban), "\n")
cat("  + wealth+urban+educ:", cs(mod,"rateA_30_34","notyettreated", ~wealth_index+pct_urban+pct_educ_secplus), "\n")
cat("\n====== (3) S2 30-34 ATT dropping recovered urban municipalities (", length(recovered_urban), ") ======\n", sep="")
cat("  drop", paste(recovered_urban,collapse=","), "\n")
cat("  S2 30-34 (not-yet, drop urban):", cs(mod,"rateA_30_34","notyettreated", ~1, drop=recovered_urban), "\n")
cat("  S2 teen  (not-yet, drop urban):", cs(mod,"rateA_15_19","notyettreated", ~1, drop=recovered_urban), "\n")
