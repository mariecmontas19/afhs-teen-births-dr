# ============================================================================
# 07z_distance_dose.R — D3 step 2: the DISTANCE/ACCESS dose, CS part (validated).
# Treatment = "an open AU within X km" (binary, absorbing — distance is monotone
# non-increasing), built from the time-varying distance panel (07y). This is the
# "more access -> fewer teen births" dose, estimated as a RADIUS GRADIENT (10/15/20/30 km).
#
# Specs (denom A, same fold(3), CS att_gt notyettreated, est=reg, universal base, muni-cluster):
#   A. ACCESS GRADIENT: CS on rateA_15_19, cohort = first year within X km (X=10,15,20,30).
#      Drop municipios already within X km in 2016 (no pre-period = always-treated).
#   B. AGE-DDD version: CS on ddd_A (15-19 minus 30-34) at X=20 (nets shocks).
#   C. 30-34 PLACEBO: CS on rateA_30_34 at X=20 (want ~0 = access is teen-specific).
#   D. SPILLOVER-ONLY: CS on acc20 restricted to municipios WITHOUT their own unit
#      (binary ever_treated==0) -> pure neighbour-access effect, no own-unit opening.
# dCDH (binary + continuous) is in 07z2. Output: output/tables/distance_dose_cs.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")

dp <- as.data.table(readRDS(file.path(DIR_CLEAN,"distance_open_panel.rds")))
p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
        , .(adm3_pcode, year, rateA_15_19, rateA_30_34, ddd_A, ever_treated)]
d  <- merge(p, dp[, .(adm3_pcode, year, dist_open_km, acc10, acc15, acc20, acc30)], by=c("adm3_pcode","year"))
# fold(3) municipios are already absent from the 155-panel (folded into parents at build);
# the inner merge keeps the same 155 the headline uses.
stopifnot(nrow(d)==155*10, sum(fold %in% d$adm3_pcode)==0)

# cohort for a binary absorbing access indicator: first year ==1; 0 if never; NA if already 1 in 2016
mk_cohort <- function(dt, accvar){
  a <- dt[, .(first1 = { w<-which(get(accvar)==1L); if(length(w)) year[min(w)] else NA_integer_ },
              in2016 = get(accvar)[year==2016L]), by=adm3_pcode]
  a[, g := fifelse(in2016==1L, NA_integer_,                      # already within X km in 2016 = always-treated -> drop
              fifelse(is.na(first1), 0L, as.integer(first1)))]    # never within = control (0)
  a[, .(adm3_pcode, g)]
}
cs_access <- function(dt, yname, accvar, restrict=NULL){
  co <- mk_cohort(dt, accvar)
  est <- merge(dt, co, by="adm3_pcode")
  if(!is.null(restrict)) est <- est[eval(restrict)]
  est <- est[!is.na(g)]
  ntr <- uniqueN(est[g>0, adm3_pcode]); nco <- uniqueN(est[g==0, adm3_pcode])
  est[, id := as.integer(factor(adm3_pcode))]
  r <- tryCatch({
    a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
          xformla=~1, data=est, control_group="notyettreated", base_period="universal",
          est_method="reg", bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
    o <- suppressMessages(aggte(a, type="group", na.rm=TRUE)); c(o$overall.att, o$overall.se)
  }, error=function(e) c(NA,NA))
  data.table(att=round(r[1],2), se=round(r[2],2), p=round(2*pnorm(-abs(r[1]/r[2])),3),
             n_treated=ntr, n_control=nco)
}

res <- list()
cat("============== 07z DISTANCE/ACCESS DOSE — CS (denom A) ==============\n")
# A. radius gradient on teen rate
for(x in c(10,15,20,30)){
  r <- cs_access(d, "rateA_15_19", paste0("acc",x))
  res[[paste0("A_within",x)]] <- cbind(block="A radius gradient", spec=paste0("within ",x," km"), outcome="rateA_15_19", r)
}
# B. age-DDD at 20km
res[["B_ddd20"]] <- cbind(block="B age-DDD", spec="within 20 km", outcome="ddd_A", cs_access(d, "ddd_A", "acc20"))
# C. 30-34 placebo at 20km
res[["C_plac20"]] <- cbind(block="C 30-34 placebo", spec="within 20 km", outcome="rateA_30_34", cs_access(d, "rateA_30_34", "acc20"))
# D. spillover only (own-never-treated municipios), 20km
res[["D_spill20"]] <- cbind(block="D spillover only", spec="within 20 km, own-untreated", outcome="rateA_15_19",
                            cs_access(d, "rateA_15_19", "acc20", restrict=quote(ever_treated==0L)))
out <- rbindlist(res)
print(out, class=FALSE)
fwrite(out, file.path(DIR_TABLES,"distance_dose_cs.csv"))
cat("\nsaved -> output/tables/distance_dose_cs.csv\n")
