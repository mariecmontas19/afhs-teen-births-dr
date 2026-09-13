# ============================================================================
# 07z2_distance_dcdh.R — D3 step 3: distance/access dose via de Chaisemartin-
# D'Haultfoeuille `did_multiplegt_dyn` (DIDmultiplegtDYN 2.2.0, pre-polars; loaded
# headless with RGL_USE_NULL=TRUE). The heterogeneity-robust dose estimator.
#   (1) BINARY access acc20 (within 20 km of an open unit) — valid SEs.
#   (2) CONTINUOUS proximity neg_dist = -(km to nearest open unit), continuous=1
#       (polynomial-1 control for the continuous baseline distance). NOTE: dCDH warns
#       the continuous-option SEs are NOT backed by a proven asymptotic-normality
#       result (only simulation evidence) -> exploratory.
# Reports dynamic Effects, average total effect (ATE), PLACEBOS + joint-placebo p
# (the parallel-trends check — placebos should be ~0). Denom A, municipio-clustered.
# Output: output/tables/distance_dose_dcdh.csv
# ============================================================================
Sys.setenv(RGL_USE_NULL="TRUE")
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(DIDmultiplegtDYN)})

dp <- as.data.table(readRDS(file.path(DIR_CLEAN,"distance_open_panel.rds")))
p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[,.(adm3_pcode,year,rateA_15_19)]
d  <- merge(p, dp[,.(adm3_pcode,year,acc20,neg_dist)], by=c("adm3_pcode","year"))
d[, id := as.integer(factor(adm3_pcode))]
d  <- as.data.frame(d)

run_dcdh <- function(treat, cont=NULL, lab){
  m <- did_multiplegt_dyn(df=d, outcome="rateA_15_19", group="id", time="year",
        treatment=treat, effects=3, placebo=2, cluster="id", continuous=cont, graph_off=TRUE)
  E <- m$results$Effects; A <- m$results$ATE; P <- m$results$Placebos
  cat(sprintf("\n===== %s =====\n", lab))
  cat("dynamic effects (event time +0,+1,+2):\n")
  for(i in 1:nrow(E)) cat(sprintf("  Effect_%d: %+.2f (se %.2f, p %.3f) | switchers %d\n",
      i, E[i,"Estimate"], E[i,"SE"], 2*pnorm(-abs(E[i,"Estimate"]/E[i,"SE"])), E[i,"Switchers"]))
  cat(sprintf("  AVG total effect: %+.2f (se %.2f, p %.3f)\n", A[1,"Estimate"], A[1,"SE"], 2*pnorm(-abs(A[1,"Estimate"]/A[1,"SE"]))))
  cat("  PLACEBOS (pre-trend; want ~0):\n")
  for(i in 1:nrow(P)) cat(sprintf("  Placebo_%d: %+.2f (se %.2f, p %.3f)\n",
      i, P[i,"Estimate"], P[i,"SE"], 2*pnorm(-abs(P[i,"Estimate"]/P[i,"SE"]))))
  cat(sprintf("  JOINT placebo p-value: %s  (low = pre-trends = design fails)\n",
      ifelse(is.null(m$results$p_jointplacebo),"NA",sprintf("%.3f", m$results$p_jointplacebo))))
  data.table(spec=lab,
    eff1=sprintf("%+.2f", E[1,"Estimate"]), eff2=sprintf("%+.2f", E[2,"Estimate"]), eff3=sprintf("%+.2f", E[3,"Estimate"]),
    ATE=round(A[1,"Estimate"],2), ATE_se=round(A[1,"SE"],2), ATE_p=round(2*pnorm(-abs(A[1,"Estimate"]/A[1,"SE"])),3),
    plac1=round(P[1,"Estimate"],2), plac2=round(P[2,"Estimate"],2),
    joint_plac_p=ifelse(is.null(m$results$p_jointplacebo), NA_real_, round(m$results$p_jointplacebo,3)))
}

cat("============== 07z2 dCDH DISTANCE/ACCESS DOSE (denom A) ==============\n")
r1 <- run_dcdh("acc20", NULL, "BINARY within-20km access")
r2 <- run_dcdh("neg_dist", 1, "CONTINUOUS proximity (-km), continuous=1 [SE caveat]")
out <- rbind(r1, r2)
fwrite(out, file.path(DIR_TABLES,"distance_dose_dcdh.csv"))
cat("\nsaved -> output/tables/distance_dose_dcdh.csv\n")
