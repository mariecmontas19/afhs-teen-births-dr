# ============================================================================
# 07b_honestdid.R — HonestDiD (Rambachan & Roth 2023) parallel-trends SENSITIVITY
# on the CS event study, under the new specs S1 (binary any-unit) & S2 (modernization),
# each on the level teen rate (rateA_15_19) and the pre-differenced age-DDD (ddd_A).
# HonestDiD is NOT a PT test — it reports the "breakdown M-bar": the smallest
# relative-magnitudes bound (post-period PT violation as a multiple of the LARGEST
# pre-period deviation) at which the robust CI for the AVERAGE post-effect first
# includes 0. Large breakdown = robust; small = fragile. The did Wald pre-test is
# singular here (see 09 block D), so this is THE parallel-trends robustness.
#
# Bridge (VERIFIED in earlier build): betahat = CS dynamic att.egt; sigma =
# t(IF)%*%IF/n^2 from the CS influence function reproduces the CS analytical SEs.
# base_period="universal" REQUIRED. Event window e in [-4,3] for the yardstick
# (well-populated; excludes 1-2-municipio far leads). Same fold(3) as 07t headline.
# Output: data/clean/honestdid_results.rds + output/figures/fig9_honestdid_*.png
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(HonestDiD); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures"); fold <- c("DOM010905","DOM051703","DOM012510")

p0  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]
mkest <- function(gt){ d<-merge(p0, gt[!is.na(g),.(adm3_pcode,g)],by="adm3_pcode"); d[,id:=as.integer(factor(adm3_pcode))]; d[] }
MBARS <- c(0.1,0.2,0.3,0.4,0.5,0.75,1,1.5,2)

honest_one <- function(est, yname, lab){
  a  <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
          xformla=~1, data=est, control_group="notyettreated", bstrap=FALSE, cband=FALSE,
          base_period="universal", clustervars="id", est_method="reg")))
  es <- suppressWarnings(suppressMessages(aggte(a, type="dynamic", na.rm=TRUE, min_e=-4, max_e=3)))
  IF <- es$inf.function$dynamic.inf.func.e; n <- nrow(IF)
  sigma_full <- t(IF) %*% IF / n / n
  egt <- es$egt; keep <- egt != -1
  chk <- max(abs(sqrt(diag(sigma_full))[keep] - es$se.egt[keep]))
  bet <- es$att.egt[keep]; sig <- sigma_full[keep, keep]
  nPre <- sum(egt[keep] < 0); nPost <- sum(egt[keep] >= 0)
  lvec <- rep(1/nPost, nPost)
  cat(sprintf("\n[%s] egt(kept)=%s | numPre=%d numPost=%d | bridge max|se diff|=%.5f | yardstick max|pre lead|=%.2f\n",
              lab, paste(egt[keep],collapse=","), nPre, nPost, chk, max(abs(bet[1:nPre]))))
  orig <- constructOriginalCS(betahat=bet, sigma=sig, numPrePeriods=nPre, numPostPeriods=nPost, l_vec=lvec)
  sens <- suppressWarnings(createSensitivityResults_relativeMagnitudes(betahat=bet, sigma=sig,
            numPrePeriods=nPre, numPostPeriods=nPost, l_vec=lvec, Mbarvec=MBARS))
  sdt <- as.data.table(sens)
  tab <- rbind(data.table(Mbar=0, lb=as.numeric(orig$lb), ub=as.numeric(orig$ub), method=as.character(orig$method)),
               sdt[, .(Mbar, lb=as.numeric(lb), ub=as.numeric(ub), method=as.character(method))])
  tab[, includes0 := lb<=0 & ub>=0][, outcome := lab][, `:=`(lb=round(lb,2), ub=round(ub,2))]
  pl <- createSensitivityPlot_relativeMagnitudes(sens, orig)
  ggsave(file.path(FIG, paste0("fig9_honestdid_", gsub("[^a-z0-9]","",tolower(lab)), ".png")), pl, width=7, height=4.5, dpi=150)
  tab[]
}

cat("==================== 07b HonestDiD SENSITIVITY — S1 & S2 (denom A) ====================\n")
dS1 <- mkest(bin); dS2 <- mkest(mod)
res <- rbind(
  honest_one(dS1, "rateA_15_19", "S1 level 15-19"),
  honest_one(dS1, "ddd_A",       "S1 age-DDD"),
  honest_one(dS2, "rateA_15_19", "S2 level 15-19"),
  honest_one(dS2, "ddd_A",       "S2 age-DDD"))
cat("\n--- robust CIs for the AVERAGE post-effect, by Mbar (relative-magnitudes) ---\n")
print(res[, .(outcome, Mbar, lb, ub, includes0)], class=FALSE)
bd <- res[method!="Original" & includes0==TRUE, .(breakdown_Mbar=min(Mbar)), by=outcome]
cat("\nBreakdown Mbar (smallest Mbar where robust CI first includes 0; higher = more robust):\n")
print(bd, class=FALSE)
saveRDS(res, file.path(DIR_CLEAN,"honestdid_results.rds"))
fwrite(res, file.path(DIR_TABLES,"honestdid_S1_S2.csv"))
cat("\nsaved -> data/clean/honestdid_results.rds + output/tables/honestdid_S1_S2.csv + figures/fig9_honestdid_*.png\n")
