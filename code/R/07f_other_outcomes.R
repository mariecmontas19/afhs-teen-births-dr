# ============================================================================
# 07f_other_outcomes.R — CS ATT on OTHER outcomes (same spec as headline:
# not-yet controls, base_period=universal, municipality-clustered bootstrap, denom A).
# TIERS:
#  Fertility:        rateA_15_19 (headline), rateA_10_14 (secondary, early childbearing)
#  Health per-POP:   lbw_per1000women (teen LBW births per 1,000 women — escapes per-birth selection,
#                    but falls mechanically if total teen births fall, so it's a burden measure)
#  Health per-BIRTH: neo_any, lbw, preterm, csection among teen births — EXPLORATORY: the births sample
#                    is endogenous to treatment (composition/selection) → NOT clean causal outcomes.
# Per-birth outcomes have 11 NA cells (municipality-years w/ 0 teen births) -> allow_unbalanced_panel.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722)

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
est <- p[always_treated==0]; est[, id := as.integer(factor(adm3_pcode))]
est[, g := fifelse(ever_treated==1L, as.integer(first_year), 0L)]

run_cs <- function(yname, tier){
  hasNA <- anyNA(est[[yname]])
  a  <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
          xformla=~1, data=est, control_group="notyettreated", allow_unbalanced_panel=hasNA,
          bstrap=TRUE, cband=TRUE, biters=1000, base_period="universal", clustervars="id", est_method="reg")))
  gr <- suppressWarnings(suppressMessages(aggte(a, type="group", na.rm=TRUE)))
  base <- mean(est[[yname]], na.rm=TRUE)
  data.table(tier=tier, outcome=yname, ATT=round(gr$overall.att,3), SE=round(gr$overall.se,3),
             t=round(gr$overall.att/gr$overall.se,2),
             p_approx=round(2*pnorm(-abs(gr$overall.att/gr$overall.se)),3),
             base_mean=round(base,3), pct_of_base=round(100*gr$overall.att/base,1), unbalanced=hasNA)
}
cat("==================== 07f CS ATT — OTHER OUTCOMES (denom A) ====================\n")
res <- rbindlist(list(
  run_cs("rateA_15_19","fertility-HEADLINE"),
  run_cs("rateA_10_14","fertility-secondary(10-14)"),
  run_cs("lbw_per1000women","health-per-POPULATION(teen LBW/1000w)"),
  run_cs("neo_any","health-per-BIRTH*selection"),
  run_cs("lbw","health-per-BIRTH*selection"),
  run_cs("preterm","health-per-BIRTH*selection"),
  run_cs("csection","health-per-BIRTH*selection")))
print(res, class=FALSE)
cat("\n* per-BIRTH outcomes: teen-birth sample is endogenous to treatment (selection) -> EXPLORATORY, not clean causal.\n")
cat("p_approx = 2-sided normal approx on CS bootstrap SE (clustered at municipality).\n")
saveRDS(res, file.path(DIR_CLEAN,"other_outcomes_cs.rds"))
