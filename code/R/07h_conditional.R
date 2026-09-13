# ============================================================================
# 07h_conditional.R — COVARIATE-CONDITIONAL CS (conditional parallel trends).
# Does the headline ATT survive conditioning on the imbalanced baseline covariates?
# Specs: (0) unconditional [headline]; (1) parsimonious core = wealth+urban+cwr2010
# (distinct SES/urbanicity/baseline-fertility axes); (2) FULL = MM's chosen set.
# Covariates time-invariant baseline; est_method="dr" (doubly-robust) when covariates
# present. Outcomes: level 15-19 (A) + age-DDD (A). NOTE few treated clusters (18) +
# collinear covariates -> the FULL spec may be unstable; the core is the safer read.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722)

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
est <- p[always_treated==0]; est[, id := as.integer(factor(adm3_pcode))]
est[, g := fifelse(ever_treated==1L, as.integer(first_year), 0L)]

specs <- list(
  `0-unconditional` = ~1,
  `cwr-only` = ~ cwr_2010_baseline,                                              # baseline fertility ALONE (mean-reversion)
  `P2-wealth+cwr` = ~ wealth_index + cwr_2010_baseline,                          # wealth comparator
  `A-noSES(urban+cwr+union+insured)` = ~ pct_urban + cwr_2010_baseline + pct_in_union + pct_insured_2013,
  `B1-quintile+cwr` = ~ mean_quintile_2018 + cwr_2010_baseline,                  # income-quintile analog of wealth+cwr
  `B2-quintile+urban+cwr+union+insured` = ~ mean_quintile_2018 + pct_urban + cwr_2010_baseline +
                       pct_in_union + pct_insured_2013,                          # income analog of MM5(wealth...)
  `MM5(wealth+urban+cwr+union+insured)` = ~ wealth_index + pct_urban + cwr_2010_baseline +
                       pct_in_union + pct_insured_2013)                          # wealth comparator

run <- function(yname, xf, label){
  em <- "reg"   # outcome-regression adjustment (no propensity model) — stable with few treated clusters
  a <- tryCatch(suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
         xformla=xf, data=est, control_group="notyettreated", est_method=em, bstrap=TRUE, cband=TRUE,
         biters=1000, base_period="universal", clustervars="id"))), error=function(e) e)
  if (inherits(a,"error")) return(data.table(spec=label, outcome=yname, est=em, ATT=NA_real_, SE=NA_real_,
                                             note=substr(conditionMessage(a),1,55)))
  gr <- tryCatch(suppressWarnings(suppressMessages(aggte(a, type="group", na.rm=TRUE))), error=function(e) e)
  if (inherits(gr,"error")) return(data.table(spec=label, outcome=yname, est=em, ATT=NA_real_, SE=NA_real_,
                                              note="aggte failed"))
  data.table(spec=label, outcome=yname, est=em, ATT=round(gr$overall.att,3), SE=round(gr$overall.se,3),
             t=round(gr$overall.att/gr$overall.se,2), note="")
}
cat("==================== 07h COVARIATE-CONDITIONAL CS (denom A) ====================\n")
res <- rbindlist(lapply(c("rateA_15_19","ddd_A"), function(y)
         rbindlist(lapply(names(specs), function(s) run(y, specs[[s]], s)))), fill=TRUE)
print(res[order(outcome,spec)], class=FALSE)
cat("\nHeadline unconditional: level -5.32, DDD -5.04. Conditional should be CLOSE if PT plausible.\n")
cat("est_method=reg (outcome-regression; stable w/ few treated). Compare to est_method=dr (07h dr run): dr was UNSTABLE.\n")
saveRDS(res, file.path(DIR_CLEAN,"conditional_cs_reg.rds"))
