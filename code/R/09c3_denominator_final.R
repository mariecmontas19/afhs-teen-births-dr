# ============================================================================
# 09c3_denominator_final.R — final numbers for the denominator robustness
# exhibit (MM 2026-07-31). Two panels:
#   Panel A: level + triple difference under denominators A and B
#   Panel B: breakdown frontier (differential post-treatment error, d = 0..15%
#            in steps of 3) + the realized A-vs-B drift placebo.
# CONVENTION: set.seed(SEED) immediately before EVERY att_gt call so each cell
# is reproducible in isolation (multiplier-bootstrap SEs depend on RNG state,
# so script-order-dependent seeding gives non-replicable SEs; see notes §59.44).
# Dropped variants (MM): the /1.10 back-test rescale (uniform, cannot move the
# %-of-baseline effect by construction) and the constant-2020-share allocation
# (differs from A by 0.16% on average because both share ONE province totals —
# it tests only the within-province split). Both live in the table notes.
# Output: denominator_panelA.csv + denominator_panelB.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
fold <- c("DOM010905","DOM051703","DOM012510"); BITERS <- 2000L; SEED <- 20260722L

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
      , .(adm3_pcode, year, womenA_15_19, womenB_15_19, rateA_15_19, rateB_15_19, ddd_A, ddd_B)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]
e <- merge(p, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
e[, id := as.integer(factor(adm3_pcode))]
e[, tp := as.integer(g>0 & year>=g)]
e[, lr_AB := 100*log(womenA_15_19/womenB_15_19)]
stopifnot(e[, uniqueN(adm3_pcode)]==146L)

est <- function(d, yn){
  set.seed(SEED)
  a <- suppressWarnings(suppressMessages(att_gt(yname=yn, tname="year", idname="id", gname="g",
        xformla=~1, data=d, control_group="notyettreated", bstrap=TRUE, cband=TRUE,
        biters=BITERS, base_period="universal", clustervars="id", est_method="reg")))
  gr <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  data.table(ATT=gr$overall.att, SE=gr$overall.se,
             p=2*pnorm(-abs(gr$overall.att/gr$overall.se)),
             lo=gr$overall.att-1.96*gr$overall.se, hi=gr$overall.att+1.96*gr$overall.se)
}

# ---- Panel A -----------------------------------------------------------------
pa <- rbindlist(list(
  cbind(series="A", estimand="level", est(e,"rateA_15_19")),
  cbind(series="A", estimand="ddd",   est(e,"ddd_A")),
  cbind(series="B", estimand="level", est(e,"rateB_15_19")),
  cbind(series="B", estimand="ddd",   est(e,"ddd_B"))))
baseA <- e[g>0][year==g-1L, mean(rateA_15_19)]; baseB <- e[g>0][year==g-1L, mean(rateB_15_19)]
pa[, pct_base := fifelse(estimand=="level", 100*ATT/fifelse(series=="A", baseA, baseB), NA_real_)]
pa[, baseline := fifelse(estimand=="level", fifelse(series=="A", baseA, baseB), NA_real_)]
cat("=== Panel A ===\n"); print(pa[, lapply(.SD, function(x) if(is.numeric(x)) round(x,3) else x)], class=FALSE)

# ---- Panel B -----------------------------------------------------------------
pb <- rbindlist(lapply(c(0,.03,.06,.09,.12,.15), function(d){
  x <- copy(e); x[, y := rateA_15_19 * fifelse(tp==1L, 1+d, 1)]
  cbind(row=sprintf("%.0f%%", 100*d), est(x, "y")) }))
pb <- rbind(pb, cbind(row="placebo_drift_AB", est(e, "lr_AB")))
cat("\n=== Panel B ===\n"); print(pb[, lapply(.SD, function(x) if(is.numeric(x)) round(x,3) else x)], class=FALSE)

fwrite(pa, file.path(DIR_TABLES,"denominator_panelA.csv"))
fwrite(pb, file.path(DIR_TABLES,"denominator_panelB.csv"))
cat("saved -> denominator_panelA.csv + denominator_panelB.csv\n")
