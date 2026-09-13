# ============================================================================
# 09c2_denominator_breakdown.R — BREAKDOWN FRONTIER for the population
# denominator (MM 2026-07-30, robustness §. Companion to 09c).
#
# WHY a breakdown frontier and not more alternative series: a denominator error
# that is COMMON to treated and control municipalities, or constant over time
# within a municipality, cannot generate the estimate — 09c shows the
# %-of-baseline effect is invariant across four constructions. The only
# denominator story that can overturn the result is a DIFFERENTIAL, POST-
# TREATMENT error: treated municipalities' teen population being increasingly
# over-projected precisely after their unit opens, which would depress their
# measured rate mechanically and mimic a decline.
#
# DIRECTION (careful): measured rate = births / measured population. If the
# treated post-opening denominator is too LARGE, the measured rate is too LOW
# and the decline is spurious. Correcting that error means DEFLATING the
# treated post denominator by (1+d), which is algebraically identical to
# INFLATING the treated post rate by (1+d). So we scan d >= 0 applied to the
# treated post-treatment rate and find d* where the effect stops being
# significant. d* is then read as: "the treated denominator would have to be
# over-projected by more than d* percent after opening, relative to controls,
# for the effect to disappear."
#
# BENCHMARK: d* is meaningless without a yardstick, so we also compute the
# REALIZED differential discrepancy between our two independently built series
# (A vs B) over the same treated/post cells. If d* greatly exceeds what two
# genuinely different constructions disagree by, the bound is comfortable.
# Output: denominator_breakdown.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
       , .(adm3_pcode, year, nb_15_19, womenA_15_19, womenB_15_19, rateA_15_19, rateB_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]
est0 <- merge(p, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est0[, id := as.integer(factor(adm3_pcode))]
est0[, treated_post := as.integer(g > 0 & year >= g)]
cat("panel:", nrow(est0), "muni-years |", est0[, uniqueN(adm3_pcode)], "munis |",
    est0[g>0, uniqueN(adm3_pcode)], "treated | treated-post cells:", est0[, sum(treated_post)], "\n")

cs_att <- function(d){
  e <- copy(est0)
  # inflate the treated POST rate by (1+d) == deflate the treated post denominator
  e[, y := rateA_15_19 * fifelse(treated_post==1L, 1 + d, 1)]
  a <- suppressWarnings(suppressMessages(att_gt(
        yname="y", tname="year", idname="id", gname="g", xformla=~1, data=e,
        control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  data.table(delta=d, ATT=o$overall.att, SE=o$overall.se,
             p=2*pnorm(-abs(o$overall.att/o$overall.se)))
}

grid <- seq(0, 0.30, by=0.01)
res  <- rbindlist(lapply(grid, function(d){ r <- cs_att(d); cat(sprintf(
   "  d=%4.0f%%  ATT=%7.3f  SE=%5.3f  p=%.3f\n", 100*d, r$ATT, r$SE, r$p)); r }))

# frontier: first delta at which the effect is no longer significant at 5% / 10%
d05 <- res[p > 0.05, min(delta, na.rm=TRUE)]; d10 <- res[p > 0.10, min(delta, na.rm=TRUE)]
d00 <- res[ATT >= 0,  min(delta, na.rm=TRUE)]

# ---- BENCHMARK: realized differential A-vs-B denominator discrepancy ---------
# ratio of the two independently constructed denominators, differenced
# treated-post vs its own pre and vs controls over the same years.
r <- copy(est0)[, ratio := womenA_15_19 / womenB_15_19]
gap <- r[g>0, .(pre = mean(ratio[year <  g]), post = mean(ratio[year >= g])), by=adm3_pcode]
tr_drift <- gap[, mean(post/pre) - 1]
ctl <- r[g==0]
yrs <- r[g>0, .(g=unique(g))][, g]
ctl_drift <- mean(sapply(yrs, function(gy){
  x <- ctl[, .(pre=mean(ratio[year < gy]), post=mean(ratio[year >= gy])), by=adm3_pcode]
  x[, mean(post/pre) - 1] }))
realized <- tr_drift - ctl_drift

cat("\n=== BREAKDOWN FRONTIER (S1 CS level, denominator A) ===\n")
cat(sprintf("headline at d=0 : ATT %.2f (SE %.2f, p %.3f)\n",
            res[delta==0]$ATT, res[delta==0]$SE, res[delta==0]$p))
cat(sprintf("loses 5%% signif. at d* = %.0f%%\n", 100*d05))
cat(sprintf("loses 10%% signif. at d  = %.0f%%\n", 100*d10))
cat(sprintf("point estimate reaches 0 at d = %s\n",
            if (is.finite(d00)) sprintf("%.0f%%", 100*d00) else ">30% (not reached on the grid)"))
cat(sprintf("\nREALIZED differential A-vs-B denominator drift (treated-post vs controls): %+.2f%%\n",
            100*realized))
cat(sprintf("=> the required error is about %.0fx the discrepancy between our two series\n",
            abs(100*d05/(100*realized))))
fwrite(res, file.path(PROJ,"output","tables","denominator_breakdown.csv"))
fwrite(data.table(d05=d05, d10=d10, d_zero=d00, realized_diff_drift=realized),
       file.path(PROJ,"output","tables","denominator_breakdown_summary.csv"))
cat("saved -> denominator_breakdown.csv + denominator_breakdown_summary.csv\n")
