# ============================================================================
# 09j_covid.R — COVID robustness (committee 2026-07-07, work plan R3).
# Question: is the headline (-6.43) an artifact of the pandemic period?
# Three variants, S1 (first opening), denominator A, CS spec identical to 07t:
#   A. DROP CALENDAR YEARS 2020-2021. Retained years {2016-19, 2022-25} recoded
#      to consecutive indices 1..8 (att_gt needs evenly spaced time; the gap
#      would break base_period="universal"). Cohorts 2020/2021 are remapped to
#      their first OBSERVED treated period (2022): in the retained panel they
#      are untreated through 2019 and treated in every observed year 2022+,
#      so under absorbing treatment g=2022(idx 5) is their observed onset.
#      Event-time for those 4 munis is shifted (true onset was in the gap) —
#      noted; variant C removes them entirely.
#   B. FULL PANEL, LATE COHORTS ONLY: treated = 2022+ openers (16); the 4
#      munis opening 2020-21 are EXCLUDED (not controls). All years 2016-2025.
#   C. A and C combined: years {2016-19, 2022-25} AND 2022+ cohorts only.
# Controls = never-treated + not-yet-treated, as in the headline.
# Baseline for % = mean teen rate at the (recoded) g-1, printed per variant.
# Output: output/tables/covid_robustness.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

pw  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
cat("cohorts:", paste(capture.output(print(bin[g>0,.N,keyby=g])), collapse=" | "), "\n")

run_cs <- function(d, glab="g", tlab="t"){
  est <- copy(d); est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname=tlab, idname="id", gname=glab,
        xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  list(att=o$overall.att, se=o$overall.se, ntr=uniqueN(est[get(glab)>0, adm3_pcode]))
}
res <- list()
add <- function(lab, r, base, note){
  res[[lab]] <<- data.table(variant=lab, n_treated=r$ntr, ATT=round(r$att,2), SE=round(r$se,2),
    p=round(2*pnorm(-abs(r$att/r$se)),3), baseline=round(base,2), pct=round(100*r$att/base,1), note=note)
  cat(sprintf("%-38s ATT=%.2f se=%.2f p=%.3f base=%.1f (%.0f%%) [%d treated]\n",
      lab, r$att, r$se, 2*pnorm(-abs(r$att/r$se)), base, 100*r$att/base, r$ntr))
}

# ---- A: drop years 2020-2021, recode time, remap 2020/21 cohorts to 2022 ----
yrs <- c(2016:2019, 2022:2025); ymap <- setNames(seq_along(yrs), yrs)     # 2016..2019->1..4, 2022..2025->5..8
dA <- merge(pw[year %in% yrs], bin[!is.na(g), .(adm3_pcode, g0=g)], by="adm3_pcode")
dA[, t := ymap[as.character(year)]]
dA[, g := fifelse(g0==0L, 0L, ymap[as.character(pmax(g0, 2022L))])]       # 2020/21 cohorts -> first observed treated yr 2022
stopifnot(dA[g0>0, uniqueN(adm3_pcode)]==20, !anyNA(dA$g))
baseA <- dA[g>0][, .SD[t==g-1L], by=adm3_pcode][, mean(rateA_15_19)]
add("A: drop 2020-21 (all 20, remapped)", run_cs(dA), baseA, "2020/21 cohorts observed-onset 2022; event time shifted for those 4")

# ---- B: full panel, 2022+ cohorts only ----
dB <- merge(pw, bin[!is.na(g) & (g==0L | g>=2022L), .(adm3_pcode, g)], by="adm3_pcode")
dB[, t := year]
stopifnot(dB[g>0, uniqueN(adm3_pcode)]==16)
baseB <- dB[g>0][, .SD[year==g-1L], by=adm3_pcode][, mean(rateA_15_19)]
add("B: full years, 2022+ cohorts (16)", run_cs(dB), baseB, "2020/21 openers excluded entirely")

# ---- C: drop 2020-21 AND 2022+ cohorts only ----
dC <- merge(pw[year %in% yrs], bin[!is.na(g) & (g==0L | g>=2022L), .(adm3_pcode, g0=g)], by="adm3_pcode")
dC[, t := ymap[as.character(year)]]
dC[, g := fifelse(g0==0L, 0L, ymap[as.character(g0)])]
baseC <- dC[g>0][, .SD[t==g-1L], by=adm3_pcode][, mean(rateA_15_19)]
add("C: drop 2020-21 + 2022+ cohorts", run_cs(dC), baseC, "strictest: no COVID years, no COVID-era openers")

RR <- rbindlist(res)
cat("\n=== COVID robustness (headline reference: -6.43, se 2.10, base 55.3) ===\n")
print(RR, class=FALSE)
fwrite(RR, file.path(TAB,"covid_robustness.csv"))
cat("\nsaved -> output/tables/covid_robustness.csv\n")
