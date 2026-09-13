# ============================================================================
# 09i_matched_controls.R — committee item R2 (§44.2): match ever-treated to
# controls on BASELINE covariates, re-run CS on the matched sample.
# Design (settled with MM 2026-07-08, plan R2):
#   PRIMARY match (Mahalanobis nearest-neighbor, ratio 1:3 with replacement):
#     STRUCTURAL baseline variables only — wealth_index, pct_urban,
#     pct_educ_secplus, sns_per10k, pct_senasa_2013, log population (2019).
#   NEVER on the baseline teen rate (Daw-Hatfield mean reversion) — but its
#     balance is REPORTED (earned-balance demonstration), as is balance on the
#     2019 birth-composition variables not matched on.
#   SENSITIVITY match: + 2019 composition (mother age, %Haitian, %no-insurance,
#     %c-section).
# CS spec = 07t (not-yet-treated controls within the matched pool).
# Output: output/tables/matched_balance.csv + matched_cs.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(MatchIt)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
fm <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

# ---- baseline (2019) municipality dataset ----
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
fl <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_facility_levels_panel.rds")))[year==2019, .(adm3_pcode, pop_tot, n_hosp)]
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))[!is.na(adm3_pcode) & age_mom %in% 15:19 & birth_year==2019]
b[adm3_pcode %in% names(fm), adm3_pcode := fm[adm3_pcode]]
comp19 <- b[, .(age_mean = mean(age_mom), sh_haitian = 100*mean(nationality=="Haitian", na.rm=TRUE),
                sh_noins = 100*mean((insurance=="None")[!is.na(insurance)]),
                sh_csec  = 100*mean((csection==TRUE | csection==1)[!is.na(csection)])), by=adm3_pcode]
base <- merge(p[year==2019, .(adm3_pcode, rate2019=rateA_15_19, wealth_index, pct_urban, pct_educ_secplus, pct_senasa_2013)],
              hc, by="adm3_pcode", all.x=TRUE)
base <- merge(base, fl, by="adm3_pcode", all.x=TRUE)
base <- merge(base, comp19, by="adm3_pcode", all.x=TRUE)
base[, log_pop := log(pop_tot)]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
base <- merge(base, tr[!is.na(g), .(adm3_pcode, g, treated=as.integer(g>0))], by="adm3_pcode")
cat("matching sample:", nrow(base), "munis |", base[,sum(treated)], "treated | NA rows:",
    sum(!complete.cases(base[, .(wealth_index, pct_urban, pct_educ_secplus, sns_per10k, pct_senasa_2013, log_pop)])), "\n")

V1 <- c("wealth_index","pct_urban","pct_educ_secplus","sns_per10k","pct_senasa_2013","log_pop")
V2 <- c(V1, "age_mean","sh_haitian","sh_noins","sh_csec")
REPORT <- unique(c(V2, "rate2019", "n_hosp"))     # balance shown for everything incl. NOT-matched-on

smd <- function(d, v) sapply(v, function(x){
  mt <- d[treated==1][[x]]; mc <- d[treated==0][[x]]
  (mean(mt,na.rm=TRUE)-mean(mc,na.rm=TRUE))/sqrt((var(mt,na.rm=TRUE)+var(mc,na.rm=TRUE))/2) })

run_match <- function(vars, lab){
  dd <- base[complete.cases(base[, ..vars])]
  f <- as.formula(paste("treated ~", paste(vars, collapse="+")))
  mo <- matchit(f, data=dd, method="nearest", distance="mahalanobis", ratio=3, replace=TRUE)
  mm <- as.data.table(match.data(mo, drop.unmatched=TRUE))
  keep <- unique(mm$adm3_pcode)
  cat(sprintf("\n== %s: %d munis kept (%d treated + %d matched controls)\n",
      lab, length(keep), base[adm3_pcode %in% keep, sum(treated)], base[adm3_pcode %in% keep, sum(1-treated)]))
  balpre  <- smd(base, REPORT); balpost <- smd(base[adm3_pcode %in% keep], REPORT)
  # CS on the matched sample
  est <- merge(p[, .(adm3_pcode, year, rateA_15_19)], base[adm3_pcode %in% keep, .(adm3_pcode, g)], by="adm3_pcode")
  est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  bs <- est[g>0][, .SD[year==g-1L], by=adm3_pcode][, mean(rateA_15_19)]
  cat(sprintf("   matched-sample CS: ATT %.2f (SE %.2f, p=%.3f) | baseline %.1f (%.0f%%)\n",
      o$overall.att, o$overall.se, 2*pnorm(-abs(o$overall.att/o$overall.se)), bs, 100*o$overall.att/bs))
  list(bal=data.table(variable=REPORT, matched_on=REPORT %in% vars, smd_pre=round(balpre,3), smd_post=round(balpost,3), spec=lab),
       cs=data.table(spec=lab, n_treated=base[adm3_pcode %in% keep, sum(treated)],
                     n_controls=base[adm3_pcode %in% keep, sum(1-treated)],
                     ATT=round(o$overall.att,2), SE=round(o$overall.se,2),
                     p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3),
                     baseline=round(bs,2), pct=round(100*o$overall.att/bs,1)))
}
r1 <- run_match(V1, "Primary: structural + log pop")
r2 <- run_match(V2, "Sensitivity: + 2019 composition")
bal <- rbind(r1$bal, r2$bal); cs <- rbind(r1$cs, r2$cs)
cat("\n=== balance (standardized mean differences; headline ref -6.43) ===\n")
print(bal[spec==bal$spec[1]], class=FALSE)
fwrite(bal, file.path(TAB,"matched_balance.csv")); fwrite(cs, file.path(TAB,"matched_cs.csv"))
cat("\nsaved -> matched_balance.csv + matched_cs.csv\n")
