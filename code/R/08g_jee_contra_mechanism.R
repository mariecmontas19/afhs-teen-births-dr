# ============================================================================
# 08g_jee_contra_mechanism.R — two mechanism datasets put through the CS machinery:
#   (1) CONTRACEPTION heterogeneity: is the AU effect larger where baseline (2013)
#       unmet need for FP was higher? (median split of treated, vs shared never-treated)
#   (2) JEE confound check: does the headline ATT survive controlling for baseline
#       (2019-20) JEE intensity? + is the 2019->2024 JEE CHANGE differentially tied to
#       AU timing (treated vs never)?
#   (3) JEE heterogeneity: effect by baseline JEE intensity (median split).
# S1 (first opening) only; Denominator A; not-yet controls; est=reg; universal base;
# muni-clustered. Reuses the cs_sub() subgroup pattern from 08_heterogeneity.R.
# CAVEATS: ~10 treated/half (wide CIs); contraception = all-ages province baseline
# (coarse); JEE baseline already high/near-saturated (limited spread). Suggestive.
# Output: output/tables/mechanism_jee_contra.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables")

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
         , .(adm3_pcode, year, rateA_15_19, wealth_index, pct_urban, pct_educ_secplus)]
hc  <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
treated <- bin[!is.na(g) & g>0, .(adm3_pcode, g)]; never <- bin[g==0, adm3_pcode]

# moderators
con <- as.data.table(readRDS(file.path(DIR_CLEAN,"dhs_contraception_province.rds")))[, .(adm3_pcode, unmet_2013, cpr_any_2013, satany_2013)]
jee <- as.data.table(readRDS(file.path(DIR_CLEAN,"jee_rollout_muni.rds")))
jw  <- dcast(jee, adm3_pcode ~ school_year, value.var="pct_jee_sch")
setnames(jw, c("2019-2020","2024-2025"), c("jee_2019","jee_2024"))
jw[, jee_chg := jee_2024 - jee_2019]

# ---- shared subgroup CS (subgroup treated + ALL never) ----------------------
cs_sub <- function(pcodes){
  est <- merge(p[, .(adm3_pcode, year, rateA_15_19)],
               rbind(treated[adm3_pcode %in% pcodes], data.table(adm3_pcode=never, g=0L)), by="adm3_pcode")
  est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  data.table(att=round(o$overall.att,2), se=round(o$overall.se,2), n=length(pcodes))
}
split_test <- function(modtab, var, label, under_high){
  M <- merge(treated, modtab, by="adm3_pcode")[!is.na(get(var))]
  med <- median(M[[var]]); hi <- M[get(var)>=med, adm3_pcode]; lo <- M[get(var)<med, adm3_pcode]
  und <- if(under_high) hi else lo; srv <- setdiff(M$adm3_pcode, und)
  ru <- cs_sub(und); rs <- cs_sub(srv)
  cat(sprintf("\n[%s] median=%.1f | treated split %d\n", label, med, nrow(M)))
  cat(sprintf("   MORE underserved %+.2f (se %.2f, n=%d) | LESS %+.2f (se %.2f, n=%d)\n",
      ru$att,ru$se,ru$n, rs$att,rs$se,rs$n))
  rbind(cbind(test=label, group="More underserved", ru), cbind(test=label, group="Less underserved", rs))
}

cat("==================== reference: overall S1 ATT ====================\n")
ref <- cs_sub(treated$adm3_pcode); cat(sprintf("overall ATT %+.2f (se %.2f), %d treated\n", ref$att, ref$se, nrow(treated)))

# (1) contraception — 3 complementary baselines. "More underserved" should ALWAYS be the
# scarcer-access half: high unmet need, OR low CPR-any, OR low satisfied-demand-any. If the
# mechanism is real, all three should show the bigger effect in the underserved half.
r_con <- split_test(con, "unmet_2013",   "Baseline unmet need (2013)",          under_high=TRUE)   # high unmet = under
r_cpr <- split_test(con, "cpr_any_2013", "Baseline CPR any (2013)",             under_high=FALSE)  # low CPR  = under
r_sat <- split_test(con, "satany_2013",  "Baseline satisfied demand any (2013)",under_high=FALSE)  # low sat  = under
# (3) JEE heterogeneity: low baseline JEE = more underserved
r_jee <- split_test(jw[, .(adm3_pcode, jee_2019)], "jee_2019", "Baseline JEE intensity (2019-20)", under_high=FALSE)

# (2a) JEE confound CONTROL: CS with baseline covariates +/- baseline JEE, common sample
cov_cs <- function(xf, dat){
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
        xformla=xf, data=dat, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE)); c(att=round(o$overall.att,2), se=round(o$overall.se,2))
}
base_cov <- merge(p, hc, by="adm3_pcode", all.x=TRUE)
base_cov <- merge(base_cov, jw[, .(adm3_pcode, jee_2019)], by="adm3_pcode", all.x=TRUE)
est <- merge(base_cov, rbind(treated, data.table(adm3_pcode=never, g=0L)), by="adm3_pcode")
n_all <- uniqueN(est$adm3_pcode)
est <- est[!is.na(jee_2019) & !is.na(wealth_index) & !is.na(sns_per10k)]   # common non-missing sample
est[, id := as.integer(factor(adm3_pcode))]
cat(sprintf("\n==================== JEE confound-control (common sample: %d of %d municipalities) ====================\n",
            uniqueN(est$adm3_pcode), n_all))
c0 <- cov_cs(~1, est); c1 <- cov_cs(~ wealth_index + sns_per10k + pct_urban + pct_educ_secplus, est)
c2 <- cov_cs(~ wealth_index + sns_per10k + pct_urban + pct_educ_secplus + jee_2019, est)
c3 <- cov_cs(~ jee_2019, est)
cat(sprintf("  no covs                 : %+.2f (se %.2f)\n", c0["att"], c0["se"]))
cat(sprintf("  headline covs           : %+.2f (se %.2f)\n", c1["att"], c1["se"]))
cat(sprintf("  headline covs + JEE 2019: %+.2f (se %.2f)\n", c2["att"], c2["se"]))
cat(sprintf("  JEE 2019 only           : %+.2f (se %.2f)   <- if ~ no-covs, JEE not confounding\n", c3["att"], c3["se"]))

# (2b) JEE timing correlation: is the 2019->2024 JEE change differential by AU status?
# Compare in-window-TREATED vs NEVER-treated only (exclude always-treated, which aren't in either group).
arm_map <- bin[, .(adm3_pcode, arm=fcase(ever_treated==1L & always_treated==0L,"treated",
                                          ever_treated==0L,"never", default=NA_character_))]
jx <- merge(jw, arm_map, by="adm3_pcode")[!is.na(jee_chg) & !is.na(arm)]
tt <- jx[, .(mean_chg=round(mean(jee_chg),1), sd=round(sd(jee_chg),1), n=.N), by=arm]
cat("\n==================== JEE rollout (2019->2024 change): treated vs never ====================\n")
print(tt, class=FALSE)
wt <- t.test(jee_chg ~ arm, data=jx)
cat(sprintf("treated vs never JEE-change diff: t=%.2f, p=%.3f -> %s\n", wt$statistic, wt$p.value,
            ifelse(wt$p.value>=0.10,"NOT differential (JEE rollout common to both -> not a differential confound)","DIFFERENTIAL (investigate)")))

out <- rbind(cbind(scenario="S1", r_con), cbind(scenario="S1", r_cpr), cbind(scenario="S1", r_sat), cbind(scenario="S1", r_jee))
fwrite(out, file.path(TAB,"mechanism_jee_contra.csv"))
cat("\nsaved -> output/tables/mechanism_jee_contra.csv\n")
