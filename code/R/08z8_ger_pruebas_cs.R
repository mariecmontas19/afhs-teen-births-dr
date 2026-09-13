# ============================================================================
# 08z8_ger_pruebas_cs.R — colleague-feedback rebuild (§59.48), two parts:
#  A. Enrollment re-measured as a bona fide WDI-style GROSS ENROLLMENT RATIO:
#     numerator unchanged (all secondary enrollment, any age); denominator =
#     population of the OFFICIAL secondary ages 12-17 (Series A), built as
#     (3/5) x pop(10-14) + (3/5) x pop(15-19) per sex (within-group uniform
#     split — flagged approximation).
#  B. Pruebas Nacionales outcomes (05q panel): municipal weighted mean
#     z-scores (conv 1, MEDIA GEN+TEC, z within year x subject x modality)
#     and test-takers per 100 pop 17-18 (= 0.4 x pop 15-19, both sexes).
#     Years {2016-19, 2022-24}; 2020 (no scores) and 2021 (absent) dropped.
#     Municipalities opening 2020 or 2021 are EXCLUDED (their base school
#     year g-1 is unobservable); identification = 2022/2023 openers (g_edu
#     2023/2024), so max exposure is Year 1 — an honesty constraint.
#
# RULE-1B PREDICTIONS (stated before estimation, 2026-08-03):
#  A. GER: numerator unchanged, denominator smooth (12-17 vs 15-19 pop) ->
#     expect the SAME qualitative result as the old per-100-pop-15-19 rate
#     (old: T +1.60, p=.003; M +1.93, p=.001; F +1.17, p=.096), with levels
#     rescaled (~85-90 vs ~99) and slightly smaller point estimates since
#     the 12-17 denominator is larger. Pre-trend leads should stay flat.
#  B. takers/100: null-to-small positive (grade-6 retention was +, but max
#     exposure is 1 yr and the test is 5-6 grades downstream of entry).
#     z-scores: ~0 or slightly NEGATIVE (retention of marginal students
#     lowers the conditional mean), ns. A LARGE or significant score effect
#     would trigger an instrument check BEFORE interpretation.
# Spec: exact 07t/08z convention — not-yet controls, universal base,
# est="reg", muni-clustered multiplier bootstrap biters=2000; set.seed
# immediately before EVERY att_gt (§59.44). g_edu = first_year + 1.
# Output: output/tables/ger_pruebas_results.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
SEED <- 20260803L; BITERS <- 2000L
TAB <- DIR_TABLES
fold <- c("DOM010905","DOM051703","DOM012510")

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g_edu := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year)+1L,
                       fifelse(ever_treated==0L, 0L, NA_integer_))]

est <- function(d, yn, tvals){
  e <- d[year_end %in% tvals]
  e[, id := as.integer(factor(adm3_pcode))]
  set.seed(SEED)
  a <- suppressWarnings(suppressMessages(att_gt(yname=yn, tname="year_end", idname="id",
        gname="g_edu", xformla=~1, data=e, control_group="notyettreated",
        base_period="universal", est_method="reg", bstrap=TRUE, cband=TRUE,
        biters=BITERS, clustervars="id", allow_unbalanced_panel=TRUE, print_details=FALSE)))
  gr <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  dy <- suppressMessages(aggte(a, type="dynamic", na.rm=TRUE))
  pre <- dy$egt < -1
  lead_p <- if(sum(pre)>1){
    w <- rep(1/sum(pre), sum(pre)); avg <- sum(w*dy$att.egt[pre])
    ifm <- dy$inf.function$dynamic.inf.func.e[, pre, drop=FALSE]
    se <- sqrt(sum((ifm %*% w)^2))/sqrt(nrow(ifm)); 2*pnorm(-abs(avg/se))
  } else NA_real_
  base <- e[g_edu>0 & year_end==g_edu-1L, mean(get(yn), na.rm=TRUE)]
  data.table(ATT=gr$overall.att, SE=gr$overall.se,
             p=2*pnorm(-abs(gr$overall.att/gr$overall.se)),
             lo=gr$overall.att-1.96*gr$overall.se, hi=gr$overall.att+1.96*gr$overall.se,
             base=base, lead_p=lead_p, n_treated=e[g_edu>0, uniqueN(adm3_pcode)],
             n_munis=e[, uniqueN(adm3_pcode)])
}

## ---- A. GER 12-17 -----------------------------------------------------------
pan <- as.data.table(readRDS(file.path(DIR_CLEAN,"educ_condicion_muni.rds")))
sec <- pan[nivel=="SECUNDARIO", .(mat=sum(mat)), by=.(adm3_pcode, year_end, sexo)]
secT <- sec[, .(sexo="T", mat=sum(mat)), by=.(adm3_pcode, year_end)]
sec <- rbind(sec, secT)
pop <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))[
        age_group %in% c("10-14","15-19")]
p1217 <- pop[, .(pop1217 = 0.6*sum(pop[age_group=="10-14"]) + 0.6*sum(pop[age_group=="15-19"])),
             by=.(adm3_pcode, year_end=year, sexo=sex)]
p1217T <- p1217[, .(sexo="T", pop1217=sum(pop1217)), by=.(adm3_pcode, year_end)]
p1217 <- rbind(p1217, p1217T)
g <- merge(sec, p1217, by=c("adm3_pcode","year_end","sexo"))
g[, ger := 100*mat/pop1217]
g <- merge(g, bin[!is.na(g_edu), .(adm3_pcode, g_edu)], by="adm3_pcode")
cat("=== A. GER (official ages 12-17), CS 2016-2025 ===\n")
resA <- rbindlist(lapply(c("T","F","M"), function(s)
  cbind(part="A_GER", outcome=paste0("GER_", s), est(g[sexo==s], "ger", 2016:2025))))
print(resA[, .(outcome, ATT=round(ATT,2), SE=round(SE,2), p=round(p,3),
               base=round(base,1), lead_p=round(lead_p,2), n_treated)], class=FALSE)

## ---- B. Pruebas Nacionales --------------------------------------------------
pr <- as.data.table(readRDS(file.path(DIR_CLEAN,"pruebas_muni.rds")))
popT <- p1217[sexo=="T", .(adm3_pcode, year_end)]  # placeholder; takers denom below
p1519T <- pop[age_group=="15-19", .(pop1519=sum(pop)), by=.(adm3_pcode, year_end=year)]
pr <- merge(pr, p1519T, by=c("adm3_pcode","year_end"))
pr[, takers100 := 100*takers/(0.4*pop1519)]
pr <- merge(pr, bin[!is.na(g_edu), .(adm3_pcode, g_edu, first_year=g_edu-1L)], by="adm3_pcode")
drop2021 <- pr[g_edu %in% c(2021L,2022L), unique(adm3_pcode)]
cat("\n=== B. Pruebas Nacionales (2016-19, 2022-24; conv 1, GEN+TEC) ===\n")
cat("excluded (opened 2020/2021, base year unobservable):", length(drop2021), "municipalities\n")
prb <- pr[!adm3_pcode %in% drop2021]
YRS <- c(2016:2019, 2022:2024)
resB <- rbindlist(lapply(c("z_esp","z_mat","z_all","takers100"), function(v)
  cbind(part="B_pruebas", outcome=v, est(prb, v, YRS))))
print(resB[, .(outcome, ATT=round(ATT,3), SE=round(SE,3), p=round(p,3),
               base=round(base,2), lead_p=round(lead_p,2), n_treated)], class=FALSE)

out <- rbind(resA, resB)
fwrite(out, file.path(TAB,"ger_pruebas_results.csv"))
cat("\nsaved -> ger_pruebas_results.csv\n")
