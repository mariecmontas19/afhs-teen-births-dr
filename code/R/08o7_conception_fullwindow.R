# ============================================================================
# 08o7_conception_fullwindow.R — FULL-WINDOW (2016-2025) age-at-conception
# headline via HYBRID classification (MM request):
#   2018-2025: EXACT age at conception (mother_dob available) — as 08o.
#   2016-2017: EXPECTED ALLOCATION (mother_dob missing): a birth at age a with
#     gestation d days contributes weight w = min(d,365)/365.25 to conception
#     age a-1 and (1-w) to age a — uses each record's ACTUAL gestational age.
#     (E[1{birthday in gestation window}] = d/365.25 under uniform birthdays.)
# VALIDATION: the expected-allocation rule applied to 2018-25 (where exact is
# known) must reproduce the exact counts closely — reported below.
# Outcome: conceptions at 15-19 per 1,000 women 15-19. S1; CS spec = 07t.
# Output: output/tables/conception_fullwindow.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
foldmap <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
b <- b[!is.na(adm3_pcode) & birth_year %in% 2016:2025 & !is.na(age_mom)]
b[adm3_pcode %in% names(foldmap), adm3_pcode := foldmap[adm3_pcode]]
b[, gest_d := as.numeric(birth_date - conception_date)]                     # = gestage_wks*7
b[, w := pmin(gest_d, 365)/365.25]                                          # P(birthday in window)
b[, age_conc := floor(as.numeric(difftime(conception_date, mother_dob, units="days"))/365.25)]
b[!is.na(age_conc) & (abs(age_mom-age_conc)>1 | age_conc>age_mom), age_conc := NA]

# ---- expected-allocation conception-15-19 count (usable all years) ----
# contribution to 15-19 conceptions: w if age_mom-1 in 15:19, plus (1-w) if age_mom in 15:19
b[, exp_c1519 := fifelse(is.na(w), NA_real_,
                 w*as.numeric((age_mom-1) %in% 15:19) + (1-w)*as.numeric(age_mom %in% 15:19))]

# ---- VALIDATION on 2018-25 (exact known) ----
v <- b[birth_year >= 2018 & !is.na(age_conc) & !is.na(exp_c1519)]
cat(sprintf("validation 2018-25: exact conceptions 15-19 = %s | expected-allocation = %s (ratio %.4f)\n",
    format(v[age_conc %in% 15:19, .N], big.mark=","),
    format(round(v[, sum(exp_c1519)]), big.mark=","),
    v[, sum(exp_c1519)] / v[age_conc %in% 15:19, .N]))

# ---- hybrid count per municipality-year ----
b[, c1519_hybrid := fifelse(birth_year >= 2018 & !is.na(age_conc),
                            as.numeric(age_conc %in% 15:19), exp_c1519)]
cat("hybrid NA (dropped):", b[is.na(c1519_hybrid), .N], "of", nrow(b), "\n")
cnt <- b[!is.na(c1519_hybrid), .(c1519 = sum(c1519_hybrid)), by=.(adm3_pcode, year=birth_year)]

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, womenA_15_19)]
d0 <- merge(p, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
d0[is.na(c1519), c1519 := 0]
d0 <- d0[!adm3_pcode %in% fold]
d0[, rate_c := 1000*c1519/womenA_15_19]
cat("panel:", nrow(d0), "rows (expect 1550) |", d0[,uniqueN(adm3_pcode)], "munis\n")

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
est0 <- merge(d0, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est0[, id := as.integer(factor(adm3_pcode))]
a <- suppressWarnings(suppressMessages(att_gt(yname="rate_c", tname="year", idname="id", gname="g",
      xformla=~1, data=est0, control_group="notyettreated", base_period="universal", est_method="reg",
      bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
bs <- est0[g>0 & year==g-1L, mean(rate_c)]
RR <- data.table(outcome="Conceptions 15-19 per 1,000 women 15-19 (HYBRID, 2016-25)",
                 ATT=round(o$overall.att,2), SE=round(o$overall.se,2),
                 p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3),
                 baseline=round(bs,2), pct=round(100*o$overall.att/bs,1), n_treated=uniqueN(est0[g>0,adm3_pcode]))
cat("\n=== full-window hybrid conception headline (refs: exact 2018-25 -8.22; birth-age -6.43) ===\n")
print(RR, class=FALSE)
fwrite(RR, file.path(TAB,"conception_fullwindow.csv"))
cat("saved -> conception_fullwindow.csv\n")
