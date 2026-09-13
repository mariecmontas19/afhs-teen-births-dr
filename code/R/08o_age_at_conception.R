# ============================================================================
# 08o_age_at_conception.R — committee item R6: classify births by the mother's
# age AT CONCEPTION (not at birth) and re-run the headline CS.
#   age_conc = floor((conception_date - mother_dob)/365.25);
#   conception_date = birth_date - gestational age (built in 03).
# DATA CONSTRAINT (verified): mother_dob is a 2018+ field (missing 99.9% in
# 2016, 53.1% in 2017, ~0% from 2018) -> analysis runs on panel years 2018-2025
# (cohorts 2020+ keep >=2 pre-years). Records with |age_mom - age_conc| > 1 or
# age_conc > age_mom (dob/gestation inconsistencies) -> age_conc = NA, excluded
# and counted. Sanity anchor: 74.7% of mothers are age_mom-1 at conception
# (theoretical P(birthday in gestation window) ~ 0.74).
# Outcomes (common denominator womenA_15_19, fold(3) as in 08n):
#   (i)  births to mothers AGED 15-19 AT BIRTH, years 2018-25  (reference)
#   (ii) births CONCEIVED at ages 15-19,      years 2018-25  (the new variable)
# Both timed by BIRTH year (consistent with denominators). S1; CS spec = 07t.
# Output: output/tables/age_at_conception.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
foldmap <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
b <- b[!is.na(adm3_pcode) & birth_year %in% 2018:2025]
b[adm3_pcode %in% names(foldmap), adm3_pcode := foldmap[adm3_pcode]]
b[, age_conc := floor(as.numeric(difftime(conception_date, mother_dob, units="days"))/365.25)]
bad <- b[!is.na(age_conc) & (abs(age_mom-age_conc)>1 | age_conc>age_mom), .N]
b[!is.na(age_conc) & (abs(age_mom-age_conc)>1 | age_conc>age_mom), age_conc := NA]
cat(sprintf("2018-25 births: %s | age_conc NA: %s (%.1f%%; of which %s inconsistent dob/gestation set NA)\n",
    format(nrow(b),big.mark=","), format(b[is.na(age_conc),.N],big.mark=","),
    100*b[,mean(is.na(age_conc))], format(bad,big.mark=",")))
cat(sprintf("teen at birth (15-19): %s | teen at conception: %s | conceived-teen born-20: %s\n",
    format(b[age_mom %in% 15:19,.N],big.mark=","), format(b[age_conc %in% 15:19,.N],big.mark=","),
    format(b[age_conc %in% 15:19 & age_mom==20,.N],big.mark=",")))

# ---- municipality-year counts + rates (common denominator A) ----
cnt <- b[, .(nb_birth=sum(age_mom %in% 15:19), nb_conc=sum(age_conc %in% 15:19)), by=.(adm3_pcode, year=birth_year)]
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[year %in% 2018:2025,
       .(adm3_pcode, year, womenA_15_19)]
d0 <- merge(p, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
for(v in c("nb_birth","nb_conc")) d0[is.na(get(v)), (v):=0L]
d0 <- d0[!adm3_pcode %in% fold]
d0[, `:=`(rate_birth=1000*nb_birth/womenA_15_19, rate_conc=1000*nb_conc/womenA_15_19)]
cat("panel check: rows", nrow(d0), "(expect 155x8=1240) | munis", d0[,uniqueN(adm3_pcode)], "| NA:", sum(is.na(d0$rate_conc)), "\n")

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]

cs_one <- function(yname){
  est <- merge(d0, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  bs <- est[g>0 & year==g-1L, mean(get(yname))]
  data.table(ATT=round(o$overall.att,2), SE=round(o$overall.se,2),
             p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3),
             baseline=round(bs,2), pct=round(100*o$overall.att/bs,1),
             n_treated=uniqueN(est[g>0,adm3_pcode]))
}
r1 <- cs_one("rate_birth"); r2 <- cs_one("rate_conc")
RR <- rbind(cbind(outcome="Age at BIRTH 15-19 (2018-25 window)", r1),
            cbind(outcome="Age at CONCEPTION 15-19 (2018-25)",  r2))
cat("\n=== age-at-conception robustness (headline ref: -6.43, 2016-25, age at birth) ===\n")
print(RR, class=FALSE)
fwrite(RR, file.path(TAB,"age_at_conception.csv"))
cat("\nsaved -> output/tables/age_at_conception.csv\n")
