# ============================================================================
# 02_diag_census_cohorts.R  —  DIAGNOSTIC (not pipeline output)
# Tracks female birth cohorts across the 2010 & 2022 censuses to judge whether
# ONE's 2015-2020 estimates "ran hot" (favor census/intercensal) or the 2022
# census undercounts (favor ONE projections). Also national-total cross-check.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))

f2010 <- file.path(DRPAPER,"analysis/datasets/population/CENSO2010RD-PERSONAS.csv")

# national single-year FEMALE counts, 2010 census (P27_SEXO 2=Mujer; age 0-110, no sentinel)
c10 <- fread(f2010, select=c("P27_SEXO","P29_EDAD_ANOS_CUMPLIDOS"), showProgress=FALSE)
F10 <- c10[P27_SEXO==2, .(n10=.N), by=.(age=P29_EDAD_ANOS_CUMPLIDOS)]
tot10 <- nrow(c10)

# national single-year FEMALE counts, 2022 census (P26_SEXO 2; P27_EDAD, drop 999)
c22 <- fread(RAW$census_csv, select=c("P26_SEXO","P27_EDAD"), showProgress=FALSE)
tot22 <- nrow(c22)
F22 <- c22[P26_SEXO==2 & P27_EDAD!=999, .(n22=.N), by=.(age=P27_EDAD)]

# cohort retention: a cohort aged `a` in 2010 is aged `a+12` in 2022 (join on birth year)
coh <- merge(F10[, .(birth=2010-age, n10)], F22[, .(birth=2022-age, n22)], by="birth")
coh[, R := n22/n10]
ag5 <- function(a) ifelse(a>=80,"80+", paste0(5*(a%/%5),"-",5*(a%/%5)+4))
coh[, age2022 := 2022 - birth]
coh[, grp22 := ag5(age2022)]

cat("=== TOTAL population cross-check ===\n")
cat("2010 census:", format(tot10,big.mark=","), " | 2022 census:", format(tot22,big.mark=","), "\n")
one2020 <- 10448499  # ONE municipio-estimate national total 2020 (verified earlier)
interp2020 <- tot10 + (tot22-tot10)*((2020-2010)/(2022-2010))
cat("ONE estimate 2020:", format(one2020,big.mark=","),
    " | intercensal(2010->2022) at 2020:", format(round(interp2020),big.mark=","),
    " | ONE/intercensal:", round(one2020/interp2020,4), "\n")

cat("\n=== FEMALE cohort retention 2010->2022 by 2022 age group ===\n")
cat("(R = census2022 / census2010 same cohort; expect ~0.97-0.99 from mortality only;\n")
cat(" R well below that = cohort loss = emigration OR 2022 undercount OR 2010 overcount)\n")
out <- coh[age2022>=12 & age2022<=54, .(n10=sum(n10), n22=sum(n22)), by=grp22]
out[, R := round(n22/n10,3)]
setorder(out, grp22)
print(out[, .(grp22, n10, n22, R)])

# ONE 2020 estimate vs 2022 census, key female groups (different cohorts -> trajectory)
pc <- as.data.table(readRDS(file.path(DIR_CLEAN,"prov_controls_2000_2030_long.rds")))
one20 <- pc[sex=="F" & year==2020, .(one2020=sum(pop)), by=age_group]
cen22 <- F22[, grp:=ag5(age)][, .(census2022=sum(n22)), by=grp]
cmp <- merge(one20, cen22, by.x="age_group", by.y="grp")
cmp[, ratio_ONE_over_census := round(one2020/census2022,3)]
cat("\n=== ONE 2020 estimate vs 2022 census, FEMALES (note: different cohorts) ===\n")
print(cmp[age_group %in% c("10-14","15-19","20-24","25-29","30-34"),
          .(age_group, one2020, census2022, ratio_ONE_over_census)])
