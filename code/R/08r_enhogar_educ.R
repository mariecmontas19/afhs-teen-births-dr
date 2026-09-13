# ============================================================================
# 08r_enhogar_educ.R — R8 first pass: EARLY HUMAN-CAPITAL indicators via a
# province-intensity cohort-exposure design (Branson-Byker 2018 analog;
# respondents carry only PROVINCE, so treatment is aggregated UP:
# province_au_intensity.rds = female-10-19-pop share in treated municipios).
#
# WAVES v1 (codebook-verified 2026-07-10; §59.3): 2018 (pre) + 2022 (post).
# 2024 public files carry NO province (REGION only, 10 macro-regions) -> excluded
# from v1; region-level extension possible later. 2015/16/21 = later extension.
#   sex: H202/P202 (1=male? 2=female? -> VERIFIED BELOW against known pop shares)
#   age: H203/P203 (years, 99=NA); attendance: H302/P302 (1 asiste / 2 asistio /
#   3 nunca / 9 NA); level: H304/P303 (1 preesc 2 prim 3 sec 4 univ 5+ postgrad,
#   8/9 NA); grade: H305/P304 (0-8, 98/99 NA). Weights: Factor_expansion /
#   F_expansion / FEXPANSION. Province: HPROVI(N) -> prov_code (codes assumed
#   = ONE codes; VERIFIED below by DN share).
#
# OUTCOMES (v1, mapping-light): (a) ENROLLED (asiste==1), women 15-19;
#   (b) BEYOND-PRIMARY (nivel>=3), women 18-22. Exposure: (a) contemporaneous
#   province intensity at survey year; (b) intensity at the year she was 15.
# MODEL: outcome ~ exposure + province FE + wave FE + age FE, weights, cluster
#   at province (32).
# PREDICTION (rule 1b): enrollment small-positive-or-null; beyond-primary null
#   (exposure too recent). This is an EXPLORATORY "early indicators" exhibit.
# Output: output/tables/enhogar_educ_firstpass.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(fixest)})
TAB <- file.path(PROJ,"output","tables")
ENH <- file.path(DRPAPER, "analysis/datasets/Encuestas de Hogares de Prósitos Múltiples (ENHOGAR)")

rd <- function(file, cols, newnames, wave){
  hdr <- names(fread(file, nrows=0))
  sel <- sapply(cols, function(p) grep(p, hdr, value=TRUE, useBytes=TRUE)[1])
  stopifnot(!anyNA(sel))
  d <- fread(file, select=unname(sel), showProgress=FALSE)
  setnames(d, unname(sel), newnames)
  d[, wave := wave]; d
}
w18 <- rd(file.path(ENH,"2018/Personas_ENH18.csv"),
          c("^H202$","^H203$","^H302$","^H304$","^H305$","^HPROVI","^Factor_expansion$"),
          c("sex","age","asiste","nivel","curso","prov","fexp"), 2018L)
w22 <- rd(file.path(ENH,"2022/Personas_ENH22.csv"),
          c("^P202$","^P203$","^P302$","^P303$","^P304$","^HPROVI","^F_expansi"),
          c("sex","age","asiste","nivel","curso","prov","fexp"), 2022L)
d <- rbind(w18, w22)
cat("pooled rows:", nrow(d), "by wave:\n"); print(d[,.N,by=wave])

# ---- verification of codings (rule 1a) ----
cat("sex split by wave (expect ~51/49):\n"); print(d[sex %in% 1:2, .(pct2=round(100*mean(sex==2),1)), by=wave])
cat("prov distinct by wave:\n"); print(d[, .(nprov=uniqueN(prov)), by=wave])
# DN check: province code 1 should be ~ 9-11% of the (weighted) population
cat("weighted share prov==1 (DN, expect ~.09-.11):\n")
print(d[!is.na(fexp), .(sh=round(sum(fexp[prov==1])/sum(fexp),3)), by=wave])

d <- d[sex==2 & age %between% c(12,30)]
d[asiste %in% 9, asiste := NA]
d[, enrolled := fifelse(!is.na(asiste), as.integer(asiste==1), NA_integer_)]
d[nivel %in% c(8,9), nivel := NA]
d[, beyond_prim := fifelse(!is.na(nivel), as.integer(nivel>=3), NA_integer_)]

# ---- exposure from province intensity ----
ip <- as.data.table(readRDS(file.path(DIR_CLEAN,"province_au_intensity.rds")))
# (a) contemporaneous (survey-year) intensity — enrollment margin
d <- merge(d, ip[, .(prov=prov_code, wave=year, exp_now=intensity)], by=c("prov","wave"), all.x=TRUE)
# (b) intensity at the year she turned 15 — attainment margin (0 before 2016)
d[, yr_at15 := wave - (age - 15L)]
ip15 <- ip[, .(prov=prov_code, yr_at15=year, exp_at15=intensity)]
d <- merge(d, ip15, by=c("prov","yr_at15"), all.x=TRUE)
d[yr_at15 < 2016, exp_at15 := 0]
cat("exposure merge: exp_now NA", d[is.na(exp_now),.N], "| exp_at15 NA", d[is.na(exp_at15),.N], "\n")

# ---- (a) enrollment, women 15-19 ----
e1 <- d[age %between% c(15,19) & !is.na(enrolled) & !is.na(exp_now)]
m1 <- feols(enrolled ~ exp_now | prov + wave + age, data=e1, weights=~fexp, cluster=~prov)
# ---- (b) beyond-primary, women 18-22 ----
e2 <- d[age %between% c(18,22) & !is.na(beyond_prim) & !is.na(exp_at15)]
m2 <- feols(beyond_prim ~ exp_at15 | prov + wave + age, data=e2, weights=~fexp, cluster=~prov)

res <- data.table(
  outcome = c("Enrolled (women 15-19)", "Beyond primary (women 18-22)"),
  exposure = c("province intensity, survey year", "province intensity at age 15"),
  coef = c(coef(m1)[["exp_now"]], coef(m2)[["exp_at15"]]),
  se   = c(se(m1)[["exp_now"]],   se(m2)[["exp_at15"]]),
  n    = c(nobs(m1), nobs(m2)),
  mean_dep = c(round(weighted.mean(e1$enrolled, e1$fexp),3), round(weighted.mean(e2$beyond_prim, e2$fexp),3)))
res[, `:=`(coef=round(coef,4), se=round(se,4), p=round(2*pnorm(-abs(coef/se)),3))]
cat("\n=== ENHOGAR early human-capital first pass (province FE + wave FE + age FE; cluster prov) ===\n")
print(res, class=FALSE)
fwrite(res, file.path(TAB,"enhogar_educ_firstpass.csv"))
cat("saved -> enhogar_educ_firstpass.csv\n")
