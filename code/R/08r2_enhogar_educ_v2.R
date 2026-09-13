# ============================================================================
# 08r2_enhogar_educ_v2.R — R8 second pass, after MM's two asks (2026-07-10):
# (1) YEARS OF EDUCATION as an outcome (08r's "enrolled" = currently attending
#     ANY level, H302/P302==1). Years built from nivel+curso; BOTH waves verified
#     to use the OLD structure (basica curso 1-8, media 1-4) -> single mapping:
#     nivel 1/never=0; nivel 2=curso; nivel 3=8+curso; nivel 4=12+curso;
#     nivel 5=16+curso. curso 98/99 and nivel 8/9 -> NA.
# (2) BETTER VALIDATION than the contemporaneous-intensity spec (whose exp_now
#     is collinear with province-x-wave FE): a COHORT-EXPOSURE design.
#     exp_at15 = province AU intensity in the year the person turned 15 varies
#     WITHIN province-x-wave across single years of age -> identified with
#     prov^wave FE (absorbs ALL province-year shocks, incl. COVID recovery)
#     + age FE (absorbs national cohort effects). Branson-Byker cohort analog.
#     NOTE (§59.6): men are NOT a placebo — AUs serve both sexes 10-19. The
#     female interaction = fertility-mediated differential channel only.
# PREDICTIONS (rule 1b), stated before running:
#   A naive years spec (exp_now, prov+wave+age FE): positive echo of the +14pp
#     enrollment artifact, ~+0.3-0.6 yrs, NOT to be believed.
#   B/C cohort specs (exp_at15, prov^wave FE): ~null for years (attainment
#     lags exposure) and small-or-null for enrollment.
#   D female x exp_at15 differential: null (08r's differential was null).
# Output: output/tables/enhogar_educ_v2.csv
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
d <- rbind(w18, w22)[sex %in% 1:2 & age %between% c(15,22)]
d[, female := as.integer(sex==2)]
d[asiste %in% 9, asiste := NA]
d[, enrolled := fifelse(!is.na(asiste), as.integer(asiste==1), NA_integer_)]
d[nivel %in% c(8,9), nivel := NA]
d[curso %in% c(98,99), curso := NA]
d[, yrs := fcase(asiste==3, 0,                       # never attended
                 nivel==1, 0,
                 nivel==2, pmin(curso, 8),
                 nivel==3, 8 + pmin(curso, 4),
                 nivel==4, 12 + pmin(curso, 8),
                 nivel==5, 16 + pmin(curso, 4))]
cat("yrs built: non-NA", d[!is.na(yrs),.N], "of", nrow(d),
    "| weighted mean by age (women):\n")
print(d[female==1 & !is.na(yrs) & !is.na(fexp),
        .(yrs=round(weighted.mean(yrs, fexp),2)), keyby=age], class=FALSE)

# ---- exposures ----
ip <- as.data.table(readRDS(file.path(DIR_CLEAN,"province_au_intensity.rds")))
d <- merge(d, ip[, .(prov=prov_code, wave=year, exp_now=intensity)], by=c("prov","wave"), all.x=TRUE)
d[, yr_at15 := wave - (age - 15L)]
d <- merge(d, ip[, .(prov=prov_code, yr_at15=year, exp_at15=intensity)], by=c("prov","yr_at15"), all.x=TRUE)
d[yr_at15 < 2016, exp_at15 := 0]
cat("exp merge NAs: exp_now", d[is.na(exp_now),.N], "| exp_at15", d[is.na(exp_at15),.N], "\n")

W  <- d[female==1 & age %between% c(15,19)]
WB <- d[age %between% c(15,19)]                       # both sexes, for interaction
W2 <- d[female==1 & age %between% c(18,22)]

mA  <- feols(yrs      ~ exp_now         | prov + wave + age, W,  weights=~fexp, cluster=~prov)
mB  <- feols(yrs      ~ exp_at15        | prov^wave + age,   W,  weights=~fexp, cluster=~prov)
mC  <- feols(enrolled ~ exp_at15        | prov^wave + age,   W,  weights=~fexp, cluster=~prov)
mD  <- feols(enrolled ~ exp_at15*female | prov^wave + age,   WB, weights=~fexp, cluster=~prov)
mDy <- feols(yrs      ~ exp_at15*female | prov^wave + age,   WB, weights=~fexp, cluster=~prov)
mB2 <- feols(yrs      ~ exp_at15        | prov^wave + age,   W2, weights=~fexp, cluster=~prov)

grab <- function(m, v, lab, samp){
  ct <- coeftable(m); i <- grep(v, rownames(ct), fixed=TRUE)[1]
  data.table(spec=lab, sample=samp, term=rownames(ct)[i],
             coef=round(ct[i,1],4), se=round(ct[i,2],4), p=round(ct[i,4],3), n=nobs(m))
}
res <- rbind(
  grab(mA, "exp_now",         "A naive: years ~ intensity now (prov+wave+age FE) [artifact-prone]", "women 15-19"),
  grab(mB, "exp_at15",        "B cohort: years ~ intensity at 15 (prov x wave FE)",                 "women 15-19"),
  grab(mB2,"exp_at15",        "B2 cohort: years ~ intensity at 15 (prov x wave FE)",                "women 18-22"),
  grab(mC, "exp_at15",        "C cohort: enrolled ~ intensity at 15 (prov x wave FE)",              "women 15-19"),
  grab(mD, "exp_at15:female", "D differential: enrolled, female x intensity at 15 (prov x wave FE)","both 15-19"),
  grab(mDy,"exp_at15:female", "Dy differential: years, female x intensity at 15 (prov x wave FE)",  "both 15-19"))
cat("\n=== ENHOGAR education v2 (cluster prov, weights fexp) ===\n")
print(res, class=FALSE)
fwrite(res, file.path(TAB,"enhogar_educ_v2.csv"))
cat("saved -> enhogar_educ_v2.csv\n")
