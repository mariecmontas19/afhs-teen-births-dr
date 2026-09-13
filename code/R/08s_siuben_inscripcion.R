# ============================================================================
# 08s_siuben_inscripcion.R — SIUBEN 2025 school inscription by municipio
# (Poverty/INSCRIPCION_MUNICIPIOS.csv; dictionary-verified: Si_insc/No_insc =
# persons AGED 3-17 in SIUBEN-registered households inscribed / not inscribed
# in an educational center; Porc_noins = % not inscribed). ONE CROSS-SECTION
# (SIUBEN 2025) -> NO DiD possible; descriptive treated-vs-control only, and
# ages 3-17 dilute the 15-19 margin heavily.
# PREDICTION (rule 1b): raw = treated municipios LOWER non-inscription (they
# are larger/more urban/richer); conditional on wealth/urbanization -> ~null.
# Output: output/tables/siuben_inscripcion.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(fixest); library(stringi)})
TAB <- file.path(PROJ,"output","tables")
norm <- function(x) toupper(trimws(stri_trans_general(as.character(x),"Latin-ASCII")))

si <- fread(file.path(DRPAPER,"analysis/datasets/Poverty/INSCRIPCION_MUNICIPIOS.csv"))
si[, `:=`(prov_norm=norm(DES_PROV), muni_norm=norm(DES_MUNI))]
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
xw[, `:=`(prov_norm=norm(prov_name), muni_norm=norm(adm3_name))]
m <- merge(si, unique(xw[, .(prov_norm, muni_norm, adm3_pcode)]), by=c("prov_norm","muni_norm"), all.x=TRUE)
cat("SIUBEN rows:", nrow(si), "| matched to pcode:", m[!is.na(adm3_pcode),.N], "\n")
if(m[is.na(adm3_pcode),.N]) print(m[is.na(adm3_pcode), .(prov_norm, muni_norm)], class=FALSE)

tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
cv <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_covariates_muni.rds")))
a <- merge(m[!is.na(adm3_pcode)], tr[, .(adm3_pcode, ever_treated, always_treated)], by="adm3_pcode")
a <- merge(a, cv[, .(adm3_pcode, wealth_index, pct_urban, n_households)], by="adm3_pcode", all.x=TRUE)
a[, treated := as.integer(ever_treated==1 | always_treated==1)]
cat("analysis rows:", nrow(a), "| treated:", a[treated==1,.N], "\n")

cat("\nweighted mean %% not inscribed (weights = Si+No):\n")
print(a[, .(pct_noins = round(weighted.mean(Porc_noins, Si_insc+No_insc),2), n=.N), by=treated], class=FALSE)
m1 <- feols(Porc_noins ~ treated, a, cluster=~prov_norm)
m2 <- feols(Porc_noins ~ treated + wealth_index + pct_urban + log(n_households), a, cluster=~prov_norm)
res <- data.table(spec=c("raw","+ wealth, urban, log households"),
                  coef=round(c(coef(m1)["treated"], coef(m2)["treated"]),3),
                  se=round(c(se(m1)["treated"], se(m2)["treated"]),3),
                  p=round(c(pvalue(m1)["treated"], pvalue(m2)["treated"]),3),
                  n=c(nobs(m1), nobs(m2)))
cat("\n=== SIUBEN 2025: Porc_noins (ages 3-17, SIUBEN households) on treated ===\n")
print(res, class=FALSE)
fwrite(res, file.path(TAB,"siuben_inscripcion.csv"))
cat("saved -> siuben_inscripcion.csv\n")
