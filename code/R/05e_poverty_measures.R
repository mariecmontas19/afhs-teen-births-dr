# ============================================================================
# 05e_poverty_measures.R — build SIUBEN ICV municipio poverty measure and COMPARE
# the three poverty/SES measures we have, to see if they agree:
#   (1) census wealth_index (2022 PCA, higher = wealthier)
#   (2) ENIGH pct_bottom2q_2018 (province, % in bottom-2 income quintiles, higher = poorer)
#   (3) SIUBEN ICV pct_poor (2025, % households in ICV-1&2 [extreme+moderate poverty])
# ICV from ICV_BARRIOS.csv (12,545 barrios) aggregated to municipio via ONE codes.
# *** All are CROSS-SECTIONAL/baseline (census 2022, ENIGH 2018, SIUBEN 2025). ***
# Output: data/clean/icv_poverty_muni.rds (158)
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table)})
POV <- file.path(dirname(RAW$pcua_xlsx), "Poverty")

# ---- SIUBEN ICV barrios -> municipio % poor (ICV-1 & ICV-2) ----
icv <- fread(file.path(POV,"ICV_BARRIOS.csv"))
xw  <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
xw[, `:=`(pc_prov=as.integer(substr(adm3_pcode,6,7)), pc_muni=as.integer(substr(adm3_pcode,8,9)))]
icvm <- icv[, .(h1=sum(ICV_1,na.rm=TRUE), h2=sum(ICV_2,na.rm=TRUE),
                h3=sum(ICV_3,na.rm=TRUE), h4=sum(ICV_4,na.rm=TRUE)), by=.(COD_PROV, COD_MUNI)]
icvm[, pct_poor_icv := round(100*(h1+h2)/(h1+h2+h3+h4),2)]
icvm <- merge(icvm, xw[, .(pc_prov, pc_muni, adm3_pcode)],
              by.x=c("COD_PROV","COD_MUNI"), by.y=c("pc_prov","pc_muni"), all.x=TRUE)
cat("ICV: barrios", nrow(icv), "-> municipios", nrow(icvm), "| unmatched to adm3:", icvm[is.na(adm3_pcode),.N], "\n")
stopifnot(icvm[is.na(adm3_pcode),.N]==0, uniqueN(icvm$adm3_pcode)==nrow(icvm))
saveRDS(icvm[, .(adm3_pcode, pct_poor_icv, n_hh=h1+h2+h3+h4)], file.path(DIR_CLEAN,"icv_poverty_muni.rds"))

# ---- assemble the three measures at municipio level ----
cw  <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_covariates_muni.rds")))[, .(adm3_pcode, wealth_index)]
enf <- as.data.table(readRDS(file.path(DIR_CLEAN,"income_ses_province.rds")))[, .(adm3_pcode, pct_bottom2q_2018)]
M <- Reduce(function(a,b) merge(a,b,by="adm3_pcode"), list(icvm[, .(adm3_pcode, pct_poor_icv)], cw, enf))
cat("merged municipios:", nrow(M), "| any NA:", sum(is.na(M)), "\n")

# ---- correlations (Pearson + Spearman) ----
cat("\n==================== POVERTY/SES MEASURE COMPARISON (n=", nrow(M), " municipios) ====================\n", sep="")
pr <- function(a,b,lab){ cat(sprintf("%-46s Pearson r=%+.3f | Spearman rho=%+.3f\n", lab,
        cor(M[[a]],M[[b]]), cor(M[[a]],M[[b]],method="spearman"))) }
cat("(wealth_index: higher=richer; pct_poor_icv & pct_bottom2q_2018: higher=poorer)\n\n")
pr("wealth_index","pct_poor_icv",        "census wealth  vs  SIUBEN ICV %poor  (expect NEG)")
pr("wealth_index","pct_bottom2q_2018",   "census wealth  vs  ENIGH %bottom-2q  (expect NEG)")
pr("pct_poor_icv","pct_bottom2q_2018",   "SIUBEN ICV %poor vs ENIGH %bottom-2q (expect POS)")
cat("\nranges: wealth_index [", paste(round(range(M$wealth_index),2),collapse=", "), "] | ",
    "ICV %poor [", paste(round(range(M$pct_poor_icv),1),collapse=", "), "] | ",
    "ENIGH %bot2q [", paste(round(range(M$pct_bottom2q_2018),1),collapse=", "), "]\n", sep="")
cat("\nNote: ENIGH is PROVINCE-level (broadcast to municipios) -> coarse; census & ICV are true municipio.\n")
cat("saved -> data/clean/icv_poverty_muni.rds (", nrow(icvm), ")\n", sep="")
