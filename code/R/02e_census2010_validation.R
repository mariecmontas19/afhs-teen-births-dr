# ============================================================================
# 02e_census2010_validation.R — validate the 2022-census SES moderators by
# recomputing the SAME variable from the 2010 census and correlating at municipio
# level. If the municipio ranking is stable (high r), the 2022 census (which is
# mid-rollout for early cohorts) is a defensible baseline. [MM 2026-06-28]
# Education matches the 2022 definition EXACTLY: women 20-49, highest level
# attended >= secondary. 2010 P37_NIVEL (1 Preprimaria, 2 Primaria, 3 Secundaria,
# 4 Universitaria) -> sec+ = P37_NIVEL>=3; SEXO 2 = women; P29 = age.
# (Urban: NO zone field in the 2010 PERSONAS microdata -> not computable here.
#  Wealth: would need the 2010 HOGARES asset PCA -> separate build.)
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table)})

c10 <- fread(file.path(DRPAPER,"analysis/datasets/population/CENSO2010RD-PERSONAS.csv"),
             select=c("PROVINCIA","MUNICIPIO","P27_SEXO","P29_EDAD_ANOS_CUMPLIDOS","P37_NIVEL"))
cat("2010 PERSONAS rows:", nrow(c10), "| P37_NIVEL values:", paste(sort(unique(c10$P37_NIVEL)),collapse=","), "\n")
w <- c10[P27_SEXO==2L & P29_EDAD_ANOS_CUMPLIDOS>=20 & P29_EDAD_ANOS_CUMPLIDOS<=49]
e10 <- w[!is.na(P37_NIVEL) & P37_NIVEL %in% 1:4,
         .(educ_2010=mean(P37_NIVEL>=3), n_w2049_2010=.N), by=.(PROVINCIA, MUNICIPIO)]

# map 2010 ONE codes -> adm3_pcode (pcode encodes ONE prov/muni: substr 6-7 / 8-9)
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
xw[, `:=`(PROVINCIA=as.integer(substr(adm3_pcode,6,7)), MUNICIPIO=as.integer(substr(adm3_pcode,8,9)))]
e10 <- merge(e10, xw[, .(adm3_pcode, PROVINCIA, MUNICIPIO)], by=c("PROVINCIA","MUNICIPIO"), all.x=TRUE)
cat("2010 muni rows:", nrow(e10), "| unmapped to adm3:", e10[is.na(adm3_pcode),.N], "\n")
if(e10[is.na(adm3_pcode),.N]>0) print(e10[is.na(adm3_pcode), .(PROVINCIA,MUNICIPIO,n_w2049_2010)])

c22 <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_covariates_muni.rds")))[, .(adm3_pcode, educ_2022=pct_educ_secplus)]
M <- merge(e10[!is.na(adm3_pcode)], c22, by="adm3_pcode")
cat("matched municipios (2010 ∩ 2022):", nrow(M), "/ 158\n\n")
cat("=== % women 20-49 with secondary+ : 2010 vs 2022 ===\n")
cat(sprintf("  national (unwtd muni mean): 2010 %.1f%%  ->  2022 %.1f%%\n", 100*mean(M$educ_2010), 100*mean(M$educ_2022)))
cat(sprintf("  Pearson  r = %.3f\n", cor(M$educ_2010, M$educ_2022)))
cat(sprintf("  Spearman rank r = %.3f  <- municipio RANKING stability (the key check)\n", cor(M$educ_2010, M$educ_2022, method="spearman")))
saveRDS(M, file.path(DIR_CLEAN,"census2010_vs_2022_educ.rds"))
cat("\nsaved -> data/clean/census2010_vs_2022_educ.rds\n")
