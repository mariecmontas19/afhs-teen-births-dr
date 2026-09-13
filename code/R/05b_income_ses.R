# ============================================================================
# 05b_income_ses.R — COARSE province-level SES robustness covariate from ENGIH 2018.
# The ENGIH "Base" has no ready continuous-income column; the usable SES signal is
# QUINTIL (national income quintile 1-5). We compute the province-weighted share of
# HOUSEHOLDS in the bottom-2 quintiles (a poverty proxy) and broadcast to municipios.
# This is SECONDARY to the census municipio wealth_index (05) — province-level only,
# ~32 distinct values, near-zero within-province variation. Robustness use only.
# FACTOR_EXPANSION & QUINTIL verified constant within household -> proper HH weight.
# Output: data/clean/income_ses_province.rds (158 municipios, broadcast province value).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl)})

d <- as.data.table(read_excel(RAW$income_xlsx, sheet="Base"))
d <- d[, .(UPM, VIVIENDA, HOGAR, ID_PROVINCIA, QUINTIL, FACTOR_EXPANSION)]
hh <- unique(d, by=c("UPM","VIVIENDA","HOGAR"))                  # one row per household (QUINTIL/weight const within HH)
cat("households:", nrow(hh), "| QUINTIL 1-5:", all(hh$QUINTIL %in% 1:5), "\n")

# province weighted % of households in bottom-2 quintiles (Q1+Q2 = poorer 40%)
prov <- hh[, .(pct_bottom2q_2018 = round(weighted.mean(QUINTIL<=2, FACTOR_EXPANSION), 4),
               mean_quintile_2018 = round(weighted.mean(QUINTIL, FACTOR_EXPANSION), 3),
               n_hh = .N), by=.(prov_code = ID_PROVINCIA)]
cat("provinces:", nrow(prov), "(expect 32)\n")

# broadcast to 158 municipios via crosswalk prov_code (ONE province code 1-32)
xw <- as.data.table(readRDS(file.path(DIR_CLEAN, "muni_crosswalk.rds")))
xw[, prov_code := as.integer(prov_code)]
cov <- merge(xw[, .(adm3_pcode, prov_norm, prov_code)], prov, by="prov_code", all.x=TRUE)
stopifnot(nrow(cov)==158, !anyNA(cov$pct_bottom2q_2018))        # Rule 7: all municipios get a province value
saveRDS(cov[, .(adm3_pcode, prov_code, pct_bottom2q_2018, mean_quintile_2018)],
        file.path(DIR_CLEAN, "income_ses_province.rds"))

# ---- validation ----
cat("\n==================== 05b ENGIH PROVINCE SES VALIDATION ====================\n")
cat("municipios covered:", nrow(cov), " | provinces with SES:", uniqueN(cov$prov_code), "\n")
cat("national weighted % HH bottom-2 quintiles:", round(100*hh[, weighted.mean(QUINTIL<=2, FACTOR_EXPANSION)],1),
    "% (expect ~40% by construction of national quintiles)\n")
cat("province % bottom-2 quintiles: range",
    paste(round(range(prov$pct_bottom2q_2018),3), collapse="-"), "| median", round(median(prov$pct_bottom2q_2018),3), "\n")
cat("poorest & richest provinces (by % bottom-2q):\n")
print(merge(prov, unique(xw[,.(prov_code, prov_norm)]), by="prov_code")[order(-pct_bottom2q_2018)][c(1:3, 30:32),
      .(prov_norm, pct_bottom2q_2018, mean_quintile_2018, n_hh)])
cat("saved -> data/clean/income_ses_province.rds\n")
