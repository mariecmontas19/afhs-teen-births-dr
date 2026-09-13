# ============================================================================
# 09f_completeness_robustness.R — is birth-registration completeness balanced /
# unmoved by treatment? (the differential-under-registration threat, §35.x)
#   (1) empirical completeness ramp: our registered national teen rate ÷ WDI
#       (World Bank SP.ADO.TFRT, current vintage) by year -> shows 2016-18 severe
#       under-count improving to ~2019 (self-contained, no external stat needed).
#   (2) balance (treated H1 vs never) on registration proxies: % rural, % Haitian
#       (teen births), % public-facility birth.
#   (3) CS PLACEBO: does treatment SHIFT the % public-facility / % Haitian share
#       (among ALL births, muni-year)? If overall ATT approx 0, treatment does not
#       change registration composition -> no differential-undercount bias.
# Denominator A. Output: output/tables/completeness_robustness.csv
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

# ---- (1) empirical completeness ramp: registered ÷ WDI ----
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
ours <- p[, .(reg_rate=1000*sum(nb_15_19)/sum(womenA_15_19)), by=year][order(year)]
wdi <- data.table(year=2016:2024,                                   # World Bank SP.ADO.TFRT (FRED SPADOTFRTDOM), current vintage
                  wdi_rate=c(78.640,75.887,72.619,66.982,58.852,56.058,53.582,52.774,50.163))
cmp <- merge(ours, wdi, by="year", all.x=TRUE)
cmp[, completeness_pct := round(100*reg_rate/wdi_rate,0)]
cat("=== (1) registered vs WDI, implied completeness ===\n"); print(cmp[,.(year, reg=round(reg_rate,1), wdi=round(wdi_rate,1), completeness_pct)], class=FALSE)

# ---- proxies ----
br <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))[!is.na(adm3_pcode)]
mc <- br[, .(pct_pubfac = 100*mean(facility=="Public", na.rm=TRUE),
             pct_haitian= 100*mean(nationality=="Haitian", na.rm=TRUE)), by=.(adm3_pcode, year=birth_year)]  # ALL births, muni-year
cen <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_covariates_muni.rds")))[, .(adm3_pcode, pct_rural=100*(1-pct_urban))]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]  # fold excluded (126 never, not 129)
bin[, arm := fifelse(ever_treated==1L & always_treated==0L,"treated",fifelse(ever_treated==0L,"never",NA_character_))]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]

# ---- (2) balance treated vs never ----
bal <- merge(bin[!is.na(arm),.(adm3_pcode,arm)], mc[, .(pct_pubfac=mean(pct_pubfac), pct_haitian=mean(pct_haitian)), by=adm3_pcode], by="adm3_pcode")
bal <- merge(bal, cen, by="adm3_pcode", all.x=TRUE)
cat("\n=== (2) balance (municipio means) ===\n")
btab <- rbindlist(lapply(c("pct_rural","pct_haitian","pct_pubfac"), function(v){
  tt <- t.test(bal[arm=="treated"][[v]], bal[arm=="never"][[v]])
  data.table(proxy=v, treated=round(mean(bal[arm=="treated"][[v]],na.rm=T),1),
             never=round(mean(bal[arm=="never"][[v]],na.rm=T),1),
             diff=round(-diff(tt$estimate),1), p=round(tt$p.value,3)) }))
print(btab, class=FALSE)

# ---- (3) CS placebo: does treatment move the proxy? ----
d <- merge(mc, bin[!is.na(g),.(adm3_pcode,g)], by="adm3_pcode")[!adm3_pcode %in% fold]
d[, id := as.integer(factor(adm3_pcode))]
# NB: DOM030607 has zero births in 2016 (9/10 years) -> att_gt's default panel
# balancing silently drops this one never-treated control; documented, immaterial.
cat("unbalanced munis (dropped by att_gt balancing):", d[, .N, by=adm3_pcode][N<10, .N], "\n")
cspl <- function(yname){
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g", xformla=~1,
        data=d, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  gr <- suppressMessages(aggte(a, type="group", na.rm=TRUE)); dy <- suppressMessages(aggte(a, type="dynamic", na.rm=TRUE))
  data.table(outcome=yname, ATT=round(gr$overall.att,3), SE=round(gr$overall.se,3),
             p=round(2*pnorm(-abs(gr$overall.att/gr$overall.se)),3), pretrend_p=round(es_pretrend_p(dy)$p,3)) }
cat("\n=== (3) CS placebo — treatment effect on registration proxies (want approx 0) ===\n")
plc <- rbind(cspl("pct_pubfac"), cspl("pct_haitian")); print(plc, class=FALSE)

fwrite(cmp, file.path(TAB,"completeness_vs_wdi.csv"))
fwrite(rbind(btab[,.(kind="balance",metric=proxy,val1=treated,val2=never,stat=diff,p)],
             plc[,.(kind="cs_placebo",metric=outcome,val1=ATT,val2=SE,stat=NA_real_,p)]), file.path(TAB,"completeness_robustness.csv"))
cat("\nsaved -> completeness_vs_wdi.csv + completeness_robustness.csv\n")
