# ============================================================================
# 05l_sisalril.R — SISALRIL SAIP data (SIP-C323B4FB, received 2026-07-10):
# Poblacion Afiliada al Seguro Familiar de Salud, 2010-2025 x regimen x ARS
# category x sex x PROVINCE x age group. Province is SISALRIL's stated minimum
# territorial disaggregation (their note 3).
# Builds:
#  (1) sisalril_province.rds — province-year series: female 15-19 subsidized
#      affiliates + affiliation RATE (per prov female 15-19 pop, ONE controls);
#      total (all-regime) female 15-19 affiliation rate. 2010-2025, time-varying
#      (upgrades the static DHS-2013 pct_senasa covariate).
#  (2) senasa_municipal_proxy.rds — MUNICIPAL proxy from births microdata
#      (insurance {Public/Private/None/Autogestion} + subsidized flag, ~80%
#      non-NA from 2018): muni-year shares among mothers 15-49 and 15-19, plus
#      a static 2018-19 baseline covariate. Mothers-only selection caveat.
#  (3) VALIDATION: births-based province aggregates (2019) vs SISALRIL
#      subsidized rate (2019) and vs DHS pct_senasa_2013 — correlations printed.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})
TAB <- file.path(PROJ,"output","tables")
norm <- function(x) toupper(trimws(stri_trans_general(as.character(x),"Latin-ASCII")))

# ---- (1) SISALRIL province-year ----
raw <- as.data.table(read_excel(file.path(DRPAPER,"analysis/datasets/SAIP/SIP-C323B4FB.xlsx"),
                                sheet="Tabla_1 ", skip=3))
setnames(raw, c("anio","regimen","ars","sexo","provincia","edad","afiliados"))
raw <- raw[!is.na(anio) & !is.na(provincia)]
raw[, `:=`(anio=as.integer(anio), afiliados=as.numeric(afiliados), prov_norm=norm(provincia))]
cat("rows:", nrow(raw), "| years:", raw[,min(anio)], "-", raw[,max(anio)],
    "| provinces:", raw[,uniqueN(prov_norm)], "\n")
# map SISALRIL province names -> prov_code via crosswalk (+ known aliases)
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
pmap <- unique(xw[, .(prov_norm=norm(prov_name), prov_code)])
alias <- c("BAHORUCO"="BAORUCO", "SALCEDO"="HERMANAS MIRABAL",
           "MONSENOL NOUEL"="MONSENOR NOUEL", "SANTO DOMINGO DE GUZMAN"="DISTRITO NACIONAL")
raw[prov_norm %in% names(alias), prov_norm := alias[prov_norm]]
un <- setdiff(raw[,unique(prov_norm)], pmap$prov_norm)
cat("unmatched SISALRIL province labels:", if(length(un)) paste(un, collapse=" | ") else "none", "\n")
sis <- merge(raw, pmap, by="prov_norm")                       # drops any non-province rows
f1519 <- sis[sexo=="Mujeres" & edad=="15-19"]
prov <- dcast(f1519[, .(af=sum(afiliados, na.rm=TRUE)), by=.(prov_code, anio, sub=grepl("Subsid", regimen))],
              prov_code + anio ~ sub, value.var="af", fill=0)
setnames(prov, c("FALSE","TRUE"), c("af_other","af_subsid"))
prov[, af_total := af_other + af_subsid]
# province female 15-19 population (ONE controls)
pp <- as.data.table(readRDS(file.path(DIR_CLEAN,"prov_controls_2000_2030_long.rds")))
pp <- pp[sex=="F" & age_group=="15-19"]                     # controls code sex as M/F (verified)
stopifnot(nrow(pp)==nrow(unique(pp[,.(prov_code, year)])))   # one row per province-year
setnames(pp, "year", "anio")
prov <- merge(prov, pp[, .(prov_code, anio, pop1519=pop)], by=c("prov_code","anio"), all.x=TRUE)
prov[, `:=`(senasa_rate_1519 = af_subsid/pop1519, affil_rate_1519 = af_total/pop1519)]
cat("province-year rows:", nrow(prov), "| pop matched:", prov[!is.na(pop1519),.N], "\n")
cat("national female-15-19 subsidized rate by year:\n")
print(prov[!is.na(pop1519), .(rate=round(sum(af_subsid)/sum(pop1519),3)), keyby=anio], class=FALSE)
saveRDS(prov, file.path(DIR_CLEAN,"sisalril_province.rds"))

# ---- (2) municipal proxy from births ----
fm <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
b <- b[!is.na(adm3_pcode) & birth_year >= 2018]
b[adm3_pcode %in% names(fm), adm3_pcode := fm[adm3_pcode]]
mk <- function(d) d[, .(n = .N,
     sh_public    = mean((insurance=="Public")[!is.na(insurance)]),
     sh_subsid    = mean(subsidized==1, na.rm=TRUE)), by=.(adm3_pcode, year=birth_year)]
mun_all  <- mk(b[age_mom %in% 15:49]); mun_all[,  agegrp := "15-49"]
mun_teen <- mk(b[age_mom %in% 15:19]); mun_teen[, agegrp := "15-19"]
mun <- rbind(mun_all, mun_teen)
base <- mun[year %in% 2018:2019, .(sh_public_1819 = weighted.mean(sh_public, n),
                                   sh_subsid_1819 = weighted.mean(sh_subsid, n)), by=.(adm3_pcode, agegrp)]
saveRDS(list(panel=mun, baseline=base), file.path(DIR_CLEAN,"senasa_municipal_proxy.rds"))
cat("\nmunicipal proxy: ", mun[,uniqueN(adm3_pcode)], "munis x years 2018-2025 (x2 age groups)\n")

# ---- (3) validation ----
mp <- unique(xw[, .(adm3_pcode, prov_code)])
bp <- merge(mun_all[year==2019], mp, by="adm3_pcode")
bprov <- bp[, .(sh_public_births = weighted.mean(sh_public, n)), by=prov_code]
val <- merge(bprov, prov[anio==2019, .(prov_code, senasa_rate_1519, affil_rate_1519)], by="prov_code")
dh <- as.data.table(readRDS(file.path(DIR_CLEAN,"dhs_province_baseline.rds")))
dhp <- unique(merge(dh[, .(adm3_pcode, pct_senasa_2013)], mp, by="adm3_pcode")[, .(prov_code, pct_senasa_2013)])
val <- merge(val, dhp, by="prov_code", all.x=TRUE)
cat("\n=== VALIDATION (province level, 2019) ===\n")
cat(sprintf("corr(births %% Public [mothers], SISALRIL subsidized rate f15-19): %.3f\n",
    val[, cor(sh_public_births, senasa_rate_1519, use="complete.obs")]))
cat(sprintf("corr(births %% Public [mothers], DHS pct_senasa_2013):            %.3f\n",
    val[, cor(sh_public_births, pct_senasa_2013, use="complete.obs")]))
cat(sprintf("corr(SISALRIL subsidized rate,  DHS pct_senasa_2013):            %.3f\n",
    val[, cor(senasa_rate_1519, pct_senasa_2013, use="complete.obs")]))
fwrite(val, file.path(TAB,"sisalril_validation.csv"))
cat("\nsaved -> sisalril_province.rds + senasa_municipal_proxy.rds + sisalril_validation.csv\n")
