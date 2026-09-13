# ============================================================================
# 05c_dhs_province.R — DHS (ENDESA) 2013 PROVINCE-level baseline covariates,
# broadcast to municipios (province value assigned to each municipio — coarse,
# like the ENGIH SES covariate). 2013 = single cross-section, 8 yrs PRE-rollout
# -> legitimate time-invariant BASELINE covariate (cannot be treatment-contaminated).
# From the births recode DRBR61FL.DTA (province = sprovin; weight = v005/1e6):
#   pct_insured_2013   — % women covered by health insurance (v481), WOMAN-level
#   pct_senasa_2013    — % women covered by SENASA subsidized regime (v481e), WOMAN-level
#   pct_pubfac_2013    — % facility deliveries in PUBLIC facilities (m15), BIRTH-level
#   pct_instdeliv_2013 — % institutional (facility) deliveries (m15), BIRTH-level
# CAVEATS: province-level (32) broadcast to 155/158 municipios (coarse); facility
# rests on ~3,710 recent births (~115/prov, noisy). DHS used for PROVINCE AGGREGATES
# ONLY (not GPS/distance — per the DHS data-use email). Output: dhs_province_baseline.rds.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(haven); library(stringi)})
nm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
prov_alias <- c("BAHORUCO"="BAORUCO","MONSENOL NOUEL"="MONSENOR NOUEL","SALCEDO"="HERMANAS MIRABAL")

dhs <- file.path(DRPAPER,"analysis/datasets/DR_2013_DHS_06222026_1212_212317/DRBR61DT/DRBR61FL.DTA")
br <- as.data.table(read_dta(dhs, col_select=c(v001,v002,v003,v005,sprovin,v481,v481e,m15,m17,v008,v011,b3)))
prpath <- file.path(dirname(dirname(dhs)),"DRPR61DT","DRPR61FL.DTA")   # PR roster (women 15-19 denominator)
br[, w := v005/1e6]
br[, prov := nm(as_factor(sprovin))][prov %in% names(prov_alias), prov := prov_alias[prov]]

# facility type from m15 labels (robust to numeric codes)
br[, m15lab := tolower(as.character(as_factor(m15)))]
br[, fac := fcase(grepl("home", m15lab), "home",
                  grepl("private|profamilia", m15lab), "private",
                  grepl("public|government|military|idss", m15lab), "public",
                  !is.na(m15lab) & m15lab!="", "other", default=NA_character_)]

wmean <- function(x,w){ ok <- !is.na(x) & !is.na(w); if(!any(ok)) return(NA_real_); sum(x[ok]*w[ok])/sum(w[ok]) }
# insurance = WOMAN-level (v481/v481e repeat across a woman's births) -> dedup
wm <- unique(br, by=c("v001","v002","v003"))
ins <- wm[, .(pct_insured_2013=round(wmean(v481,w),4), pct_senasa_2013=round(wmean(v481e,w),4),
              n_women=.N), by=prov]
# facility = BIRTH-level among m15-observed
fac <- br[!is.na(fac), .(pct_instdeliv_2013=round(wmean(as.integer(fac %in% c("public","private")), w),4),
                         n_m15=.N), by=prov]
facp <- br[fac %in% c("public","private"), .(pct_pubfac_2013=round(wmean(as.integer(fac=="public"), w),4)), by=prov]
# c-section = BIRTH-level among recent births (m17 observed) — DHS 2013 BASELINE (pre-rollout; legit covariate)
br[, cs := fcase(tolower(as.character(as_factor(m17)))=="yes",1L, tolower(as.character(as_factor(m17)))=="no",0L, default=NA_integer_)]
csec <- br[!is.na(cs), .(pct_csection_2013=round(wmean(cs,w),4), n_m17=.N), by=prov]
# teen ASFR 2013 (province) — INDEPENDENT teen-specific baseline fertility: BR teen births (15-19 at birth,
# last 36 mo, annualized) / PR women 15-19. PROXY (national ~79 vs known ~90; small-province noise -> ME attenuates).
br[, agebirth := (b3 - v011)/12]
tnum <- br[b3 >= v008-36 & b3 < v008 & agebirth>=15 & agebirth<20, .(births=sum(w)/3), by=prov]
prw <- as.data.table(read_dta(prpath, col_select=c(hv005,hv104,hv105,shprovin)))
prw[, prov := nm(as_factor(shprovin))][prov %in% names(prov_alias), prov := prov_alias[prov]]
tden <- prw[hv104==2 & hv105>=15 & hv105<=19, .(women=sum(hv005/1e6)), by=prov]
tasfr <- merge(tnum, tden, by="prov")[, .(prov, dhs_teenasfr_2013=round(1000*births/women,1))]
prov_tab <- Reduce(function(a,b) merge(a,b,by="prov",all=TRUE), list(ins, fac, facp, csec, tasfr))

cat("=== national (weighted) sanity check ===\n")
cat(sprintf("  insured %.3f | SENASA %.3f | institutional %.3f | public-among-facility %.3f | c-section %.3f\n",
    wmean(wm$v481,wm$w), wmean(wm$v481e,wm$w),
    wmean(as.integer(br[!is.na(fac)]$fac %in% c("public","private")), br[!is.na(fac)]$w),
    wmean(as.integer(br[fac %in% c("public","private")]$fac=="public"), br[fac %in% c("public","private")]$w),
    wmean(br[!is.na(cs)]$cs, br[!is.na(cs)]$w)))
cat("provinces in DHS table:", nrow(prov_tab), "(expect 32)\n")

# broadcast to municipios via normalized province name
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
xw[, prov := nm(prov_norm)]
out <- merge(xw[, .(adm3_pcode, adm3_name, prov)], prov_tab, by="prov", all.x=TRUE)
cat("\nmunicipios:", nrow(out), "| any prov unmatched (NA covariates):", out[is.na(pct_insured_2013), .N], "\n")
stopifnot(nrow(out)==nrow(xw), out[is.na(pct_insured_2013), .N]==0)
saveRDS(out[, .(adm3_pcode, prov, pct_insured_2013, pct_senasa_2013, pct_pubfac_2013, pct_instdeliv_2013,
                pct_csection_2013, dhs_teenasfr_2013)],
        file.path(DIR_CLEAN,"dhs_province_baseline.rds"))
cat("\n=== province values (head) ===\n")
print(prov_tab[order(prov)][1:8, .(prov, pct_insured_2013, pct_senasa_2013, pct_pubfac_2013, pct_instdeliv_2013, n_women, n_m15)], class=FALSE)
cat("\nsaved -> data/clean/dhs_province_baseline.rds (", nrow(out), "municipios)\n", sep="")
