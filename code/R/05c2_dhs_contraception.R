# ============================================================================
# 05c2_dhs_contraception.R — ENDESA-2013 (DHS) PROVINCE-level contraception
# baseline, broadcast to municipios, as a supply-side MECHANISM moderator.
# Computed by us from the individual recode DRIR61DT/DRIR61FL.DTA (in-union women;
# weight v005/1e6). Indicators (ALL-AGES in-union — teen-by-province is too thin):
#   unmet_2013     — unmet need for FP (v626a in {1,2})
#   mcpr_2013      — modern contraceptive prevalence (v313==3)
#   satmod_2013    — satisfied demand, modern = modern users / total demand
# The 2013 ENDESA is stratified by province x urban/rural (63 strata, 524 PSUs) ->
# province IS a design domain, so province estimates are valid (small provinces
# noisy: 5/32 have <60 in-union women). 2013 = 8 yrs PRE-rollout -> clean
# time-invariant baseline (cannot be treatment-contaminated).
# CAVEATS: all-ages (NOT teen-specific); 32-province granularity broadcast to 155
# municipios (coarse); small-province noise. Output: dhs_contraception_province.rds.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(haven); library(stringi)})
nm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
prov_alias <- c("BAHORUCO"="BAORUCO","MONSENOL NOUEL"="MONSENOR NOUEL","SALCEDO"="HERMANAS MIRABAL")
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155<- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)

ir <- as.data.table(read_dta(file.path(DRPAPER,"analysis/datasets/DHS/DRIR61DT/DRIR61FL.DTA"),
        col_select=c(v005,v502,v626a,v313,sprovin)))
ir[, w := v005/1e6][, prov := nm(as_factor(sprovin))][prov %in% names(prov_alias), prov := prov_alias[prov]]
u <- ir[v502==1]                                          # currently in-union women, all ages
u[, `:=`(unmet=as.integer(v626a %in% c(1,2)), met_any=as.integer(v626a %in% c(3,4)),
         demand=as.integer(v626a %in% c(1,2,3,4)), mod=as.integer(v313==3))]
ws <- function(x,wt) sum(x*wt, na.rm=TRUE)
prov_tab <- u[, .(n_women=.N,
   unmet_2013  = round(100*ws(unmet,w)/ws(rep(1,.N),w),1),    # unmet need (high = underserved)
   cpr_any_2013= round(100*ws(met_any,w)/ws(rep(1,.N),w),1),  # CPR, any method using (low = underserved)
   mcpr_2013   = round(100*ws(mod,w)/ws(rep(1,.N),w),1),      # modern CPR
   satany_2013 = round(100*ws(met_any,w)/ws(demand,w),1),     # satisfied demand, any (low = underserved)
   satmod_2013 = round(100*ws(mod,w)/ws(demand,w),1)), by=prov]

cat("=== national sanity (in-union, all ages) ===\n")
cat(sprintf("  unmet %.1f%% (pub ~11%%) | mCPR %.1f%% | satisfied-demand-modern %.1f%%\n",
    100*ws(u$unmet,u$w)/ws(rep(1,nrow(u)),u$w), 100*ws(u$mod,u$w)/ws(rep(1,nrow(u)),u$w),
    100*ws(u$mod,u$w)/ws(u$demand,u$w)))
cat("provinces:", nrow(prov_tab), "(expect 32) | small (n<60):", prov_tab[n_women<60,.N], "\n")

# ---- broadcast province -> municipio (folded 155) ---------------------------
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[, .(adm3_pcode, prov=nm(prov_name))]
out <- merge(xw, prov_tab, by="prov", all.x=TRUE)
cat("\nmunicipios:", nrow(out), "| unmatched province (NA covariate):", out[is.na(unmet_2013),.N], "\n")
if(out[is.na(unmet_2013),.N]>0) print(unique(out[is.na(unmet_2013), .(prov)]))
stopifnot(out[is.na(unmet_2013),.N]==0)
out[, adm3_pcode := to155(adm3_pcode)]
out <- unique(out, by="adm3_pcode")                       # 3 folded muni collapse (same province value)
stopifnot(uniqueN(out$adm3_pcode)<=155)
saveRDS(out[, .(adm3_pcode, prov, n_women, unmet_2013, cpr_any_2013, mcpr_2013, satany_2013, satmod_2013)],
        file.path(DIR_CLEAN,"dhs_contraception_province.rds"))

cat("\n=== province values (sorted by unmet need) ===\n")
print(prov_tab[order(-unmet_2013), .(prov, n_women, unmet_2013, mcpr_2013, satmod_2013)][c(1:5, 28:32)], class=FALSE)
cat("\nsaved -> data/clean/dhs_contraception_province.rds (", nrow(out), "municipios)\n", sep="")
