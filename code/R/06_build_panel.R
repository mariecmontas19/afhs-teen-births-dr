# ============================================================================
# 06_build_panel.R — FROZEN municipio x year analysis panel.
# Builds age-specific birth RATES (births / women x 1,000) = the outcomes:
#   PRIMARY  teen rate 15-19 ; SECONDARY (thin) 10-14 ; comparison 30-34 (+20-24,25-29)
#   age-DDD  = rate(15-19) - rate(30-34)   [nets out municipio x year shocks]
# under denominators A (ONE,155) & B (intercensal,155) [headline], and census-anchored
# (158) [robustness]. Births folded San Víctor->Moca / Matanzas->Baní / Baitoa->Santiago
# for the 155 geography (02d fold). Merges treatment (04) + census covariates (05/05b) +
# the insurance carry-back (2018->2016/17 municipio-year, built here from births).
# Outputs: panel_muni_year_155.rds (headline, A&B) + panel_muni_year_158.rds (census) +
#          panel_age_long_155.rds (flexible age-group long form).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table)})

AGES <- c("10-14","15-19","20-24","25-29","30-34","35-39","40-44")  # full reproductive span (births "45+" excluded: not separable into 5-yr bands, ~0 fertility)
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)

b   <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
A   <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))
B   <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_B.rds")))
CEN <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025.rds")))
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
cov <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_covariates_muni.rds")))
inc <- as.data.table(readRDS(file.path(DIR_CLEAN,"income_ses_province.rds")))
dhs <- as.data.table(readRDS(file.path(DIR_CLEAN,"dhs_province_baseline.rds")))   # DHS 2013 province baseline (05c)

bb <- b[!is.na(adm3_pcode) & !is.na(birth_year) & age_grp %in% AGES]
bb[, `:=`(ag=as.character(age_grp), year=birth_year)]

# ---------- helper: build a balanced age-group long panel (geog x year x age) ----------
build_long <- function(births_id, denomF, geog){   # births_id: data with cols id,year,ag ; denomF: id,year,ag,women
  cnt <- births_id[, .(n_births=.N), by=.(id, year, ag)]
  L <- merge(denomF, cnt, by=c("id","year","ag"), all.x=TRUE)          # denom frame is balanced
  L[is.na(n_births), n_births := 0L]
  L[, rate := 1000*n_births/women]
  L[]
}
denomF <- function(P) P[sex=="F" & age_group %in% AGES, .(id=adm3_pcode, year, ag=age_group, women=pop)]

# ---------- 155 geography: ALL THREE denominators A, B, census-anchored (all folded to 155) ----------
# census-anchored folded to 155 (the 3 created-2013 children -> parents; parents have all 10 years).
CEN155 <- copy(CEN)[, adm3_pcode := to155(adm3_pcode)][, .(pop=sum(pop)), by=.(adm3_pcode,sex,age_group,year)]
bb155 <- copy(bb)[, id := to155(adm3_pcode)]
LA <- build_long(bb155, denomF(A),      "155")[, .(id,year,ag,n_births,womenA=women,rateA=rate)]
LB <- build_long(bb155, denomF(B),      "155")[, .(id,year,ag,womenB=women,rateB=rate)]
LC <- build_long(bb155, denomF(CEN155), "155")[, .(id,year,ag,womenC=women,rateC=rate)]
long155 <- merge(merge(LA, LB, by=c("id","year","ag")), LC, by=c("id","year","ag"))
stopifnot(nrow(long155)==155*10*length(AGES))                          # balanced 7,750
saveRDS(long155, file.path(DIR_CLEAN,"panel_age_long_155.rds"))

# wide municipio-year: counts, women, rates for the key ages + age-DDD
w <- dcast(long155, id+year ~ ag, value.var=c("n_births","rateA","rateB","rateC","womenA","womenB","womenC"))
setnames(w, "id", "adm3_pcode")
ren <- function(a) gsub("-","_",a)
for(a in AGES) for(pre in c("rateA","rateB","rateC","womenA","womenB","womenC","n_births")){
  newpre <- if (pre=="n_births") "nb" else pre
  setnames(w, paste0(pre,"_",a), paste0(newpre,"_",ren(a)))
}
w[, `:=`(ddd_A = rateA_15_19 - rateA_30_34, ddd_B = rateB_15_19 - rateB_30_34,
         ddd_C = rateC_15_19 - rateC_30_34)]

# ---------- teen (15-19) at-birth health, municipio-year (155) ----------
th <- bb155[age_grp=="15-19", .(
        n_teen=.N, n_lbw=sum(low_bw==1,na.rm=TRUE), n_neo=sum(neo_any==1,na.rm=TRUE),
        lbw=mean(low_bw,na.rm=TRUE), preterm=mean(preterm,na.rm=TRUE), sga=mean(sga,na.rm=TRUE),
        csection=mean(csection,na.rm=TRUE), neo_any=mean(neo_any,na.rm=TRUE)), by=.(adm3_pcode=id, year)]
w <- merge(w, th, by=c("adm3_pcode","year"), all.x=TRUE)
w[is.na(n_lbw), n_lbw := 0L]; w[is.na(n_neo), n_neo := 0L]
w[, lbw_per1000women := 1000*n_lbw/womenA_15_19]      # LBW teen births per 1,000 women 15-19 (per-population, escapes selection)
w[, neo_per1000women := 1000*n_neo/womenA_15_19]      # neonatal-risk teen births per 1,000 women 15-19 (per-population)

# ---------- insurance carry-back (municipio-year, 155): % public / % insured among births ----------
ins_real <- bb155[!is.na(insurance), .(pct_public=mean(insurance=="Public"),
                                       pct_insured=mean(has_insurance==1L, na.rm=TRUE)), by=.(adm3_pcode=id, year)]
ins18 <- ins_real[year==2018, .(adm3_pcode, pct_public, pct_insured)]               # 2018 = base for carry-back
cb <- rbindlist(lapply(c(2016L,2017L), function(y) copy(ins18)[, `:=`(year=y, ins_carryback=TRUE)]))
ins_all <- rbind(ins_real[year>=2018][, ins_carryback:=FALSE], cb)                  # real 2018-25 + carried-back 2016-17
w <- merge(w, ins_all, by=c("adm3_pcode","year"), all.x=TRUE)

# ---------- merge treatment + covariates + income (155 = drop the 3 folded children) ----------
keep155 <- setdiff(trt$adm3_pcode, names(fold))
trt155 <- trt[adm3_pcode %in% keep155]
cov155 <- cov[adm3_pcode %in% keep155, !c("adm3_name","prov_norm"), with=FALSE]
inc155 <- inc[adm3_pcode %in% keep155]
dhs155 <- dhs[adm3_pcode %in% keep155, !c("prov"), with=FALSE]
panel155 <- Reduce(function(a,b) merge(a,b,by="adm3_pcode",all.x=TRUE),
                   list(w, trt155, cov155, inc155, dhs155))
panel155[, event_time := ifelse(ever_treated==1 & always_treated==0, year - first_year, NA_integer_)]
stopifnot(nrow(panel155)==155*10, uniqueN(panel155$adm3_pcode)==155)
stopifnot(!anyNA(panel155$ever_treated), !anyNA(panel155$always_treated))   # 2026-08-06: guard the treatment merge (no muni silently unmatched)
saveRDS(panel155, file.path(DIR_CLEAN,"panel_muni_year_155.rds"))

cat("\n==================== 06 PANEL VALIDATION ====================\n")
cat("panel_muni_year_155:", nrow(panel155), "rows (155x10) | cols:", ncol(panel155), "\n")
cat("any NA teen rate A/B/C:", panel155[, sum(is.na(rateA_15_19)|is.na(rateB_15_19)|is.na(rateC_15_19))], "\n")
cat("\nteen 15-19 rate per 1,000 women (pop-weighted), by year, denominator A:\n")
print(panel155[, .(rateA_15_19=round(weighted.mean(rateA_15_19, womenA_15_19),1),
                   rateB_15_19=round(weighted.mean(rateB_15_19, womenB_15_19),1)), by=year][order(year)])
cat("\nnational teen rate (per 1,000 person-yrs, 10y pooled):  A:",
    round(1000*sum(panel155$nb_15_19)/sum(panel155$womenA_15_19),1),
    " B:", round(1000*sum(panel155$nb_15_19)/sum(panel155$womenB_15_19),1),
    " C:", round(1000*sum(panel155$nb_15_19)/sum(panel155$womenC_15_19),1), "\n")
cat("10-14 rate (A):", round(1000*sum(panel155$nb_10_14)/sum(panel155$womenA_10_14),1),
    " | 30-34 rate (A):", round(1000*sum(panel155$nb_30_34)/sum(panel155$womenA_30_34),1), "\n")
cat("insurance carry-back rows (2016-17 filled from 2018):", panel155[ins_carryback==TRUE, .N],
    " | treated municipios in panel:", uniqueN(panel155[ever_treated==1, adm3_pcode]), "\n")
cat("saved -> panel_muni_year_155.rds (headline, A/B/C), panel_age_long_155.rds\n")
