# ============================================================================
# 02b_hamilton_perry.R  —  Municipio population projection 2016-2025 (THE denominators)
#
# Hamilton-Perry cohort-change-ratio (CCR) projection of municipio age x sex structure,
# launched from the 2022 census, with municipio cells CONTROLLED (raked) to CENSUS-ANCHORED
# province targets (ONE's official province projections rescaled so 2022 == census, keeping
# only ONE's projected growth rates). Reads the validated intermediates from 02.
#
# Year sources:
#   2016-2020 : observed ONE municipio estimates (already province-consistent)        [155 muni]
#   2021      : linear interpolation of observed-2020 & census-2022                    [155 muni]
#   2022      : census (anchor / truth)                                                [158 muni]
#   2023-2025 : CCR projection from 2022, raked to census-anchored province targets    [158 muni]
# 3 post-2020 municipios (San Victor/Matanzas/Baitoa): no 2015-2021 data -> 2022-2025 only.
#
# CCRs: from observed 2015->2020 (5-yr). Terminal 80+ uses open-interval CCR. The 0-4 group
# is held at base shares and fixed by raking (CWR unnecessary: 0-4 is NOT a study denominator
# and raking sets its level). CCRs winsorized to [0.3,3] (small-cell stability).
# Back-test (held-out): CCRs 2015->2020, project from 2020, predict 2022, compare to census.
# ============================================================================

source(here::here("code", "R", "00_config.R"))
suppressPackageStartupMessages(library(data.table))

AGE_LEVELS <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
ai <- data.table(age_group = AGE_LEVELS, ax = 1:17)
CCR_LO <- 0.3; CCR_HI <- 3

pm <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_muni_2015_2020_long.rds")))   # 2015-2020, 155
ca <- as.data.table(readRDS(file.path(DIR_CLEAN,"census2022_by_adm3_long.rds")))   # 2022, 158
pc <- as.data.table(readRDS(file.path(DIR_CLEAN,"prov_controls_2000_2030_long.rds")))
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
p2c <- xw[, .(adm3_pcode, prov_code)]

pm  <- merge(pm, ai, by="age_group")
ca  <- merge(ca, ai, by="age_group")
pc  <- merge(pc, ai, by="age_group")

# ---- E1: cohort-change ratios from observed 2015 -> 2020 ----
ccr_from <- function(d_t0, d_t1){   # d at base year t0 and t1=t0+5; returns adm3_pcode,sex,ax,CCRb
  prev <- d_t0[, .(adm3_pcode, sex, ax = ax + 1L, p_prev = pop)]   # t0 pop of (ax-1) -> aligned to ax
  R <- merge(d_t1[, .(adm3_pcode,sex,ax,p1=pop)], prev, by=c("adm3_pcode","sex","ax"), all.x=TRUE)
  t0_17 <- d_t0[ax==17, .(adm3_pcode,sex,p17=pop)]
  R <- merge(R, t0_17, by=c("adm3_pcode","sex"), all.x=TRUE)
  R[ax==17, p_prev := p_prev + p17]            # open interval: prev = t0[16] + t0[17]
  R[, CCR := p1 / p_prev]
  R[ax==1, CCR := NA_real_]                    # 0-4: handled by hold + rake
  R[, CCRb := pmin(pmax(CCR, CCR_LO), CCR_HI)]
  R[, .(adm3_pcode, sex, ax, CCR, CCRb)]
}
ccr <- ccr_from(pm[year==2015], pm[year==2020])
cat("--- E1: CCRs (2015->2020) ---\n")
cat("CCR rows:", nrow(ccr), " | winsorized (outside [0.3,3]):",
    sum(ccr$CCR<CCR_LO | ccr$CCR>CCR_HI, na.rm=TRUE), " | NA(0-4 grp):", sum(is.na(ccr$CCR)), "\n")
cat("CCR summary (ax>=2):\n"); print(round(summary(ccr[ax>=2, CCR]),3))

# ---- E2: project a base year (long: adm3_pcode,sex,ax,pop) forward 5 years via CCR ----
project5 <- function(base, ccr){
  prev <- base[, .(adm3_pcode, sex, ax = ax + 1L, p_prev = pop)]
  P <- merge(base[, .(adm3_pcode,sex,ax,pop)], prev, by=c("adm3_pcode","sex","ax"), all.x=TRUE)
  b17 <- base[ax==17, .(adm3_pcode,sex,p17=pop)]
  P <- merge(P, b17, by=c("adm3_pcode","sex"), all.x=TRUE)
  P[ax==17, p_prev := p_prev + p17]
  P <- merge(P, ccr[, .(adm3_pcode,sex,ax,CCRb)], by=c("adm3_pcode","sex","ax"), all.x=TRUE)
  P[, proj := CCRb * p_prev]
  P[ax==1, proj := pop]                        # hold 0-4 (raked later)
  P[is.na(proj), proj := pop]                  # municipios w/o CCR (3 new) -> hold census
  P[, .(adm3_pcode, sex, ax, proj)]
}

# ---- E3: build raw (pre-rake) projected/interpolated years ----
c22 <- ca[, .(adm3_pcode, sex, ax, pop)]
P27 <- project5(c22, ccr)                                       # census 2022 -> 2027
proj_grid <- merge(c22[, .(adm3_pcode,sex,ax,c=pop)], P27, by=c("adm3_pcode","sex","ax"))
years_fwd <- 2023:2025
raw_fwd <- rbindlist(lapply(years_fwd, function(t){
  g <- copy(proj_grid); g[, pop := c + (proj - c) * ((t - 2022)/5)]; g[, year := t]
  g[, .(adm3_pcode, sex, ax, year, pop)]
}))
# 2021 = linear midpoint of observed-2020 and census-2022 (155 muni that have 2020)
raw_2021 <- merge(pm[year==2020, .(adm3_pcode,sex,ax,p20=pop)],
                  c22[, .(adm3_pcode,sex,ax,c=pop)], by=c("adm3_pcode","sex","ax"))
raw_2021[, `:=`(pop = 0.5*p20 + 0.5*c, year = 2021L)]
raw_2021 <- raw_2021[, .(adm3_pcode,sex,ax,year,pop)]
raw_proj <- rbind(raw_2021, raw_fwd)
cat("\n--- E3: raw projected rows (2021,2023-2025):", nrow(raw_proj),
    " | any negative:", any(raw_proj$pop<0), " | any NA:", anyNA(raw_proj$pop), "\n")

# ---- E4: census-anchored province targets, then RAKE ----
# province census-2022 totals by (prov_code,sex,ax)
prov22 <- merge(ca, p2c, by="adm3_pcode")[, .(t22 = sum(pop)), by=.(prov_code,sex,ax)]
# ONE projected growth ratio r(t) = control(t)/control(2022); 2021 from observed-2020 midpoint
ctrl <- pc[, .(prov_code, sex, ax, year, pop)]
prov_obs2020 <- ctrl[year==2020, .(prov_code,sex,ax,c20=pop)]
mk_target <- function(t){
  if (t == 2021){
    g <- merge(prov_obs2020, prov22, by=c("prov_code","sex","ax"))
    g[, target := 0.5*c20 + 0.5*t22]
  } else {
    rt <- merge(ctrl[year==t, .(prov_code,sex,ax,ct=pop)],
                ctrl[year==2022,.(prov_code,sex,ax,c22=pop)], by=c("prov_code","sex","ax"))
    rt[, r := ct/c22]
    g <- merge(prov22, rt[, .(prov_code,sex,ax,r)], by=c("prov_code","sex","ax"))
    g[, target := t22 * r]
  }
  g[, .(prov_code,sex,ax,year=t,target)]
}
targets <- rbindlist(lapply(c(2021L,2023:2025), mk_target))
# rake: scale municipio cells so they sum (within prov x sex x ax x year) to target
rk <- merge(raw_proj, p2c, by="adm3_pcode")
rk[, grp_sum := sum(pop), by=.(prov_code,sex,ax,year)]
rk <- merge(rk, targets, by=c("prov_code","sex","ax","year"))
rk[, pop_raked := pop * target / grp_sum]
cat("\n--- E4: raking ---\n")
chk <- rk[, .(s = sum(pop_raked), tgt = target[1]), by=.(prov_code,sex,ax,year)]
cat("max |raked municipio sum - province target|:", round(max(abs(chk$s - chk$tgt)),6), "\n")
cat("any negative raked:", any(rk$pop_raked<0), " any NA:", anyNA(rk$pop_raked), "\n")

# ---- E5: assemble full 2016-2025 panel (adm3_pcode,sex,age_group,year,pop) ----
obs <- pm[year %in% 2016:2020, .(adm3_pcode,sex,ax,year,pop)]
cen <- ca[, .(adm3_pcode,sex,ax,pop)][, year := 2022L]
prj <- rk[, .(adm3_pcode,sex,ax,year,pop = pop_raked)]
panel <- rbind(obs, cen, prj)
panel <- merge(panel, ai, by="ax")[, .(adm3_pcode,sex,age_group,year,pop)]
setorder(panel, adm3_pcode, sex, year, age_group)
cat("\n--- E5: assembled panel ---\n")
cat("rows:", nrow(panel), " | years:", paste(range(panel$year),collapse="-"),
    " | municipios:", uniqueN(panel$adm3_pcode), "\n")
cat("municipio-years: 155*(2016:2021=6) + 158*(2022:2025=4) =", 155*6 + 158*4, "expected groups\n")
cat("any NA:", anyNA(panel$pop), " any negative:", any(panel$pop<0), "\n")
# national female 15-19 trajectory (sanity)
cat("national F 15-19 by year:\n")
print(panel[sex=="F" & age_group=="15-19", .(F1519 = round(sum(pop))), by=year][order(year)])

# ---- E6: BACK-TEST (held out): CCRs 2015->2020, project from 2020, predict 2022 vs census ----
P25_from20 <- project5(pm[year==2020, .(adm3_pcode,sex,ax,pop)], ccr)   # 2020 -> 2025
bt <- merge(pm[year==2020, .(adm3_pcode,sex,ax,p20=pop)], P25_from20, by=c("adm3_pcode","sex","ax"))
bt[, pred22 := p20 + (proj - p20)*(2/5)]                                # linear interp to 2022
bt <- merge(bt, ca[, .(adm3_pcode,sex,ax,actual22=pop)], by=c("adm3_pcode","sex","ax"))  # 155 muni
cat("\n--- E6: BACK-TEST (predict 2022 from 2015-2020, census held out) ---\n")
# national by age-sex
natbt <- bt[, .(pred=sum(pred22), act=sum(actual22)), by=.(sex,ax)]
natbt[, ape := abs(pred-act)/act]
cat("national-by-age-sex: MAPE =", paste0(round(100*mean(natbt$ape),2),"%"),
    " | ALPE =", paste0(round(100*mean((natbt$pred-natbt$act)/natbt$act),2),"%"), "\n")
# focus age groups (our denominators), females
cat("female denominators (national, pred vs census 2022):\n")
print(merge(natbt[sex=="F"], ai, by="ax")[age_group %in% c("10-14","15-19","30-34"),
      .(age_group, pred=round(pred), census=round(act), pct_err=round(100*(pred-act)/act,2))])
# municipio-level overall
bt[, ape := abs(pred22-actual22)/pmax(actual22,1)]
cat("municipio-cell MAPE (all sex x age x 155 muni):", paste0(round(100*mean(bt$ape),2),"%"),
    " | median APE:", paste0(round(100*median(bt$ape),2),"%"), "\n")
# 2026-08-06 (MM): persist the back-test so the paper's MAPE claim (Table 9 note,
# Data section) is reproducible from output/, not only from this console print.
fwrite(data.table(stat=c("mape_national_agesex","alpe_national_agesex","mape_municipio_cells","medape_municipio_cells"),
                  value=c(mean(natbt$ape), mean((natbt$pred-natbt$act)/natbt$act), mean(bt$ape), median(bt$ape))),
       file.path(PROJ,"output","tables","projection_validation.csv"))
cat("saved -> projection_validation.csv\n")

saveRDS(panel, file.path(DIR_CLEAN, "pop_municipio_2016_2025.rds"))
cat("\nsaved -> data/clean/pop_municipio_2016_2025.rds  (THE denominators)\n")
