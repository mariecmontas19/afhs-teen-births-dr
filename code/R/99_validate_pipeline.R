# ============================================================================
# 99_validate_pipeline.R  —  Independent end-to-end audit of clean outputs
#
# Loads every data/clean/* artifact produced by 01, 01b, 02 and runs integrity +
# cross-file consistency checks. Prints PASS/FAIL per check; stops if any FAIL.
# Re-run anytime after pipeline changes. Does NOT recompute from raw (that's the
# scripts' job); it audits the saved artifacts and their mutual consistency.
# ============================================================================

source(here::here("code", "R", "00_config.R"))
suppressPackageStartupMessages(library(data.table))

checks <- list()
ok <- function(name, cond, detail = "") {
  cond <- isTRUE(cond)
  checks[[length(checks)+1]] <<- data.table(check = name, pass = cond, detail = detail)
  cat(sprintf("[%s] %s%s\n", if (cond) "PASS" else "FAIL", name,
              if (nzchar(detail)) paste0("  (", detail, ")") else ""))
}

f <- function(x) file.path(DIR_CLEAN, x)
need <- c("muni_crosswalk.rds","muni_match_helpers.rds","pcua_blank_muni_resolved.csv",
          "pop_muni_2015_2020_long.rds","census2022_muni_code_long.rds",
          "census_code_to_adm3.rds","census2022_by_adm3_long.rds",
          "prov_controls_2000_2030_long.rds")
ok("all clean files exist", all(file.exists(f(need))),
   paste(need[!file.exists(f(need))], collapse=", "))

xw   <- as.data.table(readRDS(f("muni_crosswalk.rds")))
pm   <- as.data.table(readRDS(f("pop_muni_2015_2020_long.rds")))
cc   <- as.data.table(readRDS(f("census_code_to_adm3.rds")))
ca   <- as.data.table(readRDS(f("census2022_by_adm3_long.rds")))
pc   <- as.data.table(readRDS(f("prov_controls_2000_2030_long.rds")))
res  <- fread(f("pcua_blank_muni_resolved.csv"))
AGE17 <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
           "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")

# ---- crosswalk ----
ok("crosswalk 158 unique municipios", nrow(xw)==158 && uniqueN(xw$adm3_pcode)==158)
ok("crosswalk prov_code in 1:32", all(xw$prov_code %in% 1:32) && uniqueN(xw$prov_code)==32)
ok("crosswalk 155 in xlsx + 3 new", sum(xw$in_xlsx_2015_2020)==155 && sum(!xw$in_xlsx_2015_2020)==3)
ok("pcode encodes prov_code (chars 6-7)", all(as.integer(substr(xw$adm3_pcode,6,7))==xw$prov_code))
ok("pcode all 9 chars, start DOM", all(nchar(xw$adm3_pcode)==9 & startsWith(xw$adm3_pcode,"DOM")))

# ---- pop_muni (2015-2020) ----
ok("pop_muni rows = 155*2*17*6", nrow(pm)==31620, paste("got", nrow(pm)))
ok("pop_muni no NA / no negative", !anyNA(pm$pop) && all(pm$pop>=0))
ok("pop_muni sexes = M/F only", setequal(unique(pm$sex), c("M","F")))
ok("pop_muni 17 canonical ages", setequal(unique(pm$age_group), AGE17))
ok("pop_muni years 2015:2020", setequal(unique(pm$year), 2015:2020))
ok("pop_muni 155 municipios, all in crosswalk", uniqueN(pm$adm3_pcode)==155 && all(pm$adm3_pcode %in% xw$adm3_pcode))
ok("pop_muni no duplicate keys", !any(duplicated(pm[, .(adm3_pcode,sex,age_group,year)])))

# ---- census 2022 by adm3 ----
ok("census22 158 municipios = crosswalk set", setequal(ca$adm3_pcode, xw$adm3_pcode))
ok("census22 total = 10,773,879", sum(ca$pop)==10773879, paste("got", sum(ca$pop)))
ok("census22 no NA / no negative", !anyNA(ca$pop) && all(ca$pop>=0))
ok("census22 sexes M/F, 17 ages", setequal(unique(ca$sex),c("M","F")) && setequal(unique(ca$age_group),AGE17))
ok("census22 no duplicate keys", !any(duplicated(ca[, .(adm3_pcode,sex,age_group)])))

# ---- census_code_to_adm3 ----
ok("code2pcode 158 unique pcodes", nrow(cc)==158 && uniqueN(cc$adm3_pcode)==158)
ok("code2pcode matches pcode encoding",
   all(cc$prov_code==as.integer(substr(cc$adm3_pcode,6,7))) &
   all(cc$muni_code==as.integer(substr(cc$adm3_pcode,8,9))))

# ---- province controls ----
ok("prov_ctrl 32 provinces", uniqueN(pc$prov_code)==32 && all(pc$prov_code %in% 1:32))
ok("prov_ctrl years 2000:2030", setequal(unique(pc$year), 2000:2030))
ok("prov_ctrl M/F, 17 ages, no NA/neg",
   setequal(unique(pc$sex),c("M","F")) && setequal(unique(pc$age_group),AGE17) && !anyNA(pc$pop) && all(pc$pop>=0))
ok("prov_ctrl rows = 32*2*17*31", nrow(pc)==32*2*17*31, paste("got", nrow(pc)))
# CROSS: province total == sum of municipios for 2015-2020
mbp <- merge(pm, xw[, .(adm3_pcode, prov_code)], by="adm3_pcode")[
  , .(muni_sum=sum(pop)), by=.(prov_code,sex,age_group,year)]
cmp <- merge(pc[year %in% 2015:2020], mbp, by=c("prov_code","sex","age_group","year"))
ok("province == sum(municipios) 2015-2020 (all rows)",
   nrow(cmp)==6528 && max(abs(cmp$pop-cmp$muni_sum))==0,
   paste("rows", nrow(cmp), "maxdiff", max(abs(cmp$pop-cmp$muni_sum))))

# ---- AU blank-municipio resolution ----
ok("pcua resolved 10 units, valid pcodes",
   nrow(res)==10 && all(res$adm3_pcode %in% xw$adm3_pcode))

# ---- external sanity: national census F by key age groups ----
f1519 <- ca[sex=="F" & age_group=="15-19", sum(pop)]
f3034 <- ca[sex=="F" & age_group=="30-34", sum(pop)]
ok("national F 15-19 in plausible 380k-470k", f1519 > 380000 & f1519 < 470000, paste("=", f1519))
ok("national F 30-34 in plausible 390k-480k", f3034 > 390000 & f3034 < 480000, paste("=", f3034))

# ---- denominator series (02b/02c/02d) — STRUCTURAL checks only ----
# Audits structure/consistency, NOT projection accuracy (that's the back-test / cohort
# diagnostic). Each series: M/F, 17 ages, years 2016-2025, no NA/neg, all pcodes in crosswalk,
# 2016-2020 == observed ONE municipio estimates.
denom_files <- c(A="pop_municipio_2016_2025_A.rds",        # series A (ONE official)
                 B="pop_municipio_2016_2025_B.rds",        # series B (intercensal) [when built]
                 census="pop_municipio_2016_2025.rds")     # 2022-census-anchored (02b)
for (nm in names(denom_files)) {
  fp <- f(denom_files[nm]); if (!file.exists(fp)) next
  dn <- as.data.table(readRDS(fp))
  ok(paste0("[",nm,"] years 2016:2025, M/F, 17 ages"),
     setequal(unique(dn$year),2016:2025) && setequal(unique(dn$sex),c("M","F")) &&
     setequal(unique(dn$age_group),AGE17))
  ok(paste0("[",nm,"] no NA/neg, pcodes in crosswalk"),
     !anyNA(dn$pop) && all(dn$pop>=0) && all(dn$adm3_pcode %in% xw$adm3_pcode))
  ok(paste0("[",nm,"] no duplicate keys"), !any(duplicated(dn[,.(adm3_pcode,sex,age_group,year)])))
  # series-specific construction checks:
  if (nm %in% c("A","census")) {        # these use observed ONE estimates for 2016-2020
    d_obs <- merge(dn[year %in% 2016:2020], pm[year %in% 2016:2020],
                   by=c("adm3_pcode","sex","age_group","year"), suffixes=c("_dn","_pm"))
    ok(paste0("[",nm,"] 2016-2020 == observed ONE municipio (155)"),
       uniqueN(d_obs$adm3_pcode)==155 && nrow(d_obs)==155*5*2*17 && all(d_obs$pop_dn==d_obs$pop_pm))
  }
  if (nm == "A")      ok("[A] 155 municipios", uniqueN(dn$adm3_pcode)==155)
  if (nm == "census") ok("[census] 158 (2022-25) / 155 (2016-21)",
                         uniqueN(dn[year>=2022]$adm3_pcode)==158 && uniqueN(dn[year<=2021]$adm3_pcode)==155)
  if (nm == "B") {     # intercensal: 155 muni; 2022 == census folded to 155; NOT == ONE estimates
    ca155 <- as.data.table(readRDS(f("census2022_by_adm3_long.rds")))
    ca155[, adm3_pcode := fifelse(adm3_pcode=="DOM010905","DOM010901",
                          fifelse(adm3_pcode=="DOM051703","DOM051701",
                          fifelse(adm3_pcode=="DOM012510","DOM012501", adm3_pcode)))]
    ca155 <- ca155[, .(cpop=sum(pop)), by=.(adm3_pcode,sex,age_group)]
    d22 <- merge(dn[year==2022], ca155, by=c("adm3_pcode","sex","age_group"))
    ok("[B] 155 municipios", uniqueN(dn$adm3_pcode)==155)
    ok("[B] 2022 == census folded to 155", nrow(d22)==155*2*17 && all(abs(d22$pop-d22$cpop)<1e-6))
    ok("[B] total 2022 == 10,773,879", round(dn[year==2022,sum(pop)])==10773879)
  }
}

# ---- cleaned births (03) — structural + sanity checks ----
if (file.exists(f("births_clean_2016_2025.rds"))) {
  bb <- as.data.table(readRDS(f("births_clean_2016_2025.rds")))
  ok("births rows = 1,444,209", nrow(bb)==1444209, paste("got", nrow(bb)))   # new BDNV file 2026-06-23
  ok("births years 2016-2025", setequal(unique(na.omit(bb$birth_year)), 2016:2025))
  ok("births adm3 match >99.9% & pcodes in crosswalk",
     mean(is.na(bb$adm3_pcode))<0.001 && all(na.omit(bb$adm3_pcode) %in% xw$adm3_pcode))
  ok("births age recodes (min age 10; no 0/1/9 remain)",
     bb[!is.na(age_mom), min(age_mom)]>=10 && !any(bb$age_mom %in% c(0,1,9)))
  ok("births age_grp = 8 reproductive bands",
     setequal(levels(bb$age_grp), c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45+")))
  ok("births outcomes 0/1 + ranges plausible",
     all(bb$low_bw %in% c(0,1,NA)) && all(bb$preterm %in% c(0,1,NA)) && all(bb$csection %in% c(0,1,NA)) &&
     all(bb$sga %in% c(0,1,NA)) &&
     bb[!is.na(weight_g), max(weight_g)<=6500 & min(weight_g)>=300] &&
     bb[!is.na(gestage_wks), max(gestage_wks)<=44 & min(gestage_wks)>=20])
  ok("births SGA ~10% (8-11)", {s<-100*mean(bb$sga,na.rm=TRUE); s>=8 & s<=11}, paste0(round(100*mean(bb$sga,na.rm=TRUE),1),"%"))
  ok("births neo_any/anc4/anc8 are 0/1 & neo_any=Medio|Alto (~17.9%)",
     all(bb$neo_any %in% c(0,1,NA)) && all(bb$anc4 %in% c(0,1,NA)) && all(bb$anc8 %in% c(0,1,NA)) &&
     {a<-100*mean(bb$neo_any,na.rm=TRUE); a>=17 & a<=19},
     paste0("neo_any ", round(100*mean(bb$neo_any,na.rm=TRUE),1), "% / anc4 ", round(100*mean(bb$anc4,na.rm=TRUE),1), "%"))
  ok("births composition coverage (educ empty <=2019, present 2020+)",
     bb[birth_year<=2019, all(is.na(educ))] && bb[birth_year>=2020, mean(!is.na(educ))>0.5])
  ok("births nationality/insurance categories valid",
     all(na.omit(unique(as.character(bb$nationality))) %in% c("Dominican","Haitian","Other")) &&
     all(na.omit(unique(bb$insurance)) %in% c("Public","Autogestion","Private","None")))
  ok("births conception_date < birth_date (where valid)",
     bb[!is.na(conception_date), all(conception_date < birth_date)])
}

# ---- treatment (04) ----
if (file.exists(f("treatment_municipio.rds"))) {
  tt <- as.data.table(readRDS(f("treatment_municipio.rds")))
  um <- as.data.table(readRDS(f("pcua_units_mapped.rds")))
  ok("treatment = 158 municipios", nrow(tt)==158, paste("got", nrow(tt)))
  ok("treatment ever_treated 0/1 & treated pcodes in crosswalk",
     all(tt$ever_treated %in% c(0,1)) && all(tt[ever_treated==1, adm3_pcode] %in% xw$adm3_pcode))
  ok("treatment always_treated = first_year<2016 (=9)",
     tt[always_treated==1, .N]==9 && all(tt[always_treated==1, first_year]<2016),
     paste(tt[always_treated==1,.N], "always-treated"))
  ok("treatment n_units sum = 39 units, all mapped",   # 39 since Boca Chica duplicate dropped (§59.38, 2026-07-17; was 40 after Las Terrenas added 2026-06-25)
     sum(tt$n_units)==39 && nrow(um)==39 && all(!is.na(um$adm3_pcode)),
     paste("sum n_units", sum(tt$n_units)))
  ok("treatment cohort = first_year (treated) / NA (never)",
     tt[ever_treated==1, all(cohort==first_year)] && tt[ever_treated==0, all(is.na(cohort))])
  ok("treatment distance present (or NA if geom evicted)",
     all(is.na(tt$dist_km_nearest_pcua)) || tt[!is.na(dist_km_nearest_pcua), all(dist_km_nearest_pcua>=0)],
     if (all(is.na(tt$dist_km_nearest_pcua))) "NA (no geom cache)" else "computed")
}

# ---- census covariates (05) ----
if (file.exists(f("census_covariates_muni.rds"))) {
  cv <- as.data.table(readRDS(f("census_covariates_muni.rds")))
  ok("covariates = 158 municipios & pcodes in crosswalk",
     nrow(cv)==158 && all(cv$adm3_pcode %in% xw$adm3_pcode))
  ok("covariates no NA in key vars",
     cv[, sum(is.na(wealth_index)|is.na(pct_educ_secplus)|is.na(pct_in_union)|is.na(pct_urban)|is.na(pct_internet))]==0)
  ok("covariate proportions in [0,1]",
     cv[, all(pct_educ_secplus %between% c(0,1) & pct_in_union %between% c(0,1) &
              pct_urban %between% c(0,1) & pct_internet %between% c(0,1))])
  ok("child-woman ratios positive (2010 baseline + 2022 desc) & wealth_index finite",
     cv[, all(cwr_2010_baseline>0 & cwr_2022_desc>0 & is.finite(wealth_index))])
  ok("SES gradient sane (DN most urban & educated)",
     cv[adm3_pcode=="DOM100101", pct_urban>0.95 & pct_educ_secplus>0.8],
     "Distrito Nacional")
}

# ---- ENGIH province SES (05b) ----
if (file.exists(f("income_ses_province.rds"))) {
  iz <- as.data.table(readRDS(f("income_ses_province.rds")))
  ok("ENGIH province SES = 158 municipios, 32 provinces, no NA",
     nrow(iz)==158 && uniqueN(iz$prov_code)==32 && !anyNA(iz$pct_bottom2q_2018))
  ok("ENGIH pct_bottom2q in [0,1]", iz[, all(pct_bottom2q_2018 %between% c(0,1))])
}

# ---- analysis panel (06) ----
if (file.exists(f("panel_muni_year_155.rds"))) {
  pn <- as.data.table(readRDS(f("panel_muni_year_155.rds")))
  lg <- as.data.table(readRDS(f("panel_age_long_155.rds")))
  ok("panel balanced 155x10 = 1550 & long 10850 (7 age groups)", nrow(pn)==1550 && uniqueN(pn$adm3_pcode)==155 && nrow(lg)==10850)
  ok("panel no NA teen rates A/B/C (15-19)",
     pn[, sum(is.na(rateA_15_19)|is.na(rateB_15_19)|is.na(rateC_15_19))]==0)
  ok("panel national teen rate plausible (A 40-70/1000)",
     {r<-1000*sum(pn$nb_15_19)/sum(pn$womenA_15_19); r>=40 & r<=70}, paste0(round(1000*sum(pn$nb_15_19)/sum(pn$womenA_15_19),1),"/1000"))
  ok("panel teen births fold = clean 15-19 count (~219,068 less blank/age-NA)",
     {tot<-sum(pn$nb_15_19); tot>210000 & tot<=219068}, paste("nb_15_19 sum", sum(pn$nb_15_19)))
  ok("panel age-DDD = rate(15-19)-rate(30-34)", pn[, all(abs(ddd_A-(rateA_15_19-rateA_30_34))<1e-9)])
  ok("panel treatment merged (cohort/event_time/always_treated present)",
     all(c("cohort","event_time","always_treated","n_units") %in% names(pn)) &&
     pn[ever_treated==1 & always_treated==0, all(event_time==year-first_year)])
  ok("panel covariates merged, no NA (wealth/educ/union)",
     pn[, sum(is.na(wealth_index)|is.na(pct_educ_secplus)|is.na(pct_in_union))]==0)
  ok("panel insurance carry-back: 310 rows (155x2) flagged & pct in [0,1]",
     pn[ins_carryback==TRUE, .N]==310 && pn[!is.na(pct_public), all(pct_public %between% c(0,1))])
}
if (file.exists(f("panel_muni_month_155.rds"))) {
  pm <- as.data.table(readRDS(f("panel_muni_month_155.rds")))
  ok("monthly panel balanced 155x10x12 = 18,600, no NA births",
     nrow(pm)==18600 && pm[is.na(n_1519),.N]==0)
  ok("monthly teen births = annual (219,068) & event_month for 14 treated",
     sum(pm$n_1519)==219068 && pm[!is.na(event_month), uniqueN(id)]==14)
}

# ---- MISPAS facility build (05n/05n2/05n3/05n4, §59.11-59.20) ----
if (file.exists(f("mispas_facility_month.rds"))) {
  mfm <- as.data.table(readRDS(f("mispas_facility_month.rds")))
  ok("mispas monthly: years 2022-2026, months 1-12, age bands present",
     mfm[, min(anio)]==2022 && mfm[, max(anio)]==2026 && all(mfm$mes_n %in% 1:12) &&
     all(c("a1519","lt15","other") %in% mfm$age_band))
  ok("mispas monthly: teen events 100% muni-assigned",
     mfm[age_band %in% c("a1519","lt15") & is.na(adm3), sum(deliv+abort)]==0)
  ok("mispas monthly: no negative counts", mfm[, all(deliv>=0 & abort>=0)])
}
if (file.exists(f("mispas_cuadro29_annual.rds"))) {
  c29 <- as.data.table(readRDS(f("mispas_cuadro29_annual.rds")))
  ok("cuadro29: years 2015-2021 complete, 32 provinces (publico)",
     identical(sort(unique(c29$anio)), 2015:2021) && c29[sector=="publico", uniqueN(prov)]==32)
  ok("cuadro29: national teen deliveries in plausible band (20k-35k/yr)",
     c29[, .(d=sum(vaginal_a1519+cesarea_a1519, na.rm=TRUE)), by=anio][, all(d %between% c(20000, 35000))])
  ok("cuadro29: >=99% of teen events muni-assigned",
     c29[, sum((vaginal_a1519+cesarea_a1519+aborto_a1519)*(!is.na(adm3)), na.rm=TRUE) /
         sum(vaginal_a1519+cesarea_a1519+aborto_a1519, na.rm=TRUE)] >= 0.99)
}
if (file.exists(f("mispas_muni_panels.rds"))) {
  mp <- readRDS(f("mispas_muni_panels.rds"))
  ok("mispas panels: muni-year 2015-2025 + muni-quarter objects present",
     identical(sort(unique(mp$year$anio)), 2015:2025) && nrow(mp$quarter) > 0)
  ok("mispas panels: seam continuity 2021->2022 within 20%",
     abs(mp$year[anio==2022, sum(deliv_1519)] / mp$year[anio==2021, sum(deliv_1519)] - 1) < 0.20)
  ok("mispas panels: stillbirths only 2022+ (Cuadro 29 lacks them)",
     mp$year[anio<2022, all(is.na(sb_1519))] && mp$year[anio>=2022, !anyNA(sb_1519)])
  v <- file.path(PROJ, "output", "tables", "mispas_validation.csv")
  if (file.exists(v)) {
    vd <- fread(v)
    ok("mispas validation: corr vs NSO (prov) > .98 & vs BDNV (muni) > .90",
       vd[check=="V1_NSO_prov", cor(ours, theirs)] > 0.98 &&
       vd[check=="V2_BDNV_muni", cor(ours, theirs)] > 0.90)
  }
}
if (file.exists(f("mispas_mechanisms_quarter.rds"))) {
  mq <- as.data.table(readRDS(f("mispas_mechanisms_quarter.rds")))
  ok("mechanisms: prenatal from 2020, >=95% adolescent consults muni-assigned",
     mq[!is.na(pren_adol), min(anio)]==2020 &&
     mq[!is.na(adm3), sum(pren_adol, na.rm=TRUE)] / mq[, sum(pren_adol, na.rm=TRUE)] >= 0.95)
}
if (file.exists(f("one_embarazos_province.rds"))) {
  oe <- readRDS(f("one_embarazos_province.rds"))
  ok("ONE embarazos: 32 provinces, 2017-2026, 3 tipos",
     oe$annual[, uniqueN(prov_code)]==32 && oe$annual[, min(anio)]==2017 &&
     oe$annual[, uniqueN(tipo)]==3)
}

# ---- summary ----
res_dt <- rbindlist(checks)
cat("\n==================== AUDIT SUMMARY ====================\n")
cat(sprintf("PASS %d / %d checks\n", sum(res_dt$pass), nrow(res_dt)))
if (any(!res_dt$pass)) { cat("FAILURES:\n"); print(res_dt[pass==FALSE]); stop("AUDIT FAILED") }
cat("ALL CHECKS PASSED.\n")
