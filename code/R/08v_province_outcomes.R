# ============================================================================
# 08v_province_outcomes.R — MM directive (2026-07-13): THREE outcomes side by
# side at province level — teen LIVE BIRTHS (our BDNV), teen PREGNANCY EVENTS
# (ONE 67-A: vaginal+cesarea+abortos), and ABORTIONS (ONE 67-A) — women 15-19.
# Panel: 32 provinces x 2017-2025 (ONE 2026 partial dropped; BDNV through 2025).
# Rates per 1,000 province women 15-19 (prov_controls, sex=="F").
# MODEL (continuous dose): rate ~ province AU intensity | prov + year, cluster
# prov. Same treatment variable as the ENHOGAR/desercion designs.
# PREDICTIONS (rule 1b): births NEGATIVE (aggregated echo of the muni headline;
# smaller |coef| than a naive muni-share scaling because province aggregation
# dilutes); pregnancies NEGATIVE and similar to births (they are ~90% births);
# ABORTIONS = the open mechanism question — fewer pregnancies predicts negative;
# substitution-to-abortion predicts positive; we expect negative-or-null.
# ONE caveat carried: facility counts, not population rates -> rates use census
# denominators, facility-coverage changes are a confound absorbed partly by
# prov FE + year FE; log-count spec as robustness.
# Output: output/tables/province_outcomes_dose.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(fixest)})
TAB <- file.path(PROJ,"output","tables")

one <- readRDS(file.path(DIR_CLEAN,"one_embarazos_province.rds"))$annual
one <- one[anio %between% c(2017,2025)]
w <- dcast(one[, .(n=sum(a1519)), by=.(prov_code, anio, tipo)], prov_code + anio ~ tipo, value.var="n", fill=0)
setnames(w, c("Abortos","Cesáreas","Vaginal"), c("abort","cesarea","vaginal"))
w[, `:=`(preg = abort + cesarea + vaginal, deliv = cesarea + vaginal)]

xw <- unique(as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[, .(adm3_pcode, prov_code)])
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
bp <- merge(b[age_mom %in% 15:19 & !is.na(adm3_pcode) & birth_year %between% c(2017,2025)],
            xw, by="adm3_pcode")[, .(births_bdnv=.N), by=.(prov_code, anio=birth_year)]
w <- merge(w, bp, by=c("prov_code","anio"), all.x=TRUE)

pp <- as.data.table(readRDS(file.path(DIR_CLEAN,"prov_controls_2000_2030_long.rds")))
pp <- pp[sex=="F" & age_group=="15-19", .(prov_code, anio=year, pop=pop)]
w <- merge(w, pp, by=c("prov_code","anio"), all.x=TRUE)
stopifnot(w[is.na(pop) | is.na(births_bdnv), .N]==0, nrow(w)==32*9)
ip <- as.data.table(readRDS(file.path(DIR_CLEAN,"province_au_intensity.rds")))
w <- merge(w, ip[, .(prov_code, anio=year, int=intensity)], by=c("prov_code","anio"))
w[, `:=`(r_births = 1000*births_bdnv/pop, r_preg = 1000*preg/pop, r_abort = 1000*abort/pop)]
cat("panel:", nrow(w), "rows | national rates 2019 vs 2024 (per 1,000 w15-19):\n")
print(w[anio %in% c(2019,2024), .(births=round(1000*sum(births_bdnv)/sum(pop),1),
      preg=round(1000*sum(preg)/sum(pop),1), abort=round(1000*sum(abort)/sum(pop),1)), by=anio], class=FALSE)

run <- function(v, lab){
  mr <- feols(as.formula(paste(v,"~ int | prov_code + anio")), w, weights=~pop, cluster=~prov_code)
  ml <- feols(as.formula(paste("log(",sub("r_","",v) |> (\(x) fcase(x=="births","births_bdnv", x=="preg","preg", x=="abort","abort"))(),"+1) ~ int | prov_code + anio")), w, weights=~pop, cluster=~prov_code)
  data.table(outcome=lab,
    coef_rate=round(coef(mr)[["int"]],3), se_rate=round(se(mr)[["int"]],3), p_rate=round(coeftable(mr)["int",4],3),
    coef_log=round(coef(ml)[["int"]],3), se_log=round(se(ml)[["int"]],3), p_log=round(coeftable(ml)["int",4],3))
}
res <- rbind(run("r_births","Live births 15-19 (BDNV)"),
             run("r_preg",  "Pregnancy events 15-19 (ONE 67-A)"),
             run("r_abort", "Abortions 15-19 (ONE 67-A)"))
cat("\n=== province dose model: rate/log ~ intensity | prov + year (2017-2025, weights pop, cluster prov) ===\n")
print(res, class=FALSE)
fwrite(res, file.path(TAB,"province_outcomes_dose.csv"))
cat("saved -> province_outcomes_dose.csv\n")
