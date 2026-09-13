# ============================================================================
# 08y_mechanisms_cs.R — UTILIZATION / FIRST-STAGE mechanisms from MISPAS
# facility cuadros (05n4), muni-year, same CS spec as headline (§59.12/08x).
#  (A) adolescent prenatal-control consults per 1,000 women 15-19 (2020-2025)
#  (B) adolescent SHARE of prenatal consults (2020-2025)
#  (C) consejeria + planificacion familiar per 1,000 women 15-49 (2022-2025;
#      NOT age-split in the source) — identifiable for 2023+ cohorts only.
# PREDICTIONS (rule 1b), stated before running:
#  (A) AMBIGUOUS: fewer pregnancies -> mechanically fewer prenatal consults
#      (negative); AU pulls adolescent care in / raises checks per pregnancy
#      (positive). Net could go either way; exploratory.
#  (B) same ambiguity, netted against adults.
#  (C) POSITIVE — counseling/family planning IS the AU service; this is the
#      cleanest available first-stage utilization test. Honest caveats: short
#      panel (4 yrs), 2020-22 cohorts unusable, g=2022 has no pre-period.
# Note: 2020-21 cohorts (g <= first panel year +? ) are dropped by att_gt where
# no pre-period exists; ATTs here cover LATER cohorts than the headline.
# Output: output/tables/mechanisms_cs.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
TAB <- file.path(PROJ,"output","tables")
fold <- c("DOM010905","DOM051703","DOM012510")
fm <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")
to155 <- function(p) fifelse(p %in% names(fm), unname(fm[p]), p)

m <- as.data.table(readRDS(file.path(DIR_CLEAN,"mispas_mechanisms_quarter.rds")))
m <- m[!is.na(adm3)][, adm3_pcode := to155(adm3)]
my <- m[anio <= 2025, .(pa=sum(pren_adol, na.rm=TRUE), pu=sum(pren_adult, na.rm=TRUE),
                        cp=sum(consej, na.rm=TRUE)+sum(planif, na.rm=TRUE),
                        n_c33=sum(!is.na(consej))), by=.(adm3_pcode, year=anio)]
pw <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold &
        year %between% c(2020,2025), .(adm3_pcode, year, w1519=womenA_15_19, w1549=n_w1549)]
d <- merge(pw, my, by=c("adm3_pcode","year"), all.x=TRUE)
for(v in c("pa","pu","cp")) d[is.na(get(v)), (v) := 0]
d[, `:=`(r_pren = 1000*pa/w1519, sh_adol = fifelse(pa+pu>0, pa/(pa+pu), NA_real_),
         r_cp = 1000*cp/w1549)]
cat("panel:", nrow(d), "muni-years | mean adol prenatal rate:", round(mean(d$r_pren),1),
    "| mean adol share:", round(mean(d$sh_adol, na.rm=TRUE),3), "\n")

tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                  fifelse(ever_treated==0L, 0L, NA_integer_))]
run <- function(dd, yv, lab){
  est <- merge(dd, tr[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  est <- est[!is.na(get(yv))]
  est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(att_gt(yname=yv, tname="year", idname="id", gname="g", xformla=~1,
        data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE,
        allow_unbalanced_panel=TRUE))
  g <- aggte(a, type="group", na.rm=TRUE)
  data.table(outcome=lab, att=round(g$overall.att,4), se=round(g$overall.se,4),
             p=round(2*pnorm(-abs(g$overall.att/g$overall.se)),4),
             cohorts=paste(sort(unique(g$egt[!is.na(g$att.egt)])), collapse=","))
}
res <- rbind(
  run(d, "r_pren",  "(A) adol prenatal consults /1k w15-19, 2020-25"),
  run(d, "sh_adol", "(B) adolescent share of prenatal consults, 2020-25"),
  run(d[year >= 2022], "r_cp", "(C) consejeria+planif /1k w15-49, 2022-25"))
cat("\n=== MECHANISM CS (same estimator; later cohorts only where noted) ===\n")
print(res, class=FALSE)
fwrite(res, file.path(TAB,"mechanisms_cs.csv"))
cat("saved -> mechanisms_cs.csv\n")
