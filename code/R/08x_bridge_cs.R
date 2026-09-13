# ============================================================================
# 08x_bridge_cs.R — THE BRIDGE DESIGN (§59.12): same CS estimator, four
# outcomes, geography changes one step at a time.
#  (1) BIRTHS by RESIDENCE (rateA_15_19)          — the headline anchor
#  (2) BIRTHS by DELIVERY municipio (BDNV adm3_pcode_delivery, all facilities)
#  (3) PREGNANCY EVENTS at facilities (MISPAS deliv+abort, publico, 05n3)
#  (4) ABORTIONS at facilities (MISPAS)
# All rates per 1,000 muni women 15-19 (denominator A). CS spec = 07t headline:
# att_gt, control="notyettreated", base="universal", est="reg", biters=2000,
# cluster muni; panel 2016-2025, 146 munis (fold applied); S1 binary cohorts.
# Outcomes 2-4 zero-filled on the full grid (a muni without a maternity
# facility truly has ~0 institutional events; delivery-muni births include
# home births assigned to their muni).
# PREDICTIONS (rule 1b), stated before running:
#  (1) reproduces -6.43 (anchor; any deviation = bug).
#  (2) NEGATIVE but ATTENUATED vs (1) — AU hospitals attract teen patients
#      from neighbors, biasing delivery-based effects toward zero. Guess -3 to -6.
#  (3) close to (2), slightly more negative if pregnancies fall beyond births.
#  (4) small negative, likely ns (base rate ~2-3/1,000).
# Output: output/tables/bridge_cs.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
TAB <- file.path(PROJ,"output","tables")
fold <- c("DOM010905","DOM051703","DOM012510")
fm <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")
to155 <- function(p) fifelse(p %in% names(fm), unname(fm[p]), p)

pw <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold,
        .(adm3_pcode, year, rateA_15_19, women=womenA_15_19)]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                  fifelse(ever_treated==0L, 0L, NA_integer_))]
grid <- pw[, .(adm3_pcode, year, women)]

# outcome 2: births by delivery muni
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
bd <- b[age_mom %in% 15:19 & !is.na(adm3_pcode_delivery),
        .(n=.N, npub=sum(facility=="Public", na.rm=TRUE)),
        by=.(adm3_pcode=to155(adm3_pcode_delivery), year=birth_year)]
# outcomes 3/4: MISPAS facility events
mm <- readRDS(file.path(DIR_CLEAN,"mispas_muni_panels.rds"))$year
mm <- mm[, .(adm3_pcode, year=anio, preg=deliv_1519+abort_1519, abort=abort_1519, sb=sb_1519)]

d <- Reduce(function(x,y) merge(x, y, by=c("adm3_pcode","year"), all.x=TRUE),
            list(grid, bd, mm))
for(v in c("n","npub","preg","abort")) d[is.na(get(v)) & year<=2025, (v) := 0]
d[year >= 2022 & is.na(sb), sb := 0]          # stillbirths exist 2022+ only (Cuadro 29 lacks them)
d <- merge(d, pw[, .(adm3_pcode, year, rateA_15_19)], by=c("adm3_pcode","year"))
d[, `:=`(r_deliv = 1000*n/women, r_pub = 1000*npub/women, r_priv = 1000*(n-npub)/women,
         r_preg = 1000*preg/women, r_abort = 1000*abort/women, r_sb = 1000*sb/women)]
cat("panel:", nrow(d), "rows | means: res", round(mean(d$rateA_15_19),1),
    "| deliv-muni", round(mean(d$r_deliv),1), "| preg", round(mean(d$r_preg),1),
    "| abort", round(mean(d$r_abort),2), "\n")

# NIZAO (DOM051702, g=2020) EXCLUDED from the whole bridge (§59.19-59.20): Hospital Municipal
# Nizao expanded services ~2022-24 (teen facility events 1-10/yr through 2021 -> 100 in 2024) —
# a facility-SUPPLY change in a tiny municipality that mechanically spikes facility-based rates
# at long horizons. Same 145-muni sample across all rows; the paper headline elsewhere remains
# the full 146-muni -6.43.
est0 <- merge(d[adm3_pcode != "DOM051702"], tr[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est0[, id := as.integer(factor(adm3_pcode))]
DYN <- list()
run <- function(yv, lab, dat=est0){
  a <- att_gt(yname=yv, tname="year", idname="id", gname="g", xformla=~1, data=dat[!is.na(get(yv))],
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE,
              allow_unbalanced_panel=TRUE)
  g <- aggte(a, type="group", na.rm=TRUE)
  dy <- aggte(a, type="dynamic", na.rm=TRUE)
  DYN[[lab]] <<- data.table(outcome=lab, e=dy$egt, att=dy$att.egt, se=dy$se.egt,
                            crit=dy$crit.val.egt)
  pre <- which(dy$egt < 0 & dy$egt >= -5 & !is.na(dy$att.egt) & !is.na(dy$se.egt) & dy$se.egt > 0)
  w <- 1/dy$se.egt[pre]^2
  ptavg <- sum(dy$att.egt[pre]*w)/sum(w)
  zpre <- dy$att.egt[pre]/dy$se.egt[pre]
  # g-1 baseline among treated (same convention as 07t: mean outcome in year g-1)
  b <- dat[g>0 & year==g-1L & !is.na(get(yv)), mean(get(yv))]
  data.table(outcome=lab, att=round(g$overall.att,3), se=round(g$overall.se,3),
             p=round(2*pnorm(-abs(g$overall.att/g$overall.se)),4),
             base_gm1=round(b,2), pct=round(100*g$overall.att/b,1),
             pre_avg=round(ptavg,3), pre_maxz=round(max(abs(zpre)),2))
}
# (5) stillbirths: 2022+ only -> pre-periods exist only for 2023+ cohorts; short-panel caveat.
# PREDICTION (rule 1b): small negative (mechanically tracks pregnancies; possibly extra decline
# via better care), likely ns at base ~0.5/1,000.
# (2c) PRIVATE-sector births = the SUBSTITUTION margin (MM 2026-07-13: "estimate the private
# decline as spillovers"). PREDICTION (rule 1b): small POSITIVE (~+1.3 = (2)-(2b) wedge).
res <- rbind(run("rateA_15_19","(1) births, RESIDENCE (headline anchor)"),
             run("r_deliv",   "(2) births, DELIVERY muni (BDNV, all facilities)"),
             run("r_pub",     "(2b) births, DELIVERY muni (BDNV, PUBLIC only)"),
             run("r_priv",    "(2c) births, DELIVERY muni (BDNV, PRIVATE/other) [substitution]"),
             run("r_preg",    "(3) pregnancy events, PUBLIC facilities (MISPAS)"),
             run("r_abort",   "(4) abortions, PUBLIC facilities (MISPAS)"),
             run("r_sb",      "(5) stillbirths, PUBLIC facilities (2022+; 2023+ cohorts only)"))
cat("\n=== BRIDGE CS (identical spec; per 1,000 women 15-19; %% vs g-1 treated baseline) ===\n")
print(res, class=FALSE)
fwrite(res, file.path(TAB,"bridge_cs.csv"))
fwrite(rbindlist(DYN), file.path(TAB,"bridge_cs_dynamics.csv"))
cat("saved -> bridge_cs.csv + bridge_cs_dynamics.csv\n")
