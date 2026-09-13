# ============================================================================
# 08x4_bridge_quarterly.R — QUARTERLY bridge (reserve item promoted by MM,
# 2026-07-13): CS at quarterly frequency on the MISPAS muni-quarter panel
# (2022Q1-2026Q1, publico, ex-Nizao per §59.20). The gestational-lag test in
# the facility data: cohorts = OPENING QUARTER.
# SAMPLE: 2022+ cohorts with KNOWN opening month (12 treated: 2022:3, 2023:3,
# 2024:5, 2025:1; the 4 unknown-month treated munis are EXCLUDED — their
# quarter cannot be assigned; 2020-21 cohorts are already-treated before the
# panel starts and are excluded by construction). Controls = not-yet-treated +
# never-treated. Rates per 1,000 women 15-19 PER QUARTER (annual denominator
# carried within year; multiply by 4 for annualized-equivalent).
# PREDICTIONS (rule 1b), stated before running:
#   PREGNANCY EVENTS (deliveries+abortions): ~0 for q0-q2 (children conceived
#     before opening still arrive), decline emerging from ~q+3/q+4 — the
#     quarterly signature of a conception-margin effect.
#   ABORTIONS: should respond FASTER than deliveries (losses occur in early
#     pregnancy): decline visible by ~q+1/q+2 if conceptions fall on opening.
#   Group ATTs smaller in %% than the annual bridge (early flat quarters
#     included in the average).
# Output: output/tables/bridge_quarterly.csv (+ dynamics)
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
TAB <- file.path(PROJ,"output","tables")
fold <- c("DOM010905","DOM051703","DOM012510")

mp <- readRDS(file.path(DIR_CLEAN,"mispas_muni_panels.rds"))$quarter
mp[, `:=`(anio = as.integer(substr(qtr,1,4)), q = as.integer(substr(qtr,6,6)))]
setnames(mp, c("deliv_a1519","abort_a1519"), c("deliv_1519","abort_1519"), skip_absent=TRUE)
mp[, tq := anio*4L + q]
pw <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold &
        year %between% c(2022,2025), .(adm3_pcode, anio=year, women=womenA_15_19)]
pw25 <- pw[anio==2025][, anio := 2026L]                       # carry 2025 denominator to 2026Q1
pw <- rbind(pw, pw25)
grid <- CJ(adm3_pcode = unique(pw$adm3_pcode), tq = sort(unique(mp$tq)))
grid[, `:=`(anio = tq %/% 4L, q = tq %% 4L)][q==0, `:=`(anio=anio-1L, q=4L)]
d <- merge(grid, pw, by=c("adm3_pcode","anio"), all.x=TRUE)
d <- merge(d, mp[, .(adm3_pcode, tq, deliv_1519, abort_1519)], by=c("adm3_pcode","tq"), all.x=TRUE)
for(v in c("deliv_1519","abort_1519")) d[is.na(get(v)), (v) := 0]
d <- d[adm3_pcode != "DOM051702"]                             # ex-Nizao (§59.20)
d[, `:=`(r_preg = 1000*(deliv_1519+abort_1519)/women, r_abort = 1000*abort_1519/women)]
cat("panel:", nrow(d), "muni-quarters |", d[,uniqueN(adm3_pcode)], "munis |",
    d[,uniqueN(tq)], "quarters\n")

tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                  fifelse(ever_treated==0L, 0L, NA_integer_))]
tr[, gq := fifelse(g==0L, 0L, fifelse(!is.na(first_month) & g>=2022,
                                       g*4L + (as.integer(first_month)-1L) %/% 3L + 1L, NA_integer_))]
drop_unknown <- tr[g>=2022 & is.na(gq), adm3_pcode]
cat("treated 2022+ with known quarter:", tr[!is.na(gq) & gq>0, .N],
    "| dropped (unknown month):", length(drop_unknown), "| pre-2022 cohorts excluded:", tr[g %in% 2020:2021, .N], "\n")
est <- merge(d, tr[!is.na(gq), .(adm3_pcode, gq)], by="adm3_pcode")
est <- est[!adm3_pcode %in% drop_unknown]
est[, id := as.integer(factor(adm3_pcode))]

run <- function(yv, lab){
  a <- att_gt(yname=yv, tname="tq", idname="id", gname="gq", xformla=~1, data=est,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  g <- aggte(a, type="group", na.rm=TRUE)
  dy <- aggte(a, type="dynamic", na.rm=TRUE)
  bl <- est[gq>0 & tq==gq-1L, mean(get(yv))]
  cat(sprintf("\n%s: group ATT %+.3f (SE %.3f, p=%.4f) = %+.1f%% of q-1 baseline %.2f\n",
      lab, g$overall.att, g$overall.se, 2*pnorm(-abs(g$overall.att/g$overall.se)),
      100*g$overall.att/bl, bl))
  dd <- data.table(outcome=lab, e=dy$egt, att=dy$att.egt, se=dy$se.egt)
  print(dd[e %between% c(-6,7), .(e, att=round(att,2), se=round(se,2))], class=FALSE)
  list(row=data.table(outcome=lab, att=round(g$overall.att,3), se=round(g$overall.se,3),
       p=round(2*pnorm(-abs(g$overall.att/g$overall.se)),4), base_qm1=round(bl,2),
       pct=round(100*g$overall.att/bl,1)), dyn=dd)
}
r1 <- run("r_preg",  "pregnancy events /1k per quarter (PUBLIC, ex-Nizao)")
r2 <- run("r_abort", "abortions /1k per quarter (PUBLIC, ex-Nizao)")
fwrite(rbind(r1$row, r2$row), file.path(TAB,"bridge_quarterly.csv"))
fwrite(rbind(r1$dyn, r2$dyn), file.path(TAB,"bridge_quarterly_dynamics.csv"))
cat("\nsaved -> bridge_quarterly.csv + bridge_quarterly_dynamics.csv\n")
