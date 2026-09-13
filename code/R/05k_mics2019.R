# ============================================================================
# 05k_mics2019.R — ENHOGAR-MICS6 2019 teen-mother statistics, to complete the
# 2019 (pre) column of Table 1 where the MOH registry lacks coverage
# (education 2020+, union 2021+, prenatal 2021+). Committee request (§44.1).
#
# Files (verified local): analysis/datasets/.../ENHOGAR/2019/
#   MUJERES 15-49 (women, MICS6 codes) + HISTORIA-DE-NACIMIENTO (birth history).
# Verified codings (tabulated 2026-07-09, this session):
#   WB4 age 15-49 | WB5 ever attended (2=never, n=517) | WB6A level: 0 preesc,
#   1 primaria (grades 1-8), 2 secundaria (1-4), 4 superior (1-8) [OLD 8+4
#   structure]; WB6B = highest grade ATTENDED at that level.
#   MA1: 1 married, 2 union libre, 3 neither -> PARTNERED = {1,2}.
#   insurance: 1/2 with 73% at 1 -> 1 = insured (plausibility-checked; DR ~70%+).
#   MN5 = number of ANC visits, LAST live birth in 2y before survey (module
#   structure: MN3A..X provider, MN4AU/AN timing, MN5 count).
#   HH6 1=urban | HH7 = 11 REGIONS (no province/municipio in the public file).
#   wmweight = women's weight. Birth history: BH4C child DOB (CMC), WDOB woman
#   DOB (CMC), WDOI interview (CMC) -> age at birth = (BH4C-WDOB)/12.
# TEEN MOTHERS = women with a live birth in the 24 months before interview,
# aged 15-19 AT THAT BIRTH (n=666 unweighted). Characteristics measured AT
# SURVEY (a few months after birth for most) — flagged in the table notes.
# Education years: never/preesc=0; primaria=WB6B; secundaria=8+WB6B;
# superior=12+WB6B (old 8+4 system, matching the data's own grade ranges).
# CROSS-CHECK vs published report: % of women 15-19 who have begun
# childbearing (ever gave birth or pregnant would need CP module; we check the
# ever-gave-birth part is BELOW the published 20% begun-childbearing).
# Output: data/clean/mics2019_teen_mothers.rds + output/tables/mics2019_teen_mothers.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
MICS <- file.path(DRPAPER, "analysis/datasets/Encuestas de Hogares de Prósitos Múltiples (ENHOGAR)/2019")
TAB <- file.path(PROJ,"output","tables")

w <- fread(file.path(MICS,"ENHOGAR-MICS6-2019-PUB-MUJERES-DE-15-A-49-AÑOS.csv"),
           select=c("HH1","HH2","LN","WB4","WB5","WB6A","WB6B","MA1","insurance","MN5",
                    "wmweight","HH6","HH7","WDOI","WDOB","WM17"))
bh <- fread(file.path(MICS,"ENHOGAR-MICS6-2019-PUB-HISTORIA-DE-NACIMIENTO-MUJERES-DE-15-A-49-AÑOS.csv"),
            select=c("HH1","HH2","LN","BH4C"))
cat("women:", nrow(w), "| birth-history rows:", nrow(bh), "\n")

# ---- education years (verified old 8+4 structure) ----
w[, educ_yrs := fcase(WB5==2, 0,                       # never attended
                      WB6A==0, 0,                      # preescolar only
                      WB6A==1 & WB6B<90, pmin(WB6B, 8),
                      WB6A==2 & WB6B<90, 8 + pmin(WB6B, 4),
                      WB6A==4 & WB6B<90, 12 + pmin(WB6B, 8),
                      default = NA_real_)]
w[, partnered := fifelse(MA1 %in% c(1,2), 1L, fifelse(MA1==3, 0L, NA_integer_))]
w[, no_insurance := fifelse(insurance==2, 1L, fifelse(insurance==1, 0L, NA_integer_))]
w[, anc_visits := fifelse(!is.na(MN5) & MN5<90, as.numeric(MN5), NA_real_)]

# ---- teen mothers: live birth in 24 months before interview, aged 15-19 at birth ----
m <- merge(bh, w[, .(HH1,HH2,LN,WDOI,WDOB)], by=c("HH1","HH2","LN"))
m[, `:=`(age_at_birth = (BH4C - WDOB)/12, mo_before_svy = WDOI - BH4C)]
recent <- m[mo_before_svy >= 0 & mo_before_svy < 24]
teen_ids <- unique(recent[age_at_birth >= 15 & age_at_birth < 20,
                          .(HH1,HH2,LN, age_birth = age_at_birth)][, .SD[which.max(age_birth)], by=.(HH1,HH2,LN)])
tw <- merge(teen_ids, w, by=c("HH1","HH2","LN"))
cat("teen mothers (15-19 at a birth in last 24mo):", nrow(tw), "unweighted\n")
stopifnot(nrow(tw) > 600, !anyNA(tw$wmweight))

wm <- function(x, wt) sum(x*wt, na.rm=TRUE)/sum(wt[!is.na(x)])
stats <- function(d, lab) data.table(group=lab, n_unw=nrow(d),
  age_at_birth = round(wm(d$age_birth, d$wmweight),1),
  educ_years   = round(wm(d$educ_yrs, d$wmweight),1),
  pct_partnered= round(100*wm(d$partnered, d$wmweight),1),
  pct_noins    = round(100*wm(d$no_insurance, d$wmweight),1),
  anc_visits   = round(wm(d$anc_visits, d$wmweight),1),
  pct_educ_missing = round(100*mean(is.na(d$educ_yrs)),1))
res <- rbind(stats(tw, "Teen mothers 15-19 (birth in last 24mo), national"),
             stats(tw[HH6==1], "  urban"), stats(tw[HH6==2], "  rural"))
print(res, class=FALSE)

# ---- cross-checks ----
w1519 <- w[WB4 %in% 15:19 & WM17==1]
evb <- merge(w1519, unique(m[age_at_birth>0, .(HH1,HH2,LN, everbirth=1L)]), by=c("HH1","HH2","LN"), all.x=TRUE)
evb[is.na(everbirth), everbirth := 0L]
cat(sprintf("\ncross-check: %% of women 15-19 who have EVER given birth = %.1f%% (published 'begun childbearing incl. pregnant' = 20%% -> must be below it)\n",
    100*wm(evb$everbirth, evb$wmweight)))
cat(sprintf("cross-check: all-women 15-49 educ years = %.1f | partnered 15-19 (all, not just mothers) = %.1f%%\n",
    wm(w[WM17==1, educ_yrs], w[WM17==1, wmweight]),
    100*wm(w1519$partnered, w1519$wmweight)))
# MICS-calibrated years per BDNV-style category (for the BDNV mapping check)
w[, lev := fcase(WB5==2 | WB6A==0, "None", WB6A==1, "Primary", WB6A==2, "Secondary", WB6A==4, "University", default=NA_character_)]
cal <- w[!is.na(lev) & !is.na(educ_yrs) & WM17==1, .(mean_yrs=round(sum(educ_yrs*wmweight)/sum(wmweight),1)), keyby=lev]
cat("\nMICS-calibrated mean years by attainment level (checks the BDNV 0/6/12/16 mapping):\n"); print(cal, class=FALSE)

saveRDS(list(teen=res, calibration=cal), file.path(DIR_CLEAN,"mics2019_teen_mothers.rds"))
fwrite(res, file.path(TAB,"mics2019_teen_mothers.csv"))
cat("\nsaved -> data/clean/mics2019_teen_mothers.rds + output/tables/mics2019_teen_mothers.csv\n")
