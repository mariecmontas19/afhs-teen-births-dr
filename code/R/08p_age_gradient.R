# ============================================================================
# 08p_age_gradient.R — R4 centerpiece: effect by mother's AGE BAND, the
# spillover-gradient exhibit (methods §49.3). CS group ATT on each band's OWN
# rate (rateA_15_19 / 20_24 / 25_29 / 30_34), full window 2016-2025, S1,
# spec = 07t. Plotted as % of the g-1 baseline with 95% CIs so bands with
# different fertility levels are comparable. Prediction (§49): monotone decay
# 15-19 > 20-24 > 25-29 >= 30-34 = 0.
# Output: output/tables/age_gradient.csv + output/figures/fig_age_gradient.png
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[,
       .(adm3_pcode, year, rateA_10_14, rateA_15_19, rateA_20_24, rateA_25_29,
         rateA_30_34, rateA_35_39, rateA_40_44)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
est0 <- merge(p, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est0[, id := as.integer(factor(adm3_pcode))]

one <- function(yname, lab){
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
        xformla=~1, data=est0, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  bs <- est0[g>0 & year==g-1L, mean(get(yname))]
  data.table(band=lab, ATT=round(o$overall.att,2), SE=round(o$overall.se,2),
             p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3), baseline=round(bs,2),
             pct=round(100*o$overall.att/bs,1),
             pct_lo=round(100*(o$overall.att-1.96*o$overall.se)/bs,1),
             pct_hi=round(100*(o$overall.att+1.96*o$overall.se)/bs,1))
}
# The four original bands run FIRST, in the original order, under the original
# script-level seed, so their published bootstrap SEs reproduce byte-identically.
res4 <- rbind(one("rateA_15_19","15-19"), one("rateA_20_24","20-24"),
              one("rateA_25_29","25-29"), one("rateA_30_34","30-34"))
# Extension (2026-08-05, MM): all seven reproductive age bands. New bands are
# seeded immediately before each att_gt (the current convention), so they are
# order-independent. PREDICTIONS (rule 1b), stated before running: 10-14 is
# inside the units' service range (10-19) but its base rate is ~2 per 1,000,
# so expect a noisy zero; 35-39 and 40-44 cannot be exposed in any study year
# (like 30-34), so both must be ~0 — a significant estimate there is an
# instrument check, not a finding.
one_seeded <- function(yname, lab, seed){ set.seed(seed); one(yname, lab) }
res_new <- rbind(one_seeded("rateA_10_14","10-14",20260805L),
                 one_seeded("rateA_35_39","35-39",20260806L),
                 one_seeded("rateA_40_44","40-44",20260807L))
res <- rbind(res_new[band=="10-14"], res4, res_new[band!="10-14"])

# Pre-trend column (2026-08-05): average of the dynamic leads e in [-5,-2] with
# its ANALYTIC influence-function SE (es_pretrend_p; deterministic, so these
# re-runs cannot perturb the canonical group-ATT bootstrap stream above).
# Motivated by the 35-39 surprise: its group ATT (-4.40, p=.009) sits on a
# pre-existing differential decline (leads all negative), so the table must
# show per-band lead evidence rather than assert cleanliness in prose.
BANDS <- c(`10-14`="rateA_10_14", `15-19`="rateA_15_19", `20-24`="rateA_20_24",
           `25-29`="rateA_25_29", `30-34`="rateA_30_34", `35-39`="rateA_35_39",
           `40-44`="rateA_40_44")
pt <- rbindlist(lapply(seq_along(BANDS), function(i){
  set.seed(20260810L + i)
  a <- suppressWarnings(suppressMessages(att_gt(yname=BANDS[i], tname="year", idname="id", gname="g",
        xformla=~1, data=est0, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  dy <- suppressMessages(aggte(a, type="dynamic", na.rm=TRUE))
  z  <- es_pretrend_p(dy)
  data.table(band=names(BANDS)[i], pre_avg=round(z$avg,2), pre_se=round(z$se,2), pre_p=round(z$p,3))
}))
res <- merge(res, pt, by="band", sort=FALSE)
res[, band := factor(band, levels=names(BANDS))]; setorder(res, band); res[, band := as.character(band)]
cat("\n=== age-band gradient (2016-25; S1; own denominators; 7 bands) ===\n"); print(res, class=FALSE)
fwrite(res, file.path(TAB,"age_gradient.csv"))

# ---- figure: % of baseline with 95% CI, point + error bar ----
# 2026-09-09 (§59.94/96): figure now STOPS AT 30-34, matching Table A8 and the
# paper (35-39/40-44 dropped everywhere per MM; the old 6-band figure was stale
# and nearly reached a MoH deck). 10-14 excluded from the % plot: its base rate
# (~2/1,000) makes the percent scale explode; it stays in the csv. Colors moved
# to the blue/grey convention (§59.73; the old red predates the rule).
fig <- res[band %in% c("15-19","20-24","25-29","30-34")]
fig[, band := factor(band, levels=c("15-19","20-24","25-29","30-34"))]
cols <- c(`15-19`=PCUA_COL$blue, `20-24`="grey45", `25-29`="grey55",
          `30-34`="grey55")
colv <- cols[as.character(fig$band)]
g <- ggplot(fig, aes(band, pct)) +
  geom_hline(yintercept=0, color="grey25", linewidth=0.7) +
  geom_errorbar(aes(ymin=pct_lo, ymax=pct_hi), width=0.12, linewidth=0.9, color=colv) +
  geom_point(size=4.5, shape=21, stroke=1.5, fill="white", color=colv) +
  geom_text(aes(label=sprintf("%.0f%%", pct)), vjust=-1.3, size=4.2, fontface="bold",
            color=fifelse(as.character(fig$band) %in% c("15-19","20-24"), colv, "grey40")) +
  annotate("text", x=4, y=max(fig$pct_hi)+3, vjust=1, hjust=0.5, size=3.3,
           fontface="italic", color="grey35",
           label="comparison group:\nzero exposure by construction") +
  labs(title="The effect declines with the mother's age and vanishes by 30",
       subtitle="Callaway-Sant'Anna group ATT on each age band's own birth rate, as % of its pre-opening baseline, with 95% CI",
       x="Mother's age band", y="Effect (% of the band's g-1 baseline rate)",
       caption=paste0("First AU opening (20 treated municipalities), 2016-2025, not-yet-treated controls, municipality-clustered SEs.\n",
                      "Cohort arithmetic predicts this ordering: treated adolescents can age at most to 24 by 2025, so 20-24 is mechanically\n",
                      "reachable, 25-29 marginally (modernization margin), and 30-34 cannot be exposed in any study year.")) +
  scale_y_continuous(limits=c(min(fig$pct_lo)-4, max(fig$pct_hi)+6)) +
  theme_pcua(base_size=13)
ggsave(file.path(FIG,"fig_age_gradient.png"), g, width=8.6, height=5.4, dpi=200)
cat("saved -> age_gradient.csv + fig_age_gradient.png\n")
