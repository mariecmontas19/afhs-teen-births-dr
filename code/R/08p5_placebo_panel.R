# ============================================================================
# 08p5_placebo_panel.R — 3-panel own-rate event studies near the eligibility
# boundary: 20-24, 25-29, 30-34 (MM 2026-08-12, §59.94: replaces the earlier
# 25-29/30-34/35-39/40-44 placebo panel; the unexposable older bands 35-39/
# 40-44 are dropped, and 20-24 — the aged-out band that DOES respond — is added
# so the panel reads as the age gradient from responding [20-24] through
# marginal [25-29] to the clean comparison group [30-34]). Same spec as 08p:
# S1, not-yet-treated controls, universal base, seeded per att_gt call.
# No in-image caption (MM: notes live in the LaTeX caption).
# Output: output/figures/fig_b05_placebo_ages.png
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
fold <- c("DOM010905","DOM051703","DOM012510")
FIG <- file.path(PROJ,"output","figures")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[,
       .(adm3_pcode, year, rateA_20_24, rateA_25_29, rateA_30_34)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
est0 <- merge(p, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est0[, id := as.integer(factor(adm3_pcode))]

BANDS <- c(`20-24`="rateA_20_24", `25-29`="rateA_25_29", `30-34`="rateA_30_34")
dd <- rbindlist(lapply(seq_along(BANDS), function(i){
  set.seed(20260820L + i)
  a <- suppressWarnings(suppressMessages(att_gt(yname=BANDS[i], tname="year", idname="id", gname="g",
        xformla=~1, data=est0, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  dy <- suppressMessages(aggte(a, type="dynamic", na.rm=TRUE))
  z  <- es_pretrend_p(dy)
  data.table(band=names(BANDS)[i], e=dy$egt, att=dy$att.egt, se=dy$se.egt, crit=dy$crit.val.egt,
             pre_avg=z$avg, pre_p=z$p)
}))
dd <- dd[e>=-5 & e<=4]
dd[, `:=`(lo=att-crit*se, hi=att+crit*se)]
ann <- unique(dd[, .(band, pre_avg, pre_p)])
ann[, lab := sprintf("Avg lead %+.1f (p=%.2f)", pre_avg, pre_p)]
dd[, band := factor(band, levels=names(BANDS))]; ann[, band := factor(band, levels=names(BANDS))]
rng <- ceiling(max(abs(c(dd$lo, dd$hi)), na.rm=TRUE)/5)*5

g <- ggplot(dd, aes(e, att)) +
  geom_vline(xintercept=-0.5, linetype="dashed", color="grey55") +
  geom_hline(yintercept=0, color="grey25", linewidth=0.7) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0.14, linewidth=0.6, color=PCUA_COL$blue, na.rm=TRUE) +
  geom_point(color=PCUA_COL$blue, fill="white", shape=21, size=2.4, stroke=1.1, na.rm=TRUE) +
  geom_text(data=ann, aes(x=-5, y=rng*0.9, label=lab), hjust=0, size=3.1,
            fontface="italic", color="grey25", inherit.aes=FALSE) +
  facet_wrap(~band, ncol=2) +                          # 20-24 & 25-29 on top, 30-34 below
  scale_x_continuous(breaks=seq(-5,4,1)) +
  scale_y_continuous(limits=c(-rng, rng)) +
  labs(x="Years since first AU opening", y="ATT: births per 1,000 women in the band") +
  theme_pcua(base_size=12)
ggsave_pair(file.path(FIG,"fig_b05_placebo_ages.png"), g, width=7.6, height=6.6, dpi=200)
cat("saved -> fig_b05_placebo_ages.png\n")
print(ann, class=FALSE)
