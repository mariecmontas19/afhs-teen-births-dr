# ============================================================================
# 07d_placebo_3034.R — PLACEBO: CS estimator on the 30-34 rate (denom A).
# AUs target adolescents, so the comparison age group 30-34 should show NO
# effect. A null here (ATT~0, flat pre AND post) confirms (i) 30-34 is an
# uncontaminated comparison and (ii) the teen decline is age-specific, not a
# generic municipality shock. Same spec as the headline (not-yet controls,
# universal base, municipality-clustered bootstrap + uniform bands).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722)
FIG <- file.path(PROJ,"output","figures")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
est <- p[always_treated==0]
est[, id := as.integer(factor(adm3_pcode))]
est[, g  := fifelse(ever_treated==1L, as.integer(first_year), 0L)]

a <- att_gt(yname="rateA_30_34", tname="year", idname="id", gname="g", xformla=~1, data=est,
            control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=2000,
            base_period="universal", clustervars="id", est_method="reg")
gr <- aggte(a, type="group",   na.rm=TRUE)
dy <- aggte(a, type="dynamic", na.rm=TRUE)

cat("==================== 07d PLACEBO: 30-34 rate (denom A) ====================\n")
cat(sprintf("group ATT = %.3f  (SE %.3f)   [headline teen-15-19 ATT = -5.32]\n", gr$overall.att, gr$overall.se))
cat(sprintf("dynamic overall ATT = %.3f (SE %.3f)\n", dy$overall.att, dy$overall.se))
cat("event-study (e : att):\n"); print(data.table(e=dy$egt, att=round(dy$att.egt,2), se=round(dy$se.egt,2)), class=FALSE)

ed <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt, crit=dy$crit.val.egt)[e>=-5 & e<=4]
ed[, `:=`(lo=att-crit*se, hi=att+crit*se)]
g <- ggplot(ed, aes(e, att)) +
  geom_hline(yintercept=0, color="grey60") + geom_vline(xintercept=-0.5, linetype="dashed", color="grey60") +
  geom_ribbon(aes(ymin=lo, ymax=hi), alpha=0.15, fill="#2c7fb8") +
  geom_line(color="#2c7fb8") + geom_point(color="#2c7fb8", size=1.6) +
  labs(title="PLACEBO event study: 30-34 birth rate (denom A)",
       subtitle="comparison age group — expect no effect if 30-34 is uncontaminated",
       x="Years since first AU opened", y="ATT (births per 1,000 women)",
       caption="Callaway-Sant'Anna; not-yet-treated controls; uniform 95% bands; clustered at municipality.") +
  theme_minimal(base_size=11) + theme(plot.title=element_text(face="bold"))
ggsave(file.path(FIG,"fig7_placebo_3034.pdf"), g, width=8, height=5)
ggsave(file.path(FIG,"fig7_placebo_3034.png"), g, width=8, height=5, dpi=150)
saveRDS(list(group=gr, dynamic=dy), file.path(DIR_CLEAN,"placebo_3034.rds"))
cat("\nsaved -> output/figures/fig7_placebo_3034.{pdf,png}, data/clean/placebo_3034.rds\n")
