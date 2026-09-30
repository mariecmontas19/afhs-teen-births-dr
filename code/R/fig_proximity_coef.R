# ============================================================================
# fig_proximity_coef.R — clean 2-coefficient version of the proximity result for
# the deck (replaces the faceted event-study fig_proximity_mechanism, whose CIs
# read poorly on a slide). Numbers straight from het_master_table.csv row
# "Nearest facility (km)": att_s = NEAR (below-median distance to any facility),
# att_u = FAR (above-median). 95% CI = att ± 1.96·se. Diff = gap, p_diff.
# Output: output/figures/fig_06b_proximity.png (Panel B of the combined het figure, MM 2026-08-08)
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures")

hm <- fread(file.path(PROJ,"output","tables","het_master_table.csv"))[label=="Nearest facility (km)"]
stopifnot(nrow(hm)==1)
d <- data.table(
  grp = factor(c("Near a health facility\n(below-median distance)","Far from a health facility\n(above-median distance)"),
               levels=c("Far from a health facility\n(above-median distance)","Near a health facility\n(below-median distance)")),
  att = c(hm$att_s, hm$att_u), se = c(hm$se_s, hm$se_u), n = c(hm$n_s, hm$n_u))
d[, `:=`(lo=att-1.96*se, hi=att+1.96*se)]
cat("near:", d[grp %like% "Near", att], "far:", d[grp %like% "Far", att], "| diff:", hm$gap, "p:", hm$p_diff, "\n")

g <- ggplot(d, aes(att, grp)) +
  geom_vline(xintercept=0, color="grey40", linewidth=0.6) +
  geom_vline(xintercept=-6.43, linetype="22", color="grey35", linewidth=0.6) +
  annotate("text", x=-6.43, y="Far from a health facility\n(above-median distance)", label="overall effect (-6.4)",
           hjust=-0.05, vjust=5.5, size=3.4, color="grey35", fontface="italic") +
  geom_linerange(aes(xmin=lo, xmax=hi), linewidth=1.4, color=c(PCUA_COL$blue, "grey45")) +
  geom_point(size=6, color=c(PCUA_COL$blue, "grey45")) +
  geom_text(aes(label=sprintf("%.1f", att)), vjust=-1.4, size=4.6, fontface="bold",
            color=c(PCUA_COL$blue, "grey35")) +
  annotate("text", x=hm$att_s, y=2.45, hjust=0.5, size=3.6, color=PCUA_COL$ink,
           label=sprintf("difference: %.1f births per 1,000 (p = %.3f)", hm$gap, hm$p_diff)) +
  scale_x_continuous(breaks=seq(-16,4,4), limits=c(-17,5)) +
  labs(x="ATT: teen births per 1,000 women (15-19), with 95% CI", y=NULL) +
  theme_pcua(base_size=16) + theme(axis.text.y=element_text(size=14, face="bold", lineheight=0.95))
ggsave_pair(file.path(FIG,"fig_06b_proximity.png"), g, width=9.4, height=4.8, dpi=200)
cat("saved -> fig_06b_proximity.png\n")
