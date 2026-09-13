# ============================================================================
# fig_het_dumbbell_v2.R — deck restyle of the 4-dimension heterogeneity dumbbell
# (08l): adds a RED dashed line at the overall effect (-6.43) with a label, and
# bold axis text. Numbers straight from het_master_table.csv (in_dumbbell rows).
# Green = more-underserved subgroup (att_u), orange = less-underserved (att_s).
# Output: output/figures/fig_het_dumbbell_v2.png
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures")

hm <- fread(file.path(PROJ,"output","tables","het_master_table.csv"))[in_dumbbell==TRUE]
stopifnot(nrow(hm)==4)
lab_map <- c("Unmet need (FP)"="Unmet contraceptive need",
             "Wealth (census)"="Wealth",
             "Rural vs urban"="Rural or urban",
             "All facilities /capita"="Health facilities per capita")
hm[, lab := factor(lab_map[label], levels=rev(lab_map))]
cat("rows:", paste(hm$label, collapse=" | "), "\n")

g <- ggplot(hm) +
  geom_vline(xintercept=0, color="grey40", linewidth=0.6) +
  geom_vline(xintercept=-6.43, linetype="22", color=PCUA_COL$red, linewidth=0.6) +
  annotate("text", x=-6.43, y="Unmet contraceptive need", label="overall effect (-6.4)", hjust=1.05, vjust=-3.2,
           size=3.5, color=PCUA_COL$red, fontface="italic") +
  geom_segment(aes(x=att_u, xend=att_s, y=lab, yend=lab), color="grey80", linewidth=1.6) +
  geom_point(aes(att_u, lab, color="More underserved"), size=5) +
  geom_point(aes(att_s, lab, color="Less underserved"), size=5) +
  geom_text(aes(att_u, lab, label=sprintf("%.1f",att_u)), color=PCUA_COL$green, vjust=-1.2, size=3.6, fontface="bold") +
  geom_text(aes(att_s, lab, label=sprintf("%.1f",att_s)), color=PCUA_COL$orange, vjust=-1.2, size=3.6, fontface="bold") +
  scale_color_manual(values=c("More underserved"=PCUA_COL$green, "Less underserved"=PCUA_COL$orange), name=NULL) +
  scale_x_continuous(breaks=seq(-10,0,2)) +
  labs(title="Effects lean larger where need is higher and access scarcer",
       subtitle="Overall ATT on the 15-19 birth rate; treated municipalities split at the median of each baseline characteristic",
       x="ATT: teen births per 1,000 women (15-19)", y=NULL,
       caption="Callaway-Sant'Anna group ATT per subgroup (~10 treated municipalities each) vs. not-yet-treated controls; municipality-clustered SE.\nSubgroup differences are not individually significant at 5% (suggestive); the proximity split on the next slide is the robust one.") +
  theme_pcua(base_size=14) +
  theme(axis.text.y=element_text(size=12.5, face="bold"),
        axis.title.x=element_text(face="bold"),
        legend.position="top")
ggsave(file.path(FIG,"fig_het_dumbbell_v2.png"), g, width=9.6, height=5.2, dpi=200)
cat("saved -> fig_het_dumbbell_v2.png\n")
