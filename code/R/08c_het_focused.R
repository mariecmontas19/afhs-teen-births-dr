# ============================================================================
# 08c_het_focused.R — paper figure: the TWO robust heterogeneity dimensions only
# (rural and few health-centres-per-capita), shown for BOTH S1 and S2. These are the
# moderators whose "underserved -> larger effect" holds consistently across scenarios
# (and health-centres is the tightest-CI split). Reads heterogeneity.rds (from 08).
# Output: output/figures/fig_heterogeneity_focused.png
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures")

H <- as.data.table(readRDS(file.path(DIR_CLEAN,"heterogeneity.rds")))
keep <- c("Urban share","Health centres /10k")
HF <- H[moderator %in% keep]
HF[, mod := factor(fifelse(moderator=="Urban share","Rural vs urban","Health centres per capita"),
                   levels=c("Rural vs urban","Health centres per capita"))]
HF[, group := factor(group, levels=c("More underserved","Less underserved"))]
HF[, scen := factor(scenario, levels=c("S2","S1"),
                    labels=c("S2: any new/modernized unit","S1: first AU opening"))]

g <- ggplot(HF, aes(att, scen, color=group)) +
  geom_vline(xintercept=0, color="grey55") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=0, linewidth=1, position=position_dodge(0.5)) +
  geom_point(size=3.8, position=position_dodge(0.5)) +
  geom_text(aes(label=sprintf("%+.1f", att)), position=position_dodge(0.5), vjust=-1.0, size=3.1, show.legend=FALSE) +
  facet_wrap(~mod, ncol=1) +
  scale_color_manual(values=c("More underserved"=PCUA_COL$green, "Less underserved"=PCUA_COL$orange),
                     labels=c("More underserved (rural / few centres)","Less underserved (urban / many centres)"), name=NULL) +
  labs(title="Where the unit does more: rural areas and places with few health centres",
       subtitle="Subgroup CS ATT on the 15-19 birth rate; in-window treated split at the median. Consistent across S1 and S2.",
       x="ATT: teen births per 1,000 women (15-19)", y=NULL,
       caption="Denominator A; not-yet controls; 95% CIs; municipality-clustered. ~10 (S1) / ~14 (S2) treated per half.") +
  theme_pcua() + theme(panel.spacing=unit(14,"pt"))
ggsave(file.path(FIG,"fig_heterogeneity_focused.png"), g, width=9.2, height=5.8, dpi=200)
cat("saved -> fig_heterogeneity_focused.png\n"); print(HF[order(mod,scen,group), .(mod, scen, group, att, se)], class=FALSE)
