# ============================================================================
# fig_lac_ranking_2018.R — LAC adolescent fertility ranking, 2018, World Bank
# WDI SP.ADO.TFRT (CURRENT revised vintage; values fetched from the WB API
# 2026-07-07). DR highlighted. NOTE the vintage caveat: UNFPA's "highest in the
# region" claim for 2018 used the pre-revision WB series (92/1,000); in the
# revised series shown here the DR ranks 8th, still above the LAC average.
# Output: output/figures/fig_b01_lac_ranking.png
# ============================================================================
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
source(here::here("code","R","00_config.R"))
FIG <- file.path(PROJ,"output","figures")

d <- data.table(
  country=c("Nicaragua","Honduras","Guatemala","Guyana","Venezuela","Paraguay","Panama",
            "Dominican Republic","Ecuador","Belize","Bolivia","Colombia","Mexico",
            "El Salvador","Haiti","Brazil","Suriname","Cuba","Argentina","Peru",
            "Costa Rica","Trinidad and Tobago","Jamaica","Uruguay","Chile"),
  rate=c(98.7,85.8,81.7,76.1,75.5,75.2,75.1,72.6,72.5,72.2,69.5,65.2,63.4,
         57.9,54.6,54.3,53.6,52.4,49.9,49.1,49.0,38.4,38.2,37.8,23.7))
lac <- 60.1
d[, dr := country=="Dominican Republic"]
d[, country := factor(country, levels=rev(country))]

g <- ggplot(d, aes(rate, country, fill=dr)) +
  geom_col(width=0.72, show.legend=FALSE) +
  geom_vline(xintercept=lac, linetype="22", color="grey30", linewidth=0.5) +
  annotate("text", x=lac, y=2.2, label=sprintf("LAC average (%.0f)", lac),
           hjust=-0.05, size=3.4, color="grey30", fontface="italic") +
  geom_text(aes(label=sprintf("%.0f", rate)), hjust=-0.15, size=3,
            color="grey25", fontface="bold") +
  scale_fill_manual(values=c(`TRUE`=PCUA_COL$blue, `FALSE`="grey72")) +
  scale_x_continuous(limits=c(0,106), expand=expansion(mult=c(0,0.02))) +
  labs(       x="Births per 1,000 women 15-19", y=NULL) +
  theme_pcua(base_size=12) +
  theme(axis.text.y=element_text(size=9.5,
        face=ifelse(rev(d$dr), "bold", "plain"),
        color=ifelse(rev(d$dr), PCUA_COL$blue, "grey25")),
        panel.grid.major.y=element_blank())
ggsave(file.path(FIG,"fig_b01_lac_ranking.png"), g, width=8.6, height=6.4, dpi=200)
cat("saved -> fig_b01_lac_ranking.png\n")
