# ============================================================================
# 07z4_sd_lab.R — the SANTO DOMINGO laboratory (descriptive / illustrative; NOT a
# causal within-SD estimate — only 8 SD+DN municipalities, 1 never-treated, and the same
# urban pre-trend risk as S2). SD+DN = 30.4% of all teen births = the densest data.
#   SD3a MAP: SD+DN metro, municipalities by AU timing, unit municipalities marked.
#   SD3b TRENDS: annual teen birth rate 2016-2025 for the in-window SD municipalities,
#        vertical markers at each opening year.
#   SD4  MONTHLY GESTATIONAL-LAG FALSIFICATION (pooled over ALL in-window units, not
#        SD-only — SD alone is too thin): a unit cannot affect births until conceptions
#        AFTER opening come to term (~9 months). Within-municipality-demeaned monthly teen
#        rate by EVENT MONTH; a drop should appear only after ~month +9, not before.
# Output: output/figures/fig_sd_map.png, fig_sd_trends.png, fig_gestlag_falsification.png
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(sf); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures")
SDP <- c("SANTO DOMINGO","DISTRITO NACIONAL")

g   <- readRDS(file.path(DIR_CLEAN,"muni_geom.rds"))
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))

# ---- SD3a: SD metro map ----
sd <- trt[prov_norm %in% SDP]
sd[, status := fifelse(always_treated==1L, "Unit before 2016",
                  fifelse(ever_treated==1L, paste0("Opened ", first_year), "Never"))]
# Distrito Nacional is stored as municipality "SANTO DOMINGO DE GUZMAN" — display as "Distrito Nacional"
relab <- function(pc, nm) data.table::fifelse(pc=="DOM100101", "Distrito Nacional", stringr::str_to_title(nm))
gsd <- merge(g[g$adm3_pcode %in% sd$adm3_pcode, ], sd[, .(adm3_pcode, adm3_name, status, ever_treated)], by="adm3_pcode")
slev <- c("Unit before 2016","Opened 2020","Opened 2022","Opened 2024","Never")
gsd$status <- factor(gsd$status, levels=slev)
svals <- c("Unit before 2016"="#00441B","Opened 2020"="#238B45","Opened 2022"="#66C2A4",
           "Opened 2024"="#B7E4C7","Never"="grey85")
m1 <- ggplot(gsd) + geom_sf(aes(fill=status), color="white", linewidth=0.45) +
  geom_sf_text(aes(label=relab(adm3_pcode, adm3_name)), size=2.7, color="grey15", fontface="bold") +
  scale_fill_manual(values=svals, name="AU timing", drop=FALSE) +
  labs(title="Santo Domingo metro: the densest setting (30% of all teen births)",
       subtitle="8 municipalities coloured by when their AU opened - 3 before 2016, 4 in-window, 1 never.",
       caption="Illustrative geography; within-SD causal inference is not feasible (only 1 never-treated municipality).") +
  theme_pcua_map() + theme(legend.position="right")
ggsave(file.path(FIG,"fig_sd_map.png"), m1, width=8.5, height=6, dpi=200)

# ---- SD3b: annual teen-rate trends, ALL ever-treated SD+DN municipalities (incl. the 3
#      always-treated: Distrito Nacional, SD Este, SD Oeste). Dashed opening markers only
#      for in-window openings (first_year >= 2016); the always-treated opened pre-window.
sd_all <- sd[ever_treated==1L, .(adm3_pcode, adm3_name, first_year)]
tr <- merge(p[, .(adm3_pcode, year, rateA_15_19)], sd_all, by="adm3_pcode")
tr[, muni := relab(adm3_pcode, adm3_name)]
sd_open <- sd_all[first_year >= 2016]                       # in-window openings only
m2 <- ggplot(tr, aes(year, rateA_15_19, color=muni)) +
  geom_vline(data=sd_open, aes(xintercept=first_year, color=relab(adm3_pcode, adm3_name)),
             linetype="22", linewidth=0.5, alpha=0.7, show.legend=FALSE) +
  geom_line(linewidth=1) + geom_point(size=1.8, shape=21, fill="white", stroke=1) +
  scale_x_continuous(breaks=2016:2025) +
  scale_color_manual(values=unname(PCUA_QUAL)) +
  labs(title="Teen birth-rate trends across Santo Domingo + Distrito Nacional, 2016-2025",
       subtitle="All 7 municipalities with an AU. Dashed line = an in-window opening year (matched colour); the 3 always-treated opened before 2016.",
       x=NULL, y="Teen births per 1,000 women (15-19)", color=NULL,
       caption="Denominator A. Illustrative trends, not a causal estimate - several municipalities were already declining before opening (why within-SD causal inference is unreliable).") +
  theme_pcua()
ggsave(file.path(FIG,"fig_sd_trends.png"), m2, width=9.4, height=5.4, dpi=200)

# NOTE: the gestational-lag falsification MOVED to 07z6_gestlag_national.R — it is a
# NATIONAL test (not SD), now strengthened with stacked always-treated modernization
# event-windows. This script keeps only the SD descriptive figures.
cat("\nsaved -> fig_sd_map.png, fig_sd_trends.png  (gestational-lag -> 07z6_gestlag_national.R)\n")
