# ============================================================================
# 04d_rollout_viz.R — visual roll-out of all 39 AU/UAIA units by province /
# municipality and year, colored by modernization type (new opening / post-2016
# upgrade / old never-modernized). One point per unit at its modernization year
# (old never-modernized units shown at their recorded 1990s opening year).
# Output: output/figures/pcua_rollout_timeline.{pdf,png}
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2); library(stringi)})
FIG <- file.path(PROJ,"output","figures")

u   <- as.data.table(readRDS(file.path(DIR_CLEAN,"pcua_units_mapped.rds")))
ver <- fread(file.path(PROJ,"notes","pcua_unit_dates_verified.csv"))
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))

u <- merge(u, ver[, .(unit_id, modern_event_date, modern_event_type, never_modernized)], by="unit_id")
u <- merge(u, trt[, .(adm3_pcode, adm3_name, prov_norm, mod_cohort)], by="adm3_pcode")
u[, mev_year := as.integer(substr(modern_event_date,1,4))]
u[, open_year := fcase(never_modernized==TRUE, as.integer(year),       # old units at recorded yr
                       !is.na(mev_year),       mev_year,
                       !is.na(year),           as.integer(year),
                       default =               2023L)]
u[, type := fcase(never_modernized==TRUE,                                "Old unit, never modernized",
                  modern_event_type %in% c("upgrade","upgrade_unconfirmed"), "Post-2016 upgrade (Abinader-era remodel)",
                  default =                                               "New full-service opening")]
u[, type := factor(type, levels=c("New full-service opening",
                                  "Post-2016 upgrade (Abinader-era remodel)",
                                  "Old unit, never modernized"))]

# pretty labels; order municipalities by province then cohort
lab <- function(x) stri_trans_totitle(stri_trans_general(x,"Latin-ASCII"))
u[, muni_lab := lab(adm3_name)][, prov_lab := lab(prov_norm)]
ord <- unique(u[, .(prov_lab, muni_lab, adm3_pcode, mod_cohort)])[order(prov_lab, mod_cohort, muni_lab)]
u[, muni_lab := factor(muni_lab, levels=rev(ord$muni_lab))]

cols <- c("New full-service opening"=PCUA_COL$blue,
          "Post-2016 upgrade (Abinader-era remodel)"=PCUA_COL$orange,
          "Old unit, never modernized"="#999999")

g <- ggplot(u, aes(open_year, muni_lab, color=type, shape=type)) +
  annotate("rect", xmin=2015.5, xmax=2025.5, ymin=-Inf, ymax=Inf, fill="#f0f0f0", alpha=0.5) +
  geom_vline(xintercept=2016, linetype="dotted", color="grey50") +
  geom_vline(xintercept=2020, linetype="dashed", color="grey40") +
  annotate("text", x=2020, y=Inf, label="Abinader admin.", vjust=-0.4, size=2.7, color="grey30") +
  geom_point(size=2.6, stroke=0.9) +
  scale_color_manual(values=cols, name=NULL) +
  scale_shape_manual(values=c(16,17,4), name=NULL) +
  scale_x_continuous(breaks=seq(1992,2026,2), limits=c(1992,2026.5)) +
  labs(title="Roll-out of AU / UAIA adolescent units in the Dominican Republic",
       subtitle=sprintf("%d units by province / municipality and year; grey band = 2016-2025 study window", nrow(u)),
       x="Year unit opened or was modernized", y=NULL,
       caption="Source: notes/pcua_unit_dates_verified.csv (web + field + photo evidence). Old never-modernized units shown at recorded 1990s opening.") +
  theme_pcua(base_size=10) +
  theme(legend.text=element_text(size=8), axis.text.y=element_text(size=7.3))

ggsave(file.path(FIG,"pcua_rollout_timeline.pdf"), g, width=9, height=8.5)
ggsave(file.path(FIG,"pcua_rollout_timeline.png"), g, width=9, height=8.5, dpi=150)
cat("rows plotted:", nrow(u), "| municipalities:", uniqueN(u$adm3_pcode),
    "| provinces:", uniqueN(u$prov_lab), "\n")
cat("saved -> output/figures/pcua_rollout_timeline.{pdf,png}\n")
