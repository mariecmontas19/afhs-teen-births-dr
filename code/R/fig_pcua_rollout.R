# ============================================================================
# fig_pcua_rollout.R — Map: staggered roll-out of AU adolescent units across
# DR municipalities, colored by first-opening cohort; never-treated grey; always-
# treated (pre-2016) distinct; points sized by # units. Uses cached geometry
# (data/clean/muni_geom.rds) + treatment_municipio.rds. Saves to output/figures/.
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(sf); library(ggplot2)})

geom <- readRDS(file.path(DIR_CLEAN, "muni_geom.rds"))                 # sf, UTM 19N, adm3_pcode
trt  <- as.data.table(readRDS(file.path(DIR_CLEAN, "treatment_municipio.rds")))

# treatment group factor
trt[, grp := fifelse(ever_treated==0, "Never treated",
              fifelse(always_treated==1, "Always-treated (pre-2016)", as.character(first_year)))]
lv <- c("Never treated","Always-treated (pre-2016)","2020","2021","2022","2023","2024","2025")  # 2016 level removed: no 2016 cohort post-overhaul (vestigial legend entry, fixed 2026-07-17)
trt[, grp := factor(grp, levels=lv)]

g <- merge(geom, trt[, .(adm3_pcode, grp, ever_treated, always_treated, n_units, first_year)],
           by="adm3_pcode", all.x=TRUE)
stopifnot(nrow(g)==158, !anyNA(g$grp))                                 # Rule 7: no rows lost/unmatched

# centroids of treated municipalities for the # units overlay
ct <- st_centroid(g[g$ever_treated==1, ]); ct$n_units <- g$n_units[g$ever_treated==1]
ctc <- as.data.frame(st_coordinates(ct)); ctc$n_units <- ct$n_units

# dynamic subtitle counts (NEVER hardcode — they go stale, e.g. the 40th unit Las Terrenas)
n_muni <- nrow(g); n_units <- sum(trt$n_units, na.rm=TRUE)
n_ever <- trt[ever_treated==1, .N]; n_always <- trt[ever_treated==1 & always_treated==1, .N]; n_win <- n_ever - n_always
sub <- sprintf("%d municipalities · %d units · %d ever-treated (%d pre-2016 always-treated, %d in 2016-2025)",
               n_muni, n_units, n_ever, n_always, n_win)

pal <- c("Never treated"="grey90","Always-treated (pre-2016)"="grey55",
         "2020"="#feb24c","2021"="#fd8d3c","2022"="#fc4e2a",
         "2023"="#e31a1c","2024"="#bd0026","2025"="#800026")

p <- ggplot(g) +
  geom_sf(aes(fill=grp), color="grey97", linewidth=0.1) +
  geom_sf(data=g[g$ever_treated==1, ], fill=NA, color="grey15", linewidth=0.3) +   # outline unit municipalities
  # white circle markers with a dark halo (pop on the warm rollout palette + greys)
  geom_point(data=ctc, aes(X, Y, size=n_units), shape=21, fill=NA, color="grey10", stroke=1.6) +
  geom_point(data=ctc, aes(X, Y, size=n_units), shape=21, fill="white", color="grey20", stroke=0.6) +
  scale_fill_manual(values=pal, drop=FALSE, name="AU first opened") +
  scale_size_continuous(range=c(2.4, 6), breaks=c(1,2,3,5), name="# units in municipality") +
  guides(fill=guide_legend(order=1, override.aes=list(size=0)), size=guide_legend(order=2)) +
  theme_pcua_map() + theme(legend.key.size=unit(0.85,"lines"))

figdir <- file.path(PROJ, "output", "figures"); dir.create(figdir, recursive=TRUE, showWarnings=FALSE)
ggsave(file.path(figdir, "fig_b02_rollout_map.pdf"), p, width=10, height=7.2, device=cairo_pdf)
ggsave(file.path(figdir, "fig_b02_rollout_map.png"), p, width=10, height=7.2, dpi=150)
cat("saved -> output/figures/pcua_rollout_map.{pdf,png}\n")
cat("group counts:\n"); print(trt[, .N, by=grp][order(grp)])
