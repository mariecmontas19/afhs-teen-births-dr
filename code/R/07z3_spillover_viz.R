# ============================================================================
# 07z3_spillover_viz.R — visualize the D3 takeaway: "the effect is LOCAL to the
# treated municipality; little neighbour spillover" (which supports the S1 design).
#  (1) MAP: DR municipalities classified by 2025 AU access — has own unit / neighbour
#      within 20 km (no own unit) / far (>20 km) — with unit municipalities marked.
#  (2) COMPARISON: own-unit ATT (S1 CS, −6.43) vs neighbour-spillover ATT
#      (within 20 km, own-untreated, −2.52 ns) as point-ranges. Numbers read from the
#      already-saved verified tables (headline_S1_S2.csv, distance_dose_cs.csv).
# Output: output/figures/fig_spillover_map.png + fig_spillover_compare.png
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(sf); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures")

g   <- readRDS(file.path(DIR_CLEAN,"muni_geom.rds"))                       # 158 sf, EPSG:32619
dp  <- as.data.table(readRDS(file.path(DIR_CLEAN,"distance_open_panel.rds")))
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))

# classify each municipality by 2025 access
cls <- merge(dp[year==2025L, .(adm3_pcode, acc20_2025=acc20, dist2025=dist_open_km)],
             trt[, .(adm3_pcode, ever_treated)], by="adm3_pcode", all.x=TRUE)
cls[, access := fifelse(ever_treated==1L, "Has own AU",
                  fifelse(acc20_2025==1L, "Neighbour ≤20 km (no own unit)", "Far (>20 km)"))]
cls[, access := factor(access, levels=c("Has own AU","Neighbour ≤20 km (no own unit)","Far (>20 km)"))]
cat("municipalities by 2025 access class:\n"); print(cls[, .N, by=access][order(access)])

gm <- merge(g, cls[, .(adm3_pcode, access)], by="adm3_pcode")
unit_ctr <- st_centroid(st_geometry(gm[gm$access=="Has own AU", ]))     # mark unit municipalities
uc <- as.data.frame(st_coordinates(unit_ctr))

pal <- c("Has own AU"="#1B7837", "Neighbour ≤20 km (no own unit)"="#A6DBA0", "Far (>20 km)"="grey88")
m <- ggplot(gm) +
  geom_sf(aes(fill=access), color="white", linewidth=0.13) +
  # unit municipalities as gold circles with a white halo (pop against the green fill)
  geom_point(data=uc, aes(X, Y), shape=21, fill=NA, color="white", size=2.9, stroke=1.9) +
  geom_point(data=uc, aes(X, Y), shape=21, fill=PCUA_COL$gold, color="grey20", size=2.1, stroke=0.5) +
  scale_fill_manual(values=pal, name="2025 AU access") +
  labs(title="AU access is spatially concentrated - and the effect is local",
       subtitle="Gold circles = municipalities with their own unit. Neighbours within 20 km show little spillover (see locality table).",
       caption="Municipality (ADM3) boundaries; distance to nearest OPEN unit as of 2025.") +
  theme_pcua_map()
ggsave(file.path(FIG,"fig_spillover_map.png"), m, width=8.5, height=6.5, dpi=200)

# NOTE (2026-06-26, MM): the own-vs-neighbour 2-point comparison GRAPH was RETIRED — its
# CIs overlap heavily, so it read as weak / easy to over-interpret. The locality evidence is
# now the TABLE in 07z5_spillover_table.R (own vs neighbour-spillover + the DONUT showing S1 is
# stable when near controls are dropped, capped at 30 km). This script keeps only the map MM likes.
cat("\nsaved -> fig_spillover_map.png  (locality evidence is the TABLE in 07z5_spillover_table.R)\n")
