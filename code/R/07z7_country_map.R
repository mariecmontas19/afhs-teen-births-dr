# ============================================================================
# 07z7_country_map.R — polished CONTINUOUS-GRADIENT country maps (the "nicer"
# version of the categorical spillover_map, which is kept separately in 07z3).
#   Map A — ACCESS gradient: distance to nearest OPEN unit, 2025 (km), viridis,
#           unit municipalities marked; subtitle carries the % within 20 km.
#   Map B — OUTCOME gradient: recent teen birth rate (2022-2025 mean, /1,000).
# Both overlay AU unit municipalities (point size = number of units).
# Output: output/figures/fig_country_access_gradient.png, fig_country_rate_gradient.png
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(sf); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures")

g   <- readRDS(file.path(DIR_CLEAN,"muni_geom.rds"))
dp  <- as.data.table(readRDS(file.path(DIR_CLEAN,"distance_open_panel.rds")))[year==2025L, .(adm3_pcode, dist_open_km, acc20)]
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[, .(adm3_pcode, ever_treated, n_units)]
p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
rate_recent <- p[year %in% 2022:2025, .(teen_rate=mean(rateA_15_19, na.rm=TRUE)), by=adm3_pcode]

dat <- Reduce(function(a,b) merge(a,b,by="adm3_pcode",all.x=TRUE),
              list(dp, trt, rate_recent))
gm  <- merge(g, dat, by="adm3_pcode")
pct20 <- round(100*mean(dp$acc20, na.rm=TRUE))           # % of municipalities within 20 km in 2025
# WINSORIZE the COLOR SCALES at the 95th pct (display only — data/estimates unchanged; values
# above the cap render as the extreme colour). Improves contrast (esp. the rate map's 120.8 outlier).
d95 <- as.numeric(quantile(gm$dist_open_km, 0.95, na.rm=TRUE))
r95 <- as.numeric(quantile(gm$teen_rate,    0.95, na.rm=TRUE))
cat(sprintf("winsor caps (95th pct): distance=%.0f km (max %.0f) | teen rate=%.0f (max %.0f)\n",
    d95, max(gm$dist_open_km,na.rm=TRUE), r95, max(gm$teen_rate,na.rm=TRUE)))
# unit municipality centroids (point size = # units)
gu  <- gm[gm$ever_treated==1L, ]
uc  <- cbind(as.data.frame(st_coordinates(st_centroid(st_geometry(gu)))), n_units=gu$n_units)

base_theme <- theme_pcua_map()

# ---- Map A: ACCESS gradient (distance) ----
mA <- ggplot(gm) +
  geom_sf(aes(fill=dist_open_km), color="grey97", linewidth=0.1) +
  geom_sf(data=gu, fill=NA, color="grey15", linewidth=0.35) +                       # outline unit municipalities
  # unit municipalities: white halo + gold circle marker (gold pops on the cool mako palette)
  geom_point(data=uc, aes(X, Y, size=n_units), shape=21, fill=NA, color="white", stroke=2.0) +
  geom_point(data=uc, aes(X, Y, size=n_units), shape=21, fill="#F4B400", color="grey20", stroke=0.6) +
  scale_fill_viridis_c(option="mako", direction=-1, limits=c(0, d95), oob=scales::squish,
                       name="km to nearest\nPCUA (2025)") +
  scale_size_continuous(range=c(2.8,6), name="AU units in\nmunicipality", breaks=c(1,2,4)) +
  labs(title="Access to adolescent-care units across the Dominican Republic, 2025",
       subtitle=sprintf("Distance from each municipality to its nearest open AU. %d%% of municipalities are within 20 km; dots = municipalities with a unit.", pct20),
       caption=sprintf("Municipality (ADM3) centroid distances, EPSG:32619. Colour scale winsorized at the 95th pct (%.0f km).", d95)) + base_theme
ggsave(file.path(FIG,"fig_country_access_gradient.png"), mA, width=9, height=6.5, dpi=200)

# ---- Map B: OUTCOME gradient (recent teen rate) ----
mB <- ggplot(gm) +
  geom_sf(aes(fill=teen_rate), color="grey97", linewidth=0.1) +
  geom_sf(data=gu, fill=NA, color="grey10", linewidth=0.35) +                       # outline unit municipalities
  # unit municipalities: dark halo + white circle marker (white pops on the warm rocket palette)
  geom_point(data=uc, aes(X, Y, size=n_units), shape=21, fill=NA, color="grey10", stroke=1.8) +
  geom_point(data=uc, aes(X, Y, size=n_units), shape=21, fill="white", color="grey20", stroke=0.6) +
  scale_fill_viridis_c(option="rocket", direction=-1, limits=c(min(gm$teen_rate,na.rm=TRUE), r95), oob=scales::squish,
                       name="Teen births\nper 1,000\n(2022-2025)") +
  scale_size_continuous(range=c(2.8,6), name="AU units in\nmunicipality", breaks=c(1,2,4)) +
  labs(title="Adolescent birth rate across the Dominican Republic, 2022-2025",
       subtitle="Mean teen (15-19) birth rate per 1,000 women; dots = municipalities with an AU.",
       caption=sprintf("Denominator A. Municipality (ADM3) means. Colour scale winsorized at the 95th pct (%.0f); one small-municipality outlier (max %.0f) shown as darkest.", r95, max(gm$teen_rate,na.rm=TRUE))) + base_theme
ggsave(file.path(FIG,"fig_country_rate_gradient.png"), mB, width=9, height=6.5, dpi=200)

cat(sprintf("%% within 20 km (2025): %d%% | teen rate range: %.1f-%.1f\n",
    pct20, min(rate_recent$teen_rate), max(rate_recent$teen_rate)))
cat("saved -> fig_country_access_gradient.png + fig_country_rate_gradient.png\n")
