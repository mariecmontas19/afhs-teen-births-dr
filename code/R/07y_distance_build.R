# ============================================================================
# 07y_distance_build.R — D3 step 1: build a TIME-VARYING distance-to-nearest-OPEN-AU
# panel (municipio x year), the dose/access treatment. The panel's existing
# dist_km_nearest_pcua is STATIC (uses all units regardless of opening year) — this
# rebuilds it per year using only units open by year t.
#
# Definition: open_munis(t) = municipios whose FIRST unit opened by year t (includes
# pre-2016 always-treated, who provide access in every year). dist_open(i,t) = min
# centroid distance (km) from municipio i to any municipio in open_munis(t); 0 if i
# itself has an open unit. Distance is monotonically NON-INCREASING in t (units never
# close) -> a clean dCDH treatment path. Geometry: muni_geom.rds, CRS EPSG:32619 (m).
# Also builds binary access indicators (within 10/15/20/30 km) for a CS dose gradient.
# Output: data/clean/distance_open_panel.rds  (adm3_pcode x year)
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(sf)})

g <- readRDS(file.path(DIR_CLEAN,"muni_geom.rds"))                      # 158 muni, MULTIPOLYGON, EPSG:32619 (m)
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
# first-unit opening year per municipio (NA = never has a unit)
open_yr <- trt[ever_treated==1L, .(adm3_pcode, first_year)]
cat("municipios that ever have a unit:", nrow(open_yr), "| first_year range:", paste(range(open_yr$first_year),collapse="-"), "\n")

# centroid distance matrix (km) among all 158 municipios
ctr <- st_centroid(st_geometry(g))
D <- st_distance(ctr)                                                    # 158x158 meters
D <- matrix(as.numeric(D)/1000, nrow=nrow(D))                           # km
rownames(D) <- colnames(D) <- g$adm3_pcode
stopifnot(nrow(D)==158, isTRUE(all.equal(unname(diag(D)), rep(0,158))))

YEARS <- 2016:2025
rows <- list()
for(t in YEARS){
  open_set <- open_yr[first_year <= t, adm3_pcode]                      # units open by year t
  if(length(open_set)==0){ d_t <- rep(NA_real_, 158) } else {
    sub <- D[, open_set, drop=FALSE]                                    # dist from each muni to each open muni
    d_t <- apply(sub, 1, min)                                           # nearest open unit (0 if self in open_set)
  }
  rows[[as.character(t)]] <- data.table(adm3_pcode=g$adm3_pcode, year=t, dist_open_km=d_t,
                                        n_open_munis=length(open_set))
}
P <- rbindlist(rows)
# binary access thresholds + proximity (decreasing distance -> increasing access)
for(x in c(10,15,20,30)) P[, paste0("acc",x) := as.integer(dist_open_km <= x)]
P[, neg_dist := -dist_open_km]                                          # increasing dose for dCDH

# ---- VERIFY the variation is clean ----
P155 <- P[adm3_pcode %in% unique(as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))$adm3_pcode)]
cat("\nrows:", nrow(P155), "| municipios:", uniqueN(P155$adm3_pcode), "| balanced:", uniqueN(P155$adm3_pcode)*length(YEARS)==nrow(P155), "\n")
chg <- P155[, .(n_distinct=uniqueN(round(dist_open_km,3)), d2016=dist_open_km[year==2016], d2025=dist_open_km[year==2025]), by=adm3_pcode]
cat("municipios whose distance CHANGES over 2016-2025 (switchers):", chg[n_distinct>1,.N], "/", nrow(chg), "\n")
cat("  of which distance strictly DECREASED:", chg[d2025<d2016-1e-6,.N], "\n")
cat("distance distribution (km) by year (mean / median / %within20km):\n")
print(P155[, .(mean=round(mean(dist_open_km),1), median=round(median(dist_open_km),1),
               within20=round(100*mean(dist_open_km<=20),0), within10=round(100*mean(dist_open_km<=10),0)), by=year])
cat("\naccess-within-20km SWITCHERS (0->1 over window):",
    P155[, .(sw=any(acc20==0) & any(acc20==1)), by=adm3_pcode][sw==TRUE,.N], "municipios\n")
cat("access-within-10km switchers:",
    P155[, .(sw=any(acc10==0) & any(acc10==1)), by=adm3_pcode][sw==TRUE,.N], "municipios\n")

saveRDS(P, file.path(DIR_CLEAN,"distance_open_panel.rds"))
cat("\nsaved -> data/clean/distance_open_panel.rds\n")
