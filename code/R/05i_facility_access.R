# ============================================================================
# 05i_facility_access.R — robust GEOGRAPHIC HEALTH-ACCESS metrics from the
# geolocated SNS establishment registry (1,909 facilities, all with coords +
# NIVEL + ANIO APERTURA). Replaces the muddy "distance-to-nearest-AU-municipio"
# isolation metric (notes §26.3) with proper facility-access measures. Three:
#   (A) NEAREST-FACILITY distance (km), by level: any / hospital (NIVEL II-III) /
#       high-complexity (NIVEL III). Time-varying (facilities open by year t).
#   (B) FACILITY COUNT within 10/20/30 km (open by t).
#   (C) 2SFCA20 — 2-step floating catchment (radius 20 km): supply-to-demand
#       accessibility index; demand = women 15-44 (baseline 2016); supply = 1/facility.
# Euclidean distances (straight-line; road/travel-time would need a routing API).
# Unknown opening year (194/1909) treated as open throughout (old/established).
# Geometry: muni_geom.rds (EPSG:32619, m), 155-division (3 split children dropped).
# Output: data/clean/facility_access_panel.rds (adm3_pcode x year, 2016-2025).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(sf)})
children <- c("DOM010905","DOM051703","DOM012510")

# ---- establishments -> projected sf points ----------------------------------
f <- file.path(DRPAPER,"analysis/datasets/Hospitals and PCU in DR/Establecimientos de Salud Habilitados, SNS, 1878-2026.xlsx")
e <- as.data.table(read_excel(f))
setnames(e, c("NIVEL DE ATENCION","ANIO APERTURA","LATCENTRO","LONCENTRO"), c("nivel","anio","lat","lon"))
e <- e[!is.na(lat) & !is.na(lon)][, `:=`(lat=as.numeric(lat), lon=as.numeric(lon), anio=as.integer(anio))]
# 2026-08-07 (MM): restrict to hospitals + first-level centers (drop the 234 specialty
# consultorios + 87 diagnostic centers) so the proximity metric measures distance to the
# core public network the AUs sit inside, and matches the density covariate's scope.
# 1,909 -> 1,588; near-facility distance corr 0.999 with the pre-filter measure.
e <- e[!TIPO %in% c("CONSULTORIO","CENTRO DIAGNOSTICO")]
e[, open_from := fifelse(is.na(anio) | anio==0L, 1900L, anio)]              # unknown yr -> always open
e[, hosp := nivel %in% c("II","III")][, niv3 := nivel=="III"]
esf <- st_transform(st_as_sf(e, coords=c("lon","lat"), crs=4326), 32619)
cat("facilities:", nrow(e), "| hospitals(II/III):", sum(e$hosp), "| niv III:", sum(e$niv3), "\n")

# ---- municipio centroids (155) + demand pop (women 15-44, 2016) --------------
g <- readRDS(file.path(DIR_CLEAN,"muni_geom.rds")); g <- g[!g$adm3_pcode %in% children, ]
cent <- st_centroid(st_geometry(g)); pc <- g$adm3_pcode
pan <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
dem <- pan[year==2016, .(adm3_pcode, pop=womenA_15_19+womenA_20_24+womenA_25_29+womenA_30_34+womenA_35_39+womenA_40_44)]
dem <- dem[match(pc, adm3_pcode)]                                          # align to centroid order
stopifnot(nrow(dem)==length(pc), !anyNA(dem$pop))

# ---- distance matrix: municipios (155) x facilities (m -> km) ----------------
D <- sf::st_distance(cent, st_geometry(esf)); D <- matrix(as.numeric(D)/1000, nrow=length(pc))   # km
cat("distance matrix:", nrow(D), "munis x", ncol(D), "facilities\n")

# ---- per-year metrics --------------------------------------------------------
yr_metrics <- function(t){
  op <- e$open_from <= t                                                   # facilities open by year t
  near <- function(mask){ Dm <- D[, op & mask, drop=FALSE]; if(ncol(Dm)==0) return(rep(NA_real_,nrow(D))); apply(Dm,1,min) }
  cnt  <- function(r){ rowSums(D[, op, drop=FALSE] <= r) }
  # 2SFCA radius 20km
  W <- D[, op, drop=FALSE] <= 20                                           # munis x open-fac (within 20km)
  Pj <- as.numeric(crossprod(W, dem$pop))                                  # demand pop within 20km of each facility
  Rj <- ifelse(Pj>0, 1/Pj, 0)                                             # supply (1) / demand
  sfca <- as.numeric(W %*% Rj) * 10000                                     # accessibility per 10k women
  data.table(adm3_pcode=pc, year=t,
    near_any_km=round(near(rep(TRUE,nrow(e))),2), near_hosp_km=round(near(e$hosp),2), near_niv3_km=round(near(e$niv3),2),
    n_10km=cnt(10), n_20km=cnt(20), n_30km=cnt(30), sfca20=round(sfca,3))
}
P <- rbindlist(lapply(2016:2025, yr_metrics))
stopifnot(P[, uniqueN(adm3_pcode)]==155, P[, uniqueN(year)]==10)
saveRDS(P, file.path(DIR_CLEAN,"facility_access_panel.rds"))

cat("\n=== national medians by year (open-by-year facilities) ===\n")
print(P[, .(near_any=round(median(near_any_km),1), near_hosp=round(median(near_hosp_km),1),
            near_niv3=round(median(near_niv3_km),1), n20=median(n_20km), sfca20=round(median(sfca20),2)),
        by=year][order(year)], class=FALSE)
cat("\n=== correlations among municipios (2016) with the OLD isolation + sns_per10k ===\n")
p16 <- P[year==2016]
dp <- as.data.table(readRDS(file.path(DIR_CLEAN,"distance_open_panel.rds")))[year==2016, .(adm3_pcode, old_iso=dist_open_km)]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
M <- Reduce(function(a,b) merge(a,b,by="adm3_pcode"), list(p16, dp, hc))
cat(sprintf("  near_hosp_km vs OLD isolation_km : %.3f\n", cor(M$near_hosp_km, M$old_iso)))
cat(sprintf("  near_hosp_km vs sns_per10k       : %.3f\n", cor(M$near_hosp_km, M$sns_per10k)))
cat(sprintf("  sfca20       vs sns_per10k       : %.3f\n", cor(M$sfca20, M$sns_per10k)))
cat(sprintf("  near_any_km  vs near_hosp_km     : %.3f\n", cor(M$near_any_km, M$near_hosp_km)))
cat("\nsaved -> data/clean/facility_access_panel.rds (", nrow(P), "muni-year rows)\n", sep="")
