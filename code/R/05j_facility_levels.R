# ============================================================================
# 05j_facility_levels.R — health-facility SUPPLY DENSITY by LEVEL, per capita,
# time-varying (open-by-year), from the geolocated SNS establishment registry
# (1,909 facilities; NIVEL I=primary, II=hospital, III=high-complexity; ANIO
# APERTURA). Complements 05i (distance/access) and upgrades 05d (which used the
# act-2026 primary registry without levels). Distance != quantity: this is the
# "under-resourcing/need" measure (few hospitals per capita), distinct from
# 05i's "feasibility" (proximity). Facilities assigned to municipio by SPATIAL
# JOIN on coordinates (more robust than name-matching). pop_tot from 05d's
# health_centers_muni_year. Folded to 155. Output: health_facility_levels_panel.rds
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(sf)})
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)

f <- file.path(DRPAPER,"analysis/datasets/Hospitals and PCU in DR/Establecimientos de Salud Habilitados, SNS, 1878-2026.xlsx")
e <- as.data.table(read_excel(f))
setnames(e, c("NIVEL DE ATENCION","ANIO APERTURA","LATCENTRO","LONCENTRO"), c("nivel","anio","lat","lon"))
e <- e[!is.na(lat)&!is.na(lon)][, `:=`(lat=as.numeric(lat), lon=as.numeric(lon), anio=as.integer(anio))]
e <- e[!TIPO %in% c("CONSULTORIO","CENTRO DIAGNOSTICO")]   # 2026-08-07 (MM): hospitals + first-level only (1,909 -> 1,588), one registry across the paper
e[, open_from := fifelse(is.na(anio)|anio==0L, 1900L, anio)]
esf <- st_transform(st_as_sf(e, coords=c("lon","lat"), crs=4326), 32619)

# spatial join facility -> municipio (full 158 geometry; then fold to 155)
g <- readRDS(file.path(DIR_CLEAN,"muni_geom.rds"))
fj <- st_join(esf, g["adm3_pcode"], join=st_within)
fd <- as.data.table(st_drop_geometry(fj))[, .(adm3_pcode, nivel, open_from)]
cat("facilities spatially joined:", nrow(fd), "| unmatched (outside polygons):", fd[is.na(adm3_pcode),.N], "\n")
fd <- fd[!is.na(adm3_pcode)]; fd[, adm3_pcode := to155(adm3_pcode)]

# counts by level, open-by-year, per municipio
yrs <- 2016:2025
cnt <- rbindlist(lapply(yrs, function(t){
  fd[open_from<=t, .(n_primary=sum(nivel=="I"), n_hosp=sum(nivel %in% c("II","III")), n_niv3=sum(nivel=="III")),
     by=adm3_pcode][, year:=t]
}))
# ensure all 155 municipios present each year (0 if no facility)
allmy <- CJ(adm3_pcode=unique(to155(g$adm3_pcode)), year=yrs)
cnt <- merge(allmy, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
for(v in c("n_primary","n_hosp","n_niv3")) cnt[is.na(get(v)), (v):=0L]

# per-capita (per 10k pop) using pop_tot from 05d's time-varying file
pop <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni_year.rds")))[, .(adm3_pcode, year, pop_tot)]
cnt <- merge(cnt, pop, by=c("adm3_pcode","year"), all.x=TRUE)
cat("rows:", nrow(cnt), "| pop NA:", cnt[is.na(pop_tot),.N], "\n")
cnt[, `:=`(primary_per10k=round(1e4*n_primary/pop_tot,2), hosp_per10k=round(1e4*n_hosp/pop_tot,3),
           niv3_per10k=round(1e4*n_niv3/pop_tot,3))]
stopifnot(cnt[,uniqueN(adm3_pcode)]==155, !anyNA(cnt$primary_per10k))
saveRDS(cnt, file.path(DIR_CLEAN,"health_facility_levels_panel.rds"))

cat("\n=== national totals by level, open-by-year ===\n")
print(cnt[, .(primary=sum(n_primary), hosp=sum(n_hosp), niv3=sum(n_niv3)), by=year][order(year)][c(1,5,10)], class=FALSE)
cat("\n=== per-10k distribution (2016) ===\n")
print(cnt[year==2016, lapply(.SD, function(x) round(quantile(x,c(0,.5,1)),2)), .SDcols=c("primary_per10k","hosp_per10k","niv3_per10k")], class=FALSE)
# correlation with old sns_per10k (the 2026 primary registry measure)
old <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
m16 <- merge(cnt[year==2016], old, by="adm3_pcode")
cat(sprintf("\ncorr(primary_per10k 2016, OLD sns_per10k) = %.3f | corr(hosp_per10k, sns_per10k) = %.3f\n",
            cor(m16$primary_per10k, m16$sns_per10k), cor(m16$hosp_per10k, m16$sns_per10k)))
cat("\nsaved -> data/clean/health_facility_levels_panel.rds\n")
