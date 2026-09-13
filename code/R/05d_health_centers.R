# ============================================================================
# 05d_health_centers.R — public-health-establishments measure per municipio
# ("public services provision" proxy, analog to Garganta et al.'s per-capita
# health centers).
# 2026-08-07 (MM, Option A — ONE registry for the whole paper): source switched
# from the act-09-04-2026 directory (1,546 SNS-administered) to the SAME file
# behind 05i/05j — "Establecimientos de Salud Habilitados, SNS, 1878-2026.xlsx"
# (1,909 geolocated) filtered to hospitals + first-level centers by dropping
# TIPO in {CONSULTORIO, CENTRO DIAGNOSTICO} -> 1,588 (1,391 NIVEL I, 147 II,
# 50 III). corr(new, old sns_per10k) = 0.88; the +covs estimate moves
# -5.66 -> -5.44 (verified in a preview run before the switch). Facilities are
# assigned to municipios by SPATIAL JOIN on coordinates (05j's method), then
# folded to 155. is_hosp = NIVEL II/III.
#
# Builds: (1) SNAPSHOT counts (time-invariant) -> baseline covariate / Table 1;
#         (2) TIME-VARYING open-by-year counts (ANIO APERTURA<=t) 2016-2025 ->
#             paper-style M_mt robustness control. Per-capita = per 10,000 pop.
# *** Snapshot is absorbed by municipio FE in CS (use as baseline/heterogeneity).
#     Time-varying assumes no historical closures (we see only current-active). ***
# Outputs: data/clean/health_centers_muni.rds (155, snapshot) +
#          data/clean/health_centers_muni_year.rds (155x10, time-varying).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(sf)})
HFILE <- file.path(DRPAPER,"analysis/datasets/Hospitals and PCU in DR",
                   "Establecimientos de Salud Habilitados, SNS, 1878-2026.xlsx")
stopifnot(file.exists(HFILE))
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)

d <- as.data.table(suppressMessages(readxl::read_excel(HFILE)))
setnames(d, c("NIVEL DE ATENCION","ANIO APERTURA","LATCENTRO","LONCENTRO"), c("nivel","anio","lat","lon"))
d <- d[!is.na(lat) & !is.na(lon)][, `:=`(lat=as.numeric(lat), lon=as.numeric(lon))]
sns <- d[!TIPO %in% c("CONSULTORIO","CENTRO DIAGNOSTICO")]          # hospitals + first-level only
cat("registry:", nrow(d), "geolocated | kept after TIPO filter:", nrow(sns), "\n")
stopifnot(nrow(sns) == 1588L)

# ---- assign to municipio by spatial join on coordinates (05j's method) ----
g   <- readRDS(file.path(DIR_CLEAN,"muni_geom.rds"))
pts <- st_transform(st_as_sf(sns, coords=c("lon","lat"), crs=4326), st_crs(g))
jj  <- as.data.table(st_drop_geometry(st_join(pts, g["adm3_pcode"], join=st_within)))
stopifnot(nrow(jj) == nrow(sns))                                    # guard: no boundary-point duplicates
sns[, adm3_pcode := jj$adm3_pcode]
cat("unmatched (outside polygons):", sns[is.na(adm3_pcode), .N], "\n")
sns <- sns[!is.na(adm3_pcode)]
sns[, adm3_pcode := to155(adm3_pcode)]                              # fold to 155
sns[, `:=`(yr = suppressWarnings(as.integer(anio)),
           is_hosp = nivel %in% c("II","III"))]
sns[yr<=0, yr := NA_integer_]
cat("SNS establishments:", nrow(sns), "| missing opening year:", sns[is.na(yr),.N],
    "| hospitals:", sns[is_hosp==TRUE,.N], "\n")

# ---- total municipio population (sum F+M, all ages) from denominator A, folded ----
A <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))
stopifnot(all(c("adm3_pcode","sex","age_group","year","pop") %in% names(A)))
cat("A sex values:", paste(unique(A$sex), collapse=","), "\n")
A[, id := to155(adm3_pcode)]
pop <- A[sex %in% c("F","M"), .(pop_tot=sum(pop)), by=.(adm3_pcode=id, year)]   # total pop muni-year (155)
stopifnot(uniqueN(pop$adm3_pcode)==155)

# ---- (1) SNAPSHOT counts per municipio (time-invariant) ----
snap <- sns[, .(n_sns=.N, n_hosp=sum(is_hosp), n_primer=sum(!is_hosp)), by=adm3_pcode]
snap <- merge(data.table(adm3_pcode=unique(pop$adm3_pcode)), snap, by="adm3_pcode", all.x=TRUE)
snap[is.na(n_sns), `:=`(n_sns=0L,n_hosp=0L,n_primer=0L)]
pop2022 <- pop[year==2022L, .(adm3_pcode, pop2022=pop_tot)]
snap <- merge(snap, pop2022, by="adm3_pcode")
snap[, `:=`(sns_per10k = round(1e4*n_sns/pop2022,3), hosp_per10k = round(1e4*n_hosp/pop2022,3))]
stopifnot(nrow(snap)==155, snap[is.na(sns_per10k),.N]==0)
saveRDS(snap[, .(adm3_pcode, n_sns, n_hosp, n_primer, pop2022, sns_per10k, hosp_per10k)],
        file.path(DIR_CLEAN,"health_centers_muni.rds"))

# ---- (2) TIME-VARYING open-by-year counts 2016-2025 ----
# n_open(t) = (#SNS with valid Anio_Apertura<=t) + (#SNS missing year, treated as
# pre-existing throughout, since all are old & currently active). Per 10,000 pop(t).
mk_year <- function(y){
  open <- sns[(is.na(yr)) | (yr<=y), .(n_sns_open=.N, n_hosp_open=sum(is_hosp)), by=adm3_pcode]
  open[, year := y]; open
}
tv <- rbindlist(lapply(2016:2025, mk_year))
tv <- merge(CJ(adm3_pcode=unique(pop$adm3_pcode), year=2016:2025), tv, by=c("adm3_pcode","year"), all.x=TRUE)
tv[is.na(n_sns_open), `:=`(n_sns_open=0L, n_hosp_open=0L)]
tv <- merge(tv, pop, by=c("adm3_pcode","year"))
tv[, `:=`(sns_per10k = round(1e4*n_sns_open/pop_tot,3), hosp_per10k = round(1e4*n_hosp_open/pop_tot,3))]
stopifnot(nrow(tv)==155*10, tv[is.na(sns_per10k),.N]==0)
saveRDS(tv[, .(adm3_pcode, year, n_sns_open, n_hosp_open, pop_tot, sns_per10k, hosp_per10k)],
        file.path(DIR_CLEAN,"health_centers_muni_year.rds"))

# ---- validation report ----
cat("\n==================== 05d HEALTH CENTERS ====================\n")
cat("snapshot (155 muni): SNS centers/10k pop summary:\n"); print(summary(snap$sns_per10k))
cat("national totals: SNS centers", sum(snap$n_sns), "| hospitals", sum(snap$n_hosp), "\n")
cat("\ntime-varying national SNS-open count by year (should rise):\n")
print(tv[, .(n_open=sum(n_sns_open)), by=year][order(year)])
cat("\nsaved -> data/clean/health_centers_muni.rds (155) + health_centers_muni_year.rds (155x10)\n")
