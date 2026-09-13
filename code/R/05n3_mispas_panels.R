# ============================================================================
# 05n3_mispas_panels.R — combine 05n (2022-2026Q1, facility x month) + 05n2
# (2015-2021 annual Cuadro 29) into MUNICIPIO panels of facility-based teen
# pregnancy events. PUBLICO ONLY for a consistent series (05n's RIESS source
# has no private facilities — verified; 05n2 private block = ~0.7% of teen
# deliveries, dropped with disclosure).
#  - muni-YEAR 2015-2025 (2026 partial excluded from annual)
#  - muni-QUARTER 2022Q1-2026Q1
# Events: deliveries (vaginal+cesarea) and abortions, ages 15-19 and <15.
# VALIDATIONS (rule 1b predictions):
#  V1 vs ONE province aggregates 2017-2025 (same underlying source; ONE incl.
#     private -> our publico slightly BELOW; expect corr > .98).
#  V2 vs BDNV births at PUBLIC facilities by DELIVERY muni-year (different
#     source, same concept minus stillbirths/timing; expect corr ~ .9+).
# Output: data/clean/mispas_muni_panels.rds (list: year, quarter) +
#         output/tables/mispas_validation.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table)})
TAB <- file.path(PROJ,"output","tables")
fm <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")
to155 <- function(p) fifelse(p %in% names(fm), unname(fm[p]), p)

# ---- 2015-2021 from Cuadro 29 (publico) ----
a <- as.data.table(readRDS(file.path(DIR_CLEAN,"mispas_cuadro29_annual.rds")))
a <- a[sector=="publico" & !is.na(adm3)]
y1 <- a[, .(deliv_1519 = sum(vaginal_a1519+cesarea_a1519, na.rm=TRUE),
            abort_1519 = sum(aborto_a1519, na.rm=TRUE),
            deliv_lt15 = sum(vaginal_lt15+cesarea_lt15, na.rm=TRUE),
            abort_lt15 = sum(aborto_lt15, na.rm=TRUE)), by=.(adm3_pcode=to155(adm3), anio)]

# ---- 2022+ from facility-month ----
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"mispas_facility_month.rds")))
p <- p[!is.na(adm3) & age_band %in% c("a1519","lt15")]
mk <- function(d, by) dcast(d[, .(deliv=sum(deliv), abort=sum(abort), sb=sum(nm, na.rm=TRUE)),
        by=c(by,"age_band")],
        as.formula(paste(paste(by, collapse="+"), "~ age_band")), value.var=c("deliv","abort","sb"), fill=0)
p[, adm3_pcode := to155(adm3)]
y2 <- mk(p[anio <= 2025], c("adm3_pcode","anio"))
setnames(y2, c("deliv_a1519","abort_a1519","sb_a1519"),
             c("deliv_1519","abort_1519","sb_1519"), skip_absent=TRUE)
p[, qtr := paste0(anio, "Q", ceiling(mes_n/3))]
q2 <- mk(p, c("adm3_pcode","qtr"))
y1[, sb_1519 := NA_real_]                     # Cuadro 29 (2015-21) has no stillbirths
yr <- rbind(y1, y2[, names(y1), with=FALSE])
cat("teen stillbirths by year (2022+ only):\n")
print(yr[!is.na(sb_1519), .(sb=sum(sb_1519)), keyby=anio], class=FALSE)
cat("muni-year rows:", nrow(yr), "| munis:", yr[,uniqueN(adm3_pcode)], "| years:", yr[,paste(range(anio), collapse="-")], "\n")
cat("seam check, national teen deliveries (publico):\n")
print(yr[anio %in% 2020:2023, .(deliv=sum(deliv_1519)), keyby=anio], class=FALSE)

# ---- V1: vs ONE province aggregates ----
xw <- unique(as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[, .(adm3_pcode, prov_code)])
one <- readRDS(file.path(DIR_CLEAN,"one_embarazos_province.rds"))$annual
op <- dcast(one[anio %between% c(2017,2025), .(n=sum(a1519)), by=.(prov_code, anio, del=tipo!="Abortos")],
            prov_code + anio ~ del, value.var="n")
setnames(op, c("FALSE","TRUE"), c("one_abort","one_deliv"))
mp <- merge(yr, xw, by="adm3_pcode")[, .(deliv=sum(deliv_1519), abort=sum(abort_1519)), by=.(prov_code, anio)]
v1 <- merge(mp, op, by=c("prov_code","anio"))
cat(sprintf("\nV1 vs NSO (prov-year %d rows): corr deliv %.3f | corr abort %.3f | median ratio deliv %.3f\n",
    nrow(v1), v1[,cor(deliv,one_deliv)], v1[,cor(abort,one_abort)], v1[, median(deliv/one_deliv)]))

# ---- V2: vs BDNV public-facility births by DELIVERY muni ----
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
bd <- b[age_mom %in% 15:19 & facility=="Public" & !is.na(adm3_pcode_delivery) & birth_year %between% c(2016,2025),
        .(bdnv=.N), by=.(adm3_pcode=to155(adm3_pcode_delivery), anio=birth_year)]
v2 <- merge(yr[, .(adm3_pcode, anio, deliv=deliv_1519)], bd, by=c("adm3_pcode","anio"))
cat(sprintf("V2 vs BDNV delivery-muni (muni-year, %d rows): corr %.3f | corr logs(+1) %.3f | median ratio %.3f\n",
    nrow(v2), v2[,cor(deliv,bdnv)], v2[,cor(log1p(deliv),log1p(bdnv))], v2[bdnv>0, median(deliv/bdnv)]))
fwrite(rbind(v1[, .(check="V1_ONE_prov", key=paste(prov_code,anio), ours=deliv, theirs=one_deliv)],
             v2[, .(check="V2_BDNV_muni", key=paste(adm3_pcode,anio), ours=deliv, theirs=bdnv)]),
       file.path(TAB,"mispas_validation.csv"))
saveRDS(list(year=yr, quarter=q2), file.path(DIR_CLEAN,"mispas_muni_panels.rds"))
cat("saved -> mispas_muni_panels.rds + mispas_validation.csv\n")
