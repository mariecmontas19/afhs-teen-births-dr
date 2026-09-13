# ============================================================================
# 05m_one_embarazos.R — ONE SAIP delivery ONE-OAI-CE-296 (received 2026-07-13,
# filed in analysis/datasets/SNS/): database behind ONE's Power BI teen-
# pregnancy map. Source (ficha tecnica): Form 67-A monthly hospital-production
# registries (MISPAS/SNS RIESS) = FACILITY-BASED pregnancy events attended
# (vaginal deliveries + cesareans + ABORTIONS), province level, 2017-2026,
# quarterly from 2022, sector = Publico vs Patronato/Privado/ONG (thin: 78 rows).
# ONE's own caveat: NOT a fertility rate (facility data does not represent the
# full adolescent population) -> we use COUNTS/composition, never rates alone.
# Education tabs (Desercion 2017-22 ages 10-18; Abandono/Matriculados 2022-24
# ages 10-19) are UNDOCUMENTED in the delivery (ficha covers pregnancies only;
# presumed MINERD via ONE dashboard) -> saved with source_unverified flag.
# Outputs: data/clean/one_embarazos_province.rds (annual + quarterly),
#          data/clean/one_educacion_province.rds,
#          output/tables/one_embarazos_validation.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})
TAB <- file.path(PROJ,"output","tables")
F1 <- file.path(DRPAPER,"analysis/datasets/SNS/Data  Mapa embarazos en  adolescentes República Dominicana.xlsx")
norm <- function(x) toupper(trimws(stri_trans_general(as.character(x),"Latin-ASCII")))

e <- suppressMessages(as.data.table(read_excel(F1, sheet="Embarazo_adolescente")))
setnames(e, c("anio","trimestre","region","prov_id","prov_name","tipo","sector",
              "lt15","a1519","a1019","todas","pct1019","lat","lon","id"))
e[, `:=`(anio=as.integer(anio), prov_id=as.integer(prov_id), prov_norm=norm(prov_name))]
stopifnot(e[, uniqueN(prov_id)]==32, e[!is.na(anio), .N]==nrow(e))
# map ONE prov names -> prov_code (same aliases as 05l SISALRIL)
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
pmap <- unique(xw[, .(prov_norm=norm(prov_name), prov_code)])
alias <- c("BAHORUCO"="BAORUCO","SALCEDO"="HERMANAS MIRABAL","MONSENOL NOUEL"="MONSENOR NOUEL",
           "SANTO DOMINGO DE GUZMAN"="DISTRITO NACIONAL","ELIAS PINA"="ELIAS PINA")
e[prov_norm %in% names(alias), prov_norm := alias[prov_norm]]
un <- setdiff(e[,unique(prov_norm)], pmap$prov_norm)
cat("unmatched province labels:", if(length(un)) paste(un, collapse=" | ") else "none", "\n")
e <- merge(e, pmap, by="prov_norm"); stopifnot(e[is.na(prov_code),.N]==0)

# annual panel: 2017-21 rows are annual (trimestre NA); 2022+ sum over quarters
ann <- e[, .(lt15=sum(lt15,na.rm=TRUE), a1519=sum(a1519,na.rm=TRUE),
             todas=sum(todas,na.rm=TRUE)), by=.(prov_code, anio, tipo, sector)]
qtr <- e[!is.na(trimestre), .(lt15=sum(lt15,na.rm=TRUE), a1519=sum(a1519,na.rm=TRUE),
             todas=sum(todas,na.rm=TRUE)), by=.(prov_code, anio, trimestre, tipo, sector)]
cat("annual rows:", nrow(ann), "| quarterly rows (2022+):", nrow(qtr), "\n")
cat("national 15-19 pregnancy events by year x tipo (all sectors):\n")
print(dcast(ann[, .(n=sum(a1519)), by=.(anio,tipo)], anio ~ tipo, value.var="n"), class=FALSE)
saveRDS(list(annual=ann, quarterly=qtr), file.path(DIR_CLEAN,"one_embarazos_province.rds"))

# ---- education tabs (source UNVERIFIED - not covered by ficha/metodologia) ----
rd2 <- function(sheet){
  d <- suppressMessages(as.data.table(read_excel(F1, sheet=sheet)))
  setnames(d, old=c("Año","ID-Provincia","Des. Provincia","Edad","Valor"),
              new=c("anio","prov_id","prov_name","edad","valor"), skip_absent=TRUE)
  d[, .(anio=as.integer(anio), prov_norm=norm(prov_name), edad=as.integer(edad),
        valor=as.numeric(valor), tab=sheet)]
}
ed <- rbind(rd2("Deserción"), rd2("Abandono"), rd2("Matriculados"))
ed[prov_norm %in% names(alias), prov_norm := alias[prov_norm]]
ed <- merge(ed, pmap, by="prov_norm"); stopifnot(ed[is.na(prov_code),.N]==0)
attr(ed, "source_unverified") <- "Education tabs undocumented in ONE-OAI-CE-296; presumed MINERD via ONE dashboard; confirm before paper use."
cat("education rows:", nrow(ed), "| tabs x years:\n")
print(ed[, .N, keyby=.(tab, anio)], class=FALSE)
saveRDS(ed, file.path(DIR_CLEAN,"one_educacion_province.rds"))

# ---- validation vs our BDNV births (rule 1b prediction: deliveries [vaginal+
# cesarea, public] should track BDNV 15-19 births at province level strongly,
# corr > ~0.9 in levels; below all-facility since BDNV covers all sectors) ----
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
bp <- merge(b[age_mom %in% 15:19 & !is.na(adm3_pcode) & birth_year %in% 2017:2024],
            unique(xw[, .(adm3_pcode, prov_code)]), by="adm3_pcode")[
      , .(births_bdnv=.N), by=.(prov_code, anio=birth_year)]
dl <- ann[tipo %in% c("Vaginal","Cesáreas"), .(deliv_one=sum(a1519)), by=.(prov_code, anio)]
v <- merge(bp, dl, by=c("prov_code","anio"))
cat("\n=== VALIDATION: ONE 67-A deliveries 15-19 vs BDNV births 15-19 (prov-year, 2017-2024) ===\n")
cat(sprintf("rows: %d | corr levels: %.3f | corr logs: %.3f | median ratio NSO/BDNV: %.2f\n",
    nrow(v), v[,cor(deliv_one,births_bdnv)], v[,cor(log(deliv_one),log(births_bdnv))],
    v[, median(deliv_one/births_bdnv)]))
fwrite(v, file.path(TAB,"one_embarazos_validation.csv"))
cat("saved -> one_embarazos_province.rds + one_educacion_province.rds + one_embarazos_validation.csv\n")
