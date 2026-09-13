# ============================================================================
# 04e_write_classification_sheet.R — write a NEW sheet "Clasificacion unidades"
# into the raw "Visits to PCUAS.xlsx", PRESERVING all existing sheets. Adds, per
# unit: Region de Salud / Provincia / Municipio / Hospital / apertura year /
# inauguration-or-upgrade date / classification (Unidad integral vs Solo consultas
# especializadas). A timestamped backup of the workbook is made first.
# Source of truth: notes/pcua_unit_dates_verified.csv (built by 04b).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(openxlsx); library(stringi)})

d  <- as.data.table(fread(file.path(PROJ,"notes","pcua_unit_dates_verified.csv")))
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
d  <- merge(d, xw[,.(adm3_pcode, provN=prov_norm, municipio=adm3_name)], by="adm3_pcode", all.x=TRUE)

reg <- function(p){ fcase(
  p %in% c("DISTRITO NACIONAL","SANTO DOMINGO"), "10 - SRS Metropolitano (Ozama)",
  p %in% c("SANTIAGO","PUERTO PLATA","ESPAILLAT"), "1 - SRS Cibao Norte",
  p %in% c("LA VEGA","SANCHEZ RAMIREZ","MONSENOR NOUEL"), "2 - SRS Cibao Sur",
  p %in% c("DUARTE","MARIA TRINIDAD SANCHEZ","HERMANAS MIRABAL","SAMANA"), "3 - SRS Cibao Nordeste",
  p %in% c("SANTIAGO RODRIGUEZ","VALVERDE","MONTE CRISTI","DAJABON"), "4 - SRS Cibao Noroeste",
  p %in% c("PERAVIA","SAN CRISTOBAL","SAN JOSE DE OCOA","AZUA"), "5 - SRS Valdesia",
  p %in% c("BARAHONA","BAHORUCO","BAORUCO","INDEPENDENCIA","PEDERNALES"), "6 - SRS Enriquillo",
  p %in% c("SAN JUAN","ELIAS PINA"), "7 - SRS El Valle",
  p %in% c("LA ALTAGRACIA","LA ROMANA","EL SEIBO"), "8 - SRS Yuma",
  p %in% c("SAN PEDRO DE MACORIS","HATO MAYOR","MONTE PLATA"), "9 - SRS Higuamo",
  default=paste("??",p)) }
ti <- function(x) stri_trans_totitle(stri_trans_general(x,"Latin-ASCII"))

d[, `:=`(
  region    = reg(provN),
  apertura  = fifelse(!is.na(recorded_year), as.character(recorded_year),
                      substr(modern_event_date,1,4)),
  fecha_op  = fifelse(!is.na(modern_event_date)&modern_event_date!="", modern_event_date,
                      fifelse(!is.na(recorded_year), as.character(recorded_year), "")),
  tipo_fecha= fifelse(modern_event_type %in% c("upgrade","upgrade_unconfirmed","old_never_modernized"),
                      "Upgrade/remodelacion", "Inauguracion (nueva)"),
  clasif    = fifelse(consults_only==TRUE | never_modernized==TRUE,
                      "Solo consultas especializadas", "Unidad integral"))]
d[, regnum := suppressWarnings(as.integer(sub(" .*","",region)))]
setorder(d, regnum, provN, municipio)

sheet <- data.frame(
  `Region de Salud`              = d$region,
  `Provincia`                    = ti(d$provN),
  `Municipio`                    = ti(d$municipio),
  `Hospital`                     = d$hosp,
  `Anio apertura unidad/servicio`= d$apertura,
  `Fecha inauguracion/upgrade`   = d$fecha_op,
  `Tipo de fecha`                = d$tipo_fecha,
  `Clasificacion`                = d$clasif,
  `Fuente/Notas`                 = d$notes,
  check.names = FALSE, stringsAsFactors = FALSE)

# ---- backup, then append sheet (preserving existing sheets) ----
xlsx <- RAW$pcua_xlsx
bak  <- file.path(dirname(xlsx), "Visits to PCUAS (backup pre-clasificacion).xlsx")
file.copy(xlsx, bak, overwrite = TRUE)
cat("backup ->", bak, "\n")

wb <- loadWorkbook(xlsx)
existing <- names(wb)
SHEET <- "Clasificacion unidades"
if (SHEET %in% existing) removeWorksheet(wb, SHEET)   # idempotent re-run
addWorksheet(wb, SHEET)
writeData(wb, SHEET, sheet, withFilter = TRUE)
setColWidths(wb, SHEET, cols = 1:ncol(sheet), widths = "auto")
freezePane(wb, SHEET, firstRow = TRUE)
saveWorkbook(wb, xlsx, overwrite = TRUE)

# ---- verify ----
after <- readxl::excel_sheets(xlsx)
cat("sheets before:", paste(existing, collapse=" | "), "\n")
cat("sheets after :", paste(after, collapse=" | "), "\n")
cat("rows written:", nrow(sheet), "| classification counts:\n")
print(table(sheet$Clasificacion))
stopifnot(all(c("Unidades") %in% after), SHEET %in% after, nrow(sheet)==40L)
cat("OK\n")
