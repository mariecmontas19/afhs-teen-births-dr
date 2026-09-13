# ============================================================================
# 05g_jee_education.R — JEE (Jornada Escolar Extendida / extended school day) by
# municipio, SECONDARY level, school year 2024-2025 (the only year with a Tanda field
# locally; earlier years are on MINERD Power BI dashboards -> to be compiled separately).
# Source: MINERD school directories (datasets/education/):
#   8xl-...2024-2025.csv  -> per centro x nivel: Tanda (Jornada Extendida = JEE), Matrícula
#   RTz-...2023-2024.csv  -> per centro: Provincia, Municipio, GPS  (the school->municipio crosswalk)
# Join on "Planta Fisica" plant code (98% match). JEE intensity = share of SECONDARY
# enrolment in Jornada Extendida (Garganta et al.'s measure). Map municipio name+province
# -> adm3_pcode; fold to 155.
# Outputs: data/clean/jee_municipio_2024_2025.rds, data/clean/school_muni_crosswalk.rds
# (the crosswalk is reused to map Power-BI school lists for 2017-2024 -> municipio).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(stringi)})
ED   <- file.path(DRPAPER, "analysis/datasets/education")
norm <- function(x) toupper(stri_trans_general(trimws(x),"Latin-ASCII"))
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155<- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)

# ---- school -> municipio crosswalk (RTz 2023-24: has Provincia/Municipio/GPS) ----
rtz <- fread(file.path(ED,"RTz-8sq-centros-educativos-de-republica-dominicana-periodo-escolar-2023-2024csv.csv"),
             sep=";", encoding="Latin-1", fill=TRUE)
rtz[, plant := sub(" .*","", trimws(`Planta Fisica`))]
xw_school <- unique(rtz[, .(plant, prov_nm=norm(Provincia), muni_nm=norm(Municipio),
                            lat=as.numeric(`Coordenadas Latitud`), lon=as.numeric(`Coordenadas Longitud`))])
xw_school <- xw_school[plant!="" & !is.na(plant)][, .SD[1], by=plant]      # 1 row per plant
saveRDS(xw_school, file.path(DIR_CLEAN,"school_muni_crosswalk.rds"))
cat("school->municipio crosswalk:", nrow(xw_school), "plants\n")

# ---- 8xl 2024-25: secondary Tanda + enrolment ----
xl <- fread(file.path(ED,"8xl-relacion-de-centros-educativos-2024-2025csv.csv"),
            sep=";", encoding="Latin-1", fill=TRUE, skip=1, header=TRUE)
setnames(xl, grep("Matr", names(xl), value=TRUE)[1], "matricula")
xl[, `:=`(plant=sub(" .*","", trimws(`Planta Fisica`)), matricula=as.numeric(matricula),
          jee=as.integer(Tanda=="Jornada Extendida"))]
sec <- xl[Nivel=="3-Secundario" & !is.na(matricula)]
cat("secondary centro-rows (2024-25):", nrow(sec), "| total sec enrolment:", sum(sec$matricula), "\n")

# ---- join to municipio + map to adm3_pcode ----
sec <- merge(sec, xw_school[, .(plant, prov_nm, muni_nm)], by="plant", all.x=TRUE)
cat("secondary rows matched to a municipio:", sec[!is.na(muni_nm),.N], "/", nrow(sec),
    sprintf("(%.1f%%)\n", 100*mean(!is.na(sec$muni_nm))))
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
xw[, `:=`(prov_nm=norm(prov_name), muni_nm=norm(adm3_name))]
sec <- merge(sec, xw[, .(prov_nm, muni_nm, adm3_pcode)], by=c("prov_nm","muni_nm"), all.x=TRUE)
cat("secondary rows mapped to adm3_pcode:", sec[!is.na(adm3_pcode),.N], "/", nrow(sec),
    sprintf("(%.1f%%)\n", 100*mean(!is.na(sec$adm3_pcode))))
if(sec[is.na(adm3_pcode)&!is.na(muni_nm),.N]>0){ cat("unmapped municipio names (top):\n")
  print(sec[is.na(adm3_pcode)&!is.na(muni_nm), .N, by=.(prov_nm,muni_nm)][order(-N)][1:8]) }

# ---- aggregate: JEE share of secondary enrolment per municipio (folded to 155) ----
sec <- sec[!is.na(adm3_pcode)]; sec[, adm3_pcode := to155(adm3_pcode)]
jee <- sec[, .(sec_enrol=sum(matricula), jee_enrol=sum(matricula*jee),
               n_sec_schools=uniqueN(plant), n_jee_schools=uniqueN(plant[jee==1])), by=adm3_pcode]
jee[, `:=`(pct_jee_enrol=round(100*jee_enrol/sec_enrol,1), pct_jee_schools=round(100*n_jee_schools/n_sec_schools,1))]
stopifnot(uniqueN(jee$adm3_pcode)<=155, !anyNA(jee$pct_jee_enrol))
saveRDS(jee, file.path(DIR_CLEAN,"jee_municipio_2024_2025.rds"))

cat("\n==================== JEE 2024-2025 (secondary, by municipio) ====================\n")
cat("municipios covered:", nrow(jee), "/155 | national JEE share of secondary enrolment:",
    sprintf("%.1f%%", 100*sum(jee$jee_enrol)/sum(jee$sec_enrol)), "\n")
cat("pct_jee_enrol distribution across municipios:\n"); print(round(quantile(jee$pct_jee_enrol, c(0,.25,.5,.75,1)),1))
cat("\nsaved -> jee_municipio_2024_2025.rds + school_muni_crosswalk.rds\n")
