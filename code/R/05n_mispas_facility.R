# ============================================================================
# 05n_mispas_facility.R — MISPAS facility build, PART 1 (§59.11-59.12):
# parse ALL DATA (CRUDA) files 2022-2026Q1 into facility x month x age-group
# events (deliveries by via x nationality, ABORTOS, nacidos vivos), assign
# MUNICIPIO, save clean panel. Three vintages (verified layouts):
#  vA 2022 Q1-Q4:        "Suma de Par_dominicana_vaginal"-style cols; Periodo_mes
#                        text; Periodo_anio col.
#  vB 2023Q1-2025Q2:     "Partos vía Vaginal a Dominicanas"-style; month text;
#                        year col ("Año"/"Periodo_año") or from filename.
#  vC 2025Q3-2026Q1 (BI): vw_produccion_67A export; bracketed names; Par_* cols;
#                        age in `Servicio`; establecimiento_municipio DIRECT;
#                        numeric Periodo_mes/Periodo_anio.
# Municipio assignment: vC by muni name; vA/vB by facility-name match to the
# SNS registry (NombreCentro + Id_Provincia + Id_Municipio), passes: (1) exact
# normalized name within province, (2) unique name nationwide, (3) substring
# containment within province. 2023 annual file skipped (quarters cover it).
# 2015-2021 (INFORME Cuadro 29) = PART 2 (05n2).
# Outputs: data/clean/mispas_facility_month.rds, mispas_facility_xwalk.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})
D <- file.path(DRPAPER, "analysis/datasets/SNS/mispas_informes")
norm <- function(x){
  x <- toupper(stri_trans_general(as.character(x), "Latin-ASCII"))
  x <- gsub("[^A-Z0-9]+", " ", x)                 # BEFORE stopwords: '_' is a \\w char, breaks \\b
  x <- sub("^\\s*[0-9]+\\s+", "", x)              # leading enumeration digits ("4 MATERNIDAD ...")
  x <- gsub("\\b(HOSPITAL|HOSP|MUNICIPAL|PROVINCIAL|REGIONAL|GENERAL|DOCTOR|DRA?|MATERNIDAD|MATERNO|MAT|INFANTIL|UNIVERSITARIO|CENTRO|SANITARIO|DE|DEL|LA|EL|LOS|LAS|Y)\\b", " ", x)
  gsub(" +", " ", trimws(x))
}
MESES <- c(ENERO=1,FEBRERO=2,MARZO=3,ABRIL=4,MAYO=5,JUNIO=6,JULIO=7,AGOSTO=8,
           SEPTIEMBRE=9,OCTUBRE=10,NOVIEMBRE=11,DICIEMBRE=12)
mes2n <- function(x) fifelse(grepl("^[0-9]+$", trimws(x)), as.integer(trimws(x)),
                             MESES[toupper(trimws(x))])

fs <- list.files(D, pattern="_(DATA|Data)", full.names=TRUE)
fs <- fs[!grepl("Ao2023", fs)]
cat("DATA files:", length(fs), "\n")
pieces <- list()
for(f in fs){
  sh <- excel_sheets(f)
  s <- if(any(grepl("^partos$", tolower(sh)))) sh[grepl("^partos$", tolower(sh))][1] else sh[grep("PARTOS", toupper(sh))][1]
  d <- suppressMessages(as.data.table(read_excel(f, sheet=s)))
  if(any(grepl("\\[", names(d)))) setnames(d, gsub("^.*\\[|\\]$", "", names(d)))
  nn <- tolower(stri_trans_general(names(d), "Latin-ASCII"))
  pick <- function(...){for(p in c(...)){i <- grep(p, nn)[1]; if(!is.na(i)) return(names(d)[i])}; NA_character_}
  cols <- c(prov = pick("^establecimiento_provincia$", "^provincia"),
            muni = pick("^establecimiento_municipio$"),
            est  = pick("^establecimiento$", "establec"),
            mes  = pick("^periodo_mes", "^mes$"),
            anio = pick("periodo_an", "^ano$", "^anio$"),
            edad = pick("grupos? de edad", "^servicio$"),
            vag_dom = pick("par_dominicana_vaginal", "vaginal a dominicana"),
            ces_dom = pick("par_dominicana_cesarea", "cesarea a dominicana"),
            vag_hai = pick("par_haitiana_vaginal", "vaginal a haitiana"),
            ces_hai = pick("par_haitiana_cesarea", "cesarea a haitiana"),
            vag_otr = pick("par_otranacionalidad_vaginal", "vaginal a otras"),
            ces_otr = pick("par_otranacionalidad_cesarea", "cesarea a otras"),
            abort   = pick("par_abortos", "abortos"),
            nv      = pick("nacidos vivos"),
            nm      = pick("nacidos muertos", "nac_nacidos_muertos"))
  need <- setdiff(names(cols), c("muni","anio","nv","nm"))        # muni only in vC; anio fallback filename; nv/nm optional
  if(anyNA(cols[need])){cat("COLMAP FAIL", basename(f), ":", paste(need[is.na(cols[need])], collapse=","), "\n"); next}
  keep <- cols[!is.na(cols)]
  d <- d[, unname(keep), with=FALSE]; setnames(d, names(keep))
  if(!"muni" %in% names(d)) d[, muni := NA_character_]
  if(!"nv"   %in% names(d)) d[, nv := NA_real_]
  if(!"nm"   %in% names(d)) d[, nm := NA_real_]
  # vC BI files keep stillbirths on the separate NACIMIENTOS sheet (by Servicio=age)
  if(any(grepl("^nacimientos$", tolower(sh))) && all(is.na(d$nm))){
    n2 <- suppressMessages(as.data.table(read_excel(f, sheet=sh[grepl("^nacimientos$", tolower(sh))][1])))
    setnames(n2, gsub("^.*\\[|\\]$", "", names(n2)))
    n2 <- n2[, .(est=establecimiento, edad=Servicio, mes=Periodo_mes,
                 nm2=as.numeric(Nac_nacidos_muertos))]
    n2 <- n2[, .(nm2=sum(nm2, na.rm=TRUE)), by=.(est, edad, mes)]
    nb0 <- nrow(d)
    d <- merge(d, n2, by=c("est","edad","mes"), all.x=TRUE)
    stopifnot(nrow(d)==nb0)
    d[, nm := nm2][, nm2 := NULL]
  }
  d[, anio := if("anio" %in% names(keep)) as.integer(anio) else as.integer(sub("_.*","",basename(f)))]
  d[, `:=`(mes_n = mes2n(as.character(mes)), file = basename(f))]
  pieces[[basename(f)]] <- d
}
p <- rbindlist(pieces, fill=TRUE)
cat("files parsed:", length(pieces), "of", length(fs), "| raw rows:", nrow(p),
    "| month NA:", p[is.na(mes_n),.N], "| year NA:", p[is.na(anio),.N], "\n")
stopifnot(length(pieces)==length(fs))
if(p[is.na(mes_n),.N]){
  # verified 2026-07-13: NA-month rows are blank rows or embedded GRAND-TOTAL rows
  # (establecimiento NA; 2025Q2 total row = 2,234 abortions == sum of dated rows) -> drop
  stopifnot(p[is.na(mes_n) & !is.na(est), .N]==0)
  cat("dropping", p[is.na(mes_n),.N], "NA-month blank/total rows (all have est=NA)\n")
  p <- p[!is.na(mes_n)]
}

p[, age_band := fcase(grepl("Menor de 15|< ?15", edad), "lt15",
                      grepl("15 a 19", edad), "a1519", default="other")]
cat("age-band rows:\n"); print(p[, .N, by=age_band], class=FALSE)
num <- c("vag_dom","ces_dom","vag_hai","ces_hai","vag_otr","ces_otr","abort","nv","nm")
p[, (num) := lapply(.SD, function(x){x <- suppressWarnings(as.numeric(x)); fifelse(is.na(x), 0, x)}), .SDcols=num]
p[, deliv := vag_dom+ces_dom+vag_hai+ces_hai+vag_otr+ces_otr]
# period coverage must be disjoint across files (quarterly releases); if a month
# appears in >1 file, keep it from the LATER file wholesale (re-release logic) —
# never dedupe at facility level (distinct facilities can share normalized names)
cov <- unique(p[, .(file, anio, mes_n)])
dupper <- cov[, .N, by=.(anio, mes_n)][N>1]
if(nrow(dupper)){
  cat("overlapping periods (kept from latest file):\n"); print(dupper, class=FALSE)
  keepfile <- cov[dupper, on=.(anio, mes_n)][, .(file=max(file)), by=.(anio, mes_n)]
  p <- rbind(p[!dupper, on=.(anio, mes_n)], p[keepfile, on=.(anio, mes_n, file)])
}
cat("rows:", nrow(p), "| teen (15-19) deliveries+abortions by year:\n")
print(p[age_band=="a1519", .(deliv=sum(deliv), abort=sum(abort)), keyby=anio], class=FALSE)

# ---- municipio assignment ----
xw  <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
c2a <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_code_to_adm3.rds")))
reg <- as.data.table(suppressMessages(read_excel(file.path(DRPAPER,
  "analysis/datasets/Hospitals and PCU in DR/Hospitales-y-Centros-de-Primer-Nivel-de-Atencion-act-09-04-2026.xlsx"))))
reg <- reg[, .(name_reg=NombreCentro, prov_id=Id_Provincia, muni_id=Id_Municipio, prov_reg=Provincia)]
reg <- merge(reg, c2a, by.x=c("prov_id","muni_id"), by.y=c("prov_code","muni_code"), all.x=TRUE)
cat("registry rows unmapped to adm3:", reg[is.na(adm3_pcode),.N], "of", nrow(reg), "\n")

fac <- unique(p[, .(prov, est, muni)])
fac[, `:=`(pn=norm(prov), en=norm(est))]
reg[, `:=`(pn=norm(prov_reg), en=norm(name_reg))]
# pass 0 (vC): direct muni name within province
mn <- unique(xw[, .(pn=norm(prov_name), mn=norm(adm3_name), adm3_direct=adm3_pcode)])
fac[, mn := norm(muni)]
fac <- merge(fac, mn, by=c("pn","mn"), all.x=TRUE)
# pass 1: exact normalized facility name within province
r1 <- unique(reg[!is.na(adm3_pcode), .(pn, en, a1=adm3_pcode)])[, .SD[1], by=.(pn,en)]
fac <- merge(fac, r1, by=c("pn","en"), all.x=TRUE)
# pass 2: nationally unique facility name
r2 <- reg[!is.na(adm3_pcode), .N, by=en][N==1, en]
r2m <- unique(reg[en %in% r2 & !is.na(adm3_pcode), .(en, a2=adm3_pcode)])
fac <- merge(fac, r2m, by="en", all.x=TRUE)
# pass 3: substring containment within province
fac[, a3 := NA_character_]
nom <- fac[is.na(adm3_direct) & is.na(a1) & is.na(a2)]
for(i in seq_len(nrow(nom))){
  rp <- reg[pn==nom$pn[i] & !is.na(adm3_pcode)]
  hit <- rp[grepl(nom$en[i], rp$en, fixed=TRUE) | mapply(grepl, rp$en, nom$en[i], fixed=TRUE), adm3_pcode]
  if(length(unique(hit))==1) fac[pn==nom$pn[i] & en==nom$en[i], a3 := hit[1]]
}
# manual aliases (verified 2026-07-13): Sabana Iglesia facility = its own municipio
alias_man <- data.table(pn="SANTIAGO", en="SABANA IGLESIA",
                        am=xw[grepl("SABANA IGLESIA", toupper(stri_trans_general(adm3_name,"Latin-ASCII"))), adm3_pcode][1])
fac <- merge(fac, alias_man, by=c("pn","en"), all.x=TRUE)
fac[!is.na(am) & is.na(a3), a3 := am]
# pass 4: fuzzy (edit distance) within province, unique best under 25% of name length
nom2 <- fac[is.na(adm3_direct) & is.na(a1) & is.na(a2) & is.na(a3)]
fac[, a4 := NA_character_]
for(i in seq_len(nrow(nom2))){
  cand <- reg[pn==nom2$pn[i] & !is.na(adm3_pcode)]
  if(!nrow(cand)) next
  dd <- adist(nom2$en[i], cand$en)[1,]
  ok <- which(dd <= 0.25*nchar(nom2$en[i]))
  if(length(unique(cand$adm3_pcode[ok]))==1)
    fac[pn==nom2$pn[i] & en==nom2$en[i], a4 := cand$adm3_pcode[ok[1]]]
}
fac[, adm3 := fcoalesce(adm3_direct, a1, a2, a3, a4)]
fac2 <- fac[, .(adm3 = adm3[!is.na(adm3)][1]), by=.(prov, est)]
ev <- p[age_band %in% c("a1519","lt15"), .(ev=sum(deliv+abort)), by=.(prov, est)]
fac2 <- merge(fac2, ev, by=c("prov","est"), all.x=TRUE)[is.na(ev), ev := 0]
cat(sprintf("\nMUNI ASSIGNMENT: %d/%d facilities (%.1f%%); teen-event-weighted %.1f%%\n",
    fac2[!is.na(adm3),.N], nrow(fac2), 100*fac2[!is.na(adm3),.N]/nrow(fac2),
    100*fac2[!is.na(adm3), sum(ev)]/fac2[, sum(ev)]))
cat("unmatched with most teen events:\n"); print(fac2[is.na(adm3)][order(-ev)][1:8], class=FALSE)
nb <- nrow(p)
p <- merge(p, fac2[, .(prov, est, adm3)], by=c("prov","est"), all.x=TRUE)
stopifnot(nrow(p)==nb)  # fac2 is one row per (prov,est) -> merge must be 1:1
fwrite(fac2[order(-ev)], file.path(DIR_CLEAN, "mispas_facility_xwalk.csv"))
saveRDS(p, file.path(DIR_CLEAN, "mispas_facility_month.rds"))
cat("saved -> mispas_facility_month.rds (", nrow(p), "rows ) + mispas_facility_xwalk.csv\n")
