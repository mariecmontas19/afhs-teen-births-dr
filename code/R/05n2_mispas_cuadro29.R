# ============================================================================
# 05n2_mispas_cuadro29.R — PART 2 (§59.13): parse "EMBARAZOS EN ADOLESCENTES"
# cuadro from ANNUAL INFORME files 2015-2021 (pre-treatment years).
# Annual releases verified per title row: 2015/2016/2017 "año 20XX" (15 cols),
# 2018-2021 "enero-diciembre del año 20XX" (19 cols). (Quarterly pre-2022
# releases exist but are patchy — 2018 Q3/Q4 and 2019 Q1/Q2 absent, 2021 has a
# partial julio-agosto file — annual files are complete; quarterly panel uses
# 2022+ from 05n only.)
# Layout: one sheet; column 1 = REGION x / PROVINCIA / FACILITY hierarchy;
# header rows contain tipo blocks (Parto Vaginal / Cesareas / Abortos) x age
# cols (<15, 15-19, ...). Generic parse: locate header rows, forward-fill tipo
# across columns, pick (tipo, age) pairs.
# Facility rows -> municipio via the same 5-pass matching vs SNS registry,
# seeded by 05n's crosswalk. VALIDATION: facility sums vs province rows.
# Output: data/clean/mispas_cuadro29_annual.rds (facility x year x tipo x age)
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})
D <- file.path(DRPAPER, "analysis/datasets/SNS/mispas_informes")
norm <- function(x){
  x <- toupper(stri_trans_general(as.character(x), "Latin-ASCII"))
  x <- gsub("[^A-Z0-9]+", " ", x)                 # BEFORE stopwords: '_' is a \\w char, breaks \\b
  x <- sub("^\\s*[0-9]+\\s+", "", x)
  x <- gsub("\\bFCO\\b", "FRANCISCO", x)
  x <- gsub("\\b(HOSPITAL|HOSP|MUNICIPAL|MUNIC|PROVINCIAL|REGIONAL|GENERAL|DOCTOR|DRA?|MATERNIDAD|MATERNO|MAT|INFANTIL|UNIVERSITARIO|CENTRO|SANITARIO|LOCAL|PTE|PDTE|PRESIDENTE|SUB|DE|DEL|LA|EL|LOS|LAS|Y)\\b", " ", x)
  x <- gsub("\\b[A-Z]\\b", " ", x)                # single-letter tokens (middle initials)
  gsub(" +", " ", trimws(x))
}
ANN <- c("2015_direct_INFORME_4613.xlsx","2016_direct_INFORME_4612.xlsx",
         "2017_direct_INFORME_4622.xlsx","2018_direct_INFORME_12027.xlsx",
         "2019_direct_INFORME_15784.xlsx","2020_direct_INFORME_17757.xlsx",
         "2021_direct_INFORME_21704.xlsx")
YRS <- 2015:2021

xw  <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
provs <- unique(norm(xw$prov_name))
out <- list()
for(k in seq_along(ANN)){
  f <- file.path(D, ANN[k]); yr <- YRS[k]
  sh <- excel_sheets(f); s <- sh[grep("EMBARAZ|OBSTETRICOS EN ADOLESC", toupper(sh))][1]
  d <- suppressMessages(as.data.table(read_excel(f, sheet=s, col_names=FALSE)))
  M <- as.matrix(d)
  up <- function(r) toupper(stri_trans_general(ifelse(is.na(M[r,]), "", M[r,]), "Latin-ASCII"))
  # locate the header row pair: a row containing PARTO VAGINAL / CESAREA
  hr <- which(sapply(1:min(8,nrow(M)), function(r) any(grepl("VAGINAL", up(r)))))[1]
  ar <- which(sapply(hr:(hr+3), function(r) any(grepl("^< ?15|MENOR", up(r)))))[1] + hr - 1
  tipo_raw <- up(hr); age_raw <- up(ar)
  tipo <- rep(NA_character_, ncol(M))
  cur <- NA_character_
  for(j in 1:ncol(M)){
    t <- tipo_raw[j]
    if(grepl("VAGINAL", t)) cur <- "vaginal"
    else if(grepl("CESAREA", t)) cur <- "cesarea"
    else if(grepl("ABORTO", t)) cur <- "aborto"
    else if(grepl("TOTAL EMBARAZ|POBLACION|% EMBARAZ", t)) cur <- NA_character_
    tipo[j] <- cur
  }
  age <- fcase(grepl("^< ?15|MENOR", age_raw), "lt15",
               grepl("15-19|15 A 19", age_raw), "a1519", default=NA_character_)
  sel <- which(!is.na(tipo) & !is.na(age))
  if(length(sel) != 6) cat("NOTE", yr, ": found", length(sel), "tipo-age cols (expect 6)\n")
  body <- (ar+1):nrow(M)
  lab <- trimws(M[body, 1]); keep <- body[!is.na(lab) & lab != ""]
  dt <- data.table(raw = trimws(M[keep,1]))
  for(j in sel) dt[[paste0(tipo[j], "_", age[j])]] <- suppressWarnings(as.numeric(M[keep, j]))
  dt[, rn := norm(raw)]
  # province detection: exact/alias/fuzzy vs the 32 names, but a raw label that
  # starts with a facility word is ALWAYS a facility ("HOSPITAL SAN JOSE DE OCOA")
  isfacword <- grepl("^\\s*(HOSPITAL|CLINICA|CENT|CTRO|MATERNIDAD|CIUDAD SANITARIA)", toupper(stri_trans_general(dt$raw,"Latin-ASCII")))
  palias <- c("SALCEDO"="HERMANAS MIRABAL", "SAN PEDRO"="SAN PEDRO MACORIS",
              "BAHORUCO"="BAORUCO", "SEYBO"="SEIBO", "ELIAS PINAS"="ELIAS PINA")
  pmatch1 <- vapply(dt$rn, function(r){
    if(is.na(r) || nchar(r) < 4) return(NA_character_)
    if(r %in% provs) return(r)
    if(r %in% names(palias)) return(unname(palias[r]))
    dd <- drop(adist(r, provs))                                # ELIAS PINAS ~ ELIAS PINA
    if(min(dd) <= 2 && sum(dd==min(dd))==1) return(provs[which.min(dd)])
    hit <- provs[startsWith(r, paste0(provs, " ")) | startsWith(provs, paste0(r, " "))]
    if(length(hit)==1) return(hit)                             # VALVERDE MAO / SAN PEDRO
    NA_character_}, character(1), USE.NAMES=FALSE)
  # sequential: a row matching the CURRENT province is a facility named like it
  # ("SAN JOSE DE OCOA" hospital inside the San Jose de Ocoa block)
  kind <- character(nrow(dt)); prv <- rep(NA_character_, nrow(dt)); cur <- NA_character_
  for(i in seq_len(nrow(dt))){
    r <- dt$rn[i]
    if(grepl("^REGION", r)) {kind[i] <- "region"; next}
    if(grepl("TOTAL GENERAL|^TOTAL$|OTROS ESTABLECIMIENTOS|PATRONATOS", r)) {kind[i] <- "total"; next}
    if(!is.na(pmatch1[i]) && !isfacword[i] && !identical(pmatch1[i], cur)){
      kind[i] <- "prov"; cur <- pmatch1[i]; prv[i] <- cur; next}
    kind[i] <- "fac"; prv[i] <- cur
  }
  dt[, `:=`(kind = kind, prov = prv)]
  # second table (private sector) starts at TOTAL GENERAL / OTROS / PATRONATOS
  cut2 <- which(grepl("TOTAL GENERAL|OTROS ESTABLECIMIENTOS|PATRONATOS", dt$rn))[1]
  dt[, sector := if(is.na(cut2)) "publico" else fifelse(.I < cut2, "publico", "privado")]
  dt[sector=="privado", prov := NA_character_]                  # private block: muni via name only
  vcols <- grep("_(lt15|a1519)$", names(dt), value=TRUE)
  chk <- merge(dt[kind=="fac" & sector=="publico", lapply(.SD, sum, na.rm=TRUE), by=prov, .SDcols=vcols],
               dt[kind=="prov", lapply(.SD, sum, na.rm=TRUE), by=prov, .SDcols=vcols], by="prov")
  bad <- chk[abs(vaginal_a1519.x - vaginal_a1519.y) > pmax(2, .02*vaginal_a1519.y)]
  cat(yr, ": fac rows", dt[kind=="fac",.N], "| provinces", dt[kind=="prov",.N],
      "| missing:", paste(setdiff(provs, dt[kind=="prov", prov]), collapse=","),
      "| prov-sum mismatches:", nrow(bad), "\n")
  if(nrow(bad)) print(bad[, .(prov, fac=vaginal_a1519.x, provrow=vaginal_a1519.y)], class=FALSE)
  dt <- dt[kind=="fac"]; dt[, anio := yr]
  out[[as.character(yr)]] <- dt[, c("raw","rn","prov","sector","anio", vcols), with=FALSE]
}
a <- rbindlist(out, fill=TRUE)
cat("\nTOTAL facility-year rows:", nrow(a), "| national teen (15-19) by year:\n")
print(a[, .(deliv=sum(vaginal_a1519+cesarea_a1519, na.rm=TRUE), abort=sum(aborto_a1519, na.rm=TRUE)), keyby=anio], class=FALSE)

# ---- municipio assignment: seed from 05n xwalk, then registry passes ----
seed <- fread(file.path(DIR_CLEAN,"mispas_facility_xwalk.csv"))[!is.na(adm3) & adm3!=""]
seed <- unique(seed[, .(pn=norm(prov), en=norm(est), adm3_seed=adm3)])[, .SD[1], by=.(pn,en)]
reg <- as.data.table(suppressMessages(read_excel(file.path(DRPAPER,
  "analysis/datasets/Hospitals and PCU in DR/Hospitales-y-Centros-de-Primer-Nivel-de-Atencion-act-09-04-2026.xlsx"))))
c2a <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_code_to_adm3.rds")))
reg <- merge(reg[, .(name_reg=NombreCentro, prov_id=Id_Provincia, muni_id=Id_Municipio, prov_reg=Provincia)],
             c2a, by.x=c("prov_id","muni_id"), by.y=c("prov_code","muni_code"), all.x=TRUE)
reg[, `:=`(pn=norm(prov_reg), en=norm(name_reg))]
fac <- unique(a[, .(pn=prov, en=rn)])
fac <- merge(fac, seed, by=c("pn","en"), all.x=TRUE)
r1 <- unique(reg[!is.na(adm3_pcode), .(pn, en, a1=adm3_pcode)])[, .SD[1], by=.(pn,en)]
fac <- merge(fac, r1, by=c("pn","en"), all.x=TRUE)
r2n <- reg[!is.na(adm3_pcode), .N, by=en][N==1, en]
fac <- merge(fac, unique(reg[en %in% r2n & !is.na(adm3_pcode), .(en, a2=adm3_pcode)]), by="en", all.x=TRUE)
fac[, a3 := NA_character_]
nom <- fac[is.na(adm3_seed) & is.na(a1) & is.na(a2)]
for(i in seq_len(nrow(nom))){
  rp <- reg[pn==nom$pn[i] & !is.na(adm3_pcode)]
  if(!nrow(rp)) next
  hit <- rp[grepl(nom$en[i], rp$en, fixed=TRUE) | mapply(grepl, rp$en, nom$en[i], fixed=TRUE), adm3_pcode]
  if(length(unique(hit))==1) {fac[pn==nom$pn[i] & en==nom$en[i], a3 := hit[1]]; next}
  dd <- adist(nom$en[i], rp$en)[1,]
  ok <- which(dd <= 0.25*nchar(nom$en[i]))
  if(length(unique(rp$adm3_pcode[ok]))==1) {fac[pn==nom$pn[i] & en==nom$en[i], a3 := rp$adm3_pcode[ok[1]]]; next}
  tk <- strsplit(nom$en[i], " ")[[1]]              # token-subset: all my tokens inside theirs
  sub <- sapply(strsplit(rp$en, " "), function(t) all(tk %in% t) || all(t %in% tk))
  if(length(unique(rp$adm3_pcode[sub]))==1) fac[pn==nom$pn[i] & en==nom$en[i], a3 := rp$adm3_pcode[which(sub)[1]]]
}
# manual aliases for historical names with typos/renames/military/patronato
# facilities absent from the SNS registry — municipio via crosswalk muni name
mget <- function(pat) xw[grepl(pat, toupper(stri_trans_general(adm3_name,"Latin-ASCII")))][1, adm3_pcode]
man <- data.table(
  en  = c("MMARCELINO VELEZ","HAINA","PLAZA SALUD","MILITAR RAMON LARA",
          "CENTRAL FUERZA ARMADA","PERIFERICO ENSANCHEZ LIBERTAD","TAMBORIL ICO MARTINEZ",
          "JULIA SANTAJA"),   # = Hosp. Julia Santana, Tamayo, Bahoruco (MM 2026-07-13)
  a3m = c(reg[grepl("MARCELINO VELEZ", en), adm3_pcode][1], mget("BAJOS DE HAINA"),
          mget("SANTO DOMINGO DE GUZM"), mget("SANTO DOMINGO ESTE"),
          mget("SANTO DOMINGO DE GUZM"), mget("SANTIAGO DE LOS CAB"), mget("TAMBORIL"),
          mget("^TAMAYO")))
fac <- merge(fac, man, by="en", all.x=TRUE)
fac[is.na(a3) & !is.na(a3m), a3 := a3m]
fac[, adm3 := fcoalesce(adm3_seed, a1, a2, a3)]
a <- merge(a, fac[, .(prov=pn, rn=en, adm3)], by=c("prov","rn"), all.x=TRUE)
ev <- a[, .(ev=sum(vaginal_a1519+cesarea_a1519+aborto_a1519, na.rm=TRUE)), by=.(prov, rn, matched=!is.na(adm3))]
cat(sprintf("\nMUNI ASSIGNMENT: %.1f%% of facility-names, %.1f%% teen-event-weighted\n",
    100*mean(ev[, any(matched), by=.(prov,rn)]$V1),
    100*ev[matched==TRUE, sum(ev)]/ev[, sum(ev)]))
cat("top unmatched:\n"); print(ev[matched==FALSE][order(-ev)][1:8, .(prov, rn, ev)], class=FALSE)
saveRDS(a, file.path(DIR_CLEAN, "mispas_cuadro29_annual.rds"))
cat("saved -> mispas_cuadro29_annual.rds (", nrow(a), "rows )\n")
