# ============================================================================
# 05n4_mispas_mechanisms.R — MECHANISM cuadros from quarterly INFORMEs (§59.15):
#  C21 (2020Q1-2026Q1): prenatal-control consultations, ADOLESCENTES vs ADULTAS,
#      by facility (3 data cols).
#  C33 (2022Q1-2026Q1): consejeria + planificacion familiar by facility
#      (two month-blocks; we take each block's TOTAL column).
# Quarterly INFORMEs only (annuals skipped; 2020 annual lacks C21 anyway;
# 2021 partial julio-agosto file 19384 EXCLUDED — superseded by Q3 19565).
# Same REGION/PROVINCIA/FACILITY hierarchy as Cuadro 29 -> same sequential
# parse; facility -> muni via lookups already built in 05n/05n2 + registry.
# Output: data/clean/mispas_mechanisms_quarter.rds
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})
D <- file.path(DRPAPER, "analysis/datasets/SNS/mispas_informes")
norm <- function(x){
  x <- toupper(stri_trans_general(as.character(x), "Latin-ASCII"))
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- sub("^\\s*[0-9]+\\s+", "", x)
  x <- gsub("\\bFCO\\b", "FRANCISCO", x)
  x <- gsub("\\b(HOSPITAL|HOSP|MUNICIPAL|MUNIC|PROVINCIAL|REGIONAL|GENERAL|DOCTOR|DRA?|MATERNIDAD|MATERNO|MAT|INFANTIL|UNIVERSITARIO|CENTRO|SANITARIO|LOCAL|PTE|PDTE|PRESIDENTE|SUB|DE|DEL|LA|EL|LOS|LAS|Y)\\b", " ", x)
  x <- gsub("\\b[A-Z]\\b", " ", x)
  gsub(" +", " ", trimws(x))
}
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
provs <- unique(norm(xw$prov_name))
palias <- c("SALCEDO"="HERMANAS MIRABAL","SAN PEDRO"="SAN PEDRO MACORIS",
            "BAHORUCO"="BAORUCO","SEYBO"="SEIBO","ELIAS PINAS"="ELIAS PINA")

qfiles <- grep("Trimestre.*(INFORME|Informe)", list.files(D), value=TRUE)
qfiles <- qfiles[!grepl("19384", qfiles)]
meta <- data.table(file=qfiles)
meta[, `:=`(anio=as.integer(sub("_.*","",file)),
            q=as.integer(substr(sub(".*_(\\d)(er|do|to)Trimestre.*","\\1", file),1,1)))]
# 2020/2021 quarterly files are "direct" named — build their meta from titles
dfiles <- grep("^202[01]_direct_INFORME", list.files(D), value=TRUE)
MESQ <- c("ENERO-MARZO"=1,"ABRIL-JUNIO"=2,"JULIO-SEPTIEMBRE"=3,"OCTUBRE-DICIEMBRE"=4)

parse_hier <- function(M, ar){
  lab <- trimws(M[(ar+1):nrow(M), 1]); rows <- which(!is.na(lab) & lab!="") + ar
  rn <- norm(M[rows,1])
  isfac <- grepl("^\\s*(HOSPITAL|CLINICA|CENT|CTRO|MATERNIDAD|CIUDAD)", toupper(stri_trans_general(M[rows,1],"Latin-ASCII")))
  kind <- character(length(rows)); prv <- rep(NA_character_, length(rows)); cur <- NA_character_
  pm <- vapply(rn, function(r){
    if(is.na(r) || nchar(r)<4) return(NA_character_)
    if(r %in% provs) return(r); if(r %in% names(palias)) return(unname(palias[r]))
    dd <- drop(adist(r, provs)); if(min(dd)<=2 && sum(dd==min(dd))==1) return(provs[which.min(dd)])
    hit <- provs[startsWith(r, paste0(provs," ")) | startsWith(provs, paste0(r," "))]
    if(length(hit)==1) return(hit); NA_character_}, character(1), USE.NAMES=FALSE)
  for(i in seq_along(rows)){
    r <- rn[i]
    if(grepl("^REGION", r)) {kind[i] <- "region"; next}
    if(grepl("TOTAL GENERAL|^TOTAL$|OTROS ESTABLECIMIENTOS|PATRONATOS|FUENTE|NOTA", r)) {kind[i] <- "total"; next}
    if(!is.na(pm[i]) && !isfac[i] && !identical(pm[i], cur)) {kind[i]<-"prov"; cur<-pm[i]; prv[i]<-cur; next}
    kind[i] <- "fac"; prv[i] <- cur
  }
  list(rows=rows, rn=rn, kind=kind, prov=prv)
}

read21 <- function(f){
  sh <- excel_sheets(f); s <- sh[grep("PREN", toupper(sh))][1]; if(is.na(s)) return(NULL)
  M <- as.matrix(suppressMessages(read_excel(f, sheet=s, col_names=FALSE)))
  cell <- function(r) toupper(trimws(stri_trans_general(ifelse(is.na(M[r,]),"",M[r,]),"Latin-ASCII")))
  hr <- which(sapply(1:min(10,nrow(M)), function(r) any(cell(r)=="MUJERES ADOLESCENTES")))[1]
  if(is.na(hr)) return(NULL)
  hd <- cell(hr)
  cA <- which(hd=="MUJERES ADOLESCENTES")[1]; cU <- which(hd=="MUJERES ADULTAS")[1]
  h <- parse_hier(M, hr)
  data.table(rn=h$rn[h$kind=="fac"], prov=h$prov[h$kind=="fac"],
             pren_adol=suppressWarnings(as.numeric(M[h$rows[h$kind=="fac"], cA])),
             pren_adult=suppressWarnings(as.numeric(M[h$rows[h$kind=="fac"], cU])))
}
read33 <- function(f){
  sh <- excel_sheets(f); s <- sh[grep("PLANIF|CONSEJ", toupper(sh))][1]; if(is.na(s)) return(NULL)
  M <- as.matrix(suppressMessages(read_excel(f, sheet=s, col_names=FALSE)))
  cell <- function(r) toupper(trimws(stri_trans_general(ifelse(is.na(M[r,]),"",M[r,]),"Latin-ASCII")))
  br <- which(sapply(2:min(10,nrow(M)), function(r) any(cell(r) %in% c("SONSEJERIA","CONSEJERIA"))))[1] + 1
  if(is.na(br)) return(NULL)
  mr <- which(sapply(br:(br+3), function(r) any(cell(r)=="TOTAL")))[1] + br - 1
  bl <- cell(br)
  cur <- NA_character_; blk <- rep(NA_character_, ncol(M))
  for(j in seq_len(ncol(M))){ if(grepl("ONSEJERIA", bl[j])) cur <- "consej"
    else if(grepl("PLANIFICACION", bl[j])) cur <- "planif"; blk[j] <- cur }
  tots <- which(cell(mr)=="TOTAL")
  cC <- tots[blk[tots]=="consej"][1]; cP <- tots[blk[tots]=="planif"][1]
  h <- parse_hier(M, mr)
  data.table(rn=h$rn[h$kind=="fac"], prov=h$prov[h$kind=="fac"],
             consej=suppressWarnings(as.numeric(M[h$rows[h$kind=="fac"], cC])),
             planif=suppressWarnings(as.numeric(M[h$rows[h$kind=="fac"], cP])))
}

pieces <- list()
for(i in seq_len(nrow(meta))){
  f <- file.path(D, meta$file[i])
  d21 <- read21(f); d33 <- read33(f)
  d <- if(!is.null(d21) && !is.null(d33)) merge(d21, d33, by=c("rn","prov"), all=TRUE) else if(!is.null(d21)) d21 else d33
  if(is.null(d)) next
  d[, `:=`(anio=meta$anio[i], q=meta$q[i])]; pieces[[meta$file[i]]] <- d
}
for(f0 in dfiles){
  f <- file.path(D, f0); sh <- excel_sheets(f)
  s <- sh[grep("PREN", toupper(sh))][1]; if(is.na(s)) next
  ttl <- toupper(stri_trans_general(as.character(suppressMessages(read_excel(f, sheet=s, col_names=FALSE, n_max=1))[1,1]),"Latin-ASCII"))
  per <- names(MESQ)[sapply(names(MESQ), function(p) grepl(p, ttl))]
  if(!length(per)) next                                   # annual or unparsable
  d <- read21(f); if(is.null(d)) next
  d[, `:=`(anio=as.integer(sub(".*ANO ","",sub("\\.$","",ttl))), q=MESQ[per[1]])]
  pieces[[f0]] <- d
}
m <- rbindlist(pieces, fill=TRUE)
cat("facility-quarter rows:", nrow(m), "| coverage:\n")
print(dcast(m[, .(pren=sum(!is.na(pren_adol)), c33=sum(!is.na(consej))), by=.(anio,q)],
      anio ~ q, value.var="pren"), class=FALSE)

# ---- muni via existing lookups + registry ----
lk1 <- fread(file.path(DIR_CLEAN,"mispas_facility_xwalk.csv"))[!is.na(adm3) & adm3!="", .(pn=norm(prov), en=norm(est), adm3)]
lk2 <- unique(as.data.table(readRDS(file.path(DIR_CLEAN,"mispas_cuadro29_annual.rds")))[!is.na(adm3), .(pn=prov, en=rn, adm3)])
lk <- unique(rbind(lk1, lk2))[, .SD[1], by=.(pn,en)]
mget <- function(pat) xw[grepl(pat, toupper(stri_trans_general(adm3_name,"Latin-ASCII")))][1, adm3_pcode]
lk <- rbind(lk, data.table(
  pn = c("VALVERDE","DISTRITO NACIONAL","SANTO DOMINGO","DISTRITO NACIONAL","SANTIAGO","BAORUCO"),
  en = c("FAUSTO OVALLE ESPERANZA","UNIDAD QUEMADOS PEARL ORT SANTO DOMINGO",
         "HUGO MENDOZA","DOCENTE PADRE BILLINI","PERIFERICO ANTONIO TRUEDA MONTE ADENTRO",
         "JULIA SANTAJA"),   # Julia Santana = Tamayo, Bahoruco (MM)
  adm3 = c(mget("^ESPERANZA"), mget("SANTO DOMINGO DE GUZM"), mget("SANTO DOMINGO NORTE"),
           mget("SANTO DOMINGO DE GUZM"), mget("SANTIAGO DE LOS CAB"),
           mget("^TAMAYO"))))[, .SD[1], by=.(pn,en)]
m <- merge(m, lk, by.x=c("prov","rn"), by.y=c("pn","en"), all.x=TRUE)
ev <- m[, sum(pren_adol, na.rm=TRUE), by=is.na(adm3)]
cat(sprintf("muni match: %.1f%% of adolescent prenatal consults\n",
    100*ev[is.na==FALSE, V1]/sum(ev$V1)))
print(m[is.na(adm3), .(ev=sum(pren_adol, na.rm=TRUE)), by=.(prov,rn)][order(-ev)][1:6], class=FALSE)
saveRDS(m, file.path(DIR_CLEAN,"mispas_mechanisms_quarter.rds"))
cat("saved -> mispas_mechanisms_quarter.rds\n")
