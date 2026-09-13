# ============================================================================
# 05h_jee_rollout.R — municipio JEE (Jornada Escolar Extendida) intensity at the
# 3 school-years with downloadable per-school tanda data: 2019-2020, 2020-2021,
# 2024-2025. School-COUNT measure (robust to the level-mixing in the 2019-21 files,
# which give school-level tanda marginals NOT crossed with nivel):
#   JEE intensity = % of SECONDARY-OFFERING schools in the municipio that operate
#   any extended-day (Jornada Extendida) tanda.
# Consistent definition across all 3 years:
#   sec_school = school has secondary students (>0)
#   jee_school = school has any extended-day enrolment (>0)
#   pct_jee    = 100 * #(sec & jee) / #(sec)
# Sources (datasets/education/): 2019-20 CSV (public+semioficial, "Extendida"),
#   2020-21 XLSX (all sectors, "JORNADA EXTENDIDA"), 2024-25 8xl (per centro x nivel,
#   Tanda; mapped to municipio via school_muni_crosswalk.rds from 05g).
# CAVEATS: 3 snapshots (not annual); 2019-20 public+semioficial only; "datos
# preliminares"; school-level extended-day flag (a sec-offering school flagged JEE
# may be extended only at primary). Use for confound check + heterogeneity, NOT as
# a precise secondary-JEE rate. Output: data/clean/jee_rollout_muni.rds (long).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(stringi); library(readxl)})
ED   <- file.path(DRPAPER,"analysis/datasets/education")
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155<- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)
numv <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.-]","",as.character(x))))

mc <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[, .(prov_nm=norm(prov_name), muni_nm=norm(adm3_name), adm3_pcode)]

# ---- per-year school-level (prov, muni, sec, jee) ----------------------------
# ALL three years restricted to PUBLIC + SEMIOFICIAL (JEE is a public-sector program; the 2019-20 file
# already excludes private, so restricting the others makes the 3 snapshots COMPARABLE — without this the
# private schools, ~none extended-day, spuriously drag down the 2020-21/2024-25 shares).
PUBSEMI <- c("PUBLICO","SEMIOFICIAL")
# 2019-2020 CSV (22 cols): 4=Sector 11=Secundario 14=Extendida 21=Provincia 22=Municipio
s1920 <- fread(file.path(ED,"centros_2019-2020_publicos_tanda.csv"), sep=";", encoding="Latin-1",
               skip=11, header=FALSE, fill=TRUE)
s1920 <- s1920[norm(V4) %in% PUBSEMI]
s1920 <- s1920[, .(prov_nm=norm(V21), muni_nm=norm(V22), sec=numv(V11)>0 & !is.na(numv(V11)),
                   jee=numv(V14)>0 & !is.na(numv(V14)))]

# 2020-2021 XLSX (30 cols): 13=Secundario 18=JORNADA EXTENDIDA 29=Provincia 30=Municipio (data from row 13)
x21 <- as.data.table(suppressMessages(read_excel(file.path(ED,"centros_2020-2021_activos_tanda.xlsx"),
            sheet=1, skip=12, col_names=FALSE)))
cat("2020-21 XLSX read:", nrow(x21), "rows x", ncol(x21), "cols (expect 30)\n")
x21 <- x21[norm(x21[[4]]) %in% PUBSEMI]                    # col4 = Sector
s2021 <- data.table(prov_nm=norm(x21[[29]]), muni_nm=norm(x21[[30]]),
                    sec=numv(x21[[13]])>0 & !is.na(numv(x21[[13]])),
                    jee=numv(x21[[18]])>0 & !is.na(numv(x21[[18]])))

# 2024-2025 8xl (per centro x nivel) -> dedup to plant; map municipio via 05g crosswalk
xl <- fread(file.path(ED,"8xl-relacion-de-centros-educativos-2024-2025csv.csv"), sep=";",
            encoding="Latin-1", skip=1, header=TRUE, fill=TRUE)
setnames(xl, grep("Matr", names(xl), value=TRUE)[1], "matricula")
xl <- xl[norm(Sector) %in% PUBSEMI]                        # restrict to public+semioficial for comparability
xl[, plant := sub(" .*","", trimws(`Planta Fisica`))]
sch2425 <- xl[, .(sec=any(Nivel=="3-Secundario" & numv(matricula)>0, na.rm=TRUE),
                  jee=any(Tanda=="Jornada Extendida", na.rm=TRUE)), by=plant]
xwsch <- as.data.table(readRDS(file.path(DIR_CLEAN,"school_muni_crosswalk.rds")))[, .(plant, prov_nm, muni_nm)]
s2425 <- merge(sch2425, xwsch, by="plant", all.x=TRUE)[, .(prov_nm, muni_nm, sec, jee)]

# ---- aggregate one year's schools -> municipio JEE intensity ------------------
agg_year <- function(s, yr){
  s <- s[!is.na(prov_nm) & sec==TRUE]                       # secondary-offering only
  m <- merge(s, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
  un <- m[is.na(adm3_pcode), .N, by=.(prov_nm,muni_nm)][order(-N)]
  if(nrow(un)) { cat(sprintf("  [%s] unmapped sec-schools: %d rows, names:\n", yr, sum(un$N))); print(head(un,4), class=FALSE) }
  m <- m[!is.na(adm3_pcode)]; m[, adm3_pcode := to155(adm3_pcode)]
  out <- m[, .(n_sec=.N, n_jee=sum(jee), pct_jee_sch=round(100*mean(jee),1), school_year=yr), by=adm3_pcode]
  cat(sprintf("  [%s] municipios=%d | national pct sec-schools extended-day=%.1f%% (sec-schools=%d)\n",
              yr, nrow(out), 100*sum(m$jee)/nrow(m), nrow(m)))
  out
}
cat("=== building JEE intensity per municipio, 3 snapshots ===\n")
pan <- rbindlist(list(agg_year(s1920,"2019-2020"), agg_year(s2021,"2020-2021"), agg_year(s2425,"2024-2025")))
stopifnot(uniqueN(pan$adm3_pcode)<=155, !anyNA(pan$pct_jee_sch))
saveRDS(pan, file.path(DIR_CLEAN,"jee_rollout_muni.rds"))

cat("\n=== national school-count JEE share by year (sec-offering schools that are extended-day) ===\n")
print(pan[, .(municipios=.N, natl_pct=round(100*sum(n_jee)/sum(n_sec),1)), by=school_year][order(school_year)], class=FALSE)
cat("\npct_jee_sch distribution across municipios, by year:\n")
print(pan[, as.list(round(quantile(pct_jee_sch, c(0,.25,.5,.75,1)),0)), by=school_year][order(school_year)], class=FALSE)
cat("\nsaved -> data/clean/jee_rollout_muni.rds (", nrow(pan), "municipio-year rows)\n", sep="")
