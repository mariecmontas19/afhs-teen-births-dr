# ============================================================================
# 02d_projection_B_intercensal.R  —  DENOMINATOR SERIES B (intercensal 2010<->2022)
#
# B uses BOTH actual censuses. On the pre-2013 "155 geography" (= series A's geography):
#   2016-2021 : LINEAR interpolation of the 2010 & 2022 censuses (age-sex-group counts)
#   2022      : 2022 census (harmonized to 155 geography)
#   2023-2025 : Hamilton-Perry extrapolation from 2022, raked to census-anchored province targets
#
# Geography harmonization: 3 municipios created in 2013 (verified: San Victor<-Moca [Ley 85-13],
# Matanzas<-Bani [Ley 111-13], Baitoa<-Santiago [Ley 69-13]) are FOLDED into their parent
# (province cabecera, muni code 1) so the 2022 census matches the 2010/2015-2020 division.
# 2010 census codes == A's 155 codes (verified set-equal). 155 municipios (matches series A).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))

AGE_LEVELS <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
ai <- data.table(age_group=AGE_LEVELS, ax=1:17); CCR_LO<-0.3; CCR_HI<-3
ag5 <- function(a) ifelse(a>=80,"80+", paste0(5L*(a%/%5L),"-",5L*(a%/%5L)+4L))

xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
code2pc <- xw[, .(pcv=as.integer(substr(adm3_pcode,6,7)), mcv=as.integer(substr(adm3_pcode,8,9)), adm3_pcode)]
# verified 2013-created children -> parent (cabecera) pcode
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) ifelse(p %in% names(fold), fold[p], p)

# ---- 2010 census -> 155 geography (pcode x sex x age_group) ----
f2010 <- file.path(DRPAPER,"analysis/datasets/population/CENSO2010RD-PERSONAS.csv")
c10 <- fread(f2010, select=c("PROVINCIA","MUNICIPIO","P27_SEXO","P29_EDAD_ANOS_CUMPLIDOS"), showProgress=FALSE)
c10 <- merge(c10, code2pc, by.x=c("PROVINCIA","MUNICIPIO"), by.y=c("pcv","mcv"), all.x=TRUE)
stopifnot(sum(is.na(c10$adm3_pcode))==0)                                   # all 2010 codes map
c10[, `:=`(sex=ifelse(P27_SEXO==1,"M","F"), age_group=ag5(P29_EDAD_ANOS_CUMPLIDOS))]
cen2010 <- c10[, .(p2010=.N), by=.(adm3_pcode, sex, age_group)]
cat("--- B: 2010 census ---\n")
cat("municipios:", uniqueN(cen2010$adm3_pcode), "(expect 155) | total:", format(sum(cen2010$p2010),big.mark=","), "\n")

# ---- 2022 census -> 155 geography (fold the 3 children into parents) ----
ca <- as.data.table(readRDS(file.path(DIR_CLEAN,"census2022_by_adm3_long.rds")))  # 158
ca[, adm3_pcode := to155(adm3_pcode)]
cen2022 <- ca[, .(p2022=sum(pop)), by=.(adm3_pcode, sex, age_group)]
cat("2022 census folded to 155: municipios:", uniqueN(cen2022$adm3_pcode),
    " | total:", format(sum(cen2022$p2022),big.mark=","), "\n")
stopifnot(setequal(cen2010$adm3_pcode, cen2022$adm3_pcode), uniqueN(cen2022$adm3_pcode)==155)

# ---- 2016-2021: linear interpolation of the two censuses (per pcode,sex,age) ----
base <- merge(cen2010, cen2022, by=c("adm3_pcode","sex","age_group"), all=TRUE)
stopifnot(!anyNA(base))                                                    # both censuses cover all cells
interp <- rbindlist(lapply(2016:2021, function(t){
  g <- copy(base); g[, pop := p2010 + (p2022-p2010)*((t-2010)/(2022-2010))][, year := t]
  g[, .(adm3_pcode, sex, age_group, year, pop)] }))
cat("\n--- B: interpolation 2016-2021 --- rows:", nrow(interp),
    " | neg:", any(interp$pop<0), " NA:", anyNA(interp$pop), "\n")

# ---- 2023-2025: Hamilton-Perry from 2022, raked to census-anchored province targets ----
pm <- merge(as.data.table(readRDS(file.path(DIR_CLEAN,"pop_muni_2015_2020_long.rds"))), ai, by="age_group")
pc <- merge(as.data.table(readRDS(file.path(DIR_CLEAN,"prov_controls_2000_2030_long.rds"))), ai, by="age_group")
ccr_from <- function(d0,d1){
  prev <- d0[, .(adm3_pcode,sex,ax=ax+1L,p_prev=pop)]
  R <- merge(d1[, .(adm3_pcode,sex,ax,p1=pop)], prev, by=c("adm3_pcode","sex","ax"), all.x=TRUE)
  R <- merge(R, d0[ax==17,.(adm3_pcode,sex,p17=pop)], by=c("adm3_pcode","sex"), all.x=TRUE)
  R[ax==17, p_prev:=p_prev+p17][, CCR:=p1/p_prev][ax==1, CCR:=NA_real_]
  R[, CCRb:=pmin(pmax(CCR,CCR_LO),CCR_HI)][, .(adm3_pcode,sex,ax,CCRb)] }
project5 <- function(b, ccr){
  prev <- b[, .(adm3_pcode,sex,ax=ax+1L,p_prev=pop)]
  P <- merge(b[, .(adm3_pcode,sex,ax,pop)], prev, by=c("adm3_pcode","sex","ax"), all.x=TRUE)
  P <- merge(P, b[ax==17,.(adm3_pcode,sex,p17=pop)], by=c("adm3_pcode","sex"), all.x=TRUE)
  P[ax==17, p_prev:=p_prev+p17]
  P <- merge(P, ccr, by=c("adm3_pcode","sex","ax"), all.x=TRUE)
  P[, proj:=CCRb*p_prev][ax==1, proj:=pop][is.na(proj), proj:=pop][, .(adm3_pcode,sex,ax,proj)] }

ccr <- ccr_from(pm[year==2015], pm[year==2020])                            # 155 pcodes
c22ax <- merge(cen2022, ai, by="age_group")[, .(adm3_pcode,sex,ax,pop=p2022)]
P27 <- project5(c22ax, ccr)
grid <- merge(c22ax[, .(adm3_pcode,sex,ax,c=pop)], P27, by=c("adm3_pcode","sex","ax"))
raw_fwd <- rbindlist(lapply(2023:2025, function(t){
  g<-copy(grid); g[, pop:=c+(proj-c)*((t-2022)/5)][, year:=t]; g[,.(adm3_pcode,sex,ax,year,pop)] }))
# census-anchored province targets: prov_census_2022 * ONE growth(t)/(2022)
p2prov <- xw[, .(adm3_pcode, prov_code)]
prov22 <- merge(c22ax, p2prov, by="adm3_pcode")[, .(t22=sum(pop)), by=.(prov_code,sex,ax)]
tg <- rbindlist(lapply(2023:2025, function(t){
  r <- merge(pc[year==t,.(prov_code,sex,ax,ct=pop)], pc[year==2022,.(prov_code,sex,ax,c22=pop)],
             by=c("prov_code","sex","ax"))[, r:=ct/c22]
  merge(prov22, r[,.(prov_code,sex,ax,r)], by=c("prov_code","sex","ax"))[, .(prov_code,sex,ax,year=t,target=t22*r)] }))
rk <- merge(raw_fwd, p2prov, by="adm3_pcode")
rk[, gs := sum(pop), by=.(prov_code,sex,ax,year)]
rk <- merge(rk, tg, by=c("prov_code","sex","ax","year"))[, pop := pop*target/gs]
chk <- rk[, .(s=sum(pop), t=target[1]), by=.(prov_code,sex,ax,year)]
fwd <- merge(rk[,.(adm3_pcode,sex,ax,year,pop)], ai, by="ax")[, .(adm3_pcode,sex,age_group,year,pop)]
cat("\n--- B: H-P extrapolation 2023-2025 --- max|raked-target|:", round(max(abs(chk$s-chk$t)),6),
    " | neg:", any(fwd$pop<0), "\n")

# ---- assemble B (155, 2016-2025) ----
B <- rbind(interp, cen2022[, .(adm3_pcode,sex,age_group,pop=p2022)][, year:=2022L], fwd)
setorder(B, adm3_pcode, sex, year, age_group)
cat("\n--- B assembled ---\n")
cat("rows:", nrow(B), "(expect 155*10*2*17 =", 155*10*2*17, ") | municipios:", uniqueN(B$adm3_pcode),
    " | years:", paste(range(B$year),collapse="-"), " | neg/NA:", any(B$pop<0)||anyNA(B$pop), "\n")
cat("national F 15-19 by year (series B):\n")
print(B[sex=="F" & age_group=="15-19", .(F1519=round(sum(pop))), by=year][order(year)])
saveRDS(B, file.path(DIR_CLEAN, "pop_municipio_2016_2025_B.rds"))
cat("saved -> data/clean/pop_municipio_2016_2025_B.rds\n")
