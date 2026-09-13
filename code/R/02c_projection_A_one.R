# ============================================================================
# 02c_projection_A_one.R  —  DENOMINATOR SERIES A (ONE official)
#
# A = ONE municipio estimates 2016-2020 + ONE official PROVINCE projections 2021-2025
# distributed to municipios via Hamilton-Perry, raked to ONE province projections.
# Does NOT use the 2022 census. 155 municipios (ONE doesn't separate the 3 post-2020 ones).
# Reuses the validated H-P machinery (same CCR/project5 as 02b).
#   2016-2020 : ONE observed municipio (already == ONE province totals)
#   2021-2025 : H-P from 2020 base, linear-annualized, raked to ONE province projections
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))

AGE_LEVELS <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
ai <- data.table(age_group=AGE_LEVELS, ax=1:17); CCR_LO<-0.3; CCR_HI<-3

pm <- merge(as.data.table(readRDS(file.path(DIR_CLEAN,"pop_muni_2015_2020_long.rds"))), ai, by="age_group")
pc <- merge(as.data.table(readRDS(file.path(DIR_CLEAN,"prov_controls_2000_2030_long.rds"))), ai, by="age_group")
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
p2c <- xw[, .(adm3_pcode, prov_code)]

ccr_from <- function(d0,d1){
  prev <- d0[, .(adm3_pcode,sex,ax=ax+1L,p_prev=pop)]
  R <- merge(d1[, .(adm3_pcode,sex,ax,p1=pop)], prev, by=c("adm3_pcode","sex","ax"), all.x=TRUE)
  R <- merge(R, d0[ax==17,.(adm3_pcode,sex,p17=pop)], by=c("adm3_pcode","sex"), all.x=TRUE)
  R[ax==17, p_prev := p_prev+p17][, CCR:=p1/p_prev][ax==1, CCR:=NA_real_]
  R[, CCRb:=pmin(pmax(CCR,CCR_LO),CCR_HI)][, .(adm3_pcode,sex,ax,CCRb)]
}
project5 <- function(base, ccr){
  prev <- base[, .(adm3_pcode,sex,ax=ax+1L,p_prev=pop)]
  P <- merge(base[, .(adm3_pcode,sex,ax,pop)], prev, by=c("adm3_pcode","sex","ax"), all.x=TRUE)
  P <- merge(P, base[ax==17,.(adm3_pcode,sex,p17=pop)], by=c("adm3_pcode","sex"), all.x=TRUE)
  P[ax==17, p_prev:=p_prev+p17]
  P <- merge(P, ccr, by=c("adm3_pcode","sex","ax"), all.x=TRUE)
  P[, proj:=CCRb*p_prev][ax==1, proj:=pop][is.na(proj), proj:=pop]
  P[, .(adm3_pcode,sex,ax,proj)]
}

ccr <- ccr_from(pm[year==2015], pm[year==2020])
P25 <- project5(pm[year==2020,.(adm3_pcode,sex,ax,pop)], ccr)              # 2020 -> 2025
grid <- merge(pm[year==2020,.(adm3_pcode,sex,ax,b=pop)], P25, by=c("adm3_pcode","sex","ax"))
raw <- rbindlist(lapply(2021:2025, function(t){
  g<-copy(grid); g[, pop:=b+(proj-b)*((t-2020)/5)][, year:=t]; g[,.(adm3_pcode,sex,ax,year,pop)] }))
cat("--- A: raw H-P (2021-2025) | neg:", any(raw$pop<0), " NA:", anyNA(raw$pop), "\n")

# rake each 2021-2025 year to ONE province projections (raw prov_controls)
tg <- pc[year %in% 2021:2025, .(prov_code,sex,ax,year,target=pop)]
rk <- merge(raw, p2c, by="adm3_pcode")
rk[, gs := sum(pop), by=.(prov_code,sex,ax,year)]
rk <- merge(rk, tg, by=c("prov_code","sex","ax","year"))
rk[, pop := pop*target/gs]
chk <- rk[, .(s=sum(pop), t=target[1]), by=.(prov_code,sex,ax,year)]
cat("max |raked muni sum - ONE prov projection|:", round(max(abs(chk$s-chk$t)),6),
    " | neg:", any(rk$pop<0), "\n")

# assemble A: 2016-2020 observed + 2021-2025 raked  (155 municipios)
A <- rbind(pm[year %in% 2016:2020, .(adm3_pcode,sex,ax,year,pop)], rk[,.(adm3_pcode,sex,ax,year,pop)])
A <- merge(A, ai, by="ax")[, .(adm3_pcode,sex,age_group,year,pop)]
setorder(A, adm3_pcode, sex, year, age_group)
cat("\n--- A assembled ---\n")
cat("rows:", nrow(A), "(expect 155*10*2*17 =", 155*10*2*17, ") | municipios:", uniqueN(A$adm3_pcode),
    " | years:", paste(range(A$year),collapse="-"), " | neg/NA:", any(A$pop<0)||anyNA(A$pop), "\n")
cat("national F 15-19 by year (series A):\n")
print(A[sex=="F" & age_group=="15-19", .(F1519=round(sum(pop))), by=year][order(year)])
saveRDS(A, file.path(DIR_CLEAN, "pop_municipio_2016_2025_A.rds"))
cat("saved -> data/clean/pop_municipio_2016_2025_A.rds\n")
