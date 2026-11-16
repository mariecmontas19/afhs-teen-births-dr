# ============================================================================
# 08z5_educ_overage_decomp.R — decompose the +1.60 enrollment effect into
# OVERAGE vs roughly-ON-TRACK components, by grade group (MM 2026-07-29,
# Proposal A). Tests WHO the enrollment gain is: older/behind-schedule
# adolescents (overage) vs younger on-track students.
#
# SOBREEDAD HARMONIZED to a consistent "3+ years behind" series (verified
# 2026-07-29): the pre-2024 single "Sobreedad" column matches the 2024-25
# "3 o mas Anos de Rezago" tier grade-by-grade (6.1 vs 6.3, etc.), NOT the
# two-tier sum. So use the single column pre-2024 and the 3+ tier in 2024-25.
# (This fixes the definitional break that failed the earlier overage run.)
# 2018-19 sobre absent -> impute the COUNT from 2018 & 2020 (pre-treatment
# cell only; g_edu=first_year+1 => 2019 never enters a post-treatment ATT).
#
# Decomposition (additive, all /100 of pop 15-19 Series A both sexes):
#   overage_rate = 100*sobre/pop ; ontime_rate = 100*(mat-sobre)/pop
#   overage + ontime = total enrollment rate.
# Output: output/tables/educ_overage_decomp.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi); library(did)})
set.seed(20260729)
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155<- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)
F1 <- file.path(DRPAPER,"analysis/datasets/SAIP",
                "OAI-0877-2026 Matricula Desagregada 2015-2025 Condicion Final y Sobreedad.xlsx")
mc <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[
        , .(prov_nm=norm(prov_name), muni_nm=norm(adm3_name), adm3_pcode)]
GR <- c("PRIMERO","SEGUNDO","TERCERO","CUARTO","QUINTO","SEXTO")
grp_map <- c(PRIMERO="1-3", SEGUNDO="1-3", TERCERO="1-3",
             CUARTO="4-5", QUINTO="4-5", SEXTO="6")

pieces <- list()
for(s in excel_sheets(F1)){
  peek <- suppressMessages(as.matrix(read_excel(F1, sheet=s, col_names=FALSE, n_max=6)))
  hr <- which(apply(peek, 1, function(r) any(r=="Municipio", na.rm=TRUE) & any(r=="Nivel", na.rm=TRUE)))[1]
  d <- suppressMessages(as.data.table(read_excel(F1, sheet=s, skip=hr-1)))
  d[, tg := suppressWarnings(as.numeric(`Total general`))]
  socols <- grep("^Sobreedad", names(d), value=TRUE)
  if(length(socols)==0){
    d[, so := NA_real_]                                                   # 2018-19: column genuinely absent
  } else {
    col <- if(length(socols)==1) socols else grep("3", socols, value=TRUE)[1]  # single=3+ defn; 2024-25 take 3+ tier
    d[, so := suppressWarnings(as.numeric(get(col)))]
    d[is.na(so), so := 0]                                                 # blank cell = 0 overage students (NOT missing)
  }
  d <- d[norm(Nivel)=="SECUNDARIO" & norm(Grado) %in% GR & !is.na(tg)]
  d <- d[, .(prov_nm=norm(Provincia), muni_nm=norm(Municipio), grp=grp_map[norm(Grado)],
             mat=tg, sobre=so)]
  d[, school_year := s]; pieces[[s]] <- d
}
ed <- rbindlist(pieces); ed[, year_end := as.integer(substr(school_year,6,9))]
ed[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
ed <- merge(ed, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
un <- ed[is.na(adm3_pcode), .N, by=.(prov_nm,muni_nm)]
for(i in seq_len(nrow(un))){ cand <- mc[prov_nm==un$prov_nm[i]]; if(!nrow(cand)) next
  dd <- drop(adist(un$muni_nm[i], cand$muni_nm))
  if(min(dd)<=3 && sum(dd==min(dd))==1)
    ed[prov_nm==un$prov_nm[i] & muni_nm==un$muni_nm[i], adm3_pcode := cand$adm3_pcode[which.min(dd)]] }
ed <- ed[!is.na(adm3_pcode)]; ed[, adm3_pcode := to155(adm3_pcode)]

# aggregate to muni x year x group (also a TOTAL-secondary group), pooling sexes
g1 <- ed[, .(mat=sum(mat), sobre=sum(sobre)), by=.(adm3_pcode, year_end, grp)]
g0 <- ed[, .(grp="ALL secondary", mat=sum(mat), sobre=sum(sobre)), by=.(adm3_pcode, year_end)]
gp <- rbind(g1, g0)
# GER denominator: official secondary ages 12-17 (§59.48), both sexes.
pop <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))[
         age_group %in% c("10-14","15-19")]
pop <- pop[, .(pop = 0.6*sum(pop[age_group=="10-14"]) + 0.6*sum(pop[age_group=="15-19"])),
           by=.(adm3_pcode, year_end=year)]
gp <- merge(gp, pop, by=c("adm3_pcode","year_end"), all.x=TRUE)
# impute 2019 sobre COUNT per muni x group (pre-treatment cell)
setorder(gp, adm3_pcode, grp, year_end)
gp[, s18 := sobre[year_end==2018][1], by=.(adm3_pcode,grp)]
gp[, s20 := sobre[year_end==2020][1], by=.(adm3_pcode,grp)]
gp[year_end==2019 & is.na(sobre), sobre := (s18+s20)/2]
gp[, c("s18","s20") := NULL]
# verify the 2019 imputation actually filled every muni x group (else att_gt
# balancing drops the whole panel)
n19 <- gp[year_end==2019 & is.na(sobre), .N]
cat("2019 sobre still NA after imputation:", n19, "\n")
stopifnot(n19 == 0)
gp[, `:=`(overage_rate = 100*sobre/pop, ontime_rate = 100*(mat-sobre)/pop)]
cat("non-finite overage_rate:", gp[!is.finite(overage_rate), .N],
    "| non-finite ontime_rate:", gp[!is.finite(ontime_rate), .N], "\n")

trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[
         !adm3_pcode %in% names(fold)]
trt[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]
runD <- function(grp_, yvar){
  d <- merge(gp[grp==grp_], trt[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  d[, g_edu := fifelse(g==0L, 0L, g+1L)]; d[, id := as.integer(factor(adm3_pcode))]
  d <- d[is.finite(get(yvar))]
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  pre <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,-2)]
  lp <- 2*pnorm(-abs(pre[,sum(att/se^2)/sum(1/se^2)]/sqrt(1/pre[,sum(1/se^2)])))
  base <- d[g_edu>0 & year_end==g_edu-1L, mean(get(yvar), na.rm=TRUE)]
  data.table(group=grp_, component=sub("_rate","",yvar),
             ATT=round(gg$overall.att,3), SE=round(gg$overall.se,3),
             p=round(2*pnorm(-abs(gg$overall.att/gg$overall.se)),3), lead_p=round(lp,3),
             base=round(base,3))
}
grps <- c("ALL secondary","1-3","4-5","6")
res <- rbindlist(lapply(grps, function(x) rbind(runD(x,"overage_rate"), runD(x,"ontime_rate"))))
cat("\n============ ENROLLMENT DECOMPOSITION: OVERAGE (3+ yrs behind) vs ON-TRACK ============\n")
cat("per 100 pop 12-17 (GER convention); overage + ontime = total; CS not-yet, universal base.\n\n")
print(res, class=FALSE)
fwrite(res, file.path(PROJ,"output","tables","educ_overage_decomp.csv"))
cat("saved -> educ_overage_decomp.csv\n")
