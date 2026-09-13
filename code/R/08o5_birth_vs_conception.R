# ============================================================================
# 08o5_birth_vs_conception.R — side-by-side: AGE AT BIRTH vs AGE AT CONCEPTION,
# same window (2018-2025), same spec, same common womenA_15_19 denominator.
# Rows: 10-14 (grouped context), single ages 15..19, 20 (context),
#       subtotals 15-17, 18-19, and the 15-19 total.
# Age at conception = recorded-gestage version (03's conception_date), the
# base definition (08o4 variant A). Computes BOTH classifications fresh in one
# script so every number shares the seed/spec. S1. CS spec = 07t.
# Output: output/tables/birth_vs_conception.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
foldmap <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
b <- b[!is.na(adm3_pcode) & birth_year %in% 2018:2025]
b[adm3_pcode %in% names(foldmap), adm3_pcode := foldmap[adm3_pcode]]
b[, age_conc := floor(as.numeric(difftime(conception_date, mother_dob, units="days"))/365.25)]
b[!is.na(age_conc) & (abs(age_mom-age_conc)>1 | age_conc>age_mom), age_conc := NA]

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[year %in% 2018:2025,
       .(adm3_pcode, year, womenA_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]

band <- function(a) fifelse(a %in% 10:14, "b1014", fifelse(a %in% 15:20, paste0("b",a), NA_character_))
bb <- c("b1014", paste0("b",15:20))

run_class <- function(agecol, vlab){
  bx <- copy(b)[, bd := band(get(agecol))]
  cnt <- dcast(bx[!is.na(bd)], adm3_pcode + birth_year ~ bd, fun.aggregate=length, value.var="bd")
  setnames(cnt, "birth_year", "year")
  for(v in bb) if(!v %in% names(cnt)) cnt[, (v):=0L]
  d0 <- merge(p, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
  for(v in bb) d0[is.na(get(v)), (v):=0L]
  d0[, `:=`(b1517=b15+b16+b17, b1819=b18+b19, b1519=b15+b16+b17+b18+b19)]
  d0 <- d0[!adm3_pcode %in% fold]
  vs <- c(bb, "b1517","b1819","b1519")
  for(v in vs) d0[, (paste0("r_",v)) := 1000*get(v)/womenA_15_19]
  est0 <- merge(d0, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  est0[, id := as.integer(factor(adm3_pcode))]
  one <- function(yname, lab){
    a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
          xformla=~1, data=est0, control_group="notyettreated", base_period="universal", est_method="reg",
          bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
    o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
    bs <- est0[g>0 & year==g-1L, mean(get(yname))]
    data.table(classification=vlab, age=lab, ATT=round(o$overall.att,2), SE=round(o$overall.se,2),
               p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3),
               baseline=round(bs,2), pct=round(100*o$overall.att/bs,0))
  }
  labs <- data.table(yn=paste0("r_",vs),
                     lab=c("10--14","15","16","17","18","19","20","15--17","18--19","15--19"))
  rbindlist(lapply(seq_len(nrow(labs)), function(i) one(labs$yn[i], labs$lab[i])))
}
res <- rbind(run_class("age_mom",  "Age at BIRTH"),
             run_class("age_conc", "Age at CONCEPTION"))
cat("\n=== age at birth vs age at conception (2018-25; S1; common 15-19 denom) ===\n")
print(res, class=FALSE)
for(v in unique(res$classification)){
  sy <- res[classification==v & age %in% as.character(15:19)]
  cat(sprintf("%s: sum singles = %.2f vs 15--19 row = %.2f\n", v, sum(sy$ATT), res[classification==v & age=="15--19", ATT]))
}
fwrite(res, file.path(TAB,"birth_vs_conception.csv"))
cat("saved -> birth_vs_conception.csv\n")
