# ============================================================================
# 08o6_ages2029.R — which OLDER ages respond? Single years 20..29 + band totals
# (20-24, 25-29), under BOTH classifications (age at birth / age at conception).
# Denominators: single years 20..24 per 1,000 women 20-24 (contributions that
# sum to the 20-24 band effect); 25..29 per 1,000 women 25-29; band totals use
# their own denominators (= the standard rates, comparable to 07e placebos).
# Window 2018-2025 (mother_dob constraint, as 08o); S1; CS spec = 07t.
# 30-34 reference: placebo-clean (-0.28 ns, 07d). Output: ages2029.csv
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
       .(adm3_pcode, year, womenA_20_24, womenA_25_29)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]

run_class <- function(agecol, vlab){
  bx <- copy(b)[, ac := get(agecol)][ac %in% 20:29]
  cnt <- dcast(bx, adm3_pcode + birth_year ~ ac, fun.aggregate=length, value.var="ac")
  setnames(cnt, "birth_year", "year")
  aa <- as.character(20:29); for(v in aa) if(!v %in% names(cnt)) cnt[, (v):=0L]
  setnames(cnt, aa, paste0("a",aa))
  d0 <- merge(p, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
  for(v in paste0("a",20:29)) d0[is.na(get(v)), (v):=0L]
  d0[, `:=`(a2024=a20+a21+a22+a23+a24, a2529=a25+a26+a27+a28+a29)]
  d0 <- d0[!adm3_pcode %in% fold]
  for(a in 20:24) d0[, (paste0("r_a",a)) := 1000*get(paste0("a",a))/womenA_20_24]
  for(a in 25:29) d0[, (paste0("r_a",a)) := 1000*get(paste0("a",a))/womenA_25_29]
  d0[, `:=`(r_a2024=1000*a2024/womenA_20_24, r_a2529=1000*a2529/womenA_25_29)]
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
  labs <- data.table(yn=c(paste0("r_a",20:29), "r_a2024","r_a2529"),
                     lab=c(as.character(20:29), "20--24 (band)","25--29 (band)"))
  rbindlist(lapply(seq_len(nrow(labs)), function(i) one(labs$yn[i], labs$lab[i])))
}
res <- rbind(run_class("age_mom","Age at BIRTH"), run_class("age_conc","Age at CONCEPTION"))
cat("\n=== ages 20-29 (2018-25; S1; singles per 1,000 women of own band) ===\n")
print(res, class=FALSE)
fwrite(res, file.path(TAB,"ages2029.csv"))
cat("saved -> ages2029.csv\n")
