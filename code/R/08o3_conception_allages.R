# ============================================================================
# 08o3_conception_allages.R — age-at-conception decomposition EXTENDED to all
# adolescent ages 10-20 (committee follow-up to 08o2). Same design: outcome for
# age band a = 1000 * (births CONCEIVED at a) / womenA_15_19 (COMMON denominator
# -> rows comparable as contributions; 15..19 sum to the -8.22 headline).
# Ages 10-12 GROUPED (only 38/171/1,006 births nationally 2018-25 - single-year
# CS would be noise); 13 and 14 single. EXTRA row: 10-14 group per 1,000 women
# 10-14 (its OWN denominator = the standard 10-14 conception rate).
# Window 2018-2025; S1; CS spec = 07t. Output: conception_allages.csv
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

# ---- counts per municipality-year by conception-age band ----
b[, band := fifelse(age_conc %in% 10:12, "b1012",
           fifelse(age_conc %in% 13:20, paste0("b", age_conc), NA_character_))]
cnt <- dcast(b[!is.na(band)], adm3_pcode + birth_year ~ band, fun.aggregate=length, value.var="band")
setnames(cnt, "birth_year", "year")
bb <- c("b1012", paste0("b", 13:20))
for(v in bb) if(!v %in% names(cnt)) cnt[, (v) := 0L]

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[year %in% 2018:2025,
       .(adm3_pcode, year, womenA_15_19, womenA_10_14)]
d0 <- merge(p, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
for(v in bb) d0[is.na(get(v)), (v):=0L]
d0[, `:=`(b1014 = b1012+b13+b14, b1517 = b15+b16+b17, b1819 = b18+b19)]
d0 <- d0[!adm3_pcode %in% fold]
allb <- c(bb, "b1014", "b1517", "b1819")
for(v in allb) d0[, (paste0("r_",v)) := 1000*get(v)/womenA_15_19]      # common 15-19 denom
d0[, r_b1014_own := 1000*b1014/womenA_10_14]                            # own 10-14 denom
cat("panel:", nrow(d0), "rows (expect 1240) |", d0[,uniqueN(adm3_pcode)], "munis\n")

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
est0 <- merge(d0, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est0[, id := as.integer(factor(adm3_pcode))]

csg <- function(yname){
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
        xformla=~1, data=est0, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  bs <- est0[g>0 & year==g-1L, mean(get(yname))]
  data.table(ATT=round(o$overall.att,3), SE=round(o$overall.se,3),
             p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3),
             baseline=round(bs,2), pct=round(100*o$overall.att/bs,0))
}
bands <- data.table(
  yn    = c(paste0("r_", bb), "r_b1014", "r_b1014_own", "r_b1517", "r_b1819"),
  label = c("10--12", as.character(13:20), "10--14 (subtotal)", "10--14 (per 1,000 women 10--14)",
            "15--17 (subtotal)", "18--19 (subtotal)"))
res <- rbindlist(lapply(seq_len(nrow(bands)), function(i) cbind(age_at_conception=bands$label[i], csg(bands$yn[i]))))
cat("\n=== effect by AGE AT CONCEPTION, ages 10-20 (2018-25; S1) ===\n")
print(res, class=FALSE)
sy <- res[age_at_conception %in% as.character(15:19)]
cat(sprintf("\nDecomp check: sum ATT 15..19 = %.2f (08o headline -8.22)\n", sum(sy$ATT)))
fwrite(res, file.path(TAB,"conception_allages.csv"))
cat("saved -> conception_allages.csv\n")
