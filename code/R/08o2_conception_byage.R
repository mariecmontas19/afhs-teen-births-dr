# ============================================================================
# 08o2_conception_byage.R — single-year decomposition of the AGE-AT-CONCEPTION
# effect (companion to 08o and to Table 5/08n). Outcome for age a:
#   rate_a = 1000 * (births CONCEIVED at age a) / womenA_15_19  (COMMON denom)
# so ages 15..19 sum to the 15-19 conception headline (-8.22). Ages 14 and 20
# at conception included as context rows (NOT part of the 15-19 sum).
# Window 2018-2025 (mother_dob is a 2018+ field, see 08o); S1; CS spec = 07t;
# inconsistent dob/gestation records set NA as in 08o.
# Output: output/tables/conception_byage.csv
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

ages <- 14:20
cnt <- b[age_conc %in% ages, .N, by=.(adm3_pcode, year=birth_year, age_conc)]
cnt <- dcast(cnt, adm3_pcode + year ~ age_conc, value.var="N", fill=0L)
setnames(cnt, as.character(ages), paste0("c",ages))
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[year %in% 2018:2025,
       .(adm3_pcode, year, womenA_15_19)]
d0 <- merge(p, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
cc <- paste0("c",ages); for(v in cc) d0[is.na(get(v)), (v):=0L]
d0[, `:=`(c1517=c15+c16+c17, c1819=c18+c19)]
d0 <- d0[!adm3_pcode %in% fold]
for(v in c(cc,"c1517","c1819")) d0[, (paste0("rate_",v)) := 1000*get(v)/womenA_15_19]
cat("panel:", nrow(d0), "rows |", d0[,uniqueN(adm3_pcode)], "munis\n")

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
  data.table(ATT=round(o$overall.att,2), SE=round(o$overall.se,2),
             p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3),
             baseline=round(bs,2), pct=round(100*o$overall.att/bs,0))
}
bands <- data.table(yn=c(paste0("rate_c",ages), "rate_c1517", "rate_c1819"),
                    label=c(as.character(ages), "15--17", "18--19"))
res <- rbindlist(lapply(seq_len(nrow(bands)), function(i) cbind(age_at_conception=bands$label[i], csg(bands$yn[i]))))
cat("\n=== effect by single year of AGE AT CONCEPTION (common 15-19 denom; 2018-25; S1) ===\n")
print(res, class=FALSE)
sy <- res[age_at_conception %in% as.character(15:19)]
cat(sprintf("\nDecomp check: sum ATT 15..19 = %.2f (08o conception headline -8.22); sum baselines = %.2f (67.64)\n",
            sum(sy$ATT), sum(sy$baseline)))
fwrite(res, file.path(TAB,"conception_byage.csv"))
cat("saved -> conception_byage.csv\n")
