# ============================================================================
# 08z13_educ_excl_laromana.R — the "excluding La Romana" rows of Table 6
# (MM 2026-11-16). La Romana (DOM081201) is the only municipality that opened
# in 2019; its secondary gross enrollment ratio fell sharply in 2020-2021, so
# Table 6 reports the enrollment, dropout and overage estimates with and
# without it. This script re-runs 08z3 (enrollment and dropout by sex) and
# 08z5 (overage decomposition) with EDUC_DROP_PCODE set; each script keeps its
# own seed, so the full-sample outputs are untouched and the excluded-sample
# outputs are reproducible. It also records the enrollment-ratio series quoted
# in the table note and in the text.
# Output: output/tables/educ_final_bysex_numeric_excl_DOM081201.csv,
#         educ_final_bysex_excl_DOM081201.csv, educ_overage_decomp_excl_DOM081201.csv,
#         educ_laromana_ger.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
LR <- "DOM081201"
Sys.setenv(EDUC_DROP_PCODE = LR)
tryCatch({
  for (s in c("08z3_educ_final_table.R", "08z5_educ_overage_decomp.R"))
    sys.source(here::here("code","R",s), envir = new.env())
}, finally = Sys.unsetenv("EDUC_DROP_PCODE"))

# ---- the series behind "fell from 108 to 93": total secondary GER, as built in 08z3 ----
pan <- as.data.table(readRDS(file.path(DIR_CLEAN,"educ_condicion_muni.rds")))[nivel=="SECUNDARIO"]
agg <- pan[, .(mat=sum(mat)), by=.(adm3_pcode, year_end)]
pop <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))[age_group %in% c("10-14","15-19")]
pop <- pop[, .(pop=0.6*sum(pop)), by=.(adm3_pcode, year_end=year)]
d   <- merge(agg, pop, by=c("adm3_pcode","year_end")); d[, ger := 100*mat/pop]
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% c("DOM010905","DOM051703","DOM012510")]
stopifnot(trt[adm3_pcode==LR, first_year]==2019L, trt[ever_treated==1L & always_treated==0L & first_year==2019L, .N]==1L)
out <- merge(d[adm3_pcode==LR, .(year_end, la_romana=ger)],
             d[adm3_pcode %in% trt[ever_treated==0L, adm3_pcode], .(never_treated_mean=mean(ger)), by=year_end], by="year_end")
fwrite(out[, .(year_end, la_romana=round(la_romana,2), never_treated_mean=round(never_treated_mean,2))],
       file.path(DIR_TABLES,"educ_laromana_ger.csv"))
cat(sprintf("La Romana GER %.1f (2019) -> %.1f (2021); never-treated mean %.1f -> %.1f\n",
            out[year_end==2019, la_romana], out[year_end==2021, la_romana],
            out[year_end==2019, never_treated_mean], out[year_end==2021, never_treated_mean]))
cat("saved -> *_excl_DOM081201.csv + educ_laromana_ger.csv\n")
