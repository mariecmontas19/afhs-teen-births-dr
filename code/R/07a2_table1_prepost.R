# ============================================================================
# 07a2_table1_prepost.R — Table 1 descriptives for the deck/paper: treated (H1,
# first opening 2020-25) vs never-treated municipalities, PRE (2016-19) vs POST
# (2020-25). All cohorts open 2020+, so the calendar split is pre-treatment for
# everyone. Time-varying outcomes by period; baseline characteristics once.
# Output: output/tables/table1_prepost.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
if(!"sns_per10k" %in% names(p)){ hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]; p <- merge(p, hc, by="adm3_pcode", all.x=TRUE) }
stopifnot(all(c("pct_senasa_2013","rateA_30_34") %in% names(p)))
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, arm := fifelse(ever_treated==1L & always_treated==0L, "Treated", fifelse(ever_treated==0L, "Control", NA_character_))]
d <- merge(p, tr[!is.na(arm), .(adm3_pcode, arm)], by="adm3_pcode")
d[, period := fifelse(year<=2019L, "Pre", "Post")]
cat("munis:", d[,uniqueN(adm3_pcode)], "| treated:", d[arm=="Treated",uniqueN(adm3_pcode)], "\n")
tv <- d[, .(teen_rate=mean(rateA_15_19), rate3034=mean(rateA_30_34),
            teen_births=mean(nb_15_19), women1519=mean(womenA_15_19)), by=.(arm, period)]
bl <- d[year==2016, .(wealth=mean(wealth_index), urban=100*mean(pct_urban),
            educ=100*mean(pct_educ_secplus), sns=mean(sns_per10k), senasa=mean(pct_senasa_2013)), by=arm]
out <- dcast(melt(tv, id=c("arm","period")), variable ~ arm + period)
setcolorder(out, c("variable","Treated_Pre","Treated_Post","Control_Pre","Control_Post"))
print(out[, lapply(.SD, function(x) if(is.numeric(x)) round(x,1) else x)], class=FALSE)
print(bl[, lapply(.SD, function(x) if(is.numeric(x)) round(x,2) else x)], class=FALSE)
fwrite(out, file.path(TAB,"table1_prepost.csv")); fwrite(bl, file.path(TAB,"table1_prepost_baseline.csv"))
cat("saved -> table1_prepost.csv + table1_prepost_baseline.csv\n")
