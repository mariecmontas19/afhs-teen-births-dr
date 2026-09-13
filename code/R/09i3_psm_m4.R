# ============================================================================
# 09i3_psm_m4.R — MM follow-up: does PROPENSITY-SCORE matching with the M4
# variable set change the result vs Mahalanobis? Same eligible pool (never-
# treated with SNS hospital), same vars (structural + cwr2010 + DHS2013
# fertility + proximity), logit PS, nearest 1:3 with replacement. With 20
# treated and 9 covariates the PS is at risk of overlap failure/separation —
# reported below. Output: appended to matched_balance.csv / matched_cs.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(MatchIt)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
fm <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
fl <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_facility_levels_panel.rds")))[year==2019, .(adm3_pcode, pop_tot, n_hosp)]
fa <- as.data.table(readRDS(file.path(DIR_CLEAN,"facility_access_panel.rds")))[year==2019, .(adm3_pcode, near_any_km)]
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))[!is.na(adm3_pcode) & age_mom %in% 15:19 & birth_year==2019]
b[adm3_pcode %in% names(fm), adm3_pcode := fm[adm3_pcode]]
comp19 <- b[, .(age_mean = mean(age_mom), sh_haitian = 100*mean(nationality=="Haitian", na.rm=TRUE),
                sh_noins = 100*mean((insurance=="None")[!is.na(insurance)]),
                sh_csec  = 100*mean((csection==TRUE | csection==1)[!is.na(csection)])), by=adm3_pcode]
base <- Reduce(function(a,b2) merge(a,b2,by="adm3_pcode",all.x=TRUE),
  list(p[year==2019, .(adm3_pcode, rate2019=rateA_15_19, wealth_index, pct_urban, pct_educ_secplus,
                       pct_senasa_2013, cwr_2010_baseline, dhs_teenasfr_2013)], hc, fl, fa, comp19))
base[, `:=`(log_pop = log(pop_tot), has_hosp = as.integer(n_hosp>0))]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
base <- merge(base, tr[!is.na(g), .(adm3_pcode, g, treated=as.integer(g>0))], by="adm3_pcode")
V4 <- c("wealth_index","pct_urban","pct_educ_secplus","sns_per10k","pct_senasa_2013","log_pop",
        "cwr_2010_baseline","dhs_teenasfr_2013","near_any_km")
REPORT <- unique(c(V4,"age_mean","sh_haitian","sh_noins","sh_csec","rate2019"))
smd <- function(d, v) sapply(v, function(x){
  mt <- d[treated==1][[x]]; mc <- d[treated==0][[x]]
  (mean(mt,na.rm=TRUE)-mean(mc,na.rm=TRUE))/sqrt((var(mt,na.rm=TRUE)+var(mc,na.rm=TRUE))/2) })

pool <- base[treated==1 | has_hosp==1]
dd <- pool[complete.cases(pool[, ..V4])]
f <- as.formula(paste("treated ~", paste(V4, collapse="+")))
mo <- matchit(f, data=dd, method="nearest", distance="glm", ratio=3, replace=TRUE)
ps <- mo$distance
cat("PS overlap: treated range", paste(round(range(ps[dd$treated==1]),3), collapse="-"),
    "| control range", paste(round(range(ps[dd$treated==0]),3), collapse="-"),
    "| controls above min treated PS:", sum(ps[dd$treated==0] >= min(ps[dd$treated==1])), "\n")
mm <- as.data.table(match.data(mo, drop.unmatched=TRUE))
keep <- unique(mm$adm3_pcode)
cat(sprintf("PSM-M4: %d munis (%d treated + %d controls)\n", length(keep),
    pool[adm3_pcode %in% keep, sum(treated)], pool[adm3_pcode %in% keep, sum(1-treated)]))
balpost <- smd(base[adm3_pcode %in% keep], REPORT)
est <- merge(p[, .(adm3_pcode, year, rateA_15_19)], base[adm3_pcode %in% keep, .(adm3_pcode, g)], by="adm3_pcode")
est[, id := as.integer(factor(adm3_pcode))]
a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
      xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
      bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
bs <- est[g>0][, .SD[year==g-1L], by=adm3_pcode][, mean(rateA_15_19)]
cat(sprintf("PSM-M4 CS: ATT %.2f (SE %.2f, p=%.3f) | base %.1f (%.0f%%)  [Mahalanobis-M4: -4.11 (2.56, p=.109)]\n",
    o$overall.att, o$overall.se, 2*pnorm(-abs(o$overall.att/o$overall.se)), bs, 100*o$overall.att/bs))
bal <- data.table(variable=REPORT, matched_on=REPORT %in% V4, smd_pre=round(smd(base,REPORT),3),
                  smd_post=round(balpost,3), spec="M5: PSM (logit) with M4 vars")
cs <- data.table(spec="M5: PSM (logit) with M4 vars",
                 n_treated=pool[adm3_pcode %in% keep, sum(treated)], n_controls=pool[adm3_pcode %in% keep, sum(1-treated)],
                 ATT=round(o$overall.att,2), SE=round(o$overall.se,2),
                 p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3), baseline=round(bs,2),
                 pct=round(100*o$overall.att/bs,1))
print(bal, class=FALSE)
fwrite(rbind(fread(file.path(TAB,"matched_balance.csv")), bal, fill=TRUE), file.path(TAB,"matched_balance.csv"))
fwrite(rbind(fread(file.path(TAB,"matched_cs.csv")), cs, fill=TRUE), file.path(TAB,"matched_cs.csv"))
cat("appended -> matched_balance.csv + matched_cs.csv\n")
