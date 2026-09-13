# ============================================================================
# 04f_placement.R — committee item R5 (§44.5): what predicts WHERE the units
# were placed? "Determinants of rollout" analysis, standard in staggered DiD.
#  (1) EXTENSIVE margin: treated (first opening 2020-25) vs never, among the
#      146 estimation municipalities. LPM with HC-robust SEs + logit AMEs on
#      2019/baseline characteristics: log pop, hospital presence (SNS Nivel
#      II-III in 2019), wealth, % urban, % secondary educ, SNS per 10k,
#      SeNaSa, baseline teen rate 2019 (descriptive: did they target need?),
#      DHS 2013 teen fertility (independent need measure).
#  (2) TIMING among the 20 treated: cohort year ~ same covariates (n=20,
#      descriptive only).
# NO political variable exists in our data (party of mayor etc. not collected)
# -> stated, not proxied. Institutional record: program is First Lady-
# associated, units sit inside existing SNS hospitals (fieldwork, §13).
# Output: output/tables/placement_determinants.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(sandwich); library(lmtest)})
fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
fl <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_facility_levels_panel.rds")))[year==2019, .(adm3_pcode, pop_tot, n_hosp)]
d <- merge(p[year==2019, .(adm3_pcode, rate2019=rateA_15_19, wealth_index, pct_urban, pct_educ_secplus, pct_senasa_2013, dhs_teenasfr_2013)],
           hc, by="adm3_pcode", all.x=TRUE)
d <- merge(d, fl, by="adm3_pcode", all.x=TRUE)
d[, `:=`(log_pop = log(pop_tot), has_hosp = as.integer(n_hosp>0))]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
d <- merge(d, tr[!is.na(g), .(adm3_pcode, g, treated=as.integer(g>0))], by="adm3_pcode")
cat("sample:", nrow(d), "munis |", sum(d$treated), "treated | hospital presence: treated",
    d[treated==1, mean(has_hosp)], "vs never", round(d[treated==0, mean(has_hosp)],2), "\n")

X <- c("log_pop","has_hosp","rate2019","dhs_teenasfr_2013","wealth_index","pct_urban","pct_educ_secplus","sns_per10k","pct_senasa_2013")
dd <- d[complete.cases(d[, ..X])]
cat("complete cases:", nrow(dd), "(", sum(dd$treated), "treated )\n")
# standardize continuous X for comparable coefficients
for(v in setdiff(X,"has_hosp")) dd[, (paste0("z_",v)) := scale(get(v))]
Z <- c(paste0("z_", setdiff(X,"has_hosp")), "has_hosp")
f <- as.formula(paste("treated ~", paste(Z, collapse="+")))

# (1) LPM + robust SE
m1 <- lm(f, dd); ct <- coeftest(m1, vcov=vcovHC(m1, type="HC1"))
lpm <- data.table(term=rownames(ct), est=round(ct[,1],4), se=round(ct[,2],4), p=round(ct[,4],3), model="LPM (treated 0/1)")
# logit AME
m2 <- glm(f, dd, family=binomial())
pr <- predict(m2, type="response")
ame <- sapply(Z, function(v){ d1 <- copy(dd); d0 <- copy(dd)
  if(v=="has_hosp"){ d1[, has_hosp:=1L]; d0[, has_hosp:=0L] } else { d1[, (v):=get(v)+1]; d0[, (v):=get(v)] }
  mean(predict(m2, newdata=d1, type="response") - predict(m2, newdata=d0, type="response")) })
lgt <- data.table(term=names(ame), est=round(ame,4), se=NA_real_, p=NA_real_, model="Logit AME (+1sd / 0->1)")
res <- rbind(lpm, lgt)
cat("\n=== (1) extensive margin: what predicts getting a unit (standardized X) ===\n")
print(lpm, class=FALSE)
cat("\nLogit AMEs:\n"); print(lgt[term!="(Intercept)"], class=FALSE)

# (2) timing among treated (descriptive, n small)
t2 <- dd[treated==1]
m3 <- lm(as.formula(paste("g ~", paste(Z, collapse="+"))), t2)
ct3 <- coeftest(m3, vcov=vcovHC(m3, type="HC1"))
tim <- data.table(term=rownames(ct3), est=round(ct3[,1],3), se=round(ct3[,2],3), p=round(ct3[,4],3), model=sprintf("Timing: cohort year (n=%d, descriptive)", nrow(t2)))
cat("\n=== (2) timing among the 20 treated (descriptive) ===\n"); print(tim, class=FALSE)

fwrite(rbind(res, tim), file.path(TAB,"placement_determinants.csv"))
cat("\nsaved -> placement_determinants.csv\n")
