# ============================================================================
# 09_robustness.R — robustness battery under the NEW specs S1 (binary any-unit)
# and S2 (modernization). All Callaway-Sant'Anna (group ATT, est=reg, universal
# base, municipio-clustered, multiplier bootstrap) so no did2s/BLAS segfault.
# Same fold(3) + treatment construction as the 07t headline. Denominator A unless noted.
#
# Outcomes: level teen rate rateA_15_19 ; pre-differenced age-DDD ddd_A (=15-19 − 30-34).
# Blocks (Galarraga's list adapted + extras):
#   A  comparison-pop / pre-period / weighting / serial-corr / anticipation / drop-imputed
#   B  log spec ; alternate comparison ages (25-29, 20-24) ; denominator B
#   C  leave-one-out Santo Domingo metro (DN + Santo Domingo province)
#   D  in-time PLACEBO (fake treatment g-2, pre-treatment years only -> expect ~0)
# Cross-ESTIMATOR robustness (SA/BJS/G2S/TWFE) is in 08_estimators.R ; the proper
# triple-difference never-vs-not-yet comparison-group robustness is in 07u_checks.R.
# NOT installed (no R-4.3 binary), documented not run: de Chaisemartin-D'Haultfoeuille
# (DIDmultiplegtDYN), wild-cluster bootstrap (fwildclusterboot).
# Output: output/tables/robustness_S1_S2.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); BITERS <- 2000L

p0  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]

# build a scenario estimation frame (drop always-treated NA-g)
mkest <- function(gt){
  d <- merge(p0, gt[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  d[, id := as.integer(factor(adm3_pcode))]
  d[, wt_base := womenA_15_19[year==2016L], by=id]
  d[, `:=`(ddd_2529 = rateA_15_19 - rateA_25_29, ddd_2024 = rateA_15_19 - rateA_20_24,
           log_rate = log1p(rateA_15_19))]
  d[]
}

res <- list(); add <- function(sc,block,label,outcome,att,se,note=""){
  res[[length(res)+1]] <<- data.table(scenario=sc, block=block, spec=label, outcome=outcome,
    ATT=round(att,2), SE=round(se,2), p=round(2*pnorm(-abs(att/se)),3), note=note) }

cs <- function(d, yname, ctrl="notyettreated", wname=NULL, anticip=0L, gvar="g"){
  tryCatch({
    a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname=gvar,
          xformla=~1, data=d, control_group=ctrl, weightsname=wname, anticipation=anticip,
          bstrap=TRUE, cband=TRUE, biters=BITERS, base_period="universal", clustervars="id", est_method="reg")))
    gr <- suppressWarnings(suppressMessages(aggte(a, type="group", na.rm=TRUE)))
    c(gr$overall.att, gr$overall.se)
  }, error=function(e) c(NA_real_, NA_real_))
}

# pre-trends placebo: equally-weighted avg of pre-treatment ATT(g,t) cells, IF-based SE
ptest <- function(d, yname){
  tryCatch({
    a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
          xformla=~1, data=d, control_group="notyettreated", bstrap=FALSE, base_period="universal",
          clustervars="id", est_method="reg")))
    idx <- which(a$t < a$group & abs(a$att) > 1e-12)
    l <- rep(1/length(idx), length(idx)); pt <- sum(l*a$att[idx])
    IF <- as.matrix(a$inffunc)[, idx, drop=FALSE]; n <- nrow(IF); V <- crossprod(IF)/n/n
    c(pt, sqrt(as.numeric(t(l)%*%V%*%l)), a$Wpval)
  }, error=function(e) c(NA_real_, NA_real_, NA_real_))
}

run_scenario <- function(sc, gt){
  d <- mkest(gt)
  ## A — Galarraga-style, level + pre-differenced DDD
  for(yn in c("rateA_15_19","ddd_A")){
    r<-cs(d,yn);                                   add(sc,"A","A0 headline (not-yet)",yn,r[1],r[2])
    r<-cs(d,yn, wname="wt_base");                  add(sc,"A","A1 population-weighted",yn,r[1],r[2])
    r<-cs(d,yn, ctrl="nevertreated");              add(sc,"A","A2 never-treated only",yn,r[1],r[2])
    r<-cs(d,yn, anticip=1L);                       add(sc,"A","A3 anticipation 1yr",yn,r[1],r[2])
    r<-cs(d[cohort_imputed==FALSE], yn);           add(sc,"A","A4 drop imputed cohorts",yn,r[1],r[2])
  }
  ## B — log ; alt comparison ages ; denominator B
  r<-cs(d,"log_rate");  add(sc,"B","B1 log1p(rate)","log_rate",r[1],r[2],"~ proportional")
  r<-cs(d,"ddd_2529");  add(sc,"B","B2 comparison age 25-29","ddd_2529",r[1],r[2],"25-29 contaminated (07e)")
  r<-cs(d,"ddd_2024");  add(sc,"B","B3 comparison age 20-24","ddd_2024",r[1],r[2],"20-24 contaminated (07e)")
  r<-cs(d,"rateB_15_19");add(sc,"B","B4 denominator B (level)","rateB_15_19",r[1],r[2],"intercensal denom")
  r<-cs(d,"ddd_B");     add(sc,"B","B5 denominator B (DDD)","ddd_B",r[1],r[2],"intercensal denom")
  ## C — leave-one-out Santo Domingo metro
  dno <- d[!prov_norm %in% c("DISTRITO NACIONAL","SANTO DOMINGO")]
  r<-cs(dno,"rateA_15_19"); add(sc,"C","C1 drop Santo Domingo metro (level)","rateA_15_19",r[1],r[2],
       sprintf("%d treated remain", uniqueN(dno[g>0,adm3_pcode])))
  r<-cs(dno,"ddd_A");       add(sc,"C","C2 drop Santo Domingo metro (DDD)","ddd_A",r[1],r[2])
  ## D — PRE-TRENDS placebo: equally-weighted average of the pre-treatment ATT(g,t)
  ##     cells (t<g) from the real event study, IF-based SE (the did Wald pre-test is
  ##     singular here). Want ~0 / ns = parallel pre-trends. (Visual leads in headline
  ##     figs; HonestDiD sensitivity in 09b; anticipation in A3.)
  for(yn in c("rateA_15_19","ddd_A")){ r<-ptest(d,yn)
    add(sc,"D",sprintf("D avg pre-treatment lead (Wald p=%s)", ifelse(is.na(r[3]),"singular",sprintf("%.2f",r[3]))),
        yn, r[1], r[2], "placebo: want ~0/ns") }
  invisible(NULL)
}

cat("==================== 09 ROBUSTNESS BATTERY — S1 & S2 (denom A unless noted) ====================\n")
run_scenario("S1", bin); run_scenario("S2", mod)
out <- rbindlist(res)
print(out[, .(scenario,block,spec,outcome,ATT,SE,p)], class=FALSE)
fwrite(out, file.path(DIR_TABLES,"robustness_S1_S2.csv"))
saveRDS(out, file.path(DIR_CLEAN,"robustness_09.rds"))
cat("\nheadline anchors: S1 level -6.43 / DDD(pre-diff) ~-5.7 ; S2 level -3.95\n")
cat("saved -> output/tables/robustness_S1_S2.csv\n")
