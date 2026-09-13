# ============================================================================
# 08z2_educ_enroll_diag.R — diagnose the secondary-enrollment pre-trend
# (pooled +2.4%, joint lead p=.017; §59.39). HYPOTHESIS: enrollment is an
# unnormalized COUNT with a demographic trend (treated=urban munis on a
# different cohort-size trajectory than rural controls; national secondary
# enrollment is falling over 2016-25). The birth RATE absorbs demography;
# log-count does not. TEST: normalize female secondary enrollment by the
# female 15-19 population (panel_muni_year_155) and see if the pre-trend dies.
# Female only (the panel carries female population, not male).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260729)

pan  <- as.data.table(readRDS(file.path(DIR_CLEAN,"educ_condicion_muni.rds")))
ef   <- pan[nivel=="SECUNDARIO" & sexo=="F", .(adm3_pcode, year_end, mat)]
pop  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
          , .(adm3_pcode, year_end=year, womenA_15_19)]
d0   <- merge(ef, pop, by=c("adm3_pcode","year_end"), all.x=TRUE)
d0[, `:=`(logmat = log(mat), enroll_rate = 100*mat/womenA_15_19)]
cat("rows:", nrow(d0), "| pop merged:", d0[!is.na(womenA_15_19),.N], "\n")

fold <- c("DOM010905","DOM051703","DOM012510")
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
trt[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]
imp <- trt[cohort_imputed==TRUE, adm3_pcode]

runX <- function(yvar, ctrl="notyettreated", dropg=NULL, dropmuni=NULL, lab=""){
  d <- merge(d0, trt[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  if(!is.null(dropg))    d <- d[!(g %in% dropg)]
  if(!is.null(dropmuni)) d <- d[!(adm3_pcode %in% dropmuni)]
  d[, g_edu := fifelse(g==0L, 0L, g + 1L)]
  d[, id := as.integer(factor(adm3_pcode))]; d <- d[is.finite(get(yvar))]
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group=ctrl, base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  pre <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,-2)]
  lp  <- 2*pnorm(-abs(pre[,sum(att/se^2)/sum(1/se^2)] / sqrt(1/pre[,sum(1/se^2)])))
  data.table(spec=lab, ATT=round(gg$overall.att,4), SE=round(gg$overall.se,4),
             p=round(2*pnorm(-abs(gg$overall.att/gg$overall.se)),3), lead_p=round(lp,3))
}
res <- rbindlist(list(
  runX("logmat",      lab="log enrollment (count) — headline"),
  runX("enroll_rate", lab="enrollment RATE per 100 women 15-19 — NORMALIZED"),
  runX("logmat", ctrl="nevertreated",                    lab="log enrollment, never-treated ctrls"),
  runX("logmat", dropg=c(2020L,2021L),                   lab="log enrollment, drop 2020-21 cohorts (COVID)"),
  runX("logmat", dropmuni=imp,                           lab="log enrollment, drop 3 imputed munis"),
  runX("enroll_rate", ctrl="nevertreated",               lab="enrollment RATE, never-treated ctrls")))
cat("\n==================== ENROLLMENT PRE-TREND DIAGNOSTIC ====================\n")
print(res, class=FALSE)

# dynamic pre-path: count vs rate, side by side
dd <- merge(d0, trt[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
dd[, g_edu := fifelse(g==0L, 0L, g+1L)]; dd[, id := as.integer(factor(adm3_pcode))]
pp <- function(yvar){
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=~1,
              data=dd[is.finite(get(yvar))], control_group="notyettreated",
              base_period="universal", est_method="reg", bstrap=TRUE, cband=TRUE,
              biters=2000, clustervars="id", print_details=FALSE)
  dy <- aggte(a, type="dynamic", na.rm=TRUE)
  data.table(e=dy$egt, att=round(dy$att.egt,3), z=round(dy$att.egt/dy$se.egt,2))[e %between% c(-4,4)]
}
cat("\ncount log-enrollment path:\n");  print(pp("logmat"),      class=FALSE)
cat("\nnormalized rate path:\n");        print(pp("enroll_rate"), class=FALSE)
fwrite(res, file.path(PROJ,"output","tables","educ_enroll_diagnostic.csv"))
cat("\nsaved -> educ_enroll_diagnostic.csv\n")
