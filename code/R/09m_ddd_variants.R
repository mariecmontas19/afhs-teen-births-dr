# ============================================================================
# 09m_ddd_variants.R — DDD robustness variants with the PROPER triplediff
# estimator (Ortiz-Villavicencio & Sant'Anna 2025), replacing the
# pre-differenced gap-CS versions in Tables 9/A9 (MM 2026-08-07: "table 9 and
# table A9 should use the main triple DDD results unadjusted... I don't want
# the pre-differenced age-DDD").
# Spec mirrors 07t's ddd_one: unit = municipio x age-cell, pname = 1{15-19},
# control = notyettreated (the package's GMM combination over cohort-specific
# comparisons, Remark 4.2 / Thm 4.2 — NOT pooled), est = "dr", universal base,
# municipality-clustered, boot 999. Seed before EVERY call (project rule).
# Variants: never-treated | population-weighted | drop imputed | intercensal
# (denominator B) | drop SD metro | comparison ages 20-24 / 25-29.
# NOTE: ddd() has no anticipation argument -> the anticipation DDD cell in A9
# becomes "---" (the level anticipation row remains).
# Output: output/tables/ddd_variants.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(triplediff)})
fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]
gt <- bin[!is.na(g), .(adm3_pcode, state=g)]
imputed <- bin[cohort_imputed==TRUE & ever_treated==1L, adm3_pcode]
stopifnot(length(imputed)==3L)
sd_metro <- bin[prov_norm %in% c("DISTRITO NACIONAL","SANTO DOMINGO"), adm3_pcode]

AL <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_age_long_155.rds")))
p0 <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
wt <- p0[year==2016L, .(adm3_pcode, wt_base=womenA_15_19)]

run1 <- function(label, age2="30-34", ycol="rateA", keep=NULL, drop=NULL,
                 ctrl="notyettreated", wgt=FALSE){
  d <- AL[ag %in% c("15-19", age2), .(adm3_pcode=id, year, ag, y=get(ycol))]
  d[, partition := as.integer(ag=="15-19")]
  d <- merge(d, gt, by="adm3_pcode")
  if (!is.null(keep)) d <- d[adm3_pcode %in% keep]
  if (!is.null(drop)) d <- d[!adm3_pcode %in% drop]
  if (wgt) d <- merge(d, wt, by="adm3_pcode")
  d[, `:=`(id=as.integer(factor(paste(adm3_pcode,partition))),
           cluster=as.integer(factor(adm3_pcode)), time=as.integer(year))]
  set.seed(20260722)                                     # per-call seed (project rule)
  a <- tryCatch({
    o <- ddd(yname="y", tname="time", idname="id", gname="state", pname="partition",
             xformla=~1, data=d, control_group=ctrl, base_period="universal",
             est_method="dr", panel=TRUE, cluster="cluster",
             weightsname=if(wgt) "wt_base" else NULL, boot=TRUE, nboot=999)
    agg_ddd(o, type="group", boot=TRUE, nboot=999)$aggte_ddd
  }, error=function(e){ cat(label, "-> INFEASIBLE:", conditionMessage(e), "\n"); NULL })
  # GMM singularities (e.g. thin cohorts after a sample restriction) -> NA row.
  if (is.null(a)) return(data.table(spec=label, ATT=NA_real_, SE=NA_real_, p=NA_real_,
                                    n_treated=uniqueN(d[state>0, cluster])))
  out <- data.table(spec=label, ATT=round(a$overall.att,2), SE=round(a$overall.se,2),
                    p=round(2*pnorm(-abs(a$overall.att/a$overall.se)),3),
                    n_treated=uniqueN(d[state>0, cluster]))
  cat(sprintf("%-38s ATT %6.2f (SE %.2f, p %.3f) treated %d\n",
              label, out$ATT, out$SE, out$p, out$n_treated))
  out
}

res <- rbind(
  run1("never-treated comparisons",   ctrl="nevertreated"),
  run1("population-weighted",         wgt=TRUE),
  run1("drop imputed-date municipalities", drop=imputed),
  run1("intercensal denominators (B)", ycol="rateB"),
  run1("drop Santo Domingo metro",    drop=sd_metro),
  run1("comparison age 20-24",        age2="20-24"),
  run1("comparison age 25-29",        age2="25-29"))
fwrite(res, file.path(TAB,"ddd_variants.csv"))
cat("saved -> ddd_variants.csv\n")
