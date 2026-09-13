# ============================================================================
# 07x_ddd_2period.R — the paper-proper COVARIATE-ADJUSTED DDD for S2 (and S1),
# via a 2-period STACKED event-time collapse so the covariate DR estimator is
# feasible (the staggered triplediff covariate-DDD is singular: cohorts of 1-4).
#
# Why stacking: triplediff's 2-period DDD (gen_dgp_2periods shape) needs ONE
# treatment time (time in {1,2}, gname=2 for treated). Our cohorts span 2017-25.
# We build one sub-experiment per cohort c:
#   - treated  = municipios with g==c ; control = never-treated (g==0)
#   - pre  (time=1) = unit mean outcome over years <  c
#   - post (time=2) = unit mean outcome over years >= c   (same split for controls)
#   - partition = age eligibility (1 = 15-19 eligible ; 0 = 30-34 ineligible)
# Stack all cohorts -> all treated pooled into one period-2 group (covariates
# estimable). id = municipio x partition x stack ; CLUSTER = municipio (handles
# never-treated controls reused across stacks). control_group = never-treated
# (clean in every year; we do NOT use not-yet here because a not-yet control's
# post window >= c would include its own treatment -> contamination).
# Covariates (baseline 2022) = wealth + urban + education + HEALTH CENTERS.
# Compare uncond vs +covs against the staggered numbers from 07w.
# Output: output/tables/ddd_2period.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(triplediff)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
COVS <- ~ wealth_index + pct_urban + pct_educ_secplus + sns_per10k

# ---- baseline covariates (one row per municipio) ----
p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, wealth_index, pct_urban, pct_educ_secplus)]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
covdt <- merge(unique(p[, .(adm3_pcode, wealth_index, pct_urban, pct_educ_secplus)]), hc, by="adm3_pcode")
stopifnot(nrow(covdt)==uniqueN(p$adm3_pcode), !anyNA(covdt))

# ---- age-long outcomes: partition 1 = 15-19, 0 = 30-34 (denom A) ----
La <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_age_long_155.rds")))[ag %in% c("15-19","30-34"), .(adm3_pcode=id, year, ag, y=rateA)]
La[, partition := as.integer(ag=="15-19")]

# ---- treatment tables ----
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]

# ---- build the STACKED 2-period dataset for a given treatment table ----
build_stack <- function(gt){
  treated_g <- gt[!is.na(g) & g>0, unique(g)]
  never     <- gt[g==0, adm3_pcode]
  out <- list()
  for(c in sort(treated_g)){
    tr_units <- gt[g==c, adm3_pcode]
    units    <- c(tr_units, never)                       # treated-c + all never-treated
    d <- La[adm3_pcode %in% units]
    # per-unit, per-partition pre/post means around cohort c's split
    coll <- d[, .(pre = mean(y[year <  c]),
                  post= mean(y[year >= c])), by=.(adm3_pcode, partition)]
    coll <- coll[is.finite(pre) & is.finite(post)]       # drop if a side is empty
    long <- rbind(
      coll[, .(adm3_pcode, partition, time=1L, y=pre)],
      coll[, .(adm3_pcode, partition, time=2L, y=post)])
    long[, `:=`(stack=c,
                state=fifelse(adm3_pcode %in% tr_units, 2L, 0L))]
    out[[as.character(c)]] <- long
  }
  S <- rbindlist(out)
  S <- merge(S, covdt, by="adm3_pcode")
  S[, `:=`(id      = as.integer(factor(paste(adm3_pcode, partition, stack))),
           cluster = as.integer(factor(adm3_pcode)))]
  # balance check: every id must have exactly 2 rows (time 1 & 2)
  stopifnot(all(S[, .N, by=id]$N == 2L))
  S[]
}

run2p <- function(gt, xf){
  S <- build_stack(gt)
  ntr <- uniqueN(S[state==2L, adm3_pcode]); nco <- uniqueN(S[state==0L, adm3_pcode])
  o <- tryCatch(
    ddd(yname="y", tname="time", idname="id", gname="state", pname="partition",
        xformla=xf, data=S, control_group="nevertreated", base_period="universal",
        est_method="dr", panel=TRUE, cluster="cluster", boot=TRUE, nboot=999),
    error=function(e){ message("  ddd error: ", conditionMessage(e)); NULL })
  if(is.null(o)) return(c(att=NA_real_, se=NA_real_, ntr=ntr, nstack=uniqueN(S$stack)))
  c(att=o$ATT, se=o$se, ntr=ntr, nstack=uniqueN(S$stack))
}

fmt <- function(x) if(is.na(x["att"])) "INFEASIBLE" else
  sprintf("%+.2f (se %.2f, p %.3f)  [%d treated, %d cohort-stacks]",
          x["att"], x["se"], 2*pnorm(-abs(x["att"]/x["se"])), x["ntr"], x["nstack"])

cat("============== 2-PERIOD STACKED DDD (never-treated controls, denom A) ==============\n")
res <- list()
for(sc in list(list("S1","S1 binary any-unit", bin), list("S2","S2 modernization", mod))){
  tag<-sc[[1]]; lab<-sc[[2]]; gt<-sc[[3]]
  u <- run2p(gt, ~1); a <- run2p(gt, COVS)
  cat(sprintf("\n--- %s ---\n", lab))
  cat("  uncond :", fmt(u), "\n")
  cat("  +covs  :", fmt(a), "  (wealth+urban+educ+health centers, baseline)\n")
  res[[tag]] <- data.table(scenario=tag,
     uncond=fmt(u), covs=fmt(a))
}
fwrite(rbindlist(res), file.path(PROJ,"output","tables","ddd_2period.csv"))
cat("\nsaved -> output/tables/ddd_2period.csv\n")
