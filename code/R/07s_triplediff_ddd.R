# ============================================================================
# 07s_triplediff_ddd.R — PROPERLY SPECIFIED triple-difference via triplediff
# (Ortiz-Villavicencio & Sant'Anna 2025), replacing the naive pre-differenced
# (rate15-19 - rate30-34) CS version.
#   unit = municipality x age-cell ; pname(partition)=1 if 15-19 (eligible), 0 if 30-34
#   gname(state) = municipality first-treatment year (0 = never) ; control = "nevertreated"
#   base_period="universal" ; municipality-clustered multiplier bootstrap.
# Headline = UNCONDITIONAL DDD (covariate-adjusted DDD is INFEASIBLE here: cohorts
#   2020/2021/2025 have only 1-3 treated municipalities -> singular covariate matrix /
#   non-PD propensity score; consistent with overlap failing w/ few treated clusters.
#   Per Daw-Hatfield + Caetano-Callaway the unconditional DDD is the right call anyway).
# Scenarios: S1 binary any-unit ; S2 modernization (all modern units). est dr + reg.
# Outputs: output/tables/ddd_triplediff.csv + output/figures/ddd_es_{S1,S2}.png
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(triplediff); library(ggplot2)})
set.seed(20260722); TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")
fold <- c("DOM010905","DOM051703","DOM012510")

L <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_age_long_155.rds")))[ag %in% c("15-19","30-34"),
        .(adm3_pcode=id, year, ag, y=rateA)]
L[, partition := as.integer(ag=="15-19")]
cov <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_covariates_muni.rds")))[, .(adm3_pcode, wealth_index, pct_urban, pct_educ_secplus)]
hc  <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
covs <- merge(cov, hc, by="adm3_pcode")

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]

build <- function(gtab){
  d <- merge(L, gtab[!is.na(g), .(adm3_pcode, state=g)], by="adm3_pcode")
  d <- merge(d, covs, by="adm3_pcode")
  d[, `:=`(id=as.integer(factor(paste(adm3_pcode,partition))), cluster=as.integer(factor(adm3_pcode)), time=as.integer(year))]
  stopifnot(nrow(d)==uniqueN(d$id)*uniqueN(d$time)); d[]
}
S1 <- build(bin); S2 <- build(mod)

run_ddd <- function(d, xf, em, lab){
  out <- tryCatch({
    o <- ddd(yname="y", tname="time", idname="id", gname="state", pname="partition", xformla=xf, data=d,
             control_group="nevertreated", base_period="universal", est_method=em, panel=TRUE,
             cluster="cluster", boot=TRUE, nboot=999, cband=TRUE)
    gp <- agg_ddd(o, type="group",      boot=TRUE, nboot=999)$aggte_ddd
    es <- agg_ddd(o, type="eventstudy", boot=TRUE, nboot=999, cband=TRUE)$aggte_ddd
    list(lab=lab, att=gp$overall.att, se=gp$overall.se, es=es, ok=TRUE)
  }, error=function(e) list(lab=lab, ok=FALSE, msg=conditionMessage(e)))
  if (out$ok) cat(sprintf("  %-22s ATT=%.2f  se=%.2f  p=%.3f\n", lab, out$att, out$se,
                          2*pnorm(-abs(out$att/out$se))))
  else        cat(sprintf("  %-22s INFEASIBLE: %s\n", lab, out$msg))
  out
}

res <- list()
for(s in list(list(S1,"S1 binary any-unit"), list(S2,"S2 modernization"))){
  d <- s[[1]]; nm <- s[[2]]
  cat("\n====", nm, "| treated:", uniqueN(d[state>0,cluster]), "| never:", uniqueN(d[state==0,cluster]), "====\n")
  res[[paste0(nm," | uncond dr")]]  <- run_ddd(d, ~1, "dr", "uncond DR")
  res[[paste0(nm," | uncond reg")]] <- run_ddd(d, ~1, "reg", "uncond REG")
  res[[paste0(nm," | +covs dr")]]   <- run_ddd(d, ~wealth_index+pct_urban+pct_educ_secplus+sns_per10k, "dr", "+covs DR")
}
tab <- rbindlist(lapply(names(res), function(k){ r <- res[[k]]
  if (r$ok) data.table(spec=k, ATT=round(r$att,2), SE=round(r$se,2),
                       p=round(2*pnorm(-abs(r$att/r$se)),3), status="ok")
  else      data.table(spec=k, ATT=NA_real_, SE=NA_real_, p=NA_real_, status="infeasible") }))
cat("\n==================== triplediff DDD (overall, group agg) ====================\n")
print(tab, class=FALSE); fwrite(tab, file.path(TAB,"ddd_triplediff.csv"))

# event-study plots (unconditional DR)
mkes <- function(r, fn){
  if(!r$ok) return(invisible())
  e <- data.table(e=r$es$egt, att=r$es$att.egt, se=r$es$se.egt, crit=as.numeric(r$es$crit.val.egt))
  e <- e[e>=-5 & e<=4]; e[, `:=`(lo=att-crit*se, hi=att+crit*se)]
  g <- ggplot(e, aes(e,att)) + geom_hline(yintercept=0,color="grey60") +
    geom_vline(xintercept=-0.5,linetype="dashed",color="grey60") +
    geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.15,fill="#1b7837") +
    geom_line(color="#1b7837")+geom_point(color="#1b7837",size=1.8) +
    labs(title=paste0("Triple-difference event study — ", r$lab2),
         x="Years since treatment", y="DDD ATT (teen births per 1,000 women)",
         caption="triplediff DR; never-treated comparison; universal base; uniform 95% bands; municipality-clustered.") +
    theme_minimal(base_size=11)+theme(plot.title=element_text(face="bold"))
  ggsave(file.path(FIG,paste0(fn,".png")), g, width=8, height=5, dpi=150)
}
r1 <- res[["S1 binary any-unit | uncond dr"]]; r1$lab2 <- "binary any-unit"; mkes(r1,"ddd_es_S1")
r2 <- res[["S2 modernization | uncond dr"]];   r2$lab2 <- "modernization";   mkes(r2,"ddd_es_S2")
saveRDS(res, file.path(DIR_CLEAN,"ddd_triplediff_results.rds"))
cat("\nsaved -> output/tables/ddd_triplediff.csv + output/figures/ddd_es_{S1,S2}.png\n")
