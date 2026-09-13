# ============================================================================
# 07_eventstudy.R — Callaway–Sant'Anna staggered event study (CORE).
# Co-primary outcomes: (1) level teen rate 15-19; (2) age-DDD (15-19 − 30-34).
# Both denominators A & B. Estimation sample = in-window treated (g in 2016-25)
# + never-treated (g=0); the 9 always-treated dropped (no in-sample pre-period).
# Controls: not-yet-treated (incl. never) MAIN; never-treated robustness.
# Inference: CS multiplier bootstrap clustered at MUNICIPIO (idname=id=municipio)
#   + uniform confidence bands. (RI + TWFE-WCB in 07c; HonestDiD in 07b.)
# Outputs: data/clean/cs_results.rds + output/figures/fig6_eventstudy_*.{pdf,png}
#          + output/tables/cs_att.csv
# NOTE: all att_gt()/aggte() args verified against args()/?att_gt before the run.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722)
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))

# 2026-08-06: source treatment from treatment_municipio.rds (the source of truth),
# NOT the panel's embedded ever_treated/always_treated/first_year (CLAUDE.md hazard:
# those went stale once before). Byte-identical to the embedded fields today; the
# stopifnot makes any future divergence fail loudly instead of silently mis-coding.
.trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[
          , .(adm3_pcode, ever_treated, always_treated, first_year)]
stopifnot(all.equal(
  p[order(adm3_pcode), .(adm3_pcode, ever_treated, always_treated, first_year)][!duplicated(adm3_pcode)],
  .trt[adm3_pcode %in% p$adm3_pcode][order(adm3_pcode)]))
p[, c("ever_treated","always_treated","first_year") := NULL]
p <- merge(p, .trt, by="adm3_pcode", all.x=TRUE)
stopifnot(!anyNA(p$ever_treated))

# ---- estimation sample + CS keys ----
est <- p[always_treated==0]                                   # drop 9 always-treated
est[, id := as.integer(factor(adm3_pcode))]                   # numeric unit id (att_gt needs numeric idname)
est[, g  := fifelse(ever_treated==1L, as.integer(first_year), 0L)]   # group = first-treated yr; 0 = never
stopifnot(nrow(est) == uniqueN(est$id) * uniqueN(est$year))   # balanced panel required by CS
cat("CS estimation sample: municipalities", uniqueN(est$id), "| in-window treated",
    uniqueN(est[g>0, id]), "| never-treated", uniqueN(est[g==0, id]),
    "| muni-years", nrow(est), "\n")
cat("cohort (g) sizes:\n"); print(est[year==2016, .N, by=g][order(g)])

# ---- CS runner (args finalized after verifying ?att_gt) ----
run_cs <- function(yname, ctrl, xf, label){
  a <- att_gt(yname=yname, tname="year", idname="id", gname="g",
              xformla=xf, data=est, control_group=ctrl,
              bstrap=TRUE, cband=TRUE, biters=2000, base_period="universal",
              clustervars="id", est_method="reg", print_details=FALSE)
  list(label=label, yname=yname, ctrl=ctrl,
       att_gt=a,
       overall=aggte(a, type="group",   na.rm=TRUE),    # overall ATT (group-size weighted)
       dynamic=aggte(a, type="dynamic", na.rm=TRUE))    # event-study leads/lags
}

# ---- run the co-primary specs (denom A first; B added after A verified) ----
specs <- list(
  list(y="rateA_15_19", ctrl="notyettreated", xf=~1, lab="Level 15-19 (A), not-yet, uncond"),
  list(y="ddd_A",       ctrl="notyettreated", xf=~1, lab="Age-DDD (A), not-yet, uncond"),
  list(y="rateB_15_19", ctrl="notyettreated", xf=~1, lab="Level 15-19 (B), not-yet, uncond"),
  list(y="ddd_B",       ctrl="notyettreated", xf=~1, lab="Age-DDD (B), not-yet, uncond")
)
res <- lapply(specs, function(s) run_cs(s$y, s$ctrl, s$xf, s$lab))
names(res) <- sapply(specs, `[[`, "lab")

# ---- tidy overall ATTs (group aggregation = headline; dynamic-overall shown too) ----
gw <- function(x) if (is.null(x) || length(x)==0) NA_real_ else round(x,4)   # Wpval is NA when singular
att_tab <- rbindlist(lapply(res, function(r) data.table(
  spec=r$label, outcome=r$yname,
  ATT_group=round(r$overall$overall.att,3), SE_group=round(r$overall$overall.se,3),
  ATT_dyn=round(r$dynamic$overall.att,3),   SE_dyn=round(r$dynamic$overall.se,3),
  pretrend_Wald_p=gw(r$att_gt$Wpval))))
cat("\n==================== CS OVERALL ATT ====================\n"); print(att_tab, class=FALSE)
fwrite(att_tab, file.path(TAB,"cs_att.csv"))

# ---- event-study plots (dynamic aggregation) ----
es_df <- function(r){ d <- r$dynamic
  data.table(spec=r$label, e=d$egt, att=d$att.egt, se=d$se.egt, crit=d$crit.val.egt) }
ed <- rbindlist(lapply(res, es_df))
ed[, `:=`(lo=att-crit*se, hi=att+crit*se)]
mk_es <- function(dd, ttl, fn){
  g <- ggplot(dd, aes(e, att)) +
    geom_hline(yintercept=0, color="grey60") + geom_vline(xintercept=-0.5, linetype="dashed", color="grey60") +
    geom_ribbon(aes(ymin=lo, ymax=hi), alpha=0.15, fill="#bd0026") +
    geom_line(color="#bd0026") + geom_point(color="#bd0026", size=1.6) +
    labs(title=ttl, x="Years since first AU opened", y="ATT (births per 1,000 women)",
         caption="Callaway-Sant'Anna; not-yet-treated controls; uniform 95% bands; clustered at municipality.") +
    theme_minimal(base_size=11) + theme(plot.title=element_text(face="bold"))
  ggsave(file.path(FIG, paste0(fn,".pdf")), g, width=8, height=5)
  ggsave(file.path(FIG, paste0(fn,".png")), g, width=8, height=5, dpi=150)
}
# trim thin event-time tails for display (e=4,5 rest on 2-3 municipalities); full range kept in saved object
edp <- ed[e >= -5 & e <= 4]
mk_es(edp[spec %like% "Level 15-19 \\(A\\)"], "Event study: teen birth rate 15-19 (denom A)", "fig6_eventstudy_levelA")
mk_es(edp[spec %like% "Age-DDD \\(A\\)"],     "Event study: age-DDD 15-19 minus 30-34 (denom A)", "fig6_eventstudy_dddA")
mk_es(edp[spec %like% "Level 15-19 \\(B\\)"], "Event study: teen birth rate 15-19 (denom B)", "fig6_eventstudy_levelB")
mk_es(edp[spec %like% "Age-DDD \\(B\\)"],     "Event study: age-DDD 15-19 minus 30-34 (denom B)", "fig6_eventstudy_dddB")

saveRDS(res, file.path(DIR_CLEAN,"cs_results.rds"))
cat("\nsaved -> data/clean/cs_results.rds, output/tables/cs_att.csv, output/figures/fig6_eventstudy_*.{pdf,png}\n")
