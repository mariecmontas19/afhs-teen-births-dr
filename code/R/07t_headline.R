# ============================================================================
# 07t_headline.R — consolidated headline estimates for S1 (any new unit, binary)
# and S2 (all units incl. modernized). For each: CS level teen rate WITHOUT and
# WITH baseline covariates, and the triple-difference (triplediff, DR, no covs).
# Reports estimate / SE / p / baseline pre-treatment teen rate / % of baseline,
# and saves event-study plots for every spec. Denominator A.
#   CS: att_gt(rateA_15_19), control="notyettreated", est="reg", universal base, muni-clustered.
#   CS covariates (baseline): wealth_index + sns_per10k + pct_urban + pct_educ_secplus + pct_senasa_2013.
#   DDD: ddd(), pname=1{15-19}, gname=opening yr, control="notyettreated" (PRIMARY,
#        tighter inference — never-treated reported as robustness in 09), est="dr", no covariates.
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(triplediff); library(ggplot2)})
set.seed(20260722); TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")
fold <- c("DOM010905","DOM051703","DOM012510")
XF <- ~ wealth_index + sns_per10k + pct_urban + pct_educ_secplus + pct_senasa_2013   # +SeNaSa public-insurance coverage (DHS 2013, prov)

# ---------- panels + treatment + covariates ----------
pw <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19, wealth_index, pct_urban, pct_educ_secplus)]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
pw <- merge(pw, hc, by="adm3_pcode", all.x=TRUE)
sn <- as.data.table(readRDS(file.path(DIR_CLEAN,"dhs_province_baseline.rds")))[, .(adm3_pcode, pct_senasa_2013)]  # SeNaSa coverage, DHS 2013 (prov-broadcast)
pw <- merge(pw, sn, by="adm3_pcode", all.x=TRUE)
stopifnot(sum(is.na(pw$pct_senasa_2013))==0)
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]
La <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_age_long_155.rds")))[ag %in% c("15-19","30-34"), .(adm3_pcode=id, year, ag, y=rateA)]
La[, partition := as.integer(ag=="15-19")]

# baseline = mean teen rate among treated municipalities in the YEAR BEFORE treatment (g-1)
# [g-1 must be >= 2016 to be observed; 2016-cohorts (if any) contribute no g-1 row]
baseline <- function(gtab){
  # g-1 baseline (the estimator's own reference period; standard for event studies).
  # Valid pre-treatment for BOTH margins (S2 cohorts start 2017, so a fixed 2019 is not).
  tr <- gtab[!is.na(g) & g>0, .(adm3_pcode, g)]
  d <- merge(pw[, .(adm3_pcode, year, rateA_15_19)], tr, by="adm3_pcode")
  round(d[year == g-1L, mean(rateA_15_19)], 2)
}
b_S1 <- baseline(bin); b_S2 <- baseline(mod)

# ---------- CS (level teen rate) ----------
cs_one <- function(gtab, xf){
  est <- merge(pw, gtab[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  est[, id := as.integer(factor(adm3_pcode))]
  a <- att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g", xformla=xf, data=est,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  dy <- aggte(a,type="dynamic",na.rm=TRUE)
  csz <- est[g>0, .(n=uniqueN(adm3_pcode)), by=g]
  nbe <- data.table(e=-5:4)[, n := sapply(e, function(ee) csz[g+ee>=2016 & g+ee<=2025, sum(n)])]
  list(grp=aggte(a,type="group",na.rm=TRUE), dyn=dy, pt=es_pretrend_p(dy),
       ntr=uniqueN(est[g>0,adm3_pcode]), nbe=nbe)
}
# ---------- DDD (triplediff) ----------
ddd_one <- function(gtab){
  d <- merge(La, gtab[!is.na(g), .(adm3_pcode, state=g)], by="adm3_pcode")
  d[, `:=`(id=as.integer(factor(paste(adm3_pcode,partition))), cluster=as.integer(factor(adm3_pcode)), time=as.integer(year))]
  o <- ddd(yname="y",tname="time",idname="id",gname="state",pname="partition",xformla=~1,data=d,
           control_group="notyettreated", base_period="universal", est_method="dr", panel=TRUE,
           cluster="cluster", boot=TRUE, nboot=999, cband=TRUE)
  dy <- agg_ddd(o,type="eventstudy",boot=TRUE,nboot=999,cband=TRUE)$aggte_ddd
  csz <- d[state>0, .(n=uniqueN(cluster)), by=.(g=state)]
  nbe <- data.table(e=-5:4)[, n := sapply(e, function(ee) csz[g+ee>=2016 & g+ee<=2025, sum(n)])]
  list(grp=agg_ddd(o,type="group",boot=TRUE,nboot=999)$aggte_ddd,
       dyn=dy, pt=es_pretrend_p(dy),
       ntr=uniqueN(d[state>0,cluster]), nbe=nbe)
}

specs <- list(
  list(s="S1", lab="S1 any new unit — CS, no covs",  base=b_S1, f=function() cs_one(bin, ~1)),
  list(s="S1", lab="S1 any new unit — CS, +covs",    base=b_S1, f=function() cs_one(bin, XF)),
  list(s="S1", lab="S1 any new unit — DDD (no covs)",base=b_S1, f=function() ddd_one(bin)),
  list(s="S2", lab="S2 all incl. modernized — CS, no covs", base=b_S2, f=function() cs_one(mod, ~1)),
  list(s="S2", lab="S2 all incl. modernized — CS, +covs",   base=b_S2, f=function() cs_one(mod, XF)),
  list(s="S2", lab="S2 all incl. modernized — DDD (no covs)",base=b_S2, f=function() ddd_one(mod)))

rows <- list(); dyns <- list(); pts <- list(); nbes <- list()
for(sp in specs){
  set.seed(20260722)   # 2026-08-06: seed before EVERY spec (project rule) so each
                       # spec's multiplier-bootstrap SE is order-independent; the
                       # first spec (S1 CS no-covs headline) is unchanged from the
                       # prior single-seed run, preserving the canonical -6.43/2.10/.002.
  r <- sp$f(); att <- r$grp$overall.att; se <- r$grp$overall.se
  rows[[sp$lab]] <- data.table(scenario=sp$s, spec=sub("^S[12][^—]*— ","",sp$lab), n_treated=r$ntr,
     estimate=round(att,2), SE=round(se,2), p=round(2*pnorm(-abs(att/se)),3),
     baseline=sp$base, pct_of_baseline=round(100*att/sp$base,1))
  dyns[[sp$lab]] <- data.table(e=r$dyn$egt, att=r$dyn$att.egt, se=r$dyn$se.egt, crit=as.numeric(r$dyn$crit.val.egt))
  pts[[sp$lab]]  <- r$pt
  nbes[[sp$lab]] <- r$nbe
  cat(sprintf("%-42s ATT=%.2f se=%.2f p=%.3f base=%.1f (%.0f%%) | pre-trend leads avg=%+.2f p=%.3f\n",
              sp$lab, att, se, 2*pnorm(-abs(att/se)), sp$base, 100*att/sp$base, r$pt$avg, r$pt$p))
}
TT <- rbindlist(rows)
cat("\n==================== HEADLINE TABLE (denom A) ====================\n"); print(TT, class=FALSE)
fwrite(TT, file.path(TAB,"headline_S1_S2.csv"))

# ---------- event-study plots (all 6) — clear, method-specific titles/captions ----------
# (titles name the outcome age 15-19; the 30-34 comparison is named ONLY on the DDD plots;
#  the CS plots do NOT mention DDD/triplediff. Aligned 1:1 with `specs`/`dyns` order.)
meta <- data.table(
  fn = c("fig_04a_es_level","hl_S1_cs_cond","fig_04b_es_ddd","hl_S2_cs_uncond","hl_S2_cs_cond","hl_S2_ddd"),
  method = c("cs","cs","ddd","cs","cs","ddd"),
  col = c(PCUA_COL$blue, PCUA_COL$blue, PCUA_COL$blue, PCUA_COL$blue, PCUA_COL$blue, PCUA_COL$blue),
  title = c(
    "Effect of opening an AU on teen birth rates",
    "Effect of opening an AU on teen birth rates",
    "Effect of opening an AU on teen birth rates - Triple difference",
    "A new or modernized adolescent unit (AU) and the 15-19 birth rate",
    "A new or modernized adolescent unit (AU) and the 15-19 birth rate",
    "New/modernized AU: 15-19 birth rate, triple-difference vs ages 30-34"),
  subtitle = c(
    "Callaway-Sant'Anna staggered event study  |  no covariates",
    "Callaway-Sant'Anna staggered event study  |  baseline covariates: wealth, urban, education, health centers, SeNaSa",
    "Triple-difference (triplediff): eligible 15-19 vs comparison 30-34  |  no covariates",
    "Callaway-Sant'Anna staggered event study  |  no covariates",
    "Callaway-Sant'Anna staggered event study  |  baseline covariates: wealth, urban, education, health centers, SeNaSa",
    "Triple-difference (triplediff): eligible 15-19 vs comparison 30-34  |  no covariates"),
  xlab = c(rep("Years since first AU opening",3), rep("Years since new/modernized unit",3)))
meta[, ylab := fifelse(method=="ddd", "ATT: (15-19 minus 30-34) births per 1,000 women",
                                      "ATT: teen births per 1,000 women (15-19)")]
meta[, caption := fifelse(method=="ddd",
  "Denominator A. Uniform 95% bands (error bars); SEs clustered by municipality. Triple-difference (triplediff), not-yet-treated controls; outcome = 15-19 rate minus 30-34 rate.",
  "Denominator A. Uniform 95% bands (error bars); SEs clustered by municipality. Callaway-Sant'Anna, not-yet-treated controls; reference = year before treatment.")]

mk <- function(m, dd, pt, nbe, rng_fix=NA_real_, short_ann=FALSE){
  dd <- dd[e>=-5 & e<=4]; dd[, `:=`(lo=att-crit*se, hi=att+crit*se)]
  rng <- if (is.finite(rng_fix)) rng_fix else
         ceiling(max(abs(c(dd$lo, dd$hi)), na.rm=TRUE)/5)*5          # symmetric, full uniform-CI, round to 5
  # compact pre-trend annotation (MM 2026-08-09: one line for panel b, two for a;
  # the joint-test average and lead detail moved to the LaTeX figure notes)
  nsig <- dd[e>=-5 & e<=-2][2*pnorm(-abs(att/se)) < .05]
  l2 <- if(nrow(nsig)==0) "No individual lead significant" else
        sprintf("Individually sig. lead(s): e=%s", paste(nsig$e, collapse=","))
  l1 <- if(is.na(pt$p)) "Pre-trend test: not estimable" else sprintf("Pre-trend test: p = %.2f", pt$p)
  ann <- if(short_ann) l1 else paste(l1, l2, sep="\n")
  dp <- dd[e != -1]                                                   # e=-1 = omitted base; drawn as its own marker
  g <- ggplot(dp, aes(e,att)) +
    es_guides(label_y=rng) +                                         # post shading + treatment line/label
    geom_vline(xintercept=-0.5, linetype="22", color="grey30", linewidth=0.7) +  # crisper treatment boundary
    geom_hline(yintercept=0, color="grey25", linewidth=0.7) +        # bold zero line
    geom_errorbar(aes(ymin=lo, ymax=hi), width=0.14, linewidth=0.7, color=m$col, na.rm=TRUE) +  # standard CI bars (MM 2026-07-10, notes 57)
    geom_point(color=m$col, fill="white", shape=21, size=2.9, stroke=1.2, na.rm=TRUE) +
    annotate("point", x=-1, y=0, shape=23, size=2.7, fill="grey88", color="grey30", stroke=1.1) +  # omitted base period
    annotate("text", x=-1, y=0, label="base", vjust=2.1, size=2.7, color="grey40", fontface="italic") +
    annotate("text", x=-5, y=rng*0.93, hjust=0, vjust=1, size=3.2, fontface="italic", color="grey25", label=ann) +
    scale_x_continuous(breaks=seq(-5,4,1)) +
    scale_y_continuous(limits=c(-rng, rng), breaks=seq(-rng, rng, 5)) +
    labs(x=m$xlab, y=m$ylab) +
    theme_pcua()
  ggsave(file.path(FIG,paste0(m$fn,".png")), g, width=8.6, height=5.4, dpi=200)
}
# shared y-scale for the two PAPER panels (specs 1 = level, 3 = DDD; MM 2026-08-09)
.shared <- {
  rr <- sapply(c(1L,3L), function(i){ d <- dyns[[i]][e>=-5 & e<=4]
    max(abs(c(d$att-d$crit*d$se, d$att+d$crit*d$se)), na.rm=TRUE) })
  ceiling(max(rr)/5)*5 }
cat("shared y-range for fig_04a/fig_04b: ±", .shared, "\n")
for(i in seq_along(specs)) mk(meta[i], dyns[[i]], pts[[i]], nbes[[i]],
                              rng_fix = if (i %in% c(1L,3L)) .shared else NA_real_,
                              short_ann = (i == 3L))
cat("\nsaved -> output/tables/headline_S1_S2.csv + output/figures/hl_*.png (6, retitled)\n")
