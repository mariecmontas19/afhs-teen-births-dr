# ============================================================================
# 07m_eventstudy_modernization.R — Callaway–Sant'Anna event study under the
# MODERNIZATION treatment (04c): treatment = arrival of a modern comprehensive
# adolescent unit (new full-service opening OR post-2016 upgrade of an old unit).
# Recovers 8 of 9 binary-always-treated municipalities -> 26 in-window treated
# (vs 18 binary). Same outcomes/specs/inference as 07, swapping in mod_cohort.
# Outputs: data/clean/cs_results_mod.rds + output/tables/cs_att_mod.csv +
#          output/figures/fig6m_eventstudy_mod_*.{pdf,png}
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722)
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))

# ---- swap in modernization treatment (fold 158->155: drop the 3 fold children) ----
fold <- c("DOM010905","DOM051703","DOM012510")
modk <- mod[!adm3_pcode %in% fold, .(adm3_pcode, mod_cohort, mod_ever_treated,
                                     always_treated_mod, mod_first_type, n_modern_units)]
dropc <- intersect(c("mod_cohort","mod_ever_treated","always_treated_mod","mod_first_type",
                     "n_modern_units"), names(p))
if (length(dropc)) p <- p[, !dropc, with=FALSE]
p <- merge(p, modk, by="adm3_pcode", all.x=TRUE)
stopifnot(nrow(p)==155*10, p[is.na(mod_ever_treated), .N]==0)

# ---- CS estimation sample: drop always-treated_mod (SFM); g = modern cohort ----
est <- p[always_treated_mod==0]
est[, id := as.integer(factor(adm3_pcode))]
est[, g  := fifelse(mod_ever_treated==1L, as.integer(mod_cohort), 0L)]
stopifnot(nrow(est)==uniqueN(est$id)*uniqueN(est$year))
cat("MOD CS sample: municipalities", uniqueN(est$id),
    "| in-window modern-treated", uniqueN(est[g>0,id]),
    "| never/control", uniqueN(est[g==0,id]), "| muni-years", nrow(est), "\n")
cat("cohort (g) sizes:\n"); print(est[year==2016, .N, by=g][order(g)])

run_cs <- function(yname, ctrl, xf, label){
  a <- att_gt(yname=yname, tname="year", idname="id", gname="g",
              xformla=xf, data=est, control_group=ctrl,
              bstrap=TRUE, cband=TRUE, biters=2000, base_period="universal",
              clustervars="id", est_method="reg", print_details=FALSE)
  list(label=label, yname=yname, att_gt=a,
       overall=aggte(a, type="group",   na.rm=TRUE),
       dynamic=aggte(a, type="dynamic", na.rm=TRUE))
}
specs <- list(   # denominator A only (B dropped per MM 2026-06-24)
  list(y="rateA_15_19", ctrl="notyettreated", xf=~1, lab="Level 15-19 (A), not-yet, uncond"),
  list(y="ddd_A",       ctrl="notyettreated", xf=~1, lab="Age-DDD (A), not-yet, uncond")
)
res <- lapply(specs, function(s) run_cs(s$y, s$ctrl, s$xf, s$lab))
names(res) <- sapply(specs, `[[`, "lab")

gw <- function(x) if (is.null(x)||length(x)==0) NA_real_ else round(x,4)
att_tab <- rbindlist(lapply(res, function(r) data.table(
  spec=r$label, outcome=r$yname,
  ATT_group=round(r$overall$overall.att,3), SE_group=round(r$overall$overall.se,3),
  ATT_dyn=round(r$dynamic$overall.att,3),   SE_dyn=round(r$dynamic$overall.se,3),
  pretrend_Wald_p=gw(r$att_gt$Wpval))))
cat("\n==================== MODERNIZATION CS OVERALL ATT ====================\n")
print(att_tab, class=FALSE)
fwrite(att_tab, file.path(TAB,"cs_att_mod.csv"))

# ---- side-by-side vs the binary headline (if present) ----
bin_f <- file.path(TAB,"cs_att.csv")
if (file.exists(bin_f)){
  b <- fread(bin_f)
  cmp <- merge(b[, .(outcome, bin_ATT=ATT_group, bin_SE=SE_group)],
               att_tab[, .(outcome, mod_ATT=ATT_group, mod_SE=SE_group)], by="outcome")
  cmp[, `:=`(bin_t=round(bin_ATT/bin_SE,2), mod_t=round(mod_ATT/mod_SE,2))]
  cat("\n-------- binary (07) vs modernization (07m): ATT / SE / t --------\n")
  print(cmp, class=FALSE)
}

# ---- event-study plots (dynamic) ----
es_df <- function(r){ d <- r$dynamic
  data.table(spec=r$label, e=d$egt, att=d$att.egt, se=d$se.egt, crit=d$crit.val.egt) }
ed <- rbindlist(lapply(res, es_df)); ed[, `:=`(lo=att-crit*se, hi=att+crit*se)]
mk_es <- function(dd, ttl, fn){
  g <- ggplot(dd, aes(e, att)) +
    geom_hline(yintercept=0, color="grey60") + geom_vline(xintercept=-0.5, linetype="dashed", color="grey60") +
    geom_ribbon(aes(ymin=lo, ymax=hi), alpha=0.15, fill="#2c7fb8") +
    geom_line(color="#2c7fb8") + geom_point(color="#2c7fb8", size=1.6) +
    labs(title=ttl, x="Years since modern unit opened/upgraded", y="ATT (births per 1,000 women)",
         caption="Callaway-Sant'Anna, modernization treatment; not-yet-treated controls; uniform 95% bands; clustered at municipality.") +
    theme_minimal(base_size=11) + theme(plot.title=element_text(face="bold"))
  ggsave(file.path(FIG, paste0(fn,".pdf")), g, width=8, height=5)
  ggsave(file.path(FIG, paste0(fn,".png")), g, width=8, height=5, dpi=150)
}
edp <- ed[e >= -6 & e <= 5]
mk_es(edp[spec %like% "Level 15-19 \\(A\\)"], "Modernization event study: teen rate 15-19 (denom A)", "fig6m_eventstudy_mod_levelA")
mk_es(edp[spec %like% "Age-DDD \\(A\\)"],     "Modernization event study: age-DDD 15-19 minus 30-34 (A)", "fig6m_eventstudy_mod_dddA")

saveRDS(res, file.path(DIR_CLEAN,"cs_results_mod.rds"))
cat("\nsaved -> data/clean/cs_results_mod.rds, output/tables/cs_att_mod.csv, output/figures/fig6m_eventstudy_mod_*.{pdf,png}\n")
