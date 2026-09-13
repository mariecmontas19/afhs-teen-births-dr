# ============================================================================
# 08_heterogeneity.R — does the AU effect concentrate in UNDERSERVED areas?
# Tests MM's "first-access" theory via subgroup Callaway-Sant'Anna ATTs: split the
# in-window-treated municipalities at the MEDIAN of each baseline moderator, estimate each
# half's group-ATT vs the SHARED never-treated controls, compare. CS does not natively
# do covariate-moderated effects -> subgroup ATTs are the transparent route.
# Run for BOTH scenarios: S1 (first opening, 20 treated) and S2 (incl. modernized, 29).
# Denominator A; not-yet (= never here) controls; est=reg; universal base; muni-clustered.
#
# Moderators (baseline; "underserved" side flagged): Urban share (rural=under) |
# Baseline isolation km @ g-1 (isolated=under) | Health centres/10k (few=under) |
# Health capability (low=under) | Poverty ICV %poor (poor=under) | Education %sec+ (low=under).
# CAVEATS: ~10 (S1) / ~14 (S2) treated per half -> WIDE CIs (suggestive); moderators
# correlated (converging, not independent); S2 isolation is fuzzy (recovered urban
# municipalities already had a unit pre-2016 -> their pre-opening isolation ~ 0).
# Outputs: output/tables/heterogeneity.csv, output/figures/fig_heterogeneity_{S1,S2}.png,
#          data/clean/heterogeneity.rds.
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
         , .(adm3_pcode, year, rateA_15_19, pct_urban, pct_educ_secplus)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]

# moderator sources (baseline, municipality-level)
dp  <- as.data.table(readRDS(file.path(DIR_CLEAN,"distance_open_panel.rds")))
hc  <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
cap <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_capability_muni.rds")))[, .(adm3_pcode, capability_index)]
icv <- as.data.table(readRDS(file.path(DIR_CLEAN,"icv_poverty_muni.rds")))[, .(adm3_pcode, pct_poor_icv)]
ins <- as.data.table(readRDS(file.path(DIR_CLEAN,"inscripcion_muni.rds")))[, .(adm3_pcode, pct_enrolled)]  # SIUBEN 2025 (same vintage as ICV)
base <- unique(p[, .(adm3_pcode, pct_urban, pct_educ_secplus)])
mods <- data.table(
  name = c("Urban share","Baseline isolation","Health centres /10k","Health capability","Poverty (ICV % poor)","Education (% sec+)","School enrolment (%)"),
  var  = c("pct_urban","isolation_km","sns_per10k","capability_index","pct_poor_icv","pct_educ_secplus","pct_enrolled"),
  under_high = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))                                          # low enrolment = underserved

run_scenario <- function(tag, gt, ptitle){
  treated <- gt[!is.na(g) & g>0, .(adm3_pcode, g)]; never <- gt[g==0, adm3_pcode]
  iso <- merge(treated, dp[, .(adm3_pcode, year, dist_open_km)], by="adm3_pcode")[year==g-1L,
               .(adm3_pcode, isolation_km=dist_open_km)]
  M <- Reduce(function(a,b) merge(a,b,by="adm3_pcode",all.x=TRUE), list(treated, iso, hc, cap, icv, ins, base))
  stopifnot(nrow(M)==nrow(treated), !anyNA(M))
  cs_sub <- function(pcodes){
    est <- merge(p[, .(adm3_pcode, year, rateA_15_19)],
                 rbind(treated[adm3_pcode %in% pcodes], data.table(adm3_pcode=never, g=0L)), by="adm3_pcode")
    est[, id := as.integer(factor(adm3_pcode))]
    a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
          xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
          bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
    o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
    data.table(att=o$overall.att, se=o$overall.se, n_treated=length(pcodes))
  }
  ref <- cs_sub(treated$adm3_pcode)
  cat(sprintf("\n=== %s: overall ATT = %+.2f (se %.2f), %d treated ===\n", tag, ref$att, ref$se, nrow(treated)))
  res <- list()
  for(i in seq_len(nrow(mods))){
    v <- mods$var[i]; med <- median(M[[v]])
    hi <- M[get(v) >= med, adm3_pcode]; lo <- M[get(v) < med, adm3_pcode]
    und <- if(mods$under_high[i]) hi else lo; srv <- setdiff(M$adm3_pcode, und)
    ru <- cs_sub(und); rs <- cs_sub(srv)
    res[[i]] <- rbind(cbind(scenario=tag, moderator=mods$name[i], group="More underserved", ru),
                      cbind(scenario=tag, moderator=mods$name[i], group="Less underserved", rs))
    cat(sprintf("  %-22s underserved %+.2f (se %.2f, n=%d) | served %+.2f (se %.2f, n=%d)\n",
        mods$name[i], ru$att, ru$se, ru$n_treated, rs$att, rs$se, rs$n_treated))
  }
  H <- rbindlist(res); H[, `:=`(lo=att-1.96*se, hi=att+1.96*se, p=2*pnorm(-abs(att/se)))]
  Hp <- copy(H)[, moderator := factor(moderator, levels=rev(mods$name))][
                 , group := factor(group, levels=c("More underserved","Less underserved"))]
  g1 <- ggplot(Hp, aes(att, moderator, color=group)) +
    geom_vline(xintercept=0, color="grey55") +
    geom_vline(xintercept=ref$att, linetype="22", color="grey45") +
    geom_errorbarh(aes(xmin=lo, xmax=hi), height=0, linewidth=0.9, position=position_dodge(0.55)) +
    geom_point(size=3.4, position=position_dodge(0.55)) +
    scale_color_manual(values=c("More underserved"=PCUA_COL$green, "Less underserved"=PCUA_COL$orange), name=NULL) +
    labs(title=ptitle,
         subtitle=sprintf("Subgroup CS ATT on the 15-19 birth rate; %d treated split at each moderator's median (dashed = overall, %.1f)", nrow(treated), ref$att),
         x="ATT: teen births per 1,000 women (15-19)", y=NULL,
         caption="Denominator A; not-yet controls; 95% CIs; municipality-clustered. Few treated per half -> wide, overlapping CIs (suggestive). Moderators correlated -> converging, not independent.") +
    theme_pcua()
  ggsave(file.path(FIG, sprintf("fig_heterogeneity_%s.png", tag)), g1, width=9.4, height=5.6, dpi=200)
  H
}

H1 <- run_scenario("S1", bin, "Heterogeneity (S1, first AU opening): effect by area type")
H2 <- run_scenario("S2", mod, "Heterogeneity (S2, any new or modernized unit): effect by area type")
HH <- rbind(H1, H2)
fwrite(HH, file.path(TAB,"heterogeneity.csv")); saveRDS(HH, file.path(DIR_CLEAN,"heterogeneity.rds"))
cat("\nsaved -> output/tables/heterogeneity.csv + output/figures/fig_heterogeneity_S1.png + _S2.png\n")
