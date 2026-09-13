# ============================================================================
# 07e_compare_comparison_groups.R — choose the age-DDD comparison group on evidence.
# For each candidate X in {20-24, 25-29, 30-34}: (1) CS on the DDD outcome
# rate(15-19)-rate(X) -> ATT + PRE-TREND leads (flatter = better parallel trends);
# (2) CS placebo on rate(X) alone -> should be ~0 (uncontaminated comparison).
# Best X = flat DDD pre-trends + placebo ~0 + least cohort contamination.
# denom A; not-yet controls; municipality-clustered bootstrap + uniform bands.
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); FIG <- file.path(PROJ,"output","figures")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
est <- p[always_treated==0]
est[, id := as.integer(factor(adm3_pcode))]
est[, g  := fifelse(ever_treated==1L, as.integer(first_year), 0L)]
est[, `:=`(ddd_2024 = rateA_15_19 - rateA_20_24,
           ddd_2529 = rateA_15_19 - rateA_25_29,
           ddd_3034 = rateA_15_19 - rateA_30_34,
           ddd_3539 = rateA_15_19 - rateA_35_39,
           ddd_4044 = rateA_15_19 - rateA_40_44)]

cs <- function(yname){
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=1000,
        base_period="universal", clustervars="id", est_method="reg")))
  list(gr=aggte(a,type="group",na.rm=TRUE), dy=aggte(a,type="dynamic",na.rm=TRUE))
}
cand <- list(`20-24`=c(ddd="ddd_2024", rate="rateA_20_24"),
             `25-29`=c(ddd="ddd_2529", rate="rateA_25_29"),
             `30-34`=c(ddd="ddd_3034", rate="rateA_30_34"),
             `35-39`=c(ddd="ddd_3539", rate="rateA_35_39"),
             `40-44`=c(ddd="ddd_4044", rate="rateA_40_44"))
contam <- c(`20-24`="HIGH (teen ages into 20-24 in-window)", `25-29`="moderate",
            `30-34`="none", `35-39`="none", `40-44`="none")

rows <- list(); figs <- list()
for (nm in names(cand)){
  d <- cs(cand[[nm]]["ddd"]); pl <- cs(cand[[nm]]["rate"])
  dd <- data.table(e=d$dy$egt, att=d$dy$att.egt, se=d$dy$se.egt)
  pre <- dd[e %in% c(-4,-3,-2)]                                  # well-populated pre leads (e=-1 is ref)
  rows[[nm]] <- data.table(
    comparison = nm,
    DDD_ATT    = round(d$gr$overall.att,2),  DDD_SE = round(d$gr$overall.se,2),
    DDD_pre_maxAbsLead = round(max(abs(pre$att)),2),
    DDD_pre_anySig     = any(abs(pre$att) > 1.96*pre$se),        # TRUE = a significant pre-trend (bad)
    placebo_ATT = round(pl$gr$overall.att,2), placebo_SE = round(pl$gr$overall.se,2),
    placebo_sig = abs(pl$gr$overall.att) > 1.96*pl$gr$overall.se,# TRUE = comparison group moves (bad)
    cohort_contam = contam[nm])
  # placebo event-study figure (credible window e in [-5,2])
  ep <- dd[e>=-5 & e<=2]; ep <- merge(ep, data.table(e=pl$dy$egt, patt=pl$dy$att.egt, pse=pl$dy$se.egt, pc=pl$dy$crit.val.egt), all.x=TRUE)
  pf <- data.table(e=pl$dy$egt, att=pl$dy$att.egt, se=pl$dy$se.egt, crit=pl$dy$crit.val.egt)[e>=-5 & e<=2]
  pf[, `:=`(lo=att-crit*se, hi=att+crit*se)]
  figs[[nm]] <- ggplot(pf, aes(e,att)) +
    es_guides() +
    geom_ribbon(aes(ymin=lo,ymax=hi), fill=PCUA_COL$blue, alpha=0.16) +
    geom_line(color=PCUA_COL$blue, linewidth=0.9) +
    geom_point(color=PCUA_COL$blue, fill="white", shape=21, size=2.3, stroke=1.1) +
    scale_x_continuous(breaks=seq(-5,2,1)) +
    labs(title=paste0("Placebo event study: ", nm, " birth rate"),
         subtitle="A clean comparison age should be flat (~0): it must not respond to a teen-focused unit",
         x="Years since first AU opening", y=paste0("ATT: ", nm, " births per 1,000 women"),
         caption="Denominator A; CS, not-yet controls; uniform 95% bands; municipality-clustered; e<=2.") +
    theme_pcua()
  ggsave(file.path(FIG, paste0("fig8_placebo_", gsub("-","_",nm), ".png")), figs[[nm]], width=8, height=4.8, dpi=200)
}
tab <- rbindlist(rows)

# ---- SUMMARY figure: the single "why 30-34" plot — placebo ATT for the 3 candidate
#      comparison ages (20-24, 25-29, 30-34). Contamination "traffic-light" lollipop:
#      red->amber->green as the age moves away from the teens toward the clean comparison.
ts <- copy(tab)[comparison %in% c("20-24","25-29","30-34")]
ts[, comparison := factor(comparison, levels=c("30-34","25-29","20-24"))]   # 20-24 top -> 30-34 bottom
ts[, `:=`(lo=placebo_ATT-1.96*placebo_SE, hi=placebo_ATT+1.96*placebo_SE,
          vlab=sprintf("%+.1f", placebo_ATT))]
agecol <- c("20-24"=PCUA_COL$red, "25-29"=PCUA_COL$orange, "30-34"=PCUA_COL$green)
sg <- ggplot(ts, aes(placebo_ATT, comparison, color=comparison)) +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill=PCUA_COL$green, alpha=0.06) +  # "clean zone" ~ 0
  annotate("text", x=0, y=3.45, label="clean comparison (~0)", size=2.9, color=PCUA_COL$green, fontface="italic") +
  geom_vline(xintercept=0, color="grey55") +
  geom_segment(aes(x=0, xend=placebo_ATT, y=comparison, yend=comparison), linewidth=1.1, alpha=0.5) +  # lollipop stick
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=0, linewidth=0.7, alpha=0.6) +
  geom_point(size=5) +
  geom_text(aes(label=vlab), vjust=-1.3, size=3.7, fontface="bold", show.legend=FALSE) +
  scale_color_manual(values=agecol, guide="none") +
  scale_y_discrete(expand=expansion(add=c(0.5, 0.9))) +
  labs(title="Contamination shrinks with age: 30-34 is the clean comparison group",
       subtitle="Placebo CS ATT on each candidate age group's own birth rate - it should NOT respond to a teen-focused unit",
       x="Placebo ATT on the age-group birth rate (per 1,000 women)", y="Comparison age group",
       caption="Denominator A; CS, not-yet controls; 95% CIs; municipality-clustered. 20-24/25-29 fall as treated teens age into them (25-29 imprecise); 30-34 sits on zero.") +
  theme_pcua()
ggsave(file.path(FIG,"fig_comparison_age.png"), sg, width=9, height=4.6, dpi=200)
cat("==================== 07e COMPARISON-GROUP CHOICE (denom A) ====================\n")
print(tab, class=FALSE)
cat("\nBest = flattest DDD pre-trend (low maxAbsLead, anySig=FALSE) + placebo ~0 (placebo_sig=FALSE) + low contamination.\n")
saveRDS(tab, file.path(DIR_CLEAN,"comparison_group_choice.rds"))
