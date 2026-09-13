# ============================================================================
# 08e_educ_composition.R — MECHANISM (education): does the unit cut births to
# LESS-EDUCATED teens more? SHORT, heavily-caveated event study.
#
# Maternal education is recorded on the birth record ONLY from 2020 onward
# (100% missing 2016-2019; ~20% missing 2020-2025). So unlike the AGE
# decomposition (08b, full 2016-2025 panel), this can only run on a 6-year
# panel (2020-2025). Decomposition (valid, same logic as age):
#   rate_loweduc = 1,000 * (teen births to mothers with < secondary) / women 15-19
#   rate_secplus = 1,000 * (teen births to mothers with >=secondary) / women 15-19
# Both per the SAME well-measured denominator (women 15-19) -> a level
# "composition effect": if |ATT_loweduc| > |ATT_secplus|, the unit
# disproportionately prevents births among less-educated teens.
#
# CAVEATS (printed + in the caption):
#   - 6-year panel; educ first recorded 2020. Only cohorts g>=2021 have any
#     pre-period; g>=2022 have >=2 clean pre-years. Few treated clusters/cohort
#     -> wide CIs, pre-trends barely testable. This is SUGGESTIVE, not headline.
#   - ~20% of teen births have missing education (excluded from both numerators;
#     the two components sum to ~80% of the teen rate). NA-share is ~flat across
#     years (0.18-0.25), so dropping is ~proportional, but noted.
#   - Education here is the teen mother's schooling AT BIRTH, endogenous to the
#     birth itself -> this is "who are the prevented births", NOT a causal effect
#     of AU on educational attainment.
# Denominator A; not-yet controls; est=reg; universal base; municipality-clustered.
# Outputs: output/tables/educ_composition_effects.csv,
#          output/figures/fig_educ_composition.png
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")
YRS <- 2020:2025                                            # educ-coverage window

# ---- teen births -> low/high education counts per municipality-year -------------
b  <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
te <- b[age_mom %in% 15:19 & !is.na(adm3_pcode) & birth_year %in% YRS]
te[, loweduc := fifelse(is.na(educ), NA_integer_, as.integer(!(educ %in% c("Secondary","University"))))]
comp <- te[, .(nb_teen=.N,
               nb_low =sum(loweduc==1L, na.rm=TRUE),
               nb_high=sum(loweduc==0L, na.rm=TRUE),
               nb_na  =sum(is.na(loweduc))), by=.(adm3_pcode, year=birth_year)]

# ---- merge denominator + treatment timing (folded 155; drop always-treated) --
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
       year %in% YRS, .(adm3_pcode, year, womenA_15_19)]
d <- merge(p, comp, by=c("adm3_pcode","year"), all.x=TRUE)
for(v in c("nb_teen","nb_low","nb_high","nb_na")) d[is.na(get(v)), (v):=0L]
d <- d[!adm3_pcode %in% fold]
d[, `:=`(rate_loweduc=1000*nb_low/womenA_15_19, rate_secplus=1000*nb_high/womenA_15_19)]
# treatment timing from AUTHORITATIVE treatment_municipio.rds (panel embedded fields are STALE
# pre-overhaul §13 — they mis-code Las Terrenas/DOM032003 as never-treated; it is treated 2024)
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
d <- merge(d, bin[, .(adm3_pcode, g)], by="adm3_pcode", all.x=TRUE)
cat("panel:", d[,uniqueN(adm3_pcode)],"munis x",d[,uniqueN(year)],"years (",paste(range(d$year),collapse="-"),")\n")
cat("NA-educ share of teen births by year:\n"); print(d[, .(na_share=round(sum(nb_na)/sum(nb_teen),3)), by=year][order(year)], class=FALSE)
cat("treated cohorts in-window (g):", paste(sort(unique(d[g>0,g])),collapse=", "),
    "| never-treated:", d[g==0,uniqueN(adm3_pcode)], "\n")

# ---- CS group-ATT (+ dynamic for figure) for a decomposition outcome ---------
csrun <- function(yname){
  est <- d[!is.na(g)]; est[, id:=as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", base_period="universal",
        est_method="reg", bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  list(gr=suppressMessages(aggte(a,type="group",na.rm=TRUE)),
       dy=suppressMessages(aggte(a,type="dynamic",na.rm=TRUE)))
}
base_of <- function(v) d[!is.na(g)&g>0][year==g-1L, mean(get(v), na.rm=TRUE)]
row_of  <- function(label, yname){
  r <- csrun(yname)$gr; bs <- base_of(yname)
  data.table(component=label, ATT=round(r$overall.att,2), SE=round(r$overall.se,2),
             p=round(2*pnorm(-abs(r$overall.att/r$overall.se)),3),
             baseline_2020_25=round(bs,2), pct_of_base=round(100*r$overall.att/bs,0),
             unit="per 1,000 women 15-19")
}
# formal test of the DIFFERENTIAL: one CS on (rate_loweduc - rate_secplus). CS is
# linear so this ATT = ATT_low - ATT_high exactly, but its SE correctly accounts for
# the covariance of the two components -> the rigorous "is there an education
# differential?" test. Insignificant => broad-based (no distinguishable differential).
d[, rate_diff := rate_loweduc - rate_secplus]
rd <- csrun("rate_diff")$gr
diff_row <- data.table(component="DIFFERENTIAL (below-sec minus sec+)",
  ATT=round(rd$overall.att,2), SE=round(rd$overall.se,2),
  p=round(2*pnorm(-abs(rd$overall.att/rd$overall.se)),3),
  baseline_2020_25=NA_real_, pct_of_base=NA_real_, unit="differential, per 1,000 women 15-19")
tab <- rbind(row_of("Below secondary (None/Primary)", "rate_loweduc"),
             row_of("Secondary or higher",            "rate_secplus"),
             diff_row)
cat("\n========= EDUCATION COMPOSITION EFFECTS (S1 CS, 2020-2025 short panel) =========\n")
print(tab, class=FALSE)
cat(if(diff_row$p>=0.10)
      sprintf("-> DIFFERENTIAL p=%.3f (n.s.): no statistically distinguishable education differential -> BROAD-BASED.\n", diff_row$p)
    else sprintf("-> DIFFERENTIAL p=%.3f (sig): %s teens see the larger reduction.\n", diff_row$p,
                 if(rd$overall.att<0) "less-educated (below-secondary)" else "more-educated (secondary+)"))
fwrite(tab, file.path(TAB,"educ_composition_effects.csv"))

# ---- figure: dynamic event study, low vs high educ ---------------------------
rl <- csrun("rate_loweduc"); rh <- csrun("rate_secplus")
ptl <- es_pretrend_p(rl$dy, lo=-3); pth <- es_pretrend_p(rh$dy, lo=-3)   # joint pre-trend over PLOTTED leads (e=-3..-2)
cat(sprintf("pre-trend leads: below-sec p=%.3f (k=%d) | sec+ p=%.3f (k=%d)\n", ptl$p, ptl$k, pth$p, pth$k))
dd <- rbind(data.table(e=rl$dy$egt, att=rl$dy$att.egt, se=rl$dy$se.egt, grp="Below secondary"),
            data.table(e=rh$dy$egt, att=rh$dy$att.egt, se=rh$dy$se.egt, grp="Secondary or higher"))
dd <- dd[e>=-3 & e<=3]; dd[, `:=`(lo=att-1.96*se, hi=att+1.96*se)]
rng <- ceiling(max(abs(c(dd$lo, dd$hi)), na.rm=TRUE)/5)*5
ptlab <- function(nm,pt) if(is.na(pt$p)||pt$k==0) sprintf("%s: not estimable",nm) else
                         sprintf("%s p=%.2f (%s)",nm,pt$p,ifelse(pt$p<.05,"sig","n.s."))
ann <- sprintf("Pre-trend joint test (leads):  %s;  %s", ptlab("below-sec",ptl), ptlab("sec+",pth))
g1 <- ggplot(dd, aes(e, att, color=grp, fill=grp)) +
  es_guides(label_y=rng) +
  geom_hline(yintercept=0, color="grey25", linewidth=0.7) +              # bold zero line
  geom_ribbon(aes(ymin=lo,ymax=hi), alpha=0.09, color=NA, position=position_dodge(0.25)) +
  geom_line(linewidth=0.9, position=position_dodge(0.25)) +
  geom_point(size=2.3, shape=21, fill="white", stroke=1.1, position=position_dodge(0.25)) +
  annotate("text", x=-3, y=rng*0.95, hjust=0, vjust=1, size=2.9, fontface="italic", color="grey25", label=ann) +
  scale_color_manual(values=c("Below secondary"=PCUA_COL$red, "Secondary or higher"=PCUA_COL$blue),
                     name="Mother's education") +
  scale_fill_manual(values=c("Below secondary"=PCUA_COL$red, "Secondary or higher"=PCUA_COL$blue), guide="none") +
  scale_x_continuous(breaks=seq(-3,3,1)) +
  scale_y_continuous(limits=c(-rng, rng), breaks=seq(-rng, rng, 5)) +
  labs(title="Education mechanism: who are the prevented births? (short 2020-2025 panel)",
       subtitle="S1 CS event study on teen birth rates by mother's education (both per 1,000 women 15-19; sum to ~80% of teen rate)",
       x="Years since first AU opening", y="ATT: births per 1,000 women (15-19)",
       caption="Educ recorded 2020+; only g>=2021 cohorts contribute. ~20% of teen births miss educ. Denominator A; not-yet controls; 95% CIs; muni-clustered. SUGGESTIVE.") +
  theme_pcua()
ggsave(file.path(FIG,"fig_educ_composition.png"), g1, width=9, height=5.2, dpi=200)
cat("\nsaved -> educ_composition_effects.csv + fig_educ_composition.png\n")
