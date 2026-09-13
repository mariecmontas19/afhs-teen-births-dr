# ============================================================================
# 08j_het_summary.R — consolidates ALL S1 heterogeneity/mechanism splits into:
#   (1) a focused DUMBBELL of the cleanest cross-domain "underserved access" dims
#       (rural, health centres, contraceptive unmet need) -> fig_het_access_dumbbell.png
#   (2) a BIG categorized TABLE of every moderator (geography, health system,
#       contraception, socioeconomic, education) -> het_summary_table.csv + .tex
# Adds the WEALTH (census index) split not in 08. Reuses cs_sub (subgroup treated
# vs shared never-treated). Sources: heterogeneity.csv (08), mechanism_jee_contra.csv
# (08g). S1; Denom A; not-yet controls; est=reg; universal base; muni-clustered.
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures"); OVERALL <- -6.43

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19, wealth_index, pct_senasa_2013)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
treated <- bin[!is.na(g) & g>0, .(adm3_pcode, g)]; never <- bin[g==0, adm3_pcode]
cs_sub <- function(pcodes){
  est <- merge(p[, .(adm3_pcode, year, rateA_15_19)],
               rbind(treated[adm3_pcode %in% pcodes], data.table(adm3_pcode=never, g=0L)), by="adm3_pcode")
  est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE)); data.table(att=round(o$overall.att,2), se=round(o$overall.se,2))
}

# ---- WEALTH split (low wealth = underserved) --------------------------------
wb <- unique(p[, .(adm3_pcode, wealth_index)]); Wt <- merge(treated, wb, by="adm3_pcode")[!is.na(wealth_index)]
med <- median(Wt$wealth_index); und <- Wt[wealth_index<med, adm3_pcode]; srv <- Wt[wealth_index>=med, adm3_pcode]
ru <- cs_sub(und); rs <- cs_sub(srv)
wealth_rows <- data.table(moderator="Wealth (census index)",
   group=c("More underserved","Less underserved"), att=c(ru$att,rs$att), se=c(ru$se,rs$se))
cat("wealth split: underserved(low)", ru$att, "(se",ru$se,") | served(high)", rs$att, "(se",rs$se,")\n")

# ---- SUBSIDIZED-REGIME / insurance-enrolment split (MM 2026-08-06) -----------
# higher subsidized (SeNaSa) enrolment = poorer => "more underserved" = high share.
sb <- unique(p[, .(adm3_pcode, pct_senasa_2013)]); Sb <- merge(treated, sb, by="adm3_pcode")[!is.na(pct_senasa_2013)]
medS <- median(Sb$pct_senasa_2013); undS <- Sb[pct_senasa_2013>=medS, adm3_pcode]; srvS <- Sb[pct_senasa_2013<medS, adm3_pcode]
rus <- cs_sub(undS); rss <- cs_sub(srvS)
senasa_rows <- data.table(moderator="Subsidized regime (%)",
   group=c("More underserved","Less underserved"), att=c(rus$att,rss$att), se=c(rus$se,rss$se))
cat("subsidized split: underserved(high)", rus$att, "(se",rus$se,") | served(low)", rss$att, "(se",rss$se,")\n")

# ---- assemble all S1 moderators ---------------------------------------------
H  <- fread(file=file.path(TAB,"heterogeneity.csv"))[scenario=="S1", .(moderator, group, att, se)]
Mx <- fread(file.path(TAB,"mechanism_jee_contra.csv"))[scenario=="S1"]; setnames(Mx,"test","moderator")
Mx <- Mx[, .(moderator, group, att, se)]
ALL <- rbind(H, Mx, wealth_rows, senasa_rows)
map <- data.table(
  moderator=c("Urban share","Baseline isolation","Health centres /10k","Health capability",
              "Baseline unmet need (2013)","Baseline CPR any (2013)","Baseline satisfied demand any (2013)",
              "Poverty (ICV % poor)","Wealth (census index)","Subsidized regime (%)","Education (% sec+)","School enrolment (%)",
              "Baseline JEE intensity (2019-20)"),
  category=c("Geography & isolation","Geography & isolation","Health system","Health system",
             "Contraceptive access","Contraceptive access","Contraceptive access",
             "Socioeconomic","Socioeconomic","Socioeconomic","Education","Education","Education"),
  label=c("Rural vs urban","Distance to nearest unit","Health centres per capita","Health capability index",
          "Unmet need (FP)","Contraceptive use (CPR)","Satisfied demand",
          "Poverty (ICV % poor)","Wealth (census PCA)","Subsidized insurance (\\%)","Education (\\% secondary+)","School enrolment","JEE intensity"),
  ord=c(1,2,1,2,1,2,3,3,4,1,2,3))
ALL <- merge(ALL, map, by="moderator")
ALL[, grp := fifelse(group=="More underserved","under","served")]
# DROP 2025-SIUBEN endline measures (post-treatment -> invalid baseline moderators):
#   ICV poverty (SIUBEN 2025) and School enrolment / inscripción (SIUBEN 2025).
# Kept SES measure = wealth (2022 census, pre-rollout for most cohorts). [MM 2026-06-28]
drop2025 <- c("Poverty (ICV % poor)","School enrolment (%)")
ALL <- ALL[!moderator %in% drop2025]
W <- dcast(ALL, category+label+ord ~ grp, value.var=c("att","se"))
W[, gap := round(att_under - att_served, 2)]
W[, pattern := fcase(gap <= -1, "Underserved larger", abs(gap) < 1, "~No difference", default="Reversed (served larger)")]
catord <- c("Geography & isolation","Health system","Contraceptive access","Socioeconomic","Education")
W[, category := factor(category, levels=catord)]; setorder(W, category, ord)

# ---- per-subgroup pre-treatment BASELINE teen rate (re-derive the splits) ----
dp  <- as.data.table(readRDS(file.path(DIR_CLEAN,"distance_open_panel.rds")))
hc2 <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
cap <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_capability_muni.rds")))[, .(adm3_pcode, capability_index)]
con <- as.data.table(readRDS(file.path(DIR_CLEAN,"dhs_contraception_province.rds")))[, .(adm3_pcode, unmet_2013, cpr_any_2013, satany_2013)]
jeeW<- dcast(as.data.table(readRDS(file.path(DIR_CLEAN,"jee_rollout_muni.rds"))), adm3_pcode~school_year, value.var="pct_jee_sch")
pp  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19, pct_urban, pct_educ_secplus, wealth_index, pct_senasa_2013)]
br  <- merge(treated, pp[, .(adm3_pcode, year, rateA_15_19)], by="adm3_pcode")[year==g-1L, .(adm3_pcode, base=rateA_15_19)]
iso <- merge(treated, dp[, .(adm3_pcode, year, dist_open_km)], by="adm3_pcode")[year==g-1L, .(adm3_pcode, isolation_km=dist_open_km)]
pv  <- unique(pp[, .(adm3_pcode, pct_urban, pct_educ_secplus, wealth_index, pct_senasa_2013)])
Mx  <- Reduce(function(a,b) merge(a,b,by="adm3_pcode",all.x=TRUE),
              list(treated, br, iso, hc2, cap, pv, con, jeeW[, .(adm3_pcode, jee_2019=`2019-2020`)]))
spec <- data.table(
  moderator=c("Urban share","Baseline isolation","Health centres /10k","Health capability","Education (% sec+)",
              "Wealth (census index)","Subsidized regime (%)","Baseline unmet need (2013)","Baseline CPR any (2013)",
              "Baseline satisfied demand any (2013)","Baseline JEE intensity (2019-20)"),
  var=c("pct_urban","isolation_km","sns_per10k","capability_index","pct_educ_secplus","wealth_index",
        "pct_senasa_2013","unmet_2013","cpr_any_2013","satany_2013","jee_2019"),
  under_high=c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE))
basel <- rbindlist(lapply(seq_len(nrow(spec)), function(i){
  v<-spec$var[i]; med<-median(Mx[[v]],na.rm=TRUE)
  und <- if(spec$under_high[i]) Mx[get(v)>=med, adm3_pcode] else Mx[get(v)<med, adm3_pcode]
  srv <- setdiff(Mx$adm3_pcode, und)
  data.table(moderator=spec$moderator[i], group=c("More underserved","Less underserved"),
    n=c(length(und),length(srv)),
    baseline=c(Mx[adm3_pcode %in% und, mean(base,na.rm=TRUE)], Mx[adm3_pcode %in% srv, mean(base,na.rm=TRUE)]))
}))

# ---- FULL TABLE (per moderator): ATT(SE) both halves, baselines, gap, p(DIFFERENCE) ----
# p(diff) = test of underserved-vs-served gap. SE_diff = sqrt(se_u^2+se_s^2) treats the
# halves as independent; because they share the never-treated controls (positive
# covariance), the TRUE diff SE is smaller -> this p is CONSERVATIVE (real gap is more,
# not less, significant). A proper joint-IF test would need a combined bootstrap.
basel <- merge(basel, map[, .(moderator, label)], by="moderator")
bw <- dcast(basel, label ~ group, value.var="baseline")
setnames(bw, c("More underserved","Less underserved"), c("base_u","base_s"))
W2 <- merge(W, bw, by="label")
W2[, `:=`(p_u=2*pnorm(-abs(att_under/se_under)), p_s=2*pnorm(-abs(att_served/se_served)),
          p_diff=2*pnorm(-abs(gap)/sqrt(se_under^2 + se_served^2)))]
W2[, category := factor(category, levels=catord)]; setorder(W2, category, ord)
TT <- W2[, .(Category=category, Moderator=label,
   `ATT_u (SE)`=sprintf("%.2f (%.2f)", att_under, se_under), p_u=round(p_u,3),
   `ATT_s (SE)`=sprintf("%.2f (%.2f)", att_served, se_served), p_s=round(p_s,3),
   Gap=gap, `p(diff)`=round(p_diff,3), `Base U/S`=sprintf("%.0f/%.0f", base_u, base_s))]
cat("\n===== HETEROGENEITY (S1; overall ATT", OVERALL, "); p_u/p_s = each subgroup, p(diff) = gap (conservative) =====\n")
print(TT, class=FALSE)
fwrite(TT, file.path(TAB,"het_summary_table.csv"))

# ---- LaTeX A6 (REBUILT 2026-08-09, MM): reads het_master_table.csv — the SAME
# source as Figure 6a — so table and figure can never diverge (one-canonical
# rule). 12 moderators; retired distance-to-nearest-UNIT row dropped (§26.3/§41
# metric). Caption (short, top) + notes minipage (footnotesize, bottom).
hm <- fread(file.path(TAB,"het_master_table.csv"))
# MM 2026-08-12 (§59.92): hospital-distance row REPLACED by proximity excl.
# Santo Domingo; new 5th panel from the EDN-2019 family questionnaire (05t).
A6 <- data.table(
  label=c("Nearest facility (km)","Nearest facility (km, excl. SD)","2SFCA access",
          "Unmet need (FP)","Contraceptive use (CPR)","Satisfied demand",
          "All facilities /capita","Primary care /capita","Hospitals /capita",
          "Rural vs urban","Wealth (census)","Subsidized regime %",
          "Frequently absent","Parental school participation",
          "Family expects university","Mother completed secondary"),
  pretty=c("Distance to nearest facility (km)","Distance to nearest facility (km, excl.\\ SD)",
           "Two-step floating catchment access","Unmet need for contraception",
           "Contraceptive prevalence (any method)","Satisfied demand for contraception",
           "All facilities per 10{,}000","Primary-care facilities per 10{,}000",
           "Hospitals per 10{,}000","Urban share (rural vs.\\ urban)",
           "Wealth index (census)","Subsidized insurance (\\%)",
           "Frequently absent from school","Parental school participation",
           "Family expects university completion","Mother completed secondary"),
  cat=c(rep("Health access (distance)",3), rep("Contraceptive access",3),
        rep("Health supply (density)",3), rep("Socioeconomic and geography",3),
        rep("Adolescent vulnerability and school attachment",4)))
A6 <- merge(A6, hm, by="label", sort=FALSE); stopifnot(nrow(A6)==16L)
sup <- function(p){ s<-fifelse(p<.01,"***",fifelse(p<.05,"**",fifelse(p<.10,"*",""))); fifelse(s=="","",paste0("$^{",s,"}$")) }
A6[, `:=`(cu=sprintf("\\makecell{%.2f%s (%.2f) \\\\ {[%.2f, %.2f]}}", att_u, sup(p_u), se_u, att_u-1.96*se_u, att_u+1.96*se_u),
          cs=sprintf("\\makecell{%.2f%s (%.2f) \\\\ {[%.2f, %.2f]}}", att_s, sup(p_s), se_s, att_s-1.96*se_s, att_s+1.96*se_s))]
L <- c("\\begin{table}[htbp]\\centering",
       "\\caption{Heterogeneity of the effect by baseline municipality characteristics}",
       "\\label{tab:het}\\footnotesize",
       "\\setlength{\\tabcolsep}{4pt}\\renewcommand{\\arraystretch}{0.92}",
       "\\begin{tabular}{l c c c c}","\\toprule",
       " & More underserved & Less underserved & Gap & $p_{\\mathrm{diff}}$ \\\\","\\midrule")
for(ct in unique(A6$cat)){
  s <- A6[cat==ct]
  L <- c(L, sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", ct))
  for(j in seq_len(nrow(s)))
    L <- c(L, sprintf("\\quad %s & %s & %s & %.2f & %.3f \\\\", s$pretty[j], s$cu[j], s$cs[j], s$gap[j], s$p_diff[j]))
  L <- c(L, "\\addlinespace")
}
L <- c(L, "\\bottomrule","\\end{tabular}",
  "\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
  "\\textit{Notes:} Subgroup Callaway--Sant'Anna ATTs, plotted in \\autoref{fig:het}: the 20 treated",
  "municipalities are split at each baseline's median ($n=10$ per half, at $g-1$) against the shared",
  "never-treated controls (municipality-clustered bootstrap; single seed, fixed estimation order).",
  "The excl.-SD row repeats the proximity split among the 16 treated municipalities outside the",
  "Santo Domingo metropolitan provinces ($n=8$ per half). The two-step",
  "floating catchment (2SFCA) index measures facility supply within overlapping 20-km catchments per",
  "10{,}000 women, discounting each facility by the population competing for it, so it captures",
  "congestion-adjusted access rather than simple distance. $p_{\\mathrm{diff}}$ tests the",
  "underserved$-$served gap (conservative: shared controls) and is unadjusted for the number of",
  "contrasts; proximity in the full sample ($p=.069$) has a Holm-adjusted $p$ of $0.83$, so the splits",
  "are read as descriptive. Overall ATT $=-6.43$. Sources: NHS registry (1{,}588 facilities;",
  "distance and density), ENDESA 2013 (contraception and subsidized insurance, province level),",
  "2022 census (wealth, urbanization); the school-attachment panel uses the 2019 Evaluaci\\'on",
  "Diagn\\'ostica family questionnaire (MoE; family-weighted municipality shares, school year",
  "2018--19, pre-dating every opening in the sample). $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
  "\\end{minipage}","\\end{table}")
writeLines(L, file.path(TAB,"tab_a06_het_summary.tex"))
cat("\nsaved -> het_summary_table.csv + tab_a06_het_summary.tex (12 rows, from het_master)\n")

# ---- FOCUSED DUMBBELL: cleanest cross-domain underserved dims (4, top->bottom) -
foc <- c("Unmet need (FP)","Wealth (census PCA)","Health centres per capita","Rural vs urban")
F <- ALL[label %in% foc]
F[, meas := factor(label, levels=rev(foc))]                  # unmet need at top
F[, group := factor(group, levels=c("More underserved","Less underserved"))]
Fw <- dcast(F, meas ~ grp, value.var="att")
g <- ggplot() +
  geom_vline(xintercept=0, color="grey60") +
  geom_vline(xintercept=OVERALL, linetype="22", color="grey55") +
  geom_segment(data=Fw, aes(x=under, xend=served, y=meas, yend=meas), color="grey78", linewidth=1.6) +
  geom_point(data=F, aes(att, meas, color=group), size=4.8) +
  geom_text(data=F, aes(att, meas, label=sprintf("%+.1f", att), color=group), vjust=-1.5, size=3.5, fontface="bold", show.legend=FALSE) +
  scale_color_manual(values=c("More underserved"=PCUA_COL$green, "Less underserved"=PCUA_COL$orange),
                     labels=c("More underserved","Less underserved (better access)"), name=NULL) +
  coord_cartesian(clip="off") +
  labs(title="The effect concentrates in underserved areas, across four dimensions",
       subtitle=sprintf("Subgroup CS ATT on the 15-19 birth rate; 20 treated split at each baseline's median (dashed = overall %.1f)", OVERALL),
       x="ATT: teen births per 1,000 women (15-19)", y=NULL,
       caption="Contraception (2013 unmet need), wealth (2022 census), health infrastructure (centres/capita), geography (rural). ~10 treated/half: suggestive, not significant (SEs in het_summary_table.csv).") +
  theme_pcua()
ggsave(file.path(FIG,"fig_het_access_dumbbell.png"), g, width=9.6, height=5.0, dpi=200)
cat("\nsaved -> fig_het_access_dumbbell.png + het_summary_table.csv\n")
