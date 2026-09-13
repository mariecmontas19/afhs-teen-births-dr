# ============================================================================
# 07a_descriptives.R — Descriptive tables + figures (pre-event-study).
# Tables (output/tables/, .csv + .tex):
#   T1 birth-level descriptive stats (15-19 | 30-34 | All births)
#   T2 municipality baseline balance (in-window-treated vs never-treated, norm. diff)
#   T3 AU roll-out summary (by cohort)
# Figures (output/figures/, .pdf + .png):
#   F2 treatment-timing diagram   F3 national teen-rate trend
#   F4 age-group fertility trends (comparison-group check)
#   F5 raw event-time trends (pre-trend gate)   [F1 roll-out map already built]
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2); library(xtable)})
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")
dir.create(TAB, recursive=TRUE, showWarnings=FALSE); dir.create(FIG, recursive=TRUE, showWarnings=FALSE)

b  <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
tt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
A  <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))

## ---------- TABLE 1: birth-level descriptives (15-19 | 30-34 | All) ----------
pc1 <- function(x) sprintf("%.1f", 100*mean(x, na.rm=TRUE))                 # percent
ms  <- function(x) sprintf("%.1f (%.1f)", mean(x,na.rm=TRUE), sd(x,na.rm=TRUE))  # mean (sd)
desc <- function(d) c(
  "N (births)"                  = format(nrow(d), big.mark=","),
  "Maternal age, mean (SD)"     = ms(d$age_mom),
  "% Dominican"                 = pc1(d$nationality=="Dominican"),
  "% Haitian"                   = pc1(d$nationality=="Haitian"),
  "% in union [2021+]"          = pc1(d$in_union),
  "% public facility"           = pc1(d$facility=="Public"),
  "% physician-attended"        = pc1(d$doctor_attended),
  "% public insurance [2018+]"  = pc1(d$insurance=="Public"),
  "% no insurance [2018+]"      = pc1(d$insurance=="None"),
  "% ≥ secondary educ [2020+]" = sprintf("%.1f", 100*mean((d$educ %in% c("Secondary","University"))[!is.na(d$educ)])),
  "Prenatal visits, mean (SD) [2021+]" = ms(as.numeric(d$prenatal_checks)),
  "% ≥4 prenatal visits [2021+]"  = pc1(d$anc4),
  "% cesarean"                  = pc1(d$csection),
  "Birthweight g, mean (SD)"    = ms(d$weight_g),
  "% low birthweight (<2500g)"  = pc1(d$low_bw),
  "% preterm (<37 wk)"          = pc1(d$preterm),
  "% small-for-gest-age"        = pc1(d$sga),
  "% any neonatal risk"         = pc1(d$neo_any))
T1 <- data.table(Characteristic=names(desc(b)),
                 `Age 15-19`=desc(b[age_grp=="15-19"]),
                 `Age 30-34`=desc(b[age_grp=="30-34"]),
                 `All births`=desc(b))
fwrite(T1, file.path(TAB,"table1_birth_descriptives.csv"))
print(xtable(T1, caption="Birth-level descriptive statistics, DR live births 2016-2025.",
             label="tab:birth_desc"),
      file=file.path(TAB,"table1_birth_descriptives.tex"), include.rownames=FALSE, booktabs=TRUE)

## ---------- TABLE 2: municipality baseline balance (in-window treated vs never) ----------
base_rate <- p[year %in% 2016:2019, .(baseline_teenrate_A=mean(rateA_15_19)), by=adm3_pcode]   # pre-rollout
mc <- unique(p[, .(adm3_pcode, ever_treated, always_treated, wealth_index, pct_educ_secplus,
                   pct_urban, pct_internet, cwr_2010_baseline, pct_bottom2q_2018,
                   womenA_15_19_2016=NA_real_)])
mc[, womenA_15_19_2016 := p[year==2016][.SD, on="adm3_pcode", x.womenA_15_19]]
mc <- merge(mc, base_rate, by="adm3_pcode")
mc <- mc[always_treated==0]                                                  # in-window treated + never (always-treated dropped)
nT <- mc[ever_treated==1, .N]; nC <- mc[ever_treated==0, .N]                  # dynamic counts (NEVER hardcode)
grp <- function(v){ T<-mc[ever_treated==1][[v]]; C<-mc[ever_treated==0][[v]]
  nd <- (mean(T)-mean(C))/sqrt((var(T)+var(C))/2)
  c(sprintf("%.2f", mean(T)), sprintf("%.2f", mean(C)), sprintf("%.2f", nd)) }
vars <- c(wealth_index="Wealth index (PCA)", pct_educ_secplus="% women sec+ (census)",
          pct_urban="% urban", pct_internet="% internet", cwr_2010_baseline="Child-woman ratio 2010",
          pct_bottom2q_2018="% bottom-2 income quintiles", womenA_15_19_2016="Women 15-19 (2016)",
          baseline_teenrate_A="Teen rate 15-19, 2016-19 (A)")
T2 <- data.table(Variable=unname(vars),
                 Treated=sapply(names(vars), function(v) grp(v)[1]),
                 Never=sapply(names(vars), function(v) grp(v)[2]),
                 `Norm. diff.`=sapply(names(vars), function(v) grp(v)[3]))
setnames(T2, c("Treated","Never"), c(sprintf("Treated (n=%d)", nT), sprintf("Never-treated (n=%d)", nC)))
fwrite(T2, file.path(TAB,"table2_muni_balance.csv"))
print(xtable(T2, caption="Municipality baseline balance: in-window-treated vs never-treated.",
             label="tab:balance"),
      file=file.path(TAB,"table2_muni_balance.tex"), include.rownames=FALSE, booktabs=TRUE)

## ---------- TABLE 3: AU roll-out summary ----------
T3 <- tt[ever_treated==1, .(municipalities=.N, units=sum(n_units),
                            type=ifelse(any(always_treated==1) & all(always_treated==1),"always-treated",
                                 ifelse(all(always_treated==0),"in-window","mixed"))), by=cohort][order(cohort)]
T3[, status := fifelse(cohort<2016, "always-treated (pre-2016)", "in-window (2016-25)")]
fwrite(T3, file.path(TAB,"table3_rollout.csv"))
print(xtable(T3[, .(cohort, municipalities, units, status)], caption="AU roll-out by cohort.",
             label="tab:rollout"),
      file=file.path(TAB,"table3_rollout.tex"), include.rownames=FALSE, booktabs=TRUE)

## ---------- FIGURE 2: treatment-timing diagram (ever-treated municipalities) ----------
tim <- p[ever_treated==1, .(adm3_name, year, treated_now=as.integer(year>=first_year), always_treated, first_year)]
tim[, lab := paste0(adm3_name, ifelse(always_treated==1," *",""))]
tim[, lab := factor(lab, levels=unique(tim[order(-first_year)]$lab))]
f2 <- ggplot(tim, aes(year, lab, fill=factor(treated_now))) +
  geom_tile(color="white", linewidth=0.3) +
  scale_fill_manual(values=c("0"="grey88","1"=PCUA_COL$red), labels=c("not yet / pre","PCUA open"), name=NULL) +
  scale_x_continuous(breaks=2016:2025) +
  labs(title="AU treatment timing by municipality (ever-treated)", subtitle="* = always-treated (first unit pre-2016; excluded from binary spec)",
       x=NULL, y=NULL, caption="155-municipality analysis geography. 128 never-treated municipalities not shown.") +
  theme_pcua(base_size=9) + theme(panel.grid=element_blank(), legend.position="top",
       plot.title=element_text(face="bold"))
ggsave(file.path(FIG,"fig2_treatment_timing.pdf"), f2, width=8, height=7, device=cairo_pdf)
ggsave(file.path(FIG,"fig2_treatment_timing.png"), f2, width=8, height=7, dpi=150)

## ---------- FIGURE 3: national teen-rate trend (15-19 + 10-14, denominator A) ----------
nat <- p[, .(r15_A=1000*sum(nb_15_19)/sum(womenA_15_19),
             r1014_A=1000*sum(nb_10_14)/sum(womenA_10_14)), by=year][order(year)]
natL <- melt(nat, id.vars="year")
natL[, series := factor(variable, levels=c("r15_A","r1014_A"),
     labels=c("15-19","10-14"))]
f3 <- ggplot(natL, aes(year, value, color=series)) +
  geom_line(linewidth=0.9) + geom_point(size=1.6) +
  annotate("rect", xmin=2015.6, xmax=2019.4, ymin=-Inf, ymax=Inf, alpha=0.07, fill="grey40") +
  annotate("text", x=2017.5, y=max(natL$value)*0.96, label="registration ramp", size=2.8, color="grey35") +
  scale_x_continuous(breaks=2016:2025) +
  scale_color_manual(values=c("15-19"=PCUA_COL$red,"10-14"=PCUA_COL$blue), name=NULL) +
  labs(title="National adolescent fertility rate, Dominican Republic 2016-2025",
       y="Births per 1,000 women", x=NULL,
       caption="Source: BDNV births / NSO official denominator (series A).") +
  theme_pcua(base_size=11) + theme(legend.position="top", plot.title=element_text(face="bold"))
# fig3_national_teenrate RETIRED 2026-06-29 (two-line chart lacked the location dimension) —
# superseded by fig3b_motivation.R (time-lapse choropleth + province spaghetti). f3 kept built
# (used by nothing else) but no longer saved; old PNG/PDF archived to output/figures/archive/.
# ggsave(file.path(FIG,"fig3_national_teenrate.pdf"), f3, width=9, height=5.5, device=cairo_pdf)
# ggsave(file.path(FIG,"fig3_national_teenrate.png"), f3, width=9, height=5.5, dpi=150)

## ---------- FIGURE 4: age-specific fertility trends, ALL reproductive age groups ----------
AGES7 <- c("10-14","15-19","20-24","25-29","30-34","35-39","40-44")
numF <- b[!is.na(birth_year) & age_grp %in% AGES7, .(bn=.N), by=.(year=birth_year, age=as.character(age_grp))]
denF <- A[sex=="F" & age_group %in% AGES7, .(w=sum(pop)), by=.(year, age=age_group)]
agL  <- merge(numF, denF, by=c("year","age")); agL[, rate := 1000*bn/w]
agL[, age := factor(age, levels=AGES7)]
f4 <- ggplot(agL, aes(year, rate, color=age)) + geom_line(linewidth=0.9) + geom_point(size=1.4) +
  scale_x_continuous(breaks=2016:2025) + scale_color_viridis_d(end=0.95, name="Age group") +
  labs(title="Age-specific fertility rates by year, Dominican Republic (denom A)",
       subtitle="Comparison-group check: 15-19 (treated age) vs 30-34 + alternates; all reproductive ages 10-44",
       y="Births per 1,000 women", x=NULL,
       caption="45+ omitted (births '45+' not separable into 5-yr bands; ~0 fertility).") +
  theme_pcua(base_size=11) + theme(legend.position="top", plot.title=element_text(face="bold"))
ggsave(file.path(FIG,"fig4_agegroup_trends.pdf"), f4, width=9, height=5.5, device=cairo_pdf)
ggsave(file.path(FIG,"fig4_agegroup_trends.png"), f4, width=9, height=5.5, dpi=150)

## ---------- FIGURE 5: raw event-time trends (pre-trend gate) ----------
ev <- p[ever_treated==1 & always_treated==0 & !is.na(event_time) & event_time>=-6 & event_time<=5]
evs <- ev[, .(teen_rate=weighted.mean(rateA_15_19, womenA_15_19),
              age_ddd=weighted.mean(ddd_A, womenA_15_19), n=.N), by=event_time][order(event_time)]
evL <- melt(evs[, .(event_time, `Teen rate 15-19`=teen_rate, `Age-DDD (15-19 - 30-34)`=age_ddd)],
            id.vars="event_time", variable.name="outcome", value.name="value")
f5 <- ggplot(evL, aes(event_time, value)) +
  geom_vline(xintercept=-0.5, linetype="dashed", color="grey50") +
  geom_line(linewidth=0.9, color=PCUA_COL$red) + geom_point(size=1.8, color=PCUA_COL$red) +
  facet_wrap(~outcome, scales="free_y") +
  scale_x_continuous(breaks=-6:5) +
  labs(title="Raw event-time trends, in-window-treated municipalities (pop-weighted)",
       subtitle="Descriptive pre-trend check (not the estimator); dashed line = first AU opening",
       x="Years since first AU opened", y="Rate per 1,000 women",
       caption="Treated municipalities only; raw means by event time. event_time in [-6,5].") +
  theme_pcua(base_size=10) + theme(plot.title=element_text(face="bold"))
ggsave(file.path(FIG,"fig5_eventtime_raw.pdf"), f5, width=10, height=5, device=cairo_pdf)
ggsave(file.path(FIG,"fig5_eventtime_raw.png"), f5, width=10, height=5, dpi=150)

## ---------- APPENDIX: per-municipality 15-19 & 30-34 trends (155 analysis geography, ~10/page) ----------
ord <- unique(p[, .(adm3_pcode, adm3_name, ever_treated, always_treated, first_year)])
ord[, grp := fifelse(ever_treated==1 & always_treated==0, 1L, fifelse(always_treated==1, 2L, 3L))]
setorder(ord, grp, first_year, adm3_name); ord[, pos := .I]
mm <- melt(p[, .(adm3_pcode, year, `15-19`=rateA_15_19, `30-34`=rateA_30_34)],
           id.vars=c("adm3_pcode","year"), variable.name="age", value.name="rate")
mm <- merge(mm, ord, by="adm3_pcode"); mm[, fac := factor(adm3_name, levels=ord$adm3_name)]
per <- 10; pages <- split(ord$adm3_name, ceiling(ord$pos/per))
pdf(file.path(FIG,"appendix_muni_trends.pdf"), width=11, height=8.5)
for (k in seq_along(pages)){
  dd <- mm[adm3_name %in% pages[[k]]]
  vl <- unique(dd[ever_treated==1, .(fac, first_year)])
  g <- ggplot(dd, aes(year, rate, color=age)) +
    { if (nrow(vl)) geom_vline(data=vl, aes(xintercept=first_year), linetype="dashed", color="grey55", linewidth=0.3) } +
    geom_line(linewidth=0.6) + facet_wrap(~fac, ncol=5, scales="free_y") +
    scale_color_manual(values=c("15-19"=PCUA_COL$red,"30-34"=PCUA_COL$blue), name=NULL) +
    scale_x_continuous(breaks=c(2016,2020,2024)) +
    labs(title=sprintf("Appendix %d/%d — teen (15-19) vs comparison (30-34) fertility by municipality (denom A)", k, length(pages)),
         subtitle="dashed = AU opening (treated); order: in-window-treated -> always-treated -> never-treated",
         x=NULL, y="Births per 1,000 women") +
    theme_pcua(base_size=8) + theme(legend.position="top", strip.text=element_text(size=6.5))
  print(g)
  ggsave(file.path(FIG, sprintf("muni_trends_p%02d.png", k)), g, width=11, height=8.5, dpi=110)
}
dev.off()
cat("appendix per-municipality trends ->", length(pages), "pages (appendix_muni_trends.pdf + p01..pNN.png)\n")

cat("\n==================== 07a DESCRIPTIVES ====================\n")
cat("Tables -> table1_birth_descriptives, table2_muni_balance, table3_rollout (.csv+.tex)\n")
cat("Figures -> fig2_treatment_timing, fig4_agegroup_trends, fig5_eventtime_raw (.pdf+.png) [fig3 retired -> fig3b_motivation.R]\n")
cat("\nTable 2 balance (normalized differences):\n"); print(T2, class=FALSE)
cat("\nFig 5 event-time (raw, treated):\n"); print(evs, class=FALSE)
