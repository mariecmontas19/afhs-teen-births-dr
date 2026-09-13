# ============================================================================
# 10y_shocks_table.R — "concurrent shocks" robustness table (MM 2026-07-29):
# shows the headline surviving the two big contemporaneous shocks rather than
# only asserting it in prose. Panel A = COVID-19 (3 rows: pandemic-period
# test; cohort-restriction test; both). Panel B = Jornada Escolar Extendida
# (3 rows: no covs on the common JEE sample -> headline covs -> + baseline
# JEE 2018-19; "baseline JEE only" dropped per MM). The JEE-orthogonality
# event study (+0.91pp, p=.32) goes in the NOTES with a pointer to fig:jee,
# not in the table body (MM).
# Sources (read this session): covid_robustness.csv, jee_baseline_control.csv,
# headline_S1_S2.csv. Stars *** p<.01, ** p<.05, * p<.10.
# Output: output/tables/tab_07_policies_shocks.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")
st <- function(p) fifelse(p<.01,"^{***}", fifelse(p<.05,"^{**}", fifelse(p<.10,"^{*}","")))

hl <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1" & spec=="CS, no covs"]
cv <- fread(file.path(TAB,"covid_robustness.csv"))
je <- fread(file.path(TAB,"jee_baseline_control.csv"))
# 2026-08-07 (MM): Panel C = child-marriage ban, moved here from the composition
# table as a third concurrent policy. Source = composition_effects.csv (08b), the
# run the paper already prints (prose -5.45 p=.001); NOT child_marriage_ban_age.csv
# (08f), whose SEs differ in the 2nd decimal (different bootstrap stream).
cb <- fread(file.path(TAB,"composition_effects.csv"))[block=="Age (mother)"]
stopifnot(nrow(cb)==2, cb$component %in% c("15-17 birth rate","18-19 birth rate"))

r_hl <- sprintf("Main estimate, for reference & \\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}} & $%+.1f$ & %d & %d \\\\",
                hl$estimate, st(hl$p), hl$SE, hl$estimate-1.96*hl$SE, hl$estimate+1.96*hl$SE, hl$pct_of_baseline, 146L, hl$n_treated)
cvlab <- c("Drop pandemic years (2020--2021)",
           "Post-pandemic opening cohorts only (2022+)",
           "Both restrictions")
r_cv <- sprintf("\\quad %s & \\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}} & $%+.1f$ & %d & %d \\\\",
                cvlab, cv$ATT, st(cv$p), cv$SE, cv$ATT-1.96*cv$SE, cv$ATT+1.96*cv$SE, cv$pct, 126L + cv$n_treated, cv$n_treated)
jekeep <- je[spec != "baseline JEE 2018-19 only"]
jelab <- c("No covariates (common JEE sample)",
           "Main-specification covariates",
           "\\quad + baseline JEE coverage (2018--2019)")
r_je <- sprintf("\\quad %s & \\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}} & & %d & %d \\\\",
                jelab, jekeep$ATT, st(jekeep$p), jekeep$SE, jekeep$ATT-1.96*jekeep$SE, jekeep$ATT+1.96*jekeep$SE, c(145L,145L,145L), c(20L,20L,20L))
cb <- cb[order(component)]  # 15-17 first, then 18-19
cblab <- c("Ages 15--17 (ban-bound)", "Ages 18--19 (not directly bound)")
r_cb <- sprintf("\\quad %s & \\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}} & $%+.1f$ & %d & %d \\\\",
                cblab, cb$ATT, st(cb$p), cb$SE, cb$ATT-1.96*cb$SE, cb$ATT+1.96*cb$SE, cb$pct_of_base, c(146L,146L), c(20L,20L))

tex <- c(
"\\begin{table}[htbp]\\centering\\footnotesize",
"\\setlength{\\tabcolsep}{2.5pt}",
"\\caption{Sensitivity to concurrent policies and shocks}",
"\\label{tab:shocks}",
"\\begin{tabular}{lcccc}",
"\\toprule",
" & Estimate (SE) [95\\% CI] & \\% of baseline & Munis & Treated \\\\",
"\\midrule",
r_hl,
"\\addlinespace",
"\\multicolumn{5}{l}{\\textit{Panel A. COVID-19}} \\\\",
r_cv,
"\\addlinespace",
"\\multicolumn{5}{l}{\\textit{Panel B. Jornada Escolar Extendida (JEE)}} \\\\",
r_je,
"\\addlinespace",
"\\multicolumn{5}{l}{\\textit{Panel C. Child-marriage ban (Law 1-21, effective January 2021)}} \\\\",
r_cb,
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.96\\linewidth}\\vspace{4pt}\\footnotesize",
r"(\textit{Notes:} Each row reports a Callaway--Sant'Anna group ATT for the adolescent birth rate using not-yet-treated controls, a universal base period, and municipality-clustered multiplier-bootstrap standard errors. ``\% of baseline'' is relative to the treated mean in the year before opening. \emph{Panel A} examines pandemic calendar years and opening cohorts. Dropping 2020--2021 while retaining all treated municipalities is the most direct pandemic check. Restricting the sample to later opening cohorts also removes cohorts observed at longer exposures, so the resulting attenuation is not a clean test of pandemic confounding. \emph{Panel B} examines sensitivity to the extended school day. All rows use the 145 municipalities with JEE data; the last adds baseline JEE coverage to the main covariate set. Appendix~\autoref{fig:jee} shows that JEE coverage evolves similarly in treated and never-treated municipalities around AU openings. \emph{Panel C} splits the teen birth rate by age band using the common denominator of women aged 15--19. A response to the January 2021 child-marriage ban should be concentrated below age 18; instead, the decline is concentrated at ages 18--19. Births at ages 10--14 appear in Appendix~\autoref{tab:agegradient}. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.)",
"\\end{minipage}",
"\\end{table}")
writeLines(tex, file.path(TAB,"tab_07_policies_shocks.tex"))
cat("wrote tab_07_policies_shocks.tex\n"); cat(r_hl, r_cv, r_je, sep="\n")
