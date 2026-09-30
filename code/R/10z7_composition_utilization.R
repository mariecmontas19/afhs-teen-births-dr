# ============================================================================
# 10z7_composition_utilization.R — two small appendix exhibits (2026-08-05):
#   tab_a13_composition.tex  Mechanisms "quantity, not composition": maternal-
#                          education split (educ_composition_effects.csv) +
#                          the child-marriage-ban age bands
#                          (composition_effects.csv).
#   tab_a14_utilization.tex  The utilization signal from the hospital registries
#                          (mechanisms_cs.csv): consult levels, adolescent
#                          share, counseling series.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")
stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
cc <- function(est,se,p,d=2){ s<-stars(p); sup<-if(s=="")"" else sprintf("^{%s}",s)
  sprintf("\\makecell{$%.*f%s$ (%.*f) \\\\ {[$%.*f$, $%.*f$]}}", d, est, sup, d, se, d, est-1.96*se, d, est+1.96*se) }

## ---- composition ------------------------------------------------------------
ec <- fread(file.path(TAB,"educ_composition_effects.csv"))
cf <- fread(file.path(TAB,"composition_effects.csv"))
ge <- function(k){ r <- ec[component==k]; stopifnot(nrow(r)==1); r }
gc_ <- function(k){ r <- cf[component==k]; stopifnot(nrow(r)==1); r }
rowE <- function(k, lb){ r <- ge(k)
  sprintf("%s & %s & %s & %s \\\\", lb, cc(r$ATT,r$SE,r$p),
          fifelse(is.na(r$baseline_2020_25), "", sprintf("%.1f", r$baseline_2020_25)),
          fifelse(is.na(r$baseline_2020_25), "", sprintf("$%.1f\\%%$", 100*r$ATT/r$baseline_2020_25))) }
rowC <- function(k, lb){ r <- gc_(k)
  sprintf("\\quad %s & %s & %.1f & $%d\\%%$ \\\\", lb, cc(r$ATT,r$SE,r$p), r$baseline, as.integer(r$pct_of_base)) }
L1 <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Composition of the response by maternal education}",
"\\label{tab:composition}\\small",
"\\setlength{\\tabcolsep}{5pt}",
"\\begin{tabular}{lccc}",
"\\toprule",
" & Estimate (SE) [95\\% CI] & Baseline & \\% of baseline \\\\",
"\\midrule",
"\\multicolumn{4}{l}{\\textit{Birth rates by maternal education (per 1{,}000 women aged 15--19)}} \\\\[2pt]",
rowE("Below secondary (None/Primary)", "Less than secondary"),
rowE("Secondary or higher", "Secondary or more"),
rowE("DIFFERENTIAL (below-sec minus sec+)", "Difference: less than secondary $-$ secondary or more"),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
r"(\textit{Notes:} Each row reports a Callaway--Sant'Anna group ATT using the first-opening design, not-yet-treated controls, and municipio-clustered multiplier-bootstrap standard errors. Outcomes are birth rates per 1{,}000 women aged 15--19, split by the mother's registered education. Maternal education is observed beginning in 2020, so these estimates are identified from the shorter 2020--2025 panel and the reported baselines are treated-group means over that period. The final row reports the difference between the two education-group estimates. The estimates indicate declines in both education groups and do not reject equality across them. The analogous split by age bands affected by the 2021 child-marriage ban appears in \autoref{tab:shocks}, Panel C. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.)",
"\\end{minipage}",
"\\end{table}")
L1 <- gsub("municipio-clustered", "municipality-clustered", L1, fixed=TRUE)
writeLines(L1, file.path(TAB,"tab_a13_composition.tex"))

## ---- utilization --------------------------------------------------------------
mc <- fread(file.path(TAB,"mechanisms_cs.csv"))
gm <- function(pat){ r <- mc[grepl(pat, outcome)]; stopifnot(nrow(r)==1); r }
a <- gm("^\\(A\\)"); b <- gm("^\\(B\\)"); d <- gm("^\\(C\\)")
L2 <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Utilization signals from hospital-production registries}",
"\\label{tab:utilization}\\small",
"\\setlength{\\tabcolsep}{3.5pt}",
"\\begin{tabular}{lcc}",
"\\toprule",
" & Estimate (SE) [95\\% CI] & Opening cohorts \\\\",
"\\midrule",
sprintf("Prenatal consultations to adolescents per 1{,}000 women 15--19 & %s & 2021+ \\\\", cc(a$att,a$se,a$p,1)),
sprintf("Adolescent share of prenatal consultations (pp) & %s & 2021+ \\\\", cc(100*b$att,100*b$se,b$p)),
sprintf("Counseling and family-planning visits per 1{,}000 women 15--49 & %s & 2023+ \\\\", cc(d$att,d$se,d$p,1)),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
r"(\textit{Notes:} Each row reports a Callaway--Sant'Anna group ATT using outcomes constructed from the public-facility Form 67-A hospital-production registries and assigned to municipalities as in Section~\ref{sec:data-mechanisms}. Standard errors are municipio-clustered multiplier-bootstrap standard errors. The consultation series begin in 2020, and counseling/family-planning visits are observed only from 2022, so only the listed opening cohorts contribute to each estimate. The prenatal-consultation share is the most informative utilization margin because it is less sensitive than consultation levels to changes in the number of pregnancies and is available for more cohorts. The NHS confirmed in writing that no unit-level utilization records exist. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.)",
"\\end{minipage}",
"\\end{table}")
L2 <- gsub("municipio-clustered", "municipality-clustered", L2, fixed=TRUE)
writeLines(L2, file.path(TAB,"tab_a14_utilization.tex"))
cat("saved -> tab_a13_composition.tex + tab_a14_utilization.tex\n")
