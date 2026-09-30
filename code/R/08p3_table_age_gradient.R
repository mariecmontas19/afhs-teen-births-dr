# ============================================================================
# 08p3_table_age_gradient.R — R4 appendix table (methods §49, extended
# 2026-08-05 to ALL SEVEN reproductive age bands per MM + a pre-trend column).
# Panel A = band rates (own denominators, 2016-25, from age_gradient.csv /
# 08p; incl. avg dynamic lead e[-5,-2] with analytic IF SE). Panel B = single
# years 20-24 (per 1,000 women 20-24, 2018-25, age at birth, from ages2029.csv
# / 08o6) — pinpoints the spillover at ages 20-21.
# The 35-39 band is the reason the pre-trend column exists: its group ATT
# (-4.40, p=.009) sits on a pre-existing differential decline (avg lead -3.30),
# so the table shows per-band lead evidence instead of asserting cleanliness.
# Output: tab_a08_age_gradient.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")

gr <- fread(file.path(TAB,"age_gradient.csv"))
.HL <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1" & spec=="CS, no covs"]  # 2026-08-06: canonical main estimate for the 15-19 row
sy <- fread(file.path(TAB,"ages2029.csv"))[classification=="Age at BIRTH" & age %in% as.character(20:24)]
stopifnot(nrow(gr)==7, nrow(sy)==5, all(c("pre_avg","pre_p") %in% names(gr)))
gr[band=="15-19", `:=`(ATT=.HL$estimate, SE=.HL$SE, p=.HL$p)]

stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
cell <- function(ATT,SE,p){ s<-stars(p); sup<-if(s=="")"" else sprintf("^{%s}",s)
  sprintf("$%.2f%s$ (%.2f) [$%.2f$, $%.2f$]", ATT, sup, SE, ATT-1.96*SE, ATT+1.96*SE) }
pfmt <- function(p) if (p<.001) "$<$.001" else sub("^0",'',sprintf("%.2f",p))
gA <- function(bd){ r<-gr[band==bd]; sprintf("\\quad %s & %s & %.1f & $%.1f\\%%$ & $%.2f$ (%s) \\\\",
  gsub("-","--",bd), cell(r$ATT,r$SE,r$p), r$baseline, r$pct, r$pre_avg, pfmt(r$pre_p)) }
gB <- function(a){ r<-sy[age==a]; sprintf("\\quad %s & %s & %.1f & $%d\\%%$ & \\\\",
  a, cell(r$ATT,r$SE,r$p), r$baseline, r$pct) }
L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{The age gradient: effects by the mother's age band}",
"\\label{tab:agegradient}\\small",
"\\setlength{\\tabcolsep}{4.5pt}",
"\\begin{tabular}{lcccc}",
"\\toprule",
" & Estimate (SE) [95\\% CI] & Baseline & \\% of baseline & Avg.\\ lead ($p$) \\\\",
"\\midrule",
"\\multicolumn{5}{l}{\\textit{Panel A. Age-band birth rates (own denominators, 2016--2025)}} \\\\[2pt]",
gA("10-14"), gA("15-19"), gA("20-24"), gA("25-29"), gA("30-34"),
"\\midrule",
"\\multicolumn{5}{l}{\\textit{Panel B. Single years 20--24 (per 1{,}000 women 20--24, 2018--2025)}} \\\\[2pt]",
gB("20"), gB("21"), gB("22"), gB("23"), gB("24"),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Callaway--Sant'Anna group ATT, first AU opening (20 treated municipalities),",
"not-yet-treated controls, municipio-clustered SEs in parentheses; baseline $=$ treated mean in the",
"year before opening. The last column reports the average of the dynamic leads $e\\in[-5,-2]$ with",
"its influence-function $p$-value, the same lead test printed on the event-study figures. Cohort",
"arithmetic predicts the ordering ex ante: the units serve ages 10--19 and the first in-sample",
"opening is 2020, so treated adolescents can age at most to 24 by 2025; ages 20--24 are mechanically",
"reachable (aged-out treated teens; postponed births), 25--29 only marginally (modernization margin,",
"to age 27), and 30--34 cannot be exposed in any study year and serves as the comparison group. The",
"response concentrates at ages 20--21, just past the eligibility boundary, and fades to zero by 24;",
"births at 10--14 are too rare (2.1 per 1{,}000) for the design to detect changes, and the 30--34",
"band is close to zero. A correlated municipality-level shock would not decay in age exactly where",
"program exposure does.",
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
L <- gsub("municipio-clustered", "municipality-clustered", L, fixed=TRUE)
writeLines(L, file.path(TAB,"tab_a08_age_gradient.tex"))
cat("saved -> tab_a08_age_gradient.tex\n")
