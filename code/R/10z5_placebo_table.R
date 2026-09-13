# ============================================================================
# 10z5_placebo_table.R — appendix exhibit for Strategy §4.5 (2026-08-05, MM:
# every cited number gets an exhibit; form chosen = table with an RR panel,
# since the leads themselves are already graphed on every event-study figure).
# Panel A: the two orphan placebos (fake-timing; IF average of pre-treatment
#   cells) from placebo_wcb.csv + robustness_S1_S2.csv block D.
# Panel B: Rambachan-Roth (C-LF) confidence sets for the average post-treatment
#   effect at Mbar = 0/0.1/0.2/0.3, level + DDD, from honestdid_S1_S2.csv.
# Output: tab_a04_placebos.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")

pw <- fread(file.path(TAB,"placebo_wcb.csv"))
ft <- pw[grepl("^CS fake-timing", spec)]; stopifnot(nrow(ft)==1)
rb <- fread(file.path(TAB,"robustness_S1_S2.csv"))[scenario=="S1" & block=="D"]
dl <- rb[outcome=="rateA_15_19"]; dd <- rb[outcome=="ddd_A"]; stopifnot(nrow(dl)==1, nrow(dd)==1)
hd <- fread(file.path(TAB,"honestdid_S1_S2.csv"))
hL <- hd[outcome=="S1 level 15-19" & Mbar %in% c(0,.1,.2,.3)]
hD <- hd[outcome=="S1 age-DDD"     & Mbar %in% c(0,.1,.2,.3)]
stopifnot(nrow(hL)==4, nrow(hD)==4)

cellP <- function(est,se,p) sprintf("\\makecell{$%.2f$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}} & %.2f", est, se, est-1.96*se, est+1.96*se, p)
ci <- function(h,m){ r <- h[Mbar==m]
  sprintf("[$%.2f$, $%.2f$]%s", r$lb, r$ub, fifelse(r$includes0=="TRUE" | r$includes0==TRUE, "", "")) }
mark <- function(h,m){ r <- h[Mbar==m]; if (r$includes0==TRUE | r$includes0=="TRUE") "" else "$^{\\dagger}$" }
rowRR <- function(m){
  lab <- if (m==0) "$\\bar{M}=0$ (parallel trends exact)" else sprintf("$\\bar{M}=%.1f$", m)
  sprintf("\\quad %s & %s%s & %s%s \\\\", lab, ci(hL,m), mark(hL,m), ci(hD,m), mark(hD,m)) }

L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Placebo tests and sensitivity to parallel-trends violations}",
"\\label{tab:placebos}\\small",
"\\setlength{\\tabcolsep}{5pt}",
"\\begin{tabular}{lcc}",
"\\toprule",
"\\multicolumn{3}{l}{\\textit{Panel A. Placebo tests}} \\\\[2pt]",
" & Estimate (SE) [95\\% CI] & $p$ \\\\",
"\\midrule",
sprintf("Fake-timing placebo (openings recoded 3 years early)$^{a}$ & %s \\\\", cellP(ft$estimate, ft$se, ft$p)),
sprintf("Average pre-treatment cell, level$^{b}$ & %s \\\\", cellP(dl$ATT, dl$SE, dl$p)),
sprintf("Average pre-treatment cell, triple difference$^{b}$ & %s \\\\", cellP(dd$ATT, dd$SE, dd$p)),
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Panel B. Rambachan--Roth confidence sets for the average post-treatment effect}} \\\\[2pt]",
" & Level (15--19 rate) & Triple difference \\\\",
"\\midrule",
rowRR(0), rowRR(0.1), rowRR(0.2), rowRR(0.3),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} $^{a}$Each cohort is assigned a counterfeit opening three years before its true one",
"and the design is re-estimated on 2016--2019 data only, before any true opening; a null says the",
"design does not manufacture effects from trends alone. $^{b}$Influence-function average of the raw",
"pre-treatment $ATT(g,t)$ cells, the complement to the dynamic-lead averages printed on the",
"event-study figures; both should be (and are) indistinguishable from zero. Placebos on the birth",
"rates of unexposable age groups appear in Appendix \\autoref{tab:agegradient}. Panel B reports",
"\\citet{rambachanroth2023} C-LF confidence sets allowing post-treatment parallel-trends violations up",
"to $\\bar{M}$ times the largest pre-treatment violation; $^{\\dagger}$marks sets that exclude zero.",
"The level estimate survives violations up to 10 percent of the maximal pre-treatment trend and the",
"triple difference, with its flatter pre-period, up to 20 percent. The pre-period is estimated on",
"twenty treated clusters, so this yardstick is itself noisy; the identification case rests on the",
"convergence of leads, placebos, timing, and the age gradient rather than on any single bound.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_a04_placebos.tex"))
cat("saved -> tab_a04_placebos.tex\n")
