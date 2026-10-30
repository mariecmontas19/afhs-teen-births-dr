# ============================================================================
# 10x_denominator_panel.R — formats the denominator robustness table from the
# CSVs written by 09c3_denominator_final.R (nothing re-estimated here).
# Format convention (MM 2026-07-31): every estimate travels as one column,
# "estimate (SE) [95% CI]", with a separate p column.
# Output: output/tables/tab_a19_denominator.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- DIR_TABLES

pa <- fread(file.path(TAB,"denominator_panelA.csv"))
.HL <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1" & spec=="CS, no covs"]  # 2026-08-06: canonical
.RB <- fread(file.path(TAB,"robustness_S1_S2.csv"))[
  scenario=="S1" & spec=="B4 denominator B (level)" & outcome=="rateB_15_19"]
stopifnot(nrow(.RB)==1L)
pa[series=="A" & estimand=="level",
   `:=`(ATT=.HL$estimate, SE=.HL$SE, p=.HL$p, lo=.HL$estimate-1.96*.HL$SE, hi=.HL$estimate+1.96*.HL$SE)]
# Keep the intercensal row identical to the canonical robustness result used in
# Table 3. The raw panel file comes from a separate bootstrap stream.
pa[series=="B" & estimand=="level",
   `:=`(ATT=.RB$ATT, SE=.RB$SE, p=.RB$p, lo=.RB$ATT-1.96*.RB$SE, hi=.RB$ATT+1.96*.RB$SE)]
# 2026-08-07 (MM): triple differences are the PROPER triplediff estimator, not the
# pre-differenced gap-CS: series A from the canonical headline DDD row; series B
# from ddd_variants.csv (09m, triplediff on the intercensal rates).
.HD <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1" & spec=="DDD (no covs)"]
pa[series=="A" & estimand=="ddd",
   `:=`(ATT=.HD$estimate, SE=.HD$SE, p=.HD$p, lo=.HD$estimate-1.96*.HD$SE, hi=.HD$estimate+1.96*.HD$SE)]
.DV <- fread(file.path(TAB,"ddd_variants.csv"))[spec=="intercensal denominators (B)"]
pa[series=="B" & estimand=="ddd",
   `:=`(ATT=.DV$ATT, SE=.DV$SE, p=.DV$p, lo=.DV$ATT-1.96*.DV$SE, hi=.DV$ATT+1.96*.DV$SE)]
pb <- fread(file.path(TAB,"denominator_panelB.csv"))
pv <- fread(file.path(TAB,"projection_validation.csv"))   # 2026-08-06: back-test MAPE, reproducible (02b)
mape_nat <- 100*pv[stat=="mape_national_agesex", value]; mape_mun <- 100*pv[stat=="mape_municipio_cells", value]
pb[row=="0%", c("ATT","SE","p","lo","hi") := .(.HL$estimate, .HL$SE, .HL$p, .HL$estimate-1.96*.HL$SE, .HL$estimate+1.96*.HL$SE)]  # 2026-08-06: ladder anchor = canonical main estimate
stopifnot(nrow(pa)==4L, nrow(pb)==7L)

stars <- function(p) fifelse(p<.01,"^{***}", fifelse(p<.05,"^{**}", fifelse(p<.10,"^{*}","")))
cell  <- function(r) sprintf("\\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}}", r$ATT, stars(r$p), r$SE, r$lo, r$hi)

rA <- function(s, est, lab){
  r <- pa[series==s & estimand==est]
  pc <- if (est=="level") sprintf("$%.1f$", r$pct_base) else "N/A"
  sprintf("%s & %s & %s \\\\", lab, cell(r), pc) }
rB <- function(rw, lab){
  r <- pb[row==rw]
  sprintf("%s & %s & \\\\", lab, cell(r)) }

L <- c("\\begin{table}[H]\\centering",
"\\caption{Sensitivity to the population denominator}",
"\\label{tab:denominator}\\small",
"\\setlength{\\tabcolsep}{4pt}",
"\\begin{tabular}{lcc}",
"\\toprule",
" & Estimate (SE) [95\\% CI] & \\% of baseline \\\\",
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Panel A. Alternative denominator constructions}} \\\\[2pt]",
"\\multicolumn{3}{l}{A: Hamilton--Perry projection anchored on NSO estimates (main series)} \\\\",
rA("A","level","\\quad Teen birth rate"),
rA("A","ddd","\\quad Triple difference"),
"\\multicolumn{3}{l}{B: Intercensal series based on the 2010 and 2022 censuses} \\\\",
rA("B","level","\\quad Teen birth rate"),
rA("B","ddd","\\quad Triple difference"),
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Panel B. Differential post-treatment denominator error}} \\\\[2pt]",
rB("0%","\\quad Over-projection $\\delta = 0\\%$ (main estimate)"),
rB("3%","\\quad $\\delta = 3\\%$"),
rB("6%","\\quad $\\delta = 6\\%$"),
rB("9%","\\quad $\\delta = 9\\%$"),
rB("12%","\\quad $\\delta = 12\\%$"),
rB("15%","\\quad $\\delta = 15\\%$"),
"\\addlinespace",
rB("placebo_drift_AB","\\quad Realized differential drift, A vs.\\ B (\\%)"),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
r"(\textit{Notes:} Each cell reports a Callaway--Sant'Anna group-aggregated ATT for the first AU opening using not-yet-treated controls, a universal base period, and municipality-clustered multiplier-bootstrap standard errors. Parentheses contain standard errors and brackets contain 95\% confidence intervals. NSO publishes municipal population estimates only through 2020, so post-2020 denominators must be projected. Series A is the paper's main Hamilton--Perry projection and does not use the 2022 census. Series B is an intercensal construction based on the 2010 and 2022 censuses, with NSO province growth rates used between census years. A back-test against the held-out 2022 census indicates that Series A over-projects women aged 15--19 by about 10 percent. Removing a uniform bias rescales the level estimate and its baseline together, leaving the implied percentage effect essentially unchanged. The smaller triple-difference estimate under Series B reflects differential changes in the constructed 15--19 and 30--34 denominators during the pre-treatment period. Panel B rescales the treated post-opening rate by $1+\delta$, which is algebraically equivalent to differential over-projection of $\delta$ in the treated post-opening denominator. The final row applies the same design to the ratio of Series A to Series B and therefore measures the differential drift present across the two constructions. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.)",
"\\end{minipage}",
"\\end{table}")
L <- gsub("municipio-clustered", "municipality-clustered", L, fixed=TRUE)
writeLines(L, file.path(TAB,"tab_a19_denominator.tex"))
cat("saved -> tab_a19_denominator.tex\n")
