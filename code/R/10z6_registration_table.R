# ============================================================================
# 10z6_registration_table.R — appendix exhibit for the measurement subsection
# (2026-08-05). Panel A: registration-ramp robustness (full window vs 2019+;
# ramp_robustness.csv, re-run same day with the headline 5-covariate spec so
# the +covs row matches Table 3's -5.66). Panel B: the compositional shifts
# disclosed as a limitation + the bounding facts (undercount_battery.csv).
# Output: tab_a12_registration.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")

rr <- fread(file.path(TAB,"ramp_robustness.csv"))
.HL <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1"]  # 2026-08-06: canonical full-window main rows
rr[spec=="S1 CS level, no covs", `:=`(full_ATT=.HL[spec=="CS, no covs",estimate], full_SE=.HL[spec=="CS, no covs",SE], full_p=.HL[spec=="CS, no covs",p])]
rr[spec=="S1 CS level, +covs",   `:=`(full_ATT=.HL[spec=="CS, +covs",estimate],   full_SE=.HL[spec=="CS, +covs",SE],   full_p=.HL[spec=="CS, +covs",p])]
ub <- fread(file.path(TAB,"undercount_battery.csv"))
gA <- function(sp){ r <- rr[spec==sp]; stopifnot(nrow(r)==1); r }
gB <- function(sp){ r <- ub[spec==sp]; stopifnot(nrow(r)==1); r }
stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
cc <- function(est,se,p){ s<-stars(p); sup<-if(s=="")"" else sprintf("^{%s}",s)
  sprintf("\\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}}", est, sup, se, est-1.96*se, est+1.96*se) }
rowA <- function(sp, lb){ r <- gA(sp)
  sprintf("\\quad %s & %s & %s \\\\", lb, cc(r$full_ATT, r$full_SE, r$full_p),
          cc(r$post2019_ATT, r$post2019_SE, r$post2019_p)) }
rowB <- function(sp, lb){ r <- gB(sp)
  sprintf("\\quad %s & \\multicolumn{2}{c}{%s} \\\\", lb, cc(r$ATT, r$SE, r$p)) }
# 2026-08-10 (MM): leave-one-out ranges into the exhibit (claim 2 of the prose's
# three bounding facts; was computed in 09g block B but never typeset). The CSV
# stores min in ATT and "max %.2f; top mover PCODE" in note — 20 point estimates,
# so a RANGE, no SE.
rowLOO <- function(sp, lb){ r <- gB(sp)
  mx <- as.numeric(sub("max ([-0-9.]+);.*", "\\1", r$note))
  sprintf("\\quad\\quad %s & \\multicolumn{2}{c}{range $[%.2f$, $%.2f]$} \\\\", lb, r$ATT, mx) }

L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Registration completeness and birth composition}",
"\\label{tab:registration}\\small",
"\\setlength{\\tabcolsep}{3.5pt}",
"\\begin{tabular}{lcc}",
"\\toprule",
"\\multicolumn{3}{l}{\\textit{Panel A. Dropping the registration-ramp years (2016--2018)}} \\\\[2pt]",
" & Full window 2016--2025 & 2019--2025 only \\\\",
"\\midrule",
rowA("S1 CS level, no covs", "Level"),
rowA("S1 CS level, +covs",   "Level, covariate-adjusted"),
rowA("S1 DDD (no covs)",     "Triple difference"),
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Panel B. Compositional shifts after openings and their bounds}} \\\\[2pt]",
rowB("pubfac share, ALL births",      "\\% public-facility births, all births (pp)"),
rowB("pubfac share, NON-TEEN 25-44",  "\\% public-facility births, mothers 25--44 (pp)"),
rowLOO("pct_pubfac LOO range",        "leave-one-out, dropping each treated municipality"),
rowB("Haitian share, ALL births",     "\\% Haitian mothers, all births (pp)"),
rowB("Haitian share, NON-TEEN 25-44", "\\% Haitian mothers, mothers 25--44 (pp)"),
rowLOO("pct_haitian LOO range",       "leave-one-out, dropping each treated municipality"),
rowB("CS + baseline pubfac/Haitian shares", "Teen rate, conditioning on baseline shares"),
rowB("TWFE + contemporaneous shares", "Teen rate, conditioning on contemporaneous shares$^{c}$"),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
r"(\textit{Notes:} Panel A re-estimates the main design on the 2019--2025 window only. Because every first opening occurs in 2020 or later, the 2016--2018 registration ramp is entirely pre-treatment, so dropping those years leaves the estimates unchanged apart from trivial differences in standard errors. Panel B reports the two compositional shifts that accompany openings and the associated bounding exercises. Both shifts also appear among births to mothers aged 25--44, indicating that they reflect broader municipality-level changes in where births are recorded and which mothers appear in the registry, rather than teen-specific responses to the program. The leave-one-out rows re-estimate each shift dropping one treated municipality at a time (all-births specification); both ranges remain negative, so neither pattern is driven by a single municipality (the largest single-municipality movers are San Crist\'obal for the facility share and Santo Domingo Norte for the Haitian share). Conditioning on the baseline values of the two shares strengthens the teen estimate. Conditioning on their contemporaneous values instead ($^{c}$) conditions on post-treatment outcomes and is therefore reported only as a bad-control bound. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.)",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_a12_registration.tex"))
cat("saved -> tab_a12_registration.tex\n")
