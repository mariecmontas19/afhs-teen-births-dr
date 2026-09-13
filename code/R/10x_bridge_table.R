# ============================================================================
# 10x_bridge_table.R — formatter: bridge_cs.csv -> tab_05_preg_sns.tex (§59.17).
# Registration-independent confirmation + sector decomposition + abortion
# margin table. Stars: *** p<.01, ** p<.05, * p<.10 (project convention).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")
r <- fread(file.path(TAB,"bridge_cs.csv"))
stars <- function(p) fifelse(p<.01,"^{***}", fifelse(p<.05,"^{**}", fifelse(p<.1,"^{*}","")))
r[, row := sprintf("%s & \\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}} & %.2f & $%+.1f\\%%$ & $%+.2f$ \\\\",
   c("Births, mother's residence (main estimate)",
     "Births, delivery municipality, all facilities",
     "\\quad public facilities",
     "\\quad private/other facilities (substitution)",
     "Pregnancy events, public facilities",
     "Abortion-related attendances, public facilities",
     "Stillbirths, public facilities (descriptive)$^{\\dagger}$"),
   att, stars(p), se, att-1.96*se, att+1.96*se, base_gm1, pct, pre_avg)]
tex <- c(
"\\begin{table}[H]\\centering",
"\\caption{Effects of AU openings on births and pregnancy-related events}",
"\\label{tab:bridge}\\scriptsize",
"\\setlength{\\tabcolsep}{2pt}",
"\\begin{tabular}{lcccc}",
"\\toprule",
" & Estimate (SE) [95\\% CI] & Baseline ($g{-}1$) & \\% & Avg. pretrend \\\\",
"\\midrule",
"\\multicolumn{5}{l}{\\textit{Civil registry (BDNV), 2016--2025}} \\\\",
r$row[1:4],
"\\addlinespace",
"\\multicolumn{5}{l}{\\textit{Hospital-production registries (Form 67-A), 2015--2025}} \\\\",
r$row[5:6],
r$row[7],
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.92\\linewidth}\\vspace{4pt}\\scriptsize",
"\\textit{Notes:} Callaway--Sant'Anna group ATTs, identical specification across rows (first AU",
"opening, not-yet-treated controls, universal base period, municipality-clustered SEs in parentheses,",
"95\\% confidence intervals in brackets);",
"outcomes per 1{,}000 women 15--19 (denominator A); \\% relative to the treated mean in the year",
"before opening ($g{-}1$). ``Avg. pretrend'' is the precision-weighted average of the dynamic",
"pre-treatment coefficients ($e \\in [-5,-2]$); no lead is individually significant for any",
"outcome. Facility outcomes assign events to the municipality of the attending",
"facility (public-sector Form 67-A registries; validated against NSO province aggregates, corr",
"$0.99$, and BDNV, corr $0.96$). Abortion-related attendances include spontaneous losses, induced",
"abortions, and post-abortion care. All rows exclude Nizao (145 municipalities),",
"whose municipal hospital roughly quadrupled obstetric volume in 2024 (deliveries to women 20+",
"rose from 85 to 353; confirmed in the civil registry), a facility-supply change that",
"mechanically inflates facility-based rates; the main estimates elsewhere in the paper use the",
"full sample. $^{\\dagger}$Stillbirths are recorded only from",
"2022, leaving pre-periods for the 2023+ cohorts only; the estimate fails the pre-trend diagnostic",
"and is shown only as descriptive context; the raw decline is $-36\\%$ from 2022 to 2025.",
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
writeLines(tex, file.path(TAB,"tab_05_preg_sns.tex"))
cat("saved -> tab_05_preg_sns.tex (", length(tex), "lines )\n")
