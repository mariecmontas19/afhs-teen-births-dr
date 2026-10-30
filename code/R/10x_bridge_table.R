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
   c("Births, mother's residence (common sample)",
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
"\\label{tab:bridge}\\footnotesize",
"\\setlength{\\tabcolsep}{3.5pt}",
"\\renewcommand{\\arraystretch}{1.05}",
"\\begin{tabular}{lcccc}",
"\\toprule",
" & \\makecell{Estimate (SE) \\\\ {[95\\% CI]}} & \\makecell{Baseline \\\\ ($g{-}1$)} & \\% & \\makecell{Average \\\\ lead} \\\\",
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
"\\textit{Notes:} Each row reports a Callaway--Sant'Anna group-aggregated ATT using the first-opening design,",
"not-yet-treated controls, a universal base period, and municipality-clustered standard errors.",
"Standard errors are in parentheses and 95\\% confidence intervals are in brackets. Outcomes are",
"measured per 1{,}000 women aged 15--19; the percentage column expresses the estimate relative to",
"the treated-group mean in the year before opening ($g{-}1$). ``Average lead'' is the",
"inverse-variance-weighted mean of the dynamic pre-treatment coefficients for $e \\in [-5,-2]$.",
"No pre-treatment coefficient is individually significant for the birth, pregnancy-event, or",
"abortion-attendance outcomes. Residence-based births are assigned to the mother's municipality of",
"residence; all other outcomes are assigned to the municipality of the attending facility.",
"Public-facility Form 67-A totals correlate with National Statistics Office province aggregates",
"at $0.99$ and with BDNV",
"at $0.96$. Abortion-related attendances include spontaneous pregnancy losses and care following",
"induced abortions; they do not measure all induced abortions. All rows use the common sample of",
"145 municipalities, excluding Nizao because its municipal hospital experienced a sharp, unrelated",
"increase in obstetric volume in 2024 (deliveries to women aged 20 and older rose from 85 to 353).",
"The main estimates elsewhere use all 146 municipalities. $^{\\dagger}$Stillbirths are available",
"only from 2022, leaving pre-periods only for the 2023 and later cohorts. Because the stillbirth",
"series fails the pre-treatment diagnostic, its estimate is descriptive and is not interpreted causally.",
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
writeLines(tex, file.path(TAB,"tab_05_preg_sns.tex"))
cat("saved -> tab_05_preg_sns.tex (", length(tex), "lines )\n")
