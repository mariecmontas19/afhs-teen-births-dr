# ============================================================================
# 08o8_table_age_robustness.R — FINAL paper robustness table (MM 2026-07-09):
# two columns = age at BIRTH vs age at CONCEPTION, rows = 15-19 headline +
# single ages 15..19 + subtotals 15-17 / 18-19. Numbers read from
# birth_vs_conception.csv (08o5: both classifications, same 2018-25 window,
# same common women-15-19 denominator, singles sum to totals). Sensitivity
# references in the notes from conception_table.csv (fixed-40wk -8.42) and
# conception_fullwindow.csv (2016-25 hybrid -8.04). Overwrites the earlier
# 08o4 version of tab_a07_conception.tex (kept: conception_table.csv).
# Output: output/tables/tab_a07_conception.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")

res <- fread(file.path(TAB,"birth_vs_conception.csv"))
hyb <- fread(file.path(TAB,"conception_fullwindow.csv"))
f40 <- fread(file.path(TAB,"conception_table.csv"))[variant=="B: fixed 40 weeks" & age=="15--19 (all)"]
stopifnot(nrow(hyb)==1, nrow(f40)==1)

stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
cl <- function(v,a){ r <- res[classification==v & age==a]; stopifnot(nrow(r)==1)
  s <- stars(r$p); sup <- if(s=="")"" else sprintf("^{%s}",s)
  sprintf("$%.2f%s$ (%.2f)", r$ATT, sup, r$SE) }
pc <- function(v,a) res[classification==v & age==a, sprintf("$-$%d\\%%", abs(pct))]
rr <- function(a, lab=a) sprintf("\\quad %s & %s & %s \\\\", lab, cl("Age at BIRTH",a), cl("Age at CONCEPTION",a))

L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Effect by mother's age: age at birth vs.\\ age at conception}",
"\\label{tab:conception}\\small",
"\\begin{tabular}{lcc}",
"\\toprule",
" & Age at birth & Age at conception \\\\",
"\\midrule",
sprintf("\\textbf{15--19 (main estimate)} & %s & %s \\\\", cl("Age at BIRTH","15--19"), cl("Age at CONCEPTION","15--19")),
sprintf("\\quad \\emph{\\%% of baseline} & \\emph{%s} & \\emph{%s} \\\\[3pt]", pc("Age at BIRTH","15--19"), pc("Age at CONCEPTION","15--19")),
"\\multicolumn{3}{l}{\\textit{Single years}} \\\\[2pt]",
rr("15"), rr("16"), rr("17"), rr("18"), rr("19"),
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Subtotals}} \\\\[2pt]",
rr("15--17"), rr("18--19"),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.95\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Callaway--Sant'Anna group ATT, first AU opening (20 treated municipalities),",
"not-yet-treated controls, municipio-clustered SEs in parentheses; panel years 2018--2025 for both",
"columns (the mother's date of birth, needed for age at conception, is recorded from 2018; the",
"age-at-birth main estimate is identical on 2016--2025 and 2018--2025, $-6.43$, as all openings occur",
"2020+). Age at conception $=$ age at (birth date $-$ recorded gestational age). Outcome $=$ births",
"to mothers of the given age per 1{,}000 women 15--19 (common denominator), so single-year rows sum",
"to the 15--19 total. Records with inconsistent date-of-birth/gestation combinations ($\\approx$2.4\\%)",
"excluded. The two classifications tell one story at different clocks: conceptions are prevented from",
"age 17 up, which appears as fewer births at 18--19. Sensitivity: dating conception by a fixed 40",
sprintf("weeks instead of recorded gestation gives $%.2f$; extending to 2016--2025 by allocating pre-2018", f40$ATT),
sprintf("births in expectation by their actual gestational age gives $%.2f$ ($-$%.0f\\%%).", hyb$ATT, abs(hyb$pct)),
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
L <- gsub("municipio-clustered", "municipality-clustered", L, fixed=TRUE)
writeLines(L, file.path(TAB,"tab_a07_conception.tex"))
cat("saved -> tab_a07_conception.tex (", length(L), "lines )\n")
