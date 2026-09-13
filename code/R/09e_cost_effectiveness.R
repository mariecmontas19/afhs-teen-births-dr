# ============================================================================
# 09e_cost_effectiveness.R — threshold/break-even cost-effectiveness of AU
# openings (H1 ONLY: the effect of opening a NEW unit). No AU budget is public,
# so we compute the VALUE of the averted births and the break-even budget ceiling,
# plus external benchmarks. Every external input is a verified, cited constant:
#   MILENA RD (UNFPA, July 2021 — verified on the PDF title page; ISBN 978-9945-015-46-1; data year 2018):
#     total opportunity cost US$245M (0.29% GDP); health US$21M; ÷ 28,791
#     adolescent births (SNS 2019, cited in MILENA prologue) -> per-birth values.
#   UNFPA-INTEC 2013 (ISBN 978-9945-015-07-2): RD$32,419/public-hospital case
#     (~US$700) cross-validates the health per-birth; "treatment = 33x prevention".
#   Guttmacher Adding It Up 2019: $3 saved per $1 on contraceptive services.
#   US CPI-U 2018->2025 factor 1.27 (BLS: 251.1 -> ~319; 2025 PRELIMINARY).
#   WB SP.REG.BRTH.ZS: DR birth-registration completeness 92.2% (2019) -> averted
#     births (registration-based) are a CONSERVATIVE LOWER BOUND.
# Births averted + CI from births_averted.csv (09d). Municipality-post-years
# computed from treatment_municipio.rds. Both 2018 US$ and 2025 US$ columns.
# Output: output/tables/cost_effectiveness.csv + tab_08_cost_effectiveness.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables"); fold <- c("DOM010905","DOM051703","DOM012510")

# ---- verified external constants (sources above) ----
MIL_TOTAL <- 245e6; MIL_HEALTH <- 21e6; ADOL_BIRTHS <- 28791   # MILENA RD 2022 / SNS 2019
CPI <- 321.943/251.107                                          # BLS CPI-U annual averages, 2018->2025
GUTT <- "3:1"                                                   # Guttmacher 2019 ROI

# ---- in-house inputs ----
ba <- fread(file.path(TAB,"births_averted.csv"))[scenario=="S1"]
stopifnot(nrow(ba)==1)
HL <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1" & spec=="CS, no covs"]
HLATT <- HL$estimate; HLSE <- HL$SE   # dynamic headline for the table notes
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
h1 <- tr[ever_treated==1L & always_treated==0L]
muniyrs <- h1[, sum(2025L - first_year + 1L)]
cat(sprintf("H1: averted=%d [%d-%d] | exposed=%s | municipality-post-years=%d\n",
    ba$births_averted, ba$averted_lo, ba$averted_hi, format(ba$exposed_women_years,big.mark=","), muniyrs))

# ---- per-birth values and derived quantities ----
nar18 <- MIL_HEALTH/ADOL_BIRTHS; brd18 <- MIL_TOTAL/ADOL_BIRTHS
rows <- rbindlist(list(
  data.table(item="Teen births averted, 2016-25 [95% CI]", v2018=sprintf("%s [%s-%s]", format(ba$births_averted,big.mark=","), format(ba$averted_lo,big.mark=","), format(ba$averted_hi,big.mark=",")), v2025="", src="this paper (Table 4); registration-based -> lower bound"),
  data.table(item="Exposed teen-women-years (treated, post)", v2018=format(ba$exposed_women_years,big.mark=","), v2025="", src="this paper"),
  data.table(item="Treated municipality-post-years", v2018=as.character(muniyrs), v2025="", src="this paper"),
  data.table(item="Value per averted birth: health only (USD)", v2018=sprintf("%.0f",nar18), v2025=sprintf("%.0f",nar18*CPI), src="MILENA (UNFPA 2021): US$21M / 28,791"),
  data.table(item="Value per averted birth: total opportunity cost (USD)", v2018=format(round(brd18),big.mark=","), v2025=format(round(brd18*CPI),big.mark=","), src="MILENA (UNFPA 2021): US$245M / 28,791"),
  data.table(item="Implied benefit: health only (USD)", v2018=format(round(ba$births_averted*nar18),big.mark=","), v2025=format(round(ba$births_averted*nar18*CPI),big.mark=","), src="computed"),
  data.table(item="Implied benefit: total opportunity cost (USD)", v2018=format(round(ba$births_averted*brd18),big.mark=","), v2025=format(round(ba$births_averted*brd18*CPI),big.mark=","), src="computed"),
  data.table(item="  {[}95% CI, total, 2018 USD{]}", v2018=sprintf("%s - %s", format(round(ba$averted_lo*brd18),big.mark=","), format(round(ba$averted_hi*brd18),big.mark=",")), v2025="", src="computed from ATT CI"),
  data.table(item="Break-even cost per municipality-year: health only (USD)", v2018=format(round(ba$births_averted*nar18/muniyrs),big.mark=","), v2025=format(round(ba$births_averted*nar18*CPI/muniyrs),big.mark=","), src="computed"),
  data.table(item="Break-even cost per municipality-year: total (USD)", v2018=format(round(ba$births_averted*brd18/muniyrs),big.mark=","), v2025=format(round(ba$births_averted*brd18*CPI/muniyrs),big.mark=","), src="computed"),
  data.table(item="National opportunity cost of adolescent pregnancy", v2018="245M/yr (0.29% GDP)", v2025="", src="MILENA (UNFPA 2021)"),
  data.table(item="Return per $1 on contraceptive services (LMIC)", v2018=GUTT, v2025="", src="Guttmacher, Adding It Up 2019"),
  data.table(item="Treating adolescent pregnancy vs prevention plan", v2018="33x", v2025="", src="UNFPA-INTEC 2013")
))
print(rows, class=FALSE)
fwrite(rows, file.path(TAB,"cost_effectiveness.csv"))

# ---- LaTeX ----
esc <- function(x) gsub("%","\\\\%", gsub("\\$","\\\\$", x))
tex_rows <- sprintf("%s & %s & %s \\\\", esc(rows$item), esc(rows$v2018), esc(rows$v2025))
L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Implied benefits and break-even cost thresholds}",
"\\label{tab:costeff}\\small",
"\\setlength{\\tabcolsep}{4pt}",
"\\begin{tabular}{lcc}",
"\\toprule",
" & 2018 US\\$ & 2025 US\\$ \\\\",
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Panel A. Effect (this paper)}} \\\\[2pt]",
tex_rows[1:3],
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Panel B. Value per averted adolescent birth (UNFPA MILENA, 2021)}} \\\\[2pt]",
tex_rows[4:5],
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Panel C. Implied benefits and break-even thresholds}} \\\\[2pt]",
tex_rows[6:10],
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
sprintf("\\textit{Notes:} Treatment is a municipality's first AU opening (20 municipalities). Births averted $=$ Callaway--Sant'Anna ATT ($%.2f$, SE %.2f)", HLATT, HLSE),
"$\\times$ exposed teen-women-years; brackets $=$ 95\\% CI propagated from the ATT. Registration-based birth",
"counts undercount true births (completeness 92.2\\% in 2019, World Bank SP.REG.BRTH.ZS). If completeness is",
"unaffected by treatment, averted births and associated benefits are conservative. Per-birth values divide MILENA's annual national totals",
"(UNFPA 2021, data year 2018) by the 28{,}791 adolescent births reported for 2019, an annual-flow",
"approximation. `Total opportunity cost' adds foregone education, labor-force participation, earnings, and fiscal",
"revenue to health spending. 2025 US\\$ uses the ratio of the BLS CPI-U annual averages for 2025 (321.943)",
"and 2018 (251.107). The break-even threshold is the maximum cost per treated municipality-year at which",
"valued benefits equal costs. The calculation applies the average ATT to all post-treatment years even though",
"the effect builds with exposure. It is a threshold exercise, not a cost-effectiveness ratio; denominator A.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_08_cost_effectiveness.tex"))
cat("\nsaved -> cost_effectiveness.csv + tab_08_cost_effectiveness.tex\n")
