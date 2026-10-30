# ============================================================================
# 10z4_sensitivity_table.R — appendix exhibit backing the sensitivity prose
# (2026-08-05, MM: every result the text refers to needs a table; the term
# "battery" is retired everywhere in favor of "sensitivity to alternative
# specifications, samples, and comparison groups").
# Source: robustness_S1_S2.csv (09_robustness.R), S1 rows. Level and DDD as
# columns; the contaminated comparison-age rows are kept, clearly labeled, as
# the exhibit behind "swapping the comparison age drives the DDD to zero".
# Output: tab_a09_sensitivity.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")
rb <- fread(file.path(TAB,"robustness_S1_S2.csv"))[scenario=="S1"]
# 2026-08-06: display the ONE canonical main estimate (headline_S1_S2.csv) in the
# reference row so the headline estimate is identical to Table 3 and the abstract.
.HL <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1"]
rb[spec=="A0 headline (not-yet)" & outcome=="rateA_15_19",
   `:=`(ATT=.HL[spec=="CS, no covs",estimate], SE=.HL[spec=="CS, no covs",SE], p=.HL[spec=="CS, no covs",p])]
# 2026-08-07 (MM): the DDD column uses the PROPER triplediff estimator throughout
# (09m_ddd_variants.R + the canonical headline DDD row), NOT the pre-differenced
# gap-CS. Anticipation has no triplediff analog (no anticipation argument) and
# drop-SD-metro is GMM-singular (thin cohorts) -> those DDD cells are "---".
.DV <- fread(file.path(TAB,"ddd_variants.csv"))
dvcell <- function(sp){ r <- .DV[spec==sp]; stopifnot(nrow(r)==1)
  if (is.na(r$ATT)) "---" else cell(r$ATT, r$SE, r$p) }
hdcell <- function(){ h <- .HL[spec=="DDD (no covs)"]; cell(h$estimate, h$SE, h$p) }
# 2026-08-05 additions (MM: fold dating variants, covariate vintage, and the
# spatial-spillover checks into this exhibit as labeled groups):
sl  <- fread(file.path(TAB,"spillover_locality.csv"))
dcz <- fread(file.path(TAB,"drop_constanza.csv"))
loo <- fread(file.path(TAB,"leave_one_out_treated.csv"))
c10 <- fread(file.path(TAB,"census2010_covariate_check.csv"))
v10 <- function(k) c10[stat==k, value]
# La Vega 2022 sensitivity: the §56.1 vintage headline (-5.90, SE 2.06, p=.004),
# recorded in methodology_decisions.md §56.1/§57 ("La-Vega-2022 sensitivity on file").
LAVEGA <- list(ATT=-5.90, SE=2.06, p=0.004)

stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
cell <- function(ATT,SE,p,d=2){ s<-stars(p); sup<-if(s=="")"" else sprintf("^{%s}",s)
  sprintf("\\makecell{$%.*f%s$ (%.*f) \\\\ {[$%.*f$, $%.*f$]}}", d, ATT, sup, d, SE, d, ATT-1.96*SE, d, ATT+1.96*SE) }
get <- function(sp, oc){ r <- rb[spec==sp & outcome==oc]; stopifnot(nrow(r)==1); r }
row2 <- function(sp, lb, dddtxt){
  l <- get(sp,"rateA_15_19")
  sprintf("%s & %s & %s \\\\", lb, cell(l$ATT,l$SE,l$p), dddtxt) }
rowL <- function(sp, lb, oc="rateA_15_19"){ l <- get(sp,oc)
  sprintf("%s & %s & --- \\\\", lb, cell(l$ATT,l$SE,l$p)) }
rowD <- function(lb, dv){
  sprintf("%s & --- & %s \\\\", lb, dvcell(dv)) }
den <- function(){ l <- get("B4 denominator B (level)","rateB_15_19")
  sprintf("Intercensal population denominators & %s & %s \\\\", cell(l$ATT,l$SE,l$p), dvcell("intercensal denominators (B)")) }
dsd <- function(){ l <- get("C1 drop Santo Domingo metro (level)","rateA_15_19")
  sprintf("Drop the Santo Domingo metropolitan area & %s & --- \\\\", cell(l$ATT,l$SE,l$p)) }

loorow <- function(){
  sprintf("Drop each treated municipality in turn & \\multicolumn{2}{c}{range $[%.2f, %.2f]$} \\\\",
          min(loo$ATT), max(loo$ATT))
}

L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Sensitivity to alternative specifications, samples, and comparison groups}",
"\\label{tab:sensitivity}\\footnotesize",
"\\renewcommand{\\arraystretch}{0.84}",
"\\setlength{\\tabcolsep}{4pt}",
"\\begin{tabular}{lcc}",
"\\toprule",
" & \\makecell{Level effect \\\\ Estimate (SE) [95\\% CI]} & \\makecell{Triple difference \\\\ Estimate (SE) [95\\% CI]} \\\\[2pt]",
"\\midrule",
row2("A0 headline (not-yet)",      "Main estimate (not-yet-treated comparisons)", hdcell()),
row2("A2 never-treated only",      "Never-treated comparisons only", dvcell("never-treated comparisons")),
row2("A1 population-weighted",     "Population-weighted", dvcell("population-weighted")),
row2("A3 anticipation 1yr",        "One year of anticipation allowed", "---"),
row2("A4 drop imputed cohorts",    "Drop the imputed-date municipality", dvcell("drop imputed-date municipalities")),
den(),
dsd(),
loorow(),
rowL("B1 log1p(rate)",             "Log specification (log points)", "log_rate"),
rowD("DDD comparison ages 20--24 (potentially exposed)", "comparison age 20-24"),
rowD("DDD comparison ages 25--29 (potentially exposed)", "comparison age 25-29"),
"\\addlinespace",
"\\multicolumn{3}{l}{\\textit{Treatment dating}} \\\\",
sprintf("La Vega recoded to 2022 & %s & --- \\\\", cell(LAVEGA$ATT, LAVEGA$SE, LAVEGA$p)),
{r <- dcz[1]; sprintf("Drop Constanza & %s & --- \\\\", cell(r$ATT, r$SE, r$p))},
"\\addlinespace",
"\\multicolumn{3}{l}{\\textit{Covariate vintage (covariate-adjusted specification)}} \\\\",
sprintf("Covariates from the 2022 census & %s & --- \\\\", cell(.HL[spec=="CS, +covs",estimate], .HL[spec=="CS, +covs",SE], .HL[spec=="CS, +covs",p])),   # 2026-09-30 audit: canonical +covs row (= Table 3)
sprintf("Covariates from the 2010 census & %s & --- \\\\", cell(v10("attcovs_2010"), v10("se_2010"), v10("p_2010"))),
"\\addlinespace",
"\\multicolumn{3}{l}{\\textit{Spatial spillovers}} \\\\",
{r <- sl[grepl("Neighbour", row)]; sprintf("Treatment by proximity (unit within 20 km) & %s & --- \\\\", cell(r$ATT, r$SE, r$p))},
{r <- sl[grepl("10 km", row)]; sprintf("Donut: drop controls within 10 km of a unit & %s & --- \\\\", cell(r$ATT, r$SE, r$p))},
{r <- sl[grepl("20 km", row) & grepl("DONUT", row)]; sprintf("Donut: drop controls within 20 km & %s & --- \\\\", cell(r$ATT, r$SE, r$p))},
{r <- sl[grepl("30 km", row)]; sprintf("Donut: drop controls within 30 km & %s & --- \\\\", cell(r$ATT, r$SE, r$p))},
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{2pt}\\scriptsize",
r"(\textit{Notes:} Each row changes one element of the first-opening design. Standard errors are municipality-clustered multiplier-bootstrap standard errors except in the leave-one-out row, which reports the range obtained by dropping each of the 20 treated municipalities in turn. Triple differences compare ages 15--19 with ages 30--34 using cohort-specific not-yet-treated comparisons combined by GMM; the 2019 cohort is omitted because it makes the GMM aggregation singular. The comparison-age rows deliberately use age groups that may themselves have been exposed, so their attenuation is expected. La Vega is dated to its documented 2023 reinauguration in the main analysis; the table also reports a 2022 coding. Spatial exclusions remove nearby controls and therefore address attenuation from cross-border use. Appendix~\autoref{tab:matching} reports the matching estimates. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.)",
"\\end{minipage}",
"\\end{table}")
L <- gsub("municipio-clustered", "municipality-clustered", L, fixed=TRUE)
L <- gsub("---", "N/A", L, fixed=TRUE)
writeLines(L, file.path(TAB,"tab_a09_sensitivity.tex"))
cat("saved -> tab_a09_sensitivity.tex\n")
