# ============================================================================
# 10_table3_estimates.R — MAIN Table 3 (Proposal B: CS only) as LaTeX/booktabs.
# Single panel, all Callaway-Sant'Anna; rows name the spec (no estimator acronym):
#   "Effect on teen births per 1,000" (level)  /  + controls  /  triple-difference.
# Columns = H1 (first opening) and H2 (incl. modernization). Bottom block reports
# baseline mean, treated munis, clusters, and observations (computed inline from
# the panel for reproducibility, not hardcoded). Estimator comparison + dynamics
# live in the APPENDIX (10_appendix_tables.R).
# Sources: headline_S1_S2.csv (+ panel/treatment for N). Output: tab_03_main_estimates.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables"); fold <- c("DOM010905","DOM051703","DOM012510")

hl <- fread(file.path(TAB,"headline_S1_S2.csv"))
# 2026-08-09 (MM): never-treated-only DDD row. S1 from ddd_variants.csv (09m,
# triplediff, control="nevertreated"; verified = 07s byte-identically); S2 from
# ddd_triplediff.csv (07s, control="nevertreated"). Typeset, never re-estimated.
.DV <- fread(file.path(TAB,"ddd_variants.csv"))[spec=="never-treated comparisons"]
.TS <- fread(file.path(TAB,"ddd_triplediff.csv"))[spec=="S2 modernization | uncond dr"]
stopifnot(nrow(.DV)==1, nrow(.TS)==1)

# ---- observations / clusters, computed from the actual estimation frames ----
p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold, .(adm3_pcode,year)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]
N <- function(gt){ d<-merge(p, gt[!is.na(g),.(adm3_pcode,g)], by="adm3_pcode"); list(cl=uniqueN(d$adm3_pcode), ob=nrow(d)) }
n1 <- N(bin); n2 <- N(mod)
# triple-difference stacks ages 15-19 + 30-34 -> 2x municipio-years
ddo1 <- n1$ob*2L; ddo2 <- n2$ob*2L

stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
cell  <- function(e,se,p){ s<-stars(p); sup<-if(s=="") "" else sprintf("^{%s}",s)
  sprintf("\\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}}", e, sup, se, e-1.96*se, e+1.96*se) }
hx    <- function(sc,sp){ r<-hl[scenario==sc & spec==sp]; stopifnot(nrow(r)==1); cell(r$estimate,r$SE,r$p) }
cma   <- function(x) formatC(x, format="d", big.mark=",")

baseH1<-round(hl[scenario=="S1",baseline][1],1); baseH2<-round(hl[scenario=="S2",baseline][1],1)
ntH1<-hl[scenario=="S1",n_treated][1];          ntH2<-hl[scenario=="S2",n_treated][1]

L <- c(
"\\begin{table}[H]\\centering",
"\\caption{Effect of AU openings on the adolescent birth rate}",
"\\label{tab:estimates}\\small",
"\\begin{tabular}{lcc}",
"\\toprule",
" & First AU opening & \\makecell{First AU opening \\\\ or modernization} \\\\",
"\\midrule",
sprintf("Adolescent births per 1{,}000 women aged 15--19 & %s & %s \\\\", hx("S1","CS, no covs"), hx("S2","CS, no covs")),
sprintf("\\quad + baseline controls & %s & %s \\\\",          hx("S1","CS, +covs"),  hx("S2","CS, +covs")),
sprintf("Triple difference (15--19 vs.\\ 30--34) & %s & %s \\\\", hx("S1","DDD (no covs)"), hx("S2","DDD (no covs)")),
sprintf("\\quad never-treated comparisons only & %s & %s \\\\", cell(.DV$ATT,.DV$SE,.DV$p), cell(.TS$ATT,.TS$SE,.TS$p)),
"\\midrule",
sprintf("Baseline mean & %.1f & %.1f \\\\", baseH1, baseH2),
sprintf("Treated municipalities & %d & %d \\\\", ntH1, ntH2),
sprintf("Municipalities (clusters) & %d & %d \\\\", n1$cl, n2$cl),
sprintf("Observations (municipality-years) & %s & %s \\\\", cma(n1$ob), cma(n2$ob)),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.92\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Callaway--Sant'Anna group-time ATT; dependent variable is the adolescent",
"(15--19) birth rate per 1{,}000 women. Municipality-clustered standard errors are in parentheses;",
"95\\% confidence intervals are in brackets. The first column defines treatment as a municipality's",
"first AU opening; the second also includes modernizations of pre-existing units. Triple differences use",
"the triplediff estimator with cohort-specific not-yet-treated comparisons combined by GMM;",
"the indented row restricts the comparison group to never-treated municipalities.",
"Controls: baseline wealth index, public health facilities per 10{,}000 residents, \\% urban,",
"\\% secondary-or-more, \\% SeNaSa 2013 (time-invariant). The triple-difference nets the",
sprintf("30--34 birth rate and stacks both age groups (%s / %s observations).", cma(ddo1), cma(ddo2)),
"Estimator robustness and year-by-year dynamics appear in the Appendix.",
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}"
)
writeLines(L, file.path(TAB,"tab_03_main_estimates.tex"))
cat(sprintf("H1: clusters=%d obs=%d (DDD %d) | H2: clusters=%d obs=%d (DDD %d)\n", n1$cl,n1$ob,ddo1, n2$cl,n2$ob,ddo2))
cat(paste(L, collapse="\n"), "\n\nsaved -> output/tables/tab_03_main_estimates.tex\n")
