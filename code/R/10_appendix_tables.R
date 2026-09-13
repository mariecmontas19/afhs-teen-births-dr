# ============================================================================
# 10_appendix_tables.R — appendix companions to Table 3:
#   A1 estimator robustness  : CS / SA / BJS / TWFE (Gardner dropped) x H1/H2,
#                              overall ATT on the level teen rate (no controls).
#   A2 dynamic effects (H1)  : CS + TWFE distributed-lag, Year 0/1/2, with the
#                              #municipios identifying each horizon.
# Sources: estimators_S1_S2.csv, dynamic_panelB.csv (+ panel/treatment for N).
# Output: output/tables/tab_a01_estimators.tex, tab_a02_dynamics.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables"); fold <- c("DOM010905","DOM051703","DOM012510")

stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
cell  <- function(e,se,p){ s<-stars(p); sup<-if(s=="") "" else sprintf("^{%s}",s)
  sprintf("\\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}}", e, sup, se, e-1.96*se, e+1.96*se) }
cma   <- function(x) formatC(as.integer(x), format="d", big.mark=",")

# clusters / obs (same level frames as Table 3)
p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold, .(adm3_pcode,year)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]
N <- function(gt){ d<-merge(p, gt[!is.na(g),.(adm3_pcode,g)], by="adm3_pcode"); list(cl=uniqueN(d$adm3_pcode), ob=nrow(d)) }
n1<-N(bin); n2<-N(mod)

# ---------- A1: estimator robustness ----------
est <- fread(file.path(TAB,"estimators_S1_S2.csv"))[!grepl("Gardner", estimator)]
ex <- function(sc,pat){ r<-est[scenario==sc & grepl(pat,estimator)]; stopifnot(nrow(r)==1); cell(r$ATT,r$SE,r$p) }
A1 <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Robustness to estimator choice}",
"\\label{tab:estimators}\\small",
"\\begin{tabular}{lcc}",
"\\toprule",
" & First AU opening & \\makecell{First AU opening \\\\ or modernization} \\\\",
"\\midrule",
sprintf("Callaway--Sant'Anna & %s & %s \\\\", ex("S1","^CS"),  ex("S2","^CS")),
sprintf("Sun--Abraham & %s & %s \\\\",         ex("S1","^SA"),  ex("S2","^SA")),
sprintf("Borusyak et al.\\ (imputation) & %s & %s \\\\", ex("S1","^BJS"), ex("S2","^BJS")),
sprintf("TWFE (static benchmark) & %s & %s \\\\", ex("S1","^TWFE"), ex("S2","^TWFE")),
"\\midrule",
sprintf("Municipalities (clusters) & %d & %d \\\\", n1$cl, n2$cl),
sprintf("Observations (municipality-years) & %s & %s \\\\", cma(n1$ob), cma(n2$ob)),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.86\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Overall ATT on the adolescent (15--19) birth rate, level outcome,",
"no controls; municipality-clustered standard errors in parentheses. Gardner (2022)",
"two-stage omitted because it is numerically identical to Borusyak imputation. Controls are",
"omitted here because, being time-invariant, they are absorbed by municipality fixed",
"effects in every estimator except Callaway--Sant'Anna. The Callaway--Sant'Anna row is",
"re-estimated within the estimator-comparison script; its bootstrap standard error can",
"differ trivially from Table 3 across bootstrap runs. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}"
)
writeLines(A1, file.path(TAB,"tab_a01_estimators.tex"))

# ---------- A2: dynamic effects (H1) + A2b (H2) ----------
pb_full <- fread(file.path(TAB,"dynamic_panelB.csv"))
thin <- pb_full[scenario=="S1" & est=="CS" & yr %in% 3:4][order(yr), n_muni]   # for the note (dynamic, not hardcoded)
pb <- pb_full[scenario=="S1" & yr %in% 0:2]
dx <- function(es,y){ r<-pb[est==es & yr==y]; stopifnot(nrow(r)==1); cell(r$att,r$se,r$p) }
nm <- pb[est=="CS"][order(yr), n_muni]
A2 <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Effect by year since first AU opening}",
"\\label{tab:dynamic}\\small",
"\\begin{tabular}{lccc}",
"\\toprule",
" & Year 0 & Year 1 & Year 2 \\\\",
"\\midrule",
sprintf("Callaway--Sant'Anna & %s & %s & %s \\\\", dx("CS",0),  dx("CS",1),  dx("CS",2)),
sprintf("TWFE distributed-lag & %s & %s & %s \\\\", dx("TWFE",0),dx("TWFE",1),dx("TWFE",2)),
"\\midrule",
sprintf("Treated municipalities identifying horizon & %d & %d & %d \\\\", nm[1],nm[2],nm[3]),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.82\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Dynamic ATT on the adolescent (15--19) birth rate after a first AU opening;",
sprintf("full panel of %d municipalities, %s municipality-years. Municipality-clustered standard", n1$cl, cma(n1$ob)),
sprintf("errors in parentheses. Capped at Year 2: Years 3--4 are identified off only %d and %d", thin[1], thin[2]),
"municipalities. The small, statistically insignificant Year-0 effect is consistent with",
"the $\\sim$9-month gestational lag. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}"
)
writeLines(A2, file.path(TAB,"tab_a02_dynamics.tex"))

# ---------- A2b: dynamic effects (H2, incl. modernization) ----------
pb2 <- pb_full[scenario=="S2" & yr %in% 0:2]
dx2 <- function(es,y){ r<-pb2[est==es & yr==y]; stopifnot(nrow(r)==1); cell(r$att,r$se,r$p) }
nm2 <- pb2[est=="CS"][order(yr), n_muni]
thin2 <- pb_full[scenario=="S2" & est=="CS" & yr %in% 3:4][order(yr), n_muni]
A2b <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Effect by year since opening (H2: incl.\\ modernization)}",
"\\label{tab:dynamicH2}\\small",
"\\begin{tabular}{lccc}",
"\\toprule",
" & Year 0 & Year 1 & Year 2 \\\\",
"\\midrule",
sprintf("Callaway--Sant'Anna & %s & %s & %s \\\\", dx2("CS",0),  dx2("CS",1),  dx2("CS",2)),
sprintf("TWFE distributed-lag & %s & %s & %s \\\\", dx2("TWFE",0),dx2("TWFE",1),dx2("TWFE",2)),
"\\midrule",
sprintf("Treated municipalities identifying horizon & %d & %d & %d \\\\", nm2[1],nm2[2],nm2[3]),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.82\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Dynamic ATT on the adolescent (15--19) birth rate, H2 (any opening incl.",
sprintf("modernization; %d treated municipalities). Municipio-clustered standard errors in", 29L),
sprintf("parentheses. Capped at Year 2: Years 3--4 are identified off only %d and %d", thin2[1], thin2[2]),
"municipalities. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
writeLines(A2b, file.path(TAB,"appendix_dynamics_H2.tex"))

cat("=== A1 estimators ===\n", paste(A1,collapse="\n"), "\n\n=== A2 dynamics ===\n", paste(A2,collapse="\n"), "\n")
cat("\nsaved -> tab_a01_estimators.tex + tab_a02_dynamics.tex + appendix_dynamics_H2.tex\n")
