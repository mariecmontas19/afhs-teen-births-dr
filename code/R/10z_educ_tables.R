# ============================================================================
# 10z_educ_tables.R — formatter for the education-margin paper tables
# (MM 2026-07-29). Reads the numeric CS outputs and writes:
#   Table XA = by sex (enrollment/dropout/repetition), long format with
#              baseline, % of baseline, and joint pre-trend p.
#   Table XB = enrollment by grade group + nested Sexto overage row.
# Stars: *** p<.01, ** p<.05, * p<.10 (project convention). SEs in parentheses.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")
st <- function(p) fifelse(p<.01,"^{***}", fifelse(p<.05,"^{**}", fifelse(p<.10,"^{*}","")))

## ---------- Panel (a): outcomes x sex grid --------------------------------
## Failure (yvar "repetition" = Reprobado) DROPPED 2026-08-07 (MM: outcome
## retired; the prose clause citing it is removed in the same change).
n <- fread(file.path(TAB,"educ_final_bysex_numeric.csv"))
n <- n[yvar %in% c("enroll_rate","dropout")]
cellA <- function(r) sprintf("\\makecell{$%+.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}}",
                             r$ATT, st(r$p), r$SE, r$ATT-1.96*r$SE, r$ATT+1.96*r$SE)
rowA <- function(yv, lab, n=get("n", envir=globalenv())){
  M <- n[yvar==yv & sexo=="M"]; F <- n[yvar==yv & sexo=="F"]; T <- n[yvar==yv & sexo=="T"]
  stopifnot(nrow(M)==1, nrow(F)==1, nrow(T)==1)
  sprintf("%s & %s & %s & %s & %.1f & %.2f \\\\", lab, cellA(M), cellA(F), cellA(T), T$base, T$lead_p) }
# Rows excluding La Romana (MM 2026-11-16; 08z13_educ_excl_laromana.R): the only 2019 opener,
# whose enrollment ratio fell sharply in 2020-2021.
nx  <- fread(file.path(TAB,"educ_final_bysex_numeric_excl_DOM081201.csv"))[yvar == "enroll_rate"]
XLR <- "\\quad excluding La Romana"
panelA <- c(
"\\begin{subtable}{\\linewidth}\\centering",
"\\caption{Enrollment and dropout, by sex}\\label{tab:educ_bysex}",
"\\begin{tabular}{lccccc}",
"\\toprule",
" & Male & Female & Total & Baseline & Pre-trend $p$ \\\\",
"\\midrule",
rowA("enroll_rate","Gross enrollment ratio (ages 12--17)"),
rowA("enroll_rate", XLR, nx),
"\\addlinespace",
rowA("dropout","Dropout rate (pp of enrolled)"),   # no excluding-La-Romana row: it barely moves (MM 2026-11-16)
"\\bottomrule",
"\\end{tabular}",
"\\end{subtable}")

## ---------- Table XB: by grade group x SEX + Sexto overage ----------
## 2026-08-07 (MM): Male/Female/Total columns, mirroring Panel (a). Source =
## educ_enroll_by_gradegroup_bysex.csv (08z4 appended block; per-sex GER
## denominators, per-call seeds). The overage row remains both-sexes (08z5).
gs <- fread(file.path(TAB,"educ_enroll_by_gradegroup_bysex.csv"))
setnames(gs, "grade_group", "grp")
ov <- fread(file.path(TAB,"educ_overage_decomp.csv"))[component=="overage" & group=="6"]
ov_base <- ov$base  # g-1 treated mean, per 100 pop 12-17 (written by 08z5)
lab <- c("1-3 (Primero-Tercero)"="\\quad Grades 1--3 (ages $\\approx$12--15)",
         "4-5 (Cuarto-Quinto)"  ="\\quad Grades 4--5 (ages $\\approx$15--17)",
         "6 (Sexto, terminal)"  ="\\quad Grade 6, terminal (ages $\\approx$17--18)")
# MM 2026-08-03: entry-grades (1-3) row DROPPED from the table -- it fails its
# pre-trend test under the official-age GER denominator (lead p = .04); the
# omission is stated in the notes.
gs <- gs[grp != "1-3 (Primero-Tercero)"]
rowXB <- function(gv){
  M <- gs[grp==gv & sexo=="M"]; F <- gs[grp==gv & sexo=="F"]; T <- gs[grp==gv & sexo=="T"]
  sprintf("%s & %s & %s & %s & %.1f & %.2f \\\\", lab[gv], cellA(M), cellA(F), cellA(T), T$base, T$lead_p) }
ovrow <- sprintf("\\quad\\quad \\emph{of which overage (3+ yrs behind, both sexes)} & & & \\makecell{$%+.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}} & %.2f & %.2f \\\\",
   ov$ATT, st(ov$p), ov$SE, ov$ATT-1.96*ov$SE, ov$ATT+1.96*ov$SE, ov_base, ov$lead_p)
panelB <- c(
"\\begin{subtable}{\\linewidth}\\centering\\scriptsize",
"\\caption{Enrollment by grade group, by sex}\\label{tab:educ_grade}",
"\\setlength{\\tabcolsep}{2pt}",
"\\begin{tabular}{lccccc}",
"\\toprule",
"Gross enrollment ratio (ages 12--17) & Male & Female & Total & Baseline & Pre-trend $p$ \\\\",
"\\midrule",
rowXB("4-5 (Cuarto-Quinto)"),
"\\addlinespace",
rowXB("6 (Sexto, terminal)"),
"\\addlinespace",
ovrow,
"\\bottomrule",
"\\end{tabular}",
"\\end{subtable}")

## ---------- Panel (c): school performance (Pruebas Nacionales) ----------
## Dormant §59.58 -> RE-ACTIVATED by MM 2026-08-12 (§59.87) after the §59.83
## PN2024 convocatoria-1 fix: the dormant version's significant taker/passer
## rows were a data defect in the public 2024 slice; the honest panel is all
## precise zeros, reported as such (Pruebas only; EDN excluded, §59.84-85).
pc <- fread(file.path(TAB,"pruebas_panelC.csv"))
rowsXC <- pc[, sprintf("%s & \\makecell{$%+.2f%s$ (%.2f) \\\\ {[$%+.2f$, $%+.2f$]}} & %s & %s & %.2f \\\\",
   label, ATT, st(p), SE, lo, hi,
   fifelse(is.na(pct), "N/A", sprintf("%.2f", base)),
   fifelse(is.na(pct), "N/A", sprintf("$%+.1f$", pct)), lead_p)]
panelC <- c(
"\\begin{subtable}{\\linewidth}\\centering\\scriptsize",
"\\caption{School performance, Pruebas Nacionales}\\label{tab:educ_pruebas}",
"\\setlength{\\tabcolsep}{2pt}",
"\\begin{tabular}{lcccc}",
"\\toprule",
" & Estimate (SE) [95\\% CI] & Baseline & \\% of baseline & Pre-trend $p$ \\\\",
"\\midrule",
rowsXC,
"\\bottomrule",
"\\end{tabular}",
"\\end{subtable}")

## ---------- combined float: Table N, panels (a) + (b) + (c) ----------
comb <- c(
"\\begin{table}[H]\\centering\\footnotesize",
"\\setlength{\\tabcolsep}{2.5pt}",
"\\caption{Effects of AU openings on secondary-school outcomes}",
"\\label{tab:educ}",
panelA,
"\\\\[0.6em]",   # tighter gaps: three rows were added on 2026-11-16 and the table must stay on one page
panelB,
"\\\\[0.6em]",   # tighter gaps: three rows were added on 2026-11-16 and the table must stay on one page
panelC,
"\\begin{minipage}{\\linewidth}\\vspace{3pt}\\footnotesize",
"\\textit{Notes:} Callaway--Sant'Anna group-aggregated ATT on municipality-level secondary-schooling",
"outcomes (146 municipalities, school years 2015--16 to 2024--25); not-yet-treated controls,",
"universal base period, municipality-clustered multiplier-bootstrap standard errors in parentheses,",
"95\\% confidence intervals in brackets. The gross enrollment ratio is all secondary enrollment,",
"regardless of age, per 100 population of the official secondary ages 12--17; the dropout rate is",
"the share of enrolled students leaving during the school year.",
local({ g <- fread(file.path(TAB,"educ_laromana_ger.csv"))
  sprintf("The row labeled ``excluding La Romana'' drops La Romana, the only municipality that opened in 2019, whose enrollment ratio fell from %.0f to %.0f between 2019 and 2021.",
          g[year_end==2019, la_romana], g[year_end==2021, la_romana]) }),
"Baseline $=$ treated mean in the",
"year before opening (total column); pre-trend $p$ tests that the",
"inverse-variance-weighted average of the leads $e\\in[-5,-2]$ is zero.",
"Percentage effects are relative to that baseline; N/A indicates that the baseline is not defined for the outcome.",
"In panel (b) the male and female columns use per-sex enrollment over the per-sex official-age",
"population, so the sex-specific ratios do not average to the total.",
"Panel (b) omits grades 1--3 because that outcome fails its pre-trend test;",
"the nested row isolates students at least three years behind grade.",
"Panel (c) uses school-level results from the Pruebas Nacionales, the national terminal-secondary",
"examination (first examination round; school years 2016--2024, with the assessment scoreless in 2020",
"and cancelled in 2021; the 2024 results use student microdata for the first examination round provided by",
sprintf("MoE), aggregated to municipalities; %d treated municipalities (2020 and 2021 openers are", local({   # counted as in 08z9 (2026-11-13; was a typed 16)
  b <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% c("DOM010905","DOM051703","DOM012510")]
  pm <- unique(as.data.table(readRDS(file.path(DIR_CLEAN,"pruebas_muni.rds")))$adm3_pcode)
  b[ever_treated==1L & always_treated==0L & !first_year %in% c(2020L,2021L) & adm3_pcode %in% pm, .N] })),
"excluded because their base school year is unobservable). Scores are standardized within year",
"$\\times$ subject $\\times$ modality $\\times$ scoring regime; test-takers and passers are scaled",
"per 100 population aged 17--18, approximated as $0.4\\times$ the population aged 15--19.",
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
writeLines(comb, file.path(TAB,"tab_06_educ_spillover.tex"))
cat("wrote tab_06_educ_spillover.tex (panels a=by sex, b=by grade, c=Pruebas Nacionales; §59.87)\n")
