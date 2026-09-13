# ============================================================================
# 08q2_cohort_atts.R — COHORT-LEVEL ATT table (the publishable disaggregation;
# MM 2026-07-10). aggte(type="group") on the S1 headline spec gives one ATT
# per opening cohort (2020..2025) WITH standard errors — six legitimate
# objects, unlike the 20 per-municipality DiDs (08q, internal diagnostic).
# Baselines = cohort mean rate at its own g-1. Output: cohort_atts.csv +
# tab_a16_cohorts.tex (registered in the manifest).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold,
       .(adm3_pcode, year, rateA_15_19)]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
est <- merge(p, tr[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est[, id := as.integer(factor(adm3_pcode))]
a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
      xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
      bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
gr <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
co <- data.table(cohort=gr$egt, ATT=round(gr$att.egt,2), SE=round(gr$se.egt,2))
co[, p := round(2*pnorm(-abs(ATT/SE)),3)]
nsz <- est[g>0, .(n_munis=uniqueN(adm3_pcode)), keyby=g]
bs <- est[g>0][, .SD[year==g-1L], by=adm3_pcode][, .(baseline=round(mean(rateA_15_19),1)), keyby=g]
co <- Reduce(function(x,y) merge(x,y,by.x="cohort",by.y="g"), list(co, nsz, bs))
co[, `:=`(pct=round(100*ATT/baseline,0), max_exposure_yrs=2025-cohort+1)]
HL <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1" & spec=="CS, no covs"]
cat(sprintf("=== per-cohort ATTs (S1 headline spec; overall %.2f) ===\n", HL$estimate))
print(co, class=FALSE)
fwrite(co, file.path(TAB,"cohort_atts.csv"))

stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
rr <- function(i){ r <- co[i]; s <- stars(r$p); sup <- if(s=="")"" else sprintf("^{%s}",s)
  sprintf("%d & %d & \\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}} & %.1f & $%d\\%%$ & %d \\\\", r$cohort, r$n_munis, r$ATT, sup, r$SE, r$ATT-1.96*r$SE, r$ATT+1.96*r$SE, r$baseline, r$pct, r$max_exposure_yrs) }
L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Effect by opening cohort}",
"\\label{tab:cohorts}\\small",
"\\begin{tabular}{cccccc}",
"\\toprule",
"Cohort & Municipalities & Estimate (SE) [95\\% CI] & Baseline ($g{-}1$) & \\% & Max.\\ exposure (yrs) \\\\",
"\\midrule",
sapply(seq_len(nrow(co)), rr),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.88\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Callaway--Sant'Anna cohort-level ATTs (group aggregation), first AU opening,",
sprintf("not-yet-treated controls, municipio-clustered SEs in parentheses; overall group ATT $=%.2f$.", HL$estimate),
"Baseline $=$ cohort mean teen rate in the year before its opening. Later cohorts are observed",
"only at short exposures, where the dynamic effect is still small (Year 0 $\\approx -4$, building",
"to $\\approx -9.5$ by Year 2), so small late-cohort ATTs are expected, not anomalous.",
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
L <- gsub("municipio-clustered", "municipality-clustered", L, fixed=TRUE)
writeLines(L, file.path(TAB,"tab_a16_cohorts.tex"))
cat("saved -> cohort_atts.csv + tab_a16_cohorts.tex\n")
