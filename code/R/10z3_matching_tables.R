# ============================================================================
# 10z3_matching_tables.R — appendix exhibits for the matched-control analysis
# (2026-08-05, MM: matching results were prose-only; committee item R2
# deserves its own exhibits, and the matched municipalities must be listed).
#   tab_a10_matching.tex       Panel A = CS estimates across the 5 schemes
#                            (from matched_cs.csv, 09i/09i2/09i3);
#                            Panel B = balance, primary scheme
#                            (from matched_balance.csv).
#   tab_a11_matching_pairs.tex The match assignments: each treated municipality
#                            with its 3 nearest structural neighbors (primary
#                            scheme), reproduced from 09i's exact matchit call
#                            (deterministic given the same data + seed) and
#                            VERIFIED against the published counts (20 treated,
#                            34 unique controls) before anything is written.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(MatchIt)})
set.seed(20260722)   # same seed position as 09i's primary (first) matchit call
fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
fm <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

## ---- rebuild 09i's baseline dataset verbatim --------------------------------
p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
fl <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_facility_levels_panel.rds")))[year==2019, .(adm3_pcode, pop_tot, n_hosp)]
base <- merge(p[year==2019, .(adm3_pcode, rate2019=rateA_15_19, wealth_index, pct_urban, pct_educ_secplus, pct_senasa_2013)],
              hc, by="adm3_pcode", all.x=TRUE)
base <- merge(base, fl, by="adm3_pcode", all.x=TRUE)
base[, log_pop := log(pop_tot)]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
base <- merge(base, tr[!is.na(g), .(adm3_pcode, g, treated=as.integer(g>0))], by="adm3_pcode")

V1 <- c("wealth_index","pct_urban","pct_educ_secplus","sns_per10k","pct_senasa_2013","log_pop")
dd <- as.data.frame(base[complete.cases(base[, ..V1])])
rownames(dd) <- dd$adm3_pcode
mo <- matchit(as.formula(paste("treated ~", paste(V1, collapse="+"))),
              data=dd, method="nearest", distance="mahalanobis", ratio=3, replace=TRUE)
mmx <- mo$match.matrix
pairs <- data.table(treated_pcode=rownames(mmx),
                    c1=mmx[,1], c2=mmx[,2], c3=mmx[,3])
kept_controls <- unique(na.omit(as.vector(mmx)))
cat(sprintf("primary match reproduced: %d treated, %d unique controls\n",
            nrow(pairs), length(kept_controls)))
pub <- fread(file.path(TAB,"matched_cs.csv"))
stopifnot(nrow(pairs)==20, length(kept_controls)==pub[grepl("^Primary", spec), n_controls])

## ---- names + assemble pair list ---------------------------------------------
cw <- unique(as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[, .(adm3_pcode, adm3_name, prov_name)])
nm <- setNames(cw$adm3_name, cw$adm3_pcode)
dupnames <- cw[adm3_pcode %in% c(pairs$treated_pcode, kept_controls)][duplicated(adm3_name) | duplicated(adm3_name, fromLast=TRUE), adm3_pcode]
lab <- function(pc) fifelse(pc %in% dupnames, sprintf("%s (%s)", nm[pc], cw[match(pc, adm3_pcode), prov_name]), nm[pc])
pairs <- merge(pairs, tr[, .(treated_pcode=adm3_pcode, first_year)], by="treated_pcode")
pairs[, `:=`(treated_name=lab(treated_pcode),
             controls=paste(lab(c1), lab(c2), lab(c3), sep=", "))]
setorder(pairs, first_year, treated_name)
fwrite(pairs[, .(treated_pcode, treated_name, first_year, c1, c2, c3)],
       file.path(TAB,"matched_pairs.csv"))

esc <- function(x) gsub("([&%])","\\\\\\1", x)
L2 <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Match assignments: treated municipalities and their nearest structural neighbors}",
"\\label{tab:matching_pairs}\\small",
"\\setlength{\\tabcolsep}{4pt}",
"\\begin{tabular}{llp{8.3cm}}",
"\\toprule",
"Treated municipality & First AU & Matched never-treated controls \\\\",
"\\midrule",
pairs[, sprintf("%s & %d & %s \\\\", esc(treated_name), first_year, esc(controls))],
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Primary matching scheme: each of the 20 in-window treated municipalities is matched",
"with replacement to its three nearest never-treated neighbors by Mahalanobis distance on the wealth",
"index, the urban share, women's secondary education, NHS facilities per 10{,}000, SeNaSa affiliation,",
"and log 2019 population; the baseline adolescent birth rate is never a matching variable. Because",
"matching is with replacement, a municipality can serve as a control for more than one treated",
sprintf("municipality; the union of listed controls (%d municipalities) plus the 20 treated forms the", length(kept_controls)),
"estimation sample of the primary row in Appendix \\autoref{tab:matching}.",
"\\end{minipage}",
"\\end{table}")
writeLines(L2, file.path(TAB,"tab_a11_matching_pairs.tex"))

## ---- estimates + balance table ----------------------------------------------
bal <- fread(file.path(TAB,"matched_balance.csv"))[spec=="Primary: structural + log pop"]
stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
cellA <- function(ATT,SE,p){ s<-stars(p); sup<-if(s=="")"" else sprintf("^{%s}",s)
  sprintf("\\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}}", ATT, sup, SE, ATT-1.96*SE, ATT+1.96*SE) }
rowA <- function(sp, lb){ r <- pub[spec==sp]
  sprintf("%s & %d & %s & $%.1f\\%%$ \\\\", lb, r$n_controls, cellA(r$ATT,r$SE,r$p), r$pct) }
BALLAB <- c(log_pop="Log population (2019)", wealth_index="Wealth index",
  pct_educ_secplus="\\% women 20--49 with secondary or more", sns_per10k="NHS facilities per 10{,}000",
  pct_urban="\\% urban", pct_senasa_2013="\\% enrolled in SeNaSa (DHS 2013)",
  rate2019="Teen birth rate, 2019", n_hosp="NHS hospitals (count)",
  age_mean="Mother's age, 2019 births", sh_haitian="\\% Haitian mothers",
  sh_noins="\\% without insurance", sh_csec="\\% c-section")
rowB <- function(v){ r <- bal[variable==v]
  sprintf("\\quad %s & %.2f & %.2f & \\\\", BALLAB[[v]], r$smd_pre, r$smd_post) }
mo_on  <- bal[matched_on==TRUE, variable]; mo_off <- setdiff(names(BALLAB), mo_on)
L1 <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Matched-control estimates}",
"\\label{tab:matching}\\small",
"\\setlength{\\tabcolsep}{3.5pt}",
"\\begin{tabular}{lccc}",
"\\toprule",
"\\multicolumn{4}{l}{\\textit{Panel A. Effect of AU openings on the teen birth rate in matched samples}} \\\\[2pt]",
"Matching scheme & Controls & Estimate (SE) [95\\% CI] & \\% of baseline \\\\",
"\\midrule",
rowA("Primary: structural + log pop",            "Structural + log population (primary)"),
rowA("Sensitivity: + 2019 composition",          "\\quad + 2019 birth composition"),
rowA("M3: eligible pool (hospital) + structural","Eligible-hospital pool + structural"),
rowA("M4: + cwr2010 + DHS2013 fertility + proximity","\\quad + baseline fertility and proximity"),
rowA("M5: PSM (logit) with M4 vars",             "Propensity score (logit)"),
"\\midrule",
"\\multicolumn{4}{l}{\\textit{Panel B. Covariate balance under the primary scheme (standardized differences)}} \\\\[2pt]",
" & Before matching & After matching & \\\\",
"\\cmidrule(lr){2-3}",
"\\multicolumn{4}{l}{\\textit{Matched on:}} \\\\",
sapply(mo_on, rowB),
"\\multicolumn{4}{l}{\\textit{Not matched on:}} \\\\",
sapply(mo_off, rowB),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
r"(\textit{Notes:} Each estimate in Panel A re-estimates the Callaway--Sant'Anna first-opening specification on a matched sample consisting of the 20 treated municipalities and the listed matched controls, using not-yet-treated comparisons within the matched pool and municipio-clustered multiplier-bootstrap standard errors. Schemes 1--4 use nearest-neighbor Mahalanobis matching with replacement (1:3) on the listed covariates; the final scheme matches on a logit propensity score. The baseline adolescent birth rate is never included as a matching variable, so its post-match balance in Panel B is informative rather than imposed mechanically. Residual imbalance on wealth, education, and population reflects the limited overlap between treated municipalities and the full never-treated pool, which is why these estimates are interpreted as lower-bound evidence that brackets the covariate-adjusted specification rather than as a competing main design. Percent effects are relative to the treated mean in the year before opening ($g-1$), equal to 55.28 births per 1{,}000 women aged 15--19. Matched municipalities are listed in Appendix~\autoref{tab:matching_pairs}. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.)",
"\\end{minipage}",
"\\end{table}")
L1 <- gsub("municipio-clustered", "municipality-clustered", L1, fixed=TRUE)
L1 <- gsub("Residual imbalance on wealth, education, and population reflects the limited overlap between treated municipalities and the full never-treated pool, which is why these estimates are interpreted as lower-bound evidence that brackets the covariate-adjusted specification rather than as a competing main design.", "Residual imbalance on wealth, education, and population reflects limited overlap between treated municipalities and the never-treated pool. The matched estimates are therefore reported as a sensitivity exercise rather than as a competing main design.", L1, fixed=TRUE)
writeLines(L1, file.path(TAB,"tab_a10_matching.tex"))
cat("saved -> tab_a10_matching.tex + tab_a11_matching_pairs.tex + matched_pairs.csv\n")
