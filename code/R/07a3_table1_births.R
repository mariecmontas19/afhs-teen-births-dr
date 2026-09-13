# ============================================================================
# 07a3_table1_births.R — Table 1, MUNICIPALITY-LEVEL summary statistics:
# Treated (H1) vs Control x Pre (2019) / Post (2020-25). REWORKED per committee
# feedback (§44.1, 2026-07-09):
#   - ages 15-17 and 18-19 shown separately (rate contributions per 1,000
#     women 15-19 + mean age),
#   - YEARS of education instead of % secondary: BDNV categories mapped
#     None=0 / Primary=6 / Secondary=12 / University=16 (MICS-calibrated
#     means 0/5.8/11.2/15.2 validate the mapping; 05k),
#   - "partnered" instead of "in union" (same variable: casada or union libre),
#   - Pre-2019 cells that the registry cannot fill (educ 2020+, union 2021+,
#     prenatal 2021+) are completed from ENHOGAR-MICS 2019 teen mothers
#     (national, 05k) and FLAGGED — different instrument, survey-time.
# Birth-composition vars are per municipality-year among TEEN births, averaged
# UNWEIGHTED across municipality-years (matching the unweighted CS estimand).
# Pre = 2019 only (ramp years distort a 2016-19 average).
# Output: table1_muni_prepost.csv + table1_muni_baseline.csv + table1_muni_prepost.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))[!is.na(adm3_pcode) & age_mom %in% 15:19]
fm <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")
b[adm3_pcode %in% names(fm), adm3_pcode := fm[adm3_pcode]]
EDUC_YRS <- c(None=0, Primary=6, Secondary=12, University=16)   # MICS-calibrated 0/5.8/11.2/15.2 (05k)
b[, educ_yrs := EDUC_YRS[as.character(educ)]]

# per municipality-year composition among teen births
my <- b[, .(n_teen = .N, n_1517 = sum(age_mom<=17), n_1819 = sum(age_mom>=18),
            age_mean = mean(age_mom),
            age_1517 = mean(age_mom[age_mom<=17]), age_1819 = mean(age_mom[age_mom>=18]),
            sh_haitian = 100*mean(nationality=="Haitian", na.rm=TRUE),
            sh_partner = 100*mean((in_union==TRUE | in_union==1)[!is.na(in_union)]),
            sh_noins   = 100*mean((insurance=="None")[!is.na(insurance)]),
            educ_years = mean(educ_yrs, na.rm=TRUE),
            prenatal   = mean(as.numeric(prenatal_checks), na.rm=TRUE),
            sh_csec    = 100*mean((csection==TRUE | csection==1)[!is.na(csection)]),
            sh_pubfac  = 100*mean((facility=="Public")[!is.na(facility)])),
        by=.(adm3_pcode, year=birth_year)]
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold,
       .(adm3_pcode, year, rateA_15_19, nb_15_19, womenA_15_19, wealth_index, pct_urban, pct_educ_secplus, pct_senasa_2013)]
if(!"sns_per10k" %in% names(p)){ hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]; p <- merge(p, hc, by="adm3_pcode", all.x=TRUE) }
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, arm := fifelse(ever_treated==1L & always_treated==0L, "Treated", fifelse(ever_treated==0L, "Control", NA_character_))]
d <- merge(merge(p, my, by=c("adm3_pcode","year"), all.x=TRUE), tr[!is.na(arm), .(adm3_pcode, arm)], by="adm3_pcode")
for(v in c("n_teen","n_1517","n_1819")) d[is.na(get(v)), (v):=0L]
d[, `:=`(rate_1517 = 1000*n_1517/womenA_15_19, rate_1819 = 1000*n_1819/womenA_15_19)]
d <- d[year>=2019L]
d[, period := factor(fifelse(year==2019L, "Pre", "Post"), levels=c("Pre","Post"))]
cat("muni-years:", nrow(d), "| munis:", d[,uniqueN(adm3_pcode)], "| treated:", d[arm=="Treated",uniqueN(adm3_pcode)], "\n")

mn <- function(x) round(mean(x, na.rm=TRUE), 1)
st <- d[, .(teen_rate=mn(rateA_15_19), r1517=mn(rate_1517), r1819=mn(rate_1819),
            teen_births=round(mean(nb_15_19)), age=mn(age_mean),
            age1517=mn(age_1517), age1819=mn(age_1819),
            haitian=mn(sh_haitian), partnered=mn(sh_partner), noins=mn(sh_noins),
            educ_years=mn(educ_years), prenatal=mn(prenatal), csec=mn(sh_csec), pubfac=mn(sh_pubfac)),
        by=.(arm, period)]
out <- dcast(melt(st, id=c("arm","period")), variable ~ arm + period)
setcolorder(out, c("variable","Treated_Pre","Treated_Post","Control_Pre","Control_Post"))

# ---- MICS 2019 completion of the empty Pre cells (national teen mothers, 05k) ----
mics <- fread(file.path(TAB,"mics2019_teen_mothers.csv"))[1]     # national row
stopifnot(nrow(mics)==1)
micsfill <- c(educ_years=mics$educ_years, partnered=mics$pct_partnered, prenatal=mics$anc_visits)
for(v in names(micsfill)){
  stopifnot(all(is.na(out[variable==v, .(Treated_Pre, Control_Pre)])))   # only fill truly-empty cells
  out[variable==v, `:=`(Treated_Pre = micsfill[[v]], Control_Pre = micsfill[[v]])]
}
out[, mics_flag := fifelse(variable %in% names(micsfill), "Pre from MICS 2019 (national, survey instrument)", "")]
print(out, class=FALSE)
# Full panel (unreduced, no fold filter) for the balance block: reproduces the
# sample the retired Table 2 used, so the folded-in numbers are unchanged.
p_all <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
if(!"sns_per10k" %in% names(p_all)){
  hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
  p_all <- merge(p_all, hc, by="adm3_pcode", all.x=TRUE) }

# ---- Panel B: baseline municipality characteristics + BALANCE ----------------
# 2026-07-30 (MM): the old standalone Table 2 (balance) is FOLDED IN HERE. Its
# variables join Panel B and its normalized difference becomes a 5th column, so
# levels and balance are read off one table instead of two.
# Normalized difference = (mean_T - mean_C)/sqrt((var_T+var_C)/2), computed on
# the SAME estimation sample as Table 2 was: always-treated municipalities are
# excluded, so this is in-window-treated vs never-treated.
mcb <- unique(p_all[, .(adm3_pcode, ever_treated, always_treated, wealth_index,
                        pct_educ_secplus, pct_urban, pct_internet, cwr_2010_baseline,
                        pct_bottom2q_2018, sns_per10k, pct_senasa_2013)])
mcb[, womenA_15_19_2016 := p_all[year==2016][.SD, on="adm3_pcode", x.womenA_15_19]]
mcb <- merge(mcb, p_all[year %in% 2016:2019,
             .(baseline_teenrate_A=mean(rateA_15_19)), by=adm3_pcode], by="adm3_pcode")
mcb <- mcb[always_treated==0]
stopifnot(nrow(mcb) == mcb[ever_treated==1,.N] + mcb[ever_treated==0,.N])
cat("balance sample: treated", mcb[ever_treated==1,.N], "| never-treated", mcb[ever_treated==0,.N], "\n")

bstat <- function(v, scale100=FALSE){
  Tv <- mcb[ever_treated==1][[v]]; Cv <- mcb[ever_treated==0][[v]]
  k  <- if (scale100) 100 else 1
  list(t = k*mean(Tv), c = k*mean(Cv),
       nd = (mean(Tv)-mean(Cv))/sqrt((var(Tv)+var(Cv))/2))
}
BL <- list(
  wealth   = bstat("wealth_index"),            educ    = bstat("pct_educ_secplus", TRUE),
  urban    = bstat("pct_urban", TRUE),         internet= bstat("pct_internet", TRUE),
  bottom2q = bstat("pct_bottom2q_2018", TRUE), cwr     = bstat("cwr_2010_baseline"),
  sns      = bstat("sns_per10k"),              senasa  = bstat("pct_senasa_2013", TRUE),
  women    = bstat("womenA_15_19_2016"),       brate   = bstat("baseline_teenrate_A"))
bl <- rbindlist(lapply(names(BL), function(k)
        data.table(var=k, treated=BL[[k]]$t, never=BL[[k]]$c, norm_diff=BL[[k]]$nd)))
print(bl, class=FALSE)
fwrite(out, file.path(TAB,"table1_muni_prepost.csv")); fwrite(bl, file.path(TAB,"table1_muni_baseline.csv"))

# ---- LaTeX (paper Table 1) ----
lab <- c(teen_rate="Teen birth rate (per 1,000 women 15--19)",
         r1517="\\quad ages 15--17 (contribution)", r1819="\\quad ages 18--19 (contribution)",
         teen_births="Teen births per municipality-year", age="Mother's age (mean)",
         age1517="\\quad mean age, mothers 15--17", age1819="\\quad mean age, mothers 18--19",
         haitian="\\% Haitian mothers", partnered="\\% partnered (married or union)",
         noins="\\% without health insurance", educ_years="Years of education (mean)",
         prenatal="Prenatal visits (mean)", csec="\\% c-section", pubfac="\\% public-facility births")
fmt <- function(x, v) fifelse(is.na(x), "--", fifelse(v=="teen_births", sprintf("%.0f", x), sprintf("%.1f", x)))
row1 <- function(v){
  r <- out[variable==v]; m <- if(r$mics_flag!="") "$^{m}$" else ""
  sprintf("%s%s & %s & %s & %s & %s \\\\", lab[[v]], m,
          fmt(r$Treated_Pre,v), fmt(r$Treated_Post,v), fmt(r$Control_Pre,v), fmt(r$Control_Post,v)) }
# Panel B rows: level in treated and control, plus the normalized difference.
blr <- function(k, lab2, dig=1){
  r <- bl[var==k]
  sprintf("%s & \\multicolumn{2}{c}{%.*f} & \\multicolumn{2}{c}{%.*f} & %.2f \\\\",
          lab2, dig, r$treated, dig, r$never, r$norm_diff) }
L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Municipality-level summary statistics and baseline balance}",
"\\label{tab:table1}\\small",
"\\setlength{\\tabcolsep}{5pt}",
"\\begin{tabular}{lccccc}",
"\\toprule",
" & \\multicolumn{2}{c}{Treated (20)} & \\multicolumn{2}{c}{Control (126)} & Norm. \\\\",
"\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
" & 2019 & 2020--25 & 2019 & 2020--25 & diff. \\\\",
"\\midrule",
"\\multicolumn{6}{l}{\\textit{Panel A. Teen births and mothers (per municipality-year)}} \\\\[2pt]",
row1("teen_rate"), row1("r1517"), row1("r1819"), row1("teen_births"), row1("age"),
row1("age1517"), row1("age1819"),
row1("haitian"), row1("partnered"), row1("noins"), row1("educ_years"), row1("prenatal"),
row1("csec"), row1("pubfac"),
"\\midrule",
"\\multicolumn{6}{l}{\\textit{Panel B. Baseline municipality characteristics (time-invariant)}} \\\\[2pt]",
blr("wealth","Wealth index (2022 census, 29-item PCA)",2),
blr("educ",  "\\% women 20--49 with secondary or more"),
blr("internet","\\% households with internet"),
blr("urban", "\\% urban"),
blr("bottom2q","\\% in bottom two income quintiles (2018)"),
blr("cwr",   "Child-woman ratio, 2010 census", 0),
blr("women", "Women aged 15--19 (2016)", 0),
blr("sns",   "SNS facilities per 10{,}000"),
blr("senasa","\\% enrolled in SeNaSa (DHS 2013)"),
blr("brate", "Teen birth rate, 2016--19 baseline"),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Unweighted means across municipality-years, matching the unweighted estimand;",
"composition variables are computed among teen (15--19) births within each municipality-year. Pre $=$ 2019",
"only, because 2016--18 are registration-ramp years while 2019 is registration-complete and pre-treatment for",
"every cohort. The ages 15--17 and 18--19 rows are contributions to the teen rate and share the 15--19 denominator.",
"Years of education map registry categories as None$=$0, Primary$=$6, Secondary$=$12, University$=$16",
"(ENHOGAR-MICS 2019 calibration: 0/5.8/11.2/15.2). $^{m}$Registry coverage starts in 2020 for education and",
"2021 for partnered and prenatal; those Pre cells report the national mean for teen mothers (a birth in the",
"24 months before interview) from ENHOGAR-MICS 2019. That is a survey instrument measured at interview, so its",
"levels are not directly comparable to the registry Post cells; self-reported prenatal visits, for instance, read",
"higher than registry counts. Treated $=$ first AU opening in 2020--25.",
"Panel B is time-invariant, so a single value is reported per arm and the normalized difference is defined only",
"there. It equals $(\\bar{x}_T-\\bar{x}_C)/\\sqrt{(s^2_T+s^2_C)/2}$ and is computed excluding the nine always-served",
"municipalities, so the comparison is in-window-treated against never-treated. Values near or above one indicate",
"arms that differ materially on that characteristic, which is why the design conditions on baseline covariates",
"rather than relying on raw comparability; Section~\\ref{sec:strategy} discusses this.",
"The wealth index is the first principal component of 29 household items from the 2022 census;",
"Appendix Table~\\ref{tab:wealth_items} lists every item with its loading and sampling-adequacy statistic.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"table1_muni_prepost.tex"))
cat("saved -> table1_muni_prepost.{csv,tex} + table1_muni_baseline.csv\n")
