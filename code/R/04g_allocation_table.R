# ============================================================================
# 04g_allocation_table.R — Table 1, "The Allocation of Adolescent Units",
# mirroring Duflo (2001, AER) Table 2 (§59.55; MM 2026-08-04).
# The documented rule (gcps2019prea, ganard2021ppa) prioritized municipalities
# by adolescent-birth COUNTS. Count = rate x female population 15-19, so
# log(births) = log(pop) + log(rate) exactly, and a counts rule implies the
# two log components enter EQUALLY. Single-column LPM (Duflo's layout), HC1
# SEs; the equality test and the births-alone / hospital variants are printed
# for the PROSE (kept out of the table per MM, as in Duflo).
# Variables are 2016-2019 pre-roll-out averages (one municipality has zero
# births in 2019 alone; averages also match the balance-table baseline).
# Output: output/tables/tab_02_allocation.tex + allocation_test.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(sandwich); library(lmtest); library(car)})
fold <- c("DOM010905","DOM051703","DOM012510")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
d <- p[year %in% 2016:2019, .(nb=mean(nb_15_19), pop=mean(womenA_15_19), rate=mean(rateA_15_19)), by=adm3_pcode]
fl <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_facility_levels_panel.rds")))[
        year==2019, .(adm3_pcode, has_hosp=as.integer(n_hosp>0))]
d <- merge(d, fl, by="adm3_pcode", all.x=TRUE)
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                  fifelse(ever_treated==0L, 0L, NA_integer_))]
d <- merge(d, tr[!is.na(g), .(adm3_pcode, treated=as.integer(g>0))], by="adm3_pcode")
stopifnot(nrow(d)==146L, sum(d$treated)==20L, d[nb<=0,.N]==0)
d[, `:=`(l_nb=log(nb), l_pop=log(pop), l_rate=log(rate))]

hc <- function(m) coeftest(m, vcov=vcovHC(m, type="HC1"))
m1 <- lm(treated ~ l_nb, d);                        ct1 <- hc(m1)   # (1) the stated criterion
m2 <- lm(treated ~ l_pop, d);                       ct2 <- hc(m2)   # (2) scale alone
m  <- lm(treated ~ l_pop + l_rate, d);              ct  <- hc(m)    # (3) decomposition
m4 <- lm(treated ~ l_pop + l_rate + has_hosp, d);   ct4 <- hc(m4)   # (4) + hospital
eq <- linearHypothesis(m, "l_pop = l_rate", vcov=vcovHC(m, type="HC1"))

cat(sprintf("TABLE: l_pop %.3f (%.3f) | l_rate %.3f (%.3f) | R2 %.3f | N %d\n",
    ct["l_pop",1], ct["l_pop",2], ct["l_rate",1], ct["l_rate",2], summary(m)$r.squared, nrow(d)))
cat(sprintf("PROSE: births alone %.3f (%.3f); equality F=%.1f p=%.5f; hospital %.3f (%.3f)\n",
    ct1["l_nb",1], ct1["l_nb",2], eq$F[2], eq$`Pr(>F)`[2], ct4["has_hosp",1], ct4["has_hosp",2]))
fwrite(data.table(term=c("l_pop","l_rate","l_nb_alone","has_hosp"),
                  est=c(ct["l_pop",1],ct["l_rate",1],ct1["l_nb",1],ct4["has_hosp",1]),
                  se =c(ct["l_pop",2],ct["l_rate",2],ct1["l_nb",2],ct4["has_hosp",2]),
                  eq_F=eq$F[2], eq_p=eq$`Pr(>F)`[2], R2=summary(m)$r.squared, N=nrow(d)),
       file.path(DIR_TABLES,"allocation_test.csv"))

# ---- Panel B inputs: adoption-order endogeneity check (04i; OG comment #4, §59.95) ----
ao <- fread(file.path(DIR_TABLES,"adoption_order.csv"))
gv <- function(k) ao[stat==k, value]
stopifnot(nrow(ao)>=11)

star <- function(p) fifelse(p<.01,"^{***}", fifelse(p<.05,"^{**}", fifelse(p<.10,"^{*}","")))
cell <- function(ct, v) if(v %in% rownames(ct)) sprintf("$%.3f%s$", ct[v,1], star(ct[v,4])) else ""
sece <- function(ct, v) if(v %in% rownames(ct)) sprintf("(%.3f)", ct[v,2]) else ""
r2 <- sapply(list(m1,m2,m,m4), function(x) sprintf("%.2f", summary(x)$r.squared))
row2 <- function(lab, v, cont=""){
  c(sprintf("%s & %s & %s & %s & %s \\\\", lab, cell(ct1,v), cell(ct2,v), cell(ct,v), cell(ct4,v)),
    sprintf("%s & %s & %s & %s & %s \\\\", cont, sece(ct1,v), sece(ct2,v), sece(ct,v), sece(ct4,v))) }
L <- c("\\begin{table}[htbp]\\centering",
"\\caption{Allocation and timing of first Adolescent Unit openings}",
"\\label{tab:allocation}\\small",
"\\setlength{\\tabcolsep}{4pt}",
"\\begin{tabular}{lcccc}",
"\\toprule",
" & \\multicolumn{4}{c}{Received a first AU, 2020--2025$^{a}$} \\\\",
"\\cmidrule(lr){2-5}",
" & (1) & (2) & (3) & (4) \\\\",
"\\midrule",
"\\multicolumn{5}{l}{\\textit{Panel A. Which municipalities received a first AU (linear probability model)}} \\\\[2pt]",
row2("Log of the number of adolescent births$^{b}$", "l_nb"),
row2("Log of the number of adolescent women", "l_pop", "\\quad aged 15--19 in the municipality"),
row2("Log of the adolescent birth rate$^{b}$", "l_rate"),
row2("Public hospital in the municipality (0/1)", "has_hosp"),
sprintf("Number of observations & %d & %d & %d & %d \\\\", nrow(d), nrow(d), nrow(d), nrow(d)),
sprintf("$R^{2}$ & %s & %s & %s & %s \\\\", r2[1], r2[2], r2[3], r2[4]),
"\\midrule",
"\\multicolumn{5}{l}{\\textit{Panel B. Timing of adoption among the 20 treated municipalities$^{c}$}} \\\\[2pt]",
sprintf("Opening year on log baseline birth rate (OLS) & \\multicolumn{4}{c}{$%.2f$ (%.2f), $p=%.2f$} \\\\",
        gv("ols_lrate_b"), gv("ols_lrate_se"), gv("ols_lrate_p")),
sprintf("\\quad conditional on log adolescent population & \\multicolumn{4}{c}{$%.2f$ (%.2f), $p=%.2f$} \\\\",
        gv("ols2_lrate_b"), gv("ols2_lrate_se"), gv("ols2_lrate_p")),
sprintf("Spearman rank correlation, opening year $\\times$ baseline rate & \\multicolumn{4}{c}{$%.2f$, $p=%.2f$} \\\\",
        gv("spearman_rho"), gv("spearman_p")),
sprintf("Mean baseline rate, 2020--21 vs.\\ 2023--25 cohorts & \\multicolumn{4}{c}{%.1f vs.\\ %.1f, $p=%.2f$} \\\\",
        gv("early_mean"), gv("late_mean"), gv("welch_p")),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.86\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Heteroskedasticity-robust standard errors are in parentheses.",
"$^{a}$The dependent variable is an indicator equal to one if the municipality received its",
"first AU between 2020 and 2025; Panel A reports linear probability models for the 146 estimation",
"municipalities.",
"$^{b}$Births per 1{,}000 women aged 15--19; all regressors are 2016--2019 pre-roll-out",
"averages. The number of adolescent births, the documented allocation criterion, is the",
"product of the adolescent population and the birth rate, so allocation on counts implies",
"that the two components of column (3) enter with equal coefficients.",
"$^{c}$Panel B tests whether earlier openings went to higher-fertility municipalities among the 20 treated",
"municipalities. Opening year is regressed on the log baseline rate (HC1 standard",
"errors), the rank correlation is Spearman's $\\rho$, and the last row compares mean baseline rates",
"of the earliest (2020--2021) and latest (2023--2025) opening cohorts (Welch test).",
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(DIR_TABLES,"tab_02_allocation.tex"))
cat("saved -> tab_02_allocation.tex + allocation_test.csv\n")
