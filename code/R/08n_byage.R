# ============================================================================
# 08n_byage.R — Table 5: CS effect by mother's SINGLE YEAR of age (15..19) plus
# the 15-17 and 18-19 subtotals, as a DECOMPOSITION of the headline teen rate.
# Every outcome uses the COMMON 15-19 female denominator:
#   rate_a = 1000 * (births to age a) / women_15_19
# so the single-year contributions SUM to the 15-19 headline rate (and 15-17 +
# 18-19 do too). NB: rate_15 is "births to 15-yr-olds per 1,000 women 15-19" --
# a CONTRIBUTION to the headline, NOT the age-15 fertility rate (no single-yr denom).
# Run for H1 (first opening) and H2 (incl. modernization). Same fold(3)+CS as 07t.
# Output: output/tables/byage_single_S1_S2.csv, tab_04_byage.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

# ---- single-year birth counts per municipality-year ----
b  <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
te <- b[!is.na(adm3_pcode) & age_mom %in% 15:19]
# fold the 3 split-child pcodes into their parents (as the panel's nb_15_19 does; 39 births
# 2023-25) so the single-year counts decompose the headline outcome EXACTLY
foldmap <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")
te[adm3_pcode %in% names(foldmap), adm3_pcode := foldmap[adm3_pcode]]
cnt <- dcast(te, adm3_pcode + birth_year ~ age_mom, fun.aggregate=length, value.var="age_mom")
setnames(cnt, "birth_year", "year")
for(a in 15:19) if(!as.character(a) %in% names(cnt)) cnt[, (as.character(a)):=0L]
setnames(cnt, as.character(15:19), paste0("a",15:19))
cnt[, `:=`(a1517=a15+a16+a17, a1819=a18+a19)]

p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, womenA_15_19)]
d0 <- merge(p, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
ac <- c(paste0("a",15:19),"a1517","a1819"); for(v in ac) d0[is.na(get(v)), (v):=0L]
d0 <- d0[!adm3_pcode %in% fold]
for(v in ac) d0[, (paste0("rate_",v)) := 1000*get(v)/womenA_15_19]
cat("merge check: rows", nrow(d0), "| munis", d0[,uniqueN(adm3_pcode)], "| NA:", sum(is.na(d0$rate_a19)), "\n")

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]

bands <- data.table(yn=paste0("rate_",ac), label=c("15","16","17","18","19","15--17","18--19"))
csg <- function(d, yname){
  est <- d[!is.na(g)]; est[, id:=as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g", xformla=~1,
        data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  gr <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  bs <- est[g>0 & year==g-1L, mean(get(yname), na.rm=TRUE)]   # g-1 baseline (estimator reference; pre-treatment for both margins)
  data.table(ATT=gr$overall.att, SE=gr$overall.se, p=2*pnorm(-abs(gr$overall.att/gr$overall.se)), baseline=bs) }
run_sc <- function(gt, sc){
  d <- merge(d0, gt[, .(adm3_pcode,g)], by="adm3_pcode", all.x=TRUE)
  rbindlist(lapply(seq_len(nrow(bands)), function(i) cbind(scenario=sc, age=bands$label[i], csg(d, bands$yn[i])))) }

res <- rbind(run_sc(bin,"S1"), run_sc(mod,"S2"))
res[, `:=`(ATT=round(ATT,2), SE=round(SE,2), p=round(p,3), baseline=round(baseline,2), pct=round(100*ATT/baseline,0))]
fwrite(res, file.path(TAB,"byage_single_S1_S2.csv"))
cat("\n=== Table 5: single-year decomposition ===\n"); print(res, class=FALSE)
sy <- res[scenario=="S1" & age %in% as.character(15:19)]
HL <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1" & spec=="CS, no covs"]
cat(sprintf("\nDecomp check (H1): sum single-year ATT 15..19 = %.2f (headline %.2f); sum baselines = %.2f (%.2f)\n",
            sum(sy$ATT), HL$estimate, sum(sy$baseline), HL$baseline))

# ---- LaTeX ----
stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
cl <- function(sc,ag){ r<-res[scenario==sc & age==ag]; s<-stars(r$p); sup<-if(s=="")"" else sprintf("^{%s}",s)
  sprintf("\\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}}", r$ATT, sup, r$SE, r$ATT-1.96*r$SE, r$ATT+1.96*r$SE) }
rr <- function(ag, ind=FALSE){ lab<-if(ind) sprintf("\\quad %s",ag) else ag
  sprintf("%s & %s & %s \\\\", lab, cl("S1",ag), cl("S2",ag)) }
L <- c(
"\\begin{table}[H]\\centering",
"\\caption{Effect by mother's single year of age (decomposition of the teen birth rate)}",
"\\label{tab:byage}\\small",
"\\begin{tabular}{lcc}",
"\\toprule",
"Mother's age & First AU opening & \\makecell{First AU opening \\\\ or modernization} \\\\",
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Single years}} \\\\[2pt]",
rr("15",TRUE), rr("16",TRUE), rr("17",TRUE), rr("18",TRUE), rr("19",TRUE),
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Subtotals}} \\\\[2pt]",
rr("15--17",TRUE), rr("18--19",TRUE),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.9\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Callaway--Sant'Anna group ATT; outcome is births to mothers of the given age",
"per 1{,}000 women aged 15--19 (the \\emph{common} 15--19 denominator), so each row is that age's",
"\\emph{contribution} to the overall teen-rate effect; the single-year ATTs sum to the 15--19 total",
sprintf("($%.2f$). These are contributions, not single-year fertility rates (no single-year denominator", HL$estimate),
"exists). Municipality-clustered standard errors are in parentheses; 95\\% confidence intervals are",
"in brackets. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_04_byage.tex"))
cat("\nsaved -> byage_single_S1_S2.csv + tab_04_byage.tex\n")
