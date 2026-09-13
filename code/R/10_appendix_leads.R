# ============================================================================
# 10_appendix_leads.R — appendix lead/lag (event-time) table: the numeric CS
# event study for H1 and H2 on the level teen rate, e = -5..+4, with e=-1 as the
# universal reference and a joint pre-trend test of the plotted leads (es_pretrend_p).
# This is where the LEAD TERMS live as a table (they are also plotted in the event-
# study figures). Lags 3-4 flagged thin (few treated municipios reach them).
# Source: panel + treatment (recomputes CS dynamic). Output: output/tables/
#   leads_lags.csv, tab_a03_leads.tex
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold, .(adm3_pcode,year,rateA_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]

csdyn <- function(gt){
  d <- merge(p, gt[!is.na(g), .(adm3_pcode,g)], by="adm3_pcode"); d[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g", xformla=~1,
        data=d, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  dy <- suppressMessages(aggte(a, type="dynamic", na.rm=TRUE))
  list(dt=data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e>=-5 & e<=4], pre=es_pretrend_p(dy, -5, -2),
       nmuni=sapply(0:4, function(e) d[g>0 & (year-g)==e, uniqueN(id)])) }

S1 <- csdyn(bin); S2 <- csdyn(mod)
m  <- merge(S1$dt, S2$dt, by="e", suffixes=c("_H1","_H2"), all=TRUE)[order(e)]
m[, `:=`(att_H1=round(att_H1,2), se_H1=round(se_H1,2), att_H2=round(att_H2,2), se_H2=round(se_H2,2))]
fwrite(m, file.path(TAB,"leads_lags.csv"))
cat("=== CS event-time (e=-5..4), H1 & H2 ===\n"); print(m, class=FALSE)
cat(sprintf("\njoint pre-trend (leads -5..-2): H1 avg=%.2f p=%.3f | H2 avg=%.2f p=%.3f\n",
            S1$pre$avg, S1$pre$p, S2$pre$avg, S2$pre$p))

# ---- LaTeX ----
cl <- function(a,se){ if(is.na(a)) return("---"); if(is.na(se)) return(sprintf("$%.2f$",a))
  sprintf("\\makecell{$%.2f$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}}", a, se, a-1.96*se, a+1.96*se) }
rows <- sapply(seq_len(nrow(m)), function(i){
  e <- m$e[i]; lab <- if (e==-1) "$-1$ (reference)" else sprintf("$%d$", e)
  flag <- if (e %in% c(3,4)) "$^{\\dagger}$" else ""
  sprintf("%s%s & %s & %s \\\\", lab, flag, cl(m$att_H1[i], m$se_H1[i]), cl(m$att_H2[i], m$se_H2[i])) })
L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Event-time (lead and lag) estimates}",
"\\label{tab:leads}\\small",
"\\begin{tabular}{lcc}",
"\\toprule",
"Years since opening ($e$) & First AU opening & \\makecell{First AU opening \\\\ or modernization} \\\\",
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Leads (pre-treatment)}} \\\\[2pt]",
rows[m$e < 0],
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Lags (post-treatment)}} \\\\[2pt]",
rows[m$e >= 0],
"\\midrule",
sprintf("Joint pre-trend test ($e\\in[-5,-2]$), $p$ & %.2f & %.2f \\\\", S1$pre$p, S2$pre$p),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.86\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Callaway--Sant'Anna dynamic ATT on the adolescent (15--19) birth rate,",
"universal base period $e=-1$; municipality-clustered standard errors in parentheses. The joint",
"pre-trend test averages the plotted leads $e\\in[-5,-2]$. $^{\\dagger}$Years 3--4 are identified",
"off only 7 and 4 treated municipalities in the first-opening design and are imprecise. $^{***}p<0.01$, etc.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_a03_leads.tex"))
cat("\nsaved -> leads_lags.csv + tab_a03_leads.tex\n")
