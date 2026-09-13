# ============================================================================
# 09d_packham_bridge.R — functional-form bridge to Packham (2017): show the
# headline is not an artifact of the linear-rate specification, and translate it
# into a count/% effect + a births-averted welfare figure.
#   (1) TWFE linear rate (static post)          -> level, per 1,000
#   (2) TWFE Poisson, count w/ log-pop offset    -> exp(b)-1 = % rate change
#       (Poisson = log-link count model = the count analog of Packham's ln-rate)
#   (3) TWFE Poisson distributed-lag (Year 0/1/2) -> % effect builds with exposure
#   (4) Births averted = CS headline ATT x exposed teen-women-years (treated, post)
# CS linear headline (-6.43 / -3.95) carried as the reference anchor.
# Same fold(3)+treatment as 07t. Denominator A. Outputs:
#   output/tables/packham_bridge.csv, births_averted.csv, tab_a18_functional_form.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(fixest); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold,
         .(adm3_pcode, year, nb_15_19, womenA_15_19, rateA_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]
hl  <- fread(file.path(TAB,"headline_S1_S2.csv"))   # for CS reference + baseline

cat("rate==0 municipio-years:", p[rateA_15_19==0, .N], "\n")   # check before log

mkd <- function(gt){
  d <- merge(p, gt[!is.na(g), .(adm3_pcode,g)], by="adm3_pcode")
  d[, id := as.integer(factor(adm3_pcode))]
  d[, post := as.integer(g>0 & year>=g)]
  d[, relyr := fifelse(g>0, pmin(pmax(year-g,-5L),5L), -1L)]
  d[] }
dS1 <- mkd(bin); dS2 <- mkd(mod)

res <- list(); add <- function(sc,form,b,se,pct,pse,plo,phi) res[[length(res)+1]] <<-
  data.table(scenario=sc, form=form, coef=round(b,4), se=round(se,4),
             p=round(2*pnorm(-abs(b/se)),3), pct=round(pct,1),
             pct_se=round(pse,1), pct_lo=round(plo,1), pct_hi=round(phi,1))

run <- function(sc, d){
  base <- hl[scenario==sc & spec=="CS, no covs", baseline]
  # (1) TWFE linear rate
  m1 <- feols(rateA_15_19 ~ post | id+year, d, cluster=~id)
  b1 <- coef(m1)[["post"]]; s1 <- se(m1)[["post"]]; k <- 100/base
  add(sc,"TWFE linear (per 1,000)", b1, s1, k*b1, k*s1, k*(b1-1.96*s1), k*(b1+1.96*s1))
  # (2) TWFE Poisson, count w/ offset (the Packham-literal log-link contrast)
  m2 <- fepois(nb_15_19 ~ post | id+year, d, offset=~log(womenA_15_19), cluster=~id)
  b2 <- coef(m2)[["post"]]; s2 <- se(m2)[["post"]]
  add(sc,"TWFE Poisson (count)", b2, s2, 100*(exp(b2)-1),
      100*exp(b2)*s2, 100*(exp(b2-1.96*s2)-1), 100*(exp(b2+1.96*s2)-1))
  # (3) CS on log-rate = the heterogeneity-robust PROPORTIONAL effect (CS has no Poisson family)
  d2 <- copy(d)[, lr := log1p(rateA_15_19)]
  a  <- suppressWarnings(suppressMessages(att_gt(yname="lr", tname="year", idname="id", gname="g", xformla=~1,
         data=d2, control_group="notyettreated", base_period="universal", est_method="reg",
         bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  gl <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  bl <- gl$overall.att; sl <- gl$overall.se
  add(sc,"CS log-rate (% effect)", bl, sl, 100*(exp(bl)-1),
      100*exp(bl)*sl, 100*(exp(bl-1.96*sl)-1), 100*(exp(bl+1.96*sl)-1))
  # CS linear reference (from headline)
  cs <- hl[scenario==sc & spec=="CS, no covs"]
  add(sc,"CS linear (headline)", cs$estimate, cs$SE, cs$pct_of_baseline,
      k*cs$SE, k*(cs$estimate-1.96*cs$SE), k*(cs$estimate+1.96*cs$SE))
  invisible(m2)
}
cat("\n===== Table 4: functional form =====\n")
run("S1", dS1); run("S2", dS2)
ff <- rbindlist(res); print(ff, class=FALSE)
fwrite(ff, file.path(TAB,"packham_bridge.csv"))

# ---- Poisson distributed-lag (Year 0/1/2), S1 ----
cat("\n===== Poisson distributed-lag (S1) =====\n")
mp <- fepois(nb_15_19 ~ i(relyr, ref=-1) | id+year, dS1, offset=~log(womenA_15_19), cluster=~id)
ctp <- as.data.table(coeftable(mp), keep.rownames="term")[grepl("relyr::[0-2]$",term)]
ctp[, `:=`(yr=as.integer(sub(".*::","",term)), pct=round(100*(exp(Estimate)-1),1))]
print(ctp[,.(yr, b=round(Estimate,3), se=round(`Std. Error`,3), pct)], class=FALSE)

# ---- Births averted: CS ATT x exposed teen-women-years (treated, post) ----
cat("\n===== Births averted (CS headline ATT) =====\n")
ba <- list()
for(sc in c("S1","S2")){
  d <- if(sc=="S1") dS1 else dS2
  r  <- hl[scenario==sc & spec=="CS, no covs"]; est <- r$estimate; se <- r$SE   # current headline (-6.43, SE 2.10)
  red    <- -est                          # point reduction per 1,000
  red_lo <- -(est + 1.96*se)              # smaller reduction (upper CI on ATT)  -> fewer averted
  red_hi <- -(est - 1.96*se)              # larger reduction  (lower CI on ATT)  -> more averted
  ex  <- d[post==1, sum(womenA_15_19)]    # exposed teen-women-years (treated, post)
  act <- d[post==1, sum(nb_15_19)]        # teen births ACTUALLY observed in those cells
  av  <- red/1000*ex
  ba[[sc]] <- data.table(scenario=sc, att_per1000=round(red,2), exposed_women_years=round(ex),
                         actual_births=act, births_averted=round(av),
                         averted_lo=round(red_lo/1000*ex), averted_hi=round(red_hi/1000*ex),
                         counterfactual=round(act+av), pct_prevented=round(100*av/(act+av),1))
}
bav <- rbindlist(ba); print(bav, class=FALSE)
fwrite(bav, file.path(TAB,"births_averted.csv"))

# ---- Table 4 LaTeX (functional form + births averted) ----
stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
pc <- function(sc, fm){ r<-ff[scenario==sc & form==fm]; stopifnot(nrow(r)==1)
  s<-stars(r$p); sup<-if(s=="") "" else sprintf("^{%s}",s)
  sprintf("\\makecell{$%.1f\\%%%s$ (%.1f) \\\\ {[$%.1f\\%%$, $%.1f\\%%$]}}", r$pct, sup, r$pct_se, r$pct_lo, r$pct_hi) }
cm <- function(x) formatC(as.integer(x), format="d", big.mark=",")
bc <- function(sc){ r<-bav[scenario==sc]; sprintf("%s [%s--%s]", cm(r$births_averted), cm(r$averted_lo), cm(r$averted_hi)) }
L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Functional form and births averted}",
"\\label{tab:funcform}\\small",
"\\begin{tabular}{lcc}",
"\\toprule",
" & First AU opening & \\makecell{First AU opening \\\\ or modernization} \\\\",
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Implied \\% effect on the teen birth rate}} \\\\[2pt]",
sprintf("Callaway--Sant'Anna, linear (main estimate) & %s & %s \\\\", pc("S1","CS linear (headline)"), pc("S2","CS linear (headline)")),
sprintf("Callaway--Sant'Anna, log-rate & %s & %s \\\\", pc("S1","CS log-rate (% effect)"), pc("S2","CS log-rate (% effect)")),
sprintf("TWFE, Poisson (count) & %s & %s \\\\", pc("S1","TWFE Poisson (count)"),     pc("S2","TWFE Poisson (count)")),
"\\midrule",
sprintf("Teen births averted (treated, 2016--25) [95\\%% CI] & %s & %s \\\\", bc("S1"), bc("S2")),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.9\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Implied \\% = effect relative to the pre-treatment baseline rate",
"(linear: coefficient$/$baseline; log/Poisson: $e^{\\beta}-1$; the log outcome is $\\log(1+\\text{rate})$);",
"delta-method standard errors on the percent scale in parentheses, exact-transform 95\\% confidence",
"intervals in brackets. The CS log-rate row is the",
"heterogeneity-robust proportional effect (Callaway--Sant'Anna has no count/Poisson family).",
"The Poisson row is a static two-way fixed-effects count model with a log-population offset",
"and is the literal analog of Packham's (2017) ln-rate specification, population-weighted via the",
"birth count. \\textbf{Births averted} $=$ CS ATT $\\times$ exposed teen-women-years in treated municipalities",
"after opening; brackets $=$ 95\\% CI propagated from the ATT (normal approx.). Rough welfare approximation: it",
"applies the \\emph{average} ATT to \\emph{all} post-treatment years (the effect actually builds from",
"$\\approx0$ in Year 0, so it is a total, not year-accurate), counts treated municipalities' OWN exposure",
"only (no spillovers), and uses denominator A.",
sprintf("Sanity check: treated municipalities recorded %s teen births in these post-opening cells, so the first-opening",
        cm(bav[scenario=="S1", actual_births])),
sprintf("counterfactual is $\\approx$%s; AUs prevented $\\approx$%.0f\\%% of the would-be teen births.",
        cm(bav[scenario=="S1", counterfactual]), bav[scenario=="S1", pct_prevented]),
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_a18_functional_form.tex"))
cat("\nsaved -> packham_bridge.csv + births_averted.csv + tab_a18_functional_form.tex\n")
