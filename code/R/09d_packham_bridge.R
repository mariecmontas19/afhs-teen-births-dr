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

# ---- Birth COUNTS as the outcome, no population denominator (committee comment, 2026-11-14) ----
# Municipality fixed effects absorb differences in population size, so the count can be the outcome.
# Estimated AFTER the legacy rows and seeded per call, so the rows above keep their bootstrap stream
# (59.91 rule). No log-of-count row: some municipality-years have zero adolescent births, and Poisson
# handles zeros (Wooldridge 2023). Expectation stated before running (rule 1b): the Poisson count
# effect is close to the offset version above; the level-count estimate is negative but imprecise
# because a few large municipalities dominate it.
cat("nb_15_19==0 municipio-years (S1 sample):", dS1[nb_15_19==0, .N], "of", nrow(dS1), "\n")
run_counts <- function(sc, d){
  m3 <- fepois(nb_15_19 ~ post | id+year, d, cluster=~id)                      # no offset
  b3 <- coef(m3)[["post"]]; s3 <- se(m3)[["post"]]
  add(sc,"TWFE Poisson (count, no population term)", b3, s3, 100*(exp(b3)-1),
      100*exp(b3)*s3, 100*(exp(b3-1.96*s3)-1), 100*(exp(b3+1.96*s3)-1))
  set.seed(20260722)
  a  <- suppressWarnings(suppressMessages(att_gt(yname="nb_15_19", tname="year", idname="id", gname="g", xformla=~1,
         data=d, control_group="notyettreated", base_period="universal", est_method="reg",
         bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  gc <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  bc0 <- gc$overall.att; sc0 <- gc$overall.se
  basec <- d[g>0 & year==g-1L, mean(nb_15_19)]; kc <- 100/basec                 # mean count in the year before opening
  add(sc,"CS count (levels)", bc0, sc0, kc*bc0, kc*sc0, kc*(bc0-1.96*sc0), kc*(bc0+1.96*sc0))
  cat(sprintf("%s counts: Poisson no-offset %.4f (%.4f) | CS count ATT %.2f (%.2f) on baseline mean %.1f births\n",
              sc, b3, s3, bc0, sc0, basec))
  data.table(scenario=sc, base_count=basec)
}
count_base <- rbind(run_counts("S1", dS1), run_counts("S2", dS2))
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
# Layout and notes: MM 2026-11-14 (three panels; every number below is computed, none is typed).
bc2 <- function(sc){ r<-bav[scenario==sc]; sprintf("\\makecell{%s \\\\ {[%s--%s]}}", cm(r$births_averted), cm(r$averted_lo), cm(r$averted_hi)) }
csc <- ff[scenario=="S1" & form=="CS count (levels)"]; b1 <- bav[scenario=="S1"]
stopifnot(nrow(csc)==1, b1$births_averted + b1$actual_births == b1$counterfactual)
L <- c(
"\\begin{table}[htbp]",
"\\centering",
"\\caption{Alternative outcome specifications and estimated births averted}",
"\\label{tab:funcform}",
"\\small",
"",
"\\begin{tabular}{>{\\raggedright\\arraybackslash}p{6.0cm}cc}",   # ragged right: the justified labels stretched
"\\toprule",
" & First AU opening",
" & \\makecell{First AU opening \\\\ or upgrade event} \\\\",
"\\midrule",
"",
"\\multicolumn{3}{l}{\\textit{Panel A. Specifications accounting for population}} \\\\[2pt]",
"",
sprintf("Callaway--Sant'Anna: birth rate (main)\n & %s\n & %s \\\\", pc("S1","CS linear (headline)"), pc("S2","CS linear (headline)")),
"",
sprintf("Callaway--Sant'Anna: $\\log(1+\\text{birth rate})$\n & %s\n & %s \\\\", pc("S1","CS log-rate (% effect)"), pc("S2","CS log-rate (% effect)")),
"",
sprintf("Poisson: birth count, population offset\n & %s\n & %s \\\\", pc("S1","TWFE Poisson (count)"), pc("S2","TWFE Poisson (count)")),
"",
"\\addlinespace",
"\\multicolumn{3}{l}{\\textit{Panel B. Birth counts without population adjustment}} \\\\[2pt]",
"",
sprintf("Poisson: birth count, no population term\n & %s\n & %s \\\\", pc("S1","TWFE Poisson (count, no population term)"), pc("S2","TWFE Poisson (count, no population term)")),
"",
sprintf("Callaway--Sant'Anna: birth count\n & %s\n & %s \\\\", pc("S1","CS count (levels)"), pc("S2","CS count (levels)")),
"",
"\\midrule",
"\\multicolumn{3}{l}{\\textit{Panel C. Estimated registered adolescent births averted}} \\\\[2pt]",
"",
sprintf("Total in treated municipalities after treatment,\n2016--2025 [95\\%% CI]\n & %s\n & %s \\\\", bc2("S1"), bc2("S2")),
"",
"\\bottomrule",
"\\end{tabular}",
"",
"\\begin{minipage}{0.98\\linewidth}",   # wider note so the table fits on one page
"\\vspace{4pt}",
"\\footnotesize",
"",
"\\textit{Notes:}",
"The columns use two treatment definitions: a municipality's first AU",
"opening, and its first AU opening or documented upgrade event.",
"All outcomes refer to births to mothers aged 15--19.",
"Panels A and B report percentage effects, with standard errors in",
"parentheses and 95\\% confidence intervals in brackets.",
"",
"\\smallskip",
"\\textit{Panel A.}",
"The main specification uses births per 1,000 women aged 15--19.",
"Its percentage effect is the Callaway--Sant'Anna ATT divided by the",
"treated municipalities' mean birth rate in the year before treatment,",
"multiplied by 100.",
"The second specification uses $\\log(1+\\text{birth rate})$ and reports",
"$100(e^{\\beta}-1)$; this is a percentage effect on one plus the rate",
"and approximates the percentage effect on the rate.",
"The Poisson specification models birth counts and includes the log",
"population of women aged 15--19 as an offset, with its coefficient",
"fixed at one.",
"",
"\\smallskip",
"\\textit{Panel B.}",
"These specifications use birth counts without a population term.",
"Both Poisson models include municipality and year fixed effects and",
"report $100(e^{\\beta}-1)$.",
"The Callaway--Sant'Anna count estimate is divided by the treated",
"municipalities' mean birth count in the year before treatment.",
sprintf("For first openings, the estimated change is $%.1f$ births per", csc$coef),
sprintf("municipality-year, equivalent to $%.1f\\%%$ of the baseline mean of %.1f.", csc$pct, count_base[scenario=="S1", base_count]),
"The count specification compares absolute changes, whereas Poisson",
"models proportional changes; their identifying assumptions therefore",
"differ when municipalities vary in size.",
"For the linear specifications, standard errors and confidence intervals",
"are rescaled by the baseline mean. For the log and Poisson",
"specifications, standard errors use the delta method and confidence",
"intervals are transformed from the coefficient scale.",
"",
"\\smallskip",
"\\textit{Panel C.}",
"Estimated births averted equal the negative of the main birth-rate ATT",
"multiplied by the total woman-years of exposure among women aged 15--19",
"in treated municipalities after treatment, divided by 1,000.",
"The calculation uses population Series A and applies the average ATT",
"to all post-treatment years; it does not capture variation in effects",
"over time or spillovers to other municipalities.",
"Confidence intervals propagate uncertainty in the ATT using a normal",
"approximation and treat the population series as fixed.",
sprintf("For first openings, adding the estimated %s births averted to the", cm(b1$births_averted)),
sprintf("%s observed births gives approximately %s births without", cm(b1$actual_births), cm(b1$counterfactual)),
sprintf("treatment, implying a reduction of approximately %.0f\\%%.", b1$pct_prevented),
"These totals refer to registered births; if registration completeness",
"is unaffected by treatment, they provide a conservative estimate of",
"total births averted.",
"",
"\\smallskip",
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_a18_functional_form.tex"))
cat("\nsaved -> packham_bridge.csv + births_averted.csv + tab_a18_functional_form.tex\n")
