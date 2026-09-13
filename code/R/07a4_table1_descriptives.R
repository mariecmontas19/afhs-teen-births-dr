# ============================================================================
# 07a4_table1_descriptives.R — Table 1, Duflo (2001)-style descriptive
# statistics at BASELINE (MM, 2026-08-04). Three panels by level/source, one
# Treated column, one Never-treated column, normalized difference where the
# comparison is municipality-level and pre-treatment:
#   Panel A. Birth-record level, 2019 (registration-complete, pre-treatment
#            for every cohort): means over INDIVIDUAL teen (15-19) births.
#   Panel B. Municipality level (20 treated / 126 never-treated): baseline
#            rates and time-invariant characteristics + normalized difference.
#   Panel C. Survey benchmarks (ENDESA 2013 province measures by arm;
#            ENHOGAR-MICS 2019 national) for what the registry cannot measure
#            at baseline (educ 2020+, union & prenatal 2021+ coverage).
# Replaces table1_muni_prepost.tex in the main text (pre/post columns dropped:
# with a treated/control split, post-period means embed the treatment effect).
# Panel B numbers must REPRODUCE table1_muni_baseline.csv (same bstat).
# Output: table1_descriptives.{csv,tex}
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(fixest)})
fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
fm   <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, arm := fifelse(ever_treated==1L & always_treated==0L, "Treated",
            fifelse(ever_treated==0L, "Control", NA_character_))]
arm_map <- tr[!is.na(arm), .(adm3_pcode, arm)]
stopifnot(arm_map[arm=="Treated",.N]==20L, arm_map[arm=="Control",.N]==126L)

# ---- Panel A: individual birth records, 2019 --------------------------------
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))[
       !is.na(adm3_pcode) & age_mom %in% 15:19 & birth_year==2019L]
b[adm3_pcode %in% names(fm), adm3_pcode := fm[adm3_pcode]]
b <- merge(b, arm_map, by="adm3_pcode")
cat("Panel A births 2019:", nrow(b), "| by arm:\n"); print(b[,.N,by=arm])
pa <- b[, .(n        = .N,
            age      = mean(age_mom),
            sh1517   = 100*mean(age_mom<=17),
            sh1819   = 100*mean(age_mom>=18),
            haitian  = 100*mean(nationality=="Haitian", na.rm=TRUE),
            noins    = 100*mean((insurance=="None")[!is.na(insurance)]),
            csec     = 100*mean((csection==TRUE | csection==1)[!is.na(csection)]),
            pubfac   = 100*mean((facility=="Public")[!is.na(facility)])),
        by=arm]
# observations per statistic (records with the field observed, both arms pooled):
# only insurance has material missingness (2018+ coverage), nationality has one.
nA <- c(births_n = nrow(b), age = b[!is.na(age_mom), .N],
        sh1517 = b[!is.na(age_mom), .N], sh1819 = b[!is.na(age_mom), .N],
        haitian = b[!is.na(nationality), .N], noins = b[!is.na(insurance), .N],
        csec = b[!is.na(csection), .N], pubfac = b[!is.na(facility), .N])
cat("Panel A observations per statistic:\n"); print(nA)

# Panel A difference tests: treated-minus-control mean, from a regression of the
# record-level variable on the treated indicator with standard errors CLUSTERED
# AT THE MUNICIPALITY (births are not independent within a municipality, and
# treatment is assigned at the municipality level, so a naive two-sample t-test
# would overstate precision by an order of magnitude).
b[, tr_ind := as.integer(arm=="Treated")]
b[, `:=`(v_age = as.numeric(age_mom),
         v_sh1517 = 100*as.numeric(age_mom<=17),
         v_sh1819 = 100*as.numeric(age_mom>=18),
         v_haitian = 100*as.numeric(nationality=="Haitian"),
         v_noins   = 100*as.numeric(insurance=="None"),
         v_csec    = 100*as.numeric(csection==TRUE | csection==1),
         v_pubfac  = 100*as.numeric(facility=="Public"))]
diffA <- function(v){
  m <- feols(as.formula(paste0("v_", v, " ~ tr_ind")), data=b, cluster=~adm3_pcode, notes=FALSE)
  ct <- coeftable(m)["tr_ind", ]
  list(diff = unname(ct["Estimate"]), p = unname(ct["Pr(>|t|)"]))
}

# ---- Panel B: municipality level (reproduces table1_muni_baseline.csv) ------
p_all <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
if(!"sns_per10k" %in% names(p_all)){
  hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
  p_all <- merge(p_all, hc, by="adm3_pcode", all.x=TRUE) }
mcb <- unique(p_all[, .(adm3_pcode, ever_treated, always_treated, wealth_index,
                        pct_educ_secplus, pct_urban, pct_internet, cwr_2010_baseline,
                        pct_bottom2q_2018, sns_per10k, pct_senasa_2013)])
mcb[, womenA_15_19_2016 := p_all[year==2016][.SD, on="adm3_pcode", x.womenA_15_19]]
mcb <- merge(mcb, p_all[year %in% 2016:2019,
             .(baseline_teenrate_A=mean(rateA_15_19)), by=adm3_pcode], by="adm3_pcode")
mcb <- merge(mcb, p_all[year==2019, .(adm3_pcode, rate_2019=rateA_15_19, nb_2019=nb_15_19)],
             by="adm3_pcode")
# ENDESA 2013 province measures (all women 15-49, province value assigned to munis)
dhs <- as.data.table(readRDS(file.path(DIR_CLEAN,"dhs_contraception_province.rds")))[,
         .(adm3_pcode, unmet_2013, mcpr_2013)]
mcb <- merge(mcb, dhs, by="adm3_pcode", all.x=TRUE)
# 2026-08-07 (MM): hospital-registry outcomes at baseline. Form 67-A events are
# recorded at the ATTENDING FACILITY's municipality; municipalities with no
# reporting facility have zero events by construction (0-fill). Pregnancy events
# = deliveries + abortion-related attendances, ages 15-19. Stillbirth reporting
# begins only in 2022 (post-treatment for the 2020-21 cohorts; flagged in notes).
mis <- as.data.table(readRDS(file.path(DIR_CLEAN,"mispas_muni_panels.rds"))$year)
m19 <- mis[anio==2019L, .(adm3_pcode, preg_2019=deliv_1519+abort_1519, abort_2019=abort_1519)]
m22 <- mis[anio==2022L, .(adm3_pcode, sb_2022=sb_1519)]
mcb <- Reduce(function(a,b) merge(a,b,by="adm3_pcode",all.x=TRUE), list(mcb, m19, m22))
for(v in c("preg_2019","abort_2019","sb_2022")) mcb[is.na(get(v)), (v):=0]
# 2026-08-07 (MM): schooling at baseline (school year ending 2019, pre-treatment).
# GER and dropout from the MINERD condicion registry (secundario, both sexes);
# grade-6 terminal GER from the by-grade panel written by 08z4.
edc <- as.data.table(readRDS(file.path(DIR_CLEAN,"educ_condicion_muni.rds")))[nivel=="SECUNDARIO"]
e19 <- edc[year_end==2019L, .(mat=sum(mat), aband=sum(aband)), by=adm3_pcode]
popE <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))[
          age_group %in% c("10-14","15-19") & year==2019L,
          .(pop_1217 = 0.6*sum(pop[age_group=="10-14"]) + 0.6*sum(pop[age_group=="15-19"])), by=adm3_pcode]
e19 <- merge(e19, popE, by="adm3_pcode", all.x=TRUE)
e19[, `:=`(ger_2019 = 100*mat/pop_1217, dropout_2019 = 100*aband/mat)]
g6 <- fread(file.path(TAB,"educ_gradegroup_muni_panel.csv"))[
        grp=="6 (Sexto, terminal)" & sexo=="T" & year_end==2019L, .(adm3_pcode, ger6_2019=erate)]
mcb <- Reduce(function(a,b) merge(a,b,by="adm3_pcode",all.x=TRUE),
              list(mcb, e19[, .(adm3_pcode, ger_2019, dropout_2019)], g6))
mcb <- mcb[always_treated==0]
cat("Panel B sample: treated", mcb[ever_treated==1,.N], "| never-treated", mcb[ever_treated==0,.N],
    "| NA unmet:", mcb[is.na(unmet_2013),.N], "\n")
# Panel B/C tests: Welch two-sample t-test of the treated-vs-never-treated mean
# difference across municipalities (unequal variances; 20 vs 126 units). The two
# ENDESA rows are PROVINCE measures broadcast to municipalities, so their test
# clusters at the province: treating 146 municipalities as independent draws of a
# 32-province variable would badly overstate precision.
mcb[, prov := substr(adm3_pcode, 6, 7)]
bstat <- function(v, scale100=FALSE, cluster_prov=FALSE){
  Tv <- mcb[ever_treated==1][[v]]; Cv <- mcb[ever_treated==0][[v]]
  k  <- if (scale100) 100 else 1
  dif <- k*(mean(Tv, na.rm=TRUE) - mean(Cv, na.rm=TRUE))
  if (cluster_prov){
    md <- copy(mcb)[, `:=`(yv = k*get(v), tr_ind = as.integer(ever_treated==1))]
    ct <- coeftable(feols(yv ~ tr_ind, data=md, cluster=~prov, notes=FALSE))["tr_ind", ]
    p <- unname(ct["Pr(>|t|)"])
  } else {
    p <- t.test(Tv, Cv)$p.value                                  # Welch, unequal variances
  }
  list(t = k*mean(Tv, na.rm=TRUE), c = k*mean(Cv, na.rm=TRUE),
       nd = (mean(Tv,na.rm=TRUE)-mean(Cv,na.rm=TRUE))/sqrt((var(Tv,na.rm=TRUE)+var(Cv,na.rm=TRUE))/2),
       dif = dif, p = p)
}
BL <- list(
  rate2019 = bstat("rate_2019"),               brate   = bstat("baseline_teenrate_A"),
  births   = bstat("nb_2019"),                 women   = bstat("womenA_15_19_2016"),
  preg     = bstat("preg_2019"),               abort   = bstat("abort_2019"),
  sb       = bstat("sb_2022"),
  ger      = bstat("ger_2019"),                dropout = bstat("dropout_2019"),
  ger6     = bstat("ger6_2019"),
  wealth   = bstat("wealth_index"),            educ    = bstat("pct_educ_secplus", TRUE),
  internet = bstat("pct_internet", TRUE),      urban   = bstat("pct_urban", TRUE),
  bottom2q = bstat("pct_bottom2q_2018", TRUE), cwr     = bstat("cwr_2010_baseline"),
  sns      = bstat("sns_per10k"),              senasa  = bstat("pct_senasa_2013", TRUE),
  unmet    = bstat("unmet_2013", cluster_prov=TRUE),
  mcpr     = bstat("mcpr_2013",  cluster_prov=TRUE))

# ---- Panel C: survey benchmarks ---------------------------------------------
mics <- fread(file.path(TAB,"mics2019_teen_mothers.csv"))[1]   # national teen-mother row (05k)
stopifnot(nrow(mics)==1)
# Ever-given-birth among women 15-19 and the teen-mother sample sizes, computed
# here from the ENHOGAR-MICS 2019 microdata so no figure is hardcoded.
# DEFINITION (verified 2026-08-04): CM1 (the direct "ever given birth" question)
# reproduces the canonical national 16.1 and poorest-quintile 29.7 exactly
# (16.05 / 29.75). Reconstructing ever-birth from the dated birth history gives
# 16.10 / 29.92 instead; CM1 is the canonical definition and is used here.
MICSDIR <- file.path(DRPAPER,"analysis/datasets/Encuestas de Hogares de Prósitos Múltiples (ENHOGAR)/2019")
wm_ <- fread(file.path(MICSDIR,"ENHOGAR-MICS6-2019-PUB-MUJERES-DE-15-A-49-AÑOS.csv"),
             select=c("HH1","HH2","LN","WB4","WB5","WB6A","WB6B","MA1","MN5",
                      "wmweight","WDOI","WDOB","WM17","windex5","CM1"))
bh_ <- fread(file.path(MICSDIR,"ENHOGAR-MICS6-2019-PUB-HISTORIA-DE-NACIMIENTO-MUJERES-DE-15-A-49-AÑOS.csv"),
             select=c("HH1","HH2","LN","BH4C"))
wmean <- function(x, wt) sum(x*wt, na.rm=TRUE)/sum(wt[!is.na(x)])
w19 <- wm_[WB4 %in% 15:19 & WM17==1]
w19[, eb := fifelse(CM1==1L, 1L, fifelse(CM1==2L, 0L, NA_integer_))]
everbirth_nat <- 100*wmean(w19$eb, w19$wmweight);  n_eb_nat <- w19[!is.na(eb), .N]
everbirth_q1  <- 100*wmean(w19[windex5==1]$eb, w19[windex5==1]$wmweight)
n_eb_q1       <- w19[windex5==1 & !is.na(eb), .N]
# teen mothers: 05k's identification (birth in the 24 months before interview,
# aged 15-19 at that birth); recomputed only to obtain per-statistic Ns, with a
# hard check that the sample matches 05k's published count.
wm_[, educ_yrs := fcase(WB5==2L, 0, WB6A==0L, 0, WB6A==1L & WB6B<90L, pmin(WB6B,8),
                        WB6A==2L & WB6B<90L, 8+pmin(WB6B,4),
                        WB6A==4L & WB6B<90L, 12+pmin(WB6B,8), default=NA_real_)]
wm_[, partnered := fifelse(MA1 %in% c(1L,2L), 1L, fifelse(MA1==3L, 0L, NA_integer_))]
wm_[, anc := fifelse(!is.na(MN5) & MN5<90L, as.numeric(MN5), NA_real_)]
mm_ <- merge(bh_, wm_[, .(HH1,HH2,LN,WDOI,WDOB)], by=c("HH1","HH2","LN"))
mm_[, `:=`(ab=(BH4C-WDOB)/12, mb=WDOI-BH4C)]
ids_ <- unique(mm_[mb>=0 & mb<24 & ab>=15 & ab<20, .(HH1,HH2,LN, age_birth=ab)][,
                 .SD[which.max(age_birth)], by=.(HH1,HH2,LN)])
tw_ <- merge(ids_, wm_, by=c("HH1","HH2","LN"))
stopifnot(nrow(tw_) == mics$n_unw)               # agrees with 05k's teen-mother sample
n_tm  <- c(partnered = tw_[!is.na(partnered), .N], educ_years = tw_[!is.na(educ_yrs), .N],
           prenatal  = tw_[!is.na(anc), .N])
cat(sprintf("MICS: ever-birth national %.2f (N=%d) | poorest quintile %.2f (N=%d) | teen mothers N=%d (prenatal %d)\n",
            everbirth_nat, n_eb_nat, everbirth_q1, n_eb_q1, nrow(tw_), n_tm[["prenatal"]]))

# ---- assemble csv ------------------------------------------------------------
num <- function(x, d=1) fifelse(d==0 & abs(x)>=1000, formatC(round(x), format="d", big.mark=","),
                                sprintf(paste0("%.",d,"f"), x))
rowA <- function(v, tv, cv){
  dd <- if (v=="births_n") list(diff=NA_real_, p=NA_real_) else diffA(v)
  data.table(panel="A", var=v, treated=tv, control=cv, diff=dd$diff, p=dd$p, nd=NA_real_)
}
csv <- rbindlist(list(
  rowA("births_n", pa[arm=="Treated",n],       pa[arm=="Control",n]),
  rowA("age",      pa[arm=="Treated",age],     pa[arm=="Control",age]),
  rowA("sh1517",   pa[arm=="Treated",sh1517],  pa[arm=="Control",sh1517]),
  rowA("sh1819",   pa[arm=="Treated",sh1819],  pa[arm=="Control",sh1819]),
  rowA("haitian",  pa[arm=="Treated",haitian], pa[arm=="Control",haitian]),
  rowA("noins",    pa[arm=="Treated",noins],   pa[arm=="Control",noins]),
  rowA("csec",     pa[arm=="Treated",csec],    pa[arm=="Control",csec]),
  rowA("pubfac",   pa[arm=="Treated",pubfac],  pa[arm=="Control",pubfac]),
  rbindlist(lapply(names(BL), function(k)
    data.table(panel=fifelse(k %in% c("unmet","mcpr"), "C", "B"), var=k,
               treated=BL[[k]]$t, control=BL[[k]]$c,
               diff=BL[[k]]$dif, p=BL[[k]]$p, nd=BL[[k]]$nd))),
  data.table(panel="C", var=c("everbirth_nat","everbirth_q1","partnered","educ_years","prenatal"),
             treated=c(everbirth_nat, everbirth_q1, mics$pct_partnered, mics$educ_years, mics$anc_visits),
             control=NA_real_, diff=NA_real_, p=NA_real_, nd=NA_real_)))
obs_map <- c(nA, setNames(rep(146L, 18), c("rate2019","brate","births","women","preg","abort",
             "sb","ger","dropout","ger6","wealth","educ",
             "internet","urban","bottom2q","cwr","sns","senasa")),
             c(unmet=146L, mcpr=146L, everbirth_nat=n_eb_nat, everbirth_q1=n_eb_q1), n_tm)
csv[, obs := obs_map[var]]
stopifnot(!anyNA(csv$obs))
fwrite(csv, file.path(TAB,"table1_descriptives.csv")); print(csv, class=FALSE)

# ---- LaTeX -------------------------------------------------------------------
bm  <- function(n) formatC(n, format="d", big.mark=",")
pf  <- function(p) fifelse(p < 0.001, "$<$.001", sub("^0", "", sprintf("%.3f", p)))
# Duflo's Table 1 convention: the sample size travels in the ROW LABEL, and only
# where it departs from the panel's stated N.
rr <- function(lab, v, d=1){
  r <- csv[var==v]
  nd <- fifelse(is.na(r$nd), "", sprintf("%.2f", r$nd))          # SMD, Panel B only (OG #13/14, §59.95)
  sprintf("%s & %s & %s & %s & %s \\\\", lab, num(r$treated,d), num(r$control,d),
          pf(r$p), nd) }
rC <- function(lab, x, d=1) sprintf("%s & \\multicolumn{2}{c}{%s} & & \\\\", lab, num(x,d))
L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Baseline characteristics of treated and comparison municipalities}",
"\\label{tab:table1}\\scriptsize",
"\\setlength{\\tabcolsep}{3pt}",
"\\begin{tabular}{lcccc}",
"\\toprule",
" & Treated & Never-treated & & \\\\",
" & (20) & (126) & $p$-value & Std.\\ difference \\\\",
"\\midrule",
sprintf("\\multicolumn{5}{l}{\\textit{Panel A. Birth-record level: teen (15--19) births, 2019 (N $=$ %s)}} \\\\[2pt]",
        bm(nA[["births_n"]])),
sprintf("Number of births & %s & %s & & \\\\", bm(csv[var=="births_n",treated]),
        bm(csv[var=="births_n",control])),
rr("Mother's age (mean)","age"),
rr("\\% mothers aged 15--17","sh1517"),
rr("\\% mothers aged 18--19","sh1819"),
rr("\\% Haitian mothers","haitian"),
rr(sprintf("\\%% without health insurance (N $=$ %s)$^{a}$", bm(nA[["noins"]])),"noins"),
rr("\\% c-section","csec"),
rr("\\% public-facility births","pubfac"),
"\\midrule",
"\\multicolumn{5}{l}{\\textit{Panel B. Municipality level (N $=$ 146)}} \\\\[2pt]",
rr("Teen birth rate, 2019 (per 1,000 women 15--19)","rate2019"),
rr("Teen birth rate, 2016--19 average","brate"),
rr("Teen births, 2019 (mean count)","births",0),
rr("Teen pregnancy events, 2019 (mean count)$^{h}$","preg",0),
rr("Teen abortion-related attendances, 2019$^{h}$","abort",0),
rr("Women aged 15--19 (2016)","women",0),
rr("Gross enrollment ratio, secondary (2018--19)","ger"),
rr("Dropout rate (pp of enrolled, 2018--19)","dropout"),
rr("Grade-6 (terminal) enrollment ratio (2018--19)","ger6"),
rr("Wealth index (2022 census, 29-item PCA)","wealth",2),
rr("\\% women 20--49 with secondary or more","educ"),
rr("\\% households with internet","internet"),
rr("\\% urban","urban"),
rr("\\% in bottom two income quintiles (2018)","bottom2q"),
rr("Child-woman ratio, 2010 census","cwr",0),
rr("Public health facilities per 10{,}000 residents","sns"),
rr("\\% enrolled in SeNaSa (DHS 2013)","senasa"),
"\\midrule",
"\\multicolumn{5}{l}{\\textit{Panel C. Survey benchmarks (ENDESA 2013; ENHOGAR-MICS 2019)}} \\\\[2pt]",
rr("\\% unmet need for contraception, women 15--49 (N $=$ 146)$^{p}$","unmet"),
rr("\\% modern contraceptive prevalence, women 15--49 (N $=$ 146)$^{p}$","mcpr"),
rC(sprintf("\\%% women 15--19 ever given birth (N $=$ %s)", bm(n_eb_nat)), everbirth_nat),
rC(sprintf("\\quad poorest wealth quintile (N $=$ %s)", bm(n_eb_q1)), everbirth_q1),
rC(sprintf("Teen mothers: \\%% partnered (N $=$ %s)", bm(n_tm[["partnered"]])), mics$pct_partnered),
rC(sprintf("Teen mothers: years of education (N $=$ %s)", bm(n_tm[["educ_years"]])), mics$educ_years),
rC(sprintf("Teen mothers: prenatal visits (N $=$ %s)", bm(n_tm[["prenatal"]])), mics$anc_visits),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.94\\linewidth}\\vspace{3pt}\\scriptsize",
"\\textit{Notes:} All quantities are pre-treatment unless noted otherwise; the first cohort opens in 2020.",
"Sample sizes appear in row labels when they differ from the panel total. Reported $p$-values come from",
"Panel A regressions on 2019 records with municipality-clustered SEs and Panel B Welch $t$-tests across",
sprintf("municipalities, excluding the nine served before 2016. $^{a}$Insurance is recorded from 2018 (%s records", bm(nA[["births_n"]]-nA[["noins"]])),
"lack it); education, union status, and prenatal care begin in 2020--21 and therefore appear only in Panel C.",
"$^{h}$Hospital-registry (Form 67-A) events at the attending facility's municipality; municipalities without",
"a reporting facility are zeros. Pregnancy events $=$ deliveries $+$ abortion-related attendances, ages 15--19.",
"The standardized difference divides the mean difference by the pooled standard deviation across",
"municipalities, $\\sqrt{(s_T^2+s_C^2)/2}$. Large values for counts and population reflect the documented",
"scale-based placement criterion (\\autoref{tab:allocation}); identification relies on parallel trends",
"rather than baseline balance. Appendix~\\autoref{tab:matching}, Panel B reports covariate balance before and",
"after matching in the same metric.",
"Schooling rows are for the school year ending in 2019; gross enrollment is any-age secondary enrollment per",
"100 population aged 12--17.",
"The wealth index is the first principal component of 29 census items (Appendix \\autoref{tab:wealth_items}).",
"$^{p}$ENDESA province measures for women aged 15--49 are assigned to municipalities;",
"tests cluster at the province. MICS quantities are national, weighted, and admit no arm comparison; teen",
"mothers are women aged 15--19 with a birth in the 24",
"months before the interview.",
"\\textit{Sources:} BDNV; Form 67-A registries; MoE; 2022 census; NSO; NHS registry; SIUBEN; ENDESA 2013;",
"ENHOGAR-MICS 2019.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_01_descriptives.tex"))
cat("saved -> table1_descriptives.{csv,tex}\n")
