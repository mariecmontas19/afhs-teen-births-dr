# ============================================================================
# 08z_educ_cs.R — Education margin (SIP-502F3B01 data): CS event study of AU
# openings on FEMALE SECONDARY schooling outcomes, municipality level, exact
# 07t spec (not-yet controls, universal base, est="reg", muni-clustered
# multiplier bootstrap, biters=2000). §59.28: joins THIS paper.
#
# RULE-1B PREDICTIONS (stated before estimation, 2026-07-29):
#   Mechanical bound: headline -6.43 births/1,000 teen women; even 1:1
#   birth->dropout conversion caps the dropout effect near -0.6pp; realistic
#   -0.1 to -0.3pp on a ~4pp base -> EXPECT negative, small, likely ns.
#   (Public-data designs were null, §59.8-59.10.)
#   Onset from e>=1 (gestation + school-year timing). Leads flat (JEE-style).
#   F-M DDD same sign; log F enrollment ~0/slightly +; repetition ~0.
#   A large or significant effect would EXCEED the fertility channel and
#   trigger an instrument check BEFORE interpretation.
#
# TIMING: school year runs Aug..Jun; time index = year_end (2016..2025).
#   PRIMARY: g_edu = first_year + 1 (first school year ENDING after the
#   opening year = first substantially exposed year). SENSITIVITY: g_edu =
#   first_year. 2025-opening cohort -> g_edu 2026 > panel end: contributes as
#   not-yet-treated control (correct).
# Overage (sobreedad): 2018-19 missing in the delivery -> NOT estimated here
#   (uneven spacing); descriptive only, revisit if featured.
# Output: output/tables/educ_cs_results.csv + educ_cs_dynamics.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260729)
TAB <- file.path(PROJ,"output","tables")

pan <- as.data.table(readRDS(file.path(DIR_CLEAN,"educ_condicion_muni.rds")))
sec <- dcast(pan[nivel=="SECUNDARIO"], adm3_pcode + year_end ~ sexo,
             value.var=c("mat","aband","rep"))
sec[, `:=`(dropout_F = 100*aband_F/mat_F, dropout_M = 100*aband_M/mat_M,
           rep_F     = 100*rep_F/mat_F,
           logmat_F  = log(mat_F))]
sec[, ddd_dropout := dropout_F - dropout_M]

fold <- c("DOM010905","DOM051703","DOM012510")
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]
est <- merge(sec, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
cat("estimation munis:", uniqueN(est$adm3_pcode), "| treated:", est[g>0, uniqueN(adm3_pcode)],
    "| years:", est[,uniqueN(year_end)], "\n\n")

run <- function(yvar, gshift, lab){
  d <- copy(est)
  d[, g_edu := fifelse(g==0L, 0L, g + gshift)]
  d[, id := as.integer(factor(adm3_pcode))]
  d <- d[!is.na(get(yvar))]
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=~1,
              data=d, control_group="notyettreated", base_period="universal",
              est_method="reg", bstrap=TRUE, cband=TRUE, biters=2000,
              clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group",   na.rm=TRUE)
  dy <- aggte(a, type="dynamic", na.rm=TRUE)           # full window, filter in R (07t gotcha)
  ev <- data.table(outcome=lab, e=dy$egt, att=dy$att.egt, se=dy$se.egt, crit=dy$crit.val.egt)
  pre <- ev[e %between% c(-5,-2)]
  prew <- pre[, sum(att/se^2)/sum(1/se^2)]              # precision-weighted lead avg
  data.table(outcome=lab, ATT=round(gg$overall.att,3), SE=round(gg$overall.se,3),
             p=round(2*pnorm(-abs(gg$overall.att/gg$overall.se)),3),
             leads_avg=round(prew,3),
             leads_sig=pre[, sum(2*pnorm(-abs(att/se)) < .05, na.rm=TRUE)])[, ok := TRUE][] -> row
  list(row=row, dyn=ev)
}

specs <- list(
  c("dropout_F",   "F dropout rate (pp)"),
  c("ddd_dropout", "F-M dropout DDD (pp)"),
  c("rep_F",       "F repetition rate (pp)"),
  c("logmat_F",    "log F enrollment"))

res <- list(); dyn <- list()
for(sp in specs){
  r <- run(sp[1], 1L, paste0(sp[2], " [g+1 primary]"))
  res[[length(res)+1]] <- r$row; dyn[[length(dyn)+1]] <- r$dyn
}
# timing sensitivity on the headline outcome
r <- run("dropout_F", 0L, "F dropout rate (pp) [g+0 sensitivity]")
res[[length(res)+1]] <- r$row; dyn[[length(dyn)+1]] <- r$dyn

R <- rbindlist(res)[, ok := NULL][]
cat("==================== EDUCATION CS RESULTS (S1, not-yet, universal) ====================\n")
print(R, class=FALSE)
fwrite(R, file.path(TAB,"educ_cs_results.csv"))
fwrite(rbindlist(dyn), file.path(TAB,"educ_cs_dynamics.csv"))
cat("\nsaved -> educ_cs_results.csv + educ_cs_dynamics.csv\n")

# ---- BLOCK B: instrument checks on the two block-A surprises (2026-07-29) ----
# Rule 1b executed: log F enrollment +2.1% (p=.008) and F repetition -0.61
# (p=.098) exceeded predictions -> checked BEFORE interpretation.
# VERDICT (first run): male secondary enrollment rises MORE (+2.7%, p=.003)
# and PRIMARY enrollment (no fertility channel) rises for BOTH sexes ->
# municipality-wide enrollment/population trend, NOT a fertility-mediated
# effect. Male repetition falls MORE than female (-0.86 vs -0.61). The
# female-specific, secondary-specific increments are ~0. Education margin =
# credible bounded NULL (female dropout CI ~ +/-0.45pp contains the whole
# mechanically-implied -0.1..-0.3pp range).
w2 <- dcast(pan, adm3_pcode + year_end ~ nivel + sexo, value.var=c("mat","rep"))
w2[, `:=`(logmat_M_sec  = log(mat_SECUNDARIO_M),
          logmat_F_prim = log(mat_PRIMARIO_F),
          logmat_M_prim = log(mat_PRIMARIO_M),
          rep_M_sec     = 100*rep_SECUNDARIO_M/mat_SECUNDARIO_M,
          rep_F_prim    = 100*rep_PRIMARIO_F/mat_PRIMARIO_F)]
est2 <- merge(w2, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
runB <- function(yvar, lab){
  d <- copy(est2); d[, g_edu := fifelse(g==0L, 0L, g + 1L)]
  d[, id := as.integer(factor(adm3_pcode))]; d <- d[!is.na(get(yvar)) & is.finite(get(yvar))]
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group", na.rm=TRUE)
  data.table(outcome=lab, ATT=round(gg$overall.att,4), SE=round(gg$overall.se,4),
             p=round(2*pnorm(-abs(gg$overall.att/gg$overall.se)),3),
             leads_avg=NA_real_, leads_sig=NA_integer_)
}
B <- rbindlist(list(
  runB("logmat_M_sec",  "PLACEBO log M enrollment SECONDARY"),
  runB("logmat_F_prim", "PLACEBO log F enrollment PRIMARY"),
  runB("logmat_M_prim", "PLACEBO log M enrollment PRIMARY"),
  runB("rep_M_sec",     "PLACEBO M repetition SECONDARY (pp)"),
  runB("rep_F_prim",    "PLACEBO F repetition PRIMARY (pp)")))
cat("\n==================== BLOCK B: PLACEBO / SEX-SPECIFICITY CHECKS ====================\n")
print(B, class=FALSE)
fwrite(rbind(R, B), file.path(TAB,"educ_cs_results.csv"))
cat("educ_cs_results.csv updated with placebo block\n")

# ---- BLOCK C: F-M DDD for enrollment and repetition (MM 2026-07-29) ----
# The raw log-enrollment (+2.1%) and repetition (-0.61) block-A results failed
# the male/primary placebos; the F-M DDD is the formal within-muni-year netting
# of that common trend. DDD outcomes: log(F/M) enrollment ratio; F-M repetition.
secC <- dcast(pan[nivel=="SECUNDARIO"], adm3_pcode + year_end ~ sexo,
              value.var=c("mat","rep"))
secC[, `:=`(ddd_logmat = log(mat_F) - log(mat_M),
            ddd_rep    = 100*rep_F/mat_F - 100*rep_M/mat_M)]
estC <- merge(secC, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
runC <- function(yvar, lab){
  d <- copy(estC); d[, g_edu := fifelse(g==0L, 0L, g + 1L)]
  d[, id := as.integer(factor(adm3_pcode))]; d <- d[is.finite(get(yvar))]
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  pre <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,-2)]
  data.table(outcome=lab, ATT=round(gg$overall.att,4), SE=round(gg$overall.se,4),
             p=round(2*pnorm(-abs(gg$overall.att/gg$overall.se)),3),
             leads_avg=round(pre[, sum(att/se^2)/sum(1/se^2)],4),
             leads_sig=pre[, sum(2*pnorm(-abs(att/se)) < .05, na.rm=TRUE)])
}
Cc <- rbindlist(list(
  runC("ddd_logmat", "F-M log-enrollment DDD (log ratio)"),
  runC("ddd_rep",    "F-M repetition DDD (pp)")))
cat("\n==================== BLOCK C: F-M DDD, enrollment + repetition ====================\n")
print(Cc, class=FALSE)
fwrite(rbind(R, B, Cc, fill=TRUE), file.path(TAB,"educ_cs_results.csv"))
cat("educ_cs_results.csv updated with F-M DDD (enrollment, repetition)\n")

# ---- BLOCK D: PRESENTATION — CS by sex, dropout/repetition/enrollment/overage ----
# DDD (blocks A/C) DEMOTED to a footnote: AUs serve BOTH sexes (§59.6), so males
# are not a placebo and F-M differencing can net out a real effect. Present the
# straight CS for FEMALE and MALE separately.
# OVERAGE: the 2018-19 sheet shipped with no Sobreedad column. Impute the
# year_end=2019 overage RATE by linear interpolation of 2018 and 2020 per
# muni x sex (MM 2026-07-29). SAFE: with g_edu=first_year+1 and all cohorts
# opening 2020+, year_end 2019 is PRE-treatment for every cohort, so the
# imputed cell can only enter a lead, never a post-treatment ATT. Unimputed
# 2020-2025 window reported as sensitivity.
secD <- dcast(pan[nivel=="SECUNDARIO"], adm3_pcode + year_end ~ sexo,
              value.var=c("mat","aband","rep","sobre"))
secD[, `:=`(dropout_F=100*aband_F/mat_F, dropout_M=100*aband_M/mat_M,
            rep_F=100*rep_F/mat_F,        rep_M=100*rep_M/mat_M,
            logmat_F=log(mat_F),          logmat_M=log(mat_M),
            over_F=100*sobre_F/mat_F,     over_M=100*sobre_M/mat_M)]
setorder(secD, adm3_pcode, year_end)
# linear interpolation of the 2019 overage rate from 2018 & 2020 (per muni)
for(v in c("over_F","over_M")){
  secD[, tmp := get(v)]
  secD[, y18 := tmp[year_end==2018][1], by=adm3_pcode]
  secD[, y20 := tmp[year_end==2020][1], by=adm3_pcode]
  secD[year_end==2019 & is.na(tmp), (v) := (y18 + y20)/2]
  secD[, c("tmp","y18","y20") := NULL]
}
estD <- merge(secD, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
runD <- function(yvar, lab, ymin=NULL){
  d <- copy(estD); if(!is.null(ymin)) d <- d[year_end>=ymin]
  d[, g_edu := fifelse(g==0L, 0L, g + 1L)]
  d[, id := as.integer(factor(adm3_pcode))]; d <- d[is.finite(get(yvar))]
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  pre <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,-2)]
  base <- d[g>0 & year_end==g-1L, mean(get(yvar), na.rm=TRUE)]
  data.table(outcome=lab, ATT=round(gg$overall.att,3), SE=round(gg$overall.se,3),
             p=round(2*pnorm(-abs(gg$overall.att/gg$overall.se)),3), base_gm1=round(base,2),
             pct=round(100*gg$overall.att/base,1),
             leads_sig=pre[, sum(2*pnorm(-abs(att/se)) < .05, na.rm=TRUE)])
}
D <- rbindlist(list(
  runD("dropout_F","Dropout rate, female (pp)"),   runD("dropout_M","Dropout rate, male (pp)"),
  runD("rep_F","Repetition rate, female (pp)"),     runD("rep_M","Repetition rate, male (pp)"),
  runD("logmat_F","log enrollment, female"),         runD("logmat_M","log enrollment, male"),
  runD("over_F","Overage female (pp, 2019 imputed)"),runD("over_M","Overage male (pp, 2019 imputed)"),
  runD("over_F","Overage female (pp, 2020-25 check)", 2020L),
  runD("over_M","Overage male (pp, 2020-25 check)",   2020L)))
cat("\n==================== BLOCK D: EDUCATION CS BY SEX ====================\n")
print(D, class=FALSE)
fwrite(D, file.path(TAB,"educ_cs_bysex.csv"))
cat("saved -> educ_cs_bysex.csv\n")

# ---- BLOCK E: POOLED (all adolescents) — the right estimand (MM 2026-07-29) ----
# AUs are a WHOLE-ADOLESCENT intervention (pediatria, medicina/terapia familiar,
# psicologia clinica, ginecologia — per SNS materials), serving both sexes. The
# pooled secondary outcome is the main estimand; F/M shown alongside. Leads
# reported because for enrollment the pre-trend decides real-vs-artifact.
secE <- dcast(pan[nivel=="SECUNDARIO"], adm3_pcode + year_end ~ .,
              value.var=c("mat","aband","rep","sobre"),
              fun.aggregate=sum, na.rm=TRUE)
# sobre missing in 2019 -> sum() returned 0; restore NA then interpolate rate
secE[year_end==2019L, sobre := NA_real_]
secE[, `:=`(dropout=100*aband/mat, repetition=100*rep/mat, logmat=log(mat),
            over=100*sobre/mat)]
setorder(secE, adm3_pcode, year_end)
secE[, y18 := over[year_end==2018][1], by=adm3_pcode]
secE[, y20 := over[year_end==2020][1], by=adm3_pcode]
secE[year_end==2019 & is.na(over), over := (y18+y20)/2]
secE[, c("y18","y20") := NULL]
estE <- merge(secE, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
runE <- function(yvar, lab){
  d <- copy(estE); d[, g_edu := fifelse(g==0L, 0L, g + 1L)]
  d[, id := as.integer(factor(adm3_pcode))]; d <- d[is.finite(get(yvar))]
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  ev <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)
  pre <- ev[e %between% c(-5,-2)]
  base <- d[g>0 & year_end==g-1L, mean(get(yvar), na.rm=TRUE)]
  list(row=data.table(outcome=lab, ATT=round(gg$overall.att,3), SE=round(gg$overall.se,3),
             p=round(2*pnorm(-abs(gg$overall.att/gg$overall.se)),3), base=round(base,2),
             pct=round(100*gg$overall.att/base,1),
             lead_p=round(2*pnorm(-abs(pre[,sum(att/se^2)/sum(1/se^2)]/sqrt(1/pre[,sum(1/se^2)]))),3),
             leads_sig=pre[, sum(2*pnorm(-abs(att/se))<.05, na.rm=TRUE)]),
       dyn=data.table(outcome=lab, ev))
}
EE <- lapply(c(dropout="dropout", repetition="repetition", enrollment="logmat", overage="over"),
             function(v) runE(v, v))
poolT <- rbindlist(lapply(EE, `[[`, "row"))
cat("\n==================== BLOCK E: POOLED (ALL ADOLESCENTS) SECONDARY ====================\n")
print(poolT, class=FALSE)
cat("\nlog-enrollment dynamic path (pre-trend check):\n")
print(EE$enrollment$dyn[e %between% c(-4,4), .(e, att=round(att,3), se=round(se,3),
      z=round(att/se,2))], class=FALSE)
fwrite(poolT, file.path(TAB,"educ_cs_pooled.csv"))
fwrite(rbindlist(lapply(EE, `[[`, "dyn")), file.path(TAB,"educ_cs_pooled_dynamics.csv"))
cat("saved -> educ_cs_pooled.csv + educ_cs_pooled_dynamics.csv\n")
