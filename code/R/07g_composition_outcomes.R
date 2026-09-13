# ============================================================================
# 07g_composition_outcomes.R — EXPLORATORY CS on teen-birth COMPOSITION outcomes
# (education >=secondary, in-union, prenatal anc4/anc_early), 15-19 births, muni-year.
# *** HEAVY CAVEATS (these are NOT clean causal outcomes) ***
#  (1) SELECTION: measured only among teen BIRTHS, an endogenous sample — if AUs
#      change WHO gives birth, the mean shifts mechanically (composition, not a real
#      change in anyone's schooling/union status).
#  (2) COVERAGE: educ 2020+, union/prenatal 2021+ → most cohorts have a tiny/no
#      pre-period. We restrict to the coverage window and drop cohorts treated before
#      a pre-period exists. Few treated cohorts, short panel → underpowered.
# Same CS spec otherwise (not-yet controls, universal base, municipio-clustered boot).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722)
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)

b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
teen <- b[age_grp=="15-19" & !is.na(adm3_pcode)]
teen[, id155 := to155(adm3_pcode)]
teen[, educ_sec := fifelse(is.na(educ), NA_integer_, as.integer(educ %in% c("Secondary","University")))]
comp <- teen[, .(educ_secplus = mean(educ_sec, na.rm=TRUE),
                 in_union     = mean(in_union, na.rm=TRUE),
                 anc4         = mean(anc4, na.rm=TRUE),
                 anc_early    = mean(anc_early, na.rm=TRUE)),
             by=.(adm3_pcode=id155, year=birth_year)]
for (c in c("educ_secplus","in_union","anc4","anc_early")) comp[is.nan(get(c)), (c):=NA_real_]

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
key <- unique(p[, .(adm3_pcode, year, ever_treated, always_treated, first_year)])
d0 <- merge(key, comp, by=c("adm3_pcode","year"), all.x=TRUE)
d0 <- d0[always_treated==0]
d0[, id := as.integer(factor(adm3_pcode))]
d0[, g  := fifelse(ever_treated==1L, as.integer(first_year), 0L)]

run_comp <- function(yname, Y0){
  d <- d0[year>=Y0 & (g==0 | g>Y0)]                 # coverage window; drop cohorts w/o a pre-period
  ntreat <- uniqueN(d[g>0, id])
  a  <- tryCatch(suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
          xformla=~1, data=d, control_group="notyettreated", allow_unbalanced_panel=TRUE,
          bstrap=TRUE, cband=TRUE, biters=1000, base_period="universal", clustervars="id", est_method="reg"))),
        error=function(e) e)
  if (inherits(a,"error")) return(data.table(outcome=yname, window=paste0(Y0,"-2025"), n_treated_cohorts_munis=ntreat,
                                              ATT=NA_real_, SE=NA_real_, p=NA_real_, base=NA_real_, note=substr(conditionMessage(a),1,40)))
  gr <- suppressWarnings(suppressMessages(aggte(a, type="group", na.rm=TRUE)))
  base <- mean(d[g==0][[yname]], na.rm=TRUE)
  data.table(outcome=yname, window=paste0(Y0,"-2025"), n_treated_cohorts_munis=ntreat,
             ATT=round(gr$overall.att,3), SE=round(gr$overall.se,3),
             p=round(2*pnorm(-abs(gr$overall.att/gr$overall.se)),3), base=round(base,3),
             pct_of_base=round(100*gr$overall.att/base,1))
}
cat("==================== 07g EXPLORATORY composition outcomes (teen 15-19 births) ====================\n")
res <- rbindlist(list(run_comp("educ_secplus",2020L), run_comp("in_union",2021L),
                      run_comp("anc4",2021L), run_comp("anc_early",2021L)), fill=TRUE)
print(res, class=FALSE)
cat("\n*** SELECTION + short-window caveats: composition descriptors, NOT clean causal effects. Exploratory only. ***\n")
saveRDS(res, file.path(DIR_CLEAN,"composition_outcomes_cs.rds"))
