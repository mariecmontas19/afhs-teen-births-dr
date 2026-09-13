# ============================================================================
# 07q_scenarios.R — run ALL treatment-definition scenarios on the FINAL data and
# put them in one comparison matrix. CS (did::att_gt), denom A, not-yet-treated
# controls, municipality-clustered, base_period universal. Outcomes: level teen rate
# 15-19 (rateA_15_19) and age-DDD (ddd_A). Treatment defs built from the single
# source of truth notes/pcua_unit_dates_verified.csv (+ binary from 04).
#   S1 Any unit ever (binary, original)
#   S2 Modernization - all modern units (new+upgrade+consults pooled)
#   S3 Real integral unit only (consults-only units excluded; their lone municipalities -> controls)
#   S4 Real NEW openings only      S5 Real UPGRADES only
# Output: output/tables/cs_scenarios.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); TAB <- file.path(PROJ,"output","tables")
IMP <- 2023L

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19, ddd_A, wealth_index, pct_urban, pct_educ_secplus)]
p   <- merge(p, as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)], by="adm3_pcode", all.x=TRUE)
# Headline-CS baseline covariate set (MM 2026-06-25): wealth + public-services + urban + education,
# all time-invariant baselines. (DDD headline uses NO covariates — see 07s + notes §12.)
XF  <- ~ wealth_index + sns_per10k + pct_urban + pct_educ_secplus
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[, .(adm3_pcode, first_year, ever_bin=ever_treated, always_bin=always_treated)]
v   <- as.data.table(fread(file.path(PROJ,"notes","pcua_unit_dates_verified.csv")))

# ---- per-unit years (all units are in non-fold pcodes -> map straight to 155) ----
v[, mod_year := as.integer(substr(modern_event_date,1,4))]
v[is.na(mod_year), mod_year := recorded_year]
v[is.na(mod_year), mod_year := IMP]
v[, realunit_year := fifelse(consults_only==TRUE, NA_integer_, mod_year)]
v[, tsimpl := fcase(modern_event_type=="new_opening","new",
                    modern_event_type %in% c("upgrade","upgrade_unconfirmed","old_never_modernized"),"upgrade",
                    default="new")]
# ---- municipality-level cohorts ----
muni <- v[, {
  ru <- realunit_year[!is.na(realunit_year)]; rt <- tsimpl[!is.na(realunit_year)]
  .(mod_cohort     = min(mod_year),
    realunit_cohort= if (length(ru)) min(ru) else NA_integer_,
    realunit_type  = if (length(ru)) rt[which.min(ru)] else NA_character_)
}, by=adm3_pcode]

# ---- assemble municipality table over the 155 panel municipalities ----
M <- merge(data.table(adm3_pcode=unique(p$adm3_pcode)), bin,  by="adm3_pcode", all.x=TRUE)
M <- merge(M, muni, by="adm3_pcode", all.x=TRUE)
M[is.na(ever_bin),  ever_bin  := 0L]; M[is.na(always_bin), always_bin := 0L]
cat("panel municipalities:", nrow(M),
    "| binary in-window:", M[ever_bin==1 & always_bin==0,.N],
    "| modern (any):", M[!is.na(mod_cohort),.N],
    "| real-unit:", M[!is.na(realunit_cohort),.N],
    "| real-new:", sum(M$realunit_type=="new", na.rm=TRUE),
    "| real-upgrade:", sum(M$realunit_type=="upgrade", na.rm=TRUE), "\n")

P <- merge(p, M, by="adm3_pcode")

# ---- generic CS runner on a prepared estimation set with column g ----
runCS <- function(est, yname, xf){
  est <- copy(est); est[, idn := as.integer(factor(adm3_pcode))]
  stopifnot(nrow(est)==uniqueN(est$idn)*uniqueN(est$year))
  a <- att_gt(yname=yname, tname="year", idname="idn", gname="g", xformla=xf, data=est,
              control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=2000,
              base_period="universal", clustervars="idn", est_method="reg", print_details=FALSE)
  o <- aggte(a, type="group", na.rm=TRUE)
  list(att=round(o$overall.att,2), se=round(o$overall.se,2),
       p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3))
}

# ---- build each scenario's estimation set (g = cohort; 0 = control) ----
scen <- list()
# S1 binary any-unit: drop binary always-treated
s1 <- P[always_bin==0]; s1[, g := fifelse(ever_bin==1L, as.integer(first_year), 0L)]; scen[["S1 Any unit (binary)"]] <- s1
# S2 modernization all: drop modern always-treated (mod_cohort<2016)
s2 <- P[is.na(mod_cohort) | mod_cohort>=2016]; s2[, g := fifelse(!is.na(mod_cohort), as.integer(mod_cohort), 0L)]; scen[["S2 Modernization (all)"]] <- s2
# S3 real-unit only: treated = real unit; controls = no real unit (incl consults-only municipalities)
s3 <- P[is.na(realunit_cohort) | realunit_cohort>=2016]; s3[, g := fifelse(!is.na(realunit_cohort), as.integer(realunit_cohort), 0L)]; scen[["S3 Real unit only"]] <- s3
# S4 real NEW only: drop real-upgrade municipalities; treated = real-new; controls = no real unit
s4 <- P[is.na(realunit_cohort) | realunit_type=="new"]; s4[, g := fifelse(!is.na(realunit_cohort) & realunit_type=="new", as.integer(realunit_cohort), 0L)]; scen[["S4 Real NEW only"]] <- s4
# S5 real UPGRADE only: drop real-new municipalities; treated = real-upgrade
s5 <- P[is.na(realunit_cohort) | realunit_type=="upgrade"]; s5[, g := fifelse(!is.na(realunit_cohort) & realunit_type=="upgrade", as.integer(realunit_cohort), 0L)]; scen[["S5 Real UPGRADE only"]] <- s5

out <- rbindlist(lapply(names(scen), function(nm){
  rbindlist(lapply(c("rateA_15_19","ddd_A"), function(y){
    u  <- runCS(scen[[nm]], y, ~1)        # unconditional
    cc <- runCS(scen[[nm]], y, XF)        # conditional: wealth + urban + education
    data.table(scenario=nm, outcome=fifelse(y=="rateA_15_19","Level 15-19","Age-DDD"),
               n_treat=uniqueN(scen[[nm]][g>0,adm3_pcode]),
               ATT_uncond=u$att, SE_uncond=u$se, p_uncond=u$p,
               ATT_cond=cc$att,  SE_cond=cc$se,  p_cond=cc$p)
  }))
}))
cat("\n==================== SCENARIO MATRIX (denom A) ====================\n")
print(out, class=FALSE)
fwrite(out, file.path(TAB,"cs_scenarios.csv"))
cat("\nsaved -> output/tables/cs_scenarios.csv\n")
