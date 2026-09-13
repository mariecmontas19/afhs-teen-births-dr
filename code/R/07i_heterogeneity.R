# ============================================================================
# 07i_heterogeneity.R — heterogeneity of the teen-rate effect by MUNICIPIO-LEVEL
# BASELINE (pre-determined) moderators. Het by INDIVIDUAL maternal traits is NOT
# done here (selection + no group-specific denominators); moderators are municipality
# baselines so the split is clean.
# Design for few treated clusters: split the 18 treated at THEIR median on each
# moderator (~9 hi / 9 lo); controls = the shared NOT-YET + NEVER-treated pool
# (control_group="notyettreated") in both halves. CS group ATT, denom A, outcome
# rateA_15_19. Difference test assumes independent subgroups. Also reports the more
# powerful TWFE continuous interaction (effect per +1 SD moderator; TWFE bias caveat).
# *** STRONG CAVEAT: ~9 treated clusters per subgroup -> very wide CIs; het is
#     EXPLORATORY/directional, not definitive. ***
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(fixest)})
set.seed(20260722)

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
est <- p[always_treated==0]; est[, id := as.integer(factor(adm3_pcode))]
est[, g := fifelse(ever_treated==1L, as.integer(first_year), 0L)]
est[, base_teenrate := mean(rateA_15_19[year %in% 2016:2019]), by=adm3_pcode]   # pre-period teen rate (time-invariant)

mods <- c(`baseline teen fertility (pre-period rate)*`="base_teenrate",
          `teen fertility (DHS 2013, independent)`="dhs_teenasfr_2013",
          `baseline fertility (CWR 2010, independent)`="cwr_2010_baseline",
          `SES (wealth index)`="wealth_index",
          `urban`="pct_urban",
          `baseline union prevalence`="pct_in_union",
          `baseline insurance (DHS 2013)`="pct_insured_2013",
          `baseline education (>=primary)`="pct_educ_primplus")

cs <- function(dat){
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
        xformla=~1, data=dat, control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=1000,
        base_period="universal", clustervars="id", est_method="reg")))
  gr <- suppressWarnings(suppressMessages(aggte(a, type="group", na.rm=TRUE)))
  c(gr$overall.att, gr$overall.se)
}

rows <- list()
for (nm in names(mods)) {
  m <- mods[[nm]]
  medt <- median(unique(est[ever_treated==1, .(adm3_pcode, v=get(m))])$v)
  est[, sub := fcase(ever_treated==1L & get(m) >= medt, "hi",
                     ever_treated==1L & get(m) <  medt, "lo", default="never")]
  nhi <- uniqueN(est[sub=="hi", id]); nlo <- uniqueN(est[sub=="lo", id])
  hi <- cs(est[sub %in% c("hi","never")]); lo <- cs(est[sub %in% c("lo","never")])
  dif <- hi[1]-lo[1]; sed <- sqrt(hi[2]^2 + lo[2]^2)
  rows[[nm]] <- data.table(moderator=nm, n_trt_lo=nlo, ATT_low=round(lo[1],2), SE_low=round(lo[2],2),
                           n_trt_hi=nhi, ATT_high=round(hi[1],2), SE_high=round(hi[2],2),
                           diff_hi_minus_lo=round(dif,2), p_diff=round(2*pnorm(-abs(dif/sed)),3))
}
out <- rbindlist(rows)
cat("==================== 07i HETEROGENEITY — teen rate 15-19 (denom A) ====================\n")
cat("ATT within municipalities with LOW vs HIGH baseline moderator (treated split at treated-median; all 128 never = controls).\n")
cat("Headline pooled ATT = -5.32. *baseline-rate split uses the outcome's own pre-period (mild mean-reversion caveat).\n\n")
print(out, class=FALSE)
cat("\n*** ~9 treated clusters per subgroup -> CIs wide; het is EXPLORATORY/directional. p_diff = test of equal subgroup ATTs. ***\n")

## ---- TWFE continuous interaction (more powerful; TWFE staggered-bias caveat) ----
est[, postd := as.integer(ever_treated==1L & year>=as.integer(first_year))]
twfe <- rbindlist(lapply(names(mods), function(nm){
  est[, mz := scale(get(mods[[nm]]))[,1]]
  fit <- feols(rateA_15_19 ~ postd + postd:mz | adm3_pcode + year, data=est, cluster=~adm3_pcode)
  ct <- fit$coeftable
  data.table(moderator=nm, het_slope_perSD=round(ct["postd:mz","Estimate"],2),
             SE=round(ct["postd:mz","Std. Error"],2), p=round(ct["postd:mz","Pr(>|t|)"],3))
}))
cat("\n--- TWFE continuous interaction (ATT change per +1 SD of moderator; cluster=municipality; TWFE-bias caveat) ---\n")
print(twfe, class=FALSE)
saveRDS(list(cs_split=out, twfe_cont=twfe), file.path(DIR_CLEAN,"heterogeneity_07i.rds"))
