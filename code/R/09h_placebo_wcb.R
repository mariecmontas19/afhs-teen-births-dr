# ============================================================================
# 09h_placebo_wcb.R — two inference/design checks on the S1 headline:
#  A. FAKE-TIMING PLACEBO on the pre-treatment window 2016-2019 (all real
#     cohorts open 2020+, so 2016-19 is untreated for everyone). Shift fake
#     treatment 3 years early: g_fake = g - 3 (2020->2017, 2021->2018,
#     2022->2019). Real cohorts 2023+ map to fake 2020+ = OUTSIDE the window;
#     they are NOT recoded to never-treated — did::att_gt internally treats
#     units first-treated after the last sample period as controls, which is
#     exactly their not-yet-treated status in 2016-19. Expected: fake ATT ~ 0.
#     Plus a fake-timing TWFE (post_fake) on the same window.
#  B. WILD CLUSTER BOOTSTRAP (manual CGM, Rademacher, restricted/null-imposed
#     bootstrap-t, B=999) for the static TWFE post coefficient on the FULL
#     2016-2025 panel — the small-cluster-robust p to set beside the analytic
#     cluster-robust p (.114). fwildclusterboot is uninstallable (no gfortran,
#     see CLAUDE.md) -> implemented manually.
# Scenario S1 (first opening): 20 treated / 126 never / 146 municipios.
# Output: output/tables/placebo_wcb.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(fixest)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables")

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold, .(adm3_pcode,year,rateA_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]

d <- merge(p, bin[!is.na(g), .(adm3_pcode,g)], by="adm3_pcode")
d[, id := as.integer(factor(adm3_pcode))]
cat(sprintf("merge check: panel %d rows -> merged %d rows | %d municipios (%d treated, %d never) | NA g dropped: %d\n",
            nrow(p), nrow(d), uniqueN(d$adm3_pcode), d[g>0, uniqueN(adm3_pcode)], d[g==0, uniqueN(adm3_pcode)],
            bin[is.na(g), .N]))
stopifnot(uniqueN(d$adm3_pcode)==146L, d[g>0, uniqueN(adm3_pcode)]==20L, sum(is.na(d$rateA_15_19))==0)

res <- list(); add <- function(block, label, est, se, pv, note="") res[[length(res)+1]] <<-
  data.table(block=block, spec=label, estimate=round(est,3), se=round(se,3), p=round(pv,4), note=note)

# ===========================================================================
# PART A — fake-timing placebo on the pre-treatment window 2016-2019
# ===========================================================================
cat("\n===== A. fake-timing placebo (2016-2019, g_fake = g - 3) =====\n")
dpre <- d[year <= 2019]
dpre[, g_fake := fifelse(g>0, g - 3L, 0L)]
cat(sprintf("placebo window: %d rows, years %d-%d | fake-cohort counts (municipios):\n",
            nrow(dpre), min(dpre$year), max(dpre$year)))
print(dpre[, .(n_muni=uniqueN(adm3_pcode)), keyby=g_fake], class=FALSE)
cat("NOTE: g_fake 2020-2022 (real cohorts 2023+) lie beyond the 2019 window ->\n",
    "      not-yet-treated controls throughout; did recodes them internally.\n")

run_cs_fake <- function(boot){
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g_fake",
        xformla=~1, data=dpre, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=boot, cband=boot, biters=if(boot) 2000 else 0, clustervars="id", print_details=FALSE)))
  gr <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  list(att=gr$overall.att, se=gr$overall.se, p=2*pnorm(-abs(gr$overall.att/gr$overall.se)),
       cohorts=paste(gr$egt, collapse="/"))
}
boot_note <- "bstrap=TRUE, biters=2000"
fk <- tryCatch(run_cs_fake(TRUE), error=function(e){
  cat("bootstrap CS failed on 2016-19 window (", conditionMessage(e), ") -> analytic SE fallback\n")
  boot_note <<- "bootstrap FAILED on small window -> analytic SE"
  run_cs_fake(FALSE) })
if (!is.finite(fk$se)) {  # bootstrap ran but returned NA SE -> analytic fallback
  cat("bootstrap CS returned non-finite SE -> analytic SE fallback\n")
  boot_note <- "bootstrap SE non-finite -> analytic SE"
  fk <- run_cs_fake(FALSE)
}
cat(sprintf("fake CS overall ATT = %.3f (SE %.3f, p %.3f) | estimable fake cohorts: %s | %s\n",
            fk$att, fk$se, fk$p, fk$cohorts, boot_note))
add("A", "CS fake-timing placebo (2016-19, g-3)", fk$att, fk$se, fk$p,
    sprintf("fake cohorts %s; %s", fk$cohorts, boot_note))

# fake-timing TWFE on the same window
dpre[, post_fake := as.integer(g_fake>0 & year>=g_fake)]
cat(sprintf("post_fake=1 cells: %d (of %d rows)\n", sum(dpre$post_fake), nrow(dpre)))
mf <- feols(rateA_15_19 ~ post_fake | id + year, data=dpre, cluster=~id)
cat(sprintf("fake TWFE post coef = %.3f (SE %.3f, p %.3f)\n",
            coef(mf)[["post_fake"]], se(mf)[["post_fake"]], pvalue(mf)[["post_fake"]]))
add("A", "TWFE fake-timing placebo (2016-19, g-3)",
    coef(mf)[["post_fake"]], se(mf)[["post_fake"]], pvalue(mf)[["post_fake"]],
    sprintf("n=%d rows, %d clusters", nobs(mf), uniqueN(dpre$adm3_pcode)))

# ===========================================================================
# PART B — wild cluster bootstrap-t (CGM, Rademacher, null-imposed), B=999
# ===========================================================================
cat("\n===== B. wild cluster bootstrap, static TWFE, full panel 2016-2025 =====\n")
d[, post := as.integer(g>0 & year>=g)]
m <- feols(rateA_15_19 ~ post | id + year, data=d, cluster=~id)
b_hat <- coef(m)[["post"]]; se_hat <- se(m)[["post"]]; t_hat <- b_hat/se_hat
p_analytic <- pvalue(m)[["post"]]
cat(sprintf("TWFE post = %.3f (cluster SE %.3f, t %.3f, analytic p %.4f) | n=%d, clusters=%d\n",
            b_hat, se_hat, t_hat, p_analytic, nobs(m), uniqueN(d$id)))

# (1) restricted model under H0: beta_post = 0  (FE-only)
m0 <- feols(rateA_15_19 ~ 1 | id + year, data=d)
d[, `:=`(fit_r = fitted(m0), res_r = resid(m0))]
stopifnot(max(abs(d$fit_r + d$res_r - d$rateA_15_19)) < 1e-8)

# (2)-(3) B=999 Rademacher reps, weight drawn per MUNICIPALITY cluster
set.seed(20260722)
B <- 999L; cl <- sort(unique(d$id)); nC <- length(cl)
setorder(d, id, year)                       # fix row order so w[id] maps stably
t_star <- vapply(seq_len(B), function(b){
  w <- sample(c(-1,1), nC, replace=TRUE)    # Rademacher, one draw per cluster
  d[, ystar := fit_r + w[id]*res_r]
  mb <- feols(ystar ~ post | id + year, data=d, cluster=~id)
  coef(mb)[["post"]]/se(mb)[["post"]]
}, numeric(1))
stopifnot(length(t_star)==B, all(is.finite(t_star)))

# (4) bootstrap-t p-value
p_wcb <- mean(abs(t_star) >= abs(t_hat))
cat(sprintf("WCB (restricted, Rademacher, B=%d, %d clusters): p = %.4f  [analytic p = %.4f]\n",
            B, nC, p_wcb, p_analytic))
add("B", "TWFE post, analytic cluster-robust", b_hat, se_hat, p_analytic,
    sprintf("t=%.3f, n=%d, clusters=%d", t_hat, nobs(m), nC))
add("B", "TWFE post, wild cluster bootstrap-t", b_hat, NA_real_, p_wcb,
    sprintf("restricted CGM Rademacher, B=%d, n_clusters=%d", B, nC))

# ---- save -----------------------------------------------------------------
out <- rbindlist(res); print(out, class=FALSE)
fwrite(out, file.path(TAB,"placebo_wcb.csv"))
cat("\nsaved -> output/tables/placebo_wcb.csv\n")
