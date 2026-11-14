# =============================================================================
# 09o_cs_small_sample_inference.R — small-sample inference for the headline
# Callaway--Sant'Anna group ATT (first-opening design, 20 treated of 146
# municipalities): a check made for few treated clusters and applied to the
# main estimator itself (committee comment, 2026-11-14).
#
#     CLUSTER JACKKNIFE, Karim, Nielsen, MacKinnon and Webb (2026,
#     arXiv:2602.12043), their eq. (20):
#       CV3 = ((H-1)/H) * sum_h (ATT_(h) - ATT)^2 ,
#     where ATT_(h) re-estimates every ATT(g,t) and the aggregate with
#     municipality h omitted, centred at the full-sample estimate. All H = 146
#     municipalities are omitted in turn, treated and untreated. The paper does
#     not state a reference distribution; p-values here use t(H-1), the
#     convention it cites for cluster-robust t-statistics.
#     Caveat stated in that paper: when omitting a cluster removes a whole
#     cohort, the aggregate re-weights over the remaining cohorts. Two cohorts
#     here are single municipalities (2019 and 2025); their delete-one
#     estimates are flagged in the output.
#
# Expectation stated before running (rule 1b): a wider interval than the
# multiplier bootstrap (SE 2.01), with the estimate still distinguishable
# from zero at conventional levels.
#
# Output: output/tables/cs_small_sample_inference.csv (summary),
#         cs_jackknife_deleteone.csv
# =============================================================================
source(here::here("code", "R", "00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(parallel)})
NC <- max(1L, min(8L, detectCores() - 2L))     # est() uses no random numbers, so parallel runs are reproducible

fold <- c("DOM010905", "DOM051703", "DOM012510")
p  <- as.data.table(readRDS(file.path(DIR_CLEAN, "panel_muni_year_155.rds")))[!adm3_pcode %in% fold,
        .(adm3_pcode, year, rateA_15_19)]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN, "treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated == 1L & always_treated == 0L, as.integer(first_year),
                  fifelse(ever_treated == 0L, 0L, NA_integer_))]
gm <- tr[!is.na(g), .(adm3_pcode, g)]
stopifnot(nrow(gm) == 146L, gm[g > 0, .N] == 20L)
d0 <- merge(p, gm, by = "adm3_pcode"); stopifnot(nrow(d0) == 1460L)

# group ATT with its analytic (influence-function, municipality-clustered) SE
est <- function(d) {
  d <- copy(d); d[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(
    yname = "rateA_15_19", tname = "year", idname = "id", gname = "g",
    xformla = ~1, data = d, control_group = "notyettreated",
    bstrap = FALSE, cband = FALSE, base_period = "universal",
    clustervars = "id", est_method = "reg", print_details = FALSE)))
  o <- suppressWarnings(suppressMessages(aggte(a, type = "group", na.rm = TRUE, bstrap = FALSE)))
  c(att = unname(o$overall.att), se = unname(o$overall.se))
}

full <- est(d0)
hl <- fread(file.path(DIR_TABLES, "headline_S1_S2.csv"))[scenario == "S1" & spec == "CS, no covs"]
stopifnot(abs(round(full[["att"]], 2) - hl$estimate) < 1e-9)      # same point estimate as Table 3
cat(sprintf("full sample: ATT %.4f | analytic SE %.4f | multiplier-bootstrap SE (Table 3) %.2f\n",
            full[["att"]], full[["se"]], hl$SE))

## ---- cluster jackknife ---------------------------------------------------
H  <- nrow(gm)
jk <- rbindlist(mclapply(seq_len(H), function(h) {
  z <- est(d0[adm3_pcode != gm$adm3_pcode[h]])
  data.table(dropped_pcode = gm$adm3_pcode[h], g = gm$g[h], ATT = z[["att"]])
}, mc.cores = NC))
stopifnot(nrow(jk) == H, all(is.finite(jk$ATT)))
ncoh <- gm[g > 0, .N, by = g]
jk[, removes_cohort := g %in% ncoh[N == 1L, g]]
cv3   <- (H - 1) / H * sum((jk$ATT - full[["att"]])^2)
se_jk <- sqrt(cv3); t_jk <- full[["att"]] / se_jk
p_jk  <- 2 * pt(-abs(t_jk), df = H - 1)
ci_jk <- full[["att"]] + c(-1, 1) * qt(0.975, H - 1) * se_jk
# cross-check: the 20 treated delete-one values must equal 09n's
loo <- fread(file.path(DIR_TABLES, "leave_one_out_treated.csv"))
chk <- merge(jk[g > 0, .(dropped_pcode, ATT)], loo[, .(dropped_pcode, ATT09n = ATT)], by = "dropped_pcode")
stopifnot(nrow(chk) == 20L, max(abs(chk$ATT - chk$ATT09n)) < 1e-8)
# share of the jackknife variance that comes from treated vs untreated municipalities
sh_tr <- sum((jk[g > 0]$ATT - full[["att"]])^2) / sum((jk$ATT - full[["att"]])^2)
cat(sprintf("jackknife: SE %.3f | t %.2f | p (t, %d df) %.4f | 95%% CI [%.2f, %.2f] | treated share of variance %.2f\n",
            se_jk, t_jk, H - 1, p_jk, ci_jk[1], ci_jk[2], sh_tr))
fwrite(jk, file.path(DIR_TABLES, "cs_jackknife_deleteone.csv"))

t0 <- full[["att"]] / full[["se"]]
out <- rbind(
  data.table(method = "Multiplier bootstrap (Table 3)", estimate = hl$estimate, se = hl$SE,
             lo = hl$estimate - 1.96 * hl$SE, hi = hl$estimate + 1.96 * hl$SE, p = hl$p,
             note = "municipality-clustered, 2,000 draws"),
  data.table(method = "Analytic clustered", estimate = full[["att"]], se = full[["se"]],
             lo = full[["att"]] - 1.96 * full[["se"]], hi = full[["att"]] + 1.96 * full[["se"]],
             p = 2 * pnorm(-abs(t0)), note = "influence function"),
  data.table(method = "Cluster jackknife (CV3)", estimate = full[["att"]], se = se_jk,
             lo = ci_jk[1], hi = ci_jk[2], p = p_jk,
             note = sprintf("H=%d delete-one re-estimations; t(%d); treated share of variance %.2f", H, H - 1, sh_tr)))
out[, `:=`(estimate = round(estimate, 4), se = round(se, 4), lo = round(lo, 4), hi = round(hi, 4), p = round(p, 4))]
print(out, class = FALSE)
fwrite(out, file.path(DIR_TABLES, "cs_small_sample_inference.csv"))
cat("saved -> cs_small_sample_inference.csv, cs_jackknife_deleteone.csv\n")
