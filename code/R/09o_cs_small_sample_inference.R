# =============================================================================
# 09o_cs_small_sample_inference.R — small-sample inference for the headline
# Callaway--Sant'Anna group ATT (first-opening design, 20 treated of 146
# municipalities). Two checks made for few treated clusters and applied to the
# main estimator itself (committee comment, 2026-11-14):
#
# (1) CLUSTER JACKKNIFE, Karim, Nielsen, MacKinnon and Webb (2026,
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
# (2) PERMUTATION (randomization) TEST, MacKinnon and Webb (2020): the 20
#     observed opening years are reassigned at random across the 146
#     municipalities (the whole cohort vector is permuted), B = 1,999 draws.
#     p-values from the estimate (RI-beta) and from its t-ratio with the
#     analytic clustered standard error (RI-t). The test treats placement as
#     random; actual placement followed population size (Table 2).
#
# Expectations stated before running (rule 1b): both checks give wider
# intervals than the multiplier bootstrap (SE 2.01) and the estimate remains
# distinguishable from zero at conventional levels.
#
# Output: output/tables/cs_small_sample_inference.csv (summary),
#         cs_jackknife_deleteone.csv, cs_permutation_draws.csv
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

## ---- (1) cluster jackknife ---------------------------------------------------
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

## ---- (2) permutation test ----------------------------------------------------
B <- 1999L
set.seed(20260722)
gperm <- lapply(seq_len(B), function(b) sample(gm$g))      # all permutations drawn first, from one seed
perm <- rbindlist(mclapply(seq_len(B), function(b) {
  gp <- copy(gm)[, g := gperm[[b]]]                # permute the whole cohort vector
  z  <- tryCatch(est(merge(p, gp, by = "adm3_pcode")), error = function(e) c(att = NA_real_, se = NA_real_))
  data.table(draw = b, ATT = z[["att"]], SE = z[["se"]])
}, mc.cores = NC))
stopifnot(nrow(perm) == B)
fwrite(perm, file.path(DIR_TABLES, "cs_permutation_draws.csv"))
ok <- perm[is.finite(ATT) & is.finite(SE) & SE > 0]
t0 <- full[["att"]] / full[["se"]]
p_beta <- (1 + sum(abs(ok$ATT) >= abs(full[["att"]]))) / (nrow(ok) + 1)
p_t    <- (1 + sum(abs(ok$ATT / ok$SE) >= abs(t0))) / (nrow(ok) + 1)
cat(sprintf("permutation: %d of %d draws usable | placebo ATT mean %.3f, sd %.3f | p (estimate) %.4f | p (t-ratio) %.4f\n",
            nrow(ok), B, mean(ok$ATT), sd(ok$ATT), p_beta, p_t))

out <- rbind(
  data.table(method = "Multiplier bootstrap (Table 3)", estimate = hl$estimate, se = hl$SE,
             lo = hl$estimate - 1.96 * hl$SE, hi = hl$estimate + 1.96 * hl$SE, p = hl$p,
             note = "municipality-clustered, 2,000 draws"),
  data.table(method = "Analytic clustered", estimate = full[["att"]], se = full[["se"]],
             lo = full[["att"]] - 1.96 * full[["se"]], hi = full[["att"]] + 1.96 * full[["se"]],
             p = 2 * pnorm(-abs(t0)), note = "influence function"),
  data.table(method = "Cluster jackknife (CV3)", estimate = full[["att"]], se = se_jk,
             lo = ci_jk[1], hi = ci_jk[2], p = p_jk,
             note = sprintf("H=%d delete-one re-estimations; t(%d); treated share of variance %.2f", H, H - 1, sh_tr)),
  data.table(method = "Permutation test, estimate", estimate = full[["att"]], se = NA_real_, lo = NA_real_, hi = NA_real_,
             p = p_beta, note = sprintf("%d usable draws of %d", nrow(ok), B)),
  data.table(method = "Permutation test, t-ratio", estimate = full[["att"]], se = NA_real_, lo = NA_real_, hi = NA_real_,
             p = p_t, note = sprintf("%d usable draws of %d", nrow(ok), B)))
out[, `:=`(estimate = round(estimate, 4), se = round(se, 4), lo = round(lo, 4), hi = round(hi, 4), p = round(p, 4))]
print(out, class = FALSE)
fwrite(out, file.path(DIR_TABLES, "cs_small_sample_inference.csv"))
cat("saved -> cs_small_sample_inference.csv, cs_jackknife_deleteone.csv, cs_permutation_draws.csv\n")
