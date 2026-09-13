# ============================================================================
# 09l_bacon.R — Goodman-Bacon decomposition of the static TWFE benchmark
# (paper-plan pre-draft item; standard referee request when TWFE is shown next
# to CS). PREDICTION (rule 1b, §59.2): weights dominated by treated-vs-never
# comparisons (126 never vs 20 late-staggered treated), small later-vs-earlier
# ("forbidden") share — explaining why TWFE ~ CS here.
# Balanced panel 146 x 2016-2025; treatment = absorbing post indicator.
# Output: output/tables/bacon_decomposition.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(bacondecomp)})
fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold,
       .(adm3_pcode, year, rateA_15_19)]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
d <- merge(p, tr[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
d[, post := as.integer(g > 0 & year >= g)]
d[, id := as.integer(factor(adm3_pcode))]
cat("panel:", nrow(d), "rows,", d[,uniqueN(id)], "munis, balanced:",
    d[, .N, by=id][, uniqueN(N)]==1, "\n")

bd <- bacon(rateA_15_19 ~ post, data = as.data.frame(d), id_var = "id", time_var = "year")
bd <- as.data.table(bd)
summ <- bd[, .(weight = sum(weight), avg_est = weighted.mean(estimate, weight)), by = type]
summ[, weight := round(weight, 4)][, avg_est := round(avg_est, 2)]
cat("\n=== Goodman-Bacon decomposition (static TWFE) ===\n")
print(summ, class=FALSE)
cat(sprintf("implied TWFE = %.2f (sum of weight x estimate)\n", bd[, sum(weight*estimate)]))
fwrite(bd,   file.path(TAB,"bacon_decomposition_full.csv"))
fwrite(summ, file.path(TAB,"bacon_decomposition.csv"))
cat("saved -> bacon_decomposition{,_full}.csv\n")
