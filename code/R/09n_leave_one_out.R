# =============================================================================
# 09n_leave_one_out.R — influence of each first-opening municipality on the
# headline Callaway--Sant'Anna group ATT.
#
# Each specification drops one of the 20 treated municipalities, re-estimates
# the level effect with not-yet-treated comparisons, and records the group ATT.
# Bootstrap inference is unnecessary for this influence diagnostic: the point
# estimate is identical whether the multiplier bootstrap is requested or not.
# Output: output/tables/leave_one_out_treated.csv
# =============================================================================
source(here::here("code", "R", "00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})

fold <- c("DOM010905", "DOM051703", "DOM012510")
p <- as.data.table(readRDS(file.path(DIR_CLEAN, "panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN, "treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated == 1L & always_treated == 0L, as.integer(first_year),
                  fifelse(ever_treated == 0L, 0L, NA_integer_))]

d0 <- merge(p, tr[!is.na(g), .(adm3_pcode, g)], by = "adm3_pcode")
d0[, id := as.integer(factor(adm3_pcode))]
treated <- tr[g > 0, .(adm3_pcode, adm3_name, prov_norm, cohort = g)]
stopifnot(nrow(treated) == 20L)

estimate_att <- function(d) {
  d <- copy(d)
  d[, id := as.integer(factor(adm3_pcode))]
  a <- suppressMessages(att_gt(
    yname = "rateA_15_19", tname = "year", idname = "id", gname = "g",
    xformla = ~1, data = d, control_group = "notyettreated",
    bstrap = FALSE, cband = FALSE, base_period = "universal",
    clustervars = "id", est_method = "reg"
  ))
  gr <- suppressMessages(aggte(a, type = "group", na.rm = TRUE, bstrap = FALSE))
  unname(gr$overall.att)
}

full <- estimate_att(d0)
loo <- rbindlist(lapply(seq_len(nrow(treated)), function(i) {
  m <- treated[i]
  z <- estimate_att(d0[adm3_pcode != m$adm3_pcode])
  data.table(
    dropped_pcode = m$adm3_pcode,
    dropped_municipality = m$adm3_name,
    province = m$prov_norm,
    cohort = m$cohort,
    ATT = z
  )
}))
loo[, `:=`(
  change_from_full = ATT - full
)]
setorder(loo, ATT)

fwrite(loo, file.path(DIR_TABLES, "leave_one_out_treated.csv"))
cat(sprintf("Full-sample ATT: %.3f\n", full))
cat(sprintf("Leave-one-out range: [%.3f, %.3f]; all negative: %s\n",
            min(loo$ATT), max(loo$ATT), all(loo$ATT < 0)))
print(loo[order(-abs(change_from_full))][1:5], class = FALSE)
cat("saved -> output/tables/leave_one_out_treated.csv\n")
