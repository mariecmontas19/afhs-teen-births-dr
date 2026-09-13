# ============================================================================
# 08q_muni_decomposition.R — MUNICIPALITY-LEVEL DECOMPOSITION of the CS headline
# (MM 2026-07-10): which treated municipalities drop the most, in the
# estimator's own metric (baseline = each municipality's OWN g-1, controls =
# not-yet-treated in the same calendar years — mirroring att_gt with
# est_method="reg", control_group="notyettreated", base_period="universal").
# For treated municipality m with cohort g:
#   att_m = mean_{t>=g, t<=2025} [ (Y_mt - Y_m,g-1) - (Ybar_ctrl(t),t - Ybar_ctrl(t),g-1) ]
# where ctrl(t) = municipalities not yet treated at t (g'=0 or g'>t), m excluded.
# VALIDATION: the cohort-size-weighted average of att_m must be ~ the CS group
# ATT (-6.43); exact equality is not guaranteed (CS reg uses regression
# adjustment + cohort weights), so the check is printed.
# Output: output/tables/muni_decomposition.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold,
       .(adm3_pcode, year, rate=rateA_15_19)]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[, .(adm3_pcode, municipality=adm3_name, province=prov_name)]
d <- merge(merge(p, tr[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode"), xw, by="adm3_pcode")

treated <- d[g>0, unique(.SD), .SDcols=c("adm3_pcode","g","municipality","province")]
rows <- lapply(seq_len(nrow(treated)), function(i){
  m <- treated[i]
  ym <- d[adm3_pcode==m$adm3_pcode]
  base_m <- ym[year==m$g-1L, rate]
  post_yrs <- (m$g):2025
  contribs <- sapply(post_yrs, function(t){
    ctrl <- d[adm3_pcode != m$adm3_pcode & (g==0L | g>t)]          # not-yet-treated at t
    cb <- ctrl[year==m$g-1L, mean(rate)]; ct <- ctrl[year==t, mean(rate)]
    (ym[year==t, rate] - base_m) - (ct - cb)
  })
  data.table(municipality=m$municipality, province=m$province, cohort=m$g,
             baseline_g1=round(base_m,1), own_change=round(mean(ym[year %in% post_yrs, rate]) - base_m,1),
             att_m=round(mean(contribs),2), n_post=length(post_yrs))
})
res <- rbindlist(rows)[order(att_m)]
res[, pct_of_own_base := round(100*att_m/baseline_g1,0)]
cat("=== municipality decomposition (baseline = own g-1; controls = not-yet-treated) ===\n")
print(res, nrows=25, class=FALSE)

# validation: replicate CS-style weighting (group agg weights cohorts by treated count; within cohort equal)
w_simple <- res[, mean(att_m)]
cat(sprintf("\nvalidation: simple average of att_m = %.2f | CS group ATT (reg, headline) = -6.43\n", w_simple))
byprov <- res[, .(n_treated=.N, mean_att=round(mean(att_m),2), mean_pct=round(mean(pct_of_own_base),0)), by=province][order(mean_att)]
cat("\n=== by province (treated municipalities only) ===\n"); print(byprov, class=FALSE)
fwrite(res, file.path(TAB,"muni_decomposition.csv"))
cat("\nsaved -> muni_decomposition.csv\n")
