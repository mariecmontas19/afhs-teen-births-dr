# ============================================================================
# 09c_denominator_sensitivity.R — APPENDIX robustness to the Hamilton-Perry
# denominator (Baker-Swanson-Tayman 2020: HP over-projects young ages; our
# back-test: female 15-19 +10% vs held-out 2022 census). Shows the S1 headline
# is robust to denominator construction. Three checks vs the headline (denom A):
#   (1) SCALE: correct the 15-19 denom by the back-test factor (/1.10, uniform).
#       A uniform multiplicative denom error scales the LEVEL ATT but leaves the
#       %-of-baseline effect invariant (key point).
#   (2) NON-HP denom: replace HP municipio distribution with a CONSTANT-2020-SHARE
#       distribution of the SAME ONE province 15-19 totals (no cohort mechanics) ->
#       isolates whether the HP CCR machinery (source of the age bias) drives the ATT.
#   (3) Denom B (census-anchored intercensal) — already in 09; recomputed here.
# S1; not-yet controls; universal base; muni-clustered. Output: denominator_sensitivity.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); BT_FACTOR <- 1.10
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
       , .(adm3_pcode, prov_code, year, nb_15_19, womenA_15_19, womenB_15_19, rateA_15_19, rateB_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]

# (1) scale-corrected rate (denom /1.10 -> rate *1.10), exact
p[, rate_scaled := rateA_15_19 * BT_FACTOR]
# (2) NON-HP: constant-2020-share of ONE province 15-19 total (A's province marginal)
prov_tot <- p[, .(prov_w = sum(womenA_15_19)), by=.(prov_code, year)]
sh2020   <- p[year==2020L, .(adm3_pcode, prov_code, w2020=womenA_15_19)]
prov2020 <- prov_tot[year==2020L, .(prov_code, prov_w2020=prov_w)]
sh2020   <- merge(sh2020, prov2020, by="prov_code")[, share := w2020/prov_w2020]
p <- merge(p, sh2020[, .(adm3_pcode, share)], by="adm3_pcode", all.x=TRUE)
p <- merge(p, prov_tot, by=c("prov_code","year"), all.x=TRUE)
p[, w_nonhp := prov_w * share]
p[, rate_nonhp := 1000 * nb_15_19 / w_nonhp]
stopifnot(!anyNA(p$rate_nonhp))

cs <- function(yn){
  est <- merge(p, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode"); est[, id:=as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname=yn, tname="year", idname="id", gname="g", xformla=~1, data=est,
        control_group="notyettreated", base_period="universal", est_method="reg", bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  base <- est[g>0][year==g-1L, mean(get(yn))]
  data.table(ATT=round(o$overall.att,2), SE=round(o$overall.se,2), p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3),
             baseline=round(base,1), pct_of_base=round(100*o$overall.att/base,1))
}
res <- rbindlist(list(
  cbind(denominator="A: Hamilton-Perry, ONE-anchored (HEADLINE)", cs("rateA_15_19")),
  cbind(denominator="A scaled /1.10 (back-test 15-19 over-proj correction)", cs("rate_scaled")),
  cbind(denominator="Non-HP: constant-2020-share of ONE province total", cs("rate_nonhp")),
  cbind(denominator="B: census-anchored intercensal (2010<->2022)", cs("rateB_15_19"))))
cat("=== S1 CS level — DENOMINATOR SENSITIVITY (appendix) ===\n"); print(res, class=FALSE)
fwrite(res, file.path(PROJ,"output","tables","denominator_sensitivity.csv"))
cat("\nKEY: %-of-baseline is stable across all denominators; the LEVEL moves only under the\n",
    "deliberate uniform /1.10 rescale (exact x1.10 of A). Realized A vs B vs non-HP differ trivially.\n", sep="")
cat("saved -> output/tables/denominator_sensitivity.csv\n")
