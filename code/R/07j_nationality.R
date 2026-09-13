# ============================================================================
# 07j_nationality.R — DECOMPOSE the teen-birth effect by mother's nationality
# (Dominican vs Haitian). Outcomes = Dominican / Haitian teen (15-19) births per
# 1,000 ALL women 15-19 (denominator A = all teen women, NOT split by nationality
# -> clean per-population rate, no births-selection issue; the two components +
# other/NA sum to the total teen rate). We CANNOT build a Haitian-women-specific
# rate (no Haitian-women denominator by municipio-year; census lacks nationality).
# CAVEATS: Haitian component is small (~12% of teen births, base ~6/1000) ->
# UNDERPOWERED; "per all women" is a population rate, not Haitian fertility per se.
# CS group ATT, denom A, not-yet controls, clustered at municipio.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722)
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)

b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
teen <- b[age_grp=="15-19" & !is.na(adm3_pcode)]
teen[, id155 := to155(adm3_pcode)]
cnt <- teen[, .(n_dom = sum(nationality=="Dominican", na.rm=TRUE),
                n_hai = sum(nationality=="Haitian",   na.rm=TRUE)), by=.(adm3_pcode=id155, year=birth_year)]

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
d <- merge(p[, .(adm3_pcode, year, womenA_15_19, rateA_15_19, ever_treated, always_treated, first_year)],
           cnt, by=c("adm3_pcode","year"), all.x=TRUE)
d[is.na(n_dom), n_dom := 0L][is.na(n_hai), n_hai := 0L]
d[, `:=`(dom_per1000w = 1000*n_dom/womenA_15_19, hai_per1000w = 1000*n_hai/womenA_15_19)]
cat("national pooled rates/1000 women: total", round(1000*sum(p$nb_15_19)/sum(p$womenA_15_19),1),
    "| Dominican", round(1000*sum(d$n_dom)/sum(d$womenA_15_19),1),
    "| Haitian", round(1000*sum(d$n_hai)/sum(d$womenA_15_19),1), "\n")

est <- d[always_treated==0]; est[, id := as.integer(factor(adm3_pcode))]
est[, g := fifelse(ever_treated==1L, as.integer(first_year), 0L)]
cs <- function(yname){
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=2000,
        base_period="universal", clustervars="id", est_method="reg")))
  gr <- suppressWarnings(suppressMessages(aggte(a, type="group", na.rm=TRUE)))
  base <- est[g==0, weighted.mean(get(yname), womenA_15_19)]
  data.table(outcome=yname, ATT=round(gr$overall.att,3), SE=round(gr$overall.se,3),
             p=round(2*pnorm(-abs(gr$overall.att/gr$overall.se)),3), base=round(base,2))
}
cat("\n==================== 07j NATIONALITY DECOMPOSITION (teen rate, denom A) ====================\n")
res <- rbindlist(list(cs("rateA_15_19"), cs("dom_per1000w"), cs("hai_per1000w")))
print(res, class=FALSE)
cat("\nDominican + Haitian (+ small other/NA) ATTs should ~sum to the total. Haitian piece is small -> UNDERPOWERED.\n")
saveRDS(res, file.path(DIR_CLEAN,"nationality_07j.rds"))
