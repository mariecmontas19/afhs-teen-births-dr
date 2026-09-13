# ============================================================================
# 08p2_spillover_tests.R — R4 items (a)+(b), methods §49:
#  (a) COHORT-AGING FINGERPRINT: if the 20-21 response = aged-out treated
#      teens, it should GROW with time since opening (earliest treated teens
#      are 19 at e=0 -> reach 20 at e=1, 21 at e=2). Outcome: births to
#      mothers 20-21 per 1,000 women 20-24, full 2016-25 (age at birth needs
#      no mother_dob). CS DYNAMIC aggregation -> event-time path.
#  (b) EVENT-STUDY LEADS for the 20-24 band rate: spillover requires a
#      post-only response with flat leads. CS dynamic on rateA_20_24 +
#      es_pretrend_p joint lead test (00_theme).
# S1; spec = 07t. Output: output/tables/spillover_tests.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
foldmap <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

# ---- build 20-21 contribution rate (full window; age at birth) ----
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
b <- b[!is.na(adm3_pcode) & birth_year %in% 2016:2025 & age_mom %in% 20:21]
b[adm3_pcode %in% names(foldmap), adm3_pcode := foldmap[adm3_pcode]]
cnt <- b[, .(n2021 = .N), by=.(adm3_pcode, year=birth_year)]
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[,
       .(adm3_pcode, year, womenA_20_24, rateA_20_24)]
d0 <- merge(p, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
d0[is.na(n2021), n2021 := 0L]
d0 <- d0[!adm3_pcode %in% fold]
d0[, rate_2021 := 1000*n2021/womenA_20_24]
cat("panel:", nrow(d0), "rows |", d0[,uniqueN(adm3_pcode)], "munis\n")

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
est0 <- merge(d0, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est0[, id := as.integer(factor(adm3_pcode))]

dyn <- function(yname, lab){
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
        xformla=~1, data=est0, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  dy <- suppressMessages(aggte(a, type="dynamic", na.rm=TRUE))
  gr <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  pt <- es_pretrend_p(dy)
  dd <- data.table(outcome=lab, e=dy$egt, att=round(dy$att.egt,2), se=round(dy$se.egt,2))
  cat(sprintf("\n-- %s: overall %.2f (se %.2f, p=%.3f) | pre-trend joint p=%.3f (avg %+.2f)\n",
      lab, gr$overall.att, gr$overall.se, 2*pnorm(-abs(gr$overall.att/gr$overall.se)), pt$p, pt$avg))
  print(dd[e >= -4 & e <= 4], class=FALSE)
  list(dd=dd, overall=gr$overall.att, pt_p=pt$p)
}
rA <- dyn("rate_2021",  "(a) births to 20-21-year-olds per 1,000 women 20-24")
rB <- dyn("rateA_20_24","(b) 20-24 band rate")

out <- rbind(rA$dd, rB$dd)
fwrite(out, file.path(TAB,"spillover_tests.csv"))
cat("\nsaved -> spillover_tests.csv\n")
cat(sprintf("\nFingerprint check (a): e0 vs e2+ growth | Leads check (b): joint p=%.3f\n", rB$pt_p))
