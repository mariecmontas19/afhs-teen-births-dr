# =============================================================================
# 09q_hospital_alignment.R — the hospital administrative records as a check on
# the birth registry (methods 59.131, 2026-11-18).
#
# The registry (BDNV) and the hospital records (Form 67-A) count the same
# events when both are tabulated by municipality of delivery. The registry
# captures a smaller share of them in comparison municipalities and in early
# years, which is the registration threat discussed in the paper. This script
#  (1) tabulates registered public-facility births to mothers 15-19 as a share
#      of hospital-recorded deliveries, by group of municipalities and year
#      -> Appendix Table C3 (tab_c03_completeness.tex);
#  (2) re-estimates the main design after re-weighting each registered
#      public-facility birth by (hospital deliveries / registered public births)
#      of the municipality and year in which it was delivered, with births
#      still assigned to the mother's municipality of residence
#      -> Table A12, Panel C;
#  (3) re-estimates the pregnancy-event row of Table 5 on the municipalities
#      whose facilities report events in every year -> Table 5 note.
#
# Expectation stated before running (rule 1b): if differential registration
# completeness drove the main estimate, (2) would shrink clearly toward zero;
# the audit of 2026-11-18 found it does not (-6.64 against -6.39).
#
# Weights: applied where both counts are at least 20 in the municipality-year,
# bounded to [0.5, 3]; all other births keep weight 1.
# Each estimate has its own seed. Nothing in other scripts' outputs changes.
#
# Output: output/tables/hospital_completeness.csv, hospital_alignment.csv,
#         tab_c03_completeness.tex
# =============================================================================
source(here::here("code", "R", "00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
TAB  <- file.path(PROJ, "output", "tables")
fold <- c("DOM010905", "DOM051703", "DOM012510")
fm   <- c(DOM010905 = "DOM010901", DOM012510 = "DOM012501", DOM051703 = "DOM051701")
f155 <- function(p) fifelse(p %in% names(fm), unname(fm[p]), p)

tr <- as.data.table(readRDS(file.path(DIR_CLEAN, "treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated == 1L & always_treated == 0L, as.integer(first_year),
                  fifelse(ever_treated == 0L, 0L, NA_integer_))]
tr[, grp := fifelse(ever_treated == 1 & always_treated == 0, "Treated (20)",
            fifelse(ever_treated == 0, "Never-treated (126)", "Served before 2016 (9)"))]
stopifnot(tr[grp == "Treated (20)", .N] == 20L, tr[grp == "Never-treated (126)", .N] == 126L,
          tr[grp == "Served before 2016 (9)", .N] == 9L)

b <- as.data.table(readRDS(file.path(DIR_CLEAN, "births_clean_2016_2025.rds")))[!is.na(adm3_pcode)]
b[, `:=`(res = f155(adm3_pcode), del = f155(adm3_pcode_delivery), year = birth_year)]
mm <- as.data.table(readRDS(file.path(DIR_CLEAN, "mispas_muni_panels.rds"))$year)[
        anio %between% c(2016, 2025), .(del = adm3_pcode, year = anio, hosp = deliv_1519)]

## ---- (1) registry as a share of hospital-recorded deliveries ----------------
rp <- b[age_mom %in% 15:19 & facility == "Public" & !is.na(del), .(reg = .N), by = .(del, year)]
cv <- merge(mm, rp, by = c("del", "year"), all = TRUE)
cv[is.na(hosp), hosp := 0]; cv[is.na(reg), reg := 0]
cv <- merge(cv, tr[, .(del = adm3_pcode, grp)], by = "del", all.x = TRUE)
t1 <- cv[!is.na(grp), .(hospital = sum(hosp), registry = sum(reg), pct = round(100 * sum(reg) / sum(hosp))),
         by = .(grp, year)]
setorder(t1, grp, year)
fwrite(t1, file.path(TAB, "hospital_completeness.csv"))
w1 <- dcast(t1, year ~ grp, value.var = "pct")
cat("=== registered public births (15-19) as % of hospital-recorded deliveries ===\n"); print(w1, class = FALSE)

## ---- (2) main design on births aligned to the hospital counts ---------------
cv[, wt := fifelse(reg >= 20 & hosp >= 20, pmin(pmax(hosp / reg, 0.5), 3), 1)]
bb <- merge(b[age_mom %in% 15:19], cv[, .(del, year, wt)], by = c("del", "year"), all.x = TRUE)
bb[is.na(wt) | facility != "Public" | is.na(facility), wt := 1]
adj <- bb[, .(nb_adj = sum(wt)), by = .(adm3_pcode = res, year)]
P <- as.data.table(readRDS(file.path(DIR_CLEAN, "panel_muni_year_155.rds")))[!adm3_pcode %in% fold,
       .(adm3_pcode, year, w = womenA_15_19)]
D <- merge(P, adj, by = c("adm3_pcode", "year"), all.x = TRUE)
D[is.na(nb_adj), nb_adj := 0]; D[, yadj := 1000 * nb_adj / w]
D <- merge(D, tr[!is.na(g), .(adm3_pcode, g)], by = "adm3_pcode")
D[, id := as.integer(factor(adm3_pcode))]
stopifnot(nrow(D) == 1460L, D[g > 0, uniqueN(adm3_pcode)] == 20L)

csg <- function(d, yname) {
  a <- suppressWarnings(suppressMessages(att_gt(yname = yname, tname = "year", idname = "id", gname = "g",
         xformla = ~1, data = d, control_group = "notyettreated", base_period = "universal",
         est_method = "reg", bstrap = TRUE, cband = TRUE, biters = 2000, clustervars = "id",
         print_details = FALSE)))
  o <- suppressMessages(aggte(a, type = "group", na.rm = TRUE))
  c(att = o$overall.att, se = o$overall.se)
}
set.seed(20261118)
r2 <- csg(D, "yadj"); b2 <- D[g > 0 & year == g - 1L, mean(yadj)]

## ---- (3) Table 5's pregnancy-event row among municipalities with events every year
pw <- as.data.table(readRDS(file.path(DIR_CLEAN, "panel_muni_year_155.rds")))[!adm3_pcode %in% fold,
        .(adm3_pcode, year, women = womenA_15_19)]
pe <- as.data.table(readRDS(file.path(DIR_CLEAN, "mispas_muni_panels.rds"))$year)[
        , .(adm3_pcode, year = anio, preg = deliv_1519 + abort_1519)]
d3 <- merge(pw, pe, by = c("adm3_pcode", "year"), all.x = TRUE)
d3[is.na(preg), preg := 0]; d3[, r_preg := 1000 * preg / women]
e3 <- merge(d3[adm3_pcode != "DOM051702"], tr[!is.na(g), .(adm3_pcode, g)], by = "adm3_pcode")   # Nizao excluded as in Table 5
ok <- e3[, .(mn = min(preg)), by = adm3_pcode][mn > 0, adm3_pcode]
e3 <- e3[adm3_pcode %in% ok]; e3[, id := as.integer(factor(adm3_pcode))]
set.seed(20261118)
r3 <- csg(e3, "r_preg"); b3 <- e3[g > 0 & year == g - 1L, mean(r_preg)]

out <- data.table(
  spec     = c("Main design, births aligned to hospital counts",
               "Pregnancy events, municipalities with events every year"),
  ATT      = round(c(r2[["att"]], r3[["att"]]), 2),
  SE       = round(c(r2[["se"]],  r3[["se"]]),  2),
  baseline = round(c(b2, b3), 1),
  n_muni   = c(D[, uniqueN(adm3_pcode)], e3[, uniqueN(adm3_pcode)]),
  n_treated= c(D[g > 0, uniqueN(adm3_pcode)], e3[g > 0, uniqueN(adm3_pcode)]))
out[, p := round(2 * pnorm(-abs(ATT / SE)), 4)]
print(out, class = FALSE)
fwrite(out, file.path(TAB, "hospital_alignment.csv"))

## ---- Appendix Table C3 --------------------------------------------------------
cols <- c("Treated (20)", "Never-treated (126)", "Served before 2016 (9)")
rows <- w1[, sprintf("%d & %d & %d & %d \\\\", year, get(cols[1]), get(cols[2]), get(cols[3]))]
L <- c(
"\\begin{table}[H]\\centering",
"\\caption{Registered births as a percentage of hospital-recorded deliveries, mothers aged 15--19, by municipality of delivery}",
"\\label{tab:completeness}\\small",
"\\begin{tabular}{lccc}",
"\\toprule",
"Year & Treated (20) & Never-treated (126) & Served before 2016 (9) \\\\",
"\\midrule",
rows,
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.86\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Each cell is the number of registered births in public facilities to mothers aged 15--19, divided by the number of deliveries to women aged 15--19 that public facilities reported in the hospital administrative records, for facilities located in each group of municipalities. Values for 2016 and 2017 also reflect birth records without the mother's age.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB, "tab_c03_completeness.tex"))
cat("saved -> hospital_completeness.csv, hospital_alignment.csv, tab_c03_completeness.tex\n")
