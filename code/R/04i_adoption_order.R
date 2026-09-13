# ============================================================================
# 04i_adoption_order.R — policy-endogeneity check on ADOPTION TIMING (§59.95,
# Galarraga comment #4: "show that the first intervened municipalities were
# not the ones with the highest teen birth rate"). Among the 20 in-window
# treated municipalities: does the opening YEAR correlate with the baseline
# (2016-2019 pre-roll-out average) teen birth rate?
#   (i)  OLS: first_year ~ log baseline rate (HC1), and ~ log rate + log pop
#   (ii) Spearman rank correlation (year x baseline rate)
#   (iii) mean baseline rate, early cohorts (2020-21) vs late (2023-25)
# PREDICTION (rule 1b): no correlation — Table 2 shows placement was scale-
# driven (l_rate ns conditional on l_pop, §51/§59.55); timing showed only an
# education gradient (04f). Deterministic (no bootstrap; no seed needed).
# Output: output/tables/adoption_order.csv (consumed by 04g -> Table 2 Panel B)
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(sandwich); library(lmtest)})
fold <- c("DOM010905","DOM051703","DOM012510")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
d <- p[year %in% 2016:2019, .(rate=mean(rateA_15_19), pop=mean(womenA_15_19)), by=adm3_pcode]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tt <- tr[ever_treated==1L & always_treated==0L, .(adm3_pcode, first_year=as.integer(first_year))]
d <- merge(tt, d, by="adm3_pcode")
stopifnot(nrow(d)==20L, !anyNA(d$rate), !anyNA(d$first_year))
d[, `:=`(l_rate=log(rate), l_pop=log(pop))]

hc <- function(m) coeftest(m, vcov=vcovHC(m, type="HC1"))
m1 <- lm(first_year ~ l_rate, d);          c1 <- hc(m1)
m2 <- lm(first_year ~ l_rate + l_pop, d);  c2 <- hc(m2)
sp <- suppressWarnings(cor.test(d$first_year, d$rate, method="spearman"))
early <- d[first_year <= 2021]; late <- d[first_year >= 2023]
wt <- t.test(early$rate, late$rate)

cat(sprintf("OLS year~l_rate: %.2f (%.2f) p=%.3f | +l_pop: l_rate %.2f (%.2f) p=%.3f\n",
    c1["l_rate",1], c1["l_rate",2], c1["l_rate",4], c2["l_rate",1], c2["l_rate",2], c2["l_rate",4]))
cat(sprintf("Spearman rho = %.3f (p=%.3f)\n", sp$estimate, sp$p.value))
cat(sprintf("baseline rate: early 2020-21 (n=%d) %.1f vs late 2023-25 (n=%d) %.1f | Welch p=%.3f\n",
    nrow(early), mean(early$rate), nrow(late), mean(late$rate), wt$p.value))
fwrite(data.table(
  stat=c("ols_lrate_b","ols_lrate_se","ols_lrate_p","ols2_lrate_b","ols2_lrate_se","ols2_lrate_p",
         "spearman_rho","spearman_p","early_mean","late_mean","welch_p","n_early","n_late"),
  value=c(c1["l_rate",1], c1["l_rate",2], c1["l_rate",4], c2["l_rate",1], c2["l_rate",2], c2["l_rate",4],
          unname(sp$estimate), sp$p.value, mean(early$rate), mean(late$rate), wt$p.value,
          nrow(early), nrow(late))),
  file.path(DIR_TABLES,"adoption_order.csv"))
cat("saved -> adoption_order.csv\n")
