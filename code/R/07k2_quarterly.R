# ============================================================================
# 07k2_quarterly.R — committee item R6a (§44.6): QUARTERLY-frequency CS as a
# timing placebo/falsification. Mirrors 07k (monthly) at quarterly frequency:
# a unit opening cannot affect FERTILITY until ~9 months later, so the drop
# must be ~0 in quarters 0-2 (births conceived pre-opening) and appear from
# quarter 3, deepening with exposure. Quarterly counts are ~3x thicker than
# monthly (less noise), at the cost of coarser timing.
# Outcome: teen (15-19) births per 1,000 teen women per quarter (exp = w/4).
# Cohort = opening QUARTER; sample = never-treated + the in-window treated
# with a KNOWN opening month (as 07k). CS spec = 07t (biters 500, as 07k).
# Output: output/tables/quarterly_gestlag.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722)
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)
TAB <- file.path(PROJ,"output","tables")

b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
A <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
teen <- b[!is.na(adm3_pcode) & !is.na(birth_year) & !is.na(birth_month) & age_grp=="15-19"]
teen[, `:=`(id=to155(adm3_pcode), q=ceiling(birth_month/3))]
cnt <- teen[, .(n=.N), by=.(id, year=birth_year, q)]

ids <- unique(to155(trt$adm3_pcode))
frame <- CJ(id=ids, year=2016:2025, q=1:4)
wA <- A[sex=="F" & age_group=="15-19", .(id=adm3_pcode, year, wann=pop)]
m <- Reduce(function(x,y) merge(x,y,by=intersect(names(x),names(y)),all.x=TRUE), list(frame, wA, cnt))
m[is.na(n), n:=0L]
m[, `:=`(exp=wann/4, q_idx=(year-2016L)*4L + q)]
m[, rate := 1000*n/exp]

keep155 <- setdiff(trt$adm3_pcode, names(fold))
tt <- trt[adm3_pcode %in% keep155, .(id=adm3_pcode, ever_treated, always_treated, first_year, first_month)]
m <- merge(m, tt, by="id")
m[, gq := fcase(ever_treated==1L & always_treated==0L & !is.na(first_month),
                as.numeric((first_year-2016L)*4L + ceiling(first_month/3)),
                ever_treated==0L, 0, default=NA_real_)]
est <- m[!is.na(gq)]; est[, idn := as.integer(factor(id))]
cat("quarterly CS sample: municipalities", uniqueN(est$id), "| treated(known-quarter)", uniqueN(est[gq>0,id]),
    "| quarters", uniqueN(est$q_idx), "\n")

a <- suppressWarnings(suppressMessages(att_gt(yname="rate", tname="q_idx", idname="idn", gname="gq",
      xformla=~1, data=est, control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=500,
      base_period="universal", clustervars="idn", est_method="reg", anticipation=0)))
es <- suppressWarnings(suppressMessages(aggte(a, type="dynamic", na.rm=TRUE)))
d <- data.table(e=es$egt, att=es$att.egt, se=es$se.egt)
win <- function(lo,hi) d[e>=lo & e<=hi, .(mean_att=mean(att), n_q=.N)]
pre <- d[e<0, mean(att)]
w02 <- win(0,2); w35 <- win(3,5); w68 <- win(6,8)
# NB units: rate = 1000*n_q/(wann/4) is ALREADY annual-equivalent (exposure = quarter-person-years).
# NB interpretation (§52): municipal quarterly cells are THIN -> per-quarter ATTs swing +-7 and the
# whole path shifts with the g-1 base-quarter draw (a -4.9 LEAD at e=-2 shows the noise scale).
# Window means here are NOT a sharp falsification; the powered timing tests are the national pooled
# gestational-lag (07z6: m0-8 flat +0.7 vs post -3.6) and the annual dynamics.
cat("\n=== quarterly gestational-lag (annual-equivalent units) ===\n")
cat(sprintf("pre (all leads): %+.2f\n", pre))
cat(sprintf("Q0-Q2 (conceived pre-opening): %+.2f [%d qtrs]\n", w02$mean_att, w02$n_q))
cat(sprintf("Q3-Q5 (first exposed conceptions): %+.2f [%d qtrs]\n", w35$mean_att, w35$n_q))
cat(sprintf("Q6-Q8 (deepening): %+.2f [%d qtrs]\n", w68$mean_att, w68$n_q))
out <- data.table(window=c("pre","Q0-Q2","Q3-Q5","Q6-Q8"),
                  mean_att=round(c(pre, w02$mean_att, w35$mean_att, w68$mean_att),3),
                  n_quarters=c(d[e<0,.N], w02$n_q, w35$n_q, w68$n_q))
fwrite(out, file.path(TAB,"quarterly_gestlag.csv"))
fwrite(d, file.path(TAB,"quarterly_eventstudy_full.csv"))
cat("\nsaved -> quarterly_gestlag.csv + quarterly_eventstudy_full.csv\n")
