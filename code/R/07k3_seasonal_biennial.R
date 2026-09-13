# ============================================================================
# 07k3_seasonal_biennial.R — MM + Jessica follow-ups to the quarterly test:
#  (A) SEASONALLY ADJUSTED quarterly CS: common seasonality already cancels in
#      CS (treated vs controls, same calendar quarter); what survives is
#      MUNICIPALITY-SPECIFIC seasonality. Fix per MM: residualize each
#      municipality's quarterly rate on its OWN quarter-of-year means computed
#      from PRE-TREATMENT years (2016-2019 only, so the factors cannot absorb
#      treatment), then run the same quarterly CS on the residuals.
#  (B) BIENNIAL CS (Jessica): two-year periods 2016-17 ... 2024-25 (5 periods).
#      Outcome = teen births per 1,000 woman-years over the period. Cohort =
#      period of first opening (P3 2020-21 / P4 2022-23 / P5 2024-25). Thick
#      cells, blunt timing; complements annual + (noisy) quarterly.
# Output: output/tables/quarterly_sa.csv + biennial_cs.csv
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
keep155 <- setdiff(trt$adm3_pcode, names(fold))
tt <- trt[adm3_pcode %in% keep155, .(id=adm3_pcode, ever_treated, always_treated, first_year, first_month)]
wA <- A[sex=="F" & age_group=="15-19", .(id=adm3_pcode, year, wann=pop)]

# ---------- (A) seasonally adjusted quarterly ----------
cntq <- teen[, .(n=.N), by=.(id, year=birth_year, q)]
mq <- merge(merge(CJ(id=unique(tt$id), year=2016:2025, q=1:4), wA, by=c("id","year"), all.x=TRUE),
            cntq, by=c("id","year","q"), all.x=TRUE)
mq[is.na(n), n:=0L]
mq[, rate := 1000*n/(wann/4)]
seas <- mq[year<=2019, .(qf = mean(rate)), by=.(id, q)]                 # muni-specific quarter factor, PRE years
seas <- merge(seas, seas[, .(mn=mean(qf)), by=id], by="id")
seas[, adj := qf - mn]                                                   # deviation of quarter q from muni mean
mq <- merge(mq, seas[, .(id, q, adj)], by=c("id","q"), all.x=TRUE)
mq[is.na(adj), adj := 0]
mq[, rate_sa := rate - adj]
mq <- merge(mq, tt, by="id")
mq[, `:=`(qidx=(year-2016L)*4L+q,
          gq=fcase(ever_treated==1L & always_treated==0L & !is.na(first_month),
                   as.numeric((first_year-2016L)*4L+ceiling(first_month/3)),
                   ever_treated==0L, 0, default=NA_real_))]
eq <- mq[!is.na(gq)]; eq[, idn := as.integer(factor(id))]
a <- suppressWarnings(suppressMessages(att_gt(yname="rate_sa", tname="qidx", idname="idn", gname="gq",
      xformla=~1, data=eq, control_group="notyettreated", bstrap=TRUE, biters=500,
      base_period="universal", clustervars="idn", est_method="reg")))
es <- suppressWarnings(suppressMessages(aggte(a, type="dynamic", na.rm=TRUE)))
d <- data.table(e=es$egt, att=es$att.egt, se=es$se.egt)
win <- function(lo,hi,lab) d[e>=lo & e<=hi, .(window=lab, mean_att=round(mean(att),2),
  min_att=round(min(att),2), max_att=round(max(att),2), n_q=.N)]
outA <- rbind(win(-40,-1,"pre"), win(0,2,"Q0-Q2"), win(3,5,"Q3-Q5"), win(6,8,"Q6-Q8"))
cat("=== (A) seasonally adjusted quarterly (muni-specific pre-2020 quarter factors) ===\n")
print(outA, class=FALSE)
fwrite(outA, file.path(TAB,"quarterly_sa.csv"))

# ---------- (B) biennial ----------
cnty <- teen[, .(n=.N), by=.(id, year=birth_year)]
my <- merge(merge(CJ(id=unique(tt$id), year=2016:2025), wA, by=c("id","year"), all.x=TRUE),
            cnty, by=c("id","year"), all.x=TRUE)
my[is.na(n), n:=0L]
my[, per := ceiling((year-2015L)/2)]                                     # 1:2016-17 ... 5:2024-25
bi <- my[, .(n=sum(n), wy=sum(wann)), by=.(id, per)]
bi[, rate := 1000*n/wy]                                                  # per 1,000 woman-years
bi <- merge(bi, tt, by="id")
bi[, gp := fcase(ever_treated==1L & always_treated==0L, as.numeric(ceiling((first_year-2015L)/2)),
                 ever_treated==0L, 0, default=NA_real_)]
eb <- bi[!is.na(gp)]; eb[, idn := as.integer(factor(id))]
cat("\nbiennial cohorts:", paste(capture.output(print(eb[gp>0, uniqueN(id), by=gp][order(gp)])), collapse=" | "), "\n")
a2 <- suppressWarnings(suppressMessages(att_gt(yname="rate", tname="per", idname="idn", gname="gp",
      xformla=~1, data=eb, control_group="notyettreated", bstrap=TRUE, biters=2000,
      base_period="universal", clustervars="idn", est_method="reg")))
o2 <- suppressMessages(aggte(a2, type="group", na.rm=TRUE))
dy2 <- suppressMessages(aggte(a2, type="dynamic", na.rm=TRUE))
bs <- eb[gp>0][, .SD[per==gp-1], by=id][, mean(rate)]
cat(sprintf("\n=== (B) biennial CS: ATT %.2f (SE %.2f, p=%.3f) | base %.1f (%.0f%%) ===\n",
    o2$overall.att, o2$overall.se, 2*pnorm(-abs(o2$overall.att/o2$overall.se)), bs, 100*o2$overall.att/bs))
cat("event-period path (2-yr periods):\n")
print(data.table(e=dy2$egt, att=round(dy2$att.egt,2), se=round(dy2$se.egt,2)), class=FALSE)
outB <- data.table(spec="Biennial CS (2-yr periods)", ATT=round(o2$overall.att,2), SE=round(o2$overall.se,2),
                   p=round(2*pnorm(-abs(o2$overall.att/o2$overall.se)),3), baseline=round(bs,2),
                   pct=round(100*o2$overall.att/bs,1), n_treated=eb[gp>0, uniqueN(id)])
fwrite(outB, file.path(TAB,"biennial_cs.csv"))
cat("\nsaved -> quarterly_sa.csv + biennial_cs.csv\n")
