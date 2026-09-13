# ============================================================================
# 07k_haitian_gestlag.R — 9-MONTH GESTATIONAL-LAG FALSIFICATION, by nationality.
# A AU opening cannot affect FERTILITY until ~9 months later (conception lag).
# So: real fertility effect => the post-opening drop is ~0 in months 0-8 and
# appears at month >=9. An IMMEDIATE drop (months 0-8) => NOT fertility (those
# births were conceived pre-opening) => recording/classification artifact.
# Monthly CS (att_gt, tname=month index, cohort=opening month) on the per-1,000-
# all-women teen rate; controls = never + not-yet (differences out the steep
# secular Haitian/registration trend). Sample = never-treated + the 14 in-window
# treated with a KNOWN opening month. Compare HAITIAN vs TOTAL (benchmark).
# *** Haitian monthly counts are THIN (~100/quarter) -> noisy; read directionally. ***
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722)
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501"); to155<-function(p) fifelse(p %in% names(fold),unname(fold[p]),p)

b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
A <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
teen <- b[!is.na(adm3_pcode) & !is.na(birth_year) & !is.na(birth_month) & age_grp=="15-19"]; teen[, id:=to155(adm3_pcode)]
cnt <- teen[, .(n_hai=sum(nationality=="Haitian",na.rm=TRUE), n_dom=sum(nationality=="Dominican",na.rm=TRUE), n_tot=.N),
            by=.(id, year=birth_year, month=birth_month)]

ids <- unique(to155(trt$adm3_pcode))
frame <- CJ(id=ids, year=2016:2025, month=1:12)
wA <- A[sex=="F" & age_group=="15-19", .(id=adm3_pcode, year, wann=pop)]
m <- Reduce(function(x,y) merge(x,y,by=intersect(names(x),names(y)),all.x=TRUE), list(frame, wA, cnt))
m[is.na(n_hai),n_hai:=0L][is.na(n_dom),n_dom:=0L][is.na(n_tot),n_tot:=0L]
m[, `:=`(exp=wann/12, month_idx=(year-2016L)*12L + month)]
m[, `:=`(hai_rate=1000*n_hai/exp, dom_rate=1000*n_dom/exp, tot_rate=1000*n_tot/exp)]

# cohort in MONTH index: known-month in-window treated -> opening month_idx; never -> 0; others -> drop
keep155 <- setdiff(trt$adm3_pcode, names(fold))
tt <- trt[adm3_pcode %in% keep155, .(id=adm3_pcode, ever_treated, always_treated, first_year, first_month)]
m <- merge(m, tt, by="id")
m[, gmon := fcase(ever_treated==1L & always_treated==0L & !is.na(first_month), as.numeric((first_year-2016L)*12L + first_month),
                  ever_treated==0L, 0, default=NA_real_)]
est <- m[!is.na(gmon)]                     # never-treated + 14 known-month treated
est[, idn := as.integer(factor(id))]
cat("monthly CS sample: municipalities", uniqueN(est$id), "| treated(known-month)", uniqueN(est[gmon>0,id]),
    "| months", uniqueN(est$month_idx), "\n")

lagtest <- function(yname, lab){
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="month_idx", idname="idn", gname="gmon",
        xformla=~1, data=est, control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=500,
        base_period="universal", clustervars="idn", est_method="reg", anticipation=0)))
  es <- suppressWarnings(suppressMessages(aggte(a, type="dynamic", na.rm=TRUE)))
  d <- data.table(e=es$egt, att=es$att.egt)
  win <- function(lo,hi){ s<-d[e>=lo & e<=hi]; c(mean(s$att), nrow(s)) }   # mean ATT, # event-months
  pre  <- d[e< 0, mean(att)]
  w0_8  <- win(0,8)    # IMMEDIATE: conceived pre-opening -> ~0 if real fertility
  w9_17 <- win(9,17)   # year-1 fertility window
  w18_26<- win(18,26)  # year-2 fertility window (should DEEPEN if real & accumulating)
  cat(sprintf("[%s] pre=%.2f | m0-8=%.2f (n=%d) | m9-17=%.2f (n=%d) | m18-26=%.2f (n=%d)\n",
      lab, pre, w0_8[1], w0_8[2], w9_17[1], w9_17[2], w18_26[1], w18_26[2]))
  data.table(outcome=lab, pre=round(pre,2), months0_8=round(w0_8[1],2),
             months9_17=round(w9_17[1],2), months18_26=round(w18_26[1],2))
}
cat("\n==================== 07k GESTATIONAL-LAG by NATIONALITY (monthly CS) ====================\n")
res <- rbindlist(list(lagtest("tot_rate","TOTAL teen (benchmark)"),
                      lagtest("dom_rate","DOMINICAN teen"), lagtest("hai_rate","HAITIAN teen")))
cat("\nReal fertility effect => months0-8 ~0, drop appears at months9-17. Immediate drop (0-8) => recording artifact.\n")
saveRDS(res, file.path(DIR_CLEAN,"haitian_gestlag_07k.rds"))
