# ============================================================================
# 09g_undercount_battery.R — does differential birth-registration bias the
# headline? Follows 09f (which found CS placebo shifts: pct_pubfac -4.26 p.001,
# pct_haitian -3.49 p.04 among ALL births). Three diagnostics:
#  A. MECHANICAL-COMPOSITION test: teen births are disproportionately public-
#     facility (82.5% vs ~59% non-teen), so removing teen births mechanically
#     lowers the pubfac share of ALL births. Re-run the placebo on NON-TEEN
#     (mothers 25-44) births only: if ~0 there, the all-births shift is the
#     teen-birth decline itself (composition), NOT a registration change.
#  B. LEAVE-ONE-OUT influence: drop each treated municipio, recompute the
#     all-births pubfac/Haitian placebo ATT (analytic SE for speed) — is the
#     shift driven by 1-2 municipios?
#  C. HEADLINE ROBUSTNESS: does the -6.43 survive conditioning on the shares?
#     C1 CS + BASELINE (2016-19 mean) pubfac/Haitian shares (time-invariant,
#        Caetano-Callaway-consistent).  C2 TWFE with CONTEMPORANEOUS shares
#        (bad-control caveat: they are themselves treatment outcomes; reported
#        as descriptive bound only).
# Facility field has 0% NA in all years (verified) -> coverage artifact ruled out.
# 30-34 placebo (07d: ATT -0.28 ns) = registration affects all ages, cited.
# Output: output/tables/undercount_battery.csv
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(fixest)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")

br <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))[!is.na(adm3_pcode)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]

shares <- function(b) b[, .(pct_pubfac = 100*mean(facility=="Public", na.rm=TRUE),
                            pct_haitian= 100*mean(nationality=="Haitian", na.rm=TRUE),
                            n=.N), by=.(adm3_pcode, year=birth_year)]
mk <- function(sh){
  d <- merge(sh, bin[!is.na(g), .(adm3_pcode,g)], by="adm3_pcode")
  d[, id := as.integer(factor(adm3_pcode))]; d[] }
dALL  <- mk(shares(br))
dNT   <- mk(shares(br[age_mom %in% 25:44]))          # non-teen: AUs shouldn't touch these births
cat(sprintf("frames: ALL %d rows | NON-TEEN 25-44 %d rows (cells w/ <10 births: %d)\n",
            nrow(dALL), nrow(dNT), dNT[n<10,.N]))

cs <- function(d, yname, boot=TRUE){
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g", xformla=~1,
        data=d, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=boot, cband=boot, biters=if(boot) 2000 else 0, clustervars="id", print_details=FALSE)))
  gr <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  list(att=gr$overall.att, se=gr$overall.se, p=2*pnorm(-abs(gr$overall.att/gr$overall.se)))
}
res <- list(); add <- function(block, label, r, note="") res[[length(res)+1]] <<-
  data.table(block=block, spec=label, ATT=round(r$att,3), SE=round(r$se,3), p=round(r$p,3), note=note)

# ---- A. all-births vs non-teen placebo ----
cat("\n===== A. composition test: ALL births vs NON-TEEN (25-44) births =====\n")
add("A","pubfac share, ALL births",      cs(dALL,"pct_pubfac"))
add("A","pubfac share, NON-TEEN 25-44",  cs(dNT, "pct_pubfac"), "if ~0 -> all-births shift is teen composition")
add("A","Haitian share, ALL births",     cs(dALL,"pct_haitian"))
add("A","Haitian share, NON-TEEN 25-44", cs(dNT, "pct_haitian"), "if ~0 -> composition")
print(rbindlist(res), class=FALSE)

# ---- B. leave-one-out influence on the ALL-births placebos ----
cat("\n===== B. leave-one-out (drop each treated muni; analytic SE) =====\n")
tr_ids <- dALL[g>0, unique(adm3_pcode)]
fullmap <- c(pct_pubfac="pubfac share, ALL births", pct_haitian="Haitian share, ALL births")
for(v in c("pct_pubfac","pct_haitian")){
  loo <- rbindlist(lapply(tr_ids, function(m){
    r <- tryCatch(cs(mk(shares(br))[adm3_pcode!=m], v, boot=FALSE), error=function(e) list(att=NA,se=NA,p=NA))
    data.table(drop=m, att=round(r$att,3)) }))
  full <- res[[which(sapply(res, function(x) x$spec==fullmap[[v]]))]]$ATT
  loo[, delta := att - full]
  big <- loo[order(-abs(delta))][1:3]
  cat(sprintf("%s: full=%.2f | LOO range [%.2f, %.2f] | top movers: %s\n", v, full, min(loo$att,na.rm=T), max(loo$att,na.rm=T),
      paste(sprintf("%s(%.2f)", big$drop, big$att), collapse=" ")))
  add("B", sprintf("%s LOO range", v), list(att=min(loo$att,na.rm=T), se=NA_real_, p=NA_real_),
      sprintf("max %.2f; top mover %s", max(loo$att,na.rm=T), big$drop[1]))
}

# ---- C. headline conditioning on the shares ----
cat("\n===== C. headline (-6.43) conditioning on shares =====\n")
p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold, .(adm3_pcode,year,rateA_15_19)]
bl <- dALL[year<=2019, .(bl_pubfac=mean(pct_pubfac), bl_haitian=mean(pct_haitian)), by=adm3_pcode]   # pre-treatment baseline (all cohorts open 2020+)
dh <- merge(merge(p, bin[!is.na(g),.(adm3_pcode,g)], by="adm3_pcode"), bl, by="adm3_pcode", all.x=TRUE)
cat("baseline-share NAs after merge:", sum(is.na(dh$bl_pubfac)), "\n")
dh[, id := as.integer(factor(adm3_pcode))]
a1 <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
       xformla=~bl_pubfac+bl_haitian, data=dh, control_group="notyettreated", base_period="universal",
       est_method="reg", bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
g1 <- suppressMessages(aggte(a1, type="group", na.rm=TRUE))
add("C","CS + baseline pubfac/Haitian shares", list(att=g1$overall.att, se=g1$overall.se,
    p=2*pnorm(-abs(g1$overall.att/g1$overall.se))), "clean: baseline covariates")
# C2 TWFE with contemporaneous shares (bad control -> descriptive bound only)
dt <- merge(dh, dALL[, .(adm3_pcode,year,pct_pubfac,pct_haitian)], by=c("adm3_pcode","year"), all.x=TRUE)
dt[, post := as.integer(g>0 & year>=g)]
m0 <- feols(rateA_15_19 ~ post | id+year, dt, cluster=~id)
m1 <- feols(rateA_15_19 ~ post + pct_pubfac + pct_haitian | id+year, dt, cluster=~id)
add("C","TWFE, no controls", list(att=coef(m0)[["post"]], se=se(m0)[["post"]], p=pvalue(m0)[["post"]]))
add("C","TWFE + contemporaneous shares", list(att=coef(m1)[["post"]], se=se(m1)[["post"]], p=pvalue(m1)[["post"]]),
    "BAD CONTROL (shares are outcomes); descriptive bound only")

out <- rbindlist(res, fill=TRUE); print(out, class=FALSE)
fwrite(out, file.path(TAB,"undercount_battery.csv"))
cat("\nsaved -> output/tables/undercount_battery.csv\n")
