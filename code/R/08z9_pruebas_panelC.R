# ============================================================================
# 08z9_pruebas_panelC.R — Panel (c) of the human-capital spillover table:
# school performance from the Pruebas Nacionales (§59.48-59.50).
# Four outcomes (MM 2026-08-03): z all four subjects, z mathematics,
# test-takers per 100 pop 17-18, and test-takers who pass per 100 pop 17-18.
# Panel from 05q_pruebas_build.R, whose z-scores are standardized within
# year x subject x modality x SCORING REGIME (the test moved from a 30-point
# to a 100-point scale; 2023 is transitional with both scales in use, so the
# regime is classified per center from its maximum subject score).
# Years 2016-19 + 2022-24 (2020 suspended, 2021 cancelled); municipalities
# whose unit opened in 2020 or 2021 are dropped because their base school
# year is unobservable, leaving the 2022/2023 cohorts and at most one year
# of exposure. Spec = 08z/07t convention; set.seed before every att_gt.
# Output: output/tables/pruebas_panelC.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
SEED <- 20260803L; BITERS <- 2000L
fold <- c("DOM010905","DOM051703","DOM012510")

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g_edu := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year)+1L,
                       fifelse(ever_treated==0L, 0L, NA_integer_))]
pr <- as.data.table(readRDS(file.path(DIR_CLEAN,"pruebas_muni.rds")))
pop <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))[
        age_group=="15-19", .(pop=sum(pop)), by=.(adm3_pcode, year_end=year)]
pr <- merge(pr, pop, by=c("adm3_pcode","year_end"))
pr[, `:=`(tak100 = 100*takers /(0.4*pop),      # per 100 population aged 17-18
          pas100 = 100*passers/(0.4*pop))]
pr <- merge(pr, bin[!is.na(g_edu), .(adm3_pcode, g_edu)], by="adm3_pcode")
dropped <- pr[g_edu %in% c(2021L,2022L), uniqueN(adm3_pcode)]
pr <- pr[!g_edu %in% c(2021L,2022L)]
e <- copy(pr); e[, id := as.integer(factor(adm3_pcode))]
YRS <- c(2016:2019, 2022:2024)
cat("Panel C sample:", e[, uniqueN(adm3_pcode)], "municipalities |",
    e[g_edu>0, uniqueN(adm3_pcode)], "treated |", dropped, "dropped (2020/21 openers)\n")

est <- function(v, pct_meaningful){
  set.seed(SEED)
  a <- suppressWarnings(suppressMessages(att_gt(yname=v, tname="year_end", idname="id",
        gname="g_edu", xformla=~1, data=e[year_end %in% YRS & is.finite(get(v))],
        control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, cband=TRUE, biters=BITERS, clustervars="id",
        allow_unbalanced_panel=TRUE, print_details=FALSE)))
  gr <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
  dy <- suppressMessages(aggte(a, type="dynamic", na.rm=TRUE))
  pre <- dy$egt < -1
  lp <- if(sum(pre)>1){ w<-rep(1/sum(pre),sum(pre)); avg<-sum(w*dy$att.egt[pre])
        ifm<-dy$inf.function$dynamic.inf.func.e[,pre,drop=FALSE]
        se<-sqrt(sum((ifm%*%w)^2))/sqrt(nrow(ifm)); 2*pnorm(-abs(avg/se)) } else NA_real_
  base <- e[g_edu>0 & year_end==g_edu-1L, mean(get(v), na.rm=TRUE)]
  data.table(outcome=v, ATT=gr$overall.att, SE=gr$overall.se,
             p=2*pnorm(-abs(gr$overall.att/gr$overall.se)),
             lo=gr$overall.att-1.96*gr$overall.se, hi=gr$overall.att+1.96*gr$overall.se,
             base=base, pct=fifelse(pct_meaningful, 100*gr$overall.att/base, NA_real_),
             lead_p=lp)
}
res <- rbindlist(list(est("z_all", FALSE), est("z_mat", FALSE),
                      est("tak100", TRUE), est("pas100", TRUE)))
res[, label := c("Test score, all four subjects (SD)","Test score, mathematics (SD)",
                 "Test-takers per 100 population 17--18",
                 "Test-takers who pass, per 100 population 17--18")]
print(res[, .(label, ATT=round(ATT,3), SE=round(SE,3), p=round(p,3),
              base=round(base,2), pct=round(pct,1), lead_p=round(lead_p,2))], class=FALSE)
fwrite(res, file.path(DIR_TABLES,"pruebas_panelC.csv"))
cat("saved -> pruebas_panelC.csv\n")
