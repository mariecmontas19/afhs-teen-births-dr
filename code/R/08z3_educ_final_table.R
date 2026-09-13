# ============================================================================
# 08z3_educ_final_table.R — FINAL education table: Male / Female / Total, four
# secondary outcomes, ENROLLMENT NORMALIZED for all three groups (MM 2026-07-29).
# Enrollment rate = secondary enrollment / 15-19 population (Series A, BOTH
# sexes available) — the per-capita normalization that removes the demographic
# pre-trend confound (§59.39: log-count fails leads .014, normalized clean .86).
# Dropout / repetition / overage are already rates (share of enrolled). Overage:
# 2019 imputed from 2018 & 2020 (pre-treatment cell; §59.39 addendum). Each cell
# reports ATT, p, and the joint pre-trend-test p (our flat-leads standard).
# Output: output/tables/educ_final_bysex.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260729)

pan <- as.data.table(readRDS(file.path(DIR_CLEAN,"educ_condicion_muni.rds")))
sec <- pan[nivel=="SECUNDARIO"]
# per-sex + total aggregates of the flows
agg <- sec[, .(mat=sum(mat), aband=sum(aband), rep=sum(rep), sobre=sum(sobre)),
           by=.(adm3_pcode, year_end, sexo)]
tot <- sec[, .(sexo="T", mat=sum(mat), aband=sum(aband), rep=sum(rep), sobre=sum(sobre)),
           by=.(adm3_pcode, year_end)]
agg <- rbind(agg, tot)
agg[year_end==2019L, sobre := NA_real_]           # sobreedad absent in 2018-19 sheet

# 15-19 population denominators (Series A): F, M, and T (F+M)
# GER denominator (MM 2026-08-03, colleague feedback §59.48): population of
# the OFFICIAL secondary ages 12-17 (WDI/UIS convention; DR entrance age 12,
# duration 6), built as 0.6 x pop(10-14) + 0.6 x pop(15-19) per sex
# (within-group uniform split, flagged approximation).
pop <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))[
         age_group %in% c("10-14","15-19")]
pop <- pop[, .(pop = 0.6*sum(pop[age_group=="10-14"]) + 0.6*sum(pop[age_group=="15-19"])),
           by=.(adm3_pcode, year_end=year, sexo=sex)]
popT <- pop[, .(sexo="T", pop=sum(pop)), by=.(adm3_pcode, year_end)]
pop  <- rbind(pop, popT)
d0 <- merge(agg, pop, by=c("adm3_pcode","year_end","sexo"), all.x=TRUE)
stopifnot(d0[is.na(pop), .N] == 0)

d0[, `:=`(dropout    = 100*aband/mat,
          repetition = 100*rep/mat,
          overage    = 100*sobre/mat,
          enroll_rate= 100*mat/pop)]                # normalized enrollment
# interpolate the 2019 overage rate (pre-treatment cell), per muni x sexo
setorder(d0, adm3_pcode, sexo, year_end)
d0[, y18 := overage[year_end==2018][1], by=.(adm3_pcode, sexo)]
d0[, y20 := overage[year_end==2020][1], by=.(adm3_pcode, sexo)]
d0[year_end==2019 & is.na(overage), overage := (y18+y20)/2]
d0[, c("y18","y20") := NULL]

fold <- c("DOM010905","DOM051703","DOM012510")
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
trt[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]

cellN <- function(sx, yvar){   # numeric twin of cell(): returns ATT/SE/p/base/lead_p (for the .tex formatter)
  d <- merge(d0[sexo==sx], trt[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  d[, g_edu := fifelse(g==0L, 0L, g + 1L)]
  d[, id := as.integer(factor(adm3_pcode))]; d <- d[is.finite(get(yvar))]
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  pre <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,-2)]
  lp  <- 2*pnorm(-abs(pre[,sum(att/se^2)/sum(1/se^2)] / sqrt(1/pre[,sum(1/se^2)])))
  base <- d[g>0 & year_end==g-1L, mean(get(yvar), na.rm=TRUE)]
  data.table(sexo=sx, yvar=yvar, ATT=gg$overall.att, SE=gg$overall.se,
             p=2*pnorm(-abs(gg$overall.att/gg$overall.se)), base=base, lead_p=lp)
}

cell <- function(sx, yvar){
  d <- merge(d0[sexo==sx], trt[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  d[, g_edu := fifelse(g==0L, 0L, g + 1L)]
  d[, id := as.integer(factor(adm3_pcode))]; d <- d[is.finite(get(yvar))]
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  pre <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,-2)]
  lp  <- 2*pnorm(-abs(pre[,sum(att/se^2)/sum(1/se^2)] / sqrt(1/pre[,sum(1/se^2)])))
  base <- d[g>0 & year_end==g-1L, mean(get(yvar), na.rm=TRUE)]
  sprintf("%+.2f (p=%.2f)%s | lead p=%.2f", gg$overall.att,
          2*pnorm(-abs(gg$overall.att/gg$overall.se)),
          ifelse(!is.na(base) && base>0, sprintf(" [%+.0f%%]", 100*gg$overall.att/base), ""), lp)
}

# Overage DROPPED (MM 2026-07-29): fails pre-trends (all leads sig) AND relies
# on the imputed 2019 cell -> not presentable. Keep the 3 clean outcomes.
outs <- c(dropout="Dropout rate (pp)", repetition="Repetition rate (pp)",
          enroll_rate="Enrollment rate (per 100, normalized)")
tab <- rbindlist(lapply(names(outs), function(v) data.table(
  outcome=outs[[v]],
  Male   = cell("M", v),
  Female = cell("F", v),
  Total  = cell("T", v))))
cat("\n============ EDUCATION: MALE / FEMALE / TOTAL (enrollment normalized) ============\n")
cat("Secondary, CS (not-yet, universal base, muni-clustered); lead p = joint pre-trend test.\n\n")
print(tab, class=FALSE)
fwrite(tab, file.path(PROJ,"output","tables","educ_final_bysex.csv"))
cat("\nsaved -> educ_final_bysex.csv\n")

# numeric twin for the paper .tex formatter (10z)
num <- rbindlist(lapply(names(outs), function(v)
  rbindlist(lapply(c("M","F","T"), function(sx) cellN(sx, v)))))
fwrite(num, file.path(PROJ,"output","tables","educ_final_bysex_numeric.csv"))
cat("saved -> educ_final_bysex_numeric.csv\n")
