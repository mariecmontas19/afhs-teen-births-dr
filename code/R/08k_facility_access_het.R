# ============================================================================
# 08k_facility_access_het.R — heterogeneity by the NEW facility-access metrics
# (05i), replacing the muddy "isolation" (notes §26.3). Split the 20 in-window
# treated at the median of each metric (value at g-1, pre-treatment) vs shared
# never-treated controls. S1; Denom A; not-yet controls; est=reg; muni-clustered.
# "Underserved" = WORSE geographic access (far from facility / low 2SFCA).
# Output: output/tables/facility_access_het.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); OVERALL <- -6.43
p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
treated <- bin[!is.na(g)&g>0, .(adm3_pcode, g)]; never <- bin[g==0, adm3_pcode]
fa <- as.data.table(readRDS(file.path(DIR_CLEAN,"facility_access_panel.rds")))
M <- merge(treated, fa, by="adm3_pcode")[year==g-1L]                       # metric at g-1 (pre-treatment)

cs_sub <- function(pcodes){
  est <- merge(p, rbind(treated[adm3_pcode %in% pcodes], data.table(adm3_pcode=never, g=0L)), by="adm3_pcode")
  est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE)); data.table(att=round(o$overall.att,2), se=round(o$overall.se,2))
}
spec <- data.table(var=c("near_any_km","near_hosp_km","near_niv3_km","n_20km","sfca20"),
                   label=c("Nearest facility (km)","Nearest hospital (km)","Nearest Nivel-III (km)","Facilities within 20km","2SFCA access index"),
                   under_high=c(TRUE,TRUE,TRUE,FALSE,FALSE))                # far OR few OR low-2SFCA = underserved
res <- rbindlist(lapply(seq_len(nrow(spec)), function(i){
  v <- spec$var[i]; med <- median(M[[v]])
  und <- if(spec$under_high[i]) M[get(v)>=med, adm3_pcode] else M[get(v)<med, adm3_pcode]
  srv <- setdiff(M$adm3_pcode, und)
  ru <- cs_sub(und); rs <- cs_sub(srv); gap <- round(ru$att-rs$att,2)
  pdiff <- round(2*pnorm(-abs(gap)/sqrt(ru$se^2+rs$se^2)),3)
  cat(sprintf("  %-26s underserved %+.2f (se %.2f) | served %+.2f (se %.2f) | gap %+.2f p(diff)=%.3f\n",
      spec$label[i], ru$att,ru$se, rs$att,rs$se, gap, pdiff))
  data.table(moderator=spec$label[i], att_under=ru$att, se_under=ru$se, att_served=rs$att, se_served=rs$se, gap=gap, p_diff=pdiff)
}))
cat("=== facility-access heterogeneity (S1; overall", OVERALL, "; underserved = worse access) ===\n")
fwrite(res, file.path(PROJ,"output","tables","facility_access_het.csv"))
cat("\nsaved -> output/tables/facility_access_het.csv\n")
