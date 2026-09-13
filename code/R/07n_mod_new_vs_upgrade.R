# ============================================================================
# 07n_mod_new_vs_upgrade.R — heterogeneity within the MODERNIZATION design:
# does a brand-NEW full-service unit (first access) differ from an UPGRADE of a
# pre-existing old unit (service intensification)? Run CS separately:
#   NEW-only      = 128 controls + 21 new-opening municipios
#   UPGRADE-only  = 128 controls +  5 post-2016-upgrade municipios
# Same outcomes/inference as 07m. Decisive for whether to pool or report new-only.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722)
TAB <- file.path(PROJ,"output","tables")

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))
fold <- c("DOM010905","DOM051703","DOM012510")
modk <- mod[!adm3_pcode %in% fold, .(adm3_pcode, mod_cohort, mod_ever_treated,
                                     always_treated_mod, mod_first_type)]
dropc <- intersect(names(modk)[-1], names(p)); if (length(dropc)) p <- p[, !dropc, with=FALSE]
p <- merge(p, modk, by="adm3_pcode", all.x=TRUE)

cs1 <- function(d, yname){
  d <- copy(d); d[, id := as.integer(factor(adm3_pcode))]
  d[, g := fifelse(mod_ever_treated==1L, as.integer(mod_cohort), 0L)]
  a <- att_gt(yname=yname, tname="year", idname="id", gname="g", xformla=~1, data=d,
              control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=2000,
              base_period="universal", clustervars="id", est_method="reg", print_details=FALSE)
  ov <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  list(att=ov$overall.att, se=ov$overall.se, attdyn=dy$overall.att, sedyn=dy$overall.se,
       ntreat=uniqueN(d[g>0, adm3_pcode]))
}

base <- p[always_treated_mod==0]
samp <- list(
  "ALL modern (26)"   = base,
  "NEW-opening (21)"  = base[mod_ever_treated==0 | mod_first_type=="new"],
  "UPGRADE-only (5)"  = base[mod_ever_treated==0 | mod_first_type=="upgrade"]
)
out <- rbindlist(lapply(names(samp), function(nm){
  rbindlist(lapply(c("rateA_15_19","ddd_A"), function(y){
    r <- cs1(samp[[nm]], y)
    data.table(sample=nm, outcome=y, n_treated=r$ntreat,
               ATT=round(r$att,3), SE=round(r$se,3), t=round(r$att/r$se,2),
               ATT_dyn=round(r$attdyn,3), SE_dyn=round(r$sedyn,3))
  }))
}))
cat("\n========== MODERNIZATION: new-opening vs upgrade heterogeneity ==========\n")
print(out, class=FALSE)
fwrite(out, file.path(TAB,"cs_att_mod_new_vs_upgrade.csv"))
cat("\nsaved -> output/tables/cs_att_mod_new_vs_upgrade.csv\n")
