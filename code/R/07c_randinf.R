# ============================================================================
# 07c_randinf.R — RANDOMIZATION INFERENCE on the Callaway–Sant'Anna ATT.
# The honest few-treated-cluster significance test: permute the COHORT assignment
# (g) across municipios (sharp null of no effect for anyone), recompute the CS
# group-aggregated ATT each time, and locate the observed ATT in the permutation
# distribution. Two-sided RI p = share of |permuted ATT| >= |observed ATT|.
# Headline denominator A: level teen rate 15-19 and age-DDD (15-19 - 30-34).
# Compares the permutation SD to the CS cluster-bootstrap SE (which is optimistic
# with only 18 treated clusters).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722)
NPERM <- 500L

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
est <- p[always_treated==0]
est[, id := as.integer(factor(adm3_pcode))]
est[, g  := fifelse(ever_treated==1L, as.integer(first_year), 0L)]
idg <- unique(est[, .(id, g)])                       # 146 municipios' cohort labels (18 treated, 128 g=0)

att_group <- function(yname, dat){
  suppressWarnings(suppressMessages({
    a <- att_gt(yname=yname, tname="year", idname="id", gname="g", xformla=~1, data=dat,
                control_group="notyettreated", bstrap=FALSE, cband=FALSE, est_method="reg")
    aggte(a, type="group", na.rm=TRUE)$overall.att
  }))
}
ri_for <- function(yname){
  obs <- att_group(yname, est)
  perm <- numeric(NPERM)
  for (k in seq_len(NPERM)){
    gp <- data.table(id=idg$id, gp=sample(idg$g))     # permute cohort labels across municipios
    ep <- merge(est[, !"g"], gp, by="id"); setnames(ep, "gp", "g")
    perm[k] <- tryCatch(att_group(yname, ep), error=function(e) NA_real_)
  }
  perm <- perm[is.finite(perm)]
  data.table(outcome=yname, obs_ATT=round(obs,3),
             RI_p_2sided=round(mean(abs(perm) >= abs(obs)),4),
             perm_SD=round(sd(perm),3), n_valid_perm=length(perm))
}

cat("==================== 07c RANDOMIZATION INFERENCE ====================\n")
cat("NPERM =", NPERM, "| permute cohort g across 146 municipios | CS group-ATT, denom A\n")
res <- rbindlist(lapply(c("rateA_15_19","ddd_A"), ri_for))
print(res, class=FALSE)
cat("\nCompare perm_SD to the CS cluster-bootstrap SE (level ~2.26, DDD ~2.79): if perm_SD is\n")
cat("much larger, the bootstrap SE is optimistic and RI is the honest inference.\n")
saveRDS(res, file.path(DIR_CLEAN,"ri_results.rds"))
