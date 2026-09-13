# ============================================================================
# 07_mde_power.R — MINIMUM DETECTABLE EFFECT (MDE) / power, BEFORE estimation.
# Reports the smallest effect the design can detect at 80% power, 5% two-sided,
# so we bring detectable-effect size to the committee rather than discover
# underpowering after a null. Design-based on the REAL frozen panel:
#   est sample = in-window treated (18) + never-treated (128), drop 9 always-treated.
#   ATT via TWFE aggregate (municipio + year FE, WLS by women, municipio-clustered SE)
#   — TWFE is unbiased for a HOMOGENEOUS injected effect, so it's the right SE for MDE;
#   CS power is comparable-to-slightly-larger. Few-treated-cluster inference checked
#   with RANDOMIZATION INFERENCE (permute treatment assignment) — no fwildclusterboot needed.
# MDE(80%,5%) = (z.975 + z.80) * SE = 2.80 * SE.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(fixest)})
set.seed(20260722)

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
p[, D := as.integer(ever_treated==1 & always_treated==0 & year>=first_year)]   # binary post for in-window treated
est <- p[always_treated==0]                                                    # 146 = 18 treated + 128 never
cat("estimation sample: municipios", uniqueN(est$adm3_pcode), "(drop 9 always-treated) | treated",
    uniqueN(est[ever_treated==1, adm3_pcode]), "| muni-years", nrow(est), "\n")

zc <- qnorm(.975) + qnorm(.80)                                                 # = 2.802
mde_for <- function(yvar, wvar){
  f  <- as.formula(sprintf("%s ~ D | adm3_pcode + year", yvar))
  m  <- feols(f, data=est, cluster=~adm3_pcode, weights=as.formula(paste0("~",wvar)))
  se <- m$se[["D"]]; att <- coef(m)[["D"]]
  base <- est[D==0 & ever_treated==1, weighted.mean(get(yvar), get(wvar))]      # pre-treatment treated baseline
  # randomization inference: permute which municipios are treated+their post-timing
  uatt <- replicate(500, {
    ids <- unique(est$adm3_pcode)
    perm <- data.table(adm3_pcode=ids, Dperm_id=sample(ids))                    # relabel treatment identity
    key  <- unique(est[, .(adm3_pcode, D, year)])                              # real D pattern by (muni,year)
    pr <- merge(est[, .(adm3_pcode, year, y=get(yvar), w=get(wvar))], perm, by="adm3_pcode")
    pr <- merge(pr, key[, .(Dperm_id=adm3_pcode, year, Dp=D)], by=c("Dperm_id","year"), all.x=TRUE)
    pr[is.na(Dp), Dp:=0L]
    coef(feols(y ~ Dp | adm3_pcode + year, data=pr, weights=~w))[["Dp"]]
  })
  se_ri <- sd(uatt)
  data.table(outcome=yvar, att_hat=round(att,2), se_cluster=round(se,2), se_RI=round(se_ri,2),
             MDE80=round(zc*se,2), MDE80_RI=round(zc*se_ri,2),
             baseline=round(base,1), MDE_pct_of_base=round(100*zc*max(se,se_ri)/base,1))
}

cat("\n==================== 07 MDE / POWER ====================\n")
res <- rbindlist(lapply(list(c("rateA_15_19","womenA_15_19"), c("rateB_15_19","womenB_15_19"),
                             c("ddd_A","womenA_15_19"), c("lbw_per1000women","womenA_15_19")),
                        function(x) mde_for(x[1], x[2])))
print(res, class=FALSE)
cat("\nMDE80 = effect detectable at 80% power, 5% two-sided (rate points per 1,000). MDE_pct_of_base = % of the\n")
cat("pre-treatment treated baseline. RI = randomization-inference SE (few-treated-cluster robust).\n")
saveRDS(res, file.path(DIR_CLEAN,"mde_power.rds"))
