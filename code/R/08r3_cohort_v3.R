# ============================================================================
# 08r3_cohort_v3.R — MM directive (§59.9): rescale + re-estimate the ENHOGAR
# cohort-exposure design.
# (1) RESCALING of B2 (+0.448 per 0->1 intensity): report per-10pp and per-SD
#     contrasts of the observed exposure distribution.
# (2) CUMULATIVE EXPOSURE: cum_exp = mean province intensity over the attained
#     adolescent ages 15..min(age,19) (at-15 snapshot miscodes women exposed at
#     16-18 as untreated). Same FE structure (prov^wave + age), women 18-22.
# (3) Robustness: unweighted; leave-one-province-out range; drop DN+Santiago.
# PREDICTIONS (rule 1b): cum_exp coefficient same sign, roughly similar or
# larger than at-15 (less misclassification); per-10pp effects ~ +0.04-0.08 yrs
# (consistent w/ fertility arithmetic); LOO range should not include sign flips
# if the result is not driven by one province.
# Output: output/tables/enhogar_cohort_v3.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(fixest)})
TAB <- file.path(PROJ,"output","tables")
ENH <- file.path(DRPAPER, "analysis/datasets/Encuestas de Hogares de Prósitos Múltiples (ENHOGAR)")

rd <- function(file, cols, newnames, wave){
  hdr <- names(fread(file, nrows=0))
  sel <- sapply(cols, function(p) grep(p, hdr, value=TRUE, useBytes=TRUE)[1])
  stopifnot(!anyNA(sel))
  d <- fread(file, select=unname(sel), showProgress=FALSE)
  setnames(d, unname(sel), newnames); d[, wave := wave]; d
}
w18 <- rd(file.path(ENH,"2018/Personas_ENH18.csv"),
          c("^H202$","^H203$","^H302$","^H304$","^H305$","^HPROVI","^Factor_expansion$"),
          c("sex","age","asiste","nivel","curso","prov","fexp"), 2018L)
w22 <- rd(file.path(ENH,"2022/Personas_ENH22.csv"),
          c("^P202$","^P203$","^P302$","^P303$","^P304$","^HPROVI","^F_expansi"),
          c("sex","age","asiste","nivel","curso","prov","fexp"), 2022L)
d <- rbind(w18, w22)[sex==2 & age %between% c(18,22)]
d[asiste %in% 9, asiste := NA]
d[nivel %in% c(8,9), nivel := NA]; d[curso %in% c(98,99), curso := NA]
d[, yrs := fcase(asiste==3, 0, nivel==1, 0, nivel==2, pmin(curso,8),
                 nivel==3, 8+pmin(curso,4), nivel==4, 12+pmin(curso,8), nivel==5, 16+pmin(curso,4))]

ip <- as.data.table(readRDS(file.path(DIR_CLEAN,"province_au_intensity.rds")))
setnames(ip, c("prov_code","year","intensity"), c("prov","yr","int"))
# at-15 snapshot
d[, yr_at15 := wave - (age - 15L)]
d <- merge(d, ip[, .(prov, yr_at15=yr, exp_at15=int)], by=c("prov","yr_at15"), all.x=TRUE)
d[yr_at15 < 2016, exp_at15 := 0]
# cumulative: mean intensity over attained adolescent ages 15..min(age,19)
d[, rid := .I]
grid <- d[, .(a=15:min(age,19L)), by=.(rid, prov, wave, age)]
grid[, yr := wave - (age - a)]
grid <- merge(grid, ip, by=c("prov","yr"), all.x=TRUE)
grid[yr < 2016, int := 0]
stopifnot(grid[is.na(int) & yr>=2016, .N]==0)
cum <- grid[, .(cum_exp = mean(int)), by=rid]
d <- merge(d, cum, by="rid")
cat("exposure distributions (women 18-22, positive-exposure share and SD):\n")
print(d[, .(mean_at15=round(mean(exp_at15),3), sd_at15=round(sd(exp_at15),3),
            mean_cum=round(mean(cum_exp),3), sd_cum=round(sd(cum_exp),3),
            p90_cum=round(quantile(cum_exp,.9),3))], class=FALSE)

s <- d[!is.na(yrs)]
m_at15 <- feols(yrs ~ exp_at15 | prov^wave + age, s, weights=~fexp, cluster=~prov)
m_cum  <- feols(yrs ~ cum_exp  | prov^wave + age, s, weights=~fexp, cluster=~prov)
m_cumu <- feols(yrs ~ cum_exp  | prov^wave + age, s, cluster=~prov)                 # unweighted
m_drop <- feols(yrs ~ cum_exp  | prov^wave + age, s[!prov %in% c(1,25,32)], weights=~fexp, cluster=~prov) # drop DN, Santiago, SD prov
loo <- sapply(s[,unique(prov)], function(p)
  coef(feols(yrs ~ cum_exp | prov^wave + age, s[prov!=p], weights=~fexp, cluster=~prov))[["cum_exp"]])

sdc <- s[, sd(cum_exp)]; sda <- s[, sd(exp_at15)]
row <- function(m, v, lab, sdx) data.table(spec=lab,
  coef=round(coef(m)[[v]],4), se=round(se(m)[[v]],4), p=round(coeftable(m)[v,4],3),
  per10pp=round(coef(m)[[v]]*0.10,4), perSD=round(coef(m)[[v]]*sdx,4), n=nobs(m))
res <- rbind(
  row(m_at15, "exp_at15", "at-15 snapshot (=08r2 B2)", sda),
  row(m_cum,  "cum_exp",  "cumulative ages 15-19", sdc),
  row(m_cumu, "cum_exp",  "cumulative, unweighted", sdc),
  row(m_drop, "cum_exp",  "cumulative, drop DN/Santiago/SD", sdc))
cat("\n=== cohort design v3: years of educ, women 18-22 (prov x wave FE + age FE, cluster prov) ===\n")
print(res, class=FALSE)
cat(sprintf("\nLOO range (cumulative, weighted): [%.3f, %.3f]; all same sign: %s\n",
    min(loo), max(loo), all(sign(loo)==sign(coef(m_cum)[["cum_exp"]]))))
fwrite(res, file.path(TAB,"enhogar_cohort_v3.csv"))
cat("saved -> enhogar_cohort_v3.csv\n")
