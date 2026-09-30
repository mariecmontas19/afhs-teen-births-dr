# ============================================================================
# 07p2_newupgrade_difftest.R — formal test that first openings and upgrades of
# pre-existing units have different effects (JC review #45, 2026-09-30).
# Table A17 reports the two estimates from separate Callaway-Sant'Anna runs that
# share the 126 never-treated controls, so the estimates are correlated and a
# z-test that treats them as independent is conservative. The difference is
# tested with the runs' influence functions aligned by municipality:
#   est_k - theta_k ~ sum_i IF_{k,i} / n_k   (did::getSE: se = sqrt(mean(IF^2)/n)),
# so Var(a - b) = sum_i (IF_{a,i}/n_a - IF_{b,i}/n_b)^2, with a zero contribution
# for municipalities absent from a run; a joint multiplier bootstrap with common
# Mammen weights per municipality gives the bootstrap counterpart (did's IQR SE).
# Specification, samples and seeds are identical to 07p (the rows are asserted
# to reproduce cs_att_decomposition.csv before anything is written).
# Output: output/tables/newupgrade_difftest.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
TAB <- file.path(PROJ,"output","tables")
fold <- c("DOM010905","DOM051703","DOM012510")

## ---- samples: verbatim from 07p ---------------------------------------------
p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))
modk <- mod[!adm3_pcode %in% fold, .(adm3_pcode, mod_cohort, mod_ever_treated, always_treated_mod, mod_first_type)]
dropc <- intersect(names(modk)[-1], names(p)); if (length(dropc)) p <- p[, !dropc, with=FALSE]
p <- merge(p, modk, by="adm3_pcode", all.x=TRUE)
p <- p[always_treated_mod==0 | is.na(always_treated_mod)]
never       <- p[mod_ever_treated==0]
firstaccess <- p[always_treated==0 & mod_ever_treated==1]
recnew      <- p[always_treated==1 & mod_ever_treated==1 & mod_first_type=="new"]
recupg      <- p[always_treated==1 & mod_ever_treated==1 & mod_first_type=="upgrade"]

## ---- one CS run, returning the estimate and per-municipality contributions --
cs_if <- function(treated){
  d <- rbind(never, treated)
  lev <- sort(unique(d$adm3_pcode)); d[, id := match(adm3_pcode, lev)]
  d[, g := fifelse(mod_ever_treated==1L, as.integer(mod_cohort), 0L)]
  set.seed(20260722)                                           # same per-call seed as 07p
  a <- att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g", xformla=~1, data=d,
              control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=2000,
              base_period="universal", clustervars="id", est_method="reg", print_details=FALSE)
  o <- aggte(a, type="group", na.rm=TRUE)
  inf <- as.numeric(o$inf.function$selective.inf.func)
  # row order of the influence function = first-period rows of the estimation data
  dd <- as.data.table(o$DIDparams$data)
  ids <- dd[get(o$DIDparams$tname) == min(dd[[o$DIDparams$tname]]), get(o$DIDparams$idname)]
  n <- length(inf)
  stopifnot(length(ids)==n, n==uniqueN(d$adm3_pcode), !anyDuplicated(ids))
  list(att=o$overall.att, se_boot=o$overall.se, se_an=sqrt(mean(inf^2)/n), n=n,
       contrib=data.table(adm3_pcode=lev[ids], c=inf/n))
}
A <- cs_if(firstaccess)                       # first openings, 2020-2025
U <- cs_if(recupg)                            # upgrades of pre-existing units
N <- cs_if(rbind(firstaccess, recnew))        # all new modern units (secondary contrast)

## ---- reproduce Table A17 before using anything ------------------------------
dec <- fread(file.path(TAB,"cs_att_decomposition.csv"))
chk <- function(obj, pat){ r <- dec[grepl(pat, layer)]; stopifnot(nrow(r)==1)
  stopifnot(abs(round(obj$att,2) - r$ATT) < 1e-9, abs(round(obj$se_boot,2) - r$SE) < 1e-9) }
chk(A, "^First openings"); chk(U, "^Recovered: upgrades"); chk(N, "^All new modern units")
cat(sprintf("reproduced A17: first openings %.2f (%.2f) | upgrades %.2f (%.2f) | all new %.2f (%.2f)\n",
            A$att, A$se_boot, U$att, U$se_boot, N$att, N$se_boot))
cat(sprintf("analytic IF SEs: first openings %.2f | upgrades %.2f | all new %.2f\n", A$se_an, U$se_an, N$se_an))

## ---- difference tests -------------------------------------------------------
difftest <- function(X, Y, label){
  m <- merge(X$contrib, Y$contrib, by="adm3_pcode", all=TRUE, suffixes=c(".x",".y"))
  m[is.na(c.x), c.x := 0]; m[is.na(c.y), c.y := 0]
  shared <- m[c.x != 0 & c.y != 0, .N]
  dlt <- X$att - Y$att
  se_ind <- sqrt(X$se_an^2 + Y$se_an^2)                    # independence benchmark
  se_joint <- sqrt(sum((m$c.x - m$c.y)^2))                  # aligned influence functions
  rho <- sum(m$c.x*m$c.y) / (X$se_an*Y$se_an)               # implied correlation of the estimates
  set.seed(20260930)                                        # joint multiplier bootstrap
  B <- 2000; kap <- (sqrt(5)+1)/2; pr <- kap/sqrt(5)
  v <- matrix(ifelse(runif(nrow(m)*B) < pr, -(sqrt(5)-1)/2, kap), nrow=nrow(m))   # Mammen weights
  bd <- as.numeric(crossprod(v, m$c.x - m$c.y))
  se_bs <- (quantile(bd, .75) - quantile(bd, .25)) / (qnorm(.75) - qnorm(.25))
  data.table(contrast=label, est_x=round(X$att,2), est_y=round(Y$att,2), difference=round(dlt,2),
             se_joint=round(se_joint,2), p_joint=signif(2*pnorm(-abs(dlt/se_joint)),3),
             se_boot=round(unname(se_bs),2), p_boot=signif(2*pnorm(-abs(dlt/se_bs)),3),
             se_indep=round(se_ind,2), p_indep=signif(2*pnorm(-abs(dlt/se_ind)),3),
             corr=round(rho,3), n_units=nrow(m), shared_units=shared)
}
res <- rbind(difftest(A, U, "First openings (20) minus upgrades (6)"),
             difftest(N, U, "All new modern units (23) minus upgrades (6)"))
print(res, class=FALSE)
fwrite(res, file.path(TAB,"newupgrade_difftest.csv"))
cat("saved -> newupgrade_difftest.csv\n")
