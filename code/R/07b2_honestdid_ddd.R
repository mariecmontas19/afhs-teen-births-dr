# ============================================================================
# 07b2_honestdid_ddd.R — parallel-trends sensitivity for the TRIPLE DIFFERENCE
# using the SAME estimator as Table 3 (triplediff, Ortiz-Villavicencio &
# Sant'Anna 2025; not-yet-treated comparisons combined by GMM; first-opening
# design; 2019 cohort omitted as in 07t). MM 2026-11-13: Table A4's
# triple-difference column must use the Table 3 estimator; it previously used
# the pre-differenced gap-CS (ddd_A) from 07b / 09 block D.
#
# (1) Rambachan-Roth (2023) relative-magnitudes C-LF sets for the average
#     post-treatment effect, event window e in [-4,3] (as 07b).
# (2) Influence-function average of the raw pre-treatment ATT(g,t) cells
#     (the triplediff analogue of 09 block D).
# Covariance bridge (VERIFIED 2026-11-13): triplediff's influence function has
# one row per municipality x age cell (290 rows); summing it within municipality
# and taking crossprod/n^2 reproduces the package's clustered SEs exactly
# (asserted below to 1e-6).
# Output: output/tables/ddd_triplediff_sensitivity.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(triplediff); library(HonestDiD)})
TAB <- file.path(PROJ,"output","tables"); fold <- c("DOM010905","DOM051703","DOM012510")
MBARS <- c(0.1,0.2,0.3,0.4,0.5,0.75,1,1.5,2)

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
La <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_age_long_155.rds")))[ag %in% c("15-19","30-34"), .(adm3_pcode=id, year, ag, y=rateA)]
La[, partition := as.integer(ag=="15-19")]
d <- merge(La, bin[!is.na(g) & g != 2019L, .(adm3_pcode, state=g)], by="adm3_pcode")     # same sample as 07t ddd_one(bin)
d[, `:=`(id=as.integer(factor(paste(adm3_pcode,partition))), cluster=as.integer(factor(adm3_pcode)), time=as.integer(year))]

o  <- suppressWarnings(ddd(yname="y",tname="time",idname="id",gname="state",pname="partition",xformla=~1,data=d,
          control_group="notyettreated", base_period="universal", est_method="reg", panel=TRUE,
          cluster="cluster", boot=FALSE))
es <- agg_ddd(o, type="eventstudy", boot=FALSE)$aggte_ddd
gr <- agg_ddd(o, type="group", boot=FALSE)$aggte_ddd
hl <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1" & spec=="DDD (no covs)"]
stopifnot(abs(round(gr$overall.att,2) - hl$estimate) < 1e-9)        # same point estimate as Table 3 (-8.54)

## ---- clustered covariance from the influence function ---------------------
cl  <- o$cluster_vector; n <- nrow(es$inf.function$dynamic.inf.func.e)
IFc <- rowsum(as.matrix(es$inf.function$dynamic.inf.func.e), cl)
sigma_full <- crossprod(IFc)/n/n
ok <- !is.na(es$se.egt)
stopifnot(max(abs(sqrt(diag(sigma_full))[ok] - es$se.egt[ok])) < 1e-6)

## ---- (1) Rambachan-Roth sets ------------------------------------------------
egt <- es$egt; keep <- egt >= -4 & egt <= 3 & egt != -1
bet <- es$att.egt[keep]; sig <- sigma_full[keep, keep]
nPre <- sum(egt[keep] < 0); nPost <- sum(egt[keep] >= 0); lvec <- rep(1/nPost, nPost)
set.seed(20260722)
orig <- constructOriginalCS(betahat=bet, sigma=sig, numPrePeriods=nPre, numPostPeriods=nPost, l_vec=lvec)
sens <- suppressWarnings(createSensitivityResults_relativeMagnitudes(betahat=bet, sigma=sig,
          numPrePeriods=nPre, numPostPeriods=nPost, l_vec=lvec, Mbarvec=MBARS))
rr <- rbind(data.table(Mbar=0, lb=as.numeric(orig$lb), ub=as.numeric(orig$ub), method="Original"),
            as.data.table(sens)[, .(Mbar, lb=as.numeric(lb), ub=as.numeric(ub), method=as.character(method))])
rr[, includes0 := lb<=0 & ub>=0][, `:=`(lb=round(lb,2), ub=round(ub,2))]

## ---- (2) average pre-treatment ATT(g,t) cell --------------------------------
Mc  <- rowsum(as.matrix(o$inf_func_mat), cl)
idx <- which(o$periods < o$groups & abs(o$ATT) > 1e-12)
l   <- rep(1/length(idx), length(idx)); pt <- sum(l*o$ATT[idx])
se  <- sqrt(sum(as.numeric(Mc[, idx, drop=FALSE] %*% l)^2))/n

out <- rbind(
  cbind(stat="RR", rr, est=NA_real_, se=NA_real_, p=NA_real_, n_cells=NA_integer_),
  data.table(stat="precell", Mbar=NA_real_, lb=NA_real_, ub=NA_real_, method=NA_character_, includes0=NA,
             est=round(pt,4), se=round(se,4), p=round(2*pnorm(-abs(pt/se)),4), n_cells=length(idx)))
cat(sprintf("triplediff group ATT %.2f | event window %s | numPre %d numPost %d | max|pre lead| %.2f\n",
            gr$overall.att, paste(egt[keep],collapse=","), nPre, nPost, max(abs(bet[1:nPre]))))
print(out, class=FALSE)
fwrite(out, file.path(TAB,"ddd_triplediff_sensitivity.csv"))
cat("saved -> output/tables/ddd_triplediff_sensitivity.csv\n")
