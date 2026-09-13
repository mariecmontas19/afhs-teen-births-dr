# ============================================================================
# 08_estimators.R — heterogeneity-robust ESTIMATOR comparison for the LEVEL teen
# birth rate (15-19), under the new specs S1 (binary any-unit) & S2 (modernization).
# "Do the modern estimators agree with Callaway-Sant'Anna?" — the standard staggered-
# DiD robustness. Five estimators (overall / static post-ATT):
#   CS  = did::att_gt (notyettreated, est=reg) -> aggte group   [headline]
#   SA  = fixest::sunab (Sun-Abraham 2021), agg="att"
#   BJS = didimputation::did_imputation (Borusyak-Jaravel-Spiess 2024 imputation)
#   G2S = did2s::did2s (Gardner 2022 two-stage)
#   TWFE= fixest::feols static post dummy (the OLD Galarraga-style benchmark; biased
#         under heterogeneity — included as the contrast, not a result)
# NOTE: de Chaisemartin-D'Haultfoeuille (DIDmultiplegtDYN) and wild-cluster bootstrap
# (fwildclusterboot) are NOT installed (no binary for R 4.3) — documented, not run.
# Same fold(3) + treatment construction as 07t headline. Denominator A.
# Output: output/tables/estimators_S1_S2.csv + output/figures/fig_b03_estimators_overlay.png
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(fixest); library(didimputation); library(did2s); library(ggplot2)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold, .(adm3_pcode, year, rateA_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))[!adm3_pcode %in% fold]
mod[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]

# build per-scenario estimation frame (drop always-treated = NA g)
mkdat <- function(gt){
  d <- merge(p, gt[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  d[, id := as.integer(factor(adm3_pcode))]
  d[, g_sa := fifelse(g>0, g, 10000L)]                 # Sun-Abraham: never = 10000
  d[, post := as.integer(g>0 & year>=g)]               # binary post for G2S/TWFE
  d[, event_time := fifelse(g>0, year-g, -1000L)]
  d[]
}
dS1 <- mkdat(bin); dS2 <- mkdat(mod)

# ---- dynamic overlay (CS vs BJS), S1 headline — RUN FIRST, before did2s touches
#      BLAS/memory (did::aggte segfaults if did2s ran earlier in the session) ----
csdyn <- function(d){
  a<-suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19",tname="year",idname="id",gname="g",xformla=~1,
       data=d,control_group="notyettreated",base_period="universal",est_method="reg",
       bstrap=TRUE,cband=TRUE,biters=2000,clustervars="id",print_details=FALSE)))
  e<-suppressMessages(aggte(a,type="dynamic",na.rm=TRUE))   # full window (min_e/max_e segfaults in did)
  data.table(e=e$egt, att=e$att.egt, se=e$se.egt, est="CS")[e>=-5 & e<=4] }
bjsdyn <- function(d){
  m<-did_imputation(data=d, yname="rateA_15_19", gname="g", tname="year", idname="id",
                    cluster_var="adm3_pcode", horizon=TRUE, pretrends=-5:-2)
  m<-as.data.table(m); m[, e:=as.integer(as.character(term))]
  m[e>=-5 & e<=4, .(e, att=estimate, se=std.error, est="BJS")] }
ov <- rbind(csdyn(dS1), bjsdyn(dS1))[order(est,e)]
ov[, `:=`(lo=att-1.96*se, hi=att+1.96*se, est=factor(est, levels=c("CS","BJS"),
          labels=c("Callaway-Sant'Anna","Borusyak imputation")))]
pal <- c("Callaway-Sant'Anna"=PCUA_COL$blue, "Borusyak imputation"="grey45")
g <- ggplot(ov, aes(e, att, color=est, fill=est)) +
  es_guides() +
  geom_hline(yintercept=0, color="grey25", linewidth=0.7) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0.14, linewidth=0.7, position=position_dodge(0.35), na.rm=TRUE) +
  geom_point(position=position_dodge(0.35), size=2.6, shape=21, fill="white", stroke=1.2, na.rm=TRUE) +
  scale_color_manual(values=pal) + scale_fill_manual(values=pal) +
  scale_x_continuous(breaks=seq(-5,4,1)) +
  labs(x="Years since first AU opening", y="ATT: teen births per 1,000 women (15-19)", color=NULL, fill=NULL) +
  theme_pcua()
ggsave(file.path(FIG,"fig_b03_estimators_overlay.png"), g, width=8.6, height=5.2, dpi=200)
cat("saved overlay figure (computed before did2s).\n")

res <- list(); add <- function(sc,est,att,se,note="") res[[length(res)+1]] <<-
  data.table(scenario=sc, estimator=est, ATT=round(att,2), SE=round(se,2),
             p=round(2*pnorm(-abs(att/se)),3), note=note)

run_all <- function(sc, d){
  # CS
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
        xformla=~1, data=d, control_group="notyettreated", base_period="universal",
        est_method="reg", bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  gr <- suppressMessages(aggte(a, type="group", na.rm=TRUE)); add(sc,"CS (Callaway-Sant'Anna)",gr$overall.att,gr$overall.se)
  # Sun-Abraham
  tryCatch({ m<-feols(rateA_15_19 ~ sunab(g_sa, year) | id + year, data=d, cluster=~id)
    ct<-summary(m, agg="att")$coeftable; add(sc,"SA (Sun-Abraham)",ct[1,1],ct[1,2]) },
    error=function(e) add(sc,"SA (Sun-Abraham)",NA,NA,substr(conditionMessage(e),1,30)))
  # Borusyak-Jaravel-Spiess imputation
  tryCatch({ m<-did_imputation(data=d, yname="rateA_15_19", gname="g", tname="year", idname="id", cluster_var="adm3_pcode")
    m<-as.data.frame(m); add(sc,"BJS (Borusyak imputation)",m$estimate[1],m$std.error[1]) },
    error=function(e) add(sc,"BJS (Borusyak imputation)",NA,NA,substr(conditionMessage(e),1,30)))
  # Gardner two-stage
  tryCatch({ m<-did2s(d, yname="rateA_15_19", first_stage=~0|id+year, second_stage=~i(post,ref=FALSE),
                      treatment="post", cluster_var="id", verbose=FALSE)
    ct<-as.data.frame(summary(m)$coeftable); add(sc,"G2S (Gardner two-stage)",ct[1,1],ct[1,2]) },
    error=function(e) add(sc,"G2S (Gardner two-stage)",NA,NA,substr(conditionMessage(e),1,30)))
  # TWFE static benchmark (biased contrast)
  tryCatch({ m<-feols(rateA_15_19 ~ post | id + year, data=d, cluster=~id)
    add(sc,"TWFE (static benchmark)",coef(m)[["post"]],se(m)[["post"]],"biased under heterogeneity") },
    error=function(e) add(sc,"TWFE (static benchmark)",NA,NA,substr(conditionMessage(e),1,30)))
  invisible(NULL)
}

cat("============== 08 ESTIMATOR COMPARISON — level teen rate (denom A) ==============\n")
run_all("S1", dS1); run_all("S2", dS2)
out <- rbindlist(res); print(out, class=FALSE)
fwrite(out, file.path(TAB,"estimators_S1_S2.csv"))
cat("\nsaved -> output/tables/estimators_S1_S2.csv + output/figures/fig_b03_estimators_overlay.png\n")
