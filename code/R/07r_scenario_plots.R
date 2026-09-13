# ============================================================================
# 07r_scenario_plots.R — event-study (leads/lags) plots to inspect PRE-TRENDS for
# the key scenarios, both outcomes (level teen rate 15-19; age-DDD 15-19 - 30-34).
# Scenarios: S1 Any unit (binary), S2 Modernization (all), S4 Real NEW only.
# CS dynamic aggregation, denom A, not-yet-treated controls, uniform 95% bands.
# Output: output/figures/es_{S1,S2,S4}_{level,ddd}.png
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); FIG <- file.path(PROJ,"output","figures")

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19, ddd_A)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[, .(adm3_pcode, first_year, ever_bin=ever_treated, always_bin=always_treated)]
v   <- as.data.table(fread(file.path(PROJ,"notes","pcua_unit_dates_verified.csv")))
v[, mod_year := as.integer(substr(modern_event_date,1,4))]
v[is.na(mod_year) & !is.na(recorded_year) & recorded_year>=2016L, mod_year := recorded_year]
v[, realunit_year := fifelse(consults_only==TRUE, NA_integer_, mod_year)]
v[, tsimpl := fcase(modern_event_type=="new_opening","new",
                    modern_event_type %in% c("upgrade","upgrade_unconfirmed","old_never_modernized"),"upgrade",
                    default="new")]
muni <- v[, {ru <- realunit_year[!is.na(realunit_year)]; rt <- tsimpl[!is.na(realunit_year)]
  .(mod_cohort=min(mod_year, na.rm=TRUE),
    realunit_cohort=if(length(ru)) min(ru) else NA_integer_,
    realunit_type=if(length(ru)) rt[which.min(ru)] else NA_character_)}, by=adm3_pcode]
M <- merge(data.table(adm3_pcode=unique(p$adm3_pcode)), bin, by="adm3_pcode", all.x=TRUE)
M <- merge(M, muni, by="adm3_pcode", all.x=TRUE)
M[is.na(ever_bin), ever_bin:=0L][is.na(always_bin), always_bin:=0L]
P <- merge(p, M, by="adm3_pcode")

esdat <- function(est, yname){
  est <- copy(est); est[, idn := as.integer(factor(adm3_pcode))]
  a <- att_gt(yname=yname, tname="year", idname="idn", gname="g", xformla=~1, data=est,
              control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=2000,
              base_period="universal", clustervars="idn", est_method="reg", print_details=FALSE)
  d <- aggte(a, type="dynamic", na.rm=TRUE)
  data.table(e=d$egt, att=d$att.egt, se=d$se.egt, crit=d$crit.val.egt)
}
mkplot <- function(dd, ttl, fn){
  dd <- dd[e>=-5 & e<=4]; dd[, `:=`(lo=att-crit*se, hi=att+crit*se)]
  g <- ggplot(dd, aes(e,att)) +
    geom_hline(yintercept=0,color="grey60") + geom_vline(xintercept=-0.5,linetype="dashed",color="grey60") +
    geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.15,fill="#2c7fb8") +
    geom_line(color="#2c7fb8") + geom_point(color="#2c7fb8",size=1.8) +
    labs(title=ttl, x="Years since treatment", y="ATT (births per 1,000 women)",
         caption="CS dynamic; not-yet-treated controls; uniform 95% bands; municipality-clustered. Pre-period = e<0.") +
    theme_minimal(base_size=11) + theme(plot.title=element_text(face="bold",size=12))
  ggsave(file.path(FIG,paste0(fn,".png")), g, width=8, height=5, dpi=150)
}

# scenario estimation sets (g = cohort; 0 = control)
S1 <- P[always_bin==0];                         S1[, g := fifelse(ever_bin==1L, as.integer(first_year), 0L)]
S2 <- P[is.na(mod_cohort) | mod_cohort>=2016];  S2[, g := fifelse(!is.na(mod_cohort), as.integer(mod_cohort), 0L)]
S4 <- P[is.na(realunit_cohort) | realunit_type=="new"]; S4[, g := fifelse(!is.na(realunit_cohort) & realunit_type=="new", as.integer(realunit_cohort), 0L)]

for(s in list(list(S1,"S1","Any unit (binary)"), list(S2,"S2","All modern units"), list(S4,"S4","New openings only"))){
  est <- s[[1]]; tag <- s[[2]]; nm <- s[[3]]
  cat(tag, nm, "| treated:", uniqueN(est[g>0,adm3_pcode]), "\n")
  mkplot(esdat(est,"rateA_15_19"), paste0(nm,": teen rate 15-19 (level)"),  paste0("es_",tag,"_level"))
  mkplot(esdat(est,"ddd_A"),       paste0(nm,": age-DDD (15-19 - 30-34)"),  paste0("es_",tag,"_ddd"))
}
cat("saved -> output/figures/es_{S1,S2,S4}_{level,ddd}.png\n")
