# ============================================================================
# 08b_composition.R — MECHANISM / composition: who are the prevented teen births?
# Decompose the 15-19 birth rate into mutually-exclusive components (each per 1,000
# women 15-19, so each set SUMS to the teen rate) and run the S1 CS event study on
# each -> a causal "composition effect" table:
#   AGE         : 15-17 vs 18-19  (denominator = women 15-19, well-measured; sums to teen rate)
# NATIONALITY is done as a SHARE, NOT a rate: the population denominator under-counts
# (undocumented) Haitian women, so "Haitian births / all women" is not a valid rate (MM).
# The SHARE (% Haitian among teen births) is births/births -> no denominator -> clean
# composition measure (11/1550 muni-years have 0 teen births -> allow_unbalanced_panel).
# Education (2020+) and union (2021+)
# are SHORT panels -> descriptive treated-vs-never levels only (NOT event studies).
# Denominator A; not-yet controls; est=reg; universal base; municipality-clustered.
# Outputs: data/clean/composition_muni_year.rds, output/tables/composition_effects.csv,
#          output/tables/composition_shares.csv, output/figures/fig_composition_age.png
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")

b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
te <- b[age_mom %in% 15:19 & !is.na(adm3_pcode)]
te[, `:=`(a1517=as.integer(age_mom<=17), haiti=as.integer(nationality=="Haitian"),
          loweduc=fifelse(is.na(educ), NA_integer_, as.integer(!(educ %in% c("Secondary","University")))),
          inunion=fifelse(is.na(in_union), NA_integer_, as.integer(in_union==1L | in_union==TRUE)))]
comp <- te[, .(nb_teen=.N, nb_1517=sum(a1517), nb_h=sum(haiti),
               pct_haitian=mean(haiti, na.rm=TRUE), pct_loweduc=mean(loweduc, na.rm=TRUE),
               pct_inunion=mean(inunion, na.rm=TRUE)), by=.(adm3_pcode, year=birth_year)]

p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, womenA_15_19, rateA_15_19)]
d  <- merge(p, comp, by=c("adm3_pcode","year"), all.x=TRUE)
d[is.na(nb_teen), `:=`(nb_teen=0L, nb_1517=0L, nb_h=0L)]
d[nb_teen==0, pct_haitian := NA_real_]                                    # share undefined w/ 0 births
d[, `:=`(rate_1517=1000*nb_1517/womenA_15_19, rate_1819=1000*(nb_teen-nb_1517)/womenA_15_19)]
# nationality: SHARE only (pct_haitian, computed in `comp` with na.rm) — no rate (denominator invalid)
d <- d[!adm3_pcode %in% fold]
# treatment timing from AUTHORITATIVE treatment_municipio.rds (panel embedded fields are STALE
# pre-overhaul §13 — they mis-code Las Terrenas/DOM032003 as never-treated; it is treated 2024)
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
d <- merge(d, bin[, .(adm3_pcode, g)], by="adm3_pcode", all.x=TRUE)
saveRDS(d, file.path(DIR_CLEAN,"composition_muni_year.rds"))

# ---- CS group-ATT (+ dynamic for the figure) for a decomposition outcome ----
csrun <- function(yname, unbal=FALSE){
  est <- d[!is.na(g)]; est[, id:=as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g", xformla=~1,
        data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        allow_unbalanced_panel=unbal, bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  list(gr=suppressMessages(aggte(a,type="group",na.rm=TRUE)), dy=suppressMessages(aggte(a,type="dynamic",na.rm=TRUE)))
}
base_of <- function(v) d[!is.na(g)&g>0][year==g-1L, mean(get(v), na.rm=TRUE)]
row_of  <- function(block, label, yname, unbal=FALSE, share=FALSE){
  r <- csrun(yname, unbal)$gr; bs <- base_of(yname)
  data.table(block=block, component=label, ATT=round(r$overall.att,2), SE=round(r$overall.se,2),
             p=round(2*pnorm(-abs(r$overall.att/r$overall.se)),3),
             baseline=round(bs,2), pct_of_base=round(100*r$overall.att/bs,0),
             unit=if(share) "share (pp)" else "per 1,000 women 15-19")
}
tab <- rbind(
  row_of("Age (mother)",       "15-17 birth rate",          "rate_1517"),
  row_of("Age (mother)",       "18-19 birth rate",          "rate_1819"),
  row_of("Nationality (share)","% Haitian among teen births","pct_haitian", unbal=TRUE, share=TRUE))
cat("==================== COMPOSITION EFFECTS (S1 CS group-ATT) ====================\n")
print(tab, class=FALSE)
fwrite(tab, file.path(TAB,"composition_effects.csv"))

# ---- education / union: SHORT panel -> descriptive treated-vs-never levels only ----
prepost <- function(var, yrs){
  x <- d[year %in% yrs & !is.na(g)]
  data.table(measure=var, years=paste(range(yrs),collapse="-"),
             treated=round(x[g>0, weighted.mean(get(var), nb_teen, na.rm=TRUE)],3),
             never  =round(x[g==0, weighted.mean(get(var), nb_teen, na.rm=TRUE)],3),
             note="DESCRIPTIVE level diff (selection); panel too short for event study")
}
sh <- rbind(prepost("pct_loweduc",2020:2025), prepost("pct_inunion",2021:2025))
cat("\nEducation/union (descriptive, short panel):\n"); print(sh, class=FALSE)
fwrite(sh, file.path(TAB,"composition_shares.csv"))

# ---- figure kept: age decomposition dynamic (older vs younger) ----
ra <- csrun("rate_1517"); rb <- csrun("rate_1819")
dd <- rbind(data.table(e=ra$dy$egt, att=ra$dy$att.egt, se=ra$dy$se.egt, age="15-17"),
            data.table(e=rb$dy$egt, att=rb$dy$att.egt, se=rb$dy$se.egt, age="18-19"))[e>=-5 & e<=4]
dd[, `:=`(lo=att-1.96*se, hi=att+1.96*se)]
g1 <- ggplot(dd, aes(e, att, color=age, fill=age)) + es_guides() +
  geom_ribbon(aes(ymin=lo,ymax=hi), alpha=0.12, color=NA, position=position_dodge(0.25)) +
  geom_line(linewidth=0.9, position=position_dodge(0.25)) +
  geom_point(size=2.3, shape=21, fill="white", stroke=1.1, position=position_dodge(0.25)) +
  scale_color_manual(values=c("15-17"=PCUA_COL$red, "18-19"=PCUA_COL$blue), name="Mother's age") +
  scale_fill_manual(values=c("15-17"=PCUA_COL$red, "18-19"=PCUA_COL$blue), guide="none") +
  scale_x_continuous(breaks=seq(-5,4,1)) +
  labs(title="Which teens? The decline is concentrated among OLDER (18-19) mothers",
       subtitle="S1 CS event study on the 15-17 and 18-19 birth rates (both per 1,000 women 15-19; they sum to the teen rate)",
       x="Years since first AU opening", y="ATT: births per 1,000 women (15-19)",
       caption="Denominator A; not-yet controls; 95% pointwise CIs; municipality-clustered. age_mom single-year.") +
  theme_pcua()
ggsave(file.path(FIG,"fig_composition_age.png"), g1, width=9, height=5.2, dpi=200)
cat("\nsaved -> composition_effects.csv + composition_shares.csv + composition_muni_year.rds + fig_composition_age.png\n")
