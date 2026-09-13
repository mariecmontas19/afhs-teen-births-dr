# ============================================================================
# 08f_child_marriage_ban.R — the 2021 child-marriage ban (Ley 1-21, enacted
# Jan 2021, prohibits marriage <18) as an identification/robustness argument.
#
# The ban is NATIONAL & simultaneous -> absorbed by year FE in the staggered CS
# design, so it cannot bias the AU ATT unless it coincides differentially with
# AU timing. We address it two ways:
#
# PART A (age-discrimination test). The ban legally binds ONLY <18 (10-17);
# 18-19-year-olds can still marry (ban-free). If the ban drove the teen-birth
# decline, it would show up in 10-17, with a ~9-month+ lag (births from late
# 2021, full-year 2022+). We run the S1 CS event study on three age bands:
#   10-14 (ban-bound)  per women 10-14
#   15-17 (ban-bound)  per women 15-19  (decomposition; no single-yr denom needed)
#   18-19 (ban-free)   per women 15-19
# If the AU effect is concentrated in the ban-FREE 18-19 band, the ban is not
# the driver. (15-17 vs 18-19 mirrors the 08b age decomposition.)
#
# PART B (common-not-differential check). Plot the ban-bound minor (10-14, 15-17)
# birth rate by year for in-window-treated vs never-treated municipalities. If both
# groups move together after 2021/2022, any minor-birth dip is the COMMON national
# ban, not a differential AU effect -> year FE absorbs it.
#
# Denominator A; S1 (first AU opening); not-yet controls; muni-clustered.
# Outputs: output/tables/child_marriage_ban_age.csv,
#          output/figures/fig_ban_age_eventstudy.png, fig_ban_minor_trends.png
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")
BAN_YR <- 2021L                                  # Ley 1-21 enacted Jan 2021

# ---- teen births by age band, per municipality-year ----------------------------
b  <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
te <- b[!is.na(adm3_pcode) & age_mom %in% 10:19]
te[, band := fcase(age_mom<=14L,"b1014", age_mom<=17L,"b1517", default="b1819")]
cnt <- dcast(te, adm3_pcode + birth_year ~ band, fun.aggregate=length, value.var="age_mom")
setnames(cnt, "birth_year", "year")
for(v in c("b1014","b1517","b1819")) if(!v %in% names(cnt)) cnt[, (v):=0L]

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
       , .(adm3_pcode, year, womenA_10_14, womenA_15_19)]
d <- merge(p, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
for(v in c("b1014","b1517","b1819")) d[is.na(get(v)), (v):=0L]
d <- d[!adm3_pcode %in% fold]
cat("merge check: rows", nrow(d), "| munis", d[,uniqueN(adm3_pcode)], "| years", paste(range(d$year),collapse="-"), "\n")
d[, `:=`(rate_1014=1000*b1014/womenA_10_14,
         rate_1517=1000*b1517/womenA_15_19,
         rate_1819=1000*b1819/womenA_15_19)]
# treatment timing from AUTHORITATIVE treatment_municipio.rds (panel embedded fields are STALE
# pre-overhaul §13 — they mis-code Las Terrenas/DOM032003 as never-treated; it is treated 2024)
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
d <- merge(d, bin[, .(adm3_pcode, g)], by="adm3_pcode", all.x=TRUE)

# ---- PART A: CS group-ATT (+dynamic) per band -------------------------------
csrun <- function(yname){
  est <- d[!is.na(g)]; est[, id:=as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", base_period="universal",
        est_method="reg", bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  list(gr=suppressMessages(aggte(a,type="group",na.rm=TRUE)),
       dy=suppressMessages(aggte(a,type="dynamic",na.rm=TRUE)))
}
base_of <- function(v) d[!is.na(g)&g>0][year==g-1L, mean(get(v), na.rm=TRUE)]
row_of  <- function(label, yname, bind){
  r <- csrun(yname)$gr; bs <- base_of(yname)
  data.table(band=label, ban_status=bind, ATT=round(r$overall.att,2), SE=round(r$overall.se,2),
             p=round(2*pnorm(-abs(r$overall.att/r$overall.se)),3),
             baseline=round(bs,2), pct_of_base=round(100*r$overall.att/bs,0))
}
tabA <- rbind(row_of("10-14","rate_1014","ban-bound (<18)"),
              row_of("15-17","rate_1517","ban-bound (<18)"),
              row_of("18-19","rate_1819","ban-FREE (18-19)"))
cat("\n===== PART A: AU effect by age band (ban-bound <18 vs ban-free 18-19) =====\n")
print(tabA, class=FALSE)
fwrite(tabA, file.path(TAB,"child_marriage_ban_age.csv"))

# dynamic figure: the three bands
bands <- list(c("rate_1014","10-14 (ban-bound)"), c("rate_1517","15-17 (ban-bound)"), c("rate_1819","18-19 (ban-free)"))
objs  <- lapply(bands, function(b) csrun(b[1])$dy)
pt3   <- lapply(objs, es_pretrend_p)                            # joint pre-trend per band (from dynamic IF)
dd <- rbindlist(lapply(seq_along(bands), function(i){
         r<-objs[[i]]; data.table(e=r$egt, att=r$att.egt, se=r$se.egt, band=bands[[i]][2]) }))[e>=-5 & e<=4]
dd[, `:=`(lo=att-1.96*se, hi=att+1.96*se)]
rng <- ceiling(max(abs(c(dd$lo, dd$hi)), na.rm=TRUE)/5)*5
ptlab <- function(nm,pt) if(is.na(pt$p)||pt$k==0) sprintf("%s: not estimable",nm) else
                         sprintf("%s p=%.2f (%s)",nm,pt$p,ifelse(pt$p<.05,"sig","n.s."))
ann <- paste0("Pre-trend joint test (leads):\n  ",
              paste(mapply(function(b,pt) ptlab(sub(" .*","",b[2]),pt), bands, pt3), collapse="\n  "))
cat(sprintf("pre-trend leads: %s\n", paste(mapply(function(b,pt) sprintf("%s p=%.3f",b[2],pt$p), bands, pt3), collapse=" | ")))
gA <- ggplot(dd, aes(e, att, color=band, fill=band)) +
  es_guides(label_y=rng) +
  geom_hline(yintercept=0, color="grey25", linewidth=0.7) +              # bold zero line
  geom_ribbon(aes(ymin=lo,ymax=hi), alpha=0.08, color=NA, position=position_dodge(0.3)) +
  geom_line(linewidth=0.9, position=position_dodge(0.3)) +
  geom_point(size=2.2, shape=21, fill="white", stroke=1.1, position=position_dodge(0.3)) +
  annotate("text", x=-5, y=rng*0.97, hjust=0, vjust=1, size=2.7, fontface="italic", color="grey25", label=ann) +
  scale_color_manual(values=c("10-14 (ban-bound)"=PCUA_COL$orange,"15-17 (ban-bound)"=PCUA_COL$red,
                              "18-19 (ban-free)"=PCUA_COL$blue), name="Age band") +
  scale_fill_manual(values=c("10-14 (ban-bound)"=PCUA_COL$orange,"15-17 (ban-bound)"=PCUA_COL$red,
                             "18-19 (ban-free)"=PCUA_COL$blue), guide="none") +
  scale_x_continuous(breaks=seq(-5,4,1)) +
  scale_y_continuous(limits=c(-rng, rng), breaks=seq(-rng, rng, 5)) +
  labs(title="The AU decline lives in the ban-FREE age band (18-19), not the ban-bound minors",
       subtitle="S1 CS event study by mother's age band. The 2021 child-marriage ban binds only <18, so it cannot explain an 18-19 effect.",
       x="Years since first AU opening", y="ATT: births per 1,000 women in band's denominator",
       caption="10-14 per women 10-14; 15-17 & 18-19 per women 15-19. Denominator A; not-yet controls; 95% CIs; muni-clustered.") +
  theme_pcua()
ggsave(file.path(FIG,"fig_ban_age_eventstudy.png"), gA, width=9.4, height=5.4, dpi=200)

# ---- PART B: ban-bound minor trends, treated vs never (common-not-differential)
grp <- d[!is.na(g)]
grp[, arm := fifelse(g>0,"AU-treated (in-window)","Never-treated")]
agg <- grp[, .(rate_1014=1000*sum(b1014)/sum(womenA_10_14),
               rate_1517=1000*sum(b1517)/sum(womenA_15_19)), by=.(arm, year)]
aggl <- melt(agg, id.vars=c("arm","year"), variable.name="band", value.name="rate")
aggl[, band := factor(band, levels=c("rate_1014","rate_1517"),
                      labels=c("10-14 (ban-bound)","15-17 (ban-bound)"))]
gB <- ggplot(aggl, aes(year, rate, color=arm)) +
  annotate("rect", xmin=BAN_YR+0.75, xmax=2025.4, ymin=-Inf, ymax=Inf, fill="grey85", alpha=0.45) +
  geom_vline(xintercept=BAN_YR, linetype="dashed", color="grey45") +
  geom_line(linewidth=0.9) + geom_point(size=2, shape=21, fill="white", stroke=1) +
  facet_wrap(~band, scales="free_y") +
  scale_color_manual(values=c("PCUA-treated (in-window)"=PCUA_COL$green,"Never-treated"=PCUA_COL$orange), name=NULL) +
  scale_x_continuous(breaks=seq(2016,2024,2)) +
  labs(title="Minor (<18) birth rates fall in parallel for treated and never-treated municipalities",
       subtitle="The 2021 child-marriage ban (dashed) is a COMMON national shock; shaded = births conceived post-ban (2022+). Not differential by AU.",
       x=NULL, y="Births per 1,000 women in band",
       caption="Dashed = ban enacted Jan 2021; shaded = births from ~9 months post-ban onward. Denominator A; in-window treated vs never-treated.") +
  theme_pcua()
ggsave(file.path(FIG,"fig_ban_minor_trends.png"), gB, width=9.6, height=5.2, dpi=200)
cat("\nsaved -> child_marriage_ban_age.csv + fig_ban_age_eventstudy.png + fig_ban_minor_trends.png\n")
