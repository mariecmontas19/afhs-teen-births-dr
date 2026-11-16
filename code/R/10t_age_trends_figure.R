# ============================================================================
# 10t_age_trends_figure.R — Appendix Figure B8 (MM 2026-11-16, committee
# comment): descriptive birth rates by mother's age group, 2016-2025, for the
# 20 municipalities that received a first AU and the 126 never-treated
# municipalities. Unweighted municipality means of the rates used in the
# estimation (population Series A), so the 2019 values equal Table 1's.
# Purely descriptive: no estimation, no random numbers.
# Output: output/figures/fig_b08_age_trends.png (+ vector PDF twin)
#         output/tables/age_trends_by_group.csv (the plotted values)
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
fold <- c("DOM010905","DOM051703","DOM012510")
p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, arm := fcase(ever_treated==1L & always_treated==0L, "treated", ever_treated==0L, "never", default=NA_character_)]
d <- merge(p[, !"first_year"], tr[!is.na(arm), .(adm3_pcode, arm, open_year=first_year)], by="adm3_pcode")   # opening year from the treatment file, not the panel
nT <- d[arm=="treated", uniqueN(adm3_pcode)]; nN <- d[arm=="never", uniqueN(adm3_pcode)]
stopifnot(nT + nN == 146L)
labT <- sprintf("Municipalities with a first AU opening, %d to %d (%d)", d[arm=="treated", min(open_year)], d[arm=="treated", max(open_year)], nT)
labN <- sprintf("Never-treated municipalities (%d)", nN)
d[, grp := factor(fifelse(arm=="treated", labT, labN), levels=c(labT, labN))]

ages <- c("15-19"="rateA_15_19", "20-24"="rateA_20_24", "25-29"="rateA_25_29", "30-34"="rateA_30_34")
L <- rbindlist(lapply(names(ages), function(a)
  d[, .(rate=mean(get(ages[[a]]))), by=.(year, arm, grp)][, age := paste("Ages", a)]))
# the 2019 adolescent rates must equal Table 1 (treated vs never-treated)
t1 <- L[age=="Ages 15-19" & year==2019]
cat(sprintf("2019 rate, ages 15-19: treated %.1f | never-treated %.1f\n", t1[arm=="treated", rate], t1[arm=="never", rate]))
fwrite(L[, .(age, year, group=arm, rate=round(rate,2))][order(age, group, year)], file.path(DIR_TABLES,"age_trends_by_group.csv"))

g <- ggplot(L, aes(year, rate, color=grp, linetype=grp, shape=grp)) +
  annotate("rect", xmin=2015.6, xmax=2018.5, ymin=-Inf, ymax=Inf, fill="grey88", alpha=0.45) +   # registration still becoming complete
  geom_line(linewidth=0.9) + geom_point(size=2.2, fill="white", stroke=1) +
  facet_wrap(~age, nrow=2, scales="free_y") +
  scale_color_manual(values=c(PCUA_COL$blue, "grey40"), name=NULL) +
  scale_linetype_manual(values=c("solid","longdash"), name=NULL) +
  scale_shape_manual(values=c(21, 24), name=NULL) +
  scale_x_continuous(breaks=seq(2016, 2025, 3)) + expand_limits(y=0) +
  labs(x=NULL, y="Births per 1,000 women in the age group") +
  theme_pcua(base_size=12) +
  theme(legend.position="bottom", legend.direction="vertical", strip.text=element_text(face="bold"))
ggsave_pair(file.path(PROJ,"output","figures","fig_b08_age_trends.png"), g, width=8.6, height=6.6, dpi=200)
cat("saved -> fig_b08_age_trends.png/.pdf + age_trends_by_group.csv\n")
