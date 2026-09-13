# ============================================================================
# 10r_rollout_figure.R — replaces the old Table 3 (AU roll-out by cohort) with
# a figure. Three candidate designs are written so MM can choose:
#   A  treatment-timing panel (panelView style): one row per municipality,
#      ordered by adoption year, shaded by treatment status each year.
#   B  cumulative step plot: municipalities served and units open over time.
#   C  cohort bars: municipalities per adoption year, always-served separated.
# Counts are read from treatment_municipio.rds + the verified unit record, so
# they must reproduce the retired table exactly (checked by stopifnot below).
# Output: fig_03b_rollout_timing.png / fig_rollout_cumulative.png / fig_rollout_bars.png
# ============================================================================
source("code/R/00_config.R"); source("code/R/00_theme.R")
suppressMessages({library(data.table); library(ggplot2)})
FIG <- DIR_FIGURES

tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
u  <- fread(file.path(DIR_NOTES,"pcua_unit_dates_verified.csv"))

# Restrict to the ESTIMATION PANEL (155 municipalities). treatment_municipio.rds
# covers all 158; the three post-2020 municipalities without a population series
# are not in the panel, and keeping them would report 129 never-served against
# the 126 controls that Table 1 and the estimates actually use.
panel_munis <- unique(as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))$adm3_pcode)
tr <- tr[adm3_pcode %in% panel_munis]
stopifnot(nrow(tr) == 155L,
          tr[ever_treated==0L, .N] == 126L,
          tr[always_treated==1L, .N] == 9L,
          tr[ever_treated==1L & always_treated==0L, .N] == 20L)

YRS <- 2016:2025
# units per municipality-cohort from the verified unit record
u[, yr := suppressWarnings(as.integer(recorded_year))]
uc <- tr[ever_treated==1, .(adm3_pcode, first_year, always_treated)]
nu <- u[!is.na(adm3_pcode), .(units=.N), by=adm3_pcode]
uc <- merge(uc, nu, by="adm3_pcode", all.x=TRUE); uc[is.na(units), units:=1L]

chk <- uc[, .(municipalities=.N, units=sum(units)), by=.(first_year, always_treated)][order(first_year)]
cat("roll-out reproduced from source:\n"); print(chk, class=FALSE)
stopifnot(chk[always_treated==0, sum(municipalities)] == 20L)
stopifnot(chk[always_treated==1, sum(municipalities)] ==  9L)

# ---------------------------------------------------------------- A: timing panel
# The 126 never-served municipalities are collapsed into one annotated band:
# drawn individually they occupy 80% of the panel and compress the staircase
# that the figure exists to show.
ev  <- tr[ever_treated==1L][order(always_treated==0L, first_year, adm3_pcode)]
ev[, row := .I]
gA  <- CJ(row=ev$row, year=YRS)
gA  <- merge(gA, ev[, .(row, first_year, always_treated)], by="row")
gA[, status := fifelse(always_treated==1L, "Always served (excluded from estimation)",
              fifelse(year >= first_year, "Unit open", "Not yet treated"))]
nrow_ev <- nrow(ev); band_h <- 6                       # visual height of the collapsed band
n_never <- tr[ever_treated==0L, .N]
lab_y   <- ev[always_treated==0L, .(y=mean(row)), by=first_year]

pA <- ggplot(gA, aes(year, row, fill=status)) +
  geom_tile(colour="white", linewidth=.25, height=.9) +
  annotate("rect", xmin=min(YRS)-.5, xmax=max(YRS)+.5,
           ymin=nrow_ev+1.2, ymax=nrow_ev+1.2+band_h, fill="grey90", colour="white") +
  annotate("text", x=mean(range(YRS)), y=nrow_ev+1.2+band_h/2,
           label=sprintf("%d never-served municipalities (untreated in every year)", n_never),
           size=3.1, colour="grey30") +
  geom_text(data=lab_y, aes(x=min(YRS)-.75, y=y, label=first_year), inherit.aes=FALSE,
            hjust=1, size=2.9, colour="grey30") +
  scale_fill_manual(values=c("Unit open"="#1F4E79", "Not yet treated"="grey85",
                             "Always served (excluded from estimation)"="grey60"), name=NULL) +
  scale_x_continuous(breaks=YRS, expand=expansion(add=c(2.2,.4))) +
  scale_y_reverse(expand=expansion(add=c(.8,1))) +
  labs(x=NULL, y=NULL) +
  theme_pcua() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                       panel.grid=element_blank(), legend.position="top")
ggsave(file.path(FIG,"fig_03b_rollout_timing.png"), pA, width=9, height=5.4, dpi=300)

# ------------------------------------------------------------ B: cumulative step
cum <- rbindlist(lapply(YRS, function(y) data.table(year=y,
        municipalities = uc[always_treated==0 & first_year<=y, .N],
        units          = uc[always_treated==0 & first_year<=y, sum(units)])))
cl <- melt(cum, id.vars="year", variable.name="what", value.name="n")
cl[, what := factor(what, levels=c("units","municipalities"),
                    labels=c("Units open","Municipalities served"))]
pB <- ggplot(cl, aes(year, n, colour=what, shape=what)) +
  geom_step(linewidth=1, direction="hv") + geom_point(size=2.6) +
  scale_x_continuous(breaks=YRS) +
  scale_colour_manual(values=c("Units open"="#C0504D","Municipalities served"="#1F4E79"), name=NULL) +
  scale_shape_manual(values=c(16,17), name=NULL) +
  labs(x=NULL, y="Cumulative count") +
  theme_pcua() + theme(legend.position="top")
ggsave(file.path(FIG,"fig_rollout_cumulative.png"), pB, width=8, height=4.6, dpi=300)

# ---------------------------------------------------------------- C: cohort bars
bars <- uc[, .(municipalities=.N, units=sum(units)), by=.(first_year, always_treated)]
bars[, arm := fifelse(always_treated==1L, "Always served (pre-2016, excluded)", "In-window cohort (2020-25)")]
bars[, lbl := sprintf("%d muni.\n%d unit%s", municipalities, units, fifelse(units==1L,"","s"))]
pC <- ggplot(bars, aes(factor(first_year), municipalities, fill=arm)) +
  geom_col(width=.72) +
  geom_text(aes(label=lbl), vjust=-0.3, size=2.5, lineheight=.95, colour="grey25") +
  scale_fill_manual(values=c("In-window cohort (2020-25)"="#1F4E79",
                             "Always served (pre-2016, excluded)"="grey62"), name=NULL) +
  scale_y_continuous(expand=expansion(mult=c(0,.30))) +
  labs(x="Year of first unit in the municipality", y="Municipalities") +
  theme_pcua() + theme(legend.position="top")
ggsave(file.path(FIG,"fig_rollout_bars.png"), pC, width=9.4, height=4.8, dpi=300)

cat("saved 3 candidate figures to", FIG, "\n")
