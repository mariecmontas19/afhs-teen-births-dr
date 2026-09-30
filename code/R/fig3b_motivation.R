# ============================================================================
# fig3b_motivation.R — motivation figures that add the LOCATION dimension the
# two-line fig3 lacked. Final committee versions:
#   MAP   "time-lapse choropleth": 15-19 birth rate by municipality for 2019
#         (registration-complete peak) / 2022 / 2025, with AU-unit municipalities
#         (units open by that year) marked. STARTS AT 2019 on purpose: 2016-2018
#         are a birth-registration RAMP (national undercount), so a 2016 panel
#         would falsely read as "already low". 2019->2025 is the genuine decline.
#   SPAGHETTI (province): 32 province 15-19 trajectories + national; y capped 100.
# Denominator A. Pure panel + muni_geom + treatment_municipio (no estimation).
# Output: output/figures/fig_03a_timelapse_map.png, fig_01_spaghetti_prov.png;
#         output/tables/national_teen_rates.csv
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(sf); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures"); TAB <- file.path(PROJ,"output","tables")
p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
g   <- readRDS(file.path(DIR_CLEAN,"muni_geom.rds"))
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[, .(adm3_pcode, ever_treated, always_treated, first_year, n_units)]

# ---- national rates table (kept; reproducible) ----
natl <- p[, .(rate_popwtd=round(1000*sum(nb_15_19)/sum(womenA_15_19),1),
              median_muni=round(median(rateA_15_19),1)), by=year][order(year)]
natl[, `:=`(vs_2019=round(100*(rate_popwtd/natl[year==2019,rate_popwtd]-1)),
            era=fifelse(year<=2019,"registration ramp (undercount)","post-2019"))]
fwrite(natl, file.path(TAB,"national_teen_rates.csv"))

# ---- MAP: time-lapse 2019/2022/2025 + open-by-year unit dots ----
yrs <- c(2019L, 2022L, 2025L); labs3 <- c("2019  (registration-complete peak)","2022","2025")
cap <- as.numeric(quantile(p[year %in% yrs, rateA_15_19], 0.95, na.rm=TRUE))
gm <- do.call(rbind, lapply(yrs, function(y){
  m <- merge(g, p[year==y, .(adm3_pcode, teen=pmin(rateA_15_19, cap))], by="adm3_pcode"); m$panel <- y; m }))
gm$panel <- factor(gm$panel, levels=yrs, labels=labs3)
cent <- as.data.table(st_coordinates(st_centroid(st_geometry(g)))); cent[, adm3_pcode := g$adm3_pcode]
udots <- rbindlist(lapply(yrs, function(y){
  o <- trt[ever_treated==1L & first_year<=y, .(adm3_pcode, n_units)]
  m <- merge(cent, o, by="adm3_pcode"); m[, panel:=y]; m }))
udots[, panel := factor(panel, levels=yrs, labels=labs3)]
cat("open units by panel year:", paste(yrs, sapply(yrs, function(y) trt[ever_treated==1 & first_year<=y,.N]), sep=":"), "\n")
gMap <- ggplot(gm) +
  geom_sf(aes(fill=teen), color="grey97", linewidth=0.05) +
  geom_point(data=udots, aes(X, Y, size=n_units), shape=21, fill="#F4B400", color="grey15", stroke=0.5, alpha=0.95) +
  facet_wrap(~panel, nrow=1) +
  scale_fill_viridis_c(option="rocket", direction=-1, name="Teen births\nper 1,000\n(15-19)") +
  scale_size_continuous(range=c(1.2,3.6), breaks=c(1,2,4), name="AUs open\nby year") +
  labs() +
  theme_pcua_map() + theme(legend.position="right", strip.text=element_text(face="bold"))
ggsave_pair(file.path(FIG,"fig_03a_timelapse_map.png"), gMap, width=11, height=4.6, dpi=200)

# ---- SPAGHETTI (province) + EXTERNAL SERIES: registration vs WDI vs surveys ----
# Teen fertility is measured differently by methodology: our civil-registration
# series UNDERCOUNTS births (completeness ~48% 2016 -> 92% 2019; WB SP.REG.BRTH.ZS
# 2019 = 92.2%). Overlay the WB/WDI UN-modeled national series + survey points so
# the figure says this explicitly. WDI = LIVE BIRTHS per 1,000 women 15-19
# (SP.ADO.TFRT, current vintage, via FRED SPADOTFRTDOM, fetched 2026-07-01).
pr  <- p[, .(rate=1000*sum(nb_15_19)/sum(womenA_15_19)), by=.(prov_norm, prov_code, year)]
pr[, sd := prov_code %in% c(1L, 32L)]                       # 01 Distrito Nacional + 32 Santo Domingo = capital metro
nat <- p[, .(rate=1000*sum(nb_15_19)/sum(womenA_15_19)), by=year][order(year)]
nclip <- pr[rate>100, .N]
wdi <- data.table(year=2013:2024, rate=c(88.768,85.739,83.159,78.640,75.887,72.619,66.982,58.852,56.058,53.582,52.774,50.163))
# 2025 point (MM 2026-09-30): ENHOGAR-MICS 2025 Informe Basico (ONE-UNICEF, June 2026), adolescent
# fertility rate for the three years before the survey = 46; plotted at the survey year like 2019.
svy <- data.table(year=c(2013,2019,2025), rate=c(90,77,46),
                  lab=c("ENDESA 2013 (survey): 90","ENHOGAR-MICS 2019 (survey): 77","ENHOGAR-MICS 2025 (survey): 46"),
                  vj=c(-0.7,-0.7,1.9), hj=c(0,0,1), nx=c(0.15,0.15,-0.15))   # 2025 label sits left of its point (right edge)
cat("provinces flagged Santo Domingo metro:", paste(sort(unique(pr[sd==TRUE]$prov_norm)), collapse=" | "), "\n")
sdlab <- pr[sd==TRUE & year==2025]; sdlab[, lab := tools::toTitleCase(tolower(prov_norm))]
gP <- ggplot() +
  annotate("rect", xmin=2016, xmax=2019.5, ymin=-Inf, ymax=Inf, fill="grey85", alpha=0.30) +
  annotate("text", x=2017.7, y=8, label="registration ramp", vjust=0, size=3, color="grey40", fontface="italic") +
  geom_line(data=pr, aes(year, rate, group=prov_norm), color="grey60", alpha=0.40, linewidth=0.35) +
  geom_line(data=wdi, aes(year, rate), color="grey45", linewidth=1.5) +
  geom_point(data=wdi, aes(year, rate), color="grey45", fill="white", shape=22, size=2.2, stroke=1.1) +
  geom_line(data=nat, aes(year, rate), color=PCUA_COL$blue, linewidth=1.7) +
  geom_point(data=nat, aes(year, rate), color=PCUA_COL$blue, fill="white", shape=21, size=2.6, stroke=1.3) +
  geom_point(data=svy, aes(year, rate), color="grey30", fill="grey30", shape=23, size=3.2) +
  geom_text(data=svy, aes(year+nx, rate, label=lab, vjust=vj, hjust=hj), color="grey30", size=2.8, fontface="bold") +
  annotate("text", x=2025, y=nat[year==2025,rate], label="  National\n  (registered births)", hjust=0, vjust=0.9, size=3.1, color=PCUA_COL$blue, fontface="bold", lineheight=0.9) +
  annotate("text", x=2024, y=wdi[year==2024,rate], label=" National (WDI,\n UN-modeled)", hjust=0, vjust=-0.2, size=3.1, color="grey45", fontface="bold", lineheight=0.9) +
  scale_x_continuous(breaks=seq(2013,2025,1), expand=expansion(mult=c(0.01,0.18))) +
  scale_y_continuous(breaks=seq(0,100,20)) +
  coord_cartesian(ylim=c(0,100)) +
  labs(x=NULL, y="Births per 1,000 women 15-19") +
  theme_pcua()
ggsave_pair(file.path(FIG,"fig_01_spaghetti_prov.png"), gP, width=10.2, height=5.8, dpi=200)

# ---- SPAGHETTI (155 municipalities), SAME y-axis as province (cap 100, breaks 20) ----
nmuni <- p[, uniqueN(adm3_pcode)]; mclip <- p[rateA_15_19>100, .N]
gM <- ggplot() +
  annotate("rect", xmin=2016, xmax=2019.5, ymin=-Inf, ymax=Inf, fill="grey85", alpha=0.30) +
  annotate("text", x=2017.7, y=100, label="registration ramp", vjust=1.4, size=3, color="grey40", fontface="italic") +
  geom_line(data=p, aes(year, rateA_15_19, group=adm3_pcode), color="grey55", alpha=0.20, linewidth=0.3) +
  geom_line(data=nat, aes(year, rate), color=PCUA_COL$blue, linewidth=1.7) +
  geom_point(data=nat, aes(year, rate), color=PCUA_COL$blue, fill="white", shape=21, size=2.6, stroke=1.3) +
  annotate("text", x=2025, y=nat[year==2025,rate], label="  national", hjust=0, size=3.2, color=PCUA_COL$red, fontface="bold") +
  scale_x_continuous(breaks=seq(2016,2025,1), expand=expansion(mult=c(0.01,0.09))) +
  scale_y_continuous(breaks=seq(0,100,20)) +
  coord_cartesian(ylim=c(0,100)) +
  labs(x=NULL, y="Births per 1,000 women 15-19") +
  theme_pcua()
ggsave(file.path(FIG,"fig3b_spaghetti.png"), gM, width=9.2, height=5.4, dpi=200)

cat(sprintf("winsor cap %.0f | province-years >100: %d | municipality-years >100: %d | saved -> fig_03a_timelapse_map.png + fig_01_spaghetti_prov.png + fig3b_spaghetti.png + national_teen_rates.csv\n", cap, nclip, mclip))
