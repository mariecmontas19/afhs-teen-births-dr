# ============================================================================
# fig_change_map.R — DESCRIPTIVE map + table: change in the teen (15-19) birth
# rate, 2019 -> mean(2024-2025), all 155 municipalities (MM 2026-07-10).
# Purely descriptive (no causal language): registration-complete window,
# two-year endpoint to tame single-year noise. Treated municipalities (first
# AU opening 2020-25) marked with gold dots (as in the rollout choropleth).
# Also prints the top-15 largest declines among municipalities with >=500
# teen women in 2019 (size filter vs noise), with treatment status flagged.
# Output: output/figures/fig_change_map.png + output/tables/desc_change_2019_2425.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(sf); library(ggplot2)})
fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold]
ch <- dcast(p[year %in% c(2019,2024,2025), .(adm3_pcode, year, rateA_15_19, womenA_15_19)],
            adm3_pcode ~ year, value.var=c("rateA_15_19","womenA_15_19"))
ch[, `:=`(rate2019 = rateA_15_19_2019,
          rate_end = (rateA_15_19_2024 + rateA_15_19_2025)/2,
          women2019 = womenA_15_19_2019)]
ch[, `:=`(change = rate_end - rate2019, pct_change = 100*(rate_end - rate2019)/rate2019)]
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[, .(adm3_pcode, municipality=adm3_name, province=prov_name)]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, status := fifelse(always_treated==1L, "always-treated", fifelse(ever_treated==1L, "treated 2020-25", "never-treated"))]
d <- Reduce(function(a,b) merge(a,b,by="adm3_pcode"), list(ch[, .(adm3_pcode, rate2019, rate_end, change, pct_change, women2019)], xw, tr[,.(adm3_pcode,status)]))

top <- d[women2019>=500][order(change)][1:15, .(municipality, province, status, rate2019=round(rate2019,1),
        rate_end=round(rate_end,1), change=round(change,1), pct=round(pct_change,0))]
cat("=== top-15 declines 2019 -> 2024/25 (munis with >=500 teen women; DESCRIPTIVE) ===\n")
print(top, class=FALSE)
fwrite(d[order(change), .(adm3_pcode, municipality, province, status, women2019,
        rate2019=round(rate2019,1), rate_end=round(rate_end,1), change=round(change,1), pct=round(pct_change,0))],
       file.path(TAB,"desc_change_2019_2425.csv"))

# ---- map ----
g <- readRDS(file.path(DIR_CLEAN,"muni_geom.rds")); g <- g[!g$adm3_pcode %in% fold, ]
gm <- merge(g, d, by="adm3_pcode")
lim <- quantile(abs(d$change), .95, na.rm=TRUE)
gm$change_w <- pmax(pmin(gm$change, lim), -lim)
tpts <- suppressWarnings(st_centroid(gm[gm$status=="treated 2020-25", ]))
mp <- ggplot(gm) +
  geom_sf(aes(fill=change_w), color="white", linewidth=0.1) +
  geom_sf(data=tpts, shape=21, size=2.2, fill="gold", color="grey20", stroke=0.5) +
  scale_fill_gradient2(low=PCUA_COL$blue, mid="grey95", high=PCUA_COL$red, midpoint=0,
                       name="Change in teen\nbirths per 1,000\n(2019 to 2024-25)") +
  labs(title="Where teen births fell, 2019 to 2024-25 (descriptive)",
       subtitle="Change in the registered 15-19 birth rate by municipality; gold dots = municipalities whose first AU opened 2020-25",
       caption=paste0("Descriptive only: raw changes, no counterfactual. 2019 = first registration-complete year; endpoint = 2024-25 mean",
                      " (two years to reduce single-year noise).\nColor winsorized at the 95th percentile of |change|. Denominator A; 155 municipalities (9 always-treated shown but undotted).")) +
  theme_pcua(base_size=12) +
  theme(axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank())
ggsave(file.path(FIG,"fig_change_map.png"), mp, width=9.2, height=6.4, dpi=200)
cat("saved -> fig_change_map.png + desc_change_2019_2425.csv\n")
