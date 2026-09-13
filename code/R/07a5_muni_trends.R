# ============================================================================
# 07a5_muni_trends.R — appendix per-municipality TEEN fertility trajectories
# (MM 2026-08-06: redo of 07a's 16-page battery with the 30-34 series DROPPED,
# house figure format, title "Teen fertility by municipality", and NO in-image
# notes — notes live in the LaTeX captions). Ordering as before:
# in-window-treated (by opening year) -> always-treated -> never-treated.
# Output: output/figures/muni_trends_p01..p16.png (overwrites 07a's)
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures")

p  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[,
        .(adm3_pcode, ever_treated, always_treated, first_year)]
cw <- unique(as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[, .(adm3_pcode, adm3_name)])
ord <- merge(unique(p[, .(adm3_pcode)]), tr, by="adm3_pcode")
ord <- merge(ord, cw, by="adm3_pcode")
ord[, grp := fifelse(ever_treated==1 & always_treated==0, 1L, fifelse(always_treated==1, 2L, 3L))]
setorder(ord, grp, first_year, adm3_name); ord[, pos := .I]

mm <- merge(p[, .(adm3_pcode, year, rate=rateA_15_19)], ord, by="adm3_pcode")
mm[, fac := factor(adm3_name, levels=ord$adm3_name)]
per <- 10; pages <- split(ord$adm3_name, ceiling(ord$pos/per))
cat("municipalities:", nrow(ord), "| pages:", length(pages), "\n")

for (k in seq_along(pages)){
  dd <- mm[adm3_name %in% pages[[k]]]
  vl <- unique(dd[ever_treated==1, .(fac, first_year)])
  g <- ggplot(dd, aes(year, rate)) +
    { if (nrow(vl)) geom_vline(data=vl, aes(xintercept=first_year), linetype="dashed", color="grey55", linewidth=0.35) } +
    geom_line(linewidth=0.7, color=PCUA_COL$red) +
    facet_wrap(~fac, ncol=5, scales="free_y") +
    scale_x_continuous(breaks=c(2016,2020,2024)) +
    labs(x=NULL, y="Births per 1,000 women 15-19") +
    theme_pcua(base_size=11) + theme(strip.text=element_text(size=8.2))
  ggsave(file.path(FIG, sprintf("muni_trends_p%02d.png", k)), g, width=11, height=8.2, dpi=150)
}
pgmap <- rbindlist(lapply(seq_along(pages), function(k)
  data.table(page=k, order=seq_along(pages[[k]]), municipality=pages[[k]])))
fwrite(pgmap, file.path(PROJ,"output","tables","muni_trends_pages.csv"))
cat("saved -> muni_trends_p01..p", sprintf("%02d", length(pages)), " (teen-only) + muni_trends_pages.csv\n", sep="")
