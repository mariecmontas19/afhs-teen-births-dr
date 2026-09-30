# ============================================================================
# 10g_graphical_abstract.R — JDE graphical abstract (MM 2026-09-16/30).
# JDE guide: separate file, 531 x 1328 px (h x w) or proportionally more,
# readable at 5 x 13 cm; TIFF/EPS/PDF/Office. Three panels, left to right:
#   map of the 20 first-opening municipalities | year-by-year effect after
#   opening (CS, years 0-2; dynamic_panelB.csv) | three result tiles.
# Every estimate is read from an output CSV; only the sample descriptors in the
# footer (1.44 million births, 146 municipalities) are the paper's fixed facts.
# Output: output/figures/graphical_abstract.{pdf,png}
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2); library(sf); library(patchwork)})
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")
BLUE <- PCUA_COL$blue; G1 <- "grey20"; G2 <- "grey55"; G3 <- "grey90"
fs <- 2.3   # geom_text size (mm-ish); readable when printed at 13 x 5 cm

## ---- numbers (all from CSVs) ------------------------------------------------
hl  <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1" & spec=="CS, no covs"]
dyn <- fread(file.path(TAB,"dynamic_panelB.csv"))[scenario=="S1" & est=="CS" & thin==FALSE]
br  <- fread(file.path(TAB,"bridge_cs.csv"))
prg <- br[grepl("^\\(3\\) pregnancy events", outcome)]; abo <- br[grepl("^\\(4\\) abortions", outcome)]
hm  <- fread(file.path(TAB,"het_master_table.csv"))[label=="Nearest facility (km)"]
stopifnot(nrow(hl)==1, nrow(dyn)==3, nrow(prg)==1, nrow(abo)==1, nrow(hm)==1)
pct_head <- round(100*hl$estimate/hl$baseline)          # -12
pct_yr2  <- round(100*dyn[yr==2, att]/hl$baseline)       # -17

## ---- panel 1: map -----------------------------------------------------------
geo <- readRDS(file.path(DIR_CLEAN,"muni_geom.rds"))
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[, .(adm3_pcode, ever_treated, always_treated)]
geo <- merge(geo, trt, by="adm3_pcode", all.x=TRUE)
geo$grp <- ifelse(geo$ever_treated %in% 1 & geo$always_treated %in% 0, "first", ifelse(geo$always_treated %in% 1, "pre", "none"))
stopifnot(sum(geo$grp=="first")==20)
p1 <- ggplot(geo) + geom_sf(aes(fill=grp), color="white", linewidth=0.08) +
  scale_fill_manual(values=c(first=BLUE, pre=G2, none=G3), guide="none") +
  labs(title="20 municipalities open their\nfirst adolescent unit, 2020-25") +
  theme_void(base_size=6) + theme(plot.title=element_text(size=6.2, face="bold", color=G1, lineheight=0.95, hjust=0.5))

## ---- panel 2: year-by-year effect ---------------------------------------------
d2 <- rbind(data.table(yr=-1, att=0, se=0), dyn[, .(yr, att, se)])
d2[, `:=`(lo=att-1.96*se, hi=att+1.96*se)]
p2 <- ggplot(d2, aes(yr, att)) +
  geom_hline(yintercept=0, color=G1, linewidth=0.3) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0.12, color=BLUE, linewidth=0.35, data=d2[yr>=0]) +
  geom_point(shape=21, fill="white", color=BLUE, size=1.3, stroke=0.6) +
  annotate("text", x=2.15, y=1.2, hjust=1, vjust=0, size=fs, color=G1, lineheight=0.95,
           label=sprintf("%.1f births per 1,000 (%d%%)\n%d%% by year 2", hl$estimate, pct_head, pct_yr2)) +
  scale_x_continuous(breaks=-1:2, labels=c("before","opening\nyear","year 1","year 2")) +
  scale_y_continuous(limits=c(min(d2$lo)-0.5, 6.5), breaks=seq(-15,0,5)) +
  labs(title="Teen births fall after opening", x=NULL, y="Effect per 1,000 women 15-19") +
  theme_pcua(base_size=6) +
  theme(plot.title=element_text(size=6.2, face="bold", color=G1, hjust=0.5), axis.title.y=element_text(size=5.2),
        axis.text=element_text(size=5), panel.grid.minor=element_blank())

## ---- panel 3: result tiles ----------------------------------------------------
tiles <- data.table(y=c(3,2,1),
  big=c(sprintf("%.1f%%", prg$pct), sprintf("%.1f%%", abo$pct), sprintf("%.1f vs %.1f", hm$att_s, hm$att_u)),
  small=c("pregnancy events\nin public hospitals", "abortion-related\nattendances",
          "per 1,000, near vs\nfar from a hospital"))
p3 <- ggplot(tiles) +
  geom_tile(aes(x=0.5, y=y), width=1, height=0.86, fill="grey96", color=G3, linewidth=0.3) +
  geom_text(aes(x=0.04, y=y, label=big), hjust=0, size=2.6, fontface="bold", color=BLUE) +
  geom_text(aes(x=0.56, y=y, label=small), hjust=0, size=fs*0.85, color=G1, lineheight=0.9) +
  coord_cartesian(xlim=c(0,1), ylim=c(0.5,3.5), expand=FALSE) +
  labs(title="Fewer pregnancies, largest where\nadolescents can reach the hospital") +
  theme_void(base_size=6) + theme(plot.title=element_text(size=6.2, face="bold", color=G1, lineheight=0.95, hjust=0.5))

ga <- (p1 | p2 | p3) + plot_layout(widths=c(1, 1.1, 1.35)) +
  plot_annotation(caption="Dominican Republic | 1.44 million registered births | 146 municipalities | staggered difference-in-differences",
                  theme=theme(plot.caption=element_text(size=5, color=G2, hjust=0.5)))
ggsave(file.path(FIG,"graphical_abstract.pdf"), ga, width=13, height=5, units="cm", device=grDevices::pdf, useDingbats=FALSE)
grDevices::embedFonts(file.path(FIG,"graphical_abstract.pdf"))   # Ghostscript: embed fonts (Elsevier requirement; cairo unavailable here)
ggsave(file.path(FIG,"graphical_abstract.png"), ga, width=13, height=5, units="cm", dpi=520)
cat("saved -> graphical_abstract.pdf + .png\n")
