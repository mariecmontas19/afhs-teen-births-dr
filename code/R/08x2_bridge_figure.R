# ============================================================================
# 08x2_bridge_figure.R — event-study figures for the BRIDGE outcomes (§59.17),
# in the 07t house style (classic error bars, open points, full e in [-5,4],
# symmetric limits, post shading, far-horizon caption counts). One PNG per
# outcome (fig_bridge_*) + combined 2x2 panel (core outcomes) if patchwork
# is available. Stillbirths excluded (descriptive only, §59.16).
# Reads: output/tables/bridge_cs.csv + bridge_cs_dynamics.csv (from 08x).
# Pre-trend annotation: weighted lead average + individually significant leads
# (the JOINT lead test is not recomputable from saved dynamics; annotation says
# "lead avg" — 07t figures carry the joint test, these carry the average).
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")

res <- fread(file.path(TAB,"bridge_cs.csv"))
dyn <- fread(file.path(TAB,"bridge_cs_dynamics.csv"))
# far-horizon counts (same for all outcomes: S1 cohorts on 2016-2025 panel)
fold <- c("DOM010905","DOM051703","DOM012510")
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), NA_integer_)]
csz <- tr[!is.na(g), .N, by=g]
nbe <- data.table(e=-5:4)[, n := sapply(e, function(ee) csz[g+ee>=2016 & g+ee<=2025, sum(N)])]

meta <- data.table(
  okey = c("(1) births, RESIDENCE (headline anchor)",
          "(2b) births, DELIVERY muni (BDNV, PUBLIC only)",
          "(2c) births, DELIVERY muni (BDNV, PRIVATE/other) [substitution]",
          "(3) pregnancy events, PUBLIC facilities (MISPAS)",
          "(4) abortions, PUBLIC facilities (MISPAS)"),
  fn  = c("fig_bridge_births_residence","fig_bridge_births_delivery_public",
          "fig_bridge_births_delivery_private","fig_bridge_pregnancies_public",
          "fig_bridge_abortions_public"),
  col = c(PCUA_COL$blue, PCUA_COL$purple, PCUA_COL$orange, PCUA_COL$green, PCUA_COL$red),
  title = c("AU opening and the 15-19 birth rate (residence; headline)",
            "AU opening and 15-19 births delivered at PUBLIC facilities in the municipality",
            "AU opening and 15-19 births delivered at PRIVATE facilities (substitution margin)",
            "AU opening and 15-19 pregnancy events at public facilities (Form 67-A)",
            "AU opening and 15-19 abortion-care events at public facilities (Form 67-A)"),
  src = c("BDNV civil registry, mother's residence",
          "BDNV civil registry, municipality of delivery, public sector",
          "BDNV civil registry, municipality of delivery, private/other sector",
          "MISPAS hospital-production registries (deliveries + abortions), facility municipality",
          "MISPAS hospital-production registries, facility municipality"),
  ystep = c(5, 5, 2, 5, 1))
stopifnot(all(meta$okey %in% unique(dyn$outcome)))

plots <- list()
for(i in seq_len(nrow(meta))){
  m <- meta[i]; r <- res[outcome==m$okey]
  dd <- dyn[outcome==m$okey & e>=-5 & e<=4]
  crit <- dd[is.finite(crit), max(crit, na.rm=TRUE)]; if(!is.finite(crit)) crit <- 1.96
  dd[, `:=`(lo=att-crit*se, hi=att+crit*se)]
  rng <- ceiling(max(abs(c(dd$lo, dd$hi)), na.rm=TRUE)/m$ystep)*m$ystep
  nsig <- dd[e>=-5 & e<=-2][2*pnorm(-abs(att/se)) < .05]
  l2 <- if(nrow(nsig)==0) "No individual lead significant" else
        sprintf("Individually sig. lead(s): e=%s", paste(nsig$e, collapse=","))
  ann <- sprintf("Group ATT %+.2f (p=%.3f) = %+.1f%% of g-1 baseline %.2f\nLeads avg %+.2f; %s",
                 r$att, r$p, r$pct, r$base_gm1, r$pre_avg, l2)
  g <- ggplot(dd, aes(e,att)) +
    es_guides(label_y=rng) +
    geom_hline(yintercept=0, color="grey25", linewidth=0.7) +
    geom_errorbar(aes(ymin=lo, ymax=hi), width=0.14, linewidth=0.7, color=m$col, na.rm=TRUE) +
    geom_point(color=m$col, fill="white", shape=21, size=2.9, stroke=1.2, na.rm=TRUE) +
    annotate("text", x=-5, y=rng*0.93, hjust=0, vjust=1, size=3.2, fontface="italic",
             color="grey25", label=ann) +
    scale_x_continuous(breaks=seq(-5,4,1)) +
    scale_y_continuous(limits=c(-rng, rng), breaks=seq(-rng, rng, m$ystep)) +
    labs(title=m$title,
         subtitle=paste0("Callaway-Sant'Anna staggered event study  |  ", m$src),
         x="Years since first AU opening", y="ATT: events per 1,000 women (15-19)",
         caption=sprintf("Uniform 95%% bands (error bars); SEs clustered by municipality. Not-yet-treated controls; reference = year before treatment. Excludes Nizao\n(hospital obstetric volume ~4x in 2024 = facility-supply change). Municipalities identifying the far points: e=+3: %d; e=+4: %d (read the group ATT, not the tail).",
                         nbe[e==3, n], nbe[e==4, n])) +
    theme_pcua()
  ggsave(file.path(FIG, paste0(m$fn,".png")), g, width=8.6, height=5.4, dpi=200)
  plots[[m$fn]] <- g
  cat("saved", m$fn, "| ATT", r$att, "| rng", rng, "\n")
}
if(requireNamespace("patchwork", quietly=TRUE)){
  comb <- patchwork::wrap_plots(plots[c(1,2,4,5)], ncol=2)
  ggsave(file.path(FIG,"fig_bridge_combined.png"), comb, width=17, height=10.4, dpi=200)
  cat("saved fig_bridge_combined (2x2: residence, delivery-public, pregnancies, abortions)\n")
} else cat("patchwork not installed - combined panel skipped (individual PNGs complete)\n")

# ============================================================================
# MAIN-TEXT 2-PANEL FIGURE (MM 2026-07-17, §59.33): births (delivery muni,
# ALL facilities) + pregnancy events (public) — the pregnancy-margin exhibit
# promoted to main results. Ex-Nizao throughout (as all bridge rows).
# Abortions stay table-only. -> fig4_pregnancy_margin.png
# ============================================================================
csz2 <- tr[!is.na(g) & adm3_pcode != "DOM051702", .N, by=g]     # ex-Nizao cohort sizes
nbe2 <- data.table(e=-5:4)[, n := sapply(e, function(ee) csz2[g+ee>=2016 & g+ee<=2025, sum(N)])]
mainmeta <- data.table(
  okey = c("(2) births, DELIVERY muni (BDNV, all facilities)",
           "(3) pregnancy events, PUBLIC facilities (MISPAS)"),
  panel = c("A. Teen births (municipality of delivery, all facilities)",
            "B. Teen pregnancy events (public facilities, Form 67-A)"),
  src = c("BDNV civil registry", "MISPAS hospital-production registries"),
  col = c(PCUA_COL$blue, PCUA_COL$green), ystep = c(5, 5))
pp <- list()
for(i in seq_len(nrow(mainmeta))){
  m <- mainmeta[i]; r <- res[outcome==m$okey]
  dd <- dyn[outcome==m$okey & e>=-5 & e<=4]
  crit <- dd[is.finite(crit), max(crit, na.rm=TRUE)]; if(!is.finite(crit)) crit <- 1.96
  dd[, `:=`(lo=att-crit*se, hi=att+crit*se)]
  rng <- ceiling(max(abs(c(dd$lo, dd$hi)), na.rm=TRUE)/m$ystep)*m$ystep
  nsig <- dd[e>=-5 & e<=-2][2*pnorm(-abs(att/se)) < .05]
  l2 <- if(nrow(nsig)==0) "No individual lead significant" else
        sprintf("Individually sig. lead(s): e=%s", paste(nsig$e, collapse=","))
  ann <- sprintf("Group ATT %+.2f (p=%.3f) = %+.1f%% of g-1 baseline %.2f\nLeads avg %+.2f; %s",
                 r$att, r$p, r$pct, r$base_gm1, r$pre_avg, l2)
  pp[[i]] <- ggplot(dd, aes(e,att)) +
    es_guides(label_y=rng) +
    geom_hline(yintercept=0, color="grey25", linewidth=0.7) +
    geom_errorbar(aes(ymin=lo, ymax=hi), width=0.14, linewidth=0.7, color=m$col, na.rm=TRUE) +
    geom_point(color=m$col, fill="white", shape=21, size=2.9, stroke=1.2, na.rm=TRUE) +
    annotate("text", x=-5, y=rng*0.93, hjust=0, vjust=1, size=3.0, fontface="italic",
             color="grey25", label=ann) +
    scale_x_continuous(breaks=seq(-5,4,1)) +
    scale_y_continuous(limits=c(-rng, rng), breaks=seq(-rng, rng, m$ystep)) +
    labs(title=m$panel, subtitle=m$src,
         x="Years since first AU opening", y="ATT per 1,000 women (15-19)") +
    theme_pcua()
}
if(requireNamespace("patchwork", quietly=TRUE)){
  gm <- pp[[1]] + pp[[2]] +
    patchwork::plot_annotation(caption=sprintf(
      "Callaway-Sant'Anna, not-yet-treated controls, reference = year before opening; uniform 95%% bands (error bars), SEs clustered by municipality.\nExcludes Nizao (hospital obstetric volume ~4x in 2024 = facility-supply change; 19 treated municipalities). Municipalities identifying the far points:\ne=+3: %d; e=+4: %d (thin horizons - read the group ATT, not the tail).",
      nbe2[e==3, n], nbe2[e==4, n]))
  ggsave(file.path(FIG,"fig4_pregnancy_margin.png"), gm, width=13.4, height=5.6, dpi=200)
  cat("saved fig4_pregnancy_margin.png (2 panels: births delivery-all + pregnancies-public)\n")
}
