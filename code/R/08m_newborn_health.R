# ============================================================================
# 08m_newborn_health.R — EXTENSION: at-birth newborn-health outcomes among teen
# births (low birthweight, preterm, c-section, SGA). FULL 2016-2025 panel (these
# fields are ~98-100% recorded every year, unlike educ/union/prenatal).
#
# TWO framings, with DIFFERENT validity (stated honestly in the table + caption):
#  (1) PER-POPULATION rate = 1,000 * (teen births with outcome X) / women 15-19.
#      A clean DiD outcome (denominator unselected), the same logic as the
#      headline teen-rate and the 08b/08e decompositions. BUT it largely rides on
#      the QUANTITY channel: the unit cuts teen births ~12%, so the population
#      burden of LBW/preterm/etc. teen births falls roughly proportionally. This
#      is a "population burden" reduction, NOT evidence that surviving births are
#      healthier. -> reported as the event study (valid ITT on the burden).
#  (2) PER-BIRTH share = 100 * (teen births with X) / (teen births, X non-missing).
#      The "are the surviving births healthier / different care?" question. BUT
#      computed on a SELECTED sample — the unit changes WHO gives birth (the 18-19
#      autonomous-user shift, 08b), so the share is selection-confounded. ->
#      DESCRIPTIVE ONLY, heavy caveat (exactly like the 08e education composition).
#
# Likely UNDERPOWERED for the rare outcomes (LBW ~15%, preterm ~8%, SGA ~11% of
# teen births; per-population these are thin) with 20 treated clusters -> expect
# suggestive/null. c-section (53%) has more signal but the larger selection issue.
# Denominator A; S1 (first AU opening); not-yet controls; est=reg; universal
# base; municipality-clustered. Graph standards per 00_theme (es_pretrend_p etc., §29.2).
# Outputs: output/tables/newborn_health_effects.csv, output/figures/fig_newborn_health.png
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")
YRS <- 2016:2025
OUT <- data.table(var=c("low_bw","preterm","csection","sga"),
                  label=c("Low birthweight (<2500g)","Preterm (<37 wks)","C-section","Small-for-gest-age"),
                  col=c(PCUA_COL$red, PCUA_COL$orange, PCUA_COL$purple, PCUA_COL$blue))

# ---- teen births -> per-muni-year counts (numerator) + non-missing denominators
b  <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
te <- b[age_mom %in% 15:19 & !is.na(adm3_pcode) & birth_year %in% YRS]
agg <- te[, {
  l <- list(nb_teen=.N)
  for(v in OUT$var){ x <- get(v); l[[paste0("n_",v)]] <- sum(x==1L, na.rm=TRUE); l[[paste0("d_",v)]] <- sum(!is.na(x)) }
  l
}, by=.(adm3_pcode, year=birth_year)]

# ---- denominator (women 15-19) + treatment timing; fold to 155; drop always-treated
# treatment timing from the AUTHORITATIVE treatment_municipio.rds (NOT the panel's embedded
# ever_treated/first_year, which are STALE — pre-overhaul §13; they mis-code DOM032003 as
# never-treated when it is treated 2024). This matches the headline 07t exactly -> 20 treated.
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[
       year %in% YRS, .(adm3_pcode, year, womenA_15_19)]
d <- merge(p, agg, by=c("adm3_pcode","year"), all.x=TRUE)
zc <- c("nb_teen", paste0("n_",OUT$var), paste0("d_",OUT$var)); for(v in zc) d[is.na(get(v)), (v):=0L]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]
d <- merge(d, bin[, .(adm3_pcode, g)], by="adm3_pcode", all.x=TRUE)
d <- d[!adm3_pcode %in% fold]
nb0 <- nrow(d)
# per-population rate (per 1,000 women 15-19) and per-birth share (%, descriptive)
for(i in seq_len(nrow(OUT))){
  v<-OUT$var[i]; d[, (paste0("pop_",v)) := 1000*get(paste0("n_",v))/womenA_15_19]
  d[, (paste0("shr_",v)) := fifelse(get(paste0("d_",v))>0, 100*get(paste0("n_",v))/get(paste0("d_",v)), NA_real_)]
}
stopifnot(nrow(d)==nb0)
cat("panel:", d[,uniqueN(adm3_pcode)], "munis x", d[,uniqueN(year)], "years | treated cohorts:",
    paste(sort(unique(d[g>0,g])),collapse=","), "| treated:", d[g>0,uniqueN(adm3_pcode)],
    "| never:", d[g==0,uniqueN(adm3_pcode)], "| always (dropped):", d[is.na(g),uniqueN(adm3_pcode)], "\n")

# ---- CS group-ATT (+ dynamic for the figure) --------------------------------
csrun <- function(yname){
  est <- d[!is.na(g)]; est[, id:=as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", base_period="universal",
        est_method="reg", bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  list(gr=suppressMessages(aggte(a,type="group",na.rm=TRUE)), dy=suppressMessages(aggte(a,type="dynamic",na.rm=TRUE)))
}
base_of <- function(v) d[!is.na(g)&g>0][year==g-1L, mean(get(v), na.rm=TRUE)]
row_of  <- function(label, yname, frame, unit){
  r <- csrun(yname)$gr; bs <- base_of(yname)
  data.table(outcome=label, frame=frame, ATT=round(r$overall.att,3), SE=round(r$overall.se,3),
             p=round(2*pnorm(-abs(r$overall.att/r$overall.se)),3),
             baseline=round(bs,2), pct_of_base=round(100*r$overall.att/bs,1), unit=unit)
}
tab <- rbindlist(c(
  lapply(seq_len(nrow(OUT)), function(i) row_of(OUT$label[i], paste0("pop_",OUT$var[i]),
            "per-population (VALID ITT; burden, quantity-driven)", "per 1,000 women 15-19")),
  lapply(seq_len(nrow(OUT)), function(i) row_of(OUT$label[i], paste0("shr_",OUT$var[i]),
            "per-birth share (DESCRIPTIVE; selection-confounded)", "% of teen births"))))
cat("\n================= NEWBORN-HEALTH EFFECTS (S1 CS, 2016-2025) =================\n")
print(tab, class=FALSE)
cat("\nNOTE: per-population = valid ITT but mostly the QUANTITY channel (fewer teen births ->\n",
    "fewer adverse-outcome teen births); per-birth share = composition among the SELECTED set of\n",
    "teen mothers who still give birth -> DESCRIPTIVE only. Headline teen-rate effect is -11.6%.\n", sep="")
fwrite(tab, file.path(TAB,"newborn_health_effects.csv"))

# ---- presentation LaTeX table (booktabs; graphs reserved for headline) -------
star <- function(p) fifelse(is.na(p),"",fifelse(p<.01,"^{***}",fifelse(p<.05,"^{**}",fifelse(p<.10,"^{*}",""))))  # 2026-08-06: superscript stars (match every other table); was bare *** rendering mid-cell
pop <- tab[grepl("per-population", frame)]; shr <- tab[grepl("per-birth", frame)]
fmt <- function(d) paste(sprintf("%s & %.1f & \\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}} & %+.1f & %.2f \\\\",
         gsub("<","$<$",d$outcome), d$baseline, d$ATT, star(d$p), d$SE, d$ATT-1.96*d$SE, d$ATT+1.96*d$SE, d$pct_of_base, d$p), collapse="\n")
tex <- c("\\begin{tabular}{lrrrr}", "\\toprule",
  "Outcome & Baseline & Estimate (SE) [95\\% CI] & \\% base & $p$ \\\\", "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel A. Population burden --- valid ITT (per 1{,}000 women 15--19)}} \\\\",
  fmt(pop), "\\addlinespace", "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel B. Composition share --- \\% of teen births (descriptive; selection-confounded)}} \\\\",
  fmt(shr), "\\bottomrule", "\\end{tabular}")
tex <- gsub("municipio-clustered", "municipality-clustered", tex, fixed=TRUE)
writeLines(tex, file.path(TAB,"tab_a05_newborn_health.tex"))
cat("\n--- presentation table (tab_a05_newborn_health.tex) ---\n", paste(tex, collapse="\n"), "\n", sep="")

# ---- figure: per-population dynamic event study, 4 outcomes (faceted) ---------
# build dd + per-facet pre-trend p
dd <- rbindlist(lapply(seq_len(nrow(OUT)), function(i){
  r <- csrun(paste0("pop_",OUT$var[i]))
  pt <- es_pretrend_p(r$dy)
  data.table(outcome=factor(OUT$label[i], levels=OUT$label),
             e=r$dy$egt, att=r$dy$att.egt, se=r$dy$se.egt, pt_p=pt$p)
}))[e>=-5 & e<=4]
dd[, `:=`(lo=att-1.96*se, hi=att+1.96*se)]
# per-facet symmetric range (geom_blank forces symmetric, zero-centered, free_y facets)
rngs <- dd[, .(rng=ceiling(max(abs(c(lo,hi)),na.rm=TRUE))), by=outcome]
blanks <- rbind(copy(rngs)[, .(outcome, e=0, att=rng)], copy(rngs)[, .(outcome, e=0, att=-rng)])
ann <- dd[, .(e=-5, att=Inf, lab=sprintf("pre-trend p=%.2f (%s)", pt_p[1], ifelse(pt_p[1]<.05,"sig","n.s."))), by=outcome]
g <- ggplot(dd, aes(e, att)) +
  es_guides() +
  geom_hline(yintercept=0, color="grey25", linewidth=0.6) +
  geom_ribbon(aes(ymin=lo, ymax=hi), fill="grey50", alpha=0.12) +
  geom_line(aes(color=outcome), linewidth=0.9) +
  geom_point(aes(color=outcome), fill="white", shape=21, size=2.1, stroke=1) +
  geom_blank(data=blanks) +
  geom_text(data=ann, aes(label=lab), hjust=0, vjust=1.4, size=2.7, fontface="italic", color="grey25") +
  facet_wrap(~outcome, scales="free_y", ncol=2) +
  scale_color_manual(values=setNames(OUT$col, OUT$label), guide="none") +
  scale_x_continuous(breaks=seq(-5,4,1)) +
  labs(title="Newborn-health burden among teen births (per-population), extension",
       subtitle="S1 CS event study; rate of adverse-outcome teen births per 1,000 women 15-19. Mostly the QUANTITY channel (fewer births).",
       x="Years since first AU opening", y="ATT: adverse-outcome teen births per 1,000 women 15-19",
       caption="Denominator A; not-yet controls; 95% pointwise CIs; muni-clustered. Per-population = valid burden (quantity-driven); per-birth shares (composition) are selection-confounded -> table, descriptive only. EXTENSION.") +
  theme_pcua()
ggsave(file.path(FIG,"fig_newborn_health.png"), g, width=9.4, height=6.4, dpi=200)
cat("\nsaved -> newborn_health_effects.csv + tab_a05_newborn_health.tex + fig_newborn_health.png (backup)\n")
