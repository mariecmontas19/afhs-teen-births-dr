# ============================================================================
# 09k_drop_constanza.R — VERIFICATION sensitivity (MM 2026-07-10, notes §57):
# headline excluding CONSTANZA (DOM021302) entirely. Constanza's date is the
# best-documented of all units (SNS Digital 27-Jul-2022), but its series is
# small and volatile (91-189 births/yr) with a trough g-1 baseline (2021), and
# a local delivery/registration shift in 2019 (§55.2) — its +13 contribution
# ATTENUATES the headline, so this exclusion is the "does the noisy town
# matter" check. Classic-CI event-study figure + CSV row.
# Output: output/tables/drop_constanza.csv + output/figures/hl_S1_noconstanza.png
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")
CONSTANZA <- "DOM021302"

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[!adm3_pcode %in% fold, .(adm3_pcode, year, rateA_15_19)]
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
est <- merge(p, tr[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")[adm3_pcode != CONSTANZA]
est[, id := as.integer(factor(adm3_pcode))]
a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g", xformla=~1,
      data=est, control_group="notyettreated", base_period="universal", est_method="reg",
      bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
o  <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
dy <- suppressMessages(aggte(a, type="dynamic", na.rm=TRUE))
pt <- es_pretrend_p(dy)
bs <- est[g>0][, .SD[year==g-1L], by=adm3_pcode][, mean(rateA_15_19)]
csz <- est[g>0, .(n=uniqueN(adm3_pcode)), by=g]
n3 <- csz[g+3>=2016 & g+3<=2025, sum(n)]; n4 <- csz[g+4>=2016 & g+4<=2025, sum(n)]
res <- data.table(spec="Drop Constanza (verification)", n_treated=uniqueN(est[g>0,adm3_pcode]),
                  ATT=round(o$overall.att,2), SE=round(o$overall.se,2),
                  p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3),
                  baseline=round(bs,2), pct=round(100*o$overall.att/bs,1))
cat("=== drop-Constanza verification ===\n"); print(res, class=FALSE)
fwrite(res, file.path(TAB,"drop_constanza.csv"))

dd <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt, crit=as.numeric(dy$crit.val.egt))[e>=-5 & e<=4]
dd[, `:=`(lo=att-crit*se, hi=att+crit*se)]
rng <- ceiling(max(abs(c(dd$lo,dd$hi)), na.rm=TRUE)/5)*5
ann <- sprintf("Pre-trend joint test (leads): avg %+.1f, p = %.2f (%s)", pt$avg, pt$p, ifelse(pt$p<.05,"sig","n.s."))
g <- ggplot(dd, aes(e, att)) +
  annotate("rect", xmin=-0.5, xmax=4.5, ymin=-Inf, ymax=Inf, fill="grey92", alpha=0.5) +
  geom_hline(yintercept=0, color="grey25", linewidth=0.7) +
  geom_vline(xintercept=-0.5, linetype="dashed", color="grey45", linewidth=0.5) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0.14, linewidth=0.7, color=PCUA_COL$blue, na.rm=TRUE) +
  geom_point(size=2.9, shape=21, stroke=1.2, fill="white", color=PCUA_COL$blue, na.rm=TRUE) +
  annotate("text", x=-5, y=rng*0.93, hjust=0, vjust=1, size=3.2, fontface="italic", color="grey25", label=ann) +
  annotate("text", x=-0.35, y=-rng*0.93, hjust=0, size=3.2, color="grey35", fontface="italic", label="first AU opening") +
  scale_x_continuous(breaks=seq(-5,4,1)) +
  scale_y_continuous(limits=c(-rng,rng), breaks=seq(-rng,rng,5)) +
  labs(title="Verification: excluding Constanza",
       subtitle=sprintf("Callaway-Sant'Anna, 15-19 birth rate, %d treated municipalities (Constanza excluded)", res$n_treated),
       x="Years since first AU opening", y="ATT: teen births per 1,000 women (15-19)",
       caption=sprintf("Denominator A. Uniform 95%% bands (error bars); SEs clustered by municipality; not-yet-treated controls; reference = year before treatment.\nConstanza (small, volatile series; trough g-1 baseline; local registration shift 2019) attenuates the headline: group ATT here %.2f (p=%.3f) vs full sample.\nMunicipalities identifying the far points: e=+3: %d; e=+4: %d (thin horizons - read the group ATT, not the tail).", res$ATT, res$p, n3, n4)) +
  theme_pcua()
ggsave(file.path(FIG,"hl_S1_noconstanza.png"), g, width=8.6, height=5.4, dpi=200)
cat("saved -> drop_constanza.csv + hl_S1_noconstanza.png\n")
