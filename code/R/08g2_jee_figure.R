# ============================================================================
# 08g2_jee_figure.R — UPGRADED JEE parallel-rollout figure (§59.24) from the
# full annual panel (jee_full_muni.rds, MINERD OAI-0790-2026), replacing the
# 3-snapshot version from 08g. Same filename (fig_b07_jee_parallel_rollout.png).
# Two pooled series (% of secondary-offering public/semioficial schools with
# any Jornada Extendida): AU-treated (20 S1 munis) vs never-treated (126).
# Always-treated munis excluded (no clean pre-period). Caption carries the
# formal test: CS event study of %JEE around AU openings = +0.91pp (p=.32).
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures")
fold <- c("DOM010905","DOM051703","DOM012510")

jee <- as.data.table(readRDS(file.path(DIR_CLEAN,"jee_full_muni.rds")))[!is.na(n_sec)]
tr  <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
tr[, grp := fcase(ever_treated==1L & always_treated==0L, "AU municipalities (treated 2020-25)",
                  ever_treated==0L, "Never-treated municipalities", default=NA_character_)]
d <- merge(jee, tr[!is.na(grp), .(adm3_pcode, grp)], by="adm3_pcode")
s <- d[, .(pct = 100*sum(n_jee)/sum(n_sec)), by=.(grp, year_end, school_year)]
cat("series points:", nrow(s), "\n"); print(dcast(s, school_year ~ grp, value.var="pct"), class=FALSE)

chk <- fread(file.path(PROJ,"output","tables","jee_full_checks.csv"))
att <- chk[check=="C2_jee_on_AU_att", value]; pce <- chk[check=="C2_p", value]

gA <- ggplot(s, aes(year_end, pct, color=grp)) +
  annotate("rect", xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, fill="grey85", alpha=0.35) +
  annotate("text", x=2022.5, y=97, label="AU openings (2020-25)", size=3.2, color="grey35", fontface="italic") +
  geom_line(linewidth=1.1) +
  geom_point(size=2.8, fill="white", shape=21, stroke=1.2) +
  scale_color_manual(values=setNames(c(PCUA_COL$blue, "grey45"),
    c("AU municipalities (treated 2020-25)","Never-treated municipalities")), name=NULL) +
  scale_x_continuous(breaks=2016:2025) +
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,20)) +
  labs(title="JEE expanded in parallel for treated and never-treated municipalities",
       x="School year (ending)", y="% of secondary schools with JEE",
       caption=sprintf("MINERD per-center registries (OAI-0790-2026), public + semioficial sectors; pooled school counts per group. Always-treated municipalities excluded.\nFormal test: Callaway-Sant'Anna event study of %%JEE around AU openings = %+.2fpp (p = %.2f) - JEE rollout is orthogonal to AU timing.", att, pce)) +
  theme_pcua() + theme(legend.position=c(0.72, 0.14))
gA <- gA + labs(title="(A) JEE expansion across all municipalities",
                caption=NULL)

# ---- Panel B: EVENT-TIME — %JEE around each municipality's own AU opening ----
suppressPackageStartupMessages(library(did))
fold3 <- fold
tr2 <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold3]
tr2[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]
est <- merge(jee[!is.na(pct_jee_sch), .(adm3_pcode, year=year_end, pct_jee_sch)],
             tr2[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
est[, id := as.integer(factor(adm3_pcode))]
a <- att_gt(yname="pct_jee_sch", tname="year", idname="id", gname="g", xformla=~1, data=est,
            control_group="notyettreated", base_period="universal", est_method="reg",
            bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE,
            allow_unbalanced_panel=TRUE)
dy <- aggte(a, type="dynamic", na.rm=TRUE)
dd <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,4)]
crit <- max(dy$crit.val.egt, 1.96, na.rm=TRUE)
dd[, `:=`(lo=att-crit*se, hi=att+crit*se)]
rng <- ceiling(max(abs(c(dd$lo,dd$hi)), na.rm=TRUE)/5)*5
gB <- ggplot(dd, aes(e, att)) +
  es_guides(label_y=rng) +
  geom_hline(yintercept=0, color="grey25", linewidth=0.7) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0.14, linewidth=0.7, color=PCUA_COL$blue, na.rm=TRUE) +
  geom_point(color=PCUA_COL$blue, fill="white", shape=21, size=2.9, stroke=1.2, na.rm=TRUE) +
  annotate("text", x=-5, y=rng*0.92, hjust=0, vjust=1, size=3.3, fontface="italic", color="grey25",
           label=sprintf("Group ATT %+.2fpp (p = %.2f):\nJEE does not move when the AU opens", att, pce)) +
  scale_x_continuous(breaks=seq(-5,4,1)) +
  scale_y_continuous(limits=c(-rng,rng)) +
  labs(title="(B) Effect of AU openings on JEE coverage among secondary schools",
       x="Years since first AU opening", y="ATT: pp of secondary schools with JEE") +
  theme_pcua()
if(requireNamespace("patchwork", quietly=TRUE)){
  comb <- patchwork::wrap_plots(list(gA, gB), ncol=1) +
    patchwork::plot_annotation()
  ggsave(file.path(FIG,"fig_b07_jee_parallel_rollout.png"), comb, width=8.6, height=10.2, dpi=200)
  cat("saved -> fig_b07_jee_parallel_rollout.png (two-panel: calendar + event-time)\n")
} else {
  ggsave(file.path(FIG,"fig_b07_jee_parallel_rollout.png"), gB, width=8.6, height=5.4, dpi=200)
  cat("patchwork missing - saved event-time panel only\n")
}
