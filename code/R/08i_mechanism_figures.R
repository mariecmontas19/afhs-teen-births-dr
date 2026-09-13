# ============================================================================
# 08i_mechanism_figures.R — two paper figures from the mechanism analysis (08g):
#   (1) fig_contraception_mechanism.png — the AU effect concentrates where
#       baseline (2013) contraceptive access was scarcest (3 complementary measures).
#   (2) fig_09_jee_parallel_rollout.png — JEE expanded in PARALLEL for treated vs
#       never-treated municipalities (the timing-correlation / "common-not-differential"
#       evidence that JEE is not a differential confound).
# Reads mechanism_jee_contra.csv (08g) + jee_rollout_muni.rds + treatment.
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures")
fold <- c("DOM010905","DOM051703","DOM012510"); OVERALL <- -6.43   # S1 overall ATT (07t/08g)

# ---- (1) contraception-mechanism figure -------------------------------------
M <- fread(file.path(TAB,"mechanism_jee_contra.csv"))
C <- M[grepl("unmet|CPR|satisfied", test)]
relab <- c("Baseline unmet need (2013)"="Unmet need\n(high = scarce access)",
           "Baseline CPR any (2013)"="Contraceptive use\n(low = scarce access)",
           "Baseline satisfied demand any (2013)"="Satisfied demand\n(low = scarce access)")
C[, meas := factor(relab[test], levels=rev(relab))]          # unmet need on top
C[, group := factor(group, levels=c("More underserved","Less underserved"))]
# --- TABLE (carries the SEs a clean figure can't) ---
C[, lab := sprintf("%.2f (%.2f)", att, se)]
TB <- dcast(C, test ~ group, value.var="lab")[, .(Baseline=test,
        `More underserved (scarce access)`=`More underserved`, `Less underserved (better access)`=`Less underserved`)]
cat("\n=== contraception mechanism table — ATT (SE), per 1,000 women 15-19 ===\n"); print(TB, class=FALSE)
fwrite(TB, file.path(TAB,"contraception_mechanism_table.csv"))
# --- DUMBBELL FIGURE (focuses on the gap, not the wide CIs) ---
W <- dcast(C, meas ~ group, value.var="att")
g1 <- ggplot() +
  geom_vline(xintercept=0, color="grey60") +
  geom_vline(xintercept=OVERALL, linetype="22", color="grey55") +
  geom_segment(data=W, aes(x=`More underserved`, xend=`Less underserved`, y=meas, yend=meas),
               color="grey78", linewidth=1.6) +
  geom_point(data=C, aes(att, meas, color=group), size=4.8) +
  geom_text(data=C, aes(att, meas, label=sprintf("%+.1f", att), color=group),
            vjust=-1.5, size=3.5, fontface="bold", show.legend=FALSE) +
  scale_color_manual(values=c("More underserved"=PCUA_COL$green, "Less underserved"=PCUA_COL$orange),
                     labels=c("More underserved (scarce contraceptive access)","Less underserved (better access)"), name=NULL) +
  coord_cartesian(clip="off") +
  labs(title="The AU effect concentrates where contraceptive access was scarcest",
       subtitle=sprintf("Subgroup CS ATT on the 15-19 birth rate: scarce-access half vs better-access half, each baseline split at its median (dashed = overall %.1f)", OVERALL),
       x="ATT: teen births per 1,000 women (15-19)", y=NULL,
       caption="Baseline = 2013 ENDESA, all-ages province. ~10 treated/half: gaps are suggestive, not significant (SEs in the companion table).") +
  theme_pcua()
ggsave(file.path(FIG,"fig_contraception_mechanism.png"), g1, width=9.6, height=4.7, dpi=200)

# ---- (2) JEE parallel-rollout figure (treated vs never) ----------------------
jee <- as.data.table(readRDS(file.path(DIR_CLEAN,"jee_rollout_muni.rds")))
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, arm := fcase(ever_treated==1L & always_treated==0L, "AU-treated (in-window)",
                   ever_treated==0L, "Never-treated", default=NA_character_)]
jt <- merge(jee, bin[, .(adm3_pcode, arm)], by="adm3_pcode")[!is.na(arm)]
# BALANCED sample: municipalities with JEE at both endpoints (so change-of-means == mean-of-changes,
# matching the t-test in the caption)
jw <- dcast(jt, adm3_pcode + arm ~ school_year, value.var="pct_jee_sch")
setnames(jw, c("2019-2020","2024-2025"), c("j19","j24"))
jw <- jw[!is.na(j19) & !is.na(j24)]; jw[, chg := j24 - j19]
bal <- jw$adm3_pcode
jt <- jt[adm3_pcode %in% bal]
jt[, yr := as.integer(substr(school_year,1,4))]              # 2019, 2020, 2024 (start year)
traj <- jt[, .(jee=mean(pct_jee_sch), n=.N), by=.(arm, yr, school_year)]
# change + t-test on the SAME balanced sample -> caption numbers (computed, not hardcoded)
ch <- jw[, .(mean_chg=mean(chg), n=.N), by=arm]
tt <- t.test(chg ~ arm, data=jw)
chT <- ch[arm=="AU-treated (in-window)", mean_chg]; chN <- ch[arm=="Never-treated", mean_chg]
cap2 <- sprintf("2019->2024 change: +%.1f (treated) vs +%.1f (never); difference not significant (t=%.2f, p=%.2f). Public+semioficial; %d municipalities.",
                chT, chN, tt$statistic, tt$p.value, length(bal))
cat("JEE trajectory (balanced sample) by arm:\n"); print(traj[order(arm,yr)], class=FALSE)
cat("change:", "treated", round(chT,1), "never", round(chN,1), "| t=", round(tt$statistic,2), "p=", round(tt$p.value,3), "\n")
g2 <- ggplot(traj, aes(yr, jee, color=arm)) +
  annotate("rect", xmin=2020, xmax=2024, ymin=-Inf, ymax=Inf, fill="grey92", alpha=0.5) +
  annotate("text", x=2022, y=62, label="no directory data\n2021–2024", color="grey55", size=3, lineheight=0.9, fontface="italic") +
  geom_line(data=traj[yr<=2020], linewidth=1.1) +                      # observed (adjacent years): solid
  geom_line(data=traj[yr>=2020], linewidth=1.1, linetype="21") +       # 4-year gap: dashed (connects, not observed)
  geom_point(size=3.2, shape=21, fill="white", stroke=1.3) +
  geom_text(aes(label=sprintf("%.0f%%", jee)), vjust=-1.1, size=3.2, show.legend=FALSE) +
  scale_color_manual(values=c("PCUA-treated (in-window)"=PCUA_COL$green, "Never-treated"=PCUA_COL$orange), name=NULL) +
  scale_x_continuous(breaks=c(2019,2020,2024), labels=c("2019-20","2020-21","2024-25"), expand=expansion(mult=c(0.04,0.08))) +
  scale_y_continuous(limits=c(60,92)) +
  annotate("text", x=2019, y=90.5, hjust=0, color="grey30", size=3.4, fontface="italic",
           label=sprintf("Rollout change 2019-24:  treated +%.1f  vs  never +%.1f    (t = %.2f, p = %.2f, n.s.)", chT, chN, tt$statistic, tt$p.value)) +
  labs(title="JEE expanded in parallel for treated and never-treated municipalities",
       subtitle="% of public secondary schools operating extended-day, by AU status",
       x="School year", y="% secondary schools extended-day (JEE)",
       caption="Dashed segment = 4-year data gap (no directory data 2021-2024). Public + semioficial; 143 municipalities.") +
  theme_pcua()
ggsave(file.path(FIG,"fig_09_jee_parallel_rollout.png"), g2, width=9.6, height=5.2, dpi=200)
cat("\nsaved -> fig_contraception_mechanism.png + fig_09_jee_parallel_rollout.png\n")
