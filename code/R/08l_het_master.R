# ============================================================================
# 08l_het_master.R — MASTER heterogeneity consolidation (S1). Recomputes every
# moderator split in ONE place (subgroup CS ATT, 20 treated split at median vs
# shared never-treated), with per-subgroup p AND difference p AND baselines.
# Produces: (1) master table (csv+tex, all moderators), (2) reframed OVERVIEW
# dumbbell (6 dims: effect is broadly uniform — differences NOT significant),
# (3) the PROXIMITY mechanism figure (close vs far, all + excl. Santo Domingo).
# Denom A; not-yet controls; est=reg; universal base; muni-clustered. [2026-06-28]
# (JEE held out — awaiting MM's exact school data.)
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(did); library(ggplot2)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510")
TAB <- file.path(PROJ,"output","tables"); FIG <- file.path(PROJ,"output","figures"); OVERALL <- -6.43

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[, .(adm3_pcode, year, rateA_15_19, wealth_index, pct_urban)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
treated <- bin[!is.na(g)&g>0, .(adm3_pcode, g)]; never <- bin[g==0, adm3_pcode]

# ---- assemble moderator values for treated (time-varying ones at g-1) --------
con <- as.data.table(readRDS(file.path(DIR_CLEAN,"dhs_contraception_province.rds")))[, .(adm3_pcode, unmet_2013, cpr_any_2013, satany_2013)]
sub <- as.data.table(readRDS(file.path(DIR_CLEAN,"senasa_subsidized_muni.rds")))
lv  <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_facility_levels_panel.rds")))[, .(adm3_pcode, year, primary_per10k, hosp_per10k)]
lv[, all_per10k := primary_per10k + hosp_per10k]      # ALL facilities (primary I + hospitals II/III) per 10k
fa  <- as.data.table(readRDS(file.path(DIR_CLEAN,"facility_access_panel.rds")))[, .(adm3_pcode, year, near_any_km, near_hosp_km, sfca20)]
base_rate <- merge(treated, p[, .(adm3_pcode, year, rateA_15_19)], by="adm3_pcode")[year==g-1L, .(adm3_pcode, base=rateA_15_19)]
fix <- merge(treated, unique(p[, .(adm3_pcode, wealth_index, pct_urban)]), by="adm3_pcode")
fix <- Reduce(function(a,b) merge(a,b,by="adm3_pcode",all.x=TRUE), list(fix, con, sub, base_rate))
tv  <- merge(treated, merge(lv, fa, by=c("adm3_pcode","year")), by="adm3_pcode")[year==g-1L][, year:=NULL]
M   <- merge(fix, tv, by=c("adm3_pcode","g"))
stopifnot(nrow(M)==nrow(treated))
## familia-questionnaire moderators (05t, EDN 2019 3S wave; §59.92) — merged
## here (no RNG use), but their splits are ESTIMATED AFTER aA/aB below so the
## original 12-row + proximity seed stream is preserved (§59.91).
fmq <- as.data.table(readRDS(file.path(DIR_CLEAN,"familia_moderators_2019.rds")))
M   <- merge(M, fmq[, .(adm3_pcode, mom_secplus, expect_uni, parent_part, absent)], by="adm3_pcode", all.x=TRUE)
stopifnot(nrow(M)==nrow(treated), M[is.na(mom_secplus), .N]==0L)

cs_sub <- function(pcodes){
  est <- merge(p[, .(adm3_pcode, year, rateA_15_19)], rbind(treated[adm3_pcode %in% pcodes], data.table(adm3_pcode=never, g=0L)), by="adm3_pcode")
  est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g", xformla=~1,
        data=est, control_group="notyettreated", base_period="universal", est_method="reg", bstrap=TRUE, biters=2000,
        clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE)); list(att=o$overall.att, se=o$overall.se)
}
spec <- data.table(
  label   =c("Unmet need (FP)","Contraceptive use (CPR)","Satisfied demand","Wealth (census)","Subsidized regime %",
             "Rural vs urban","All facilities /capita","Primary care /capita","Hospitals /capita","Nearest facility (km)","Nearest hospital (km)","2SFCA access"),
  var     =c("unmet_2013","cpr_any_2013","satany_2013","wealth_index","subsidized_share",
             "pct_urban","all_per10k","primary_per10k","hosp_per10k","near_any_km","near_hosp_km","sfca20"),
  category=c("Contraceptive access","Contraceptive access","Contraceptive access","Socioeconomic","Socioeconomic",
             "Geography","Health supply (density)","Health supply (density)","Health supply (density)","Health access (distance)","Health access (distance)","Health access (distance)"),
  under_high=c(TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE),
  in_dumbbell=c(TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE))
res <- rbindlist(lapply(seq_len(nrow(spec)), function(i){
  v<-spec$var[i]; med<-median(M[[v]]); und<-if(spec$under_high[i]) M[get(v)>=med,adm3_pcode] else M[get(v)<med,adm3_pcode]
  srv<-setdiff(M$adm3_pcode,und); ru<-cs_sub(und); rs<-cs_sub(srv); gap<-ru$att-rs$att
  data.table(label=spec$label[i], category=spec$category[i], in_dumbbell=spec$in_dumbbell[i],
    att_u=round(ru$att,2), se_u=round(ru$se,2), p_u=round(2*pnorm(-abs(ru$att/ru$se)),3),
    att_s=round(rs$att,2), se_s=round(rs$se,2), p_s=round(2*pnorm(-abs(rs$att/rs$se)),3),
    base_u=round(M[adm3_pcode %in% und, mean(base)],1), base_s=round(M[adm3_pcode %in% srv, mean(base)],1),
    gap=round(gap,2), p_diff=round(2*pnorm(-abs(gap/sqrt(ru$se^2+rs$se^2))),3), n_u=length(und), n_s=length(srv))
}))
cat("=== MASTER het table (S1; overall", OVERALL, ") ===\n")
print(res[, .(label, category, under=sprintf("%.1f(%.1f)%s",att_u,se_u,fifelse(p_u<.05,"*","")),
              served=sprintf("%.1f(%.1f)%s",att_s,se_s,fifelse(p_s<.05,"*","")), gap, p_diff)], class=FALSE)
fwrite(res, file.path(TAB,"het_master_table.csv"))

# ---- (2) OVERVIEW dumbbell (fig_06a) is built BELOW, after res_full, because
# two of its dimensions (Frequently absent, Parental participation) are the
# familia rows estimated in the extension block (MM 2026-08-12, §59.93).

# ---- (3) PROXIMITY mechanism figure: close vs far; EMPHASIZE the difference ---
# difference SE = sqrt(se_near^2+se_far^2) is CONSERVATIVE (shared never-treated controls ->
# positive covariance -> true SE smaller -> true p even smaller); so this UNDERSTATES significance.
M[, sd := substr(adm3_pcode,6,7) %in% c("01","32")]
prox <- function(MM, tag){
  med <- median(MM$near_any_km); far <- MM[near_any_km>=med, adm3_pcode]; close <- MM[near_any_km<med, adm3_pcode]
  rf <- cs_sub(far); rc <- cs_sub(close); gap <- rc$att - rf$att; pd <- 2*pnorm(-abs(gap/sqrt(rc$se^2+rf$se^2)))
  list(pts=rbind(data.table(sample=tag, grp="Near a facility", att=rc$att, se=rc$se),
                 data.table(sample=tag, grp="Far from facility", att=rf$att, se=rf$se)),
       dif=data.table(sample=tag, gap=gap, pd=pd))
}
# aA/aB MUST be computed here (this exact stream position) so the excl-SD row
# below reuses aB and the 12 core rows stay byte-identical (§59.91). The FIGURE
# (fig_b04) is rebuilt below from the canonical het_master rows so table and
# figure share ONE p-value (MM 2026-08-12, §59.93); aA's own pd is diagnostic only.
aA <- prox(M, "All treated (20)"); aB <- prox(M[sd==FALSE], "Excl. Santo Domingo (16)")
cat(sprintf("\nproximity (raw prox() diagnostic): all p=%.3f | excl-SD p=%.3f\n", aA$dif$pd, aB$dif$pd))

# ---- A6 EXTENSION rows (MM 2026-08-12, §59.92) — estimated AFTER aA/aB so all
# numbers above are byte-identical to the pre-extension vintage (§59.91).
# (1) Proximity excl. Santo Domingo as a TABLE row, reusing aB (no re-estimation):
#     more-underserved = FAR half, less = NEAR half; gap = far - near.
MB  <- M[sd==FALSE]; medB <- median(MB$near_any_km)
farB <- MB[near_any_km>=medB, adm3_pcode]; nearB <- setdiff(MB$adm3_pcode, farB)
pF <- aB$pts[grp=="Far from facility"]; pN <- aB$pts[grp=="Near a facility"]
row_exsd <- data.table(label="Nearest facility (km, excl. SD)", category="Health access (distance)",
  in_dumbbell=FALSE,
  att_u=round(pF$att,2), se_u=round(pF$se,2), p_u=round(2*pnorm(-abs(pF$att/pF$se)),3),
  att_s=round(pN$att,2), se_s=round(pN$se,2), p_s=round(2*pnorm(-abs(pN$att/pN$se)),3),
  base_u=round(M[adm3_pcode %in% farB, mean(base)],1), base_s=round(M[adm3_pcode %in% nearB, mean(base)],1),
  gap=round(pF$att-pN$att,2), p_diff=round(aB$dif$pd,3), n_u=length(farB), n_s=length(nearB))
# (2) Adolescent vulnerability and school attachment (familia questionnaire, 05t):
spec2 <- data.table(
  label   =c("Frequently absent","Parental school participation","Family expects university","Mother completed secondary"),
  var     =c("absent","parent_part","expect_uni","mom_secplus"),
  category=rep("Adolescent vulnerability and school attachment",4),
  under_high=c(TRUE,FALSE,FALSE,FALSE), in_dumbbell=rep(FALSE,4))
res2 <- rbindlist(lapply(seq_len(nrow(spec2)), function(i){
  v<-spec2$var[i]; med<-median(M[[v]]); und<-if(spec2$under_high[i]) M[get(v)>=med,adm3_pcode] else M[get(v)<med,adm3_pcode]
  srv<-setdiff(M$adm3_pcode,und); ru<-cs_sub(und); rs<-cs_sub(srv); gap<-ru$att-rs$att
  data.table(label=spec2$label[i], category=spec2$category[i], in_dumbbell=spec2$in_dumbbell[i],
    att_u=round(ru$att,2), se_u=round(ru$se,2), p_u=round(2*pnorm(-abs(ru$att/ru$se)),3),
    att_s=round(rs$att,2), se_s=round(rs$se,2), p_s=round(2*pnorm(-abs(rs$att/rs$se)),3),
    base_u=round(M[adm3_pcode %in% und, mean(base)],1), base_s=round(M[adm3_pcode %in% srv, mean(base)],1),
    gap=round(gap,2), p_diff=round(2*pnorm(-abs(gap/sqrt(ru$se^2+rs$se^2))),3), n_u=length(und), n_s=length(srv))
}))
res_full <- rbind(res, row_exsd, res2)
cat("\n=== A6 extension rows (canonical) ===\n")
print(res_full[13:17, .(label, att_u, se_u, att_s, se_s, gap, p_diff)], class=FALSE)
fwrite(res_full, file.path(TAB,"het_master_table.csv"))

# ---- (2) OVERVIEW dumbbell (fig_06a): ONE dimension per A6 section, ORDERED BY
# WIDTH (MM 2026-08-12, §59.93): proximity (distance), unmet need (contraceptive),
# rurality (geography), facilities/capita (density), frequently absent (adolescent
# vulnerability). Dark blue = the more-negative (larger) half.
Dd <- res_full[label %in% c("Nearest facility (km)","All facilities /capita",
                            "Frequently absent","Unmet need (FP)","Rural vs urban")]
base_lab <- c("Nearest facility (km)"="Proximity: distance\nto nearest facility",
              "Unmet need (FP)"="Unmet need for\ncontraception",
              "Rural vs urban"="Rurality",
              "Frequently absent"="Frequently absent\nfrom school",
              "All facilities /capita"="Health facilities per\n10,000 population")
tag_u <- c("Nearest facility (km)"="far","Unmet need (FP)"="high","Rural vs urban"="rural",
           "Frequently absent"="often","All facilities /capita"="fewer")
tag_s <- c("Nearest facility (km)"="near","Unmet need (FP)"="low","Rural vs urban"="urban",
           "Frequently absent"="seldom","All facilities /capita"="more")
Dd[, u_blue := att_u <= att_s]                       # blue = more-negative side
Dd[, tag := sprintf("(%s vs. %s)", fifelse(u_blue, tag_u[label], tag_s[label]),
                                   fifelse(u_blue, tag_s[label], tag_u[label]))]
Dd[, lab := paste(base_lab[label], tag)]
Dd[, w := abs(gap)]; setorder(Dd, -w)                # widest dumbbell first
Dd[, lab := factor(lab, levels=rev(lab))]            # -> widest at top of the panel
Dd[, emph := label=="Nearest facility (km)"]
divy <- nrow(Dd) - 0.5                                # faint divider below proximity (top row)
P <- rbind(Dd[, .(lab, emph, att=att_u, blue=u_blue)],
           Dd[, .(lab, emph, att=att_s, blue=!u_blue)])
Pe <- P[emph==TRUE]; Po <- P[emph==FALSE]
g1 <- ggplot() +
  geom_hline(yintercept=divy, color="grey80", linewidth=0.5) +   # so proximity reads as the headline
  geom_vline(xintercept=0, color="grey60") +
  geom_vline(xintercept=OVERALL, linetype="22", color="grey45") +
  geom_segment(data=Dd[emph==FALSE], aes(x=att_u, xend=att_s, y=lab, yend=lab), color="grey85", linewidth=1.3) +
  geom_segment(data=Dd[emph==TRUE],  aes(x=att_u, xend=att_s, y=lab, yend=lab), color="grey55", linewidth=2.1) +
  geom_point(data=Po[blue==TRUE],  aes(att, lab), color=PCUA_COL$blue, size=3.9, alpha=0.55) +
  geom_point(data=Po[blue==FALSE], aes(att, lab), color="grey45", size=3.9, alpha=0.55) +
  geom_text(data=Po[blue==TRUE],  aes(att, lab, label=sprintf("%.1f",att)), color=PCUA_COL$blue, vjust=-1.4, size=3.0, alpha=0.9) +
  geom_text(data=Po[blue==FALSE], aes(att, lab, label=sprintf("%.1f",att)), color="grey40", vjust=-1.4, size=3.0, alpha=0.9) +
  geom_point(data=Pe[blue==TRUE],  aes(att, lab), color=PCUA_COL$blue, size=5.6) +
  geom_point(data=Pe[blue==FALSE], aes(att, lab), color="grey45", size=5.6) +
  geom_text(data=Pe[blue==TRUE],  aes(att, lab, label=sprintf("%.1f",att)), color=PCUA_COL$blue, vjust=-1.3, size=3.6, fontface="bold") +
  geom_text(data=Pe[blue==FALSE], aes(att, lab, label=sprintf("%.1f",att)), color="grey35", vjust=-1.3, size=3.6, fontface="bold") +
  coord_cartesian(clip="off") +
  labs(x="ATT: teen births per 1,000 women (15-19)", y=NULL) +
  theme_pcua(base_size=14) +
  theme(legend.position="none",
        axis.text.y=element_text(size=13, face="bold", lineheight=0.95))
ggsave(file.path(FIG,"fig_06a_het_dumbbell.png"), g1, width=9.8, height=5.4, dpi=200)

# ---- (3) PROXIMITY mechanism figure (fig_b04): near vs far, all + excl-SD.
# Points AND p-value come from the SAME het_master rows as Table A6, so table
# and figure are unified (MM 2026-08-12, §59.93): all p=.069, excl-SD p=.044.
pr2 <- res_full[label %in% c("Nearest facility (km)","Nearest facility (km, excl. SD)")]
pr2[, sample := fifelse(label=="Nearest facility (km)","All treated (20)","Excl. Santo Domingo (16)")]
samp_lv <- c("All treated (20)","Excl. Santo Domingo (16)")
PX <- rbind(pr2[, .(sample, grp="Near a facility",  att=att_s, se=se_s)],
            pr2[, .(sample, grp="Far from facility", att=att_u, se=se_u)])
PX[, `:=`(lo=att-1.96*se, hi=att+1.96*se,
          grp=factor(grp, levels=c("Far from facility","Near a facility")),
          sample=factor(sample, levels=samp_lv))]
DF <- pr2[, .(sample=factor(sample, levels=samp_lv), gap=att_s-att_u, pd=p_diff)]
DF[, lab := sprintf("%s     Near vs Far: %+.1f  (p = %.3f%s)", as.character(sample), gap, pd,
                    fifelse(pd<.05,"**",fifelse(pd<.10,"*","")))]
faclab <- setNames(DF$lab, as.character(DF$sample))
g2 <- ggplot(PX, aes(att, grp, color=grp)) +
  geom_vline(xintercept=0, color="grey60") + geom_vline(xintercept=OVERALL, linetype="22", color="grey45") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=0, linewidth=0.7, alpha=0.55) +
  geom_point(size=4.2, shape=21, fill="white", stroke=1.4) +
  geom_text(aes(label=sprintf("%+.1f",att)), vjust=-1.2, size=3.6, fontface="bold", show.legend=FALSE) +
  facet_wrap(~sample, ncol=1, labeller=labeller(sample=faclab)) +
  scale_color_manual(values=c("Far from facility"="grey55", "Near a facility"=PCUA_COL$blue), guide="none") +
  scale_x_continuous(breaks=seq(-24, 4, 2)) +
  labs(x="ATT: teen births per 1,000 women (15-19)", y=NULL) +
  theme_pcua()
ggsave(file.path(FIG,"fig_b04_proximity_mechanism.png"), g2, width=9.4, height=5.4, dpi=200)
cat("\nsaved -> het_master_table.csv (17 rows) + fig_06a_het_dumbbell.png (5 dims by width)",
    "+ fig_b04_proximity_mechanism.png (p from table rows)\n")
