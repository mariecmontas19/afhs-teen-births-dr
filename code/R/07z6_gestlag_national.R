# ============================================================================
# 07z6_gestlag_national.R — NATIONAL gestational-lag falsification (strengthened).
# A AU cannot affect births until conceptions AFTER the event come to term (~9 mo),
# so the within-municipio-demeaned monthly teen rate should NOT fall in event-months
# 0-8, only from ~+9 on; a drop before month 0 = pre-trends.
#
# Event pool (STACKED event-windows, +/-24 months, demeaned within each window):
#   (a) IN-WINDOW first openings: in-window treated municipios w/ a known opening MONTH.
#   (b) ALWAYS-TREATED modernization events w/ MONTH-precision dates in 2016-2025
#       (added per MM to ~double the events; year-only dates EXCLUDED — imputing a month
#       would blur the very timing this test relies on). A municipio with several
#       month-precision modernizations contributes one stacked window per event.
# Formal check: mean demeaned rate in pre[-24,-1] vs lag[0,8] vs post[9,24].
# Output: output/figures/fig_05_gestlag_national.png + output/tables/gestlag_national.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
FIG <- file.path(PROJ,"output","figures"); W <- 24L

mp  <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_month_155.rds")))[, .(adm3_pcode=id, year, month, rateA_ann)]
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
v   <- fread(file.path(DIR_NOTES,"pcua_unit_dates_verified.csv"))
v[, `:=`(myear=as.integer(substr(modern_event_date,1,4)), mmon=as.integer(substr(modern_event_date,6,7)))]

# (a) in-window first openings with a known month
ev_new <- trt[ever_treated==1L & always_treated==0L & !is.na(first_month),
              .(adm3_pcode, eyear=first_year, emon=first_month, type="new opening")]
# (b) always-treated modernization events, month-precision, 2016-2025 (stacked: all events)
at <- trt[always_treated==1L, adm3_pcode]
ev_mod <- unique(v[adm3_pcode %in% at & !is.na(mmon) & myear %between% c(2016,2025),
                   .(adm3_pcode, eyear=myear, emon=mmon, type="modernization")])
ev <- rbind(ev_new, ev_mod); ev[, event_id := .I]
cat("event-windows: new openings =", nrow(ev_new), "| modernizations =", nrow(ev_mod),
    "| TOTAL =", nrow(ev), "(from", uniqueN(ev$adm3_pcode), "municipios)\n")

# build stacked, within-window-demeaned event time
stk <- rbindlist(lapply(seq_len(nrow(ev)), function(i){
  e <- ev[i]; s <- mp[adm3_pcode==e$adm3_pcode]
  s[, em := (year*12L+month) - (e$eyear*12L+e$emon)]
  s <- s[em %between% c(-W, W)]
  s[, rate_dm := rateA_ann - mean(rateA_ann, na.rm=TRUE)]
  s[, .(event_id=e$event_id, adm3_pcode=e$adm3_pcode, type=e$type, em, rate_dm)]
}))
L <- stk[, .(rate_dm=mean(rate_dm, na.rm=TRUE), n=.N), by=em][order(em)]

# formal window means
wm <- function(lo,hi) stk[em %between% c(lo,hi), .(mean=mean(rate_dm,na.rm=TRUE), n=.N)]
pre <- wm(-24,-1); lagw <- wm(0,8); post <- wm(9,24)
cat(sprintf("window means (demeaned rate): pre[-24,-1]=%+.2f (n=%d) | lag[0,8]=%+.2f (n=%d) | post[9,24]=%+.2f (n=%d)\n",
    pre$mean,pre$n, lagw$mean,lagw$n, post$mean,post$n))
tt <- t.test(stk[em %between% c(9,24), rate_dm], stk[em %between% c(0,8), rate_dm])
cat(sprintf("post[9,24] vs lag[0,8] difference = %+.2f, p = %.3f (Welch, event-months)\n", diff(rev(tt$estimate)), tt$p.value))
# 2026-08-08 (MM/committee): formal inference clustered at the MUNICIPALITY, the
# treatment-assignment level (windows overlap and municipalities repeat across
# events, so event-months are not independent).
suppressPackageStartupMessages(library(fixest))
stk[, win := fcase(em %between% c(-24,-1), "pre", em %between% c(0,8), "lag", em %between% c(9,24), "post")]
m <- feols(rate_dm ~ 0 + win, data=stk, cluster=~adm3_pcode, notes=FALSE)
h <- hypotheses <- coef(m); V <- vcov(m)
dpl <- h[["winpost"]] - h[["winlag"]]; sepl <- sqrt(V["winpost","winpost"] + V["winlag","winlag"] - 2*V["winpost","winlag"])
cat(sprintf("post-vs-lag, muni-clustered: %+.2f (SE %.2f, p = %.3f) | clusters = %d\n",
            dpl, sepl, 2*pnorm(-abs(dpl/sepl)), uniqueN(stk$adm3_pcode)))
dlp <- h[["winlag"]] - h[["winpre"]]; selp <- sqrt(V["winlag","winlag"] + V["winpre","winpre"] - 2*V["winlag","winpre"])
cat(sprintf("lag-vs-pre  (placebo, should be ~0): %+.2f (SE %.2f, p = %.3f)\n", dlp, selp, 2*pnorm(-abs(dlp/selp))))
gl <- data.table(contrast=c("post_vs_lag","lag_vs_pre"), diff=c(dpl,dlp), se=c(sepl,selp),
                 p=round(2*pnorm(-abs(c(dpl/sepl, dlp/selp))),3), clusters=uniqueN(stk$adm3_pcode))
fwrite(gl, file.path(DIR_TABLES,"gestlag_inference.csv"))
fwrite(rbind(cbind(window="pre[-24,-1]",pre), cbind(window="lag[0,8]",lagw), cbind(window="post[9,24]",post)),
       file.path(DIR_TABLES,"gestlag_national.csv"))

g <- ggplot(L, aes(em, rate_dm)) +
  annotate("rect", xmin=0, xmax=9, ymin=-Inf, ymax=Inf, fill="grey50", alpha=0.10) +  # no-response-possible window
  geom_hline(yintercept=0, color="grey60", linewidth=0.4) +
  geom_vline(xintercept=0, linetype="22", color="grey35", linewidth=0.5) +
  geom_vline(xintercept=9, linetype="dotted", color=PCUA_COL$blue, linewidth=0.8) +
  annotate("text", x=0,  y=Inf, label="opening", vjust=1.5, hjust=-0.08, size=3.1, color="grey35", fontface="italic") +
  annotate("text", x=4.5,y=-Inf, label="response impossible (gestation)", vjust=-0.8, size=2.8, color="grey55") +
  geom_point(alpha=0.4, size=1, color="grey45") +
  geom_smooth(method="loess", span=0.6, se=TRUE, color=PCUA_COL$blue, fill=PCUA_COL$blue, alpha=0.12) +
  labs(x="Months since AU event", y="Demeaned teen birth rate (annualized /1,000)") +
  theme_pcua()
ggsave_pair(file.path(FIG,"fig_05_gestlag_national.png"), g, width=9.2, height=5.2, dpi=200)
cat("saved -> fig_05_gestlag_national.png + gestlag_national.csv\n")
