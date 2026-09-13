# ============================================================================
# 08d_infant_mortality.R — EXTENSION: does opening an AU lower municipality infant /
# neonatal mortality? Source: M_INFANTIL_HARVARD_2016-2023.xlsx (one row per infant
# death <1yr; residence municipality by NAME; GRUPO_EDAD = early/late neonatal vs
# postneonatal). Per-sheet column names differ across years (HAB/RES/Habitual) ->
# canonical matcher. Deaths mapped to adm3_pcode via (ONE province code + name),
# folded to 155, aggregated to municipality x year. Denominator = live births (BDNV).
#   NMR = neonatal (<=27 days) deaths per 1,000 live births
#   IMR = infant   (<1 year)   deaths per 1,000 live births
# CS event study (S1). HEAVY CAVEATS: rare/noisy at municipality level; AGGREGATE (not
# teen-specific, no mother age in the file); data only to 2023 so treated cohorts have
# a SHORT post-window (2020-23 cohorts only; 2024-25 contribute as not-yet controls).
# Outputs: data/clean/infant_mortality_muni_year.rds, output/tables/infant_mortality.csv,
#          output/figures/fig_infant_mortality.png
# ============================================================================
source(here::here("code","R","00_config.R")); source(here::here("code","R","00_theme.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi); library(did); library(ggplot2)})
set.seed(20260722); fold5 <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold5), unname(fold5[p]), p)
MFILE <- file.path(RAW$mortality_dir, "M_INFANTIL_HARVARD_2016-2023.xlsx")
stopifnot(file.exists(MFILE))

# ---- crosswalk: ONE province code + normalized municipality name ----
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
norm <- function(x) toupper(stri_trans_general(trimws(x),"Latin-ASCII"))
xw[, `:=`(pc_prov=as.integer(substr(adm3_pcode,6,7)), nm=norm(adm3_name))]

# ---- read all year sheets with a per-sheet canonical column picker ----
pick <- function(n, must) { h <- n[Reduce(`&`, lapply(must, function(p) grepl(p, n, ignore.case=TRUE)))]; h[1] }
m <- rbindlist(lapply(as.character(2016:2023), function(s){
  x <- as.data.table(read_excel(MFILE, sheet=s, col_types="text"))
  pcol <- pick(names(x), c("ID","PROV","HAB|RES")); mcol <- pick(names(x), c("MUNICIPIO","HAB|RES"))
  stopifnot(!is.na(pcol), !is.na(mcol))
  data.table(year=as.integer(s), pc_prov=as.integer(x[[pcol]]), muni=norm(x[[mcol]]), grp=x[["GRUPO_EDAD"]])
}), fill=TRUE)
m[, neonatal := as.integer(grepl("^Neonatal", grp, ignore.case=TRUE))]           # ^Neonatal: early+late only (EXCLUDES "Posneonatal" which contains 'neonatal')
cat("infant deaths read 2016-2023:", nrow(m), "| neonatal:", sum(m$neonatal), "| GRUPO_EDAD types:\n"); print(m[,.N,by=grp])

# ---- map to adm3_pcode via (province code, normalized name) ----
mm <- merge(m, xw[, .(pc_prov, nm, adm3_pcode)], by.x=c("pc_prov","muni"), by.y=c("pc_prov","nm"), all.x=TRUE)
cat(sprintf("\nmatched to municipality: %d / %d (%.1f%%)\n", mm[!is.na(adm3_pcode),.N], nrow(mm), 100*mean(!is.na(mm$adm3_pcode))))
if(mm[is.na(adm3_pcode),.N]>0){ cat("UNMATCHED (top):\n"); print(mm[is.na(adm3_pcode), .N, by=.(pc_prov,muni)][order(-N)][1:10]) }
mm <- mm[!is.na(adm3_pcode)]; mm[, adm3_pcode := to155(adm3_pcode)]

# ---- deaths per municipality x year ----
dth <- mm[, .(n_infant=.N, n_neonatal=sum(neonatal)), by=.(adm3_pcode, year)]

# ---- denominator: total live births per municipality x year (BDNV, folded to 155) ----
b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))[!is.na(adm3_pcode)]
b[, id := to155(adm3_pcode)]
births <- b[birth_year %in% 2016:2023, .(live_births=.N), by=.(adm3_pcode=id, year=birth_year)]

# ---- panel 2016-2023 (155 muni) + treatment (S1) ----
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
trt[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
panel <- CJ(adm3_pcode=unique(births$adm3_pcode), year=2016:2023)
panel <- merge(panel, births, by=c("adm3_pcode","year"), all.x=TRUE)
panel <- merge(panel, dth,    by=c("adm3_pcode","year"), all.x=TRUE)
panel[is.na(n_infant), `:=`(n_infant=0L, n_neonatal=0L)]
panel <- merge(panel, trt[, .(adm3_pcode, g)], by="adm3_pcode")
panel[, `:=`(IMR=1000*n_infant/live_births, NMR=1000*n_neonatal/live_births)]
panel <- panel[!is.na(g) & !is.na(live_births) & live_births>0]
fold3 <- c("DOM010905","DOM051703","DOM012510"); panel <- panel[!adm3_pcode %in% fold3]
saveRDS(panel, file.path(DIR_CLEAN,"infant_mortality_muni_year.rds"))
cat(sprintf("\npanel: %d muni x year | national IMR ~%.1f, NMR ~%.1f per 1,000 births\n",
    nrow(panel), 1000*sum(panel$n_infant)/sum(panel$live_births), 1000*sum(panel$n_neonatal)/sum(panel$live_births)))

# ---- CS event study (S1) ----
csm <- function(yname){
  est <- copy(panel); est[, id:=as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g", xformla=~1,
        data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
  list(gr=suppressMessages(aggte(a,type="group",na.rm=TRUE)), dy=suppressMessages(aggte(a,type="dynamic",na.rm=TRUE)))
}
ri <- csm("IMR"); rn <- csm("NMR")
bi <- panel[g>0][year==g-1L, mean(IMR)]; bn <- panel[g>0][year==g-1L, mean(NMR)]
res <- data.table(outcome=c("Infant mortality (IMR)","Neonatal mortality (NMR)"),
  ATT=round(c(ri$gr$overall.att, rn$gr$overall.se),2))
res <- data.table(outcome=c("Infant mortality (IMR)","Neonatal mortality (NMR)"),
  ATT=round(c(ri$gr$overall.att, rn$gr$overall.att),2), SE=round(c(ri$gr$overall.se, rn$gr$overall.se),2),
  p=round(2*pnorm(-abs(c(ri$gr$overall.att/ri$gr$overall.se, rn$gr$overall.att/rn$gr$overall.se))),3),
  baseline=round(c(bi,bn),1))
cat("\n==================== INFANT / NEONATAL MORTALITY (S1 CS, per 1,000 births) ====================\n"); print(res, class=FALSE)
fwrite(res, file.path(PROJ,"output","tables","infant_mortality.csv"))

# ---- figure: dynamic event study (IMR) ----
dd <- data.table(e=ri$dy$egt, att=ri$dy$att.egt, se=ri$dy$se.egt)[e>=-4 & e<=3]
dd[, `:=`(lo=att-1.96*se, hi=att+1.96*se)]
g <- ggplot(dd, aes(e, att)) + es_guides() +
  geom_ribbon(aes(ymin=lo,ymax=hi), fill=PCUA_COL$teal, alpha=0.15) +
  geom_line(color=PCUA_COL$teal, linewidth=0.9) + geom_point(color=PCUA_COL$teal, fill="white", shape=21, size=2.4, stroke=1.1) +
  scale_x_continuous(breaks=seq(-4,3,1)) +
  labs(title="Infant mortality after an AU opens (exploratory extension)",
       subtitle=sprintf("S1 CS event study on infant deaths per 1,000 live births (baseline %.1f); data only to 2023", bi),
       x="Years since first AU opening", y="ATT: infant deaths per 1,000 live births",
       caption="Aggregate (NOT teen-specific; file has no mother age). Rare event -> noisy. Short post-window (to 2023). Not-yet controls; 95% CIs; muni-clustered.") +
  theme_pcua()
ggsave(file.path(PROJ,"output","figures","fig_infant_mortality.png"), g, width=9, height=5, dpi=200)
cat("\nsaved -> infant_mortality_muni_year.rds + infant_mortality.csv + fig_infant_mortality.png\n")
