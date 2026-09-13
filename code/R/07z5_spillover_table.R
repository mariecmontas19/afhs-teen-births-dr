# ============================================================================
# 07z5_spillover_table.R — replaces the weak 2-point spillover GRAPH with a TABLE
# making the "effect is local / S1 design is clean" point honestly:
#   (1) OWN municipio gets a unit (S1 headline CS)         -> the effect
#   (2) NEIGHBOUR within 20 km, no own unit (spillover)    -> small, ns
#   (3) DONUT: re-estimate S1 dropping never-treated controls within X km of a unit.
#       If S1 is stable, spillover is NOT contaminating the controls -> design clean.
# Econ significance stars: *** p<.01, ** p<.05, * p<.10 (MM convention).
# Denom A; CS att_gt notyettreated/reg/universal/muni-cluster. Same 155-panel as headline.
# Output: output/tables/spillover_locality.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722)

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[,.(adm3_pcode,year,rateA_15_19,ever_treated)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
dp  <- as.data.table(readRDS(file.path(DIR_CLEAN,"distance_open_panel.rds")))
d   <- merge(p, bin[!is.na(g),.(adm3_pcode,g)], by="adm3_pcode")
d   <- merge(d, dp[year==2025L,.(adm3_pcode, dist2025=dist_open_km)], by="adm3_pcode")

stars <- function(pv) fifelse(pv<.01,"***", fifelse(pv<.05,"**", fifelse(pv<.10,"*","")))
csrun <- function(dat, gvar="g"){
  dat <- copy(dat)[!is.na(get(gvar))]; dat[, id:=as.integer(factor(adm3_pcode))]
  a<-suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19",tname="year",idname="id",gname=gvar,xformla=~1,data=dat,
       control_group="notyettreated",base_period="universal",est_method="reg",bstrap=TRUE,biters=2000,clustervars="id",print_details=FALSE)))
  o<-suppressMessages(aggte(a,type="group",na.rm=TRUE)); pv<-2*pnorm(-abs(o$overall.att/o$overall.se))
  data.table(ATT=round(o$overall.att,2), SE=round(o$overall.se,2), p=round(pv,3), sig=stars(pv),
    CI95=sprintf("[%.1f, %.1f]", o$overall.att-1.96*o$overall.se, o$overall.att+1.96*o$overall.se),
    n_treated=uniqueN(dat[get(gvar)>0,adm3_pcode]), n_control=uniqueN(dat[get(gvar)==0,adm3_pcode]))
}
# neighbour-within-20km cohort (own-untreated only): first year within 20 km; drop already-within in 2016
acc <- merge(p[,.(adm3_pcode,year,ever_treated)], dp[,.(adm3_pcode,year,acc20)], by=c("adm3_pcode","year"))
co <- acc[, .(first1={w<-which(acc20==1L); if(length(w)) year[min(w)] else NA_integer_}, in2016=acc20[year==2016L],
              own=ever_treated[1]), by=adm3_pcode]
co[, gN := fifelse(own==1L, NA_integer_, fifelse(in2016==1L, NA_integer_, fifelse(is.na(first1),0L,as.integer(first1))))]
dN <- merge(p, co[,.(adm3_pcode,gN)], by="adm3_pcode")

rows <- list()
rows[["own"]]  <- cbind(row="1. Own municipio gets a unit (S1)", csrun(d, "g"))
rows[["nb"]]   <- cbind(row="2. Neighbour <=20 km, no own unit (spillover)", csrun(dN, "gN"))
for(x in c(10,20,30)){
  dd <- copy(d)[!(g==0 & dist2025<=x)]
  rows[[paste0("donut",x)]] <- cbind(row=sprintf("3. DONUT: S1 dropping controls <=%d km", x), csrun(dd, "g"))
}
out <- rbindlist(rows)
cat("============== LOCALITY / SPILLOVER TABLE (denom A; *** p<.01 ** p<.05 * p<.10) ==============\n")
print(out, class=FALSE)
fwrite(out, file.path(DIR_TABLES,"spillover_locality.csv"))

# ---- donut depletion note: justify the 30 km cap (control pool empties past it) ----
ctl <- unique(d[g==0, .(adm3_pcode, dist2025)])
left <- sapply(c(30,40,50), function(x) ctl[dist2025>x, .N])
cap <- sprintf(paste0(
  "Notes. Each row is a Callaway-Sant'Anna group-ATT on the teen birth rate (denom A; not-yet-treated controls; ",
  "municipio-clustered). Row 1 = headline (own unit opens). Row 2 = proximity instead of ownership (treatment = a ",
  "unit opens within 20 km of a municipio that never gets its own). Rows 3 = the donut: the SAME own-unit (S1) ",
  "estimate, re-run after deleting never-treated controls within X km of a unit, to test whether spillover ",
  "contaminated the controls. The donut is capped at 30 km because the control pool empties beyond it: of %d ",
  "never-treated controls (max distance to nearest unit = %d km), only %d remain past 30 km, %d past 40 km, and ",
  "%d past 50 km, so wider radii estimate off a tiny, remote, selected comparison group rather than removing ",
  "spillover. Stars: *** p<.01, ** p<.05, * p<.10."),
  nrow(ctl), round(max(ctl$dist2025)), left[1], left[2], left[3])
writeLines(cap, file.path(DIR_TABLES,"spillover_locality_note.txt"))
cat("\n", cap, "\n", sep="")
cat("\nsaved -> output/tables/spillover_locality.csv + spillover_locality_note.txt\n")
