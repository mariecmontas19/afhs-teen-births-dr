# ============================================================================
# 05k2_mics_facility_by_age.R — facility delivery by mother's age at birth,
# ENHOGAR-MICS 2019 (JC review #12: is registration coverage lower for teen
# births?). The BDNV is compiled from health facilities (2019: 16 of 162,059
# records are home/other births), so births outside facilities are the births
# it can miss. Reference birth = the woman's most recent live birth in the 24
# months before interview (the MN module's reference birth), dated from the
# birth history; age at birth = (BH4C - WDOB)/12. MN20 place codes follow the
# MICS6 standard: 11-12 home, 21-26 public facility, 31-36 private facility,
# 76/96 other. Weighted by wmweight.
# Output: output/tables/mics2019_facility_delivery_by_age.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
MICSDIR <- file.path(DRPAPER,"analysis/datasets/Encuestas de Hogares de Prósitos Múltiples (ENHOGAR)/2019")
wm <- fread(file.path(MICSDIR,"ENHOGAR-MICS6-2019-PUB-MUJERES-DE-15-A-49-AÑOS.csv"),
            select=c("HH1","HH2","LN","WM17","wmweight","WDOI","WDOB","MN20"))
bh <- fread(file.path(MICSDIR,"ENHOGAR-MICS6-2019-PUB-HISTORIA-DE-NACIMIENTO-MUJERES-DE-15-A-49-AÑOS.csv"),
            select=c("HH1","HH2","LN","BH4C"))
w <- wm[WM17==1 & !is.na(MN20)]
m <- merge(bh, w[, .(HH1,HH2,LN,WDOI,WDOB)], by=c("HH1","HH2","LN"))
m[, `:=`(ab=(BH4C-WDOB)/12, mb=WDOI-BH4C)]
last <- m[mb>=0 & mb<24][order(-BH4C)][, .SD[1], by=.(HH1,HH2,LN)]   # most recent birth in 24 months = MN reference birth
d <- merge(last[, .(HH1,HH2,LN,ab)], w, by=c("HH1","HH2","LN"))
cat("women with MN20 answered:", nrow(w), "| linked to a dated birth in the last 24 months:", nrow(d), "\n")
d[, place := fcase(MN20 %in% c(11,12), "home", MN20 %in% c(21,22,23,26), "public facility",
                   MN20 %in% c(31,32,33,36), "private facility", default="other")]
d[, grp := fifelse(ab < 20, "mother <20 at birth", "mother 20+ at birth")]
wmean <- function(x,wt) sum(x*wt)/sum(wt)
out <- d[, .(n_unw=.N, facility=100*wmean(place %in% c("public facility","private facility"), wmweight),
             public=100*wmean(place=="public facility", wmweight),
             home=100*wmean(place=="home", wmweight), other=100*wmean(place=="other", wmweight)), by=grp]
print(out[order(grp)])
tot <- d[, .(n_unw=.N, facility=100*wmean(place %in% c("public facility","private facility"), wmweight))]
print(tot)
fwrite(rbind(out, data.table(grp="all", tot), fill=TRUE), file.path(DIR_TABLES,"mics2019_facility_delivery_by_age.csv"))
