# ============================================================================
# 06b_monthly_panel.R — MONTHLY municipio panel for the 9-month GESTATIONAL-LAG
# FALSIFICATION (secondary). A AU opening in month m cannot affect births until
# ~9 months later, so a flat 0-8-month response with a break at ~month 9 is clean
# evidence the effect is real (not pre-trend/spurious). Teen (15-19) births by
# municipio(155) x year x calendar-month; monthly exposure = annual women/12;
# event_month = months since first opening (for the 14 in-window treated municipios
# with a known opening month). Calendar-month FE handle seasonality (one-step).
# Output: data/clean/panel_muni_month_155.rds (155 x 10 x 12 = 18,600).
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table)})

fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)

b   <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
A   <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))
B   <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_B.rds")))
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))

# teen (15-19) births by municipio(155) x year x month
bb <- b[!is.na(adm3_pcode) & !is.na(birth_year) & !is.na(birth_month) & age_grp=="15-19"]
bb[, id := to155(adm3_pcode)]
cnt <- bb[, .(n_1519=.N), by=.(id, year=birth_year, month=birth_month)]

# balanced frame 155 x 10 x 12, + monthly exposure (annual women 15-19 / 12)
ids <- unique(to155(trt$adm3_pcode))                                  # 155
frame <- CJ(id=ids, year=2016:2025, month=1:12)
wA <- A[sex=="F" & age_group=="15-19", .(id=adm3_pcode, year, womenA_ann=pop)]
wB <- B[sex=="F" & age_group=="15-19", .(id=adm3_pcode, year, womenB_ann=pop)]
m <- Reduce(function(x,y) merge(x,y,by=intersect(names(x),names(y)),all.x=TRUE),
            list(frame, wA, wB, cnt))
m[is.na(n_1519), n_1519 := 0L]
m[, `:=`(expA = womenA_ann/12, expB = womenB_ann/12, cal_month = factor(month))]
m[, `:=`(rateA_ann = 1000*n_1519/expA, rateB_ann = 1000*n_1519/expB)]  # annualized monthly ASFR

# treatment monthly timing (155): first opening year+month; event_month = months since opening
keep155 <- setdiff(trt$adm3_pcode, names(fold))
tt <- trt[adm3_pcode %in% keep155, .(id=adm3_pcode, ever_treated, always_treated, first_year, first_month, cohort, n_units)]
m <- merge(m, tt, by="id", all.x=TRUE)
m[, month_idx := (year-2016L)*12L + month]                            # absolute month index
m[, event_month := ifelse(ever_treated==1 & always_treated==0 & !is.na(first_month),
                          (year-first_year)*12L + (month-first_month), NA_integer_)]
stopifnot(nrow(m)==155*10*12, m[is.na(n_1519),.N]==0)
saveRDS(m, file.path(DIR_CLEAN,"panel_muni_month_155.rds"))

cat("\n==================== 06b MONTHLY PANEL VALIDATION ====================\n")
cat("rows:", nrow(m), "(155 x 10 x 12 = 18,600) | teen births total:", sum(m$n_1519), "(expect 218,968 less 3 NA-month)\n")
cat("in-window treated municipios with usable opening MONTH (event_month defined):",
    m[!is.na(event_month), uniqueN(id)], "\n")
cat("\nteen births by CALENDAR MONTH (seasonality check, pooled):\n")
print(m[, .(births=sum(n_1519)), by=month][order(month)])
cat("\nevent_month range (months since opening):", paste(range(m$event_month,na.rm=TRUE),collapse=" to "), "\n")
cat("mean teen births/municipio-month:", round(mean(m$n_1519),2), "(thin -> monthly is falsification only)\n")
cat("saved -> data/clean/panel_muni_month_155.rds\n")
