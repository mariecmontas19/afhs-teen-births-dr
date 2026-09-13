# ============================================================================
# 08z6_educ_sector_urbanrural.R — enrollment effect: SECTOR decomposition
# (public / private / semi-official) + URBAN/RURAL adjustment (MM 2026-07-29).
# The AUs are inside PUBLIC hospitals, so a public-concentrated enrollment
# effect supports the mechanism; a private-sector effect would signal a
# demographic/confound. Enrollment rate = sector enrollment / pop 15-19
# (Series A, both sexes); sectors are additive to the +1.60 total.
# Urban/rural: (B) condition on BASELINE % urban (census, time-invariant) via
# CS conditional PT; (C) split treated munis at median baseline % urban.
# NOTE on "adjust for overage": overage is downstream of treatment (bad
# control), so the overage-adjusted enrollment is the ON-TRACK outcome from
# 08z5 (+0.92, p=.10, clean leads), NOT overage as a regressor.
# Output: output/tables/educ_sector_urbanrural.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi); library(did)})
set.seed(20260729)
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155<- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)
F1 <- file.path(DRPAPER,"analysis/datasets/SAIP",
                "OAI-0877-2026 Matricula Desagregada 2015-2025 Condicion Final y Sobreedad.xlsx")
mc <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[
        , .(prov_nm=norm(prov_name), muni_nm=norm(adm3_name), adm3_pcode)]

pieces <- list()
for(s in excel_sheets(F1)){
  peek <- suppressMessages(as.matrix(read_excel(F1, sheet=s, col_names=FALSE, n_max=6)))
  hr <- which(apply(peek,1,function(r) any(r=="Municipio",na.rm=TRUE)&any(r=="Nivel",na.rm=TRUE)))[1]
  d <- suppressMessages(as.data.table(read_excel(F1, sheet=s, skip=hr-1)))
  d[, tg := suppressWarnings(as.numeric(`Total general`))]
  d <- d[norm(Nivel)=="SECUNDARIO" & !is.na(tg)]
  d <- d[, .(prov_nm=norm(Provincia), muni_nm=norm(Municipio), sector=norm(Sector), mat=tg)]
  d[, school_year := s]; pieces[[s]] <- d
}
ed <- rbindlist(pieces); ed[, year_end := as.integer(substr(school_year,6,9))]
ed[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
ed <- merge(ed, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
un <- ed[is.na(adm3_pcode), .N, by=.(prov_nm,muni_nm)]
for(i in seq_len(nrow(un))){ cand <- mc[prov_nm==un$prov_nm[i]]; if(!nrow(cand)) next
  dd <- drop(adist(un$muni_nm[i], cand$muni_nm))
  if(min(dd)<=3 && sum(dd==min(dd))==1)
    ed[prov_nm==un$prov_nm[i] & muni_nm==un$muni_nm[i], adm3_pcode := cand$adm3_pcode[which.min(dd)]] }
ed <- ed[!is.na(adm3_pcode)]; ed[, adm3_pcode := to155(adm3_pcode)]
cat("sector labels:", paste(sort(unique(ed$sector)), collapse=" | "), "\n")

pop <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))[
         age_group=="15-19", .(pop=sum(pop)), by=.(adm3_pcode, year_end=year)]
# complete muni x year x sector grid (0-fill absent combos) so sectors are
# additive and share ONE balanced panel; a muni with no private school = 0,
# not missing.
sw <- dcast(ed, adm3_pcode + year_end ~ sector, value.var="mat",
            fun.aggregate=sum, fill=0)
for(sc in c("PUBLICO","PRIVADO","SEMIOFICIAL")) if(!sc %in% names(sw)) sw[, (sc) := 0]
sw[, TOTAL := PUBLICO + PRIVADO + SEMIOFICIAL]
sec <- melt(sw, id.vars=c("adm3_pcode","year_end"), variable.name="sector",
            value.name="mat")[, sector := as.character(sector)]
sec <- merge(sec, pop, by=c("adm3_pcode","year_end"), all.x=TRUE)
sec[, erate := 100*mat/pop]

urb <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_covariates_muni.rds")))[, .(adm3_pcode, pct_urban)]
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% names(fold)]
trt[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]

runCS <- function(dt, yvar="erate", xf=~1, lab=""){
  d <- copy(dt); d[, g_edu := fifelse(g==0L, 0L, g+1L)]; d[, id := as.integer(factor(adm3_pcode))]
  d <- d[is.finite(get(yvar))]
  a <- att_gt(yname=yvar, tname="year_end", idname="id", gname="g_edu", xformla=xf, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  pre <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,-2)]
  lp <- 2*pnorm(-abs(pre[,sum(att/se^2)/sum(1/se^2)]/sqrt(1/pre[,sum(1/se^2)])))
  base <- d[g>0 & year_end==g-1L, mean(get(yvar), na.rm=TRUE)]
  data.table(spec=lab, ATT=round(gg$overall.att,3), SE=round(gg$overall.se,3),
             p=round(2*pnorm(-abs(gg$overall.att/gg$overall.se)),3),
             pct=round(100*gg$overall.att/base,1), lead_p=round(lp,3))
}

# --- A. sector decomposition ---
A <- rbindlist(lapply(c("TOTAL","PUBLICO","SEMIOFICIAL","PRIVADO"), function(sc)
  runCS(merge(sec[sector==sc], trt[!is.na(g), .(adm3_pcode,g)], by="adm3_pcode"),
        lab=paste0("Sector: ", sc))))

# --- B. total enrollment, +baseline % urban covariate ---
totm <- merge(sec[sector=="TOTAL"], trt[!is.na(g), .(adm3_pcode,g)], by="adm3_pcode")
totm <- merge(totm, urb, by="adm3_pcode", all.x=TRUE)
B <- rbind(runCS(totm, lab="Total (unconditional)"),
           runCS(totm, xf=~pct_urban, lab="Total (+ baseline % urban)"))

# --- C. urban/rural split of treated munis (median baseline % urban among treated) ---
med <- urb[adm3_pcode %in% trt[g>0, adm3_pcode], median(pct_urban, na.rm=TRUE)]
totm[, treated_urban := pct_urban >= med]
# keep never-treated (g==0) in BOTH subsamples as controls; split only the treated
Curb <- runCS(totm[g==0L | (g>0L & treated_urban==TRUE)],  lab=sprintf("Treated URBAN (>= %.2f urban)", med))
Crur <- runCS(totm[g==0L | (g>0L & treated_urban==FALSE)], lab=sprintf("Treated RURAL (< %.2f urban)", med))
C <- rbind(Curb, Crur)

cat("\n===== A. ENROLLMENT RATE BY SECTOR (per 100 pop 15-19; additive to TOTAL) =====\n"); print(A, class=FALSE)
cat("\n===== B. URBAN COVARIATE ADJUSTMENT (total enrollment) =====\n"); print(B, class=FALSE)
cat("\n===== C. URBAN vs RURAL TREATED MUNIS (total enrollment) =====\n"); print(C, class=FALSE)
fwrite(rbind(A,B,C, fill=TRUE), file.path(PROJ,"output","tables","educ_sector_urbanrural.csv"))
cat("\nsaved -> educ_sector_urbanrural.csv\n")
