# ============================================================================
# 08z10_edn6p_placebo.R — Evaluación Diagnóstica 6to PRIMARIA placebo (§59.84).
# MINERD microdata received 2026-08-12 (see Evaluacion Diagnostica/README).
#
# DESIGN: two-period municipality DiD on 6th-grade-primary test z-scores,
# waves 2018 and 2024 (the two 6P EDN waves). Students are ~11-12 years old —
# below the AU target population — so the AU mechanism predicts ZERO here;
# a nonzero effect would flag general school/area improvement (a confound
# for the schooling-outcomes table), not adolescent-services response.
#   Treated: municipalities whose first AU opened between the waves
#            (g_edu in 2019..2024, school-year convention g_edu = first_year+1).
#   Control: never-treated + not-yet-by-2024 (g_edu 0 or > 2024).
#   Excluded: pre-2016 (always) and g_edu <= 2018 (already treated at wave 1).
# Estimate: OLS of muni Delta z (2024-2018) on treated, HC2 robust SE.
# z built like 05q: center-level within year x subject standardization
# (student-weighted), muni = student-weighted mean. REPORT-ONLY (MM 2026-08-12):
# writes output/tables/edn6p_placebo.csv, feeds NO paper exhibit.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold155 <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold155), unname(fold155[p]), p)
EDN <- file.path(DRPAPER,"analysis/datasets/Evaluacion Diagnostica")

## ---- 1. center code -> adm3 (same OAI-0790 logic as 05q_pruebas_build.R) ---
F1 <- file.path(DRPAPER,"analysis/datasets/SAIP/OAI-0790-2026 Lista de centros con datos de niveles y tandas 2015-2025.xlsx")
mc <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[
        , .(prov_nm=norm(prov_name), muni_nm=norm(adm3_name), adm3_pcode)]
pieces <- list()
for(s in excel_sheets(F1)){
  peek <- suppressMessages(as.matrix(read_excel(F1, sheet=s, col_names=FALSE, n_max=6)))
  hr <- which(apply(peek,1,function(r) any(r=="Regional",na.rm=TRUE)&any(r=="Municipio",na.rm=TRUE)))[1]
  d <- suppressMessages(as.data.table(read_excel(F1, sheet=s, skip=hr-1)))
  pv <- names(d)[toupper(names(d))=="PROVINCIA"][1]; mn <- names(d)[toupper(names(d))=="MUNICIPIO"][1]
  hit8 <- function(v) mean(grepl("^\\s*\\d{7,8}\\s*-", as.character(v)), na.rm=TRUE)
  cc <- names(d)[vapply(d, hit8, 0) > 0.5][1]
  stopifnot(!is.na(cc), !is.na(pv), !is.na(mn))
  x <- d[, .(centro=as.character(get(cc)), prov_nm=norm(get(pv)), muni_nm=norm(get(mn)))]
  x[, code := sub("^\\s*(\\d{7,8})\\s*-.*$", "\\1", centro)]
  x <- x[grepl("^\\d{7,8}$", code)][, code := formatC(as.integer(code), width=8, flag="0")]
  pieces[[s]] <- unique(x[, .(code, prov_nm, muni_nm)])
}
cw <- unique(rbindlist(pieces))
cw[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
cw <- merge(cw, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
un <- unique(cw[is.na(adm3_pcode), .(prov_nm, muni_nm)])
for(i in seq_len(nrow(un))){
  cand <- mc[prov_nm==un$prov_nm[i]]
  hit <- cand[agrepl(un$muni_nm[i], muni_nm, max.distance=0.15)]
  if(nrow(hit)==1) cw[prov_nm==un$prov_nm[i] & muni_nm==un$muni_nm[i], adm3_pcode := hit$adm3_pcode]
}
cw <- cw[!is.na(adm3_pcode)]
amb <- cw[, .(n=uniqueN(adm3_pcode)), by=code][n>1]
code2adm <- unique(cw[!code %in% amb$code, .(code, adm3_pcode)])
cat("OAI center codes mapped:", nrow(code2adm), "| ambiguous dropped:", nrow(amb), "\n")

## ---- 2. EDN 6P 2018 (student level) -> center x subject means --------------
e18 <- as.data.table(read_excel(file.path(EDN,"EDN6P2018.xlsx"), sheet="Sheet1", col_types="text"))
setnames(e18, c("REGIONAL_NOMBRE","REGIONAL_CODIGO","DISTRITO_NOMBRE","DISTRITO_CODIGO",
                "CENTRO_NOMBRE","CENTRO_CODIGO","CODIGOSIGERD","SECTOR","ZONA","TANDA",
                "id_est","SEXO","EDAD","GRADO","QF","QC",
                "p_len","n_len","p_mat","n_mat","p_nat","n_nat","p_soc","n_soc"))
for(v in c("p_len","p_mat","p_nat","p_soc")) e18[, (v) := suppressWarnings(as.numeric(get(v)))]
e18[, code := formatC(as.integer(CENTRO_CODIGO), width=8, flag="0")]
c18 <- melt(e18, id.vars="code", measure.vars=c("p_len","p_mat","p_nat","p_soc"),
            variable.name="subj", value.name="score")[!is.na(score) & score>0]
c18 <- c18[, .(score=mean(score), n=.N), by=.(code, subj)][, year := 2018L]
cat("EDN6P2018: students", format(nrow(e18),big.mark=","), "-> center-subject cells", nrow(c18), "\n")

## ---- 3. EDN 6P 2024 (center x subject x nivel) -> center x subject means ---
e24 <- as.data.table(read_excel(file.path(EDN,"EDN6P2024.xlsx"), sheet="EDN_6P_2024", col_types="text"))
setnames(e24, c("REGIONAL_NOMBRE","CENTRO_NOMBRE","CENTRO_CODIGO","CODIGOSIGERD",
                "GRADO","MATRICULA","Asignatura","Nivel","puntaje"))
e24[, `:=`(mat_n = suppressWarnings(as.numeric(MATRICULA)),
           puntaje = suppressWarnings(as.numeric(puntaje)))]
e24 <- e24[!is.na(mat_n) & mat_n>0 & !is.na(puntaje) & puntaje>0]
e24[, code := formatC(as.integer(CENTRO_CODIGO), width=8, flag="0")]
smap <- c("Lengua Española"="p_len","Lengua Espanola"="p_len","Matemática"="p_mat","Matematica"="p_mat",
          "Ciencias de la Naturaleza"="p_nat","Ciencias Sociales"="p_soc")
e24[, subj := smap[trimws(Asignatura)]]
cat("EDN6P2024 Asignatura values:", paste(e24[,unique(trimws(Asignatura))], collapse=" | "), "\n")
stopifnot(!anyNA(e24$subj))
c24 <- e24[, .(score=weighted.mean(puntaje, mat_n), n=sum(mat_n)), by=.(code, subj)][, year := 2024L]
cat("EDN6P2024: center-subject cells", nrow(c24), "| students (matricula):",
    format(e24[, sum(mat_n)]/uniqueN(e24$subj), big.mark=","), "per subject approx\n")

## ---- 4. z within year x subject; municipality panel ------------------------
cc <- rbind(c18, c24)
cc[, subj := as.character(subj)]
cc[, `:=`(mu = weighted.mean(score, n),
          sg = sqrt(sum(n*(score-weighted.mean(score,n))^2)/sum(n))), by=.(year, subj)]
cc[, z := (score-mu)/sg]
cc <- merge(cc, code2adm, by="code")
mr <- cc[, .(matched=sum(n)), by=year]
tot <- rbind(c18[, .(n=sum(n)), by=year], c24[, .(n=sum(n)), by=year])
print(merge(mr, tot, by="year")[, .(year, match_pct=round(100*matched/n,1))], class=FALSE)
cc[, adm3_pcode := to155(adm3_pcode)]
muni <- cc[, .(z = weighted.mean(z, n)), by=.(adm3_pcode, year, subj)]
muni <- muni[, .(z_all = mean(z), nsubj=.N), by=.(adm3_pcode, year)]
w <- dcast(muni, adm3_pcode ~ year, value.var="z_all")
setnames(w, c("2018","2024"), c("z18","z24"))
w <- w[!is.na(z18) & !is.na(z24)][, dz := z24 - z18]

## ---- 5. two-period DiD ------------------------------------------------------
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[
        !adm3_pcode %in% names(fold155)]
bin[, g_edu := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year)+1L,
                       fifelse(ever_treated==0L, 0L, NA_integer_))]   # NA = always/pre-2016
w <- merge(w, bin[, .(adm3_pcode, g_edu)], by="adm3_pcode")
w <- w[!is.na(g_edu)]                       # drop pre-2016 (treated at both waves)
w <- w[!(g_edu > 0 & g_edu <= 2018)]        # drop treated already at wave 1
w[, treated := as.integer(g_edu >= 2019 & g_edu <= 2024)]
cat("\nplacebo sample:", nrow(w), "municipalities |", sum(w$treated), "treated (opened between waves) |",
    nrow(w)-sum(w$treated), "control (never/not-yet-by-2024)\n")
m <- lm(dz ~ treated, data=w)
V <- sandwich::vcovHC(m, type="HC2")
b <- coef(m)["treated"]; se <- sqrt(V["treated","treated"])
cat(sprintf("\nEDN 6P placebo DiD (Delta z 2024-2018, treated vs control):\n  %+0.3f SD (SE %.3f), p = %.3f, 95%% CI [%+.3f, %+.3f]\n",
    b, se, 2*pnorm(-abs(b/se)), b-1.96*se, b+1.96*se))
cat(sprintf("  mean Delta z: treated %+.3f | control %+.3f\n",
    w[treated==1, mean(dz)], w[treated==0, mean(dz)]))
res <- data.table(outcome="EDN 6P z (2-period DiD 2018->2024)", ATT=b, SE=se,
                  p=2*pnorm(-abs(b/se)), lo=b-1.96*se, hi=b+1.96*se,
                  n_muni=nrow(w), n_treated=sum(w$treated))
fwrite(res, file.path(DIR_TABLES,"edn6p_placebo.csv"))
saveRDS(w[, .(adm3_pcode, dz, treated, g_edu)], file.path(DIR_CLEAN,"edn6p_muni_dz.rds"))  # for the 08z11 3S-6P contrast
cat("saved -> edn6p_placebo.csv + edn6p_muni_dz.rds (report-only; NO paper exhibit)\n")
