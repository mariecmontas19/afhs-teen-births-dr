# ============================================================================
# 08z11_edn3s_did.R — Evaluación Diagnóstica 3ro SECUNDARIA, 2019 -> 2025
# (§59.85). Luis (MINERD) re-sent the 2025 file WITH geography 2026-08-12:
# EDN20253S_v2_congeografia.xlsx (student x subject, CENTRO_CODIGO included).
#
# ON-MECHANISM COUNTERPART to the 08z10 6P placebo: 3ro-secundaria students
# (~14-16, inside the AU 10-19 window) in the same two-period municipality
# DiD design, waves 2019 and 2025.
#   Treated: first AU opened between the waves (g_edu 2020..2025).
#   Control: never-treated + not-yet-by-2025 (g_edu 0 or >= 2026).
#   Excluded: pre-2016 (always) and g_edu <= 2019.
# ALSO reports the within-municipality contrast against the 6P placebo
# (ddz = dz_3S - dz_6P): under the "AU municipalities trend upward generally"
# reading (§59.84), ddz ~ 0; an adolescent-specific learning effect => ddz > 0.
# PREDICTION (rule 1b, stated before running): 3S ~ +0.2-0.3 = 6P, ddz ~ 0.
# REPORT-ONLY (MM): writes output/tables/edn3s_did.csv, feeds NO paper exhibit.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold155 <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold155), unname(fold155[p]), p)
EDN <- file.path(DRPAPER,"analysis/datasets/Evaluacion Diagnostica")

## ---- 1. center -> adm3 crosswalk (same OAI-0790 logic as 05q/08z10) --------
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

## ---- 2. wave 2019: school-level means (EDN3S2019) ---------------------------
e19 <- as.data.table(read_excel(file.path(EDN,"EDN3S2019.xlsx"), sheet="Hoja1", col_types="text"))
e19 <- e19[, 1:10]
setnames(e19, c("distrito","centro_nm","code","sector","zona","n","m_esp","m_mat","m_soc","m_nat"))
for(v in c("n","m_esp","m_mat","m_soc","m_nat")) e19[, (v) := suppressWarnings(as.numeric(get(v)))]
e19 <- e19[!is.na(code) & !is.na(n) & n>0]
e19[, code := formatC(as.integer(code), width=8, flag="0")]
c19 <- melt(e19, id.vars=c("code","n"), measure.vars=c("m_esp","m_mat","m_soc","m_nat"),
            variable.name="subj", value.name="score")[!is.na(score) & score>0]
c19[, subj := sub("^m_","",subj)][, year := 2019L]
c19 <- c19[, .(code, subj, score, n, year)]
cat("EDN3S2019: schools", uniqueN(e19$code), "| students (matricula):", format(sum(e19$n),big.mark=","), "\n")

## ---- 3. wave 2025: student x subject microdata (v2, with geography) --------
e25 <- as.data.table(read_excel(file.path(EDN,"EDN20253S_v2_congeografia.xlsx"),
                                sheet="EDN_3S_2025_quintilSE", col_types="text"))
stopifnot(all(c("CENTRO_CODIGO","Asignatura","ScoreFinal","EstadoAsistencia") %in% names(e25)))
e25[, score := suppressWarnings(as.numeric(ScoreFinal))]
e25 <- e25[norm(EstadoAsistencia)=="PRESENTE" & !is.na(score) & score>0]
e25[, code := formatC(as.integer(CENTRO_CODIGO), width=8, flag="0")]
e25[, asg := norm(Asignatura)]
e25[, subj := fifelse(grepl("MATEM", asg), "mat",
              fifelse(grepl("LENGUA|ESPA", asg), "esp",
              fifelse(grepl("NATUR", asg), "nat",
              fifelse(grepl("SOCIAL", asg), "soc", NA_character_))))]
cat("EDN2025 3S Asignatura -> subj:\n"); print(e25[, .N, by=.(asg, subj)])
stopifnot(!anyNA(e25$subj))
c25 <- e25[, .(score=mean(score), n=.N), by=.(code, subj)][, year := 2025L]
cat("EDN2025 3S: centers", uniqueN(c25$code), "| student-subject rows kept:", format(nrow(e25),big.mark=","), "\n")

## ---- 4. z within year x subject; municipality panel; DiD -------------------
cc <- rbind(c19, c25, fill=TRUE)
cc[, subj := as.character(subj)]
cc[, `:=`(mu = weighted.mean(score, n),
          sg = sqrt(sum(n*(score-weighted.mean(score,n))^2)/sum(n))), by=.(year, subj)]
cc[, z := (score-mu)/sg]
cc <- merge(cc, code2adm, by="code")
mm <- cc[, .(matched_students=sum(n)), by=year]
print(mm, class=FALSE)
cc[, adm3_pcode := to155(adm3_pcode)]
muni <- cc[, .(z = weighted.mean(z, n)), by=.(adm3_pcode, year, subj)]
muni <- muni[, .(z_all = mean(z)), by=.(adm3_pcode, year)]
w <- dcast(muni, adm3_pcode ~ year, value.var="z_all")
setnames(w, c("2019","2025"), c("z19","z25"))
w <- w[!is.na(z19) & !is.na(z25)][, dz := z25 - z19]

bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[
        !adm3_pcode %in% names(fold155)]
bin[, g_edu := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year)+1L,
                       fifelse(ever_treated==0L, 0L, NA_integer_))]
w <- merge(w, bin[, .(adm3_pcode, g_edu)], by="adm3_pcode")
w <- w[!is.na(g_edu)][!(g_edu > 0 & g_edu <= 2019)]
w[, treated := as.integer(g_edu >= 2020 & g_edu <= 2025)]
cat("\n3S sample:", nrow(w), "municipalities |", sum(w$treated), "treated |",
    nrow(w)-sum(w$treated), "control\n")
m <- lm(dz ~ treated, data=w); V <- sandwich::vcovHC(m, type="HC2")
b <- coef(m)["treated"]; se <- sqrt(V["treated","treated"])
cat(sprintf("\nEDN 3S DiD (Delta z 2025-2019): %+0.3f SD (SE %.3f), p=%.3f, CI [%+.3f, %+.3f]\n",
    b, se, 2*pnorm(-abs(b/se)), b-1.96*se, b+1.96*se))
cat(sprintf("  mean dz: treated %+.3f | control %+.3f\n", w[treated==1,mean(dz)], w[treated==0,mean(dz)]))

## ---- 5. within-municipality contrast vs the 6P placebo (3S - 6P) -----------
p6 <- as.data.table(readRDS(file.path(DIR_CLEAN,"edn6p_muni_dz.rds")))   # from 08z10 (saved below if absent)
w2 <- merge(w, p6[, .(adm3_pcode, dz6p=dz)], by="adm3_pcode")
w2[, ddz := dz - dz6p]
m2 <- lm(ddz ~ treated, data=w2); V2 <- sandwich::vcovHC(m2, type="HC2")
b2 <- coef(m2)["treated"]; se2 <- sqrt(V2["treated","treated"])
cat(sprintf("\n3S minus 6P within-muni contrast (n=%d): %+0.3f SD (SE %.3f), p=%.3f\n",
    nrow(w2), b2, se2, 2*pnorm(-abs(b2/se2))))

res <- data.table(outcome=c("EDN 3S z (2019->2025 DiD)","3S minus 6P contrast (DDD)"),
                  ATT=c(b,b2), SE=c(se,se2),
                  p=c(2*pnorm(-abs(b/se)), 2*pnorm(-abs(b2/se2))),
                  n_muni=c(nrow(w), nrow(w2)),
                  n_treated=c(sum(w$treated), sum(w2$treated)))
fwrite(res, file.path(DIR_TABLES,"edn3s_did.csv"))
cat("saved -> edn3s_did.csv (report-only; NO paper exhibit)\n")
