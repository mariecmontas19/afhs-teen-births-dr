# ============================================================================
# 08z4_educ_by_grade.R — enrollment effect BY SECONDARY GRADE (MM 2026-07-29).
# Duflo-style "effect where the mechanism predicts": if the +2% enrollment
# effect (§59.39) is the units retaining at-risk adolescents, it should
# concentrate in the UPPER secondary grades (ages ~15-18), where dropout and
# adolescent-pregnancy risk peak, not in lower grades.
# Each grade's enrollment normalized by the SAME official-age 12-17 population
# (GER convention, §59.48) so grade effects are comparable and additive to the
# +1.48 total GER effect.
# Reads the raw MINERD condicion file for the grade dimension (05p aggregated
# it away). Matching mirrors 05p (exact name within province + LA MATA alias
# + fuzzy<=3), fold-3 to 155.
# Output: output/tables/educ_enroll_by_grade.csv
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

GORD <- c("PRIMERO","SEGUNDO","TERCERO","CUARTO","QUINTO","SEXTO")
pieces <- list()
for(s in excel_sheets(F1)){
  peek <- suppressMessages(as.matrix(read_excel(F1, sheet=s, col_names=FALSE, n_max=6)))
  hr <- which(apply(peek, 1, function(r) any(r=="Municipio", na.rm=TRUE) & any(r=="Nivel", na.rm=TRUE)))[1]
  d <- suppressMessages(as.data.table(read_excel(F1, sheet=s, skip=hr-1)))
  d[, tg := suppressWarnings(as.numeric(`Total general`))]
  d <- d[norm(Nivel)=="SECUNDARIO" & norm(Grado) %in% GORD & !is.na(tg)]
  d <- d[, .(prov_nm=norm(Provincia), muni_nm=norm(Municipio), grado=norm(Grado), mat=tg)]
  d[, school_year := s]; pieces[[s]] <- d
}
ed <- rbindlist(pieces)
ed[, year_end := as.integer(substr(school_year, 6, 9))]
ed[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
ed <- merge(ed, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
un <- ed[is.na(adm3_pcode), .N, by=.(prov_nm, muni_nm)]
for(i in seq_len(nrow(un))){
  cand <- mc[prov_nm==un$prov_nm[i]]; if(!nrow(cand)) next
  dd <- drop(adist(un$muni_nm[i], cand$muni_nm))
  if(min(dd)<=3 && sum(dd==min(dd))==1)
    ed[prov_nm==un$prov_nm[i] & muni_nm==un$muni_nm[i], adm3_pcode := cand$adm3_pcode[which.min(dd)]]
}
ed <- ed[!is.na(adm3_pcode)]; ed[, adm3_pcode := to155(adm3_pcode)]
gp <- ed[, .(mat=sum(mat)), by=.(adm3_pcode, year_end, grado)]

# GER denominator: official secondary ages 12-17 (§59.48), both sexes.
pop <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))[
         age_group %in% c("10-14","15-19")]
pop <- pop[, .(pop = 0.6*sum(pop[age_group=="10-14"]) + 0.6*sum(pop[age_group=="15-19"])),
           by=.(adm3_pcode, year_end=year)]
gp <- merge(gp, pop, by=c("adm3_pcode","year_end"), all.x=TRUE)
gp[, erate := 100*mat/pop]

trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[
         !adm3_pcode %in% c("DOM010905","DOM051703","DOM012510")]
trt[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
                   fifelse(ever_treated==0L, 0L, NA_integer_))]

runG <- function(gr){
  d <- merge(gp[grado==gr], trt[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  d[, g_edu := fifelse(g==0L, 0L, g + 1L)]; d[, id := as.integer(factor(adm3_pcode))]
  a <- att_gt(yname="erate", tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  gg <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  pre <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,-2)]
  lp  <- 2*pnorm(-abs(pre[,sum(att/se^2)/sum(1/se^2)]/sqrt(1/pre[,sum(1/se^2)])))
  base <- d[g>0 & year_end==g-1L, mean(erate, na.rm=TRUE)]
  data.table(grade=gr, ATT=round(gg$overall.att,3), SE=round(gg$overall.se,3),
             p=round(2*pnorm(-abs(gg$overall.att/gg$overall.se)),3),
             base=round(base,2), pct=round(100*gg$overall.att/base,1), lead_p=round(lp,3))
}
G <- rbindlist(lapply(GORD, runG))
G[, grade := factor(grade, levels=GORD)]; setorder(G, grade)
cat("\n============ ENROLLMENT EFFECT BY SECONDARY GRADE (per 100 pop 15-19) ============\n")
cat("DR Secundario = grades 1-6 (ages ~12-18); upper grades = the pregnancy/dropout-risk window.\n\n")
print(G, class=FALSE)
cat("\nsum of grade ATTs:", round(sum(G$ATT),2), "(cf. total enrollment ATT +1.60)\n")
fwrite(G, file.path(PROJ,"output","tables","educ_enroll_by_grade.csv"))
cat("saved -> educ_enroll_by_grade.csv\n")

# ---- GROUPED: Primero-Tercero / Cuarto-Quinto / Sexto solo (MM 2026-07-29) ----
# 1ro-3ro = primer ciclo (ages ~12-15); 4to-5to = ~15-17; 6to = terminal grade
# (completing secondary, the margin most tied to the dropout/pregnancy decision).
grp_map <- c(PRIMERO="1-3 (Primero-Tercero)", SEGUNDO="1-3 (Primero-Tercero)",
             TERCERO="1-3 (Primero-Tercero)", CUARTO="4-5 (Cuarto-Quinto)",
             QUINTO="4-5 (Cuarto-Quinto)",    SEXTO="6 (Sexto, terminal)")
gpg <- gp[, grp := grp_map[grado]][
          , .(mat=sum(mat), pop=pop[1]), by=.(adm3_pcode, year_end, grp)]
gpg[, erate := 100*mat/pop]
runGG <- function(gg_){
  d <- merge(gpg[grp==gg_], trt[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  d[, g_edu := fifelse(g==0L, 0L, g + 1L)]; d[, id := as.integer(factor(adm3_pcode))]
  a <- att_gt(yname="erate", tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  ag <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  pre <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,-2)]
  lp  <- 2*pnorm(-abs(pre[,sum(att/se^2)/sum(1/se^2)]/sqrt(1/pre[,sum(1/se^2)])))
  base <- d[g>0 & year_end==g-1L, mean(erate, na.rm=TRUE)]
  data.table(grade_group=gg_, ATT=round(ag$overall.att,3), SE=round(ag$overall.se,3),
             p=round(2*pnorm(-abs(ag$overall.att/ag$overall.se)),3),
             base=round(base,2), pct=round(100*ag$overall.att/base,1), lead_p=round(lp,3))
}
GG <- rbindlist(lapply(c("1-3 (Primero-Tercero)","4-5 (Cuarto-Quinto)","6 (Sexto, terminal)"), runGG))
cat("\n============ ENROLLMENT EFFECT BY GRADE GROUP (per 100 pop 15-19) ============\n")
print(GG, class=FALSE)
cat("\nsum of group ATTs:", round(sum(GG$ATT),2), "(cf. total +1.60)\n")
fwrite(GG, file.path(PROJ,"output","tables","educ_enroll_by_gradegroup.csv"))
cat("saved -> educ_enroll_by_gradegroup.csv\n")

# ============================================================================
# APPENDED 2026-08-07 (MM): grade-group effects BY SEX (Table 7 Panel B gains
# Male/Female/Total columns) + muni-year grade-group panel (Table 1 baselines).
# Second extraction pass keeping Sexo — deliberately separate from the block
# above so the existing T outputs stay byte-identical under the original seed
# stream. New att_gt calls are seeded individually (project rule).
# ============================================================================
pieces2 <- list()
for(s in excel_sheets(F1)){
  peek <- suppressMessages(as.matrix(read_excel(F1, sheet=s, col_names=FALSE, n_max=6)))
  hr <- which(apply(peek, 1, function(r) any(r=="Municipio", na.rm=TRUE) & any(r=="Nivel", na.rm=TRUE)))[1]
  d <- suppressMessages(as.data.table(read_excel(F1, sheet=s, skip=hr-1)))
  d[, tg := suppressWarnings(as.numeric(`Total general`))]
  d <- d[norm(Nivel)=="SECUNDARIO" & norm(Grado) %in% GORD & !is.na(tg)]
  d <- d[, .(prov_nm=norm(Provincia), muni_nm=norm(Municipio), grado=norm(Grado),
             sexo=norm(Sexo), mat=tg)]
  d[, school_year := s]; pieces2[[s]] <- d
}
eds <- rbindlist(pieces2)
eds[, year_end := as.integer(substr(school_year, 6, 9))]
eds[, sexo := fcase(sexo %chin% c("MASCULINO","M"), "M", sexo %chin% c("FEMENINO","F"), "F", default=NA_character_)]
stopifnot(!anyNA(eds$sexo))
eds[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
eds <- merge(eds, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
un2 <- eds[is.na(adm3_pcode), .N, by=.(prov_nm, muni_nm)]
for(i in seq_len(nrow(un2))){
  cand <- mc[prov_nm==un2$prov_nm[i]]; if(!nrow(cand)) next
  dd <- drop(adist(un2$muni_nm[i], cand$muni_nm))
  if(min(dd)<=3 && sum(dd==min(dd))==1)
    eds[prov_nm==un2$prov_nm[i] & muni_nm==un2$muni_nm[i], adm3_pcode := cand$adm3_pcode[which.min(dd)]]
}
eds <- eds[!is.na(adm3_pcode)]; eds[, adm3_pcode := to155(adm3_pcode)]
eds[, grp := grp_map[grado]]
gs <- eds[, .(mat=sum(mat)), by=.(adm3_pcode, year_end, grp, sexo)]
# consistency: by-sex total must reproduce the total-extraction counts
chk <- merge(gs[, .(mFS=sum(mat)), by=.(adm3_pcode, year_end)],
             gp[, .(mT=sum(mat)),  by=.(adm3_pcode, year_end)], by=c("adm3_pcode","year_end"))
stopifnot(chk[abs(mFS-mT)>0.5, .N] == 0)

# by-sex GER denominator: 0.6 x pop(10-14) + 0.6 x pop(15-19), per sex (§59.48)
popS <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))[
          age_group %in% c("10-14","15-19")]
popS <- popS[, .(pop = 0.6*sum(pop[age_group=="10-14"]) + 0.6*sum(pop[age_group=="15-19"])),
             by=.(adm3_pcode, year_end=year, sexo=sex)]
gs <- merge(gs, popS, by=c("adm3_pcode","year_end","sexo"), all.x=TRUE)
gs[, erate := 100*mat/pop]
# total (T) rows from the sex-summed counts over the both-sex denominator
gT <- gs[, .(sexo="T", mat=sum(mat), pop=sum(pop)), by=.(adm3_pcode, year_end, grp)]
gT[, erate := 100*mat/pop]
gpanel <- rbind(gs, gT)
fwrite(gpanel, file.path(PROJ,"output","tables","educ_gradegroup_muni_panel.csv"))
cat("saved -> educ_gradegroup_muni_panel.csv (", nrow(gpanel), "rows )\n")

runGGS <- function(gg_, sx){
  d <- merge(gpanel[grp==gg_ & sexo==sx], trt[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  d[, g_edu := fifelse(g==0L, 0L, g + 1L)]; d[, id := as.integer(factor(adm3_pcode))]
  set.seed(20260807)                       # per-call seed (project rule, new calls only)
  a <- att_gt(yname="erate", tname="year_end", idname="id", gname="g_edu", xformla=~1, data=d,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  ag <- aggte(a, type="group", na.rm=TRUE); dy <- aggte(a, type="dynamic", na.rm=TRUE)
  pre <- data.table(e=dy$egt, att=dy$att.egt, se=dy$se.egt)[e %between% c(-5,-2)]
  lp  <- 2*pnorm(-abs(pre[,sum(att/se^2)/sum(1/se^2)]/sqrt(1/pre[,sum(1/se^2)])))
  base <- d[g>0 & year_end==g-1L, mean(erate, na.rm=TRUE)]
  data.table(grade_group=gg_, sexo=sx, ATT=round(ag$overall.att,3), SE=round(ag$overall.se,3),
             p=round(2*pnorm(-abs(ag$overall.att/ag$overall.se)),3),
             base=round(base,2), pct=round(100*ag$overall.att/base,1), lead_p=round(lp,3))
}
grid <- CJ(gg=c("1-3 (Primero-Tercero)","4-5 (Cuarto-Quinto)","6 (Sexto, terminal)"),
           sx=c("M","F","T"), sorted=FALSE)
GGS <- rbindlist(Map(runGGS, grid$gg, grid$sx))
cat("\n============ GRADE-GROUP x SEX (GER per 100 official-age pop, per-sex denominators) ============\n")
print(GGS, class=FALSE)
# T columns must agree with the published totals (same data, freshly seeded draws):
cmpT <- merge(GGS[sexo=="T", .(grade_group, ATT_new=ATT)], GG[, .(grade_group, ATT_old=ATT)], by="grade_group")
cat("\nT-vs-published check (point estimates identical by construction):\n"); print(cmpT, class=FALSE)
stopifnot(cmpT[abs(ATT_new-ATT_old)>1e-6, .N] == 0)
fwrite(GGS, file.path(PROJ,"output","tables","educ_enroll_by_gradegroup_bysex.csv"))
cat("saved -> educ_enroll_by_gradegroup_bysex.csv\n")
