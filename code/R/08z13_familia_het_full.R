# ============================================================================
# 08z13_familia_het_full.R — FULL heterogeneity SCREEN of the main fertility
# effect by every CuestionarioFamilia 2019 (3S wave) indicator family (§59.89).
# MM 2026-08-12: "show me ... with all the variables ... just want to see if
# there is something worth including to table a06." REPORT-ONLY screen.
#
# 23 municipality-level moderators (family-weighted means; 0=Vacio, '*'=Multi,
# blank -> NA; checkbox items coded 1=Si/2=No in the data). Wave = school year
# 2018-19 = PRE-treatment for every in-window opener (g_edu >= 2020).
# Machinery = exact 08l clone (median split of the 20 treated, shared never
# pool, gap SE = sqrt(se_u^2+se_s^2)); biters=999 for the screen (point
# estimates are biters-invariant; any candidate re-run at 2000 to match A6).
# Holm across the 23 gaps reported (screen multiplicity). No paper change.
# Output: output/tables/familia_het_full.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi); library(did)})
set.seed(20260722)
BITERS <- 999L
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))
fold155 <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold155), unname(fold155[p]), p)

## ---- 1. center -> adm3 (OAI-0790 block, as in 05q/08z10-12) ----------------
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
  cc <- names(d)[vapply(d, hit8, 0) > 0.5][1]; stopifnot(!is.na(cc),!is.na(pv),!is.na(mn))
  x <- d[, .(centro=as.character(get(cc)), prov_nm=norm(get(pv)), muni_nm=norm(get(mn)))]
  x[, code := sub("^\\s*(\\d{7,8})\\s*-.*$", "\\1", centro)]
  x <- x[grepl("^\\d{7,8}$", code)][, code := formatC(as.integer(code), width=8, flag="0")]
  pieces[[s]] <- unique(x[, .(code, prov_nm, muni_nm)])
}
cw <- unique(rbindlist(pieces)); cw[muni_nm=="LA MATA", muni_nm := "VILLA LA MATA"]
cw <- merge(cw, mc, by=c("prov_nm","muni_nm"), all.x=TRUE)
un <- unique(cw[is.na(adm3_pcode), .(prov_nm, muni_nm)])
for(i in seq_len(nrow(un))){ cand<-mc[prov_nm==un$prov_nm[i]]; hit<-cand[agrepl(un$muni_nm[i],muni_nm,max.distance=0.15)]
  if(nrow(hit)==1) cw[prov_nm==un$prov_nm[i]&muni_nm==un$muni_nm[i], adm3_pcode:=hit$adm3_pcode] }
cw <- cw[!is.na(adm3_pcode)]; amb <- cw[, .(n=uniqueN(adm3_pcode)), by=code][n>1]
code2adm <- unique(cw[!code %in% amb$code, .(code, adm3_pcode)])

## ---- 2. read questionnaire; build family-level indicators ------------------
FQ <- file.path(DRPAPER,"analysis/datasets/Evaluacion Diagnostica/CuestionarioFamilia2019.xlsx")
q <- as.data.table(read_excel(FQ, sheet="Familia_EVA3roSec_2019_REVISADA", col_types="text"))
q[, code := formatC(suppressWarnings(as.integer(CENTRO_CODIGO)), width=8, flag="0")]
q <- q[grepl("^\\d{8}$", code)]
nv  <- function(v){ y<-suppressWarnings(as.numeric(q[[v]])); y[y==0]<-NA_real_; y }   # 0=Vacio,'*'->NA
idx <- function(vs){ m<-sapply(vs, nv); m[m==0]<-NA; rowMeans(m, na.rm=TRUE) }         # 1-4 battery mean
me <- nv("p25_educ_madre"); me[me==8]<-NA; pe <- nv("p26_educ_padre"); pe[pe==8]<-NA
ab <- nv("p15_ausencia");   ab[ab==6]<-NA; st <- nv("p17_estudia");    st[st==5]<-NA
fam <- data.table(code=q$code,
  mom_secplus = as.integer(me>=5),          dad_secplus = as.integer(pe>=5),
  mom_fixed   = as.integer(nv("p27_lab_madre")==1), dad_fixed = as.integer(nv("p28_lab_padre")==1),
  inc_20k     = as.integer(nv("p36_ingreso")>=5),   internet  = as.integer(nv("p34f_internet")==1),
  nevera      = as.integer(nv("p33d_nevera")==1),   books     = nv("p37_q_libros"),
  transfer    = as.integer(nv("p29k_ninguno")==2),  hhsize    = nv("p23_personash"),
  creole      = as.integer(nv("p24_idioma")==3),
  expect_uni  = as.integer(nv("p9_nivelesperado")==3), attend_imp = as.integer(nv("p10_asistencia")==3),
  study_2h    = as.integer(st>=3),
  fam_activ   = idx(c("p8a_leemos","p8d_tarea","p8e_biblioteca","p8f_cuentos_historias","p8h_comentamos","p8i_conversamos")),
  parent_part = idx(c("p7a_reuniones","p7b_part_extra","p7c_ayuda_extra","p7d_comites")),
  child_works = as.integer(nv("p30_estu_trabaja")==1), repeated = as.integer(nv("p18_repitencia")>=2),
  preschool   = as.integer(nv("p12_educacion_preescolar")==1), absent = as.integer(ab>=3),
  sch_violence= idx(c("p5a_robos","p5b_peleas_estu","p5c_amenazas_estu","p5d_danos_estu")),
  barrio_viol = idx(c("p6a_drogas","p6c_vandalismo","p6e_peleas","p6g_peleas_armas","p6j_agresiones","p6k_robos")),
  sch_satisf  = idx(c("p1a_aprendizaje","p1b_calidaddocente","p1c_seguimiento","p2c_valores","p2e_psicologia","p3a_limpieza","p3d_desayuno")))
fam <- merge(fam, code2adm, by="code")[, adm3_pcode := to155(adm3_pcode)]
cat("families matched to municipality:", format(nrow(fam),big.mark=","), "\n")
MODS <- setdiff(names(fam), c("code","adm3_pcode"))
mod <- fam[, c(list(n_fam=.N), lapply(.SD, mean, na.rm=TRUE)), by=adm3_pcode, .SDcols=MODS]

## ---- 3. het machinery (08l clone) ------------------------------------------
p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% names(fold155)]
treated <- tr[ever_treated==1L & always_treated==0L, .(adm3_pcode, g=as.integer(first_year))]
never   <- tr[ever_treated==0L, adm3_pcode]
M <- merge(treated, mod, by="adm3_pcode", all.x=TRUE)
cs_sub <- function(pcodes){
  est <- merge(p[, .(adm3_pcode, year, rateA_15_19)],
               rbind(treated[adm3_pcode %in% pcodes], data.table(adm3_pcode=never, g=0L)), by="adm3_pcode")
  est[, id := as.integer(factor(adm3_pcode))]
  a <- suppressWarnings(suppressMessages(att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g",
        xformla=~1, data=est, control_group="notyettreated", base_period="universal", est_method="reg",
        bstrap=TRUE, biters=BITERS, clustervars="id", print_details=FALSE)))
  o <- suppressMessages(aggte(a, type="group", na.rm=TRUE)); list(att=o$overall.att, se=o$overall.se)
}
# under_high = TRUE where a HIGH value marks disadvantage/underserved
UH <- c(transfer=TRUE, hhsize=TRUE, creole=TRUE, child_works=TRUE, repeated=TRUE, absent=TRUE,
        sch_violence=TRUE, barrio_viol=TRUE)
PRETTY <- c(mom_secplus="Mother completed secondary$+$", dad_secplus="Father completed secondary$+$",
  mom_fixed="Mother in fixed employment", dad_fixed="Father in fixed employment",
  inc_20k="Household income $\\geq$ RD\\$20{,}000", internet="Home internet connection",
  nevera="Household has a refrigerator", books="Books at home (1--6)", transfer="Receives a cash transfer",
  hhsize="Household size (persons)", creole="Creole spoken at home",
  expect_uni="Family expects university", attend_imp="Attendance ``very important''",
  study_2h="Studies $\\geq$2 hours/day", fam_activ="Family educational activities (1--4)",
  parent_part="Parental school participation (1--4)", child_works="Adolescent works outside home",
  repeated="Ever repeated a grade", preschool="Attended preschool", absent="Frequently absent",
  sch_violence="In-school violence (1--4)", barrio_viol="Neighborhood violence (1--4)",
  sch_satisf="Satisfaction with school (1--4)")
CAT <- c(mom_secplus="Socioeconomic status", dad_secplus="Socioeconomic status", mom_fixed="Socioeconomic status",
  dad_fixed="Socioeconomic status", inc_20k="Socioeconomic status", internet="Socioeconomic status",
  nevera="Socioeconomic status", books="Socioeconomic status", transfer="Socioeconomic status",
  hhsize="Household structure", creole="Household structure",
  expect_uni="Valuation of education", attend_imp="Valuation of education", study_2h="Valuation of education",
  fam_activ="Valuation of education", parent_part="Valuation of education",
  child_works="Adolescent margins", repeated="Adolescent margins", preschool="Adolescent margins", absent="Adolescent margins",
  sch_violence="Context and environment", barrio_viol="Context and environment", sch_satisf="Context and environment")

res <- rbindlist(lapply(MODS, function(v){
  med <- median(M[[v]], na.rm=TRUE); uh <- isTRUE(UH[v])
  und <- if(uh) M[get(v)>=med, adm3_pcode] else M[get(v)<med, adm3_pcode]
  srv <- setdiff(M$adm3_pcode, und)
  if(length(und)<3 || length(srv)<3) return(NULL)
  ru <- cs_sub(und); rs <- cs_sub(srv); gap <- ru$att - rs$att
  data.table(var=v, category=CAT[v], label=PRETTY[v], median=round(med,3),
             att_hi=round(ru$att,2), se_hi=round(ru$se,2), p_hi=round(2*pnorm(-abs(ru$att/ru$se)),3),
             att_lo=round(rs$att,2), se_lo=round(rs$se,2), p_lo=round(2*pnorm(-abs(rs$att/rs$se)),3),
             gap=round(gap,2), p_diff=round(2*pnorm(-abs(gap/sqrt(ru$se^2+rs$se^2))),3),
             n_hi=length(und), n_lo=length(srv))
}))
res[, holm := round(p.adjust(p_diff, "holm"), 3)]
setorder(res, p_diff)
print(res[, .(category, label, att_hi, att_lo, gap, p_diff, holm)], class=FALSE)
fwrite(res, file.path(DIR_TABLES,"familia_het_full.csv"))
cat("\nmunis with data:", nrow(mod), "| treated covered:", M[!is.na(mom_secplus),.N], "/20",
    "| median families/treated-muni:", round(median(M$n_fam,na.rm=TRUE)), "\n")
cat("saved -> familia_het_full.csv (REPORT-ONLY screen; biters=999)\n")
