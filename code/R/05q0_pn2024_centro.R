# ============================================================================
# 05q0_pn2024_centro.R — authoritative 2024 Pruebas Nacionales center table
# from MINERD convocatoria-1 microdata (received 2026-08-12; §59.8x).
#
# WHY: the public datos.gob.do 2024 slice labeled convocatoria 1 carries
# 368,032 "students" (~3.3x the true conv-1 count within matched centers) —
# more than terminal-grade enrollment — the exact defect flagged provisional
# in §59.50. MINERD's student microdata (83,765 rows, conv 1 only) replaces it.
#
# VERIFIED against the public 2024 rows before adoption:
#   - public subject columns = the TEST score (resp_*): corr .89, means 56/57
#     (the final grade fina_* correlates only .70, mean 78) -> aggregate resp_.
#   - modalities here: ACADEMICA + TECNICO PROFESIONAL + TECNICO ARTES; ARTES
#     is EXCLUDED (public years exclude it; naive grepl("TECNICO") would not).
# Output: data/clean/pn2024_centro.rds in the public-file column schema, one
# row per center x modality group, ready to rbind inside 05q_pruebas_build.R.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(readxl); library(stringi)})
norm <- function(x) toupper(stri_trans_general(trimws(as.character(x)),"Latin-ASCII"))

F1 <- file.path(DRPAPER,"analysis/datasets/Pruebas Nacionales/PN2024_convocatoria1_microdata_MINERD.xlsx")
d <- as.data.table(read_excel(F1, sheet="Secundaria", col_types="text"))
stopifnot(nrow(d)==83765L, all(d$CONVOCATORIA=="1"), all(d$PERIODO=="2024"))

d[, modalidad := norm(MODALIDAD)]
d[, grp := fifelse(grepl("ACADEMIC|GENERAL", modalidad), "GEN",
           fifelse(grepl("TECNICO", modalidad) & !grepl("ARTES", modalidad), "TEC", NA_character_))]
cat("modalidad -> grp:\n"); print(d[, .N, by=.(modalidad, grp)])
d <- d[!is.na(grp)]                                   # drops TECNICO ARTES (public years exclude it)

for (v in c("resp_esp","resp_mat","resp_soc","resp_nat")) d[, (v) := suppressWarnings(as.numeric(get(v)))]
d[, code := formatC(as.integer(CENTRO_CODIGO), width=8, flag="0")]
d[, distrito := substr(gsub("\\D","",DISTRITO_CODIGO), 1, 4)]
d[, promovido := trimws(Condicion_Final)=="PROMOVIDO"]
stopifnot(!anyNA(d$promovido), all(nchar(d$code)==8L))

# center x modality-group means of the TEST score (>0 only, matching 05q's
# score>0 rule: absent-per-subject students carry resp 0/NA)
mz <- function(x) if(any(x>0, na.rm=TRUE)) mean(x[x>0], na.rm=TRUE) else NA_real_
p24 <- d[, .(periodo = 2024L, conv = "1",
             centro  = CENTRO_NOMBRE[1],
             esp = mz(resp_esp), mat = mz(resp_mat), soc = mz(resp_soc), nat = mz(resp_nat),
             n_est = .N,
             n_f = sum(SEXO=="F"), n_m = sum(SEXO=="M"),
             n_prom = sum(promovido), n_apl = sum(!promovido)),
         by=.(code, distrito, modalidad, grp)]

cat("\ncenters x grp rows:", nrow(p24), "| centers:", p24[,uniqueN(code)],
    "| students:", format(sum(p24$n_est), big.mark=","),
    "| passers:", format(sum(p24$n_prom), big.mark=","),
    sprintf("(pass rate %.1f%%)", 100*sum(p24$n_prom)/sum(p24$n_est)), "\n")
cat("mean esp:", round(weighted.mean(p24$esp, p24$n_est, na.rm=TRUE),1),
    "| all scores on the new (100-pt) scale:", p24[, all(pmax(esp,mat,soc,nat,na.rm=TRUE)>30, na.rm=TRUE)], "\n")
stopifnot(sum(p24$n_est) + nrow(d[FALSE]) <= 83765L, sum(p24$n_est) > 75000L)

saveRDS(p24, file.path(DIR_CLEAN,"pn2024_centro.rds"))
cat("saved -> data/clean/pn2024_centro.rds\n")
