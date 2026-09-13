# ============================================================================
# 05r_census2010_check.R — Do the 2022-census covariates reflect persistent
# pre-treatment variation? (MM, 2026-08-04). Two deliverables:
#   (1) muni-level correlations between 2010-census analogues and the 2022
#       covariates (wealth index, % women 20-49 secondary+),
#   (2) the +covs CS headline re-estimated with the 2010-census versions of
#       wealth and education (urban, SNS supply, and SeNaSa unchanged: ZONA
#       is absent from the 2010 public microdata, and the latter two are
#       already pre-treatment measures).
# PREDICTIONS (rule 1b), stated before running:
#   - corr(wealth_2010, wealth_2022) at the muni level: 0.85-0.95
#   - corr(educ_2010,  educ_2022):                      0.85-0.95
#   - +covs S1 estimate with 2010 wealth/educ: close to -5.66 (within ~0.5),
#     same significance. Anything below corr 0.7 or a sign/significance change
#     triggers an instrument check before interpretation.
# 2010 wealth index mirrors the v2 recipe on the items the 2010 census holds:
#   14 durables (H09B-H09O; estufa + motor excluded, mirroring the v2 drops)
#   + 5 dwelling/services (inodoro, garbage collected, piped water inside,
#   gas/electric cooking, grid lighting). No paredes/techo/piso (vivienda
#   file not held) and no crowding (household size not in the hogares file).
# Output: output/tables/census2010_covariate_check.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
TAB  <- file.path(PROJ,"output","tables")
fold <- c("DOM010905","DOM051703","DOM012510")
POPDIR <- file.path(dirname(RAW$census_csv))

# ---- 1. 2010 hogares: wealth index -----------------------------------------
dur <- c("H09B_NEVERA","H09C_LAVADORA","H09D_TELEVISOR","H09E_RADIO","H09F_CISTERNA",
         "H09G_TINACO","H09H_COMPUTADORA","H09I_INTERNET","H09J_INVERSOR","H09K_PLANTA",
         "H09L_TELEFONO_FIJO","H09M_CELULAR","H09N_AIRE","H09O_AUTOMOVIL")
hh <- fread(file.path(POPDIR,"CENSO2010RD-HOGARES.csv"),
            select=c("PROVINCIA","MUNICIPIO",dur,"H12_SANITARIO","H14_BASURA",
                     "H15_PROCEDENCIA_AGUA","H16_COMBUSTIBLE","H17_ALUMBRADO"),
            showProgress=FALSE)
cat("2010 hogares rows:", format(nrow(hh), big.mark=","), "\n")
for (c in dur) hh[, (c) := fifelse(get(c)==1L, 1, fifelse(get(c)==2L, 0, NA_real_))]
hh[, `:=`(
  toilet_flush  = as.numeric(H12_SANITARIO==1L),
  garbage_coll  = as.numeric(H14_BASURA %in% c(1L,2L)),
  water_piped_in= as.numeric(H15_PROCEDENCIA_AGUA==1L),
  cook_gas_elec = as.numeric(H16_COMBUSTIBLE %in% c(1L,4L)),
  light_grid    = as.numeric(H17_ALUMBRADO %in% c(1L,4L))
)]
pcacols <- c(dur,"toilet_flush","garbage_coll","water_piped_in","cook_gas_elec","light_grid")
A <- as.matrix(hh[, ..pcacols]); cc <- complete.cases(A)
pc <- prcomp(A[cc,], center=TRUE, scale.=TRUE)
sc <- rep(NA_real_, nrow(hh)); sc[cc] <- pc$x[,1]
if (cor(sc[cc], rowSums(A[cc, dur])) < 0) sc <- -sc
hh[, wealth10 := as.numeric(scale(sc))]
cat("2010 wealth PCA (19 items): PC1 explains",
    round(100*summary(pc)$importance[2,1],1), "% | complete HH:", sum(cc), "\n")

# ---- map ONE codes -> adm3 (same bridge as 2022; codes are stable) ----------
m <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_code_to_adm3.rds")))
w10 <- merge(hh[, .(wealth_2010=mean(wealth10, na.rm=TRUE), n_hh10=.N),
                 by=.(PROVINCIA,MUNICIPIO)],
             m, by.x=c("PROVINCIA","MUNICIPIO"), by.y=c("prov_code","muni_code"), all.x=TRUE)
cat("2010 munis mapped:", w10[!is.na(adm3_pcode),.N], "/", nrow(w10), "\n")
w10 <- w10[!is.na(adm3_pcode)]

# ---- 2. 2010 personas: % women 20-49 secondary+ ------------------------------
pe <- fread(file.path(POPDIR,"CENSO2010RD-PERSONAS.csv"),
            select=c("PROVINCIA","MUNICIPIO","P27_SEXO","P29_EDAD_ANOS_CUMPLIDOS","P37_NIVEL"),
            showProgress=FALSE)
cat("2010 personas rows:", format(nrow(pe), big.mark=","), "\n")
cat("P37_NIVEL values:", paste(sort(unique(pe$P37_NIVEL)), collapse=","), "\n")
# P37: 1 Preprimaria 2 Primaria 3 Media/secundaria 4 Universitaria (9/NA no declarado)
e10 <- pe[P27_SEXO==2L & P29_EDAD_ANOS_CUMPLIDOS>=20 & P29_EDAD_ANOS_CUMPLIDOS<=49,
          .(educ_2010 = mean(!is.na(P37_NIVEL) & P37_NIVEL %in% 3:4), n_w10=.N),
          by=.(PROVINCIA,MUNICIPIO)]
e10 <- merge(e10, m, by.x=c("PROVINCIA","MUNICIPIO"), by.y=c("prov_code","muni_code"), all.x=TRUE)
e10 <- e10[!is.na(adm3_pcode)]

# ---- 3. correlations with the 2022 covariates on the estimation sample ------
p22 <- unique(as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[,
        .(adm3_pcode, ever_treated, always_treated, wealth_index, pct_educ_secplus)])
d <- Reduce(function(a,b) merge(a,b,by="adm3_pcode"),
            list(p22, w10[,.(adm3_pcode,wealth_2010)], e10[,.(adm3_pcode,educ_2010)]))
est <- d[always_treated==0]
cat("estimation-sample munis with both vintages:", nrow(est), "\n")
r_w <- est[, cor(wealth_2010, wealth_index)]
r_e <- est[, cor(educ_2010,  pct_educ_secplus)]
cat(sprintf("corr wealth 2010 vs 2022: %.3f | corr educ 2010 vs 2022: %.3f\n", r_w, r_e))

# ---- 4. +covs CS with 2010 wealth/educ (07t spec, S1) ------------------------
pw <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[,
        .(adm3_pcode, year, rateA_15_19, wealth_index, pct_urban, pct_educ_secplus)]
hc <- as.data.table(readRDS(file.path(DIR_CLEAN,"health_centers_muni.rds")))[, .(adm3_pcode, sns_per10k)]
pw <- merge(pw, hc, by="adm3_pcode", all.x=TRUE)
sn <- as.data.table(readRDS(file.path(DIR_CLEAN,"dhs_province_baseline.rds")))[, .(adm3_pcode, pct_senasa_2013)]
pw <- merge(pw, sn, by="adm3_pcode", all.x=TRUE)
pw <- merge(pw, d[, .(adm3_pcode, wealth_2010, educ_2010)], by="adm3_pcode", all.x=TRUE)
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year),
           fifelse(ever_treated==0L, 0L, NA_integer_))]
cs_one <- function(xf, seed){
  estd <- merge(pw, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  estd <- estd[complete.cases(estd)]
  estd[, id := as.integer(factor(adm3_pcode))]
  set.seed(seed)                                  # immediately before att_gt (house rule)
  a <- att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g", xformla=xf, data=estd,
              control_group="notyettreated", base_period="universal", est_method="reg",
              bstrap=TRUE, cband=TRUE, biters=2000, clustervars="id", print_details=FALSE)
  g <- aggte(a, type="group", na.rm=TRUE)
  list(att=g$overall.att, se=g$overall.se, n=uniqueN(estd$adm3_pcode))
}
r22 <- cs_one(~ wealth_index + sns_per10k + pct_urban + pct_educ_secplus + pct_senasa_2013, 20260722)
r10 <- cs_one(~ wealth_2010  + sns_per10k + pct_urban + educ_2010        + pct_senasa_2013, 20260804)
p_of <- function(r) 2*pnorm(-abs(r$att/r$se))
cat(sprintf("+covs 2022 vintage: ATT %.2f (SE %.2f, p=%.3f) N=%d\n", r22$att, r22$se, p_of(r22), r22$n))
cat(sprintf("+covs 2010 vintage: ATT %.2f (SE %.2f, p=%.3f) N=%d\n", r10$att, r10$se, p_of(r10), r10$n))

out <- data.table(
  stat = c("corr_wealth_2010_2022","corr_educ_2010_2022",
           "attcovs_2022","se_2022","p_2022","attcovs_2010","se_2010","p_2010","n_munis"),
  value = c(r_w, r_e, r22$att, r22$se, p_of(r22), r10$att, r10$se, p_of(r10), r10$n))
fwrite(out, file.path(TAB,"census2010_covariate_check.csv"))
cat("saved -> census2010_covariate_check.csv\n")
