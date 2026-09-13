# ============================================================================
# 05_covariates.R — Municipio BASELINE contextual covariates from the 2022 census
# (time-invariant, for CS conditional-parallel-trends). Codings verified vs the
# official XCNPV codebooks (notes/methodology_decisions §5/§6):
#   education P43 (1 Preprimaria..6 Doctorado; secondary+ = P43>=3)
#   civil     P63 (in union = {5 Casada/o, 6 Unida/o}; 9 No declarado -> NA)
#   ZONA (1 Urbano / 2 Rural) ; assets P15A..P15R (1 Sí / 2 No / 9 sin info)
# Household-level vars use P25_ORDEN==1 (one row per household). Person-level
# vars use all women in the age band. Mapped to adm3_pcode via census_code_to_adm3.
# Output: data/clean/census_covariates_muni.rds (158).  [ENGIH SES + insurance
# carry-back handled separately in 05b/06.]
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table)})

f <- file.path(dirname(RAW$census_csv), "BD_FINAL_VIVIENDA_HOGAR_PERSONA_XCNPV_PUB.csv")
stopifnot(file.exists(f))
assetcols <- paste0("P15", LETTERS[1:18])                       # P15A..P15R
dwellcols <- c("P01","P03","P04","P05","P06","P10","P11","P13","P14","P17","P18","P19")
sel <- c("PROVINCIA","MUNICIPIO","ZONA","P25_ORDEN","P26_SEXO","P27_EDAD","P43","P63",
         assetcols, dwellcols)
cat("reading combined census file (selected cols)...\n")
d <- fread(f, select=sel, showProgress=FALSE)
cat("rows:", format(nrow(d), big.mark=","), "\n")

# Household id + size (needed for the crowding component): the census has no
# household id column — households are contiguous person blocks in FILE ORDER
# with P25_ORDEN restarting at 1 (verified 100% sequential, §59.30). Must be
# computed BEFORE the adm3 merge (merge sorts and destroys row order).
d[, hid := cumsum(P25_ORDEN == 1L)]
d[, hh_size := .N, by=hid]

# verify codings before use (Rule: don't assume)
cat("P26_SEXO values:", paste(sort(unique(d$P26_SEXO)), collapse=","),
    "| ZONA:", paste(sort(unique(d$ZONA)), collapse=","),
    "| P43 range:", paste(range(d$P43, na.rm=TRUE), collapse="-"),
    "| P63 vals:", paste(sort(unique(d$P63)), collapse=","), "\n")
FEMALE <- 2L   # 1=Hombre, 2=Mujer (standard ONE coding; confirm in print above)

# ---- map to adm3_pcode via ONE codes ----
m <- as.data.table(readRDS(file.path(DIR_CLEAN, "census_code_to_adm3.rds")))
d <- merge(d, m, by.x=c("PROVINCIA","MUNICIPIO"), by.y=c("prov_code","muni_code"), all.x=TRUE)
cat("rows mapped to adm3_pcode:", d[!is.na(adm3_pcode), .N], "/", nrow(d),
    " (unmapped:", d[is.na(adm3_pcode), .N], ")\n")
d <- d[!is.na(adm3_pcode)]                                      # drop the 999 age-sentinel/unmapped

# ---- household-level (one row per HH): wealth PCA, % internet, % urban ----
# WEALTH INDEX v2 (MM 2026-07-17, §59.30): 29 components = 16 durables
# (P15A estufa + P15Q motor/pasola dropped: MSA .76/.62) + 13 dwelling/
# service components (Galarraga-Ecuador style). KMO .9248, all item MSAs
# >= .85; muni index corr -.906 with SIUBEN ICV (was -.834 with 18 durables).
# Codings verified vs Libro_de_codigos XCNPV + tabulation (§59.30):
#   P01 tipo (2=Apartamento; 5/6/8=cuarteria/barracon/local no construido)
#   P03 paredes (1=Block/concreto) P04 techo (1=Concreto)
#   P05 piso (1 Mosaico/3 Granito/4 Marmol/5 Ceramica) P06 cocina (1=Si dentro)
#   P10 agua uso general (1=acueducto dentro; NOT P09 drinking = botellones)
#   P11 sanitario (1=Inodoro) P13 conectado (1=Red publica; asked only if P11)
#   P14 basura (1/2=recogida) P16 tenencia DROPPED (MSA .67, like Ecuador .71)
#   P17 dormitorios (99=sentinel) P18 combustible (1 Gas/4 Elec) P19 luz (1=red)
hh <- d[P25_ORDEN==1]
for (c in assetcols) hh[, (c) := fifelse(get(c)==1L, 1, fifelse(get(c)==2L, 0, NA_real_))]
na9 <- function(x) fifelse(x==9L, NA_integer_, x)
hh[, `:=`(
  apartment      = as.numeric(P01==2L),
  marginal_dwell = as.numeric(P01 %in% c(5L,6L,8L)),
  wall_block     = as.numeric(na9(P03)==1L),
  roof_concrete  = as.numeric(na9(P04)==1L),
  floor_quality  = as.numeric(P05 %in% c(1L,3L,4L,5L)),
  kitchen_inside = as.numeric(P06==1L),
  water_piped_in = as.numeric(P10==1L),
  toilet_flush   = as.numeric(na9(P11)==1L),
  sewer          = fifelse(is.na(na9(P11)), NA_real_,
                           as.numeric(na9(P11)==1L & !is.na(P13) & P13==1L)),
  garbage_coll   = as.numeric(P14 %in% c(1L,2L)),
  cook_gas_elec  = as.numeric(na9(P18) %in% c(1L,4L)),
  light_grid     = as.numeric(na9(P19)==1L),
  crowded        = fifelse(P17 >= 90L, NA_real_,
                           as.numeric(fifelse(P17==0L, TRUE, hh_size/P17 > 2)))
)]
dur16   <- setdiff(assetcols, c("P15Q","P15A"))
candnew <- c("crowded","apartment","marginal_dwell","wall_block","roof_concrete",
             "floor_quality","kitchen_inside","water_piped_in","toilet_flush","sewer",
             "garbage_coll","cook_gas_elec","light_grid")
pcacols <- c(dur16, candnew)                                    # 29 components
A <- as.matrix(hh[, ..pcacols]); cc <- complete.cases(A)
pc <- prcomp(A[cc, ], center=TRUE, scale.=TRUE)                 # wealth index = PC1
sc <- rep(NA_real_, nrow(hh)); sc[cc] <- pc$x[, 1]
if (cor(sc[cc], rowSums(A[cc, dur16])) < 0) sc <- -sc          # orient: higher = wealthier
hh[, wealth := as.numeric(scale(sc))]
kmo <- psych::KMO(cor(A[cc, ]))
cat("wealth PCA v2 (29 items): PC1 explains", round(100*summary(pc)$importance[2,1],1),
    "% | KMO", round(kmo$MSA,4), "| min item MSA", round(min(kmo$MSAi),3),
    "| complete HH:", sum(cc), "\n")

hh_m <- hh[, .(wealth_index   = mean(wealth, na.rm=TRUE),
               pct_internet   = mean(P15M==1L, na.rm=TRUE),
               pct_urban      = mean(ZONA==1L, na.rm=TRUE),
               n_households   = .N), by=adm3_pcode]

# ---- person-level: % women 20-49 secondary+, % women 15-49 in union ----
edu_m <- d[P26_SEXO==FEMALE & P27_EDAD>=20 & P27_EDAD<=49,
           .(pct_educ_secplus = mean(!is.na(P43) & P43>=3),                    # >=secondary (P43>=3)
             pct_educ_primplus = mean(!is.na(P43) & P43>=2),                   # >=primary (P43>=2: Primaria+)
             n_w2049=.N), by=adm3_pcode]
un_m  <- d[P26_SEXO==FEMALE & P27_EDAD>=15 & P27_EDAD<=49 & P63 %in% 1:7,
           .(pct_in_union = mean(P63 %in% c(5L,6L)), n_w1549=.N), by=adm3_pcode]
# child-woman ratio 2022 = DESCRIPTIVE ONLY (children 0-4 born 2018-22 OVERLAP the rollout -> not a clean
# control, would be partly post-treatment). children 0-4 per 1,000 women 15-49 (a fertility PROXY, NOT a rate).
cw22 <- d[, .(ch04 = sum(P27_EDAD<=4, na.rm=TRUE),
              w1549 = sum(P26_SEXO==FEMALE & P27_EDAD>=15 & P27_EDAD<=49, na.rm=TRUE)), by=adm3_pcode]
cw22[, cwr_2022_desc := round(1000*ch04/w1549, 1)]

# child-woman ratio 2010 = CLEAN PRE-TREATMENT BASELINE control (children 0-4 born 2006-10, pre-rollout).
# 2010 census: P27_SEXO 2=Mujer; age = P29_EDAD_ANOS_CUMPLIDOS. Codes map 155/155 to census_code_to_adm3.
f10 <- file.path(dirname(RAW$census_csv), "CENSO2010RD-PERSONAS.csv")
c10 <- fread(f10, select=c("PROVINCIA","MUNICIPIO","P27_SEXO","P29_EDAD_ANOS_CUMPLIDOS"), showProgress=FALSE)
c10 <- merge(c10, m, by.x=c("PROVINCIA","MUNICIPIO"), by.y=c("prov_code","muni_code"), all.x=TRUE)
stopifnot(sum(is.na(c10$adm3_pcode))==0)                          # all 2010 codes map (155)
cw10 <- c10[, .(ch04 = sum(P29_EDAD_ANOS_CUMPLIDOS<=4, na.rm=TRUE),
                w1549 = sum(P27_SEXO==2L & P29_EDAD_ANOS_CUMPLIDOS>=15 & P29_EDAD_ANOS_CUMPLIDOS<=49, na.rm=TRUE)),
            by=adm3_pcode]
cw10[, cwr_2010_baseline := round(1000*ch04/w1549, 1)]
# 3 created-2013 municipios absent in 2010 -> inherit PARENT's 2010 value (same territory in 2010; fold from 02d)
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
addc <- data.table(adm3_pcode=names(fold),
                   cwr_2010_baseline=cw10$cwr_2010_baseline[match(fold, cw10$adm3_pcode)])
cw10b <- rbind(cw10[, .(adm3_pcode, cwr_2010_baseline)], addc)
cat("2010 baseline CWR: municipios =", nrow(cw10), "(155) + 3 parent-filled =", nrow(cw10b), "\n")

# ---- assemble 158-municipio covariate table ----
xw <- as.data.table(readRDS(file.path(DIR_CLEAN, "muni_crosswalk.rds")))[, .(adm3_pcode, adm3_name, prov_norm)]
cov <- Reduce(function(a,b) merge(a,b,by="adm3_pcode",all.x=TRUE),
              list(xw, hh_m, edu_m[,.(adm3_pcode,pct_educ_secplus,pct_educ_primplus,n_w2049)],
                   un_m[,.(adm3_pcode,pct_in_union,n_w1549)],
                   cw22[,.(adm3_pcode,cwr_2022_desc)], cw10b[,.(adm3_pcode,cwr_2010_baseline)]))
stopifnot(nrow(cov)==158)                                       # Rule 7
saveRDS(cov, file.path(DIR_CLEAN, "census_covariates_muni.rds"))

# ---- validation ----
cat("\n==================== 05 COVARIATES VALIDATION ====================\n")
cat("municipios:", nrow(cov), " | any NA in key covars:",
    cov[, sum(is.na(wealth_index)|is.na(pct_educ_secplus)|is.na(pct_in_union)|is.na(pct_urban))], "\n")
cat("\nsummary of municipio covariates:\n")
print(summary(as.data.frame(cov[, .(pct_educ_secplus, pct_in_union, pct_urban, pct_internet,
                                     wealth_index, cwr_2010_baseline, cwr_2022_desc)])))
cat("CWR check — 2010 baseline (clean control) vs 2022 (descriptive only), correlation:",
    round(cov[, cor(cwr_2010_baseline, cwr_2022_desc)],3), "\n")
cat("\nnational check (person-weighted, women 20-49 sec+):",
    round(100*d[P26_SEXO==FEMALE & P27_EDAD>=20 & P27_EDAD<=49, mean(!is.na(P43)&P43>=3)],1), "%\n")
cat("saved -> data/clean/census_covariates_muni.rds\n")
