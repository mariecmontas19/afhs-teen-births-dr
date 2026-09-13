# ============================================================================
# 05x_kmo_wealth.R — KMO sampling-adequacy diagnostic for the wealth-index PCA
# (MM request 2026-07-17). Replicates the EXACT asset matrix of 05_covariates.R
# (household rows P25_ORDEN==1, mapped-to-adm3 only, assets P15A..P15R recoded
# 1/0/NA, complete cases), then reports:
#   - overall KMO (psych::KMO on the correlation matrix) + per-item MSA
#   - PC1 loadings, item means, PC variance shares
# Output: output/tables/kmo_wealth.csv + console report.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(psych)})

f <- file.path(dirname(RAW$census_csv), "BD_FINAL_VIVIENDA_HOGAR_PERSONA_XCNPV_PUB.csv")
stopifnot(file.exists(f))
assetcols <- paste0("P15", LETTERS[1:18])

labels <- c(
  P15A="Estufa (stove)",                    P15B="Nevera (refrigerator)",
  P15C="Lavadora de ropa (washing machine)",P15D="Televisor (TV)",
  P15E="Radio / equipo de musica",          P15F="Cisterna (cistern)",
  P15G="Tinaco (rooftop water tank)",       P15H="Computadora de escritorio (desktop)",
  P15I="Computadora portatil (laptop)",     P15J="Tableta (tablet)",
  P15K="Celular (cell phone)",              P15L="Telefono fijo (landline)",
  P15M="Internet",                          P15N="Inversor (power inverter)",
  P15O="Planta electrica (generator)",      P15P="Aire acondicionado (AC)",
  P15Q="Motor o pasola (motorbike)",        P15R="Automovil de uso privado (car)")

cat("reading census (asset cols only)...\n")
d <- fread(f, select=c("PROVINCIA","MUNICIPIO","P25_ORDEN", assetcols), showProgress=FALSE)
cat("rows:", format(nrow(d), big.mark=","), "\n")

# same mapped-municipality filter as 05_covariates.R
m <- as.data.table(readRDS(file.path(DIR_CLEAN, "census_code_to_adm3.rds")))
d <- merge(d, m, by.x=c("PROVINCIA","MUNICIPIO"), by.y=c("prov_code","muni_code"), all.x=TRUE)
d <- d[!is.na(adm3_pcode)]

hh <- d[P25_ORDEN==1]
for (c in assetcols) hh[, (c) := fifelse(get(c)==1L, 1, fifelse(get(c)==2L, 0, NA_real_))]
A <- as.matrix(hh[, ..assetcols]); cc <- complete.cases(A)
A <- A[cc, ]
cat("households (complete cases):", format(nrow(A), big.mark=","), "\n")

# correlation matrix -> KMO (psych) ------------------------------------------
R <- cor(A)
k <- psych::KMO(R)

# PCA (identical call to 05) --------------------------------------------------
pc <- prcomp(A, center=TRUE, scale.=TRUE)
load1 <- pc$rotation[, 1]
if (sum(load1) < 0) load1 <- -load1   # orient as in 05 (higher = wealthier)
varsh <- summary(pc)$importance[2, ]

res <- data.table(item=assetcols,
                  label=labels[assetcols],
                  own_share=round(colMeans(A), 3),
                  msa=round(k$MSAi[assetcols], 3),
                  pc1_loading=round(load1[assetcols], 3))
setorder(res, -msa)

cat("\n================ KMO / PCA DIAGNOSTIC — WEALTH INDEX ================\n")
cat("Overall KMO (MSA):", round(k$MSA, 4), "\n")
cat("PC1 variance share:", round(100*varsh[1], 1), "% | PC2:",
    round(100*varsh[2], 1), "% | PC3:", round(100*varsh[3], 1), "%\n")
cat("Eigenvalues > 1 (Kaiser):", sum(pc$sdev^2 > 1),
    "| PC1 eigenvalue:", round(pc$sdev[1]^2, 2), "\n\n")
print(res, nrows=20, class=FALSE)

fwrite(res, file.path(DIR_TABLES, "kmo_wealth.csv"))
cat("\nsaved -> output/tables/kmo_wealth.csv\n")
