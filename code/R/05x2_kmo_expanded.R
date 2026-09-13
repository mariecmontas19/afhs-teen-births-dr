# ============================================================================
# 05x2_kmo_expanded.R — Expanded wealth-index trial (MM request 2026-07-17):
# drop P15Q (motorbike), add Ecuador-style dwelling/service/tenure components,
# and compare KMO / PC1 / external validity across candidate index specs.
# Codings verified against Libro_de_códigos XCNPV (session 2026-07-17):
#   P01 tipo vivienda (2=Apartamento; 5/6/8=cuartería/barracón/local no constr.)
#   P03 paredes (1=Block o concreto; 9=SI)   P04 techo (1=Concreto; 9=SI)
#   P05 piso (1 Mosaico/3 Granito/4 Mármol/5 Cerámica; 7=Tierra)
#   P06 cocina (1=Sí dentro)                 P09 agua beber (1=acueducto dentro)
#   P11 sanitario (1=Inodoro; 9=SI)          P13 conectado (1=Red pública) [asked if P11 has service]
#   P14 basura (1=ayuntamiento, 2=empresa privada)
#   P16 tenencia (1/2=Propia; 9=SI)          P17 dormitorios (count)
#   P18 combustible (1=Gas propano, 4=Electricidad; 9=SI)
#   P19 alumbrado (1=tendido público; 9=SI)
# Specs: S0=current 18 | S1=17 (no moto) | S2=17+13 candidates | S3=pruned.
# Output: output/tables/kmo_wealth_specs.csv + kmo_wealth_expanded_items.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(psych)})

f <- file.path(dirname(RAW$census_csv), "BD_FINAL_VIVIENDA_HOGAR_PERSONA_XCNPV_PUB.csv")
assetcols <- paste0("P15", LETTERS[1:18])
newvars   <- c("PHOGAR","P01","P03","P04","P05","P06","P09","P10","P11","P13","P14","P16","P17","P18","P19")
# NB (found 2026-07-17): P09 drinking water is botellones-dominated (3.14M of
# 3.74M households) -> useless as a piped-water wealth item (MSA .60). The
# Ecuador "water piped into home" analog is P10 (general-use water; 1 =
# acueducto dentro de la vivienda). P17 uses 99 as a sentinel -> NA.

cat("reading census...\n")
d <- fread(f, select=c("PROVINCIA","MUNICIPIO","P25_ORDEN", assetcols, newvars), showProgress=FALSE)

# Household id: PHOGAR is the household NUMBER within the dwelling (1..15), not
# an id (diagnosed 2026-07-17). Households are contiguous person blocks in file
# order with P25_ORDEN restarting at 1 -> hid = cumsum(orden==1), computed
# BEFORE any row filtering. Verified below: orden must be sequential 1..N
# within every block.
d[, hid := cumsum(P25_ORDEN == 1L)]
chk <- d[, .(seqok = all(P25_ORDEN == seq_len(.N))), by=hid]
cat("household blocks:", format(nrow(chk), big.mark=","),
    "| sequential-order check:", round(100*mean(chk$seqok), 3), "% pass\n")
stopifnot(mean(chk$seqok) > 0.999)
d[, hh_size := .N, by=hid]

m <- as.data.table(readRDS(file.path(DIR_CLEAN, "census_code_to_adm3.rds")))
d <- merge(d, m, by.x=c("PROVINCIA","MUNICIPIO"), by.y=c("prov_code","muni_code"),
           all.x=TRUE, sort=FALSE)
d <- d[!is.na(adm3_pcode)]
hh <- d[P25_ORDEN == 1L]
cat("households:", format(nrow(hh), big.mark=","), "| mean size:", round(mean(hh$hh_size),2), "\n")

# --- verify codings by tabulation before building dummies (rule 1) ---
for (v in c("P01","P03","P04","P05","P06","P09","P11","P13","P14","P16","P18","P19"))
  cat(v, ":", paste(capture.output(print(table(hh[[v]], useNA="ifany")))[2:3], collapse=" / "), "\n")
cat("P17 (bedrooms) range:", paste(range(hh$P17, na.rm=TRUE), collapse="-"),
    "| zeros:", hh[P17==0, .N], "\n\n")

# --- durables (same recode as 05) ---
for (c in assetcols) hh[, (c) := fifelse(get(c)==1L, 1, fifelse(get(c)==2L, 0, NA_real_))]

# --- candidate components (9/blank = NA; sewer coded over all households) ---
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
  owns_home      = as.numeric(na9(P16) %in% c(1L,2L)),
  cook_gas_elec  = as.numeric(na9(P18) %in% c(1L,4L)),
  light_grid     = as.numeric(na9(P19)==1L),
  crowded        = fifelse(P17 >= 90L, NA_real_,
                           as.numeric(fifelse(P17==0L, TRUE, hh_size/P17 > 2)))
)]
cand <- c("crowded","apartment","marginal_dwell","wall_block","roof_concrete",
          "floor_quality","kitchen_inside","water_piped_in","toilet_flush","sewer",
          "garbage_coll","owns_home","cook_gas_elec","light_grid")

run_spec <- function(cols, name) {
  X <- as.matrix(hh[, ..cols]); cc <- complete.cases(X); X <- X[cc, ]
  R <- cor(X); k <- psych::KMO(R)
  pc <- prcomp(X, center=TRUE, scale.=TRUE)
  s <- pc$x[,1]
  # orient: positive correlation with durables count (higher = wealthier)
  dur <- intersect(cols, assetcols)
  if (cor(s, rowSums(X[, dur, drop=FALSE])) < 0) { s <- -s; pc$rotation[,1] <- -pc$rotation[,1] }
  # municipality-level index (mean of standardized household scores)
  mi <- data.table(adm3_pcode=hh$adm3_pcode[cc],
                   w=as.numeric(scale(s)))[, .(wi = mean(w)), by=adm3_pcode]
  list(name=name, n_items=length(cols), n_hh=nrow(X), kmo=k$MSA, msai=k$MSAi,
       pc1=summary(pc)$importance[2,1], load=pc$rotation[,1], cols=cols, muni=mi)
}

dur17 <- setdiff(assetcols, "P15Q")
S0 <- run_spec(assetcols, "S0 current 18 durables")
S1 <- run_spec(dur17, "S1 17 durables (no moto)")
S2 <- run_spec(c(dur17, cand), "S2 17 + 14 candidates")

# S3: prune — iteratively drop lowest-MSA item until all items >= 0.80
cols3 <- c(dur17, cand)
repeat {
  s <- run_spec(cols3, "S3")
  if (min(s$msai) >= 0.80) break
  worst <- names(which.min(s$msai))
  cat("  prune:", worst, "MSA", round(min(s$msai),3), "\n")
  cols3 <- setdiff(cols3, worst)
}
S3 <- run_spec(cols3, sprintf("S3 pruned (%d items)", length(cols3)))

# --- external validation vs ICV ---
icv <- as.data.table(readRDS(file.path(DIR_CLEAN, "icv_poverty_muni.rds")))
SL <- list(S0=S0, S1=S1, S2=S2, S3=S3)
M <- Reduce(function(a,b) merge(a,b,by="adm3_pcode"),
            lapply(names(SL), function(n) setnames(copy(SL[[n]]$muni), "wi", paste0("wi_",n))))
M <- merge(M, icv[, .(adm3_pcode, pct_poor_icv)], by="adm3_pcode")

specs <- rbindlist(lapply(names(SL), function(n) { s <- SL[[n]]; data.table(
  spec=s$name, items=s$n_items, hh=s$n_hh, KMO=round(s$kmo,4),
  min_item_MSA=round(min(s$msai),3), PC1_var=round(100*s$pc1,1),
  cor_with_current=round(cor(M[[paste0("wi_",n)]], M$wi_S0), 4),
  cor_ICV=round(cor(M[[paste0("wi_",n)]], M$pct_poor_icv), 3))}))
cat("\n================= SPEC COMPARISON =================\n")
print(specs, class=FALSE)

items3 <- data.table(item=S3$cols, msa=round(S3$msai[S3$cols],3),
                     pc1_loading=round(S3$load[S3$cols],3),
                     share=round(colMeans(as.matrix(hh[, ..cols3]), na.rm=TRUE),3))
setorder(items3, -msa)
cat("\n=========== S3 (pruned) ITEM TABLE ===========\n")
print(items3, nrows=40, class=FALSE)
its2 <- sort(round(S2$msai,3)); cat("\nS2 lowest-MSA items:", paste(names(head(its2,5)), head(its2,5), collapse=" | "), "\n")

fwrite(specs, file.path(DIR_TABLES, "kmo_wealth_specs.csv"))
fwrite(items3, file.path(DIR_TABLES, "kmo_wealth_expanded_items.csv"))
cat("saved -> kmo_wealth_specs.csv + kmo_wealth_expanded_items.csv\n")
