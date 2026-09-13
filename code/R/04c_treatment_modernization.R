# ============================================================================
# 04c_treatment_modernization.R — ALTERNATIVE treatment: MODERNIZATION design.
#
# Treatment = arrival of a MODERN comprehensive adolescent unit (new full-service
# SNS opening, OR post-2016 modernization/remodel of a pre-existing basic unit).
# Rationale (Marie, field-verified): the new units are Abinader-administration
# full-service units; the older units were REMODELED/upgraded after 2016 (service
# expansion beyond contraception). Honest research question = effect of a modern
# comprehensive adolescent unit, not merely "any unit ever".
#
# Per-unit modernization year (from notes/pcua_unit_dates_verified.csv, built in
# 04b from web + field + photo evidence):
#   never_modernized  -> NA (old 1990s unit, never upgraded; does NOT switch a muni)
#   verified modern_event_date -> its year
#   else recorded opening year (in-window units w/ unverified-but-recorded date)
#   else IMPUTE_YEAR (4 undated units w/ no found date)  [same 2023 as 04]
# Municipio cohort = earliest modern-unit year among its units.
#   always_treated_mod = cohort < 2016 (only SFM: San Vicente KOICA 2013).
# new_vs_upgrade = type of the cohort-defining (earliest modern) unit, for a
#   heterogeneity split (new access vs service-intensification).
#
# Output: data/clean/treatment_municipio_mod.rds (158 muni) — same key columns as
# treatment_municipio.rds PLUS mod_cohort / always_treated_mod / mod_first_type /
# n_modern_units, so 07m can swap it in. Anti-hallucination: cohorts trace to 04b.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table)})
IMPUTE_YEAR <- 2023L                                    # same as 04 (center of SNS rollout)

u   <- as.data.table(readRDS(file.path(DIR_CLEAN,"pcua_units_mapped.rds")))      # 39 units, mapped
ver <- fread(file.path(PROJ,"notes","pcua_unit_dates_verified.csv"))            # 04b verified dates
trt <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))    # 158 muni (binary design)

# ---- per-unit modernization year ----
v <- ver[, .(unit_id, modern_event_date, modern_event_type, never_modernized)]
u <- merge(u, v, by="unit_id", all.x=TRUE, sort=FALSE)
# 39 units since the Boca Chica dedup (§59.38, 2026-07-17)
stopifnot(nrow(u)==39L, u[is.na(modern_event_type) & is.na(never_modernized), .N] >= 0)  # all matched
u[, mev_year := as.integer(substr(modern_event_date,1,4))]
u[, mod_year := fcase(
      never_modernized==TRUE,        NA_integer_,
      !is.na(mev_year),              mev_year,
      !is.na(year),                  as.integer(year),
      default =                      IMPUTE_YEAR)]
# cohort-defining type (new access vs upgrade/extant), simplified
u[, type_simpl := fcase(
      modern_event_type=="new_opening",                                   "new",
      modern_event_type %in% c("upgrade","upgrade_unconfirmed"),          "upgrade",
      modern_event_type=="old_never_modernized",                          "old_never_mod",
      default =                                                           "new")]  # "unknown" in-window recorded openings = new SNS units

cat("per-unit modernization year (sorted):\n")
print(u[order(mod_year, na.last=TRUE), .(unit_id, hosp=substr(hosp,1,34), adm3_pcode, mod_year, type_simpl, never_modernized)])

# ---- municipio modernization cohort = earliest modern-unit year ----
tm <- u[, {
  ok <- !is.na(mod_year)
  if (!any(ok)) {
    .(mod_cohort=NA_integer_, mod_first_type=NA_character_, n_modern_units=0L, n_units=.N)
  } else {
    yo <- mod_year[ok]; to <- type_simpl[ok]; k <- which.min(yo)
    .(mod_cohort=min(yo), mod_first_type=to[k], n_modern_units=sum(ok), n_units=.N)
  }
}, by=adm3_pcode]

# ---- attach to the 158-muni frame; never-treated + old-only stay untreated ----
m <- merge(trt, tm, by="adm3_pcode", all.x=TRUE)
m[, n_modern_units := fifelse(is.na(n_modern_units), 0L, n_modern_units)]
m[, mod_ever_treated   := as.integer(!is.na(mod_cohort))]
m[, always_treated_mod := as.integer(!is.na(mod_cohort) & mod_cohort < 2016L)]
# old-only municipios (have a unit but NO modern unit) -> ever_mod=0 (act as controls)
m[, old_only := as.integer(ever_treated==1L & mod_ever_treated==0L)]

saveRDS(m, file.path(DIR_CLEAN,"treatment_municipio_mod.rds"))

# ---- validation ----
cat("\n==================== 04c MODERNIZATION TREATMENT ====================\n")
cat("municipios:", nrow(m),
    "| modern-ever-treated:", m[mod_ever_treated==1,.N],
    "| never/old-only controls:", m[mod_ever_treated==0,.N], "\n")
cat("always-treated_mod (modern cohort <2016, dropped from CS):", m[always_treated_mod==1,.N],
    "->", m[always_treated_mod==1, adm3_name], "\n")
cat("IN-WINDOW modern-treated (2016-2025, identifying variation):",
    m[mod_ever_treated==1 & always_treated_mod==0,.N],
    "   [binary design had", trt[ever_treated==1 & always_treated==0,.N], "]\n")
cat("old-only municipios (unit but no modern upgrade -> controls):", m[old_only==1,.N],
    if (m[old_only==1,.N]) paste0(" -> ", paste(m[old_only==1, adm3_name], collapse=", ")) else "", "\n")
cat("\nmodern cohort distribution (in-window treated):\n")
print(m[mod_ever_treated==1 & always_treated_mod==0, .N, by=.(mod_cohort, mod_first_type)][order(mod_cohort)])
cat("\nRECOVERED municipios (binary always-treated -> modernization in-window):\n")
print(m[always_treated==1 & always_treated_mod==0,
        .(adm3_name, prov_norm, binary_first=first_year, mod_cohort, mod_first_type)][order(mod_cohort)])
cat("\nnew-vs-upgrade split among in-window modern-treated:\n")
print(m[mod_ever_treated==1 & always_treated_mod==0, .N, by=mod_first_type])
cat("saved -> data/clean/treatment_municipio_mod.rds\n")
