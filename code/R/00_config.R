# ============================================================================
# 00_config.R  —  Paths, constants, and shared setup
# DR Paper 3: AU adolescent-care-unit openings & teen births (event study)
#
# Sourced at the top of every numbered script. Defines absolute paths to the
# RAW data (which stays read-only in ../analysis/datasets/) and to the clean
# project tree. No GB-scale duplication: we point at originals.
#
# ALL paths below were verified to exist on 2026-06-18.
# ============================================================================

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(data.table)
})

# ---- Project root (the new R pipeline lives here) --------------------------
# here::here() anchors on the dr-pcua-event-study/ directory (.here sentinel).
PROJ <- here::here()

# ---- DRpaper root (one level up: original data + literature live here) -----
DRPAPER <- normalizePath(file.path(PROJ, ".."))

# ---- Clean project tree ----------------------------------------------------
DIR_RAW     <- file.path(PROJ, "data", "raw")     # pointers / small derived raw
DIR_CLEAN   <- file.path(PROJ, "data", "clean")   # frozen analysis datasets
DIR_TABLES  <- file.path(PROJ, "output", "tables")
DIR_FIGURES <- file.path(PROJ, "output", "figures")
DIR_NOTES   <- file.path(PROJ, "notes")

# ---- RAW source files (read-only; in ../analysis/datasets) -----------------
RAW <- list(
  births_xlsx    = file.path(DRPAPER, "analysis/datasets/births/BDNV 2016-2025 Harvard Act. 23-06-2026.xlsx"),  # updated 2026-06-23 (old "Actualizados" replaced)
  pop_muni_xlsx  = file.path(DRPAPER, "analysis/datasets/population/cuadro_poblacion_municipio_2015 a 2020.xlsx"),
  pop_prov_xlsx  = file.path(DRPAPER, "analysis/datasets/population/cuadro_poblacion_region_provincia_2000 a 2030.xlsx"),
  census_csv     = file.path(DRPAPER, "analysis/datasets/population/BD PERSONAS XCNPV.csv"),
  pcua_xlsx      = file.path(DRPAPER, "analysis/datasets/Visits to PCUAS.xlsx"),
  income_xlsx    = file.path(DRPAPER, "analysis/datasets/income and expenses/Sociodemograficas_e_ingresos.xlsx"),
  # Municipio = ADM3 (158 units); Province = ADM2 (32). Verified 2026-06-18.
  shp_muni       = file.path(DRPAPER, "analysis/figures/Maps/dom_adm_2025_ab_shp/dom_admbnda_adm3_2025_AB.shp"),
  shp_prov       = file.path(DRPAPER, "analysis/figures/Maps/dom_adm_2025_ab_shp/dom_admbnda_adm2_2025_AB.shp"),
  mortality_dir  = file.path(DRPAPER, "analysis/datasets/mortality")
)

# ---- Verify raw inputs exist on source (fail loudly, never silently) -------
.check_raw <- function(raw = RAW) {
  miss <- raw[!file.exists(unlist(raw))]
  if (length(miss)) {
    stop("Missing raw inputs:\n", paste0(" - ", names(miss), ": ", unlist(miss), collapse = "\n"))
  }
  invisible(TRUE)
}

# ---- Analysis constants ----------------------------------------------------
YEARS         <- 2016:2025          # births / panel window
PROJ_YEARS    <- 2016:2025          # population projection target span
CENSUS_YEAR   <- 2022               # hard anchor (census microdata)
TEEN_AGES     <- c("15-19")         # primary treated age group
COMPARE_AGES  <- c("30-34", "25-29", "20-24")  # 30-34 = preferred (provisional)
SEC_AGES      <- c("10-14")         # noisy secondary outcome
MISSING_PCUA_OPENYEAR <- 2016       # 4 units with unknown date -> code as 2016 (per MM)

# Reproducibility
set.seed(20260618)

# Convenience
options(scipen = 999)
message("00_config.R loaded. PROJ = ", PROJ)
