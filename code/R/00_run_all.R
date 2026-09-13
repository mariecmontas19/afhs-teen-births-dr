# ============================================================================
# 00_run_all.R -- master script: runs the CORE pipeline in order.
# Scripts are numbered by stage (01 crosswalk ... 10 tables); the ~180 other
# numbered scripts build appendix exhibits and robustness checks and are
# documented in README.md. Restricted inputs (Ministry of Public Health birth
# registry) must be present under ../analysis/datasets/ (see README, Data access).
# NOTE: this order transcribes the documented pipeline; a full end-to-end
# re-execution from raw data was last completed on 2026-07-13 (99_validate: 90/90).
# ============================================================================
Sys.setenv(LANG = "en_US.UTF-8", LC_ALL = "en_US.UTF-8")  # accented xlsx names (03, 04)
core <- c(
  "01_crosswalk.R", "01b_pcua_blank_municipio.R",
  "02_population_projection.R", "02b_hamilton_perry.R",
  "02c_projection_A_one.R", "02d_projection_B_intercensal.R",
  "03_clean_births.R",
  "04_treatment.R", "04b_unit_dates_verified.R", "04c_treatment_modernization.R",
  "05_covariates.R", "05b_income_ses.R", "05c_dhs_province.R", "05c2_dhs_contraception.R",
  "05d_health_centers.R", "05e_poverty_measures.R", "05i_facility_access.R",
  "05j_facility_levels.R", "05h2_jee_full.R",
  "06_build_panel.R", "06b_monthly_panel.R",
  "07_eventstudy.R", "07s_triplediff_ddd.R", "07t_headline.R", "07t2_dynamic_panelB.R",
  "08_estimators.R", "08l_het_master.R", "08n_byage.R", "08x_bridge_cs.R",
  "09_robustness.R", "09e_cost_effectiveness.R",
  "10_tables.R",
  "99_validate_pipeline.R"
)
for (s in core) {
  message("\n==== ", s, " ====")
  source(here::here("code", "R", s), echo = FALSE)
}
