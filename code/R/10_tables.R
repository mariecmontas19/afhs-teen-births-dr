# ============================================================================
# 10_tables.R — consolidation wrapper for ALL paper tables. One entry point that
#   (1) re-runs the lightweight CSV->LaTeX formatters (build = "format" below),
#   (2) optionally re-runs the estimation and build scripts (build = "estimate") when
#       FAST=FALSE — a full reproducible rebuild,
#   (3) verifies every table's .tex exists and writes a manifest.
# The heavy scripts (estimation) are the source of truth for their own .tex; this
# wrapper does not duplicate their logic. Output: output/tables/tables_manifest.csv
# Usage:  Rscript code/R/10_tables.R            # fast: re-format + verify
#         FAST=FALSE Rscript code/R/10_tables.R # full rebuild incl. estimation
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")
FAST <- !identical(toupper(Sys.getenv("FAST","TRUE")), "FALSE")
cat(sprintf("10_tables.R | FAST=%s (%s)\n", FAST, if(FAST) "re-format light tables + verify" else "FULL rebuild incl. estimation"))

# manifest: paper label, .tex file, producing script, build type.
# Updated 2026-11-18 to the paper's current tables (8 main, A1-A19, C1-C3), one row per file the
# paper inputs. build = "format": a light formatter that reads saved CSVs and is re-run here;
# build = "estimate": the table is written by its own estimation or build script and is re-run
# here only when FAST=FALSE; build = "standalone": 10z_educ_tables.R reads an object from the global
# environment, so it runs on its own (as 00_run_all.R does) and is only verified here.
man <- rbindlist(list(
  data.table(label="Table 1   Characteristics of treated and comparison municipalities", tex="tab_01_descriptives.tex", script="07a4_table1_descriptives.R", build="estimate"),
  data.table(label="Table 2   Allocation and timing of first openings", tex="tab_02_allocation.tex", script="04g_allocation_table.R", build="estimate"),
  data.table(label="Table 3   Main estimates", tex="tab_03_main_estimates.tex", script="10_table3_estimates.R", build="format"),
  data.table(label="Table 4   Effect by single year of age", tex="tab_04_byage.tex", script="08n_byage.R", build="estimate"),
  data.table(label="Table 5   Births and pregnancy-related events", tex="tab_05_preg_sns.tex", script="10x_bridge_table.R", build="format"),
  data.table(label="Table 6   Secondary-school outcomes", tex="tab_06_educ_spillover.tex", script="10z_educ_tables.R", build="standalone"),
  data.table(label="Table 7   Concurrent policies and shocks", tex="tab_07_policies_shocks.tex", script="10y_shocks_table.R", build="format"),
  data.table(label="Table 8   Benefits and break-even cost thresholds", tex="tab_08_cost_effectiveness.tex", script="09e_cost_effectiveness.R", build="format"),
  data.table(label="Table A1  Estimator choice and inference", tex="tab_a01_estimators.tex", script="10_appendix_tables.R", build="format"),
  data.table(label="Table A2  Effect by year since opening", tex="tab_a02_dynamics.tex", script="10_appendix_tables.R", build="format"),
  data.table(label="Table A3  Lead and lag estimates", tex="tab_a03_leads.tex", script="10_appendix_leads.R", build="estimate"),
  data.table(label="Table A4  Placebo tests and sensitivity bounds", tex="tab_a04_placebos.tex", script="10z5_placebo_table.R", build="format"),
  data.table(label="Table A5  Newborn outcomes", tex="tab_a05_newborn_health.tex", script="08m_newborn_health.R", build="estimate"),
  data.table(label="Table A6  Heterogeneity", tex="tab_a06_het_summary.tex", script="08j_het_summary.R", build="estimate"),
  data.table(label="Table A7  Age at birth vs. age at conception", tex="tab_a07_conception.tex", script="08o8_table_age_robustness.R", build="format"),
  data.table(label="Table A8  Age gradient", tex="tab_a08_age_gradient.tex", script="08p3_table_age_gradient.R", build="format"),
  data.table(label="Table A9  Alternative specifications, samples, comparison groups", tex="tab_a09_sensitivity.tex", script="10z4_sensitivity_table.R", build="format"),
  data.table(label="Table A10 Matched-control estimates", tex="tab_a10_matching.tex", script="10z3_matching_tables.R", build="estimate"),
  data.table(label="Table A11 Match assignments", tex="tab_a11_matching_pairs.tex", script="10z3_matching_tables.R", build="estimate"),
  data.table(label="Table A12 Registration completeness and birth composition", tex="tab_a12_registration.tex", script="10z6_registration_table.R", build="format"),
  data.table(label="Table A13 Response by maternal education", tex="tab_a13_composition.tex", script="10z7_composition_utilization.R", build="format"),
  data.table(label="Table A14 Utilization signals, hospital records", tex="tab_a14_utilization.tex", script="10z7_composition_utilization.R", build="format"),
  data.table(label="Table A15 Wealth-index components", tex="tab_a15_wealth_items.tex", script="10w_wealth_items_table.R", build="format"),
  data.table(label="Table A16 Effect by opening cohort", tex="tab_a16_cohorts.tex", script="08q2_cohort_atts.R", build="estimate"),
  data.table(label="Table A17 First openings and upgrade events (07p writes, 07p2 adds the difference test)", tex="tab_a17_decomposition.tex", script="07p2_newupgrade_difftest.R", build="estimate"),
  data.table(label="Table A18 Alternative outcome specifications, births averted", tex="tab_a18_functional_form.tex", script="09d_packham_bridge.R", build="estimate"),
  data.table(label="Table A19 Population denominator", tex="tab_a19_denominator.tex", script="10x_denominator_panel.R", build="format"),
  data.table(label="Table C1  Projected population, Santo Domingo province", tex="tab_a20_projection_sd.tex", script="02f_projection_appendix.R", build="estimate"),
  data.table(label="Table C2  Treatment-timing provenance, 39 units", tex="tab_a21_unit_provenance.tex", script="04h_provenance_table.R", build="estimate"),
  data.table(label="Table C3  Registered births as a share of hospital deliveries", tex="tab_c03_completeness.tex", script="09q_hospital_alignment.R", build="estimate")
))

# which scripts to (re-)run: always the light formatters; estimation only if !FAST
to_run <- unique(man[build=="format" | (!FAST & build=="estimate"), script])
for(s in to_run){
  cat(sprintf("  running %-26s ... ", s))
  ok <- tryCatch({ invisible(capture.output(source(file.path(PROJ,"code","R",s), local=new.env()))); "ok" },
                 error=function(e) paste0("ERROR: ", conditionMessage(e)))
  cat(ok, "\n")
}

# verify every .tex + manifest
man[, path := file.path(TAB, tex)]
man[, exists := file.exists(path)]
man[, n_lines := sapply(path, function(p) if(file.exists(p)) length(readLines(p, warn=FALSE)) else NA_integer_)]
man[, modified := ifelse(exists, format(file.info(path)$mtime, "%Y-%m-%d %H:%M"), NA_character_)]
fwrite(man[, .(label, tex, script, build, exists, n_lines, modified)], file.path(TAB,"tables_manifest.csv"))

cat("\n=== paper table manifest ===\n")
print(man[, .(label, tex, build, exists, n_lines)], class=FALSE)
miss <- man[exists==FALSE]
if(nrow(miss)){ cat(sprintf("\n*** MISSING %d .tex: %s\n", nrow(miss), paste(miss$tex, collapse=", ")))
} else { cat(sprintf("\nAll %d paper tables present. manifest -> output/tables/tables_manifest.csv\n", nrow(man))) }
