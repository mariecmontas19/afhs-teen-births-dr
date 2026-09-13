# ============================================================================
# 10_tables.R — consolidation wrapper for ALL paper tables. One entry point that
#   (1) re-runs the lightweight CSV->LaTeX formatters (Table 3, appendix A1/A2),
#   (2) optionally re-runs the heavy estimation scripts (Table 4/5, leads) when
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

# manifest: paper label, .tex file, producing script, build type
man <- rbindlist(list(
  data.table(label="Table 1  Municipality pre/post (reworked §44)", tex="table1_muni_prepost.tex", script="07a3_table1_births.R", build="format"),
  data.table(label="Appx A9  Birth-level descriptives",  tex="table1_birth_descriptives.tex", script="07a_descriptives.R",     build="estimate"),
  data.table(label="Table 2  Municipality balance",      tex="table2_muni_balance.tex",       script="07a_descriptives.R",     build="estimate"),
  data.table(label="Table 3  Main estimates (CS)",       tex="tab_03_main_estimates.tex",          script="10_table3_estimates.R",  build="format"),
  data.table(label="Table 4  Functional form + averted", tex="tab_a18_functional_form.tex",    script="09d_packham_bridge.R",   build="estimate"),
  data.table(label="Table 5  By single-year age",        tex="tab_04_byage.tex",              script="08n_byage.R",            build="estimate"),
  data.table(label="Table 6  AU roll-out",               tex="table3_rollout.tex",            script="07a_descriptives.R",     build="estimate"),
  data.table(label="Appx A1  Estimator robustness",      tex="tab_a01_estimators.tex",       script="10_appendix_tables.R",   build="format"),
  data.table(label="Appx A2  Dynamic effects",           tex="tab_a02_dynamics.tex",         script="10_appendix_tables.R",   build="format"),
  data.table(label="Appx A3  Lead/lag terms",            tex="tab_a03_leads.tex",            script="10_appendix_leads.R",    build="estimate"),
  data.table(label="Appx A4  Newborn health",            tex="tab_a05_newborn_health.tex",      script="08m_newborn_health.R",   build="estimate"),
  data.table(label="Appx A5  Heterogeneity summary",     tex="tab_a06_het_summary.tex",               script="08j_het_summary.R",      build="estimate"),
  data.table(label="Table 8  Cost-effectiveness (main text, own section; MM 2026-07-16)", tex="tab_09_cost_effectiveness.tex",  script="09e_cost_effectiveness.R", build="format"),
  data.table(label="Appx A7  Age at birth vs conception", tex="tab_a07_conception.tex",         script="08o8_table_age_robustness.R", build="format"),
  data.table(label="Appx A8  Age gradient (spillover)",   tex="tab_a08_age_gradient.tex",    script="08p3_table_age_gradient.R",   build="format"),
  data.table(label="Appx A10 Effect by opening cohort",   tex="tab_a16_cohorts.tex",         script="08q2_cohort_atts.R",          build="estimate"),
  data.table(label="Table 4M  Births, pregnancies, abortions (MAIN, promoted §59.33)", tex="tab_05_preg_sns.tex", script="10x_bridge_table.R", build="format")
))

# which scripts to (re-)run: always the light formatters; estimation only if !FAST
to_run <- unique(man[build=="format" | !FAST, script])
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
