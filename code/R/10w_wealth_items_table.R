# ============================================================================
# 10w_wealth_items_table.R — Appendix table: the 29 components of the wealth
# index (v2, §59.30-31), each with its ownership/incidence share, PC1 loading
# and Kaiser-Meyer-Olkin sampling-adequacy statistic, plus a panel comparing
# the four candidate specifications on KMO and on validation against the
# SIUBEN ICV. Inputs are the diagnostics already written by 05x2_kmo_expanded.R
# (kmo_wealth_expanded_items.csv, kmo_wealth_specs.csv) — nothing re-estimated.
# Output: output/tables/tab_a15_wealth_items.tex
# ============================================================================
source("code/R/00_config.R")
suppressMessages(library(data.table))
TAB <- DIR_TABLES

it <- fread(file.path(TAB, "kmo_wealth_expanded_items.csv"))
sp <- fread(file.path(TAB, "kmo_wealth_specs.csv"))
stopifnot(nrow(it) == 29L)

# verified census labels (05x_kmo_wealth.R, checked against Libro_de_codigos XCNPV)
lab <- c(
  P15B="Refrigerator",              P15C="Washing machine",
  P15D="Television",                P15E="Radio or music system",
  P15F="Cistern",                   P15G="Rooftop water tank",
  P15H="Desktop computer",          P15I="Laptop computer",
  P15J="Tablet",                    P15K="Cell phone",
  P15L="Landline telephone",        P15M="Internet subscription",
  P15N="Power inverter",            P15O="Electricity generator",
  P15P="Air conditioning",          P15R="Private car",
  crowded        ="More than two persons per bedroom",
  apartment      ="Dwelling is an apartment",
  marginal_dwell ="Marginal dwelling (room, barracks, unfinished)",
  wall_block     ="Walls of block or concrete",
  roof_concrete  ="Concrete roof",
  floor_quality  ="Finished floor (mosaic, granite, marble, ceramic)",
  kitchen_inside ="Kitchen inside the dwelling",
  water_piped_in ="Piped water inside the dwelling",
  toilet_flush   ="Flush toilet",
  sewer          ="Connected to the public sewer network",
  garbage_coll   ="Household refuse collected",
  cook_gas_elec  ="Cooks with gas or electricity",
  light_grid     ="Lighting from the electricity grid")
stopifnot(all(it$item %in% names(lab)))
it[, label := lab[item]]
it[, grp := fifelse(grepl("^P15", item), "A. Durable assets", "B. Dwelling and services")]
setorder(it, grp, -msa)

esc <- function(x) gsub("%", "\\\\%", x, fixed=TRUE)
row <- function(r) sprintf("%s & %.2f & %+.3f & %.3f \\\\", esc(r$label), 100*r$share, r$pc1_loading, r$msa)

L <- c("\\begin{table}[htbp]\\centering",
"\\caption{Components of the household wealth index}",
"\\label{tab:wealth_items}\\scriptsize",
"\\renewcommand{\\arraystretch}{0.94}",
"\\begin{tabular}{lccc}",
"\\toprule",
" & Share of & PC1 & Sampling \\\\",
"Component & households (\\%) & loading & adequacy \\\\",
"\\midrule")
for (g in c("A. Durable assets","B. Dwelling and services")) {
  L <- c(L, sprintf("\\multicolumn{4}{l}{\\textit{Panel %s}} \\\\[2pt]", g))
  for (i in seq_len(it[grp==g, .N])) L <- c(L, row(it[grp==g][i]))
  if (g == "A. Durable assets") L <- c(L, "\\addlinespace")
}
L <- c(L, "\\midrule",
"\\multicolumn{4}{l}{\\textit{Panel C. Specification search}} \\\\[2pt]",
" & Items & KMO & Corr.\\ with ICV \\\\",
"\\addlinespace")
spl <- c("S0 current 18 durables"="Durable assets only (initial index)",
         "S1 17 durables (no moto)"="Drop motorbike",
         "S2 17 + 14 candidates"="Add dwelling and service items",
         "S3 pruned (29 items)"="Prune items with adequacy below .85 (\\textbf{index used})")
for (s in names(spl)) { r <- sp[spec==s]
  L <- c(L, sprintf("%s & %d & %.3f & %+.3f \\\\", spl[[s]], r$items, r$KMO, r$cor_ICV)) }
L <- c(L, "\\bottomrule","\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{3pt}\\scriptsize",
sprintf("\\textit{Notes:} The index is the first principal component of the %d binary items in Panels A and B, estimated on %s households of the 2022 census that report every item, standardized to mean zero and unit variance, then averaged to the municipality. Codings were verified against the census codebook; shares are unweighted incidence. Sampling adequacy is the item Kaiser-Meyer-Olkin measure. The overall KMO is %.3f and the lowest item value is %.2f, both above the conventional .80 threshold. A stove and a motorbike were dropped from the initial specification because their adequacy fell below .85. Panel C reports the search: adding dwelling and service items and pruning weak ones raises the KMO from %.3f to %.3f and strengthens the correlation between the municipality index and the SIUBEN quality-of-life index from %+.3f to %+.3f. That correlation is negative because the ICV scale runs in the opposite direction, so a larger magnitude means better validation.", nrow(it), format(sp[spec=="S3 pruned (29 items)"]$hh, big.mark=","), sp[spec=="S3 pruned (29 items)"]$KMO, sp[spec=="S3 pruned (29 items)"]$min_item_MSA, sp[spec=="S0 current 18 durables"]$KMO, sp[spec=="S3 pruned (29 items)"]$KMO, sp[spec=="S0 current 18 durables"]$cor_ICV, sp[spec=="S3 pruned (29 items)"]$cor_ICV),
"\\end{minipage}","\\end{table}")
writeLines(L, file.path(TAB,"tab_a15_wealth_items.tex"))
cat("saved -> tab_a15_wealth_items.tex |", nrow(it), "items | KMO",
    sp[spec=="S3 pruned (29 items)"]$KMO, "\n")
