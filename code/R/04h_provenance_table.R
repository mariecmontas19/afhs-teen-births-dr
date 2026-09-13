# ============================================================================
# 04h_provenance_table.R — treatment-timing provenance exhibit (MM 2026-08-08:
# "a referee will likely want a compact appendix table listing each AU,
# municipality, first opening date, modernization date, and source quality").
# All 39 units, organized by MUNICIPALITY treatment status (matches estimation):
#   Panel A = 20 in-window municipalities (21 units) = the identifying variation
#   Panel B =  9 pre-2016 municipalities (18 units) = excluded from S1, enter S2
# Source tiers (from notes/pcua_unit_dates_verified.csv 'provenance'):
#   SNS doc > Web/press > Field > SNS list (recorded, unconfirmed) > Interim.
# Typesets only — no estimation. Output: tab_a21_unit_provenance.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")

v  <- fread(file.path(PROJ,"notes","pcua_unit_dates_verified.csv"))
tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[
        , .(adm3_pcode, adm3_name, prov_norm, ever_treated, always_treated, first_year)]
v <- merge(v, tr, by="adm3_pcode", all.x=TRUE)
stopifnot(nrow(v)==39L, !anyNA(v$ever_treated))
v[, mstatus := fifelse(always_treated==1, "pre2016", "inwindow")]
stopifnot(v[mstatus=="inwindow", uniqueN(adm3_pcode)]==20L,
          v[mstatus=="pre2016",  uniqueN(adm3_pcode)]==9L)

tc <- function(x) tools::toTitleCase(tolower(x))
v[, mn := fifelse(!is.na(muni) & muni!="", muni, tc(adm3_name))]
v[, pv := fifelse(!is.na(prov)  & prov!="",  prov, tc(prov_norm))]
v[, unit := gsub("^Hospital ", "", hosp)]
v[, unit := gsub("^(Municipal|Regional|Materno Infantil|Materno|Pediatrico|Pediátrico|General|Docente|Provincial|Infantil|Universitario) ", "", unit)]

# opening: recorded date; the four imputed years show the ESTIMATION year + dagger
v[, oy   := fifelse(!is.na(recorded_year), as.character(recorded_year), as.character(first_year))]
v[, open := fifelse(!is.na(recorded_month), sprintf("%s-%02d", oy, as.integer(recorded_month)), oy)]
v[year_imputed==TRUE, open := sprintf("%d$^{\\dagger}$", first_year)]
# modernization + type
tymap <- c(new_opening="new", upgrade="upgrade", upgrade_unconfirmed="upgrade?",
           old_never_modernized="", unknown="")
v[, mtype := tymap[modern_event_type]]
v[, modern := fcase(never_modernized==TRUE, "None",
                    modern_event_type=="unknown", "None",
                    default = sprintf("%s (%s)", substr(modern_event_date,1,7), mtype))]
smap <- c(SNS_doc="NHS document", web="Web/press", field="Field", user_confirmed="Field",
          recorded_only="NHS list", interim="Interim")
v[, src := smap[provenance]]
stopifnot(!anyNA(v$src), !anyNA(v$mn), !anyNA(v$pv))

esc <- function(x) gsub("([&%#])", "\\\\\\1", x)
rows <- function(dt){
  dt <- dt[order(oy, mn)]
  dt[, sprintf("%s & %s & %s & %s & %s & %s \\\\",
               esc(pv), esc(mn), esc(unit), open, modern, src)]
}
L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Treatment-timing provenance: the 39 adolescent units}",
"\\label{tab:unit_provenance}\\scriptsize",
"\\setlength{\\tabcolsep}{3pt}\\renewcommand{\\arraystretch}{0.92}",
"\\begin{tabular}{llp{5.6cm}ccc}",
"\\toprule",
"Province & Municipality & Unit (hospital) & Opening & Modernization & Source \\\\",
"\\midrule",
sprintf("\\multicolumn{6}{l}{\\textit{Panel A. First openings during the roll-out (20 municipalities, %d units): the identifying variation}} \\\\[2pt]",
        v[mstatus=="inwindow", .N]),
rows(v[mstatus=="inwindow"]),
"\\addlinespace\\midrule",
sprintf("\\multicolumn{6}{l}{\\textit{Panel B. Municipalities served before 2016 (9 municipalities, %d units): excluded from the first-opening design}} \\\\[2pt]",
        v[mstatus=="pre2016", .N]),
rows(v[mstatus=="pre2016"]),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.98\\linewidth}\\vspace{4pt}\\scriptsize",
"\\textit{Notes:} One row per unit; the treatment date of a municipality with several units is its",
"earliest. Panel A municipalities enter the first-opening design at their opening year; Panel B",
"municipalities are excluded from that design and enter the modernization design at their modernization",
"year, alongside Panel A. \\emph{Source} is the strongest evidence behind the unit's dates: NHS",
"document $>$ dated web or press record $>$ author fieldwork $>$ NHS internal list (recorded date",
"without independent confirmation) $>$ interim placeholder. $^{\\dagger}$Opening year imputed to the",
"roll-out midpoint (three municipalities; dropping them: Appendix \\autoref{tab:sensitivity}) or,",
"for Villa Mella, to its municipality's documented first opening. \\emph{Modernization} is the",
"documented arrival of the comprehensive-service model (new full-service unit or upgrade of an",
"existing one); \\emph{None} indicates no documented modernization event. La Vega and Nizao coding",
"choices are discussed in \\ref{app:provenance}.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_a21_unit_provenance.tex"))
cat("saved -> tab_a21_unit_provenance.tex (", nrow(v), "units )\n")
