# ============================================================================
# 02f_projection_appendix.R — population-projection transparency exhibits
# (MM 2026-08-06):
#   (1) tab_a20_projection_sd.tex  — ILLUSTRATIVE in-paper table (Appendix C.1):
#       women 15-19, Santo Domingo province + each of its municipalities,
#       2016-2025, Series A and Series B panels.
#   (2) paper/population_projections/projection_tables_body.tex — the FULL
#       standalone volume: municipality x age-group x year, province-first,
#       four parts (Series A/B x Female/Male). Reads the FROZEN canonical
#       pop_municipio_2016_2025_A/B.rds (verified byte-identical from raw,
#       methods §59.64) — typesets, never re-estimates.
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages(library(data.table))
TAB <- file.path(PROJ,"output","tables")
VOL <- file.path(PROJ,"paper","population_projections")
dir.create(VOL, showWarnings=FALSE, recursive=TRUE)

cw <- unique(as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))[, .(adm3_pcode, adm3_name, prov_name)])
A  <- merge(as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds"))), cw, by="adm3_pcode", all.x=TRUE)
B  <- merge(as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_B.rds"))), cw, by="adm3_pcode", all.x=TRUE)
stopifnot(!anyNA(A$prov_name), !anyNA(B$prov_name), nrow(A)==52700, nrow(B)==52700)
AGE_LV <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
            "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
stopifnot(setequal(unique(A$age_group), AGE_LV))
esc  <- function(x) gsub("([&%#])","\\\\\\1", x)
tc   <- function(x) tools::toTitleCase(tolower(x))
numf <- function(v) formatC(round(v), format="d", big.mark=",")

## ---- (1) illustrative table: Santo Domingo province, women 15-19 ------------
sd_ill <- function(dt, lab){
  s <- dt[prov_name=="SANTO DOMINGO" & sex=="F" & age_group=="15-19"]
  stopifnot(nrow(s) > 0)
  w <- dcast(s, adm3_name ~ year, value.var="pop")
  setorder(w, adm3_name)
  tot <- s[, .(pop=sum(pop)), by=year]
  rows <- c(sprintf("\\textbf{Santo Domingo (province)} & %s \\\\",
                    paste(numf(tot[order(year), pop]), collapse=" & ")),
            w[, sprintf("\\quad %s & %s \\\\", esc(tc(adm3_name)),
                        paste(numf(unlist(.SD)), collapse=" & ")), by=adm3_name,
              .SDcols=as.character(2016:2025)]$V1)
  c(sprintf("\\multicolumn{11}{l}{\\textit{%s}} \\\\[2pt]", lab), rows)
}
L <- c(
"\\begin{table}[H]\\centering",
"\\caption{Projected population of women aged 15--19: Santo Domingo province, both denominator series}",
"\\label{tab:projection_sd}\\scriptsize",
"\\setlength{\\tabcolsep}{2pt}",
"\\begin{tabular}{lrrrrrrrrrr}",
"\\toprule",
paste(" &", paste(2016:2025, collapse=" & "), "\\\\"),
"\\midrule",
sd_ill(A, "Panel A. Series A (NSO estimates 2016--2020; Hamilton--Perry extension 2021--2025)"),
"\\addlinespace\\midrule",
sd_ill(B, "Panel B. Series B (intercensal interpolation, 2010 and 2022 censuses)"),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Illustrative extract of the municipality-level population projections used as",
"denominators; the province row sums its municipalities (2010 municipal division: the three",
"post-2010 municipalities are folded into their parent municipalities). The complete projections,",
"every municipality by five-year age group, sex, year, and series, appear in the supplementary",
"volume \\emph{Population Projections for the Municipalities of the Dominican Republic, 2016--2025}",
"and as machine-readable files in the replication package.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_a20_projection_sd.tex"))
cat("saved -> tab_a20_projection_sd.tex (", A[prov_name=="SANTO DOMINGO", uniqueN(adm3_pcode)], "SD municipalities )\n")

## ---- (2) standalone volume body ---------------------------------------------
part <- function(dt, series_lab, sx, sx_lab){
  d <- dt[sex==sx]
  d[, age_group := factor(age_group, levels=AGE_LV)]
  provs <- sort(unique(d$prov_name))
  out <- c(sprintf("\\section*{%s --- %s}", series_lab, sx_lab),
           "\\begin{footnotesize}",
           "\\setlength{\\tabcolsep}{2.5pt}",
           "\\begin{longtable}{lrrrrrrrrrr}",
           "\\toprule",
           paste("Age group &", paste(2016:2025, collapse=" & "), "\\\\"),
           "\\midrule\\endhead",
           "\\bottomrule\\endfoot")
  for (pv in provs){
    out <- c(out, sprintf("\\multicolumn{11}{l}{\\textbf{%s (province)}} \\\\", esc(tc(pv))))
    for (mn in sort(unique(d[prov_name==pv, adm3_name]))){
      out <- c(out, sprintf("\\multicolumn{11}{l}{\\quad\\textit{%s}} \\\\", esc(tc(mn))))
      w <- dcast(d[prov_name==pv & adm3_name==mn], age_group ~ year, value.var="pop")
      setorder(w, age_group)
      out <- c(out, w[, sprintf("\\quad\\quad %s & %s \\\\", age_group,
                paste(numf(unlist(.SD)), collapse=" & ")), by=age_group,
                .SDcols=as.character(2016:2025)]$V1)
    }
    out <- c(out, "\\addlinespace")
  }
  c(out, "\\end{longtable}", "\\end{footnotesize}", "\\clearpage")
}
body <- c(part(A, "Series A", "F", "Females"), part(A, "Series A", "M", "Males"),
          part(B, "Series B", "F", "Females"), part(B, "Series B", "M", "Males"))
writeLines(body, file.path(VOL,"projection_tables_body.tex"))
cat("saved -> projection_tables_body.tex (", length(body), "lines )\n")

## ---- (3) METHODOLOGY SECTION for the supplementary volume (MM 2026-08-11) ----
## UN-manual register: purpose, inputs, notation, numbered procedure, worked
## example (computed HERE from the inputs and ASSERTED equal to the deposited
## Series A value), validation table (from projection_validation.csv), limits.
pm  <- merge(as.data.table(readRDS(file.path(DIR_CLEAN,"pop_muni_2015_2020_long.rds"))),
             data.table(age_group=AGE_LV, ax=1:17), by="age_group")
pcx <- merge(as.data.table(readRDS(file.path(DIR_CLEAN,"prov_controls_2000_2030_long.rds"))),
             data.table(age_group=AGE_LV, ax=1:17), by="age_group")
xw2 <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
pv  <- fread(file.path(PROJ,"output","tables","projection_validation.csv"))
gv  <- function(k) 100*pv[stat==k, value]

## worked example: Series A, women 15-19, Azua (DOM010101), year 2023
EXM <- "DOM050201"                          # Azua (verified pcode; province 2)
EXP <- xw2[adm3_pcode==EXM, unique(prov_code)]
w1014_15 <- pm[adm3_pcode==EXM & sex=="F" & age_group=="10-14" & year==2015, pop]
w1519_20 <- pm[adm3_pcode==EXM & sex=="F" & age_group=="15-19" & year==2020, pop]
w1014_20 <- pm[adm3_pcode==EXM & sex=="F" & age_group=="10-14" & year==2020, pop]
stopifnot(length(EXP)==1, !is.na(EXP), length(w1014_15)==1, length(w1519_20)==1, length(w1014_20)==1)
ccr_ex   <- min(max(w1519_20 / w1014_15, 0.3), 3)
proj25   <- ccr_ex * w1014_20
raw23    <- w1519_20 + (proj25 - w1519_20) * (3/5)
## raking factor: province group sum of raw 2023 over all municipios in EXP
mun_p <- xw2[prov_code==EXP, unique(adm3_pcode)]
d15 <- unique(pm[adm3_pcode %in% mun_p & sex=="F" & age_group=="10-14" & year==2015, .(adm3_pcode, a=pop)])
d20 <- unique(pm[adm3_pcode %in% mun_p & sex=="F" & age_group=="15-19" & year==2020, .(adm3_pcode, b=pop)])
d20c<- unique(pm[adm3_pcode %in% mun_p & sex=="F" & age_group=="10-14" & year==2020, .(adm3_pcode, c0=pop)])
dg <- Reduce(function(u,v) merge(u,v,by="adm3_pcode"), list(d15,d20,d20c))
dg[, cc := pmin(pmax(b/a, 0.3), 3)][, raw := b + (cc*c0 - b) * (3/5)]
gsum   <- dg[, sum(raw)]
target <- pcx[prov_code==EXP & sex=="F" & age_group=="15-19" & year==2023, pop]
final23 <- raw23 * target / gsum
Aval <- A[adm3_pcode==EXM & sex=="F" & age_group=="15-19" & year==2023, pop]
# LENGTH-GUARDED assertion (stopifnot(logical(0)) passes vacuously — never rely on it)
stopifnot(length(target)==1, length(final23)==1, length(Aval)==1, abs(final23 - Aval) < 1e-6)
cat(sprintf("worked example verified: Azua F 15-19 2023 manual=%.3f deposit=%.3f\n", final23, Aval))

f0 <- function(x) formatC(x, format="d", big.mark=",")
M <- c(
r"(\section{Purpose and scope})",
r"(This volume documents, at the level of reproducible arithmetic, the construction of the two municipality-level population series used as denominators in the accompanying paper, and reports the complete series (municipality $\times$ sex $\times$ five-year age group $\times$ year, 2016--2025) in Part~I--Part~IV. The National Statistics Office (NSO; \emph{Oficina Nacional de Estad\'istica}) publishes municipality population estimates only through 2020, and, in response to an information request filed under Law 200-04, confirmed in writing (August 2026) that it does not maintain subnational estimates or projections consistent with the 2022 National Population and Housing Census. The series documented here are therefore, to the author's knowledge, the only municipality-level population series consistent with the country's most recent census. The method follows the cohort-change-ratio approach of \citeper{Hamilton and Perry (1962)} in the small-area implementation recommended by Smith, Tayman and Swanson (2013), within the general cohort framework of the United Nations projection manuals (United Nations, 1956).)",
"",
r"(\section{Data inputs})",
r"(\begin{center}\small\begin{tabular}{p{5.1cm}p{4.6cm}p{2.4cm}p{2.6cm}})",
r"(\toprule Input & Source & Vintage & Geography \\ \midrule)",
r"(Municipal population estimates, by sex and five-year age group, 2015--2020 & NSO, official municipal estimates & pre-census & 155 municipalities \\)",
r"(Province population projections, by sex and five-year age group, 2000--2030 & NSO, official province projections (2014 revision) & pre-census & 32 provinces \\)",
r"(IX National Population and Housing Census & NSO, census microdata & 2010 & 155 municipalities \\)",
r"(X National Population and Housing Census & NSO, census tabulations & 2022 & 158 municipalities \\ \bottomrule)",
r"(\end{tabular}\end{center})",
r"(All series are expressed on the 2010 municipal division of 155 municipalities: the three municipalities created in 2013 --- San V\'ictor (Ley 85-13), Matanzas (Ley 111-13), and Baitoa (Ley 69-13) --- are folded into their parent municipalities (Moca, Ban\'i, and Santiago, respectively) wherever a source reports them separately, so that every input and output shares one consistent geography.)",
"",
r"(\section{Notation})",
r"(Let $P^{s}_{m,x}(t)$ denote the population of sex $s \in \{F, M\}$, five-year age group $x \in \{1, \dots, 17\}$ (where $x=1$ is ages 0--4, $x=2$ is 5--9, \dots, and $x=17$ is the open interval 80+), in municipality $m$, at mid-year $t$. Each municipality belongs to a province $p(m)$. A hat ($\hat P$) denotes an unraked projected value; a tilde ($\tilde P$) denotes a final, raked value.)",
"",
r"(\section{Computational procedure})",
r"(\subsection*{Step 1. Cohort-change ratios})",
r"(Cohort-change ratios (CCRs) are computed from the two endpoints of the observed municipal series, 2015 and 2020, for every municipality and sex:)",
r"(\begin{equation} CCR^{s}_{m,x} \;=\; \frac{P^{s}_{m,x}(2020)}{P^{s}_{m,x-1}(2015)}, \qquad x = 2, \dots, 16, \end{equation})",
r"(\begin{equation} CCR^{s}_{m,17} \;=\; \frac{P^{s}_{m,17}(2020)}{P^{s}_{m,16}(2015) + P^{s}_{m,17}(2015)}, \end{equation})",
r"(where equation (2) is the standard open-interval ratio: survivors into 80+ come from both 75--79 and 80+ five years earlier. Each ratio embeds cohort survival and net migration jointly, which is the defining economy of the Hamilton--Perry method: no separate mortality or migration schedules are required. To guard against instability in small cells, ratios are winsorized to the interval $[0.3, 3]$:)",
r"(\begin{equation} \overline{CCR}^{s}_{m,x} \;=\; \min\!\big\{\max\{CCR^{s}_{m,x},\, 0.3\},\, 3\big\}. \end{equation})",
r"(No ratio is defined for $x=1$ (ages 0--4), which would require a fertility model; the 0--4 group is instead held at its base-year value and its final level is set entirely by the raking step (Step 4). Ages 0--4 are not a denominator in the paper.)",
"",
r"(\subsection*{Step 2. Five-year projection})",
r"(From a base year $b$, the population five years ahead is)",
r"(\begin{equation} \hat P^{s}_{m,x}(b{+}5) \;=\; \overline{CCR}^{s}_{m,x} \cdot P^{s}_{m,x-1}(b), \qquad x = 2, \dots, 16, \end{equation})",
r"(with the open interval using $P^{s}_{m,16}(b) + P^{s}_{m,17}(b)$ as the base cohort and $\hat P^{s}_{m,1}(b{+}5) = P^{s}_{m,1}(b)$.)",
"",
r"(\subsection*{Step 3. Annualization})",
r"(Intermediate years are obtained by linear interpolation between the base year and the five-year projection:)",
r"(\begin{equation} \hat P^{s}_{m,x}(t) \;=\; P^{s}_{m,x}(b) + \big[\hat P^{s}_{m,x}(b{+}5) - P^{s}_{m,x}(b)\big]\cdot\frac{t-b}{5}, \qquad t = b{+}1, \dots, b{+}5. \end{equation})",
"",
r"(\subsection*{Step 4. Raking to province targets})",
r"(Within every province $\times$ sex $\times$ age group $\times$ year cell, municipal values are scaled proportionally so that they sum exactly to a province target $T^{s}_{p,x}(t)$:)",
r"(\begin{equation} \tilde P^{s}_{m,x}(t) \;=\; \hat P^{s}_{m,x}(t) \cdot \frac{T^{s}_{p(m),x}(t)}{\sum_{m' \in p(m)} \hat P^{s}_{m',x}(t)}. \end{equation})",
r"(Raking guarantees province-level consistency by construction; the Hamilton--Perry step determines only how each province total is distributed across its municipalities.)",
"",
r"(\section{Series A: official-inputs series (main)})",
r"(Series A uses only official NSO inputs and never uses the 2022 census. For 2016--2020 it reproduces the NSO municipal estimates as published (which already sum to NSO province totals). For 2021--2025 it applies Steps 1--4 with base year $b = 2020$ and targets equal to NSO's official province projections as published, $T^{s}_{p,x}(t) = O^{s}_{p,x}(t)$.)",
"",
r"(\section{Series B: intercensal series (robustness)})",
r"(Series B uses both censuses. For 2016--2021, each cell is a linear interpolation between the two census counts:)",
r"(\begin{equation} P^{s}_{m,x}(t) \;=\; P^{s}_{m,x}(2010) + \big[P^{s}_{m,x}(2022) - P^{s}_{m,x}(2010)\big]\cdot\frac{t-2010}{12}. \end{equation})",
r"(For 2022 it is the census count itself. For 2023--2025 it applies Steps 2--4 with base year $b = 2022$ (the census), the same cohort-change ratios as Series A (Step 1), and census-anchored targets that carry the census level forward by NSO's projected growth rates only:)",
r"(\begin{equation} T^{s}_{p,x}(t) \;=\; C^{s}_{p,x}(2022) \cdot \frac{O^{s}_{p,x}(t)}{O^{s}_{p,x}(2022)}, \end{equation})",
r"(where $C$ denotes the census and $O$ the NSO province projection.)",
"",
r"(\section{Worked example})",
sprintf(r"(Consider women aged 15--19 in Azua (\texttt{%s}), province code %s, in Series A for the year 2023. \textbf{Step 1:} the observed inputs are $P^{F}_{10\text{--}14}(2015) = %s$ and $P^{F}_{15\text{--}19}(2020) = %s$, giving $CCR = %s/%s = %.4f$ (within $[0.3,3]$, so unaltered by winsorization). \textbf{Step 2:} applied to the 2020 base cohort $P^{F}_{10\text{--}14}(2020) = %s$, the projected 2025 value is $%.4f \times %s = %.1f$. \textbf{Step 3:} annualizing to 2023, $\hat P = %s + (%.1f - %s) \cdot 3/5 = %.1f$. \textbf{Step 4:} the NSO province projection target for this cell is $T = %s$ and the unraked municipal values in the province sum to $%.1f$, giving the raked value $\tilde P = %.1f \times %s / %.1f = %.1f$ --- which is, to machine precision, the value reported for this cell in Part~I of this volume.)",
        EXM, EXP, f0(w1014_15), f0(w1519_20), f0(w1519_20), f0(w1014_15), ccr_ex,
        f0(w1014_20), ccr_ex, f0(w1014_20), proj25,
        f0(w1519_20), proj25, f0(w1519_20), raw23,
        f0(round(target)), gsum, raw23, f0(round(target)), gsum, final23),
"",
r"(\section{Validation})",
r"(The projection step is validated by a held-out back-test: using the Step-1 ratios (2015$\to$2020), the 2020 base is projected forward and annualized to 2022 (Steps 2--3, unraked, so the test evaluates the cohort-change step itself rather than the raking identity), and the prediction is compared with the held-out 2022 census:)",
r"(\begin{equation} \hat P^{s}_{m,x}(2022) \;=\; P^{s}_{m,x}(2020) + \big[\hat P^{s}_{m,x}(2025) - P^{s}_{m,x}(2020)\big] \cdot \tfrac{2}{5}. \end{equation})",
r"(\begin{center}\small\begin{tabular}{lc})",
r"(\toprule Statistic & Value \\ \midrule)",
sprintf(r"(Mean absolute percentage error, national age--sex cells & %.2f\%% \\)", gv("mape_national_agesex")),
sprintf(r"(Mean signed percentage error, national age--sex cells & %.2f\%% \\)", gv("alpe_national_agesex")),
sprintf(r"(Mean absolute percentage error, municipality cells & %.2f\%% \\)", gv("mape_municipio_cells")),
sprintf(r"(Median absolute percentage error, municipality cells & %.2f\%% \\ \bottomrule)", gv("medape_municipio_cells")),
r"(\end{tabular}\end{center})",
"",
r"(\section{Limitations})",
r"(Cohort-change ratios embed the 2015--2020 pattern of survival and net migration and hold it fixed through 2025; a municipality whose migration profile changed after 2020 will be captured only through its province total. The 0--4 age group carries no cohort information and is set by raking alone. The winsorization bounds, $[0.3, 3]$, bind only in small cells and trade a small bias for variance reduction. Series A inherits any error in NSO's official province projections; Series B inherits the assumption of linear intercensal change. The accompanying paper shows that the treatment-effect estimates are insensitive to the choice between the two series.)"
)
M <- gsub(r"(\\citeper\{([^}]*)\})", r"(\1)", M)   # plain-text citation (no bib machinery inside include)
writeLines(M, file.path(VOL,"methodology.tex"))
cat("saved -> methodology.tex (worked example verified against deposit)\n")
