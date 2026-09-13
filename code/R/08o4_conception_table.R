# ============================================================================
# 08o4_conception_table.R — CONSOLIDATED age-at-conception robustness table
# (MM 2026-07-09): main 15-19 + single ages 15..19, under TWO gestation
# definitions:
#   (A) recorded gestational age: conception_date = birth_date - gestage_wks*7
#       (built in 03; = what 08o/08o2 used)
#   (B) fixed 40 weeks:           conc40 = birth_date - 280 days
#       (sensitivity to measurement error in the recorded gestage field)
# Both use age at conception = floor((conc - mother_dob)/365.25); same LMP-proxy
# convention; inconsistency filter |age_mom-age_conc|>1 or age_conc>age_mom -> NA.
# Window 2018-2025 (mother_dob is a 2018+ field); S1; common womenA_15_19
# denominator (single ages sum to the 15-19 total); CS spec = 07t.
# Output: output/tables/conception_table.csv + tab_a07_conception.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
set.seed(20260722); fold <- c("DOM010905","DOM051703","DOM012510"); TAB <- file.path(PROJ,"output","tables")
foldmap <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

b <- as.data.table(readRDS(file.path(DIR_CLEAN,"births_clean_2016_2025.rds")))
b <- b[!is.na(adm3_pcode) & birth_year %in% 2018:2025]
b[adm3_pcode %in% names(foldmap), adm3_pcode := foldmap[adm3_pcode]]
mkage <- function(conc){
  a <- floor(as.numeric(difftime(conc, b$mother_dob, units="days"))/365.25)
  fifelse(!is.na(a) & (abs(b$age_mom-a)>1 | a>b$age_mom), NA_real_, a)
}
b[, age_A := mkage(conception_date)]                 # recorded gestational age
b[, age_B := mkage(birth_date - 280L)]               # fixed 40 weeks
cat(sprintf("agreement A vs B (both non-NA): %.1f%% identical\n",
    100*b[!is.na(age_A) & !is.na(age_B), mean(age_A==age_B)]))

p <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))[year %in% 2018:2025,
       .(adm3_pcode, year, womenA_15_19)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]

run_variant <- function(agevar, vlab){
  bx <- copy(b)[, ac := get(agevar)]
  cnt <- dcast(bx[ac %in% 15:19], adm3_pcode + birth_year ~ ac, fun.aggregate=length, value.var="ac")
  setnames(cnt, "birth_year", "year"); setnames(cnt, as.character(15:19), paste0("a",15:19), skip_absent=TRUE)
  for(v in paste0("a",15:19)) if(!v %in% names(cnt)) cnt[, (v):=0L]
  d0 <- merge(p, cnt, by=c("adm3_pcode","year"), all.x=TRUE)
  for(v in paste0("a",15:19)) d0[is.na(get(v)), (v):=0L]
  d0[, a1519 := a15+a16+a17+a18+a19]
  d0 <- d0[!adm3_pcode %in% fold]
  for(v in c(paste0("a",15:19),"a1519")) d0[, (paste0("r_",v)) := 1000*get(v)/womenA_15_19]
  est0 <- merge(d0, bin[!is.na(g), .(adm3_pcode, g)], by="adm3_pcode")
  est0[, id := as.integer(factor(adm3_pcode))]
  one <- function(yname, lab){
    a <- suppressWarnings(suppressMessages(att_gt(yname=yname, tname="year", idname="id", gname="g",
          xformla=~1, data=est0, control_group="notyettreated", base_period="universal", est_method="reg",
          bstrap=TRUE, biters=2000, clustervars="id", print_details=FALSE)))
    o <- suppressMessages(aggte(a, type="group", na.rm=TRUE))
    bs <- est0[g>0 & year==g-1L, mean(get(yname))]
    data.table(variant=vlab, age=lab, ATT=round(o$overall.att,2), SE=round(o$overall.se,2),
               p=round(2*pnorm(-abs(o$overall.att/o$overall.se)),3),
               baseline=round(bs,2), pct=round(100*o$overall.att/bs,0))
  }
  rbindlist(c(list(one("r_a1519","15--19 (all)")),
              lapply(15:19, function(a) one(paste0("r_a",a), as.character(a)))))
}
res <- rbind(run_variant("age_A","A: recorded gestational age"),
             run_variant("age_B","B: fixed 40 weeks"))
cat("\n=== age-at-conception table (2018-25; S1; common 15-19 denominator) ===\n")
print(res, class=FALSE)
fwrite(res, file.path(TAB,"conception_table.csv"))

# ---- LaTeX (paper robustness table) ----
stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
cl <- function(v,a){ r <- res[variant==v & age==a]; s <- stars(r$p); sup <- if(s=="")"" else sprintf("^{%s}",s)
  sprintf("\\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}}", r$ATT, sup, r$SE, r$ATT-1.96*r$SE, r$ATT+1.96*r$SE) }
rr <- function(a, lab=a) sprintf("\\quad %s & %s & %s \\\\", lab,
  cl("A: recorded gestational age", a), cl("B: fixed 40 weeks", a))
L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{Effect by mother's age at conception (robustness)}",
"\\label{tab:conception}\\small",
"\\begin{tabular}{lcc}",
"\\toprule",
" & \\multicolumn{2}{c}{Conception dated by} \\\\",
"\\cmidrule(lr){2-3}",
"Age at conception & Recorded gestational age & Fixed 40 weeks \\\\",
"\\midrule",
sprintf("\\textbf{15--19 (main estimate)} & %s & %s \\\\[2pt]", cl("A: recorded gestational age","15--19 (all)"), cl("B: fixed 40 weeks","15--19 (all)")),
rr("15"), rr("16"), rr("17"), rr("18"), rr("19"),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.92\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Callaway--Sant'Anna group ATT, first AU opening (20 treated municipalities),",
"panel years 2018--2025 (the mother's date of birth is recorded from 2018). Age at conception $=$",
"age at (birth date $-$ gestation); column 1 uses the recorded gestational age, column 2 a fixed",
"40 weeks. Outcome $=$ births conceived at the given age per 1{,}000 women 15--19 (common",
"denominator), so single-year rows sum to the 15--19 total. Records with inconsistent date-of-birth/",
"gestation combinations ($\\approx$2.4\\%) excluded. Age-at-birth reference on the same window:",
"$-6.43$ ($-12\\%$). Municipio-clustered standard errors in parentheses; 95\\% confidence",
"intervals in brackets.",
"$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_a07_conception.tex"))
cat("\nsaved -> conception_table.csv + tab_a07_conception.tex\n")
