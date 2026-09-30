# ============================================================================
# 07p_decomposition.R — WATERFALL decomposition explaining why the pooled
# modernization ATT is smaller than first-access. Each layer is CS vs the same
# never-treated/not-yet controls (g = mod_cohort), denominator A, LEVEL outcome
# rateA_15_19 (the pre-differenced ddd_A was retired from exhibits, MM
# 2026-08-07; §59.68).
# REWRITTEN 2026-08-08 (MM/committee: "no table reports that decomposition"):
# sample now DATA-DRIVEN from treatment_municipio_mod.rds (post-dedup: 20
# in-window first openings + 3 recovered-new + 6 recovered-upgrades = 29;
# was hardcoded 19/3/5/27), per-call seeds (project rule), and the script now
# EMITS the appendix exhibit tab_a17_decomposition.tex (label tab:decomposition).
# CROSS-CHECK: the pooled row must reproduce Table 3's H2 estimate (-3.95).
# Output: cs_att_decomposition.csv + tab_a17_decomposition.tex
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(did)})
TAB <- file.path(PROJ,"output","tables")
fold <- c("DOM010905","DOM051703","DOM012510")

p   <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_muni_year_155.rds")))   # has binary ever/always_treated
mod <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio_mod.rds")))
modk <- mod[!adm3_pcode %in% fold, .(adm3_pcode, mod_cohort, mod_ever_treated, always_treated_mod, mod_first_type)]
dropc <- intersect(names(modk)[-1], names(p)); if (length(dropc)) p <- p[, !dropc, with=FALSE]
p <- merge(p, modk, by="adm3_pcode", all.x=TRUE)
p <- p[always_treated_mod==0 | is.na(always_treated_mod)]

never <- p[mod_ever_treated==0]                                # never any modern unit
firstaccess <- p[always_treated==0 & mod_ever_treated==1]      # in-window first openings
recnew <- p[always_treated==1 & mod_ever_treated==1 & mod_first_type=="new"]      # recovered, first modern unit = new
recupg <- p[always_treated==1 & mod_ever_treated==1 & mod_first_type=="upgrade"]  # recovered, first modern unit = upgrade
nFA <- uniqueN(firstaccess$adm3_pcode); nRN <- uniqueN(recnew$adm3_pcode)
nUP <- uniqueN(recupg$adm3_pcode);      nNV <- uniqueN(never$adm3_pcode)
cat("set sizes: first-access", nFA, "| recovered-new", nRN,
    "(", paste(unique(recnew$adm3_pcode),collapse=","), ") | upgrades", nUP, "| never", nNV, "\n")
stopifnot(nFA + nRN + nUP == mod[!adm3_pcode %in% fold & mod_ever_treated==1 & always_treated_mod==0, .N])

cs1 <- function(treated){
  d <- rbind(never, treated); d[, id := as.integer(factor(adm3_pcode))]
  d[, g := fifelse(mod_ever_treated==1L, as.integer(mod_cohort), 0L)]
  set.seed(20260722)                                           # per-call seed (project rule)
  a <- att_gt(yname="rateA_15_19", tname="year", idname="id", gname="g", xformla=~1, data=d,
              control_group="notyettreated", bstrap=TRUE, cband=TRUE, biters=2000,
              base_period="universal", clustervars="id", est_method="reg", print_details=FALSE)
  o <- aggte(a, type="group", na.rm=TRUE)
  base <- d[g>0 & year==g-1L, mean(rateA_15_19)]
  c(att=o$overall.att, se=o$overall.se, n=uniqueN(treated$adm3_pcode), base=base)
}
sets <- list(); lay <- function(nm, dt) sets[[nm]] <<- dt
lay(sprintf("First openings, 2020--2025 (main design; %d)", nFA), firstaccess)
lay(sprintf("Recovered: first modern unit is new (%d)", nRN), recnew)
lay(sprintf("All new modern units (%d)", nFA+nRN), rbind(firstaccess, recnew))
lay(sprintf("Recovered: upgrades of pre-existing units (%d)", nUP), recupg)
lay(sprintf("Pooled modernization design (%d)", nFA+nRN+nUP), rbind(firstaccess, recnew, recupg))

out <- rbindlist(lapply(names(sets), function(nm){
  r <- cs1(sets[[nm]])
  data.table(layer=nm, n_treated=r[["n"]], ATT=round(r[["att"]],2), SE=round(r[["se"]],2),
             p=round(2*pnorm(-abs(r[["att"]]/r[["se"]])),3), baseline=round(r[["base"]],1),
             pct=round(100*r[["att"]]/r[["base"]],1))
}))
# one-canonical-result rule: the pooled row IS Table 3's H2 estimate -> display the
# canonical headline values (same point; SE differs in the 2nd decimal across seed streams).
.H2 <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S2" & spec=="CS, no covs"]
stopifnot(abs(out[.N, ATT] - round(.H2$estimate,2)) < 0.02)
out[.N, `:=`(ATT=round(.H2$estimate,2), SE=round(.H2$SE,2), p=.H2$p)]
cat("\n==================== NEW-VS-UPGRADE DECOMPOSITION (level, denom A) ====================\n")
print(out, class=FALSE)
fwrite(out, file.path(TAB,"cs_att_decomposition.csv"))

# ---- PANEL B (MM 2026-08-09): the 30-34 comparison band under each design ----
# CS on the 30-34 rate: H1 (binary first-opening cohorts), H2 (modernization
# cohorts, all 29), and the 9 recovered municipalities alone. Women 30-34 cannot
# be exposed (units serve 10-19; oldest exposable is 24 by 2025), so H1's zero
# validates the DDD comparison band and H2's positive response is the urban
# postponement that inflates the H2 triple difference.
AL <- as.data.table(readRDS(file.path(DIR_CLEAN,"panel_age_long_155.rds")))[ag=="30-34", .(adm3_pcode=id, year, y=rateA)]
bin <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))[!adm3_pcode %in% fold]
bin[, g := fifelse(ever_treated==1L & always_treated==0L, as.integer(first_year), fifelse(ever_treated==0L,0L,NA_integer_))]
cs34 <- function(gt){
  d <- merge(AL, gt[!is.na(g), .(adm3_pcode,g)], by="adm3_pcode"); d[, id := as.integer(factor(adm3_pcode))]
  set.seed(20260722)                                           # per-call seed (project rule)
  a <- suppressWarnings(att_gt(yname="y", tname="year", idname="id", gname="g", xformla=~1, data=d,
              control_group="notyettreated", bstrap=TRUE, biters=2000,
              base_period="universal", clustervars="id", est_method="reg", print_details=FALSE))
  o <- aggte(a, type="group", na.rm=TRUE)
  base <- d[g>0 & year==g-1L, mean(y)]
  c(att=o$overall.att, se=o$overall.se, base=base)
}
modg <- mod[!adm3_pcode %in% fold]
modg[, g := fifelse(mod_ever_treated==1L & always_treated_mod==0L, as.integer(mod_cohort), fifelse(mod_ever_treated==0L,0L,NA_integer_))]
rec9 <- rbind(modg[always_treated==1 & mod_ever_treated==1, .(adm3_pcode, g)],
              modg[mod_ever_treated==0, .(adm3_pcode, g)])
b_h1  <- cs34(bin);  b_h2 <- cs34(modg);  b_r9 <- cs34(rec9)
.H1 <- fread(file.path(TAB,"headline_S1_S2.csv"))[scenario=="S1" & spec=="CS, no covs"]
imp_h1 <- .H1$estimate - b_h1[["att"]]; imp_h2 <- .H2$estimate - b_h2[["att"]]
cat(sprintf("\nPANEL B (30-34 band): H1 %+.2f (%.2f) | H2 %+.2f (%.2f) | recovered-9 %+.2f (%.2f) | implied DDD %.2f / %.2f\n",
    b_h1[["att"]], b_h1[["se"]], b_h2[["att"]], b_h2[["se"]], b_r9[["att"]], b_r9[["se"]], imp_h1, imp_h2))
fwrite(data.table(set=c("H1 first opening","H2 modernization","recovered nine"),
                  ATT=round(c(b_h1[["att"]],b_h2[["att"]],b_r9[["att"]]),2),
                  SE =round(c(b_h1[["se"]], b_h2[["se"]], b_r9[["se"]]),2),
                  implied_ddd=c(round(imp_h1,2), round(imp_h2,2), NA)),
       file.path(TAB,"band3034_decomposition.csv"))

# ---- appendix exhibit ---------------------------------------------------------
stars <- function(pp) fifelse(pp<.01,"^{***}", fifelse(pp<.05,"^{**}", fifelse(pp<.10,"^{*}","")))
rowx <- function(i){ r <- out[i]
  sprintf("%s & \\makecell{$%.2f%s$ (%.2f) \\\\ {[$%.2f$, $%.2f$]}} & %.1f & $%+.1f$ & %d \\\\",
          r$layer, r$ATT, stars(r$p), r$SE, r$ATT-1.96*r$SE, r$ATT+1.96*r$SE, r$baseline, r$pct, r$n_treated) }
c34 <- function(v){ att<-v[["att"]]; se<-v[["se"]]; p<-2*pnorm(-abs(att/se))
  sprintf("\\makecell{$%+.2f%s$ (%.2f) \\\\ {[$%+.2f$, $%+.2f$]}}", att, stars(p), se, att-1.96*se, att+1.96*se) }
# never-treated triple differences quoted in the note: read where Table 3 reads them (07s output)
.NT <- fread(file.path(TAB,"ddd_triplediff.csv"))
.NT1 <- .NT[spec=="S1 binary any-unit | uncond dr", ATT]; .NT2 <- .NT[spec=="S2 modernization | uncond dr", ATT]
stopifnot(length(.NT1)==1, length(.NT2)==1)
L <- c(
"\\begin{table}[htbp]\\centering",
"\\caption{New units versus upgrades: decomposing the modernization estimate}",
"\\label{tab:decomposition}\\small",
"\\setlength{\\tabcolsep}{4pt}",
"\\textit{Panel A. New units versus upgrades}\\par\\smallskip",
"\\begin{tabular}{lcccc}",
"\\toprule",
" & Estimate (SE) [95\\% CI] & Baseline & \\% of base & Treated \\\\",
"\\midrule",
rowx(1), rowx(2), rowx(3),
"\\addlinespace",
rowx(4),
"\\midrule",
rowx(5),
"\\bottomrule",
"\\end{tabular}",
"\\par\\medskip",
"\\textit{Panel B. The 30--34 comparison band (placebo)}\\par\\smallskip",
"\\begin{tabular}{lcc}",
"\\toprule",
" & First AU opening & \\makecell{First AU opening \\\\ or modernization} \\\\",
"\\midrule",
sprintf("Effect on the 30--34 birth rate & %s & %s \\\\", c34(b_h1), c34(b_h2)),
sprintf("\\quad recovered nine municipalities alone & N/A & %s \\\\", c34(b_r9)),
sprintf("Implied triple difference (Table 3 level $-$ band) & $%.2f$ & $%.2f$ \\\\", imp_h1, imp_h2),
"\\bottomrule",
"\\end{tabular}",
"\\begin{minipage}{0.97\\linewidth}\\vspace{4pt}\\footnotesize",
"\\textit{Notes:} Callaway--Sant'Anna group ATTs; not-yet-treated comparisons, universal base period,",
"municipality-clustered multiplier bootstrap, seed reset before each estimate. \\emph{Panel A}: the teen",
"birth rate, estimated separately for each subset of the modernization design's treated municipalities",
"against the shared never-treated comparisons, with modernization-cohort timing throughout.",
"\\emph{Recovered} municipalities are the nine served before 2016, entering through their first",
"modern-unit event: a newly built full-service unit (new) or a documented remodel (upgrade). Baseline",
"$=$ treated mean in the year before the modernization event. The pooled row reproduces the modernization column",
"of \\autoref{tab:estimates}; the upgrade subset is small and its estimate correspondingly imprecise.",
"\\emph{Panel B}: the 30--34 birth rate, the triple difference's comparison band; women aged 30--34",
"cannot be exposed (the units serve ages 10--19, and the oldest woman exposable as an adolescent is 24",
"by 2025). The near-zero response under the first-opening design validates the comparison band; under",
"the modernization design the band \\emph{rises}, driven entirely by the nine recovered municipalities,",
"consistent with urban fertility postponement rather than the program. Subtracting the band effect from",
"the level estimates of \\autoref{tab:estimates} approximately reproduces its never-treated triple differences",
sprintf("($%.2f$, $%.2f$): the larger modernization triple difference is the comparison band moving, not a", .NT1, .NT2),
"larger effect on teens. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
"\\end{minipage}",
"\\end{table}")
writeLines(L, file.path(TAB,"tab_a17_decomposition.tex"))
cat("saved -> cs_att_decomposition.csv + tab_a17_decomposition.tex\n")
