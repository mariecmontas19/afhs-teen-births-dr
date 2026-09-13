# ============================================================================
# 08t_census_educ.R — R8 census check (MM 2026-07-10): MUNICIPAL education DiD,
# census 2010 (baseline, pre-rollout for ALL munis) -> census 2022 (endline).
# Attendance item is IDENTICAL across censuses (2010 P36_ASISTE / 2022 P42:
# 1 "Si asiste"); beyond-primary from nivel (2010 P37 / 2022 P43, >=3 = sec+,
# same coding, already used in 02e/05). Muni mapping via census_code_to_adm3
# (155/155, proven). SIUBEN 2025 NOT used as endline (poor households only,
# ages 3-17; kept as the separate descriptive null in 08s).
# GROUPS at census day (Nov-2022): always (pre-2016 units, LONG exposure),
# t2020_22 (opened 2020-22, <=2.5y), notyet (2023-25 openers -> built-in
# selection falsification: same placement process, zero exposure), never (ref).
# PREDICTIONS (rule 1b): fertility-channel arithmetic (-11.6% births x ~20%
# cumulative teen-birth risk x partial schooling disruption) => women-15-19
# attendance gain <1pp even in ALWAYS group; t2020_22 ~0 (too recent);
# notyet MUST be ~0 (else selection trends); men ~0 or small. ENHOGAR B2
# (+0.45 yrs at full intensity) would instead imply a LARGE visible gain in
# the always group — this is the adjudicator between the two.
# Output: output/tables/census_educ_did.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(fixest)})
TAB <- file.path(PROJ,"output","tables")
POP <- file.path(DRPAPER,"analysis/datasets/population")
fm  <- c(DOM010905="DOM010901", DOM012510="DOM012501", DOM051703="DOM051701")

m <- as.data.table(readRDS(file.path(DIR_CLEAN,"census_code_to_adm3.rds")))

c10 <- fread(file.path(POP,"CENSO2010RD-PERSONAS.csv"),
             select=c("PROVINCIA","MUNICIPIO","P27_SEXO","P29_EDAD_ANOS_CUMPLIDOS","P36_ASISTE","P37_NIVEL"),
             showProgress=FALSE)
setnames(c10, c("prov","muni","sex","age","asiste","nivel"))
c22 <- fread(file.path(POP,"BD_FINAL_VIVIENDA_HOGAR_PERSONA_XCNPV_PUB.csv"),
             select=c("PROVINCIA","MUNICIPIO","P26_SEXO","P27_EDAD","P42","P43"),
             showProgress=FALSE)
setnames(c22, c("prov","muni","sex","age","asiste","nivel"))
c10[, year := 2010L]; c22[, year := 2022L]
d <- rbind(c10, c22)[age %between% c(15,22)]
rm(c10, c22); gc(verbose=FALSE)

# ---- coding verification before use (rule 1a) ----
cat("asiste values by year (ages 15-22):\n"); print(d[, .N, keyby=.(year, asiste)], class=FALSE)
cat("sex split (expect ~50/50):\n"); print(d[, .(pct2=round(100*mean(sex==2),1)), by=year], class=FALSE)
d[asiste %in% c(8,9), asiste := NA]
d[nivel  %in% c(8,9) & year==2010, nivel := NA]; d[nivel %in% 9 & year==2022, nivel := NA]
d[, att := as.integer(asiste==1)]
d[, sec := as.integer(nivel>=3)]

d <- merge(d, m, by.x=c("prov","muni"), by.y=c("prov_code","muni_code"), all.x=TRUE)
cat("unmapped person rows:", d[is.na(adm3_pcode),.N], "\n")
d <- d[!is.na(adm3_pcode)]
d[adm3_pcode %in% names(fm), adm3_pcode := fm[adm3_pcode]]

# ---- municipal aggregates ----
agg <- rbind(
  d[age %between% c(15,19), .(y=mean(att, na.rm=TRUE), n=.N, outcome="att1519"),   by=.(adm3_pcode, year, sex)],
  d[age %between% c(18,22), .(y=mean(sec, na.rm=TRUE), n=.N, outcome="sec1822"),   by=.(adm3_pcode, year, sex)])

tr <- as.data.table(readRDS(file.path(DIR_CLEAN,"treatment_municipio.rds")))
tr[, grp := fcase(always_treated==1L, "always",
                  ever_treated==1L & first_year <= 2022, "t2020_22",
                  ever_treated==1L & first_year >  2022, "notyet",
                  default = "never")]
cat("groups:\n"); print(tr[, .N, keyby=grp], class=FALSE)
agg <- merge(agg, tr[, .(adm3_pcode, grp)], by="adm3_pcode")
agg[, `:=`(post = as.integer(year==2022), grp = factor(grp, c("never","always","t2020_22","notyet")))]

run <- function(oc, sx){
  a <- agg[outcome==oc & sex==sx]
  f <- feols(y ~ i(grp, post, ref="never") | adm3_pcode + post, a, weights=~n, cluster=~adm3_pcode)
  ct <- coeftable(f)
  data.table(outcome=oc, sex=fifelse(sx==2,"women","men"), grp=gsub(".*::","",gsub(":post","",rownames(ct))),
             coef=round(ct[,1],4), se=round(ct[,2],4), p=round(ct[,4],3),
             base2010=round(weighted.mean(a[year==2010 & grp!="never", y], a[year==2010 & grp!="never", n]),3))
}
res <- rbind(run("att1519",2), run("att1519",1), run("sec1822",2), run("sec1822",1))
cat("\n=== CENSUS DiD 2010->2022 (muni FE + post, weights n, cluster muni) ===\n")
print(res, class=FALSE)
cat("\nraw weighted means, att1519 women, by grp x year:\n")
print(dcast(agg[outcome=="att1519" & sex==2, .(y=round(weighted.mean(y,n),3)), by=.(grp,year)], grp ~ year), class=FALSE)
fwrite(res, file.path(TAB,"census_educ_did.csv"))
cat("saved -> census_educ_did.csv\n")
