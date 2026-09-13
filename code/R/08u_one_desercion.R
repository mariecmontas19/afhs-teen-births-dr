# ============================================================================
# 08u_one_desercion.R — ONE dashboard education tabs (source presumed MINERD,
# UNVERIFIED — §59.9). DATA REALITY CHECK (rule 7, 2026-07-13): the Desercion
# tab is a SINGLE 2022 CROSS-SECTION padded with zeros for 2017-2021 (every
# prov-age cell has exactly {0, one 2022 value}) -> NO panel; the 2017-2021
# "years" are fake. Abandono & Matriculados DO vary 2022-2024 (verified) ->
# genuine 3-year panel, and intensity moves 2022->2024 (12 openings in
# 2023-24), so a DDD is identified there.
# DESIGNS:
#  (a) ABANDONO % DDD: valor ~ intensity(prov,yr) x teen(15-18)
#      | prov^anio + edad^anio, 2022-2024, cluster prov. Ages 10-12 = within-
#      province comparison; also excl. 13-14.
#  (b) MATRICULADOS (counts) same DDD on log(valor).
#  (c) Desercion 2022 cross-section: valor ~ int x teen | prov + edad
#      (descriptive only; no time variation).
# PREDICTIONS (rule 1b): if the fertility-mediated schooling channel is real,
# abandono interaction NEGATIVE small (teen abandono mean ~10%, effect maybe
# -0.5 to -2pp per full intensity), matriculados interaction POSITIVE small;
# honest expectation: null-ish, 3 years x 32 prov is thin. Desercion 2022
# cross-section: no prediction (level correlations = selection).
# Output: output/tables/one_educ_ddd.csv
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table); library(fixest)})
TAB <- file.path(PROJ,"output","tables")

ed <- as.data.table(readRDS(file.path(DIR_CLEAN,"one_educacion_province.rds")))
ed[, tab_ascii := fcase(grepl("Deserci", tab), "desercion", grepl("Abandono", tab), "abandono",
                        grepl("Matricul", tab), "matriculados")]
ip <- as.data.table(readRDS(file.path(DIR_CLEAN,"province_au_intensity.rds")))
ed <- merge(ed, ip[, .(prov_code, anio=year, int=intensity)], by=c("prov_code","anio"))
ed[, teen := as.integer(edad >= 15)]

# verify the padding finding holds before proceeding
de <- ed[tab_ascii=="desercion"]
stopifnot(de[anio < 2022, all(valor==0)], de[anio==2022, mean(valor>0)] > .5)
cat("CONFIRMED: Desercion pre-2022 all zeros (padding); 2022 real. Using 2022 cross-section only.\n")

ab <- ed[tab_ascii=="abandono"]; ma <- ed[tab_ascii=="matriculados"]
cat("abandono panel:", nrow(ab), "rows | teen mean", ab[teen==1, round(mean(valor),2)],
    "| child(10-12) mean", ab[edad<=12, round(mean(valor),2)], "\n")
m1 <- feols(valor ~ int:teen | prov_code^anio + edad^anio, ab, cluster=~prov_code)
m2 <- feols(valor ~ int:teen | prov_code^anio + edad^anio, ab[!edad %in% 13:14], cluster=~prov_code)
m3 <- feols(log(valor) ~ int:teen | prov_code^anio + edad^anio, ma, cluster=~prov_code)
m4 <- feols(valor ~ int:teen | prov_code + edad, de[anio==2022], cluster=~prov_code)

g <- function(m, lab){ct <- coeftable(m); i <- grep("teen", rownames(ct))[1]
  data.table(spec=lab, coef=round(ct[i,1],4), se=round(ct[i,2],4), p=round(ct[i,4],3), n=nobs(m))}
res <- rbind(g(m1,"Abandono % DDD 2022-24: int x teen"),
             g(m2,"Abandono % DDD excl. 13-14"),
             g(m3,"log Matriculados DDD 2022-24: int x teen"),
             g(m4,"Desercion 2022 cross-section: int x teen (descriptive)"))
cat("\n=== ONE education DDD (prov x year + age x year FE, cluster prov) ===\n")
print(res, class=FALSE)
fwrite(res, file.path(TAB,"one_educ_ddd.csv"))
cat("saved -> one_educ_ddd.csv\n")
