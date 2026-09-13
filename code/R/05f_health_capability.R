# ============================================================================
# 05f_health_capability.R — municipio-level HEALTH-SYSTEM CAPABILITY index from the
# SNS registry (structural capability, the closest proxy to "quality" we have — NOT
# accreditation/outcomes/patient-satisfaction). Complements the QUANTITY measure
# (sns_per10k, 05d). Same SNS-only filter + ONE-code municipio join + fold-to-155.
#
# Components per municipio (SNS-administered centers):
#   n_specialized        = # NIVEL ESPECIALIZADO centers (hospitals)
#   specialized_per10k   = specialized centers per 10,000 pop (2022)
#   diag_share           = share of SNS centers with >=1 diagnostic capability
#                          (lab | ultrasound | X-ray)  [recorded mainly for primary level]
#   emerg_share          = share of SNS centers with an emergency module
#   capability_index     = mean of z-scores(specialized_per10k, diag_share, emerg_share)
# CAVEAT: the PNA_* equipment flags are populated mainly for PRIMER NIVEL centers, so
# the index combines facility LEVEL (specialized count) with primary-care diagnostic
# richness; it under-captures equipment inside hospitals. Documented, not hidden.
# Output: data/clean/health_capability_muni.rds (155)
# ============================================================================
source(here::here("code","R","00_config.R"))
suppressPackageStartupMessages({library(data.table)})
HFILE <- file.path(dirname(RAW$pcua_xlsx), "Hospitals and PCU in DR",
                   "Hospitales-y-Centros-de-Primer-Nivel-de-Atencion-act-09-04-2026.xlsx")
stopifnot(file.exists(HFILE))
fold <- c(DOM010905="DOM010901", DOM051703="DOM051701", DOM012510="DOM012501")
to155 <- function(p) fifelse(p %in% names(fold), unname(fold[p]), p)

d  <- as.data.table(suppressMessages(readxl::read_excel(HFILE)))
xw <- as.data.table(readRDS(file.path(DIR_CLEAN,"muni_crosswalk.rds")))
xw[, `:=`(pc_prov=as.integer(substr(adm3_pcode,6,7)), pc_muni=as.integer(substr(adm3_pcode,8,9)))]

sns <- d[Administrada_Por=="SNS"]
sns <- merge(sns, xw[, .(pc_prov, pc_muni, adm3_pcode)],
             by.x=c("Id_Provincia","Id_Municipio"), by.y=c("pc_prov","pc_muni"), all.x=TRUE)
stopifnot(sns[is.na(adm3_pcode), .N] == 0)
sns[, adm3_pcode := to155(adm3_pcode)]

# capability flags (coerce missing -> 0; verified flags are 0/1 in the registry)
num0 <- function(x){ x <- suppressWarnings(as.integer(x)); fifelse(is.na(x), 0L, x) }
sns[, `:=`(is_spec  = as.integer(Nivel_atencion=="NIVEL ESPECIALIZADO"),
           lab=num0(PNA_Laboratorio), sono=num0(PNA_Sonografia),
           xray=num0(PNA_Rayox_X),    emerg=num0(PNA_Emergencia))]
sns[, diag_any := as.integer(lab|sono|xray)]

cap <- sns[, .(n_sns=.N, n_specialized=sum(is_spec),
               diag_share=mean(diag_any), emerg_share=mean(emerg)), by=adm3_pcode]

# population (2022) for per-capita, folded to 155 (same as 05d)
A <- as.data.table(readRDS(file.path(DIR_CLEAN,"pop_municipio_2016_2025_A.rds")))
A[, id := to155(adm3_pcode)]
pop2022 <- A[sex %in% c("F","M") & year==2022L, .(pop2022=sum(pop)), by=.(adm3_pcode=id)]
stopifnot(uniqueN(pop2022$adm3_pcode)==155)

cap <- merge(data.table(adm3_pcode=pop2022$adm3_pcode), cap, by="adm3_pcode", all.x=TRUE)
cap[is.na(n_sns), `:=`(n_sns=0L, n_specialized=0L, diag_share=0, emerg_share=0)]
cap <- merge(cap, pop2022, by="adm3_pcode")
cap[, specialized_per10k := 1e4*n_specialized/pop2022]

# composite: mean of z-scores of the 3 capability components
z <- function(x){ s<-sd(x); if(s==0) return(rep(0,length(x))); (x-mean(x))/s }
cap[, capability_index := rowMeans(cbind(z(specialized_per10k), z(diag_share), z(emerg_share)))]
stopifnot(nrow(cap)==155, !anyNA(cap$capability_index))

saveRDS(cap[, .(adm3_pcode, n_sns, n_specialized, specialized_per10k, diag_share, emerg_share,
                capability_index, pop2022)], file.path(DIR_CLEAN,"health_capability_muni.rds"))

cat("==================== 05f HEALTH CAPABILITY (155 muni) ====================\n")
cat("national: SNS centers", sum(cap$n_sns), "| specialized (hospitals)", sum(cap$n_specialized), "\n")
cat("municipios with >=1 specialized hospital:", cap[n_specialized>0,.N], "\n")
cat("diag_share summary (share of centers w/ lab|ultrasound|x-ray):\n"); print(round(summary(cap$diag_share),3))
cat("emerg_share summary:\n"); print(round(summary(cap$emerg_share),3))
cat("capability_index summary:\n"); print(round(summary(cap$capability_index),3))
cat("\nsaved -> data/clean/health_capability_muni.rds\n")
