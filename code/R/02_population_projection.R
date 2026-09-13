# ============================================================================
# 02_population_projection.R  —  Municipio population by age x sex x year
#
# Part A: parse the 155 municipio sheets (ONE estimates 2015-2020) -> tidy long.
# Part B: parse 2022 census microdata -> municipio x age x sex counts.
# Part C: map census (prov_code, muni_code) -> adm3_pcode and VALIDATE the ONE
#         municipio-code ordering (sheet order vs census code) by population.
# Part D: parse province controls (2000-2030).
# Part E: Hamilton-Perry projection + IPF raking to province controls + back-test.
#
# Canonical key = adm3_pcode (from data/clean/muni_crosswalk.rds).
# ============================================================================

source(here::here("code", "R", "00_config.R"))
suppressPackageStartupMessages({ library(readxl); library(data.table); library(stringi) })
norm_name <- function(x){ x<-stringi::stri_trans_general(as.character(x),"Latin-ASCII"); x<-toupper(x)
  x<-gsub("[^A-Z0-9 ]"," ",x); x<-gsub("\\s+"," ",x); trimws(x) }

xw <- as.data.table(readRDS(file.path(DIR_CLEAN, "muni_crosswalk.rds")))
muni_sheets <- xw[!is.na(xlsx_sheet), .(adm3_pcode, prov_code, xlsx_sheet, xlsx_idx)]
AGE_LEVELS <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")

# age label -> canonical ("0- 4"->"0-4", "80 y más"->"80+"); NA if not an age row
age_canon <- function(x){
  a <- gsub("\\s","", as.character(x))                       # drop whitespace
  a <- stringi::stri_trans_general(a, "Latin-ASCII")
  out <- ifelse(grepl("^80", a), "80+",
         ifelse(grepl("^[0-9]{1,2}-[0-9]{1,2}$", a), a, NA_character_))
  # SOURCE TYPO FIX: ONE's Baoruco province sheet (Mujeres) mislabels the 10-14 row as
  # "0-14" (sits between 5-9 and 15-19). "0-14" is not a valid 5-yr band -> it is 10-14.
  out <- ifelse(out == "0-14", "10-14", out)
  out
}

# ---- Part A: parse one municipio sheet -> long (sex, age_group, year, pop) ----
parse_pop_sheet <- function(sheet, pcode){
  d <- as.data.frame(read_excel(RAW$pop_muni_xlsx, sheet = sheet,
                                col_names = FALSE, .name_repair = "minimal"))
  lab <- as.character(d[[1]])
  labn <- norm_name(lab)
  # locate year header row: cols 2-7 equal 2015..2020
  yr_row <- which(sapply(seq_len(nrow(d)), function(i){
    v <- suppressWarnings(as.numeric(unlist(d[i, 2:7]))); isTRUE(all(v == 2016:2021 - 1))
  }))
  stopifnot(length(yr_row) == 1)
  years <- 2015:2020
  recs <- list(); cur_sex <- NA_character_
  for (i in seq_len(nrow(d))){
    if (labn[i] %in% c("AMBOS SEXOS","HOMBRES","MUJERES")) { cur_sex <- labn[i]; next }
    ag <- age_canon(lab[i])
    if (!is.na(ag) && !is.na(cur_sex)){
      vals <- suppressWarnings(as.numeric(unlist(d[i, 2:7])))
      recs[[length(recs)+1]] <- data.table(adm3_pcode=pcode, sex=cur_sex,
                                            age_group=ag, year=years, pop=vals)
    }
  }
  rbindlist(recs)
}

cat("Parsing", nrow(muni_sheets), "municipio sheets ...\n")
poplist <- vector("list", nrow(muni_sheets)); val <- vector("list", nrow(muni_sheets))
for (k in seq_len(nrow(muni_sheets))){
  s <- muni_sheets$xlsx_sheet[k]; pc <- muni_sheets$adm3_pcode[k]
  p <- parse_pop_sheet(s, pc); poplist[[k]] <- p
  # per-sheet checks: 17 ages x 3 sexes x 6 yrs; Ambos == H + M; no NA
  w <- dcast(p, age_group + year ~ sex, value.var = "pop")
  val[[k]] <- data.table(sheet=s, pcode=pc, nrows=nrow(p),
                         n_age=uniqueN(p$age_group), n_sex=uniqueN(p$sex),
                         anyNA=anyNA(p$pop),
                         ambos_ok=all(abs(w$`AMBOS SEXOS` - (w$HOMBRES + w$MUJERES)) < 1e-6))
}
pop_muni <- rbindlist(poplist)
vchk <- rbindlist(val)
# Recode sexes. NOTE: 2 sheets (Las Terrenas, Cotuí) lack the "Ambos sexos" LABEL row,
# so their total block isn't parsed; Hombres+Mujeres are complete in all 155 sheets.
# We keep only M/F (irreducible) and DERIVE totals; cross-check T==M+F where T present.
pop_muni[, sex := fcase(sex=="HOMBRES","M", sex=="MUJERES","F", sex=="AMBOS SEXOS","T")]
chk_t <- dcast(pop_muni[sex %in% c("T","M","F")], adm3_pcode+age_group+year ~ sex, value.var="pop")
chk_t <- chk_t[!is.na(T)]   # only sheets that reported a total
cat("\n--- Part A validation ---\n")
cat("sheets parsed:", nrow(vchk), " | sheets reporting Ambos total:", uniqueN(
    pop_muni[sex=="T", adm3_pcode]), "(expect 153; 2 lack the label)\n")
cat("all sheets have 17 ages:", all(vchk$n_age==17), " | M & F present all sheets:",
    all(pop_muni[, .(ok = all(c("M","F") %in% sex)), by=adm3_pcode]$ok), "\n")
cat("any NA pop:", anyNA(pop_muni$pop),
    " | T == M+F on all reporting rows:", all(abs(chk_t$T - (chk_t$M+chk_t$F)) < 1e-6), "\n")
# spot-check Azua F 15-19 2015 == 4600 (from manual inspection of the sheet)
cat("check Azua F 15-19 2015 == 4600: ",
    pop_muni[adm3_pcode==xw[adm3_name=="AZUA", adm3_pcode] & sex=="F" &
             age_group=="15-19" & year==2015, pop] == 4600, "\n")
# keep only the irreducible M/F long table
pop_muni <- pop_muni[sex %in% c("M","F")][order(adm3_pcode, sex, factor(age_group, AGE_LEVELS), year)]
cat("final pop_muni rows (M/F only):", nrow(pop_muni),
    " (expect 155*2*17*6 =", 155*2*17*6, ")\n")
saveRDS(pop_muni, file.path(DIR_CLEAN, "pop_muni_2015_2020_long.rds"))
cat("saved -> data/clean/pop_muni_2015_2020_long.rds\n")

# ---- Part B: parse 2022 census microdata -> (prov_code,muni_code) x sex x age ----
# Verified from Persona codebook: P26_SEXO 1=Hombre,2=Mujer; P27_EDAD=completed years,
# 0="Menos de 1"; 999=unknown age (excluded from age groups, count reported).
cat("\n--- Part B: 2022 census ---\n")
cen <- fread(RAW$census_csv, select = c("PROVINCIA","MUNICIPIO","P26_SEXO","P27_EDAD"),
             showProgress = FALSE)
cen[, sex := fifelse(P26_SEXO == 1L, "M", "F")]
n_unknown_age <- cen[P27_EDAD == 999, .N]
cat("rows:", format(nrow(cen),big.mark=","), " | unknown-age (999):",
    format(n_unknown_age,big.mark=","), sprintf("(%.3f%%)", 100*n_unknown_age/nrow(cen)), "\n")
cen <- cen[P27_EDAD != 999]
# 5-year age groups; >=80 -> "80+"; labels must match AGE_LEVELS exactly
cen[, age_group := fifelse(P27_EDAD >= 80, "80+",
                           paste0(5L*(P27_EDAD %/% 5L), "-", 5L*(P27_EDAD %/% 5L) + 4L))]
stopifnot(all(cen$age_group %in% AGE_LEVELS))
census22 <- cen[, .(n = .N), by = .(prov_code = PROVINCIA, muni_code = MUNICIPIO, sex, age_group)]
# validation
cat("census municipios (prov,muni pairs):", uniqueN(census22[, .(prov_code, muni_code)]),
    "(expect 158)\n")
cat("total placed pop:", format(sum(census22$n),big.mark=","),
    " + unknown =", format(sum(census22$n)+n_unknown_age,big.mark=","),
    " (raw rows", format(nrow(cen)+n_unknown_age,big.mark=","), ")\n")
cat("national F 15-19 (2022 census):", format(census22[sex=="F" & age_group=="15-19", sum(n)],big.mark=","),
    " | F 30-34:", format(census22[sex=="F" & age_group=="30-34", sum(n)],big.mark=","), "\n")
saveRDS(census22, file.path(DIR_CLEAN, "census2022_muni_code_long.rds"))
cat("saved -> data/clean/census2022_muni_code_long.rds (keyed by ONE prov/muni CODES; ",
    "mapped to adm3_pcode in Part C)\n", sep="")

# ---- Part C: map census (prov_code,muni_code) -> adm3_pcode (EXACT via pcode encoding) ----
# The OCHA ADM3 pcode encodes ONE's codes exactly: substr(pcode,6,7)=ONE province code,
# substr(pcode,8,9)=ONE municipio code. VERIFIED (audit 2026-06-19): the 158 pcode-encoded
# (prov,muni) pairs equal the 158 census (prov,muni) pairs EXACTLY (set-equal, 0 diffs) and
# match xw$prov_code. So this is an EXACT join — NO sheet-order or population assumption.
# (An earlier population-matching heuristic agreed with this for all 158, incl. the Santo
# Domingo Pedro Brand/Los Alcarrizos case — kept now only as the sanity ratio below.)
#
# THE RATIO BELOW IS A SANITY CHECK, NOT A DATA-QUALITY TARGET, and is NOT expected to be 1:
# numerator = 2022 census head-count; denominator = ONE's 2020 *projection* (from the 2010
# census). ratio != 1 reflects 2020->2022 change + projection-vs-census error + migration.
# National: 2020 est 10,448,499 -> 2022 census 10,773,879 (1.031, pop-wtd); median muni ~1.06.
# Full write-up: notes/population_projection_methods.md.
cat("\n--- Part C: census-code <-> adm3_pcode (exact pcode encoding) ---\n")
code2pcode <- xw[, .(adm3_pcode,
                     prov_code = as.integer(substr(adm3_pcode, 6, 7)),
                     muni_code = as.integer(substr(adm3_pcode, 8, 9)))]
stopifnot(nrow(code2pcode) == 158, uniqueN(code2pcode$adm3_pcode) == 158)
cen_pairs <- unique(census22[, .(prov_code, muni_code)])
stopifnot(nrow(fsetdiff(cen_pairs, code2pcode[, .(prov_code, muni_code)])) == 0,
          nrow(fsetdiff(code2pcode[, .(prov_code, muni_code)], cen_pairs)) == 0)
stopifnot(all(code2pcode$prov_code == xw$prov_code[match(code2pcode$adm3_pcode, xw$adm3_pcode)]))
cat("exact census<->pcode mapping confirmed for all 158 municipios\n")

# build final census 2022 keyed by adm3_pcode
census22_pc <- merge(census22, code2pcode, by = c("prov_code","muni_code"), all.x = TRUE)
stopifnot(sum(is.na(census22_pc$adm3_pcode)) == 0)
census22_pc <- census22_pc[, .(pop = sum(n)), by = .(adm3_pcode, sex, age_group)][, year := 2022L]
cat("final census22 by adm3_pcode: municipios =", uniqueN(census22_pc$adm3_pcode),
    " total pop =", format(sum(census22_pc$pop), big.mark=","), "\n")

# sanity ratio: a CORRECT mapping yields ~1 (see header). 3 post-2020 municipios have no 2020 data.
ratio_chk <- merge(pop_muni[year==2020, .(xpop2020=sum(pop)), by=adm3_pcode],
                   census22_pc[, .(cpop2022=sum(pop)), by=adm3_pcode], by="adm3_pcode")
ratio_chk[, ratio := cpop2022/xpop2020]
cat("sanity ratio (155 municipios w/ 2020 data): "); print(round(summary(ratio_chk$ratio),3))
cat("ratios <0.7 or >1.5 (none expected):", sum(ratio_chk$ratio<0.7 | ratio_chk$ratio>1.5), "\n")
if (any(ratio_chk$ratio<0.7|ratio_chk$ratio>1.5)) print(ratio_chk[ratio<0.7|ratio>1.5])
cat("3 new (post-2020) municipios present in census22_pc:",
    all(xw[in_xlsx_2015_2020==FALSE, adm3_pcode] %in% census22_pc$adm3_pcode), "\n")
saveRDS(code2pcode, file.path(DIR_CLEAN, "census_code_to_adm3.rds"))
saveRDS(census22_pc, file.path(DIR_CLEAN, "census2022_by_adm3_long.rds"))
cat("saved -> data/clean/census_code_to_adm3.rds + census2022_by_adm3_long.rds\n")

# ---- Part D: province controls 2000-2030 (IPF raking targets) ----
cat("\n--- Part D: province controls 2000-2030 ---\n")
# generalized label-based parser (auto-detects the year-header row; handles 6- or 31-yr sheets)
parse_age_sex_sheet <- function(path, sheet){
  d <- as.data.frame(read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal"))
  lab <- as.character(d[[1]]); labn <- norm_name(lab)
  yr_row <- NA_integer_
  for (i in seq_len(nrow(d))){
    v <- suppressWarnings(as.numeric(unlist(d[i, -1]))); yrs <- v[!is.na(v)]
    if (length(yrs) >= 2 && all(yrs >= 1990 & yrs <= 2040) && all(diff(yrs) == 1)) { yr_row <- i; break }
  }
  stopifnot(!is.na(yr_row))
  yvals <- suppressWarnings(as.numeric(unlist(d[yr_row, ]))); ycols <- which(!is.na(yvals) & yvals>=1990 & yvals<=2040)
  years <- yvals[ycols]
  recs <- list(); cur_sex <- NA_character_
  for (i in seq_len(nrow(d))){
    if (labn[i] %in% c("AMBOS SEXOS","HOMBRES","MUJERES")){ cur_sex <- labn[i]; next }
    ag <- age_canon(lab[i])
    if (!is.na(ag) && !is.na(cur_sex)){
      vals <- suppressWarnings(as.numeric(unlist(d[i, ycols])))
      recs[[length(recs)+1]] <- data.table(sex=cur_sex, age_group=ag, year=years, pop=vals)
    }
  }
  rbindlist(recs)
}

# identify the 32 province sheets (exclude Total País and "Región *") and map to prov_code
psheets <- excel_sheets(RAW$pop_prov_xlsx)
psheets <- psheets[!(norm_name(psheets) %in% "TOTAL PAIS") & !grepl("^REGION", norm_name(psheets))]
prov_lookup <- unique(xw[, .(prov_code, prov_name, prov_norm = norm_name(prov_name))])
prov_sheet_alias <- c("MA TRINIDAD SANCHEZ" = "MARIA TRINIDAD SANCHEZ")  # sheet abbreviates "María"
psheet_dt <- data.table(sheet = psheets, snorm = norm_name(psheets))
psheet_dt[snorm %in% names(prov_sheet_alias), snorm := prov_sheet_alias[snorm]]
psheet_dt <- merge(psheet_dt, prov_lookup, by.x = "snorm", by.y = "prov_norm", all.x = TRUE)
cat("province sheets:", length(psheets), " mapped to prov_code:", sum(!is.na(psheet_dt$prov_code)),
    " UNMATCHED:", sum(is.na(psheet_dt$prov_code)), "\n")
if (any(is.na(psheet_dt$prov_code))) print(psheet_dt[is.na(prov_code)])
stopifnot(sum(is.na(psheet_dt$prov_code)) == 0, uniqueN(psheet_dt$prov_code) == 32)

prov_ctrl <- rbindlist(lapply(seq_len(nrow(psheet_dt)), function(k){
  p <- parse_age_sex_sheet(RAW$pop_prov_xlsx, psheet_dt$sheet[k])
  p[, prov_code := psheet_dt$prov_code[k]]; p
}))
prov_ctrl[, sex := fcase(sex=="HOMBRES","M", sex=="MUJERES","F", sex=="AMBOS SEXOS","T")]
# keep M/F (derive totals); validate T==M+F where present
ct <- dcast(prov_ctrl, prov_code+age_group+year ~ sex, value.var="pop")[!is.na(T)]
cat("province parse: rows", nrow(prov_ctrl), " years", min(prov_ctrl$year),"-",max(prov_ctrl$year),
    " | T==M+F:", all(abs(ct$T-(ct$M+ct$F))<1e-6), "\n")
prov_ctrl <- prov_ctrl[sex %in% c("M","F")]

# CROSS-CHECK: province totals 2015-2020 == sum of their municipios (Part A) ?
muni_byprov <- merge(pop_muni, xw[, .(adm3_pcode, prov_code)], by="adm3_pcode")[
  , .(muni_sum = sum(pop)), by=.(prov_code, sex, age_group, year)]
cmp <- merge(prov_ctrl[year %in% 2015:2020], muni_byprov,
             by=c("prov_code","sex","age_group","year"))
cmp[, d := pop - muni_sum]
cat("cross-check province vs sum(municipios) 2015-2020: max |diff| =", max(abs(cmp$d)),
    " | rows compared:", nrow(cmp), "\n")
if (max(abs(cmp$d))>0) { cat("provinces with any nonzero diff:\n")
  print(cmp[d!=0, .(maxabs=max(abs(d)), n=.N), by=prov_code][order(-maxabs)]) }
saveRDS(prov_ctrl, file.path(DIR_CLEAN, "prov_controls_2000_2030_long.rds"))
cat("saved -> data/clean/prov_controls_2000_2030_long.rds (prov_code,sex,age_group,year,pop)\n")
