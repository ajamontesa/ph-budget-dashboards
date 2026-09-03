suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(stringr); library(purrr); library(tidyr)
})
source("R/load_data.R")
PAP <- load_pap_data()

cat("series:", paste(PAP$series, collapse = " "), "\n\n")

# --- column selection logic, mirroring the server ---------------------
pick <- function(doc, yrs, cls) {
  s <- PAP$series
  if (doc != "BOTH") s <- s[str_starts(s, doc)]
  s <- s[str_extract(s, "\\d{4}") %in% yrs]
  cols <- as.vector(t(outer(s, cls, amt_col)))
  cols[cols %in% PAP$amt_cols]
}

cat("BOTH / 2026-2027 / TOTAL:\n"); print(pick("BOTH", c("2026","2027"), "TOTAL"))
cat("\nGAA / 2025-2026 / TOTAL+1PS:\n"); print(pick("GAA", c("2025","2026"), c("TOTAL","1PS")))
cat("\nNEP / 2027 / all classes:\n"); print(pick("NEP", "2027", unname(EXP_CLASSES)))
cat("\nGAA / 2027 (should be empty - no enacted GAA):\n"); print(pick("GAA", "2027", "TOTAL"))

# --- filter logic -----------------------------------------------------
d <- PAP$data
cat("\n--- filters ---\n")
cat("all rows:", nrow(d), "\n")
doh <- d %>% filter(UACS_DPT_DSC == "Department of Health (DOH)")
cat("DOH:", nrow(doh), "| agencies:", n_distinct(doh$UACS_AGY_DSC), "\n")
lfp <- d %>% filter(PAP_TYPE == "Locally-Funded Project")
cat("Locally-Funded:", nrow(lfp), "\n")
sch <- d %>% filter(str_detect(PAP, fixed("school building", ignore_case = TRUE)))
cat("PAP search 'school building':", nrow(sch), "\n")
irr <- d %>% filter(str_detect(PROGRAM, fixed("irrigation", ignore_case = TRUE)))
cat("PROGRAM search 'irrigation':", nrow(irr), "\n")

# --- unit conversion --------------------------------------------------
cat("\n--- units (DOE NEP 2027 total) ---\n")
doe <- d %>% filter(str_detect(UACS_DPT_DSC, "Energy"))
v <- sum(doe$NEP_2027_EXP_TOTAL, na.rm = TRUE)
for (u in UNIT_CHOICES) {
  cat(sprintf("  %-20s %s\n", names(UNIT_CHOICES)[UNIT_CHOICES == u],
              format(v / (as.numeric(u) / SOURCE_UNIT), big.mark = ",", scientific = FALSE)))
}
cat("  (verified DBM figure: 2,028,303,000 pesos)\n")

# --- trend aggregation, all-NA guard ----------------------------------
cat("\n--- trend for DPWH ---\n")
dp <- d %>% filter(str_detect(UACS_DPT_DSC, "Public Works"))
tot <- tibble(series = PAP$series) %>%
  mutate(DOC = str_extract(series, "^[A-Z]+"),
         FY  = as.integer(str_extract(series, "\\d{4}")),
         value = map_dbl(series, function(ss) {
           v <- dp[[amt_col(ss, "TOTAL")]]
           if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE) / 1e9
         }))
print(as.data.frame(tot %>% mutate(value = round(value, 1))), row.names = FALSE)
cat("\nNA rows dropped from plot:", sum(is.na(tot$value)), "\n")
