## =====================================================================
## PH BUDGET P/A/P BROWSER -- DATA LOADING
##
## Reads Compiled_-_PAPs.xlsx from the ph-budget-analysis repository and
## prepares it for the dashboard. Sourced by app.R.
## =====================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(purrr); library(tibble)
})

DATA_URL <- paste0("https://raw.githubusercontent.com/ajamontesa/",
                   "ph-budget-analysis/main/data/Compiled_-_PAPs.xlsx")

DATA_SHEETS <- c("NGAs", "DPWH-sub")

## Local copy used in preference to the download, so the app still starts
## if GitHub is unreachable and so local edits can be tested before pushing.
LOCAL_COPY <- "data/Compiled_-_PAPs.xlsx"

## ---------------------------------------------------------------------
## UNITS
##
## The workbook stores PESOS, not thousands. Verified against figures
## checked line-by-line against the published DBM documents:
##
##   DOE            NEP 2027   2,028,303,000
##   DOE            GAA 2026   2,963,524,000
##   National Museum NEP 2027  1,530,502,000
##   Natl Maritime Polytechnic NEP 2021  132,094,000
##
## All four match the workbook exactly in pesos. If the workbook is ever
## rescaled to thousands, set SOURCE_UNIT to 1e3 and nothing else changes.
## ---------------------------------------------------------------------
SOURCE_UNIT <- 1        # one stored unit = 1 peso

UNIT_CHOICES <- c("Pesos"               = "1",
                  "Thousands of pesos"  = "1e3",
                  "Millions of pesos"   = "1e6",
                  "Billions of pesos"   = "1e9")

UNIT_LABEL <- c("1"   = "\u20b1",
                "1e3" = "\u20b1'000",
                "1e6" = "\u20b1M",
                "1e9" = "\u20b1B")

EXP_CLASSES <- c("Total"                                    = "TOTAL",
                 "Personnel Services"                       = "1PS",
                 "Maintenance and Other Operating Expenses" = "2MOOE",
                 "Financial Expenses"                       = "3FE",
                 "Capital Outlays"                          = "6CO")

## Project-type prefixes. The source varies between singular and plural
## ("Locally-Funded Project:" vs "Locally-Funded Projects:"), sometimes
## omits the noun ("Locally-Funded:"), and contains three rows under the
## PNP spelled "Locally-Funed Project:". These patterns absorb all of it.
RX_LFP <- regex("^\\s*locally[-\\s]*fun[a-z]*ed", ignore_case = TRUE)
RX_FAP <- regex("^\\s*foreign[-\\s]*assisted", ignore_case = TRUE)

ID_COLS <- c("DEPARTMENT", "UACS_DPT_DSC", "AGENCY", "UACS_AGY_DSC",
             "PREXC_PROG", "PROGRAM", "PREXC_SUBPROG", "PAP")


## ---------------------------------------------------------------------
## Blanks are not zeroes.
##
## Order matters here:
##
##   1. Drop P/A/Ps that have not been labelled yet.
##   2. Fill NA with 0 across every expense column. This makes the
##      year-level test below unambiguous -- otherwise `all(x == 0)`
##      returns NA whenever a year mixes zeroes and blanks, and the
##      result depends on which cells happen to be empty in the source.
##   3. Drop rows that are zero in EVERY expense column of EVERY year.
##      Such a row carries no information in any document or any year.
##   4. Only then apply the per-year rule: where every expense column for
##      a fiscal year is zero, that year carries no information for this
##      P/A/P and is set back to NA so it renders blank rather than as a
##      real zero.
##
## The year group spans BOTH documents, so NEP and GAA for a year are
## blanked together. FY2027 has no GAA, so its group is NEP-only.
## ---------------------------------------------------------------------
blank_zero_years <- function(d, years) {
  for (y in years) {
    cols <- names(d)[str_detect(names(d), paste0("_", y, "_EXP_"))]
    if (!length(cols)) next
    ## after the NA fill there are no NAs left, so this test is unambiguous
    flag <- rowSums(as.matrix(d[cols]) != 0) == 0
    d[cols] <- lapply(d[cols], function(v) if_else(flag, NA_real_, v))
  }
  d
}


## ---------------------------------------------------------------------
## Load and prepare
## ---------------------------------------------------------------------
load_pap_data <- function(url = DATA_URL, local = LOCAL_COPY) {

  path <- if (file.exists(local)) {
    message("reading local copy: ", local)
    local
  } else {
    tmp <- tempfile(fileext = ".xlsx")
    message("downloading: ", url)
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    tmp
  }

  present <- excel_sheets(path)
  use <- intersect(DATA_SHEETS, present)
  if (!length(use)) stop("none of the expected sheets found in ", path)
  if (length(use) < length(DATA_SHEETS))
    warning("missing sheet(s): ", str_c(setdiff(DATA_SHEETS, present), collapse = ", "))

  raw <- map_dfr(use, function(s) {
    read_xlsx(path, sheet = s, col_types = "text") %>% mutate(SHEET = s)
  })

  missing_id <- setdiff(ID_COLS, names(raw))
  if (length(missing_id))
    stop("missing identifier column(s): ", str_c(missing_id, collapse = ", "))

  amt_cols <- names(raw)[str_detect(names(raw), "_EXP_")]
  if (!length(amt_cols)) stop("no amount columns matching '_EXP_' found")

  raw <- raw %>% mutate(across(all_of(amt_cols), ~ suppressWarnings(as.numeric(.x))))

  years <- sort(unique(str_extract(amt_cols, "(?<=_)\\d{4}(?=_EXP_)")))

  dat <- raw %>%
    ## 1. Drop P/A/Ps that have not been labelled yet.
    filter(!is.na(PAP)) %>%
    ## 2. Blanks become zeroes so the year test below is unambiguous.
    mutate(across(all_of(amt_cols), ~ tidyr::replace_na(.x, 0))) %>%
    ## 3. Drop rows with nothing anywhere, in any year or document.
    filter(if_any(all_of(amt_cols), ~ .x != 0)) %>%
    ## 4. Re-blank whole years that are entirely zero.
    blank_zero_years(years) %>%
    mutate(
      PROGRAM  = coalesce(PROGRAM, "(program not labelled)"),
      PAP_TYPE = case_when(str_detect(PAP, RX_LFP) ~ "Locally-Funded Project",
                           str_detect(PAP, RX_FAP) ~ "Foreign-Assisted Project",
                           TRUE                    ~ "Regular Activity"),
      ## Statutory order: never alphabetise by description.
      SORT_KEY = str_c(DEPARTMENT, AGENCY, PREXC_PROG,
                       str_pad(PREXC_SUBPROG, 12, "right", "0"))
    ) %>%
    arrange(SORT_KEY)

  ## The series actually present, in fiscal order, NEP before GAA.
  series <- str_remove(amt_cols[str_ends(amt_cols, "_EXP_TOTAL")], "_EXP_TOTAL")
  series <- series[order(str_extract(series, "\\d{4}"),
                         match(str_extract(series, "^[A-Z]+"), c("NEP", "GAA")))]

  list(
    data     = dat,
    years    = years,
    series   = series,
    docs     = unique(str_extract(series, "^[A-Z]+")),
    amt_cols = amt_cols,
    depts    = sort(unique(dat$UACS_DPT_DSC)),
    n_raw    = nrow(raw),
    n_used   = nrow(dat)
  )
}


## Column name for a series and expense class, e.g. "NEP_2027_EXP_TOTAL".
amt_col <- function(series, class) str_c(series, "_EXP_", class)

## Pretty header, e.g. "NEP 2027 - Total".
amt_label <- function(series, class) {
  cls <- names(EXP_CLASSES)[match(class, EXP_CLASSES)]
  short <- c("Total" = "Total", "Personnel Services" = "PS",
             "Maintenance and Other Operating Expenses" = "MOOE",
             "Financial Expenses" = "FE", "Capital Outlays" = "CO")[cls]
  str_c(str_replace(series, "_", " "), " \u2014 ", short)
}
