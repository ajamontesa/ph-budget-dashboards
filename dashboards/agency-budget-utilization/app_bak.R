# =============================================================================
# Agency Budget & Utilization Dashboard
# PH Budget Dashboards — People's Budget Coalition
#
# Scope: NATIONAL AGENCY level. Departments, their bureaux and attached
# agencies, plus the Total Budget and Total NGAs aggregate blocks.
# A companion P/A/P-level dashboard is planned separately; it covers
# appropriations only, since DBM does not publish P/A/P execution data.
#
# Data source: public Google Sheet "PH Budget Data Set", tab NEP-GAA-All-Obl-Dis
# ALL FIGURES ARE IN THOUSANDS OF PESOS, exactly as published by DBM.
# No unit conversion is applied to the stored values; the display-unit selector
# only rescales what is shown, and every table/axis is labelled with its unit.
#
# Tabs
#   1. Budget Overview   whole-of-budget, driven by its own year controls
#   2. Agency Trends     driven by the sidebar department / agency filters
#   3. Key Indicators    indicators down the rows, years across the columns
#   4. Data Viewer       the full table
#   5. Notes             caveats that travel with the data
#
# Run locally with:  shiny::runApp("agency-budget-utilization")
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(DT)
library(scales)
library(readr)

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------

SHEET_ID  <- "1P3q46DGcN3SZ7cRwXfEhAMiDqCqQof1s3LV0-CK2lIY"
SHEET_TAB <- "NEP-GAA-All-Obl-Dis"

SHEET_URL <- paste0("https://docs.google.com/spreadsheets/d/", SHEET_ID)

# Canonical particulars. Internal keys are used throughout the pipeline;
# labels are what a reader sees in the Data Viewer and its filter.
#
# Appropriations come in two bases:
#   New   = new appropriations only, what Congress legislates for the year
#   Total = New + Automatic (RLIP, special accounts, debt service and so on)
# Total is always >= New for the same line.
PARTICULAR_KEYS <- c("NEP_new", "NEP_total", "GAA_new", "GAA_total",
                     "Allotments", "Obligations", "Disbursements")

PARTICULAR_LABELS <- c(
  NEP_new       = "NEP New Appropriations",
  NEP_total     = "NEP Total Appropriations",
  GAA_new       = "GAA New Appropriations",
  GAA_total     = "GAA Total Appropriations",
  Allotments    = "Allotments",
  Obligations   = "Obligations",
  Disbursements = "Disbursements"
)

# Key groups. Anything that needs "the NEP rows" or "the appropriations rows"
# refers to these rather than spelling out literals: a stale literal after the
# New/Total split is what silently emptied the budget-year selector once
# already, and a literal gives no error when it stops matching.
NEP_KEYS       <- c("NEP_new", "NEP_total")
GAA_KEYS       <- c("GAA_new", "GAA_total")
APPROP_KEYS    <- c(NEP_KEYS, GAA_KEYS)
EXECUTION_KEYS <- c("Allotments", "Obligations", "Disbursements")

# Appropriations basis. New is the default: it is what Congress actually
# legislates, so it is the fairer basis for comparing agencies and for reading
# what the legislature changed.
BASIS_CHOICES <- c("New Appropriations" = "new", "Total Appropriations" = "total")
BASIS_LABEL   <- c(new = "New Appropriations", total = "Total Appropriations")
BASIS_SHORT   <- c(new = "New", total = "Total")

# Lighter fills for the Total bar sitting behind the New bar
FILL_TOTAL_DEPT <- "#A9C4D3"
FILL_TOTAL_AGCY <- "#BFE1EB"

AGG_TOTAL_BUDGET <- "Total Budget"
AGG_TOTAL_NGAS   <- "Total National Government Agencies (NGAs)"

# Subjects for which percent-share panels are meaningless: they are the
# denominator, so every share is either 100% or undefined.
SHARE_SUPPRESSED <- c(AGG_TOTAL_BUDGET, AGG_TOTAL_NGAS)

# --- Cache -----------------------------------------------------------------
# How long a fetched copy of the sheet is reused before being refetched.
# Override at deploy time with PBC_AGENCY_CACHE_TTL (seconds). Namespaced per
# dashboard so a sibling app can be tuned independently.
CACHE_TTL_SECONDS <- as.numeric(Sys.getenv("PBC_AGENCY_CACHE_TTL", "3600"))

# How long to wait before retrying after a failed fetch. Short enough to
# recover quickly from a transient Google outage, long enough that a sustained
# one does not turn every page view into another doomed request.
CACHE_RETRY_SECONDS <- 120

# How often an open session checks whether another session has refreshed the
# shared cache. Long-lived tabs pick up new data without a manual reload.
CACHE_POLL_MS <- 5 * 60 * 1000

# --- Responsive behaviour --------------------------------------------------
# Below this viewport width the app switches to its narrow layout: fewer bars,
# heights computed from row count rather than fixed, tighter labels, and tables
# without frozen columns.
MOBILE_BREAKPOINT <- 768

# Rankings are trimmed on a phone. Thirty bars in a 360px-wide panel is not a
# smaller version of the desktop chart, it is an unreadable one.
N_TOP_BUDGET <- 30;  N_TOP_BUDGET_MOBILE <- 12
N_TOP_RATES  <- 15;  N_TOP_RATES_MOBILE  <- 8
N_TOP_CONG   <- 15;  N_TOP_CONG_MOBILE   <- 8

# Indicator registry. One place to add an indicator: it flows into the
# Key Indicators table, its formatting and its CSV export automatically.
IND_KEYS <- c("obl_rate", "dis_rate",
              "nep_sh_total", "gaa_sh_total",
              "nep_sh_ngas",  "gaa_sh_ngas",
              "nep_sh_dept",  "gaa_sh_dept",
              "nep_chg", "gaa_chg", "nep_vs_prev_gaa", "gaa_vs_nep")

IND_LABELS <- c("Obligation Rate", "Disbursement Rate",
                "NEP % of Total Budget", "GAA % of Total Budget",
                "NEP % of Total NGAs",   "GAA % of Total NGAs",
                "NEP % of Department",   "GAA % of Department",
                "NEP % Change", "GAA % Change",
                "NEP vs Prior GAA %", "GAA vs NEP %")

# Which indicators are directional, and so are shown with an explicit sign.
IND_SIGNED <- c(rep(FALSE, 8), rep(TRUE, 4))

# House palette
PBC_NAVY  <- "#1B4965"
PBC_BLUE  <- "#5FA8D3"
PBC_TEAL  <- "#62B6CB"
PBC_RUST  <- "#BC4749"
PBC_GREEN <- "#386641"
PBC_GREY  <- "#6C757D"

# ---------------------------------------------------------------------------
# 1. Helpers
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# Ordering within a facet: tag each label with its group so the two panels sort
# independently, then strip the tag back off at the axis. make.unique guards
# against agencies sharing a name across departments, which would otherwise
# produce duplicate factor levels and error out.
reorder_within_grp <- function(x, by, grp) {
  key <- make.unique(paste0(x, "___", grp))
  factor(key, levels = key[order(grp, by)])
}

# Division that refuses to produce Inf / NaN. 0 / 100 stays 0 (a reported zero
# is real information); 0 / 0 and x / 0 become NA (undefined, not zero).
safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "\u2014", sprintf(paste0("%.", digits, "f%%"), 100 * x))
}

fmt_pct_signed <- function(x, digits = 1) {
  ifelse(is.na(x), "\u2014", sprintf(paste0("%+.", digits, "f%%"), 100 * x))
}

fmt_amt <- function(x, scale_div = 1, digits = 0) {
  ifelse(is.na(x), "\u2014",
         formatC(x / scale_div, format = "f", big.mark = ",", digits = digits))
}

fmt_amt_signed <- function(x, scale_div = 1, digits = 0) {
  ifelse(is.na(x), "\u2014",
         formatC(x / scale_div, format = "f", big.mark = ",",
                 digits = digits, flag = "+"))
}

# Wrap long labels across lines. Deliberately does NOT truncate: an elided
# agency name is worse than a three-line one, since several agencies differ
# only in their tail ("...Research and Development Institute" vs "...Service").
wrap_lab <- function(x, width = 34) str_wrap(x, width = width)

UNIT_CHOICES <- c(
  "\u20b1 Thousands (as published)" = "thousands",
  "\u20b1 Millions"                 = "millions",
  "\u20b1 Billions"                 = "billions"
)

unit_divisor <- function(unit) switch(unit, thousands = 1, millions = 1e3, billions = 1e6, 1)
unit_label   <- function(unit) switch(unit, thousands = "\u20b1 '000", millions = "\u20b1 M",
                                      billions = "\u20b1 B", "\u20b1 '000")
unit_digits  <- function(unit) switch(unit, thousands = 0, millions = 1, billions = 2, 0)

# ---------------------------------------------------------------------------
# 2. Ingest
# ---------------------------------------------------------------------------

# Read the published sheet as CSV. Everything is read as character first so
# that we control numeric parsing ourselves: an empty cell must become NA and
# a literal "0" must stay 0. These two states are NOT interchangeable — the
# zeros are lifted straight from DBM's SAAODB and are true to the source.
fetch_sheet_raw <- function() {
  csv_url <- paste0(
    "https://docs.google.com/spreadsheets/d/", SHEET_ID,
    "/gviz/tq?tqx=out:csv&sheet=", utils::URLencode(SHEET_TAB, reserved = TRUE)
  )

  out <- try(
    readr::read_csv(csv_url,
                    col_types = readr::cols(.default = readr::col_character()),
                    na = character(), progress = FALSE),
    silent = TRUE
  )

  if (inherits(out, "try-error")) {
    if (!requireNamespace("googlesheets4", quietly = TRUE)) {
      stop("Could not read the sheet via CSV, and googlesheets4 is not installed. ",
           "Install it with install.packages('googlesheets4') for a fallback route.")
    }
    googlesheets4::gs4_deauth()
    out <- googlesheets4::read_sheet(SHEET_ID, sheet = SHEET_TAB, col_types = "c")
  }

  as.data.frame(out, stringsAsFactors = FALSE)
}

# Map a sheet PARTICULAR onto an internal key.
#
# The New/Total distinction is load-bearing and this matching is deliberately
# strict about it. A loose "starts with NEP" rule maps both
# "NEP New Appropriations" and "NEP Total Appropriations" onto one key, and the
# de-duplicating summarise downstream then ADDS them — every NEP figure
# silently inflated to New + Total, with no error raised anywhere.
#
# So qualified forms are matched explicitly; a bare "NEP"/"GAA" is read as the
# historical Total form for backward compatibility; and anything else starting
# with NEP or GAA returns NA rather than being guessed at. An unrecognised row
# disappearing is a visible failure. A silently doubled one is not.
normalize_particular <- function(x) {
  x <- str_squish(x)
  case_when(
    str_detect(x, regex("^NEP\\b.*\\bnew\\b",   ignore_case = TRUE)) ~ "NEP_new",
    str_detect(x, regex("^NEP\\b.*\\btotal\\b", ignore_case = TRUE)) ~ "NEP_total",
    str_detect(x, regex("^GAA\\b.*\\bnew\\b",   ignore_case = TRUE)) ~ "GAA_new",
    str_detect(x, regex("^GAA\\b.*\\btotal\\b", ignore_case = TRUE)) ~ "GAA_total",
    str_detect(x, regex("^NEP$|^NEP\\s+appropriations$", ignore_case = TRUE)) ~ "NEP_total",
    str_detect(x, regex("^GAA$|^GAA\\s+appropriations$", ignore_case = TRUE)) ~ "GAA_total",
    str_detect(x, regex("^Allot", ignore_case = TRUE)) ~ "Allotments",
    str_detect(x, regex("^Oblig", ignore_case = TRUE)) ~ "Obligations",
    str_detect(x, regex("^Disb",  ignore_case = TRUE)) ~ "Disbursements",
    TRUE ~ NA_character_
  )
}

# "" -> NA (not reported); "0" -> 0 (reported zero). Never interchangeable.
parse_amount <- function(x) {
  x <- str_squish(x)
  x[x %in% c("", "-", "--", "\u2014", "NA", "N/A", "n/a")] <- NA_character_
  x <- str_remove_all(x, "[,\u20b1\\s]")
  neg <- str_detect(x, "^\\(.*\\)$") & !is.na(x)
  x <- str_remove_all(x, "[()]")
  val <- suppressWarnings(as.numeric(x))
  val[neg] <- -val[neg]
  val
}

# Reshape the wide sheet into a tidy long frame.
#
# Sheet row order is captured as dept_ord / agency_ord and carried through the
# whole pipeline. The sheet follows the GAA's own structural sequence, so
# alphabetizing would destroy it; every table and filter sorts on these keys.
#
# `expense_class` and `period` are pinned at one level each. When PS/MOOE/FE/CO
# and quarterly utilization arrive, they come in as extra ROWS rather than
# forcing a rewrite of every reactive.
tidy_budget <- function(raw) {

  names(raw) <- str_squish(names(raw))

  find_col <- function(pattern) {
    hit <- which(str_detect(names(raw), regex(pattern, ignore_case = TRUE)))
    if (length(hit) == 0) NA_integer_ else hit[1]
  }
  col_dept <- find_col("^department$")
  col_agcy <- find_col("^agency$")
  col_part <- find_col("^particular")

  if (any(is.na(c(col_dept, col_agcy, col_part)))) {
    stop("Could not find DEPARTMENT / AGENCY / PARTICULAR columns in the sheet. ",
         "Found: ", paste(names(raw), collapse = " | "))
  }

  year_cols <- names(raw)[str_detect(names(raw), "^\\d{4}$")]
  if (length(year_cols) == 0) stop("No 4-digit year columns found in the sheet header.")

  df <- raw[, c(names(raw)[c(col_dept, col_agcy, col_part)], year_cols)]
  names(df)[1:3] <- c("department", "agency", "particular")

  keep <- vapply(df, function(col) any(!is.na(col) & str_squish(col) != ""), logical(1))
  df <- df[, keep, drop = FALSE]
  year_cols <- intersect(year_cols, names(df))

  df$department <- str_squish(df$department)
  df$agency     <- str_squish(df$agency)

  # Keep the raw label so anything unmatched can be reported: a new row type in
  # the sheet should surface as a visible warning, not vanish quietly.
  particular_raw <- str_squish(df$particular)
  df$particular  <- normalize_particular(particular_raw)
  unknown_particulars <- sort(unique(
    particular_raw[is.na(df$particular) & !is.na(particular_raw) & particular_raw != ""]
  ))

  df <- df %>%
    filter(!is.na(department), department != "",
           !is.na(agency),     agency != "",
           !is.na(particular))

  sheet_order <- df %>%
    mutate(.row = row_number()) %>%
    group_by(department, agency) %>%
    summarise(agency_ord = min(.row), .groups = "drop") %>%
    group_by(department) %>%
    mutate(dept_ord = min(agency_ord)) %>%
    ungroup() %>%
    select(department, agency, dept_ord, agency_ord)

  long <- df %>%
    pivot_longer(all_of(year_cols), names_to = "year", values_to = "amount_chr") %>%
    mutate(year = as.integer(year), amount = parse_amount(amount_chr)) %>%
    select(-amount_chr)

  # Aggregate blocks are department == agency AND a "Total ..." name. This
  # catches Total Budget, Total NGAs, and any Total GOCCs / LGUs / SPFs blocks
  # added later, without a hard-coded list.
  long <- long %>%
    mutate(
      level = case_when(
        str_detect(department, regex("^total\\b", ignore_case = TRUE)) &
          department == agency ~ "Aggregate",
        department == agency   ~ "Department",
        TRUE                   ~ "Agency"
      ),
      expense_class = "All",
      period        = "Annual"
    )

  # `level` is a single value per row, but membership is not exclusive. A
  # department with no separate bureau rows — DPWH, OP, OVP, DOE and others —
  # IS its own sole agency. Classifying it only as a department made it vanish
  # from every agency ranking, which quietly dropped some of the largest
  # spenders in the budget from the agency view.
  #
  # So membership is carried as two independent flags rather than inferred
  # from `level`. `level` is kept for display, since "Department" is still the
  # honest label for such a row.
  structure_flags <- long %>%
    distinct(department, agency, level) %>%
    group_by(department) %>%
    mutate(n_child_agencies = sum(level == "Agency")) %>%
    ungroup() %>%
    mutate(
      is_department = level == "Department",
      is_agency     = level == "Agency" |
                      (level == "Department" & n_child_agencies == 0)
    ) %>%
    select(department, agency, n_child_agencies, is_department, is_agency)

  long %>%
    group_by(department, agency, level, particular, expense_class, period, year) %>%
    summarise(
      amount = if (all(is.na(amount))) NA_real_ else sum(amount, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(sheet_order, by = c("department", "agency")) %>%
    left_join(structure_flags, by = c("department", "agency")) %>%
    select(department, agency, level, is_department, is_agency, n_child_agencies,
           dept_ord, agency_ord, particular, expense_class, period, year, amount) %>%
    structure(unknown_particulars = unknown_particulars)
}

# ---------------------------------------------------------------------------
# 3. Indicators
# ---------------------------------------------------------------------------

widen_budget <- function(long) {
  w <- long %>%
    select(department, agency, level, is_department, is_agency,
           dept_ord, agency_ord, year, particular, amount) %>%
    pivot_wider(names_from = particular, values_from = amount)

  for (p in PARTICULAR_KEYS) if (!p %in% names(w)) w[[p]] <- NA_real_

  yr <- range(long$year, na.rm = TRUE)
  w %>%
    group_by(department, agency, level, is_department, is_agency,
             dept_ord, agency_ord) %>%
    complete(year = seq(yr[1], yr[2])) %>%
    arrange(year, .by_group = TRUE) %>%
    ungroup()
}

# `basis` selects which appropriations series drives every NEP/GAA indicator:
# "new" (new appropriations only) or "total" (new + automatic). Both raw series
# are retained on the frame either way, because the largest-budgets chart draws
# them together regardless of which one the ranking uses.
build_indicators <- function(long, basis = "new") {

  w <- widen_budget(long)

  nep_col <- if (identical(basis, "new")) "NEP_new" else "NEP_total"
  gaa_col <- if (identical(basis, "new")) "GAA_new" else "GAA_total"
  w$NEP <- w[[nep_col]]
  w$GAA <- w[[gaa_col]]

  tot_budget <- w %>%
    filter(department == AGG_TOTAL_BUDGET) %>%
    select(year, tb_nep = NEP, tb_gaa = GAA) %>%
    distinct(year, .keep_all = TRUE)

  tot_ngas <- w %>%
    filter(department == AGG_TOTAL_NGAS) %>%
    select(year, tn_nep = NEP, tn_gaa = GAA) %>%
    distinct(year, .keep_all = TRUE)

  dept_tot <- w %>%
    filter(level == "Department") %>%
    select(department, year, dp_nep = NEP, dp_gaa = GAA) %>%
    distinct(department, year, .keep_all = TRUE)

  w %>%
    left_join(tot_budget, by = "year") %>%
    left_join(tot_ngas,   by = "year") %>%
    left_join(dept_tot,   by = c("department", "year")) %>%
    group_by(department, agency, level) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      # Both rates share Allotments as the denominator, so the gap between
      # them reads directly as the obligated-but-unpaid overhang.
      obl_rate = safe_div(Obligations,   Allotments),
      dis_rate = safe_div(Disbursements, Allotments),

      nep_sh_total = safe_div(NEP, tb_nep),
      gaa_sh_total = safe_div(GAA, tb_gaa),
      nep_sh_ngas  = safe_div(NEP, tn_nep),
      gaa_sh_ngas  = safe_div(GAA, tn_gaa),

      # Undefined for department and aggregate rows (they are the denominator),
      # so left as NA rather than a decorative 100%.
      nep_sh_dept = if_else(level == "Agency", safe_div(NEP, dp_nep), NA_real_),
      gaa_sh_dept = if_else(level == "Agency", safe_div(GAA, dp_gaa), NA_real_),

      nep_chg = safe_div(NEP, lag(NEP)) - 1,
      gaa_chg = safe_div(GAA, lag(GAA)) - 1,

      # The only change available for the newest NEP year, and the comparison
      # advocates actually argue over.
      nep_vs_prev_gaa = safe_div(NEP, lag(GAA)) - 1,

      # CONGRESSIONAL ADJUSTMENT, within the same fiscal year. Positive =
      # Congress augmented the line; negative = it was cut, usually to fund
      # augmentations elsewhere. Congress cannot raise the total, so these
      # largely net out across the budget.
      gaa_vs_nep     = safe_div(GAA, NEP) - 1,
      gaa_less_nep   = GAA - NEP,

      # A disbursement of exactly zero against non-zero obligations is faithful
      # to the SAAODB but almost certainly a non-submitted report. Used to keep
      # these rows out of the disbursement-rate rankings.
      zero_disb_flag = !is.na(Disbursements) & Disbursements == 0 &
                       !is.na(Obligations)   & Obligations   > 0
    ) %>%
    ungroup() %>%
    select(-tb_nep, -tb_gaa, -tn_nep, -tn_gaa, -dp_nep, -dp_gaa)
}

# Rows belonging to the requested view. Not `level == lvl`: a sole-agency
# department belongs to both.
at_level <- function(df, lvl) {
  if (identical(lvl, "Department")) filter(df, is_department) else filter(df, is_agency)
}

has_any_value <- function(df) {
  rowSums(!is.na(df[, PARTICULAR_KEYS, drop = FALSE])) > 0
}

# ---------------------------------------------------------------------------
# 4. Reference years
# ---------------------------------------------------------------------------

# The data frontier is ragged: NEP runs a year ahead of GAA, which runs a year
# ahead of execution. Never assume a single "latest year".
reference_years <- function(long) {
  yr_with <- function(p) {
    v <- long %>% filter(particular %in% p, !is.na(amount)) %>% pull(year)
    if (length(v) == 0) NA_integer_ else max(v)
  }
  # Either basis counts: the frontier is a property of the year, not of which
  # appropriations series the reader has selected. Keeping it stable across the
  # toggle also stops the year selectors jumping when the basis changes.
  list(nep   = yr_with(NEP_KEYS),
       gaa   = yr_with(GAA_KEYS),
       alloc = yr_with("Allotments"),
       obl   = yr_with("Obligations"),
       disb  = yr_with("Disbursements"))
}

# Years for which a given particular has any data at all.
years_with <- function(long, particulars) {
  long %>%
    filter(particular %in% particulars, !is.na(amount)) %>%
    pull(year) %>% unique() %>% sort()
}

# ---------------------------------------------------------------------------
# 4b. Shared cache
# ---------------------------------------------------------------------------

# One copy of the data per R process, shared by every concurrent session.
#
# Without this, each new visitor re-downloads the sheet and re-runs the full
# tidy-and-indicator pipeline. Ten concurrent readers meant ten identical
# downloads and ten identical computations: slow for them, wasteful of metered
# active hours, and a burst of requests at Google from a single IP.
#
# The environment lives at package scope rather than inside server(), so it
# persists across sessions. Shiny serves one session at a time within a
# process, so no locking is needed.
.cache <- new.env(parent = emptyenv())
.cache$long       <- NULL   # tidy long frame
.cache$ind_new    <- NULL   # indicators on the New Appropriations basis
.cache$ind_total  <- NULL   # indicators on the Total Appropriations basis
.cache$has_new    <- FALSE  # does the sheet actually carry New rows?
.cache$unknown    <- character(0)  # PARTICULAR labels the parser did not match
.cache$ref        <- NULL   # reference years
.cache$fetched_at <- NULL   # last SUCCESSFUL fetch
.cache$next_check <- NULL   # earliest time we should try again
.cache$last_error <- NULL   # message from the most recent failed attempt

cache_is_fresh <- function() {
  !is.null(.cache$long) && !is.null(.cache$next_check) && Sys.time() < .cache$next_check
}

# Refresh the shared cache if it is stale. Returns TRUE if new data was loaded.
#
# On failure with a usable copy already cached, the stale copy is kept and the
# error recorded rather than thrown: a Google hiccup should degrade the
# dashboard to slightly old figures, not blank it. With nothing cached at all
# there is nothing to fall back on, so the error propagates.
load_budget_data <- function(force = FALSE) {
  if (!force && cache_is_fresh()) return(invisible(FALSE))

  raw <- try(fetch_sheet_raw(), silent = TRUE)

  if (inherits(raw, "try-error")) {
    .cache$last_error <- conditionMessage(attr(raw, "condition"))
    .cache$next_check <- Sys.time() + CACHE_RETRY_SECONDS
    if (is.null(.cache$long)) {
      stop("Could not read the source sheet: ", .cache$last_error)
    }
    return(invisible(FALSE))
  }

  # Both bases are computed once here rather than per session, so flipping the
  # toggle is a lookup rather than a recomputation of the whole pipeline.
  parsed <- try({
    long <- tidy_budget(raw)
    list(
      long      = long,
      ind_new   = build_indicators(long, "new"),
      ind_total = build_indicators(long, "total"),
      ref       = reference_years(long),
      has_new   = any(long$particular %in% c("NEP_new", "GAA_new") &
                        !is.na(long$amount)),
      unknown   = attr(long, "unknown_particulars") %||% character(0)
    )
  }, silent = TRUE)

  if (inherits(parsed, "try-error")) {
    .cache$last_error <- conditionMessage(attr(parsed, "condition"))
    .cache$next_check <- Sys.time() + CACHE_RETRY_SECONDS
    if (is.null(.cache$long)) {
      stop("Could not parse the source sheet: ", .cache$last_error)
    }
    return(invisible(FALSE))
  }

  .cache$long       <- parsed$long
  .cache$ind_new    <- parsed$ind_new
  .cache$ind_total  <- parsed$ind_total
  .cache$has_new    <- parsed$has_new
  .cache$unknown    <- parsed$unknown
  .cache$ref        <- parsed$ref
  .cache$fetched_at <- Sys.time()
  .cache$next_check <- Sys.time() + CACHE_TTL_SECONDS
  .cache$last_error <- NULL
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# 5. Plot theme
# ---------------------------------------------------------------------------

theme_pbc <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title         = element_text(face = "bold", colour = PBC_NAVY, size = base_size * 1.1),
      plot.subtitle      = element_text(colour = PBC_GREY, size = base_size * 0.85),
      plot.caption       = element_text(colour = PBC_GREY, size = base_size * 0.72, hjust = 0),
      axis.title         = element_text(colour = PBC_GREY, size = base_size * 0.8),
      axis.text          = element_text(size = base_size * 0.95),
      legend.text        = element_text(size = base_size * 0.95),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "grey92"),
      strip.text         = element_text(face = "bold", colour = PBC_NAVY),
      legend.position    = "top",
      legend.title       = element_blank()
    )
}

# Plot height derived from row count rather than hard-coded. A fixed pixel
# height is the main reason the charts were unusable on a phone: 30 bars in
# 1040px is comfortable at 700px wide and illegible at 360px.
plot_height <- function(n_rows, per_row, pad, floor_px = 300) {
  max(floor_px, round(n_rows * per_row + pad))
}

empty_plot <- function(msg) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = str_wrap(msg, 60), colour = PBC_GREY, size = 4.2) +
    theme_void()
}

# ---------------------------------------------------------------------------
# 6. UI
# ---------------------------------------------------------------------------

# Reports the viewport width to the server on connect and on resize. Shiny
# renders plots server-side as raster images, so it cannot reflow them the way
# CSS would; the server has to know how wide the screen is to size them.
viewport_reporter <- tags$script(HTML(sprintf("
  (function() {
    function send() {
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('viewport_width', window.innerWidth, {priority: 'event'});
      }
    }
    $(document).on('shiny:connected', send);
    var t = null;
    window.addEventListener('resize', function() {
      clearTimeout(t); t = setTimeout(send, 250);
    });
    window.addEventListener('orientationchange', function() { setTimeout(send, 300); });
  })();
")))

mobile_css <- tags$style(HTML(sprintf("
  @media (max-width: %dpx) {
    .card-body { padding: 0.55rem !important; }
    .card-header { padding: 0.5rem 0.7rem !important; font-size: 0.95rem; }
    .navbar-brand { font-size: 1rem; }
    table.dataTable { font-size: 0.78rem; }
    table.dataTable td, table.dataTable th { padding: 0.35rem 0.4rem !important; }
    .form-label { margin-bottom: 0.15rem; }
    .shiny-input-container { margin-bottom: 0.6rem; }
    .bslib-value-box .value-box-value { font-size: 1.3rem !important; }
    /* Radio groups wrap instead of overflowing the screen edge */
    .shiny-options-group { display: flex; flex-wrap: wrap; gap: 0.15rem 0.9rem; }
  }
", MOBILE_BREAKPOINT)))

ui <- page_navbar(
  id = "nav",
  title = "Agency Budget & Utilization",
  header = tags$head(
    tags$meta(name = "viewport",
              content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    viewport_reporter,
    mobile_css
  ),
  theme = bs_theme(
    version = 5,
    primary = PBC_NAVY,
    base_font = font_google("Source Sans 3", local = FALSE),
    heading_font = font_google("Source Sans 3", local = FALSE)
  ),
  # Tabs whose content should stretch to the viewport rather than scroll.
  fillable = c("indicators", "data"),

  sidebar = sidebar(
    width = 330,
    title = "Filters",

    # Appropriations basis governs every NEP/GAA figure on every tab, so it
    # lives above the tab-conditional block and stays visible throughout.
    # A per-tab copy would need two inputs kept in sync, which is a bug
    # waiting to happen.
    radioButtons("basis", "Appropriations basis",
                 choices = BASIS_CHOICES, selected = "new"),
    div(class = "small text-muted",
        "New = what Congress legislates for the year. Total = New + Automatic ",
        "(RLIP, special accounts, debt service). Total is always at least as ",
        "large as New."),
    uiOutput("basis_warning"),

    hr(),

    # The Overview tab is whole-of-budget and carries its own year controls,
    # so the department / agency filters are hidden there to avoid implying
    # they do something.
    conditionalPanel(
      condition = "input.nav == 'overview'",
      div(class = "small text-muted",
          "The Budget Overview covers the whole budget and is not affected by ",
          "department or agency filters. Its year controls are on the tab itself.")
    ),

    conditionalPanel(
      condition = "input.nav != 'overview'",
      selectizeInput("f_dept", "Department", choices = NULL, multiple = TRUE,
                     options = list(placeholder = "All departments \u2014 type to search")),
      selectizeInput("f_agcy", "Agency", choices = NULL, multiple = TRUE,
                     options = list(placeholder = "All agencies \u2014 type to search")),
      conditionalPanel(
        condition = "input.nav == 'data'",
        selectizeInput("f_part", "Particular",
                       choices = setNames(PARTICULAR_KEYS, PARTICULAR_LABELS),
                       selected = PARTICULAR_KEYS, multiple = TRUE,
                       options = list(placeholder = "All particulars"))
      ),
      sliderInput("f_years", "Years", min = 2016, max = 2027,
                  value = c(2016, 2027), step = 1, sep = "")
    ),

    hr(),
    selectInput("unit", "Display units", choices = UNIT_CHOICES, selected = "thousands"),
    div(class = "small text-muted",
        "Source values are stored in thousands of pesos. This selector rescales ",
        "the display only \u2014 it does not alter the underlying data or any ratio."),

    hr(),
    actionButton("refresh", "Refresh from Google Sheet",
                 icon = icon("rotate"), class = "btn-primary w-100"),
    div(class = "small text-muted mt-2", textOutput("last_loaded", inline = TRUE)),
    div(class = "small mt-2", a("Open the source sheet", href = SHEET_URL, target = "_blank"))
  ),

  # =========================================================================
  # Tab 1 — Budget Overview (whole of budget)
  # =========================================================================
  nav_panel(
    "Budget Overview", value = "overview", icon = icon("chart-column"),

    div(class = "mt-2"),
    layout_columns(
      col_widths = breakpoints(sm = c(6, 6, 6, 6), md = c(3, 3, 3, 3)),
      value_box("Latest NEP year",  textOutput("vb_nep"),  showcase = icon("file-lines"),      theme = "primary"),
      value_box("Latest GAA year",  textOutput("vb_gaa"),  showcase = icon("file-signature"),  theme = "secondary"),
      value_box("Latest execution", textOutput("vb_exec"), showcase = icon("gauge-high"),      theme = "info"),
      value_box("Agencies covered", textOutput("vb_n"),    showcase = icon("building-columns"), theme = "light")
    ),

    card(
      card_header("Year controls for this tab"),
      card_body(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput("ov_year", "Budget year", choices = NULL),
          radioButtons("ov_measure", "Measure", inline = TRUE,
                       choices = c("GAA", "NEP"), selected = "GAA"),
          selectInput("ov_exec_year", "Utilization year", choices = NULL),
          div()
        ),
        div(class = "small text-muted",
            "The budget year drives the largest-budgets and congressional-adjustment ",
            "charts; the utilization year drives the rate rankings. They move ",
            "independently because the data frontier is ragged \u2014 the newest ",
            "proposal year has no enacted counterpart and no execution data yet."),
        div(class = "small text-muted mt-1",
            HTML(paste0("Whether NEP and GAA are read as <b>New</b> or <b>Total</b> ",
                        "appropriations is set by the <b>Appropriations basis</b> ",
                        "control in the sidebar, and applies to this tab and to ",
                        "Agency Trends alike.")))
      )
    ),

    card(
      card_header("Largest budgets"),
      card_body(
        div(class = "small text-muted mb-2",
            "Each bar shows both bases: the pale bar is Total Appropriations and ",
            "the solid bar drawn over it is New Appropriations, so the gap between ",
            "them is the automatic component. The basis toggle sets which one the ",
            "ranking and the printed value use \u2014 both bars are always shown."),
        layout_columns(
          col_widths = c(6, 6),
          plotOutput("plot_top_dept", height = "auto"),
          plotOutput("plot_top_agcy", height = "auto")
        )
      )
    ),

    card(
      card_header("Utilization: best and worst performers"),
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4),
          radioButtons("rate_type", "Order by", inline = TRUE,
                       choices = c("Obligation Rate" = "obl_rate",
                                   "Disbursement Rate" = "dis_rate"),
                       selected = "obl_rate"),
          numericInput("min_allot", "Minimum allotment (\u20b1 B)", value = 1, min = 0, step = 0.5),
          div()
        ),
        div(class = "small text-muted mb-2",
            "Both rates are plotted for every line; the selector only changes which ",
            "one the ranking is sorted on. The connecting bar is the gap between them ",
            "\u2014 money obligated but not yet paid out. A size floor keeps the ranking ",
            "meaningful: without it, tiny offices with a single rounding-scale ",
            "allotment dominate both tails."),
        layout_columns(
          col_widths = c(6, 6),
          plotOutput("plot_rates_dept", height = "auto"),
          plotOutput("plot_rates_agcy", height = "auto")
        )
      )
    ),

    card(
      card_header("What Congress cut and augmented"),
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4),
          radioButtons("cong_level", "Show", inline = TRUE,
                       choices = c("Departments" = "Department", "Agencies" = "Agency"),
                       selected = "Department"),
          radioButtons("cong_metric", "Rank by", inline = TRUE,
                       choices = c("Percent change" = "pct", "Peso change" = "abs"),
                       selected = "pct"),
          div()
        ),
        div(class = "small text-muted mb-2",
            "GAA against NEP within the same fiscal year, on whichever basis the ",
            "sidebar toggle selects \u2014 showing both at once here would be ",
            "unreadable. Ranking by percent favours small agencies where a modest ",
            "peso augmentation is a large proportion; ranking by pesos shows where ",
            "the money actually moved."),
        plotOutput("plot_cong", height = "auto")
      )
    )
  ),

  # =========================================================================
  # Tab 2 — Agency Trends (driven by the sidebar filters)
  # =========================================================================
  nav_panel(
    "Agency Trends", value = "trends", icon = icon("chart-line"),
    card(
      card_header("Trends for the current selection"),
      card_body(
        div(class = "small text-muted mb-2", textOutput("ts_subject")),
        plotOutput("plot_ts_rates",  height = "auto"),
        uiOutput("ui_ts_shares"),
        plotOutput("plot_ts_chg",    height = "auto"),
        plotOutput("plot_ts_cong",   height = "auto")
      )
    )
  ),

  # =========================================================================
  # Tab 3 — Key Indicators (indicators down, years across)
  # =========================================================================
  nav_panel(
    "Key Indicators", value = "indicators", icon = icon("percent"),
    card(
      full_screen = TRUE, fill = TRUE,
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span("Indicators by year"),
            downloadButton("dl_ind", "Download CSV", class = "btn-sm btn-primary"))
      ),
      card_body(
        fillable = TRUE, padding = 8,
        div(class = "small text-muted mb-1",
            HTML(paste0(
              "<b>Obligation Rate</b> = Obligations \u00f7 Allotments. ",
              "<b>Disbursement Rate</b> = Disbursements \u00f7 Allotments. ",
              "<b>Share of Department</b> appears for agency rows only. ",
              "All NEP and GAA figures follow the <b>Appropriations basis</b> ",
              "selected in the sidebar. ",
              "<b>NEP vs Prior GAA</b> compares this year's proposal against last ",
              "year's enacted budget. <b>GAA vs NEP</b> is the congressional ",
              "adjustment within the same year. An em-dash (\u2014) means not ",
              "reported; 0.0% is a reported zero."
            ))),
        uiOutput("mobile_table_note_ind"),
        DTOutput("tbl_ind", height = "100%")
      )
    )
  ),

  # =========================================================================
  # Tab 4 — Data Viewer
  # =========================================================================
  nav_panel(
    "Data Viewer", value = "data", icon = icon("table"),
    card(
      full_screen = TRUE, fill = TRUE,
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span("Full data"),
            downloadButton("dl_data", "Download CSV", class = "btn-sm btn-primary"))
      ),
      card_body(
        fillable = TRUE, padding = 8,
        div(class = "small text-muted mb-1", textOutput("unit_note_data")),
        uiOutput("mobile_table_note_data"),
        DTOutput("tbl_data", height = "100%")
      )
    )
  ),

  # =========================================================================
  # Tab 5 — Notes
  # =========================================================================
  nav_panel(
    "Notes", value = "notes", icon = icon("circle-info"),
    card(card_body(HTML('
      <h5>Units</h5>
      <p>Every figure in the source sheet is in <b>thousands of pesos</b>, as published by
      the DBM. Stored values are never converted; the display-unit selector rescales the
      presentation only, and ratios are unaffected.</p>

      <h5>Departments that are their own sole agency</h5>
      <p>Some departments have no separate bureau or attached-agency lines in the source
      sheet — DPWH, the Office of the President, the Office of the Vice-President and
      DOE among them. Such a department <b>is</b> its own only agency, so it appears in
      both the department and the agency views rather than being confined to the
      department one. Excluding them from agency rankings would drop several of the
      largest spenders in the budget out of the agency picture entirely.</p>
      <p>Their share-of-parent-department figure is left blank rather than shown as 100%,
      which would be true but uninformative.</p>

      <h5>Ordering</h5>
      <p>Departments and agencies appear in <b>the order they occupy in the source sheet</b>,
      which follows the GAA\'s own structural sequence, not alphabetical order. Every table,
      filter and export preserves it.</p>

      <h5>Coverage</h5>
      <p>This dashboard covers the <b>national agency level</b>: departments, their bureaux
      and attached agencies, and the aggregate blocks. Programme, Activity and Project
      detail is out of scope here and is planned as a separate dashboard, which will carry
      appropriations only \u2014 DBM does not publish P/A/P-level execution data.</p>
      <h5>New versus Total Appropriations</h5>
      <p>NEP and GAA are published on two bases, and the sidebar toggle switches every
      figure on the Budget Overview, Agency Trends and Key Indicators tabs between
      them.</p>
      <ul>
        <li><b>New Appropriations</b> are what Congress legislates for the year. This is
        the default, and it is the fairer basis for comparing agencies or for reading
        what the legislature changed.</li>
        <li><b>Total Appropriations</b> are New plus Automatic \u2014 Retirement and Life
        Insurance Premiums, special accounts, debt service and similar items that do not
        pass through the annual appropriations debate.</li>
      </ul>
      <p>Total is always at least as large as New for the same line. Agencies with heavy
      automatic components look very different on the two bases, which is exactly why the
      largest-budgets chart draws both: the pale tail beyond the solid bar is the
      automatic component.</p>
      <p>The congressional-adjustment chart follows the toggle rather than showing both,
      since a diverging bar carrying two bases at once is unreadable.</p>
      <p>The Data Viewer is unaffected by the toggle: it shows the sheet as published,
      with New and Total as separate rows.</p>

      <p>GOCCs, LGU transfers and Special Purpose Funds have NEP and GAA figures only.
      DBM does not publish disaggregated SAAODB reports for these, so utilization panels
      are blank for them by design rather than by error.</p>

      <h5>The ragged data frontier</h5>
      <p>NEP runs one year ahead of GAA, which runs one year ahead of execution. The newest
      NEP year has no enacted counterpart and no utilization data. Reference years are
      detected from the data on every load rather than hard-coded, and the Budget Overview
      carries separate selectors for the budget year and the utilization year because the
      two frontiers do not coincide.</p>

      <h5>Reading a blank cell</h5>
      <p>A blank is <b>not</b> a zero, and it does not always mean the same thing. It may
      mean any of the following, and the dashboard cannot tell them apart:</p>
      <ul>
        <li><b>The agency did not yet exist</b> in that year, or had not yet been separated
        out as its own line \u2014 the Fertilizer and Pesticide Authority and the National
        Fisheries Research and Development Institute both appear only from the late 2010s.</li>
        <li><b>The agency was housed under a different mother department</b> that year.
        Agencies are moved between departments across, and sometimes within,
        administrations, so a blank can mark a reorganisation rather than an absence. The
        same body may appear elsewhere in the sheet under its former parent.</li>
        <li><b>The agency was abolished, merged or renamed</b>, so the line stops.</li>
        <li><b>The figure was not reported</b> for that year \u2014 most often an execution
        report that was never submitted.</li>
        <li><b>The data frontier has not reached that year yet</b>, as with the newest
        NEP year, which has no GAA and no execution data.</li>
      </ul>
      <p>Because of the reorganisation case, a department-filtered trend can truncate an
      agency at its transfer year without the break being visible. When tracking a body
      across a reorganisation, filter by agency rather than by department, and treat an
      apparent gap as a question to check against the GAA rather than as a finding.</p>

      <h5>Zeros are different from blanks</h5>
      <p>A literal zero is carried as zero and shown as 0.0%, because it is what DBM
      published. The two are never interchanged.</p>
      <p><b>Caveat on zero disbursements.</b> A handful of rows report disbursements of
      exactly zero against substantial obligations \u2014 the Senate in 2018, 2019 and 2023,
      the House of Representatives in 2017, and the Commission on Appointments in 2022,
      among others. These are lifted directly from DBM\'s published SAAODB and are true to
      the source, but they almost certainly reflect a non-submitted FAR rather than
      literally no cash released. Read a 0.0% disbursement rate as information missing,
      not as a failure to spend. On the Budget Overview these lines are left unplotted on the
      disbursement axis rather than drawn at zero, so their obligation rate still shows
      while the missing disbursement reads as missing.</p>

      <h5>Allotments above GAA</h5>
      <p>Allotments routinely exceed the agency GAA line because continuing appropriations,
      automatic appropriations and Special Purpose Fund releases all land in allotments
      without appearing in that line. PHilMech is the clearest case: RCEF is tariff-funded
      and sits outside the agency GAA line entirely. Utilization rates above 100% follow
      from this and are not errors.</p>

      <h5>What Congress changed</h5>
      <p>The GAA-versus-NEP indicator compares the enacted budget against the proposal
      within the same fiscal year. Congress cannot raise the overall total, only realign
      within it, so augmentations to one line are funded by reductions elsewhere and the
      adjustments largely net out across the budget.</p>

      <h5>Percent shares of the aggregates</h5>
      <p>Percent-share panels are suppressed when the subject is Total Budget or Total
      NGAs, since those blocks are the denominator and every share would be either 100%
      or undefined.</p>
    '))
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "People\u2019s Budget Coalition"))
)

# ---------------------------------------------------------------------------
# 7. Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Load ---------------------------------------------------------------
  # --- Responsive state ---------------------------------------------------
  # Defaults to the wide layout until the browser reports in, so a desktop
  # session never flashes the narrow layout on load.
  is_mobile <- reactive({
    w <- input$viewport_width
    !is.null(w) && is.numeric(w) && w < MOBILE_BREAKPOINT
  })

  n_top_budget <- reactive(if (is_mobile()) N_TOP_BUDGET_MOBILE else N_TOP_BUDGET)
  n_top_rates  <- reactive(if (is_mobile()) N_TOP_RATES_MOBILE  else N_TOP_RATES)
  n_top_cong   <- reactive(if (is_mobile()) N_TOP_CONG_MOBILE   else N_TOP_CONG)

  # Narrower wrapping and slightly smaller type on a phone: the label column
  # would otherwise crowd out the bars entirely.
  lab_width <- reactive(if (is_mobile()) 22 else 34)
  base_sz   <- reactive(if (is_mobile()) 10.5 else 12)
  val_sz    <- reactive(if (is_mobile()) 2.7 else 3.3)

  # Rows wrap to more lines when narrow, so each needs more vertical room.
  h_top   <- function() plot_height(n_top_budget(),     if (is_mobile()) 42 else 30, 160)
  h_rates <- function() plot_height(n_top_rates() * 2,  if (is_mobile()) 40 else 29, 220)
  h_cong  <- function() plot_height(n_top_cong()  * 2,  if (is_mobile()) 40 else 23, 190)
  h_ts    <- function() if (is_mobile()) 290 else 340

  # Warm the shared cache. A no-op costing nothing when another session has
  # already loaded it, so only the first visitor after a cold start or an
  # expiry pays the download.
  if (!cache_is_fresh()) {
    withProgress(message = "Reading the Google Sheet\u2026", value = 0.5, {
      load_budget_data()
    })
  }

  # Bumped whenever this session should re-read the cache. Downstream reactives
  # depend on it, so they recompute on refresh and at no other time.
  data_version <- reactiveVal(.cache$fetched_at)

  observeEvent(input$refresh, {
    withProgress(message = "Refreshing from Google Sheet\u2026", value = 0.5, {
      load_budget_data(force = TRUE)
    })
    data_version(.cache$fetched_at)
    if (!is.null(.cache$last_error)) {
      showNotification(
        paste0("Could not reach the sheet; showing the last good copy. ",
               .cache$last_error),
        type = "warning", duration = 10
      )
    }
  })

  # A tab left open for hours picks up a refresh made by any other session.
  # load_budget_data() returns immediately while the cache is still fresh, so
  # this poll is nearly free.
  observe({
    invalidateLater(CACHE_POLL_MS, session)
    load_budget_data()
    if (!identical(.cache$fetched_at, isolate(data_version()))) {
      data_version(.cache$fetched_at)
    }
  })

  # Reading the cache rather than recomputing: the pipeline has already run.
  budget_long <- reactive({ data_version(); .cache$long })
  # If the sheet carries no New rows at all, fall back to Total rather than
  # serving empty charts, and say so.
  basis <- reactive({
    b <- input$basis %||% "new"
    data_version()
    if (identical(b, "new") && !isTRUE(.cache$has_new)) "total" else b
  })

  output$basis_warning <- renderUI({
    data_version()
    msgs <- list()
    if (identical(input$basis %||% "new", "new") && !isTRUE(.cache$has_new)) {
      msgs <- c(msgs, list(div(
        class = "small mt-2 p-2 border-start border-3 border-warning bg-light",
        "No New Appropriations rows were found in the sheet, so figures are ",
        "shown on the Total basis.")))
    }
    if (length(.cache$unknown)) {
      msgs <- c(msgs, list(div(
        class = "small mt-2 p-2 border-start border-3 border-warning bg-light",
        paste0("Unrecognised PARTICULAR rows were skipped: ",
               paste(utils::head(.cache$unknown, 6), collapse = "; "),
               ". They are not included in any figure."))))
    }
    if (length(msgs)) tagList(msgs) else NULL
  })

  budget_ind  <- reactive({
    data_version()
    if (identical(basis(), "new")) .cache$ind_new else .cache$ind_total
  })

  # Wording used in titles and captions so every chart states its basis.
  basis_lab   <- reactive(unname(BASIS_LABEL[basis()]))
  basis_short <- reactive(unname(BASIS_SHORT[basis()]))
  ref_years   <- reactive({ data_version(); .cache$ref })

  output$last_loaded <- renderText({
    data_version()
    stamp <- .cache$fetched_at
    if (is.null(stamp)) return("Not yet loaded")
    age_min <- as.numeric(difftime(Sys.time(), stamp, units = "mins"))
    paste0("Sheet read ", format(stamp, "%d %b %Y, %H:%M"),
           " (", if (age_min < 1) "just now" else paste0(round(age_min), " min ago"), ")",
           if (!is.null(.cache$last_error)) " \u2014 last refresh failed, showing cached copy" else "")
  })

  # --- Sidebar filter choices (in sheet order, not alphabetical) ----------
  observeEvent(budget_long(), {
    d <- budget_long()

    depts <- d %>% distinct(department, dept_ord) %>% arrange(dept_ord) %>% pull(department)
    updateSelectizeInput(session, "f_dept", choices = depts,
                         selected = isolate(input$f_dept), server = TRUE)

    yrs <- range(d$year, na.rm = TRUE)
    updateSliderInput(session, "f_years", min = yrs[1], max = yrs[2],
                      value = isolate(input$f_years) %||% yrs)

    # --- Overview-local year controls ------------------------------------
    bud_years  <- years_with(d, APPROP_KEYS)
    exec_years <- years_with(d, "Allotments")

    # These guards used to fail silently. An empty selector leaves input$ov_year
    # NULL, every req() downstream halts, and the charts render as blank space
    # with nothing anywhere saying why. Now it says why.
    if (length(bud_years)) {
      updateSelectInput(session, "ov_year",
                        choices = rev(as.character(bud_years)),
                        selected = as.character(max(bud_years)))
    } else {
      showNotification(
        paste0("No NEP or GAA rows were recognised, so the budget-year selector ",
               "is empty and the appropriations charts cannot draw. Check the ",
               "PARTICULAR column in the sheet."),
        type = "error", duration = NULL
      )
    }

    if (length(exec_years)) {
      updateSelectInput(session, "ov_exec_year",
                        choices = rev(as.character(exec_years)),
                        selected = as.character(max(exec_years)))
    } else {
      showNotification(
        paste0("No Allotments rows were recognised, so the utilization year ",
               "selector is empty and the rate charts cannot draw."),
        type = "warning", duration = NULL
      )
    }
  })

  # Measure toggle offers only what exists for the chosen year, and prefers
  # GAA. Where only the proposal exists — the newest year — it falls back to
  # NEP rather than showing an empty chart.
  observeEvent(input$ov_year, {
    req(input$ov_year)
    d <- budget_long() %>%
      filter(year == as.integer(input$ov_year), !is.na(amount))
    have <- unique(d$particular)
    avail <- c(if (any(GAA_KEYS %in% have)) "GAA",
               if (any(NEP_KEYS %in% have)) "NEP")
    if (!length(avail)) return()
    sel <- if ("GAA" %in% avail) "GAA" else "NEP"
    if (!is.null(input$ov_measure) && input$ov_measure %in% avail) sel <- input$ov_measure
    updateRadioButtons(session, "ov_measure", choices = avail, selected = sel, inline = TRUE)
  })

  # Agency choices cascade off the department selection.
  observe({
    d <- budget_long()
    if (length(input$f_dept)) d <- d %>% filter(department %in% input$f_dept)
    agcys <- d %>% distinct(agency, dept_ord, agency_ord) %>%
      arrange(dept_ord, agency_ord) %>% pull(agency)
    updateSelectizeInput(session, "f_agcy", choices = agcys,
                         selected = isolate(input$f_agcy), server = TRUE)
  })

  apply_filters <- function(df) {
    if (length(input$f_dept)) df <- df %>% filter(department %in% input$f_dept)
    if (length(input$f_agcy)) df <- df %>% filter(agency     %in% input$f_agcy)
    if (!is.null(input$f_years)) {
      df <- df %>% filter(year >= input$f_years[1], year <= input$f_years[2])
    }
    df
  }

  filtered_long <- reactive({
    df <- apply_filters(budget_long())
    if (length(input$f_part)) df <- df %>% filter(particular %in% input$f_part)
    df
  })

  filtered_ind <- reactive({
    d <- apply_filters(budget_ind())
    d[has_any_value(d), , drop = FALSE]
  })

  # --- Value boxes --------------------------------------------------------
  output$vb_nep  <- renderText(as.character(ref_years()$nep   %||% "\u2014"))
  output$vb_gaa  <- renderText(as.character(ref_years()$gaa   %||% "\u2014"))
  output$vb_exec <- renderText(as.character(ref_years()$alloc %||% "\u2014"))
  output$vb_n    <- renderText({
    d <- budget_long() %>% filter(is_agency)
    format(nrow(distinct(d, department, agency)), big.mark = ",")
  })

  # =========================================================================
  # Tab 1 — Budget Overview
  # =========================================================================

  ov_year    <- reactive({ req(input$ov_year);      as.integer(input$ov_year) })
  ov_exec_yr <- reactive({ req(input$ov_exec_year); as.integer(input$ov_exec_year) })
  ov_measure <- reactive(input$ov_measure %||% "GAA")

  # Both appropriations bases are drawn on every bar. Total is always >= New
  # for the same line, so the pale Total bar is drawn first and the solid New
  # bar over it, leaving the automatic component visible as the exposed tail.
  # The basis toggle changes only the ranking and the printed value.
  top_budget_plot <- function(lvl) {
    req(input$ov_year, input$ov_measure)
    meas <- ov_measure()          # "NEP" or "GAA"
    yr   <- ov_year()
    div  <- unit_divisor(input$unit)
    b    <- basis()

    col_new   <- paste0(meas, "_new")
    col_total <- paste0(meas, "_total")
    rank_col  <- if (identical(b, "new")) col_new else col_total

    df <- budget_ind() %>%
      at_level(lvl) %>%
      filter(year == yr) %>%
      mutate(v_new   = .data[[col_new]],
             v_total = .data[[col_total]],
             v_rank  = .data[[rank_col]]) %>%
      filter(!is.na(v_rank), v_rank > 0) %>%
      slice_max(v_rank, n = n_top_budget(), with_ties = FALSE)

    if (nrow(df) == 0) {
      return(empty_plot(paste0("No ", meas, " ", basis_lab(),
                               " at this level for FY ", yr, ".")))
    }

    solid <- if (lvl == "Department") PBC_NAVY else PBC_TEAL
    pale  <- if (lvl == "Department") FILL_TOTAL_DEPT else FILL_TOTAL_AGCY

    lab_total <- "Total Appropriations (New + Automatic)"
    lab_new   <- "New Appropriations"

    df <- df %>%
      mutate(lab = wrap_lab(agency, width = lab_width()),
             ykey = reorder(lab, v_rank),
             # The printed value follows the toggle; it is placed past the
             # longer of the two bars so it never sits on top of a bar.
             txt = fmt_amt(if (identical(b, "new")) v_new else v_total,
                           div, unit_digits(input$unit)),
             tip = pmax(v_total, v_new, na.rm = TRUE))

    ggplot(df) +
      geom_col(aes(x = ykey, y = v_total / div, fill = lab_total), width = 0.75) +
      geom_col(aes(x = ykey, y = v_new   / div, fill = lab_new),   width = 0.75) +
      geom_text(aes(x = ykey, y = tip / div, label = txt),
                hjust = -0.12, size = val_sz(), colour = PBC_GREY) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.24))) +
      scale_fill_manual(
        values = setNames(c(pale, solid), c(lab_total, lab_new)),
        breaks = c(lab_new, lab_total)
      ) +
      labs(
        title = paste0("Top ", n_top_budget(), " ",
                       if (lvl == "Department") "departments" else "agencies",
                       " by ", meas, " ", basis_short(), ", FY ", yr),
        subtitle = paste0(
          if (meas == "NEP") "Proposed appropriations \u2014 not yet enacted."
          else "Enacted appropriations.",
          " Ranked and labelled by ", basis_lab(), "."),
        x = NULL, y = unit_label(input$unit),
        caption = paste0("Both bases shown: the exposed pale tail is the automatic ",
                         "component. Aggregate rows excluded. Source: DBM.")
      ) +
      theme_pbc(base_size = base_sz()) +
      theme(legend.position = "top")
  }

  output$plot_top_dept <- renderPlot(top_budget_plot("Department"), height = h_top)
  output$plot_top_agcy <- renderPlot(top_budget_plot("Agency"),     height = h_top)

  # Utilization is shown as a dot plot rather than bars so that BOTH rates can
  # appear on the same row. The "Order by" selector only decides which rate the
  # ranking is sorted on; neither rate is ever hidden.
  rates_plot <- function(lvl) {
    req(input$ov_exec_year)
    yr       <- ov_exec_yr()
    order_by <- input$rate_type %||% "obl_rate"
    floor_thousands <- (input$min_allot %||% 0) * 1e6   # ₱B -> thousands

    order_lab <- if (order_by == "obl_rate") "Obligation Rate" else "Disbursement Rate"
    n_show <- n_top_rates()
    hi_lab <- paste("Highest", n_show, "\u2014", order_lab)
    lo_lab <- paste("Lowest",  n_show, "\u2014", order_lab)

    df <- budget_ind() %>%
      at_level(lvl) %>%
      filter(year == yr,
             !is.na(Allotments), Allotments >= floor_thousands) %>%
      # A flagged zero disbursement is missing information, not 0% performance,
      # so it is blanked rather than plotted at the floor. The obligation point
      # for that line still shows.
      mutate(dis_plot = if_else(zero_disb_flag, NA_real_, dis_rate),
             ord_val  = if (order_by == "obl_rate") obl_rate else dis_plot) %>%
      filter(!is.na(ord_val))

    if (nrow(df) < 2) {
      return(empty_plot(paste0(
        "Not enough ", tolower(lvl), " rows clear the size floor for FY ", yr,
        ". Try lowering it.")))
    }

    both <- bind_rows(
      df %>% slice_max(ord_val, n = n_show, with_ties = FALSE) %>% mutate(grp = hi_lab),
      df %>% slice_min(ord_val, n = n_show, with_ties = FALSE) %>% mutate(grp = lo_lab)
    ) %>%
      mutate(lab = wrap_lab(agency, width = lab_width()))

    # The row-position factor must be built BEFORE reshaping to long: after the
    # pivot each agency occupies two rows, and make.unique would split them onto
    # two separate axis positions.
    both <- both %>% mutate(ykey = reorder_within_grp(lab, ord_val, grp))

    pts <- both %>%
      select(ykey, grp, `Obligation Rate` = obl_rate, `Disbursement Rate` = dis_plot) %>%
      pivot_longer(c(`Obligation Rate`, `Disbursement Rate`),
                   names_to = "rate", values_to = "val") %>%
      filter(!is.na(val))

    seg <- both %>%
      filter(!is.na(obl_rate), !is.na(dis_plot)) %>%
      select(ykey, grp, obl_rate, dis_plot)

    ggplot() +
      geom_hline(yintercept = 1, linetype = "dashed", colour = "grey75", linewidth = 0.4) +
      geom_segment(data = seg,
                   aes(x = ykey, xend = ykey, y = obl_rate, yend = dis_plot),
                   colour = "grey78", linewidth = 1.1, lineend = "round") +
      geom_point(data = pts, aes(x = ykey, y = val, colour = rate),
                 size = if (is_mobile()) 2.4 else 2.9) +
      coord_flip(clip = "off") +
      facet_wrap(~grp, scales = "free_y", ncol = 1) +
      scale_x_discrete(labels = function(x) sub("___.*$", "", x)) +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         breaks = scales::breaks_pretty(n = 6),
                         expand = expansion(mult = c(0.03, 0.08))) +
      # Always show the full 0-100% range so a low rate reads as low rather
      # than being stretched to fill the panel. Values above 100% push the
      # axis out; they never get clipped.
      expand_limits(y = c(0, 1)) +
      scale_colour_manual(values = c("Obligation Rate"   = PBC_NAVY,
                                     "Disbursement Rate" = PBC_BLUE)) +
      labs(
        title = paste0(if (lvl == "Department") "Departments" else "Agencies",
                       ": utilization, FY ", yr),
        subtitle = paste0("Ranked by ", order_lab, ". Obligations \u00f7 Allotments and ",
                          "Disbursements \u00f7 Allotments. Minimum allotment: \u20b1",
                          format(input$min_allot %||% 0, nsmall = 1), "B."),
        x = NULL, y = NULL,
        caption = paste0("Dashed line marks 100%. Rates above it reflect releases against ",
                         "continuing or automatic appropriations. Reported zero ",
                         "disbursements are left unplotted \u2014 see Notes. Source: DBM SAAODB.")
      ) +
      theme_pbc(base_size = base_sz()) +
      theme(panel.grid.major.x = element_line(colour = "grey95"))
  }

  output$plot_rates_dept <- renderPlot(rates_plot("Department"), height = h_rates)
  output$plot_rates_agcy <- renderPlot(rates_plot("Agency"),     height = h_rates)

  output$plot_cong <- renderPlot({
    req(input$ov_year)
    yr     <- ov_year()
    lvl    <- input$cong_level  %||% "Department"
    metric <- input$cong_metric %||% "pct"
    div    <- unit_divisor(input$unit)

    df <- budget_ind() %>%
      at_level(lvl) %>%
      filter(year == yr, !is.na(NEP), !is.na(GAA), NEP > 0)

    if (nrow(df) < 2) {
      return(empty_plot(paste0(
        "FY ", yr, " has no enacted budget to compare against the proposal. ",
        "Choose an earlier budget year to see congressional adjustments.")))
    }

    df <- df %>%
      mutate(val = if (metric == "pct") gaa_vs_nep else gaa_less_nep) %>%
      filter(!is.na(val), abs(val) > 1e-9)   # drop lines Congress left untouched

    if (nrow(df) < 2) {
      return(empty_plot(paste0("Congress left these lines unchanged in FY ", yr, ".")))
    }

    both <- bind_rows(
      df %>% slice_max(val, n = n_top_cong(), with_ties = FALSE),
      df %>% slice_min(val, n = n_top_cong(), with_ties = FALSE)
    ) %>%
      distinct(department, agency, .keep_all = TRUE) %>%
      mutate(lab = wrap_lab(agency, width = lab_width()),
             dir = if_else(val >= 0, "Augmented by Congress", "Cut by Congress"),
             txt = if (metric == "pct") fmt_pct_signed(val)
                   else fmt_amt_signed(val, div, unit_digits(input$unit)),
             plot_val = if (metric == "pct") val else val / div)

    ggplot(both, aes(x = reorder(lab, plot_val), y = plot_val, fill = dir)) +
      geom_col(width = 0.75) +
      geom_hline(yintercept = 0, colour = PBC_GREY, linewidth = 0.4) +
      geom_text(aes(label = txt, hjust = if_else(plot_val >= 0, -0.12, 1.12)),
                size = val_sz(), colour = PBC_GREY) +
      coord_flip(clip = "off") +
      scale_y_continuous(
        labels = if (metric == "pct") percent_format(accuracy = 1) else label_comma(),
        expand = expansion(mult = c(0.20, 0.20))
      ) +
      scale_fill_manual(values = c("Augmented by Congress" = PBC_GREEN,
                                   "Cut by Congress"       = PBC_RUST)) +
      labs(
        title = paste0("Largest congressional adjustments, FY ", yr, " \u2014 ",
                       if (lvl == "Department") "departments" else "agencies",
                       " (", basis_short(), ")"),
        subtitle = paste0("GAA against NEP in the same year, on the ", basis_lab(),
                          " basis. Top and bottom ", n_top_cong(), " by ",
                          if (metric == "pct") "percentage change" else "peso change", "."),
        x = NULL,
        y = if (metric == "pct") "GAA vs NEP" else paste0("GAA less NEP (", unit_label(input$unit), ")"),
        caption = paste0("Congress cannot raise the overall total, so augmentations are ",
                         "funded by cuts elsewhere. Source: DBM.")
      ) +
      theme_pbc(base_size = base_sz())
  }, height = h_cong)

  # =========================================================================
  # Tab 2 — Agency Trends
  # =========================================================================

  # Default subject is Total NGAs until the user narrows the filters.
  ts_subject <- reactive({
    ind <- budget_ind()
    f   <- filtered_ind()

    if (length(input$f_agcy) == 1) {
      list(df = f %>% filter(agency == input$f_agcy), name = input$f_agcy)
    } else if (length(input$f_dept) == 1 && !length(input$f_agcy)) {
      list(df = f %>% filter(department == input$f_dept, level != "Agency"),
           name = input$f_dept)
    } else if (!length(input$f_dept) && !length(input$f_agcy)) {
      list(df = ind %>% filter(department == AGG_TOTAL_NGAS), name = AGG_TOTAL_NGAS)
    } else {
      list(df = NULL, name = NULL)
    }
  })

  output$ts_subject <- renderText({
    s <- ts_subject()
    if (is.null(s$name)) {
      "Select a single department or agency to see its trend."
    } else {
      paste0("Showing: ", s$name,
             if (identical(s$name, AGG_TOTAL_NGAS))
               " (default \u2014 narrow the filters to change)" else "")
    }
  })

  ts_data <- reactive({
    s <- ts_subject()
    if (is.null(s$df) || nrow(s$df) == 0) return(NULL)
    d <- s$df
    if (!is.null(input$f_years)) {
      d <- d %>% filter(year >= input$f_years[1], year <= input$f_years[2])
    }
    d
  })

  output$plot_ts_rates <- renderPlot({
    d <- ts_data()
    if (is.null(d)) return(empty_plot("Select a single department or agency."))

    pl <- d %>%
      select(year, `Obligation Rate` = obl_rate, `Disbursement Rate` = dis_rate) %>%
      pivot_longer(-year, names_to = "ind", values_to = "val") %>%
      filter(!is.na(val))

    if (nrow(pl) == 0) return(empty_plot("No utilization data for this selection."))

    ggplot(pl, aes(year, val, colour = ind)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.2) +
      scale_colour_manual(values = c("Obligation Rate" = PBC_NAVY,
                                     "Disbursement Rate" = PBC_BLUE)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      # Anchored to 0-100% so year-on-year movement is read against the whole
      # scale, not against a window that rescales itself each time the filter
      # changes. Rates above 100% extend the axis rather than being cut off.
      expand_limits(y = c(0, 1)) +
      scale_x_continuous(breaks = scales::breaks_width(1)) +
      labs(title = "Utilization rates", x = NULL, y = NULL,
           caption = paste0("Both rates use Allotments as the denominator; the gap is ",
                            "the obligated-but-unpaid overhang.")) +
      theme_pbc(base_size = base_sz() + 1)
  }, height = h_ts)

  # Percent shares are suppressed for the aggregate blocks, which are the
  # denominator: every share there is either 100% or undefined.
  output$ui_ts_shares <- renderUI({
    s <- ts_subject()
    if (!is.null(s$name) && s$name %in% SHARE_SUPPRESSED) {
      div(class = "small text-muted my-3 p-2 border-start border-3",
          paste0("Percent shares are not shown for ", s$name,
                 " \u2014 it is the denominator, so every share would be 100% or undefined."))
    } else {
      plotOutput("plot_ts_shares", height = "auto")
    }
  })

  output$plot_ts_shares <- renderPlot({
    d <- ts_data()
    if (is.null(d)) return(empty_plot("Select a single department or agency."))

    # Share-of-department is deliberately absent: six series on one panel was
    # unreadable. It remains available in the Key Indicators table.
    pl <- d %>%
      select(year,
             `NEP % of Total Budget` = nep_sh_total,
             `GAA % of Total Budget` = gaa_sh_total,
             `NEP % of Total NGAs`   = nep_sh_ngas,
             `GAA % of Total NGAs`   = gaa_sh_ngas) %>%
      pivot_longer(-year, names_to = "ind", values_to = "val") %>%
      filter(!is.na(val))

    if (nrow(pl) == 0) return(empty_plot("No share data for this selection."))

    ggplot(pl, aes(year, val, colour = ind)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.9) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      scale_x_continuous(breaks = scales::breaks_width(1)) +
      scale_colour_manual(values = c(
        "NEP % of Total Budget" = PBC_NAVY,  "GAA % of Total Budget" = PBC_BLUE,
        "NEP % of Total NGAs"   = PBC_GREEN, "GAA % of Total NGAs"   = PBC_TEAL
      )) +
      labs(title = paste0("Percent shares (", basis_short(), " Appropriations)"),
           x = NULL, y = NULL,
           caption = paste0("Share of parent department is not plotted here \u2014 see the ",
                            "Key Indicators tab for it.")) +
      theme_pbc(base_size = base_sz() + 1)
  }, height = h_ts)

  output$plot_ts_chg <- renderPlot({
    d <- ts_data()
    if (is.null(d)) return(empty_plot("Select a single department or agency."))

    pl <- d %>%
      select(year,
             `NEP vs prior NEP` = nep_chg,
             `GAA vs prior GAA` = gaa_chg,
             `NEP vs prior GAA` = nep_vs_prev_gaa) %>%
      pivot_longer(-year, names_to = "ind", values_to = "val") %>%
      filter(!is.na(val))

    if (nrow(pl) == 0) return(empty_plot("No change data for this selection."))

    ggplot(pl, aes(year, val, fill = ind)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.72) +
      geom_hline(yintercept = 0, colour = PBC_GREY, linewidth = 0.4) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_x_continuous(breaks = scales::breaks_width(1)) +
      scale_fill_manual(values = c("NEP vs prior NEP" = PBC_TEAL,
                                   "GAA vs prior GAA" = PBC_NAVY,
                                   "NEP vs prior GAA" = PBC_RUST)) +
      labs(title = paste0("Year-on-year change (", basis_short(), " Appropriations)"),
           x = NULL, y = NULL,
           caption = paste0("For the newest NEP year no GAA exists yet, so only ",
                            "'NEP vs prior GAA' is available.")) +
      theme_pbc(base_size = base_sz() + 1)
  }, height = h_ts)

  output$plot_ts_cong <- renderPlot({
    d <- ts_data()
    if (is.null(d)) return(empty_plot("Select a single department or agency."))

    pl <- d %>% select(year, val = gaa_vs_nep) %>% filter(!is.na(val))
    if (nrow(pl) == 0) return(empty_plot("No congressional adjustment for this selection."))

    pl <- pl %>% mutate(dir = if_else(val >= 0, "Augmented", "Cut"))

    ggplot(pl, aes(year, val, fill = dir)) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 0, colour = PBC_GREY, linewidth = 0.4) +
      geom_text(aes(label = fmt_pct_signed(val),
                    vjust = if_else(val >= 0, -0.4, 1.3)),
                size = 3.1, colour = PBC_GREY) +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         expand = expansion(mult = c(0.12, 0.12))) +
      scale_x_continuous(breaks = scales::breaks_width(1)) +
      scale_fill_manual(values = c("Augmented" = PBC_GREEN, "Cut" = PBC_RUST)) +
      labs(title = paste0("Congressional adjustment: GAA vs NEP, same year (",
                          basis_short(), ")"), x = NULL, y = NULL,
           caption = paste0("Positive means the enacted budget exceeded the proposal. ",
                            "No bar for the newest NEP year, which has no GAA yet.")) +
      theme_pbc(base_size = base_sz() + 1)
  }, height = h_ts)

  # =========================================================================
  # Tab 3 — Key Indicators (indicators down the rows, years across)
  # =========================================================================

  # Long form: one row per agency-indicator-year, ready to pivot either way.
  ind_long <- reactive({
    filtered_ind() %>%
      select(department, agency, level, dept_ord, agency_ord, year, all_of(IND_KEYS)) %>%
      pivot_longer(all_of(IND_KEYS), names_to = "key", values_to = "val") %>%
      mutate(
        ind_pos   = match(key, IND_KEYS),
        Indicator = factor(IND_LABELS[ind_pos], levels = IND_LABELS),
        signed    = IND_SIGNED[ind_pos]
      ) %>%
      # Drop indicator rows that are empty across every year in view: share of
      # department for aggregate rows, utilization for GOCC/LGU/SPF blocks.
      group_by(department, agency, Indicator) %>%
      filter(any(!is.na(val))) %>%
      ungroup()
  })

  # Numeric wide form, for the CSV export.
  ind_wide_num <- reactive({
    ind_long() %>%
      arrange(dept_ord, agency_ord, Indicator, year) %>%
      select(department, agency, level, dept_ord, agency_ord, Indicator, year, val) %>%
      pivot_wider(names_from = year, values_from = val) %>%
      arrange(dept_ord, agency_ord, Indicator) %>%
      select(-dept_ord, -agency_ord) %>%
      rename(Department = department, Agency = agency, Level = level)
  })

  # Formatted wide form, for display. Formatting differs by indicator, so it
  # has to happen before the pivot, while each row still knows what it is.
  ind_wide_disp <- reactive({
    ind_long() %>%
      mutate(txt = if_else(signed, fmt_pct_signed(val), fmt_pct(val))) %>%
      arrange(dept_ord, agency_ord, Indicator, year) %>%
      select(department, agency, level, dept_ord, agency_ord, Indicator, year, txt) %>%
      pivot_wider(names_from = year, values_from = txt) %>%
      arrange(dept_ord, agency_ord, Indicator) %>%
      select(-dept_ord, -agency_ord) %>%
      rename(Department = department, Agency = agency, Level = level)
  })

  output$tbl_ind <- renderDT({
    disp <- ind_wide_disp()
    disp$Indicator <- as.character(disp$Indicator)

    if (is_mobile()) {
      # Four identity columns plus twelve years cannot coexist on a phone.
      # Department and Level are folded away and the remaining identity is
      # collapsed into one column, leaving a single narrow anchor beside the
      # years. Frozen columns are dropped: at this width they would consume
      # the entire screen, and DT's FixedColumns is unreliable on touch.
      disp <- disp %>%
        mutate(Row = paste0(Agency, " \u2014 ", Indicator), .before = 1) %>%
        select(-Department, -Agency, -Level, -Indicator)

      return(datatable(
        disp,
        rownames = FALSE,
        options = list(
          order = list(),
          dom = "ftip",
          pageLength = 15,
          lengthChange = FALSE,
          scrollX = TRUE,
          autoWidth = FALSE,
          columnDefs = list(
            list(className = "dt-right", targets = 1:(ncol(disp) - 1)),
            list(width = "170px", targets = 0)
          )
        )
      ))
    }

    datatable(
      disp,
      rownames = FALSE,
      filter = "top",
      extensions = c("FixedColumns", "Scroller"),
      # order = list() stops DT re-sorting column 1 and undoing sheet order.
      options = list(
        order = list(),
        dom = "ftipr",
        deferRender = TRUE,
        scroller = TRUE,
        scrollY = "calc(100vh - 320px)",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        fixedColumns = list(leftColumns = 4),
        columnDefs = list(list(className = "dt-right", targets = 4:(ncol(disp) - 1)))
      )
    )
  })

  output$dl_ind <- downloadHandler(
    filename = function() paste0("ph-agency-budget-indicators_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- ind_wide_num()
      writeLines(
        c("# PH Budget Data Set - agency-level key indicators, filtered export",
          "# Indicators down the rows, fiscal years across the columns.",
          paste0("# Appropriations basis: ", BASIS_LABEL[[basis()]],
                 " (set by the sidebar toggle)."),
          "# Rates and shares are PROPORTIONS (0.85 = 85.0%).",
          "# Obligation Rate = Obligations / Allotments.",
          "# Disbursement Rate = Disbursements / Allotments.",
          "# 'NEP vs Prior GAA %' compares the proposal against last year's enacted budget.",
          "# 'GAA vs NEP %' is the congressional adjustment within the same fiscal year.",
          "# Rows follow the source sheet's order, not alphabetical order.",
          "# Blank = not computable (missing or zero denominator).",
          paste0("# Source: ", SHEET_URL),
          paste0("# Exported: ", Sys.time())),
        file
      )
      suppressWarnings(readr::write_csv(df, file, append = TRUE, col_names = TRUE))
    }
  )

  # =========================================================================
  # Tab 4 — Data Viewer
  # =========================================================================

  narrow_table_note <- function(cols) {
    div(class = "small text-muted mb-1 fst-italic",
        paste0("Narrow screen: ", cols, " are combined into one column and the ",
               "years scroll sideways. Rotate to landscape or open on a wider ",
               "screen for the full table."))
  }
  output$mobile_table_note_data <- renderUI(
    if (is_mobile()) narrow_table_note("agency and particular")
  )
  output$mobile_table_note_ind <- renderUI(
    if (is_mobile()) narrow_table_note("agency and indicator")
  )

  output$unit_note_data <- renderText({
    paste0("Amounts in ", unit_label(input$unit),
           ". Source values are in thousands of pesos. Rows follow sheet order. ",
           "An em-dash means not reported; 0 is a reported zero.")
  })

  data_view <- reactive({
    filtered_long() %>%
      select(department, agency, level, dept_ord, agency_ord, particular, year, amount) %>%
      mutate(particular = factor(particular, levels = PARTICULAR_KEYS)) %>%
      arrange(dept_ord, agency_ord, particular, year) %>%
      pivot_wider(names_from = year, values_from = amount) %>%
      arrange(dept_ord, agency_ord, particular) %>%
      select(-dept_ord, -agency_ord) %>%
      # Back to the sheet's own wording for display: "NEP_new" is an internal
      # key, not something a reader should have to decode.
      mutate(particular = unname(PARTICULAR_LABELS[as.character(particular)]))
  })

  output$tbl_data <- renderDT({
    df  <- data_view()
    div <- unit_divisor(input$unit)
    dg  <- unit_digits(input$unit)

    yr_cols <- setdiff(names(df), c("department", "agency", "level", "particular"))
    disp <- df
    for (cl in yr_cols) disp[[cl]] <- fmt_amt(disp[[cl]], div, dg)
    names(disp)[1:4] <- c("Department", "Agency", "Level", "Particular")

    if (is_mobile()) {
      # Same treatment as the indicators table: one identity column, no frozen
      # columns, ordinary paging instead of a 100vh scroll body. Mobile
      # browsers resize the viewport as their chrome hides and reappears, so a
      # vh-based table height jumps around while the reader scrolls.
      disp <- disp %>%
        mutate(Row = paste0(Agency, " \u2014 ", Particular), .before = 1) %>%
        select(-Department, -Agency, -Level, -Particular)

      return(datatable(
        disp,
        rownames = FALSE,
        options = list(
          order = list(),
          dom = "ftip",
          pageLength = 15,
          lengthChange = FALSE,
          scrollX = TRUE,
          autoWidth = FALSE,
          columnDefs = list(
            list(className = "dt-right", targets = 1:(ncol(disp) - 1)),
            list(width = "170px", targets = 0)
          )
        )
      ))
    }

    datatable(
      disp,
      rownames = FALSE,
      filter = "top",
      extensions = c("FixedColumns", "Scroller"),
      options = list(
        order = list(),
        dom = "ftipr",
        deferRender = TRUE,
        scroller = TRUE,
        scrollY = "calc(100vh - 300px)",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        fixedColumns = list(leftColumns = 2),
        columnDefs = list(list(className = "dt-right", targets = 4:(ncol(disp) - 1)))
      )
    )
  })

  output$dl_data <- downloadHandler(
    filename = function() paste0("ph-agency-budget-data_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- data_view()
      writeLines(
        c("# PH Budget Data Set - agency level, filtered export",
          "# All amounts in THOUSANDS of pesos, as published by DBM.",
          "# Appropriations appear on both bases as separate rows:",
          "#   'New Appropriations'   = what Congress legislates for the year",
          "#   'Total Appropriations' = New + Automatic. Total >= New always.",
          "# Rows follow the source sheet's order, not alphabetical order.",
          "# Blank = not reported. 0 = reported zero (faithful to SAAODB).",
          paste0("# Source: ", SHEET_URL),
          paste0("# Exported: ", Sys.time())),
        file
      )
      suppressWarnings(readr::write_csv(df, file, append = TRUE, col_names = TRUE))
    }
  )
}

shinyApp(ui, server)
