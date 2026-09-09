# =============================================================================
# P/A/P Browser Dashboard
# PH Budget Dashboards — People's Budget Coalition
#
# Scope: PROGRAM / ACTIVITY / PROJECT level. The granular lines inside each
# agency's headline programs, across NEP and GAA, FY 2020-2027.
# Appropriations only: DBM publishes no P/A/P-level execution data, so there
# are no allotment, obligation or disbursement figures here. Agency-level
# execution lives in the companion agency-budget-utilization dashboard.
#
# Data source: Compiled_-_PAPs.xlsx in the ph-budget-analysis repo,
# sheets "NGAs" and "DPWH-sub". Other sheets are ignored. Read at run time and
# cached, so publishing an update to that repo updates this dashboard without
# a redeploy — the same arrangement as the agency dashboard and its sheet.
#
# UNITS: this workbook stores PESOS, not the thousands used elsewhere in this
# repo. See SOURCE_UNIT below — the departure is deliberate and verified, and
# is confined to one constant.
#
# Tabs
#   1. Browse   the P/A/P table, full viewport height
#   2. Trend    NEP vs GAA totals for the current selection, by fiscal year
#   3. Notes    caveats that travel with the data
#
# Run locally with:  shiny::runApp("dashboards/pap-browser")
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(DT)
library(scales)
library(readxl)
library(readr)

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------

DATA_URL <- paste0("https://raw.githubusercontent.com/ajamontesa/",
                   "ph-budget-analysis/main/data/Compiled_-_PAPs.xlsx")

# Sheets are DETECTED, not listed. Any sheet carrying all of ID_COLS and at
# least one "_EXP_" column is read and bound. That is what makes the dashboard
# survive the workbook growing: the SUCs sheet already exists but is missing
# PROGRAM and PAP, so it is skipped today and picked up with no code change on
# the day those labels are filled in. Sheets that have figures but lack
# identifiers are reported in the sidebar as pending rather than ignored
# silently. Other Executive Offices arrived this way -- they were once their own
# pending sheet and are now folded into NGAs, and neither move needed an edit
# here.
#
# NGAs holds every agency at the 12-digit PREXC code; DPWH-sub holds DPWH at the
# 7-digit code, which aggregates its project lines. DPWH does not appear in
# NGAs, so binding them does not double count -- and the loader checks that
# assumption on every read rather than trusting it.
SHEET_IGNORE <- c("headers")     # templates and scratch sheets, by exact name

# The workbook is read from ph-budget-analysis at run time, the same way the
# agency dashboard reads its Google Sheet: publishing an update to the source
# repo is all that is needed to update this dashboard, with no redeploy.
#
# LOCAL_COPY is a development convenience only. If a file exists at this path
# it is used instead of the download, which makes it possible to test a change
# to the workbook before pushing it. It is NOT committed: a committed copy
# would have to be listed in manifest.json to reach the server at all, so it
# would silently go stale the first time the manifest was regenerated without
# it. Keep it in .gitignore.
LOCAL_COPY <- "data/Compiled_-_PAPs.xlsx"

# --- Units -----------------------------------------------------------------
# House convention across this repo is thousands of pesos, as DBM publishes.
# THIS WORKBOOK IS THE EXCEPTION: it stores pesos. Verified against figures
# checked line-by-line against the published DBM documents —
#
#   DOE                            NEP 2027   2,028,303,000
#   DOE                            GAA 2026   2,963,524,000
#   National Museum                NEP 2027   1,530,502,000
#   National Maritime Polytechnic  NEP 2021     132,094,000
#
# all four match the workbook exactly in pesos. Stored values are never
# converted; the selector rescales presentation only. If the workbook is ever
# restated in thousands, set SOURCE_UNIT to 1e3 and nothing else changes.
SOURCE_UNIT <- 1          # one stored unit = 1 peso

UNIT_CHOICES <- c(
  "\u20b1 Pesos (as stored)" = "pesos",
  "\u20b1 Thousands"         = "thousands",
  "\u20b1 Millions"          = "millions",
  "\u20b1 Billions"          = "billions"
)

unit_divisor <- function(u) switch(u, pesos = 1, thousands = 1e3,
                                   millions = 1e6, billions = 1e9, 1) / SOURCE_UNIT
unit_label   <- function(u) switch(u, pesos = "\u20b1", thousands = "\u20b1 '000",
                                   millions = "\u20b1 M", billions = "\u20b1 B", "\u20b1")
unit_digits  <- function(u) switch(u, pesos = 0, thousands = 0,
                                   millions = 2, billions = 3, 0)

# --- Expense classes -------------------------------------------------------
# The classes in play are read from the column headers (NEP_2027_EXP_1PS and so
# on), not hard-coded. These are only the display labels; a class that appears
# in the workbook without an entry here still shows, under its raw code, and is
# named in the sidebar so it can be given a label.
EXP_LABELS <- c(TOTAL   = "Total",
                "1PS"   = "Personnel Services (PS)",
                "2MOOE" = "Maintenance and Other Operating (MOOE)",
                "3FE"   = "Financial Expenses (FE)",
                "6CO"   = "Capital Outlays (CO)")

EXP_SHORT <- c(TOTAL = "Total", "1PS" = "PS", "2MOOE" = "MOOE",
               "3FE" = "FE", "6CO" = "CO")

# Total first, then by the leading digit of the class code.
exp_order <- function(codes) {
  c(intersect("TOTAL", codes), sort(setdiff(codes, "TOTAL")))
}

exp_label <- function(code) unname(coalesce(EXP_LABELS[code], code))
exp_short <- function(code) unname(coalesce(EXP_SHORT[code], code))

# --- Documents -------------------------------------------------------------
# Also read from the headers. Two orderings, deliberately different:
#
# Both orderings put NEP first: the proposal comes before the enacted budget,
# so a reader scanning left to right within a fiscal year sees what was asked
# for and then what was granted.
#
#   COLS  column order in the table
#   PLOT  bar order within each year group, so GAA sits on the RIGHT
#
# A document type that turns up in the workbook without an entry here still
# works: it is appended after the known ones and given a fallback fill.
DOC_ORDER_COLS <- c("NEP", "GAA")
DOC_ORDER_PLOT <- c("NEP", "GAA")

DOC_LABELS <- c(NEP = "NEP only", GAA = "GAA only")

order_docs <- function(docs, ref) c(intersect(ref, docs), sort(setdiff(docs, ref)))

# --- P/A/P type ------------------------------------------------------------
# Read from the head of the P/A/P label. The source is not uniform: it varies
# between singular and plural ("Locally-Funded Project:" in NGAs vs
# "Locally-Funded Projects:" in DPWH-sub), sometimes omits the noun
# ("Locally-Funded:"), and carries three PNP rows spelled "Locally-Funed
# Project:". These patterns absorb all of it. Anything else is a regular
# activity of the agency rather than a discrete project.
RX_LFP <- regex("^\\s*locally[-\\s]*fun[a-z]*ed", ignore_case = TRUE)
RX_FAP <- regex("^\\s*foreign[-\\s]*assisted", ignore_case = TRUE)

TYPE_REGULAR <- "Regular Activity"
TYPE_LFP     <- "Locally-Funded Project"
TYPE_FAP     <- "Foreign-Assisted Project"
TYPE_CHOICES <- c(TYPE_REGULAR, TYPE_LFP, TYPE_FAP)

# --- Program tier ----------------------------------------------------------
# The first digit of PREXC_PROG places a program in the GAA's own three tiers:
# 1 = General Administration and Support, 2 = Support to Operations,
# 3 and up = Operations. Taken from the code rather than the label, which is
# what makes it hold for every agency: tier 1 is always "1000" and tier 2
# always "2000", whatever wording the agency uses.
#
# Overhead is worth being able to set aside -- GAS and STO are the running
# cost of the agency, not the service it delivers -- so each tier is a toggle.
TIER_GAS <- "General Administration and Support"
TIER_STO <- "Support to Operations"
TIER_OPS <- "Operations"
TIER_CHOICES <- c(TIER_OPS, TIER_STO, TIER_GAS)

prexc_tier <- function(prog) {
  case_when(str_sub(prog, 1, 1) == "1" ~ TIER_GAS,
            str_sub(prog, 1, 1) == "2" ~ TIER_STO,
            TRUE                       ~ TIER_OPS)
}

# --- Table grain -----------------------------------------------------------
# P/A/P is the default. Program rolls the P/A/Ps up to their headline program,
# which is the level most readers start from.
GRAIN_PAP  <- "pap"
GRAIN_PROG <- "program"
GRAIN_CHOICES <- c("P/A/P detail" = GRAIN_PAP, "Program totals" = GRAIN_PROG)

# Identity columns by grain: the program roll-up has no P/A/P column, so the
# frozen block and its widths shrink with it.
ID_COLS_BY_GRAIN <- list(
  pap     = c("Department", "Agency", "Program", "P/A/P"),
  program = c("Department", "Agency", "Program")
)
# Both identity blocks total the same width, so the figure columns begin at the
# same point and line up between the two views. Trimmed from the earlier
# figures to give the numbers more room.
ID_WIDTHS_BY_GRAIN <- list(
  pap     = c("90px", "100px", "110px", "140px"),   # 440px
  program = c("110px", "125px", "205px")            # 440px
)

# Every figure column is the same fixed width in both views. Left to itself DT
# apportions whatever is spare, which is what let the first figure column
# collapse when the identity block lost a column.
AMT_COL_WIDTH <- "94px"

ID_COLS <- c("DEPARTMENT", "UACS_DPT_DSC", "AGENCY", "UACS_AGY_DSC",
             "PREXC_PROG", "PROGRAM", "PREXC_SUBPROG", "PAP")

# --- Cache -----------------------------------------------------------------
# How long a fetched copy of the workbook is reused before being refetched.
# Override at deploy time with PBC_PAP_CACHE_TTL (seconds). Namespaced per
# dashboard so a sibling app can be tuned independently.
CACHE_TTL_SECONDS <- as.numeric(Sys.getenv("PBC_PAP_CACHE_TTL", "3600"))

# Short enough to recover quickly from a transient outage, long enough that a
# sustained one does not turn every page view into another doomed request.
CACHE_RETRY_SECONDS <- 120

# --- Responsive behavior --------------------------------------------------
# Below this viewport width the app switches to its narrow layout: the table
# collapses its identity columns, drops frozen columns, and the trend chart
# shrinks. Matches the breakpoint used by the agency dashboard.
MOBILE_BREAKPOINT <- 768

# Widest identity block, used only to size the CSS selectors. The block itself
# is chosen per grain from ID_COLS_BY_GRAIN. Every identity column is frozen
# and sized: freezing a leading pair would scroll Program and P/A/P out of
# view, which are the two a reader needs while comparing years.
N_ID_PAP  <- length(ID_COLS_BY_GRAIN$pap)
N_ID_PROG <- length(ID_COLS_BY_GRAIN$program)
N_ID_COLS <- max(N_ID_PAP, N_ID_PROG)

# House palette
PBC_NAVY  <- "#1B4965"
# A deeper navy for the enacted GAA bar, so the pair reads as proposal (light)
# then enacted (dark) rather than as two arbitrary colors.
PBC_NAVY_DEEP <- "#10293C"
PBC_BLUE  <- "#5FA8D3"
PBC_TEAL  <- "#62B6CB"
PBC_RUST  <- "#BC4749"
PBC_GREEN <- "#386641"
PBC_GREY  <- "#6C757D"

# ---------------------------------------------------------------------------
# 1. Helpers
# ---------------------------------------------------------------------------

# Wrap long labels across lines. Deliberately does NOT truncate: several P/A/Ps
# differ only in their tail, so an ellipsis would make them indistinguishable.
wrap_lab <- function(x, width = 34) str_wrap(x, width = width)

# Column name for a series and expense class, e.g. "NEP_2027_EXP_TOTAL".
amt_col <- function(series, class) str_c(series, "_EXP_", class)

# Header a reader sees, e.g. "NEP 2027 — Total".
amt_header <- function(col) {
  series <- str_remove(col, "_EXP_.*")
  class  <- str_remove(col, "^.*_EXP_")
  str_c(str_replace(series, "_", " "), " \u2014 ", exp_short(class))
}

# ---------------------------------------------------------------------------
# 2. Ingest
# ---------------------------------------------------------------------------

# readxl needs a file on disk, so a remote workbook is downloaded first.
# Everything is read as character and parsed here, so that we control the
# numeric conversion: an empty cell must become NA and a literal "0" must stay
# 0 until the cleaning rules below decide otherwise.
fetch_workbook_raw <- function() {
  path <- if (file.exists(LOCAL_COPY)) {
    LOCAL_COPY
  } else {
    tmp <- tempfile(fileext = ".xlsx")
    utils::download.file(DATA_URL, tmp, mode = "wb", quiet = TRUE)
    tmp
  }

  # Classify every sheet by what its header carries.
  #   ready   : all identifiers and at least one figure column -> read
  #   pending : figures but missing identifiers -> named in the sidebar
  #   other   : neither -> ignored without comment (pivot tables, scratch)
  ready <- character(0); pending <- list()

  for (nm in setdiff(excel_sheets(path), SHEET_IGNORE)) {
    hdr <- tryCatch(names(read_xlsx(path, sheet = nm, n_max = 0)),
                    error = function(e) character(0))
    if (!length(hdr)) next
    has_amt <- any(str_detect(hdr, "_EXP_"))
    miss_id <- setdiff(ID_COLS, hdr)
    if (has_amt && !length(miss_id)) ready <- c(ready, nm)
    else if (has_amt) pending[[nm]] <- miss_id
  }

  if (!length(ready)) {
    stop("No sheet in the workbook carries both the identifier columns (",
         str_c(ID_COLS, collapse = ", "), ") and figures.")
  }

  raw <- bind_rows(lapply(ready, function(s) {
    read_xlsx(path, sheet = s, col_types = "text") %>% mutate(SHEET = s)
  }))

  attr(raw, "sheets_read")    <- ready
  attr(raw, "sheets_pending") <- pending
  raw
}

# --- Cleaning ---------------------------------------------------------------
# Applied in this order, and the order matters:
#
#   1. Drop P/A/Ps that have not been labeled yet.
#   2. Blanks become zeroes across every expense column. Without this the
#      year test in step 4 returns NA whenever a year mixes zeroes and blanks,
#      so the result would depend on which cells happen to be empty.
#   3. Drop rows that are zero in every expense column of every year — they
#      carry nothing in any document or any year.
#   4. Re-blank whole years that are entirely zero, so they render as "no data"
#      rather than as a real zero.
#
# The year group in step 4 spans BOTH documents, so NEP and GAA for a year are
# blanked together. FY2027 has no GAA, so its group is NEP-only.
blank_zero_years <- function(d, years) {
  for (y in years) {
    cols <- names(d)[str_detect(names(d), paste0("_", y, "_EXP_"))]
    if (!length(cols)) next
    flag <- rowSums(as.matrix(d[cols]) != 0) == 0
    d[cols] <- lapply(d[cols], function(v) if_else(flag, NA_real_, v))
  }
  d
}

tidy_pap <- function(raw) {

  missing_id <- setdiff(ID_COLS, names(raw))
  if (length(missing_id)) {
    stop("Workbook is missing identifier column(s): ",
         str_c(missing_id, collapse = ", "))
  }

  amt_cols <- names(raw)[str_detect(names(raw), "_EXP_")]
  if (!length(amt_cols)) stop("No amount columns matching '_EXP_' were found.")

  raw <- raw %>% mutate(across(all_of(amt_cols),
                               ~ suppressWarnings(as.numeric(.x))))

  years <- sort(unique(str_extract(amt_cols, "(?<=_)\\d{4}(?=_EXP_)")))

  n_raw <- nrow(raw)
  labeled <- raw %>% filter(!is.na(PAP))
  n_unlabeled <- n_raw - nrow(labeled)

  filled <- labeled %>%
    mutate(across(all_of(amt_cols), ~ replace_na(.x, 0)))

  kept <- filled %>% filter(if_any(all_of(amt_cols), ~ .x != 0))
  n_empty <- nrow(filled) - nrow(kept)

  dat <- kept %>%
    blank_zero_years(years) %>%
    mutate(
      PROGRAM  = coalesce(PROGRAM, "(program not labeled)"),
      PAP_TYPE = case_when(str_detect(PAP, RX_LFP) ~ TYPE_LFP,
                           str_detect(PAP, RX_FAP) ~ TYPE_FAP,
                           TRUE                    ~ TYPE_REGULAR),
      # Statutory order, never alphabetical. DEPARTMENT and AGENCY are the
      # fixed-width UACS numeric codes and follow the GAA's own structural
      # sequence; PREXC codes order the P/A/Ps within an agency. The codes are
      # ordering keys only and are not shown in the table.
      AGENCY_KEY = str_c(DEPARTMENT, "|", AGENCY),
      PROG_TIER  = prexc_tier(PREXC_PROG),
      SORT_KEY = str_c(DEPARTMENT, AGENCY, PREXC_PROG,
                       str_pad(PREXC_SUBPROG, 12, "right", "0"))
    ) %>%
    arrange(SORT_KEY)

  # Vocabularies are read from the headers, so a new fiscal year, a new
  # document type or a new expense class needs no code change.
  docs    <- order_docs(unique(str_extract(amt_cols, "^[A-Z]+")), DOC_ORDER_COLS)
  classes <- exp_order(unique(str_extract(amt_cols, "(?<=_EXP_).*$")))

  # Series in fiscal order, NEP before GAA within a year. Built from the
  # amount columns rather than assumed, so a year that carries only a NEP (the
  # newest proposal) or only a GAA is handled without special-casing.
  series <- unique(str_remove(amt_cols, "_EXP_.*$"))
  series <- series[order(str_extract(series, "\\d{4}"),
                         match(str_extract(series, "^[A-Z]+"), docs))]

  # A sheet that repeats rows already present in another would double count
  # silently. Checked on every read rather than assumed. Note that a renamed
  # agency produces two rows on one code legitimately, so the count is reported
  # rather than treated as fatal -- it is a prompt to look, not a failure.
  dup_keys <- dat %>%
    count(DEPARTMENT, AGENCY, PREXC_SUBPROG, name = "n") %>%
    filter(n > 1) %>% nrow()

  # One program CODE can carry two different PROGRAM labels inside one agency.
  # Two cases exist today: the National Museum's program 3101 has its 40
  # locally-funded rows labeled "Locally-Funded Projects" rather than "Museums
  # Program", and one of DOH-OSEC's 26 rows under 3103 says "Health Systems
  # Strengthening Program" where the other 25 say "Public Health Program".
  #
  # The code is the identity, so the roll-up groups on it and shows the label
  # used by the most P/A/Ps. The collisions are counted and reported, because
  # they are encoding slips worth fixing at source rather than facts about the
  # budget.
  prog_labels <- dat %>%
    count(DEPARTMENT, AGENCY, PREXC_PROG, PROGRAM, name = "n") %>%
    arrange(DEPARTMENT, AGENCY, PREXC_PROG, desc(n))

  prog_canon <- prog_labels %>%
    distinct(DEPARTMENT, AGENCY, PREXC_PROG, .keep_all = TRUE) %>%
    select(DEPARTMENT, AGENCY, PREXC_PROG, PROGRAM_CANON = PROGRAM)

  prog_label_clashes <- prog_labels %>%
    count(DEPARTMENT, AGENCY, PREXC_PROG, name = "k") %>%
    filter(k > 1) %>% nrow()

  dat <- dat %>%
    left_join(prog_canon, by = c("DEPARTMENT", "AGENCY", "PREXC_PROG"))

  unknown_classes <- setdiff(classes, names(EXP_LABELS))
  unknown_docs    <- setdiff(docs, DOC_ORDER_COLS)

  # Department and agency pick lists in code order, not alphabetical.
  depts <- dat %>% distinct(DEPARTMENT, UACS_DPT_DSC) %>%
    arrange(DEPARTMENT) %>% pull(UACS_DPT_DSC)

  # Agency directory. The NAME is not unique -- "Office of the Secretary"
  # belongs to 22 different departments -- so the selector is keyed on
  # DEPARTMENT + AGENCY and never on the label. Every label carries its
  # department abbreviation, which both disambiguates the duplicates and lets a
  # reader search by department ("DOE") in the agency box.
  # One code can carry more than one NAME over the series, because agencies get
  # renamed: department 08 / agency 004 is both "Philippine State College of
  # Aeronautics" and "National Aviation Academy of the Philippines". The code is
  # the identity, so the two are one entry in the picker and one continuous
  # series in the table. The label shown is the one still in use in the most
  # recent document, so the picker reflects what an agency is called now.
  latest_total <- amt_cols[str_ends(amt_cols, "_EXP_TOTAL")]
  latest_total <- latest_total[length(latest_total)]

  agencies <- dat %>%
    mutate(AGENCY_KEY = str_c(DEPARTMENT, "|", AGENCY),
           .live = !is.na(.data[[latest_total]])) %>%
    group_by(DEPARTMENT, UACS_DPT_DSC, AGENCY, UACS_AGY_DSC, AGENCY_KEY) %>%
    summarize(live = any(.live), .groups = "drop") %>%
    arrange(DEPARTMENT, AGENCY, desc(live)) %>%
    distinct(AGENCY_KEY, .keep_all = TRUE) %>%
    mutate(
      DEPT_ABBR  = coalesce(str_match(UACS_DPT_DSC, "\\(([^)]+)\\)$")[, 2],
                            UACS_DPT_DSC),
      AGENCY_LABEL = str_c(UACS_AGY_DSC, " \u2014 ", DEPT_ABBR)
    ) %>%
    select(-live)

  list(
    data       = dat,
    years      = years,
    series     = series,
    docs       = docs,
    classes    = classes,
    amt_cols   = amt_cols,
    depts      = depts,
    agencies   = agencies,
    n_rows     = nrow(dat),
    n_agencies = n_distinct(str_c(dat$DEPARTMENT, dat$AGENCY)),
    n_unlabeled = n_unlabeled,
    n_empty      = n_empty,
    sheets_read     = attr(raw, "sheets_read") %||% character(0),
    sheets_pending  = attr(raw, "sheets_pending") %||% list(),
    dup_keys        = dup_keys,
    prog_label_clashes = prog_label_clashes,
    unknown_classes = unknown_classes,
    unknown_docs    = unknown_docs
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Shared cache -----------------------------------------------------------
# Lives at package scope rather than inside server(), so it persists across
# sessions. Only the first visitor after a cold start or an expiry pays the
# download and the pipeline.
.cache <- new.env(parent = emptyenv())
.cache$pap        <- NULL
.cache$fetched_at <- NULL
.cache$next_check <- NULL
.cache$last_error <- NULL

cache_is_fresh <- function() {
  !is.null(.cache$pap) && !is.null(.cache$next_check) && Sys.time() < .cache$next_check
}

# On failure with a usable copy already cached, the stale copy is kept and the
# error recorded rather than thrown: a GitHub hiccup should degrade the
# dashboard to slightly old figures, not blank it. With nothing cached there is
# nothing to fall back on, so the error propagates.
load_pap_data <- function(force = FALSE) {
  if (!force && cache_is_fresh()) return(invisible(FALSE))

  raw <- try(fetch_workbook_raw(), silent = TRUE)
  if (inherits(raw, "try-error")) {
    .cache$last_error <- conditionMessage(attr(raw, "condition"))
    .cache$next_check <- Sys.time() + CACHE_RETRY_SECONDS
    if (is.null(.cache$pap)) stop("Could not read the workbook: ", .cache$last_error)
    return(invisible(FALSE))
  }

  parsed <- try(tidy_pap(raw), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    .cache$last_error <- conditionMessage(attr(parsed, "condition"))
    .cache$next_check <- Sys.time() + CACHE_RETRY_SECONDS
    if (is.null(.cache$pap)) stop("Could not parse the workbook: ", .cache$last_error)
    return(invisible(FALSE))
  }

  .cache$pap        <- parsed
  .cache$fetched_at <- Sys.time()
  .cache$next_check <- Sys.time() + CACHE_TTL_SECONDS
  .cache$last_error <- NULL
  invisible(TRUE)
}

load_pap_data()
PAP0 <- .cache$pap

# ---------------------------------------------------------------------------
# 3. Responsive plumbing
# ---------------------------------------------------------------------------
# Shiny renders plots server-side as raster images, so it cannot reflow them
# the way CSS reflows a div; the server has to know how wide the screen is.
viewport_reporter <- tags$script(HTML("
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
"))

app_css <- tags$style(HTML(sprintf("
  .card-body { padding: 0.6rem 0.75rem; }

  table.dataTable thead th,
  table.dataTable tbody td {
    font-size: 0.80rem;
    padding: 0.28rem 0.45rem;
  }

  /* Identity columns wrap rather than truncate -- several P/A/Ps differ only
     in their tail -- and carry a smaller face than the numbers, which are what
     a reader is actually comparing.

     The count differs by grain: four columns at P/A/P level, three at Program
     level. A single nth-child rule sized to the wider grain would style the
     first FIGURE column as an identity column in the Program view, wrapping and
     shrinking its header. So the table carries a grain class and each grain
     gets its own rule.

     FixedColumns clones the frozen block into its own table. That clone holds
     only identity columns whichever grain is showing, so it can be styled
     wholesale without counting. */
  .pap-grain-pap  thead th:nth-child(-n+%d),
  .pap-grain-pap  tbody td:nth-child(-n+%d),
  .pap-grain-prog thead th:nth-child(-n+%d),
  .pap-grain-prog tbody td:nth-child(-n+%d),
  .DTFC_LeftBodyWrapper table tbody td,
  .DTFC_LeftHeadWrapper table thead th {
    white-space: normal !important;
    word-break: break-word;
    font-size: 0.72rem;
    line-height: 1.2;
    vertical-align: top;
  }

  /* Figures never wrap. Their headers do, so a long series label sits on two
     lines instead of squeezing the column. */
  .pap-grain-pap  tbody td:nth-child(n+%d),
  .pap-grain-prog tbody td:nth-child(n+%d) { white-space: nowrap; }
  .pap-grain-pap  thead th:nth-child(n+%d),
  .pap-grain-prog thead th:nth-child(n+%d) {
    white-space: normal;
    line-height: 1.15;
    vertical-align: bottom;
  }

  /* Selection recap under the trend chart. */
  .pap-recap {
    border-top: 1px solid #e3e8ee;
    margin: 0.35rem 0.25rem 0 0.25rem;
    padding: 0.5rem 0.25rem 0.15rem 0.25rem;
    font-size: 0.85rem;
  }
  .pap-recap .recap-head { color: %s; font-weight: 600; margin-bottom: 0.3rem; }
  .pap-recap ul { margin: 0; padding-left: 1.1rem; }
  .pap-recap li { color: %s; margin-bottom: 0.1rem; }
  .pap-recap .recap-all { color: %s; }
  .pap-recap code { color: %s; background: #f1f5f8; padding: 0 0.2rem; }

  /* Scroller sizes its own body; keep the wrapper from adding a second bar. */
  .dataTables_scrollBody { border-bottom: 1px solid #e3e8ee; }

  @media (max-width: %dpx) {
    .card-body { padding: 0.5rem !important; }
    .navbar-brand { font-size: 1rem; }
    table.dataTable thead th, table.dataTable tbody td {
      font-size: 0.76rem; padding: 0.3rem 0.4rem;
    }
    /* The identity column is agency and P/A/P joined, which can run long.
       Cap and wrap it, or it pushes every figure off the side of the screen
       and the horizontal scroll never reaches them. */
    table.dataTable thead th:first-child,
    table.dataTable tbody td:first-child {
      max-width: 165px;
      min-width: 165px;
      white-space: normal !important;
      word-break: break-word;
      font-size: 0.72rem;
      line-height: 1.2;
    }
    /* Give the sidebar room to breathe once it is opened from the toggle. */
    .bslib-sidebar-layout > .sidebar { font-size: 0.9rem; }
  }
",
   # sprintf is positional: these must stay in the order the placeholders
   # appear above -- eight column counts, four colors, then the breakpoint.
   N_ID_PAP, N_ID_PAP, N_ID_PROG, N_ID_PROG,
   N_ID_PAP + 1, N_ID_PROG + 1, N_ID_PAP + 1, N_ID_PROG + 1,
   PBC_NAVY, PBC_GREY, PBC_GREY, PBC_NAVY,
   MOBILE_BREAKPOINT)))

# ---------------------------------------------------------------------------
# 4. UI
# ---------------------------------------------------------------------------

ui <- page_navbar(
  id = "nav",
  title = "P/A/P Browser",
  header = tags$head(
    tags$meta(name = "viewport",
              content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    viewport_reporter,
    app_css
  ),
  theme = bs_theme(
    version = 5,
    primary = PBC_NAVY,
    base_font = font_google("Source Sans 3", local = FALSE),
    heading_font = font_google("Source Sans 3", local = FALSE)
  ),
  # Tabs whose content should stretch to the viewport rather than scroll.
  fillable = "browse",

  sidebar = sidebar(
    width = 330,
    title = "Filters",

    # -- what to look at ---------------------------------------------------
    selectInput("dept", "Department",
                choices = c("All departments" = "", PAP0$depts), selected = ""),

    # Populated with every agency at startup and searchable, so an agency can
    # be found without picking its department first. Selecting a department
    # narrows the list; it is not a precondition.
    selectizeInput(
      "agency", "Agency",
      choices  = c("All agencies" = "",
                   setNames(PAP0$agencies$AGENCY_KEY, PAP0$agencies$AGENCY_LABEL)),
      selected = "",
      options  = list(placeholder = "All agencies",
                      searchField = c("label"))
    ),

    textInput("q_program", "Search PROGRAM",
              placeholder = "headline program, e.g. health"),

    textInput("q_pap", "Search P/A/P",
              placeholder = "granular line, e.g. school building"),

    checkboxGroupInput("pap_type", "P/A/P type",
                       choices = TYPE_CHOICES, selected = TYPE_CHOICES),

    # Overhead on or off. Both are on by default, so the opening view is still
    # the whole data set; unticking them leaves only the programs that deliver
    # a service.
    checkboxGroupInput("tier", "Program tier",
                       choices = TIER_CHOICES, selected = TIER_CHOICES),

    hr(),

    # -- how to show it ----------------------------------------------------
    # Built from the document types found in the workbook, so a third one
    # would appear here on its own.
    radioButtons("grain", "Show",
                 choices = GRAIN_CHOICES, selected = GRAIN_PAP, inline = TRUE),

    radioButtons(
      "doc", "Document",
      choices = c(
        setNames("BOTH", if (length(PAP0$docs) > 1) "Both" else "All"),
        setNames(PAP0$docs,
                 vapply(PAP0$docs,
                        function(d) unname(coalesce(DOC_LABELS[d], str_c(d, " only"))),
                        character(1)))
      ),
      selected = "BOTH", inline = TRUE
    ),

    selectizeInput("years", "Fiscal years",
                   choices = PAP0$years, selected = PAP0$years, multiple = TRUE,
                   options = list(plugins = list("remove_button"))),

    # Also from the workbook. An expense class added by DBM appears here under
    # its raw code until a label is added to EXP_LABELS.
    checkboxGroupInput(
      "classes", "Expense class",
      choices  = setNames(PAP0$classes,
                          vapply(PAP0$classes, exp_label, character(1))),
      selected = intersect("TOTAL", PAP0$classes)
    ),

    selectInput("unit", "Display units",
                choices = UNIT_CHOICES, selected = "millions"),

    hr(),
    actionButton("refresh", "Refresh from source", class = "btn-sm btn-outline-secondary"),
    uiOutput("source_note")
  ),

  nav_panel(
    "Browse", value = "browse", icon = icon("table"),
    # card(fill) + card_body(fillable) is what gives the table the viewport.
    # Bare outputs in a fillable nav_panel collapse to zero height.
    card(
      full_screen = TRUE, fill = TRUE,
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span("Programs, Activities and Projects"),
            downloadButton("dl", "Download CSV", class = "btn-sm btn-primary"))
      ),
      card_body(
        fillable = TRUE, padding = 8,
        DTOutput("tbl", height = "100%")
      )
    )
  ),

  nav_panel(
    "Trend", value = "trend",
    card(
      card_header("Total of the current selection, by fiscal year"),
      plotOutput("trend", height = "440px"),
      # What "current selection" actually means, spelled out under the chart.
      # Without it a filtered total reads as a whole-of-budget total.
      div(class = "pap-recap", uiOutput("selection_recap")),
      div(class = "small text-muted px-2 pb-1",
          "A fiscal year in which every expense column is zero carries no ",
          "information and is omitted rather than plotted at zero. ",
          "The newest fiscal year has no enacted GAA.")
    )
  ),

  nav_panel("Notes", value = "notes", card(card_body(htmlOutput("notes"))))
)

# ---------------------------------------------------------------------------
# 5. Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  pap <- reactiveVal(PAP0)

  observeEvent(input$refresh, {
    ok <- try(load_pap_data(force = TRUE), silent = TRUE)
    if (!inherits(ok, "try-error")) pap(.cache$pap)
    showNotification(
      if (is.null(.cache$last_error)) "Refreshed from source."
      else paste("Refresh failed; showing last good copy.", .cache$last_error),
      type = if (is.null(.cache$last_error)) "message" else "warning"
    )
  })

  is_mobile <- reactive({
    w <- input$viewport_width
    !is.null(w) && is.numeric(w) && w < MOBILE_BREAKPOINT
  })

  # -- department narrows the agency list, but is not a precondition -------
  # An agency can be chosen with no department set. Selecting a department
  # trims the list to that department, and keeps the current agency if it
  # still belongs there rather than silently resetting it.
  observeEvent(input$dept, {
    ag <- pap()$agencies
    if (nzchar(input$dept)) ag <- ag %>% filter(UACS_DPT_DSC == input$dept)
    keep <- if (!is.null(input$agency) && input$agency %in% ag$AGENCY_KEY)
      input$agency else ""
    updateSelectizeInput(
      session, "agency",
      choices  = c("All agencies" = "", setNames(ag$AGENCY_KEY, ag$AGENCY_LABEL)),
      selected = keep
    )
  }, ignoreInit = TRUE)

  # -- which amount columns to show ----------------------------------------
  shown_series <- reactive({
    s <- pap()$series
    if (input$doc != "BOTH") s <- s[str_starts(s, input$doc)]
    yrs <- input$years
    if (!length(yrs)) yrs <- pap()$years
    s[str_extract(s, "\\d{4}") %in% yrs]
  })

  shown_cols <- reactive({
    cls <- input$classes
    if (!length(cls)) cls <- "TOTAL"
    cols <- as.vector(t(outer(shown_series(), cls, amt_col)))
    cols[cols %in% pap()$amt_cols]
  })

  # -- filtered rows --------------------------------------------------------
  filtered <- reactive({
    d <- pap()$data

    if (nzchar(input$dept))   d <- d %>% filter(UACS_DPT_DSC == input$dept)
    # Keyed on DEPARTMENT + AGENCY, never on the name: filtering on
    # "Office of the Secretary" would return 22 departments' worth of rows.
    if (nzchar(input$agency %||% "")) d <- d %>% filter(AGENCY_KEY == input$agency)

    if (length(input$pap_type)) {
      d <- d %>% filter(PAP_TYPE %in% input$pap_type)
    } else {
      d <- d[0, ]
    }

    if (length(input$tier)) {
      d <- d %>% filter(PROG_TIER %in% input$tier)
    } else {
      d <- d[0, ]
    }

    q1 <- str_trim(input$q_program %||% "")
    if (nzchar(q1)) d <- d %>% filter(str_detect(PROGRAM, fixed(q1, ignore_case = TRUE)))

    q2 <- str_trim(input$q_pap %||% "")
    if (nzchar(q2)) d <- d %>% filter(str_detect(PAP, fixed(q2, ignore_case = TRUE)))

    d
  })

  # -- table ----------------------------------------------------------------
  # PREXC codes are ordering keys, not content: they are used to sort and are
  # then dropped, so the reader sees names rather than numbers.

  # Blanks are not zeroes, so a roll-up cannot simply sum(na.rm = TRUE): a
  # program whose every P/A/P is blank for a year would come out as a real 0.
  sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

  # Rows at the requested grain, before any unit scaling or column renaming.
  # Program totals roll the P/A/Ps up to their headline program; the P/A/P and
  # type columns fall away with them.
  grained <- reactive({
    d <- filtered()
    cols <- shown_cols()
    if (identical(input$grain, GRAIN_PROG)) {
      # Grouped on CODES only -- department, agency, program -- never on the
      # display names. One program code is one row even where the workbook
      # spells its program name two ways, and equally where it spells the
      # AGENCY two ways: the Office of the Presidential Adviser on the Peace
      # Process became ...on Peace, Reconciliation and Unity mid-series, and
      # grouping on the name split its program into two half-rows. Display
      # names are taken from the last row in the group, the same
      # most-recent-name rule the agency picker uses.
      d %>%
        group_by(DEPARTMENT, AGENCY, PREXC_PROG, PROGRAM = PROGRAM_CANON) %>%
        summarize(UACS_DPT_DSC = dplyr::last(UACS_DPT_DSC),
                  UACS_AGY_DSC = dplyr::last(UACS_AGY_DSC),
                  across(all_of(cols), sum_or_na),
                  N_PAPS = n(), .groups = "drop") %>%
        arrange(DEPARTMENT, AGENCY, PREXC_PROG)
    } else {
      d
    }
  })

  id_cols   <- reactive(ID_COLS_BY_GRAIN[[input$grain %||% GRAIN_PAP]])
  id_widths <- reactive(ID_WIDTHS_BY_GRAIN[[input$grain %||% GRAIN_PAP]])

  table_data <- reactive({
    cols <- shown_cols()
    div  <- unit_divisor(input$unit)
    prog_grain <- identical(input$grain, GRAIN_PROG)

    d <- grained()

    if (is_mobile()) {
      # Identity collapses into one column on a phone: several identity columns
      # plus a year scroll leaves no room for the figures.
      out <- d %>%
        transmute(IDENTITY = if (prog_grain)
                    str_c(UACS_AGY_DSC, " \u2014 ", PROGRAM)
                  else
                    str_c(UACS_AGY_DSC, " \u2014 ", PAP),
                  across(all_of(cols), ~ .x / div))
      names(out)[1] <- if (prog_grain) "Agency \u2014 Program" else "Agency \u2014 P/A/P"
    } else if (prog_grain) {
      out <- d %>%
        transmute(Department = UACS_DPT_DSC,
                  Agency     = UACS_AGY_DSC,
                  Program    = PROGRAM,
                  across(all_of(cols), ~ .x / div))
    } else {
      # PAP_TYPE is a filter, not a column: it is one of three repeated values,
      # so it costs width without telling a reader anything the P/A/P label
      # does not already say.
      out <- d %>%
        transmute(Department = UACS_DPT_DSC,
                  Agency     = UACS_AGY_DSC,
                  Program    = PROGRAM,
                  `P/A/P`    = PAP,
                  across(all_of(cols), ~ .x / div))
    }
    out
  })

  output$tbl <- renderDT({
    cols <- shown_cols()
    d <- table_data()
    n_id <- ncol(d) - length(cols)
    widths <- if (is_mobile()) character(0) else id_widths()
    # Tells the stylesheet how many leading columns are identity, so the first
    # figure column is not styled as one when the grain changes.
    grain_class <- if (identical(input$grain, GRAIN_PROG))
      "pap-grain-prog" else "pap-grain-pap"
    names(d) <- c(names(d)[seq_len(n_id)], vapply(cols, amt_header, character(1)))

    mob <- is_mobile()

    if (mob) {
      # Ordinary paging on a phone: mobile browsers resize the viewport as
      # their chrome hides and reappears, which makes a vh-sized scroll body
      # jump while the reader scrolls. FixedColumns is also unreliable on touch.
      dt <- datatable(
        d,
        rownames = FALSE,
        selection = "none",
        options = list(
          order = list(),
          # No "f": DT's own search box is redundant next to the sidebar
          # searches, and dropping it returns a row of vertical space.
          dom = "tip",
          pageLength = 15,
          lengthChange = FALSE,
          scrollX = TRUE,
          autoWidth = FALSE,
          columnDefs = list(
            list(className = "dt-right", targets = seq(n_id, ncol(d) - 1))
          )
        )
      )
    } else {
      # Paging rather than DT's Scroller extension. Scroller virtualizes by
      # assuming every row is the same height, and the identity columns wrap to
      # a variable number of lines -- which made it mis-size the body, leaving a
      # short table that still scrolled even on a handful of rows. Paging at 100
      # keeps the DOM small without that assumption.
      #
      # scrollCollapse is deliberately OFF: with it on, the body shrinks to fit
      # the rows and the card is left half empty when a filter is narrow. Off,
      # the body always occupies the viewport height it is given.
      dt <- datatable(
        d,
        rownames = FALSE,
        selection = "none",
        class = paste("compact stripe hover", grain_class),
        extensions = "FixedColumns",
        options = list(
          order = list(),
          # No "f": DT's own search box is redundant next to the sidebar
          # searches, and dropping it returns a row of vertical space.
          dom = "tip",
          paging = TRUE,
          pageLength = 100,
          lengthChange = FALSE,
          scrollY = "calc(100vh - 235px)",
          scrollX = TRUE,
          scrollCollapse = FALSE,
          autoWidth = TRUE,
          # All four identity columns are frozen. Freezing two of four would
          # scroll Program and P/A/P out of view, which are the two a reader
          # needs to keep sight of while comparing years.
          fixedColumns = list(leftColumns = n_id),
          columnDefs = c(
            lapply(seq_len(n_id) - 1, function(i) {
              list(width = widths[i + 1], targets = i)
            }),
            list(list(className = "dt-right", width = AMT_COL_WIDTH,
                      targets = seq(n_id, ncol(d) - 1)))
          )
        )
      )
    }

    if (length(cols)) {
      dt <- dt %>% formatRound(columns = names(d)[-seq_len(n_id)],
                               digits = unit_digits(input$unit), mark = ",")
    }
    dt
  }, server = TRUE)

  # -- trend -----------------------------------------------------------------
  output$trend <- renderPlot({
    d <- filtered()
    s <- shown_series()
    if (!nrow(d) || !length(s)) return(NULL)

    div <- unit_divisor(input$unit)

    tot <- tibble(series = s) %>%
      mutate(
        DOC = str_extract(series, "^[A-Z]+"),
        FY  = as.integer(str_extract(series, "\\d{4}")),
        # An all-blank year stays blank rather than collapsing to a zero.
        value = vapply(series, function(ss) {
          v <- d[[amt_col(ss, "TOTAL")]]
          if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE) / div
        }, numeric(1))
      ) %>%
      filter(!is.na(value))

    if (!nrow(tot)) return(NULL)

    # GAA sits to the LEFT and NEP to the RIGHT within each fiscal year. The
    # factor levels drive the dodge order, so this is set explicitly: left to
    # itself ggplot orders alphabetically, which happens to give the same pair
    # today and would not survive a third document type.
    lev <- order_docs(unique(tot$DOC), DOC_ORDER_PLOT)
    tot$DOC <- factor(tot$DOC, levels = lev)

    # Fills for whatever document types are present, so an unrecognized one
    # still draws rather than dropping out of the plot.
    fills <- c(NEP = PBC_BLUE, GAA = PBC_NAVY_DEEP)
    extra <- setdiff(lev, names(fills))
    if (length(extra)) {
      spare <- c(PBC_TEAL, PBC_RUST, PBC_GREEN, PBC_GREY)
      fills <- c(fills, setNames(rep_len(spare, length(extra)), extra))
    }

    base_sz <- if (is_mobile()) 11 else 13
    val_sz  <- if (is_mobile()) 2.7 else 3.2

    ggplot(tot, aes(x = factor(FY), y = value, fill = DOC)) +
      geom_col(position = position_dodge2(preserve = "single"), width = 0.72) +
      geom_text(aes(label = comma(value, accuracy = 0.1)),
                position = position_dodge2(width = 0.72, preserve = "single"),
                vjust = -0.4, size = val_sz, color = PBC_NAVY) +
      scale_fill_manual(values = fills, breaks = lev, name = NULL) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.13))) +
      labs(x = "Fiscal year", y = sprintf("Total (%s)", unit_label(input$unit))) +
      theme_minimal(base_size = base_sz) +
      theme(panel.grid.major.x = element_blank(),
            legend.position = "top",
            plot.margin = margin(10, 14, 10, 10))
  })

  # -- what "the current selection" actually is -------------------------------
  # Printed under the trend chart. A filtered total looks exactly like a
  # whole-of-budget total, so the filters in force are stated rather than left
  # for the reader to remember.
  output$selection_recap <- renderUI({
    p <- pap()
    d <- filtered()

    esc <- function(x) htmltools::htmlEscape(x)
    q_prog <- str_trim(input$q_program %||% "")
    q_pap  <- str_trim(input$q_pap %||% "")
    types  <- input$pap_type %||% character(0)
    yrs    <- input$years %||% p$years
    cls    <- input$classes; if (!length(cls)) cls <- "TOTAL"

    # Narrowing filters: listed only when they are actually narrowing.
    bits <- character(0)

    if (nzchar(input$dept))
      bits <- c(bits, sprintf("Department: <b>%s</b>", esc(input$dept)))

    if (nzchar(input$agency %||% "")) {
      lab <- p$agencies$AGENCY_LABEL[match(input$agency, p$agencies$AGENCY_KEY)]
      bits <- c(bits, sprintf("Agency: <b>%s</b>", esc(lab %||% input$agency)))
    }

    if (nzchar(q_prog))
      bits <- c(bits, sprintf("PROGRAM contains <code>%s</code>", esc(q_prog)))

    if (nzchar(q_pap))
      bits <- c(bits, sprintf("P/A/P contains <code>%s</code>", esc(q_pap)))

    all_types <- c(TYPE_REGULAR, TYPE_LFP, TYPE_FAP)
    if (!setequal(types, all_types)) {
      bits <- c(bits, sprintf("P/A/P type: <b>%s</b>",
                              if (!length(types)) "none selected"
                              else esc(str_c(types, collapse = ", "))))
    }

    tiers <- input$tier %||% character(0)
    if (!setequal(tiers, TIER_CHOICES)) {
      dropped <- setdiff(TIER_CHOICES, tiers)
      bits <- c(bits, sprintf("Program tier: <b>%s</b>%s",
                              if (!length(tiers)) "none selected"
                              else esc(str_c(tiers, collapse = ", ")),
                              if (length(dropped) && length(tiers))
                                sprintf(" (excluding %s)", esc(str_c(dropped, collapse = ", ")))
                              else ""))
    }

    if (!setequal(yrs, p$years)) {
      bits <- c(bits, sprintf("Fiscal years: <b>%s</b> (of %s\u2013%s)",
                              esc(str_c(range(yrs), collapse = "\u2013")),
                              min(p$years), max(p$years)))
    }

    if (!identical(input$doc, "BOTH"))
      bits <- c(bits, sprintf("Document: <b>%s only</b>", esc(input$doc)))

    # Always stated, because they change what the bars mean rather than which
    # rows are counted.
    always <- c(
      sprintf("Table is at <b>%s</b> level",
              if (identical(input$grain, GRAIN_PROG)) "Program" else "P/A/P"),
      sprintf("Bars are the <b>%s</b> column, in <b>%s</b>",
              if (identical(cls, "TOTAL")) "Total"
              else str_c(vapply(cls, exp_short, character(1)), collapse = " + "),
              esc(unit_label(input$unit)))
    )

    n_all <- p$n_rows
    n_now <- nrow(d)
    n_agy <- n_distinct(str_c(d$DEPARTMENT, d$AGENCY))
    n_shown <- nrow(grained())
    unit_word <- if (identical(input$grain, GRAIN_PROG)) "programs" else "P/A/Ps"

    head_txt <- if (!length(bits)) {
      sprintf(paste0("<span class='recap-all'>No filters applied \u2014 the bars ",
                     "are the whole data set: all <b>%s</b> labeled P/A/Ps ",
                     "across <b>%s</b> agencies%s.</span>"),
              comma(n_all), comma(p$n_agencies),
              if (identical(input$grain, GRAIN_PROG))
                sprintf(", rolled up into <b>%s</b> programs", comma(n_shown)) else "")
    } else if (n_now == 0) {
      "<b>Nothing matches the current filters</b>, so there is nothing to plot."
    } else {
      sprintf(paste0("Showing <b>%s</b> of %s labeled P/A/Ps (%s%%), ",
                     "across <b>%s</b> of %s agencies%s."),
              comma(n_now), comma(n_all),
              formatC(100 * n_now / n_all, format = "f", digits = 1),
              comma(n_agy), comma(p$n_agencies),
              if (identical(input$grain, GRAIN_PROG))
                sprintf(", rolled up into <b>%s</b> %s", comma(n_shown), unit_word) else "")
    }

    tagList(
      div(class = "recap-head", HTML("Current selection")),
      HTML(head_txt),
      HTML(sprintf("<ul>%s</ul>",
                   str_c(sprintf("<li>%s</li>", c(bits, always)), collapse = "")))
    )
  })

  # -- source note in the sidebar --------------------------------------------
  output$source_note <- renderUI({
    p <- pap()
    when <- if (!is.null(.cache$fetched_at))
      format(.cache$fetched_at, "%d %b %Y %H:%M") else "\u2014"

    # Anything the loader could not use is reported here rather than dropped
    # quietly. These are the things that change as the workbook grows.
    notes <- list()

    if (length(p$sheets_pending)) {
      notes <- c(notes, list(div(
        class = "text-warning",
        sprintf("Sheets awaiting labels: %s. They carry figures but not yet %s.",
                str_c(names(p$sheets_pending), collapse = ", "),
                str_c(unique(unlist(p$sheets_pending)), collapse = " / "))
      )))
    }
    if (p$dup_keys > 0) {
      notes <- c(notes, list(div(
        class = "text-danger",
        sprintf("%s duplicated P/A/P key(s) across sheets \u2014 figures may be double counted.",
                comma(p$dup_keys))
      )))
    }
    if (p$prog_label_clashes > 0) {
      notes <- c(notes, list(div(
        class = "text-warning",
        sprintf("%s program code(s) carry more than one PROGRAM label; the roll-up uses the most common one.",
                comma(p$prog_label_clashes))
      )))
    }
    if (length(p$unknown_classes)) {
      notes <- c(notes, list(div(
        class = "text-warning",
        sprintf("Unlabeled expense class: %s.",
                str_c(p$unknown_classes, collapse = ", "))
      )))
    }
    if (length(p$unknown_docs)) {
      notes <- c(notes, list(div(
        class = "text-warning",
        sprintf("Unrecognized document type: %s.",
                str_c(p$unknown_docs, collapse = ", "))
      )))
    }
    if (!is.null(.cache$last_error)) {
      notes <- c(notes, list(div(class = "text-warning",
                                 "Last refresh failed; showing last good copy.")))
    }

    div(class = "small text-muted mt-2",
        div(sprintf("%s P/A/Ps \u00b7 %s agencies", comma(p$n_rows), comma(p$n_agencies))),
        div(sprintf("FY %s\u2013%s \u00b7 %s",
                    min(p$years), max(p$years),
                    str_c(p$docs, collapse = " + "))),
        div(sprintf("Sheets: %s", str_c(p$sheets_read, collapse = ", "))),
        div(paste("Loaded", when)),
        notes)
  })

  # -- notes ------------------------------------------------------------------
  output$notes <- renderUI({
    p <- pap()
    HTML(sprintf('
<h5>P/A/P Browser</h5>
<p>Program, Activity and Project level figures from the <strong>National
Expenditure Program</strong> (NEP, the Executive\'s proposal) and the
<strong>General Appropriations Act</strong> (GAA, as enacted by Congress),
FY2020&ndash;FY2027.</p>

<p><strong>Appropriations only.</strong> DBM publishes no P/A/P-level execution
data, so there are no allotment, obligation or disbursement figures here.
Agency-level execution is in the companion
<em>Agency Budget &amp; Utilization</em> dashboard.</p>

<h6>Coverage</h6>
<ul>
<li>%s labeled P/A/Ps across %s agencies.</li>
<li><strong>Not yet included:</strong> State Universities and Colleges (SUCs).
Everything else is in, Other Executive Offices among them.</li>
<li><strong>FY2027 has no GAA</strong> &mdash; the budget is not yet enacted.
Selecting "GAA only" together with FY2027 correctly yields no columns.</li>
</ul>

<h6>Units</h6>
<p>This workbook stores <strong>pesos</strong>. That is a departure from the rest
of this repo, where DBM figures are carried in thousands as published. Stored
values are never converted; the display-unit selector rescales presentation
only.</p>

<h6>Data conventions</h6>
<ul>
<li><strong>A blank is not a zero.</strong> An empty cell means no data for that
fiscal year. Where every expense column for a year is zero, that year carries no
information for that P/A/P and is shown blank rather than as a real zero. The
year group spans both documents, so NEP and GAA for a year are blanked together.</li>
<li><strong>%s unlabeled P/A/Ps are excluded</strong> &mdash; currently the last
few under the Philippine Commission on Women &mdash; and a further %s rows that
are zero in every column of every year. Removing them changes no series total.</li>
<li><strong>Statutory order is preserved</strong>, not alphabetical. Departments
and agencies are ordered by their UACS numeric codes, which follow the GAA\'s own
structural sequence; P/A/Ps are ordered by PREXC code within an agency. The codes
are ordering keys and are not displayed.</li>
<li><strong>DPWH is at the 7-digit PREXC code</strong>, which aggregates its
project lines. Every other agency is at the 12-digit code. Without this DPWH
would contribute tens of thousands of individual project rows. DPWH does not
appear in the NGAs sheet, so nothing is double counted.</li>
<li><strong>Labels are wrapped, never truncated</strong> &mdash; several P/A/Ps
differ only in their tail.</li>
<li><strong>P/A/P type is read from the label.</strong> The source varies between
singular and plural, sometimes omits the noun, and carries three PNP rows
spelled "Locally-Funed Project". All variants are matched.</li>
</ul>

<h6>Source</h6>
<p><code>Compiled_-_PAPs.xlsx</code>, sheets <code>NGAs</code> and
<code>DPWH-sub</code>, in
<a href="https://github.com/ajamontesa/ph-budget-analysis" target="_blank"
   rel="noopener">ph-budget-analysis</a>.
Compiled from DBM NEP and GAA electronic extracts by the People\'s Budget
Coalition.</p>',
      comma(p$n_rows), comma(p$n_agencies),
      comma(p$n_unlabeled), comma(p$n_empty)))
  })

  # -- download ----------------------------------------------------------------
  output$dl <- downloadHandler(
    filename = function() paste0("ph-pap-browser-data_", Sys.Date(), ".csv"),
    content = function(file) {
      cols <- shown_cols()
      d <- table_data()
      n_id <- ncol(d) - length(cols)
      names(d) <- c(names(d)[seq_len(n_id)], vapply(cols, amt_header, character(1)))
      readr::write_csv(d, file, na = "")
    }
  )
}

shinyApp(ui, server)
