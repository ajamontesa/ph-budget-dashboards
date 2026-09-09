## Robustness check: build a synthetic "future" workbook and confirm the app
## picks up the changes with no code edit.
##
##   - a new fiscal year (GAA 2027 enacted, NEP 2028 proposed)
##   - a newly labeled sheet (SUCs gains PROGRAM and PAP)
##   - a new P/A/P row in an existing sheet
##   - a new expense class (5DS)
##
##   Rscript --vanilla test_future.R

suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr); library(stringr); library(shiny)
})

# Use a local development copy if one is present, otherwise pull the published
# workbook -- the same order the app itself uses, so the test runs on a clean
# checkout with no data directory.
src <- "data/Compiled_-_PAPs.xlsx"
if (!file.exists(src)) {
  src <- tempfile(fileext = ".xlsx")
  url <- paste0("https://raw.githubusercontent.com/ajamontesa/",
                "ph-budget-analysis/main/data/Compiled_-_PAPs.xlsx")
  message("no local copy; downloading ", url)
  utils::download.file(url, src, mode = "wb", quiet = TRUE)
}
stopifnot(file.exists(src))

ngas <- read_xlsx(src, sheet = "NGAs",     col_types = "text")
dpwh <- read_xlsx(src, sheet = "DPWH-sub", col_types = "text")
sucs <- read_xlsx(src, sheet = "SUCs",     col_types = "text")

amt <- names(ngas)[str_detect(names(ngas), "_EXP_")]
cls <- unique(str_extract(amt, "(?<=_EXP_).*$"))

add_series <- function(d, series) {
  for (c0 in cls) d[[paste0(series, "_EXP_", c0)]] <- "1000"
  d
}

# 1. new fiscal years
ngas2 <- ngas %>% add_series("GAA_2027") %>% add_series("NEP_2028")
dpwh2 <- dpwh %>% add_series("GAA_2027") %>% add_series("NEP_2028")

# 2. a new expense class on an existing year
ngas2$NEP_2027_EXP_5DS <- "500"
dpwh2$NEP_2027_EXP_5DS <- "500"

# 3. a brand new P/A/P row
newrow <- ngas2[1, ]
newrow$PREXC_SUBPROG <- "319999999999"
newrow$PAP <- "Locally-Funded Project: Synthetic New Line"
newrow$PROGRAM <- "SYNTHETIC TEST PROGRAM"
newrow$PREXC_PROG <- "3199"
ngas2 <- bind_rows(ngas2, newrow)

# 4. SUCs gains the labels it is missing, so the sheet becomes readable
sucs2 <- sucs %>%
  mutate(PROGRAM = "SUC OPERATIONS PROGRAM",
         PAP = "Higher education services") %>%
  add_series("GAA_2027") %>% add_series("NEP_2028")
sucs2$NEP_2027_EXP_5DS <- "500"

tmpdir <- file.path(tempdir(), "future"); dir.create(tmpdir, showWarnings = FALSE)
dir.create(file.path(tmpdir, "data"), showWarnings = FALSE)
write_xlsx(list(NGAs = ngas2, `DPWH-sub` = dpwh2, SUCs = sucs2),
           file.path(tmpdir, "data", "Compiled_-_PAPs.xlsx"))
file.copy("app.R", file.path(tmpdir, "app.R"), overwrite = TRUE)

owd <- setwd(tmpdir); on.exit(setwd(owd))

pass <- 0; fail <- 0
chk <- function(label, cond, detail = "") {
  if (isTRUE(cond)) { pass <<- pass + 1; cat(sprintf("  PASS  %s\n", label)) }
  else { fail <<- fail + 1; cat(sprintf("  FAIL  %s  %s\n", label, detail)) }
}

testServer(shinyAppDir("."), {
  p <- pap()

  cat("\n-- new fiscal years --\n")
  chk("2028 detected", "2028" %in% p$years, str_c(p$years, collapse = " "))
  chk("GAA 2027 detected", "GAA_2027" %in% p$series)
  chk("NEP 2028 detected", "NEP_2028" %in% p$series)

  cat("\n-- series ordering (NEP before GAA within a year, columns) --\n")
  i27 <- match(c("NEP_2027", "GAA_2027"), p$series)
  chk("NEP_2027 precedes GAA_2027 in columns", i27[1] < i27[2], str_c(i27, collapse = "<"))

  cat("\n-- new expense class --\n")
  chk("5DS detected", "5DS" %in% p$classes, str_c(p$classes, collapse = " "))
  chk("5DS reported as unlabeled", "5DS" %in% p$unknown_classes)

  cat("\n-- newly labeled sheet --\n")
  chk("SUCs now read", "SUCs" %in% p$sheets_read,
      str_c(p$sheets_read, collapse = ", "))
  chk("SUCs no longer pending", !"SUCs" %in% names(p$sheets_pending))
  # The real SUCs sheet carries 295 codes bearing two agency names apiece --
  # renames, not double counting. The loader reports them; it must not choke,
  # and the picker must still show one entry per code.
  chk("duplicate keys reported, not fatal", p$dup_keys > 0, p$dup_keys)
  chk("agency picker has one entry per code",
      !any(duplicated(p$agencies$AGENCY_KEY)))

  cat("\n-- new P/A/P row --\n")
  session$setInputs(dept = "", agency = "", doc = "BOTH", years = p$years,
                    classes = "TOTAL", unit = "millions",
                    q_program = "", q_pap = "Synthetic New Line",
                    grain = "pap",
                    tier = c("Operations", "Support to Operations",
                             "General Administration and Support"),
                    pap_type = c("Regular Activity", "Locally-Funded Project",
                                 "Foreign-Assisted Project"))
  chk("new P/A/P is findable", nrow(filtered()) == 1, nrow(filtered()))
  chk("new P/A/P typed from its label",
      identical(filtered()$PAP_TYPE[1], "Locally-Funded Project"))

  cat("\n-- UI picks up the new vocabulary --\n")
  session$setInputs(q_pap = "", classes = p$classes, years = p$years)
  # Not every series carries every class -- 5DS exists only for NEP 2027 --
  # so the column set is the intersection that actually exists, not the product.
  cols <- shown_cols()
  chk("selected columns all exist", all(cols %in% p$amt_cols), length(cols))
  chk("new class appears in the column set", any(str_ends(cols, "_5DS")))
  chk("no phantom columns invented",
      length(cols) == sum(p$amt_cols %in% cols), length(cols))
  session$setInputs(classes = "TOTAL", doc = "GAA")
  chk("GAA now includes 2027", "GAA_2027" %in% shown_series())

  cat("\n-- outputs still render --\n")
  session$setInputs(doc = "BOTH")
  chk("table renders", !inherits(try(output$tbl, silent = TRUE), "try-error"))
  chk("trend renders", !inherits(try(output$trend, silent = TRUE), "try-error"))
  chk("sidebar note renders", !inherits(try(output$source_note, silent = TRUE), "try-error"))
})

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
