## Exercises the app's server logic without an HTTP layer.
##   Rscript --vanilla test_server.R
suppressPackageStartupMessages({library(shiny); library(dplyr); library(stringr)})

app <- shinyAppDir(".")

# testServer evaluates inside the server function, which cannot see the
# constants and helpers defined at the top of app.R. Load them into their own
# environment so the tests can assert on them directly.
APP <- new.env()
suppressMessages(sys.source("app.R", envir = APP))

pass <- 0; fail <- 0
chk <- function(label, cond, detail = "") {
  if (isTRUE(cond)) { pass <<- pass + 1; cat(sprintf("  PASS  %s\n", label)) }
  else { fail <<- fail + 1; cat(sprintf("  FAIL  %s  %s\n", label, detail)) }
}

testServer(app, {
  cat("\n-- defaults --\n")
  ALL_TIERS <- c("Operations", "Support to Operations",
                 "General Administration and Support")
  session$setInputs(dept = "", agency = "", doc = "BOTH",
                    years = c("2020","2021","2022","2023","2024","2025","2026","2027"),
                    classes = "TOTAL", unit = "millions",
                    q_program = "", q_pap = "", grain = "pap", tier = ALL_TIERS,
                    pap_type = c("Regular Activity", "Locally-Funded Project",
                                 "Foreign-Assisted Project"))
  chk("all rows shown", nrow(filtered()) == 2759, nrow(filtered()))
  chk("depts in code order", identical(pap()$depts[1], "Congress of the Philippines (CONGRESS)"), pap()$depts[1])
  chk("PREXC codes not in table", !any(grepl("PREXC", names(table_data()))))
  chk("15 total columns", length(shown_cols()) == 15, length(shown_cols()))
  chk("default unit millions", input$unit == "millions")

  cat("\n-- document toggle --\n")
  session$setInputs(doc = "GAA")
  chk("GAA only -> 7 series", length(shown_series()) == 7, length(shown_series()))
  chk("no NEP columns", !any(str_starts(shown_cols(), "NEP")))
  session$setInputs(doc = "NEP")
  chk("NEP only -> 8 series", length(shown_series()) == 8, length(shown_series()))
  session$setInputs(doc = "BOTH")

  cat("\n-- expense classes --\n")
  session$setInputs(classes = c("TOTAL","1PS","2MOOE","3FE","6CO"))
  chk("all classes -> 75 columns", length(shown_cols()) == 75, length(shown_cols()))
  session$setInputs(classes = "TOTAL")

  cat("\n-- year filter --\n")
  session$setInputs(years = c("2026","2027"))
  chk("2026-27 -> 3 series (no GAA 2027)", length(shown_cols()) == 3, length(shown_cols()))
  session$setInputs(years = c("2020","2021","2022","2023","2024","2025","2026","2027"))

  cat("\n-- department / agency --\n")
  session$setInputs(dept = "Department of Energy (DOE)")
  chk("DOE rows", nrow(filtered()) == 19, nrow(filtered()))
  doe_total <- sum(filtered()$NEP_2027_EXP_TOTAL, na.rm = TRUE)
  chk("DOE NEP2027 = 2,028,303,000 pesos", isTRUE(all.equal(doe_total, 2028303000)),
      format(doe_total, big.mark = ","))
  session$setInputs(dept = "")

  cat("\n-- agency without a department --\n")
  # "Office of the Secretary" belongs to 22 departments; the selector is keyed
  # on DEPARTMENT|AGENCY so picking DOE's OSEC must not pull in the other 21.
  session$setInputs(agency = "09|001")
  chk("agency selectable with no department", nrow(filtered()) == 19, nrow(filtered()))
  chk("agency filter is unambiguous", n_distinct(filtered()$UACS_DPT_DSC) == 1,
      n_distinct(filtered()$UACS_DPT_DSC))
  chk("agency total still ties",
      isTRUE(all.equal(sum(filtered()$NEP_2027_EXP_TOTAL, na.rm = TRUE), 2028303000)))
  session$setInputs(agency = "")

  cat("\n-- mobile --\n")
  session$setInputs(viewport_width = 390)
  chk("is_mobile TRUE at 390px", is_mobile())
  chk("identity collapses to one column",
      names(table_data())[1] == "Agency \u2014 P/A/P", names(table_data())[1])
  chk("mobile table renders",
      !inherits(try(output$tbl, silent = TRUE), "try-error"))
  chk("mobile trend renders",
      !inherits(try(output$trend, silent = TRUE), "try-error"))
  session$setInputs(viewport_width = 1440)
  chk("back to wide layout", !is_mobile())

  cat("\n-- project type --\n")
  session$setInputs(pap_type = "Locally-Funded Project")
  chk("LFP only", all(filtered()$PAP_TYPE == "Locally-Funded Project"))
  chk("LFP count", nrow(filtered()) == 1132, nrow(filtered()))
  session$setInputs(pap_type = "Foreign-Assisted Project")
  chk("FAP count", nrow(filtered()) == 88, nrow(filtered()))
  session$setInputs(pap_type = c("Regular Activity","Locally-Funded Project",
                                 "Foreign-Assisted Project"))

  cat("\n-- search --\n")
  session$setInputs(q_pap = "school building")
  chk("PAP search narrows", nrow(filtered()) > 0 && nrow(filtered()) < 20, nrow(filtered()))
  session$setInputs(q_pap = "")
  session$setInputs(q_program = "health")
  chk("PROGRAM search narrows", nrow(filtered()) > 0, nrow(filtered()))
  chk("PROGRAM search matches", all(str_detect(str_to_lower(filtered()$PROGRAM), "health")))
  session$setInputs(q_program = "")

  cat("\n-- units --\n")
  session$setInputs(dept = "Department of Energy (DOE)", classes = "TOTAL",
                    years = "2027", doc = "NEP")
  session$setInputs(unit = "pesos")
  v1 <- sum(table_data()$NEP_2027_EXP_TOTAL, na.rm = TRUE)
  chk("pesos", isTRUE(all.equal(v1, 2028303000)), format(v1, big.mark = ","))
  session$setInputs(unit = "thousands")
  v2 <- sum(table_data()$NEP_2027_EXP_TOTAL, na.rm = TRUE)
  chk("thousands", isTRUE(all.equal(v2, 2028303)), format(v2, big.mark = ","))
  session$setInputs(unit = "millions")
  v3 <- sum(table_data()$NEP_2027_EXP_TOTAL, na.rm = TRUE)
  chk("millions", isTRUE(all.equal(v3, 2028.303)), format(v3))

  cat("\n-- trend: document order and default coverage --\n")
  session$setInputs(dept = "", agency = "", doc = "BOTH", classes = "TOTAL",
                    unit = "millions", q_program = "", q_pap = "",
                    years = pap()$years, grain = APP$GRAIN_PAP,
                    tier = APP$TIER_CHOICES,
                    pap_type = c("Regular Activity", "Locally-Funded Project",
                                 "Foreign-Assisted Project"))
  lev <- APP$order_docs(c("GAA", "NEP"), APP$DOC_ORDER_PLOT)
  chk("NEP is left of GAA in the plot",
      which(lev == "NEP") < which(lev == "GAA"), str_c(lev, collapse = " -> "))
  chk("GAA fill is the darker of the two",
      # crude luminance: the deep navy must be darker than the light blue
      sum(grDevices::col2rgb(APP$PBC_NAVY_DEEP)) <
        sum(grDevices::col2rgb(APP$PBC_BLUE)))

  # With nothing applied the chart must be the whole data set, not a subset.
  in_view <- sum(filtered()$NEP_2027_EXP_TOTAL, na.rm = TRUE)
  whole   <- sum(pap()$data$NEP_2027_EXP_TOTAL, na.rm = TRUE)
  chk("default selection is the whole data set", isTRUE(all.equal(in_view, whole)))
  chk("default row count is every labeled P/A/P",
      nrow(filtered()) == pap()$n_rows, nrow(filtered()))

  cat("\n-- selection recap --\n")
  recap_text <- function() {
    gsub("\\s+", " ", gsub("<[^>]*>", "",
      paste(head(as.character(output$selection_recap), 1), collapse = " ")))
  }
  r0 <- recap_text()
  chk("recap says no filters applied", grepl("No filters applied", r0), r0)
  chk("recap names the unit", grepl("M", r0))

  session$setInputs(dept = "Department of Energy (DOE)", doc = "GAA",
                    years = c("2025", "2026"), q_pap = "management",
                    classes = c("1PS", "2MOOE"), unit = "thousands")
  r1 <- recap_text()
  chk("recap reports the subset", grepl("Showing", r1), r1)
  chk("recap names the department", grepl("Department of Energy", r1))
  chk("recap names the P/A/P search", grepl("management", r1))
  chk("recap names the document", grepl("GAA only", r1))
  chk("recap names the year range", grepl("2025", r1) && grepl("2026", r1))
  chk("recap names the expense classes", grepl("PS \\+ MOOE", r1), r1)

  session$setInputs(dept = "", doc = "BOTH", years = pap()$years, q_pap = "",
                    classes = "TOTAL", unit = "millions")

  cat("\n-- program tier toggles --\n")
  session$setInputs(dept = "", agency = "", q_pap = "", q_program = "",
                    doc = "BOTH", years = pap()$years, classes = "TOTAL",
                    unit = "millions", grain = APP$GRAIN_PAP,
                    tier = APP$TIER_CHOICES)
  n_all_tiers <- nrow(filtered())
  chk("all tiers is every row", n_all_tiers == pap()$n_rows, n_all_tiers)

  session$setInputs(tier = APP$TIER_OPS)
  ops <- filtered()
  chk("Operations only excludes overhead", nrow(ops) < n_all_tiers, nrow(ops))
  chk("only Operations rows remain",
      all(ops$PROG_TIER == APP$TIER_OPS))
  chk("no 1000 or 2000 program codes left",
      !any(str_sub(ops$PREXC_PROG, 1, 1) %in% c("1", "2")))

  session$setInputs(tier = c(APP$TIER_OPS, APP$TIER_STO))
  chk("dropping GAS alone works", nrow(filtered()) > nrow(ops))
  session$setInputs(tier = character(0))
  chk("no tier selected -> 0 rows, no error", nrow(filtered()) == 0)
  session$setInputs(tier = APP$TIER_CHOICES)

  cat("\n-- program roll-up --\n")
  session$setInputs(grain = APP$GRAIN_PAP)
  pap_rows <- nrow(grained())
  pap_total <- sum(grained()$NEP_2027_EXP_TOTAL, na.rm = TRUE)
  chk("P/A/P grain is the default shape", pap_rows == pap()$n_rows, pap_rows)
  chk("P/A/P grain has a P/A/P column", "P/A/P" %in% names(table_data()))

  session$setInputs(grain = APP$GRAIN_PROG)
  prog_rows <- nrow(grained())
  prog_total <- sum(grained()$NEP_2027_EXP_TOTAL, na.rm = TRUE)
  chk("program grain has fewer rows", prog_rows < pap_rows, prog_rows)
  chk("roll-up preserves the total", isTRUE(all.equal(pap_total, prog_total)),
      sprintf("%.0f vs %.0f", pap_total, prog_total))
  chk("program grain drops the P/A/P column",
      !"P/A/P" %in% names(table_data()))
  chk("program grain keeps three identity columns",
      length(id_cols()) == 3, str_c(id_cols(), collapse = ","))

  # Blanks must survive the roll-up: a program blank for a whole year has to
  # stay blank rather than summing to a real zero.
  chk("roll-up keeps blanks blank", any(is.na(grained()$NEP_2020_EXP_TOTAL)))

  # Roll-up must agree with a hand computation, agency by agency.
  session$setInputs(dept = "Department of Energy (DOE)", grain = APP$GRAIN_PAP)
  by_hand <- filtered() %>% group_by(PREXC_PROG) %>%
    summarize(t = sum(NEP_2027_EXP_TOTAL, na.rm = TRUE), .groups = "drop")
  session$setInputs(grain = APP$GRAIN_PROG)
  rolled <- grained() %>% select(PREXC_PROG, t = NEP_2027_EXP_TOTAL)
  cmp <- dplyr::full_join(by_hand, rolled, by = "PREXC_PROG")
  chk("DOE program totals match a hand roll-up",
      isTRUE(all.equal(cmp$t.x, cmp$t.y)) && nrow(cmp) == nrow(rolled), nrow(cmp))
  session$setInputs(dept = "", grain = APP$GRAIN_PAP)

  cat("\n-- DT search box removed --\n")
  chk("table dom has no search box",
      !grepl("\"dom\":\"[a-z]*f", as.character(output$tbl)))

  cat("\n-- table has no DT search box --\n")
  chk("dom omits the search input",
      !grepl('"dom":"[^"]*f', as.character(output$tbl)))

  cat("\n-- program tier toggles --\n")
  session$setInputs(dept = "", agency = "", doc = "BOTH", years = pap()$years,
                    classes = "TOTAL", unit = "millions", q_program = "",
                    q_pap = "", grain = "pap", tier = ALL_TIERS,
                    pap_type = c("Regular Activity", "Locally-Funded Project",
                                 "Foreign-Assisted Project"))
  n_all <- nrow(filtered())
  chk("all tiers is the whole set", n_all == pap()$n_rows, n_all)

  session$setInputs(tier = "Operations")
  ops <- filtered()
  chk("Operations only excludes GAS and STO",
      all(ops$PROG_TIER == "Operations") &&
        !any(str_sub(ops$PREXC_PROG, 1, 1) %in% c("1", "2")))
  chk("Operations is smaller than the whole set", nrow(ops) < n_all, nrow(ops))

  session$setInputs(tier = c("Operations", "Support to Operations"))
  chk("dropping GAS only removes tier 1",
      !any(str_sub(filtered()$PREXC_PROG, 1, 1) == "1") &&
        any(str_sub(filtered()$PREXC_PROG, 1, 1) == "2"))

  session$setInputs(tier = character(0))
  chk("no tier selected -> 0 rows, no error", nrow(filtered()) == 0)
  session$setInputs(tier = ALL_TIERS)

  cat("\n-- program-level roll-up --\n")
  session$setInputs(grain = "pap")
  pap_rows <- nrow(grained())
  pap_tot  <- sum(grained()$NEP_2027_EXP_TOTAL, na.rm = TRUE)
  chk("P/A/P grain is the default shape", pap_rows == pap()$n_rows, pap_rows)
  chk("P/A/P identity has four columns", length(id_cols()) == 4)

  session$setInputs(grain = "program")
  prog_rows <- nrow(grained())
  prog_tot  <- sum(grained()$NEP_2027_EXP_TOTAL, na.rm = TRUE)
  chk("roll-up reduces the row count", prog_rows < pap_rows, prog_rows)
  chk("roll-up preserves the total", isTRUE(all.equal(pap_tot, prog_tot)))
  chk("program identity has three columns", length(id_cols()) == 3)
  chk("P/A/P column is gone", !"P/A/P" %in% names(table_data()))
  chk("one row per department+agency+program",
      !any(duplicated(str_c(grained()$DEPARTMENT, grained()$AGENCY,
                            grained()$PREXC_PROG))))

  # Blanks must not become zeroes in the roll-up.
  chk("roll-up keeps blank years blank", any(is.na(grained()$NEP_2020_EXP_TOTAL)))

  # Roll-up must agree with a hand-computed group sum.
  session$setInputs(dept = "Department of Energy (DOE)", grain = "pap")
  byhand <- filtered() %>% group_by(PREXC_PROG) %>%
    summarize(t = sum(NEP_2027_EXP_TOTAL, na.rm = TRUE), .groups = "drop")
  session$setInputs(grain = "program")
  rolled <- grained() %>% select(PREXC_PROG, t = NEP_2027_EXP_TOTAL)
  j <- dplyr::full_join(byhand, rolled, by = "PREXC_PROG")
  chk("DOE program totals match a hand roll-up",
      nrow(j) == nrow(byhand) && isTRUE(all.equal(j$t.x, j$t.y)), nrow(j))

  session$setInputs(dept = "", grain = "pap")

  cat("\n-- recap covers the new controls --\n")
  session$setInputs(tier = "Operations", grain = "program")
  r2 <- gsub("\\s+", " ", gsub("<[^>]*>", "",
        paste(head(as.character(output$selection_recap), 1), collapse = " ")))
  chk("recap names the tier filter", grepl("Program tier", r2), r2)
  chk("recap says excluding overhead", grepl("excluding", r2))
  chk("recap names the grain", grepl("Program level", r2))
  chk("recap reports the roll-up count", grepl("rolled up into", r2))
  session$setInputs(tier = ALL_TIERS, grain = "pap")

  cat("\n-- program tier toggles --\n")
  session$setInputs(dept = "", agency = "", doc = "BOTH", years = pap()$years,
                    classes = "TOTAL", unit = "millions", q_program = "",
                    q_pap = "", grain = APP$GRAIN_PAP,
                    pap_type = c("Regular Activity", "Locally-Funded Project",
                                 "Foreign-Assisted Project"),
                    tier = APP$TIER_CHOICES)
  n_all_tiers <- nrow(filtered())

  session$setInputs(tier = APP$TIER_OPS)
  ops <- filtered()
  chk("Operations only", all(ops$PROG_TIER == APP$TIER_OPS))
  chk("overhead removed", nrow(ops) < n_all_tiers, nrow(ops))
  chk("no tier-1 or tier-2 codes remain",
      !any(str_sub(ops$PREXC_PROG, 1, 1) %in% c("1", "2")))

  session$setInputs(tier = c(APP$TIER_OPS, APP$TIER_STO))
  chk("GAS excluded, STO kept",
      !any(filtered()$PROG_TIER == APP$TIER_GAS) &&
        any(filtered()$PROG_TIER == APP$TIER_STO))

  session$setInputs(tier = character(0))
  chk("no tier selected -> 0 rows, no error", nrow(filtered()) == 0)
  session$setInputs(tier = APP$TIER_CHOICES)
  chk("tiers restored", nrow(filtered()) == n_all_tiers)

  cat("\n-- program roll-up --\n")
  session$setInputs(grain = APP$GRAIN_PAP)
  pap_rows <- nrow(grained())
  pap_tot  <- sum(grained()$NEP_2027_EXP_TOTAL, na.rm = TRUE)

  session$setInputs(grain = APP$GRAIN_PROG)
  prog_rows <- nrow(grained())
  prog_tot  <- sum(grained()$NEP_2027_EXP_TOTAL, na.rm = TRUE)

  chk("roll-up has fewer rows", prog_rows < pap_rows,
      sprintf("%d -> %d", pap_rows, prog_rows))
  chk("roll-up preserves the total", isTRUE(all.equal(pap_tot, prog_tot)),
      sprintf("%.0f vs %.0f", pap_tot, prog_tot))
  chk("roll-up drops the P/A/P column",
      !"P/A/P" %in% names(table_data()), str_c(names(table_data())[1:4], collapse = ","))
  chk("roll-up identity block is three columns",
      length(id_cols()) == 3, length(id_cols()))
  chk("roll-up widths match the identity block",
      length(id_widths()) == length(id_cols()))

  # Blanks must not become zeroes in a roll-up: a program blank for a whole
  # year has to stay blank rather than summing to a real 0.
  chk("roll-up keeps blanks blank", any(is.na(grained()$NEP_2020_EXP_TOTAL)))

  # One agency checked against a roll-up done by hand.
  session$setInputs(dept = "Department of Energy (DOE)", grain = APP$GRAIN_PAP)
  by_hand <- filtered() %>% group_by(PREXC_PROG) %>%
    summarize(t = sum(NEP_2027_EXP_TOTAL, na.rm = TRUE), .groups = "drop")
  session$setInputs(grain = APP$GRAIN_PROG)
  from_app <- grained() %>% select(PREXC_PROG, t = NEP_2027_EXP_TOTAL)
  cmp <- dplyr::full_join(by_hand, from_app, by = "PREXC_PROG")
  chk("DOE program totals match a hand roll-up",
      isTRUE(all.equal(cmp$t.x, cmp$t.y)), nrow(cmp))

  session$setInputs(dept = "", grain = APP$GRAIN_PAP)

  cat("\n-- column sizing is consistent across grains --\n")
  # The identity block is a different NUMBER of columns per grain but the same
  # total WIDTH, so the figure columns start at the same point in both views.
  w <- function(x) sum(as.numeric(sub("px", "", x)))
  chk("identity blocks are the same total width",
      w(APP$ID_WIDTHS_BY_GRAIN$pap) == w(APP$ID_WIDTHS_BY_GRAIN$program),
      sprintf("%d vs %d", w(APP$ID_WIDTHS_BY_GRAIN$pap),
              w(APP$ID_WIDTHS_BY_GRAIN$program)))
  chk("a width is given for every identity column in each grain",
      length(APP$ID_WIDTHS_BY_GRAIN$pap) == length(APP$ID_COLS_BY_GRAIN$pap) &&
        length(APP$ID_WIDTHS_BY_GRAIN$program) == length(APP$ID_COLS_BY_GRAIN$program))

  # Regression: the stylesheet counted identity columns with one fixed number,
  # so at Program grain the first FIGURE column was styled as an identity
  # column -- wrapped, word-broken and shrunk. Each grain now tags the table.
  session$setInputs(grain = APP$GRAIN_PAP)
  tbl_pap <- as.character(output$tbl)
  chk("P/A/P grain tags the table", grepl("pap-grain-pap", tbl_pap))
  chk("figure columns are given an explicit width",
      grepl(APP$AMT_COL_WIDTH, tbl_pap, fixed = TRUE), APP$AMT_COL_WIDTH)

  session$setInputs(grain = APP$GRAIN_PROG)
  tbl_prog <- as.character(output$tbl)
  chk("Program grain tags the table", grepl("pap-grain-prog", tbl_prog))
  chk("Program grain still sizes its figure columns",
      grepl(APP$AMT_COL_WIDTH, tbl_prog, fixed = TRUE))

  css <- paste(as.character(APP$app_css), collapse = " ")
  chk("stylesheet has a rule per grain",
      grepl("pap-grain-pap  thead th:nth-child(-n+4)", css, fixed = TRUE) &&
        grepl("pap-grain-prog thead th:nth-child(-n+3)", css, fixed = TRUE))
  chk("first figure column is not styled as identity at either grain",
      grepl("pap-grain-pap  tbody td:nth-child(n+5)", css, fixed = TRUE) &&
        grepl("pap-grain-prog tbody td:nth-child(n+4)", css, fixed = TRUE))

  session$setInputs(grain = APP$GRAIN_PAP)

  cat("\n-- empty selection is safe --\n")
  session$setInputs(pap_type = character(0))
  chk("no type selected -> 0 rows, no error", nrow(filtered()) == 0)
  session$setInputs(pap_type = c("Regular Activity","Locally-Funded Project",
                                 "Foreign-Assisted Project"))
  session$setInputs(classes = character(0))
  chk("no class selected -> falls back to TOTAL", length(shown_cols()) >= 1,
      length(shown_cols()))
})

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
