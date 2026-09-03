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
  session$setInputs(dept = "", agency = "", doc = "BOTH",
                    years = c("2020","2021","2022","2023","2024","2025","2026","2027"),
                    classes = "TOTAL", unit = "millions",
                    q_program = "", q_pap = "",
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
                    years = pap()$years,
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
