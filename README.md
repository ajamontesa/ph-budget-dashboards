# PH Budget Dashboards

Interactive dashboards over the People's Budget Coalition's budget data,
companions to the report and briefing suite at
<https://ajamontesa.github.io/ph-budget-analysis/>.

## Dashboards in this repo

| Directory | Scope | Data available | Source | Status |
|---|---|---|---|---|
| `dashboards/agency-budget-utilization/` | National agency level — departments, bureaux, attached agencies, aggregate blocks | NEP, GAA, allotments, obligations, disbursements | [PH Budget Data Set](https://docs.google.com/spreadsheets/d/1P3q46DGcN3SZ7cRwXfEhAMiDqCqQof1s3LV0-CK2lIY) (Google Sheet) | Live |
| `dashboards/pap-browser/` | Program / Activity / Project level | NEP and GAA only — DBM publishes no P/A/P execution data | `Compiled_-_PAPs.xlsx` in [ph-budget-analysis](https://github.com/ajamontesa/ph-budget-analysis) | Live |

The two are complements, not overlapping views. The agency dashboard answers
"how much did this agency get, and how much of it did they actually spend"; the
P/A/P dashboard answers "what is inside that agency's budget line". Only the
agency dashboard carries execution, and only it covers the whole budget — the
P/A/P dataset does not yet include OEOs or SUCs, so its departmental totals are
not national totals.

Each dashboard is a self-contained directory with its own `app.R`,
`manifest.json` and `README.md`, published to Posit Connect Cloud as separate
content with its own URL.
`app.R` is deliberately kept as the filename inside each directory: Shiny and
`rsconnect` both expect that entry point, and the directory name carries the
identity.

```
ph-budget-dashboards/
├── README.md                                  this file
├── DEPLOY.md                                  deployment, applies to all dashboards
├── ph-budget-dashboards.Rproj
├── dashboards/
│   ├── agency-budget-utilization/
│   │   ├── app.R
│   │   ├── manifest.json                      GENERATED — see DEPLOY.md
│   │   └── README.md                          scope, indicators, data conventions
│   └── pap-browser/
│       ├── app.R
│       ├── manifest.json                      GENERATED — see DEPLOY.md
│       ├── test_server.R                      reactive tests via shiny::testServer
│       ├── test_future.R                      robustness to workbook growth
│       └── README.md
└── conventions/
    └── embed/
        ├── agency-budget-utilization.html     copy into ph-budget-analysis
        └── pap-browser.html
```

## Naming conventions

- **Directories** are lowercase and hyphenated, describing the *slice of the
  budget* rather than the technology: `agency-budget-utilization`, not
  `dashboard` or `app`.
- **Embed pages** in `conventions/embed/` take the same name as the dashboard
  directory. They are templates: copy them into the `ph-budget-analysis` repo,
  where they become the page a reader navigates to from the index.
- **CSV exports** are prefixed by scope — `ph-agency-budget-data_YYYY-MM-DD.csv`,
  `ph-pap-browser-data_YYYY-MM-DD.csv` — so a reader who downloads from more than
  one dashboard ends up with files they can tell apart.
- **Environment variables** are namespaced per dashboard —
  `PBC_AGENCY_CACHE_TTL`, `PBC_PAP_CACHE_TTL` — so cache behavior can be tuned
  independently.

## Shared conventions across all dashboards

These hold everywhere and are worth keeping consistent as dashboards are added.

- **Figures are carried as published, and never converted.** Display-unit
  selectors rescale presentation only. The agency dataset is in **thousands of
  pesos**, as DBM publishes it. The P/A/P workbook is in **pesos** — a deliberate
  exception, verified against four figures checked line-by-line against the
  published DBM documents, and confined to a single `SOURCE_UNIT` constant in
  that app. Every table and axis is labeled with its unit; check which one you
  are in before comparing figures across the two dashboards.
- **A blank is not a zero.** An empty cell means not reported, an agency that
  did not yet exist, one housed under a different mother department that year, or
  a data frontier that has not reached that year. A literal zero is carried as
  zero because it is what DBM published. The two are never interchanged.
- **Sheet order is preserved**, not alphabetized. Both dashboards sort on the
  source's own structural sequence — captured row position in the agency
  dashboard, the fixed-width UACS `DEPARTMENT` and `AGENCY` codes in the P/A/P
  dashboard — and both suppress DT's initial sort so it cannot undo it. Codes
  used only for ordering are not displayed.
- **The data frontier is ragged.** NEP runs a year ahead of GAA, which runs a
  year ahead of execution. Reference years are detected from the data on every
  load, never hard-coded.
- **Appropriations come on two bases.** New Appropriations is what Congress
  legislates for the year; Total is New plus Automatic. Total is always at least
  as large as New. Dashboards that carry both default to New and label which
  basis is in view. The P/A/P workbook carries New Appropriations only, so it has
  no basis toggle.
- **Layouts adapt below 768px.** Plot heights are derived from row count rather
  than hard-coded, ranking sizes are trimmed, and tables collapse their identity
  columns and drop frozen columns. Any new dashboard should do the same.
- **Identity columns are sized, wrapped and frozen as a block.** Left to size
  themselves they take half the table and push the figures off-screen. Give them
  explicit widths totalling about a third of the viewport, let them wrap rather
  than truncate, set them a smaller face than the numbers, and freeze all of them
  rather than a leading pair.
- **American spelling** throughout, in code, labels and prose.
- **`DEPARTMENT` + `AGENCY` is the join key**, but agencies move between
  departments across and within administrations, so it is a key rather than a
  stable identity.
- **Data is fetched at run time, cached, and degrades gracefully.** No dashboard
  ships a data file; each reads its source over the network on first use and
  holds it in a cache shared across sessions with a one-hour default TTL. So
  updating the source updates the dashboard without a redeploy. Each keeps
  serving the last good copy if the source becomes unreachable, notes the failure
  in the sidebar rather than erroring out, and offers a manual refresh.
- **Fonts are linked, not downloaded.** Both apps use
  `font_google(..., local = FALSE)`, which emits a `<link>` rather than fetching
  the font server-side at theme-compile time. The default (`local = TRUE`) fails
  on a host without outbound internet, and `bootswatch` presets pull web fonts
  the same way. See DEPLOY.md.

## Deploying

Every dashboard is published to **Posit Connect Cloud** from this repository:
Connect Cloud watches a branch, and a push rebuilds the content. `rsconnect` is
used only to generate `manifest.json`, never to deploy.

See [DEPLOY.md](DEPLOY.md). Each dashboard deploys independently; the process is
the same for all of them, with the directory path as the only thing that changes.
