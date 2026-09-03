# P/A/P Browser Dashboard

Part of [PH Budget Dashboards](../../README.md). Covers the **Program,
Activity and Project level** — the granular lines inside each agency's headline
programs — across NEP and GAA, FY 2020–2027.

**Appropriations only.** DBM publishes no P/A/P-level execution data, so there
are no allotment, obligation or disbursement figures here. Agency-level
execution is in the companion
[Agency Budget & Utilization](../agency-budget-utilization/README.md) dashboard,
which is also the place to go for whole-of-budget rankings and utilization rates.

Source: `Compiled_-_PAPs.xlsx` in
[ph-budget-analysis](https://github.com/ajamontesa/ph-budget-analysis), sheets
`NGAs` and `DPWH-sub`. Other sheets in that workbook are ignored.

The workbook is read over the network at run time, the same arrangement the
agency dashboard has with its Google Sheet: pushing an updated
`Compiled_-_PAPs.xlsx` to `ph-budget-analysis` updates this dashboard, with no
redeploy. Only the source differs — a GitHub raw URL rather than a Sheet.

A workbook at `data/Compiled_-_PAPs.xlsx` is used in preference to the download
when one is present, which is how to test a change to the data before pushing
it. That path is **gitignored and never committed**: anything not listed in
`manifest.json` does not reach the server, so a committed copy would go stale
the first time the manifest was regenerated without it. See
[DEPLOY.md](../../DEPLOY.md).

The fetched data is held in a cache shared by every concurrent session, with a
one-hour time-to-live (override with `PBC_PAP_CACHE_TTL`, in seconds). There is a
**Refresh from source** button in the sidebar. If the source becomes unreachable
the app keeps serving the last good copy and notes the failure in the sidebar,
rather than erroring out.

## Units

**This workbook stores pesos.** That is a departure from the rest of this repo,
where DBM figures are carried in thousands as published, and it is deliberate
rather than an oversight. It was verified against four figures checked
line-by-line against the published DBM documents:

| Agency | Document | Pesos |
|---|---|---|
| Department of Energy | NEP 2027 | 2,028,303,000 |
| Department of Energy | GAA 2026 | 2,963,524,000 |
| National Museum | NEP 2027 | 1,530,502,000 |
| National Maritime Polytechnic | NEP 2021 | 132,094,000 |

All four match the workbook exactly in pesos. Reading it as thousands would put
every figure out by a factor of 1,000.

As everywhere in this repo, stored values are never converted; the display-unit
selector rescales presentation only. The departure is confined to one constant:

```r
SOURCE_UNIT <- 1     # one stored unit = 1 peso
```

If the workbook is ever restated in thousands, set it to `1e3` and nothing else
changes.

## Install

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "stringr",
  "ggplot2", "DT", "scales", "readxl", "readr"
))
```

`bslib` must be **≥ 0.5.0** — the layout uses `page_navbar(sidebar = ...)`, which
does not exist in earlier versions. `readxl` is the one dependency this dashboard
has that the agency dashboard does not.

## Run

```r
# from the repo root
shiny::runApp("dashboards/pap-browser")
```

## Tests

```r
# from the repo root
source("dashboards/pap-browser/test_server.R")   # reactives, filters, units, mobile
source("dashboards/pap-browser/test_future.R")   # robustness to workbook growth
```

Both print `N passed, 0 failed`.

`test_server.R` drives the server through `shiny::testServer` — filters,
toggles, unit conversion and the empty-selection edge cases — without needing a
browser. It asserts specific row counts (2,759 P/A/Ps; 19 DOE rows; 88
foreign-assisted) and the DOE NEP 2027 figure above.

Those counts **will** change as OEOs, SUCs and the rest of PSHS are encoded.
Update the expected numbers when that happens rather than deleting the
assertions — they are what catches a workbook that has silently lost rows.

`test_future.R` is the complement. It builds a synthetic workbook a version or
two ahead — an extra fiscal year, a new expense class, a new P/A/P row, and the
`SUCs` sheet with its labels filled in — and asserts the app absorbs all of it
without a code change. Run it after any change to the loader.

## What the app does

Three tabs.

### Finding an agency

The agency box holds all 180 agencies and is searchable, so a department does
not have to be chosen first. Picking one narrows the list; it is a convenience,
not a precondition, and an agency already chosen survives a department change if
it still belongs there.

Agency *names* are not unique — **"Office of the Secretary" belongs to 22
different departments** — so the selector is keyed on `DEPARTMENT|AGENCY` and
never on the label. Filtering on the name alone would return 22 departments'
worth of rows. Every label carries its department abbreviation
("Office of the Secretary — DOE"), which disambiguates the duplicates and lets
you search by department in the agency box.

### The table

**1. Browse** — the table and nothing else. There is no summary strip and no
value boxes above it: the figures are the point, and every 90px of chrome is a
row of data not shown. The table body fills the viewport and uses DT's
**Scroller** extension, so only the visible window is rendered rather than all
2,759 rows. A CSV of whatever is currently filtered downloads from the card
header as `ph-pap-browser-data_YYYY-MM-DD.csv`.

The four identity columns — Department, Agency, Program, P/A/P — are held to
about a third of the table by explicit pixel widths (`ID_COL_WIDTHS`, totalling
roughly 500px) and wrap rather than truncate, in a smaller face than the
figures. Left to size themselves they take half the table and the numbers are
pushed off-screen on open.

**All four are frozen**, not two: freezing Department and Agency alone would
scroll Program and P/A/P out of view, and those are the two a reader needs to
keep sight of while comparing years. `autoWidth = TRUE` is required for the
widths to be honoured, and the CSS selectors are doubled because FixedColumns
clones the frozen block into a second table that needs the same widths or the
two halves drift apart as the body scrolls.

There is deliberately no **Type** column. P/A/P type is one of three repeated
values, so it costs width without saying anything the P/A/P label does not
already say; it remains a sidebar filter.

The table pages at 100 rows rather than using DT's **Scroller** extension.
Scroller virtualizes by assuming every row is the same height, and the identity
columns wrap to a variable number of lines — which made it mis-size the body,
leaving a short table that still scrolled on a handful of rows. `scrollCollapse`
is also off: with it on, the body shrinks to fit the rows and the card is left
half empty whenever a filter is narrow. Off, the body always occupies the height
it is given, so a one-agency view fills the screen the same as the full table.

**2. Trend** — NEP against GAA for the current selection, by fiscal year, with
**NEP on the left and GAA on the right** in each year group, matching the order
in the table and the sequence in which the documents are produced: what was
proposed, then what was enacted. GAA carries the darker fill, so the pair reads
as proposal then enacted rather than as two arbitrary colors. Years with no data
are omitted rather than plotted at zero.

The factor levels are set explicitly rather than left to ggplot's alphabetical
default, which would put GAA first and would not survive a third document type
in any case.

Beneath the chart is a **recap of the current selection**. With nothing applied
it says so plainly — the bars are the whole data set, all labeled P/A/Ps across
every agency. Once anything is filtered it reports how much of the data is in
view ("Showing 100 of 2,759 labeled P/A/Ps (3.6%), across 7 of 179 agencies")
and lists each filter in force. It always names the expense class and display
unit, since those change what the bars mean rather than which rows are counted.

This exists because a filtered total looks exactly like a whole-of-budget total.
Someone screenshotting the chart mid-analysis should not be able to mistake one
department's locally-funded projects for the national aggregate.

**3. Notes** — the caveats that travel with the data.

## Filters

The sidebar is ordered by what a reader reaches for first: narrow to a subject,
then decide how to show it.

| Control | Behavior |
|---|---|
| **Department** | In UACS code order, not alphabetical. |
| **Agency** | Searchable, lists every agency, and does **not** require a department first. Selecting a department narrows the list. |
| **Search PROGRAM** | Free-text over the 4-digit headline programs. |
| **Search P/A/P** | Free-text over the granular P/A/P labels. |
| **P/A/P type** | Regular Activity, Locally-Funded Project, Foreign-Assisted Project. |
| **Document** | GAA, NEP, or both. Defaults to both. |
| **Fiscal years** | Any subset of FY2020–FY2027. Defaults to all. |
| **Expense class** | Total, PS, MOOE, FE, CO. Defaults to Total only. |
| **Display units** | Pesos, thousands, millions, billions. |

Selecting every expense class across every year gives 75 amount columns, which
is why Total alone is the default.

## Program versus P/A/P

The two search boxes hit different levels of the same hierarchy, and the
distinction is the point of this dashboard.

- **PROGRAM** is the agency's headline program, at the 4-digit PREXC code.
- **P/A/P** is the granular activity or project inside it, at the 12-digit code.

So `Search PROGRAM` for "health" finds whole programs; `Search P/A/P` for
"school building" finds individual project lines wherever they sit.

## Data conventions the app enforces

- **A blank is not a zero.** An empty cell means no data for that fiscal year.
  Where every expense column for a year is zero, that year carries no information
  for that P/A/P and is shown blank rather than as a real zero. The year group
  spans **both** documents, so NEP and GAA for a year are blanked together.
- **Unlabeled and empty rows are excluded.** 106 P/A/Ps have no label yet — most
  of them Philippine Science High School System — and a further 33 rows are zero
  in every column of every year. Removing them changes no series total. The
  counts are reported on the Notes tab so a reader can see what was dropped.
- **Statutory order is preserved**, not alphabetical. `DEPARTMENT` and `AGENCY`
  are fixed-width UACS numeric codes that follow the GAA's own structural
  sequence, so departments run Congress, OP, OVP, DAR, DA … rather than
  alphabetically. PREXC codes order the P/A/Ps within an agency. **The codes are
  ordering keys and are not displayed** — a reader wants names, and the numbers
  cost two columns of width. DT's initial sort is suppressed with
  `order = list()` so it cannot undo the ordering.
- **DPWH is at the 7-digit PREXC code**, which aggregates its project lines.
  Every other agency is at the 12-digit code. Without this DPWH would contribute
  tens of thousands of individual project rows and swamp the table. DPWH does not
  appear in the `NGAs` sheet, so binding the two sheets does not double count.
- **Labels are wrapped, never truncated.** Several P/A/Ps differ only in their
  tail, so an ellipsis would make them indistinguishable.
- **P/A/P type is read from the head of the label**, and the source is not
  uniform. It varies between singular and plural (`Locally-Funded Project:` in
  `NGAs` versus `Locally-Funded Projects:` in `DPWH-sub`), sometimes omits the
  noun (`Locally-Funded:`), and carries three PNP rows spelled
  `Locally-Funed Project:`. The matchers absorb all of it — see `RX_LFP` and
  `RX_FAP` in `app.R`. The typo is worth fixing at source, but the app does not
  depend on that happening.
- **FY2027 has no GAA.** The budget is not yet enacted. Selecting "GAA only"
  together with FY2027 correctly yields no columns rather than an error.

## Growing the workbook

The dashboard reads its shape from the workbook rather than from constants, so
the usual kinds of update need **no code change and no redeploy** — push the new
`Compiled_-_PAPs.xlsx` to `ph-budget-analysis` and it appears within the cache
TTL, or immediately via **Refresh from source**.

| Change to the workbook | What happens |
|---|---|
| New fiscal year (`GAA_2027_EXP_*`, `NEP_2028_EXP_*`) | Year appears in the picker, columns in the table, bars in the trend. |
| A year that has only a NEP, or only a GAA | Handled; the missing document simply has no columns. |
| New P/A/P rows | Appear, and are typed Locally-Funded / Foreign-Assisted from the label. |
| New agency or department | Appears in the pickers, in UACS code order. |
| **A sheet gains its `PROGRAM` and `PAP` labels** | The sheet starts being read. |
| New expense class (e.g. `_EXP_5DS`) | Appears as a filter under its raw code, and is named in the sidebar so a label can be added to `EXP_LABELS`. |
| New document type beyond NEP/GAA | Appears as a filter, is ordered after the known two, and gets a fallback fill in the trend. |

**Sheets are detected, not listed.** Any sheet carrying all eight identifier
columns and at least one `_EXP_` column is read and bound. The `SUCs` and `OEOs`
sheets already exist in the workbook but are missing `PROGRAM` and `PAP`, so they
are skipped today — and will be picked up automatically on the day those labels
are filled in. The sidebar names them as awaiting labels rather than ignoring
them silently. `SHEET_IGNORE` holds the handful of template and scratch sheets to
skip by name.

Three things are checked on every read and reported in the sidebar rather than
swallowed: **duplicate P/A/P keys across sheets** (which would double count),
**expense classes with no label**, and **unrecognized document types**.

### One thing to watch when SUCs and OEOs land

The `SUCs` sheet currently carries **295 codes that bear two agency names
apiece** — for example department 08 / agency 004 is both "Philippine State
College of Aeronautics" and "National Aviation Academy of the Philippines".
Those are renames, not duplicates, and the code is the identity: the picker shows
one entry per code, labeled with the name still in use in the most recent
document, and the table gives one continuous series across the rename.

But the duplicate-key count in the sidebar will jump when those sheets go live,
and it is worth reading it then. A rename is fine; the same P/A/P appearing in
two different sheets is not, and the counter cannot tell them apart on its own.

## Coverage

**Not yet included:** Other Executive Offices (OEOs) and State Universities and
Colleges (SUCs). Philippine Science High School System under DOST is incomplete.

This means departmental totals here are **not** comparable to the agency
dashboard's whole-of-budget figures, and should not be read as national totals.

## Mobile

The app adapts below a 768px viewport, using the same viewport-reporting script
as the agency dashboard: Shiny renders plots server-side as raster images and
cannot reflow them the way CSS reflows a div, so the server has to be told how
wide the screen is.

On a narrow screen the table **collapses its identity columns into one** —
"Agency — P/A/P" — and the years scroll sideways. That column is capped at 165px
and wraps; uncapped it runs the width of a long project title and pushes every
figure past the end of the horizontal scroll.

Frozen columns are dropped on mobile: at phone width four frozen columns consume
the whole screen, and DT's FixedColumns is unreliable on touch. The table also
switches to a viewport-height-free paging layout, because mobile browsers resize
the viewport as their chrome hides and reappears, which makes a vh-sized scroll
body jump while the reader scrolls.

The sidebar collapses to a toggle, so all the filters remain reachable.

The wide layout is the default until the browser reports in, so a desktop session
never flashes the narrow layout on load.

## Deploying

See [DEPLOY.md](../../DEPLOY.md).
