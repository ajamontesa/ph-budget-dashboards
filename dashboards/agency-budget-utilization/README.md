# Agency Budget & Utilization Dashboard

Part of [PH Budget Dashboards](../README.md). Covers the **national agency
level** — departments, their bureaux and attached agencies, plus the Total
Budget and Total NGAs aggregate blocks — across NEP, GAA, allotments,
obligations and disbursements, FY 2016–2027.

Programme, Activity and Project detail is out of scope here; that is a separate
dashboard, and it will carry appropriations only, since DBM publishes no
P/A/P-level execution data.

Source: the public
[PH Budget Data Set](https://docs.google.com/spreadsheets/d/1P3q46DGcN3SZ7cRwXfEhAMiDqCqQof1s3LV0-CK2lIY).

The app reads the Google Sheet directly, so publishing an update to the sheet
is all that is needed to update the dashboard. There is a **Refresh from Google
Sheet** button in the sidebar for pulling changes immediately.

The fetched data is held in a cache shared by every concurrent session, with a
one-hour time-to-live (override with the `PBC_AGENCY_CACHE_TTL` environment variable,
in seconds). Only the first visitor after a cold start or an expiry pays the
download and the pipeline; everyone else reads the cached copy. If the sheet
becomes unreachable the app keeps serving the last good copy and notes the
failed refresh in the sidebar, rather than erroring out.

For deployment, see [DEPLOY.md](../DEPLOY.md).

## Units

Every figure in the source sheet is in **thousands of pesos**, as published by
DBM. Stored values are never converted. The display-unit selector rescales the
presentation only; ratios are unaffected.

## Install

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "stringr",
  "ggplot2", "DT", "scales", "readr"
))

# Optional fallback route if the CSV endpoint is ever unavailable
install.packages("googlesheets4")
```

`bslib` must be **≥ 0.5.0** — the layout uses `page_navbar(sidebar = ...)`,
`value_box()` and `layout_columns()`, none of which exist in earlier versions.

## Run

```r
# from the repo root
shiny::runApp("agency-budget-utilization")
```

No authentication is needed: the sheet is public and is read through the
CSV export endpoint. If that endpoint fails, the app falls back to
`googlesheets4` in deauthorised mode.

## What the app does

Five tabs, split by what drives them.

**1. Budget Overview** — whole-of-budget, and deliberately *not* affected by the
department / agency filters, which are hidden while this tab is open. It carries
its own controls instead: a **budget year** selector with an NEP/GAA measure
toggle, and a separate **utilization year** selector. The two move independently
because the data frontier is ragged — the newest proposal year has no enacted
counterpart and no execution data. The measure toggle offers only what exists for
the chosen year and prefers GAA, falling back to NEP for the newest year.

Charts: Top 30 departments and Top 30 agencies side by side; utilization for
departments and agencies side by side, as dot plots showing the obligation and
disbursement rate together with the gap between them; and the largest
congressional adjustments, rankable by percent or by pesos.

**2. Agency Trends** — everything that depends on the sidebar selection:
utilization rates, percent shares, year-on-year change, and the congressional
adjustment over time. Defaults to Total NGAs. Percent shares cover Total Budget
and Total NGAs only; share of parent department lives in the Key Indicators
table, since six series on one panel was unreadable. The whole shares panel is
suppressed when the subject is Total Budget or Total NGAs, as those blocks are
the denominator.

**3. Key Indicators** — indicators down the rows, fiscal years across the
columns, at full viewport height. Department, Agency, Level and Indicator stay
frozen while the years scroll. CSV export gives the same shape as numeric
proportions.

**4. Data Viewer** — the full table at full viewport height, years across the
columns, with a CSV download of whatever is currently filtered.

**5. Notes** — the caveats that travel with the data.

## Mobile

The app adapts below a 768px viewport. A small script reports the browser width
to the server on connect, on resize and on orientation change; the server needs
this because Shiny renders plots as server-side raster images and cannot reflow
them the way CSS reflows a div.

On a narrow screen:

- **Rankings are trimmed** — 30 bars becomes 12, and top/bottom 15 becomes 8.
  Thirty bars in a 360px panel is not a smaller version of the desktop chart,
  it is an unreadable one.
- **Plot heights are computed from row count** rather than fixed in pixels, with
  extra room per row because labels wrap to more lines when narrow.
- **Labels wrap tighter and type shrinks slightly**, so the label column does
  not crowd out the bars.
- **Tables collapse their identity columns into one** — "Agency — Indicator" —
  and the years scroll sideways. Frozen columns are dropped: at phone width four
  frozen columns consume the whole screen, and DT's FixedColumns is unreliable
  on touch.
- **Tables use ordinary paging** instead of a `100vh`-based scroll body, because
  mobile browsers resize the viewport as their chrome hides and reappears, which
  makes a vh-sized table jump while the reader scrolls.

The wide layout is the default until the browser reports in, so a desktop
session never flashes the narrow layout on load.

## Definitions

| Indicator | Definition |
|---|---|
| Obligation Rate | Obligations ÷ Allotments |
| Disbursement Rate | Disbursements ÷ Allotments |
| NEP / GAA % of Total Budget | Agency ÷ the `Total Budget` block |
| NEP / GAA % of Total NGAs | Agency ÷ the `Total National Government Agencies (NGAs)` block |
| NEP / GAA % of Department | Agency ÷ its parent department row (agency rows only) |
| NEP / GAA % Change | Like-for-like against the prior year |
| NEP vs Prior GAA % | This year's proposal against last year's enacted budget |
| GAA vs NEP % | Congressional adjustment within the same fiscal year |

Both utilization rates share Allotments as the denominator, so the gap between
them reads directly as the obligated-but-unpaid overhang.

`GAA vs NEP %` shows what Congress did to the proposal within the same fiscal
year. Congress cannot raise the overall total, only realign within it, so
augmentations to one line are funded by reductions elsewhere.

For the newest NEP year there is no enacted counterpart, so `GAA % Change` is
empty and `NEP vs Prior GAA %` carries the comparison.

## Data conventions the app enforces

- **Blank ≠ zero.** An empty cell is carried as missing. A literal zero is
  carried as zero, because that is what DBM published. The two are never
  interchanged, and they are displayed differently (`—` versus `0.0%`).
- **A blank is not a zero, and not always the same thing.** It may mean the
  agency did not yet exist, was housed under a different mother department that
  year, was abolished or renamed, was simply not reported, or that the data
  frontier has not reached that year. The dashboard cannot tell these apart, so
  the Notes tab spells them out. The reorganisation case matters most in
  practice: a department-filtered trend can truncate an agency at its transfer
  year without the break being visible, so track a body by agency rather than by
  department across a reorganisation.
- **A department with no bureau rows is its own sole agency.** DPWH, OP, OVP,
  DOE and others have no separate attached-agency lines in the sheet, so they
  appear in both the department and the agency views. Treating "agency" as
  everything that is not a department would drop several of the largest
  spenders out of the agency picture entirely. Departments that *do* have
  bureau rows stay out of the agency view, so nothing is double-counted.
- **Utilization axes are anchored at 0–100%.** A low rate should read as low
  rather than being stretched to fill the panel. Rates above 100% extend the
  axis rather than being clipped.
- **Sheet order is preserved.** Departments and agencies appear in the order
  they occupy in the source sheet, which follows the GAA's own structural
  sequence. Alphabetizing would destroy it, so every table, filter and export
  sorts on captured row position and DT's initial sort is suppressed.
- **Reported zero disbursements are left unplotted, not drawn at zero.** Zero
  disbursements against non-zero obligations are almost certainly non-submitted
  FARs, so on the utilization dot plot the disbursement point is omitted while
  the obligation point still shows — the gap reads as missing rather than as 0%
  performance. They remain in the data and tables exactly as published; the
  caveat is on the Notes tab.
- **Labels are wrapped, never truncated.** Several agencies differ only in the
  tail of their names, so an ellipsis would make them indistinguishable.
- **Rates above 100% are expected.** Allotments routinely exceed the agency GAA
  line because continuing appropriations, automatic appropriations and SPF
  releases land in allotments without appearing there.
- **Aggregate blocks are excluded from rankings.** Any row whose department name
  begins with "Total" and equals its agency name is treated as an aggregate, so
  `Total GOCCs` / `Total LGUs` / `Total SPFs` blocks will be handled correctly
  when added without a code change.
- **Total Appropriations = New + Automatic.** Agencies with large automatic
  components are not directly comparable to peers funded mostly by new
  appropriations.

## Built to extend

The tidy schema carries `expense_class` and `period` columns, currently pinned
at `"All"` and `"Annual"`. When expense class (PS / MOOE / FE / CO) and
quarterly utilization are added, they arrive as additional **rows** rather than
forcing a rewrite of the reactives. The same applies to a P/A/P-level dataset:
it slots in as a further grouping column rather than a parallel app.

Reference years are detected from the data on every load, never hard-coded, so
the ragged frontier moves on its own as the sheet is updated.
