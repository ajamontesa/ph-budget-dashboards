# PH Budget Dashboards

Interactive dashboards over the People's Budget Coalition's public
[PH Budget Data Set](https://docs.google.com/spreadsheets/d/1P3q46DGcN3SZ7cRwXfEhAMiDqCqQof1s3LV0-CK2lIY),
companions to the report and briefing suite at
<https://ajamontesa.github.io/ph-budget-analysis/>.

## Dashboards in this repo

| Directory | Scope | Data available | Status |
|---|---|---|---|
| `agency-budget-utilization/` | National agency level — departments, bureaux, attached agencies, aggregate blocks | NEP, GAA, allotments, obligations, disbursements | Live |
| `pap-budget/` | Programme / Activity / Project level | NEP and GAA only — DBM publishes no P/A/P execution data | Planned |

Each dashboard is a self-contained directory with its own `app.R` and
`manifest.json`, deployed to Connect Cloud as separate content. `app.R` is
deliberately kept as the filename inside each directory: Shiny and `rsconnect`
both expect that entry point, and the directory name carries the identity.

**`manifest.json` is generated, not written by hand.** It is a snapshot of the
R version and package versions on the machine that produced it, so it has to be
created locally, where the app is known to run:

```r
rsconnect::writeManifest(appDir = "agency-budget-utilization",
                         appPrimaryDoc = "app.R")
```

Commit the result — Connect Cloud requires it for any R content and cannot use
an `renv.lock` instead. Regenerate it whenever a `library()` call is added or
removed, or the deploy will install the wrong set of packages.

```
ph-budget-dashboards/
├── README.md                              this file
├── DEPLOY.md                              deployment, applies to all dashboards
├── agency-budget-utilization/
│   ├── app.R
│   ├── manifest.json                      GENERATED — see below
│   └── README.md                          scope, indicators, data conventions
├── pap-budget/                            (future, same shape)
└── embed/
    └── agency-budget-utilization.html     copy into ph-budget-analysis
```

## Naming conventions

- **Directories** are lowercase and hyphenated, describing the *slice of the
  budget* rather than the technology: `agency-budget-utilization`, not
  `dashboard` or `app`.
- **Embed pages** in `embed/` take the same name as the dashboard directory.
  They are templates: copy them into the `ph-budget-analysis` repo, where they
  become the page a reader navigates to from the index.
- **CSV exports** are prefixed by scope — `ph-agency-budget-data_YYYY-MM-DD.csv`
  — so a reader who downloads from more than one dashboard ends up with files
  they can tell apart.
- **Environment variables** are namespaced per dashboard, e.g.
  `PBC_AGENCY_CACHE_TTL`, so cache behaviour can be tuned independently.

## Shared conventions across all dashboards

These hold everywhere and are worth keeping consistent as dashboards are added.

- **All figures are in thousands of pesos**, as published by DBM. Stored values
  are never converted; display-unit selectors rescale presentation only.
- **A blank is not a zero.** An empty cell means not reported, an agency that
  did not yet exist, one housed under a different mother department that year,
  or a data frontier that has not reached that year. A literal zero is carried
  as zero because it is what DBM published. The two are never interchanged.
- **Sheet order is preserved**, not alphabetized. The sheet follows the GAA's
  own structural sequence.
- **The data frontier is ragged.** NEP runs a year ahead of GAA, which runs a
  year ahead of execution. Reference years are detected from the data on every
  load, never hard-coded.
- **Appropriations are Total Appropriations** — New plus Automatic.
- **Layouts adapt below 768px.** Plot heights are derived from row count rather
  than hard-coded, ranking sizes are trimmed, and tables collapse their identity
  columns and drop frozen columns. Any new dashboard should do the same.
- **`DEPARTMENT` + `AGENCY` is the join key**, but agencies move between
  departments across and within administrations, so it is a key rather than a
  stable identity.

## Deploying

See `DEPLOY.md`. Each dashboard deploys independently; the process is the same
for all of them, with the directory path as the only thing that changes.
