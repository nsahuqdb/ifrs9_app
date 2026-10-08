# IFRS 9 ECL — Shiny app

The user interface only. All calculation, reporting and analytics live in the
**`ifrs9qdb`** package, which this app loads at start-up. There is no `R/`
engine folder here: one copy of the logic, in the package, covered by the
package's tests.

## What is in this folder

```
app/            the Shiny app: app.R, 14 page modules, www/
R/              four assistant helpers only (llm_*.R) — deliberately not in
                the package, since they are app features rather than model code
config/         model.yml, model_inputs.yml, overlays.yml and the rest
config.yml      run configuration (paths, run settings, logging)
data-raw/static static reference CSVs
input/          drop raw extracts here
output/         single-folder batch output
runs/           the run archive the Runs page reads
```

Configuration and static reference stay **with the project**, not in the
package. They change every quarter and are the analyst's to edit; the copies
inside the package are a reference only.

## Install

From the git repositories: clone `ifrs9qdb` (the engine) and this repository
side by side, start R in this folder (renv activates itself), then

```r
renv::restore(exclude = "ifrs9qdb")   # the pinned CRAN packages, first time only
renv::install("../ifrs9qdb")          # the engine, from its clone
```

Re-run the `renv::install()` line after pulling a new engine version.

From a built package instead:

```r
install.packages("ifrs9qdb_1.0.1.tar.gz", repos = NULL, type = "source")

install.packages(c(
  "shiny", "bslib", "reactable", "echarts4r", "DT", "shinyAce",
  "htmltools", "htmlwidgets", "jsonlite", "yaml", "readr", "tibble",
  "markdown", "commonmark", "base64enc", "later", "curl", "openssl",
  "httr", "httr2"
))
```

## Run

```r
shiny::runApp("app")
```

from the project root. The console should show:

```
[app] ifrs9qdb 1.0.0 loaded (325 exported, 87 internal attached)
```

If the package is missing the app stops immediately with an install
instruction rather than failing later with a missing-function error.

## Why the internals are attached

The package exports its public API and keeps dot-prefixed helpers private,
which is correct for a package. The app genuinely uses about two dozen of them
— `.fer_classify_stage`, `.an_ingredients`, `.stress_rows`, `.build_gcc_history`
and similar. Rather than widen the package's public surface to suit one caller,
or scatter `:::` through the modules, `app.R` attaches the namespace once and
records why. If the app ever stops needing them, the block can go.

## Upgrading the engine

Install the new `ifrs9qdb` and restart the app. Nothing in this folder changes,
which is the point: the engine version is visible in the start-up message and
in the run manifest, and the app cannot silently diverge from the tested code.

Analytics -> Compare two runs -> Movement needs an engine with `ecl_bridge()`;
with an older one the tab says so instead of failing.
