# BET 2026 Model Exploration Guide

This guide shows the intended day-to-day workflow. Keep the README short; use
this file when you need to remember how the pieces fit together.

## Core idea

Kflow runs dependency-aware Docker jobs from R:

```text
base -> sensitivity -> diagnostics -> plot -> report
base ----------------> diagnostics -> plot -> report
```

The local R session only registers and launches jobs. The actual MFCL work runs
inside the Docker image:

```text
ghcr.io/pacificcommunity/bet2026-flow:latest
```

## Where to edit

Most routine edits happen in `R/workflow.R`.

The starter objects are:

- `base_models`: base model rows.
- `starter_sensitivity_recipes`: short recipe table for sensitivities.
- `starter_diagnostics_recipes`: short recipe table for diagnostics.
- `sensitivity_models`: generated Kflow rows.
- `diagnostics_runs`: generated Kflow rows.
- `plot_runs`: plot jobs that collect one or many diagnostics.
- `report_runs`: Quarto report jobs.

For many models, add rows to recipe tables rather than copying long Kflow rows.

## Naming rules

Use short tokens that make sense in tables and plots:

- `MODEL_TOKEN`: model identity, such as `Base4R`, `FixM`, `FixVB`.
- `PLOT_LABEL`: label shown in figures.
- `REPORT_LABEL`: label shown in reports.
- `CHANGE_TOKEN`: compact change name, such as `FixM`.
- `CHANGE_GROUP`: broad group, such as `growth` or `natural-mortality`.
- `CHANGE_DETAIL`: exact note about what changed.
- `PARENT_MODEL_TOKEN`: upstream model token.
- `RECIPE_TOKEN`: recipe name used to generate the row.

Every job writes these fields to `model-registry.csv` and
`model-registry.json`.

## Add a sensitivity

Add one row to `starter_sensitivity_recipes`:

```r
starter_sensitivity_recipes <- rbind(
  starter_sensitivity_recipes,
  data.frame(
    RECIPE_TOKEN = "SteepLow",
    RECIPE_KEY = "steepLow",
    RECIPE_FAMILY = "stock-recruitment",
    RECIPE_LABEL = "Lower steepness",
    INPUT_VARIANT_SUFFIX = "steepLow",
    PATCH_SCRIPT = "patches/steep_low.R",
    CHANGE_DETAIL = "Lower stock-recruitment steepness sensitivity.",
    stringsAsFactors = FALSE
  )
)

sensitivity_models <- build_sensitivity_rows(base_models, starter_sensitivity_recipes)
```

Then create `sensitivity/patches/steep_low.R`. Start from an existing patch:

```r
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE)), "recipe_marker_helpers.R"))

copy_input_with_recipe_marker(
  recipe = "SteepLow",
  intended_change = "Lower stock-recruitment steepness sensitivity.",
  full_run_note = "Replace this marker with the explicit MFCL input edit before inference runs."
)
```

For real inference, replace the marker with the exact `.ini`, `.frq`, `.tag`,
`.par`, or other input edit.

## Add diagnostics

Diagnostics can consume either a base model or a sensitivity model. The starter
flow already builds diagnostics for the base model and all starter sensitivities.

To add another diagnostics recipe, add a row to `starter_diagnostics_recipes` and
rebuild:

```r
diagnostics_runs <- rbind(
  build_diagnostics_rows(base_models, starter_diagnostics_recipes, "bet2026-base"),
  build_diagnostics_rows(sensitivity_models, starter_diagnostics_recipes, "bet2026-sensitivity")
)
```

## Plot and report

`plot_runs$INPUT_KEY` can contain one diagnostics key or a comma-separated list.
That lets one plot job collect many models:

```r
plot_runs$INPUT_KEY <- paste(diagnostics_runs$JOB_KEY, collapse = ",")
```

The plot job writes:

- `depletion-smoke-combined.csv`
- `model-exploration-overview.svg`
- `depletion-smoke.png`
- `mfclshiny-report-figures/`
- `plot-input-registry.csv`

The report job copies upstream figures automatically into the Quarto report.

## Register and launch

```r
source("R/workflow.R")
register_tasks()

launch_base()
launch_sensitivity()
launch_diagnostics()
launch_plot()
launch_report()
```

For a quick plan preview:

```r
source("R/workflow.R")
source("R/plan.R")

plan <- build_starter_plan()

preview_plan(plan)
```

## Runtime notes

The default backend is the included MFCL executable:

```text
mfcl/exe/mfclo64_2026_02_04_vsn2278
```

The repository is structured so `MFCL_BACKEND=mfclrtmb` can be added later
through a backend script without changing the Kflow dependency layout.

Private helper packages such as `mfclkit`, `mfclshiny`, `mfclrtmb`, and
`KflowKit` can be updated at Docker container startup when `GIT_PAT` or
`GITHUB_PAT` is available. The public smoke path still runs without exposing
private package code.
