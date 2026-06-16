# Tuna Model Exploration Guide

This guide shows the intended day-to-day workflow. Keep the README short; use
this file when you need to remember how the pieces fit together.

## Core idea

Kflow runs dependency-aware Docker jobs from R:

```text
base -> sensitivity -> diagnostics -> plot -> report
base ----------------> diagnostics -> plot -> report
```

The local R session registers and launches jobs. The actual MFCL work runs
inside the Docker image:

```text
ghcr.io/pacificcommunity/tuna-flow:latest
```

This tag was built by GitHub Actions run `27601366609` from Docker image commit
`3b3677a6c73fb7fd184af6eea39efd2364931ad1` and resolves to
`sha256:192445f72e92fd1d92932468384913d44e2889b3fd0ec3f3c17c87376e12cdd3`.

This repository contains the workflow logic. Assessment-specific MFCL inputs are
kept in separate input-bundle repositories so the same workflow can be reused
for BET, YFT, SKJ, ALB, or another tuna assessment.

## Species preset

Set these before sourcing `R/workflow.R` when you want a different species or
assessment year:

```r
Sys.setenv(
  FLOW_SPECIES = "BET",
  FLOW_SPECIES_LABEL = "Bigeye tuna",
  FLOW_ASSESSMENT_YEAR = "2026",
  FLOW_SOURCE_REPO = "PacificCommunity/ofp-sam-bet2026-inputs",
  FLOW_SOURCE_REF = "main",
  FLOW_BASE_INPUT_DIR = "mfcl/inputs/2023_4region_1007",
  FLOW_MFCL_PROGRAM = "/home/mfcl/mfclo64"
)

source("R/workflow.R")
```

With that preset, task codes are generated as `bet-2026-base`,
`bet-2026-sensitivity`, `bet-2026-diagnostics`, `bet-2026-plot`, and
`bet-2026-report`. For YFT, set `FLOW_SPECIES = "YFT"` and choose the YFT input
directory; the same tables and dependencies still work.

For local dry runs before the input bundle is pushed, set `SOURCE_PATH` or
`FLOW_SOURCE_PATH` to a local input-bundle checkout. Do not put a local path into
remote Kflow jobs unless that path also exists inside the container runtime.

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

## Input bundle layout

An input bundle is a regular GitHub repository with a small, predictable layout:

```text
mfcl/
  inputs/
    2023_4region_1007/
      doitall.sh
      *.frq
      *.ini
      *.tag
      *.age_length
metadata/
  README.md
```

The executable is supplied by the Docker image. The workflow only needs
`FLOW_MFCL_PROGRAM` to point to the executable visible inside the container.
For host-only tests, override it with a local absolute path.

Optional metadata such as fishery definitions, tag-release labels, region maps,
and report text can be added under `metadata/`. If those files are absent,
downstream plotting should use numeric MFCL identifiers rather than inventing
labels.

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

Every job writes these fields, plus `FLOW_SPECIES` and
`FLOW_ASSESSMENT_YEAR`, to `model-registry.csv` and `model-registry.json`.

## Add a sensitivity

Most common MFCL sensitivities use the generic recipe columns already supported
by `scripts/run-mfcl-input-recipe.R`. Add one row to
`mfcl_input_recipe_library`, or select existing tokens through
`FLOW_STARTER_SENSITIVITIES`:

```r
Sys.setenv(
  FLOW_STARTER_SENSITIVITIES = "FixM,FixVB,Sel4,IndexCvHalf,TagMix2"
)
source("R/workflow.R")
```

To add a new recipe, keep the token short and the change detail explicit:

```r
mfcl_input_recipe_library <- rbind(
  mfcl_input_recipe_library,
  data.frame(
    RECIPE_TOKEN = "MHalf",
    RECIPE_KEY = "mHalf",
    RECIPE_FAMILY = "natural-mortality",
    RECIPE_LABEL = "Natural mortality half",
    INPUT_VARIANT_SUFFIX = "m_half",
    PATCH_SCRIPT = "scripts/run-mfcl-input-recipe.R",
    CHANGE_DETAIL = "Multiplies the natural mortality section by 0.5.",
    INPUT_RECIPE_INI_SECTION = "natural mortality",
    INPUT_RECIPE_INI_SECTION_MULTIPLIER = "0.5",
    INPUT_RECIPE_TO_INI_VERSION = "1007",
    INPUT_RECIPE_NORMALIZE_INI = "1",
    INPUT_RECIPE_VALIDATE_INI = "1",
    INPUT_RECIPE_PREFER_MFCLKIT = "1",
    stringsAsFactors = FALSE
  )
)
```

For changes not covered by the generic recipe columns, add a small R patch script
and point `PATCH_SCRIPT` to it. That script should make the exact `.ini`, `.frq`,
`.tag`, `.par`, `doitall.sh`, or other input edit and fail clearly if the
expected input structure is not present.

## INI version policy

All workflow-generated MFCL inputs are normalized to `.ini` version 1007 before
recipe edits. Conversion is delegated to `mfclkit`, which uses MFCL-aware
conversion logic and can validate the converted file. The flow intentionally
does not use approximate fallback converters; if an old `.ini` cannot be
converted cleanly, the job should fail so the input problem is visible.

## Add diagnostics

Diagnostics can consume either a base model or a sensitivity model. The starter
flow already builds diagnostics for the base model and all starter sensitivities.

To add another diagnostics recipe, add a row to `starter_diagnostics_recipes` and
rebuild:

```r
diagnostics_runs <- rbind(
  build_diagnostics_rows(base_models, starter_diagnostics_recipes, flow_task_codes[["base"]]),
  build_diagnostics_rows(sensitivity_models, starter_diagnostics_recipes, flow_task_codes[["sensitivity"]])
)
```

## Plot and report

`plot_runs$INPUT_KEY` can contain one diagnostics key or a comma-separated list.
That lets one plot job collect many models:

```r
plot_runs$INPUT_KEY <- paste(diagnostics_runs$JOB_KEY, collapse = ",")
```

The plot job writes:

- `report-figures/depletion-smoke.png`
- `plot-summary.csv`
- `model-registry.csv`

The report job writes:

- `ofp-sam-bet2026-report.pdf` or `.html`, depending on `REPORT_RENDER_FORMAT`
- `report-summary.csv`
- `model-registry.csv`

The report job is registered separately as `ofp-sam-bet2026-report`. It clones
`PacificCommunity/ofp-sam-2026-BET-report`, copies upstream Kflow figures into
the report, and renders only the compact report outputs. That lets you rerun
only the report stage from an existing plot job:

```r
launch_report(report_from(flow_task_codes[["plot"]], "plot-depletion-smoke", "report-rerun"))
```

If figures already exist in a report-style `Figures/` directory, pass that path
through `REPORT_FIGURE_INPUT_DIR` and keep `REPORT_REWRITE_FIGURES=false` so the
report source's own figure section is rendered as-is.

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

The starter backend is the MFCL executable supplied by the Docker image:

```text
/home/mfcl/mfclo64
```

The repository is structured so `MFCL_BACKEND=mfclrtmb` can be added later
through a backend script without changing the Kflow dependency layout.

Private helper packages such as `mfclkit`, `mfclshiny`, `mfclrtmb`, and
`KflowKit` can be updated at Docker container startup when
`KFLOW_RUNTIME_UPDATE=auto` and `GIT_PAT` or `GITHUB_PAT` are available. The
public smoke path keeps runtime updates off, so it runs without exposing private
package code or contacting private GitHub repositories.
Kflow clone tokens are not forwarded into the container unless a job explicitly
sets `KFLOW_FORWARD_GITHUB_TOKEN_TO_RUNTIME=1`.
