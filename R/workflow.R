# BET Kflow launch layer.
#
# This is the main file to edit day to day.
# - Edit the tables near the top to add/remove base models, sensitivities,
#   diagnostics, plot jobs, or reports.
# - Use the launch_*() functions near the bottom to submit jobs to Kflow.
# - Keep heavy job internals in R/job_helpers.R and large combinatorics in
#   R/plan.R so this file stays readable.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

bet_nonempty <- function(x, y = "") {
  if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(as.character(x[[1]]))) y else as.character(x[[1]])
}

bet_require_kflowkit <- function() {
  if (!requireNamespace("KflowKit", quietly = TRUE)) {
    stop(
      "KflowKit is required to register or launch jobs. Install it with remotes::install_github('kyuhank/KflowKit').",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ---- Project defaults ------------------------------------------------------------
#
# These defaults are applied to every launched job. Override them with
# Sys.setenv(...) before sourcing this file, or add matching columns to any row
# in the tables below.

bet_kflow_repo <- Sys.getenv("BET_KFLOW_REPO", "PacificCommunity/ofp-sam-2026-BET-flow")
bet_kflow_branch <- Sys.getenv("BET_KFLOW_BRANCH", "main")
bet_source_repo <- Sys.getenv("BET_SOURCE_REPO", "flow_checkout")
bet_source_ref <- Sys.getenv("BET_SOURCE_REF", "")
bet_docker_image <- Sys.getenv("BET_DOCKER_IMAGE", "ghcr.io/pacificcommunity/bet2026-flow:latest")
bet_flow_group <- paste0("bet-", format(Sys.time(), "%Y%m%d-%H%M%S"))
bet_task_codes <- c(
  base = "bet2026-base",
  sensitivity = "bet2026-sensitivity",
  diagnostics = "bet2026-diagnostics",
  plot = "bet2026-plot",
  report = "bet2026-report"
)
bet_default_program <- "mfcl/exe/mfclo64_2026_02_04_vsn2278"
bet_default_input_dir <- "mfcl/inputs/2023_4region"

# ---- Small editable starter tables ----------------------------------------------
#
# These tables are intentionally plain data.frames. To add a model, add a row.
# To skip a model, delete or filter a row. Any extra column becomes an environment
# variable for the Kflow job, so you can add MFCL or helper-script settings
# without changing the launch functions.
#
# Naming convention:
# - JOB_KEY: stable Kflow dependency key.
# - MODEL_TOKEN: compact readable token for figures/tables, e.g. FixM.
# - CHANGE_TOKEN: what changed relative to the parent, e.g. FixM.
# - CHANGE_SUMMARY: human-readable explanation for future you.

base_models <- data.frame(
  RUN_LABEL = "base-4r-smoke",
  JOB_KEY = "base-4r-smoke",
  MODEL_KEY = "base-4r-smoke",
  MODEL_TOKEN = "Base4R",
  MODEL_NAME = "BET 2023 4-region base smoke",
  BASE_MODEL_KEY = "",
  CHANGE_TOKEN = "Base4R",
  CHANGE_GROUP = "base",
  CHANGE_SUMMARY = "Runs the included 2023 4-region BET MFCL input through a fast makepar smoke check.",
  INPUT_VARIANT = "2023_4region",
  SOURCE_REPO = "flow_checkout",
  SOURCE_REF = "",
  USE_FLOW_SOURCE = "1",
  MFCL_BACKEND = "mfcl_smoke",
  PROGRAM_PATH = bet_default_program,
  PATCH_SCRIPT = "",
  PATCH_INPUT_DIR = "",
  PATCH_OUTPUT_DIR = bet_default_input_dir,
  JOB_TITLE = "BET 2026 base: 4-region smoke",
  JOB_DESCRIPTION = "Runs the included 2023 4-region BET input with the MFCL executable using a fast makepar smoke check.",
  MAKE_TARGETS = "mfcl-smoke",
  BASE_DIR = bet_default_input_dir,
  MODEL_DIR = "model/base-4r-smoke",
  COLLECT_PATHS = "model/base-4r-smoke,mfcl/inputs/2023_4region,mfcl/exe/mfclo64_2026_02_04_vsn2278",
  stringsAsFactors = FALSE
)

starter_sensitivity_recipes <- data.frame(
  RECIPE_TOKEN = c("NoAgeSmoke", "FixM", "FixVB"),
  RECIPE_KEY = c("noAgeSmoke", "fixM", "fixVB"),
  RECIPE_FAMILY = c("age-data", "natural-mortality", "growth"),
  RECIPE_LABEL = c("No age data", "Fixed M", "Fixed growth"),
  INPUT_VARIANT_SUFFIX = c("noAgeSmoke", "fixM", "fixVB"),
  PATCH_SCRIPT = c("patches/no_age_smoke.R", "patches/fix_m_smoke.R", "patches/fix_vb_smoke.R"),
  CHANGE_DETAIL = c(
    "No-age sensitivity marker based on the BET report noAge idea.",
    "Fixed natural mortality marker. Replace the smoke marker with the exact M-fix edit for inference runs.",
    "Fixed von Bertalanffy growth marker. Replace the smoke marker with the exact growth-fix edit for inference runs."
  ),
  stringsAsFactors = FALSE
)

starter_diagnostics_recipes <- data.frame(
  RECIPE_TOKEN = "JitterSmoke",
  RECIPE_KEY = "jitter",
  RECIPE_FAMILY = "diagnostics",
  RECIPE_LABEL = "Jitter smoke",
  CHANGE_DETAIL = "Lightweight jitter-style diagnostics aggregation for fast dependency testing.",
  MAKE_TARGETS = "diagnostics-smoke",
  JITTER_SEED = 40,
  stringsAsFactors = FALSE
)

build_sensitivity_rows <- function(bases, recipes) {
  rows <- list()
  for (base_index in seq_len(nrow(bases))) {
    base <- bases[base_index, , drop = FALSE]
    for (recipe_index in seq_len(nrow(recipes))) {
      recipe <- recipes[recipe_index, , drop = FALSE]
      job_key <- paste("sens", recipe$RECIPE_KEY, sep = "-")
      input_variant <- paste("2023_4region", recipe$INPUT_VARIANT_SUFFIX, sep = "_")
      base_dir <- file.path("mfcl/inputs", input_variant)
      model_dir <- file.path("model", job_key)
      model_token <- recipe$RECIPE_TOKEN
      rows[[length(rows) + 1L]] <- data.frame(
        RUN_LABEL = job_key,
        JOB_KEY = job_key,
        MODEL_KEY = job_key,
        MODEL_TOKEN = model_token,
        MODEL_NAME = paste("BET", recipe$RECIPE_LABEL, "sensitivity smoke"),
        MODEL_LABEL = paste(base$MODEL_TOKEN, recipe$RECIPE_TOKEN, sep = " | "),
        PLOT_LABEL = recipe$RECIPE_TOKEN,
        REPORT_LABEL = paste(recipe$RECIPE_TOKEN, "-", recipe$RECIPE_LABEL),
        BASE_MODEL_KEY = base$JOB_KEY,
        PARENT_MODEL_KEY = base$JOB_KEY,
        PARENT_MODEL_TOKEN = base$MODEL_TOKEN,
        RECIPE_TOKEN = recipe$RECIPE_TOKEN,
        RECIPE_FAMILY = recipe$RECIPE_FAMILY,
        RECIPE_LABEL = recipe$RECIPE_LABEL,
        CHANGE_TOKEN = recipe$RECIPE_TOKEN,
        CHANGE_GROUP = recipe$RECIPE_FAMILY,
        CHANGE_DETAIL = recipe$CHANGE_DETAIL,
        CHANGE_SUMMARY = paste(recipe$CHANGE_DETAIL, "Runs a fast makepar smoke check."),
        INPUT_VARIANT = input_variant,
        INPUT_TASK = bet_task_codes[["base"]],
        INPUT_KEY = base$JOB_KEY,
        SOURCE_REPO = "flow_checkout",
        SOURCE_REF = "",
        USE_FLOW_SOURCE = "1",
        MFCL_BACKEND = "mfcl_smoke",
        PROGRAM_PATH = bet_default_program,
        MAKE_TARGETS = "mfcl-smoke",
        BASE_DIR = base_dir,
        MODEL_DIR = model_dir,
        PATCH_SCRIPT = recipe$PATCH_SCRIPT,
        PATCH_INPUT_DIR = bet_default_input_dir,
        PATCH_OUTPUT_DIR = base_dir,
        JOB_TITLE = paste("Sensitivity:", recipe$RECIPE_TOKEN),
        JOB_DESCRIPTION = paste("Builds", recipe$RECIPE_LABEL, "from", base$MODEL_TOKEN, "and runs a fast MFCL smoke check."),
        COLLECT_PATHS = paste(base$MODEL_DIR, model_dir, base_dir, sep = ","),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

build_diagnostics_rows <- function(parent_rows, recipes, input_task) {
  rows <- list()
  for (parent_index in seq_len(nrow(parent_rows))) {
    parent <- parent_rows[parent_index, , drop = FALSE]
    parent_input_task <- if (length(input_task) > 1L) input_task[[parent_index]] else input_task
    for (recipe_index in seq_len(nrow(recipes))) {
      recipe <- recipes[recipe_index, , drop = FALSE]
      seed <- suppressWarnings(as.integer(recipe$JITTER_SEED)) + parent_index - 1L
      job_key <- paste("diag", parent$MODEL_TOKEN, recipe$RECIPE_KEY, sep = "-")
      job_key <- gsub("[^A-Za-z0-9_-]+", "-", job_key)
      model_token <- paste(parent$MODEL_TOKEN, recipe$RECIPE_TOKEN, sep = "_")
      rows[[length(rows) + 1L]] <- data.frame(
        RUN_LABEL = job_key,
        JOB_KEY = job_key,
        MODEL_KEY = job_key,
        MODEL_TOKEN = model_token,
        MODEL_NAME = paste("BET", parent$MODEL_TOKEN, recipe$RECIPE_LABEL, "diagnostics smoke"),
        MODEL_LABEL = paste(parent$MODEL_TOKEN, recipe$RECIPE_TOKEN, sep = " | "),
        PLOT_LABEL = paste(parent$MODEL_TOKEN, recipe$RECIPE_TOKEN, sep = " + "),
        REPORT_LABEL = paste(parent$MODEL_TOKEN, recipe$RECIPE_LABEL, sep = " - "),
        BASE_MODEL_KEY = bet_nonempty(parent$BASE_MODEL_KEY, parent$JOB_KEY),
        PARENT_MODEL_KEY = parent$JOB_KEY,
        PARENT_MODEL_TOKEN = parent$MODEL_TOKEN,
        RECIPE_TOKEN = recipe$RECIPE_TOKEN,
        RECIPE_FAMILY = recipe$RECIPE_FAMILY,
        RECIPE_LABEL = recipe$RECIPE_LABEL,
        CHANGE_TOKEN = recipe$RECIPE_TOKEN,
        CHANGE_GROUP = recipe$RECIPE_FAMILY,
        CHANGE_DETAIL = recipe$CHANGE_DETAIL,
        CHANGE_SUMMARY = paste(recipe$CHANGE_DETAIL, "Input:", parent$MODEL_TOKEN),
        INPUT_VARIANT = parent$INPUT_VARIANT %||% "",
        INPUT_TASK = parent_input_task,
        INPUT_KEY = parent$JOB_KEY,
        SOURCE_REPO = "flow_checkout",
        SOURCE_REF = "",
        USE_FLOW_SOURCE = "1",
        MFCL_BACKEND = "diagnostics_smoke",
        PROGRAM_PATH = bet_default_program,
        MAKE_TARGETS = recipe$MAKE_TARGETS,
        BASE_DIR = parent$BASE_DIR %||% "",
        MODEL_DIR = file.path("model", job_key),
        JITTER_SEED = seed,
        JITTER_SMOKE_ONLY = "1",
        PATCH_SCRIPT = "",
        PATCH_INPUT_DIR = "",
        PATCH_OUTPUT_DIR = "",
        JOB_TITLE = paste("Diagnostics:", parent$MODEL_TOKEN, recipe$RECIPE_TOKEN),
        JOB_DESCRIPTION = paste("Creates a short diagnostics summary from", parent$MODEL_TOKEN, "using", recipe$RECIPE_LABEL, "."),
        COLLECT_PATHS = file.path("model", job_key),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

sensitivity_models <- build_sensitivity_rows(base_models, starter_sensitivity_recipes)

diagnostics_runs <- rbind(
  build_diagnostics_rows(base_models, starter_diagnostics_recipes, bet_task_codes[["base"]]),
  build_diagnostics_rows(sensitivity_models, starter_diagnostics_recipes, bet_task_codes[["sensitivity"]])
)

plot_runs <- data.frame(
  RUN_LABEL = "plot-depletion-smoke",
  JOB_KEY = "plot-depletion-smoke",
  MODEL_KEY = "plot-depletion-smoke",
  MODEL_TOKEN = "PlotDepletionSmoke",
  MODEL_NAME = "BET depletion smoke plot package",
  BASE_MODEL_KEY = "base-4r-smoke",
  CHANGE_TOKEN = "Plot",
  CHANGE_GROUP = "plot",
  CHANGE_SUMMARY = "Collects selected smoke outputs into a simple depletion plot package.",
  JOB_TITLE = "Plot: depletion smoke",
  JOB_DESCRIPTION = "Creates a simple depletion plot from selected model and diagnostics outputs.",
  INPUT_TASK = bet_task_codes[["diagnostics"]],
  INPUT_KEY = paste(diagnostics_runs$JOB_KEY, collapse = ","),
  PLOT_TITLE = "BET 2026 depletion smoke check",
  PLOT_BACKEND = "mfclshiny",
  MFCLSHINY_SCRIPT = "hooks/depletion_smoke.R",
  stringsAsFactors = FALSE
)

report_runs <- data.frame(
  RUN_LABEL = "report-depletion-smoke",
  JOB_KEY = "report-depletion-smoke",
  MODEL_KEY = "report-depletion-smoke",
  MODEL_TOKEN = "ReportDepletionSmoke",
  MODEL_NAME = "BET depletion smoke Quarto report",
  BASE_MODEL_KEY = "base-4r-smoke",
  CHANGE_TOKEN = "Report",
  CHANGE_GROUP = "report",
  CHANGE_SUMMARY = "Renders the selected depletion smoke plot into a Quarto HTML report.",
  JOB_TITLE = "Report: BET depletion smoke",
  JOB_DESCRIPTION = "Renders a Quarto report from the selected depletion smoke plot.",
  INPUT_TASK = bet_task_codes[["plot"]],
  INPUT_KEY = "plot-depletion-smoke",
  REPORT_TITLE = "BET 2026 Kflow depletion smoke report",
  stringsAsFactors = FALSE
)

# ---- Row normalization -----------------------------------------------------------
#
# common_env() fills in defaults and standard metadata columns. It accepts extra
# columns and leaves them in place; those extra columns are passed to Kflow as
# job environment variables.

common_env <- function(rows) {
  rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  rows$SOURCE_REPO <- if ("SOURCE_REPO" %in% names(rows)) rows$SOURCE_REPO else bet_source_repo
  rows$SOURCE_REF <- if ("SOURCE_REF" %in% names(rows)) rows$SOURCE_REF else bet_source_ref
  rows$USE_FLOW_SOURCE <- if ("USE_FLOW_SOURCE" %in% names(rows)) {
    rows$USE_FLOW_SOURCE
  } else {
    ifelse(tolower(rows$SOURCE_REPO) %in% c("flow_checkout", "local", ".", "flow", "this"), "1", "0")
  }
  rows$USE_LOCAL_SOURCE <- if ("USE_LOCAL_SOURCE" %in% names(rows)) rows$USE_LOCAL_SOURCE else ""
  rows$MFCL_BACKEND <- if ("MFCL_BACKEND" %in% names(rows)) rows$MFCL_BACKEND else "mfcl_exe"
  rows$BACKEND_SCRIPT <- if ("BACKEND_SCRIPT" %in% names(rows)) rows$BACKEND_SCRIPT else ""
  rows$BACKEND_COMMAND <- if ("BACKEND_COMMAND" %in% names(rows)) rows$BACKEND_COMMAND else ""
  rows$MFCLKIT_SCRIPT <- if ("MFCLKIT_SCRIPT" %in% names(rows)) rows$MFCLKIT_SCRIPT else ""
  rows$MFCLSHINY_SCRIPT <- if ("MFCLSHINY_SCRIPT" %in% names(rows)) rows$MFCLSHINY_SCRIPT else ""
  rows$PROGRAM_PATH <- if ("PROGRAM_PATH" %in% names(rows)) rows$PROGRAM_PATH else bet_default_program
  rows$FLOW_GROUP <- if ("FLOW_GROUP" %in% names(rows)) rows$FLOW_GROUP else bet_flow_group
  rows$MODEL_KEY <- if ("MODEL_KEY" %in% names(rows)) rows$MODEL_KEY else rows$JOB_KEY
  rows$MODEL_TOKEN <- if ("MODEL_TOKEN" %in% names(rows)) rows$MODEL_TOKEN else rows$RUN_LABEL
  rows$MODEL_LABEL <- if ("MODEL_LABEL" %in% names(rows)) rows$MODEL_LABEL else rows$MODEL_TOKEN
  rows$PLOT_LABEL <- if ("PLOT_LABEL" %in% names(rows)) rows$PLOT_LABEL else rows$MODEL_TOKEN
  rows$REPORT_LABEL <- if ("REPORT_LABEL" %in% names(rows)) rows$REPORT_LABEL else rows$MODEL_NAME %||% rows$MODEL_TOKEN
  rows$BASE_MODEL_KEY <- if ("BASE_MODEL_KEY" %in% names(rows)) rows$BASE_MODEL_KEY else ""
  rows$PARENT_MODEL_KEY <- if ("PARENT_MODEL_KEY" %in% names(rows)) rows$PARENT_MODEL_KEY else rows$BASE_MODEL_KEY
  rows$PARENT_MODEL_TOKEN <- if ("PARENT_MODEL_TOKEN" %in% names(rows)) rows$PARENT_MODEL_TOKEN else ""
  rows$CHANGE_TOKEN <- if ("CHANGE_TOKEN" %in% names(rows)) rows$CHANGE_TOKEN else rows$MODEL_TOKEN
  rows$CHANGE_GROUP <- if ("CHANGE_GROUP" %in% names(rows)) rows$CHANGE_GROUP else ""
  rows$CHANGE_SUMMARY <- if ("CHANGE_SUMMARY" %in% names(rows)) rows$CHANGE_SUMMARY else rows$JOB_DESCRIPTION
  rows$CHANGE_DETAIL <- if ("CHANGE_DETAIL" %in% names(rows)) rows$CHANGE_DETAIL else rows$CHANGE_SUMMARY
  rows$RECIPE_TOKEN <- if ("RECIPE_TOKEN" %in% names(rows)) rows$RECIPE_TOKEN else rows$CHANGE_TOKEN
  rows$RECIPE_FAMILY <- if ("RECIPE_FAMILY" %in% names(rows)) rows$RECIPE_FAMILY else rows$CHANGE_GROUP
  rows$RECIPE_LABEL <- if ("RECIPE_LABEL" %in% names(rows)) rows$RECIPE_LABEL else rows$RECIPE_TOKEN
  rows
}

# Convert a row's INPUT_TASK/INPUT_KEY columns into KflowKit input selectors.
# INPUT_KEY may contain one key or a comma-separated list of keys.
input_selector_for_row <- function(row) {
  task <- as.character(row[["INPUT_TASK"]] %||% "")
  key <- as.character(row[["INPUT_KEY"]] %||% "")
  if (!nzchar(task) || !nzchar(key)) {
    return(list())
  }
  keys <- trimws(unlist(strsplit(key, ",")))
  keys <- keys[nzchar(keys)]
  if (length(keys) > 1) {
    return(list(KflowKit::kflow_input_keys(task, keys)))
  }
  list(KflowKit::kflow_input_key(task, keys))
}

# ---- Row builders ----------------------------------------------------------------
#
# These are small helpers for interactive use. They let you create a new row
# without remembering every metadata column.

base_model <- function(job_key,
                       token,
                       name,
                       make_targets,
                       base_dir,
                       model_dir,
                       input_variant = token,
                       change_summary = name,
                       patch_script = "",
                       patch_input_dir = "",
                       patch_output_dir = base_dir,
                       collect_paths = paste(model_dir, base_dir, sep = ",")) {
  data.frame(
    RUN_LABEL = job_key,
    JOB_KEY = job_key,
    MODEL_KEY = job_key,
    MODEL_TOKEN = token,
    MODEL_NAME = name,
    BASE_MODEL_KEY = "",
    CHANGE_TOKEN = token,
    CHANGE_GROUP = "base",
    CHANGE_SUMMARY = change_summary,
    INPUT_VARIANT = input_variant,
    PATCH_SCRIPT = patch_script,
    PATCH_INPUT_DIR = patch_input_dir,
    PATCH_OUTPUT_DIR = patch_output_dir,
    JOB_TITLE = paste("Base:", token),
    JOB_DESCRIPTION = change_summary,
    MAKE_TARGETS = make_targets,
    BASE_DIR = base_dir,
    MODEL_DIR = model_dir,
    COLLECT_PATHS = collect_paths,
    stringsAsFactors = FALSE
  )
}

sensitivity_model <- function(job_key,
                              token,
                              name,
                              base_key = "base-4r-smoke",
                              make_targets,
                              base_dir,
                              model_dir,
                              input_variant = token,
                              change_group = "sensitivity",
                              change_summary = name,
                              patch_script = "",
                              patch_input_dir = "",
                              patch_output_dir = base_dir,
                              collect_paths = paste(model_dir, base_dir, sep = ",")) {
  data.frame(
    RUN_LABEL = job_key,
    JOB_KEY = job_key,
    MODEL_KEY = job_key,
    MODEL_TOKEN = token,
    MODEL_NAME = name,
    BASE_MODEL_KEY = base_key,
    CHANGE_TOKEN = token,
    CHANGE_GROUP = change_group,
    CHANGE_SUMMARY = change_summary,
    INPUT_VARIANT = input_variant,
    PATCH_SCRIPT = patch_script,
    PATCH_INPUT_DIR = patch_input_dir,
    PATCH_OUTPUT_DIR = patch_output_dir,
    JOB_TITLE = paste("Sensitivity:", token),
    JOB_DESCRIPTION = change_summary,
    INPUT_TASK = bet_task_codes[["base"]],
    INPUT_KEY = base_key,
    MAKE_TARGETS = make_targets,
    BASE_DIR = base_dir,
    MODEL_DIR = model_dir,
    COLLECT_PATHS = collect_paths,
    stringsAsFactors = FALSE
  )
}

# ---- Launch helpers --------------------------------------------------------------

launch_rows <- function(task_code, target_folder, rows, tags = list(stage = target_folder), ...) {
  bet_require_kflowkit()
  rows <- common_env(rows)
  lapply(seq_len(nrow(rows)), function(index) {
    row <- rows[index, , drop = FALSE]
    KflowKit::kflow_job_launch(
      report_code = task_code,
      config = row,
      repo = bet_kflow_repo,
      branch = bet_kflow_branch,
      target_folder = target_folder,
      docker_image = bet_docker_image,
      input_jobs = input_selector_for_row(row),
      tags = utils::modifyList(list(project = "bet-kflow"), tags),
      ...
    )
  })
}

launch_rows_batched <- function(task_code,
                                target_folder,
                                rows,
                                batch_size = Inf,
                                limit = Inf,
                                tags = list(stage = target_folder),
                                ...) {
  rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  if (is.finite(limit)) {
    rows <- utils::head(rows, limit)
  }
  if (!nrow(rows)) {
    return(list())
  }
  if (!is.finite(batch_size) || batch_size <= 0) {
    return(launch_rows(task_code, target_folder, rows, tags = tags, ...))
  }
  groups <- split(seq_len(nrow(rows)), ceiling(seq_len(nrow(rows)) / batch_size))
  lapply(groups, function(index) {
    launch_rows(task_code, target_folder, rows[index, , drop = FALSE], tags = tags, ...)
  })
}

register_tasks <- function(...) {
  bet_require_kflowkit()
  KflowKit::kflow_register_workflow(
    paths = c("base", "sensitivity", "diagnostics", "plot", "report"),
    codes = unname(bet_task_codes[c("base", "sensitivity", "diagnostics", "plot", "report")]),
    repo = bet_kflow_repo,
    branch = bet_kflow_branch,
    target_folders = c("base", "sensitivity", "diagnostics", "plot", "report"),
    checkout = "full",
    ...
  )
}

launch_base <- function(rows = base_models, ...) {
  launch_rows(bet_task_codes[["base"]], "base", rows, tags = list(stage = "base"), ...)
}

launch_sensitivity <- function(rows = sensitivity_models, ...) {
  launch_rows(bet_task_codes[["sensitivity"]], "sensitivity", rows, tags = list(stage = "sensitivity"), ...)
}

launch_diagnostics <- function(rows = diagnostics_runs, ...) {
  launch_rows(bet_task_codes[["diagnostics"]], "diagnostics", rows, tags = list(stage = "diagnostics"), ...)
}

launch_plot <- function(rows = plot_runs, ...) {
  launch_rows(bet_task_codes[["plot"]], "plot", rows, tags = list(stage = "plot"), ...)
}

launch_report <- function(rows = report_runs, ...) {
  launch_rows(bet_task_codes[["report"]], "report", rows, tags = list(stage = "report"), ...)
}

diagnostics_from <- function(input_task,
                             input_key,
                             job_key,
                             token = job_key,
                             title = paste("Diagnostics:", input_key),
                             make_targets = "jitter_smoke",
                             base_dir,
                             model_dir,
                             jitter_seed = 40) {
  data.frame(
    RUN_LABEL = job_key,
    JOB_KEY = job_key,
    MODEL_KEY = job_key,
    MODEL_TOKEN = token,
    MODEL_NAME = title,
    BASE_MODEL_KEY = if (identical(input_task, bet_task_codes[["base"]])) input_key else "",
    CHANGE_TOKEN = token,
    CHANGE_GROUP = "diagnostics",
    CHANGE_SUMMARY = paste("Runs diagnostics from", input_task, input_key),
    JOB_TITLE = title,
    JOB_DESCRIPTION = paste("Runs diagnostics from", input_task, input_key),
    INPUT_TASK = input_task,
    INPUT_KEY = input_key,
    MAKE_TARGETS = make_targets,
    BASE_DIR = base_dir,
    MODEL_DIR = model_dir,
    JITTER_SEED = jitter_seed,
    JITTER_SMOKE_ONLY = "1",
    COLLECT_PATHS = paste0(model_dir, "/jitter,", base_dir),
    stringsAsFactors = FALSE
  )
}

plot_from <- function(input_task, input_key, job_key, title = paste("Plot:", input_key)) {
  data.frame(
    RUN_LABEL = job_key,
    JOB_KEY = job_key,
    JOB_TITLE = title,
    JOB_DESCRIPTION = paste("Creates plots from", input_task, input_key),
    INPUT_TASK = input_task,
    INPUT_KEY = input_key,
    PLOT_TITLE = title,
    PLOT_BACKEND = "manifest",
    stringsAsFactors = FALSE
  )
}

report_from <- function(input_task, input_key, job_key, title = paste("Report:", input_key)) {
  data.frame(
    RUN_LABEL = job_key,
    JOB_KEY = job_key,
    JOB_TITLE = title,
    JOB_DESCRIPTION = paste("Renders a Quarto report from", input_task, input_key),
    INPUT_TASK = input_task,
    INPUT_KEY = input_key,
    REPORT_TITLE = title,
    stringsAsFactors = FALSE
  )
}

launch_example_flow <- function(...) {
  list(
    base = launch_base(...),
    sensitivity = launch_sensitivity(...),
    diagnostics = launch_diagnostics(...),
    plot = launch_plot(...),
    report = launch_report(...)
  )
}

launch_stage <- function(stage, rows, batch_size = Inf, limit = Inf, ...) {
  stage <- match.arg(stage, c("base", "sensitivity", "diagnostics", "plot", "report"))
  switch(
    stage,
    base = launch_rows_batched(bet_task_codes[["base"]], "base", rows, batch_size = batch_size, limit = limit, tags = list(stage = "base"), ...),
    sensitivity = launch_rows_batched(bet_task_codes[["sensitivity"]], "sensitivity", rows, batch_size = batch_size, limit = limit, tags = list(stage = "sensitivity"), ...),
    diagnostics = launch_rows_batched(bet_task_codes[["diagnostics"]], "diagnostics", rows, batch_size = batch_size, limit = limit, tags = list(stage = "diagnostics"), ...),
    plot = launch_rows_batched(bet_task_codes[["plot"]], "plot", rows, batch_size = batch_size, limit = limit, tags = list(stage = "plot"), ...),
    report = launch_rows_batched(bet_task_codes[["report"]], "report", rows, batch_size = batch_size, limit = limit, tags = list(stage = "report"), ...)
  )
}

launch_plan <- function(plan,
                        stages = names(plan),
                        batch_size = Inf,
                        limit = Inf,
                        ...) {
  out <- list()
  for (stage in stages) {
    rows <- plan[[stage]]
    if (is.null(rows)) {
      warning("Plan has no stage named: ", stage)
      next
    }
    out[[stage]] <- launch_stage(stage, rows, batch_size = batch_size, limit = limit, ...)
  }
  out
}
