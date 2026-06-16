# BET Kflow launch layer.
#
# This is the main file to edit day to day.
# - Edit the tables near the top to add/remove base models, sensitivities,
#   diagnostics, plot jobs, or reports.
# - Use the launch_*() functions near the bottom to submit jobs to Kflow.
# - Keep heavy job internals in R/job_helpers.R and large combinatorics in
#   R/plan.R so this file stays readable.

library(KflowKit)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# ---- Project defaults ------------------------------------------------------------
#
# These defaults are applied to every launched job. Override them with
# Sys.setenv(...) before sourcing this file, or add matching columns to any row
# in the tables below.

bet_kflow_repo <- Sys.getenv("BET_KFLOW_REPO", "PacificCommunity/ofp-sam-2026-BET-flow")
bet_kflow_branch <- Sys.getenv("BET_KFLOW_BRANCH", "main")
bet_source_repo <- Sys.getenv("BET_SOURCE_REPO", "PacificCommunity/ofp-sam-2026-BET")
bet_source_ref <- Sys.getenv("BET_SOURCE_REF", "main")
bet_docker_image <- Sys.getenv("BET_DOCKER_IMAGE", "ghcr.io/pacificcommunity/bet2026-flow:latest")
bet_flow_group <- paste0("bet-", format(Sys.time(), "%Y%m%d-%H%M%S"))

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
  RUN_LABEL = "base-4region",
  JOB_KEY = "base-4region",
  MODEL_KEY = "base-4region",
  MODEL_TOKEN = "Base4R",
  MODEL_NAME = "2023 4-region representative base",
  BASE_MODEL_KEY = "",
  CHANGE_TOKEN = "Base",
  CHANGE_GROUP = "base",
  CHANGE_SUMMARY = "9-to-4 representative BET base input collapse and MFCL run.",
  INPUT_VARIANT = "2023_4region",
  PATCH_SCRIPT = "",
  PATCH_INPUT_DIR = "mfcl/inputs/2023_rep",
  PATCH_OUTPUT_DIR = "mfcl/inputs/2023_4region",
  JOB_TITLE = "Base: 4-region representative",
  JOB_DESCRIPTION = "Runs the 2023 4-region representative BET base model with the MFCL executable.",
  MAKE_TARGETS = "run-4region",
  BASE_DIR = "mfcl/inputs/2023_4region",
  MODEL_DIR = "model/2023R4",
  COLLECT_PATHS = "model/2023R4,mfcl/inputs/2023_4region",
  stringsAsFactors = FALSE
)

sensitivity_models <- data.frame(
  RUN_LABEL = c("sens-fixM", "sens-fixVB-M"),
  JOB_KEY = c("sens-fixM", "sens-fixVB-M"),
  MODEL_KEY = c("sens-fixM", "sens-fixVB-M"),
  MODEL_TOKEN = c("FixM", "FixVBM"),
  MODEL_NAME = c("Sensitivity FixM", "Sensitivity FixVB_M"),
  BASE_MODEL_KEY = "base-4region",
  CHANGE_TOKEN = c("FixM", "FixVBM"),
  CHANGE_GROUP = c("movement", "growth"),
  CHANGE_SUMMARY = c(
    "Uses the fixM input variant from the BET source repository.",
    "Uses the fixVB_M input variant from the BET source repository."
  ),
  INPUT_VARIANT = c("2023_4region_fixM", "2023_4region_fixVB_M"),
  PATCH_SCRIPT = "",
  PATCH_INPUT_DIR = c("mfcl/inputs/2023_fixM", "mfcl/inputs/2023_fixVB_M"),
  PATCH_OUTPUT_DIR = c("mfcl/inputs/2023_4region_fixM", "mfcl/inputs/2023_4region_fixVB_M"),
  JOB_TITLE = c("Sensitivity: fixM", "Sensitivity: fixVB_M"),
  JOB_DESCRIPTION = c(
    "Runs the fixM sensitivity from the selected base model.",
    "Runs the fixVB_M sensitivity from the selected base model."
  ),
  INPUT_TASK = "Base",
  INPUT_KEY = "base-4region",
  MAKE_TARGETS = c("run-4region-fixM", "run-4region-fixVB_M"),
  BASE_DIR = c("mfcl/inputs/2023_4region_fixM", "mfcl/inputs/2023_4region_fixVB_M"),
  MODEL_DIR = c("model/2023R4_fixM", "model/2023R4_fixVB_M"),
  COLLECT_PATHS = c(
    "model/2023R4_fixM,mfcl/inputs/2023_4region_fixM",
    "model/2023R4_fixVB_M,mfcl/inputs/2023_4region_fixVB_M"
  ),
  stringsAsFactors = FALSE
)

diagnostics_runs <- data.frame(
  RUN_LABEL = c("diag-from-base", "diag-from-fixM"),
  JOB_KEY = c("diag-from-base", "diag-from-fixM"),
  MODEL_KEY = c("diag-from-base", "diag-from-fixM"),
  MODEL_TOKEN = c("DiagBase", "DiagFixM"),
  MODEL_NAME = c("Diagnostics from base", "Diagnostics from FixM"),
  BASE_MODEL_KEY = c("base-4region", "base-4region"),
  CHANGE_TOKEN = c("JitterSmoke", "JitterSmoke"),
  CHANGE_GROUP = "diagnostics",
  CHANGE_SUMMARY = c(
    "Runs a smoke jitter diagnostics job directly from the base model.",
    "Runs a smoke jitter diagnostics job from the FixM sensitivity."
  ),
  INPUT_VARIANT = c("2023_4region", "2023_4region_fixM"),
  PATCH_SCRIPT = "",
  PATCH_INPUT_DIR = c("mfcl/inputs/2023_4region", "mfcl/inputs/2023_4region_fixM"),
  PATCH_OUTPUT_DIR = c("mfcl/inputs/2023_4region", "mfcl/inputs/2023_4region_fixM"),
  JOB_TITLE = c("Diagnostics: base smoke jitter", "Diagnostics: fixM smoke jitter"),
  JOB_DESCRIPTION = c(
    "Runs a light diagnostics pass directly from the base model.",
    "Runs a light diagnostics pass from the fixM sensitivity model."
  ),
  INPUT_TASK = c("Base", "Sensitivity"),
  INPUT_KEY = c("base-4region", "sens-fixM"),
  MAKE_TARGETS = "jitter_smoke",
  BASE_DIR = c("mfcl/inputs/2023_4region", "mfcl/inputs/2023_4region_fixM"),
  MODEL_DIR = c("model/2023R4", "model/2023R4_fixM"),
  JITTER_SEED = c(40, 41),
  JITTER_SMOKE_ONLY = "1",
  COLLECT_PATHS = c(
    "model/2023R4/jitter,mfcl/inputs/2023_4region",
    "model/2023R4_fixM/jitter,mfcl/inputs/2023_4region_fixM"
  ),
  stringsAsFactors = FALSE
)

plot_runs <- data.frame(
  RUN_LABEL = "plot-diagnostics",
  JOB_KEY = "plot-diagnostics",
  MODEL_KEY = "plot-diagnostics",
  MODEL_TOKEN = "PlotDiagnostics",
  MODEL_NAME = "Diagnostics plot package",
  BASE_MODEL_KEY = "base-4region",
  CHANGE_TOKEN = "Plot",
  CHANGE_GROUP = "plot",
  CHANGE_SUMMARY = "Collects selected diagnostics outputs into a plotting package.",
  JOB_TITLE = "Plot: diagnostics overview",
  JOB_DESCRIPTION = "Creates a lightweight plot package from selected diagnostics outputs.",
  INPUT_TASK = "Diagnostics",
  INPUT_KEY = "diag-from-base",
  PLOT_TITLE = "BET diagnostics overview",
  PLOT_BACKEND = "manifest",
  stringsAsFactors = FALSE
)

report_runs <- data.frame(
  RUN_LABEL = "report-bet",
  JOB_KEY = "report-bet",
  MODEL_KEY = "report-bet",
  MODEL_TOKEN = "ReportBET",
  MODEL_NAME = "BET Kflow Quarto report",
  BASE_MODEL_KEY = "base-4region",
  CHANGE_TOKEN = "Report",
  CHANGE_GROUP = "report",
  CHANGE_SUMMARY = "Renders the selected plot outputs into a Quarto HTML report.",
  JOB_TITLE = "Report: BET Kflow summary",
  JOB_DESCRIPTION = "Renders a Quarto report from selected plot outputs.",
  INPUT_TASK = "Plot",
  INPUT_KEY = "plot-diagnostics",
  REPORT_TITLE = "BET 2026 Kflow report",
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
  rows$MFCL_BACKEND <- if ("MFCL_BACKEND" %in% names(rows)) rows$MFCL_BACKEND else "mfcl_exe"
  rows$BACKEND_SCRIPT <- if ("BACKEND_SCRIPT" %in% names(rows)) rows$BACKEND_SCRIPT else ""
  rows$BACKEND_COMMAND <- if ("BACKEND_COMMAND" %in% names(rows)) rows$BACKEND_COMMAND else ""
  rows$MFCLKIT_SCRIPT <- if ("MFCLKIT_SCRIPT" %in% names(rows)) rows$MFCLKIT_SCRIPT else ""
  rows$MFCLSHINY_SCRIPT <- if ("MFCLSHINY_SCRIPT" %in% names(rows)) rows$MFCLSHINY_SCRIPT else ""
  rows$PROGRAM_PATH <- if ("PROGRAM_PATH" %in% names(rows)) rows$PROGRAM_PATH else "mfcl/exe/mfclo64_2026"
  rows$FLOW_GROUP <- if ("FLOW_GROUP" %in% names(rows)) rows$FLOW_GROUP else bet_flow_group
  rows$MODEL_KEY <- if ("MODEL_KEY" %in% names(rows)) rows$MODEL_KEY else rows$JOB_KEY
  rows$MODEL_TOKEN <- if ("MODEL_TOKEN" %in% names(rows)) rows$MODEL_TOKEN else rows$RUN_LABEL
  rows$BASE_MODEL_KEY <- if ("BASE_MODEL_KEY" %in% names(rows)) rows$BASE_MODEL_KEY else ""
  rows$CHANGE_TOKEN <- if ("CHANGE_TOKEN" %in% names(rows)) rows$CHANGE_TOKEN else rows$MODEL_TOKEN
  rows$CHANGE_GROUP <- if ("CHANGE_GROUP" %in% names(rows)) rows$CHANGE_GROUP else ""
  rows$CHANGE_SUMMARY <- if ("CHANGE_SUMMARY" %in% names(rows)) rows$CHANGE_SUMMARY else rows$JOB_DESCRIPTION
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
    return(list(kflow_input_keys(task, keys)))
  }
  list(kflow_input_key(task, keys))
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
                              base_key = "base-4region",
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
    INPUT_TASK = "Base",
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
  rows <- common_env(rows)
  lapply(seq_len(nrow(rows)), function(index) {
    row <- rows[index, , drop = FALSE]
    kflow_job_launch(
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
  kflow_register_workflow(
    paths = c("base", "sensitivity", "diagnostics", "plot", "report"),
    codes = c("Base", "Sensitivity", "Diagnostics", "Plot", "Report"),
    repo = bet_kflow_repo,
    branch = bet_kflow_branch,
    target_folders = c("base", "sensitivity", "diagnostics", "plot", "report"),
    checkout = "full",
    ...
  )
}

launch_base <- function(rows = base_models, ...) {
  launch_rows("Base", "base", rows, tags = list(stage = "base"), ...)
}

launch_sensitivity <- function(rows = sensitivity_models, ...) {
  launch_rows("Sensitivity", "sensitivity", rows, tags = list(stage = "sensitivity"), ...)
}

launch_diagnostics <- function(rows = diagnostics_runs, ...) {
  launch_rows("Diagnostics", "diagnostics", rows, tags = list(stage = "diagnostics"), ...)
}

launch_plot <- function(rows = plot_runs, ...) {
  launch_rows("Plot", "plot", rows, tags = list(stage = "plot"), ...)
}

launch_report <- function(rows = report_runs, ...) {
  launch_rows("Report", "report", rows, tags = list(stage = "report"), ...)
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
    BASE_MODEL_KEY = if (identical(input_task, "Base")) input_key else "",
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
    base = launch_rows_batched("Base", "base", rows, batch_size = batch_size, limit = limit, tags = list(stage = "base"), ...),
    sensitivity = launch_rows_batched("Sensitivity", "sensitivity", rows, batch_size = batch_size, limit = limit, tags = list(stage = "sensitivity"), ...),
    diagnostics = launch_rows_batched("Diagnostics", "diagnostics", rows, batch_size = batch_size, limit = limit, tags = list(stage = "diagnostics"), ...),
    plot = launch_rows_batched("Plot", "plot", rows, batch_size = batch_size, limit = limit, tags = list(stage = "plot"), ...),
    report = launch_rows_batched("Report", "report", rows, batch_size = batch_size, limit = limit, tags = list(stage = "report"), ...)
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
