# Tuna Kflow launch layer.
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

flow_nonempty <- function(x, y = "") {
  if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(as.character(x[[1]]))) y else as.character(x[[1]])
}

flow_require_kflowkit <- function() {
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

flow_env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

flow_env_any <- function(names, default = "") {
  for (name in names) {
    value <- flow_env(name, "")
    if (nzchar(value)) {
      return(value)
    }
  }
  default
}

flow_split_csv <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) {
    return(character())
  }
  parts <- unlist(strsplit(as.character(value[[1]]), "[,\n\r]+"))
  trimws(parts[nzchar(trimws(parts))])
}

flow_slug <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "-", as.character(x)))
  gsub("^-+|-+$", "", x)
}

flow_title_case <- function(x) {
  x <- gsub("[_-]+", " ", x)
  words <- strsplit(tolower(x), "[[:space:]]+")[[1]]
  words <- words[nzchar(words)]
  paste(paste0(toupper(substr(words, 1, 1)), substr(words, 2, nchar(words))), collapse = " ")
}

flow_species <- toupper(flow_env_any(c("FLOW_SPECIES", "TUNA_FLOW_SPECIES"), "BET"))
flow_assessment_year <- flow_env_any(c("FLOW_ASSESSMENT_YEAR", "TUNA_FLOW_ASSESSMENT_YEAR"), "2026")
flow_species_label <- flow_env_any(
  c("FLOW_SPECIES_LABEL", "TUNA_FLOW_SPECIES_LABEL"),
  switch(flow_species, BET = "Bigeye tuna", YFT = "Yellowfin tuna", SKJ = "Skipjack tuna", ALB = "Albacore tuna", flow_title_case(flow_species))
)
flow_assessment_label <- trimws(paste(flow_species, flow_assessment_year))
flow_species_slug <- tolower(gsub("[^A-Za-z0-9]+", "-", flow_species))
flow_task_prefix <- flow_env_any(
  c("FLOW_TASK_PREFIX", "TUNA_FLOW_TASK_PREFIX"),
  paste(flow_species_slug, flow_assessment_year, sep = "-")
)
flow_project_tag <- flow_env_any(c("FLOW_PROJECT_TAG", "TUNA_FLOW_PROJECT_TAG"), "tuna-flow")
flow_flow_group <- paste(flow_project_tag, flow_species_slug, format(Sys.time(), "%Y%m%d-%H%M%S"), sep = "-")
flow_kflow_repo <- flow_env_any(c("FLOW_KFLOW_REPO", "TUNA_FLOW_REPO"), "PacificCommunity/ofp-sam-tuna-flow")
flow_kflow_branch <- flow_env_any(c("FLOW_KFLOW_BRANCH", "TUNA_FLOW_BRANCH"), "main")
flow_source_repo <- flow_env_any(c("FLOW_SOURCE_REPO", "TUNA_FLOW_SOURCE_REPO"), "PacificCommunity/ofp-sam-bet2026-inputs")
flow_source_ref <- flow_env_any(c("FLOW_SOURCE_REF", "TUNA_FLOW_SOURCE_REF"), "main")
flow_source_path <- flow_env_any(c("FLOW_SOURCE_PATH", "TUNA_FLOW_SOURCE_PATH", "SOURCE_PATH"), "")
flow_report_repo <- flow_env_any(c("FLOW_REPORT_REPO", "TUNA_FLOW_REPORT_REPO"), "PacificCommunity/ofp-sam-2026-BET-report")
flow_report_ref <- flow_env_any(c("FLOW_REPORT_REF", "TUNA_FLOW_REPORT_REF"), "main")
flow_report_path <- flow_env_any(c("FLOW_REPORT_PATH", "TUNA_FLOW_REPORT_PATH"), "report")
flow_report_main <- flow_env_any(c("FLOW_REPORT_MAIN", "TUNA_FLOW_REPORT_MAIN"), "bet-2026.qmd")
flow_docker_image <- flow_env_any(
  c("FLOW_DOCKER_IMAGE", "TUNA_FLOW_DOCKER_IMAGE"),
  "ghcr.io/pacificcommunity/tuna-flow:latest"
)
flow_task_codes <- setNames(paste(flow_task_prefix, c("base", "sensitivity", "diagnostics", "plot", "report"), sep = "-"), c(
  "base", "sensitivity", "diagnostics", "plot", "report"
))
flow_task_codes[["report"]] <- flow_env_any(
  c("FLOW_REPORT_TASK_CODE", "TUNA_FLOW_REPORT_TASK_CODE"),
  paste(flow_task_prefix, "report", sep = "-")
)
flow_default_program <- flow_env_any(c("FLOW_MFCL_PROGRAM", "TUNA_FLOW_MFCL_PROGRAM"), "/home/mfcl/mfclo64")
flow_default_input_dir <- flow_env_any(c("FLOW_BASE_INPUT_DIR", "TUNA_FLOW_BASE_INPUT_DIR"), "mfcl/inputs/2023_4region_1007")
flow_base_input_dirs <- flow_split_csv(flow_env_any(c("FLOW_BASE_INPUT_DIRS", "TUNA_FLOW_BASE_INPUT_DIRS"), flow_default_input_dir))
if (!length(flow_base_input_dirs)) {
  flow_base_input_dirs <- flow_default_input_dir
}
flow_default_input_variant <- flow_env_any(c("FLOW_BASE_INPUT_VARIANT", "TUNA_FLOW_BASE_INPUT_VARIANT"), basename(flow_base_input_dirs[[1]]))
flow_base_job_key <- flow_env_any(c("FLOW_BASE_JOB_KEY", "TUNA_FLOW_BASE_JOB_KEY"), "base-4r-smoke")
flow_base_token <- flow_env_any(c("FLOW_BASE_TOKEN", "TUNA_FLOW_BASE_TOKEN"), "Base4R")
flow_report_file_stem <- flow_env_any(c("FLOW_REPORT_FILE_STEM", "TUNA_FLOW_REPORT_FILE_STEM"), paste(flow_task_prefix, "report", sep = "-"))
flow_ini_version_target <- flow_env_any(c("FLOW_MFCL_INI_VERSION_TARGET", "MFCL_INI_VERSION_TARGET"), "1007")
flow_base_tokens <- flow_split_csv(flow_env_any(c("FLOW_BASE_TOKENS", "TUNA_FLOW_BASE_TOKENS"), ""))
flow_base_job_keys <- flow_split_csv(flow_env_any(c("FLOW_BASE_JOB_KEYS", "TUNA_FLOW_BASE_JOB_KEYS"), ""))
flow_base_names <- flow_split_csv(flow_env_any(c("FLOW_BASE_NAMES", "TUNA_FLOW_BASE_NAMES"), ""))
flow_starter_sensitivity_tokens <- flow_split_csv(flow_env_any(
  c("FLOW_STARTER_SENSITIVITIES", "TUNA_FLOW_STARTER_SENSITIVITIES"),
  "FixM,FixVB,Sel4,IndexCvHalf"
))

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

flow_base_token_for <- function(index, base_dir) {
  if (length(flow_base_tokens) >= index && nzchar(flow_base_tokens[[index]])) {
    return(flow_base_tokens[[index]])
  }
  if (length(flow_base_input_dirs) == 1L) {
    return(flow_base_token)
  }
  paste0("Base", gsub("[^A-Za-z0-9]+", "", flow_title_case(basename(base_dir))))
}

flow_base_job_key_for <- function(index, token) {
  if (length(flow_base_job_keys) >= index && nzchar(flow_base_job_keys[[index]])) {
    return(flow_base_job_keys[[index]])
  }
  if (length(flow_base_input_dirs) == 1L) {
    return(flow_base_job_key)
  }
  paste("base", flow_slug(token), sep = "-")
}

build_base_model_rows <- function(base_dirs = flow_base_input_dirs) {
  rows <- lapply(seq_along(base_dirs), function(index) {
    base_dir <- base_dirs[[index]]
    input_variant <- basename(base_dir)
    token <- flow_base_token_for(index, base_dir)
    job_key <- flow_base_job_key_for(index, token)
    model_name <- if (length(flow_base_names) >= index && nzchar(flow_base_names[[index]])) {
      flow_base_names[[index]]
    } else {
      paste(flow_assessment_label, input_variant, "base smoke")
    }
    model_dir <- file.path("model", job_key)
    data.frame(
      RUN_LABEL = job_key,
      JOB_KEY = job_key,
      MODEL_KEY = job_key,
      MODEL_TOKEN = token,
      MODEL_NAME = model_name,
      BASE_MODEL_KEY = "",
      CHANGE_TOKEN = token,
      CHANGE_GROUP = "base",
      CHANGE_SUMMARY = paste("Runs", input_variant, flow_species_label, "MFCL input through a fast last-par smoke check."),
      INPUT_VARIANT = input_variant,
      SOURCE_REPO = flow_source_repo,
      SOURCE_REF = flow_source_ref,
      SOURCE_PATH = flow_source_path,
      USE_FLOW_SOURCE = "0",
      MFCL_BACKEND = "mfcl_smoke",
      PROGRAM_PATH = flow_default_program,
      PATCH_SCRIPT = "scripts/run-mfcl-input-recipe.R",
      PATCH_INPUT_DIR = base_dir,
      PATCH_OUTPUT_DIR = base_dir,
      INPUT_RECIPE_TO_INI_VERSION = flow_ini_version_target,
      INPUT_RECIPE_NORMALIZE_INI = "1",
      INPUT_RECIPE_VALIDATE_INI = "1",
      INPUT_RECIPE_PREFER_MFCLKIT = "1",
      RECIPE_TOKEN = token,
      RECIPE_FAMILY = "base",
      RECIPE_LABEL = "Base input",
      JOB_TITLE = paste(flow_assessment_label, "base:", input_variant, "smoke"),
      JOB_DESCRIPTION = paste("Normalizes the included", input_variant, flow_species_label, "input to MFCL .ini", flow_ini_version_target, "and runs a fast MFCL last-par smoke check."),
      MAKE_TARGETS = "mfcl-smoke",
      BASE_DIR = base_dir,
      MODEL_DIR = model_dir,
      COLLECT_PATHS = paste(c(model_dir, base_dir, if (!grepl("^/", flow_default_program)) flow_default_program else character()), collapse = ","),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

mfcl_input_recipe_library <- data.frame(
  RECIPE_TOKEN = c("FixM", "FixVB", "FixMFixVB", "Sel4", "IndexCvHalf", "TagMix2", "TagRepHalf", "MHalf"),
  RECIPE_KEY = c("fixM", "fixVB", "fixMFixVB", "sel4", "indexCvHalf", "tagMix2", "tagRepHalf", "mHalf"),
  RECIPE_FAMILY = c("natural-mortality", "growth", "natural-mortality-growth", "selectivity", "index-cv", "tagging", "tagging", "natural-mortality"),
  RECIPE_LABEL = c("Fixed M", "Fixed growth", "Fixed M and growth", "Selectivity spline 4", "Index CV half", "Tag mixing period 2", "Tag reporting half", "Natural mortality half"),
  INPUT_VARIANT_SUFFIX = c("fixM", "fixVB", "fixM_fixVB", "sel4", "index_cv_half", "tag_mix2", "tag_rep_half", "m_half"),
  PATCH_SCRIPT = "scripts/run-mfcl-input-recipe.R",
  CHANGE_DETAIL = c(
    "Sets MFCL estimation switch 121 to zero in doitall.sh while preserving the start .par for smoke evaluation.",
    "Sets MFCL estimation switches 12, 13, and 14 to zero in doitall.sh while preserving the start .par for smoke evaluation.",
    "Sets MFCL estimation switches 121, 12, 13, and 14 to zero in doitall.sh while preserving the start .par for smoke evaluation.",
    "Sets fish flag 61 selectivity spline nodes to 4 in doitall.sh.",
    "Halves negative-fishery fish flag 92 index CV values in doitall.sh.",
    "Converts the .ini to version 1007 if needed and sets tag mixing period flag 1 to 2.",
    "Converts the .ini to version 1007 if needed and multiplies tag reporting rates by 0.5.",
    "Converts the .ini to version 1007 if needed and multiplies the natural mortality section by 0.5."
  ),
  INPUT_RECIPE_FIXED_PARAMS = c("M", "VB", "M,VB", "", "", "", "", ""),
  INPUT_RECIPE_INDEX_CV_HALF = c("0", "0", "0", "0", "1", "0", "0", "0"),
  INPUT_RECIPE_INDEX_FISHERIES = "",
  INPUT_RECIPE_SEL_NODES = c("", "", "", "4", "", "", "", ""),
  INPUT_RECIPE_SEL_FISHERIES = "",
  INPUT_RECIPE_TAG_MIXING_PERIODS = c("", "", "", "", "", "2", "", ""),
  INPUT_RECIPE_TAG_MIXING_GROUPS = "",
  INPUT_RECIPE_TAG_REPORTING_MULTIPLIER = c("", "", "", "", "", "", "0.5", ""),
  INPUT_RECIPE_TAG_REPORTING_GROUPS = "",
  INPUT_RECIPE_TAG_REPORTING_FISHERIES = "",
  INPUT_RECIPE_INI_SECTION = c("", "", "", "", "", "", "", "natural mortality"),
  INPUT_RECIPE_INI_SECTION_MULTIPLIER = c("", "", "", "", "", "", "", "0.5"),
  INPUT_RECIPE_INI_SECTION_VALUE = "",
  INPUT_RECIPE_INI_SECTION_ROWS = "",
  INPUT_RECIPE_INI_SECTION_COLS = "",
  INPUT_RECIPE_INI_SECTION_EXACT = c("0", "0", "0", "0", "0", "0", "0", "0"),
  INPUT_RECIPE_TO_INI_VERSION = flow_ini_version_target,
  INPUT_RECIPE_NORMALIZE_INI = "1",
  INPUT_RECIPE_VALIDATE_INI = "1",
  INPUT_RECIPE_PREFER_MFCLKIT = "1",
  stringsAsFactors = FALSE
)

base_models <- build_base_model_rows()

starter_sensitivity_recipes <- mfcl_input_recipe_library[
  mfcl_input_recipe_library$RECIPE_TOKEN %in% flow_starter_sensitivity_tokens,
  ,
  drop = FALSE
]

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
  if (!nrow(bases) || !nrow(recipes)) {
    return(data.frame())
  }
  rows <- list()
  for (base_index in seq_len(nrow(bases))) {
    base <- bases[base_index, , drop = FALSE]
    for (recipe_index in seq_len(nrow(recipes))) {
      recipe <- recipes[recipe_index, , drop = FALSE]
      job_key <- paste("sens", flow_slug(base$MODEL_TOKEN), recipe$RECIPE_KEY, sep = "-")
      if (nrow(bases) == 1L) {
        job_key <- paste("sens", recipe$RECIPE_KEY, sep = "-")
      }
      input_variant <- paste(base$INPUT_VARIANT, recipe$INPUT_VARIANT_SUFFIX, sep = "_")
      base_dir <- file.path(dirname(base$BASE_DIR), input_variant)
      model_dir <- file.path("model", job_key)
      model_token <- if (nrow(bases) == 1L) recipe$RECIPE_TOKEN else paste(base$MODEL_TOKEN, recipe$RECIPE_TOKEN, sep = "_")
      row <- data.frame(
        RUN_LABEL = job_key,
        JOB_KEY = job_key,
        MODEL_KEY = job_key,
        MODEL_TOKEN = model_token,
        MODEL_NAME = paste(flow_assessment_label, base$MODEL_TOKEN, recipe$RECIPE_LABEL, "sensitivity smoke"),
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
        CHANGE_SUMMARY = paste(recipe$CHANGE_DETAIL, "Runs a fast last-par smoke check."),
        INPUT_VARIANT = input_variant,
        INPUT_TASK = flow_task_codes[["base"]],
        INPUT_KEY = base$JOB_KEY,
        SOURCE_REPO = base$SOURCE_REPO %||% flow_source_repo,
        SOURCE_REF = base$SOURCE_REF %||% flow_source_ref,
        SOURCE_PATH = base$SOURCE_PATH %||% "",
        USE_FLOW_SOURCE = base$USE_FLOW_SOURCE %||% "0",
        MFCL_BACKEND = base$MFCL_BACKEND %||% "mfcl_smoke",
        PROGRAM_PATH = base$PROGRAM_PATH %||% flow_default_program,
        MAKE_TARGETS = "mfcl-smoke",
        BASE_DIR = base_dir,
        MODEL_DIR = model_dir,
        PATCH_SCRIPT = recipe$PATCH_SCRIPT,
        PATCH_INPUT_DIR = base$BASE_DIR,
        PATCH_OUTPUT_DIR = base_dir,
        JOB_TITLE = paste("Sensitivity:", recipe$RECIPE_TOKEN),
        JOB_DESCRIPTION = paste("Builds", recipe$RECIPE_LABEL, "from", base$MODEL_TOKEN, "input settings and runs a fast MFCL last-par smoke check."),
        COLLECT_PATHS = paste(model_dir, base_dir, sep = ","),
        stringsAsFactors = FALSE
      )
      recipe_env_cols <- grep("^INPUT_RECIPE_", names(recipe), value = TRUE)
      for (name in recipe_env_cols) {
        row[[name]] <- as.character(recipe[[name]])
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  do.call(rbind, rows)
}

build_diagnostics_rows <- function(parent_rows, recipes, input_task) {
  if (!nrow(parent_rows) || !nrow(recipes)) {
    return(data.frame())
  }
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
        MODEL_NAME = paste(flow_assessment_label, parent$MODEL_TOKEN, recipe$RECIPE_LABEL, "diagnostics smoke"),
        MODEL_LABEL = paste(parent$MODEL_TOKEN, recipe$RECIPE_TOKEN, sep = " | "),
        PLOT_LABEL = paste(parent$MODEL_TOKEN, recipe$RECIPE_TOKEN, sep = " + "),
        REPORT_LABEL = paste(parent$MODEL_TOKEN, recipe$RECIPE_LABEL, sep = " - "),
        BASE_MODEL_KEY = flow_nonempty(parent$BASE_MODEL_KEY, parent$JOB_KEY),
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
        SOURCE_REPO = parent$SOURCE_REPO %||% flow_source_repo,
        SOURCE_REF = parent$SOURCE_REF %||% flow_source_ref,
        SOURCE_PATH = parent$SOURCE_PATH %||% "",
        USE_FLOW_SOURCE = parent$USE_FLOW_SOURCE %||% "0",
        MFCL_BACKEND = "diagnostics_smoke",
        PROGRAM_PATH = parent$PROGRAM_PATH %||% flow_default_program,
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
  build_diagnostics_rows(base_models, starter_diagnostics_recipes, flow_task_codes[["base"]]),
  build_diagnostics_rows(sensitivity_models, starter_diagnostics_recipes, flow_task_codes[["sensitivity"]])
)

plot_runs <- data.frame(
  RUN_LABEL = "plot-depletion-smoke",
  JOB_KEY = "plot-depletion-smoke",
  MODEL_KEY = "plot-depletion-smoke",
  MODEL_TOKEN = "PlotDepletionSmoke",
  MODEL_NAME = paste(flow_assessment_label, "depletion smoke plot package"),
  BASE_MODEL_KEY = flow_base_job_key,
  CHANGE_TOKEN = "Plot",
  CHANGE_GROUP = "plot",
  CHANGE_SUMMARY = "Collects selected smoke outputs into a simple depletion plot package.",
  JOB_TITLE = "Plot: depletion smoke",
  JOB_DESCRIPTION = "Creates a simple depletion plot from selected model and diagnostics outputs.",
  INPUT_TASK = flow_task_codes[["diagnostics"]],
  INPUT_KEY = paste(diagnostics_runs$JOB_KEY, collapse = ","),
  PLOT_TITLE = paste(flow_assessment_label, "depletion smoke check"),
  PLOT_BACKEND = "mfclshiny",
  MFCLSHINY_SCRIPT = "hooks/depletion_smoke.R",
  MODEL_LABEL = "Depletion smoke",
  PLOT_LABEL = "Depletion",
  REPORT_LABEL = "Depletion smoke plots",
  stringsAsFactors = FALSE
)

report_runs <- data.frame(
  RUN_LABEL = "report-depletion-smoke",
  JOB_KEY = "report-depletion-smoke",
  MODEL_KEY = "report-depletion-smoke",
  MODEL_TOKEN = "ReportDepletionSmoke",
  MODEL_NAME = paste(flow_assessment_label, "depletion smoke Quarto report"),
  BASE_MODEL_KEY = flow_base_job_key,
  CHANGE_TOKEN = "Report",
  CHANGE_GROUP = "report",
  CHANGE_SUMMARY = "Renders the selected depletion smoke plot into a Quarto PDF report.",
  JOB_TITLE = paste("Report:", flow_species, "depletion smoke"),
  JOB_DESCRIPTION = "Renders a Quarto report from the selected depletion smoke plot.",
  INPUT_TASK = flow_task_codes[["plot"]],
  INPUT_KEY = "plot-depletion-smoke",
  REPORT_TITLE = paste(flow_assessment_label, "Kflow depletion smoke report"),
  REPORT_FILE_STEM = flow_report_file_stem,
  REPORT_SOURCE_REPO = flow_report_repo,
  REPORT_SOURCE_REF = flow_report_ref,
  REPORT_SOURCE_PATH = flow_report_path,
  REPORT_TEMPLATE_MAIN = flow_report_main,
  REPORT_FIGURE_DIR = "Figures/kflow",
  REPORT_RENDER_FORMAT = "pdf",
  REPORT_REQUIRE_PLOTS = "true",
  REPORT_REWRITE_FIGURES = "true",
  REPORT_REWRITE_TABLES = "true",
  MODEL_LABEL = "Depletion smoke report",
  PLOT_LABEL = "Report",
  REPORT_LABEL = "Depletion smoke report",
  stringsAsFactors = FALSE
)

# ---- Runtime package defaults ----------------------------------------------------

runtime_package_specs <- function(backend, stage = "", plot_backend = "", mfclshiny_script = "") {
  specs <- c(
    mfclrtmb = "mfclrtmb=PacificCommunity/ofp-sam-mfclrtmb@main",
    mfclkit = "mfclkit=PacificCommunity/ofp-sam-mfclkit@main",
    mfclshiny = "mfclshiny=PacificCommunity/mfclshiny@main"
  )
  backend <- tolower(as.character(backend %||% "mfcl_exe"))
  stage <- tolower(as.character(stage %||% ""))
  plot_backend <- tolower(as.character(plot_backend %||% ""))
  mfclshiny_script <- as.character(mfclshiny_script %||% "")
  if (identical(stage, "report")) {
    return("none")
  }
  if (identical(stage, "plot") || identical(plot_backend, "mfclshiny") || nzchar(mfclshiny_script)) {
    return(specs[["mfclshiny"]])
  }
  if (identical(stage, "diagnostics") || identical(backend, "diagnostics_smoke")) {
    return("none")
  }
  if (identical(backend, "mfclrtmb")) {
    return(paste(c(specs[["mfclrtmb"]], specs[["mfclkit"]]), collapse = ","))
  }
  if (backend %in% c("mfcl_exe", "mfcl_smoke")) {
    return(specs[["mfclkit"]])
  }
  "none"
}

# ---- Row normalization -----------------------------------------------------------
#
# common_env() fills in defaults and standard metadata columns. It accepts extra
# columns and leaves them in place; those extra columns are passed to Kflow as
# job environment variables.

common_env <- function(rows) {
  rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  rows$SOURCE_REPO <- if ("SOURCE_REPO" %in% names(rows)) rows$SOURCE_REPO else flow_source_repo
  rows$SOURCE_REF <- if ("SOURCE_REF" %in% names(rows)) rows$SOURCE_REF else flow_source_ref
  rows$SOURCE_PATH <- if ("SOURCE_PATH" %in% names(rows)) rows$SOURCE_PATH else flow_source_path
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
  rows$PROGRAM_PATH <- if ("PROGRAM_PATH" %in% names(rows)) rows$PROGRAM_PATH else flow_default_program
  rows$COMPACT_OUTPUTS <- if ("COMPACT_OUTPUTS" %in% names(rows)) rows$COMPACT_OUTPUTS else "true"
  rows$COLLECT_SOURCE_ARTIFACTS <- if ("COLLECT_SOURCE_ARTIFACTS" %in% names(rows)) rows$COLLECT_SOURCE_ARTIFACTS else "false"
  rows$SMOKE_INPUT_PAR <- if ("SMOKE_INPUT_PAR" %in% names(rows)) rows$SMOKE_INPUT_PAR else "last"
  rows$SMOKE_OUTPUT_PAR <- if ("SMOKE_OUTPUT_PAR" %in% names(rows)) rows$SMOKE_OUTPUT_PAR else ""
  rows$SMOKE_FEVALS <- if ("SMOKE_FEVALS" %in% names(rows)) rows$SMOKE_FEVALS else "1"
  rows$INPUT_RECIPE_REMOVE_START_FILES <- if ("INPUT_RECIPE_REMOVE_START_FILES" %in% names(rows)) {
    rows$INPUT_RECIPE_REMOVE_START_FILES
  } else {
    ifelse(tolower(rows$MFCL_BACKEND) == "mfcl_smoke", "0", "1")
  }
  rows$FLOW_GROUP <- if ("FLOW_GROUP" %in% names(rows)) rows$FLOW_GROUP else flow_flow_group
  rows$FLOW_SPECIES <- if ("FLOW_SPECIES" %in% names(rows)) rows$FLOW_SPECIES else flow_species
  rows$FLOW_SPECIES_LABEL <- if ("FLOW_SPECIES_LABEL" %in% names(rows)) rows$FLOW_SPECIES_LABEL else flow_species_label
  rows$FLOW_ASSESSMENT_YEAR <- if ("FLOW_ASSESSMENT_YEAR" %in% names(rows)) rows$FLOW_ASSESSMENT_YEAR else flow_assessment_year
  rows$FLOW_TASK_PREFIX <- if ("FLOW_TASK_PREFIX" %in% names(rows)) rows$FLOW_TASK_PREFIX else flow_task_prefix
  rows$KFLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES <- if ("KFLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES" %in% names(rows)) {
    rows$KFLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES
  } else {
    "false"
  }
  rows$KFLOW_RUNTIME_UPDATE <- if ("KFLOW_RUNTIME_UPDATE" %in% names(rows)) {
    rows$KFLOW_RUNTIME_UPDATE
  } else {
    "off"
  }
  rows$TUNA_FLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES <- if ("TUNA_FLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES" %in% names(rows)) {
    rows$TUNA_FLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES
  } else {
    "true"
  }
  rows$TUNA_FLOW_RUNTIME_UPDATE <- if ("TUNA_FLOW_RUNTIME_UPDATE" %in% names(rows)) {
    rows$TUNA_FLOW_RUNTIME_UPDATE
  } else {
    "auto"
  }
  rows$KFLOW_RUNTIME_PACKAGES <- if ("KFLOW_RUNTIME_PACKAGES" %in% names(rows)) {
    rows$KFLOW_RUNTIME_PACKAGES
  } else {
    stage <- if ("CHANGE_GROUP" %in% names(rows)) rows$CHANGE_GROUP else ""
    plot_backend <- if ("PLOT_BACKEND" %in% names(rows)) rows$PLOT_BACKEND else ""
    mfclshiny_script <- if ("MFCLSHINY_SCRIPT" %in% names(rows)) rows$MFCLSHINY_SCRIPT else ""
    mapply(
      runtime_package_specs,
      rows$MFCL_BACKEND,
      stage,
      plot_backend,
      mfclshiny_script,
      USE.NAMES = FALSE
    )
  }
  rows$KFLOW_RUNTIME_GITHUB_AUTH <- if ("KFLOW_RUNTIME_GITHUB_AUTH" %in% names(rows)) {
    rows$KFLOW_RUNTIME_GITHUB_AUTH
  } else {
    "true"
  }
  rows$KFLOW_FORWARD_GITHUB_TOKEN_TO_RUNTIME <- if ("KFLOW_FORWARD_GITHUB_TOKEN_TO_RUNTIME" %in% names(rows)) {
    rows$KFLOW_FORWARD_GITHUB_TOKEN_TO_RUNTIME
  } else {
    "true"
  }
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
  rows$REPORT_TEMPLATE_DIR <- if ("REPORT_TEMPLATE_DIR" %in% names(rows)) rows$REPORT_TEMPLATE_DIR else ""
  rows$REPORT_TEMPLATE_MAIN <- if ("REPORT_TEMPLATE_MAIN" %in% names(rows)) rows$REPORT_TEMPLATE_MAIN else ""
  rows$REPORT_RENDER_FORMAT <- if ("REPORT_RENDER_FORMAT" %in% names(rows)) rows$REPORT_RENDER_FORMAT else ""
  rows[] <- lapply(rows, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
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
    INPUT_TASK = flow_task_codes[["base"]],
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
  flow_require_kflowkit()
  rows <- common_env(rows)
  lapply(seq_len(nrow(rows)), function(index) {
    row <- rows[index, , drop = FALSE]
    KflowKit::kflow_job_launch(
      report_code = task_code,
      config = row,
      repo = flow_kflow_repo,
      branch = flow_kflow_branch,
      target_folder = target_folder,
      docker_image = flow_docker_image,
      input_jobs = input_selector_for_row(row),
      tags = utils::modifyList(list(project = flow_project_tag, species = flow_species, assessment_year = flow_assessment_year), tags),
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
  flow_require_kflowkit()
  task_paths <- c("base", "sensitivity", "diagnostics", "plot", "report")
  KflowKit::kflow_register_workflow(
    paths = task_paths,
    codes = unname(flow_task_codes[task_paths]),
    names = unname(flow_task_codes[task_paths]),
    repo = flow_kflow_repo,
    branch = flow_kflow_branch,
    target_folders = task_paths,
    docker_images = rep(flow_docker_image, length(task_paths)),
    checkout = list(mode = "full", paths = list()),
    ...
  )
}

launch_base <- function(rows = base_models, ...) {
  launch_rows(flow_task_codes[["base"]], "base", rows, tags = list(stage = "base"), ...)
}

launch_sensitivity <- function(rows = sensitivity_models, ...) {
  launch_rows(flow_task_codes[["sensitivity"]], "sensitivity", rows, tags = list(stage = "sensitivity"), ...)
}

launch_diagnostics <- function(rows = diagnostics_runs, ...) {
  launch_rows(flow_task_codes[["diagnostics"]], "diagnostics", rows, tags = list(stage = "diagnostics"), ...)
}

launch_plot <- function(rows = plot_runs, ...) {
  launch_rows(flow_task_codes[["plot"]], "plot", rows, tags = list(stage = "plot"), ...)
}

launch_report <- function(rows = report_runs, ...) {
  launch_rows(flow_task_codes[["report"]], "report", rows, tags = list(stage = "report"), ...)
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
    BASE_MODEL_KEY = if (identical(input_task, flow_task_codes[["base"]])) input_key else "",
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

report_from <- function(input_task = "",
                        input_key = "",
                        job_key,
                        title = paste("Report:", input_key),
                        report_source_repo = flow_report_repo,
                        report_source_ref = flow_report_ref,
                        report_source_path = flow_report_path,
                        report_main = flow_report_main,
                        require_plots = nzchar(input_task) && nzchar(input_key),
                        rewrite_figures = nzchar(input_task) && nzchar(input_key),
                        rewrite_tables = nzchar(input_task) && nzchar(input_key),
                        copy_figure_tree = !isTRUE(rewrite_figures),
                        figure_input_dir = "") {
  data.frame(
    RUN_LABEL = job_key,
    JOB_KEY = job_key,
    JOB_TITLE = title,
    JOB_DESCRIPTION = if (nzchar(input_task) && nzchar(input_key)) {
      paste("Renders a Quarto report from", input_task, input_key)
    } else {
      paste("Renders", report_source_repo, "directly")
    },
    INPUT_TASK = input_task,
    INPUT_KEY = input_key,
    REPORT_TITLE = title,
    REPORT_FILE_STEM = flow_report_file_stem,
    REPORT_SOURCE_REPO = report_source_repo,
    REPORT_SOURCE_REF = report_source_ref,
    REPORT_SOURCE_PATH = report_source_path,
    REPORT_TEMPLATE_MAIN = report_main,
    REPORT_RENDER_FORMAT = "pdf",
    REPORT_REQUIRE_PLOTS = ifelse(isTRUE(require_plots), "true", "false"),
    REPORT_REWRITE_FIGURES = ifelse(isTRUE(rewrite_figures), "true", "false"),
    REPORT_REWRITE_TABLES = ifelse(isTRUE(rewrite_tables), "true", "false"),
    REPORT_COPY_FIGURE_TREE = ifelse(isTRUE(copy_figure_tree), "true", "false"),
    REPORT_FIGURE_INPUT_DIR = figure_input_dir,
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
    base = launch_rows_batched(flow_task_codes[["base"]], "base", rows, batch_size = batch_size, limit = limit, tags = list(stage = "base"), ...),
    sensitivity = launch_rows_batched(flow_task_codes[["sensitivity"]], "sensitivity", rows, batch_size = batch_size, limit = limit, tags = list(stage = "sensitivity"), ...),
    diagnostics = launch_rows_batched(flow_task_codes[["diagnostics"]], "diagnostics", rows, batch_size = batch_size, limit = limit, tags = list(stage = "diagnostics"), ...),
    plot = launch_rows_batched(flow_task_codes[["plot"]], "plot", rows, batch_size = batch_size, limit = limit, tags = list(stage = "plot"), ...),
    report = launch_rows_batched(flow_task_codes[["report"]], "report", rows, batch_size = batch_size, limit = limit, tags = list(stage = "report"), ...)
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
