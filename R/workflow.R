# Tuna Kflow launch layer.
#
# This is the main file to edit day to day.
# - Edit the tables near the top to add/remove base models, sensitivities,
#   selftests, plot jobs, or reports.
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

flow_bool_value <- function(value, default = FALSE) {
  if (is.null(value) || !length(value) || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) {
    return(default)
  }
  tolower(trimws(as.character(value[[1]]))) %in% c("1", "true", "yes", "y", "on")
}

flow_bind_rows <- function(...) {
  tables <- list(...)
  if (length(tables) == 1L && is.list(tables[[1]]) && !is.data.frame(tables[[1]])) {
    tables <- tables[[1]]
  }
  tables <- Filter(function(x) is.data.frame(x) && (nrow(x) > 0 || ncol(x) > 0), tables)
  if (!length(tables)) {
    return(data.frame())
  }
  columns <- unique(unlist(lapply(tables, names), use.names = FALSE))
  tables <- lapply(tables, function(x) {
    missing <- setdiff(columns, names(x))
    for (name in missing) {
      x[[name]] <- NA_character_
    }
    x[columns]
  })
  do.call(rbind, tables)
}

flow_first_col <- function(row, names, default = "") {
  for (name in names) {
    if (name %in% names(row)) {
      value <- as.character(row[[name]][[1]])
      if (!is.na(value) && nzchar(value)) {
        return(value)
      }
    }
  }
  default
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

flow_species <- toupper(flow_env_any(c("FLOW_SPECIES", "TUNA_FLOW_SPECIES"), "TUNA"))
flow_assessment_year <- flow_env_any(c("FLOW_ASSESSMENT_YEAR", "TUNA_FLOW_ASSESSMENT_YEAR"), "YYYY")
flow_species_label <- flow_env_any(
  c("FLOW_SPECIES_LABEL", "TUNA_FLOW_SPECIES_LABEL"),
  switch(flow_species, BET = "Bigeye tuna", YFT = "Yellowfin tuna", SKJ = "Skipjack tuna", ALB = "Albacore tuna", flow_title_case(flow_species))
)
flow_assessment_label <- trimws(paste(flow_species, flow_assessment_year))
flow_species_slug <- tolower(gsub("[^A-Za-z0-9]+", "-", flow_species))
flow_assessment_slug <- flow_env_any(
  c("FLOW_ASSESSMENT_SLUG", "TUNA_FLOW_ASSESSMENT_SLUG"),
  paste0(flow_species_slug, flow_assessment_year)
)
flow_report_folder <- flow_env_any(
  c("FLOW_REPORT_FOLDER", "TUNA_FLOW_REPORT_FOLDER"),
  paste(flow_species_slug, flow_assessment_year, "report", sep = "-")
)
flow_task_prefix <- flow_env_any(
  c("FLOW_TASK_PREFIX", "TUNA_FLOW_TASK_PREFIX"),
  paste(flow_species_slug, flow_assessment_year, sep = "-")
)
flow_project_tag <- flow_env_any(c("FLOW_PROJECT_TAG", "TUNA_FLOW_PROJECT_TAG"), "tuna-flow")
flow_flow_group <- paste(flow_project_tag, flow_species_slug, format(Sys.time(), "%Y%m%d-%H%M%S"), sep = "-")
flow_kflow_repo <- flow_env_any(c("FLOW_KFLOW_REPO", "TUNA_FLOW_REPO"), "PacificCommunity/ofp-sam-tuna-flow")
flow_kflow_branch <- flow_env_any(c("FLOW_KFLOW_BRANCH", "TUNA_FLOW_BRANCH"), "main")
flow_config_file <- flow_env("FLOW_CONFIG", "")
flow_config_dir_default <- if (nzchar(flow_config_file)) {
  tools::file_path_sans_ext(flow_config_file)
} else {
  file.path("configs", flow_assessment_slug)
}
flow_config_dir <- flow_env_any(c("FLOW_TABLE_DIR", "TUNA_FLOW_TABLE_DIR"), flow_config_dir_default)
flow_table_path <- function(file) {
  file.path(flow_config_dir, file)
}
flow_read_table <- function(file, env_names = character()) {
  override <- flow_env_any(env_names, "")
  path <- if (nzchar(override)) override else flow_table_path(file)
  if (!nzchar(path) || !file.exists(path)) {
    return(data.frame())
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
flow_source_repo <- flow_env_any(
  c("FLOW_SOURCE_REPO", "TUNA_FLOW_SOURCE_REPO"),
  paste0("PacificCommunity/ofp-sam-", flow_assessment_slug, "-inputs")
)
flow_source_ref <- flow_env_any(c("FLOW_SOURCE_REF", "TUNA_FLOW_SOURCE_REF"), "main")
flow_source_path <- flow_env_any(c("FLOW_SOURCE_PATH", "TUNA_FLOW_SOURCE_PATH", "SOURCE_PATH"), "")
flow_report_repo <- flow_env_any(
  c("FLOW_REPORT_REPO", "TUNA_FLOW_REPORT_REPO"),
  paste0("PacificCommunity/ofp-sam-", flow_assessment_slug, "-report")
)
flow_report_ref <- flow_env_any(c("FLOW_REPORT_REF", "TUNA_FLOW_REPORT_REF"), "main")
flow_report_path <- flow_env_any(c("FLOW_REPORT_PATH", "TUNA_FLOW_REPORT_PATH"), flow_report_folder)
flow_report_main <- flow_env_any(c("FLOW_REPORT_MAIN", "TUNA_FLOW_REPORT_MAIN"), "assessment-report.qmd")
flow_docker_image <- flow_env_any(
  c("FLOW_DOCKER_IMAGE", "TUNA_FLOW_DOCKER_IMAGE"),
  "ghcr.io/pacificcommunity/tuna-flow:v1.5"
)
flow_run_mode <- tolower(flow_env_any(c("FLOW_RUN_MODE", "TUNA_FLOW_RUN_MODE"), "smoke"))
flow_is_full_run <- flow_run_mode %in% c("full", "production", "assessment", "e2e")
flow_model_backend <- flow_env_any(
  c("FLOW_MODEL_BACKEND", "TUNA_FLOW_MODEL_BACKEND"),
  if (isTRUE(flow_is_full_run)) "mfcl_full" else "mfcl_smoke"
)
flow_model_make_targets <- flow_env_any(
  c("FLOW_MODEL_MAKE_TARGETS", "TUNA_FLOW_MODEL_MAKE_TARGETS"),
  if (identical(flow_model_backend, "mfcl_full")) "mfcl-full" else "mfcl-smoke"
)
flow_validation_tasks <- c("selftest", "jitter", "retro", "hessian", "likprof")
flow_task_names <- c("base", "sensitivity", flow_validation_tasks, "plot", "report")
flow_task_codes <- setNames(paste(flow_task_prefix, flow_task_names, sep = "-"), flow_task_names)
flow_task_labels <- c(
  base = "Base model",
  sensitivity = "Sensitivity",
  selftest = "Model selftest",
  jitter = "Jitter",
  retro = "Retro",
  hessian = "Hessian",
  likprof = "Likprof",
  plot = "Report figures",
  report = "Assessment report"
)
flow_task_codes[["diagnostics"]] <- flow_task_codes[["selftest"]]
flow_task_codes[["report"]] <- flow_env_any(
  c("FLOW_REPORT_TASK_CODE", "TUNA_FLOW_REPORT_TASK_CODE"),
  paste(flow_task_prefix, "report", sep = "-")
)
flow_default_program <- flow_env_any(c("FLOW_MFCL_PROGRAM", "TUNA_FLOW_MFCL_PROGRAM"), "/home/mfcl/mfclo64")
flow_default_input_dir <- flow_env_any(c("FLOW_BASE_INPUT_DIR", "TUNA_FLOW_BASE_INPUT_DIR"), "mfcl/inputs/base")
flow_base_input_dirs <- flow_split_csv(flow_env_any(c("FLOW_BASE_INPUT_DIRS", "TUNA_FLOW_BASE_INPUT_DIRS"), flow_default_input_dir))
if (!length(flow_base_input_dirs)) {
  flow_base_input_dirs <- flow_default_input_dir
}
flow_default_input_variant <- flow_env_any(c("FLOW_BASE_INPUT_VARIANT", "TUNA_FLOW_BASE_INPUT_VARIANT"), basename(flow_base_input_dirs[[1]]))
flow_base_job_key <- flow_env_any(c("FLOW_BASE_JOB_KEY", "TUNA_FLOW_BASE_JOB_KEY"), if (isTRUE(flow_is_full_run)) "base" else "base-smoke")
flow_base_token <- flow_env_any(c("FLOW_BASE_TOKEN", "TUNA_FLOW_BASE_TOKEN"), "Base")
flow_report_file_stem <- flow_env_any(c("FLOW_REPORT_FILE_STEM", "TUNA_FLOW_REPORT_FILE_STEM"), paste(flow_task_prefix, "report", sep = "-"))
flow_ini_version_target <- flow_env_any(c("FLOW_MFCL_INI_VERSION_TARGET", "MFCL_INI_VERSION_TARGET"), "1007")
flow_base_tokens <- flow_split_csv(flow_env_any(c("FLOW_BASE_TOKENS", "TUNA_FLOW_BASE_TOKENS"), ""))
flow_base_job_keys <- flow_split_csv(flow_env_any(c("FLOW_BASE_JOB_KEYS", "TUNA_FLOW_BASE_JOB_KEYS"), ""))
flow_base_names <- flow_split_csv(flow_env_any(c("FLOW_BASE_NAMES", "TUNA_FLOW_BASE_NAMES"), ""))
flow_starter_sensitivity_tokens <- flow_split_csv(flow_env_any(
  c("FLOW_STARTER_SENSITIVITIES", "TUNA_FLOW_STARTER_SENSITIVITIES"),
  "FixM,FixVB,Sel4,IndexCvHalf"
))
flow_starter_selftest_tokens <- flow_split_csv(flow_env_any(
  c("FLOW_SELFTESTS", "TUNA_FLOW_SELFTESTS", "FLOW_STARTER_SELFTESTS", "TUNA_FLOW_STARTER_SELFTESTS"),
  "check,jitter,retro,hessian,likprof"
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

flow_base_specs <- flow_read_table("base-models.csv", c("FLOW_BASE_MODELS_CSV", "TUNA_FLOW_BASE_MODELS_CSV"))
if (nrow(flow_base_specs)) {
  spec_dirs <- vapply(seq_len(nrow(flow_base_specs)), function(index) {
    flow_first_col(flow_base_specs[index, , drop = FALSE], c("BASE_DIR", "INPUT_DIR", "input_dir", "base_dir"), "")
  }, character(1))
  spec_dirs <- spec_dirs[nzchar(spec_dirs)]
  if (length(spec_dirs)) {
    flow_base_input_dirs <- spec_dirs
  }
}

flow_base_token_for <- function(index, base_dir, spec = NULL) {
  if (!is.null(spec) && nrow(spec)) {
    value <- flow_first_col(spec, c("MODEL_TOKEN", "TOKEN", "BASE_TOKEN", "token"), "")
    if (nzchar(value)) {
      return(value)
    }
  }
  if (length(flow_base_tokens) >= index && nzchar(flow_base_tokens[[index]])) {
    return(flow_base_tokens[[index]])
  }
  if (length(flow_base_input_dirs) == 1L) {
    return(flow_base_token)
  }
  paste0("Base", gsub("[^A-Za-z0-9]+", "", flow_title_case(basename(base_dir))))
}

flow_base_job_key_for <- function(index, token, spec = NULL) {
  if (!is.null(spec) && nrow(spec)) {
    value <- flow_first_col(spec, c("JOB_KEY", "job_key"), "")
    if (nzchar(value)) {
      return(value)
    }
  }
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
    spec <- if (nrow(flow_base_specs) >= index) flow_base_specs[index, , drop = FALSE] else data.frame()
    base_dir <- base_dirs[[index]]
    input_variant <- flow_first_col(spec, c("INPUT_VARIANT", "VARIANT", "variant"), basename(base_dir))
    token <- flow_base_token_for(index, base_dir, spec = spec)
    job_key <- flow_base_job_key_for(index, token, spec = spec)
    model_name <- if (length(flow_base_names) >= index && nzchar(flow_base_names[[index]])) {
      flow_base_names[[index]]
    } else if (nrow(spec)) {
      flow_first_col(spec, c("MODEL_NAME", "NAME", "name"), paste(flow_assessment_label, token))
    } else {
      paste(flow_assessment_label, token)
    }
    model_dir <- flow_first_col(spec, c("MODEL_DIR", "model_dir"), file.path("model", job_key))
    backend <- flow_first_col(spec, c("MFCL_BACKEND", "BACKEND", "backend"), flow_model_backend)
    make_targets <- flow_first_col(spec, c("MAKE_TARGETS", "make_targets"), flow_model_make_targets)
    run_label <- flow_first_col(spec, c("RUN_LABEL", "run_label"), job_key)
    title <- flow_first_col(spec, c("JOB_TITLE", "TITLE", "title"), paste("Base:", token))
    description <- flow_first_col(
      spec,
      c("JOB_DESCRIPTION", "DESCRIPTION", "description"),
      paste("Runs", input_variant, flow_species_label, "base model.")
    )
    data.frame(
      RUN_LABEL = run_label,
      JOB_KEY = job_key,
      MODEL_KEY = job_key,
      MODEL_TOKEN = token,
      MODEL_NAME = model_name,
      BASE_MODEL_KEY = "",
      CHANGE_TOKEN = token,
      CHANGE_GROUP = "base",
      CHANGE_SUMMARY = description,
      INPUT_VARIANT = input_variant,
      SOURCE_REPO = flow_source_repo,
      SOURCE_REF = flow_source_ref,
      SOURCE_PATH = flow_source_path,
      USE_FLOW_SOURCE = "0",
      MFCL_BACKEND = backend,
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
      JOB_TITLE = title,
      JOB_DESCRIPTION = description,
      MAKE_TARGETS = make_targets,
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

flow_merge_recipe_specs <- function(defaults, specs, token_col = "RECIPE_TOKEN") {
  if (!is.data.frame(specs) || !nrow(specs)) {
    return(defaults)
  }
  rows <- lapply(seq_len(nrow(specs)), function(index) {
    spec <- specs[index, , drop = FALSE]
    token <- flow_first_col(spec, c(token_col, "TOKEN", "token"), "")
    base <- data.frame()
    if (nzchar(token) && token_col %in% names(defaults)) {
      hits <- defaults[tolower(as.character(defaults[[token_col]])) == tolower(token), , drop = FALSE]
      if (nrow(hits)) {
        base <- hits[1, , drop = FALSE]
      }
    }
    if (!nrow(base)) {
      base <- spec
    } else {
      for (name in names(spec)) {
        value <- as.character(spec[[name]][[1]])
        if (!is.na(value) && nzchar(value)) {
          base[[name]] <- value
        }
      }
    }
    base
  })
  flow_bind_rows(rows)
}

base_models <- build_base_model_rows()

flow_sensitivity_specs <- flow_read_table("sensitivity-recipes.csv", c("FLOW_SENSITIVITY_RECIPES_CSV", "TUNA_FLOW_SENSITIVITY_RECIPES_CSV"))
starter_sensitivity_recipes <- if (nrow(flow_sensitivity_specs)) {
  flow_merge_recipe_specs(mfcl_input_recipe_library, flow_sensitivity_specs)
} else {
  mfcl_input_recipe_library[
    mfcl_input_recipe_library$RECIPE_TOKEN %in% flow_starter_sensitivity_tokens,
    ,
    drop = FALSE
  ]
}

selftest_recipe_library <- data.frame(
  RECIPE_TOKEN = c("check", "jitter", "retro", "hessian", "likprof"),
  RECIPE_KEY = c("check", "jitter", "retro", "hessian", "likprof"),
  RECIPE_FAMILY = c("selftest", "jitter", "retro", "hessian", "likprof"),
  RECIPE_LABEL = c("Check", "Jitter", "Retro", "Hessian", "Likprof"),
  CHANGE_DETAIL = c(
    "Checks that upstream model payloads, summaries, and key quantities are readable.",
    "Runs a seeded jitter-style selftest summary from the parent model output.",
    "Runs a retrospective-style selftest summary from the parent model output.",
    "Records a Hessian selftest placeholder tied to the parent model output.",
    "Records a likelihood-profile selftest placeholder tied to the parent model output."
  ),
  MAKE_TARGETS = "selftest",
  SELFTEST_MODE = c("selftest", "jitter", "retro", "hessian", "likprof"),
  SELFTEST_SEED = c(1000L, 2000L, 3000L, 4000L, 5000L),
  RETRO_PEEL = c(NA, NA, 1L, NA, NA),
  MFCL_BACKEND = "selftest",
  stringsAsFactors = FALSE
)
flow_selftest_specs <- flow_read_table("selftests.csv", c("FLOW_SELFTESTS_CSV", "TUNA_FLOW_SELFTESTS_CSV"))
starter_selftest_recipes <- if (nrow(flow_selftest_specs)) {
  flow_merge_recipe_specs(selftest_recipe_library, flow_selftest_specs)
} else {
  selftest_recipe_library[
    selftest_recipe_library$RECIPE_TOKEN %in% flow_starter_selftest_tokens,
    ,
    drop = FALSE
  ]
}

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
      backend <- flow_first_col(recipe, c("MFCL_BACKEND", "BACKEND", "backend"), base$MFCL_BACKEND %||% flow_model_backend)
      make_targets <- flow_first_col(recipe, c("MAKE_TARGETS", "make_targets"), flow_model_make_targets)
      row <- data.frame(
        RUN_LABEL = job_key,
        JOB_KEY = job_key,
        MODEL_KEY = job_key,
        MODEL_TOKEN = model_token,
        MODEL_NAME = paste(base$MODEL_TOKEN, recipe$RECIPE_LABEL),
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
        CHANGE_SUMMARY = recipe$CHANGE_DETAIL,
        INPUT_VARIANT = input_variant,
        INPUT_TASK = flow_task_codes[["base"]],
        INPUT_KEY = base$JOB_KEY,
        SOURCE_REPO = base$SOURCE_REPO %||% flow_source_repo,
        SOURCE_REF = base$SOURCE_REF %||% flow_source_ref,
        SOURCE_PATH = base$SOURCE_PATH %||% "",
        USE_FLOW_SOURCE = base$USE_FLOW_SOURCE %||% "0",
        MFCL_BACKEND = backend,
        PROGRAM_PATH = base$PROGRAM_PATH %||% flow_default_program,
        MAKE_TARGETS = make_targets,
        BASE_DIR = base_dir,
        MODEL_DIR = model_dir,
        PATCH_SCRIPT = recipe$PATCH_SCRIPT,
        PATCH_INPUT_DIR = base$BASE_DIR,
        PATCH_OUTPUT_DIR = base_dir,
        JOB_TITLE = paste("Sensitivity:", recipe$RECIPE_TOKEN),
        JOB_DESCRIPTION = paste("Runs", recipe$RECIPE_TOKEN, "from", base$MODEL_TOKEN, "."),
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

build_selftest_rows <- function(parent_rows, recipes, input_task) {
  if (!nrow(parent_rows) || !nrow(recipes)) {
    return(data.frame())
  }
  rows <- list()
  for (parent_index in seq_len(nrow(parent_rows))) {
    parent <- parent_rows[parent_index, , drop = FALSE]
    parent_input_task <- if (length(input_task) > 1L) input_task[[parent_index]] else input_task
    for (recipe_index in seq_len(nrow(recipes))) {
      recipe <- recipes[recipe_index, , drop = FALSE]
      seed_base <- suppressWarnings(as.integer(flow_first_col(recipe, c("SELFTEST_SEED", "JITTER_SEED"), "1000")))
      if (!is.finite(seed_base)) seed_base <- 1000L
      seed <- seed_base + parent_index - 1L
      selftest_mode <- flow_first_col(recipe, c("SELFTEST_MODE", "MODE", "mode"), recipe$RECIPE_KEY)
      validation_stage <- tolower(flow_first_col(recipe, c("VALIDATION_STAGE", "RECIPE_FAMILY", "SELFTEST_MODE", "MODE"), selftest_mode))
      validation_stage <- if (validation_stage %in% flow_validation_tasks) validation_stage else "selftest"
      validation_code <- flow_task_codes[[validation_stage]]
      job_key <- if (identical(validation_stage, "selftest")) {
        paste("selftest", parent$MODEL_TOKEN, recipe$RECIPE_KEY, sep = "-")
      } else {
        paste(validation_stage, parent$MODEL_TOKEN, sep = "-")
      }
      job_key <- gsub("[^A-Za-z0-9_-]+", "-", job_key)
      model_token <- paste(parent$MODEL_TOKEN, recipe$RECIPE_TOKEN, sep = "_")
      rows[[length(rows) + 1L]] <- data.frame(
        RUN_LABEL = job_key,
        JOB_KEY = job_key,
        MODEL_KEY = job_key,
        MODEL_TOKEN = model_token,
        MODEL_NAME = paste(parent$MODEL_TOKEN, recipe$RECIPE_LABEL),
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
        CHANGE_GROUP = validation_stage,
        CHANGE_DETAIL = recipe$CHANGE_DETAIL,
        CHANGE_SUMMARY = paste(recipe$CHANGE_DETAIL, "Parent:", parent$MODEL_TOKEN),
        VALIDATION_STAGE = validation_stage,
        VALIDATION_TASK_CODE = validation_code,
        INPUT_VARIANT = parent$INPUT_VARIANT %||% "",
        INPUT_TASK = parent_input_task,
        INPUT_KEY = parent$JOB_KEY,
        SOURCE_REPO = parent$SOURCE_REPO %||% flow_source_repo,
        SOURCE_REF = parent$SOURCE_REF %||% flow_source_ref,
        SOURCE_PATH = parent$SOURCE_PATH %||% "",
        USE_FLOW_SOURCE = parent$USE_FLOW_SOURCE %||% "0",
        MFCL_BACKEND = flow_first_col(recipe, c("MFCL_BACKEND", "BACKEND"), "selftest"),
        PROGRAM_PATH = parent$PROGRAM_PATH %||% flow_default_program,
        MAKE_TARGETS = recipe$MAKE_TARGETS,
        BASE_DIR = parent$BASE_DIR %||% "",
        MODEL_DIR = file.path("model", validation_stage, job_key),
        SELFTEST_MODE = selftest_mode,
        SELFTEST_SEED = seed,
        JITTER_SEED = seed,
        RETRO_PEEL = flow_first_col(recipe, c("RETRO_PEEL", "PEEL"), ""),
        LIKPROF_TARGET = flow_first_col(recipe, c("LIKPROF_TARGET", "PROFILE_TARGET"), ""),
        JITTER_SMOKE_ONLY = "0",
        PATCH_SCRIPT = "",
        PATCH_INPUT_DIR = "",
        PATCH_OUTPUT_DIR = "",
        JOB_TITLE = paste(flow_task_labels[[validation_stage]], ":", parent$MODEL_TOKEN),
        JOB_DESCRIPTION = paste(recipe$RECIPE_LABEL, "check for", parent$MODEL_TOKEN, "seed", seed),
        COLLECT_PATHS = file.path("model", validation_stage, job_key),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

sensitivity_models <- build_sensitivity_rows(base_models, starter_sensitivity_recipes)

validation_runs <- flow_bind_rows(
  build_selftest_rows(base_models, starter_selftest_recipes, flow_task_codes[["base"]]),
  build_selftest_rows(sensitivity_models, starter_selftest_recipes, flow_task_codes[["sensitivity"]])
)
validation_runs_by_stage <- stats::setNames(lapply(flow_validation_tasks, function(stage) {
  validation_runs[validation_runs$VALIDATION_STAGE == stage, , drop = FALSE]
}), flow_validation_tasks)
selftest_runs <- validation_runs_by_stage[["selftest"]]
jitter_runs <- validation_runs_by_stage[["jitter"]]
retro_runs <- validation_runs_by_stage[["retro"]]
hessian_runs <- validation_runs_by_stage[["hessian"]]
likprof_runs <- validation_runs_by_stage[["likprof"]]
diagnostics_runs <- validation_runs

flow_plot_job_key <- flow_env_any(c("FLOW_PLOT_JOB_KEY", "TUNA_FLOW_PLOT_JOB_KEY"), if (isTRUE(flow_is_full_run)) "plot-report-figures" else "plot-key-quantities-smoke")
flow_report_job_key <- flow_env_any(c("FLOW_REPORT_JOB_KEY", "TUNA_FLOW_REPORT_JOB_KEY"), if (isTRUE(flow_is_full_run)) "report-assessment" else "report-key-quantities-smoke")
flow_plot_title <- flow_env_any(c("FLOW_PLOT_TITLE", "TUNA_FLOW_PLOT_TITLE"), paste(flow_assessment_label, "report figures"))
flow_report_render_format <- flow_env_any(c("FLOW_REPORT_RENDER_FORMAT", "TUNA_FLOW_REPORT_RENDER_FORMAT"), "pdf")

plot_runs <- data.frame(
  RUN_LABEL = flow_plot_job_key,
  JOB_KEY = flow_plot_job_key,
  MODEL_KEY = flow_plot_job_key,
  MODEL_TOKEN = "Plot",
  MODEL_NAME = paste(flow_assessment_label, "report figures"),
  BASE_MODEL_KEY = flow_base_job_key,
  CHANGE_TOKEN = "Plot",
  CHANGE_GROUP = "plot",
  CHANGE_SUMMARY = "Builds the mfclshiny report-ready figure bundle from selected model and selftest outputs.",
  JOB_TITLE = "Plot: report figures",
  JOB_DESCRIPTION = "Builds mfclshiny report figures, tables, and HTML review.",
  INPUT_TASK = paste(validation_runs$VALIDATION_TASK_CODE, collapse = ","),
  INPUT_KEY = paste(validation_runs$JOB_KEY, collapse = ","),
  PLOT_TITLE = flow_plot_title,
  PLOT_BACKEND = "mfclshiny",
  MFCLSHINY_SCRIPT = "hooks/depletion_smoke.R",
  REPORT_FIGURE_BASENAME = "key-quantities",
  MODEL_LABEL = "Report figures",
  PLOT_LABEL = "Report figures",
  REPORT_LABEL = "Report figures",
  stringsAsFactors = FALSE
)

report_runs <- data.frame(
  RUN_LABEL = flow_report_job_key,
  JOB_KEY = flow_report_job_key,
  MODEL_KEY = flow_report_job_key,
  MODEL_TOKEN = "Report",
  MODEL_NAME = paste(flow_assessment_label, "assessment report"),
  BASE_MODEL_KEY = flow_base_job_key,
  CHANGE_TOKEN = "Report",
  CHANGE_GROUP = "report",
  CHANGE_SUMMARY = "Renders the assessment report from the selected report-ready figure bundle.",
  JOB_TITLE = "Report: assessment",
  JOB_DESCRIPTION = "Renders the Quarto assessment report from Kflow outputs.",
  INPUT_TASK = flow_task_codes[["plot"]],
  INPUT_KEY = flow_plot_job_key,
  REPORT_TITLE = flow_report_file_stem,
  REPORT_FILE_STEM = flow_report_file_stem,
  REPORT_SOURCE_REPO = flow_report_repo,
  REPORT_SOURCE_REF = flow_report_ref,
  REPORT_SOURCE_PATH = flow_report_path,
  REPORT_TEMPLATE_MAIN = flow_report_main,
  REPORT_FIGURE_DIR = "Figures/generated",
  REPORT_RENDER_FORMAT = flow_report_render_format,
  REPORT_REQUIRE_PLOTS = "true",
  REPORT_REWRITE_FIGURES = "false",
  REPORT_REWRITE_TABLES = "false",
  REPORT_COPY_FIGURE_TREE = "true",
  REPORT_INCLUDE_GENERATED_TABLES = "true",
  KFLOW_RUNTIME_PACKAGES = "none",
  MODEL_LABEL = "Assessment report",
  PLOT_LABEL = "Report",
  REPORT_LABEL = "Assessment report",
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
  if (identical(stage, "selftest") || identical(backend, "diagnostics_smoke") || identical(backend, "selftest")) {
    return("none")
  }
  if (identical(backend, "mfclrtmb")) {
    return(paste(c(specs[["mfclrtmb"]], specs[["mfclkit"]]), collapse = ","))
  }
  if (identical(backend, "mfcl_smoke")) {
    return(specs[["mfclkit"]])
  }
  if (identical(backend, "mfcl_full")) {
    return(paste(c(specs[["mfclkit"]], specs[["mfclshiny"]]), collapse = ","))
  }
  if (identical(backend, "mfcl_exe")) {
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
  runtime_require_provided <- "KFLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES" %in% names(rows)
  runtime_update_provided <- "KFLOW_RUNTIME_UPDATE" %in% names(rows)
  runtime_auth_provided <- "KFLOW_RUNTIME_GITHUB_AUTH" %in% names(rows)
  runtime_forward_token_provided <- "KFLOW_FORWARD_GITHUB_TOKEN_TO_RUNTIME" %in% names(rows)
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
  rows$MFCL_FULL_SCRIPT <- if ("MFCL_FULL_SCRIPT" %in% names(rows)) rows$MFCL_FULL_SCRIPT else "doitall.sh"
  rows$MFCL_FULL_LIVE_LOG_STREAM <- if ("MFCL_FULL_LIVE_LOG_STREAM" %in% names(rows)) {
    rows$MFCL_FULL_LIVE_LOG_STREAM
  } else {
    ifelse(tolower(rows$MFCL_BACKEND) == "mfcl_full", "stderr", "none")
  }
  rows$SELFTEST_MODE <- if ("SELFTEST_MODE" %in% names(rows)) rows$SELFTEST_MODE else ""
  rows$SELFTEST_SEED <- if ("SELFTEST_SEED" %in% names(rows)) rows$SELFTEST_SEED else ""
  rows$RETRO_PEEL <- if ("RETRO_PEEL" %in% names(rows)) rows$RETRO_PEEL else ""
  rows$LIKPROF_TARGET <- if ("LIKPROF_TARGET" %in% names(rows)) rows$LIKPROF_TARGET else ""
  rows$MFCL_REQUIRE_DERIVED <- if ("MFCL_REQUIRE_DERIVED" %in% names(rows)) {
    rows$MFCL_REQUIRE_DERIVED
  } else {
    ifelse(tolower(rows$MFCL_BACKEND) %in% c("mfcl_smoke", "mfcl_full"), "true", "false")
  }
  rows$MFCL_REQUIRE_KEY_QUANTITIES <- if ("MFCL_REQUIRE_KEY_QUANTITIES" %in% names(rows)) {
    rows$MFCL_REQUIRE_KEY_QUANTITIES
  } else {
    ifelse(tolower(rows$MFCL_BACKEND) %in% c("mfcl_smoke", "mfcl_full"), "true", "false")
  }
  rows$MFCL_SMOKE_LIVE_LOG_STREAM <- if ("MFCL_SMOKE_LIVE_LOG_STREAM" %in% names(rows)) {
    rows$MFCL_SMOKE_LIVE_LOG_STREAM
  } else {
    ifelse(tolower(rows$MFCL_BACKEND) == "mfcl_smoke", "stderr", "none")
  }
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
    ""
  }
  rows$KFLOW_RUNTIME_UPDATE <- if ("KFLOW_RUNTIME_UPDATE" %in% names(rows)) {
    rows$KFLOW_RUNTIME_UPDATE
  } else {
    ""
  }
  rows$TUNA_FLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES <- if ("TUNA_FLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES" %in% names(rows)) {
    rows$TUNA_FLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES
  } else {
    ""
  }
  rows$TUNA_FLOW_RUNTIME_UPDATE <- if ("TUNA_FLOW_RUNTIME_UPDATE" %in% names(rows)) {
    rows$TUNA_FLOW_RUNTIME_UPDATE
  } else {
    ""
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
  disabled_runtime_specs <- c("", "0", "false", "no", "off", "none", "skip")
  runtime_specs_disabled <- tolower(trimws(rows$KFLOW_RUNTIME_PACKAGES)) %in% disabled_runtime_specs
  if (!isTRUE(runtime_require_provided)) {
    rows$KFLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES <- ifelse(
      runtime_specs_disabled,
      "false",
      "true"
    )
  }
  if (!isTRUE(runtime_update_provided)) {
    rows$KFLOW_RUNTIME_UPDATE <- ifelse(runtime_specs_disabled, "off", "auto")
  }
  rows$KFLOW_RUNTIME_GITHUB_AUTH <- if (isTRUE(runtime_auth_provided)) {
    rows$KFLOW_RUNTIME_GITHUB_AUTH
  } else {
    ifelse(runtime_specs_disabled, "false", "true")
  }
  rows$KFLOW_FORWARD_GITHUB_TOKEN_TO_RUNTIME <- if (isTRUE(runtime_forward_token_provided)) {
    rows$KFLOW_FORWARD_GITHUB_TOKEN_TO_RUNTIME
  } else {
    ifelse(runtime_specs_disabled, "false", "true")
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
  tasks <- trimws(unlist(strsplit(task, ",")))
  tasks <- tasks[nzchar(tasks)]
  keys <- trimws(unlist(strsplit(key, ",")))
  keys <- keys[nzchar(keys)]
  if (length(tasks) > 1L) {
    if (length(tasks) != length(keys)) {
      stop("When INPUT_TASK lists multiple task codes, INPUT_KEY must list the same number of keys.", call. = FALSE)
    }
    grouped <- split(keys, tasks)
    return(lapply(names(grouped), function(task_code) {
      KflowKit::kflow_input_keys(task_code, grouped[[task_code]])
    }))
  }
  task <- tasks[[1]]
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
                              base_key = flow_base_job_key,
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
  task_paths <- c("base", "sensitivity", flow_validation_tasks, "plot", "report")
  KflowKit::kflow_register_workflow(
    paths = task_paths,
    codes = unname(flow_task_codes[task_paths]),
    names = unname(flow_task_labels[task_paths]),
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

launch_selftest <- function(rows = selftest_runs, ...) {
  launch_rows(flow_task_codes[["selftest"]], "selftest", rows, tags = list(stage = "selftest"), ...)
}

launch_jitter <- function(rows = jitter_runs, ...) {
  launch_rows(flow_task_codes[["jitter"]], "jitter", rows, tags = list(stage = "jitter"), ...)
}

launch_retro <- function(rows = retro_runs, ...) {
  launch_rows(flow_task_codes[["retro"]], "retro", rows, tags = list(stage = "retro"), ...)
}

launch_hessian <- function(rows = hessian_runs, ...) {
  launch_rows(flow_task_codes[["hessian"]], "hessian", rows, tags = list(stage = "hessian"), ...)
}

launch_likprof <- function(rows = likprof_runs, ...) {
  launch_rows(flow_task_codes[["likprof"]], "likprof", rows, tags = list(stage = "likprof"), ...)
}

launch_validation <- function(rows = validation_runs, batch_size = Inf, limit = Inf, ...) {
  rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  out <- list()
  for (stage in flow_validation_tasks) {
    stage_rows <- rows[rows$VALIDATION_STAGE == stage, , drop = FALSE]
    if (nrow(stage_rows)) {
      out[[stage]] <- launch_rows_batched(
        flow_task_codes[[stage]],
        stage,
        stage_rows,
        batch_size = batch_size,
        limit = limit,
        tags = list(stage = stage),
        ...
      )
    }
  }
  out
}

launch_diagnostics <- launch_validation

launch_plot <- function(rows = plot_runs, ...) {
  launch_rows(flow_task_codes[["plot"]], "plot", rows, tags = list(stage = "plot"), ...)
}

launch_report <- function(rows = report_runs, ...) {
  launch_rows(flow_task_codes[["report"]], "report", rows, tags = list(stage = "report"), ...)
}

selftest_from <- function(input_task,
                          input_key,
                          job_key,
                          token = job_key,
                          title = paste("Selftest:", input_key),
                          make_targets = "selftest",
                          base_dir,
                          model_dir,
                          selftest_mode = "jitter",
                          selftest_seed = 40) {
  data.frame(
    RUN_LABEL = job_key,
    JOB_KEY = job_key,
    MODEL_KEY = job_key,
    MODEL_TOKEN = token,
    MODEL_NAME = title,
    BASE_MODEL_KEY = if (identical(input_task, flow_task_codes[["base"]])) input_key else "",
    CHANGE_TOKEN = token,
    CHANGE_GROUP = "selftest",
    CHANGE_SUMMARY = paste("Runs a model selftest from", input_task, input_key),
    JOB_TITLE = title,
    JOB_DESCRIPTION = paste("Runs a model selftest from", input_task, input_key),
    INPUT_TASK = input_task,
    INPUT_KEY = input_key,
    MAKE_TARGETS = make_targets,
    BASE_DIR = base_dir,
    MODEL_DIR = model_dir,
    MFCL_BACKEND = "selftest",
    SELFTEST_MODE = selftest_mode,
    SELFTEST_SEED = selftest_seed,
    JITTER_SEED = selftest_seed,
    JITTER_SMOKE_ONLY = "0",
    COLLECT_PATHS = paste(model_dir, base_dir, sep = ","),
    stringsAsFactors = FALSE
  )
}

diagnostics_from <- selftest_from

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
                        rewrite_figures = FALSE,
                        rewrite_tables = FALSE,
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
    CHANGE_GROUP = "report",
    KFLOW_RUNTIME_PACKAGES = "none",
    REPORT_TITLE = title,
    REPORT_FILE_STEM = flow_report_file_stem,
    REPORT_SOURCE_REPO = report_source_repo,
    REPORT_SOURCE_REF = report_source_ref,
    REPORT_SOURCE_PATH = report_source_path,
    REPORT_TEMPLATE_MAIN = report_main,
    REPORT_FIGURE_DIR = "Figures/generated",
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
    selftest = launch_selftest(...),
    jitter = launch_jitter(...),
    retro = launch_retro(...),
    hessian = launch_hessian(...),
    likprof = launch_likprof(...),
    plot = launch_plot(...),
    report = launch_report(...)
  )
}

launch_stage <- function(stage, rows, batch_size = Inf, limit = Inf, ...) {
  stage <- match.arg(stage, c("base", "sensitivity", "diagnostics", flow_validation_tasks, "plot", "report"))
  switch(
    stage,
    base = launch_rows_batched(flow_task_codes[["base"]], "base", rows, batch_size = batch_size, limit = limit, tags = list(stage = "base"), ...),
    sensitivity = launch_rows_batched(flow_task_codes[["sensitivity"]], "sensitivity", rows, batch_size = batch_size, limit = limit, tags = list(stage = "sensitivity"), ...),
    diagnostics = launch_validation(rows, batch_size = batch_size, limit = limit, ...),
    selftest = launch_rows_batched(flow_task_codes[["selftest"]], "selftest", rows, batch_size = batch_size, limit = limit, tags = list(stage = "selftest"), ...),
    jitter = launch_rows_batched(flow_task_codes[["jitter"]], "jitter", rows, batch_size = batch_size, limit = limit, tags = list(stage = "jitter"), ...),
    retro = launch_rows_batched(flow_task_codes[["retro"]], "retro", rows, batch_size = batch_size, limit = limit, tags = list(stage = "retro"), ...),
    hessian = launch_rows_batched(flow_task_codes[["hessian"]], "hessian", rows, batch_size = batch_size, limit = limit, tags = list(stage = "hessian"), ...),
    likprof = launch_rows_batched(flow_task_codes[["likprof"]], "likprof", rows, batch_size = batch_size, limit = limit, tags = list(stage = "likprof"), ...),
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
