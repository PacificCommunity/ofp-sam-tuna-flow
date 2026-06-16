# Tuna Kflow exploration-plan helpers.
#
# Use this file when the number of models grows beyond a few hand-written rows.
# It builds stage tables from recipes:
#   base rows x sensitivity recipes x diagnostic recipes -> plot/report rows
#
# Kflow still handles dependencies. This file only creates readable data.frames
# with JOB_KEY, INPUT_TASK, and INPUT_KEY columns.

required_plan_columns <- c(
  "RUN_LABEL", "JOB_KEY", "MODEL_KEY", "MODEL_TOKEN", "MODEL_NAME",
  "BASE_MODEL_KEY", "CHANGE_TOKEN", "CHANGE_GROUP", "CHANGE_SUMMARY"
)

plan_task_code <- function(stage) {
  if (exists("flow_task_codes", inherits = TRUE)) {
    return(get("flow_task_codes", inherits = TRUE)[[stage]])
  }
  species <- tolower(Sys.getenv("FLOW_SPECIES", Sys.getenv("TUNA_FLOW_SPECIES", "tuna")))
  year <- Sys.getenv("FLOW_ASSESSMENT_YEAR", Sys.getenv("TUNA_FLOW_ASSESSMENT_YEAR", ""))
  prefix <- Sys.getenv("FLOW_TASK_PREFIX", Sys.getenv("TUNA_FLOW_TASK_PREFIX", paste0(species, year)))
  paste(prefix, stage, sep = "-")
}

plan_flow_title <- function(default = "Tuna model exploration") {
  if (exists("flow_assessment_label", inherits = TRUE)) {
    return(paste(get("flow_assessment_label", inherits = TRUE), "model exploration"))
  }
  default
}

assert_plan_table <- function(x, label) {
  if (!nrow(x)) {
    return(invisible(TRUE))
  }
  missing <- setdiff(required_plan_columns, names(x))
  if (length(missing)) {
    stop(sprintf("%s is missing required columns: %s", label, paste(missing, collapse = ", ")), call. = FALSE)
  }
  duplicates <- x$JOB_KEY[duplicated(x$JOB_KEY)]
  if (length(duplicates)) {
    stop(sprintf("%s has duplicate JOB_KEY values: %s", label, paste(unique(duplicates), collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

plan_has_columns <- function(x, columns) {
  all(columns %in% names(x))
}

plan_bind_rows <- function(...) {
  tables <- list(...)
  if (length(tables) == 1L && is.list(tables[[1]]) && !is.data.frame(tables[[1]])) {
    tables <- tables[[1]]
  }
  tables <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, tables)
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

plan_nonempty <- function(value, default = "") {
  if (is.null(value) || !length(value) || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) {
    return(default)
  }
  as.character(value[[1]])
}

build_starter_plan <- function() {
  list(
    base = common_env(base_models),
    sensitivity = common_env(sensitivity_models),
    diagnostics = common_env(diagnostics_runs),
    plot = common_env(plot_runs),
    report = common_env(report_runs)
  )
}

build_recipe_plan <- function(bases = base_models,
                              sensitivity_recipes = starter_sensitivity_recipes,
                              diagnostics_recipes = starter_diagnostics_recipes,
                              plots = plot_runs,
                              reports = report_runs,
                              include_base_diagnostics = TRUE,
                              include_sensitivity_diagnostics = TRUE) {
  bases <- common_env(bases)
  sensitivity_rows <- common_env(build_sensitivity_rows(bases, sensitivity_recipes))
  diagnostic_inputs <- plan_bind_rows(
    if (isTRUE(include_base_diagnostics)) bases else NULL,
    if (isTRUE(include_sensitivity_diagnostics)) sensitivity_rows else NULL
  )
  diagnostic_input_tasks <- ifelse(
    diagnostic_inputs$JOB_KEY %in% bases$JOB_KEY,
    plan_task_code("base"),
    plan_task_code("sensitivity")
  )
  diagnostic_rows <- common_env(build_diagnostics_rows(
    diagnostic_inputs,
    diagnostics_recipes,
    diagnostic_input_tasks
  ))
  plots <- common_env(plots)
  reports <- common_env(reports)
  if (nrow(plots) && nrow(diagnostic_rows)) {
    plots$INPUT_TASK <- plan_task_code("diagnostics")
    plots$INPUT_KEY <- paste(diagnostic_rows$JOB_KEY, collapse = ",")
  }
  if (nrow(reports) && nrow(plots)) {
    reports$INPUT_TASK <- plan_task_code("plot")
    reports$INPUT_KEY <- paste(plots$JOB_KEY, collapse = ",")
  }
  list(
    base = bases,
    sensitivity = sensitivity_rows,
    diagnostics = diagnostic_rows,
    plot = plots,
    report = reports
  )
}

diagnostic_recipes <- function() {
  data.frame(
    DIAGNOSTIC_TOKEN = c("JitterSmoke", "Retro4", "HessianP1"),
    DIAGNOSTIC_GROUP = c("jitter", "retro", "hessian"),
    MAKE_TARGETS = c("jitter_smoke", "retro", "hessian"),
    JITTER_SEED = c(40, NA, NA),
    RETRO_PEEL = c(NA, 4, NA),
    HESSIAN_PART = c(NA, NA, 1),
    NSPLIT = c(NA, NA, 5),
    DESCRIPTION = c(
      "Light jitter smoke test for fast screening.",
      "Four-year retrospective diagnostic.",
      "Hessian part 1 diagnostic."
    ),
    stringsAsFactors = FALSE
  )
}

plot_recipes <- function() {
  data.frame(
    PLOT_TOKEN = "Overview",
    PLOT_BACKEND = "manifest",
    DESCRIPTION = "Model exploration overview plot package.",
    stringsAsFactors = FALSE
  )
}

report_recipes <- function() {
  data.frame(
    REPORT_TOKEN = "Summary",
    DESCRIPTION = "Quarto summary report for selected model exploration outputs.",
    stringsAsFactors = FALSE
  )
}

expand_sensitivities <- function(bases, sensitivities) {
  # Create one sensitivity job per base x sensitivity recipe.
  # The generated JOB_KEY keeps both tokens so the dependency graph is readable.
  rows <- list()
  for (base_index in seq_len(nrow(bases))) {
    base <- bases[base_index, , drop = FALSE]
    for (sens_index in seq_len(nrow(sensitivities))) {
      sens <- sensitivities[sens_index, , drop = FALSE]
      row <- sens
      row$BASE_MODEL_KEY <- base$JOB_KEY
      row$INPUT_TASK <- plan_task_code("base")
      row$INPUT_KEY <- base$JOB_KEY
      row$JOB_KEY <- paste(base$JOB_KEY, sens$CHANGE_TOKEN, sep = "--")
      row$RUN_LABEL <- row$JOB_KEY
      row$MODEL_KEY <- row$JOB_KEY
      row$MODEL_TOKEN <- paste(base$MODEL_TOKEN, sens$CHANGE_TOKEN, sep = "_")
      row$MODEL_NAME <- paste(base$MODEL_NAME, sens$MODEL_NAME, sep = " | ")
      row$JOB_TITLE <- paste("Sensitivity:", row$MODEL_TOKEN)
      row$JOB_DESCRIPTION <- paste(sens$CHANGE_SUMMARY, "Base:", base$MODEL_TOKEN)
      rows[[length(rows) + 1L]] <- row
    }
  }
  plan_bind_rows(rows)
}

normalize_sensitivity_recipes <- function(sensitivities) {
  sensitivities <- as.data.frame(sensitivities, stringsAsFactors = FALSE)
  if (!nrow(sensitivities)) {
    return(sensitivities)
  }
  if (!"CHANGE_TOKEN" %in% names(sensitivities) && "RECIPE_TOKEN" %in% names(sensitivities)) {
    sensitivities$CHANGE_TOKEN <- sensitivities$RECIPE_TOKEN
  }
  if (!"CHANGE_GROUP" %in% names(sensitivities) && "RECIPE_FAMILY" %in% names(sensitivities)) {
    sensitivities$CHANGE_GROUP <- sensitivities$RECIPE_FAMILY
  }
  if (!"CHANGE_SUMMARY" %in% names(sensitivities)) {
    if ("CHANGE_DETAIL" %in% names(sensitivities)) {
      sensitivities$CHANGE_SUMMARY <- sensitivities$CHANGE_DETAIL
    } else if ("RECIPE_LABEL" %in% names(sensitivities)) {
      sensitivities$CHANGE_SUMMARY <- sensitivities$RECIPE_LABEL
    }
  }
  if (!"MODEL_NAME" %in% names(sensitivities)) {
    if ("RECIPE_LABEL" %in% names(sensitivities)) {
      sensitivities$MODEL_NAME <- sensitivities$RECIPE_LABEL
    } else {
      sensitivities$MODEL_NAME <- sensitivities$CHANGE_TOKEN
    }
  }
  missing <- setdiff(c("CHANGE_TOKEN", "CHANGE_GROUP", "CHANGE_SUMMARY", "MODEL_NAME"), names(sensitivities))
  if (length(missing)) {
    stop(
      "sensitivity recipes are missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  sensitivities
}

plan_uses_workflow_recipe_builders <- function(sensitivities, diagnostics) {
  exists("build_sensitivity_rows", inherits = TRUE) &&
    exists("build_diagnostics_rows", inherits = TRUE) &&
    (any(c("RECIPE_TOKEN", "RECIPE_KEY") %in% names(sensitivities)) ||
      any(c("RECIPE_TOKEN", "RECIPE_KEY") %in% names(diagnostics)))
}

expand_diagnostics <- function(model_rows, diagnostics) {
  # Create diagnostics from base rows, sensitivity rows, or both.
  # Each row points back to its parent through INPUT_TASK and INPUT_KEY.
  rows <- list()
  for (model_index in seq_len(nrow(model_rows))) {
    model <- model_rows[model_index, , drop = FALSE]
    input_task <- if (identical(as.character(model$CHANGE_GROUP), "base")) {
      plan_task_code("base")
    } else {
      plan_task_code("sensitivity")
    }
    for (diag_index in seq_len(nrow(diagnostics))) {
      diag <- diagnostics[diag_index, , drop = FALSE]
      token <- paste(model$MODEL_TOKEN, diag$DIAGNOSTIC_TOKEN, sep = "_")
      job_key <- paste(model$JOB_KEY, diag$DIAGNOSTIC_TOKEN, sep = "--")
      row <- data.frame(
        RUN_LABEL = job_key,
        JOB_KEY = job_key,
        MODEL_KEY = job_key,
        MODEL_TOKEN = token,
        MODEL_NAME = paste(model$MODEL_NAME, diag$DIAGNOSTIC_TOKEN, sep = " | "),
        BASE_MODEL_KEY = plan_nonempty(model$BASE_MODEL_KEY, if (identical(input_task, plan_task_code("base"))) model$JOB_KEY else ""),
        CHANGE_TOKEN = diag$DIAGNOSTIC_TOKEN,
        CHANGE_GROUP = paste("diagnostics", diag$DIAGNOSTIC_GROUP, sep = ":"),
        CHANGE_SUMMARY = diag$DESCRIPTION,
        INPUT_VARIANT = model$INPUT_VARIANT %||% "",
        PATCH_SCRIPT = "",
        PATCH_INPUT_DIR = model$BASE_DIR %||% "",
        PATCH_OUTPUT_DIR = model$BASE_DIR %||% "",
        JOB_TITLE = paste("Diagnostics:", token),
        JOB_DESCRIPTION = paste(diag$DESCRIPTION, "Input:", model$MODEL_TOKEN),
        INPUT_TASK = input_task,
        INPUT_KEY = model$JOB_KEY,
        MAKE_TARGETS = diag$MAKE_TARGETS,
        BASE_DIR = model$BASE_DIR,
        MODEL_DIR = model$MODEL_DIR,
        JITTER_SEED = diag$JITTER_SEED,
        RETRO_PEEL = diag$RETRO_PEEL,
        HESSIAN_PART = diag$HESSIAN_PART,
        NSPLIT = diag$NSPLIT,
        JITTER_SMOKE_ONLY = ifelse(identical(as.character(diag$MAKE_TARGETS), "jitter_smoke"), "1", ""),
        COLLECT_PATHS = paste(model$MODEL_DIR, model$BASE_DIR, sep = ","),
        stringsAsFactors = FALSE
      )
      rows[[length(rows) + 1L]] <- row
    }
  }
  plan_bind_rows(rows)
}

expand_plots <- function(diagnostics, plots, group_by = c("BASE_MODEL_KEY")) {
  # Group many diagnostics into plot jobs. The default makes one plot bundle per
  # base model; change group_by if you want plots by diagnostic type, change
  # group, or a custom grouping column.
  if (!nrow(diagnostics)) {
    return(data.frame())
  }
  group_by <- group_by[group_by %in% names(diagnostics)]
  if (!length(group_by)) {
    diagnostics$.plot_group <- "all"
    group_by <- ".plot_group"
  }
  group_values <- unique(diagnostics[group_by])
  rows <- list()
  for (group_index in seq_len(nrow(group_values))) {
    group <- group_values[group_index, , drop = FALSE]
    selector <- rep(TRUE, nrow(diagnostics))
    for (name in names(group)) {
      selector <- selector & diagnostics[[name]] == group[[name]]
    }
    keys <- diagnostics$JOB_KEY[selector]
    group_token <- paste(unlist(group, use.names = FALSE), collapse = "_")
    for (plot_index in seq_len(nrow(plots))) {
      plot <- plots[plot_index, , drop = FALSE]
      job_key <- paste("plot", plot$PLOT_TOKEN, group_token, sep = "--")
      rows[[length(rows) + 1L]] <- data.frame(
        RUN_LABEL = job_key,
        JOB_KEY = job_key,
        MODEL_KEY = job_key,
        MODEL_TOKEN = paste("Plot", plot$PLOT_TOKEN, group_token, sep = "_"),
        MODEL_NAME = paste("Plot", plot$PLOT_TOKEN, group_token),
        BASE_MODEL_KEY = plan_nonempty(group$BASE_MODEL_KEY, ""),
        CHANGE_TOKEN = plot$PLOT_TOKEN,
        CHANGE_GROUP = "plot",
        CHANGE_SUMMARY = plot$DESCRIPTION,
        JOB_TITLE = paste("Plot:", plot$PLOT_TOKEN, group_token),
        JOB_DESCRIPTION = plot$DESCRIPTION,
        INPUT_TASK = plan_task_code("diagnostics"),
        INPUT_KEY = paste(keys, collapse = ","),
        PLOT_TITLE = paste(plan_flow_title(), group_token),
        PLOT_BACKEND = plot$PLOT_BACKEND,
        stringsAsFactors = FALSE
      )
    }
  }
  plan_bind_rows(rows)
}

expand_reports <- function(plots, reports) {
  # Report rows usually consume one or more Plot jobs. INPUT_KEY is
  # comma-separated here because KflowKit accepts multiple keys for one task.
  rows <- list()
  for (report_index in seq_len(nrow(reports))) {
    report <- reports[report_index, , drop = FALSE]
    job_key <- paste("report", report$REPORT_TOKEN, sep = "--")
    rows[[length(rows) + 1L]] <- data.frame(
      RUN_LABEL = job_key,
      JOB_KEY = job_key,
      MODEL_KEY = job_key,
      MODEL_TOKEN = paste("Report", report$REPORT_TOKEN, sep = "_"),
      MODEL_NAME = paste("Report", report$REPORT_TOKEN),
      BASE_MODEL_KEY = "",
      CHANGE_TOKEN = report$REPORT_TOKEN,
      CHANGE_GROUP = "report",
      CHANGE_SUMMARY = report$DESCRIPTION,
      JOB_TITLE = paste("Report:", report$REPORT_TOKEN),
      JOB_DESCRIPTION = report$DESCRIPTION,
      INPUT_TASK = plan_task_code("plot"),
      INPUT_KEY = paste(plots$JOB_KEY, collapse = ","),
      REPORT_TITLE = paste(plan_flow_title(), report$REPORT_TOKEN),
      stringsAsFactors = FALSE
    )
  }
  plan_bind_rows(rows)
}

build_exploration_plan <- function(bases,
                                   sensitivities,
                                   diagnostics = NULL,
                                   plots = NULL,
                                   reports = NULL,
                                   include_base_diagnostics = TRUE,
                                   include_sensitivity_diagnostics = TRUE) {
  bases <- common_env(bases)
  assert_plan_table(bases, "bases")

  if (is.null(diagnostics)) {
    diagnostics <- if (exists("starter_diagnostics_recipes", inherits = TRUE)) {
      get("starter_diagnostics_recipes", inherits = TRUE)
    } else {
      diagnostic_recipes()
    }
  }
  if (is.null(plots)) {
    plots <- if (exists("plot_runs", inherits = TRUE)) {
      get("plot_runs", inherits = TRUE)
    } else {
      plot_recipes()
    }
  }
  if (is.null(reports)) {
    reports <- if (exists("report_runs", inherits = TRUE)) {
      get("report_runs", inherits = TRUE)
    } else {
      report_recipes()
    }
  }

  if (plan_uses_workflow_recipe_builders(sensitivities, diagnostics)) {
    plan <- build_recipe_plan(
      bases = bases,
      sensitivity_recipes = sensitivities,
      diagnostics_recipes = diagnostics,
      plots = plots,
      reports = reports,
      include_base_diagnostics = include_base_diagnostics,
      include_sensitivity_diagnostics = include_sensitivity_diagnostics
    )
    validate_plan(plan, yaml_paths = NULL)
    return(plan)
  }

  if (plan_has_columns(sensitivities, c(required_plan_columns, "INPUT_TASK", "INPUT_KEY"))) {
    sensitivity_rows <- common_env(sensitivities)
  } else {
    sensitivity_rows <- common_env(expand_sensitivities(bases, normalize_sensitivity_recipes(sensitivities)))
  }
  assert_plan_table(sensitivity_rows, "sensitivity rows")
  diagnostic_inputs <- plan_bind_rows(
    if (isTRUE(include_base_diagnostics)) bases else NULL,
    if (isTRUE(include_sensitivity_diagnostics)) sensitivity_rows else NULL
  )
  diagnostic_rows <- expand_diagnostics(diagnostic_inputs, diagnostics)
  plot_rows <- expand_plots(diagnostic_rows, plots)
  report_rows <- expand_reports(plot_rows, reports)

  list(
    base = bases,
    sensitivity = common_env(sensitivity_rows),
    diagnostics = common_env(diagnostic_rows),
    plot = common_env(plot_rows),
    report = common_env(report_rows)
  )
}

plan_yaml_optional <- function(path) {
  if (!file.exists(path)) {
    stop("Kflow YAML file not found: ", path, call. = FALSE)
  }
  if (requireNamespace("yaml", quietly = TRUE)) {
    parsed <- yaml::read_yaml(path)
    return(as.character(parsed$job_config$optional %||% character()))
  }
  lines <- readLines(path, warn = FALSE)
  start <- grep("^[[:space:]]*optional:[[:space:]]*$", lines)
  if (!length(start)) {
    return(character())
  }
  lines <- lines[(start[[1]] + 1L):length(lines)]
  optional <- character()
  for (line in lines) {
    if (grepl("^[[:space:]]{4,}-[[:space:]]+", line)) {
      optional <- c(optional, sub("^[[:space:]]*-[[:space:]]+", "", line))
    } else if (nzchar(trimws(line)) && grepl("^[^[:space:]]|^[[:space:]]{0,2}[^-[:space:]]", line)) {
      break
    }
  }
  optional
}

default_yaml_paths <- function() {
  c(
    base = "base/kflow.yaml",
    sensitivity = "sensitivity/kflow.yaml",
    diagnostics = "diagnostics/kflow.yaml",
    plot = "plot/kflow.yaml",
    report = "report/kflow.yaml"
  )
}

validate_plan_yaml_coverage <- function(plan, yaml_paths = default_yaml_paths()) {
  rows <- list()
  for (stage in intersect(names(plan), names(yaml_paths))) {
    optional <- plan_yaml_optional(yaml_paths[[stage]])
    missing <- setdiff(names(plan[[stage]]), optional)
    rows[[length(rows) + 1L]] <- data.frame(
      stage = stage,
      yaml = yaml_paths[[stage]],
      missing = paste(missing, collapse = ","),
      stringsAsFactors = FALSE
    )
    if (length(missing)) {
      stop(
        "Plan table for ", stage, " has columns not listed in ",
        yaml_paths[[stage]], ": ", paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

validate_plan <- function(plan, yaml_paths = default_yaml_paths()) {
  for (stage in names(plan)) {
    assert_plan_table(plan[[stage]], stage)
  }
  if (!is.null(yaml_paths)) {
    validate_plan_yaml_coverage(plan, yaml_paths)
  }
  invisible(TRUE)
}

preview_plan <- function(plan) {
  counts <- data.frame(
    stage = names(plan),
    jobs = vapply(plan, nrow, integer(1)),
    stringsAsFactors = FALSE
  )
  print(counts)
  invisible(counts)
}

write_plan_tables <- function(plan, dir = "plan-output") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (name in names(plan)) {
    utils::write.csv(plan[[name]], file.path(dir, paste0(name, ".csv")), row.names = FALSE)
  }
  invisible(normalizePath(dir, winslash = "/", mustWork = FALSE))
}
