kflow_infix_default <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

`%||%` <- kflow_infix_default

kflow_env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

kflow_bool <- function(name, default = FALSE) {
  value <- tolower(kflow_env(name, if (isTRUE(default)) "true" else "false"))
  value %in% c("1", "true", "yes", "y")
}

kflow_split <- function(value) {
  if (is.null(value) || !nzchar(value)) {
    return(character())
  }
  parts <- unlist(strsplit(value, "[,\n\r]+"))
  trimws(parts[nzchar(trimws(parts))])
}

kflow_out_dir <- function() {
  out_dir <- kflow_env("OUTPUT_DIR", "outputs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir
}

kflow_input_dir <- function() {
  input_dir <- kflow_env("INPUT_DIR", "inputs")
  dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
  input_dir
}

kflow_log_file <- function(out_dir, name = "job.log") {
  log_dir <- file.path(out_dir, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(log_dir, name)
}

kflow_note <- function(..., log_file = NULL) {
  text <- paste0(...)
  message(text)
  if (!is.null(log_file)) {
    cat(text, "\n", file = log_file, append = TRUE)
  }
}

kflow_secret_env_names <- function() {
  c(
    "GIT_PAT",
    "GITHUB_PAT",
    "GH_TOKEN",
    "KFLOW_API_TOKEN",
    "KFLOW_GITHUB_TOKEN",
    "KFLOW_PERSONAL_TOKEN"
  )
}

kflow_sanitize_shell_command <- function(command) {
  unset <- paste(sprintf("unset %s", kflow_secret_env_names()), collapse = "; ")
  paste(unset, command, sep = "; ")
}

kflow_run_shell <- function(command, workdir = getwd(), log_file = NULL, sanitize_env = FALSE) {
  old <- setwd(workdir)
  on.exit(setwd(old), add = TRUE)
  if (isTRUE(sanitize_env)) {
    command <- kflow_sanitize_shell_command(command)
  }
  if (is.null(log_file)) {
    status <- system2("bash", c("-lc", shQuote(command)))
  } else {
    log_file <- normalizePath(log_file, winslash = "/", mustWork = FALSE)
    dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    wrapped <- sprintf("(%s) >> %s 2>&1", command, shQuote(log_file))
    status <- system2("bash", c("-lc", shQuote(wrapped)))
  }
  if (!identical(status, 0L)) {
    stop(sprintf("Command failed with exit status %s: %s", status, command), call. = FALSE)
  }
  invisible(TRUE)
}

kflow_clone_source <- function(work_dir = "work/source", log_file = NULL) {
  source_repo <- kflow_env("SOURCE_REPO", kflow_env("FLOW_SOURCE_REPO", "PacificCommunity/ofp-sam-bet2026-inputs"))
  source_ref <- kflow_env("SOURCE_REF", kflow_env("FLOW_SOURCE_REF", "main"))
  source_url <- sprintf("https://github.com/%s.git", source_repo)
  unlink(work_dir, recursive = TRUE, force = TRUE)
  dir.create(dirname(work_dir), recursive = TRUE, showWarnings = FALSE)

  clone_branch <- if (nzchar(source_ref)) {
    sprintf(
      "git clone --depth 1 --branch %s %s %s",
      shQuote(source_ref), shQuote(source_url), shQuote(work_dir)
    )
  } else {
    sprintf("git clone --depth 1 %s %s", shQuote(source_url), shQuote(work_dir))
  }
  ok <- tryCatch({
    kflow_run_shell(clone_branch, log_file = log_file)
    TRUE
  }, error = function(e) {
    kflow_note("Branch/tag clone failed, retrying with checkout: ", conditionMessage(e), log_file = log_file)
    FALSE
  })

  if (!isTRUE(ok)) {
    unlink(work_dir, recursive = TRUE, force = TRUE)
    kflow_run_shell(
      sprintf("git clone --depth 1 %s %s", shQuote(source_url), shQuote(work_dir)),
      log_file = log_file
    )
    if (nzchar(source_ref)) {
      kflow_run_shell(sprintf("git fetch --depth 1 origin %s || true", shQuote(source_ref)), workdir = work_dir, log_file = log_file)
      kflow_run_shell(sprintf("git checkout %s", shQuote(source_ref)), workdir = work_dir, log_file = log_file)
    }
  }

  normalizePath(work_dir, winslash = "/", mustWork = TRUE)
}

kflow_prepare_source_path <- function(source_path, work_dir = "work/source", log_file = NULL) {
  source_path <- normalizePath(source_path, winslash = "/", mustWork = TRUE)
  unlink(work_dir, recursive = TRUE, force = TRUE)
  dir.create(dirname(work_dir), recursive = TRUE, showWarnings = FALSE)
  kflow_copy_tree(source_path, work_dir)
  exe_files <- list.files(file.path(work_dir, "mfcl", "exe"), pattern = "^mfcl", full.names = TRUE)
  if (length(exe_files)) {
    Sys.chmod(exe_files, mode = "0755", use_umask = FALSE)
  }
  kflow_note("Prepared source from local SOURCE_PATH: ", source_path, log_file = log_file)
  normalizePath(work_dir, winslash = "/", mustWork = TRUE)
}

kflow_flow_root <- function() {
  candidates <- unique(c(
    kflow_env("KFLOW_FLOW_ROOT", ""),
    file.path(getwd(), ".."),
    getwd()
  ))
  candidates <- candidates[nzchar(candidates)]
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  hits <- candidates[file.exists(file.path(candidates, "R", "workflow.R"))]
  if (length(hits)) {
    return(normalizePath(hits[[1]], winslash = "/", mustWork = TRUE))
  }
  normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
}

kflow_copy_tree <- function(from, to) {
  if (!dir.exists(from)) {
    stop(sprintf("Directory does not exist: %s", from), call. = FALSE)
  }
  unlink(to, recursive = TRUE, force = TRUE)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  entries <- list.files(from, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(entries)) {
    ok <- file.copy(entries, to, overwrite = TRUE, recursive = TRUE, copy.date = TRUE)
    if (any(!ok)) {
      stop(sprintf("Failed to copy some files from %s to %s", from, to), call. = FALSE)
    }
  }
  invisible(normalizePath(to, winslash = "/", mustWork = TRUE))
}

kflow_prepare_flow_source <- function(work_dir = "work/source", log_file = NULL) {
  flow_root <- kflow_flow_root()
  mfcl_dir <- file.path(flow_root, "mfcl")
  if (!dir.exists(mfcl_dir)) {
    stop("Flow checkout source requested, but this checkout has no mfcl/ directory.", call. = FALSE)
  }
  unlink(work_dir, recursive = TRUE, force = TRUE)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  kflow_copy_tree(mfcl_dir, file.path(work_dir, "mfcl"))
  exe_files <- list.files(file.path(work_dir, "mfcl", "exe"), pattern = "^mfcl", full.names = TRUE)
  if (length(exe_files)) {
    Sys.chmod(exe_files, mode = "0755", use_umask = FALSE)
  }
  kflow_note("Prepared flow checkout source from ", flow_root, log_file = log_file)
  normalizePath(work_dir, winslash = "/", mustWork = TRUE)
}

kflow_use_flow_source <- function() {
  source_repo <- tolower(kflow_env("SOURCE_REPO", kflow_env("FLOW_SOURCE_REPO", "PacificCommunity/ofp-sam-bet2026-inputs")))
  kflow_bool("USE_FLOW_SOURCE", FALSE) ||
  kflow_bool("USE_LOCAL_SOURCE", FALSE) ||
    source_repo %in% c("flow_checkout", "local", ".", "flow", "this")
}

kflow_checkout_source <- function(work_dir = "work/source", log_file = NULL) {
  source_path <- kflow_env("SOURCE_PATH", kflow_env("FLOW_SOURCE_PATH", ""))
  if (nzchar(source_path)) {
    kflow_prepare_source_path(source_path, work_dir, log_file = log_file)
  } else if (kflow_use_flow_source()) {
    kflow_prepare_flow_source(work_dir, log_file = log_file)
  } else {
    kflow_clone_source(work_dir, log_file = log_file)
  }
}

kflow_write_manifest <- function(root, out_file) {
  files <- if (dir.exists(root)) list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE) else character()
  if (!length(files)) {
    manifest <- data.frame(path = character(), size = numeric(), is_dir = logical(), stringsAsFactors = FALSE)
  } else {
    info <- file.info(files)
    rel <- substr(normalizePath(files, winslash = "/", mustWork = FALSE), nchar(normalizePath(root, winslash = "/", mustWork = FALSE)) + 2L, 100000L)
    manifest <- data.frame(
      path = rel,
      size = info$size,
      is_dir = info$isdir,
      modified = as.character(info$mtime),
      stringsAsFactors = FALSE
    )
  }
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(manifest, out_file, row.names = FALSE)
  invisible(manifest)
}

kflow_copy_path <- function(path, root, dest_root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  rel <- substr(path, nchar(root) + 2L, nchar(path))
  dest <- file.path(dest_root, rel)
  if (dir.exists(path)) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    entries <- list.files(path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(entries)) {
      file.copy(entries, dest, overwrite = TRUE, recursive = TRUE, copy.date = TRUE)
    }
  } else {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(path, dest, overwrite = TRUE, copy.date = TRUE)
  }
  invisible(dest)
}

kflow_collect_paths <- function(source_dir, out_dir, patterns, dest_name = "source-artifacts") {
  patterns <- kflow_split(patterns)
  dest_root <- file.path(out_dir, dest_name)
  dir.create(dest_root, recursive = TRUE, showWarnings = FALSE)
  matches <- character()
  for (pattern in patterns) {
    matches <- c(matches, Sys.glob(file.path(source_dir, pattern)))
  }
  matches <- unique(matches[file.exists(matches)])
  for (path in matches) {
    kflow_copy_path(path, source_dir, dest_root)
  }
  utils::write.csv(
    data.frame(pattern = patterns, matched = length(matches), stringsAsFactors = FALSE),
    file.path(out_dir, "collected-paths.csv"),
    row.names = FALSE
  )
  kflow_write_manifest(dest_root, file.path(out_dir, "source-artifacts-manifest.csv"))
  invisible(matches)
}

kflow_sync_input_artifacts <- function(input_dir, source_dir, artifact_dir_name = "source-artifacts", log_file = NULL) {
  if (!dir.exists(input_dir)) {
    return(character())
  }
  dirs <- list.dirs(input_dir, recursive = TRUE, full.names = TRUE)
  roots <- dirs[basename(dirs) == artifact_dir_name]
  copied <- character()
  for (root in roots) {
    entries <- list.files(root, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    for (entry in entries) {
      kflow_copy_path(entry, root, source_dir)
      copied <- c(copied, entry)
    }
  }
  if (length(copied)) {
    kflow_note("Copied ", length(copied), " upstream artifact roots into source checkout.", log_file = log_file)
  }
  invisible(copied)
}

kflow_copy_seed_files <- function(input_dir, source_dir, pattern, copy_to, log_file = NULL) {
  if (!nzchar(pattern) || !nzchar(copy_to) || !dir.exists(input_dir)) {
    return(invisible(character()))
  }
  all_files <- list.files(input_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  hits <- all_files[grepl(pattern, all_files)]
  if (!length(hits)) {
    kflow_note("No upstream seed files matched pattern: ", pattern, log_file = log_file)
    return(invisible(character()))
  }
  dest_dir <- file.path(source_dir, copy_to)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(hits, dest_dir, overwrite = TRUE, recursive = TRUE)
  kflow_note("Copied ", length(hits), " seed files into ", copy_to, log_file = log_file)
  invisible(hits)
}

kflow_set_common_make_env <- function() {
  values <- c(
    program_path = kflow_env("PROGRAM_PATH", kflow_env("FLOW_MFCL_PROGRAM", "/home/mfcl/mfclo64")),
    base_dir = kflow_env("BASE_DIR", ""),
    model_dir = kflow_env("MODEL_DIR", ""),
    hessian_part = kflow_env("HESSIAN_PART", "1"),
    nsplit = kflow_env("NSPLIT", "5"),
    jitter_seed = kflow_env("JITTER_SEED", "40"),
    jitter_cv = kflow_env("JITTER_CV", "0.2"),
    jitter_smoke_only = kflow_env("JITTER_SMOKE_ONLY", "1"),
    retro_peel = kflow_env("RETRO_PEEL", "4"),
    n_mixing_periods = kflow_env("N_MIXING_PERIODS", "2")
  )
  values <- values[nzchar(values)]
  do.call(Sys.setenv, as.list(values))
  invisible(values)
}

kflow_make_vars <- function() {
  value <- kflow_env("MAKE_VARS", "")
  if (!nzchar(value)) {
    return("")
  }
  value
}

kflow_existing_script <- function(script) {
  if (!nzchar(script)) {
    return("")
  }
  candidates <- c(
    script,
    file.path(getwd(), script),
    file.path(dirname(getwd()), script)
  )
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) "" else hit
}

kflow_run_make_targets <- function(source_dir, targets, log_file = NULL) {
  targets <- kflow_split(targets)
  if (!length(targets)) {
    stop("No make targets were provided.", call. = FALSE)
  }
  kflow_set_common_make_env()
  make_vars <- kflow_make_vars()
  for (target in targets) {
    kflow_note("Running make target: ", target, log_file = log_file)
    command <- paste(c("make", target, make_vars), collapse = " ")
    kflow_run_shell(command, workdir = source_dir, log_file = log_file, sanitize_env = TRUE)
  }
  invisible(targets)
}

kflow_mfcl_program <- function(source_dir) {
  program <- kflow_env("PROGRAM_PATH", kflow_env("FLOW_MFCL_PROGRAM", "/home/mfcl/mfclo64"))
  program <- if (grepl("^/", program)) program else file.path(source_dir, program)
  if (!file.exists(program)) {
    exe_dir <- file.path(source_dir, "mfcl", "exe")
    candidates <- list.files(exe_dir, pattern = "^mfclo64", full.names = TRUE)
    if (length(candidates)) {
      program <- candidates[[1]]
    }
  }
  if (!file.exists(program)) {
    stop(sprintf("MFCL executable was not found: %s", program), call. = FALSE)
  }
  Sys.chmod(program, mode = "0755", use_umask = FALSE)
  normalizePath(program, winslash = "/", mustWork = TRUE)
}

kflow_write_smoke_depletion <- function(target_dir, stage) {
  years <- 2017:2023
  regions <- paste0("Region ", 1:4)
  grid <- expand.grid(year = years, region = regions, stringsAsFactors = FALSE)
  trend <- stats::setNames(seq(0.43, 0.34, length.out = length(years)), years)
  region_offset <- stats::setNames(c(0.025, 0.005, -0.010, -0.025), regions)
  token <- kflow_env("CHANGE_TOKEN", kflow_env("MODEL_TOKEN", "Base"))
  token_offset <- (sum(utf8ToInt(token)) %% 7) / 200
  stage_offset <- switch(stage, base = 0, sensitivity = -0.025, diagnostics = 0.010, 0)
  value <- trend[as.character(grid$year)] + region_offset[grid$region] + token_offset + stage_offset
  grid$depletion <- round(pmax(0.05, pmin(0.95, as.numeric(value))), 3)
  grid$stage <- stage
  grid$model_key <- kflow_env("MODEL_KEY", kflow_env("JOB_KEY", ""))
  grid$model_token <- kflow_env("MODEL_TOKEN", kflow_env("RUN_LABEL", ""))
  grid$model_label <- kflow_env("MODEL_LABEL", grid$model_token[[1]])
  grid$plot_label <- kflow_env("PLOT_LABEL", grid$model_token[[1]])
  grid$report_label <- kflow_env("REPORT_LABEL", grid$model_label[[1]])
  grid$change_token <- token
  grid$change_group <- kflow_env("CHANGE_GROUP", "")
  grid$change_detail <- kflow_env("CHANGE_DETAIL", kflow_env("CHANGE_SUMMARY", ""))
  grid$parent_model_key <- kflow_env("PARENT_MODEL_KEY", kflow_env("BASE_MODEL_KEY", ""))
  grid$parent_model_token <- kflow_env("PARENT_MODEL_TOKEN", "")
  grid$recipe_token <- kflow_env("RECIPE_TOKEN", token)
  grid$recipe_family <- kflow_env("RECIPE_FAMILY", kflow_env("CHANGE_GROUP", ""))
  grid$recipe_label <- kflow_env("RECIPE_LABEL", token)
  grid$source <- "mfcl_makepar_smoke"
  out_file <- file.path(target_dir, "depletion-smoke.csv")
  utils::write.csv(grid, out_file, row.names = FALSE)
  invisible(grid)
}

kflow_mfcl_log_summary <- function(log_file, out_dir, model_dir) {
  lines <- if (!is.null(log_file) && file.exists(log_file)) {
    readLines(log_file, warn = FALSE)
  } else {
    character()
  }
  error_hits <- grep("\\b(ERROR|Error)\\b", lines, value = TRUE)
  warning_hits <- grep("\\b(WARNING|Warning)\\b", lines, value = TRUE)
  summary <- data.frame(
    log_file = if (!is.null(log_file)) log_file else "",
    error_count = length(error_hits),
    warning_count = length(warning_hits),
    fail_on_log_error = kflow_bool("MFCL_SMOKE_FAIL_ON_LOG_ERROR", FALSE),
    stringsAsFactors = FALSE
  )
  utils::write.csv(summary, file.path(out_dir, "mfcl-log-summary.csv"), row.names = FALSE)
  utils::write.csv(summary, file.path(model_dir, "mfcl-log-summary.csv"), row.names = FALSE)
  if (length(error_hits)) {
    writeLines(utils::head(error_hits, 30), file.path(out_dir, "mfcl-log-errors.txt"))
    writeLines(utils::head(error_hits, 30), file.path(model_dir, "mfcl-log-errors.txt"))
    text <- paste0(
      "MFCL log contains ", length(error_hits),
      " flagged line(s). See mfcl-log-summary.csv and mfcl-log-errors.txt."
    )
    cat(text, "\n")
    if (!is.null(log_file)) {
      cat(text, "\n", file = log_file, append = TRUE)
    }
    if (kflow_bool("MFCL_SMOKE_FAIL_ON_LOG_ERROR", FALSE)) {
      stop("MFCL smoke log contains error-looking lines and MFCL_SMOKE_FAIL_ON_LOG_ERROR=1.", call. = FALSE)
    }
  }
  summary
}

kflow_run_mfcl_smoke <- function(source_dir, out_dir, stage, log_file = NULL) {
  base_dir <- file.path(source_dir, kflow_env("BASE_DIR", kflow_env("FLOW_BASE_INPUT_DIR", "mfcl/inputs/2023_4region_1007")))
  model_dir <- file.path(source_dir, kflow_env("MODEL_DIR", file.path("model", kflow_env("JOB_KEY", "smoke"))))
  if (!dir.exists(base_dir)) {
    stop(sprintf("MFCL input directory was not found: %s", base_dir), call. = FALSE)
  }

  kflow_copy_tree(base_dir, model_dir)
  program <- kflow_mfcl_program(source_dir)
  frq <- kflow_env("MFCL_FRQ", "")
  ini <- kflow_env("MFCL_INI", "")
  if (!nzchar(frq)) {
    frq_candidates <- list.files(model_dir, pattern = "\\.frq$", full.names = FALSE, ignore.case = TRUE)
    if (!length(frq_candidates)) {
      stop("No .frq file found in ", model_dir, call. = FALSE)
    }
    if (length(frq_candidates) > 1L) {
      kflow_note("Multiple .frq files found; using first: ", frq_candidates[[1]], log_file = log_file)
    }
    frq <- frq_candidates[[1]]
  }
  if (!nzchar(ini)) {
    ini_candidates <- list.files(model_dir, pattern = "\\.ini$", full.names = FALSE, ignore.case = TRUE)
    if (!length(ini_candidates)) {
      stop("No .ini file found in ", model_dir, call. = FALSE)
    }
    if (length(ini_candidates) > 1L) {
      kflow_note("Multiple .ini files found; using first: ", ini_candidates[[1]], log_file = log_file)
    }
    ini <- ini_candidates[[1]]
  }
  par <- kflow_env("SMOKE_PAR", "00.par")
  command <- paste(
    shQuote(program),
    shQuote(frq),
    shQuote(ini),
    shQuote(par),
    "-makepar"
  )
  kflow_note("Running MFCL smoke makepar in ", model_dir, log_file = log_file)
  kflow_run_shell(command, workdir = model_dir, log_file = log_file, sanitize_env = TRUE)

  output_par <- file.path(model_dir, par)
  if (!file.exists(output_par)) {
    stop(sprintf("MFCL smoke run did not create %s", output_par), call. = FALSE)
  }
  log_summary <- kflow_mfcl_log_summary(log_file, out_dir, model_dir)
  smoke <- data.frame(
    stage = stage,
    run_label = kflow_env("RUN_LABEL", ""),
    job_key = kflow_env("JOB_KEY", ""),
    model_key = kflow_env("MODEL_KEY", ""),
    model_token = kflow_env("MODEL_TOKEN", ""),
    change_token = kflow_env("CHANGE_TOKEN", ""),
    input_dir = kflow_env("BASE_DIR", ""),
    model_dir = kflow_env("MODEL_DIR", ""),
    executable = basename(program),
    frq = frq,
    ini = ini,
    par = par,
    par_size = file.info(output_par)$size,
    mfcl_log_error_count = log_summary$error_count[[1]],
    mfcl_log_warning_count = log_summary$warning_count[[1]],
    stringsAsFactors = FALSE
  )
  utils::write.csv(smoke, file.path(model_dir, "mfcl-smoke-summary.csv"), row.names = FALSE)
  utils::write.csv(smoke, file.path(out_dir, "mfcl-smoke-summary.csv"), row.names = FALSE)
  kflow_write_smoke_depletion(model_dir, stage)
  kflow_write_manifest(model_dir, file.path(model_dir, "model-manifest.csv"))
  invisible(smoke)
}

kflow_run_diagnostics_smoke <- function(source_dir, out_dir, stage = "diagnostics", log_file = NULL) {
  model_dir <- file.path(source_dir, kflow_env("MODEL_DIR", file.path("model", kflow_env("JOB_KEY", "diagnostics"))))
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  depletion_files <- list.files(source_dir, pattern = "^depletion-smoke[.]csv$", recursive = TRUE, full.names = TRUE)
  depletion <- kflow_read_csv_union(depletion_files)
  if (nrow(depletion)) {
    parent <- depletion
    parent$model_role <- "parent"
    diagnostic <- depletion
    diagnostic$stage <- stage
    diagnostic$model_key <- kflow_env("MODEL_KEY", kflow_env("JOB_KEY", ""))
    diagnostic$model_token <- kflow_env("MODEL_TOKEN", kflow_env("RUN_LABEL", ""))
    diagnostic$model_label <- kflow_env("MODEL_LABEL", diagnostic$model_token[[1]])
    diagnostic$plot_label <- kflow_env("PLOT_LABEL", diagnostic$model_token[[1]])
    diagnostic$report_label <- kflow_env("REPORT_LABEL", diagnostic$model_label[[1]])
    diagnostic$change_token <- kflow_env("CHANGE_TOKEN", "JitterSmoke")
    diagnostic$change_detail <- kflow_env("CHANGE_DETAIL", kflow_env("CHANGE_SUMMARY", ""))
    diagnostic$parent_model_key <- kflow_env("PARENT_MODEL_KEY", kflow_env("INPUT_KEY", ""))
    diagnostic$parent_model_token <- kflow_env("PARENT_MODEL_TOKEN", "")
    diagnostic$recipe_token <- kflow_env("RECIPE_TOKEN", diagnostic$change_token[[1]])
    diagnostic$recipe_family <- kflow_env("RECIPE_FAMILY", kflow_env("CHANGE_GROUP", ""))
    diagnostic$recipe_label <- kflow_env("RECIPE_LABEL", diagnostic$change_token[[1]])
    diagnostic$source <- "diagnostics_smoke_from_parent"
    diagnostic$model_role <- "diagnostics"
    diagnostic$depletion <- round(pmax(0.05, pmin(0.95, as.numeric(diagnostic$depletion) + 0.005)), 3)
    depletion <- rbind(parent, diagnostic)
    utils::write.csv(depletion, file.path(model_dir, "depletion-smoke.csv"), row.names = FALSE)
  } else {
    depletion <- kflow_write_smoke_depletion(model_dir, stage)
    depletion$model_role <- "diagnostics"
    utils::write.csv(depletion, file.path(model_dir, "depletion-smoke.csv"), row.names = FALSE)
  }
  final_year <- suppressWarnings(max(as.integer(depletion$year), na.rm = TRUE))
  final <- depletion[depletion$year == final_year & depletion$model_key == kflow_env("MODEL_KEY", kflow_env("JOB_KEY", "")), , drop = FALSE]
  diagnostics <- data.frame(
    stage = stage,
    run_label = kflow_env("RUN_LABEL", ""),
    job_key = kflow_env("JOB_KEY", ""),
    parent_task = kflow_env("INPUT_TASK", ""),
    parent_key = kflow_env("INPUT_KEY", ""),
    diagnostic = kflow_env("CHANGE_TOKEN", "JitterSmoke"),
    final_year = final_year,
    mean_final_depletion = round(mean(as.numeric(final$depletion), na.rm = TRUE), 3),
    input_depletion_files = length(depletion_files),
    stringsAsFactors = FALSE
  )
  utils::write.csv(diagnostics, file.path(model_dir, "diagnostics-summary.csv"), row.names = FALSE)
  utils::write.csv(diagnostics, file.path(out_dir, "diagnostics-summary.csv"), row.names = FALSE)
  kflow_write_manifest(model_dir, file.path(model_dir, "model-manifest.csv"))
  kflow_note("Wrote diagnostics smoke summary from ", length(depletion_files), " depletion files.", log_file = log_file)
  invisible(diagnostics)
}

kflow_patch_script_path <- function(script) {
  kflow_existing_script(script)
}

kflow_apply_patch_script <- function(source_dir, input_dir, out_dir, stage, log_file = NULL) {
  script <- kflow_env("PATCH_SCRIPT", "")
  script_path <- kflow_patch_script_path(script)
  if (!nzchar(script_path)) {
    if (nzchar(script)) {
      stop(sprintf("PATCH_SCRIPT was set but not found: %s", script), call. = FALSE)
    }
    return(invisible(FALSE))
  }

  patch_input <- kflow_env("PATCH_INPUT_DIR", "")
  patch_output <- kflow_env("PATCH_OUTPUT_DIR", "")
  if (!nzchar(patch_input)) {
    patch_input <- kflow_env("BASE_DIR", "")
  }
  if (!nzchar(patch_output)) {
    patch_output <- patch_input
  }
  patch_input_abs <- if (nzchar(patch_input)) file.path(source_dir, patch_input) else source_dir
  patch_output_abs <- if (nzchar(patch_output)) file.path(source_dir, patch_output) else patch_input_abs
  dir.create(patch_output_abs, recursive = TRUE, showWarnings = FALSE)

  Sys.setenv(
    KFLOW_PATCH_SOURCE_DIR = source_dir,
    KFLOW_PATCH_INPUT_DIR = patch_input_abs,
    KFLOW_PATCH_OUTPUT_DIR = patch_output_abs,
    KFLOW_PATCH_STAGE = stage,
    KFLOW_PATCH_OUT_DIR = normalizePath(out_dir, winslash = "/", mustWork = FALSE),
    PATCH_ARGS = kflow_env("PATCH_ARGS", "")
  )

  kflow_note("Applying patch script: ", script_path, log_file = log_file)
  kflow_run_shell(sprintf("Rscript %s", shQuote(script_path)), workdir = source_dir, log_file = log_file, sanitize_env = TRUE)

  utils::write.csv(
    data.frame(
      patch_script = script,
      patch_script_path = script_path,
      patch_input_dir = patch_input,
      patch_output_dir = patch_output,
      stringsAsFactors = FALSE
    ),
    file.path(out_dir, "patch-summary.csv"),
    row.names = FALSE
  )
  invisible(TRUE)
}

kflow_run_optional_script <- function(env_name, source_dir, out_dir, stage, log_file = NULL) {
  script <- kflow_env(env_name, "")
  script_path <- kflow_existing_script(script)
  if (!nzchar(script_path)) {
    if (nzchar(script)) {
      stop(sprintf("%s was set but not found: %s", env_name, script), call. = FALSE)
    }
    return(invisible(FALSE))
  }
  Sys.setenv(
    KFLOW_SOURCE_DIR = source_dir,
    KFLOW_STAGE = stage,
    KFLOW_OUT_DIR = normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  )
  kflow_note("Running optional script ", env_name, ": ", script_path, log_file = log_file)
  kflow_run_shell(sprintf("Rscript %s", shQuote(script_path)), workdir = source_dir, log_file = log_file, sanitize_env = TRUE)
  invisible(TRUE)
}

kflow_run_backend <- function(source_dir, out_dir, stage, log_file = NULL) {
  backend <- kflow_env("MFCL_BACKEND", "mfcl_exe")
  if (identical(backend, "mfcl_exe")) {
    kflow_run_make_targets(source_dir, kflow_env("MAKE_TARGETS", ""), log_file = log_file)
  } else if (identical(backend, "mfcl_smoke")) {
    kflow_run_mfcl_smoke(source_dir, out_dir, stage, log_file = log_file)
  } else if (identical(backend, "diagnostics_smoke")) {
    kflow_run_diagnostics_smoke(source_dir, out_dir, stage, log_file = log_file)
  } else if (identical(backend, "mfclrtmb")) {
    command <- kflow_env("BACKEND_COMMAND", "")
    script <- kflow_env("BACKEND_SCRIPT", "")
    if (nzchar(command)) {
      kflow_note("Running mfclrtmb backend command.", log_file = log_file)
      kflow_run_shell(command, workdir = source_dir, log_file = log_file, sanitize_env = TRUE)
    } else {
      script_path <- kflow_existing_script(script)
      if (!nzchar(script_path)) {
        stop("MFCL_BACKEND=mfclrtmb needs BACKEND_COMMAND or BACKEND_SCRIPT.", call. = FALSE)
      }
      Sys.setenv(
        KFLOW_SOURCE_DIR = source_dir,
        KFLOW_STAGE = stage,
        KFLOW_OUT_DIR = normalizePath(out_dir, winslash = "/", mustWork = FALSE)
      )
      kflow_note("Running mfclrtmb backend script: ", script_path, log_file = log_file)
      kflow_run_shell(sprintf("Rscript %s", shQuote(script_path)), workdir = source_dir, log_file = log_file, sanitize_env = TRUE)
    }
  } else {
    stop(sprintf("Unknown MFCL_BACKEND: %s", backend), call. = FALSE)
  }

  kflow_run_optional_script("MFCLKIT_SCRIPT", source_dir, out_dir, stage, log_file = log_file)
  invisible(TRUE)
}

kflow_registry_values <- function(stage, extra = list()) {
  values <- list(
    stage = stage,
    model_key = kflow_env("MODEL_KEY", kflow_env("JOB_KEY", "")),
    model_token = kflow_env("MODEL_TOKEN", kflow_env("RUN_LABEL", "")),
    model_name = kflow_env("MODEL_NAME", kflow_env("JOB_TITLE", "")),
    model_label = kflow_env("MODEL_LABEL", kflow_env("MODEL_TOKEN", "")),
    plot_label = kflow_env("PLOT_LABEL", kflow_env("MODEL_TOKEN", "")),
    report_label = kflow_env("REPORT_LABEL", kflow_env("MODEL_NAME", "")),
    run_label = kflow_env("RUN_LABEL", ""),
    job_key = kflow_env("JOB_KEY", ""),
    base_model_key = kflow_env("BASE_MODEL_KEY", ""),
    parent_model_key = kflow_env("PARENT_MODEL_KEY", ""),
    parent_model_token = kflow_env("PARENT_MODEL_TOKEN", ""),
    parent_task = kflow_env("INPUT_TASK", ""),
    parent_key = kflow_env("INPUT_KEY", ""),
    recipe_token = kflow_env("RECIPE_TOKEN", ""),
    recipe_family = kflow_env("RECIPE_FAMILY", ""),
    recipe_label = kflow_env("RECIPE_LABEL", ""),
    change_token = kflow_env("CHANGE_TOKEN", ""),
    change_group = kflow_env("CHANGE_GROUP", ""),
    change_detail = kflow_env("CHANGE_DETAIL", ""),
    change_summary = kflow_env("CHANGE_SUMMARY", kflow_env("JOB_DESCRIPTION", "")),
    input_variant = kflow_env("INPUT_VARIANT", ""),
    patch_script = kflow_env("PATCH_SCRIPT", ""),
    patch_input_dir = kflow_env("PATCH_INPUT_DIR", ""),
    patch_output_dir = kflow_env("PATCH_OUTPUT_DIR", ""),
    make_targets = kflow_env("MAKE_TARGETS", ""),
    make_vars = kflow_env("MAKE_VARS", ""),
    base_dir = kflow_env("BASE_DIR", ""),
    model_dir = kflow_env("MODEL_DIR", ""),
    mfcl_backend = kflow_env("MFCL_BACKEND", "mfcl_exe"),
    flow_species = kflow_env("FLOW_SPECIES", ""),
    flow_species_label = kflow_env("FLOW_SPECIES_LABEL", ""),
    flow_assessment_year = kflow_env("FLOW_ASSESSMENT_YEAR", ""),
    flow_task_prefix = kflow_env("FLOW_TASK_PREFIX", ""),
    program_path = kflow_env("PROGRAM_PATH", kflow_env("FLOW_MFCL_PROGRAM", "/home/mfcl/mfclo64")),
    source_repo = kflow_env("SOURCE_REPO", "PacificCommunity/ofp-sam-bet2026-inputs"),
    source_ref = kflow_env("SOURCE_REF", ""),
    source_path = kflow_env("SOURCE_PATH", ""),
    use_flow_source = kflow_env("USE_FLOW_SOURCE", ""),
    use_local_source = kflow_env("USE_LOCAL_SOURCE", ""),
    flow_group = kflow_env("FLOW_GROUP", ""),
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
  c(values, extra)
}

kflow_write_registry <- function(out_dir, stage, extra = list()) {
  values <- kflow_registry_values(stage, extra = extra)
  registry <- as.data.frame(values, stringsAsFactors = FALSE)
  utils::write.csv(registry, file.path(out_dir, "model-registry.csv"), row.names = FALSE)
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(values, file.path(out_dir, "model-registry.json"), auto_unbox = TRUE, pretty = TRUE)
  } else {
    dput(values, file.path(out_dir, "model-registry.json"))
  }
  writeLines(values$model_token %||% "", file.path(out_dir, "model-token.txt"))
  invisible(registry)
}

kflow_write_summary <- function(out_dir, stage, extra = list()) {
  values <- c(
    list(
      stage = stage,
      run_label = kflow_env("RUN_LABEL", ""),
      job_key = kflow_env("JOB_KEY", ""),
      job_title = kflow_env("JOB_TITLE", ""),
      flow_species = kflow_env("FLOW_SPECIES", ""),
      flow_species_label = kflow_env("FLOW_SPECIES_LABEL", ""),
      flow_assessment_year = kflow_env("FLOW_ASSESSMENT_YEAR", ""),
      flow_task_prefix = kflow_env("FLOW_TASK_PREFIX", ""),
      source_repo = kflow_env("SOURCE_REPO", "PacificCommunity/ofp-sam-bet2026-inputs"),
      source_ref = kflow_env("SOURCE_REF", ""),
      source_path = kflow_env("SOURCE_PATH", ""),
      use_flow_source = kflow_env("USE_FLOW_SOURCE", ""),
      use_local_source = kflow_env("USE_LOCAL_SOURCE", ""),
      backend = kflow_env("MFCL_BACKEND", "mfcl_exe"),
      make_targets = kflow_env("MAKE_TARGETS", ""),
      make_vars = kflow_env("MAKE_VARS", ""),
      base_dir = kflow_env("BASE_DIR", ""),
      model_dir = kflow_env("MODEL_DIR", ""),
      model_token = kflow_env("MODEL_TOKEN", ""),
      model_label = kflow_env("MODEL_LABEL", ""),
      plot_label = kflow_env("PLOT_LABEL", ""),
      report_label = kflow_env("REPORT_LABEL", ""),
      change_token = kflow_env("CHANGE_TOKEN", ""),
      change_group = kflow_env("CHANGE_GROUP", ""),
      change_detail = kflow_env("CHANGE_DETAIL", ""),
      change_summary = kflow_env("CHANGE_SUMMARY", ""),
      parent_model_key = kflow_env("PARENT_MODEL_KEY", ""),
      parent_model_token = kflow_env("PARENT_MODEL_TOKEN", ""),
      recipe_token = kflow_env("RECIPE_TOKEN", ""),
      recipe_family = kflow_env("RECIPE_FAMILY", ""),
      recipe_label = kflow_env("RECIPE_LABEL", ""),
      patch_script = kflow_env("PATCH_SCRIPT", "")
    ),
    extra
  )
  summary <- data.frame(
    name = names(values),
    value = unname(vapply(values, as.character, character(1))),
    stringsAsFactors = FALSE
  )
  utils::write.csv(summary, file.path(out_dir, "kflow-job-summary.csv"), row.names = FALSE)
  writeLines(sprintf("%s: %s", summary$name, summary$value), file.path(out_dir, "kflow-job-summary.txt"))
  invisible(summary)
}

kflow_stage_start <- function(stage) {
  out_dir <- kflow_out_dir()
  input_dir <- kflow_input_dir()
  log_file <- kflow_log_file(out_dir, paste0(stage, ".log"))
  label <- trimws(paste(kflow_env("FLOW_SPECIES", "Tuna"), kflow_env("FLOW_ASSESSMENT_YEAR", "")))
  kflow_note("Starting ", label, " Kflow stage: ", stage, log_file = log_file)
  list(out_dir = out_dir, input_dir = input_dir, log_file = log_file)
}

kflow_read_csv_union <- function(files) {
  files <- files[file.exists(files)]
  if (!length(files)) {
    return(data.frame())
  }
  tables <- lapply(files, function(file) {
    x <- tryCatch(utils::read.csv(file, stringsAsFactors = FALSE), error = function(e) data.frame())
    if (nrow(x) || ncol(x)) {
      x$source_file <- file
    }
    x
  })
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
