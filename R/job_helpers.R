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
  text <- paste(..., collapse = "")
  message(text)
  if (!is.null(log_file)) {
    cat(text, "\n", file = log_file, append = TRUE)
  }
}

kflow_run_shell <- function(command, workdir = getwd(), log_file = NULL) {
  old <- setwd(workdir)
  on.exit(setwd(old), add = TRUE)
  if (is.null(log_file)) {
    status <- system2("bash", c("-lc", command))
  } else {
    log_file <- normalizePath(log_file, winslash = "/", mustWork = FALSE)
    dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    wrapped <- sprintf("set -o pipefail; %s 2>&1 | tee -a %s", command, shQuote(log_file))
    status <- system2("bash", c("-lc", wrapped))
  }
  if (!identical(status, 0L)) {
    stop(sprintf("Command failed with exit status %s: %s", status, command), call. = FALSE)
  }
  invisible(TRUE)
}

kflow_clone_source <- function(work_dir = "work/source", log_file = NULL) {
  source_repo <- kflow_env("SOURCE_REPO", "PacificCommunity/ofp-sam-2026-BET")
  source_ref <- kflow_env("SOURCE_REF", "main")
  source_url <- sprintf("https://github.com/%s.git", source_repo)
  unlink(work_dir, recursive = TRUE, force = TRUE)
  dir.create(dirname(work_dir), recursive = TRUE, showWarnings = FALSE)

  clone_branch <- sprintf(
    "git clone --depth 1 --branch %s %s %s",
    shQuote(source_ref), shQuote(source_url), shQuote(work_dir)
  )
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
    kflow_run_shell(sprintf("git fetch --depth 1 origin %s || true", shQuote(source_ref)), workdir = work_dir, log_file = log_file)
    kflow_run_shell(sprintf("git checkout %s", shQuote(source_ref)), workdir = work_dir, log_file = log_file)
  }

  normalizePath(work_dir, winslash = "/", mustWork = TRUE)
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
    return(character())
  }
  all_files <- list.files(input_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  hits <- all_files[grepl(pattern, all_files)]
  if (!length(hits)) {
    kflow_note("No upstream seed files matched pattern: ", pattern, log_file = log_file)
    return(character())
  }
  dest_dir <- file.path(source_dir, copy_to)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(hits, dest_dir, overwrite = TRUE, recursive = TRUE)
  kflow_note("Copied ", length(hits), " seed files into ", copy_to, log_file = log_file)
  invisible(hits)
}

kflow_set_common_make_env <- function() {
  values <- c(
    program_path = kflow_env("PROGRAM_PATH", "mfcl/exe/mfclo64_2026"),
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
    kflow_run_shell(command, workdir = source_dir, log_file = log_file)
  }
  invisible(targets)
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
  kflow_run_shell(sprintf("Rscript %s", shQuote(script_path)), workdir = source_dir, log_file = log_file)

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
  kflow_run_shell(sprintf("Rscript %s", shQuote(script_path)), workdir = source_dir, log_file = log_file)
  invisible(TRUE)
}

kflow_run_backend <- function(source_dir, out_dir, stage, log_file = NULL) {
  backend <- kflow_env("MFCL_BACKEND", "mfcl_exe")
  if (identical(backend, "mfcl_exe")) {
    kflow_run_make_targets(source_dir, kflow_env("MAKE_TARGETS", ""), log_file = log_file)
  } else if (identical(backend, "mfclrtmb")) {
    command <- kflow_env("BACKEND_COMMAND", "")
    script <- kflow_env("BACKEND_SCRIPT", "")
    if (nzchar(command)) {
      kflow_note("Running mfclrtmb backend command.", log_file = log_file)
      kflow_run_shell(command, workdir = source_dir, log_file = log_file)
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
      kflow_run_shell(sprintf("Rscript %s", shQuote(script_path)), workdir = source_dir, log_file = log_file)
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
    run_label = kflow_env("RUN_LABEL", ""),
    job_key = kflow_env("JOB_KEY", ""),
    base_model_key = kflow_env("BASE_MODEL_KEY", ""),
    parent_task = kflow_env("INPUT_TASK", ""),
    parent_key = kflow_env("INPUT_KEY", ""),
    change_token = kflow_env("CHANGE_TOKEN", ""),
    change_group = kflow_env("CHANGE_GROUP", ""),
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
    program_path = kflow_env("PROGRAM_PATH", "mfcl/exe/mfclo64_2026"),
    source_repo = kflow_env("SOURCE_REPO", "PacificCommunity/ofp-sam-2026-BET"),
    source_ref = kflow_env("SOURCE_REF", "main"),
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
      source_repo = kflow_env("SOURCE_REPO", "PacificCommunity/ofp-sam-2026-BET"),
      source_ref = kflow_env("SOURCE_REF", "main"),
      backend = kflow_env("MFCL_BACKEND", "mfcl_exe"),
      make_targets = kflow_env("MAKE_TARGETS", ""),
      make_vars = kflow_env("MAKE_VARS", ""),
      base_dir = kflow_env("BASE_DIR", ""),
      model_dir = kflow_env("MODEL_DIR", ""),
      model_token = kflow_env("MODEL_TOKEN", ""),
      change_token = kflow_env("CHANGE_TOKEN", ""),
      change_group = kflow_env("CHANGE_GROUP", ""),
      change_summary = kflow_env("CHANGE_SUMMARY", ""),
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
  kflow_note("Starting BET Kflow stage: ", stage, log_file = log_file)
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
