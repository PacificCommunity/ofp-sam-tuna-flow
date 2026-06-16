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

kflow_github_token_env_name <- function() {
  names <- c("GITHUB_PAT", "GIT_PAT", "GH_TOKEN", "KFLOW_GITHUB_TOKEN", "KFLOW_PERSONAL_TOKEN")
  hits <- names[nzchar(Sys.getenv(names, unset = ""))]
  if (length(hits)) hits[[1L]] else ""
}

kflow_github_clone_url <- function(repo) {
  if (grepl("^(https?|git)://|^git@", repo)) {
    return(repo)
  }
  sprintf("https://github.com/%s.git", repo)
}

kflow_git_auth_args <- function() {
  token_name <- kflow_github_token_env_name()
  if (!nzchar(token_name)) {
    return("")
  }
  sprintf("-c http.extraheader=\"Authorization: Bearer ${%s}\"", token_name)
}

kflow_clone_github_repo <- function(repo, ref = "main", work_dir = "work/source", log_file = NULL) {
  source_url <- kflow_github_clone_url(repo)
  auth_args <- kflow_git_auth_args()
  unlink(work_dir, recursive = TRUE, force = TRUE)
  dir.create(dirname(work_dir), recursive = TRUE, showWarnings = FALSE)

  git_cmds <- if (nzchar(auth_args)) c(paste("git", auth_args), "git -c credential.helper=") else "git"
  git_cmds <- unique(git_cmds)
  clone_error <- NULL
  ok <- FALSE

  for (git_cmd in git_cmds) {
    unlink(work_dir, recursive = TRUE, force = TRUE)
    clone_branch <- if (nzchar(ref)) {
      sprintf(
        "%s clone --depth 1 --branch %s %s %s",
        git_cmd, shQuote(ref), shQuote(source_url), shQuote(work_dir)
      )
    } else {
      sprintf("%s clone --depth 1 %s %s", git_cmd, shQuote(source_url), shQuote(work_dir))
    }
    ok <- tryCatch({
      kflow_run_shell(clone_branch, log_file = log_file)
      TRUE
    }, error = function(e) {
      clone_error <<- e
      kflow_note("Branch/tag clone failed: ", conditionMessage(e), log_file = log_file)
      FALSE
    })
    if (isTRUE(ok)) {
      break
    }
  }

  if (!isTRUE(ok)) {
    kflow_note("Retrying clone without branch checkout.", log_file = log_file)
    for (git_cmd in git_cmds) {
      unlink(work_dir, recursive = TRUE, force = TRUE)
      ok <- tryCatch({
        kflow_run_shell(
          sprintf("%s clone --depth 1 %s %s", git_cmd, shQuote(source_url), shQuote(work_dir)),
          log_file = log_file
        )
        TRUE
      }, error = function(e) {
        clone_error <<- e
        kflow_note("Clone failed: ", conditionMessage(e), log_file = log_file)
        FALSE
      })
      if (!isTRUE(ok)) {
        next
      }
      if (nzchar(ref)) {
        kflow_run_shell(sprintf("%s fetch --depth 1 origin %s || true", git_cmd, shQuote(ref)), workdir = work_dir, log_file = log_file)
        kflow_run_shell(sprintf("%s checkout %s", git_cmd, shQuote(ref)), workdir = work_dir, log_file = log_file)
      }
      break
    }
  }

  if (!isTRUE(ok)) {
    stop(conditionMessage(clone_error), call. = FALSE)
  }

  if (nzchar(ref) && dir.exists(work_dir)) {
    kflow_run_shell(
      sprintf("git rev-parse --is-inside-work-tree >/dev/null"),
      workdir = work_dir,
      log_file = log_file
    )
  }

  normalizePath(work_dir, winslash = "/", mustWork = TRUE)
}

kflow_clone_source <- function(work_dir = "work/source", log_file = NULL) {
  source_repo <- kflow_env("SOURCE_REPO", kflow_env("FLOW_SOURCE_REPO", "PacificCommunity/ofp-sam-bet2026-inputs"))
  source_ref <- kflow_env("SOURCE_REF", kflow_env("FLOW_SOURCE_REF", "main"))
  kflow_clone_github_repo(source_repo, source_ref, work_dir = work_dir, log_file = log_file)
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

kflow_maybe_collect_paths <- function(source_dir, out_dir, patterns) {
  if (!kflow_bool("COLLECT_SOURCE_ARTIFACTS", FALSE)) {
    return(invisible(character()))
  }
  kflow_collect_paths(source_dir, out_dir, patterns)
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

kflow_latest_par <- function(model_dir) {
  pars <- list.files(model_dir, pattern = "[.]par$", full.names = FALSE, ignore.case = TRUE)
  pars <- pars[!grepl("^smoke-", pars)]
  if (!length(pars)) {
    return("")
  }
  numeric_key <- suppressWarnings(as.numeric(sub("^([0-9]+).*", "\\1", pars)))
  if (any(is.finite(numeric_key))) {
    candidates <- pars[is.finite(numeric_key)]
    candidate_keys <- numeric_key[is.finite(numeric_key)]
    return(candidates[order(candidate_keys, candidates)][[length(candidates)]])
  }
  info <- file.info(file.path(model_dir, pars))
  pars[order(info$mtime, pars)][[length(pars)]]
}

kflow_par_footer <- function(path) {
  out <- c(objective = NA_real_, max_gradient = NA_real_)
  if (!nzchar(path) || !file.exists(path)) {
    return(out)
  }
  lines <- readLines(path, warn = FALSE)
  objective_i <- grep("# Objective function value", lines, fixed = TRUE)
  gradient_i <- grep("# Maximum magnitude gradient", lines, fixed = TRUE)
  if (length(objective_i) && objective_i[[1]] < length(lines)) {
    out[["objective"]] <- suppressWarnings(as.numeric(lines[[objective_i[[1]] + 1L]]))
  }
  if (length(gradient_i) && gradient_i[[1]] < length(lines)) {
    out[["max_gradient"]] <- suppressWarnings(as.numeric(lines[[gradient_i[[1]] + 1L]]))
  }
  out
}

kflow_smoke_switch_args <- function(fevals = 1L, report = TRUE) {
  switches <- c(
    1, 1, as.integer(fevals),
    1, 189, as.integer(isTRUE(report)),
    1, 190, as.integer(isTRUE(report)),
    1, 188, as.integer(isTRUE(report)),
    1, 187, 0,
    1, 186, 0
  )
  c("-switch", length(switches) / 3L, switches)
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

kflow_write_smoke_model_info <- function(model_dir, stage, run_mode, program, frq, ini, input_par, output_par, footer, log_summary) {
  info <- list(
    payload_type = "kflow_smoke_model",
    stage = stage,
    description = kflow_env("JOB_DESCRIPTION", ""),
    program_path = program,
    frq_file = frq,
    ini_file = ini,
    par_in = input_par,
    par_out = output_par,
    base_dir = kflow_env("BASE_DIR", ""),
    model_dir = kflow_env("MODEL_DIR", ""),
    run_mode = run_mode,
    smoke_fevals = suppressWarnings(as.integer(kflow_env("SMOKE_FEVALS", "1"))),
    objective = footer[["objective"]],
    obj_fun = footer[["objective"]],
    max_gradient = footer[["max_gradient"]],
    max_grad = footer[["max_gradient"]],
    registry = kflow_registry_values(stage),
    mfcl_log = as.list(log_summary[1, , drop = TRUE])
  )
  saveRDS(info, file.path(model_dir, "model_info.rds"), compress = "xz")
  invisible(info)
}

kflow_find_smoke_rep_file <- function(model_dir, output_par_name = "") {
  candidates <- character()
  if (nzchar(output_par_name)) {
    candidates <- c(
      file.path(model_dir, paste0("plot-", output_par_name, ".rep")),
      file.path(model_dir, paste0(tools::file_path_sans_ext(output_par_name), ".rep"))
    )
  }
  candidates <- c(
    candidates,
    file.path(model_dir, "ests.rep"),
    list.files(model_dir, pattern = "[.]rep$", full.names = TRUE, ignore.case = TRUE)
  )
  candidates <- unique(candidates[file.exists(candidates)])
  if (!length(candidates)) {
    return("")
  }
  priority <- function(path) {
    name <- basename(path)
    if (nzchar(output_par_name) && identical(name, paste0("plot-", output_par_name, ".rep"))) return(1L)
    if (grepl("^plot-.*[.]rep$", name, ignore.case = TRUE)) return(2L)
    if (identical(name, "ests.rep")) return(3L)
    10L
  }
  candidates[order(vapply(candidates, priority, integer(1)), file.info(candidates)$mtime, decreasing = FALSE)][[1L]]
}

kflow_try_build_rep_payload <- function(model_dir, output_par_name = "", log_file = NULL) {
  payload_file <- file.path(model_dir, "model_payload.rds")
  rep_file <- kflow_find_smoke_rep_file(model_dir, output_par_name = output_par_name)
  if (!nzchar(rep_file) || !file.exists(rep_file)) {
    return("flr4mfcl_rep_payload_unavailable:no_rep_file")
  }
  if (!requireNamespace("FLR4MFCL", quietly = TRUE)) {
    return("flr4mfcl_rep_payload_unavailable:FLR4MFCL_missing")
  }
  status <- tryCatch({
    rep_out <- suppressWarnings(FLR4MFCL::read.MFCLRep(rep_file))
    info <- if (file.exists(file.path(model_dir, "model_info.rds"))) {
      tryCatch(readRDS(file.path(model_dir, "model_info.rds")), error = function(e) NULL)
    } else {
      NULL
    }
    payload <- list(
      version = "kflow_rep_payload_v1",
      created_at = as.character(Sys.time()),
      folder = normalizePath(model_dir, winslash = "/", mustWork = FALSE),
      files = list(rep = normalizePath(rep_file, winslash = "/", mustWork = FALSE)),
      data = list(RepOut = rep_out, info = info)
    )
    saveRDS(payload, payload_file, compress = "xz")
    "flr4mfcl_rep_payload_ok"
  }, error = function(e) {
    paste("flr4mfcl_rep_payload_skipped:", conditionMessage(e))
  })
  kflow_note("Fallback RepOut payload status: ", status, log_file = log_file)
  status
}

kflow_try_build_mfclshiny_payload <- function(model_dir, out_dir, output_par_name = "", log_file = NULL) {
  payload_file <- file.path(model_dir, "model_payload.rds")
  if (requireNamespace("mfclshiny", quietly = TRUE) &&
      "build_model_payload" %in% getNamespaceExports("mfclshiny")) {
    status <- tryCatch({
      mfclshiny::build_model_payload(model_dir, output_file = payload_file, overwrite = TRUE)
      "mfclshiny_payload_ok"
    }, error = function(e) {
      paste("mfclshiny_payload_skipped:", conditionMessage(e))
    })
  } else {
    status <- "mfclshiny_payload_unavailable"
  }
  if (!file.exists(payload_file)) {
    fallback_status <- kflow_try_build_rep_payload(model_dir, output_par_name = output_par_name, log_file = log_file)
    status <- paste(status, fallback_status, sep = "; ")
  }
  writeLines(status, file.path(out_dir, "model-payload-status.txt"))
  kflow_note("Model payload status: ", status, log_file = log_file)
  if (file.exists(payload_file)) {
    file.copy(payload_file, file.path(out_dir, "model_payload.rds"), overwrite = TRUE)
  }
  file.exists(payload_file)
}

kflow_mfclshiny_payload_env <- function() {
  if (!requireNamespace("mfclshiny", quietly = TRUE)) {
    return(NULL)
  }
  factory <- tryCatch(
    get("mfclshiny_payload_env", envir = asNamespace("mfclshiny"), inherits = FALSE),
    error = function(e) NULL
  )
  if (is.function(factory)) {
    return(tryCatch(factory(), error = function(e) NULL))
  }

  tool <- system.file("app", "tools", "model_payload.R", package = "mfclshiny")
  if (!nzchar(tool) || !file.exists(tool)) {
    return(NULL)
  }
  env <- new.env(parent = asNamespace("mfclshiny"))
  tryCatch({
    sys.source(tool, envir = env, keep.source = FALSE)
    env
  }, error = function(e) NULL)
}

kflow_array_to_df <- function(x, value_col = "data") {
  out <- tryCatch(as.data.frame(x), error = function(e) NULL)
  if (!is.null(out) && nrow(out) > 0 && value_col %in% names(out)) {
    return(out)
  }

  d <- dim(x)
  if (is.null(d) || !length(d)) {
    return(data.frame(data = suppressWarnings(as.numeric(x)), stringsAsFactors = FALSE))
  }

  dn <- tryCatch(dimnames(x), error = function(e) NULL)
  if (is.null(dn)) {
    dn <- vector("list", length(d))
  }
  if (length(dn) < length(d)) {
    dn <- c(dn, vector("list", length(d) - length(dn)))
  } else if (length(dn) > length(d)) {
    dn <- dn[seq_along(d)]
  }

  dim_cols <- names(dn)
  if (is.null(dim_cols) || length(dim_cols) != length(d) || any(!nzchar(dim_cols))) {
    dim_cols <- c("age", "year", "unit", "season", "area", "iter")[seq_along(d)]
    missing <- is.na(dim_cols)
    dim_cols[missing] <- paste0("dim", which(missing))
  }

  grid <- expand.grid(lapply(d, seq_len), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  names(grid) <- dim_cols
  for (i in seq_along(d)) {
    labels <- dn[[i]]
    if (is.null(labels) || length(labels) != d[[i]]) {
      labels <- as.character(seq_len(d[[i]]))
    } else {
      labels <- as.character(labels)
    }
    grid[[dim_cols[[i]]]] <- labels[grid[[dim_cols[[i]]]]]
  }

  vals <- suppressWarnings(as.numeric(c(x)))
  expected_n <- prod(d)
  if (length(vals) != expected_n) {
    vals <- rep(NA_real_, expected_n)
  }
  grid[[value_col]] <- vals
  grid
}

kflow_extract_yearly_sum <- function(slot_obj, scale = 1) {
  slot_df <- tryCatch(kflow_array_to_df(slot_obj), error = function(e) NULL)
  if (is.null(slot_df) || !nrow(slot_df) || !"year" %in% names(slot_df) || !"data" %in% names(slot_df)) {
    return(NULL)
  }
  slot_df$year <- suppressWarnings(as.numeric(slot_df$year))
  slot_df$data <- suppressWarnings(as.numeric(slot_df$data))
  slot_df <- slot_df[is.finite(slot_df$year) & is.finite(slot_df$data), , drop = FALSE]
  if (!nrow(slot_df)) {
    return(NULL)
  }
  out <- stats::aggregate(data ~ year, data = slot_df, FUN = sum)
  out$data <- out$data / scale
  out
}

kflow_extract_rep_timeseries_fallback <- function(rep_obj, scenario = NA_character_, log_file = NULL) {
  bio_fish <- tryCatch(kflow_array_to_df(slot(rep_obj, "adultBiomass")), error = function(e) NULL)
  bio_nofish <- tryCatch(kflow_array_to_df(slot(rep_obj, "adultBiomass_nofish")), error = function(e) NULL)
  if (is.null(bio_fish) || is.null(bio_nofish)) {
    return(data.frame())
  }
  needed <- c("year", "season", "data")
  if (!all(needed %in% names(bio_fish)) || !all(needed %in% names(bio_nofish))) {
    return(data.frame())
  }

  for (col in needed) {
    bio_fish[[col]] <- suppressWarnings(as.numeric(bio_fish[[col]]))
    bio_nofish[[col]] <- suppressWarnings(as.numeric(bio_nofish[[col]]))
  }
  bio_fish <- bio_fish[is.finite(bio_fish$year) & is.finite(bio_fish$season) & is.finite(bio_fish$data), , drop = FALSE]
  bio_nofish <- bio_nofish[is.finite(bio_nofish$year) & is.finite(bio_nofish$season) & is.finite(bio_nofish$data), , drop = FALSE]
  if (!nrow(bio_fish) || !nrow(bio_nofish)) {
    return(data.frame())
  }

  bio_fish <- stats::aggregate(data ~ year + season, data = bio_fish, FUN = sum)
  names(bio_fish)[names(bio_fish) == "data"] <- "bio_fish"
  bio_nofish <- stats::aggregate(data ~ year + season, data = bio_nofish, FUN = sum)
  names(bio_nofish)[names(bio_nofish) == "data"] <- "bio_nofish"
  merged <- merge(bio_fish, bio_nofish, by = c("year", "season"), all = FALSE)
  if (!nrow(merged)) {
    return(data.frame())
  }

  merged$depletion <- merged$bio_fish / pmax(merged$bio_nofish, .Machine$double.eps)
  dep <- stats::aggregate(depletion ~ year, data = merged, FUN = function(x) mean(x, na.rm = TRUE))
  sp <- stats::aggregate(bio_fish ~ year, data = merged, FUN = function(x) mean(x, na.rm = TRUE) / 1e3)
  names(sp)[names(sp) == "bio_fish"] <- "spawning_potential"
  out <- merge(dep, sp, by = "year", all = FALSE)

  rec_df <- NULL
  for (slot_name in c("rec_region", "eq_rec", "rec")) {
    rec_df <- tryCatch(
      kflow_extract_yearly_sum(slot(rep_obj, slot_name), scale = if (identical(slot_name, "rec")) 1 else 1e6),
      error = function(e) NULL
    )
    if (!is.null(rec_df) && nrow(rec_df)) {
      break
    }
  }
  if (!is.null(rec_df) && nrow(rec_df)) {
    names(rec_df)[names(rec_df) == "data"] <- "recruitment"
    out <- merge(out, rec_df, by = "year", all = TRUE)
  }

  fm_df <- tryCatch(kflow_array_to_df(slot(rep_obj, "fm")), error = function(e) NULL)
  popn_df <- tryCatch(kflow_array_to_df(slot(rep_obj, "popN")), error = function(e) NULL)
  used_fm <- FALSE
  if (!is.null(fm_df) && !is.null(popn_df) && nrow(fm_df) && nrow(popn_df)) {
    fm_df$data <- suppressWarnings(as.numeric(fm_df$data))
    popn_df$data <- suppressWarnings(as.numeric(popn_df$data))
    popn_df$N <- popn_df$data
    popn_df$data <- NULL
    numeric_cols <- intersect(c("age", "year", "unit", "season", "area", "iter"), union(names(fm_df), names(popn_df)))
    for (col in numeric_cols) {
      if (col %in% names(fm_df)) {
        fm_df[[col]] <- suppressWarnings(as.numeric(fm_df[[col]]))
      }
      if (col %in% names(popn_df)) {
        popn_df[[col]] <- suppressWarnings(as.numeric(popn_df[[col]]))
      }
    }
    join_cols <- intersect(c("age", "year", "unit", "season", "area", "iter"), intersect(names(fm_df), names(popn_df)))
    if (all(c("year", "season") %in% join_cols)) {
      fm_popn <- merge(fm_df, popn_df, by = join_cols, all = FALSE)
      fm_popn <- fm_popn[
        is.finite(fm_popn$year) & is.finite(fm_popn$season) &
          is.finite(fm_popn$data) & is.finite(fm_popn$N),
        ,
        drop = FALSE
      ]
      if (nrow(fm_popn)) {
        fm_popn$catch <- fm_popn$data * fm_popn$N
        yearly <- stats::aggregate(
          cbind(total_catch = catch, total_N = N) ~ year + season,
          data = fm_popn,
          FUN = sum
        )
        if (nrow(yearly)) {
          yearly$harvest_rate <- yearly$total_catch / pmax(yearly$total_N, .Machine$double.eps)
          yearly$inst_F <- -log(pmax(1 - yearly$harvest_rate, 0.001))
          fm_year <- stats::aggregate(inst_F ~ year, data = yearly, FUN = sum)
          names(fm_year)[names(fm_year) == "inst_F"] <- "fishing_mortality"
          out <- merge(out, fm_year, by = "year", all = TRUE)
          used_fm <- TRUE
        }
      }
    }
  }
  if (!used_fm && !is.null(fm_df) && nrow(fm_df) && all(c("year", "data") %in% names(fm_df))) {
    fm_df$year <- suppressWarnings(as.numeric(fm_df$year))
    fm_df$data <- suppressWarnings(as.numeric(fm_df$data))
    fm_df <- fm_df[is.finite(fm_df$year) & is.finite(fm_df$data), , drop = FALSE]
    if (nrow(fm_df)) {
      fm_year <- stats::aggregate(data ~ year, data = fm_df, FUN = function(x) mean(x, na.rm = TRUE))
      names(fm_year)[names(fm_year) == "data"] <- "fishing_mortality"
      out <- merge(out, fm_year, by = "year", all = TRUE)
    }
  }

  out$scenario <- scenario
  out$peel <- 0L
  attr(out, "kflow_derived_source") <- "mfcl_payload_rep_timeseries_fallback"
  out
}

kflow_write_payload_debug_summary <- function(payload_file, out_dir) {
  payload <- tryCatch(readRDS(payload_file), error = function(e) NULL)
  rep_obj <- tryCatch(payload$data$RepOut, error = function(e) NULL)
  slots <- c("adultBiomass", "adultBiomass_nofish", "rec_region", "eq_rec", "rec", "fm", "popN")
  rows <- lapply(slots, function(slot_name) {
    slot_obj <- tryCatch(slot(rep_obj, slot_name), error = function(e) e)
    as_df <- if (!inherits(slot_obj, "error")) tryCatch(as.data.frame(slot_obj), error = function(e) e) else slot_obj
    fallback <- if (!inherits(slot_obj, "error")) tryCatch(kflow_array_to_df(slot_obj), error = function(e) e) else slot_obj
    data.frame(
      payload_file = basename(payload_file),
      rep_class = paste(class(rep_obj), collapse = "/"),
      slot = slot_name,
      slot_class = paste(class(slot_obj), collapse = "/"),
      slot_dim = if (!inherits(slot_obj, "error")) paste(dim(slot_obj), collapse = "x") else "",
      as_data_frame_ok = !inherits(as_df, "error"),
      as_data_frame_rows = if (!inherits(as_df, "error")) nrow(as_df) else NA_integer_,
      as_data_frame_error = if (inherits(as_df, "error")) conditionMessage(as_df) else "",
      fallback_ok = !inherits(fallback, "error"),
      fallback_rows = if (!inherits(fallback, "error")) nrow(fallback) else NA_integer_,
      fallback_error = if (inherits(fallback, "error")) conditionMessage(fallback) else "",
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, rows)
  utils::write.csv(summary, file.path(out_dir, "payload-debug-summary.csv"), row.names = FALSE)
  invisible(summary)
}

kflow_extract_payload_timeseries <- function(payload_file, stage, input_par = "", output_par = "", log_file = NULL) {
  if (!file.exists(payload_file)) {
    return(data.frame())
  }
  payload <- tryCatch(readRDS(payload_file), error = function(e) NULL)
  rep_obj <- tryCatch(payload$data$RepOut, error = function(e) NULL)
  if (is.null(rep_obj)) {
    return(data.frame())
  }

  scenario <- kflow_env("PLOT_LABEL", kflow_env("MODEL_TOKEN", kflow_env("RUN_LABEL", "Model")))
  env <- kflow_mfclshiny_payload_env()
  out <- NULL
  derived_source <- "mfcl_payload_rep_timeseries"
  if (!is.null(env) && is.function(env$mp_extract_rep_timeseries)) {
    out <- tryCatch(
      env$mp_extract_rep_timeseries(rep_obj, scenario = scenario),
      error = function(e) {
        kflow_note("Could not extract MFCL payload timeseries: ", conditionMessage(e), log_file = log_file)
        NULL
      }
    )
  }
  if (is.null(out) || !nrow(out)) {
    kflow_note("mfclshiny payload timeseries was empty; trying direct RepOut extraction.", log_file = log_file)
    out <- kflow_extract_rep_timeseries_fallback(rep_obj, scenario = scenario, log_file = log_file)
    derived_source <- attr(out, "kflow_derived_source") %||% "mfcl_payload_rep_timeseries_fallback"
  }
  if (is.null(out) || !nrow(out)) {
    return(data.frame())
  }

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out$year <- suppressWarnings(as.numeric(out$year))
  out$depletion <- suppressWarnings(as.numeric(out$depletion))
  out <- out[is.finite(out$year) & is.finite(out$depletion), , drop = FALSE]
  if (!nrow(out)) {
    return(data.frame())
  }

  for (name in intersect(c("spawning_potential", "recruitment", "fishing_mortality"), names(out))) {
    out[[name]] <- suppressWarnings(as.numeric(out[[name]]))
  }
  if (!"region" %in% names(out)) {
    out$region <- "All"
  }
  out$stage <- stage
  model_token <- kflow_env("MODEL_TOKEN", kflow_env("RUN_LABEL", ""))
  out$model_key <- kflow_env("MODEL_KEY", kflow_env("JOB_KEY", model_token))
  out$model_token <- model_token
  out$model_label <- kflow_env("MODEL_LABEL", out$model_token[[1]])
  out$plot_label <- kflow_env("PLOT_LABEL", out$model_token[[1]])
  out$report_label <- kflow_env("REPORT_LABEL", out$model_label[[1]])
  out$change_token <- kflow_env("CHANGE_TOKEN", out$model_token[[1]])
  out$change_group <- kflow_env("CHANGE_GROUP", "")
  out$change_detail <- kflow_env("CHANGE_DETAIL", kflow_env("CHANGE_SUMMARY", ""))
  out$parent_model_key <- kflow_env("PARENT_MODEL_KEY", kflow_env("BASE_MODEL_KEY", ""))
  out$parent_model_token <- kflow_env("PARENT_MODEL_TOKEN", "")
  out$recipe_token <- kflow_env("RECIPE_TOKEN", out$change_token[[1]])
  out$recipe_family <- kflow_env("RECIPE_FAMILY", kflow_env("CHANGE_GROUP", ""))
  out$recipe_label <- kflow_env("RECIPE_LABEL", out$change_token[[1]])
  out$source <- derived_source
  out$derived_source <- derived_source
  out$smoke_input_par <- input_par
  out$smoke_output_par <- output_par
  out$payload_file <- basename(payload_file)
  out
}

kflow_assert_key_quantities <- function(data, context = "MFCL payload") {
  if (!nrow(data) || !"depletion" %in% names(data)) {
    stop(context, " did not provide depletion timeseries.", call. = FALSE)
  }
  require_key_quantities <- kflow_bool(
    "MFCL_REQUIRE_KEY_QUANTITIES",
    kflow_bool("KFLOW_REQUIRE_KEY_QUANTITIES", FALSE)
  )
  if (!isTRUE(require_key_quantities)) {
    return(invisible(TRUE))
  }
  required <- c("spawning_potential", "recruitment", "fishing_mortality")
  missing <- required[!vapply(required, function(name) {
    name %in% names(data) && any(is.finite(suppressWarnings(as.numeric(data[[name]]))))
  }, logical(1))]
  if (length(missing)) {
    stop(
      context, " did not provide required key derived quantities: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

kflow_read_csv_file <- function(file) {
  if (!file.exists(file)) {
    return(data.frame())
  }
  tryCatch(utils::read.csv(file, stringsAsFactors = FALSE), error = function(e) data.frame())
}

kflow_write_fallback_payload <- function(out_dir, stage) {
  payload <- list(
    payload_type = "kflow_smoke_payload",
    payload_version = 1L,
    stage = stage,
    registry = kflow_read_csv_file(file.path(out_dir, "model-registry.csv")),
    summary = kflow_read_csv_file(file.path(out_dir, "kflow-job-summary.csv")),
    smoke = kflow_read_csv_file(file.path(out_dir, "mfcl-smoke-summary.csv")),
    diagnostics = kflow_read_csv_file(file.path(out_dir, "diagnostics-summary.csv")),
    depletion = kflow_read_csv_file(file.path(out_dir, "depletion-smoke.csv")),
    mfcl_log = kflow_read_csv_file(file.path(out_dir, "mfcl-log-summary.csv")),
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  )
  saveRDS(payload, file.path(out_dir, "model_payload.rds"), compress = "xz")
  invisible(payload)
}

kflow_write_stage_payload <- function(out_dir, stage, model_dir = "") {
  model_payload <- if (nzchar(model_dir)) file.path(model_dir, "model_payload.rds") else ""
  if (nzchar(model_payload) && file.exists(model_payload)) {
    file.copy(model_payload, file.path(out_dir, "model_payload.rds"), overwrite = TRUE)
  } else if (!file.exists(file.path(out_dir, "model_payload.rds"))) {
    kflow_write_fallback_payload(out_dir, stage)
  }
  invisible(file.path(out_dir, "model_payload.rds"))
}

kflow_compact_outputs <- function(out_dir, keep = c("model_payload.rds", "model-registry.csv", "depletion-smoke.csv")) {
  if (!kflow_bool("COMPACT_OUTPUTS", TRUE)) {
    return(invisible(FALSE))
  }
  files <- list.files(out_dir, full.names = TRUE, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if (!length(files)) {
    return(invisible(TRUE))
  }
  out_root <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  rel <- substring(normalizePath(files, winslash = "/", mustWork = FALSE), nchar(out_root) + 2L)
  rel <- gsub("\\\\", "/", rel)
  keep <- gsub("\\\\", "/", keep)
  keep_file <- rel %in% keep
  remove <- files[!keep_file]
  if (length(remove)) {
    unlink(remove, force = TRUE)
  }
  dirs <- rev(list.dirs(out_dir, full.names = TRUE, recursive = TRUE))
  for (dir in dirs) {
    if (!length(list.files(dir, all.files = TRUE, no.. = TRUE))) {
      unlink(dir, recursive = TRUE, force = TRUE)
    }
  }
  invisible(TRUE)
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
  input_par <- kflow_env("SMOKE_INPUT_PAR", kflow_env("SMOKE_PAR", "last"))
  input_par <- if (tolower(input_par) %in% c("", "last", "latest")) kflow_latest_par(model_dir) else input_par
  run_mode <- "last_par"
  output_par_name <- kflow_env("SMOKE_OUTPUT_PAR", "")
  if (nzchar(input_par)) {
    if (!nzchar(output_par_name)) {
      output_par_name <- paste0("smoke-", tools::file_path_sans_ext(basename(input_par)), ".par")
    }
    fevals <- suppressWarnings(as.integer(kflow_env("SMOKE_FEVALS", "1")))
    if (!is.finite(fevals) || fevals < 1L) {
      fevals <- 1L
    }
    command <- paste(
      shQuote(program),
      shQuote(frq),
      shQuote(input_par),
      shQuote(output_par_name),
      paste(kflow_smoke_switch_args(fevals = fevals, report = TRUE), collapse = " ")
    )
    kflow_note("Running MFCL smoke from last par ", input_par, " in ", model_dir, log_file = log_file)
  } else {
    run_mode <- "makepar"
    output_par_name <- kflow_env("SMOKE_OUTPUT_PAR", "00.par")
    command <- paste(
      shQuote(program),
      shQuote(frq),
      shQuote(ini),
      shQuote(output_par_name),
      "-makepar"
    )
    kflow_note("No input .par found; running MFCL smoke makepar in ", model_dir, log_file = log_file)
  }
  kflow_run_shell(command, workdir = model_dir, log_file = log_file, sanitize_env = TRUE)

  output_par <- file.path(model_dir, output_par_name)
  if (!file.exists(output_par)) {
    stop(sprintf("MFCL smoke run did not create %s", output_par), call. = FALSE)
  }
  log_summary <- kflow_mfcl_log_summary(log_file, out_dir, model_dir)
  footer <- kflow_par_footer(output_par)
  info <- kflow_write_smoke_model_info(
    model_dir = model_dir,
    stage = stage,
    run_mode = run_mode,
    program = program,
    frq = frq,
    ini = ini,
    input_par = input_par,
    output_par = output_par_name,
    footer = footer,
    log_summary = log_summary
  )
  kflow_try_build_mfclshiny_payload(model_dir, out_dir, output_par_name = output_par_name, log_file = log_file)
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
    run_mode = run_mode,
    frq = frq,
    ini = ini,
    input_par = input_par,
    output_par = output_par_name,
    par_size = file.info(output_par)$size,
    objective = info$objective,
    max_gradient = info$max_gradient,
    mfcl_log_error_count = log_summary$error_count[[1]],
    mfcl_log_warning_count = log_summary$warning_count[[1]],
    stringsAsFactors = FALSE
  )
  utils::write.csv(smoke, file.path(model_dir, "mfcl-smoke-summary.csv"), row.names = FALSE)
  utils::write.csv(smoke, file.path(out_dir, "mfcl-smoke-summary.csv"), row.names = FALSE)
  payload_timeseries <- kflow_extract_payload_timeseries(
    file.path(model_dir, "model_payload.rds"),
    stage = stage,
    input_par = input_par,
    output_par = output_par_name,
    log_file = log_file
  )
  if (nrow(payload_timeseries)) {
    kflow_assert_key_quantities(payload_timeseries, context = "MFCL payload")
    depletion <- payload_timeseries
    utils::write.csv(depletion, file.path(model_dir, "depletion-smoke.csv"), row.names = FALSE)
    kflow_note("Wrote MFCL payload-derived key quantities from ", output_par_name, ".", log_file = log_file)
  } else if (kflow_bool("MFCL_REQUIRE_DERIVED", kflow_bool("KFLOW_REQUIRE_MFCL_DERIVED", FALSE))) {
    kflow_write_payload_debug_summary(file.path(model_dir, "model_payload.rds"), out_dir)
    stop("MFCL smoke completed but no payload-derived timeseries were available.", call. = FALSE)
  } else {
    depletion <- kflow_write_smoke_depletion(model_dir, stage)
    depletion$derived_source <- "synthetic_smoke_fallback"
    depletion$smoke_input_par <- input_par
    depletion$smoke_output_par <- output_par_name
    utils::write.csv(depletion, file.path(model_dir, "depletion-smoke.csv"), row.names = FALSE)
  }
  utils::write.csv(depletion, file.path(out_dir, "depletion-smoke.csv"), row.names = FALSE)
  kflow_write_manifest(model_dir, file.path(model_dir, "model-manifest.csv"))
  invisible(smoke)
}

kflow_run_diagnostics_smoke <- function(source_dir, out_dir, stage = "diagnostics", log_file = NULL) {
  model_dir <- file.path(source_dir, kflow_env("MODEL_DIR", file.path("model", kflow_env("JOB_KEY", "diagnostics"))))
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  input_dir <- kflow_env("INPUT_DIR", "inputs")
  search_roots <- unique(normalizePath(c(source_dir, input_dir), winslash = "/", mustWork = FALSE))
  depletion_files <- unique(unlist(lapply(search_roots, function(root) {
    list.files(root, pattern = "^depletion-smoke[.]csv$", recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE))
  depletion <- kflow_read_csv_union(depletion_files)
  if (nrow(depletion)) {
    parent <- depletion
    parent$model_role <- "parent"
    if (!"derived_source" %in% names(parent)) {
      parent$derived_source <- "upstream"
    }
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
    diagnostic$derived_source <- "diagnostics_smoke_from_parent"
    diagnostic$model_role <- "diagnostics"
    diagnostic$depletion <- round(pmax(0.05, pmin(0.95, as.numeric(diagnostic$depletion) + 0.005)), 3)
    if ("spawning_potential" %in% names(diagnostic)) {
      diagnostic$spawning_potential <- round(suppressWarnings(as.numeric(diagnostic$spawning_potential)) * 1.005, 6)
    }
    if ("recruitment" %in% names(diagnostic)) {
      diagnostic$recruitment <- round(suppressWarnings(as.numeric(diagnostic$recruitment)) * 0.995, 6)
    }
    if ("fishing_mortality" %in% names(diagnostic)) {
      diagnostic$fishing_mortality <- round(suppressWarnings(as.numeric(diagnostic$fishing_mortality)) * 1.005, 6)
    }
    depletion <- rbind(parent, diagnostic)
    utils::write.csv(depletion, file.path(model_dir, "depletion-smoke.csv"), row.names = FALSE)
  } else {
    depletion <- kflow_write_smoke_depletion(model_dir, stage)
    depletion$model_role <- "diagnostics"
    depletion$derived_source <- "synthetic_diagnostics_smoke_fallback"
    utils::write.csv(depletion, file.path(model_dir, "depletion-smoke.csv"), row.names = FALSE)
  }
  utils::write.csv(depletion, file.path(out_dir, "depletion-smoke.csv"), row.names = FALSE)
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
  saveRDS(
    list(
      payload_type = "kflow_smoke_diagnostics",
      payload_version = 1L,
      stage = stage,
      diagnostics = diagnostics,
      depletion = depletion,
      registry = kflow_registry_values(stage),
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ),
    file.path(model_dir, "model_payload.rds"),
    compress = "xz"
  )
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
