# Generic MFCL input recipe helpers.
#
# This file is intentionally package-shaped: the functions avoid project-specific
# names and keep all recipe state in input/output directories. It can be lifted
# into a small package later if the workflow grows beyond this repository.

mfcl_recipe_default <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

`%||%` <- mfcl_recipe_default

mfcl_recipe_env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

mfcl_recipe_bool <- function(value, default = FALSE) {
  if (is.null(value) || !length(value) || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) {
    return(default)
  }
  tolower(trimws(as.character(value[[1]]))) %in% c("1", "true", "yes", "y", "on")
}

mfcl_recipe_split <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) {
    return(character())
  }
  parts <- unlist(strsplit(paste(as.character(value), collapse = ","), "[,;[:space:]]+", perl = TRUE), use.names = FALSE)
  unique(trimws(parts[nzchar(trimws(parts))]))
}

mfcl_recipe_abs <- function(path, root = getwd(), must_work = FALSE) {
  if (!nzchar(path)) {
    return("")
  }
  out <- if (grepl("^/", path)) path else file.path(root, path)
  normalizePath(out, winslash = "/", mustWork = must_work)
}

mfcl_recipe_same_path <- function(a, b) {
  identical(
    normalizePath(a, winslash = "/", mustWork = FALSE),
    normalizePath(b, winslash = "/", mustWork = FALSE)
  )
}

mfcl_recipe_detect_file <- function(path, pattern, label, required = TRUE, preferred = "") {
  if (nzchar(preferred)) {
    candidate <- file.path(path, preferred)
    if (file.exists(candidate)) {
      return(preferred)
    }
    if (isTRUE(required)) {
      stop(label, " file was requested but not found: ", candidate, call. = FALSE)
    }
  }
  x <- list.files(path, pattern = pattern, full.names = FALSE, ignore.case = TRUE)
  if (!length(x)) {
    if (isTRUE(required)) {
      stop("No ", label, " file found in ", path, call. = FALSE)
    }
    return("")
  }
  if (length(x) > 1L) {
    warning("Multiple ", label, " files found; using first: ", x[[1]], call. = FALSE)
  }
  x[[1]]
}

mfcl_recipe_copy_dir <- function(src, dst, overwrite = TRUE) {
  if (!dir.exists(src)) {
    stop("Input directory does not exist: ", src, call. = FALSE)
  }
  if (dir.exists(dst) && isTRUE(overwrite)) {
    unlink(dst, recursive = TRUE, force = TRUE)
  }
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(files)) {
    ok <- file.copy(files, dst, recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
    if (any(!ok)) {
      stop("Failed to copy all files from ", src, " to ", dst, call. = FALSE)
    }
  }
  invisible(normalizePath(dst, winslash = "/", mustWork = TRUE))
}

mfcl_recipe_metadata_file <- function(input_dir) {
  file.path(input_dir, "input_change_metadata.rds")
}

mfcl_recipe_normalize_tokens <- function(x) {
  mfcl_recipe_split(x)
}

mfcl_recipe_read_metadata <- function(input_dir) {
  path <- mfcl_recipe_metadata_file(input_dir)
  if (!file.exists(path)) {
    return(list(version = 1L, tokens = character(), operations = list()))
  }
  meta <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(meta)) {
    meta <- list(version = 1L, tokens = character(), operations = list())
  }
  meta$version <- meta$version %||% 1L
  meta$tokens <- mfcl_recipe_normalize_tokens(meta$tokens)
  if (is.null(meta$operations) || !is.list(meta$operations)) {
    meta$operations <- list()
  }
  meta
}

mfcl_recipe_write_metadata <- function(input_dir, meta) {
  meta$tokens <- mfcl_recipe_normalize_tokens(meta$tokens)
  meta$updated_at <- as.character(Sys.time())
  saveRDS(meta, mfcl_recipe_metadata_file(input_dir), compress = "xz")
  invisible(meta)
}

mfcl_recipe_append_metadata <- function(input_dir,
                                        token,
                                        label = token,
                                        operation = "",
                                        source_dir = "",
                                        details = list()) {
  token <- mfcl_recipe_normalize_tokens(token)
  if (!length(token)) {
    return(invisible(mfcl_recipe_read_metadata(input_dir)))
  }
  meta <- mfcl_recipe_read_metadata(input_dir)
  meta$tokens <- unique(c(meta$tokens, token))
  meta$operations[[length(meta$operations) + 1L]] <- list(
    token = token,
    label = paste(as.character(label), collapse = " "),
    operation = operation,
    source_dir = source_dir,
    target_dir = input_dir,
    created_at = as.character(Sys.time()),
    details = details
  )
  meta$label <- paste(meta$tokens, collapse = " + ")
  labels <- vapply(meta$operations, function(x) as.character(x$label %||% ""), character(1))
  labels <- unique(labels[nzchar(labels)])
  meta$description <- paste(labels, collapse = "; ")
  mfcl_recipe_write_metadata(input_dir, meta)
}

mfcl_recipe_ini_version <- function(path) {
  if (!file.exists(path)) {
    return(NA_integer_)
  }
  lines <- readLines(path, n = 80L, warn = FALSE)
  lines <- sub("#.*$", "", lines)
  tokens <- unlist(strsplit(trimws(lines), "\\s+"), use.names = FALSE)
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) {
    return(NA_integer_)
  }
  out <- suppressWarnings(as.integer(tokens[[1]]))
  if (is.finite(out)) out else NA_integer_
}

mfcl_recipe_convert_ini <- function(path,
                                    to_version = 1007L,
                                    output_path = path,
                                    frq_path = NULL,
                                    validate = FALSE,
                                    prefer_mfclkit = TRUE) {
  from_version <- mfcl_recipe_ini_version(path)
  if (!is.finite(from_version)) {
    stop("Could not detect INI version in ", path, call. = FALSE)
  }
  to_version <- as.integer(to_version)
  if (identical(as.integer(from_version), to_version)) {
    return(invisible(list(
      input_path = path,
      output_path = output_path,
      from_version = as.integer(from_version),
      to_version = to_version,
      converted = FALSE,
      backend = "none"
    )))
  }

  if (isTRUE(prefer_mfclkit) && requireNamespace("mfclkit", quietly = TRUE)) {
    info <- mfclkit::mfk_ini_convert(
      path,
      to_version = to_version,
      output_path = output_path,
      frq_path = frq_path,
      validate = isTRUE(validate),
      backup = FALSE
    )
    info$backend <- "mfclkit"
    return(invisible(info))
  }

  stop(
    "INI conversion ", from_version, " -> ", to_version,
    " requires mfclkit. The flow does not use approximate fallback converters ",
    "because silent input changes can invalidate model interpretation.",
    call. = FALSE
  )
}

mfcl_recipe_normalize_ini_dir <- function(input_dir,
                                          to_version = 1007L,
                                          frq_name = "",
                                          ini_name = "",
                                          validate = FALSE,
                                          prefer_mfclkit = TRUE) {
  if (is.null(to_version) || !nzchar(as.character(to_version)) || as.integer(to_version) <= 0L) {
    return(invisible(NULL))
  }
  ini_name <- mfcl_recipe_detect_file(input_dir, "\\.ini$", ".ini", preferred = ini_name)
  frq_name <- mfcl_recipe_detect_file(input_dir, "\\.frq$", ".frq", required = FALSE, preferred = frq_name)
  ini_path <- file.path(input_dir, ini_name)
  frq_path <- if (nzchar(frq_name)) file.path(input_dir, frq_name) else NULL
  before <- mfcl_recipe_ini_version(ini_path)
  info <- mfcl_recipe_convert_ini(
    ini_path,
    to_version = as.integer(to_version),
    output_path = ini_path,
    frq_path = frq_path,
    validate = validate,
    prefer_mfclkit = prefer_mfclkit
  )
  after <- mfcl_recipe_ini_version(ini_path)
  mfcl_recipe_append_metadata(
    input_dir,
    token = paste0("ini", after),
    label = paste0("INI normalized to version ", after),
    operation = "ini_normalization",
    source_dir = input_dir,
    details = list(before = before, after = after, backend = info$backend %||% "")
  )
  utils::write.csv(
    data.frame(
      ini_file = ini_name,
      frq_file = frq_name,
      from_version = before,
      to_version = after,
      converted = isTRUE(info$converted),
      backend = info$backend %||% "",
      stringsAsFactors = FALSE
    ),
    file.path(input_dir, "ini-version-normalization.csv"),
    row.names = FALSE
  )
  invisible(info)
}

mfcl_recipe_remove_start_files <- function(input_dir) {
  files <- c(
    list.files(input_dir, pattern = "\\.par([0-9]+)?$", full.names = TRUE),
    file.path(input_dir, "indepvar.rpt")
  )
  files <- files[file.exists(files)]
  if (length(files)) {
    unlink(files, force = TRUE)
  }
  basename(files)
}

mfcl_recipe_normalize_fixed_params <- function(x) {
  vals <- toupper(mfcl_recipe_split(x))
  vals <- sub("^GROWTH$", "VB", vals)
  vals <- sub("^NATURALMORTALITY$", "M", vals)
  vals <- sub("^NATURAL_MORTALITY$", "M", vals)
  vals <- sub("^FIXM$", "M", vals)
  vals <- sub("^FIXVB$", "VB", vals)
  vals <- sub("^FIXVBM$", "VBM", vals)
  if ("VBM" %in% vals || "VBANDM" %in% vals) vals <- c(vals, "VB", "M")
  unique(vals[vals %in% c("VB", "M")])
}

mfcl_recipe_switch_lines_to_zero <- function(lines, switch_ids) {
  changes <- data.frame(switch = character(), line = integer(), old = character(), new = character())
  for (switch_id in as.character(switch_ids)) {
    pattern <- paste0("^(\\s*1\\s+", switch_id, "\\s+)([-+]?[0-9.]+)(\\s*(#.*)?)$")
    hits <- grepl(pattern, lines, perl = TRUE)
    if (!any(hits)) next
    hit_idx <- which(hits)
    captures <- regmatches(lines[hits], regexec(pattern, lines[hits], perl = TRUE))
    old_values <- vapply(captures, `[[`, character(1), 3L)
    lines[hits] <- vapply(captures, function(x) paste0(x[[2]], "0", x[[4]]), character(1))
    changes <- rbind(changes, data.frame(switch = switch_id, line = hit_idx, old = old_values, new = "0"))
  }
  list(lines = lines, changes = changes)
}

mfcl_recipe_apply_fixed_parameters <- function(input_dir, fixed_params) {
  groups <- mfcl_recipe_normalize_fixed_params(fixed_params)
  if (!length(groups)) {
    return(invisible(NULL))
  }
  doitall <- file.path(input_dir, "doitall.sh")
  if (!file.exists(doitall)) {
    stop("No doitall.sh found in ", input_dir, call. = FALSE)
  }
  switch_ids <- character()
  if ("M" %in% groups) switch_ids <- c(switch_ids, "121")
  if ("VB" %in% groups) switch_ids <- c(switch_ids, "12", "13", "14")
  lines <- readLines(doitall, warn = FALSE)
  result <- mfcl_recipe_switch_lines_to_zero(lines, unique(switch_ids))
  missing <- setdiff(unique(switch_ids), unique(result$changes$switch))
  if (length(missing)) {
    stop("Could not find required MFCL switch(es) in doitall.sh: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  writeLines(result$lines, doitall, useBytes = TRUE)
  removed <- mfcl_recipe_remove_start_files(input_dir)
  token <- if (all(c("M", "VB") %in% groups)) c("fixM", "fixVB") else if ("M" %in% groups) "fixM" else "fixVB"
  label <- if (all(c("M", "VB") %in% groups)) {
    "Natural mortality and growth fixed"
  } else if ("M" %in% groups) {
    "Natural mortality fixed"
  } else {
    "Growth fixed"
  }
  info <- list(fixed_groups = groups, switches_zeroed = result$changes, removed_start_files = removed)
  saveRDS(info, file.path(input_dir, "fixed-parameters-info.rds"), compress = "xz")
  mfcl_recipe_append_metadata(input_dir, token = token, label = label, operation = "fixed_parameters", details = info)
  invisible(info)
}

mfcl_recipe_flag92_entries <- function(lines) {
  pattern <- "(^|[[:space:]])(-?[0-9]+)([[:space:]]+92[[:space:]]+)([0-9]+)"
  rows <- list()
  for (idx in seq_along(lines)) {
    matches <- gregexpr(pattern, lines[[idx]], perl = TRUE)[[1]]
    if (identical(matches[[1]], -1L)) next
    lengths <- attr(matches, "match.length")
    texts <- substring(lines[[idx]], matches, matches + lengths - 1L)
    rows <- c(rows, lapply(seq_along(texts), function(match_idx) {
      h <- regmatches(texts[[match_idx]], regexec("^([[:space:]]*)(-?[0-9]+)([[:space:]]+92[[:space:]]+)([0-9]+)$", texts[[match_idx]]))[[1]]
      data.frame(
        line = idx,
        match_start = matches[[match_idx]],
        match_length = lengths[[match_idx]],
        prefix = paste0(h[[2]], h[[3]], h[[4]]),
        fishery = as.integer(h[[3]]),
        value = as.integer(h[[5]])
      )
    }))
  }
  if (!length(rows)) {
    return(data.frame(line = integer(), match_start = integer(), match_length = integer(), prefix = character(), fishery = integer(), value = integer()))
  }
  do.call(rbind, rows)
}

mfcl_recipe_replace_inline_matches <- function(lines, entries, value_col = "updated") {
  entries$replacement <- paste0(entries$prefix, entries[[value_col]])
  for (line_idx in unique(entries$line)) {
    line_entries <- entries[entries$line == line_idx, , drop = FALSE]
    line_entries <- line_entries[order(line_entries$match_start, decreasing = TRUE), , drop = FALSE]
    line <- lines[[line_idx]]
    for (entry_idx in seq_len(nrow(line_entries))) {
      start <- line_entries$match_start[[entry_idx]]
      end <- start + line_entries$match_length[[entry_idx]] - 1L
      line <- paste0(substr(line, 1L, start - 1L), line_entries$replacement[[entry_idx]], substr(line, end + 1L, nchar(line)))
    }
    lines[[line_idx]] <- line
  }
  lines
}

mfcl_recipe_apply_index_cv_half <- function(input_dir, fisheries = "") {
  doitall <- file.path(input_dir, "doitall.sh")
  if (!file.exists(doitall)) {
    stop("No doitall.sh found in ", input_dir, call. = FALSE)
  }
  target <- suppressWarnings(as.integer(mfcl_recipe_split(fisheries)))
  target <- unique(target[is.finite(target)])
  lines <- readLines(doitall, warn = FALSE)
  entries <- mfcl_recipe_flag92_entries(lines)
  entries <- entries[entries$fishery < 0, , drop = FALSE]
  if (length(target)) {
    entries <- entries[abs(entries$fishery) %in% abs(target), , drop = FALSE]
  }
  if (!nrow(entries)) {
    stop("No negative fishery flag 92 entries found for index CV half recipe.", call. = FALSE)
  }
  entries$updated <- floor(entries$value / 2 + 0.5)
  lines <- mfcl_recipe_replace_inline_matches(lines, entries)
  writeLines(lines, doitall, useBytes = TRUE)
  removed <- mfcl_recipe_remove_start_files(input_dir)
  info <- list(index_fisheries = entries$fishery, original = entries$value, updated = entries$updated, removed_start_files = removed)
  saveRDS(info, file.path(input_dir, "index-cv-half-info.rds"), compress = "xz")
  mfcl_recipe_append_metadata(input_dir, token = "cvH", label = "Index CPUE CV flags halved", operation = "index_cv_half", details = info)
  invisible(info)
}

mfcl_recipe_parse_ints <- function(x) {
  vals <- suppressWarnings(as.integer(mfcl_recipe_split(x)))
  unique(vals[is.finite(vals)])
}

mfcl_recipe_apply_selectivity_nodes <- function(input_dir, node_count, fisheries = "") {
  node_count <- suppressWarnings(as.integer(node_count[[1]]))
  if (!is.finite(node_count) || node_count <= 0L) {
    stop("Selectivity node count must be a positive integer.", call. = FALSE)
  }
  target_fisheries <- mfcl_recipe_parse_ints(fisheries)
  doitall <- file.path(input_dir, "doitall.sh")
  if (!file.exists(doitall)) {
    stop("No doitall.sh found in ", input_dir, call. = FALSE)
  }
  lines <- readLines(doitall, warn = FALSE)
  pattern <- "^([[:space:]]*)(-?[0-9]+)([[:space:]]+61[[:space:]]+)([0-9]+)(.*)$"
  hits <- regexec(pattern, lines)
  parsed <- regmatches(lines, hits)
  rows <- lapply(seq_along(parsed), function(line_idx) {
    h <- parsed[[line_idx]]
    if (length(h) != 6L) {
      return(NULL)
    }
    data.frame(line = line_idx, prefix = paste0(h[[2]], h[[3]], h[[4]]), fishery = as.integer(h[[3]]), value = as.integer(h[[5]]), suffix = h[[6]])
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    stop("No fish flag 61 entries found for selectivity node recipe.", call. = FALSE)
  }
  entries <- do.call(rbind, rows)
  if (length(target_fisheries)) {
    entries <- entries[abs(entries$fishery) %in% abs(target_fisheries), , drop = FALSE]
  } else if (any(entries$fishery == -999L)) {
    entries <- entries[entries$fishery == -999L, , drop = FALSE]
  }
  if (!nrow(entries)) {
    stop("No requested fish flag 61 entries found.", call. = FALSE)
  }
  for (idx in seq_len(nrow(entries))) {
    lines[[entries$line[[idx]]]] <- paste0(entries$prefix[[idx]], node_count, entries$suffix[[idx]])
  }
  writeLines(lines, doitall, useBytes = TRUE)
  removed <- mfcl_recipe_remove_start_files(input_dir)
  info <- list(node_count = node_count, target_fisheries = entries$fishery, original = entries$value, updated = rep(node_count, nrow(entries)), removed_start_files = removed)
  saveRDS(info, file.path(input_dir, "selectivity-nodes-info.rds"), compress = "xz")
  mfcl_recipe_append_metadata(input_dir, token = paste0("sel", node_count), label = paste0(node_count, "-node selectivity splines"), operation = "selectivity_spline_nodes", details = info)
  invisible(info)
}

mfcl_recipe_apply_tag_mixing <- function(input_dir, periods, groups = "") {
  if (!requireNamespace("mfclkit", quietly = TRUE)) {
    stop("mfclkit is required for tag mixing recipes.", call. = FALSE)
  }
  ini <- file.path(input_dir, mfcl_recipe_detect_file(input_dir, "\\.ini$", ".ini"))
  frq_name <- mfcl_recipe_detect_file(input_dir, "\\.frq$", ".frq", required = FALSE)
  frq <- if (nzchar(frq_name)) file.path(input_dir, frq_name) else ""
  groups_int <- mfcl_recipe_parse_ints(groups)
  periods_int <- mfcl_recipe_parse_ints(periods)
  if (!length(periods_int)) {
    stop("Tag mixing periods are required.", call. = FALSE)
  }
  mfclkit::mfk_ini_set_tag_mixing_period(
    ini,
    periods = periods_int,
    groups = if (length(groups_int)) groups_int else NULL,
    frq_path = if (file.exists(frq)) frq else NULL
  )
  info <- list(periods = periods_int, groups = groups_int)
  mfcl_recipe_append_metadata(input_dir, token = paste0("tagMix", paste(periods_int, collapse = "_")), label = "Tag mixing period edit", operation = "tag_mixing", details = info)
  invisible(info)
}

mfcl_recipe_apply_tag_reporting <- function(input_dir, multiplier, groups = "", fisheries = "") {
  if (!requireNamespace("mfclkit", quietly = TRUE)) {
    stop("mfclkit is required for tag reporting recipes.", call. = FALSE)
  }
  multiplier <- suppressWarnings(as.numeric(multiplier[[1]]))
  if (!is.finite(multiplier)) {
    stop("Tag reporting multiplier must be numeric.", call. = FALSE)
  }
  ini <- file.path(input_dir, mfcl_recipe_detect_file(input_dir, "\\.ini$", ".ini"))
  groups_int <- mfcl_recipe_parse_ints(groups)
  fisheries_int <- mfcl_recipe_parse_ints(fisheries)
  mfclkit::mfk_ini_scale_tag_reporting(
    ini,
    multiplier = multiplier,
    groups = if (length(groups_int)) groups_int else NULL,
    fisheries = if (length(fisheries_int)) fisheries_int else NULL
  )
  info <- list(multiplier = multiplier, groups = groups_int, fisheries = fisheries_int)
  mfcl_recipe_append_metadata(input_dir, token = paste0("tagRep", format(multiplier, trim = TRUE)), label = "Tag reporting multiplier edit", operation = "tag_reporting", details = info)
  invisible(info)
}

mfcl_recipe_apply_ini_section <- function(input_dir, section, multiplier = "", value = "", rows = "", cols = "", exact = FALSE) {
  if (!requireNamespace("mfclkit", quietly = TRUE)) {
    stop("mfclkit is required for generic INI section recipes.", call. = FALSE)
  }
  if (!nzchar(section)) {
    return(invisible(NULL))
  }
  ini <- file.path(input_dir, mfcl_recipe_detect_file(input_dir, "\\.ini$", ".ini"))
  row_idx <- mfcl_recipe_parse_ints(rows)
  col_idx <- mfcl_recipe_parse_ints(cols)
  if (nzchar(multiplier)) {
    mult <- suppressWarnings(as.numeric(multiplier))
    mfclkit::mfk_ini_scale_section(
      ini,
      section,
      multiplier = mult,
      rows = if (length(row_idx)) row_idx else NULL,
      cols = if (length(col_idx)) col_idx else NULL,
      exact = exact
    )
  } else {
    vals <- suppressWarnings(as.numeric(mfcl_recipe_split(value)))
    mfclkit::mfk_ini_set_section(
      ini,
      section,
      value = vals,
      rows = if (length(row_idx)) row_idx else NULL,
      cols = if (length(col_idx)) col_idx else NULL,
      exact = exact
    )
  }
  info <- list(section = section, multiplier = multiplier, value = value, rows = row_idx, cols = col_idx, exact = exact)
  mfcl_recipe_append_metadata(input_dir, token = paste0("ini", gsub("[^A-Za-z0-9]+", "", section)), label = paste("INI section edit:", section), operation = "ini_section", details = info)
  invisible(info)
}

mfcl_recipe_write_summary <- function(input_dir, out_dir, recipe_token, source_input, output_input, actions) {
  meta <- mfcl_recipe_read_metadata(input_dir)
  summary <- data.frame(
    recipe_token = recipe_token,
    source_input = source_input,
    output_input = output_input,
    tokens = paste(meta$tokens, collapse = ","),
    actions = paste(actions, collapse = ","),
    ini_version = {
      ini <- mfcl_recipe_detect_file(input_dir, "\\.ini$", ".ini", required = FALSE)
      if (nzchar(ini)) mfcl_recipe_ini_version(file.path(input_dir, ini)) else NA_integer_
    },
    stringsAsFactors = FALSE
  )
  utils::write.csv(summary, file.path(input_dir, "mfcl-input-recipe-summary.csv"), row.names = FALSE)
  if (nzchar(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(summary, file.path(out_dir, "mfcl-input-recipe-summary.csv"), row.names = FALSE)
  }
  invisible(summary)
}

mfcl_recipe_run <- function(input_dir,
                            output_dir,
                            recipe_token,
                            to_ini_version = 1007L,
                            normalize_ini = TRUE,
                            ini_validate = FALSE,
                            prefer_mfclkit = TRUE,
                            fixed_params = "",
                            index_cv_half = FALSE,
                            index_fisheries = "",
                            sel_nodes = "",
                            sel_fisheries = "",
                            tag_mixing_periods = "",
                            tag_mixing_groups = "",
                            tag_reporting_multiplier = "",
                            tag_reporting_groups = "",
                            tag_reporting_fisheries = "",
                            ini_section = "",
                            ini_section_multiplier = "",
                            ini_section_value = "",
                            ini_section_rows = "",
                            ini_section_cols = "",
                            ini_section_exact = FALSE,
                            out_dir = "") {
  input_dir <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
  output_dir <- mfcl_recipe_abs(output_dir, must_work = FALSE)
  in_place <- mfcl_recipe_same_path(input_dir, output_dir)
  work_dir <- if (in_place) tempfile("mfcl-recipe-input-") else output_dir
  mfcl_recipe_copy_dir(input_dir, work_dir, overwrite = TRUE)

  actions <- character()
  if (isTRUE(normalize_ini)) {
    mfcl_recipe_normalize_ini_dir(
      work_dir,
      to_version = to_ini_version,
      validate = ini_validate,
      prefer_mfclkit = prefer_mfclkit
    )
    actions <- c(actions, paste0("ini", to_ini_version))
  }
  if (nzchar(fixed_params)) {
    mfcl_recipe_apply_fixed_parameters(work_dir, fixed_params)
    actions <- c(actions, paste0("fixed:", fixed_params))
  }
  if (isTRUE(index_cv_half)) {
    mfcl_recipe_apply_index_cv_half(work_dir, index_fisheries)
    actions <- c(actions, "index_cv_half")
  }
  if (nzchar(sel_nodes)) {
    mfcl_recipe_apply_selectivity_nodes(work_dir, sel_nodes, sel_fisheries)
    actions <- c(actions, paste0("sel_nodes:", sel_nodes))
  }
  if (nzchar(tag_mixing_periods)) {
    mfcl_recipe_apply_tag_mixing(work_dir, tag_mixing_periods, tag_mixing_groups)
    actions <- c(actions, paste0("tag_mixing:", tag_mixing_periods))
  }
  if (nzchar(tag_reporting_multiplier)) {
    mfcl_recipe_apply_tag_reporting(work_dir, tag_reporting_multiplier, tag_reporting_groups, tag_reporting_fisheries)
    actions <- c(actions, paste0("tag_reporting:", tag_reporting_multiplier))
  }
  if (nzchar(ini_section)) {
    mfcl_recipe_apply_ini_section(
      work_dir,
      section = ini_section,
      multiplier = ini_section_multiplier,
      value = ini_section_value,
      rows = ini_section_rows,
      cols = ini_section_cols,
      exact = ini_section_exact
    )
    actions <- c(actions, paste0("ini_section:", ini_section))
  }
  if (!length(actions)) {
    actions <- "copy_only"
    mfcl_recipe_append_metadata(work_dir, token = recipe_token, label = paste("Copied input for", recipe_token), operation = "copy_only", source_dir = input_dir)
  }

  if (in_place) {
    mfcl_recipe_copy_dir(work_dir, output_dir, overwrite = TRUE)
    unlink(work_dir, recursive = TRUE, force = TRUE)
  }
  mfcl_recipe_write_summary(output_dir, out_dir, recipe_token, input_dir, output_dir, actions)
}

mfcl_recipe_run_from_env <- function() {
  mfcl_recipe_run(
    input_dir = mfcl_recipe_env("KFLOW_PATCH_INPUT_DIR"),
    output_dir = mfcl_recipe_env("KFLOW_PATCH_OUTPUT_DIR"),
    recipe_token = mfcl_recipe_env("RECIPE_TOKEN", mfcl_recipe_env("MODEL_TOKEN", "Recipe")),
    to_ini_version = as.integer(mfcl_recipe_env("INPUT_RECIPE_TO_INI_VERSION", mfcl_recipe_env("MFCL_INI_VERSION_TARGET", "1007"))),
    normalize_ini = mfcl_recipe_bool(mfcl_recipe_env("INPUT_RECIPE_NORMALIZE_INI", "1"), TRUE),
    ini_validate = mfcl_recipe_bool(mfcl_recipe_env("INPUT_RECIPE_VALIDATE_INI", "1"), TRUE),
    prefer_mfclkit = mfcl_recipe_bool(mfcl_recipe_env("INPUT_RECIPE_PREFER_MFCLKIT", "1"), TRUE),
    fixed_params = mfcl_recipe_env("INPUT_RECIPE_FIXED_PARAMS", ""),
    index_cv_half = mfcl_recipe_bool(mfcl_recipe_env("INPUT_RECIPE_INDEX_CV_HALF", "0"), FALSE),
    index_fisheries = mfcl_recipe_env("INPUT_RECIPE_INDEX_FISHERIES", ""),
    sel_nodes = mfcl_recipe_env("INPUT_RECIPE_SEL_NODES", ""),
    sel_fisheries = mfcl_recipe_env("INPUT_RECIPE_SEL_FISHERIES", ""),
    tag_mixing_periods = mfcl_recipe_env("INPUT_RECIPE_TAG_MIXING_PERIODS", ""),
    tag_mixing_groups = mfcl_recipe_env("INPUT_RECIPE_TAG_MIXING_GROUPS", ""),
    tag_reporting_multiplier = mfcl_recipe_env("INPUT_RECIPE_TAG_REPORTING_MULTIPLIER", ""),
    tag_reporting_groups = mfcl_recipe_env("INPUT_RECIPE_TAG_REPORTING_GROUPS", ""),
    tag_reporting_fisheries = mfcl_recipe_env("INPUT_RECIPE_TAG_REPORTING_FISHERIES", ""),
    ini_section = mfcl_recipe_env("INPUT_RECIPE_INI_SECTION", ""),
    ini_section_multiplier = mfcl_recipe_env("INPUT_RECIPE_INI_SECTION_MULTIPLIER", ""),
    ini_section_value = mfcl_recipe_env("INPUT_RECIPE_INI_SECTION_VALUE", ""),
    ini_section_rows = mfcl_recipe_env("INPUT_RECIPE_INI_SECTION_ROWS", ""),
    ini_section_cols = mfcl_recipe_env("INPUT_RECIPE_INI_SECTION_COLS", ""),
    ini_section_exact = mfcl_recipe_bool(mfcl_recipe_env("INPUT_RECIPE_INI_SECTION_EXACT", "0"), FALSE),
    out_dir = mfcl_recipe_env("KFLOW_PATCH_OUT_DIR", "")
  )
}
