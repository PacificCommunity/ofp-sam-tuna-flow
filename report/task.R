source("../R/job_helpers.R")

ctx <- kflow_stage_start("report")
kflow_write_manifest(ctx$input_dir, file.path(ctx$out_dir, "input-manifest.csv"))

render_format <- tolower(kflow_env("REPORT_RENDER_FORMAT", "pdf"))
plot_pattern <- if (identical(render_format, "pdf")) "[.](svg|png|jpg|jpeg|pdf)$" else "[.](svg|png|jpg|jpeg)$"

relative_to <- function(path, root) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (startsWith(path, paste0(root, "/"))) {
    return(substring(path, nchar(root) + 2L))
  }
  basename(path)
}

scan_plot_files <- function(root) {
  if (!nzchar(root) || !dir.exists(root)) {
    return(data.frame(file = character(), rel = character(), stringsAsFactors = FALSE))
  }
  files <- list.files(root, pattern = plot_pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  data.frame(
    file = normalizePath(files, winslash = "/", mustWork = FALSE),
    rel = vapply(files, relative_to, character(1), root = root),
    stringsAsFactors = FALSE
  )
}

plot_priority <- function(rel) {
  if (grepl("(^|/)report-figures/key-quantities-smoke[.]png$", rel, ignore.case = TRUE)) return(1L)
  if (grepl("(^|/)key-quantities-smoke[.]png$", rel, ignore.case = TRUE)) return(2L)
  if (grepl("(^|/)report-figures/depletion-smoke[.]png$", rel, ignore.case = TRUE)) return(3L)
  if (grepl("(^|/)depletion-smoke[.]png$", rel, ignore.case = TRUE)) return(4L)
  if (grepl("(^|/)report-figures/", rel, ignore.case = TRUE)) return(5L)
  if (grepl("model-exploration-overview[.](png|svg)$", rel, ignore.case = TRUE)) return(6L)
  20L
}

figure_label <- function(rel) {
  stem <- tolower(tools::file_path_sans_ext(basename(rel)))
  if (identical(stem, "key-quantities-smoke")) {
    return("Key derived quantities")
  }
  if (identical(stem, "depletion-smoke")) {
    return("Depletion")
  }
  label <- gsub("[-_]+", " ", stem)
  paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
}

yaml_scalar <- function(value) {
  sprintf("\"%s\"", gsub("\"", "'", as.character(value %||% "")))
}

rewrite_yaml_scalars <- function(path, values) {
  values <- values[nzchar(names(values)) & nzchar(as.character(values))]
  if (!length(values)) {
    return(invisible(FALSE))
  }
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character()
  for (key in names(values)) {
    replacement <- sprintf("%s: %s", key, yaml_scalar(values[[key]]))
    hit <- grep(sprintf("^%s\\s*:", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", key)), lines)
    if (length(hit)) {
      lines[[hit[[1]]]] <- replacement
    } else {
      lines <- c(lines, replacement)
    }
  }
  writeLines(lines, path)
  invisible(TRUE)
}

figure_input_roots <- kflow_split(kflow_env("REPORT_FIGURE_INPUT_DIR", ""))
figure_roots <- unique(c(ctx$input_dir, figure_input_roots))
plot_index <- do.call(rbind, lapply(figure_roots, scan_plot_files))
if (!is.null(plot_index) && nrow(plot_index)) {
  plot_index <- plot_index[order(
    vapply(plot_index$rel, plot_priority, integer(1)),
    ifelse(grepl("[.]png$", plot_index$rel, ignore.case = TRUE), 0L, 1L),
    plot_index$rel
  ), , drop = FALSE]
  if (identical(render_format, "pdf")) {
    has_non_svg <- any(!grepl("[.]svg$", plot_index$rel, ignore.case = TRUE))
    if (has_non_svg) {
      plot_index <- plot_index[!grepl("[.]svg$", plot_index$rel, ignore.case = TRUE), , drop = FALSE]
    }
  }
  figure_keys <- tolower(tools::file_path_sans_ext(basename(plot_index$rel)))
  plot_index <- plot_index[!duplicated(figure_keys), , drop = FALSE]
  file_hashes <- as.character(tools::md5sum(plot_index$file))
  plot_index <- plot_index[!duplicated(file_hashes), , drop = FALSE]
} else {
  plot_index <- data.frame(file = character(), rel = character(), stringsAsFactors = FALSE)
}

registry_files <- list.files(ctx$input_dir, pattern = "^model-registry[.]csv$", recursive = TRUE, full.names = TRUE)
summary_files <- list.files(ctx$input_dir, pattern = "summary[.]csv$", recursive = TRUE, full.names = TRUE)
registries <- kflow_read_csv_union(registry_files)
summaries <- kflow_read_csv_union(summary_files)
utils::write.csv(registries, file.path(ctx$out_dir, "report-input-registry.csv"), row.names = FALSE)
utils::write.csv(summaries, file.path(ctx$out_dir, "report-input-summaries.csv"), row.names = FALSE)

report_source_repo <- kflow_env("REPORT_SOURCE_REPO", "")
report_source_ref <- kflow_env("REPORT_SOURCE_REF", "main")
report_source_path <- kflow_env("REPORT_SOURCE_PATH", "")
template_dir_setting <- kflow_env("REPORT_TEMPLATE_DIR", "templates/tuna-assessment")
template_main <- kflow_env("REPORT_TEMPLATE_MAIN", kflow_env("REPORT_MAIN", if (nzchar(report_source_repo)) "assessment-report.qmd" else "assessment.qmd"))
title <- kflow_env("REPORT_TITLE", if (nzchar(report_source_repo)) "" else "Tuna Kflow report")
report_file_stem <- kflow_env("REPORT_FILE_STEM", if (nzchar(report_source_repo)) "bet-2026-report" else "tuna-flow-report")
effective_render_format <- render_format
report_ext <- if (identical(effective_render_format, "pdf")) "pdf" else "html"
report_file <- paste0(report_file_stem, ".", report_ext)

if (nzchar(report_source_repo)) {
  report_checkout <- kflow_clone_github_repo(
    report_source_repo,
    report_source_ref,
    work_dir = file.path("work", "report-source"),
    log_file = ctx$log_file
  )
  report_source_root <- if (nzchar(report_source_path)) file.path(report_checkout, report_source_path) else report_checkout
  if (!dir.exists(report_source_root)) {
    stop(sprintf("Report source path was not found: %s", report_source_root), call. = FALSE)
  }
  render_dir <- file.path(ctx$out_dir, "quarto-render")
  kflow_copy_tree(report_source_root, render_dir)
} else {
  template_candidates <- unique(c(
    template_dir_setting,
    file.path(getwd(), template_dir_setting),
    file.path(kflow_flow_root(), "report", template_dir_setting)
  ))
  template_candidates <- normalizePath(template_candidates, winslash = "/", mustWork = FALSE)
  template_source <- template_candidates[dir.exists(template_candidates)][1]
  if (is.na(template_source)) {
    stop(sprintf("Report template directory was not found: %s", template_dir_setting), call. = FALSE)
  }
  render_dir <- file.path(ctx$out_dir, "quarto-render")
  kflow_copy_tree(template_source, render_dir)
}

main_qmd <- file.path(render_dir, template_main)
if (!file.exists(main_qmd)) {
  stop(sprintf("Report template main file was not found: %s", main_qmd), call. = FALSE)
}

report_config_file <- file.path(render_dir, "report-config.yml")
if (nzchar(title) && file.exists(report_config_file)) {
  rewrite_yaml_scalars(
    report_config_file,
    c(
      title = title,
      species = kflow_env("FLOW_SPECIES", ""),
      species_label = kflow_env("FLOW_SPECIES_LABEL", ""),
      assessment_year = kflow_env("FLOW_ASSESSMENT_YEAR", "")
    )
  )
} else if (nzchar(title)) {
  main_lines <- readLines(main_qmd, warn = FALSE)
  title_line <- grep("^title:", main_lines)[1]
  if (!is.na(title_line)) {
    main_lines[[title_line]] <- sprintf("title: \"%s\"", gsub("\"", "'", title))
    writeLines(main_lines, main_qmd)
  }
}

input_dir_template <- file.path(render_dir, "kflow-inputs")
dir.create(input_dir_template, recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(file.path(ctx$out_dir, "report-input-registry.csv"), input_dir_template, overwrite = TRUE))
invisible(file.copy(file.path(ctx$out_dir, "report-input-summaries.csv"), input_dir_template, overwrite = TRUE))

figure_dir_setting <- kflow_env("REPORT_FIGURE_DIR", if (nzchar(report_source_repo)) "Figures/kflow" else "Figures")
plot_dir <- file.path(render_dir, figure_dir_setting)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
copied <- character()
copied_sources <- character()
if (nrow(plot_index)) {
  for (index in seq_len(nrow(plot_index))) {
    source_file <- plot_index$file[[index]]
    target <- if (index == 1L && grepl("key-quantities-smoke[.]png$", source_file, ignore.case = TRUE)) {
      "key-quantities-smoke.png"
    } else if (index == 1L && grepl("depletion-smoke[.]png$", source_file, ignore.case = TRUE)) {
      "depletion-smoke.png"
    } else {
      sprintf("plot-%03d.%s", index, tools::file_ext(source_file))
    }
    file.copy(source_file, file.path(plot_dir, target), overwrite = TRUE)
    copied <- c(copied, file.path(figure_dir_setting, target))
    copied_sources <- c(copied_sources, plot_index$rel[[index]])
  }
}

copy_figure_tree <- kflow_bool(
  "REPORT_COPY_FIGURE_TREE",
  identical(tolower(kflow_env("REPORT_REWRITE_FIGURES", "")), "false")
)
if (isTRUE(copy_figure_tree) && length(figure_input_roots)) {
  figures_root <- file.path(render_dir, "Figures")
  dir.create(figures_root, recursive = TRUE, showWarnings = FALSE)
  for (root in figure_input_roots) {
    if (!dir.exists(root)) {
      next
    }
    entries <- list.files(root, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(entries)) {
      ok <- file.copy(entries, figures_root, recursive = TRUE, overwrite = TRUE, copy.date = TRUE)
      if (any(!ok)) {
        stop(sprintf("Failed to copy some report figure files from %s", root), call. = FALSE)
      }
    }
  }
}

existing_render_figures <- list.files(file.path(render_dir, "Figures"), pattern = plot_pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
if (!length(copied) && !length(existing_render_figures) && kflow_bool("REPORT_REQUIRE_PLOTS", FALSE)) {
  stop("REPORT_REQUIRE_PLOTS is true but no upstream or report-source plot images were found.", call. = FALSE)
}

section_dir <- file.path(render_dir, "sections")
dir.create(section_dir, recursive = TRUE, showWarnings = FALSE)

rewrite_tables_default <- !nzchar(report_source_repo) || !file.exists(file.path(section_dir, "Tables.qmd"))
if (kflow_bool("REPORT_REWRITE_TABLES", rewrite_tables_default)) {
  table_section <- c(
    "# Kflow Model Summary",
    "",
    "```{r kflow-model-registry, echo=FALSE, message=FALSE, warning=FALSE}",
    "safe_read_csv <- function(path) {",
    "  tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())",
    "}",
    "registry <- safe_read_csv('kflow-inputs/report-input-registry.csv')",
    "columns <- intersect(c('stage', 'model_token', 'model_label', 'change_token', 'change_group', 'parent_model_token', 'recipe_token', 'flow_species', 'flow_assessment_year'), names(registry))",
    "if (nrow(registry) && length(columns)) {",
    "  print(utils::head(registry[columns], 30))",
    "} else {",
    "  cat('No Kflow registry rows were provided.')",
    "}",
    "```",
    "",
    "```{r kflow-job-summaries, echo=FALSE, message=FALSE, warning=FALSE}",
    "summaries <- safe_read_csv('kflow-inputs/report-input-summaries.csv')",
    "if (nrow(summaries)) {",
    "  print(utils::head(summaries, 30))",
    "} else {",
    "  cat('No Kflow summary rows were provided.')",
    "}",
    "```"
  )
  writeLines(table_section, file.path(section_dir, "Tables.qmd"))
}

rewrite_figures_default <- !nzchar(report_source_repo) || length(copied) > 0L || !file.exists(file.path(section_dir, "Figures.qmd"))
if (kflow_bool("REPORT_REWRITE_FIGURES", rewrite_figures_default)) {
  figure_section <- if (length(copied)) {
    c(
      "# Figures",
      "",
      "Kflow-generated figures from upstream plot jobs.",
      "",
      unlist(lapply(seq_along(copied), function(index) {
        c(
          sprintf("## %s", figure_label(copied_sources[[index]])),
          "",
          sprintf("![](%s){#fig-kflow-%03d fig-align=\"center\" width=100%%}", copied[[index]], index),
          ""
        )
      }), use.names = FALSE)
    )
  } else {
    c("# Figures", "", "No plot images were provided by upstream jobs.")
  }
  writeLines(figure_section, file.path(section_dir, "Figures.qmd"))
}

render_command <- kflow_env("REPORT_RENDER_COMMAND", "")
if (nzchar(render_command)) {
  kflow_run_shell(render_command, workdir = render_dir, log_file = ctx$log_file, sanitize_env = TRUE)
} else {
  quarto_available <- nzchar(Sys.which("quarto"))
  if (!quarto_available) {
    stop("Quarto is required for report rendering but was not found on PATH.", call. = FALSE)
  }
  old <- setwd(render_dir)
  status <- tryCatch(
    system2("quarto", c("render", template_main, "--to", effective_render_format, "--output", report_file)),
    finally = setwd(old)
  )
  if (!identical(status, 0L)) {
    stop("quarto render failed", call. = FALSE)
  }
}

report_output_file <- kflow_env("REPORT_OUTPUT_FILE", report_file)
rendered_candidates <- unique(c(
  file.path(render_dir, report_output_file),
  file.path(render_dir, report_file),
  list.files(render_dir, pattern = sprintf("[.]%s$", report_ext), recursive = FALSE, full.names = TRUE, ignore.case = TRUE)
))
rendered_report <- rendered_candidates[file.exists(rendered_candidates)][1]
if (is.na(rendered_report)) {
  stop(sprintf("Rendered report file was not found. Expected one of: %s", paste(basename(rendered_candidates), collapse = ", ")), call. = FALSE)
}
final_report_file <- basename(report_file)
invisible(file.copy(rendered_report, file.path(ctx$out_dir, final_report_file), overwrite = TRUE))

report_summary <- data.frame(
  run_label = kflow_env("RUN_LABEL", ""),
  job_key = kflow_env("JOB_KEY", ""),
  registry_rows = nrow(registries),
  summary_rows = nrow(summaries),
  upstream_plot_files = nrow(plot_index),
  copied_plot_files = length(copied),
  report_source_repo = report_source_repo,
  report_source_ref = report_source_ref,
  report_source_path = report_source_path,
  primary_plot_file = if (length(copied)) copied[[1]] else "",
  primary_plot_source = if (length(copied_sources)) copied_sources[[1]] else "",
  template_dir = if (nzchar(report_source_repo)) report_source_repo else template_dir_setting,
  template_main = template_main,
  requested_render_format = render_format,
  effective_render_format = effective_render_format,
  report_file = final_report_file,
  stringsAsFactors = FALSE
)
utils::write.csv(report_summary, file.path(ctx$out_dir, "report-summary.csv"), row.names = FALSE)
writeLines(capture.output(print(report_summary)), file.path(ctx$out_dir, "report-summary.txt"))
kflow_write_registry(ctx$out_dir, "report")
kflow_write_summary(ctx$out_dir, "report")
kflow_compact_outputs(
  ctx$out_dir,
  keep = c(
    final_report_file,
    "report-summary.csv",
    "model-registry.csv"
  )
)
