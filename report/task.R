source("../R/job_helpers.R")

ctx <- kflow_stage_start("report")
kflow_write_manifest(ctx$input_dir, file.path(ctx$out_dir, "input-manifest.csv"))

registry_files <- list.files(ctx$input_dir, pattern = "^model-registry[.]csv$", recursive = TRUE, full.names = TRUE)
summary_files <- list.files(ctx$input_dir, pattern = "summary[.]csv$", recursive = TRUE, full.names = TRUE)
plot_files <- list.files(ctx$input_dir, pattern = "[.](svg|png|jpg|jpeg)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

registries <- kflow_read_csv_union(registry_files)
summaries <- kflow_read_csv_union(summary_files)
utils::write.csv(registries, file.path(ctx$out_dir, "report-input-registry.csv"), row.names = FALSE)
utils::write.csv(summaries, file.path(ctx$out_dir, "report-input-summaries.csv"), row.names = FALSE)

template_dir_setting <- kflow_env("REPORT_TEMPLATE_DIR", "templates/bet-2026")
template_main <- kflow_env("REPORT_TEMPLATE_MAIN", "bet-2026.qmd")
render_format <- kflow_env("REPORT_RENDER_FORMAT", "html")
title <- kflow_env("REPORT_TITLE", "Tuna Kflow report")
report_file_stem <- kflow_env("REPORT_FILE_STEM", "tuna-flow-report")
quarto_available <- nzchar(Sys.which("quarto"))
if (!quarto_available) {
  stop("Quarto is required for report rendering but was not found on PATH.", call. = FALSE)
}
effective_render_format <- render_format
report_ext <- if (identical(effective_render_format, "pdf")) "pdf" else "html"
report_file <- paste0(report_file_stem, ".", report_ext)

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
main_qmd <- file.path(render_dir, template_main)
if (!file.exists(main_qmd)) {
  stop(sprintf("Report template main file was not found: %s", main_qmd), call. = FALSE)
}

main_lines <- readLines(main_qmd, warn = FALSE)
title_line <- grep("^title:", main_lines)[1]
if (!is.na(title_line)) {
  main_lines[[title_line]] <- sprintf("title: \"%s\"", gsub("\"", "'", title))
  writeLines(main_lines, main_qmd)
}

input_dir_template <- file.path(render_dir, "kflow-inputs")
dir.create(input_dir_template, recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(file.path(ctx$out_dir, "report-input-registry.csv"), input_dir_template, overwrite = TRUE))
invisible(file.copy(file.path(ctx$out_dir, "report-input-summaries.csv"), input_dir_template, overwrite = TRUE))

plot_dir <- file.path(render_dir, "Figures")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
copied <- character()
for (index in seq_along(plot_files)) {
  target <- sprintf("plot-%03d.%s", index, tools::file_ext(plot_files[index]))
  file.copy(plot_files[index], file.path(plot_dir, target), overwrite = TRUE)
  copied <- c(copied, file.path("Figures", target))
}

table_section <- c(
  "# Kflow Model Summary",
  "",
  "```{r kflow-model-registry, echo=FALSE, message=FALSE, warning=FALSE}",
  "registry <- read.csv('kflow-inputs/report-input-registry.csv', stringsAsFactors = FALSE)",
  "columns <- intersect(c('stage', 'model_token', 'model_label', 'change_token', 'change_group', 'parent_model_token', 'recipe_token', 'flow_species', 'flow_assessment_year'), names(registry))",
  "if (nrow(registry) && length(columns)) {",
  "  print(utils::head(registry[columns], 30))",
  "} else {",
  "  cat('No Kflow registry rows were provided.')",
  "}",
  "```",
  "",
  "```{r kflow-job-summaries, echo=FALSE, message=FALSE, warning=FALSE}",
  "summaries <- read.csv('kflow-inputs/report-input-summaries.csv', stringsAsFactors = FALSE)",
  "if (nrow(summaries)) {",
  "  print(utils::head(summaries, 30))",
  "} else {",
  "  cat('No Kflow summary rows were provided.')",
  "}",
  "```"
)
section_dir <- file.path(render_dir, "sections")
dir.create(section_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(table_section, file.path(section_dir, "Tables.qmd"))

figure_section <- if (length(copied)) {
  c(
    "# Figures",
    "",
    "Kflow-generated figures from upstream plot jobs.",
    "",
    unlist(lapply(seq_along(copied), function(index) {
      c(
        sprintf("## Kflow figure %s", index),
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

if (quarto_available) {
  old <- setwd(render_dir)
  on.exit(setwd(old), add = TRUE)
  status <- system2("quarto", c("render", template_main, "--to", effective_render_format, "--output", report_file))
  if (!identical(status, 0L)) {
    stop("quarto render failed", call. = FALSE)
  }
  invisible(file.copy(file.path(render_dir, report_file), file.path(ctx$out_dir, report_file), overwrite = TRUE))
}

report_summary <- data.frame(
  run_label = kflow_env("RUN_LABEL", ""),
  job_key = kflow_env("JOB_KEY", ""),
  registry_rows = nrow(registries),
  summary_rows = nrow(summaries),
  plot_files = length(copied),
  template_dir = template_dir_setting,
  template_main = template_main,
  requested_render_format = render_format,
  effective_render_format = effective_render_format,
  report_file = report_file,
  stringsAsFactors = FALSE
)
utils::write.csv(report_summary, file.path(ctx$out_dir, "report-summary.csv"), row.names = FALSE)
writeLines(capture.output(print(report_summary)), file.path(ctx$out_dir, "report-summary.txt"))
kflow_write_registry(ctx$out_dir, "report")
kflow_write_summary(ctx$out_dir, "report")
