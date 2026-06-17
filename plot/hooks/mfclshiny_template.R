# Template hook for non-interactive mfclshiny plotting.
#
# Kflow sets:
# - KFLOW_INPUT_DIR: upstream outputs
# - KFLOW_OUT_DIR: plot output directory
# - PLOT_TITLE: plot title
#
input_dir <- Sys.getenv("KFLOW_INPUT_DIR", Sys.getenv("INPUT_DIR", "inputs"))
out_dir <- Sys.getenv("KFLOW_OUT_DIR", Sys.getenv("OUTPUT_DIR", "outputs"))
plot_title <- Sys.getenv("PLOT_TITLE", "Tuna model report figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

report_figure_dir <- file.path(out_dir, "report-figures")
dir.create(report_figure_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("mfclshiny", quietly = TRUE)) {
  writeLines("mfclshiny_not_available", file.path(out_dir, "mfclshiny-status.txt"))
} else {
  args <- list(
    model_dir = input_dir,
    output_dir = report_figure_dir,
    title = plot_title,
    figure_basename = "key-quantities-smoke",
    formats = c("png", "pdf", "svg"),
    width = 12,
    height = 8,
    dpi = 300,
    overwrite = TRUE,
    species_label = Sys.getenv("FLOW_SPECIES_LABEL", "selected stock"),
    assessment_year = Sys.getenv("FLOW_ASSESSMENT_YEAR", "")
  )
  if ("plot_style" %in% names(formals(mfclshiny::build_report_figures))) {
    args$plot_style <- "shiny_stock"
  }
  status <- tryCatch({
    result <- do.call(mfclshiny::build_report_figures, args)
    paste("report_figures_ok:", nrow(result$figures), "figures")
  }, error = function(e) {
    paste("report_figures_failed:", conditionMessage(e))
  })
  writeLines(status, file.path(out_dir, "mfclshiny-status.txt"))
}
