source("../R/job_helpers.R")

ctx <- kflow_stage_start("plot")
kflow_write_manifest(ctx$input_dir, file.path(ctx$out_dir, "input-manifest.csv"))

registry_files <- list.files(ctx$input_dir, pattern = "^model-registry[.]csv$", recursive = TRUE, full.names = TRUE)
summary_files <- list.files(ctx$input_dir, pattern = "^kflow-job-summary[.]csv$", recursive = TRUE, full.names = TRUE)
manifest_files <- list.files(ctx$input_dir, pattern = "manifest[.]csv$", recursive = TRUE, full.names = TRUE)

registries <- kflow_read_csv_union(registry_files)
summaries <- kflow_read_csv_union(summary_files)
manifests <- kflow_read_csv_union(manifest_files)

utils::write.csv(registries, file.path(ctx$out_dir, "plot-input-registry.csv"), row.names = FALSE)
utils::write.csv(summaries, file.path(ctx$out_dir, "plot-input-summaries.csv"), row.names = FALSE)
utils::write.csv(manifests, file.path(ctx$out_dir, "plot-input-manifests.csv"), row.names = FALSE)

plot_backend <- kflow_env("PLOT_BACKEND", "manifest")
if (identical(plot_backend, "mfclshiny") && nzchar(kflow_env("MFCLSHINY_SCRIPT", ""))) {
  Sys.setenv(
    KFLOW_INPUT_DIR = normalizePath(ctx$input_dir, winslash = "/", mustWork = FALSE),
    KFLOW_OUT_DIR = normalizePath(ctx$out_dir, winslash = "/", mustWork = FALSE),
    PLOT_TITLE = kflow_env("PLOT_TITLE", "BET model exploration")
  )
  kflow_run_optional_script("MFCLSHINY_SCRIPT", getwd(), ctx$out_dir, "plot", log_file = ctx$log_file)
} else {
  counts <- if (nrow(registries) && "change_group" %in% names(registries)) {
    as.data.frame(table(change_group = registries$change_group), stringsAsFactors = FALSE)
  } else {
    data.frame(change_group = "input_files", Freq = nrow(manifests), stringsAsFactors = FALSE)
  }
  utils::write.csv(counts, file.path(ctx$out_dir, "plot-counts.csv"), row.names = FALSE)

  svg(file.path(ctx$out_dir, "model-exploration-overview.svg"), width = 9, height = 5)
  old_mar <- par(mar = c(6, 5, 3, 1))
  on.exit(par(old_mar), add = TRUE)
  values <- counts$Freq
  names(values) <- counts$change_group
  if (!length(values) || all(values == 0)) {
    plot.new()
    title(kflow_env("PLOT_TITLE", "BET model exploration"))
    text(0.5, 0.5, "No input registry rows found")
  } else {
    barplot(
      values,
      las = 2,
      col = "#2f83b7",
      border = "#195577",
      ylab = "Input jobs",
      main = kflow_env("PLOT_TITLE", "BET model exploration")
    )
    grid(nx = NA, ny = NULL, col = "#d7e3ea")
  }
  dev.off()
}

plot_summary <- data.frame(
  run_label = kflow_env("RUN_LABEL", ""),
  job_key = kflow_env("JOB_KEY", ""),
  plot_backend = plot_backend,
  input_registry_rows = nrow(registries),
  input_summary_rows = nrow(summaries),
  input_manifest_rows = nrow(manifests),
  plot_file = "model-exploration-overview.svg",
  stringsAsFactors = FALSE
)
utils::write.csv(plot_summary, file.path(ctx$out_dir, "plot-summary.csv"), row.names = FALSE)
writeLines(capture.output(print(plot_summary)), file.path(ctx$out_dir, "plot-summary.txt"))
kflow_write_registry(ctx$out_dir, "plot")
kflow_write_summary(ctx$out_dir, "plot")

