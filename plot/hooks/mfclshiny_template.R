# Template hook for non-interactive mfclshiny plotting.
#
# Kflow sets:
# - KFLOW_INPUT_DIR: upstream outputs
# - KFLOW_OUT_DIR: plot output directory
# - PLOT_TITLE: plot title
#
# Replace the body with the eventual mfclshiny batch plotting call.

input_dir <- Sys.getenv("KFLOW_INPUT_DIR", Sys.getenv("INPUT_DIR", "inputs"))
out_dir <- Sys.getenv("KFLOW_OUT_DIR", Sys.getenv("OUTPUT_DIR", "outputs"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

writeLines(
  c(
    "mfclshiny hook template",
    paste("Input:", input_dir),
    paste("Output:", out_dir),
    "Replace this template with the mfclshiny batch plot call."
  ),
  file.path(out_dir, "mfclshiny-hook-template.txt")
)

