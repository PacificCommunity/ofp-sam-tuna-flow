# Template patch script for a sensitivity input variant.
#
# Copy this file, edit the input changes, then point PATCH_SCRIPT at the copy
# from one row in R/workflow.R or a generated plan table.

input_dir <- Sys.getenv("KFLOW_PATCH_INPUT_DIR")
output_dir <- Sys.getenv("KFLOW_PATCH_OUTPUT_DIR")

if (!dir.exists(input_dir)) {
  stop("Input directory not found: ", input_dir, call. = FALSE)
}

unlink(output_dir, recursive = TRUE, force = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(list.files(input_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE), output_dir, recursive = TRUE, overwrite = TRUE)

# Example edit location:
# frq_file <- file.path(output_dir, "bet.frq")
# lines <- readLines(frq_file, warn = FALSE)
# lines <- gsub("old token", "new token", lines, fixed = TRUE)
# writeLines(lines, frq_file)

writeLines(
  c(
    "Sensitivity patch template completed.",
    paste("Input:", input_dir),
    paste("Output:", output_dir)
  ),
  file.path(Sys.getenv("KFLOW_PATCH_OUT_DIR", "outputs"), "patch-template-note.txt")
)

