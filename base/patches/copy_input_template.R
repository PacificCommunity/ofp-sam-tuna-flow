# Template patch script for a base-model input variant.
#
# This script copies PATCH_INPUT_DIR to PATCH_OUTPUT_DIR. Add edits after the
# copy step to create a new base input variant.

input_dir <- Sys.getenv("KFLOW_PATCH_INPUT_DIR")
output_dir <- Sys.getenv("KFLOW_PATCH_OUTPUT_DIR")

if (!dir.exists(input_dir)) {
  stop("Input directory not found: ", input_dir, call. = FALSE)
}

unlink(output_dir, recursive = TRUE, force = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(list.files(input_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE), output_dir, recursive = TRUE, overwrite = TRUE)

# Example edit location:
# ini_file <- file.path(output_dir, "bet.ini")
# lines <- readLines(ini_file, warn = FALSE)
# lines <- sub("old value", "new value", lines, fixed = TRUE)
# writeLines(lines, ini_file)

writeLines(
  c(
    "Base patch template completed.",
    paste("Input:", input_dir),
    paste("Output:", output_dir)
  ),
  file.path(Sys.getenv("KFLOW_PATCH_OUT_DIR", "outputs"), "patch-template-note.txt")
)

