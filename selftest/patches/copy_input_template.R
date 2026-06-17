# Template patch script for selftest-specific input edits.
#
# Most selftests should not need this, but it is here for cases where a model
# validation check needs a special input preparation step.

input_dir <- Sys.getenv("KFLOW_PATCH_INPUT_DIR")
output_dir <- Sys.getenv("KFLOW_PATCH_OUTPUT_DIR")

if (!dir.exists(input_dir)) {
  stop("Input directory not found: ", input_dir, call. = FALSE)
}

if (!identical(normalizePath(input_dir, mustWork = TRUE), normalizePath(output_dir, mustWork = FALSE))) {
  unlink(output_dir, recursive = TRUE, force = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(input_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE), output_dir, recursive = TRUE, overwrite = TRUE)
}

writeLines(
  c(
    "Selftest patch template completed.",
    paste("Input:", input_dir),
    paste("Output:", output_dir)
  ),
  file.path(Sys.getenv("KFLOW_PATCH_OUT_DIR", "outputs"), "patch-template-note.txt")
)
