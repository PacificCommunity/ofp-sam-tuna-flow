copy_input_with_recipe_marker <- function(recipe,
                                          intended_change,
                                          full_run_note,
                                          readme_lines = character()) {
  source_dir <- Sys.getenv("KFLOW_PATCH_SOURCE_DIR")
  input_dir <- Sys.getenv("KFLOW_PATCH_INPUT_DIR")
  output_dir <- Sys.getenv("KFLOW_PATCH_OUTPUT_DIR")
  out_dir <- Sys.getenv("KFLOW_PATCH_OUT_DIR", "outputs")

  if (!dir.exists(input_dir)) {
    stop("Patch input directory does not exist: ", input_dir, call. = FALSE)
  }

  unlink(output_dir, recursive = TRUE, force = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(input_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(files)) {
    ok <- file.copy(files, output_dir, overwrite = TRUE, recursive = TRUE, copy.date = TRUE)
    if (any(!ok)) {
      stop("Failed to copy all input files for ", recipe, ".", call. = FALSE)
    }
  }

  rel_path <- function(path) {
    sub(
      paste0("^", normalizePath(source_dir, winslash = "/", mustWork = FALSE), "/?"),
      "",
      normalizePath(path, winslash = "/", mustWork = FALSE)
    )
  }

  metadata <- data.frame(
    recipe = recipe,
    source_input = rel_path(input_dir),
    output_input = rel_path(output_dir),
    intended_change = intended_change,
    full_run_note = full_run_note,
    stringsAsFactors = FALSE
  )

  utils::write.csv(metadata, file.path(output_dir, "sensitivity-metadata.csv"), row.names = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    metadata,
    file.path(out_dir, paste0(gsub("[^A-Za-z0-9]+", "-", tolower(recipe)), "-patch-summary.csv")),
    row.names = FALSE
  )

  readme <- c(
    paste(recipe, "sensitivity marker"),
    "",
    "This smoke recipe is intentionally conservative.",
    "It preserves the MFCL input files so the fast makepar check remains runnable.",
    readme_lines
  )
  writeLines(readme, file.path(output_dir, paste0("README-", recipe, ".txt")))
  invisible(metadata)
}
