script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE)), "recipe_marker_helpers.R"))

copy_input_with_recipe_marker(
  recipe = "FixM",
  intended_change = "Fixed natural mortality sensitivity recipe marker. The smoke run records the intended M-fix change while preserving runnable source inputs.",
  full_run_note = "Replace this marker with the explicit MFCL parameter-control edit that fixes natural mortality before using the sensitivity for assessment inference.",
  readme_lines = c(
    "Use this script as the place to add the exact MFCL control-file or parameter-file edit for fixed natural mortality.",
    "Keep the MODEL_TOKEN as FixM, or use a more specific token such as FixM0.35 when the value is known."
  )
)
