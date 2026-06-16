script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE)), "recipe_marker_helpers.R"))

copy_input_with_recipe_marker(
  recipe = "FixVB",
  intended_change = "Fixed von Bertalanffy growth sensitivity recipe marker. The smoke run records the intended growth-fix change while preserving runnable source inputs.",
  full_run_note = "Replace this marker with the explicit MFCL growth-parameter edit before using the sensitivity for assessment inference.",
  readme_lines = c(
    "Use this script as the place to add the exact MFCL control-file or parameter-file edit for fixed von Bertalanffy growth.",
    "Keep the MODEL_TOKEN as FixVB, or use a more specific token such as FixVB_LinfK when the setting is known."
  )
)
