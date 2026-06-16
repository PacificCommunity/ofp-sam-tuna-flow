script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE)), "recipe_marker_helpers.R"))

copy_input_with_recipe_marker(
  recipe = "NoAgeSmoke",
  intended_change = "No-age sensitivity recipe marker. The smoke run keeps source files intact and records where the full no-age input edit should be implemented.",
  full_run_note = "Replace this marker with an explicit age-data edit before using the sensitivity for assessment inference.",
  readme_lines = "Use this script as the place to add the explicit age-data edit for the full no-age sensitivity."
)
