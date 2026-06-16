#!/usr/bin/env Rscript

args <- commandArgs(FALSE)
script_arg <- grep("^--file=", args, value = TRUE)[1]
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg) else "scripts/run-mfcl-input-recipe.R"
repo_root <- normalizePath(file.path(dirname(script_file), ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "mfcl_input_recipes.R"))

mfcl_recipe_run_from_env()
