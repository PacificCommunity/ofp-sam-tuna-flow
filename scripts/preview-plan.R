source("R/workflow.R")
source("R/plan.R")

plan <- build_exploration_plan(
  bases = base_models,
  sensitivities = sensitivity_models,
  diagnostics = diagnostic_recipes(),
  plots = plot_recipes(),
  reports = report_recipes()
)

preview_plan(plan)
write_plan_tables(plan)

