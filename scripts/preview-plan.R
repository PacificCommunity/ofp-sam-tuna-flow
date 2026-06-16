source("R/workflow.R")
source("R/plan.R")

plan <- build_starter_plan()

preview_plan(plan)
validate_plan(plan)
write_plan_tables(plan)
