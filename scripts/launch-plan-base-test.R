source("R/workflow.R")
source("R/plan.R")

plan <- build_starter_plan()

launch_plan(plan, stages = "base", limit = 1)
