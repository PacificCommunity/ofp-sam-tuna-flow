source("R/workflow.R")
source("R/plan.R")

plan <- build_starter_plan()

preview_plan(plan)
launch_plan(plan, stages = c("sensitivity", "selftest", "jitter", "retro", "hessian", "likprof"), batch_size = 50)
