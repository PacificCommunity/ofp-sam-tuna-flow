source("../R/job_helpers.R")

ctx <- kflow_stage_start("base")
kflow_write_manifest(ctx$input_dir, file.path(ctx$out_dir, "input-manifest.csv"))

source_dir <- kflow_checkout_source("work/source", log_file = ctx$log_file)
kflow_apply_patch_script(source_dir, ctx$input_dir, ctx$out_dir, "base", log_file = ctx$log_file)
kflow_run_backend(source_dir, ctx$out_dir, "base", log_file = ctx$log_file)
kflow_maybe_collect_paths(source_dir, ctx$out_dir, kflow_env("COLLECT_PATHS", paste(kflow_env("MODEL_DIR", "model"), kflow_env("BASE_DIR", "mfcl/inputs"), sep = ",")))
kflow_write_registry(ctx$out_dir, "base")
kflow_write_summary(ctx$out_dir, "base")
kflow_write_stage_payload(ctx$out_dir, "base", file.path(source_dir, kflow_env("MODEL_DIR", file.path("model", kflow_env("JOB_KEY", "smoke")))))
kflow_compact_outputs(ctx$out_dir)
