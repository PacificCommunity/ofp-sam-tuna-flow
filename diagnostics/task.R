source("../R/job_helpers.R")

ctx <- kflow_stage_start("diagnostics")
kflow_write_manifest(ctx$input_dir, file.path(ctx$out_dir, "input-manifest.csv"))

source_dir <- kflow_checkout_source("work/source", log_file = ctx$log_file)
kflow_sync_input_artifacts(ctx$input_dir, source_dir, log_file = ctx$log_file)
kflow_copy_seed_files(
  ctx$input_dir,
  source_dir,
  kflow_env("DIAGNOSTIC_SEED_GLOB", ""),
  kflow_env("DIAGNOSTIC_SEED_COPY_TO", ""),
  log_file = ctx$log_file
)
kflow_apply_patch_script(source_dir, ctx$input_dir, ctx$out_dir, "diagnostics", log_file = ctx$log_file)
kflow_run_backend(source_dir, ctx$out_dir, "diagnostics", log_file = ctx$log_file)
kflow_maybe_collect_paths(source_dir, ctx$out_dir, kflow_env("COLLECT_PATHS", paste(kflow_env("MODEL_DIR", "model"), kflow_env("BASE_DIR", "mfcl/inputs"), sep = ",")))
kflow_write_registry(ctx$out_dir, "diagnostics")
kflow_write_summary(ctx$out_dir, "diagnostics")
kflow_write_stage_payload(ctx$out_dir, "diagnostics", file.path(source_dir, kflow_env("MODEL_DIR", file.path("model", kflow_env("JOB_KEY", "diagnostics")))))
kflow_compact_outputs(ctx$out_dir)
