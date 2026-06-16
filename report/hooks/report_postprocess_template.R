# Optional post-processing hook for report outputs.
#
# Add custom archive, upload, or notification logic here if needed later.

out_dir <- Sys.getenv("KFLOW_OUT_DIR", Sys.getenv("OUTPUT_DIR", "outputs"))
writeLines("Report postprocess hook template.", file.path(out_dir, "report-postprocess-template.txt"))

