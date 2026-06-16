#!/usr/bin/env bash
set -euo pipefail

load_env() {
  local file="$1"
  if [[ -f "$file" ]]; then
    set -a
    source "$file"
    set +a
  fi
}

if [[ -n "${KFLOW_JOB_CONFIG_FILE:-}" ]]; then
  load_env "$KFLOW_JOB_CONFIG_FILE"
elif [[ -f job.env ]]; then
  load_env job.env
elif [[ -f configs/default.env ]]; then
  load_env configs/default.env
fi

mkdir -p "${OUTPUT_DIR:-outputs}" "${INPUT_DIR:-inputs}"
Rscript task.R

