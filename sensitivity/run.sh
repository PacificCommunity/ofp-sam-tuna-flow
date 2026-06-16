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

run_runtime_package_update() {
  local update_status
  if [[ -z "${R_LIBS_USER:-}" ]]; then
    export R_LIBS_USER="${KFLOW_RUNTIME_LIBRARY:-${PWD}/.R-library}"
  fi
  mkdir -p "${R_LIBS_USER}"
  if [[ ! -x /usr/local/bin/30-update-kflow-runtime-packages ]]; then
    return 0
  fi
  if bash /usr/local/bin/30-update-kflow-runtime-packages; then
    return 0
  fi
  update_status=$?
  case "${KFLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES:-false}" in
    1|true|TRUE|yes|YES|on|ON) exit "$update_status" ;;
  esac
  if [[ "$update_status" -eq 42 || "$update_status" -eq 43 ]]; then
    exit "$update_status"
  fi
  echo "[kflow-runtime-update] Runtime package update failed; continuing with bundled packages." >&2
}

if [[ -n "${KFLOW_JOB_CONFIG_FILE:-}" ]]; then
  load_env "$KFLOW_JOB_CONFIG_FILE"
elif [[ -f job.env ]]; then
  load_env job.env
elif [[ -f configs/default.env ]]; then
  load_env configs/default.env
fi

mkdir -p "${OUTPUT_DIR:-outputs}" "${INPUT_DIR:-inputs}"
run_runtime_package_update
Rscript task.R
