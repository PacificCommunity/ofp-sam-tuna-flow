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

load_default_env() {
  local file="$1"
  local line key
  [[ -f "$file" ]] || return 0
  while IFS= read -r line || [[ -n "$line" ]]; do
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    [[ -z "$line" || "$line" == \#* || "$line" != *=* ]] && continue
    key="${line%%=*}"
    key="${key%"${key##*[![:space:]]}"}"
    [[ "$key" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]] || continue
    [[ -z "${!key+x}" ]] || continue
    set -a
    source <(printf '%s\n' "$line")
    set +a
  done < "$file"
}

drop_runtime_tokens() {
  unset GIT_PAT GITHUB_PAT GH_TOKEN KFLOW_GITHUB_TOKEN KFLOW_PERSONAL_TOKEN
}

prepare_runtime_package_update() {
  if [[ -n "${TUNA_FLOW_RUNTIME_UPDATE:-}" ]]; then
    export KFLOW_RUNTIME_UPDATE="${TUNA_FLOW_RUNTIME_UPDATE}"
  fi
  if [[ -n "${TUNA_FLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES:-}" ]]; then
    export KFLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES="${TUNA_FLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES}"
  fi
}

runtime_packages_disabled() {
  case "${KFLOW_RUNTIME_PACKAGES:-}" in
    ""|0|false|FALSE|no|NO|off|OFF|none|NONE|skip|SKIP) return 0 ;;
    *) return 1 ;;
  esac
}

ensure_runtime_library() {
  local preferred="${R_LIBS_USER:-${KFLOW_RUNTIME_LIBRARY:-}}"
  local fallback="${PWD}/.R-library"
  if [[ -z "$preferred" ]]; then
    preferred="$fallback"
  fi
  if mkdir -p "$preferred" 2>/dev/null && [[ -w "$preferred" ]]; then
    export R_LIBS_USER="$preferred"
  else
    export R_LIBS_USER="$fallback"
    mkdir -p "$R_LIBS_USER"
  fi
  export KFLOW_RUNTIME_LIBRARY="$R_LIBS_USER"
  if [[ -z "${KFLOW_RUNTIME_STATE_DIR:-}" ]]; then
    export KFLOW_RUNTIME_STATE_DIR="${PWD}/.kflow-runtime-cache"
  fi
  mkdir -p "$KFLOW_RUNTIME_STATE_DIR" 2>/dev/null || true
}

install_runtime_cran_dependencies() {
  if runtime_packages_disabled; then
    return 0
  fi
  case "${KFLOW_RUNTIME_PACKAGES:-}" in
    *mfclrtmb=*)
      Rscript -e 'lib <- Sys.getenv("R_LIBS_USER"); if (!nzchar(lib)) stop("R_LIBS_USER is required for runtime installs", call. = FALSE); dir.create(lib, recursive = TRUE, showWarnings = FALSE); .libPaths(unique(c(lib, .libPaths()))); options(repos = c(CRAN = "https://cloud.r-project.org")); pkgs <- c("TMB", "RTMB"); missing <- setdiff(pkgs, rownames(utils::installed.packages())); if (length(missing)) utils::install.packages(missing, lib = lib, dependencies = TRUE, repos = getOption("repos")); missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(missing)) { message("[kflow-runtime-update] Required CRAN package(s) unavailable after install: ", paste(missing, collapse = ", ")); quit(save = "no", status = 45) }' ;;
  esac
}

run_runtime_package_update() {
  local update_status
  if runtime_packages_disabled; then
    return 0
  fi
  ensure_runtime_library
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

verify_runtime_packages() {
  case "${KFLOW_RUNTIME_REQUIRE_PRIVATE_PACKAGES:-false}" in
    1|true|TRUE|yes|YES|on|ON) ;;
    *) return 0 ;;
  esac
  if runtime_packages_disabled; then
    return 0
  fi
  Rscript -e 'lib <- Sys.getenv("R_LIBS_USER"); if (nzchar(lib)) .libPaths(unique(c(lib, .libPaths()))); spec <- Sys.getenv("KFLOW_RUNTIME_PACKAGES"); parts <- trimws(strsplit(spec, ",", fixed = TRUE)[[1]]); parts <- parts[nzchar(parts) & grepl("=", parts, fixed = TRUE)]; pkgs <- sub("=.*$", "", parts); missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(missing)) { message("[kflow-runtime-update] Required runtime package(s) unavailable after update: ", paste(missing, collapse = ", ")); quit(save = "no", status = 44) }'
}

if [[ -n "${KFLOW_JOB_CONFIG_FILE:-}" ]]; then
  load_env "$KFLOW_JOB_CONFIG_FILE"
elif [[ -f job.env ]]; then
  load_env job.env
elif [[ -f configs/default.env ]]; then
  load_default_env configs/default.env
fi

mkdir -p "${OUTPUT_DIR:-outputs}" "${INPUT_DIR:-inputs}"
prepare_runtime_package_update
ensure_runtime_library
install_runtime_cran_dependencies
run_runtime_package_update
verify_runtime_packages
drop_runtime_tokens
Rscript task.R
