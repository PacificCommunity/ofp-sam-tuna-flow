# Tuna Kflow model exploration

Kflow-ready model exploration workflow for tuna stock assessments.

The repo is generic. It does not store MFCL input files or assessment report
sources. Kflow job config supplies the input bundle, report repository, and
species/year metadata through environment variables such as
`FLOW_SOURCE_REPO`, `FLOW_BASE_INPUT_DIR`, `FLOW_REPORT_REPO`, and
`FLOW_REPORT_PATH`.

The included BET 2026 starter config points to:

- `FLOW_SOURCE_REPO=PacificCommunity/ofp-sam-bet2026-inputs`
- `FLOW_BASE_INPUT_DIR=mfcl/inputs/2023_4region_1007`
- `FLOW_REPORT_REPO=PacificCommunity/ofp-sam-bet2026-report`
- `FLOW_REPORT_PATH=bet-2026-report`
- `FLOW_MFCL_PROGRAM=/home/mfcl/mfclo64` from the Docker image
- `FLOW_DOCKER_IMAGE=ghcr.io/pacificcommunity/tuna-flow:v1.5`

The starter flow runs one base model, several explicit sensitivity recipes
(`FixM`, `FixVB`, `Sel4`, `IndexCvHalf`), independent model validation tasks
(`selftest`, `jitter`, `retro`, `hessian`, `likprof`), report-ready figures,
and a Quarto report. The generic report template
lives in `PacificCommunity/ofp-sam-tuna-report`; the BET 2026 report draft
lives in `PacificCommunity/ofp-sam-bet2026-report`.

Start here:

- [Model exploration guide](vignettes/model-exploration-guide.md)
- [Workflow table](R/workflow.R)
- [Plan helpers](R/plan.R)

Kflow is a lightweight workflow layer developed by Kyuhan Kim for launching
dependency-aware Docker jobs from R. It keeps large model explorations organized
by sending jobs to remote compute, wiring outputs into downstream steps, and
leaving an auditable trail of what changed. The broader tooling is still
evolving; if there is wider interest, the general templates and examples can be
shared more openly later.
