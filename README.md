# Tuna Kflow model exploration

Kflow-ready model exploration workflow for tuna stock assessments.

The repo is generic. It does not store MFCL input files or assessment report
sources. Kflow job config supplies the input bundle, report repository, and
species/year metadata through environment variables such as
`FLOW_SOURCE_REPO`, `FLOW_BASE_INPUT_DIR`, `FLOW_OUTPUTS_REPO`,
`FLOW_CURATION_REPO`, and `FLOW_REPORT_REPO`.

The included BET 2026 starter config points to:

- `FLOW_SOURCE_REPO=PacificCommunity/ofp-sam-bet2026-inputs`
- `FLOW_BASE_INPUT_DIR=mfcl/inputs/2023_4region_1007`
- `FLOW_OUTPUTS_REPO=PacificCommunity/ofp-sam-bet-2026-outputs`
- `FLOW_CURATION_REPO=PacificCommunity/ofp-sam-bet-2026-curation`
- `FLOW_REPORT_REPO=PacificCommunity/ofp-sam-bet-2026-report`
- `FLOW_REPORT_PATH=bet-2026-report`
- `FLOW_MFCL_PROGRAM=/home/mfcl/mfclo64` from the Docker image
- `FLOW_DOCKER_IMAGE=ghcr.io/pacificcommunity/tuna-flow:v1.6`

The starter flow runs one base model, several explicit sensitivity recipes
(`FixM`, `FixVB`, `Sel4`, `IndexCvHalf`), independent model validation tasks
(`selftest`, `jitter`, `retro`, `hessian`, `likprof`), report-ready figures,
curation, and a Quarto report. The BET 2026 report workflow is intentionally
split into four assessment-specific repositories:

```text
upstream model/output jobs -> outputs -> curation -> report
```

`outputs` makes the full mfclshiny figure/table bundle. `curation` chooses the
figures and tables, sets order/section/captions, and writes report-ready QMD.
`report` renders the assessment report and records the upstream outputs job id,
curation job id, and curation/report repository commits in provenance artifacts.

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
