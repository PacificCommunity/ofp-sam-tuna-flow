# Tuna Kflow model exploration

Kflow-ready model exploration workflow for tuna stock assessments.

The repo is generic. Set `FLOW_SPECIES`, `FLOW_ASSESSMENT_YEAR`, an input-bundle
repository, and recipe tables to produce species-specific task names, model
labels, plots, and reports. The starter preset is a BET 2026 smoke workflow
using:

- `PacificCommunity/ofp-sam-bet2026-inputs`
- `mfcl/inputs/2023_4region_1007`
- `/home/mfcl/mfclo64` from the Docker image
- `ghcr.io/pacificcommunity/tuna-flow:latest`

The starter flow runs one base model, several explicit sensitivity recipes
(`FixM`, `FixVB`, `Sel4`, `IndexCvHalf`), jitter-style diagnostics, a key
derived quantities figure, and a Quarto report. The report task is registered
separately as
`bet-2026-report` and renders from
`PacificCommunity/ofp-sam-2026-BET-report`, so it can be rerun on existing
figures without relaunching model or plot jobs.

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
