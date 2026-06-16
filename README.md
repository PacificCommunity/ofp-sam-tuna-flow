# BET 2026 Kflow model exploration

Kflow-ready model exploration workflow for the 2026 Bigeye tuna assessment.

This repo is intentionally small and runnable. It includes:

- `mfcl/inputs/2023_4region/`
- `mfcl/exe/mfclo64_2026_02_04_vsn2278`
- Kflow tasks for base, sensitivity, diagnostics, plot, and Quarto report jobs.

The starter flow runs a fast MFCL smoke check, three sensitivity markers
(`NoAgeSmoke`, `FixM`, `FixVB`), jitter-style diagnostics, a depletion figure,
and a Quarto report.

Start here:

- [Model exploration guide](vignettes/model-exploration-guide.md)
- [Workflow table](R/workflow.R)
- [Plan helpers](R/plan.R)

Kflow is a lightweight workflow layer developed by Kyuhan Kim for launching
dependency-aware Docker jobs from R. This repository uses it for the BET
assessment workflow. The broader tooling is still evolving; if there is wider
interest, the general templates and examples can be shared more openly later.
