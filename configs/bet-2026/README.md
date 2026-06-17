# BET 2026 Flow Edits

Edit these small CSV files for day-to-day work. The R workflow reads them and
builds Kflow jobs from the rows.

- `base-models.csv`: add one row per base model.
- `sensitivity-recipes.csv`: add one row per sensitivity recipe.
- `selftests.csv`: add/check independent validation rows. `RECIPE_FAMILY`
  controls the Kflow task folder and should be one of `selftest`, `jitter`,
  `retro`, `hessian`, or `likprof`.

The job labels are intentionally short. Longer context belongs in
`DESCRIPTION` or `CHANGE_DETAIL`, which is written to job summaries and report
tables.

Report figure placement is controlled in the report repository through
`bet-2026-report/catalog/figures.csv`. Use the filenames written by the plot job
under `report-figures/figures/` and `report-figures/tables/`.
