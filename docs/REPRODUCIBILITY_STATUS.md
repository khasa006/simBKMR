# BKMR reproducibility status

**Updated:** November 6, 2026. **Branch:** `organize-bkmr-projects-2026-11`, draft pull request #1. This branch has not been merged.

| Component | Status | Evidence |
|---|---|---|
| Existing Project 1 GitHub source | Present on main | `R/`, `data/`; not reorganized or rerun |
| Project 2 directory and documentation | Added on draft branch | `project2_dynamic_threshold/README.md` |
| Threshold logistic coefficients | Independently reproduced from saved PIP outputs | `docs/NUMERICAL_VERIFICATION_2026-11-06.md` |
| Fixed/dynamic MAD and maximum deviations | Independently reproduced from saved PIP outputs | `docs/NUMERICAL_VERIFICATION_2026-11-06.md` |
| Supplementary Table 1 values | All 246 rows matched saved PIP outputs | `docs/SUPPLEMENT_AND_FIGURES_AUDIT_2026-11-06.md` |
| Figures 1–2 | Numerically reconstructed from saved outputs, not pixel-identical | `project2_dynamic_threshold/reconstruct_figures_supplement.R` |
| NHANES sample size and response CV | Recomputed from uploaded historical CSV: n=949; CV=15.18097698 | `docs/NHANES_PROVENANCE_AUDIT_2026-11-06.md` |
| NHANES PIP threshold arithmetic | 0.646321..., consistent with 0.646 | `docs/NUMERICAL_VERIFICATION_2026-11-06.md` |
| RStudio NHANES data-input checker | Added; local execution on GitHub CSV still pending | `project2_dynamic_threshold/check_nhanes_inputs.R` |
| Original Project 2 R scripts and six CSV inputs | Committed on draft branch | `project2_dynamic_threshold/R_original/` and `project2_dynamic_threshold/data/mleData_20240920/` |
| NHANES BKMR MCMC, Table 3, Figures 3–4 | Not independently reproduced | Original candidate urinary-metal script identified: `realDataAnalysis_20240110.R` |
| End-to-end data generation, simulation rerun and training/validation split | Not verified | Requires computational workflow and dependency audit |
| Copyright/data-sharing authorization | Not verified | Review before public release or Zenodo deposition |

## Issues to resolve

1. Selected original scripts and six permitted threshold-input CSVs are now on the draft branch. Review file provenance and redistribution rights before merging.
2. Review the `realDataAnalysis_20240110.R` urinary-metal script against the manuscript's NHANES methods. Do not confuse it with `R/bloodMetalDataAnalysis_20250123.R`, which analyzes blood metals.
3. Run the RStudio NHANES input checker on the existing GitHub CSV and then decide whether full BKMR MCMC rerun is feasible.
4. Reconcile the historical script's conditional 250/500 selection-frequency denominator with the group-size definition. For the saved six PIP CSVs, it happens to match group sizes, so the published summary metrics are unchanged.
5. Reconcile licensing: the existing `LICENSE` contains **CC0 1.0**, while the top-level README says MIT. Do not change either without reviewing intended permissions.
6. Do not represent the empirical 5% reference as formal frequentist Type I error control.

**Important:** Output-based numerical verification is not equivalent to end-to-end reproducibility. The draft PR is not a final Open Materials or Open Data deposit.
