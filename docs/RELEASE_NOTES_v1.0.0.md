# simBKMR v1.0.0 — First archived research-materials release

This is the first tagged public research-materials snapshot for two related Bayesian kernel machine regression (BKMR) studies.

## Included

- **Project 1:** Historical scripts and documentation for BKMR sensitivity to data distribution.
- **Project 2:** Original R scripts for the dynamic posterior inclusion probability (PIP) threshold study, six saved `mleData_20240920` PIP CSV datasets spanning n = 150, 300, 600, 1000, 2000, and 2934, and numerical-check scripts.
- Reproducibility status reports, supplement audit, NHANES input audit, and CC0-sharing information.

## Verification scope

Saved-output checks reproduce logistic coefficients C = 1.30460, beta1 = 0.59867, beta2 = 0.43565, the published deviation summaries, all 246 Supplementary Table 1 rows, and NHANES threshold arithmetic 0.646 for CV = 15.18 and n = 949. Manuscript Figures 1 and 2 were reconstructed numerically from saved PIP outputs.

**Limitations:** The full historical BKMR simulation-generation and MCMC procedures, the 25,000-iteration NHANES BKMR fit, Figures 3–4, and complete independent validation provenance have not been rerun or confirmed end to end. Several original scripts preserve their historical absolute paths. Readers should consult the documentation before attempting a rerun. The empirical 5% reference is a simulation-calibration target, not frequentist Type I error control.

## License and data

Original author-owned research materials are released under CC0 1.0 Universal. Third-party source data and dependencies retain their own rights and terms.

## Related research

- Hasan and colleagues, *The Sensitivity of Bayesian Kernel Machine Regression (BKMR) to Data Distribution: A Comprehensive Simulation Analysis*.
- Hasan and colleagues, *Enhancing Bayesian Kernel Machine Regression: A Dynamic Thresholding Framework to Address Variability and Skewness in the Data*, accepted by *Research in Statistics*, November 2026. Journal DOI pending.

When a Zenodo DOI is assigned, please cite the archived release. This release is a reproducibility resource, not a claim that every original simulation was recomputed.
