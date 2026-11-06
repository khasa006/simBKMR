# Project 2: Dynamic PIP threshold for BKMR

**Manuscript:** *Enhancing Bayesian Kernel Machine Regression: A Dynamic Thresholding Framework to Address Variability and Skewness in the Data*, accepted in *Research in Statistics* on November 6, 2026 (publication DOI pending).

This study builds a response-CV- and sample-size-dependent posterior inclusion probability (PIP) selection cutoff, estimated using a logistic threshold model, then compares its simulation-based empirical false-positive selection frequencies with a fixed PIP cutoff of 0.5.

## Materials identified in the uploaded research archive

Original scripts (not yet added to this branch) include:
- `mleBetaCalculation{150,300,600,1000,2000,2934}_20240920.R`
- `pip_quantile_then_logistic_20241209.R`
- `compareTestSize_20241211.R`
- `analysisResultsReport_20241217.qmd`

Six candidate inputs were located in `mleData_20240920/`: `mleData150_df.csv`, `mleData300_df.csv`, `mleData600_df.csv`, `mleData1000_df.csv`, `mleData2000_df.csv`, and `mleData2934_df.csv`.

**Warning:** Identifying these files is not the same as verifying that they reproduce the accepted paper. In particular, the historical comparison script conditionally uses denominators of 250 or 500 when converting selection counts into frequencies; this requires methodological verification. The logistic coefficients, Figures 1–2, Supplementary Table 1, and NHANES workflow have not been independently reproduced.

The 5% reference is a simulation-calibration target, **not** a guarantee of frequentist Type I error control.

The corrected supplementary column headings are:
- Empirical False-Positive Selection Frequency (Fixed PIP = 0.5)
- Empirical False-Positive Selection Frequency (Dynamic PIP)

Do not deposit restricted NHANES or other non-shareable participant data. Before public archiving, document all required inputs, their provenance, software/package versions, and successful rerun results.

See `../docs/REPRODUCIBILITY_STATUS.md` for next steps.
