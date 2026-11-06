# Project 2 — Dynamic PIP threshold for BKMR

**Status: partial reproduction materials; not yet numerically verified.**

Corresponding accepted manuscript: *Enhancing Bayesian Kernel Machine Regression: A Dynamic Thresholding Framework to Address Variability and Skewness in the Data*, *Research in Statistics* (accepted November 6, 2026; bibliographic details/DOI pending).

## Included files

`R_original/`: date-stamped original analysis scripts preserved byte-for-byte. Main entry points appear to be:

1. `mleBetaCalculation{150,300,600,1000,2000,2934}_20240920.R`: sample-size-specific processing/estimation scripts. Input provenance not verified.
2. `pip_quantile_then_logistic_20241209.R`: exploratory logistic threshold fitting (not identical to every subsequent formulation).
3. `compareTestSize_20241211.R`: logistic threshold estimation and comparison.
4. `analysisResultsReport_20241217.qmd`: a Quarto report with potentially additional dependencies.

`data/mleData_20240920/`: six explicitly referenced original CSV inputs.

## Reproduction status / caution

- The scripts have **not** been successfully run in an independent R environment and are NOT guaranteed to reproduce the published numbers.
- `compareTestSize_20241211.R` contains a conditional count denominator (250 or 500), which must be reconciled with the underlying simulation design and formula for empirical false-positive selection frequency. Do not silently replace calculations.
- Training/validation independence, random seeds, packages, Figure 1/Figure 2, Supplementary Table 1, and NHANES workflows remain to be checked.
- Do not publish a misleading claim of formally controlled Type I error; the 5% level is a *simulation-calibration reference*.
- Original filenames retain historical terminology like `testSize`, but public-facing labels in Supplementary Table 1 must be **Empirical False-Positive Selection Frequency (Fixed PIP = 0.5)** and **Empirical False-Positive Selection Frequency (Dynamic PIP)**.

## Simple inspection in RStudio

Open the repository and run `source("project2_dynamic_threshold/verify_inputs.R")` from its root. This checks input structure and prints counts; it does not estimate parameters or validate results.
