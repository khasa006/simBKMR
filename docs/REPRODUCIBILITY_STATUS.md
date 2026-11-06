# Reproducibility audit and integration checklist

**Date:** November 6, 2026  
**Scope:** Two related BKMR studies; existing public repo `khasa006/simBKMR`.

## GitHub integration status

- [x] Existing default branch and top-level files inspected.
- [x] A separate organization branch created, preserving the original `main` branch.
- [x] Project-specific introductory documentation drafted.
- [ ] Original historical scripts and permitted datasets integrated into the branch.
- [ ] Full script dependencies and absolute file paths mapped.
- [ ] Expected R package versions recorded.
- [ ] Reproduction checked against published model coefficients and tables.
- [ ] Figure 1 and Figure 2 regenerated and compared.
- [ ] Supplementary Table 1 headings verified and upstream calculations reconciled.
- [ ] NHANES data provenance and preprocessing documented.
- [ ] Licensing and coauthor approval for public sharing checked.
- [ ] Final release archived with Zenodo DOI.

## Confirmed candidate inputs

Six `mleData_20240920` CSV files were found for n = 150, 300, 600, 1000, 2000, and 2934. Alternative `mleData_20241126` files exist and differ in size. They must not be treated as interchangeable without comparison against manuscript values.

## Numerical verification issues

The historical `compareTestSize_20241211.R` script uses a conditional denominator of 250 or 500 when calculating a proportion. The study's simulation unit, null-predictor count, number of replicates, and the appropriate empirical frequency denominator must be verified before interpreting or changing this calculation.

Reported manuscript logistic estimates requiring independent reproduction: C = 1.30460, beta1 = 0.59867, beta2 = 0.43565. Preserve all original scripts and report discrepancies transparently.

**This branch currently documents the proposed organization only; it is not a complete or verified reproducibility release.**
