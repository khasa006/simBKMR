# Independent numerical audit of threshold analysis

Date: November 6, 2026. This is a numerical reanalysis of uploaded CSV outputs, **not** a full rerun of original BKMR MCMC.

Inputs: six `mleData_20240920/mleData{n}_df.csv` files for n=150,300,600,1000,2000,2934, 64,500 PIP records in 246 CV/sample-size groups. Each sample-size dataset has 39 CV groups containing 250 entries and two groups containing 500.

Using the 95th percentile of PIP for each CV/sample-size combination and fitting the nonlinear curve from `compareTestSize_20241211.R` independently with SciPy gives:

| Coefficient | Independently fitted | Reported |
|---|---:|---:|
| C | 1.304602101 | 1.30460 |
| beta1 | 0.598669409 | 0.59867 |
| beta2 | 0.435651200 | 0.43565 |

All agree to five decimal places.

The formula at CV=15.18, n=949 predicts 0.646321317, which rounds to the published NHANES cutoff of 0.646. This verifies the **calculation of the cutoff**, not the NHANES model fit or the suitability of the outcome CV.

| Comparison metric | Recomputed | Manuscript |
|---|---:|---:|
| Fixed cutoff MAD from 0.05 | 0.046 | 0.046 |
| Dynamic cutoff MAD from 0.05 | 0.026 | 0.026 |
| Fixed maximum absolute deviation | 0.398 | 0.398 |
| Dynamic maximum absolute deviation | 0.074 | 0.074 |

The historical script conditionally divides counts by 250 or 500. That is not a sound general denominator rule: it should use the actual group count. In these **specific** CSVs, the conditional denominator happens to match actual group size in all 246 groups, for both cutoffs. Therefore no numerical discrepancy was observed in these summary metrics. Do not silently alter the historical source script.

**Unverified:** rerunning BKMR sampling, validating original null-predictor labels, training/validation split, Figure 1 and Figure 2 image reproduction, Supplementary Table 1 line-by-line, NHANES fitting, and alternative dated datasets. Empirical false-positive selection frequencies are simulation-calibration metrics, not guarantees of frequentist Type I error control.
