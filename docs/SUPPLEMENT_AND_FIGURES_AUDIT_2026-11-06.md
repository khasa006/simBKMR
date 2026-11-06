# Supplementary Table 1 and Figures 1–2 audit

**Audit date:** 2026-11-06. Source: uploaded `Paper_2nd/supplementary_table_20261104.docx` from research archive, plus six `mleData_20240920` PIP-output CSVs.

## Supplementary Table 1

- 246 data rows = 41 CV conditions × 6 sample sizes.
- All 246 fixed-PIP (0.5) selection frequencies match independently recomputed values from the saved CSV records within numerical precision (maximum absolute difference < 1.2e-16).
- All 246 dynamic-PIP selection frequencies also match (maximum absolute difference < 1.2e-16).
- CV display values differ by no more than 3.4e-9 from the saved CSV values because of decimal formatting.
- The requested replacement headings are present as **tracked insertions** in the archived 2026-11-04 supplement. Accepting tracked changes retains them and removes the two old headings.
- A clean, reformatted DOCX was prepared locally, with frequencies displayed to three decimal places. This is **display formatting only**: all original frequencies are exact multiples of .004 and no selection value changes.
- Do not claim that a table reconstructed from stored PIP outputs independently verifies the original BKMR simulation design.

The final headings must read:
1. `Empirical False-Positive Selection Frequency (Fixed PIP = 0.5)`
2. `Empirical False-Positive Selection Frequency (Dynamic PIP)`

## Figure 1

The 246 PIP 95th-percentile values and the fitted threshold curves were replotted from the saved PIP outputs and reproduce the qualitative shape and sample-size ordering of manuscript Figure 1. Deterministic scatter rather than the original `geom_jitter()` positions means the recreated figure is not pixel-identical.

## Figure 2

The same group-level fixed and dynamic empirical selection frequencies were replotted, including the 0.05 simulation reference. Its pattern is consistent with manuscript Figure 2: large upward departures for the fixed threshold at high CV and generally smaller departures for the dynamic cutoff. Plotting appearance and original jitter/point styling are not yet checked for exact image reproduction.

## Remaining checks

- Full original BKMR MCMC and simulation/data-generation rerun not completed.
- Whether the 20240920 files came exclusively from the claimed independent validation split has not been established.
- NHANES BKMR fitting, covariate/data-cleaning provenance, and figures 3–4 not independently reproduced.
- Final public archiving still requires review of licenses, data permissions, and dependencies.

This audit documents checks from **saved output data**, not a complete end-to-end reproduction.
