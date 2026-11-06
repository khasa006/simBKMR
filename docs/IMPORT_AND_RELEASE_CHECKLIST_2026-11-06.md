# Final import and publication checklist (November 6, 2026)

The authors authorized public sharing of **selected eligible original code and datasets** under CC0 1.0 Universal. The existing repo `LICENSE` is already CC0; the draft branch README was corrected to match.

## Local package ready for import

Filename: `simBKMR_CC0_release_candidate_20261106.zip`  
SHA256: `bf1d8c0e4a333d09c501dd43cd2c16dc912e5448ea7551d4957c12126a0a677d`  
Archive size: about 399 KB compressed. The ZIP has been integrity-tested.

The ZIP includes historical R/Rmd/Qmd files for Projects 1 and 2, the six `mleData_20240920` PIP CSV inputs (64,500 PIP records total), a base-R verification script, source-file manifest, and documentation. The ZIP **does not include the current full repository**, which must be retained.

**Transfer limitation:** The connected GitHub text-writing interface cannot ingest the local binary ZIP or its constituent files as a batch. Therefore the original script/data files are **not yet committed**. Copy the extracted `project1_bkmr_sensitivity/`, `project2_dynamic_threshold/`, and `docs/` additions into a checkout of the draft branch. Preserve any existing files on that branch; do not overwrite the new README and verification scripts or delete the existing `R/` and `data/` directories. Inspect overlapping files and merge intentionally. A PR review should show the additions before merge.

## What was verified

- Logistic coefficients and NHANES threshold arithmetic match manuscript reported values to stated precision, using saved PIP outputs.
- Published median and maximum absolute deviations match saved-output calculations.
- 246/246 Supplementary Table 1 rows match saved-output calculations.
- Figures 1–2 numerical patterns were reconstructed from the saved outputs.
- A separately supplied NHANES input gives 2,934 original rows, 949 complete cases, CV 15.18098.

## What was NOT verified

- Re-running the BKMR simulated-data generator and MCMC from scratch.
- The complete training/validation provenance of the six saved PIP files.
- NHANES 25,000-iteration BKMR model/PIP table and Figures 3–4.
- All hard-coded historical paths, package versions, and environment compatibility.
- A GitHub tagged release, Zenodo DOI, and the publisher's Open Science Badge award.

## Essential final release gates

1. Import the candidate code/data files to the draft PR (do not replace existing repository files inadvertently).
2. Run `Rscript project2_dynamic_threshold/verify_results_from_saved_PIP.R` from repository root on a machine with R.
3. Run NHANES input check, and label full MCMC reproduction as pending if not rerun.
4. Review terms for any third-party data and check source scripts for private information.
5. Merge reviewed PR, publish a versioned GitHub release, and archive it through Zenodo. Only then cite stable release URL/DOI in journal badge form.
6. Use explicit caveats: saved-output calculation verification is not a claim of full end-to-end reproducibility or frequentist Type I error control.

## Post-import update — November 6, 2026

The author committed and pushed the selected original code and six saved PIP CSV datasets through GitHub Desktop. GitHub confirms all six CSVs and original analysis scripts are present on the draft branch. Pull request #1 currently reports a clean merge state. Prior statements in this document that the files have not been uploaded or remain to be imported describe the earlier staging stage and are now superseded. **No full original MCMC rerun has been performed.**
