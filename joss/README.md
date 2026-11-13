# JOSS preparation workspace (draft)

This directory holds **preparatory materials only** for a possible JOSS submission involving the CRAN package [simBKMRdata](https://github.com/khasa006/simBKMRdata) and research analyses archived here in [simBKMR](https://github.com/khasa006/simBKMR).

## Preservation policy

- The existing analysis scripts, data, research README files, published release v1.0.0, and Zenodo archive remain unchanged.
- The CRAN package repository `simBKMRdata` will not be modified.
- These files are on a **separate draft branch** and are not a final submission.
- Do not treat source-code proximity as proof that the current package version was imported by the manuscript analyses.

## Submission repository decision (unresolved)

Current [JOSS policy](https://joss.readthedocs.io/en/latest/submitting.html) requires the JOSS paper to be hosted in a Git repository **together with the software under review**; it permits a temporary branch. The manuscript in this directory primarily describes the R **package**, whose source lives in a different repository. Therefore, this draft is useful for preparation but **not yet a policy-compliant submission location** for that package.

Two defensible options:

1. Submit `simBKMRdata` as the software: retain its `main` branch untouched and, only if authorized, place the reviewed JOSS paper on a short-lived branch alongside the package code. No merge or CRAN code change is required.
2. Submit `simBKMR` as a distinct reusable software project: first establish that its software is independently reusable, installable or reproducibly executable, tested, and maintained (not only manuscript-specific scripts). Then revise the JOSS paper to describe that software accurately.

Do not claim the second path satisfies JOSS simply because it contains the analyses.

## Files

- `paper.md`: draft JOSS manuscript about `simBKMRdata` (not yet final).
- `paper.bib`: bibliographic draft requiring author and metadata review.
- `SUBMISSION_CHECKLIST.md`: steps and unresolved author decisions.

## Research resources

- [Project 1: BKMR sensitivity](../project1_bkmr_sensitivity/README.md)
- [Project 2: Dynamic PIP threshold](../project2_dynamic_threshold/README.md)
- [Numerical verification](../docs/NUMERICAL_VERIFICATION_2026-11-06.md)
- [Reproducibility status](../docs/REPRODUCIBILITY_STATUS.md)
- [Versioned research archive](https://doi.org/10.5281/zenodo.24021746)

## Limitations

Saved-output reconstructions do not verify the full original BKMR MCMC pipeline. Manuscript claims and statistical documentation should distinguish empirical PIP calibration from formal Type I error control. Dataset redistribution and exact correspondence between manuscript scripts and released package functions require verification.
