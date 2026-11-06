# Data and code sharing declaration

**Decision recorded:** November 6, 2026.

The project authors have confirmed that **selected original research scripts and permitted datasets** may be shared publicly and selected **CC0 1.0 Universal** as the intended reuse dedication.

## Scope

- The existing `LICENSE` file is CC0 1.0 Universal; the draft branch README now states this correctly.
- CC0 is intended only for material that the rights holders are authorized to dedicate. It does **not** override restrictions applying to third-party inputs, software, raw NHANES sources, or copyrighted publications.
- The user explicitly approved sharing selected shareable materials, **not** publishing all raw archives. The 1+ GB research data folder, earlier manuscript drafts, cached model objects, duplicate simulation outputs, private paths, credentials, and any restricted source files are outside this blanket scope.
- Source code is preserved before any readability, path, or denominator edits. Reproducibility scripts are separated from historical analysis scripts.
- Numerical checks from saved PIP outputs match the reported logistic coefficients, published deviation summaries, and all 246 Supplementary Table 1 rows. End-to-end MCMC verification remains pending.
- Before deposition, review each selected file for sensitive fields, provenance and redistribution rights. All additions remain on the unmerged draft branch until reviewed.

## Release checklist

- [x] Research authors selected CC0 1.0 and authorized release of selected eligible research materials.
- [x] README license statement aligned with existing CC0 file on draft branch.
- [ ] Selected original scripts added with unmodified copies.
- [ ] Six PIP-output CSV datasets screened and added, or deposited separately with permanent DOI.
- [ ] Restricted and third-party sources excluded or linked upstream.
- [ ] Reproducible R package environment and relative execution paths documented.
- [ ] Draft PR reviewed and merged.
- [ ] Versioned GitHub release and Zenodo snapshot created.
- [ ] Journal's Open Science Badge declaration submitted with stable URL/DOI, if criteria met.

Do not claim that the original 25,000-iteration NHANES BKMR model or all historical simulations have been independently rerun.
