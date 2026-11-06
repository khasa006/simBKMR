# Project 1 — BKMR sensitivity to data distribution

The existing public `simBKMR` repository documents the BKMR sensitivity study (data preparation, covariance simulations, response simulations, effect-size evaluation and power analysis). `R_original/` contains selected *unmodified* historical scripts from the submitted project archive.

These are a preservation/organization set, **not a verified complete executable pipeline**. Some entry points named in the current GitHub README were not located under the same filenames in the uploaded archive, and upstream input datasets have not been mapped to each script. The existing top-level `R/` and `data/` in GitHub must remain unchanged until equivalence is established.

Suggested review sequence: (1) preparation and parameter estimation; (2) simulated predictor generation; (3) response generation; (4) mean/variance selection simulations; (5) power simulations; (6) summary tables. See source code for actual dependencies. Do not assume these run out-of-the-box.
