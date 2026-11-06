# NHANES analysis provenance and repository readiness

Audit date: November 6, 2026. This audit is based on the uploaded research archive and the current GitHub repository.

## Manuscript urinary-metal analysis

The accepted dynamic-threshold manuscript reports 2011–2014 NHANES, 2,934 initial rows, 949 complete cases, and outcome CV 15.18. The original `r_code/realDataAnalysis_20240110.R` in the uploaded archive uses eight **urinary** metals (`UBARIUM`, `UCADMIUM`, `UCOBALT`, `UMANGE`, `ULEAD`, `UTIN`, `USTOUM`, `UURANIUM`) and `CSCORE`.

Independent checks on the uploaded `cognitiveSurveyData_df.csv` with descriptive column names yield:
- 2,934 input rows.
- 949 complete-case rows across the eight urinary metals and cognitive score.
- Mean CSCORE 0.04995288625816649; standard deviation 0.7583336165360675.
- CV (SD/mean) = 15.18097698332881.
- The previously verified logistic formula gives threshold 0.646321317 for rounded CV=15.18 and n=949.

The repository's current `data/cognitiveSurveyData_df.csv` has short uppercase column names and is the expected input for the historical R script. A base-R input check was added at `project2_dynamic_threshold/check_nhanes_inputs.R`. This script **has not yet been executed against the repository's copy** and should be run in RStudio before full validation is claimed.

## Distinguish blood and urine scripts

The current `R/bloodMetalDataAnalysis_20250123.R` in the GitHub repository analyzes **blood** metals, includes mercury, and is not the corresponding script for the accepted paper's eight-metal urinary mixture. Do not substitute it for `realDataAnalysis_20240110.R`.

The original urinary-metal script uses log10(x+1) transformation, SD scaling, complete cases, `fields::cover.design` with 50 knots, `bkmr::kmbayes` with 25,000 iterations, and PIP extraction. Full MCMC rerun has **not** been conducted. It also contains historical plotting code that should be examined against Figures 3–4.

The uploaded ZIP contains scripts `r_code/bkmrUrine_20230112.Rmd` and `r_code/dataPrep_20221118.Rmd` that may help trace earlier NHANES preparation. Their compatibility with the accepted workflow has not been verified.

## Publication data and sharing checks

- NHANES public-use data can generally be accessed from the source, but check any linkage, derived data, or disclosure restrictions before republishing the assembled dataset.
- Verify whether the GitHub `data/cognitiveSurveyData_df.csv` is equivalent to the uploaded historical CSV: the headings differ and some variables may be recoded.
- For full paper reproduction, confirm raw data download URLs, cycles, joins, missing-data rules, PIP values in Table 3, and Figures 3–4.
- Repository `LICENSE` contains CC0 1.0 Universal text; the current top-level README instead states MIT. This is a **documentation/licensing inconsistency**, not yet resolved; coauthor approval may be needed before changing reuse terms.
- The draft branch contains documentation and checking scripts. The complete original source scripts and large input datasets have **not yet been committed** through GitHub.

**Status:** Sample size and CV independently checked against an uploaded historical CSV; threshold arithmetic matches. End-to-end NHANES BKMR reproduction is pending.
