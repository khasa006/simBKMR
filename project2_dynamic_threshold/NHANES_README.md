# NHANES urinary-metal component (candidate original script)

The included `R_original/realDataAnalysis_20240110.R` uses eight urinary metals, rather than the blood-metal pipeline. It reads `./data/cognitiveSurveyData_df.csv` with uppercase column names. In the GitHub repository that input currently lives at repository-root `data/`.

Independent calculation from an uploaded historical CSV with descriptive column names confirms 2,934 initial rows; 949 complete cases for eight urinary metals and cognitive score; response CV 15.1809769833. The PIP threshold 0.6463 is verified from the published formula, **not** by rerunning NHANES MCMC.

The old analysis script should remain unchanged as evidence of provenance. It uses `bkmr`, `tidyverse`, `fields` and 25,000 MCMC iterations; do not claim full reproduction without rerunning and checking Table 3 and Figures 3–4.
