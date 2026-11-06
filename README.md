# PhD Thesis Analysis Code Repository

This repository contains analysis code for two projects related to my PhD thesis. The code is organized into separate scripts that must be executed in a specific order to reproduce the analyses.

## Project 1: The Sensitivity of Bayesian Kernel Machine Regression (BKMR) to Data Distribution: A Comprehensive Simulation Analysis

### Steps to Reproduce the Analysis

#### 1. Data Acquisition and Preparation
- `dataPrep_20221118.R` – Prepares raw data for analysis.

#### 2. Data Transformation
- `createParam_20240111.R` – Performs necessary data transformations.

#### 3. Gender-Specific Segmentation
- `createParam_20240111.R` – Segments data based on gender.

#### 4. Parameter Estimation
- `createParam_20240111.R` – Estimates key model parameters.

#### 5. Parametric Bootstrap Simulation of Metal Data
- Diagonal: `createSimData_20230605.R`
- Unstructured: `createSImulatedData_20240111.R`

#### 6. Sample Generation and Comparison (Only for Unstructured)
- `checkSimGender_20230605.R` – Compares generated samples by gender.

#### 7. Effect Size Simulation
- Mean Effect Size Analysis:
  - `testSizeAnalysisMean_20240220.R`
  - `unstrucTestSizeAnalysisMean_20230220.R`
- Variance Effect Size Analysis:
  - `testSizeAnalysisVar_20240220.R`
  - `unstucTestSizeAnalysisVar_20240225.R`

#### 8. Power Analysis Simulation
- Unstructured Covariance Matrix:
  - `generateSignal_20240112.R`
- Diagonal Covariance Matrix:
  - `diagPowerAnalysis_20240307.R`

#### 9. BKMR Model Fitting and Evaluation
- `powerAnalysis_20240127.R`
- `diagPowerAnalysis_20240307.R`

### 10. Results

#### Effect Size Analysis
- `testSizeCVComparisonMean_20240220.R`
- `testSizeComparisonVar_20240220.R`
- `cvTestSizeComparision_20240220.R`

#### Power Analysis (Unstructured Covariance Matrix)
- `PowerAnalysisResult_20240127.R`

#### Power Analysis (Diagonal Covariance Matrix)
- `diagPowerAnalysisResults_20240307.R`

#### Real Data Analysis
- `bloodMelatDataAnalysis_20250123.R`

## Project 2: Dynamic Thresholding Framework for BKMR

The accepted *Research in Statistics* study proposes a simulation-calibrated PIP selection threshold depending on response CV and sample size. See [Project 2 documentation](project2_dynamic_threshold/README.md), [numerical verification](docs/NUMERICAL_VERIFICATION_2026-11-06.md), and [release/import checklist](docs/IMPORT_AND_RELEASE_CHECKLIST_2026-11-06.md). **Selected original R scripts and all six saved PIP CSV inputs are now committed on the draft pull-request branch.** Published coefficients and selection-frequency summaries were independently checked from saved outputs; full original BKMR MCMC was not rerun.

## How to Use This Repository
1. Clone this repository:
   ```sh
   git clone https://github.com/khasa006/simBKMR.git
   ```
2. Navigate to the project folder:
   ```sh
   cd repository-name
   ```
3. Run the scripts in the specified order to reproduce the analysis.

## Requirements
- R (latest version recommended)
- Required R packages (listed in each script; install as needed using `install.packages()`)

## Contact
For any questions or clarifications, feel free to contact me at [khasa006@fiu.edu].

## License
The repository's [LICENSE](LICENSE) contains the **Creative Commons CC0 1.0 Universal public-domain dedication**, not MIT. The project authors have chosen CC0 for original research scripts and datasets they are authorized to release. Third-party datasets, source materials, and software dependencies remain subject to their own terms. Do not assume this CC0 dedication covers redistributed NHANES source data or material owned by third parties.

