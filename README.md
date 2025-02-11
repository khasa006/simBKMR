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

## Project 2: [To Be Added]

## How to Use This Repository
1. Clone this repository:
   ```sh
   git clone https://github.com/yourusername/repository-name.git
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
This project is licensed under the [MIT License](LICENSE).

