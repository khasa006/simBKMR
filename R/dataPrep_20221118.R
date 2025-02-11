# cognitive_bkmr.R
# Author: Kazi Tanvir Hasan
# Date: 2022-10-29
# Purpose: Prepare NHANES dataset for cognitive function analysis

# Load necessary libraries
library(nhanesA)   # For retrieving NHANES datasets
library(survey)    # For survey design and weighted analysis
library(tidyverse) # For data manipulation (dplyr, tidyr, ggplot2)

# -----------------------------------------------------------------------------
# Load NHANES datasets (2011-2012)
# -----------------------------------------------------------------------------

demographicG <- nhanes('DEMO_G')
medicalConditionsG <- nhanes('MCQ_G')
cognitiveFunctionG <- nhanes('CFQ_G')
bloodMetalG <- nhanes('PBCD_G')
urineMetalG <- nhanes('UHM_G')
diabetesG <- nhanes('DIQ_G')
smokingCigaretteG <- nhanes('SMQ_G')
bmiG <- nhanes('BMX_G')
bloodPressureCholesterolG <- nhanes('BPQ_G')
alcoholUseG <- nhanes('ALQ_G')

# -----------------------------------------------------------------------------
# Load NHANES datasets (2013-2014)
# -----------------------------------------------------------------------------

demographicH <- nhanes('DEMO_H')
medicalConditionsH <- nhanes('MCQ_H')
cognitiveFunctionH <- nhanes('CFQ_H')
bloodMetalH <- nhanes('PBCD_H')
urineMetalH <- nhanes('UM_H')
diabetesH <- nhanes('DIQ_H')
smokingCigaretteH <- nhanes('SMQ_H')
bmiH <- nhanes('BMX_H')
bloodPressureCholesterolH <- nhanes('BPQ_H')
alcoholUseH <- nhanes('ALQ_H')

# -----------------------------------------------------------------------------
# Append datasets from both cycles (2011-2012 and 2013-2014)
# -----------------------------------------------------------------------------

demographicInfo_df <- bind_rows(demographicG, demographicH)
diabetesInfo_df <- bind_rows(diabetesG, diabetesH)
smokingInfo_df <- bind_rows(smokingCigaretteG, smokingCigaretteH)
alcoholInfo_df <- bind_rows(alcoholUseG, alcoholUseH)
bloodPressureCholesterolInfo_df <- bind_rows(
  bloodPressureCholesterolG, bloodPressureCholesterolH
)
bmiInfo_df <- bind_rows(bmiG, bmiH)
bloodMetalInfo_df <- bind_rows(bloodMetalG, bloodMetalH)
urineMetalInfo_df <- bind_rows(urineMetalG, urineMetalH)
cognitiveInfo_df <- bind_rows(cognitiveFunctionG, cognitiveFunctionH)
medicalInfo_df <- bind_rows(medicalConditionsG, medicalConditionsH)

# -----------------------------------------------------------------------------
# Recode demographic data
# -----------------------------------------------------------------------------

demographicInfo_df <- demographicInfo_df %>%
  mutate(
    # Convert gender to categorical values
    GENDER = case_when(RIAGENDR == 1 ~ "Male", RIAGENDR == 2 ~ "Female"),

    # Convert race to readable labels
    RACE = case_when(
      RIDRETH1 == 1 ~ "Mexican American",
      RIDRETH1 == 2 ~ "Other Hispanic",
      RIDRETH1 == 3 ~ "Non Hispanic White",
      RIDRETH1 == 4 ~ "Non Hispanic Black",
      RIDRETH1 == 5 ~ "Other Race"
    ),

    # Convert marital status to categorical values
    MARTLS = case_when(
      DMDMARTL == 1 ~ "Married",
      DMDMARTL == 2 ~ "Widowed",
      DMDMARTL == 3 ~ "Divorced",
      DMDMARTL == 4 ~ "Separated",
      DMDMARTL == 5 ~ "Never Married",
      DMDMARTL == 6 ~ "Living with Partner",
      TRUE ~ "unknown"
    ),

    # Convert education level to meaningful categories
    EDUCL = case_when(
      DMDEDUC2 <= 2 ~ "<High School",
      DMDEDUC2 == 3 ~ "High School",
      DMDEDUC2 > 3 & DMDEDUC2 <= 5 ~ ">High School",
      TRUE ~ "unknown"
    )
  ) %>%
  select(
    SEQN, GENDER, RIDAGEYR, RACE, EDUCL, MARTLS, INDFMPIR, WTINT2YR, WTMEC2YR,
    SDMVSTRA, SDMVPSU
  ) %>%
  rename(AGE = RIDAGEYR, PRTIO = INDFMPIR)

# -----------------------------------------------------------------------------
# Merge all datasets
# -----------------------------------------------------------------------------

cognitiveProject_df <- demographicInfo_df %>%
  left_join(diabetesInfo_df, by = 'SEQN') %>%
  left_join(smokingInfo_df, by = 'SEQN') %>%
  left_join(alcoholInfo_df, by = 'SEQN') %>%
  left_join(bloodPressureCholesterolInfo_df, by = 'SEQN') %>%
  left_join(bmiInfo_df, by = 'SEQN') %>%
  left_join(bloodMetalInfo_df, by = 'SEQN') %>%
  left_join(urineMetalInfo_df, by = 'SEQN') %>%
  left_join(medicalInfo_df, by = 'SEQN')

# -----------------------------------------------------------------------------
# Define survey design for weighted analysis
# -----------------------------------------------------------------------------

cognitiveSurveyDesign_svyd <- svydesign(
  data = cognitiveProject_df,
  ids = ~SDMVPSU,    # Primary sampling units
  strata = ~SDMVSTRA, # Stratification variable
  weights = ~WTMEC2YR, # Two-year weight for MEC exam
  nest = TRUE,
  survey.lonely.psu = "adjust"
)

# -----------------------------------------------------------------------------
# Generate descriptive statistics
# -----------------------------------------------------------------------------

cognitiveSurveyData_df <- cognitiveSurveyDesign_svyd[["variables"]] %>%
  tibble()

# NHANES updates its data frequently. To ensure reproducibility, use the data already saved in the "data" folder.
