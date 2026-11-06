# Test size comparison for diagonal and unstructured covariance data
# When means are fixed but SDs are not
# Kazi Tanvir Hasan and Dr. Gabriel Odom
# 2024-02-20

library(tidyverse)

# Define a function to process effect size results
processEffectSize <- function(filePath, designName, meanValue, sdValue) {
  # Read effect size results from file
  effectSizeResult <- read_rds(filePath)
  
  # Calculate effect size for each variable
  metalGroupEffectSize <- effectSizeResult %>%
    map_dfr(pluck, "simBloodPIP") %>% 
    group_by(variable) %>% 
    summarise(testSize = mean(PIP > 0.5))
  
  # Calculate overall effect size
  metalOverallEffectSize <- effectSizeResult %>%
    map_dfr(pluck, "simBloodPIP") %>%  
    summarise(testSize = mean(PIP > 0.5))  
  
  # Combine group and overall effect sizes
  actualEffectSize <- bind_rows(metalGroupEffectSize, metalOverallEffectSize) %>% 
    # Handle missing variable names
    mutate(variable = ifelse(is.na(variable), "Overall", variable)) %>% 
    # Add design, mean, and sd information
    mutate(
      design = designName,
      mean = meanValue,
      sd = sdValue
    ) %>% 
    # Select relevant columns
    select(variable, design, testSize, mean, sd)
  
  return(actualEffectSize)
}

# Define file paths and corresponding SD values for diagonal covariance data
diagFilePaths <- c(
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY01.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY02.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY03.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY04.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY05.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY06.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY07.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY08.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY09.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY10.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY11.rds",
  "data/diagTestSizeAnalysisVar_20240220/diagtestSizeVarY12.rds"
)

# Define file paths and corresponding SD values for unstructured covariance data
unstrucFilePaths <- c(
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY01.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY02.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY03.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY04.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY05.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY06.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY07.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY08.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY09.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY10.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY11.rds",
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY12.rds"
)

# Define SD values
sdValues <- c(0.10, 0.25, 0.50, 2/3, 1.0, 1.5, 2.0, 3.5, 5.0, 7.5, 10.0, 15.0)

# Process each effect size result for diagonal covariance data
diagEffectSizeVar <- map2_dfr(
  diagFilePaths,
  sdValues,
  ~ processEffectSize(.x, paste0("diag_test_size", .y), mean = -1.0, sd = .y)
)

# Process each effect size result for unstructured covariance data
unstrucEffectSizeVar <- map2_dfr(
  unstrucFilePaths,
  sdValues,
  ~ processEffectSize(.x, paste0("unstruc_test_size", .y), mean = -1.0, sd = .y)
)

# Plot for diagonal covariance data
ggplot(data = diagEffectSizeVar %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = testSize
  ) +
  # Scatter Plot
  geom_jitter() +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  labs(
    x = "Coefficient of Variation",
    y = "Test Size (Fixed Means)",
    title = "Comparing the Relationship between CV and Test Size.",
    subtitle = "Diagonal Covariance Data with Different SDs"
  )

ggplot(data = diagEffectSizeVar %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = testSize,
    group = abs(sd/mean)
  ) +
  # Box Plot
  geom_boxplot(position = "dodge") +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  labs(
    x = "Coefficient of Variation",
    y = "Test Size (Fixed Means)",
    title = "Comparing the Relationship between CV and Test Size.",
    subtitle = "Diagonal Covariance Data with Different SDs"
  )

# Plot for unstructured covariance data
ggplot(data = unstrucEffectSizeVar %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = testSize
  ) +
  # Scatter Plot
  geom_jitter() +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  labs(
    x = "Coefficient of Variation",
    y = "Test Size (Fixed Means)",
    title = "Comparing the Relationship between CV and Test Size.",
    subtitle = "Unstructured Covariance Data with Different SDs"
  )

ggplot(data = unstrucEffectSizeVar %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = testSize,
    group = abs(sd/mean)
  ) +
  # Box Plot
  geom_boxplot(position = "dodge") +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  labs(
    x = "Coefficient of Variation",
    y = "Test Size (Fixed Means)",
    title = "Comparing the Relationship between CV and Test Size.",
    subtitle = "Unstructured Covariance Data with Different SDs"
  )
