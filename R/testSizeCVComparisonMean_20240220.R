# Test size comparison for diagonal and unstructured covariance data
# When SDs are fixed but Means are not
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
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY01.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY02.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY03.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY04.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY05.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY06.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY07.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY08.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY09.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY10.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY11.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY12.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY13.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY14.rds",
  "data/diagTestSizeAnalysisMean_20240220/diagtestSizeMeanY15.rds"
)

# Define file paths and corresponding SD values for unstructured covariance data
unstrucFilePaths <- c(
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY01.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY02.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY03.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY04.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY05.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY06.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY07.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY08.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY09.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY10.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY11.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY12.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY13.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY14.rds",
  "data/unstrucTestSizeAnalysisMean_20240220/unstructestSizeMeanY15.rds"
)

# Define Mean values
meanValues <- c(
  -0.223, 0.94, -0.08, -1.61, -1.27, -0.19, -4.25, -1.9, -2.4, -0.01, -0.025,
  -0.05, -0.30, -0.50, -0.70
)

# Process each effect size result for diagonal covariance data
diagEffectSizeMean <- map2_dfr(
  diagFilePaths,
  meanValues,
  ~ processEffectSize(.x, paste0("diag_test_size", .y), mean = .y, sd = 0.10)
)

# Process each effect size result for unstructured covariance data
unstrucEffectSizeMean <- map2_dfr(
  unstrucFilePaths,
  meanValues,
  ~ processEffectSize(.x, paste0("unstruc_test_size", .y), mean = .y, sd = 0.10)
)

# Plot for diagonal covariance data
ggplot(data = diagEffectSizeMean %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = testSize
  ) +
  # Scatter Plot
  geom_jitter() +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  labs(
    x = "Coefficient of Variation",
    y = "Test Size (Fixed Sds)",
    title = "Comparing the Relationship between CV and Test Size.",
    subtitle = "Diagonal Covariance Data with Different Means"
  )

ggplot(data = diagEffectSizeMean %>% filter(variable != "Overall")) +
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
    y = "Test Size (Fixed Sds)",
    title = "Comparing the Relationship between CV and Test Size.",
    subtitle = "Diagonal Covariance Data with Different Means"
  )

# Plot for unstructured covariance data
ggplot(data = unstrucEffectSizeMean %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = testSize
  ) +
  # Scatter Plot
  geom_jitter() +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  labs(
    x = "Coefficient of Variation",
    y = "Test Size (Fixed Sds)",
    title = "Comparing the Relationship between CV and Test Size.",
    subtitle = "Unstructured Covariance Data with Different Means"
  )

ggplot(data = unstrucEffectSizeMean %>% filter(variable != "Overall")) +
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
    y = "Test Size (Fixed Sds)",
    title = "Comparing the Relationship between CV and Test Size.",
    subtitle = "Unstructured Covariance Data with Different Means"
  )
