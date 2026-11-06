# Quantiles of samples
# Gabriel Odom and Tanvir Hasan
# 2024-12-09

library(tidyverse)

# Read data files using tidyverse functions (read_csv)
mleDataDf150 <- read_csv("data/mleData_20240920/mleData150_df.csv")
mleDataDf300 <- read_csv("data/mleData_20240920/mleData300_df.csv")
mleDataDf600 <- read_csv("data/mleData_20240920/mleData600_df.csv")
mleDataDf1000 <- read_csv("data/mleData_20240920/mleData1000_df.csv")
mleDataDf2000 <- read_csv("data/mleData_20240920/mleData2000_df.csv")
mleDataDf2934 <- read_csv("data/mleData_20240920/mleData2934_df.csv")

# Combine datasets for mleData and summaryLong into one dataframe
mleResultDf <- bind_rows(
  mleDataDf150 %>% mutate(sampleSize = 150),
  mleDataDf300 %>% mutate(sampleSize = 300),
  mleDataDf600 %>% mutate(sampleSize = 600),
  mleDataDf1000 %>% mutate(sampleSize = 1000),
  mleDataDf2000 %>% mutate(sampleSize = 2000),
  mleDataDf2934 %>% mutate(sampleSize = 2934)
)

# Clean up individual datasets to free memory
rm(
  mleDataDf150, mleDataDf300, mleDataDf600, mleDataDf1000, mleDataDf2000, mleDataDf2934
)

# Calculate 95th percentile for PIP grouped by sample size and CV
pipQuantileDf <- mleResultDf %>%
  group_by(sampleSize, CV) %>%
  summarise(
    PIP_q95 = quantile(PIP, 0.95),  # Compute the 95th percentile for PIP
    .groups = "drop"
  )

# Plot the data
ggplot(data = pipQuantileDf) + 
  theme(legend.position = "bottom") +
  aes(x = log2(CV), y = PIP_q95, colour = factor(sampleSize, ordered = TRUE)) + 
  guides(colour = guide_legend(title = "Sample Size")) +
  geom_jitter(alpha = 0.5)

# # Define the custom logistic function with two independent variables
# calculateY2Vars <- function(A, D, beta1, beta2, x1, x2) {
#   # Where:
#   # A: Upper asymptote (similar to single-variable logistic models)
#   # D: Lower asymptote (similar to single-variable logistic models)
#   # beta1: The coefficient for the first independent variable (x1), controlling the midpoint shift for x1
#   # beta2: The coefficient for the second independent variable (x2), controlling the midpoint shift for x2
#   # x1: The first independent variable (log-transformed CV in this case)
#   # x2: The second independent variable (log-transformed sampleSize in this case)
#   
#   # The logistic function is applied to both independent variables (x1 and x2),
#   # where their respective midpoint shifts are controlled by beta1 and beta2.
#   y <- (A / (1 + exp(-(beta1 * x1 + beta2 * x2)))) + D
#   
#   # Return the calculated value of y
#   return(y)
# }
# 
# # Fit the model using nls
# model <- nls(
#   PIP_q95 ~ calculateY2Vars(
#     A, 
#     D, 
#     beta1, 
#     beta2, 
#     x1 = log2(CV), 
#     x2 = log10(sampleSize)
#   ),
#   data = pipQuantileDf,
#   start = list(
#     A = 1, D = 0, beta1 = 0.5, beta2 = 0.5
#   )
# )
# 
# # Summarize the fitted model
# summary(model)
# 
# # Predict PIP_q95 using the custom logistic model (calculateY2Vars)
# pipQuantileDf$predPIPq95 <- predict(model, pipQuantileDf)
# 
# ggplot(data = pipQuantileDf) + 
#   theme(legend.position = "bottom") +
#   aes(
#     x = log2(CV),
#     colour = factor(sampleSize, ordered = TRUE)
#   ) + 
#   guides(colour = guide_legend(title = "Sample Size")) +
#   geom_jitter(aes(y = PIP_q95), alpha = 0.5) + 
#   geom_line(aes(y = predPIPq95))
# 
# ##########
# 
# 
# 
# # Define the pipThresh_fn function
# pipThresh_fn <- function(cv, ss) {
#   (2.61333 / (1 + exp(-(0.16310 * log2(cv) - 0.30950 * log10(ss))))) - 0.42716
# }
# 
# # Define the list of cv values and sample sizes
# cvValues <- c(
#   0.02352941, 0.02818383, 0.03162278, 0.03548134, 0.04166667, 0.04786301,
#   0.05263158, 0.06211180, 0.07079458, 0.07874016, 0.08912509, 0.10000000,
#   0.10638298, 0.12589250, 0.14285714, 0.15848930, 0.17782790, 0.20000000,
#   0.22387210, 0.25000000, 0.28183830, 0.31622780, 0.33333333, 0.39810720,
#   0.44843049, 0.50000000, 0.52631579, 0.60255960, 0.66666667, 0.75857760,
#   0.87096360, 1.00000000, 1.25000000, 1.50000000, 2.00000000, 3.50000000,
#   4.00000000, 5.00000000, 7.50000000, 10.00000000, 15.00000000
# )
# 
# sampleSizes <- c(150, 300, 600, 1000, 2000, 2934)
# 
# # Create a data frame of all combinations of sampleSize and CV using expand_grid
# cvSampleCombinations <- expand_grid(sampleSize = sampleSizes, CV = cvValues)
# 
# # Perform the summarisation for each combination of sampleSize and CV
# results <- cvSampleCombinations %>%
#   left_join(mleResultDf, by = c("sampleSize", "CV")) %>%  # Join the combinations with mleResultDf
#   group_by(sampleSize, CV) %>%
#   summarise(
#     testSizeOld = sum(PIP < 0.5),
#     testSizeNew = sum(
#       PIP < pipThresh_fn(cv = unique(CV), ss = unique(sampleSize))
#     ),
#     .groups = 'drop'  # Drop the grouping after summarisation
#   )
# 
# # View the results
# results
# 
# ggplot(data = results %>% filter(testSizeOld <= 250)) + aes(x = log2(CV), size = log10(sampleSize)) + geom_hline(yintercept = 0.95) + geom_point(aes(y = testSizeOld/250), color = "orange", shape = 21) + geom_point(aes(y = testSizeNew/250), color = "green", shape = 21)


calculateY2Vars2 <- function(A = 0, K = 1, C, beta1, beta2, x1, x2) {
  # Where:
  # A: Upper asymptote (similar to single-variable logistic models)
  # C: typically takes a value of 1
  # beta1: The coefficient for the first independent variable (x1), controlling the midpoint shift for x1
  # beta2: The coefficient for the second independent variable (x2), controlling the midpoint shift for x2
  # x1: The first independent variable (log-transformed CV in this case)
  # x2: The second independent variable (log-transformed sampleSize in this case)
  
  # The logistic function is applied to both independent variables (x1 and x2),
  # where their respective midpoint shifts are controlled by beta1 and beta2.
  y <- (
    A + (K - A) / (C + exp(-beta1 * x1))^ ((beta2 * x2)) 
  ) 
  
  # Return the calculated value of y
  return(y)
}

# Fit the model using nls
model <- nls(
  PIP_q95 ~ calculateY2Vars2(
    A = 0,
    K = 1, 
    C,
    beta1, 
    beta2, 
    x1 = log2(CV), 
    x2 = log10(sampleSize)
  ),
  data = pipQuantileDf,
  start = list(
    C = 1, beta1 = 1, beta2 = 1
  )
)

# Summarize the fitted model
summary(model)

# Predict PIP_q95 using the custom logistic model (calculateY2Vars)
pipQuantileDf$predPIPq95 <- predict(model, pipQuantileDf)

cvPIP95Plot <- ggplot(data = pipQuantileDf) + 
  theme(legend.position = "bottom") +
  aes(
    x = log2(CV),
    colour = factor(sampleSize, ordered = TRUE)
  ) + 
  labs(
    x = "log2(CV)", 
    y = "95th Percentile PIP",
  ) + 
  guides(colour = guide_legend(title = "Sample Size")) +
  geom_jitter(aes(y = PIP_q95), alpha = 0.5) + 
  geom_line(aes(y = predPIPq95))

cvPIP95Plot

# ggsave(
#   "figure1_20250311.tiff",
#   plot = cvPIP95Plot,
#   width = 8,
#   height = 6,
#   dpi = 300,
#   compression = "lzw",
#   bg = "white"
# )

##########



# Define the pipThresh_fn function
pipThresh_fn <- function(cv, ss) {
  (
    0 + (1 - 0) / (1.30460 + exp(-0.59867 * log2(cv)))^ (0.43565 * (log10(ss))) 
  ) 
}

# Define the list of cv values and sample sizes
cvValues <- c(
  0.02352941, 0.02818383, 0.03162278, 0.03548134, 0.04166667, 0.04786301,
  0.05263158, 0.06211180, 0.07079458, 0.07874016, 0.08912509, 0.10000000,
  0.10638298, 0.12589250, 0.14285714, 0.15848930, 0.17782790, 0.20000000,
  0.22387210, 0.25000000, 0.28183830, 0.31622780, 0.33333333, 0.39810720,
  0.44843049, 0.50000000, 0.52631579, 0.60255960, 0.66666667, 0.75857760,
  0.87096360, 1.00000000, 1.25000000, 1.50000000, 2.00000000, 3.50000000,
  4.00000000, 5.00000000, 7.50000000, 10.00000000, 15.00000000
)

sampleSizes <- c(150, 300, 600, 1000, 2000, 2934)

# Create a data frame of all combinations of sampleSize and CV using expand_grid
cvSampleCombinations <- expand_grid(sampleSize = sampleSizes, CV = cvValues)

# Perform the summarisation for each combination of sampleSize and CV
results <- mleResultDf %>%
  left_join(cvSampleCombinations, by = c("sampleSize", "CV")) %>%  # Join the combinations with mleResultDf
  group_by(sampleSize, CV) %>%
  summarise(
    testSizeOld = sum(PIP < 0.5),
    testSizeNew = sum(
      PIP < pipThresh_fn(cv = unique(CV), ss = unique(sampleSize))
    ),
    .groups = 'drop'  # Drop the grouping after summarisation
  )

# View the results
results <- results %>%
  mutate(
    testSizeOldProp = case_when(
      testSizeOld <= 250 ~ testSizeOld / 250,
      testSizeOld > 250  ~ testSizeOld / 500
    ),
    testSizeNewProp = case_when(
      testSizeNew <= 250 ~ testSizeNew / 250,
      testSizeNew > 250  ~ testSizeNew / 500
    )
  )

results %>% 
  summarise(
    static_mse = mean((testSizeOldProp - 0.95)^2),
    dynamic_mse = mean((testSizeNewProp - 0.95)^2),
    static_mad = median(abs(testSizeOldProp - 0.95)),
    dynamic_mad = median(abs(testSizeNewProp - 0.95)),
    static_max = max(abs(testSizeOldProp - 0.95)),
    dynamic_max = max(abs(testSizeNewProp - 0.95))
  ) 
# Because the test sizes are not normally distributed, MAD is more appropriate
# Dynamic MAD = 0.0260 while Static MAD = 0.0460



comparisionPlot <- ggplot(data = results) +
  aes(x = log2(CV), size = log10(sampleSize)) +
  geom_point(
    aes(y = 1 - testSizeNewProp, fill = "Dynamic"), 
    shape = 23, color = "black", alpha = 0.7
  ) + 
  geom_point(
    aes(y = 1 - testSizeOldProp, fill = "Fixed (0.5)"), 
    shape = 21, color = "black", alpha = 0.7
  ) + 
  geom_hline(yintercept = 1 - 0.95, linetype = "dashed", color = "red") + 
  scale_fill_manual(
    name = "Threshold", 
    values = c("Fixed (0.5)" = "orange", "Dynamic" = "green")
  ) +
  labs(
    x = "Coefficient of Variation (log2 transformed)",
    y = "Empirical False-Positive Selection Rate (ideal = 0.05)",
    size = "Sample Size \n(log10 transformed)"
  ) +
  theme_minimal()

comparisionPlot

# Save the plot as a high-resolution TIFF file
# ggsave(
#   "figure2_20250508.tiff",
#   plot = comparisionPlot,
#   width = 8,
#   height = 6,
#   dpi = 300,
#   compression = "lzw",
#   bg = "white"
# )

