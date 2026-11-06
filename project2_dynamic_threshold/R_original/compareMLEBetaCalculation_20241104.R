# Compare MLE and Fit the Model
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2024-12-01

library(ggpmisc)
library(quantreg)
library(lqr)
library(tidyverse)

# Read data files using tidyverse functions (read_csv)
mleDataDf150 <- read_csv("data/mleData_20240920/mleData150_df.csv")
mleDataDf300 <- read_csv("data/mleData_20240920/mleData300_df.csv")
mleDataDf600 <- read_csv("data/mleData_20240920/mleData600_df.csv")
mleDataDf1000 <- read_csv("data/mleData_20240920/mleData1000_df.csv")
mleDataDf2000 <- read_csv("data/mleData_20240920/mleData2000_df.csv")
mleDataDf2934 <- read_csv("data/mleData_20240920/mleData2934_df.csv")

# summaryLongDf150 <- read_csv("data/mleData_20240920/summaryLong_df150.csv")
# summaryLongDf300 <- read_csv("data/mleData_20240920/summaryLong_df300.csv")
# summaryLongDf600 <- read_csv("data/mleData_20240920/summaryLong_df600.csv")
# summaryLongDf1000 <- read_csv("data/mleData_20240920/summaryLong_df1000.csv")
# summaryLongDf2000 <- read_csv("data/mleData_20240920/summaryLong_df2000.csv")
# summaryLongDf2934 <- read_csv("data/mleData_20240920/summaryLong_df2934.csv")

# Combine datasets for mleData and summaryLong into one dataframe
mleResultDf <- bind_rows(
  mleDataDf150 %>% mutate(sampleSize = 150),
  mleDataDf300 %>% mutate(sampleSize = 300),
  mleDataDf600 %>% mutate(sampleSize = 600),
  mleDataDf1000 %>% mutate(sampleSize = 1000),
  mleDataDf2000 %>% mutate(sampleSize = 2000),
  mleDataDf2934 %>% mutate(sampleSize = 2934)
)

# summaryResultDf <- bind_rows(
#   summaryLongDf150 %>% mutate(sampleSize = 150),
#   summaryLongDf300 %>% mutate(sampleSize = 300),
#   summaryLongDf600 %>% mutate(sampleSize = 600),
#   summaryLongDf1000 %>% mutate(sampleSize = 1000),
#   summaryLongDf2000 %>% mutate(sampleSize = 2000),
#   summaryLongDf2934 %>% mutate(sampleSize = 2934)
# ) %>%
#   filter(Statistic %in% c("mean_pip", "sd_pip", "pip_95"))

# # Plot the relationship between CV and various statistics
# plotStatistics <- function(statisticName) {
#   ggplot(summaryResultDf %>% filter(Statistic == statisticName)) +
#     aes(
#       x = CV, 
#       y = Value, 
#       group = sampleSize, 
#       color = factor(sampleSize)
#     ) +
#     scale_x_log10() +  # Log-scale for CV (Coefficient of Variation)
#     labs(
#       x = "Coefficient of Variation (logCV)", 
#       y = "Value", 
#       title = paste("CV vs", statisticName)
#     ) +
#     geom_point() +  # Scatter plot of data points
#     stat_smooth(method = "loess") +  # Add a LOESS smoothing line
#     stat_poly_eq(
#       aes(label = after_stat(eq.label)), 
#       formula = y ~ x, 
#       parse = TRUE, 
#       label.x = "right", 
#       label.y = "top"
#     ) +  # Add equation on the plot
#     facet_wrap(~Statistic, scales = "free")  # Faceting for different statistics
# }
# 
# # Plot for all statistics: mean_pip, sd_pip, pip_95
# plotStatistics("mean_pip")
# plotStatistics("sd_pip")
# plotStatistics("pip_95")

######## Model Fitting #######


# Quantile Regression
qr_model <- rq(PIP ~ log2(CV) + log10(sampleSize), data = mleResultDf, tau = 0.95)

summary.rq(qr_model)
# QPIP = 0.53 + 0.07 * log2(CV) - 0.049 * log10(sampleSize)

# Create a new dataframe based on the formula
qPIP_df <- mleResultDf %>%
  mutate(
    QPIP = 0.53 + 0.07 * log2(CV) - 0.049 * log10(sampleSize)
  )

# # View the new dataframe
# head(qPIP_df)


# 
# ggplot(mleResultDf) +
#   aes(x = log2(CV), y = PIP, color = sampleSize) +
#   scale_color_viridis_c() +
#   geom_jitter(alpha = 0.01)
# 
# # Plot for each sampleSize with fitted lines of CV vs PIP on the same plot
# ggplot(qPIP_df) + 
#   aes(x = log2(CV), y = PIP, color = factor(sampleSize)) +  # Color by sampleSize
#   scale_color_viridis_d() +  # Using viridis color scale
#   geom_jitter(alpha = 0.01) +  # Jitter for better visibility
#   
#   # Add fitted lines for each sampleSize on the same plot
#   geom_smooth(se = FALSE, aes(group = factor(sampleSize))) + 
#   
#   theme_minimal()  # Apply a minimal theme for better clarity


# Create the ggplot with the jittered points from mleResultDf and smooth lines from qPIP_df
ggplot(mleResultDf) +
  aes(x = log2(CV), y = PIP, color = factor(sampleSize)) +
  geom_jitter(alpha = 0.01) +  # Jitter points to avoid overlap
  geom_smooth(
    data = qPIP_df, 
    aes(x = log2(CV), y = QPIP, group = sampleSize), 
    size = 1
  ) +  # Smoothing lines based on QPIP_df
  labs(
    title = "Jittered Points with Smoothing Lines by Sample Size (from QR Model)",
    x = "Log2(CV)",
    y = "PIP"
  ) +
  theme_minimal() +
  scale_color_viridis_d()  # Optional: For a nice color scale



# # Logistic Linear Quantile Regression
# lqrModel <- Log.lqr(
#   PIP ~ log2(CV) + log10(sampleSize),
#   data = mleResultDf ,
#   p = 0.95
# )
# # I ran it in the HPC
# 
# 
# lqrResults <- read_rds("data/mleData_20240920/lqr_model.rds")
# 
# 
# 
# # Quantile Regression
# 
# # Create a new dataframe based on the formula
# lqPIP_df <- mleResultDf %>%
#   mutate(
#     LQPIP = exp(4.7422928 + 0.6759276 * log2(CV) - 1.4351169 * log10 (sampleSize)) / 
#       (1 + exp(4.7422928 + 0.6759276 * log2(CV) - 1.4351169 * log10 (sampleSize)))
#   )
# 
# # # View the new dataframe
# # head(qPIP_df)
# 
# 
# # 
# # ggplot(mleResultDf) +
# #   aes(x = log2(CV), y = PIP, color = sampleSize) +
# #   scale_color_viridis_c() +
# #   geom_jitter(alpha = 0.01)
# # 
# # # Plot for each sampleSize with fitted lines of CV vs PIP on the same plot
# # ggplot(qPIP_df) + 
# #   aes(x = log2(CV), y = PIP, color = factor(sampleSize)) +  # Color by sampleSize
# #   scale_color_viridis_d() +  # Using viridis color scale
# #   geom_jitter(alpha = 0.01) +  # Jitter for better visibility
# #   
# #   # Add fitted lines for each sampleSize on the same plot
# #   geom_smooth(se = FALSE, aes(group = factor(sampleSize))) + 
# #   
# #   theme_minimal()  # Apply a minimal theme for better clarity
# 
# 
# # Base plot with mleResultDf for jitter points
# combinedPlot_lqr <- ggplot(mleResultDf) +
#   aes(x = log2(CV), y = PIP, color = factor(sampleSize)) +
#   geom_jitter(alpha = 0.01) +  # Points from mleResultDf
#   
#   # Add smoothed lines from qPIP_df, treating sampleSize as a factor
#   geom_smooth(
#     data = lqPIP_df, 
#     aes(group = factor(sampleSize), color = factor(sampleSize)), 
#     formula = y ~ s(x, bs = "cs"), 
#     se = TRUE
#   ) +
#   
#   # Adjust color scale for smoothed lines
#   scale_color_viridis_d(name = "Sample Size (Discrete)") +
#   
#   # Apply minimal theme
#   theme_minimal()
# 
# # Print the plot
# combinedPlot_lqr
# 
# 
# 
# ### Test Size ####
# 
# pipThreshold_fn <- function(CV, sampleSize) {
#   exp(4.7422928 + 0.6759276 * log2(CV) - 1.4351169 * log10 (sampleSize)) / 
#     (1 + exp(4.7422928 + 0.6759276 * log2(CV) - 1.4351169 * log10 (sampleSize)))
# }
# 
# pipThreshold_fn(CV = 0.02352941, sampleSize = 2934)
# 
# 
# mleResultDf %>% 
#   filter(sampleSize == 2000) %>% 
#   # Some floating point error makes us filter to a range
#   # filter(CV < 0.13, CV > 0.12) %>% 
#   filter(CV == 4) %>% 
#   summarise(
#     test_size_old = sum(PIP < 0.5),
#     test_size_new = sum(
#       PIP < pipThreshold_fn(CV = unique(CV), sampleSize = unique(sampleSize))
#     )
#   )
# 
# mleResultDf %>% 
#   pull(CV) %>% 
#   unique() %>% 
#   sort()
