# Calculate mle of PIP's using beta distribution 
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2024-09-20
# Updated: 2024-10-10


library(fitdistrplus)    # For fitting distributions
library(moments)         # For calculating skewness
library(tidyverse)

set.seed(20240920)
# Define a function to process effect size results
processEffectSize <- function(filePath, designName, meanValue, sdValue) {
  # Read effect size results from file
  effectSizeResult <- read_rds(filePath)
  
  # Calculate effect size for each variable
  metalGroupEffectSize <- effectSizeResult %>%
    map_dfr(pluck, "simBloodPIP") 
  
  # Combine group effect sizes and add additional information
  actualEffectSize <- metalGroupEffectSize %>%
    # Handle missing variable names
    mutate(variable = ifelse(is.na(variable), "Overall", variable)) %>%
    # Add design, mean, and sd information
    mutate(
      design = designName,
      mean = meanValue,
      sd = sdValue
    ) %>%
    # Select relevant columns
    select(variable, design, PIP, mean, sd)
  
  return(actualEffectSize)
}




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

# Process each effect size result for unstructured covariance data
unstrucEffectSizeMean <- map2_dfr(
  unstrucFilePaths,
  meanValues,
  ~ processEffectSize(.x, paste0("unstruc_test_size", .y), mean = .y, sd = 0.10)
)

# Plot for unstructured covariance data
ggplot(data = unstrucEffectSizeMean %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = PIP
  ) +
  # Scatter Plot
  geom_jitter() +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  labs(
    x = "Coefficient of Variation",
    y = "PIP",
    title = "Comparing the Relationship between CV and Test Size.",
    subtitle = "Unstructured Covariance Data with Different Means"
  )

ggplot(data = unstrucEffectSizeMean %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = PIP,
    group = abs(sd/mean)
  ) +
  # Box Plot
  geom_boxplot(position = "dodge") +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  labs(
    x = "Coefficient of Variation",
    y = "Test Size (Fixed Sds)",
    title = "Comparing the Relationship between CV and Test Size.",
    subtitle = "Unstructured Covariance Data with Different Means"
  )


# Define file paths and corresponding SD values for unstructured covariance data
unstrucFilePathsSd <- c(
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
  "data/unstrucTestSizeAnalysisVar_20240220/unstructestSizeVarY12.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY13.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY14.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY15.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY16.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY17.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY18.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY19.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY20.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY21.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY22.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY23.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY24.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY25.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY26.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY27.rds",
  "data/unstrucTestSizeAnalysisVar_20240923/unstructestSizeVarY28.rds"
)

# Define SD values
sdValues <- c(
  0.10, 0.25, 0.50, 2/3, 1.0, 1.5, 2.0, 3.5, 5.0, 7.5, 10.0, 15.0, 0.02818383, 
  0.03162278, 0.03548134, 0.04786301, 0.07079458, 0.08912509, 0.1258925,
  0.1584893, 0.1778279, 0.2238721, 0.2818383, 0.3162278, 0.3981072, 0.6025596, 
  0.7585776, 0.8709636
)

# Process each effect size result for unstructured covariance data
unstrucEffectSizeVar <- map2_dfr(
  unstrucFilePathsSd,
  sdValues,
  ~ processEffectSize(.x, paste0("unstruc_test_size", .y), mean = -1.0, sd = .y)
)


# Plot for unstructured covariance data
ggplot(data = unstrucEffectSizeVar %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = PIP
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
    y = PIP,
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


testData <- unstrucEffectSizeMean %>% bind_rows(unstrucEffectSizeVar)


ggplot(data = testData %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = PIP
  ) +
  # Scatter Plot
  geom_jitter(alpha = 0.2) +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  labs(
    x = "Coefficient of Variation",
    y = "PIPs",
    title = "Comparing the Relationship between CV and PIP"
  )



ggplot(data = testData %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = PIP,
    group = abs(sd/mean)
  ) +
  # Box Plot
  geom_boxplot(position = "dodge") +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  labs(
    x = "Coefficient of Variation",
    y = "PIPs",
    title = "Comparing the Relationship between CV and PIP"
  )




# Combine them into a dataframe for easier access
mleData_df <- testData %>%
  dplyr::filter(variable != "Overall") %>%
  mutate(CV = abs(sd / mean)) %>%
  dplyr::select(variable, CV, PIP) %>% 
  arrange(CV)


# Combine them into a dataframe for easier access
mleData_df <- testData %>%
  dplyr::filter(variable != "Overall") %>%
  mutate(CV = abs(sd / mean)) %>%
  dplyr::select(variable, CV, PIP) %>%
  arrange(CV)

# Step 1: Get the second smallest and second largest values using dplyr
pip_summary <- mleData_df %>%
  summarise(
    second_smallest = sort(unique(PIP))[2],
    second_largest = sort(unique(PIP))[length(unique(PIP)) - 1]
  )

secondSmallest <- pip_summary$second_smallest
secondLargest <- pip_summary$second_largest

# Step 2: Replace min and max using mutate and case_when with different random values
mleData_df <- mleData_df %>%
  mutate(PIP = case_when(
    PIP == min(PIP) ~ runif(n(), 0, secondSmallest),  # Generate random value for each min PIP
    PIP == max(PIP) ~ runif(n(), secondLargest, 1),   # Generate random value for each max PIP
    TRUE ~ PIP                                        # Leave other values unchanged
  ))


# Replace min and max using mutate and case_when with different random values
mleData_df <- mleData_df %>%
  mutate(PIP = case_when(
    PIP == min(PIP) ~ runif(n(), 0, secondSmallest),  # Generate random value for each min PIP
    PIP == max(PIP) ~ runif(n(), secondLargest, 1),   # Generate random value for each max PIP
    TRUE ~ PIP                                        # Leave other values unchanged
  ))

# Now the dataframe has the min and max values replaced
# write_csv(mleData_df, file = "./data/mleData_20240920/mleData2934_df.csv")

mleData_ls <- 
  mleData_df %>%
  group_by(CV) %>%
  split(f = .$CV)


# Function to compute MLE for beta distribution and summary statistics with error handling
compute_mle_summary <- function(data) {
  
  # Try fitting the beta distribution using MLE
  fit <- fitdist(data$PIP, "beta", method = "mle")
  
  # Calculate the mean, standard deviation, and skewness
  mean_pip <- mean(data$PIP)
  sd_pip <- sd(data$PIP)
  skewness_pip <- skewness(data$PIP)
  pip_90 <- quantile(data$PIP,probs =  0.90)
  pip_95 <- quantile(data$PIP,probs =  0.95)
  pip_99 <- quantile(data$PIP,probs =  0.99)
  
  # Extract the shape parameters (alpha and beta)
  alpha <- fit$estimate["shape1"]
  beta <- fit$estimate["shape2"]
  
  # Create a tibble with the summary statistics
  tibble(
    alpha = alpha,
    beta = beta,
    mean_pip = mean_pip,
    sd_pip = sd_pip,
    skewness_pip = skewness_pip,
    pip_90 = pip_90,
    pip_95 = pip_95,
    pip_99 = pip_99
  )
}

# Apply the function to each tibble in the list with map_df
summary_df <- map_df(mleData_ls, compute_mle_summary, .id = "CV") %>% 
  mutate(CV = as.numeric(CV))

# Check the result
summary_df

# write_csv(summary_df, file = "./data/mleData_20240920/mleSummary2934_df.csv")


# Several papers and books discuss the MLE estimation of Beta distribution parameters, particularly the issues arising with values at the boundaries (0 and 1) and numerical optimization methods.
# 
# Key References:
#   Johnson, N. L., Kotz, S., & Balakrishnan, N. (1995). Continuous Univariate Distributions, Volume 2. Wiley.
# 
# This book provides a comprehensive treatment of the Beta distribution, including its properties, methods of estimation, and practical applications.
# Casella, G., & Berger, R. L. (2002). Statistical Inference. Duxbury.
# 
# This is a standard reference for statistical inference, including methods for MLE and the Beta distribution. The book explains the MLE process for various distributions and how numerical optimization techniques are used.
# Gupta, A. K., & Nadarajah, S. (2004). Handbook of Beta Distribution and Its Applications. CRC Press.
# 
# This book specifically focuses on the Beta distribution and its wide range of applications, providing insights into the estimation of parameters via MLE and other methods.
# Minka, T. (2002). Estimating a Dirichlet distribution.
# 
# Link to the paper: Though the paper focuses on the Dirichlet distribution, the Beta distribution is a special case of it. This paper provides a good discussion on estimating the parameters of Beta and Dirichlet distributions using MLE.
# Smithson, M., & Verkuilen, J. (2006). A better lemon squeezer? Maximum-likelihood regression with Beta-distributed dependent variables. Psychological Methods, 11(1), 54.
# 
# This paper discusses how to perform MLE for Beta-distributed variables and addresses challenges that arise in practice, such as boundary issues and appropriate parameter constraints.
# These references will give you a theoretical foundation and practical insights into the Beta distribution and the estimation of its parameters.
# 
# If you want more detailed discussion on numerical methods, Chapter 9 of Numerical Recipes in C (Press et al.) discusses optimization techniques like those used in optim() and can be applied to MLE estimation problems.




# ######### Graphs #########
# summaryLong_df2934 <- summary_df %>% 
#   mutate(beta_log = log10(beta)) %>% 
#   pivot_longer(
#     !CV,
#     names_to = "Statistic",
#     values_to = "Value"
#   )
# 
# # write_csv(summaryLong_df2934, file = "./data/mleData_20240920/summaryLong_df2934.csv")
# 
# ggplot(
#   summaryLong_df2934 %>% 
#     filter(CV > 0.025)
# ) +
#   aes(
#     x = CV,
#     y = Value,
#     group = Statistic
#   ) +
#   scale_x_log10() +
#   geom_point() +
#   stat_smooth(method = "lm") +
#   facet_wrap(~Statistic, scales = "free") 
# 
# 
# # alpha colud be linearly related to log10(CV)
# ggplot(summary_df[-1, ]) +
#   aes(x = CV, y = alpha) +
#   scale_x_log10() +
#   scale_y_log10() +
#   geom_point() +
#   stat_smooth() +
#   stat_smooth(method = "lm", color = "red")
# 
# 
# # beta colud be linearly related to log10(CV)
# ggplot(summary_df) +
#   aes(x = CV, y = beta) +
#   scale_x_log10() +
#   scale_y_log10() +
#   geom_point() +
#   stat_smooth() +
#   stat_smooth(method = "lm", color = "red")
# 
# 
# # mean of pip is related to log10(CV) but it is not linear
# ggplot(summary_df) +
#   aes(x = CV, y = mean_pip) +
#   scale_x_log10() +
#   scale_y_log10() +
#   geom_point() +
#   stat_smooth() +
#   stat_smooth(method = "lm", color = "red")
# 
# # sd of pip linearly related to log10(CV)
# ggplot(summary_df) +
#   aes(x = CV, y = sd_pip) +
#   scale_x_log10() +
#   # scale_y_log10() +
#   geom_point() +
#   stat_smooth() +
#   stat_smooth(method = "lm", color = "red")
# 
# 
# # skewness of pip linearly related to log10(CV)
# ggplot(summary_df[-1, ]) +
#   aes(x = CV, y = skewness_pip + 1) +
#   scale_x_log10() +
#   scale_y_log10() +
#   geom_point() +
#   stat_smooth() +
#   stat_smooth(method = "lm", color = "red")
# 
# 
# 
# ### Next Step ####
# plot(y = 1:41, x = log10(summary_df$CV))
# 
# 
# 
# ### Need to do ######
# # need design points -
# 
# 10^c(-1.55, -1.5, -1.45)
# 
# 10^c(-0.9, -0.8, -0.75, -0.65, -0.55, -0.5, -0.4)
# 
# 10^c(-0.22, -0.12, -0.06)
# 
# 10^c(0.24, 0.38, 0.45)
# 
# allDesignCV_num <- sort(
#   c(
#     log10(summary_df$CV), 
#     c(-1.55, -1.5, -1.45, -1.32, -1.15, -1.05),
#     c(-0.9, -0.8, -0.75, -0.65, -0.55, -0.5, -0.4),
#     c(-0.22, -0.12, -0.06),
#     c(0.24, 0.38, 0.45)
#   )
# )
# 
# plot(x = allDesignCV_num, y = seq_along(allDesignCV_num))
# 
# # 1. Start running more simulation
# 
# 
# 
# ggplot(summary_df) +
#   aes(x = CV, y = pip_90) +
#   scale_x_log10() +
#   # scale_y_log10() +
#   geom_point() +
#   stat_smooth() +
#   stat_smooth(method = "lm", color = "red")
# 
# ggplot(summary_df) +
#   aes(x = CV, y = pip_95) +
#   scale_x_log10() +
#   # scale_y_log10() +
#   geom_point() +
#   stat_smooth() +
#   stat_smooth(method = "lm", color = "red")
# 
# ggplot(summary_df) +
#   aes(x = CV, y = pip_99) +
#   scale_x_log10() +
#   # scale_y_log10() +
#   geom_point() +
#   stat_smooth() +
#   stat_smooth(method = "lm", color = "red")
# 
# 
# 
# ggplot(
#   summaryLong_df2934
# ) +
#   aes(
#     x = CV,
#     y = Value,
#     group = Statistic
#   ) +
#   scale_x_log10() +
#   labs(
#     x = "Coefficient of Variation (logCV)",
#     y = "Value",
#     title = "Comparing the Relationship between CV and PIP Statistics",
#   ) +
#   geom_point() +
#   stat_smooth(method = "lm") +
#   facet_wrap(~Statistic, scales = "free") 
# 
# 
# ggplot(
#   summaryLong_df2934 
#   # %>% 
#   #   filter(CV < 5.1)
# ) +
#   aes(
#     x = CV,
#     y = Value,
#     group = Statistic
#   ) +
#   scale_x_log10() +
#   labs(
#     x = "Coefficient of Variation (logCV)",
#     y = "Value",
#     title = "Comparing the Relationship between CV and PIP Statistics",
#   ) +
#   geom_point() +
#   stat_smooth(method = "lm") +
#   facet_wrap(~Statistic, scales = "free") 
# 
# #################################
#   # Install ggpmisc if not already installed
#   # install.packages("ggpmisc")
#   
#   library(ggplot2)
# library(ggpmisc)
# 
# ggplot(summaryLong_df2934) +
#   aes(
#     x = CV,
#     y = Value,
#     group = Statistic
#   ) +
#   scale_x_log10() +
#   labs(
#     x = "Coefficient of Variation (logCV)",
#     y = "Value",
#     title = "Comparing the Relationship between CV and PIP Statistics"
#   ) +
#   geom_point() +
#   stat_smooth(method = "loess") + # Choose method here (e.g., "lm" for linear)
#   stat_poly_eq(
#     aes(label = after_stat(eq.label)), # Displays the equation
#     formula = y ~ x,                   # Set formula for lm or loess
#     parse = TRUE,
#     label.x = "right",
#     label.y = "top"
#   ) +
#   facet_wrap(~Statistic, scales = "free")
# 
# 
