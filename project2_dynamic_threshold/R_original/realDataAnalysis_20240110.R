# BKMR analysis on real NHANES data
# Kazi Tanvir Hasan & Dr. Gabriel Odom
# 2025-01-10


# Load required libraries
library(bkmr) # For Bayesian kernel machine regression
library(tidyverse)

# Set a random seed for reproducibility
set.seed(2025)

# Read the survey data
cognitiveSurveyData_df <- read_csv("./data/cognitiveSurveyData_df.csv")

# Select relevant columns for analysis
analysisData_df <- cognitiveSurveyData_df %>% 
  select(
    UBARIUM, UCADMIUM, UCOBALT, UMANGE, ULEAD, UTIN, USTOUM, UURANIUM, CSCORE
  ) %>% 
  mutate_at(
    vars(UBARIUM, UCADMIUM, UCOBALT, UMANGE, ULEAD, UTIN, USTOUM, UURANIUM),
    ~ log10(. + 1) %>% as.vector
  ) %>%
  mutate_at(
    vars(UBARIUM, UCADMIUM, UCOBALT, UMANGE, ULEAD, UTIN, USTOUM, UURANIUM),
    ~ . / sd(., na.rm = TRUE)
  ) %>%
  na.omit()

# Extract the response variable (CSCORE)
cscore <- analysisData_df[["CSCORE"]] 

# Convert exposure variables to a matrix for modeling
expos <- data.matrix(
  analysisData_df[c("UBARIUM", "UCADMIUM", "UCOBALT", "UMANGE", "ULEAD", "UTIN", "USTOUM", "UURANIUM")]
)

# covar <- data.matrix(
#   analysisData_df[c("GENDER", "AGE", "MARTLS", "PRTIO", "RACE", "EDUCL", "BPS", 
#                     "BCS", "DIABETES", "BMI", "ALQS", "SMQS", "STROKE", "HATTACK")]
# )


# Generate knot points using cover design for Bayesian modeling
knots50 <- fields::cover.design(expos, nd = 50)$design

# Fit the Bayesian kernel machine regression model
modelFit <- kmbayes(
  y = cscore,       # Response variable
  Z = expos,        # Exposure matrix
  X = NULL,         # No additional NULLiates
  iter = 25000,     # Number of MCMC iterations
  family = "gaussian", # Gaussian response
  verbose = TRUE,  # Suppress output
  varsel = TRUE,    # Enable variable selection
  knots = knots50   # Knots for spline fitting
)

# Extract posterior inclusion probabilities (PIPs) and sort them
pipFit <- ExtractPIPs(modelFit) %>% 
  arrange(desc(PIP))

pipFit

# Generate univariate predictor-response relationships
predRespUnivar <- PredictorResponseUnivar(fit = modelFit)

predRespUnivar <- predRespUnivar %>%
  mutate(variable = recode(
    variable,
    "UBARIUM" = "Barium",
    "UCADMIUM" = "Cadmium",
    "UCOBALT" = "Cobalt",
    "UMANGE" = "Manganese",
    "ULEAD" = "Lead",
    "UTIN" = "Tin",
    "USTOUM" = "Strontium",
    "UURANIUM" = "Uranium")
  )


# Plot univariate predictor-response functions
predRespUnivarPlot <- ggplot(
  predRespUnivar, 
  aes(
    z, 
    est, 
    ymin = est - 1.96 * se, 
    ymax = est + 1.96 * se
  )
) + 
  geom_smooth(stat = "identity") +  # Add smooth lines with confidence intervals
  facet_wrap(~ variable) +          # Create separate plots for each variable
  ylab("h(z)")                      # Label the y-axis


predRespUnivarPlot

# Save the plot as a high-resolution TIFF file
# ggsave(
#   "figure3_20250311.tiff",
#   plot = predRespUnivarPlot,
#   width = 8,
#   height = 6,
#   dpi = 300,
#   compression = "lzw",
#   bg = "white"
# )


# Compute overall risk summaries
risksOverall <- OverallRiskSummaries(
  fit = modelFit, 
  y = cscore, 
  Z = expos, 
  X = NULL, 
  qs = seq(0.25, 0.75, by = 0.05), # Quantiles for risk summaries
  q.fixed = 0.5,                  # Reference quantile
  method = "exact"
)

# Plot overall risk summaries
risksOverallPlot <- ggplot(
  risksOverall, 
  aes(
    quantile, 
    est, 
    ymin = est - 1.96 * sd, 
    ymax = est + 1.96 * sd
  )
) + 
  geom_pointrange()  # Add point ranges for risk estimates

risksOverallPlot

# ggsave(
#   "figure4_20250311.tiff",
#   plot = risksOverallPlot,
#   width = 8,
#   height = 6,
#   dpi = 300,
#   compression = "lzw",
#   bg = "white"
# )


# Compute single-variable risk summaries
risksSingvar <- SingVarRiskSummaries(
  fit = modelFit,
  y = cscore,
  Z = expos,
  X = NULL,
  qs.diff = c(0.25, 0.75),          # Quantile difference for risk estimation
  q.fixed = c(0.25, 0.50, 0.75),   # Reference quantiles
  method = "exact"
)

# Plot single-variable risk summaries
ggplot(
  risksSingvar, 
  aes(
    variable, 
    est, 
    ymin = est - 1.96 * sd, 
    ymax = est + 1.96 * sd, 
    col = as.factor(q.fixed)       # Color by reference quantile
  )
) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + # Adjust position for clarity
  coord_flip() # Flip coordinates for better readability

# Compute interaction risk summaries
risksInt <- SingVarIntSummaries(
  fit = modelFit,
  y = cscore,
  Z = expos,
  X = NULL,
  qs.diff = c(0.25, 0.75),         # Quantile differences for interactions
  qs.fixed = c(0.25, 0.75),       # Fixed quantiles for interactions
  method = "exact"
)

# Plot interaction risk summaries
ggplot(
  risksInt, 
  aes(
    variable, 
    est, 
    ymin = est - 1.96 * sd, 
    ymax = est + 1.96 * sd
  )
) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + # Adjust position
  geom_hline(yintercept = 0, lty = 2, col = "brown") +       # Add a reference line
  coord_flip()  # Flip coordinates for readability
