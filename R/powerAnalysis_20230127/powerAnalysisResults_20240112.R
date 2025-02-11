# Summarise the BKMR (Power Analysis, PIPs, Skewness) Results
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2024-01-12
# update: 2024-01-18
# update: 2024-01-20
# update: 2024-01-22
# update: 2024-01-23
# update: 2024-01-25




# Load required libraries
library(tidyverse)

####### Normal Low ############
normalLow <- read_rds(
  "data/powerBKMR_20240112/normalLow3_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedNormalLowPip <- normalLow %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerNormalLow <- combinedNormalLowPip %>% 
  group_by(variable) %>% 
  summarise(testSize = mean(PIP > 0.5))

groupPowerNormalLow

####### Normal Medium ############
normalMedium <- read_rds(
  "data/powerBKMR_20240112/normalMedium3_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedNormalMediumPip <- normalMedium %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerNormalMedium <- combinedNormalMediumPip %>% 
  group_by(variable) %>% 
  summarise(testSize = mean(PIP > 0.5))

groupPowerNormalMedium

####### Normal High ############
normalHigh <- read_rds(
  "data/powerBKMR_20240112/normalHigh3_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedNormalHighPip <- normalHigh %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerNormalHigh <- combinedNormalHighPip %>% 
  group_by(variable) %>% 
  summarise(testSize = mean(PIP > 0.5))

groupPowerNormalHigh

####### Skewed Low ############
skewedLow <- read_rds(
  "data/powerBKMR_20240112/skewedLow_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedSkewedLowPip <- skewedLow %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerSkewedLow <- combinedSkewedLowPip %>% 
  group_by(variable) %>% 
  summarise(testSize = mean(PIP > 0.5))

groupPowerSkewedLow

####### Skewed Medium ############
skewedMedium <- read_rds(
  "data/powerBKMR_20240112/skewedMedium_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedSkewedMediumPip <- skewedMedium %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerSkewedMedium <- combinedSkewedMediumPip %>% 
  group_by(variable) %>% 
  summarise(testSize = mean(PIP > 0.5))

groupPowerSkewedMedium

####### Skewed High ############
skewedHigh <- read_rds(
  "data/powerBKMR_20240112/skewedHigh_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedSkewedHighPip <- skewedHigh %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerSkewedHigh <- combinedSkewedHighPip %>% 
  group_by(variable) %>% 
  summarise(testSize = mean(PIP > 0.5))

groupPowerSkewedHigh

####### Interaction Low ############
interactionLow <- read_rds(
  "data/powerBKMR_20240112/interactionLow_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedInteractionLowPip <- interactionLow %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerInteractionLow <- combinedInteractionLowPip %>%
  group_by(variable) %>%
  summarise(testSize = mean(PIP > 0.5))

groupPowerInteractionLow

powerInteractionLow <- combinedInteractionLowPip %>%
  summarise(testSize = mean(PIP > 0.5))

powerInteractionLow

####### Interaction Medium ############
interactionMedium <- read_rds(
  "data/powerBKMR_20240112/interactionMedium_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedInteractionMediumPip <- interactionMedium %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerInteractionMedium <- combinedInteractionMediumPip %>%
  group_by(variable) %>%
  summarise(testSize = mean(PIP > 0.5))

groupPowerInteractionMedium

powerInteractionMedium <- combinedInteractionMediumPip %>%
  summarise(testSize = mean(PIP > 0.5))

powerInteractionMedium

####### Interaction High ############
interactionHigh <- read_rds(
  "data/powerBKMR_20240112/interactionHigh_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedInteractionHighPip <- interactionHigh %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerInteractionHigh <- combinedInteractionHighPip %>%
  group_by(variable) %>%
  summarise(testSize = mean(PIP > 0.5))

groupPowerInteractionHigh

powerInteractionHigh <- combinedInteractionHighPip %>%
  summarise(testSize = mean(PIP > 0.5))

powerInteractionHigh

####### Full Factorial Low ############
fullFactorialLow <- read_rds(
  "data/powerBKMR_20240112/fullFactorialLow2_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedFullFactorialLowPip <- fullFactorialLow %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerFullFactorialLow <- combinedFullFactorialLowPip %>%
  group_by(variable) %>%
  summarise(testSize = mean(PIP > 0.5))

groupPowerFullFactorialLow

powerFullFactorialLow <- combinedFullFactorialLowPip %>%
  summarise(testSize = mean(PIP > 0.5))

powerFullFactorialLow

####### Full Factorial Medium ############
fullFactorialMedium <- read_rds(
  "data/powerBKMR_20240112/fullFactorialMedium3_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedFullFactorialMediumPip <- fullFactorialMedium %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerFullFactorialMedium <- combinedFullFactorialMediumPip %>%
  group_by(variable) %>%
  summarise(testSize = mean(PIP > 0.5))

groupPowerFullFactorialMedium

powerFullFactorialMedium <- combinedFullFactorialMediumPip %>%
  summarise(testSize = mean(PIP > 0.5))

powerFullFactorialMedium

####### Full Factorial High ############
fullFactorialHigh <- read_rds(
  "data/powerBKMR_20240112/fullFactorialHigh3_20240112.rds"
)

# Extracting simBloodPIP data from the list and combining into a single data frame
combinedFullFactorialHighPip <- fullFactorialHigh %>%
  map_dfr(pluck, "simBloodPIP")

# Find Test Size
groupPowerFullFactorialHigh <- combinedFullFactorialHighPip %>%
  group_by(variable) %>%
  summarise(testSize = mean(PIP > 0.5))

groupPowerFullFactorialHigh

powerFullFactorialHigh <- combinedFullFactorialHighPip %>%
  summarise(testSize = mean(PIP > 0.5))

powerFullFactorialHigh

######## View the Combined Test Size ########

# Combine all the groupPower_ results
combinedGroupPower <- left_join(
  groupPowerNormalLow, groupPowerNormalMedium, by = "variable"
) %>%
  left_join(
    ., groupPowerNormalHigh, by = "variable"
  ) %>%
  left_join(
    ., groupPowerSkewedLow, by = "variable"
  ) %>%
  left_join(
    ., groupPowerSkewedMedium, by = "variable"
  ) %>%
  left_join(
    ., groupPowerSkewedHigh, by = "variable"
  ) %>%
  left_join(
    ., groupPowerInteractionLow, by = "variable"
  ) %>%
  left_join(
    ., groupPowerInteractionMedium, by = "variable"
  ) %>%
  left_join(
    ., groupPowerInteractionHigh, by = "variable"
  ) %>%
  left_join(
    ., groupPowerFullFactorialLow, by = "variable"
  ) %>%
  left_join(
    ., groupPowerFullFactorialMedium, by = "variable"
  ) %>%
  left_join(
    ., groupPowerFullFactorialHigh, by = "variable"
  ) %>%
  rename(
    "normalLow(-0.03)" = testSize.x,
    "normalMedium(-.08)" = testSize.y,
    "normalHigh(-0.12)" = testSize.x.x,
    "skewedLow(-0.02)" = testSize.y.y,
    "skewedMedium(-0.05)" = testSize.x.x.x,
    "skewedHigh(-0.10)" = testSize.y.y.y,
    "interactionLow(-0.01)" = testSize.x.x.x.x,
    "interactionMedium(-0.05)" = testSize.y.y.y.y,
    "interactionHigh(-0.10)" = testSize.x.x.x.x.x,
    "fullFactorialLow" = testSize.y.y.y.y.y,
    "fullFactorialMedium" = testSize.x.x.x.x.x.x,
    "fullFactorialHigh" = testSize.y.y.y.y.y.y
  ) 

####### Create Table for PIP #########
res_ls <- list(
  normalLow = combinedNormalLowPip,
  normalMedium = combinedNormalMediumPip,
  normalHigh = combinedNormalHighPip,
  skewedLow = combinedSkewedLowPip,
  skewedMedium = combinedSkewedMediumPip,
  skewedHigh = combinedSkewedHighPip,
  interactionLow = combinedInteractionLowPip,
  interactionMedium = combinedInteractionMediumPip,
  interactionHigh = combinedInteractionHighPip,
  fullFactorialLow = combinedFullFactorialLowPip,
  fullFactorialMedium = combinedFullFactorialMediumPip,
  fullFactorialHigh = combinedFullFactorialHighPip
)

# This bind_cols() solution assumes the metals are in the same order.
# combinedPIP_df <- bind_cols(
#   combinedNormalLowPip %>% select(variable, normalLow = PIP),
#   combinedNormalMediumPip %>% select(normalMedium = PIP),
#   combinedNormalHighPip %>% select(normalHigh = PIP),
#   combinedSkewedLowPip %>% select(skewedLow = PIP),
#   combinedSkewedMediumPip %>% select(skewedMedium = PIP),
#   combinedSkewedHighPip %>% select(skewedHigh = PIP),
#   combinedInteractionLowPip %>% select(interactionLow = PIP),
#   combinedInteractionMediumPip %>% select(interactionMedium = PIP),
#   combinedInteractionHighPip %>% select(interactionHigh = PIP),
#   combinedFullFactorialLowPip %>% select(fullFactorialLow = PIP),
#   combinedFullFactorialMediumPip %>% select(fullFactorialMedium = PIP),
#   combinedFullFactorialHighPip %>% select(fullFactorialHigh = PIP)
# ) %>% 

combinePIP_df <-
  res_ls %>%
  map(.f = ~{
    .x %>% 
      mutate(simDataIndex = rep(1:10, each = 5))
  }) %>% 
  bind_rows(.id = "design") %>%
  # mutate(simDataIndex = rep(1:10, each = 50)) %>%
  pivot_wider(
    names_from = design,
    values_from = PIP
  ) %>% 
  arrange(variable, simDataIndex)
  
# res_df <- 
#   # Reduce(left_join, res_ls)
# 
#   group_by(variable) %>%
#   mutate(simDataIndex = row_number()) %>%
#   ungroup() %>%
#   arrange(variable, simDataIndex) %>%
#   # Move the "index" column after "variable"
#   select(
#     variable, simDataIndex, everything()
#   )
          
######## Add Skweness to PIP Table ########
source("r_code/generateSignal_20240112.R")

set.seed(2312)
selectedDatasets <- sample(modified_datasets, 10, replace = FALSE)



# Function to calculate skewness for each variable in a dataset
calculateSkewness <- function(dataset) {
  dataset %>%
    select_if(is.numeric) %>%
    summarise_all(e1071::skewness)
}

# Apply the function to each dataset in selectedDatasets
skewnessMetals <- map(selectedDatasets, ~ calculateSkewness(.x[, 2:6]))

# Create a data frame
skewness_df <- bind_rows(
  map(seq_along(skewnessMetals), function(i) {
    # Create a data frame for each dataset
    data.frame(
      simDataIndex = i, # Index of the dataset
      variable = names(selectedDatasets[[i]])[2:6], # Variable names for columns 2:6 
      skewness = as.vector(t(skewnessMetals[[i]])) # Skewness values
    )
  })
) %>% arrange(variable)

# write_csv(skewness_df, file = "data/metricPlotDataset_20240125/skewness_df_20240125.csv")

# Combine the PIP and Skewness dataframe into a single dataframe

pipSkewness_df <- left_join(
  x = combinePIP_df,
  y = skewness_df,
  by = c("variable", "simDataIndex")
)


# write_csv(pipSkewness_df, file = "data/metricPlotDataset_20240125/pipSkewness_df_20240125.csv")

