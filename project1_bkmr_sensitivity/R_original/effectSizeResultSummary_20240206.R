# Summarise the Effect Size Results (Test Size)
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2024-01-06

# Load required libraries
library(tidyverse)

# Results of BKMR
effectSizeResult <- read_rds(
  "data/testBKMR_20240206/effectAnalysis_20240206.rds"
)



# Find Test Size

metalGroupEffectSize<- effectSizeResult %>%
  map_dfr(pluck, "simBloodPIP") %>% 
  group_by(variable) %>% 
  summarise(test_size = mean(PIP > 0.5))

metalOverallEffectSize <- effectSizeResult %>%
  map_dfr(pluck, "simBloodPIP") %>%  
  summarise(test_size = mean(PIP > 0.5))  

actualEffectSize <- bind_rows(metalGroupEffectSize, metalOverallEffectSize) %>% 
  mutate(variable = ifelse(is.na(variable), "Overall", variable)) %>% 
  rename(
    "Variable" = "variable",
    "Test Size" = "test_size"
  )

# Modified Test Size

metalModifiedGroupEffectSize<- effectSizeResult %>% 
  map_dfr(pluck, "simBloodPIP") %>% 
  group_by(variable) %>% 
  summarise(test_size = mean(PIP > 0.68))

metalModifiedOverallEffectSize <- effectSizeResult %>% 
  map_dfr(pluck, "simBloodPIP") %>% 
  summarise(test_size = mean(PIP > 0.68)) 

modifiedEffectSize <- bind_rows(
  metalModifiedGroupEffectSize, metalModifiedOverallEffectSize
) %>% 
  mutate(
    variable = ifelse(is.na(variable), "Overall", variable)
  ) %>% 
  rename(
    "Variable" = "variable",
    "Modified Test Size" = "test_size"
  )


