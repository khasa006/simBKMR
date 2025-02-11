# Simulation Study: Data Preparation for Simulation Study
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# update: 2024-01-11

library(tidyverse)


#load the dataset

cognitiveSurveyData_df <- read_csv("./data/cognitiveSurveyData_df.csv")


# log10 transformation of metals
cognitiveLogData <- 
  cognitiveSurveyData_df %>% 
  dplyr::select(
    GENDER, BLEAD, BCADMIUM, BMERCURY, BSELEUM,BMANGE, UBARIUM, 
    UCADMIUM, UCOBALT, UMANGE, ULEAD, UTIN, USTOUM, UURANIUM
  ) %>% 
  mutate_at(
    vars(
      BLEAD, BCADMIUM, BMERCURY, BSELEUM,BMANGE, UBARIUM, UCADMIUM, UCOBALT,
      UMANGE, ULEAD, UTIN, USTOUM, UURANIUM
    ), ~ log10(. + 1) %>% as.vector
  ) %>% 
  mutate_at(
    vars(
      BLEAD, BCADMIUM, BMERCURY, BSELEUM,BMANGE, UBARIUM, UCADMIUM, UCOBALT,
      UMANGE, ULEAD, UTIN, USTOUM, UURANIUM
    ),  ~ . / sd(., na.rm = TRUE)
  )

# write_csv(cognitiveLogData, "data/cognitiveLogData_20240111.csv")

# Split the data based on GENDER (Sex)

splitData <- split(
  x = cognitiveLogData,
  f = cognitiveLogData$GENDER
)

# Calculate parameters for each variable by gender

# From Dr. Odom: estimate our own parameter lsit via Method of Moments:
# shape = mean^2
# rate = mean
# This will yield a mean of alpha / beta = (mean^2) / mean = mean; and
#   variance of alpha / beta^2 = (mean^2) / mean^2 = 1

# Gamma distribution parameter estimation function
gammaDist <- function(x) {
  params <- summarise(
    data.frame(x = x),
    mu = mean(as.numeric(x), na.rm = TRUE),
    sigma = sd(as.numeric(x), na.rm = TRUE)
  )
  
  shape <- (params$mu)^2 
  rate <- params$mu 
  
  result <- data.frame(shape = shape, rate = rate)
  return(result)
}

# Calculate parameters for each variable by gender
param_list <- map(
  splitData,
  ~ select(.x, -GENDER) %>%
    map_df(gammaDist, .id = "Variable")
    
)

cor_list <- map(
  splitData,
  ~ select(.x, -GENDER) %>%
    as.matrix() %>% 
    cor(
      use = "pairwise.complete.obs",
      method = "spearman"
    )
)

# Save the list using write_rds()
# write_rds(param_list, "./data/param_list1_20230605.rds")
# 
# write_rds(cor_list, "./data/cor_list1_20240111.rds")

