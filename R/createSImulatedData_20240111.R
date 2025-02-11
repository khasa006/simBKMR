# Simulate Data from multivariate Gamma for paramter created at 2023-06-05
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2023-06-05
# update: 2023-06-13
# update: 2024-01-11

# load the libraries

library(lcmix)
library(tidyverse)

####### simulate data for 2023-06-03 parameter lits #########

set.seed(230605) # for reproducibility

# read simulated data info
param_list <- read_rds("./data/param_list1_20230605.rds")
# From Dr. Odom: estimate our own parameter lsit via Method of Moments:
# shape = mean^2
# rate = mean
# This will yield a mean of alpha / beta = (mean^2) / mean = mean; and
#   variance of alpha / beta^2 = (mean^2) / mean^2 = 1

cor_list <- read_rds("./data/cor_list1_20240111.rds")

# define the number of samples to simulate
num_samples <- 100


# loop over the number of samples
for (i in 1:num_samples) {
  
  
  # Theoretical mean vector:
  param_list$Female$shape / param_list$Female$rate
  # Theoretical variance vector:
  sqrt(param_list$Female$shape / (param_list$Female$rate)^2)
  # extract the mean, covariance and skewness info for Female from simData_info
  shape_female <- param_list$Female$shape %>%
    unlist() # convert to vector
  
  rate_female <- param_list$Female$rate %>%
    unlist()
  
  # corr_female <- diag(length(param_list$Female$shape)) %>%
  #   as.matrix()
  # corr_female <- diag(13) 
  corr_female <- cor_list$Female
  
  # Simulate 1506 observations from a multivariate gamma distribution for Female
  female_data <- 
    rmvgamma(
      n = 1506, shape = shape_female, rate = rate_female, corr = corr_female
    ) %>% 
    data.frame()
  
  names(female_data) <- param_list$Female$Variable
  
  # extract the mean, covariance and skewness info for Male from simData_info
  shape_male <- param_list$Male$shape %>%
    unlist() # convert to vector
  
  rate_male <- param_list$Male$rate %>%
    unlist()
  
  # corr_male <- diag(length(param_list$Male$shape)) %>%
  #   as.matrix()
  corr_male <- cor_list$Male
  # corr_male <- diag(13)
  
  # Simulate 1428 observations from a multivariate gamma distribution for Male
  male_data <- 
    rmvgamma(
      n = 1428, shape = shape_male, rate = rate_male, corr = corr_male
    ) %>% 
    data.frame()
  
  names(male_data) <- param_list$Male$Variable
  
  # combine male and female dataset to get final simulated data
  simulated_data <- bind_rows(female_data, male_data) %>% 
    mutate(
      GENDER = c(rep("Female", 1506), rep("Male", 1428)), .before = 1
    )
  
  # add a new column to identify the sample ID
  simulated_data$SampleID <- i
  
  # save simulated data to a file
  #  file_name <- paste0(
  #    "./data/sim_data_20240111/simData_",
  #    str_pad(i, width = 3, side = "left", pad = "0"),
  #    ".csv"
  #  )
  # write_csv(
  #   simulated_data,
  #   file = file_name
  # )
}



