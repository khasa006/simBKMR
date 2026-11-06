# Check Simulated Data by Gender created at 2023-06-05
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2023-06-05
# update: 2023-06-13
# update: 2024-01-11

# Load the packages
library(patchwork)
library(corrplot)
library(moments)
library(tidyverse)


# Create a function to calculate parameters of the variables for female
sim_param_female <- function(dataset){
  # a data set for one gender
  df <- read_csv(dataset)
  
  x <- df %>% 
    filter(GENDER == "Female") %>% 
    select(-c(GENDER, SampleID))
  
  # colmeans
  mu <- colMeans(x, na.rm = TRUE)
  
  # col std devs
  sigma <- map_dbl(x, .f = ~{sd(.x, na.rm = TRUE)})
  
  # skewness
  skew <- map_dbl(x, .f = ~{e1071::skewness(.x, na.rm = TRUE)})
  
  # correlation matrix
  corr_mat <- cor(x, use = "pairwise.complete.obs")
  
  list(
    means = mu,
    sds = sigma,
    skewness = skew,
    corr = corr_mat
  )
}

# Create a function to calculate parameters of the variables for male
sim_param_male <- function(dataset){
  # a data set for one gender
  df <- read_csv(dataset)
  
  x <- df %>% 
    filter(GENDER == "Male") %>% 
    select(-c(GENDER, SampleID))
  
  # colmeans
  mu <- colMeans(x, na.rm = TRUE)
  
  # col std devs
  sigma <- map_dbl(x, .f = ~{sd(.x, na.rm = TRUE)})
  
  # skewness
  skew <- map_dbl(x, .f = ~{e1071::skewness(.x, na.rm = TRUE)})
  
  # correlation matrix
  corr_mat <- cor(x, use = "pairwise.complete.obs")
  
  list(
    means = mu,
    sds = sigma,
    skewness = skew,
    corr = corr_mat
  )
}

#### Calculate Parameters of Original Data #######

cognitiveLogData_20240111 <- read_csv("./data/cognitiveLogData_20240111.csv")

# mean
mean_by_gender <- cognitiveLogData_20240111 %>%
  group_by(GENDER) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE)
  )

original_mean_female <- mean_by_gender %>% 
  filter(GENDER == "Female")

original_mean_male <- mean_by_gender %>% 
  filter(GENDER == "Male")

# sd
sd_by_gender <- cognitiveLogData_20240111 %>%
  group_by(GENDER) %>%
  summarise(
    across(where(is.numeric), sd, na.rm = TRUE)
  )

original_sd_female <- sd_by_gender %>% 
  filter(GENDER == "Female")

original_sd_male <- sd_by_gender %>% 
  filter(GENDER == "Male")


# skewness 
skewness_by_gender <- cognitiveLogData_20240111 %>%
  group_by(GENDER) %>%
  summarise(
    across(where(is.numeric), skewness, na.rm = TRUE)
  )

original_skewness_female <- skewness_by_gender %>% 
  filter(GENDER == "Female")

original_skewness_male <- skewness_by_gender %>% 
  filter(GENDER == "Male")


####### Check the Simulated Data #############

# Provide dataset path
data_file_char <- list.files("data/sim_data_20240111/", pattern = ".csv")
data_path_char <- paste0("data/sim_data_20240111/", data_file_char)

# List to store means, standard deviations, skewness, and correlations for each dataset
means_list <- list()
sds_list <- list()
skewness_list <- list()
corr_list <- list()

##### For female

# Iterate over the 100 datasets
for (i in data_path_char) {
  # Call the sim_param_female function with the dataset
  result <- sim_param_female(i)
  
  # Store the means, standard deviations, skewness, and correlations in the respective lists
  means_list[[i]] <- result$means
  sds_list[[i]] <- result$sds
  skewness_list[[i]] <- result$skewness
  corr_list[[i]] <- result$corr
}

# Combine the means, standard deviations, skewness, and correlations into separate data frames
means_female <- bind_rows(means_list, .id = "Dataset") %>%
  filter(Dataset != "original") %>%
  bind_rows(
    original_mean_female %>% 
      select(-GENDER), 
    .id = "Dataset"
  )

sds_female <- bind_rows(sds_list, .id = "Dataset") %>%
  filter(Dataset != "original") %>%
  bind_rows(
    original_sd_female %>% 
      select(-GENDER), 
    .id = "Dataset"
  )

skewness_female <- bind_rows(skewness_list, .id = "Dataset") %>%
  filter(Dataset != "original") %>%
  bind_rows(
    original_mean_female %>% 
      select(-GENDER), 
    .id = "Dataset"
  )

corr_female <- corr_list[[1]]


##### For male

# Iterate over the ten datasets
for (i in data_path_char) {
  # Call the sim_param_female function with the dataset
  result <- sim_param_male(i)
  
  # Store the means, standard deviations, skewness, and correlations in the respective lists
  means_list[[i]] <- result$means
  sds_list[[i]] <- result$sds
  skewness_list[[i]] <- result$skewness
  corr_list[[i]] <- result$corr
}

# Combine the means, standard deviations, skewness, and correlations into separate data frames
means_male <- bind_rows(means_list, .id = "Dataset") %>%
  filter(Dataset != "original") %>%
  bind_rows(
    original_mean_male %>% 
      select(-GENDER), 
    .id = "Dataset"
  )

sds_male <- bind_rows(sds_list, .id = "Dataset") %>%
  filter(Dataset != "original") %>%
  bind_rows(
    original_sd_male %>% 
      select(-GENDER), 
    .id = "Dataset"
  )

skewness_male <- bind_rows(skewness_list, .id = "Dataset") %>%
  filter(Dataset != "original") %>%
  bind_rows(
    original_mean_male %>% 
      select(-GENDER), 
    .id = "Dataset"
  )

corr_male <- corr_list[[1]]



