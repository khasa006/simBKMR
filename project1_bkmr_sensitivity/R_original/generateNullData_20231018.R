# Generate Data Under Null Hypothesis
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2023-10-18

library(tidyverse)

set.seed(231018)
num_samples <- 2934  # Number of samples

# Simulate y values for different effects
y_values_list <- list(
  weak_effect = list(
    y1 = rnorm(num_samples, mean = -0.223, sd = 0.10),
    y2 = rnorm(num_samples, mean = 0.94 , sd = 0.10),
    y3 = rnorm(num_samples, mean = -0.08, sd = 0.10)
  ),
  medium_effect = list(
    y4 = rnorm(num_samples, mean = -1.61, sd = 0.10),
    y5 = rnorm(num_samples, mean = -1.27, sd = 0.10),
    y6 = rnorm(num_samples, mean = -0.19, sd = 0.10)
  ),
  strong_effect = list(
    y7 = rnorm(num_samples, mean = -4.25, sd = 0.10),
    y8 = rnorm(num_samples, mean = -1.9, sd = 0.10),
    y9 = rnorm(num_samples, mean = -2.4, sd = 0.10)
  )
)

# Provide dataset path
data_file_char <- list.files("data/sim_data_20230605/", pattern = ".csv", full.names = TRUE)

# Use map to read and modify each dataset
modified_datasets <- map(data_file_char, ~ {
  dataset <- read_csv(.x)
  y_values <- y_values_list
  
  dataset %>%
    bind_cols(y_values)
})



# Provide output directory for saving modified datasets
output_dir <- "data/generateNullData_20231018/"

# Use walk2 to save each modified dataset
walk2(modified_datasets, data_file_char, ~ {
  output_file <- file.path(output_dir, basename(.y))
  write_csv(.x, output_file)
})


