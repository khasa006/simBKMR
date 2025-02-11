# Generate Data Under Null Hypothesis
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2024-02-25

library(tidyverse)

set.seed(231018)
num_samples <- 2934  # Number of samples

# Simulate y values for different effects
y_values_list <- list(

  y1 = rnorm(num_samples, mean = -1.0, sd = 0.10),
  y2 = rnorm(num_samples, mean = -1.0 , sd = 0.25),
  y3 = rnorm(num_samples, mean = -1.0, sd = 0.50),
  
  y4 = rnorm(num_samples, mean = -1.0, sd = 2/3),
  y5 = rnorm(num_samples, mean = -1.0, sd = 1.0),
  y6 = rnorm(num_samples, mean = -1.0, sd = 1.5),
  
  y7 = rnorm(num_samples, mean = -1.0, sd = 2.0),
  y8 = rnorm(num_samples, mean = -1.0, sd = 3.5),
  y9 = rnorm(num_samples, mean = -1.0, sd = 5.0),
  
  y10 = rnorm(num_samples, mean = -1.0, sd = 7.5),
  y11 = rnorm(num_samples, mean = -1.0, sd = 10.0),
  y12 = rnorm(num_samples, mean = -1.0, sd = 15.0)
  
)

# Provide dataset path
data_file_char <- list.files("/home/khasan/BKMR/data/sim_data_20240111/", pattern = ".csv", full.names = TRUE)

# Use map to read and modify each dataset
modifiedDatasets <- map(data_file_char, ~ {
  dataset <- read_csv(.x)
  y_values <- y_values_list
  
  dataset %>%
    bind_cols(y_values)
})



# # Provide output directory for saving modified datasets
# output_dir <- "data/diagTestSizeAnalysisVar_20240220/"
# 
# # Use walk2 to save each modified dataset
# walk2(modifiedDatasets, data_file_char, ~ {
#   output_file <- file.path(output_dir, basename(.y))
#   write_csv(.x, output_file)
# })


