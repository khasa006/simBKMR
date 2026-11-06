# Generate Data Under Null Hypothesis
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2024-02-20

library(tidyverse)

set.seed(231018)
num_samples <- 2934  # Number of samples

# Simulate y values for different effects
y_values_list <- list(
 # order by magnitude = y3, y6, y1, y2, y5, y4, y8, y9, y7
    y1 = rnorm(num_samples, mean = -0.223, sd = 0.10),
    y2 = rnorm(num_samples, mean = 0.94 , sd = 0.10),
    y3 = rnorm(num_samples, mean = -0.08, sd = 0.10),
  
    y4 = rnorm(num_samples, mean = -1.61, sd = 0.10),
    y5 = rnorm(num_samples, mean = -1.27, sd = 0.10),
    y6 = rnorm(num_samples, mean = -0.19, sd = 0.10),
  
    y7 = rnorm(num_samples, mean = -4.25, sd = 0.10),
    y8 = rnorm(num_samples, mean = -1.9, sd = 0.10),
    y9 = rnorm(num_samples, mean = -2.4, sd = 0.10),
    
    y10 = rnorm(num_samples, mean = -0.01, sd = 0.10),
    y11 = rnorm(num_samples, mean = -0.025, sd = 0.10),
    y12 = rnorm(num_samples, mean = -0.05, sd = 0.10),
    
    y13 = rnorm(num_samples, mean = -0.30, sd = 0.10),
    y14 = rnorm(num_samples, mean = -0.50, sd = 0.10),
    y15 = rnorm(num_samples, mean = -0.70, sd = 0.10)
  
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



# Provide output directory for saving modified datasets
#output_dir <- "data/testSizeAnalysisMean_20240220/"

# Use walk2 to save each modified dataset
#walk2(modifiedDatasets, data_file_char, ~ {
#  output_file <- file.path(output_dir, basename(.y))
#  write_csv(.x, output_file)
#})

