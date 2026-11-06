# Generate Signal (Reponse Variable)
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# Test 1: 2024-01-12
# Test 2: 2024-01-12
# Test 3: 2024-01-13
# update: 2024-01-14
# Test 4:2024-01-16
# Update: 2024-01-18
# Test 5: 2024-01-20
# Test 6: 2024-01-21
# Test 7: 2024-01-22
# Test 8: 2024-01-23
# Final: 2024-01-27



library(tidyverse)


# Function to generate 12 columns for different signal types based on features
generate_signals <- function(df) {
  # explain why we choose sd = 2
  set.seed(2401)
  noise_num <- rnorm(n = nrow(df), sd = 2)
  
  df %>%
    # Define Signal
    mutate(
      normal_low = -0.03 * BLEAD,
      normal_medium = -0.08 * BLEAD,
      normal_high = -0.12 * BLEAD,
      skewed_low = -0.02 * BMERCURY,
      skewed_medium = -0.10 * BMERCURY,
      skewed_high = -0.15 * BMERCURY,
      interaction_low = -0.01 * BLEAD * BMERCURY,
      interaction_medium = -0.05 * BLEAD * BMERCURY,
      interaction_high = -0.1 * BLEAD * BMERCURY,
      full_factorial_low = 0.5 * (
        - 0.03 * BLEAD - 0.04 * BMERCURY - 0.01 * BLEAD * BMERCURY
      ),
      full_factorial_medium = 0.5 * (
        - 0.07 * BLEAD - 0.06 * BMERCURY - 0.03 * BLEAD * BMERCURY
      ),
      full_factorial_high = 0.5 * (
        - 0.12 * BLEAD - 0.08 * BMERCURY - 0.05 * BLEAD * BMERCURY
      )
    ) %>% 
    # Add Random Error
    mutate(
      across(normal_low:full_factorial_high, ~ .x + noise_num)
    )
}

# Provide dataset path
data_file_char <- list.files("data/sim_data_20240111/", pattern = ".csv", full.names = TRUE)

# Use map to read, modify each dataset, and generate signal columns
modified_datasets <- map(data_file_char, ~ {
  dataset <- read_csv(.x)
  # Assuming BLEAD and BMERCURY columns exist in the dataset
  generate_signals(dataset)
})


# # Create output directory if it doesn't exist
# if (!dir.exists("data/generateSignal_20231221")) {
#   dir.create("data/generateSignal_20231221")
# }
# 
# # Save each modified dataset using purrr
# walk2(modified_datasets, seq_along(modified_datasets), ~ {
#   output_file <- file.path("data/generateSignal_20231221", sprintf("simulated_datasets_%03d.csv", .y))
#   write_csv(.x, output_file)
# })



