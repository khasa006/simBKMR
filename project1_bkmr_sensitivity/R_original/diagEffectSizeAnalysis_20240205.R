# Generate Response, Perform Effect Size Analysis for Diagonal Covarience Matrix
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2024-02-05

# load required libraries
suppressPackageStartupMessages(
  {
    library(bkmr)
    library(doParallel)
    library(tidyverse)
  }
)

# Set seed for reproducibility
set.seed(2312)


###### Generate Response ###########
numSamples <- 2934  # Number of samples

# Simulate response values for different effects
response <- rnorm(n = numSamples, mean = 0, sd = 2) 

# Provide dataset path
data_file_char <- list.files(
  "data/sim_data_20230605/", pattern = ".csv", full.names = TRUE
)

# Use map to read, modify each dataset, and generate response columns
modifiedDatasets <- map(data_file_char, ~ {
  dataset <- read_csv(.x)
  
  # Add the 'cscoreEffect' column to each dataset
  dataset <- bind_cols(dataset, cscoreSimulated = response)
  
  # Return the modified dataset
  dataset
})

######## Effect Size ANalysis ###########

# Function to perform BKMR analysis
perform_bkmr_analysis <- function(dataset) {
  cscore <- unlist(dataset[, 16]) 
  expos <- data.matrix(dataset[, c(2:6)])
  knots50 <- fields::cover.design(expos, nd = 50)$design
  
  fitkm_simBlood <- kmbayes(
    y = cscore, 
    Z = expos,
    X = NULL, 
    iter = 10000,
    family = "gaussian",
    verbose = FALSE, 
    varsel = TRUE,
    knots = knots50
  )
  
  simBloodPIP <- ExtractPIPs(fitkm_simBlood) %>% arrange(desc(PIP))
  
  predRespUnivar <- PredictorResponseUnivar(fit = fitkm_simBlood)
  
  risks.overall <- OverallRiskSummaries(
    fit = fitkm_simBlood, 
    y = cscore, 
    Z = expos, 
    X = NULL, 
    qs = seq(0.25, 0.75, by = 0.05), 
    q.fixed = 0.5, 
    method = "exact"
  )
  
  risks.singvar <- SingVarRiskSummaries(
    fit = fitkm_simBlood,
    y = cscore,
    Z = expos,
    X = NULL,
    qs.diff = c(0.25, 0.75),
    q.fixed = c(0.25, 0.50, 0.75),
    method = "exact"
  )
  
  list(
    simBloodPIP = simBloodPIP,
    predRespUnivar = predRespUnivar,
    risks.overall = risks.overall,
    risks.singvar = risks.singvar
  )
}

# Initialize a cluster with 52 cores
cl <- makeCluster(52)
registerDoParallel(cl)

# Select 100 datasets
selectedDatasets <- sample(modifiedDatasets, 15, replace = FALSE)

# Perform analysis in parallel
analysisResults <- foreach(i = 1:15, .packages = c("bkmr", "tidyverse")) %dopar% {
  perform_bkmr_analysis(selectedDatasets[[i]])
}

# Stop the parallel cluster
stopCluster(cl)



# Save results
if (!dir.exists(paste0(here::here(), "/data/testBKMR_20240205"))) {
  dir.create(paste0(here::here(), "/data/testBKMR_20240205"))
}


saveRDS(
  analysisResults,
  file =  here::here(
    "data", "testBKMR_20240205", "diagEffectAnalysis_20240205.rds"
  )
)
