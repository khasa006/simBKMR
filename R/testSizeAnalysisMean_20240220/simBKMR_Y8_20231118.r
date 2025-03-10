# BKMR analysis of Strong Effect when β_1≠0
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2023-11-15

suppressPackageStartupMessages(
  {
    library(bkmr)
    library(doParallel)
    library(tidyverse)
  }
)

source("/home/khasan/BKMR/r_code/generateResponse_20231018.R")

# Function to perform BKMR analysis
perform_bkmr_analysis <- function(dataset) {
  cscore <- unlist(dataset[, 23]) 
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

# Select 50 random datasets
set.seed(2311)
selected_datasets <- sample(modified_datasets, 50, replace = FALSE)

# Perform analysis in parallel
analysis_results <- foreach(i = 1:50, .packages = c("bkmr", "tidyverse")) %dopar% {
  perform_bkmr_analysis(selected_datasets[[i]])
}

# Stop the parallel cluster
stopCluster(cl)

# Save results
output_dir <- "/home/khasan/BKMR/data/simBKMRY8_20231118"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

saveRDS(
  analysis_results,
  file = file.path(output_dir, "analysis_results.rds")
)