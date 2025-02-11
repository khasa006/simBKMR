# BKMR Power Analysis of Full Factorial High
# Kazi Tanvir Hasan and Dr. Gabriel Odom 
# 2024-01-12
# update: 2024-01-18
# update: 2024-01-20
# update: 2024-01-22
# update: 2024-01-23
# Final: 2024-01-27


suppressPackageStartupMessages(
  {
    library(bkmr)
    library(doParallel)
    library(tidyverse)
  }
)

# source("r_code/generateSignal_20240112.R")

source("/home/khasan/BKMR/r_code/powerAnalysis_20230127/generateSignal_20240112.R")

# Function to perform BKMR analysis
perform_bkmr_analysis <- function(dataset) {
  cscore <- unlist(dataset[, 27]) 
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
set.seed(2312)
selected_datasets <- sample(modified_datasets, 100, replace = FALSE)

# Perform analysis in parallel
analysis_results <- foreach(i = 1:100, .packages = c("bkmr", "tidyverse")) %dopar% {
  perform_bkmr_analysis(selected_datasets[[i]])
}

# Stop the parallel cluster
stopCluster(cl)

# # Save results
# if (!dir.exists(paste0(here::here(), "/data/powerBKMR_20240112"))) {
#   dir.create(paste0(here::here(), "/data/powerBKMR_20240112"))
# }
# 
# saveRDS(
#   analysis_results,
#   file =  here::here("data", "powerBKMR_20240112", "fullFactorialHigh3_20240112.rds")
# )

# Save results
if (!dir.exists(paste0(here::here(), "/data/powerBKMR_20240127"))) {
  dir.create(paste0(here::here(), "/data/powerBKMR_20240127"))
}


saveRDS(
  analysis_results,
  file = here::here("data", "powerBKMR_20240127", "fullFactorialHigh_20240127.rds")
)
