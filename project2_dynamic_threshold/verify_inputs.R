# Read-only structural check; run from repository root in RStudio.
folder <- "project2_dynamic_threshold/data/mleData_20240920"
ns <- c(150, 300, 600, 1000, 2000, 2934)
for (n in ns) {
  f <- file.path(folder, paste0("mleData", n, "_df.csv"))
  stopifnot(file.exists(f))
  d <- read.csv(f, check.names=FALSE)
  stopifnot(all(c("variable", "CV", "PIP") %in% names(d)))
  cat(basename(f), ": ", nrow(d), " rows; ",
      length(unique(d$CV)), " unique CV values\n", sep="")
}
cat("Structural input check passed; numerical reproduction NOT established.\n")
