# Verify dynamic PIP threshold results against six saved PIP CSV files
# This does NOT re-run BKMR MCMC or the original NHANES analysis.
# Run from the directory project2_dynamic_threshold after adding the
# six CSV files to data/mleData_20240920.
# No external R packages are required.

sizes <- c(150, 300, 600, 1000, 2000, 2934)
root <- file.path("data", "mleData_20240920")
paths <- file.path(root, sprintf("mleData%d_df.csv", sizes))
if (!all(file.exists(paths))) {
  stop("Missing inputs: ", paste(paths[!file.exists(paths)], collapse = ", "))
}
parts <- Map(function(path, n) {
  x <- read.csv(path, check.names = FALSE)
  stopifnot(all(c("CV", "PIP") %in% names(x)))
  x$sampleSize <- n
  x
}, paths, sizes)
df <- do.call(rbind, parts)

# PIP 95th percentile per observed (sample size, CV) group
groups <- split(seq_len(nrow(df)), interaction(df$sampleSize, df$CV, drop = TRUE))
quantiles <- do.call(rbind, lapply(groups, function(ix) {
  data.frame(sampleSize = df$sampleSize[ix[1]],
             CV = df$CV[ix[1]],
             PIP_q95 = unname(quantile(df$PIP[ix], 0.95)),
             group_n = length(ix))
}))
rownames(quantiles) <- NULL
stopifnot(nrow(quantiles) == 246L)

# Identical curve expression to the historical R script.
curve_fn <- function(C, beta1, beta2, cv, ss) {
  (C + exp(-beta1 * log2(cv)))^(-beta2 * log10(ss))
}
fit <- nls(PIP_q95 ~ curve_fn(C, beta1, beta2, CV, sampleSize),
           data = quantiles,
           start = list(C = 1, beta1 = 1, beta2 = 1))
print(coef(fit))
target <- c(C = 1.30460, beta1 = 0.59867, beta2 = 0.43565)
cat("Coefficients match reported estimates to 5 decimals:",
    all(round(coef(fit), 5) == round(target, 5)), "\n")

# Use reported rounded coefficients for manuscript-comparison metrics.
df$below_fixed <- df$PIP < 0.5
df$below_dynamic <- df$PIP <
  curve_fn(target["C"], target["beta1"], target["beta2"],
           df$CV, df$sampleSize)
summary_rows <- do.call(rbind, lapply(groups, function(ix) {
  n <- length(ix)
  data.frame(
    sampleSize = df$sampleSize[ix[1]],
    CV = df$CV[ix[1]],
    n = n,
    fixed_rate = 1 - sum(df$below_fixed[ix]) / n,
    dynamic_rate = 1 - sum(df$below_dynamic[ix]) / n
  )
}))
rownames(summary_rows) <- NULL
for (k in c("fixed_rate", "dynamic_rate")) {
  cat(k, "MAD =", median(abs(summary_rows[[k]] - 0.05)),
      "; maximum absolute deviation =",
      max(abs(summary_rows[[k]] - 0.05)), "\n")
}
cat("NHANES threshold calculated from reported CV and n:",
    curve_fn(target["C"], target["beta1"], target["beta2"],
             cv = 15.18, ss = 949), "\n")

# Optional local outputs; do not commit generated results unless reviewed.
write.csv(quantiles, "pip_95_quantiles_verified.csv", row.names = FALSE)
write.csv(summary_rows, "selection_frequencies_verified.csv", row.names = FALSE)
