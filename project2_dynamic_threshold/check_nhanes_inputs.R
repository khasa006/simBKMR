# Reproduce the NHANES complete-case count and cognitive-score CV.
# Run from the ROOT of the simBKMR repository:
#   Rscript project2_dynamic_threshold/check_nhanes_inputs.R
# No external packages or MCMC sampling required.
#
# This uses the existing public repository dataset, which has short
# uppercase column names (unlike one historical CSV with verbose headers).
# This check does NOT reproduce the BKMR PIPs or Figures 3–4.
input <- "data/cognitiveSurveyData_df.csv"
if (!file.exists(input)) stop("Expected existing repository file: ", input)
d <- read.csv(input, stringsAsFactors = FALSE, check.names = FALSE)
metals <- c("UBARIUM", "UCADMIUM", "UCOBALT", "UMANGE",
            "ULEAD", "UTIN", "USTOUM", "UURANIUM")
required <- c(metals, "CSCORE")
missing <- setdiff(required, names(d))
if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
cc <- d[complete.cases(d[, required]), required]
cv <- sd(cc$CSCORE) / mean(cc$CSCORE)
cat("Initial rows: ", nrow(d), "\n", sep = "")
cat("Complete-case rows: ", nrow(cc), "\n", sep = "")
cat("Mean cognitive score: ", mean(cc$CSCORE), "\n", sep = "")
cat("SD cognitive score: ", sd(cc$CSCORE), "\n", sep = "")
cat("Cognitive score CV: ", format(cv, digits = 10), "\n", sep = "")
stopifnot(nrow(d) == 2934L, nrow(cc) == 949L,
          round(cv, 2) == 15.18)
cat("Manuscript sample size and CV: matched.\n")
