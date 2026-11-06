# Saved-output verification: dynamic threshold paper
# Uses base R only. It does NOT regenerate simulated data or run BKMR MCMC.
# From the simBKMR repository root:
#   Rscript project2_dynamic_threshold/verify_results_from_saved_PIP.R
input_dir <- file.path('project2_dynamic_threshold','data','mleData_20240920')
sizes <- c(150L,300L,600L,1000L,2000L,2934L)
files <- file.path(input_dir,sprintf('mleData%d_df.csv',sizes))
if (!all(file.exists(files))) stop('Missing PIP input CSV(s): ', paste(files[!file.exists(files)],collapse=', '))
parts <- Map(function(path,n) {
  d <- read.csv(path,check.names=FALSE)
  if (!all(c('CV','PIP') %in% names(d))) stop('Wrong columns: ',path)
  data.frame(sampleSize=n,CV=d$CV,PIP=d$PIP)
}, files, sizes)
d <- do.call(rbind,parts)
groups <- split(seq_len(nrow(d)),interaction(d$sampleSize,d$CV,drop=TRUE))
sumdf <- do.call(rbind,lapply(groups,function(ix) {
  p <- d$PIP[ix]; cv <- d$CV[ix[1]]; n <- d$sampleSize[ix[1]]
  data.frame(sampleSize=n,CV=cv,groupN=length(ix),PIP_q95=unname(quantile(p,.95)),
             fixed=mean(p >= 0.5))
}))
rownames(sumdf) <- NULL
if (nrow(sumdf) != 246) stop('Expected 246 groups, found ',nrow(sumdf))
threshold <- function(C,beta1,beta2,CV,sampleSize) {
  (C+exp(-beta1*log2(CV)))^(-beta2*log10(sampleSize))
}
fit <- nls(PIP_q95 ~ threshold(C,beta1,beta2,CV,sampleSize),
           data=sumdf,start=list(C=1,beta1=1,beta2=1))
print(coef(fit))
published <- c(C=1.30460,beta1=0.59867,beta2=0.43565)
cat('Coefficient rounded-to-5-digit agreement:',
    all(round(coef(fit),5) == round(published,5)),'\n')
# Published rounded coefficients reproduce the stated frequency metrics.
sumdf$dynamic <- vapply(groups, function(ix) {
  cv <- d$CV[ix[1]]; n <- d$sampleSize[ix[1]]
  mean(d$PIP[ix] >= threshold(published['C'],published['beta1'],published['beta2'],cv,n))
}, numeric(1))
for (nm in c('fixed','dynamic')) {
  delta <- abs(sumdf[[nm]]-0.05)
  cat(nm, ' median absolute deviation: ',median(delta),
      ' max absolute deviation: ',max(delta),'\n',sep='')
}
nhanes <- threshold(published['C'],published['beta1'],published['beta2'],15.18,949)
cat('NHANES threshold arithmetic (CV 15.18, n 949): ',nhanes,'\n',sep='')
# Output values; do not imply independent generation from raw simulation inputs.
write.csv(sumdf,file='project2_dynamic_threshold/saved_PIP_summary_checked.csv',row.names=FALSE)
