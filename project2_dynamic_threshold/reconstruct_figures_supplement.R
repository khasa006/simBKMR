# Saved-output reconstruction of manuscript Figures 1-2 and Supplementary Table 1
# Execute from project2_dynamic_threshold/ with six CSV files in data/mleData_20240920/.
# Produces figures/tables from stored PIP values; does NOT rerun BKMR MCMC.
suppressPackageStartupMessages(library(ggplot2))
sizes <- c(150,300,600,1000,2000,2934)
paths <- file.path("data","mleData_20240920",sprintf("mleData%d_df.csv",sizes))
stopifnot(all(file.exists(paths)))
parts <- Map(function(p,n) {
  z <- read.csv(p)
  stopifnot(all(c("CV","PIP") %in% names(z)))
  data.frame(sampleSize=n,CV=z$CV,PIP=z$PIP)
},paths,sizes)
d <- do.call(rbind,parts)
idx <- split(seq_len(nrow(d)),interaction(d$sampleSize,d$CV,drop=TRUE))
f <- function(cv,n) (1.30460+exp(-0.59867*log2(cv)))^(-0.43565*log10(n))
res <- do.call(rbind,lapply(idx,function(i) {
  n <- d$sampleSize[i[1]]; cv <- d$CV[i[1]];pip <- d$PIP[i]
  data.frame(sampleSize=n,CV=cv,N=length(i),
             PIP_q95=as.numeric(quantile(pip,.95)),
             fixed=mean(pip >= .5),
             dynamic=mean(pip >= f(cv,n)),
             pred=f(cv,n))
}))
res <- res[order(res$sampleSize,res$CV),]
dir.create("reconstructed_results",showWarnings=FALSE)
write.csv(res[,c("sampleSize","CV","fixed","dynamic")],
          "reconstructed_results/Supplementary_Table_1_values.csv",row.names=FALSE)
res$sampleSizeFactor <- factor(res$sampleSize,levels=sizes)
p1 <- ggplot(res,aes(x=log2(CV),colour=sampleSizeFactor))+
  geom_point(aes(y=PIP_q95),alpha=.55)+
  geom_line(aes(y=pred),linewidth=.65)+
  labs(x="log2(CV)",y="95th Percentile PIP",colour="Sample Size")+
  theme_minimal()+theme(legend.position="bottom")
ggsave("reconstructed_results/Figure1_reconstructed.png",p1,width=8,height=6,dpi=300)
long <- rbind(data.frame(res[,c("CV","sampleSize")],threshold="Fixed (0.5)",rate=res$fixed),
              data.frame(res[,c("CV","sampleSize")],threshold="Dynamic",rate=res$dynamic))
p2 <- ggplot(long,aes(x=log2(CV),y=rate,size=log10(sampleSize)))+
  geom_point(aes(fill=threshold,shape=threshold),color="black",alpha=.65)+
  geom_hline(yintercept=.05,colour="red",linetype="dashed")+
  scale_shape_manual(values=c("Fixed (0.5)"=21,"Dynamic"=23))+
  scale_fill_manual(values=c("Fixed (0.5)"="orange","Dynamic"="green"))+
  labs(x="Coefficient of Variation (log2 transformed)",
       y="Empirical False-Positive Selection Frequency",
       size="Sample size (log10 transformed)")+
  theme_minimal()
ggsave("reconstructed_results/Figure2_reconstructed.png",p2,width=8,height=6,dpi=300)
cat("Rows:",nrow(res),"; fixed MAD:",median(abs(res$fixed-.05)),
    "; dynamic MAD:",median(abs(res$dynamic-.05)),"\n")
# Any exact equality at the cutoff should be checked against the original manuscript
# convention (< versus <=). For the supplied data, this requires no numerical change.
