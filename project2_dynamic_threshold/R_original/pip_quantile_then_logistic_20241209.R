# Quantiles of samples
# Gabriel Odom and Tanvir Hasan
# 2024-12-09



library(tidyverse)

# Read data files using tidyverse functions (read_csv)
mleDataDf150 <- read_csv("data/mleData_20240920/mleData150_df.csv")
mleDataDf300 <- read_csv("data/mleData_20240920/mleData300_df.csv")
mleDataDf600 <- read_csv("data/mleData_20240920/mleData600_df.csv")
mleDataDf1000 <- read_csv("data/mleData_20240920/mleData1000_df.csv")
mleDataDf2000 <- read_csv("data/mleData_20240920/mleData2000_df.csv")
mleDataDf2934 <- read_csv("data/mleData_20240920/mleData2934_df.csv")

# summaryLongDf150 <- read_csv("bkmr_simulation_stuff/data/mleData_20240920/summaryLong_df150.csv")
# summaryLongDf300 <- read_csv("bkmr_simulation_stuff/data/mleData_20240920/summaryLong_df300.csv")
# summaryLongDf600 <- read_csv("bkmr_simulation_stuff/data/mleData_20240920/summaryLong_df600.csv")
# summaryLongDf1000 <- read_csv("bkmr_simulation_stuff/data/mleData_20240920/summaryLong_df1000.csv")
# summaryLongDf2000 <- read_csv("bkmr_simulation_stuff/data/mleData_20240920/summaryLong_df2000.csv")
# summaryLongDf2934 <- read_csv("bkmr_simulation_stuff/data/mleData_20240920/summaryLong_df2934.csv")

# Combine datasets for mleData and summaryLong into one dataframe
mleResultDf <- bind_rows(
	mleDataDf150 %>% mutate(sampleSize = 150),
	mleDataDf300 %>% mutate(sampleSize = 300),
	mleDataDf600 %>% mutate(sampleSize = 600),
	mleDataDf1000 %>% mutate(sampleSize = 1000),
	mleDataDf2000 %>% mutate(sampleSize = 2000),
	mleDataDf2934 %>% mutate(sampleSize = 2934)
)

# summaryResultDf <- bind_rows(
# 	summaryLongDf150 %>% mutate(sampleSize = 150),
# 	summaryLongDf300 %>% mutate(sampleSize = 300),
# 	summaryLongDf600 %>% mutate(sampleSize = 600),
# 	summaryLongDf1000 %>% mutate(sampleSize = 1000),
# 	summaryLongDf2000 %>% mutate(sampleSize = 2000),
# 	summaryLongDf2934 %>% mutate(sampleSize = 2934)
# ) %>%
# 	filter(Statistic %in% c("mean_pip", "sd_pip", "pip_95"))

rm(
	mleDataDf150, mleDataDf300, mleDataDf600, mleDataDf1000, mleDataDf2000,
	mleDataDf2934
)

# rm(
# 	summaryLongDf150, summaryLongDf300, summaryLongDf600, summaryLongDf1000,
# 	summaryLongDf2000, summaryLongDf2934
# )


###  Find Sample Quantiles  ###
# pipQuantile_df <- 
# 	mleResultDf %>% 
# 	group_by(sampleSize, CV) %>% 
# 	# SOme of the groups have 1000 rows???
# 	sample_n(500) %>% 
# 	mutate(replicate = sample(rep(1:5, each = 5 * 20))) %>% 
# 	group_by(replicate, .add = TRUE) %>% 
# 	summarise(
# 		PIP_q95 = sort(PIP)[95]
# 	) %>% 
# 	select(-replicate) %>% 
# 	ungroup()


# 2 and 10 appears twice in CV

pipQuantile_df <- 
  mleResultDf %>% 
  group_by(sampleSize, CV) %>% 
  # # Sample 250 rows from each group
  # sample_n(250) %>% 
  # # Create 'replicate' vector matching the size of the group (250)
  # mutate(replicate = rep(1:5, length.out = n())) %>% 
  # group_by(replicate, .add = TRUE) %>% 
  summarise(
    PIP_q95 = quantile(PIP, 0.95)  # Compute the 95th percentile for PIP
  ) %>% 
  # select(-replicate) %>% 
  ungroup()


ggplot(data = pipQuantile_df) + 
	theme(legend.position = "bottom") +
	aes(
		x = log2(CV),
		y = PIP_q95,
		colour = factor(sampleSize, ordered = TRUE)
	) + 
	guides(colour = guide_legend(title = "Sample Size")) +
	geom_jitter(alpha = 0.5)

pip_q95_mod <- lm(
	formula = log(PIP_q95 / (1 - PIP_q95)) ~ log2(CV) + log10(sampleSize),
	data = pipQuantile_df
)

pipQuantile_df$predPIPq95 <- predict.lm(pip_q95_mod, newdata = pipQuantile_df)
pipPred_df <- 
	pipQuantile_df %>% 
	mutate(pred = exp(predPIPq95) / (1 + exp(predPIPq95)))

ggplot(data = pipPred_df) + 
	theme(legend.position = "bottom") +
	aes(
		x = log2(CV),
		colour = factor(sampleSize, ordered = TRUE)
	) + 
	guides(colour = guide_legend(title = "Sample Size")) +
	geom_jitter(aes(y = PIP_q95), alpha = 0.5) + 
	geom_line(aes(y = pred))
# Tanvir: Maybe try a 4-parameter logistic curve?


###  make a function  ###
coefficients(summary(pip_q95_mod))

pipThresh_fn <- function(cv, ss) {
	exp(3.6248275 + 0.8074649 * log2(cv) - 1.6192377 * log10(ss)) / 
		(1 + exp(3.6248275 + 0.8074649 * log2(cv) - 1.6192377 * log10(ss)))
}
# Test
pipThresh_fn(cv = 0.0235, ss = 150)


###  test some values  ###

mleResultDf %>%
  filter(sampleSize == 1000) %>%
  # Some floating point error makes us filter to a range
  # filter(CV < 0.013, CV > 0.014) %>%
  filter(CV == 0.5) %>%
  summarise(
    test_size_old = sum(PIP < 0.5),
    test_size_new = sum(
      PIP < pipThresh_fn(cv = unique(CV), ss = unique(sampleSize))
    )
  )


mleResultDf %>%
  filter(sampleSize == 150) %>%
  # Some floating point error makes us filter to a range
  # filter(CV < 0.013, CV > 0.014) %>%
  filter(CV == 0.2) %>%
  summarise(
    test_size_old = sum(PIP < 0.5),
    test_size_new = sum(
      PIP < pipThresh_fn(cv = unique(CV), ss = unique(sampleSize))
    )
  )


mleResultDf %>%
  filter(sampleSize == 150) %>%
  # Some floating point error makes us filter to a range
  # filter(CV < 0.013, CV > 0.014) %>%
  filter(CV == 0.1) %>%
  summarise(
    test_size_old = sum(PIP < 0.5),
    test_size_new = sum(
      PIP < pipThresh_fn(cv = unique(CV), ss = unique(sampleSize))
    )
  )

mleResultDf %>%
  filter(sampleSize == 1000) %>%
  # Some floating point error makes us filter to a range
  filter(CV < 0.053, CV > 0.052) %>%
  # filter(CV == 0.2) %>%
  summarise(
    test_size_old = sum(PIP < 0.5),
    test_size_new = sum(
      PIP < pipThresh_fn(cv = unique(CV), ss = unique(sampleSize))
    )
  )



mleResultDf %>%
  pull(CV) %>%
  unique() %>%
  sort()

