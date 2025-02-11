# Plot the PIPs Decision and Skweness
# Kazi Tanvir Hasan nd Dr. Gabriel Odom
# 2024-01-25
# update: 2024-01-27

library(patchwork)
library(tidyverse)

# Read data
pipSkewness_df <- read_csv(
  "data/metricPlotDataset_20240125/pipSkewness_df_20240125.csv"
)

# Set color-blind friendly colors
color_palette <- c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00")

###### Treated Data ######## 
# Set up the 4x3 layout
par(mfrow = c(4, 3))

# Plot 1
treatedNormal <- pipSkewness_df %>% 
  # for normal only lead was treated
  filter(variable == "BLEAD") %>% 
  select(variable, normalLow, normalMedium, normalHigh, skewness) %>%
  gather(key = "measurement", value = "value", -variable, -skewness) %>%
  # Specify the order of levels
  mutate(
    measurement = factor(
      measurement, 
      levels = c("normalLow", "normalMedium", "normalHigh")
    )
  ) %>%  
  ggplot() +
  aes(x = skewness, y = value, color = variable) +
  geom_point() +
  geom_hline(aes(yintercept = 0.5, linetype = "PIP = 0.5"), color = "red") +
  scale_color_manual(values = color_palette[[1]]) +  # Use color-blind friendly colors
  facet_wrap(~ measurement, scales = "free_y") +
  ggtitle("BLEAD - Normal") +
  xlab("Skewness") +
  ylab("PIP") +
  scale_x_continuous(
    limits = c(0, max(pipSkewness_df$skewness))
  )  + 
  ylim(0, 1) + 
  # Add legend for geom_hline
  guides(
    color = guide_legend(title = "Variable"),
    linetype = guide_legend(title = "Threshold")
  )

# Plot 2
treatedSkewed <- pipSkewness_df %>% 
  # for skewed only mercury was treated
  filter(variable == "BMERCURY") %>% 
  select(variable, skewedLow, skewedMedium, skewedHigh, skewness) %>%
  gather(key = "measurement", value = "value", -variable, -skewness) %>%
  # Specify the order of levels
  mutate(
    measurement = factor(
      measurement, 
      levels = c("skewedLow", "skewedMedium", "skewedHigh")
    )
  ) %>%  
  ggplot() +
  aes(x = skewness, y = value, color = variable) +
  geom_point() +
  geom_hline(aes(yintercept = 0.5, linetype = "PIP = 0.5"), color = "red") +
  scale_color_manual(values = color_palette[[2]]) +  # Use color-blind friendly colors
  facet_wrap(~ measurement, scales = "free_y") +
  ggtitle("BMERCURY - Skewed") +
  xlab("Skewness") +
  ylab("PIP") +
  scale_x_continuous(
    limits = c(0, max(pipSkewness_df$skewness))
  )  + 
  ylim(0, 1) + 
  # Add legend for geom_hline
  guides(
    color = guide_legend(title = "Variable"),
    linetype = guide_legend(title = "Threshold")
  )

# Plot 3
treatedInteraction <- pipSkewness_df %>% 
  # for interaction only lead and mercury were treated
  filter(variable %in% c("BLEAD", "BMERCURY")) %>% 
  select(
    variable, interactionLow, interactionMedium, interactionHigh, skewness
  ) %>%
  gather(key = "measurement", value = "value", -variable, -skewness) %>%
  # Specify the order of levels
  mutate(
    measurement = factor(
      measurement, 
      levels = c("interactionLow", "interactionMedium", "interactionHigh")
    )
  ) %>% 
  ggplot() +
  aes(x = skewness, y = value, color = variable) +
  geom_point() +
  geom_hline(aes(yintercept = 0.5, linetype = "PIP = 0.5"), color = "red") +
  scale_color_manual(values = color_palette) +  # Use color-blind friendly colors
  facet_wrap(~ measurement, scales = "free_y") +
  ggtitle("Combined - Interaction") +
  xlab("Skewness") +
  ylab("PIP") +
  scale_x_continuous(
    limits = c(0, max(pipSkewness_df$skewness))
  )  + 
  ylim(0, 1) + 
  # Add legend for geom_hline
  guides(
    color = guide_legend(title = "Variable"),
    linetype = guide_legend(title = "Threshold")
  )

# Plot 4
treatedFullFactorial <- pipSkewness_df %>% 
  # for interaction only lead and mercury were treated
  filter(variable %in% c("BLEAD", "BMERCURY")) %>% 
  select(
    variable, fullFactorialLow, fullFactorialMedium, fullFactorialHigh, skewness
  ) %>%
  gather(key = "measurement", value = "value", -variable, -skewness) %>%
  # Specify the order of levels
  mutate(
    measurement = factor(
      measurement, 
      levels = c("fullFactorialLow", "fullFactorialMedium", "fullFactorialHigh")
    )
  ) %>% 
  ggplot() +
  aes(x = skewness, y = value, color = variable) +
  geom_point() +
  geom_hline(aes(yintercept = 0.5, linetype = "PIP = 0.5"), color = "red") +
  scale_color_manual(values = color_palette) +  # Use color-blind friendly colors
  facet_wrap(~ measurement, scales = "free_y") +
  ggtitle("Combined - Full Factorial") +
  xlab("Skewness") +
  ylab("PIP") +
  scale_x_continuous(
    limits = c(0, max(pipSkewness_df$skewness))
  )  + 
  ylim(0, 1) + 
  # Add legend for geom_hline
  guides(
    color = guide_legend(title = "Variable"),
    linetype = guide_legend(title = "Threshold")
  )

# Combine plots using patchwork
treatedPlots <- treatedNormal + treatedSkewed + treatedInteraction + treatedFullFactorial

# Display the combined plot
treatedPlots

###### Untreated Data ######## 

# Set up the 4x3 layout
par(mfrow = c(4, 3))

# Plot 5
untreatedNormal <- pipSkewness_df %>% 
  filter(variable %in% c("BCADMIUM", "BMANGE","BMERCURY", "BSELEUM")) %>% 
  select(variable, normalLow, normalMedium, normalHigh, skewness) %>%
  gather(key = "measurement", value = "value", -variable, -skewness) %>%
  # Specify the order of levels
  mutate(
    measurement = factor(
      measurement, 
      levels = c("normalLow", "normalMedium", "normalHigh")
    )
  ) %>%
  ggplot() +
  aes(x = skewness, y = value, color = variable) +
  geom_point() +
  geom_hline(aes(yintercept = 0.5, linetype = "PIP = 0.5"), color = "red") +
  scale_color_manual(values = color_palette) +  # Use color-blind friendly colors
  facet_wrap(~ measurement, scales = "free_y") +
  ggtitle("Untreated - Normal") +
  xlab("Skewness") +
  ylab("PIP") +
  scale_x_continuous(
    limits = c(0, max(pipSkewness_df$skewness))
  )  + 
  ylim(0, 1) + 
  # Add legend for geom_hline
  guides(
    color = guide_legend(title = "Variable"),
    linetype = guide_legend(title = "Threshold")
  )

# Plot 6
untreatedSkewed <- pipSkewness_df %>% 
  filter(variable %in% c("BCADMIUM", "BMANGE", "BLEAD", "BSELEUM")) %>% 
  select(variable, skewedLow, skewedMedium, skewedHigh, skewness) %>%
  gather(key = "measurement", value = "value", -variable, -skewness) %>%
  # Specify the order of levels
  mutate(
    measurement = factor(
      measurement, 
      levels = c("skewedLow", "skewedMedium", "skewedHigh")
    )
  ) %>% 
  ggplot() +
  aes(x = skewness, y = value, color = variable) +
  geom_point() +
  geom_hline(aes(yintercept = 0.5, linetype = "PIP = 0.5"), color = "red") +
  scale_color_manual(values = color_palette) +  # Use color-blind friendly colors
  facet_wrap(~ measurement, scales = "free_y") +
  ggtitle("Untreated - Skewed") +
  xlab("Skewness") +
  ylab("PIP") +
  scale_x_continuous(
    limits = c(0, max(pipSkewness_df$skewness))
  )  + 
  ylim(0, 1) + 
  # Add legend for geom_hline
  guides(
    color = guide_legend(title = "Variable"),
    linetype = guide_legend(title = "Threshold")
  )

# Plot 7
untreatedInteraction <- pipSkewness_df %>% 
  filter(variable %in% c("BCADMIUM", "BMANGE", "BSELEUM")) %>% 
  select(
    variable, interactionLow, interactionMedium, interactionHigh, skewness
  ) %>%
  gather(key = "measurement", value = "value", -variable, -skewness) %>%
  # Specify the order of levels
  mutate(
    measurement = factor(
      measurement, 
      levels = c("interactionLow", "interactionMedium", "interactionHigh")
    )
  ) %>% 
  ggplot() +
  aes(x = skewness, y = value, color = variable) +
  geom_point() +
  geom_hline(aes(yintercept = 0.5, linetype = "PIP = 0.5"), color = "red") +
  scale_color_manual(values = color_palette) + # Use color-blind friendly colors
  facet_wrap(~ measurement, scales = "free_y") +
  ggtitle("Untreated - Interaction") +
  xlab("Skewness") +
  ylab("PIP") +
  scale_x_continuous(
    limits = c(0, max(pipSkewness_df$skewness))
  )  + 
  ylim(0, 1) + 
  # Add legend for geom_hline
  guides(
    color = guide_legend(title = "Variable"),
    linetype = guide_legend(title = "Threshold")
  )

# Plot 8
untreatedFullFactorial <- pipSkewness_df %>% 
  filter(variable %in% c("BCADMIUM", "BMANGE", "BSELEUM")) %>% 
  select(
    variable, fullFactorialLow, fullFactorialMedium, fullFactorialHigh, skewness
  ) %>%
  gather(key = "measurement", value = "value", -variable, -skewness) %>%
  # Specify the order of levels
  mutate(
    measurement = factor(
      measurement, 
      levels = c("fullFactorialLow", "fullFactorialMedium", "fullFactorialHigh")
    )
  ) %>% 
  ggplot() +
  aes(x = skewness, y = value, color = variable) +
  geom_point() +
  geom_hline(aes(yintercept = 0.5, linetype = "PIP = 0.5"), color = "red") +
  scale_color_manual(values = color_palette) +  # Use color-blind friendly colors
  facet_wrap(~ measurement, scales = "free_y") +
  ggtitle("Untreated - Full Factorial") +
  xlab("Skewness") +
  ylab("PIP") +
  scale_x_continuous(
    limits = c(0, max(pipSkewness_df$skewness))
  )  + 
  ylim(0, 1) + 
  # Add legend for geom_hline
  guides(
    color = guide_legend(title = "Variable"),
    linetype = guide_legend(title = "Threshold")
  )

# Combine plots using patchwork
untreatedPlots <- untreatedNormal + untreatedSkewed + untreatedInteraction + untreatedFullFactorial

# Display the combined plot
untreatedPlots
