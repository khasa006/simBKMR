library(tidyverse)

# Define color palette
color_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")

# Combine all data frames into one, adding a variable to identify each type
diagEffectSizeMean$Type <- "Diagonal Covariance Data with Different Means"
unstrucEffectSizeMean$Type <- "Unstructured Covariance Data with Different Means"
diagEffectSizeVar$Type <- "Diagonal Covariance Data with Different SDs"
unstrucEffectSizeVar$Type <- "Unstructured Covariance Data with Different SDs"

all_data <- rbind(diagEffectSizeMean, unstrucEffectSizeMean, diagEffectSizeVar, unstrucEffectSizeVar)

# Plot
ggplot(all_data %>% filter(variable != "Overall")) +
  aes(
    x = abs(sd/mean),
    y = testSize,
    color = Type
  ) +
  # Scatter Plot
  geom_jitter() +
  geom_hline(aes(yintercept = 0.05, linetype = "Threshold"), color = "red") +
  labs(
    x = "Coefficient of Variation",
    y = "Test Size",
    title = "Comparing the Relationship between CV and Test Size"
  ) +
  scale_color_manual(values = color_palette, guide = "none") +# Set color palette
  scale_linetype_manual(values = "dashed", name = "Test Size (0.05)") +
  facet_wrap(~Type, scales = "fixed") +  # Ensure shared axes
  theme_minimal() +
  theme(
    legend.position = "top",  # Move legend to the top
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white")  # Set plot background to white
  )


# Saving the plot as a .tiff file with 300 dpi
# ggsave("cv_test_size.tiff", dpi = 300)

