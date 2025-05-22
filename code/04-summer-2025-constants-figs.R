#figures with constants only####################################################
output_norberg <- read.csv("data/output-norberg.csv")
output_norberg2 <- output_norberg %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_")

all_rfus_3 <- read.csv("data/tpc_processed_all_rfus.csv")

# TPC ---------------------------------------------------------------------
all_rfus_3 %>% 
  filter(temp_treatment %in% c("blank", "14C", "30C")) %>%
  rename(Treatment = temp_treatment) %>% 
  ggplot(aes(x = days, y = RFU, colour = Treatment, group = well_id)) + 
  geom_line() +
  facet_wrap(~ temp, scales = "free") +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    strip.text = element_text(face = "bold")  # Makes facet labels bold
  ) + 
  scale_color_manual(values = c("14C" = "#145da0", 
                                "30C" = "#bc1823", 
                                "6F" = "#ff8210", 
                                "48F" = "#800080", 
                                "blank" = "grey"))  

output_norberg2 %>% 
  filter(incubator %in% c("blank", "14C", "30C")) %>%
  rename(Treatment = incubator, 
         Growth = predicted_growth, 
         Temperature = temp) %>% 
  ggplot(aes(Temperature, Growth, colour = Treatment)) + 
  geom_point(size = 0.01) + 
  ylim(0, 1.5) + 
  theme_minimal() +
  scale_color_manual(values = c("14C" = "#145da0", 
                                "30C" = "#bc1823", 
                                "6F" = "#ff8210", 
                                "48F" = "#800080")) + 
  labs(
    x = "Temperature (°C)",
    y = "Growth rate (individual/day)"
  )


# BAH, preds --------------------------------------------------------------
preds_plot_data <- output_norberg2 %>%
  filter(incubator %in% c("14C", "30C")) %>% 
  filter(temp %in% c(14.0, 30.0)) %>%
  select(incubator, temp, flask, predicted_growth) %>%
  mutate(temp = factor(temp, levels = c(14, 30), labels = c("14°C", "30°C")))  # Add degree symbols

# Update p_values to match new temp labels
p_values <- data.frame(
  temp = factor(c("14°C", "30°C"), levels = c("14°C", "30°C")),  # Updated to match the new labels
  group1 = "14C",
  group2 = "30C",
  p = c(1.575e-10, 1.331e-06),
  label = c("***", "***")
)

# Plot
ggplot(preds_plot_data, aes(x = incubator, y = predicted_growth, fill = incubator)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 1) +
  facet_wrap(~temp) +
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "Predicted growth rate (individual/day)",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823")) +
  geom_text(data = p_values, aes(x = 1.5, y = max(preds_plot_data$predicted_growth) + 0.1, label = label), inherit.aes = FALSE)


# BAH, actual -------------------------------------------------------------
actual <- read.csv("data/growthtool-gdat-sum.csv")

actual <- actual %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_") %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp")) %>% 
  unite("unique_well", "plate", "well", sep = "_") %>% 
  rename("temp" = "tpctemp")

act_plot_data <- actual %>%
  filter(incubator %in% c("14C", "30C")) %>% 
  filter(temp %in% c(14.0, 30.0)) %>%
  select(incubator, temp, flask, mu) %>%
  mutate(temp = factor(temp, levels = c(14, 30), labels = c("14°C", "30°C")))  # Add degree symbol

# Manually define p-values for t-test and Wilcoxon
p_values <- data.frame(
  temp = factor(c("14°C", "30°C"), levels = c("14°C", "30°C")),  
  group1 = "14C",
  group2 = "30C",
  p = c(1.06e-12, 1.295e-06),  # t-test for 14°C, Wilcoxon for 30°C
  label = c("***", "***")  # Significance stars
)

# Plot with manual significance labels
ggplot(act_plot_data, aes(x = incubator, y = mu, fill = incubator)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 1) +  # Scatter points
  facet_wrap(~temp) +  # Separate panels for each temp
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "Growth rate (individual/day)",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823")) +  # Custom colors
  geom_text(data = p_values, aes(x = 1.5, y = max(act_plot_data$mu) + 0.1, label = label), inherit.aes = FALSE)

# rmax --------------------------------------------------------------------
unique_rmax_df <- output_norberg2 %>%
  distinct(rmax, .keep_all = TRUE)


#chat helps
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(rstatix)
library(ggsignif)
library(purrr)  # Needed for transpose()

# Ensure 'incubator' is a factor and filter desired groups
unique_rmax_df <- unique_rmax_df %>%
  mutate(incubator = as.factor(incubator)) %>%
  filter(incubator %in% c("14C", "30C"))

# Perform Games-Howell test
rmax_games_howell_results <- unique_rmax_df %>%
  games_howell_test(rmax ~ incubator) %>%
  mutate(significance = ifelse(p.adj < 0.05, "***", "ns"))  # Convert p-values into stars

# Filter only significant results (those with adjusted p-value < 0.05)
significant_results <- rmax_games_howell_results %>%
  filter(p.adj < 0.05) %>%
  select(group1, group2, significance)

# Define the groups to be included
included_groups <- c("14C", "30C")

# Filter significance results for only included groups
filtered_significant_results <- significant_results %>%
  filter(group1 %in% included_groups & group2 %in% included_groups)

# Create comparison list from the filtered results
comparison_list <- filtered_significant_results %>%
  transmute(comp = map2(group1, group2, ~ c(.x, .y))) %>%
  pull(comp)

# Extract matching annotations
annotations <- filtered_significant_results$significance

# Define staggered y-positions (adjust as needed)
y_positions <- seq(
  from = max(unique_rmax_df$rmax, na.rm = TRUE) + 0.05,
  by = 0.04,
  length.out = length(comparison_list)
)

# Create the plot
plot <- unique_rmax_df %>%
  ggplot(aes(x = incubator, y = rmax, fill = incubator)) + 
  geom_boxplot(alpha = 1, outlier.shape = NA) +  
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 1) +  
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "r max (individual/day)",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", 
                               "30C" = "#bc1823"))

# Add significance annotations
plot + 
  geom_signif(
    comparisons = comparison_list, 
    annotations = annotations,
    tip_length = 0.02, 
    textsize = 6,
    y_position = y_positions
  )
# T breadth ---------------------------------------------------------------
output_norberg3 <- output_norberg2 %>%
  filter(predicted_growth >= 0.5 * rmax) %>% 
  unite("trt", "incubator", "flask", sep = "_") %>% 
  group_by(trt) %>%
  summarise(t_breadth = max(temp) - min(temp), .groups = "drop") %>% 
  separate(col = trt, into = c("incubator", "flask"), sep = "_") %>% 
  filter(incubator %in% c("14C", "30C"))

output_norberg3 %>%
  ggplot(aes(x = incubator, y = t_breadth, fill = incubator)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +  
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 1) +  
  stat_compare_means(
    comparisons = list(c("14C", "30C")),
    method = "t.test",
    label = "p.signif"
  ) +  
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "T breadth (°C)",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823"))
