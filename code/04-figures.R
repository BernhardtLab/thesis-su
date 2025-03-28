#figures!
output_norberg <- read.csv("data/output-norberg.csv")
output_norberg2 <- output_norberg %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_")

# tpc ---------------------------------------------------------------------
output_norberg2 %>% 
  rename(Treatment = incubator, 
         Growth = predicted_growth, 
         Temperature = temp) %>% 
  ggplot(aes(Temperature, Growth, colour = flask)) + 
  geom_line(size = 0.1) + 
  ylim(0, 1.5) + 
  theme_minimal() + 
  facet_wrap(~Treatment)

output_norberg2 %>% 
  rename(Treatment = incubator, 
         Growth = predicted_growth, 
         Temperature = temp) %>% 
  ggplot(aes(Temperature, Growth, colour = Treatment)) + 
  geom_point(size = 0.01) + 
  ylim(0, 1.5) + 
  theme_minimal() 

# local adaptation --------------------------------------------------------
#preds
preds_plot_data <- output_norberg2 %>%
  filter(incubator %in% c("14C", "30C")) %>% 
  filter(temp %in% c(14.0, 30.0)) %>%
  select(incubator, temp, flask, predicted_growth) %>%
  mutate(temp = factor(temp, levels = c(14, 30)))  # Ensure proper ordering

# Manually define p-values
p_values <- data.frame(
  temp = factor(c(14, 30)),  # Ensure matching factor levels
  group1 = "14C",
  group2 = "30C",
  p = c(1.575e-10, 0.5),  # Your t-test results
  label = c("***", "ns")  # Significance stars
)

# Plot with manual significance labels
ggplot(preds_plot_data, aes(x = incubator, y = predicted_growth, fill = incubator)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +  # Scatter points
  facet_wrap(~temp) +  # Separate panels for each temp
  theme_minimal() +
  labs(
    x = "Incubator",
    y = "Predicted growth",
    fill = "Incubator"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823")) +  # Custom colors
  geom_text(data = p_values, aes(x = 1.5, y = max(preds_plot_data$predicted_growth) + 0.1, label = label), inherit.aes = FALSE)

#actual
act_plot_data <- actual %>%
  filter(incubator %in% c("14C", "30C")) %>% 
  filter(temp %in% c(14.0, 30.0)) %>%
  select(incubator, temp, flask, mu) %>%
  mutate(temp = factor(temp, levels = c(14, 30)))  # Ensure proper ordering

# Manually define p-values for t-test and Wilcoxon
p_values <- data.frame(
  temp = factor(c(14, 30)),  # Ensure matching factor levels
  group1 = "14C",
  group2 = "30C",
  p = c(1.06e-12, 1.295e-06),  # t-test for 14°C, Wilcoxon for 30°C
  label = c("***", "***")  # Significance stars
)

# Plot with manual significance labels
ggplot(act_plot_data, aes(x = incubator, y = mu, fill = incubator)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +  # Scatter points
  facet_wrap(~temp) +  # Separate panels for each temp
  theme_minimal() +
  labs(
    x = "Incubator",
    y = "Predicted growth",
    fill = "Incubator"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823")) +  # Custom colors
  geom_text(data = p_values, aes(x = 1.5, y = max(act_plot_data$mu) + 0.1, label = label), inherit.aes = FALSE)


# T breadth shaded in -----------------------------------------------------
output_norberg3 <- output_norberg2 %>%
  filter(predicted_growth >= 0.5 * rmax) %>% 
  unite("trt", "incubator", "flask", sep = "_") %>% 
  group_by(trt) %>%
  summarise(t_breadth = max(temp) - min(temp), .groups = "drop") %>% 
  separate(col = trt, into = c("incubator", "flask"), sep = "_")

output_norberg2 %>% 
  rename(Treatment = incubator, 
         Growth = predicted_growth, 
         Temperature = temp, 
         rmax = rmax) %>%  # Rename columns
  ggplot(aes(Temperature, Growth, colour = flask)) +  
  # Line graph with black color for all lines
  geom_line(size = 0.1, colour = "black") +  
  # Shade the area under the curve where Growth >= 0.5 * rmax
  geom_ribbon(aes(ymin = 0, ymax = ifelse(Growth >= 0.5 * rmax, Growth, NA)), 
              fill = "blue", alpha = 0.3) +  # Blue shade with some transparency
  ylim(0, 1.5) +  # Limit the y-axis from 0 to 1.5 for Growth
  theme_minimal() +  # Use a minimal theme
  facet_wrap(~Treatment)  # Facet by Treatment (e.g., 14C, 30C)

output_norberg2 %>% 
  rename(Treatment = incubator, 
         Growth = predicted_growth, 
         Temperature = temp, 
         rmax = rmax) %>%  # Rename columns
  ggplot(aes(Temperature, Growth, colour = flask, fill = flask)) +  
  # Lines will be black
  geom_line(size = 0.1, colour = "black") +  
  # Shade the area under the curve where Growth >= 0.5 * rmax with flask-specific colors
  geom_ribbon(aes(ymin = 0, ymax = ifelse(Growth >= 0.5 * rmax, Growth, NA)),
              alpha = 0.5) +  # Adjust transparency for shading
  # Define unique colors for each flask
  scale_fill_manual(values = c("flask1" = "#FF6347",  # Tomato
                               "flask2" = "#4682B4",  # SteelBlue
                               "flask3" = "#32CD32",  # LimeGreen
                               "flask4" = "#FFD700",  # Gold
                               "flask5" = "#8A2BE2",  # BlueViolet
                               "flask6" = "#A52A2A",  # Brown
                               "flask7" = "#D2691E",  # Chocolate
                               "flask8" = "#C71585")) + # MediumVioletRed
  ylim(0, 1.5) +  # Limit the y-axis from 0 to 1.5 for Growth
  theme_minimal() +  # Use a minimal theme
  facet_wrap(~Treatment)  # Facet by Treatment (e.g., 14C, 30C)

output_norberg2 %>% 
  rename(Treatment = incubator, 
         Growth = predicted_growth, 
         Temperature = temp, 
         rmax = rmax) %>%  # Rename columns
  ggplot(aes(Temperature, Growth)) +  
  # Lines will be black for all flasks
  geom_line(size = 0.1) +  
  # Shade the area under the curve where Growth >= 0.5 * rmax, use flask colors
  geom_ribbon(aes(ymin = 0, ymax = ifelse(Growth >= 0.5 * rmax, Growth, NA), fill = "flask"),
              alpha = 0.5) +  
  ylim(0, 1.5) +  # Limit the y-axis from 0 to 1.5 for Growth
  theme_minimal() +  # Use a minimal theme
  facet_wrap(~Treatment)  # Facet by Treatment (e.g., 14C, 30C)

output_norberg2 %>% 
  rename(Treatment = incubator,
         Temperature = temp, 
         rmax = rmax) %>%  # Rename columns
  ggplot(aes(Temperature, predicted_growth)) +  
  # Lines will be black for all flasks
  geom_line(aes(colour = "black"), size = 0.1) +  
  # Shade the area under the curve where Growth >= 0.5 * rmax, use flask colors
  geom_ribbon(data = subset(output_norberg2, predicted_growth >= 0.5 * rmax), 
              aes(x = temp, ymax = predicted_growth, fill = flask), 
              ymin = 0, alpha = 0.3) +  # Adjust transparency for shading
  # Define unique colors for each flask for shading
  scale_fill_manual(name = '', values = c("flask1" = "#FF6347",  # Tomato
                                          "flask2" = "#4682B4",  # SteelBlue
                                          "flask3" = "#32CD32",  # LimeGreen
                                          "flask4" = "#FFD700",  # Gold
                                          "flask5" = "#8A2BE2",  # BlueViolet
                                          "flask6" = "#A52A2A",  # Brown
                                          "flask7" = "#D2691E",  # Chocolate
                                          "flask8" = "#C71585")) + # MediumVioletRed
  ylim(0, 1.5) +  # Limit the y-axis from 0 to 1.5 for Growth
  theme_minimal() +  # Use a minimal theme
  facet_wrap(~Treatment)  # Facet by Treatment (e.g., 14C, 30C)

output_norberg2 %>% 
  rename(Treatment = incubator, 
         Growth = predicted_growth, 
         Temperature = temp, 
         rmax = rmax) %>%  # Rename columns
  ggplot(aes(Temperature, Growth, colour = flask, fill = flask)) +  
  # Lines will be black
  geom_line(size = 0.1, colour = "black") +  
  # Shade the area under the curve where Growth >= 0.5 * rmax with flask-specific colors
  geom_ribbon(aes(ymin = 0, ymax = ifelse(Growth >= 0.5 * rmax, Growth, NA), fill = flask),
              alpha = 0.5, show.legend = FALSE) +  # Adjust transparency for shading, hide legend for fill
  ylim(0, 1.5) +  # Limit the y-axis from 0 to 1.5 for Growth
  theme_minimal() +  # Use a minimal theme
  facet_wrap(~Treatment)  # Facet by Treatment (e.g., 14C, 30C)

#so far unsuccesful in getting final figure
################################################################################
output_norberg3 %>%
  ggplot(aes(x = incubator, y = t_breadth, fill = incubator)) +
  # Create a boxplot
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  
  # Add scatter points for better visualization
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +  
  # Add Tukey's HSD test results with significance stars
  stat_compare_means(aes(group = incubator), comparisons = list(c("14C", "30C"), c("14C", "48F"), c("30C", "6F")), 
                     method = "t.test", label = "p.signif") +  
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "T breadth (°C)",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823", "6F" = "#ff8210", "48F" = "#800080"))  # Customize colors

# r max -------------------------------------------------------------------
output_norberg2 %>%
  ggplot(aes(x = incubator, y = rmax, fill = incubator)) +
  # Create a boxplot
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  
  # Add scatter points for better visualization
  geom_jitter(position = position_jitterdodge(jitter.width = 0), alpha = 0.5) +  
  # Add Tukey's HSD test results with significance stars
  stat_compare_means(aes(group = incubator), comparisons = list(c("14C", "30C"), c("14C", "48F"), c("30C", "6F")), 
                     method = "t.test", label = "p.signif") +  
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "r max",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823", "6F" = "#ff8210", "48F" = "#800080"))  # Customize colors

# tradeoff ----------------------------------------------------------------
unique_rmax_df <- output_norberg2 %>%
  distinct(rmax, .keep_all = TRUE)
tradeoff_df <- left_join(output_norberg3, unique_rmax_df)

tradeoff_df %>% 
  ggplot(aes(rmax, t_breadth, colour = incubator)) + 
  geom_point() +
  theme_minimal() + 
  labs(
    x = "r max",
    y = "T breadth",
    colour = "Treatment")


# T opt -------------------------------------------------------------------
unique_topt_df %>% 
  ggplot(aes(x = incubator, y = topt, fill = incubator)) + 
  # Create a boxplot
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  
  # Add scatter points for better visualization
  geom_jitter(position = position_jitterdodge(jitter.width = 0), alpha = 0.5) +  
  # Add significance annotation for the 30C vs. 14C difference
  geom_signif(comparisons = list(c("30C", "14C")), 
              annotations = "*",  # Or "**" for p < 0.01
              tip_length = 0.02, 
              textsize = 6) +
  # Basic theme and labels
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "T opt",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823", "6F" = "#ff8210", "48F" = "#800080"))

# T max -------------------------------------------------------------------
unique_tmax_df %>% 
  ggplot(aes(x = incubator, y = tmax, fill = incubator)) + 
  # Create a boxplot
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  
  # Add scatter points for better visualization
  geom_jitter(position = position_jitterdodge(jitter.width = 0), alpha = 0.5) +  
  # Since ANOVA showed no significant differences, we do not add significance stars
  # Add a basic theme and labels
  theme_minimal() +
  labs(
    x = "Treatment",
    y = "T max",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823", "6F" = "#ff8210", "48F" = "#800080"))  # Customize colors

# fluctuation period ---------------------------------------------------------
#copying over the df making#####################################################
output_norberg_f <- output_norberg2 %>%
  # Separate the 'incubator' column into 'num' and 'chr'
  mutate(
    num = substr(incubator, 1, nchar(incubator) - 1),  # Everything but the last character
    chr = substr(incubator, nchar(incubator), nchar(incubator))  # Last character
  ) %>%
  # Create the 'period_fluctuation' column based on 'chr'
  mutate(
    period_fluctuation = case_when(
      chr == "C" ~ "inf",  # If 'chr' is "C", set "inf"
      chr == "F" ~ as.character(num),  # If 'chr' is "F", set 'num' value
      TRUE ~ NA_character_  # For other cases (optional), set NA or other default
    )
  )
# bring in T breadth
output_norberg_f3 <- output_norberg_f %>% 
  filter(predicted_growth >= 0.5 * rmax) %>% 
  unite("trt", "incubator", "flask", sep = "_") %>% 
  group_by(trt) %>%
  summarise(t_breadth = max(temp) - min(temp), .groups = "drop") %>% 
  separate(col = trt, into = c("incubator", "flask"), sep = "_")

output_norberg_f4 <- left_join(output_norberg_f, output_norberg_f3)
View(output_norberg_f4)

output_norberg_f4 %>% 
  ggplot(aes(x = period_fluctuation, y = t_breadth, colour = incubator)) + 
  geom_point() + 
  theme_minimal() + 
  labs(
    x = "Fluctuation period (hours)",
    y = "T breadth",
    colour = "Treatment")

output_norberg_f4 %>% 
  ggplot(aes(x = period_fluctuation, y = rmax, colour = incubator)) + 
  geom_point() + 
  theme_minimal() + 
  labs(
    x = "Fluctuation period (hours)",
    y = "r max",
    colour = "Treatment")

output_norberg_f4 %>% 
  ggplot(aes(x = period_fluctuation, y = topt, colour = incubator)) + 
  geom_point() + 
  theme_minimal() + 
  labs(
    x = "Fluctuation period (hours)",
    y = "T opt",
    colour = "Treatment")
