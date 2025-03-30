#local adaptation
output_norberg <- read.csv("data/output-norberg.csv")
View(output_norberg)
str(output_norberg)

output_norberg2 <- output_norberg %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_")
View(output_norberg2)

output_norberg2 %>% 
  rename("Treatment" = "incubator") %>% 
  ggplot(aes(temp, predicted_growth, colour = Treatment)) + 
  geom_line() + 
  ylim(0, 1.5) + 
  theme_minimal() + 
  xlab("Temperature") + 
  ylab("Predicted growth") + 
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    strip.text = element_text(face = "bold")  # Makes facet labels (temp) bold
  )
# preds constants at 14 and 30 deg ----------------------------------------------
###14 deg
t14C_14deg <- output_norberg2 %>%
  filter(incubator == "14C", temp == 14.0) %>%
  select(incubator, flask, predicted_growth, topt, rmax, tmax)
shapiro.test(t14C_14deg$predicted_growth) #W = 0.95337, p-value = 0.7451

t30C_14deg <- output_norberg2 %>%
  filter(incubator == "30C", temp == 14.0) %>%
  select(incubator, flask, predicted_growth, topt, rmax, tmax)
shapiro.test(t30C_14deg$predicted_growth) #W = 0.97458, p-value = 0.9313
#normal data --> t test

t.test(t14C_14deg$predicted_growth, t30C_14deg$predicted_growth) #p-value = 3.151e-10
t.test(t14C_14deg$predicted_growth, t30C_14deg$predicted_growth, alternative = "greater") #p-value = 1.575e-10
#preds: 14C is sig greater than 30C at 14 deg

###30 deg
t14C_30deg <- output_norberg2 %>%
  filter(incubator == "14C", temp == 30.0) %>%
  select(incubator, flask, predicted_growth, topt, rmax, tmax)
shapiro.test(t14C_30deg$predicted_growth) #W = 0.82771, p-value = 0.05619

t30C_30deg <- output_norberg2 %>%
  filter(incubator == "30C", temp == 30.0) %>%
  select(incubator, flask, predicted_growth, topt, rmax, tmax)
shapiro.test(t30C_30deg$predicted_growth) #W = 0.97289, p-value = 0.9196
#normal data --> t test

t.test(t14C_30deg$predicted_growth, t30C_30deg$predicted_growth) #p-value = 2.662e-06
t.test(t14C_30deg$predicted_growth, t30C_30deg$predicted_growth, alternative = "greater") #p-value = 1.331e-06
#preds: no sig diff between 14C and 30C at 30 deg

#graphing with chat
# Combine the data for plotting
library(ggpubr)
# Load required package
library(ggpubr)

# Prepare data
preds_plot_data <- output_norberg2 %>%
  filter(incubator %in% c("14C", "30C")) %>% 
  filter(temp %in% c(14.0, 30.0)) %>%
  select(incubator, temp, flask, predicted_growth) %>%
  mutate(temp = factor(temp, levels = c(14, 30)))  # Ensure proper ordering

# Define comparisons for significance testing at each temperature separately
preds_comparisons <- list(
  c("14C", "30C")  # Compare 14C vs. 30C at each temp
)

# Plot
ggplot(preds_plot_data, aes(x = temp, y = predicted_growth, fill = incubator)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplot without outliers
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +  # Scatter points
  stat_compare_means(aes(group = incubator), 
                     comparisons = preds_comparisons, 
                     method = "t.test", 
                     label = "p.signif") +  # Adds significance stars
  theme_minimal() +
  labs(
    x = "Temperature (°C)",
    y = "Predicted Growth",
    fill = "Treatment"
  ) +
  scale_fill_manual(values = c("14C" = "#145da0", "30C" = "#bc1823")) +  # Custom colors
  facet_wrap(~temp)  # Facet by temperature to compare within each temp

# Load required package
library(ggpubr)

# Prepare data
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


# actual constants at 14 deg and 30 deg -----------------------------------
actual <- read.csv("data/growthtool-gdat-sum.csv")

actual <- actual %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_") %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp")) %>% 
  unite("unique_well", "plate", "well", sep = "_") %>% 
  rename("temp" = "tpctemp")

###14 deg
act_t14C_14deg <- actual %>%
  filter(incubator == "14C", temp == 14.0) %>%
  select(incubator, flask, mu)
act_t14C_14deg$mu <- as.numeric(act_t14C_14deg$mu)
shapiro.test(act_t14C_14deg$mu) #W = 0.96331, p-value = 0.5083

act_t30C_14deg <- actual %>%
  filter(incubator == "30C", temp == 14.0) %>%
  select(incubator, flask, mu)
act_t30C_14deg$mu <- as.numeric(act_t30C_14deg$mu)
shapiro.test(act_t30C_14deg$mu) #W = 0.93776, p-value = 0.1454

t.test(act_t14C_14deg$mu, act_t30C_14deg$mu) #p-value = 2.12e-12
t.test(act_t14C_14deg$mu, act_t30C_14deg$mu, alternative = "greater") #p-value = 1.06e-12
#act: 14C is sig greater than 30C at 14 deg

###30 deg
act_t14C_30deg <- actual %>%
  filter(incubator == "14C", temp == 30.0) %>%
  select(incubator, flask, mu)
act_t14C_30deg$mu <- as.numeric(act_t14C_30deg$mu)
shapiro.test(act_t14C_30deg$mu) #W = 0.96316, p-value = 0.505

act_t30C_30deg <- actual %>%
  filter(incubator == "30C", temp == 30.0) %>%
  select(incubator, flask, mu)
act_t30C_30deg$mu <- as.numeric(act_t30C_30deg$mu)
shapiro.test(act_t30C_30deg$mu) #W = 0.8792, p-value = 0.00801
#not normally distributed

#Wilcox
wilcox.test(act_t14C_30deg$mu, act_t30C_30deg$mu) #W = 507, p-value = 1.295e-06
#30 different

#graphing with chat
# Prepare data
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
