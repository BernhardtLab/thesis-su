#Joey finished tpc! now doing stat comparisons
output_norberg <- read.csv("data/output-norberg.csv")
View(output_norberg)
str(output_norberg)

output_norberg2 <- output_norberg %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_")
View(output_norberg2)

# constants at 14 and 30 deg ----------------------------------------------
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

t.test(t14C_30deg$predicted_growth, t30C_30deg$predicted_growth) #p-value = 1
t.test(t14C_30deg$predicted_growth, t30C_30deg$predicted_growth, alternative = "greater") #p-value = 0.5
#preds: no sig diff between 14C and 30C at 30 deg


# comparing preds and actual gr -------------------------------------------
actual <- read.csv("data/growthtool-gdat-sum.csv")

actual <- actual %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_") %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp")) %>% 
  unite("unique_well", "plate", "well", sep = "_")

act_t14C_14deg <- actual %>% 
  filter(incubator == "14C", tpctemp == "14")
act_t14C_14deg$tpctemp <- as.numeric(act_t14C_14deg$tpctemp)
shapiro.test(act_t14C_14deg$mu) #W = 0.96331, p-value = 0.5083

t.test(t14C_14deg$predicted_growth, act_t14C_14deg$mu) #t = -20.59, df = 23.165, p-value < 2.2e-16
#14C at 14 deg preds and real sig diff!

act_t30C_30deg <- actual %>% 
  filter(incubator == "30C", tpctemp == "30")
act_t30C_30deg$tpctemp <- as.numeric(act_t30C_30deg$tpctemp)
shapiro.test(act_t30C_30deg$mu) #W = 0.8792, p-value = 0.00801 #not normal


# getting t breadth -------------------------------------------------------
output_norberg3 <- output_norberg2 %>%
  filter(predicted_growth >= 0.5 * rmax) %>% 
  unite("trt", "incubator", "flask", sep = "_") %>% 
  group_by(trt) %>%
  summarise(t_breadth = max(temp) - min(temp), .groups = "drop") %>% 
  separate(col = trt, into = c("incubator", "flask"), sep = "_")

output_norberg3 %>% 
  ggplot(aes(incubator, t_breadth)) + geom_point()

#chat helps
output_norberg3_ci <- output_norberg2 %>%
  filter(predicted_growth >= 0.5 * rmax) %>% 
  unite("trt", "incubator", "flask", sep = "_") %>% 
  group_by(trt) %>%
  summarise(t_breadth = max(temp) - min(temp), .groups = "drop") %>%
  separate(col = trt, into = c("incubator", "flask"), sep = "_") %>%
  group_by(incubator) %>%
  summarise(
    mean_breadth = mean(t_breadth),
    se_breadth = sd(t_breadth) / sqrt(n()),
    ci_lower = mean_breadth - qt(0.975, df = n() - 1) * se_breadth,
    ci_upper = mean_breadth + qt(0.975, df = n() - 1) * se_breadth,
    .groups = "drop"
  )

output_norberg3_ci %>%
  ggplot(aes(x = incubator, y = mean_breadth)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) + 
  labs(x = "Incubator", y = "Thermal Breadth", title = "Thermal Breadth by Incubator with Confidence Intervals") +
  theme_minimal()

#test for normality
shapiro.test(output_norberg3$t_breadth) #W = 0.99237, p-value = 0.9978

output_norberg3 %>% 
  filter(incubator == "14C") %>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.98135, p-value = 0.9693
output_norberg3 %>% 
  filter(incubator == "30C") %>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.96121, p-value = 0.8216
output_norberg3 %>% 
  filter(incubator == "6F") %>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.97224, p-value = 0.9149
output_norberg3 %>% 
  filter(incubator == "48F") %>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.97713, p-value = 0.9473
#all normal

#ANOVA
#test for variance
library(car)
leveneTest(t_breadth ~ incubator, data = output_norberg3)
#variances are assumed to be homogeneous

tbread_anova <- aov(t_breadth ~ incubator, data = output_norberg3)
summary(tbread_anova)
# The p-value (8.04e-05) is highly significant (much smaller than 0.05), indicating that there is a significant difference in the mean t_breadth across the different incubator groups.
# The F value (10.58) suggests that the between-group variation is significantly larger than the within-group variation

#Tukey's HSD test 
tukey_result <- TukeyHSD(tbread_anova)
print(tukey_result)
#significant p values: 30C-14C, 48F-14C, 6F-30C

#effect sizes for significant pairs
library(rstatix)  # For Cohen's d calculation

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Extract the Tukey results into a dataframe
tukey_df <- as.data.frame(tukey_result$incubator) %>%
  rownames_to_column(var = "comparison") %>%
  filter(`p adj` < 0.05)  # Select only significant pairs

#Cohen's d for significant pairs
#install.packages("rstatix")
library(rstatix)
effect_sizes <- tukey_df %>%
  mutate(
    group1 = sub("-.*", "", comparison),  # Extract first group
    group2 = sub(".*-", "", comparison)   # Extract second group
  ) %>%
  rowwise() %>%
  mutate(
    d = cohens_d(
      output_norberg3 %>% filter(incubator %in% c(group1, group2)), 
      t_breadth ~ incubator
    )$effsize
  ) %>%
  select(comparison, d)

View(effect_sizes)
#Large effects; need work on interpretation


# r max -------------------------------------------------------------------
#check for normality
#shapiro on output_norberg2 was being weird, trouble shooting - making a smaller df with unique r max values
unique_rmax_df <- output_norberg2 %>%
  distinct(rmax, .keep_all = TRUE)

unique_rmax_df %>% 
  filter(incubator == "14C") %>% 
  pull(rmax) %>%  
  shapiro.test() #W = 0.82612, p-value = 0.0540
unique_rmax_df %>% 
  filter(incubator == "30C") %>% 
  pull(rmax) %>%  
  shapiro.test() #W = 0.94799, p-value = 0.6909
unique_rmax_df %>% 
  filter(incubator == "6F") %>% 
  pull(rmax) %>%  
  shapiro.test() #W = 0.90803, p-value = 0.3404
unique_rmax_df %>% 
  filter(incubator == "48F") %>% 
  pull(rmax) %>%  
  shapiro.test() #W = 0.89391, p-value = 0.2544

#ANOVA
#test for variance
leveneTest(rmax ~ incubator, data = unique_rmax_df)
#homogeneity of variances is violated

#Welch’s ANOVA is a robust alternative to ANOVA that does not assume equal variances
oneway.test(rmax ~ incubator, data = unique_rmax_df, var.equal = FALSE)
#F = 15.797, num df = 3.000, denom df = 15.315, p-value = 5.944e-05
#sig diff between r max among incubators

#post hoc: Games Howell
games_howell_results <- unique_rmax_df %>%
  games_howell_test(rmax ~ incubator)
View(games_howell_results)
#significant pairs: 14C-30C, 14C-48F, 14C-6F


# summary table -------------------------------------------------------------------
output_norberg2

rmax_df <- data.frame(output_norberg2$incubator, )

avg_rmax__tmax_df <- output_norberg2 %>%
  group_by(incubator) %>%
  summarise(
    avg_rmax = mean(rmax, na.rm = TRUE), 
    avg_tmax = mean(tmax, na.rm = TRUE))

avg_t_bread <- output_norberg3 %>% 
  group_by(incubator) %>% 
  summarise(
    avg_t_breadth = mean(t_breadth, na.rm = TRUE))

sum_df <- left_join(avg_rmax__tmax_df, avg_t_bread)

