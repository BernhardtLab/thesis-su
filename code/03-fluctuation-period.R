#fluctuations
output_norberg <- read.csv("data/output-norberg.csv")

output_norberg2 <- output_norberg %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_")
View(output_norberg2)


# make new df with fluctuation regime -------------------------------------
#chat separates by fluctuation regime
library(dplyr)
library(tidyr)

# Create the new df output_norberg_f
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

# View the resulting df
View(output_norberg_f)

# bring in T breadth ------------------------------------------------------
output_norberg_f3 <- output_norberg_f %>% 
  filter(predicted_growth >= 0.5 * rmax) %>% 
  unite("trt", "incubator", "flask", sep = "_") %>% 
  group_by(trt) %>%
  summarise(t_breadth = max(temp) - min(temp), .groups = "drop") %>% 
  separate(col = trt, into = c("incubator", "flask"), sep = "_")

output_norberg_f4 <- left_join(output_norberg_f, output_norberg_f3)

# analysis ----------------------------------------------------------------
###T breadth####################################################################
output_norberg_f4 %>% 
  ggplot(aes(period_fluctuation, t_breadth, colour = incubator)) + geom_point()

#test for normality
shapiro_t_breadth <- output_norberg_f4 %>%
  group_by(period_fluctuation) %>%
  summarise(
    shapiro_p_value = shapiro.test(t_breadth)$p.value,
    .groups = 'drop'
  ) #all not normal??

tbread6 <- output_norberg_f4 %>%
  filter(period_fluctuation == "6")
shapiro.test(tbread6$t_breadth) #W = 0.91863, p-value < 2.2e-16
#also not normal, but diff p value

tbread48 <- output_norberg_f4 %>%
  filter(period_fluctuation == "48")
shapiro.test(tbread48$t_breadth) #W = 0.92393, p-value < 2.2e-16

output_norberg_f4 %>% 
  filter(period_fluctuation == "6") %>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.91863, p-value < 2.2e-16
output_norberg_f4 %>% 
  filter(period_fluctuation == "48") %>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.92393, p-value < 2.2e-16
output_norberg_f4 %>% 
  filter(period_fluctuation == "inf") %>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.96065, p-value < 2.2e-16

#weirdness, but any way everything is not normal..

#not normal ANOVA --> KW test
#checking assumptions
boxplot(t_breadth ~ period_fluctuation, data = output_norberg_f4, 
        main = "Boxplot of t_breadth by period_fluctuation",
        xlab = "Period Fluctuation", ylab = "t_breadth")
ggplot(output_norberg_f4, aes(x = t_breadth, fill = period_fluctuation)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of t_breadth by period_fluctuation")

library(car)
leveneTest(t_breadth ~ period_fluctuation, data = output_norberg_f4)
#Even with unequal variances, Kruskal-Wallis can still be valid, but the results should be interpreted cautiously. It is a non-parametric test, so it's relatively resistant to some of the assumptions that ANOVA relies on (such as normality and homogeneity of variance).

#KW test
kruskal_tbread <- kruskal.test(t_breadth ~ period_fluctuation, data = output_norberg_f4)
print(kruskal_tbread) #Kruskal-Wallis chi-squared = 374.66, df = 2, p-value < 2.2e-16
#why is p value all same?

