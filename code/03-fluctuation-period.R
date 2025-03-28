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
View(output_norberg_f)

# bring in T breadth ------------------------------------------------------
output_norberg_f3 <- output_norberg_f %>% 
  filter(predicted_growth >= 0.5 * rmax) %>% 
  unite("trt", "incubator", "flask", sep = "_") %>% 
  group_by(trt) %>%
  summarise(t_breadth = max(temp) - min(temp), .groups = "drop") %>% 
  separate(col = trt, into = c("incubator", "flask"), sep = "_") #same as output_norberg_3

output_norberg_f4 <- left_join(output_norberg_f, output_norberg_f3)

# analysis ----------------------------------------------------------------
###T breadth####################################################################
output_norberg_f4 %>% 
  ggplot(aes(period_fluctuation, t_breadth, colour = incubator)) + geom_point()

#weirdness with normality tetsing, so taking just the t breadth and separating that df to avoid many values of the same thing
out_nor_f5 <- output_norberg_f3 %>% 
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
View(out_nor_f5)

#test for normality
out_nor_f5 %>%
  filter(period_fluctuation == "6") %>% 
  pull(t_breadth) %>%  
  shapiro.test()#W = 0.97224, p-value = 0.9149 #matches line 163 of 03-stat-comaprsions
out_nor_f5 %>%
  filter(period_fluctuation == "48")%>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.97713, p-value = 0.9473 #matches
out_nor_f5 %>%
  filter(period_fluctuation == "inf")%>% 
  pull(t_breadth) %>%  
  shapiro.test() #W = 0.98023, p-value = 0.9654

leveneTest(t_breadth ~ period_fluctuation, data = out_nor_f5) #p = 0.01189 
#variances are sig diff

#KW
kruskal.test(t_breadth ~ period_fluctuation, data = out_nor_f5)
#Kruskal-Wallis chi-squared = 2.2022, df = 2, p-value = 0.3325

#Welch ANOVA
oneway.test(t_breadth ~ period_fluctuation, data = out_nor_f5, var.equal = FALSE)
#F = 2.2601, num df = 2.000, denom df = 17.182, p-value = 0.1345

# r max -------------------------------------------------------------------
#make df with unique values only
out_nor_f5 <- out_nor_f5 %>% 
  unite("trt", "incubator", "flask", sep = "_")
output_norberg_f <- output_norberg_f %>% 
  unite("trt", "incubator", "flask", sep = "_")

out_nor_6 <- out_nor_f5 %>%
  left_join(output_norberg_f %>% distinct(trt, .keep_all = TRUE), by = "trt") %>% 
  rename("period_fluctuation" = "period_fluctuation.x")
View(out_nor_6)

#test for normality
out_nor_6 %>%
  filter(period_fluctuation == "6") %>% 
  pull(rmax) %>%  
  shapiro.test()#W = 0.90803, p-value = 0.3404 #matches line 236 of 03-stat-comaprsions
out_nor_6 %>%
  filter(period_fluctuation == "48")%>% 
  pull(rmax) %>%  
  shapiro.test() #W = 0.89391, p-value = 0.2544 #matches
out_nor_6 %>%
  filter(period_fluctuation == "inf")%>% 
  pull(rmax) %>%  
  shapiro.test() #W = 0.91591, p-value = 0.1449

leveneTest(rmax ~ period_fluctuation, data = out_nor_6) #p = 0.001336 

#KW
kruskal.test(rmax ~ period_fluctuation, data = out_nor_6)
#Kruskal-Wallis chi-squared = 6.3203, df = 2, p-value = 0.04242

#Welch ANOVA
oneway.test(rmax ~ period_fluctuation, data = out_nor_6, var.equal = FALSE)
#F = 5.4761, num df = 2.000, denom df = 18.912, p-value = 0.0133

#post hoc for Welch: Games Howell
rmax_games_howell_results <- out_nor_6 %>%
  games_howell_test(rmax ~ period_fluctuation)
View(rmax_games_howell_results)
#48 has a significantly higher rmax than inf (p = 0.009, estimate = 0.0776)
