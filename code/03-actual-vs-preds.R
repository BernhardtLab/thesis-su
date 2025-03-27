output_norberg <- read.csv("data/output-norberg.csv")

output_norberg2 <- output_norberg %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_")

# comparing actual vs preds -----------------------------------------------
actual <- read.csv("data/growthtool-gdat-sum.csv")

actual <- actual %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_") %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp")) %>% 
  unite("unique_well", "plate", "well", sep = "_") %>% 
  rename("temp" = "tpctemp")

actual <- actual %>% 
  mutate(tpctemp = as.numeric(tpctemp)) %>%  
  mutate(tpctemp = as.numeric(sprintf("%.1f", tpctemp)))    #unsuccefully made decimal point to match using chat


#chat does for loop
library(dplyr)
library(ggpubr)  

comparison <- data.frame(
  incubator = character(),
  temp = numeric(),
  mu = numeric(),
  predicted_growth = numeric(),
  test_used = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

unique_incubators <- intersect(unique(actual$incubator), unique(output_norberg2$incubator))
actual$temp <- as.numeric(as.character(actual$temp))
output_norberg2$temp <- as.numeric(as.character(output_norberg2$temp))
unique_temps <- intersect(unique(actual$temp), unique(output_norberg2$temp))

for (inc in unique_incubators) {
  actual_inc <- actual %>% filter(incubator == inc)
  output_inc <- output_norberg2 %>% filter(incubator == inc)
  
  for (temp in unique_temps) {
    print(paste("Processing:", inc, "Temp:", temp))  # Debugging print
    
    actual_temp <- actual_inc %>% filter(temp == !!temp)
    output_temp <- output_inc %>% filter(temp == !!temp)
    
    if (nrow(actual_temp) == 0 | nrow(output_temp) == 0) {
      print(paste("Skipping:", inc, "Temp:", temp, "(No data)"))
      next
    }
    
    mu_values <- actual_temp$mu
    growth_values <- output_temp$predicted_growth
    
    if (length(mu_values) < 3 | length(growth_values) < 3) {
      print(paste("Skipping:", inc, "Temp:", temp, "(Not enough data)"))
      next
    }
    
    shapiro_mu <- tryCatch(shapiro.test(head(mu_values, 500))$p.value, error = function(e) NA)
    shapiro_growth <- tryCatch(shapiro.test(head(growth_values, 500))$p.value, error = function(e) NA)
    
    test_result <- tryCatch({
      if (shapiro_mu > 0.05 & shapiro_growth > 0.05) {
        t.test(mu_values, growth_values)
      } else {
        wilcox.test(mu_values, growth_values)
      }
    }, error = function(e) return(list(p.value = NA, method = "Error")))
    
    test_used <- ifelse(is.list(test_result), test_result$method, "Error")
    
    significance <- ifelse(test_result$p.value < 0.05, "Yes", "No")
    
    comparison <- rbind(comparison, data.frame(
      incubator = inc,
      temp = temp,
      mu = shapiro_mu,
      predicted_growth = shapiro_growth,
      test_used = test_used,
      p_value = test_result$p.value,
      significance = significance
    ))
  }
}

View(comparison)
write_csv(comparison, "data/actual_preds_comparison.csv")

#look which are no sig
comparison %>% filter(significance == "No")
#four pairs, all at 42 deg

