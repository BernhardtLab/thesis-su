#weird estimates - not cutting off at the highest increase (line145)
augment_model <- function(model, data) {
  augmented_data <- augment(model, data = data)
  return(augmented_data)
}

fitting_window_log_linear_aug <- function(x) {
  growth_rates <- a2 %>% 
    group_by(unique_well, temp, temp_treatment) %>% 
    top_n(n = -x, wt = days) %>% 
    do({
      model <- lm(log(RFU) ~ days, data = .)
      augmented_data <- augment_model(model, data = .)  # Call augment_model to augment the fit
      augmented_data %>%
        mutate(number_of_points = x)  # Add the number of points
    }) %>%
    ungroup()
  
  return(growth_rates)
}

x = 1

windows <- seq(1,19, by = 1)

multi_fits_aug <- windows %>% 
  map_df(fitting_window_log_linear_aug, .id = "iteration") %>% 
  filter(temp_treatment != "blank")

View(multi_fits_aug)

multfit <- left_join(multi_fits, multi_fits_aug)
View(multfit)

multfit %>% 
  filter(temp == 12) %>% 
  filter(unique_well == "1_D6_12") %>% 
  #select(.fitted, estimate) %>% 
  View
#highest estimate 3.88, highest .fitted 8.11

multfit %>% 
  filter(temp == 12) %>% 
  filter(unique_well == "1_D6_12") %>% 
  ggplot(aes(x = days, y = .fitted)) + 
  geom_point() + 
  geom_line()
