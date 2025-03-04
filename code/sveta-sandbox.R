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

windows <- seq(1,17, by = 1)

multi_fits <- windows %>% 
  map_df(fitting_window_log_linear_aug, .id = "iteration") %>% 
  filter(temp_treatment != "blank")

View(multi_fits)