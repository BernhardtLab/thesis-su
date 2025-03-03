

x <- 8

a2 %>% 
  group_by(unique_well, temp, temp_treatment) %>% 
  filter(unique_well == "1_D6_12") %>% 
  top_n(n = -x, wt = days) %>%
  do(tidy(lm(log(RFU) ~ days, data = .))) %>% 
  mutate(number_of_points = x) %>% 
  ungroup()