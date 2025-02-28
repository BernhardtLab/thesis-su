#Sveta Uzhova
#acclimation W25
#extracting growth rates

#packages-----------------------------------------------------------------------
library(tidyverse)
library(broom)
library(lubridate)
library(minpack.lm)
library(rootSolve)

#data---------------------------------------------------------------------------
all_rfu <- read_csv("data/tpc_processed_all_rfus.csv") 
View(all_rfu)

a2 <- all_rfu %>%
  group_by(unique_well, temp) %>%
  mutate(start_time = min(date_time)) %>%
  mutate(days = interval(start_time, date_time)/ddays(1)) %>%
  filter(treatment != "na") %>%
  mutate(measured = case_when(temp == 12 & days > 1 & days < 8 ~ "yes",
                              temp == 14 & days < 8 ~ "yes",
                              temp == 20 & days < 6.5 ~ "yes",
                              temp == 25 & days < 6.5 ~ "yes",
                              temp == 30 & days < 6 ~ "yes",
                              temp == 36 & days < 6.5 ~ "yes",
                              temp == 38 & days > 0.5 & days < 2 ~ "yes",
                              temp == 42 & days > 0 & days < 2 ~ "yes",
                              TRUE  ~ "no")) %>%
  filter(measured == "yes")
View(a2) #need to change the days

#ctrl shift c for #

all_rfu %>%
  group_by(unique_well, temp) %>%
  filter(temp == 42) %>%
  mutate(start_time = min(date_time)) %>%
  mutate(days = interval(start_time, date_time)/ddays(1)) %>%
  ggplot(aes(x = days, y = RFU, group = unique_well, color = temp_treatment)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ temp_treatment, scales = "free")

a2 %>%
  ggplot(aes(x = days, y = RFU, group = unique_well, color = temp_treatment)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ temp, scales = "free")

x = 1

##Code to determine which points to include to capture exponential growth
fitting_window_log_linear <- function(x) {
  growth_rates <- a2 %>% 
    group_by(unique_well, temp, temp_treatment) %>% 
    top_n(n = -x, wt = days) %>% 
    do(tidy(lm(log(RFU) ~ days, data = .))) %>% 
    mutate(number_of_points = x) %>% 
    ungroup()
}

#17 windows from read 0 to read 16 -- need to change
windows <- seq(1,9, by = 1)

multi_fits <- windows %>% 
  map_df(fitting_window_log_linear, .id = "iteration")

##Plotting to visualize number of points 
multi_fits %>% 
  filter(term == "days") %>% 
  ggplot(aes(x = number_of_points, y = estimate, group = unique_well, color = temp_treatment)) + geom_point() + geom_line() +
  facet_wrap( ~ temp, scales = "free")

#Filtering to select for days
exp_fits_top <- multi_fits %>%
  filter(term == "days") %>%
  group_by(unique_well, temp, temp_treatment) %>% 
  top_n(n = 1, wt = estimate) %>% 
  select("temp_treatemnt" != blank) #?

##Plotting
exp_fits_top %>% 
  ggplot(aes(x = temp, y = estimate, group = unique_well, color = temp_treatment)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("exponential fits")
View(exp_fits_top) #here, check for presence of al tpc test temps

exp_fits_top %>% 
  filter(temp_treatment %in% c("6F", "24F", "48F")) %>% 
  ggplot(aes(x = temp, y = estimate, group = unique_well, color = temp_treatment)) + 
  geom_point() + 
  ggtitle("fluctuating point")

#saving-------------------------------------------------------------------------
write_csv(exp_fits_top, "data/growth-sequential-fits.csv")


