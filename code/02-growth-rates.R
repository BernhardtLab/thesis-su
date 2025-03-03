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
  mutate(measured = case_when(temp == 12 & days > 1 & days < 9 ~ "yes",
                              temp == 14 & days < 9 ~ "yes",
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
  filter(temp == 12) %>%
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

#16 windows from read 0 to read 15
windows <- seq(1,17, by = 1)

multi_fits <- windows %>% 
  map_df(fitting_window_log_linear, .id = "iteration") %>% 
  filter(temp_treatment != "blank")

##Plotting to visualize number of points 
multi_fits %>% 
  filter(term == "days") %>%
  filter(unique_well == "1_D6_12") %>% View
  ggplot(aes(x = number_of_points, y = estimate, group = unique_well, color = temp_treatment)) + geom_point() + geom_line() +
  facet_wrap( ~ temp, scales = "free")

#Filtering to select for days
exp_fits_top <- multi_fits %>%
  filter(term == "days") %>%
  group_by(unique_well, temp, temp_treatment) %>% 
  top_n(n = 1, wt = estimate) 

### look into this one: filter(temp == 12, temp_treatment == "14C", well_id == "1_D6")


##Plotting
exp_fits_top %>% 
  ggplot(aes(x = temp, y = estimate, group = unique_well, color = temp_treatment)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("exponential fits")
View(exp_fits_top) #here, check for presence of all tpc test temps - yes

exp_fits_top %>% 
  filter(temp_treatment %in% c("6F", "48F")) %>% 
  ggplot(aes(x = temp, y = estimate, group = unique_well, color = temp_treatment)) + 
  geom_point() + 
  ggtitle("fluctuating point")

#saving-------------------------------------------------------------------------
#write_csv(exp_fits_top, "data/growth-sequential-fits.csv")

##Selecting temp, rep, number_of_points columns and discarding the rest, making rep column characters
e2 <- exp_fits_top %>%
  select(unique_well, temp, temp_treatment, number_of_points)

##Joining a2 and e2 by temp and population
a3 <- left_join(a2, e2)

View(a3)

##Splitting the grouped data frame into a list of data frames, where each data frame corresponds 
##to one unique combination of rep and temp.
##Need to group split to get correct time points in the next step
abundances_split <- a2 %>%
  arrange(days) %>%
  group_by(unique_well, temp) %>%  
  group_split() 

##Each row's original position in the data frame will be recorded in a new column called time_point. 
##This tracks the sequence of the rows.
p2 <- abundances_split %>% 
  map_df(rownames_to_column, var = "time_point") 

a4 <- left_join(p2, e2)

a5 <- a4 %>% 
  group_by(unique_well) %>% 
  filter(days == 0) %>% 
  mutate(N0 = RFU) %>% 
  select(N0, unique_well)
#a5 doesn't have any unique wells ending in 46 --> no 46 deg
#because of filter(days == 0)


a5b <- all_rfu %>% 
  group_by(unique_well) %>% 
  filter(days == 0) %>% 
  mutate(N0 = RFU) %>% 
  select(N0, unique_well)

a6 <- inner_join(a4, a5b)
#a6 %>% ggplot(aes(temp, N0, group = unique_well, colour = temp_treatment)) + geom_point()
a6 %>% filter(temp == 42) %>% View

##Only including time points that are less than the number of points
a7 <-  a6 %>% 
  mutate(time_point = as.numeric(time_point)) %>% 
  filter(time_point <= number_of_points) ### it's looking like this filtering step isn't quite working as we had hoped -- it's keeping some time points for the 14C treatment that have clearly passed their exponential phase. So, we need to go back and figure out how to trim those out.
View(a7)

a7 %>% 
  # filter(temp == 30) %>% 
  ggplot(aes(x = days, y = RFU, group = unique_well, color = temp_treatment)) + geom_point() + geom_line() + 
  facet_wrap( ~ temp, scales = "free")  

write_csv(a7, "data/growth-estimates.csv")


a7 %>% 
  filter(temp == 12, temp_treatment == "14C", well_id == "1_D6", unique_well == "1_D6_12") %>% 
  ggplot(aes(x = days, y = RFU, group = unique_well), color = "blue") + geom_point() + geom_line() + 
  facet_wrap( ~ temp, scales = "free")  



##Fitting the model
results <- a7 %>% 
  group_by(unique_well, temp, temp_treatment, treatment) %>% #added treatemnt to have ind flask
  do(tidy(nlsLM(RFU ~ N0 * exp(r*days),
                data= .,  start=list(r=0.1),
                control = nls.control(maxiter=2000, minFactor=1/204800000)))) %>% 
  ungroup() 
write_csv(results, "data/gr-estimate.csv")

##Plotting
results %>% 
  ggplot(aes(x = temp, y = estimate, group = temp_treatment, color = temp_treatment)) + 
  geom_point() + 
  geom_smooth(se=F) + 
  theme_minimal() +
  ggtitle("results?")
