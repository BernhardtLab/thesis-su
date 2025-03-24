#Sveta Uzhova
#graphing RFU reads

#packages-----------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(janitor)
library(lubridate)
library(cellranger)
library(purrr)
library(fs)
library(here)
library(dplyr)
library(stringr)

#data###########################################################################
#plate layout-------------------------------------------------------------------
plate_setup <- read.csv("plate-layout/final_plate_setup.csv")
#View(plate_setup)
#making it match F24 set up
plate_setup <- plate_setup %>% 
  rename(incubator = treatment) %>% 
  unite("treatment", incubator, flask, sep = "_") %>% 
  select(-number)

#RFUs---------------------------------------------------------------------------
RFUs <- c(list.files("data/raw_rfus", full.names = T, recursive = TRUE))
RFUs <- RFUs[!grepl("\\.xpt$", RFUs)]

names(RFUs) <- RFUs %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = "_su$", replacement = "") %>% 
  gsub(pattern = "data/raw_rfus", replacement = "")

all_plates <- map_df(RFUs, read_excel, range = "B30:N38", .id = "file_name") %>% 
  rename(row = `...1`)
#View(all_plates)

#getting times------------------------------------------------------------------
all_times <- map_df(RFUs, read_excel, range = "A6:B8", .id = "file_name") %>% 
  clean_names() %>%
  rename(data = plate_1) 
#str(all_times$data)

all_times <- all_times %>% 
  filter(!is.na(data)) %>% 
  spread(key = plate_number, value = data) %>% 
  separate(Time, into = c("crap", "Time"), sep = " ") %>% 
  select(-crap) 

all_times <- map_df(RFUs, read_excel, range = "A6:B8", .id = "file_name") %>% 
  clean_names() %>%
  rename(data = plate_1) %>% 
  filter(!is.na(data)) %>% 
  spread(key = plate_number, value = data) %>% 
  separate(Time, into = c("crap", "Time"), sep = " ") %>% 
  select(-crap) %>% 
  separate(file_name, into = c("nothing", "crap2", "other"), sep = "/", remove = FALSE) %>% 
  select(-nothing) %>% 
  select(-crap2) %>% 
  select(-plate_2) %>% 
  separate(other, into = c("plate", "temp", "read"), sep = "_", remove = F) %>% 
  select(-other)
#View(all_times)

#file name, time, temp, RFUs----------------------------------------------------
all_plates2 <- left_join(all_plates, all_times, by = "file_name")
#View(all_plates2)

#get well id and combine into long format
all_temp_rfus <- all_plates2 %>% 
  gather(key = column, value = RFU, 3:14) %>%
  unite(row, column, col = "well", remove = FALSE, sep = "") %>% 
  mutate(column = formatC(column, width = 2, flag = 0))

all_temp_rfus$plate_number <- str_sub(all_temp_rfus$plate, -1)

all_temp_rfus <- all_temp_rfus %>% 
  unite(plate_number, well, col = "well_id", remove = F, sep = "_")

#View(all_temp_rfus)

#bringing in trt
plate_setup <- plate_setup %>% 
  unite(plate, well, col = "well_id", remove = F, sep = "_") %>% 
  separate(treatment, into = c("temp_treatment", "replicate_no"), sep = "_", remove = F)
#View(plate_setup)

all_rfus_raw <- inner_join(all_temp_rfus, plate_setup, by = c("well_id"))

#cleaning up df
all_rfus_raw <- all_rfus_raw %>% 
  select(-c(well.x, plate.x, well.y, plate.y, row, column, read, plate_number)) 
#View(all_rfus_raw)

#unite time
all_rfus_2 <- all_rfus_raw %>% 
  unite(col = date_time, Date, Time, sep = " ") %>% 
  mutate(date_time = ymd_hms(date_time)) 
#str(all_rfus_2$date_time)

all_rfus_2 <- all_rfus_2 %>% 
  mutate(unique_well = paste(well_id, temp, sep = "_"))
#View(all_rfus_2)

##make time 0
all_rfus_3 <- all_rfus_2 %>% 
  group_by(unique_well, temp) %>% 
  mutate(start_time = min(date_time)) %>% 
  mutate(days = interval(start_time, date_time)/ddays(1))
#View(all_rfus_3)

#saving-------------------------------------------------------------------------
write_csv(all_rfus_3, "data/tpc_processed_all_rfus.csv")

#graphing-----------------------------------------------------------------------
all_rfus_3 %>% 
  ggplot(aes(x = days, y = RFU, color = temp_treatment, group = well_id)) + 
  geom_line() + 
  facet_wrap( ~ temp, scales = "free") +
  ggtitle("RFUs")

all_rfus_3 %>% 
  rename("Treatment" = "temp_treatment") %>% 
  ggplot(aes(days, RFU, colour = Treatment, group = well_id)) + 
  geom_line() +
  facet_wrap( ~ temp, scales = "free") +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    strip.text = element_text(face = "bold")  # Makes facet labels (temp) bold
  )


# all_rfus_3 %>% 
#   ggplot(aes(x = days, y = RFU, color = temp_treatment, group = well_id)) + 
#   geom_line() + 
#   facet_wrap(~ factor(temp, levels = c(6, 12, 18, 24, 30, 36, 42, 46)), scales = "free")
# 
# all_rfus_3 %>% 
#   filter(temp_treatment == "blank") %>% 
#   ggplot(aes(x = days, y = RFU)) + 
#   geom_point() + 
#   facet_wrap( ~ temp, scales = "free") +
#   ggtitle("blanks")
# 
# all_rfus_3 %>% 
#   filter(temp_treatment == "blank") %>% 
#   filter(RFU > 300) %>% 
#   View()