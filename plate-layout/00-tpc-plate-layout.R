#Sveta Uzhova
#playout layout for TPC

#packages-----------------------------------------------------------------------
library(tidyverse)
library(readxl)

#data---------------------------------------------------------------------------
plate <- read_excel("plate-layout/plate_layout.xlsx", sheet = "plate")
assignments <- read_excel("plate-layout/plate_layout.xlsx", sheet = "trt")

final <- left_join(plate, assignments) #joining by random number sequence
final <- final %>% 
  mutate_all(~ replace(., is.na(.), "blank")) #adding blank assignemnts to empty cells = ring of combo

View(final)
write_csv(final, "plate-layout/final_plate_setup.csv")
