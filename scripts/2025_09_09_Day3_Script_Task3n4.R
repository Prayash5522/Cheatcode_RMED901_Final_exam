#Day 3 - Task 3

rm(list = ls())

library(here)
library(skimr)
library(naniar)
library(ggplot2)
library(GGally)
library(janitor)
library(tidyverse)


tidy_data_day_2 <- read_delim(here("data", "2025_09_08_tidy_version_day2.txt"))

###removing -year, -month, -baseline_esr_cat variables 

tidy_data_day_2 <- tidy_data_day_2 %>% select(-year, -month, -baseline_esr_cat)

glimpse(tidy_data_day_2)

##I removed the duplicates from the tidy_data_day_2 using the patient_id coloumn

tidy_data_day_2 <- tidy_data_day_2 %>%
  distinct(patient_id, .keep_all = TRUE)

##import and assign the additional data set
Additiona_data <- read_delim(here("data", "2025_09_05_original_exam_data_join.txt"))

## arrang both data sets tidy_data_day_2 and the Additiona_data by the patient_id

tidy_data_day_2 <- tidy_data_day_2%>%
  arrange(patient_id)
Additiona_data <- Additiona_data%>%
  arrange(patient_id)
glimpse(Additiona_data)

##joining the tidy_data_day_2 and the Additiona_data into one file 

joined_data <- tidy_data_day_2 %>%
  left_join(Additiona_data, by = "patient_id")

glimpse(joined_data)

###saving the joined_data

fileName <- paste0("2025_09_08_joined_data_day2", ".txt")

write_delim(
  joined_data, 
  file = here("data", fileName),
  delim = "\t"
)




