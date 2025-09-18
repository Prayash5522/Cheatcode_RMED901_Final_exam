## Day 2 - Task 2

rm(list = ls())

library(here)
library(skimr)
library(naniar)
library(ggplot2)
library(GGally)
library(janitor)
library(tidyverse)


original_data <- read_delim(here("data", "2025_09_05_tidy_version_day1.txt"))

# I am renaimimng the coloumn strep_resistance , and seperated it into two coloumns which called strep_resistance_level,
# strep_resistance

original_data <- original_data %>%
  separate(strep_resistance, into = c("strep", "resistance"), sep = "_", extra = "merge")

print(original_data)

original_data <- original_data %>% 
  mutate(strep = if_else(strep == 1, "sensitive", strep),
         strep = if_else(strep == 2, "modrate", strep),
         strep = if_else(strep == 3, "resistance", strep))

original_data <- original_data %>%
  separate(resistance, into = c("resistance", "concenteration_after_six_months"), sep = "_", extra = "merge")

print(original_data)

original_data <- original_data %>%
  rename(strep_resistance_level = strep)

original_data <- original_data %>%
  rename(strep_resistance = concenteration_after_six_months)

original_data <- original_data %>% select(-resistance)

#### dividing baseline_temp_cat into Fahrenheit and Celsius

original_data <- original_data %>% 
  separate(baseline_temp_cat, into = c("number_temp", "baseline_temp"), 
           sep = 2) %>% select(-number_temp) %>% 
  separate_wider_delim(baseline_temp, "/", names = c("baseline_temp_fahren", "baseline_temp_cels"))

original_data <- original_data %>% 
  separate(baseline_temp_cels, into = c("baseline_temp_cels" , "number"),
           sep = -1) %>% select(-number)

original_data <- original_data %>% 
  separate(baseline_temp_fahren, into = c("baseline_temp_fahren" , "number"),
           sep = -1) %>% select(-number)

###dividing esr cat variable 

original_data <- original_data %>% 
  separate(baseline_esr_cat, into = c(NA, "baseline_esr_cat"), sep = 2)


####dividing 6m_radiologica

original_data <- original_data %>% 
  separate(`6m_radiologic`, into = c(NA, "6m_radiologic"), sep = 2)


fileName <- paste0("2025_09_08_tidy_version_day2", ".txt")

write_delim(
  original_data, 
  file = here("data", fileName),
  delim = "\t"
)







