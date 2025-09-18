## Day 1 - Task 1 and 2

rm(list = ls())

library(here)
library(skimr)
library(naniar)
library(ggplot2)
library(GGally)
library(janitor)
library(tidyverse)

original_data <- read_delim(here("data", "2025_09_05_original_exam_data.txt"))

#understanding the data

skimr::skim(original_data)

#separate baseline_condition into two parts
#but before that checking if 1 = good and so on and there are not any values where 
#1 = good or 2 = poor and so on

original_data %>% count(baseline_condition)

#we find just 3 unique values: 1_good, 2_fair and 3_poor

# so we good to separate the baseline_condition into two part first

original_data <- original_data %>% 
  separate(baseline_condition, into = c("condition_number", "baseline_condition"))

#keep the condition number while dropping variable baseline_condition and rename it 
#interms of "good", "fair" and "poor"

original_data <- original_data %>% select(-baseline_condition)

original_data <- original_data %>% rename(baseline_condition = "condition_number")

original_data <- original_data %>% 
  mutate(baseline_condition = if_else(baseline_condition == 1, "good", baseline_condition),
         baseline_condition = if_else(baseline_condition == 2, "fair", baseline_condition),
         baseline_condition = if_else(baseline_condition == 3, "poor", baseline_condition))



# I seperated the gender_arm variable into two variables , one of them contain the gender and the other contain the arm
# delete the gender arm 

original_data <- original_data %>% 
  mutate(arm = sub(".*_", "", gender_arm)) %>%
  select(-gender_arm) 
original_data 


fileName <- paste0("2025_09_05_tidy_version_day1", ".txt")

write_delim(
  original_data, 
  file = here("data", fileName),
  delim = "\t"
)
