## Main script

library(tidyverse)
library(here)
library(skimr)
library(naniar)

original_data <- read_delim(here("data", "2025_09_05_original_exam_data.txt"))




# I seperated the gender_arm variable into two variables , one of them contain the gender and the other contain the arm
# delete the gender arm 
original_data <- original_data %>% 
  +     mutate(arm = sub(".*_", "", gender_arm)) %>%
  +     select(-gender_arm)
> original_data