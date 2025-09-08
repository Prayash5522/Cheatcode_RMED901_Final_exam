## Main script

library(tidyverse)
library(here)
library(skimr)
library(naniar)

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

# am renaimimng the coloumn strep_resistance , and seperated it into two coloumns which called strep_resistance_level,
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


