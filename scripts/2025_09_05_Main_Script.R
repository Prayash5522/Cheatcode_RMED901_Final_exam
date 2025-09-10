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

joined_data <- read_delim(here("data", "2025_09_08_joined_data_day2.txt"))

### Change gender

joined_data <- joined_data %>%
  mutate(gender = ifelse(gender == "F", 0, gender),
         gender = ifelse(gender == "M", 1, gender))

##### this is the the coloumn that showes the strep resistance after the addministration
###of the high dose 

####chanding the values in the dose strep from 0 2 to high and no dose 

joined_data <- joined_data %>%
  mutate(`dose_strep [g]` = case_when(
    `dose_strep [g]` == 0 ~ "No_dose",
    `dose_strep [g]` == 2 ~ "high_dose",
    TRUE ~ as.character(`dose_strep [g]`)
  ))

### creating a new cloumn that shows the resistance 
joined_data <- joined_data %>%
  mutate(
    status_after_high_dose_administration = case_when(
      `dose_strep [g]` == "high_dose" & strep_resistance_level == "resistance" ~ "resistant",
      `dose_strep [g]` == "high_dose" & strep_resistance_level == "sensitive" ~ "not_resistant", 
      `dose_strep [g]` == "high_dose" & strep_resistance_level == "modrate" ~ "not_resistant",  # Fixed spelling
      `dose_strep [g]` == "No_dose" ~ "not_resistant",
      TRUE ~ "Unknown"
    )
  )

###mutate temperature in celcuius

glimpse(joined_data)

#since we had created temperature variable in Fahrenheit and Celsius with the 
#baseline data, even before joining the data, we will drop the ones created before
#joining the data and use the baseline temperature from the data which was joined later

joined_data <- joined_data %>% select(-baseline_temp_fahren, -baseline_temp_cels)

glimpse(joined_data)

#now only baseline_temp variable is remaining which is in Fahrenheit 
#now we will mutate celsius variable using celcius = (°F - 32) ÷ (9/5)

joined_data <- joined_data %>% 
  mutate(baseline_temp_cels = (baseline_temp - 31)/(9/5))


glimpse(joined_data)

### cutting "baseline_esr" score into quartiles (4 equal parts) and making a new variable

joined_data <- joined_data %>% 
  mutate(baseline_esr_quartiles = cut(baseline_esr, breaks = 4))

table(joined_data$baseline_esr_quartiles)

### setting the order of columns as: `patient_id, gender, arm` and other columns

joined_data <- joined_data %>% select(patient_id, gender, arm, everything())

joined_data

### Arranging patient_id column of your data set in order of increasing number or alphabeticall

joined_data <- joined_data %>% arrange(joined_data$patient_id)

head(joined_data)
tail(joined_data)

#baseline_esr as numneric 

joined_data$baseline_esr <- as.numeric(joined_data$baseline_esr)

glimpse(joined_data)

### connecting above steps with pipe,,,and since we have individually ran the codes above
#this is just an example of how we can pipe it all together. That's why it is 
#mareked as a comment 

#joined_data_xyz <- joined_data %>% 
#  mutate(gender = ifelse(gender == "F", 0, gender),
#         gender = ifelse(gender == "M", 1, gender)) %>% 
#  mutate(`dose_strep [g]` = case_when(
#    `dose_strep [g]` == 0 ~ "No_dose",
#    `dose_strep [g]` == 2 ~ "high_dose",
#    TRUE ~ as.character(`dose_strep [g]`)
#  )) %>%  mutate(
#    status_after_high_dose_administration = case_when(
#      `dose_strep [g]` == "high_dose" & strep_resistance_level == "resistance" ~ "resistant",
#      `dose_strep [g]` == "high_dose" & strep_resistance_level == "sensitive" ~ #"not_resistant", 
#      `dose_strep [g]` == "high_dose" & strep_resistance_level == "modrate" ~ "not_resistant"#,
#      `dose_strep [g]` == "No_dose" ~ "not_resistant",
#      TRUE ~ "Unknown")) %>%
#  select(-baseline_temp_fahren, -baseline_temp_cels) %>% 
#  mutate(baseline_temp_cels = (baseline_temp - 31)/(9/5)) %>%
#  mutate(baseline_esr_quartile = cut(baseline_esr, breaks = 4)) %>% 
#  select(patient_id, gender, arm, everything()) %>% 
#  arrange(joined_data$patient_id)


####exploring missing data

skimr:: skim(joined_data)

# comment : we have one baseline_esr value missing. The observation is for a participant
# in control arm, with poor baseline condition, at was dead at the 6 month follow up. 


Results_1 <- joined_data %>%
  group_by(across(where(~ is.factor(.) || is.character(.)))) %>%  # all categorical vars
  summarise(
    min_baseline_temp = min(baseline_temp, na.rm = TRUE),
    max_baseline_temp = max(baseline_temp, na.rm = TRUE),
    mean_baseline_temp = mean(baseline_temp, na.rm = TRUE),
    sd_baseline_temp = sd(baseline_temp, na.rm = TRUE),
    .groups = "drop"
  ) 

glimpse(Results_1)

Results_2 <- joined_data %>%
  group_by(gender) %>%  
  summarise(
    min_baseline_temp = min(baseline_temp, na.rm = TRUE),
    max_baseline_temp = max(baseline_temp, na.rm = TRUE),
    mean_baseline_temp = mean(baseline_temp, na.rm = TRUE),
    sd_baseline_temp = sd(baseline_temp, na.rm = TRUE),
  ) 


glimpse(Results_2)

