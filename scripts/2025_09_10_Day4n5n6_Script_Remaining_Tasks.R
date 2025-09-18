##Day 4,5 & 6 - Task 3 , 4 and 5

rm(list = ls())

library(here)
library(skimr)
library(naniar)
library(ggplot2)
library(GGally)
library(janitor)
library(tidyverse)

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

##stratifying all categorical_variable

Stratifying_all_categorical_variable <- joined_data %>%
  group_by(across(where(~ is.factor(.) || is.character(.)))) %>%  # all categorical vars
  summarise(
    min_baseline_temp = min(baseline_temp, na.rm = TRUE),
    max_baseline_temp = max(baseline_temp, na.rm = TRUE),
    mean_baseline_temp = mean(baseline_temp, na.rm = TRUE),
    sd_baseline_temp = sd(baseline_temp, na.rm = TRUE),
    .groups = "drop"
  ) 

glimpse(Stratifying_all_categorical_variable)

##stratifying gender with baseline_temp

stratify_gender_temp <- joined_data %>%
  group_by(gender) %>%  
  summarise(
    min_baseline_temp = min(baseline_temp, na.rm = TRUE),
    max_baseline_temp = max(baseline_temp, na.rm = TRUE),
    mean_baseline_temp = mean(baseline_temp, na.rm = TRUE),
    sd_baseline_temp = sd(baseline_temp, na.rm = TRUE),
  ) 

glimpse(stratify_gender_temp)

###stratifying different varibables for the value of rad_num

##baseline_condition is fair

stratify_fair_rad_num <- joined_data %>%
  group_by(baseline_condition) %>% filter(baseline_condition == "fair") %>% 
  summarise(
    min_rad_num = min(rad_num, na.rm = TRUE),
    max_rad_num = max(rad_num, na.rm = TRUE),
    mean_rad_num = mean(rad_num, na.rm = TRUE),
    sd_rad_num = sd(rad_num, na.rm = TRUE),
  ) 

glimpse(stratify_fair_rad_num)


##only for females

stratify_females_rad_num <- joined_data %>%
  group_by(gender) %>% filter(gender == 0) %>% 
  summarise(
    min_rad_num = min(rad_num, na.rm = TRUE),
    max_rad_num = max(rad_num, na.rm = TRUE),
    mean_rad_num = mean(rad_num, na.rm = TRUE),
    sd_rad_num = sd(rad_num, na.rm = TRUE),
  ) 

glimpse(stratify_females_rad_num)

##Only for persons with baseline temperature 100-100.9F

stratify_temp_rad_num <- joined_data %>%
  mutate(
    baseline_tempt100_100.9F = 
      if_else(baseline_temp >= 0 & baseline_temp <= 100.9, 1, 0)) %>%
  group_by(baseline_tempt100_100.9F) %>% 
  filter(baseline_tempt100_100.9F == 1) %>%
  summarise(
    min_rad_num  = min(rad_num, na.rm = TRUE),
    max_rad_num  = max(rad_num, na.rm = TRUE),
    mean_rad_num = mean(rad_num, na.rm = TRUE),
    sd_rad_num   = sd(rad_num, na.rm = TRUE)
  )
glimpse(stratify_temp_rad_num)

#Only for persons that developed resistance to streptomycin

stratify_resistance_rad_num <- joined_data %>%
  mutate(
    resistance_streptomycin = 
      if_else(strep_resistance_level == "resistance", 1, 0)) %>%
  group_by(resistance_streptomycin) %>% 
  filter(resistance_streptomycin == 1) %>%
  summarise(
    min_rad_num  = min(rad_num, na.rm = TRUE),
    max_rad_num  = max(rad_num, na.rm = TRUE),
    mean_rad_num = mean(rad_num, na.rm = TRUE),
    sd_rad_num   = sd(rad_num, na.rm = TRUE)
  )
glimpse(stratify_resistance_rad_num)


###two categorical columns in one table

gender_strep_resistance <- joined_data %>% janitor::tabyl(gender,strep_resistance_level)

gender_strep_resistance

#### Are there any correlated measurements? (hint: `GGally::ggcorr` or search
### online for correlation matrix in R) ? for answering this questions ,  I mad a heat map
### that showes the correlation between each numerical variable 

#### selection of the numerical variable 
correlation_check <- joined_data %>% 
  select(where(is.numeric))

###### plotting of the heat map for all the numerical variables within my file
ggcorr(correlation_check, 
       label = TRUE,                # show correlation coefficients
       label_round = 2,             # round decimals
       hjust = 0.75, size = 3)      # adjust label placement & size


###### comment , if I wanted to see the variabl names
names(correlation_check)


#### assining the names of the observation within the gender , into male = 1 , female = 0 
#joined_data <- joined_data %>%
#  mutate(gender = if_else(gender, 
#                         levels = c(0, 1), 
#                         labels = c("Female", "Male")))

##### ploting the curve comaring between the male and female 

ggplot(joined_data, aes(x = gender, y = baseline_esr, fill = gender)) +
  geom_boxplot() +
  geom_jitter(aes(color = "black"), width = 0.2, alpha = 0.6) +
  scale_fill_manual(values = c("1" = "pink", "0" = "red")) +
  scale_color_manual(values = c("1" = "pink", "0" = "red")) +
  labs(x = "Gender", y = "ESR at baseline (mm/hr)")
labs(x = "Gender", y = "ESR at baseline (mm/hr)") # 0 is female and # 1 is male


##### check if the genderaffect the baseline ESR (mm/hr) , it shwoed that the gender 
##### does not affect the esr basline , the p value is .5

joined_data %>%
  t.test(baseline_esr ~ gender, data = .) %>%
  broom::tidy()

####linear relationship check for variables baseline_esr and temperature

#visual inspection
joined_data %>% 
  ggplot(aes(x = baseline_esr, y = baseline_temp)) +
  geom_point() + 
  geom_smooth(method = "lm")

#seems to have a linear relation

#display numbers/p-values in standard (non-scientific) format
options(scipen = 999)

#running linear regression
joined_data %>% 
  lm(baseline_esr ~ baseline_temp, data = .) %>%
  broom::tidy()

####randomization check

##Does the randomization arm depend on the gender?

glimpse(joined_data)

table(joined_data$arm, joined_data$gender)

chisq.test(joined_data$arm, joined_data$gender) %>% 
  broom::tidy()

# since the p value is 0.946, at the significance level of 95% confidence interval,
# we can conclude that randomization does not depend on the gender.


### randomization arm depend on erythrocyte sedimentation rate in mm per hour at baseline

glimpse(joined_data)

joined_data %>%
  t.test(baseline_esr ~ arm, data = .) %>% 
  broom::tidy()

# since the p-value is 0.366, at the significance level of 95% confidence interval,
# we can conclude that randomization does not depend of erythrocyte sedimentation rate in mm per hour at baseline.

##### Is there an association between streptomycin resistance after 6 months of therapy
### and erythrocyte sedimentation rate in mm per hour at baseline?

### I Mad anova test to do that becaus I have continoues and categorical data
#### which need to use anova 
### the p value is not statistically significat = .05 , thats why strep resistance level
## dont affect the basline-esr 

## One way ANOVA

joined_data %>%
  mutate(baseline_esr = log(baseline_esr)) %>% aov(baseline_esr ~ `6m_radiologic` , data =.) %>%
  summary()

#### This shows that there is a statistically significant difference in baseline temperature across different Likert score 
### categories of 6-month radiologic response




