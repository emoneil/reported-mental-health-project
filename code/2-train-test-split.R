library(tidyverse)

# read in the cleaned data
mental_health_data = read_tsv("../data/clean/mental_health_data.tsv")

factor_cols = c("sex", "urban_status", "marital", "education", 
                "own_home", "veteran", "employment", "child_count", "income", 
                "race", "age", "health_coverage", "med_cost", "check_up", 
                "flu_shot", "pneumonia_shot", "HIV_test", "chol_check", 
                "personal_doctor", "high_blood_pressure", "high_cholesterol", 
                "diabetes", "deaf", "blind", "smoker", "exercise", 
                "strength_activity_index", "heart_attack", "stroke", 
                "asthma", "depression")
mental_health_data = mental_health_data %>% 
  mutate(across(all_of(factor_cols), factor))

set.seed(1)
n = nrow(mental_health_data)
train_samples = sample(1:n, round(0.8*n))
# split mental_health_data into training and test sets
mental_health_train = mental_health_data[train_samples,]
mental_health_test = mental_health_data[-train_samples,]

# save the train and test data
write_tsv(x = mental_health_train, file = "../data/clean/mental_health_train.tsv")
write_tsv(x = mental_health_test, file = "../data/clean/mental_health_test.tsv")