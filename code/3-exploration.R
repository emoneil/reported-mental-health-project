# load libraries
library(kableExtra)
library(maps)
library(tidyverse)
library(cdlTools)
library(ggcorrplot)
library(cowplot)

# read in the cleaned data
mental_health_data = read_tsv("../data/clean/mental_health_data.tsv")
table(mental_health_data$mental_health)
nrow(mental_health_data[mental_health_data$mental_health > 0,])

# read in the training data
mental_health_train = read_tsv("../data/clean/mental_health_train.tsv") 

factor_cols = c("sex", "urban_status", "marital", "education", 
                "own_home", "veteran", "employment", "child_count", "income", 
                "race", "age", "health_coverage", "med_cost", "check_up", 
                "flu_shot", "pneumonia_shot", "HIV_test", "chol_check", 
                "personal_doctor", "high_blood_pressure", "high_cholesterol", 
                "diabetes", "deaf", "blind", "smoker", "exercise", 
                "strength_activity_index", "heart_attack", "stroke", 
                "asthma", "depression")
mental_health_train = mental_health_train %>%
  mutate(across(all_of(factor_cols), factor))

# calculate median days mental health not good
median_bad_mental_health_days = mental_health_train %>%
  summarise(median(mental_health)) %>%
  pull()
median_bad_mental_health_days

# calculate mean days mental health not good
mean_bad_mental_health_days = mental_health_train %>%
  summarise(mean(mental_health)) %>%
  pull()
mean_bad_mental_health_days

# create histogram of days mental health not good
response_hist = mental_health_train %>%
  ggplot(aes(x = mental_health)) + 
  geom_histogram(binwidth = 5) +
  labs(x = "Number of days of month mental health not good", 
       y = "Count of respondents") +
  theme_bw()

# save the response histogram
ggsave(filename = "../results/response-histogram.png", 
       plot = response_hist, 
       device = "png", 
       width = 5, 
       height = 3)

# create histogram of transformed response
mental_health_train %>%
  ggplot(aes(x = log1p(mental_health))) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Log transformed number of days of\nmonth mental health not good",
       y = "Count of respondents") +
  theme_bw()

unique(mental_health_train$state)
mental_health_train %>% group_by(state) %>% summarise(median(mental_health))

# examine top 10 states with lowest mental health
mental_health_train %>% 
  select(state, mental_health) %>%
  group_by(state) %>%
  summarise(num_more_than_10_bad_days = sum(mental_health > 10)) %>%
  arrange(desc(num_more_than_10_bad_days)) %>%
  head(10) %>%
  mutate(state = fips(state, to = "Name")) %>%
  write_tsv("../results/top-10-states-low-mental-health-data.tsv")

# check count
mental_health_train %>%
  filter(state == 12) %>% 
  filter(mental_health > 10) %>% 
  nrow()

# examine top 10 states by number of mental health facilities
mental_health_train %>% 
  select(state, diag_eval) %>%
  group_by(state) %>%
  summarise(diag_eval = mean(diag_eval)) %>%
  arrange(desc(diag_eval)) %>%
  head(10) %>%
  mutate(state = fips(state, to = "Name")) %>%
  write_tsv("../results/top-10-states-mental-health-facilities-data.tsv")


# create a heatmap of days mental health not good across the U.S.
# create stratified sample based on state to reduce computing requirement
set.seed(1)
mental_health_data_sample = mental_health_train %>%
  group_by(state) %>%
  sample_n(100)

mental_health_data_sample %>% group_by(state) %>% 
  summarize(mean_mental_health = mean(mental_health)) %>% 
  arrange(desc(mean_mental_health))

n_distinct(mental_health_data_sample$state)
fips(unique(mental_health_data_sample$state), to = "Name")

p = map_data("state") %>%
  as_tibble() %>% 
  left_join(mental_health_data_sample %>% 
              mutate(state = fips(state, to = "Name")) %>%
              rename(region = state,
                     `Days mental health not good` = mental_health) %>% 
              mutate(region = str_to_lower(region)), 
            by = c("region")) %>%
  ggplot() + 
  geom_polygon(data=map_data("state"), 
               aes(x=long, y=lat, group=group),
               color="black", fill=NA,  size = 1, alpha = .3) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = `Days mental health not good`),
               color="darkblue", size = .1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_void()

ggsave(filename = "../results/response-map.png", 
       plot = p, 
       device = "png", 
       width = 9, 
       height = 4)


# distribution of response by race
response_by_race = mental_health_train %>%
  filter(race != 9) %>%
  ggplot(aes(x = race, y = mental_health, fill = race)) + 
  geom_boxplot() +
  labs(x = "Race", 
       y = "Number of days mental health not good") + 
  scale_x_discrete(labels=c("White", "Black", "AI or AN", "Asian", "PI", 
                            "Other", "Multiracial", "Hispanic")) +
  theme_bw() + theme(legend.position = "none")


# distribution of response by gender
response_by_gender = mental_health_train %>%
  ggplot(aes(x = sex, y = mental_health, fill = sex)) + 
  geom_boxplot() +
  labs(x = "Gender", 
       y = "Number of days mental health not good") + 
  scale_x_discrete(labels=c("Male", "Female")) +
  theme_bw() + theme(legend.position = "none")

plot_grid(response_by_race, response_by_gender, align = "h")


# mental health not good and physical health not good overlayed density plots
mental_health_train %>% 
  filter(mental_health != 0 & physical_health != 0) %>%
  pivot_longer(c(mental_health, physical_health),
               names_to = "mental_or_physical",
               values_to = "num_days_not_good") %>%
  ggplot() + 
  geom_density(aes(x=num_days_not_good, colour=mental_or_physical))


# distribution of mental health vs. med_cost
mental_health_train %>%
  filter(med_cost != 7 & med_cost != 9) %>%
  ggplot(aes(x=med_cost, y=mental_health, fill=med_cost)) + 
  geom_violin() + geom_boxplot(width=0.1) + 
  scale_fill_brewer(palette="OrRd") + 
  xlab("Whether Could Not See Doctor Because of Cost") + 
  ylab("Number of days mental health not good") +
  scale_x_discrete(labels=c("Yes, barrier of cost", 
                            "No, not barrier of cost")) +
  theme(legend.position="none") +
  theme_bw()

# mental health proportion of respondents >= 15 days vs. med_cost *
mental_health_vs_med_cost = mental_health_train %>%
  filter(med_cost != 7 & med_cost != 9) %>%
  mutate(mental_health_15 = ifelse(mental_health >= 15, 1, 0)) %>%
  group_by(med_cost) %>%
  summarize(prop_mental_health_15 = mean(mental_health_15)) %>%
  ggplot(aes(x=med_cost, y=prop_mental_health_15, fill=med_cost)) + 
  geom_bar(stat='identity') + 
  scale_fill_brewer(palette="Set1") + 
  xlab("Whether Could Not See Doctor Because of Cost") + 
  ylab("Proportion that days mental\nhealth not good >= 15 days") +
  scale_x_discrete(labels=c("Yes, barrier of cost", 
                            "No, not barrier of cost")) +
  scale_fill_discrete(guide="none") +
  theme_bw()

ggsave(filename = "../results/mental-health-vs-med-cost.png", 
      plot = mental_health_vs_med_cost, 
      device = "png", 
      width = 5, 
      height = 5)


# bar plot of average num state facilities with pay assistance vs. mental health not good >= vs. < 15
pay_asst_scale_data = mental_health_train %>%
  mutate(mental_health_15 = ifelse(mental_health >= 15, 1, 0)) %>%
  group_by(mental_health_15) %>%
  summarize(avg_num_facilities = mean(pay_assist))
pay_asst_scale_data %>% 
  ggplot(aes(x=factor(mental_health_15), y=avg_num_facilities, 
             fill=factor(mental_health_15))) + 
  geom_bar(stat='identity') +
  xlab("Number of Days Mental Health Not Good Greater vs. Less than 15") + 
  ylab("Average number of state mental health\nfacilities with pay assistance") +
  scale_x_discrete(labels=c("Greater than/equal to 15 days", 
                            "Less than 15 days")) +
  theme(legend.position="none") +
  theme_bw()

# proportion whose MH was not good for >= 15 days by race *
race_mental_health_data = mental_health_train %>%
  mutate(mental_health_15 = ifelse(mental_health >= 15, 1, 0)) %>%
  filter(race != 9) %>%
  group_by(race) %>%
  summarize(prop_mental_health_15 = mean(mental_health_15))
mental_health_vs_race = race_mental_health_data %>% 
  ggplot(aes(x=race, y=prop_mental_health_15, 
             fill=race)) + 
  geom_bar(stat='identity') +
  xlab("Race") + 
  ylab("Proportion for race that days mental\nhealth not good >= 15 days") +
  scale_x_discrete(labels=c("White", "Black", "AI or AN", "Asian", "PI", 
                            "Other", "Multiracial", "Hispanic")) +
  scale_fill_discrete(guide="none") +
  theme_bw()

ggsave(filename = "../results/mental-health-vs-race.png", 
       plot = mental_health_vs_race, 
       device = "png", 
       width = 7, 
       height = 4)


# examine relationships among features:
# correlation between explanatory variables w/i mental health facility category
mental_health_facility_features = mental_health_train %>% 
  select(diag_eval, diet_exer_counsel, housing_services, employ_services, 
         emergency_services, suicide_prev_services, sed_services, smi_services, 
         other_languages, fee_scale, pay_assist, payment_cash,
         payment_medicare, payment_medicaid)
corr = round(cor(mental_health_facility_features), 1)
corr_plot_facility = ggcorrplot(corr,
                                ggtheme = ggplot2::theme_bw,
                                colors = c("#6D9EC1", "white", "#E46726"),
                                tl.cex = 8) +
  ggtitle("State mental health facilities") +
  theme(plot.title = element_text(hjust = 0.5))

# plot correlations among numeric variables
numeric_variables = mental_health_train %>% 
  select(mental_health,
         diag_eval, diet_exer_counsel, housing_services, employ_services, 
         emergency_services, suicide_prev_services, sed_services, smi_services, 
         other_languages, fee_scale, pay_assist, payment_cash,
         payment_medicare, payment_medicaid, 
         physical_health, bmi, alcohol_consumption, 
         fruits, vegetables, french_fries)
corr = round(cor(numeric_variables), 1)
numeric_variables_corr_plot = ggcorrplot(corr,
                                         type = "upper",
                                         ggtheme = ggplot2::theme_bw,
                                         colors = c("#6D9EC1", "white", "#E46726"),
                                         tl.cex = 6)



ggsave(filename = "../results/corr-among-numeric-variables-plot.png", 
       plot = numeric_variables_corr_plot, 
       device = "png", 
       width = 5, 
       height = 5)


# mental_health vs. depression
response_by_depression = mental_health_train %>%
  ggplot(aes(x = depression, y = mental_health, fill = depression)) + 
  geom_boxplot() +
  labs(x = "Whether ever told had a depressive disorder", 
       y = "Number of days mental health not good") + 
  scale_x_discrete(labels=c("Yes", "No", "Don’t know/\nNot sure", "Refused")) +
  theme_bw() + theme(legend.position = "none")

ggsave(filename = "../results/mental-health-vs-depression-plot.png", 
       plot = response_by_depression, 
       device = "png", 
       width = 6, 
       height = 4)

# mental_health vs. age
response_by_age = mental_health_train %>%
  ggplot(aes(x = age, y = mental_health, fill = age)) + 
  geom_boxplot() +
  labs(x = "Age", 
       y = "Number of days mental health not good") + 
  scale_x_discrete(labels=c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
                            "40 to 44", "45 to 49", "50 to 54", "55 to 59", 
                            "60 to 64", "65 to 69", "70 to 74", "75 to 79",
                            "80 or older", "Don’t know/\nRefused/\nMissing")) +
  theme_bw() + theme(legend.position = "none")

ggsave(filename = "../results/mental-health-vs-age-plot.png", 
       plot = response_by_age, 
       device = "png", 
       width = 10, 
       height = 4)

# exercise and physical health
physical_health_by_exercise = mental_health_train %>%
  ggplot(aes(x = exercise, y = physical_health, fill = exercise)) + 
  geom_boxplot() +
  labs(x = "Whether participated in any physical\nactivities or exercises in past month", 
       y = "Numbers of days physical health not good") + 
  scale_x_discrete(labels=c("Yes", "No", "Don’t know/Not sure", "Refused")) +
  theme_bw() + theme(legend.position = "none")


# plot grid for boxplots for physical health relationship:
# bmi and high blood pressure
bmi_by_high_blood_pressure = mental_health_train %>%
  ggplot(aes(x = high_blood_pressure, y = bmi/100, 
             fill = high_blood_pressure)) + 
  geom_boxplot() +
  labs(x = "Whether ever told high blood pressure", 
       y = "BMI") + 
  scale_x_discrete(labels=c("Yes", "Yes, female,\nonly pregnancy", 
                            "No", "Borderline\nhigh or\npre-hypertensive", 
                            "Don’t know/\nNot Sure", "Refused")) +
  theme_bw(base_size = 13) + theme(legend.position = "none")

# bmi and diabetes
bmi_by_diabetes = mental_health_train %>%
  ggplot(aes(x = diabetes, y = bmi/100, fill = diabetes)) + 
  geom_boxplot() +
  labs(x = "Whether ever told had diabetes", 
       y = "BMI") + 
  scale_x_discrete(labels=c("Yes", "Yes, female,\nonly pregnancy", 
                            "No", "No, pre-diabetes\nor borderline", 
                            "Don’t know/\nNot Sure", "Refused")) +
  theme_bw(base_size = 13) + theme(legend.position = "none")

# physical health and high blood pressure
physical_health_by_high_blood_pressure = mental_health_train %>%
  ggplot(aes(x = high_blood_pressure, y = physical_health, 
             fill = high_blood_pressure)) + 
  geom_boxplot() +
  labs(x = "Whether ever told high blood pressure", 
       y = "Number of days physical health not good") + 
  scale_x_discrete(labels=c("Yes", "Yes, female,\nonly pregnancy", 
                            "No", "Borderline\nhigh or\npre-hypertensive", 
                            "Don’t know/\nNot Sure", "Refused")) +
  theme_bw(base_size = 13) + theme(legend.position = "none")

# physical health and diabetes
physical_health_by_diabetes = mental_health_train %>%
  ggplot(aes(x = diabetes, y = physical_health, fill = diabetes)) + 
  geom_boxplot() +
  labs(x = "Whether ever told had diabetes", 
       y = "Number of days physical health not good") + 
  scale_x_discrete(labels=c("Yes", "Yes, female,\nonly pregnancy", 
                            "No", "No, pre-diabetes\nor borderline", 
                            "Don’t know/\nNot Sure", "Refused")) +
  theme_bw(base_size = 13) + theme(legend.position = "none")

png(width = 15, 
    height = 10,
    res = 300,
    units = "in", 
    filename = "../results/physical-health-relationships-plot-grid.png")
plot_grid(bmi_by_high_blood_pressure, bmi_by_diabetes, 
          physical_health_by_high_blood_pressure, physical_health_by_diabetes, 
          nrow = 2)
dev.off()

# remove depression and state (the latter will not be used for any modeling)
mental_health_train = mental_health_train %>% select(-depression, -state)
mental_health_test = mental_health_test %>% select(-depression, -state)

write_tsv(x = mental_health_train, file = "../data/clean/mental_health_train.tsv")
write_tsv(x = mental_health_test, file = "../data/clean/mental_health_test.tsv")

