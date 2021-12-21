# load libraries
library(tidyverse)
library(kableExtra)
library(glmnetUtils) # to run ridge and lasso
source("../code/functions/plot_glmnet.R") # for lasso/ridge trace plots

# read in the training data
mental_health_train = read_tsv("../data/clean/mental_health_train.tsv")

factor_cols = c("sex", "urban_status", "marital", "education", 
                "own_home", "veteran", "employment", "child_count", "income", 
                "race", "age", "health_coverage", "med_cost", "check_up", 
                "flu_shot", "pneumonia_shot", "HIV_test", "chol_check", 
                "personal_doctor", "high_blood_pressure", "high_cholesterol", 
                "diabetes", "deaf", "blind", "smoker", "exercise", 
                "strength_activity_index", "heart_attack", "stroke", "asthma")
mental_health_train = mental_health_train %>% 
  mutate(across(all_of(factor_cols), factor))


lm_fit = lm(mental_health ~ ., data = mental_health_train)
log_lm_fit = lm(log1p(mental_health) ~ ., data = mental_health_train)
summary(lm_fit)
r_squared = summary(lm_fit)$r.squared

# save the linear fit object
save(lm_fit, file = "../results/lm_fit.Rda")

hist(lm_fit$residuals)
hist(log_lm_fit$residuals)

# compare original with log transform training error
linear_predictions = predict(lm_fit, mental_health_train) %>% 
  as.numeric()
linear_train_error = (linear_predictions - 
                       mental_health_train$mental_health)^2
linear_train_rmse = sqrt(mean(linear_train_error))

log_linear_predictions = predict(log_lm_fit, mental_health_train) %>% 
  as.numeric()
back_transformed_train_error = (exp(log_linear_predictions) -
                                  mental_health_train$mental_health)^2
back_transformed_train_rmse = sqrt(mean(back_transformed_train_error))


# run ridge regression
set.seed(1)
ridge_fit = cv.glmnet(mental_health ~ .,   
                      alpha = 0,                 
                      nfolds = 10,               
                      data = mental_health_train)

# save the ridge fit object
save(ridge_fit, file = "../results/ridge_fit.Rda")

set.seed(1)
log_ridge_fit = cv.glmnet(log1p(mental_health) ~ .,
                      alpha = 0,
                      nfolds = 10,
                      data = mental_health_train)

# compare original with log transform training error
ridge_predictions = predict(ridge_fit, mental_health_train) %>% 
  as.numeric()
ridge_train_error = (ridge_predictions - 
                            mental_health_train$mental_health)^2
ridge_train_rmse = sqrt(mean(ridge_train_error))

log_ridge_predictions = predict(log_ridge_fit, mental_health_train) %>% 
  as.numeric()
back_transformed_train_error = (exp(log_ridge_predictions) -
                            mental_health_train$mental_health)^2
back_transformed_train_rmse = sqrt(mean(back_transformed_train_error))


# create ridge CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "../results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

# create ridge trace plot
ridge_trace_plot = plot_glmnet(ridge_fit, mental_health_train, 
                               features_to_plot = 6)
ggsave(filename = "../results/ridge-trace-plot.png", 
       plot = ridge_trace_plot, 
       device = "png", 
       width = 6, 
       height = 4)


# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(mental_health ~ .,   
                      alpha = 1,
                      nfolds = 10,               
                      data = mental_health_train)

# save the lasso fit object
save(lasso_fit, file = "../results/lasso_fit.Rda")

num_features_lasso = lasso_fit$nzero[lasso_fit$lambda == lasso_fit$lambda.1se]

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "../results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
lasso_trace_plot = plot_glmnet(lasso_fit, mental_health_train, 
                               features_to_plot = 6)
ggsave(filename = "../results/lasso-trace-plot.png", 
       plot = lasso_trace_plot, 
       device = "png", 
       width = 6, 
       height = 4)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, mental_health_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("../results/lasso-features-table.tsv")


# run elastic net regression
set.seed(1)
elnet_fit = cva.glmnet(mental_health ~ .,
                       nfolds = 10,
                       data = mental_health_train)

# save the elastic net fit object
save(elnet_fit, file = "../results/elnet_fit.Rda")

# plot the minimum CV error for each value of alpha
elnet_cv_error_alpha_plot = plot_cva_glmnet(elnet_fit)
ggsave(filename = "../results/elnet-cv-error-alpha-plot.png", 
       plot = elnet_cv_error_alpha_plot, 
       device = "png", 
       width = 6, 
       height = 4)

elnet_fit_best = extract_best_elnet(elnet_fit)
elnet_fit_best$alpha

# create elastic net CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "../results/elnet-cv-plot.png")
plot(elnet_fit_best)
dev.off()

# create elastic net trace plot
elnet_trace_plot = plot_glmnet(elnet_fit_best, mental_health_train, 
                               features_to_plot = 6)
ggsave(filename = "../results/elnet-trace-plot.png", 
       plot = elnet_trace_plot, 
       device = "png", 
       width = 6, 
       height = 4)

# extract features selected by elastic net and their coefficients
beta_hat_std = extract_std_coefs(elnet_fit_best, mental_health_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("../results/elnet-features-table.tsv")

