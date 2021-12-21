# load libraries
library(tidyverse)
library(kableExtra)
library(glmnetUtils)
library(randomForest)
library(gbm)

# load train data
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

# load test data
mental_health_test = read_tsv("../data/clean/mental_health_test.tsv")
factor_cols = c("sex", "urban_status", "marital", "education", 
                "own_home", "veteran", "employment", "child_count", "income", 
                "race", "age", "health_coverage", "med_cost", "check_up", 
                "flu_shot", "pneumonia_shot", "HIV_test", "chol_check", 
                "personal_doctor", "high_blood_pressure", "high_cholesterol", 
                "diabetes", "deaf", "blind", "smoker", "exercise", 
                "strength_activity_index", "heart_attack", "stroke", "asthma")
mental_health_test = mental_health_test %>% 
  mutate(across(all_of(factor_cols), factor))

# load linear fit object
load("../results/lm_fit.Rda")

# load ridge fit object
load("../results/ridge_fit.Rda")

# load lasso fit object
load("../results/lasso_fit.Rda")

# load elastic net fit object
load("../results/elnet_fit.Rda")


# load optimal tree object
load("../results/optimal_tree.Rda")

# load rf object
load("../results/rf_fit_tuned.Rda")

# load gbm object
load("../results/gbm_fit_tuned.Rda")


# generate test error based on intercept-only prediction rule
prediction = mean(mental_health_train$mental_health)
intercept_only_rmse = sqrt(mean((prediction - 
                                   mental_health_test$mental_health)^2))


# evaluate linear rmse
linear_predictions = predict(lm_fit, newdata = mental_health_test)
linear_error = (linear_predictions - mental_health_test$mental_health)^2
linear_rmse = sqrt(mean(linear_error))

# evaluate ridge rmse
ridge_predictions = predict(ridge_fit, 
                            newdata = mental_health_test, 
                            s = "lambda.1se") %>% as.numeric()
ridge_error = (ridge_predictions - mental_health_test$mental_health)^2
ridge_rmse = sqrt(mean(ridge_error))

# evaluate lasso rmse
lasso_predictions = predict(lasso_fit, 
                            newdata = mental_health_test, 
                            s = "lambda.1se") %>% as.numeric()
lasso_error = (lasso_predictions - mental_health_test$mental_health)^2
lasso_rmse = sqrt(mean(lasso_error))

# evaluate elastic net rmse
elnet_predictions = predict(elnet_fit,
                            alpha = elnet_fit$alpha,
                            newdata = mental_health_test,
                            s = "lambda.1se") %>% as.numeric()
elnet_error = (elnet_predictions - mental_health_test$mental_health)^2
elnet_rmse = sqrt(mean(elnet_error))

# create table for regression methods test errors
tibble(`Regression method` = c("Linear", "Ridge", "Lasso", "Elastic Net"), 
       `Test RMSE` = c(linear_rmse, 
                       ridge_rmse, 
                       lasso_rmse,
                       elnet_rmse))


# evaluate tree-based methods
# compute test error of the tuned decision tree
dt_predictions  = predict(optimal_tree, newdata = mental_health_test)
dt_rmse = sqrt(mean((dt_predictions-mental_health_test$mental_health)^2))

# compute test error of the random forest
rf_predictions = predict(rf_fit_tuned, newdata = mental_health_test)
rf_rmse = sqrt(mean((rf_predictions-mental_health_test$mental_health)^2))

# compute test error of the boosting model
gbm_predictions= predict(gbm_fit_tuned,
                         n.trees = optimal_num_trees,
                         newdata = mental_health_test)
gbm_rmse = sqrt(mean((gbm_predictions-mental_health_test$mental_health)^2))

# create table of these three tree-based model test errors
error_for_tree_models = tribble(
  ~`Tree-based Model`, ~`Test RMSE`,
  #------/-------
  "Decision tree", dt_rmse,
  "Random forest", rf_rmse,
  "Boosting", gbm_rmse,
)



# determine training error for models
linear_train_predictions = predict(lm_fit, mental_health_train) %>% 
  as.numeric()
linear_train_error = (linear_train_predictions - 
                       mental_health_train$mental_health)^2
linear_train_rmse = sqrt(mean(linear_train_error))

ridge_train_predictions = predict(ridge_fit, 
                                  mental_health_train,
                                  s = "lambda.1se") %>% 
  as.numeric()
ridge_train_error = (ridge_train_predictions - 
                       mental_health_train$mental_health)^2
ridge_train_rmse = sqrt(mean(ridge_train_error))

lasso_train_predictions = predict(lasso_fit, 
                                  mental_health_train, 
                                  s = "lambda.1se") %>% 
  as.numeric()
lasso_train_error = (lasso_train_predictions - 
                       mental_health_train$mental_health)^2
lasso_train_rmse = sqrt(mean(lasso_train_error))

elnet_train_predictions = predict(elnet_fit, 
                                  mental_health_train, 
                                  alpha = elnet_fit$alpha,
                                  s = "lambda.1se") %>% 
  as.numeric()
elnet_train_error = (elnet_train_predictions - 
                       mental_health_train$mental_health)^2
elnet_train_rmse = sqrt(mean(elnet_train_error))

dt_train_predictions = predict(optimal_tree, mental_health_train) %>% 
  as.numeric()
dt_train_error = (dt_train_predictions - 
                       mental_health_train$mental_health)^2
dt_train_rmse = sqrt(mean(dt_train_error))

rf_train_predictions = predict(rf_fit_tuned, mental_health_train) %>% 
  as.numeric()
rf_train_error = (rf_train_predictions - 
                    mental_health_train$mental_health)^2
rf_train_rmse = sqrt(mean(rf_train_error))

gbm_train_predictions = predict(gbm_fit_tuned, mental_health_train) %>% 
  as.numeric()
gbm_train_error = (gbm_train_predictions - 
                    mental_health_train$mental_health)^2
gbm_train_rmse = sqrt(mean(gbm_train_error))

train_error_for_models = tribble(
  ~`Model`, ~`Train RMSE`,
  #------/-------
  "Linear regression", linear_train_rmse,
  "Ridge regression", ridge_train_rmse,
  "Lasso regression", lasso_train_rmse,
  "Elastic net regression", elnet_train_rmse,
  "Decision tree", dt_train_rmse,
  "Random forest", rf_train_rmse,
  "Boosting", gbm_train_rmse
)
write_tsv(train_error_for_models, "../results/train-model-evaluation.tsv")


# combine all model test errors and intercept-only model test error
test_error_for_models = tribble(
  ~`Model`, ~`Test RMSE`,
  #------/-------
  "Linear regression", linear_rmse,
  "Ridge regression", ridge_rmse,
  "Lasso regression", lasso_rmse,
  "Elastic net regression", elnet_rmse,
  "Decision tree", dt_rmse,
  "Random forest", rf_rmse,
  "Boosting", gbm_rmse,
  "Intercept-only", intercept_only_rmse,
)
write_tsv(test_error_for_models, "../results/test-model-evaluation.tsv")
