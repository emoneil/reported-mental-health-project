# load libraries
library(rpart) # to train decision trees
library(rpart.plot) # to plot decision trees
library(randomForest) # random forests
library(gbm) # boosting
library(tidyverse) # tidyverse
library(kableExtra)
library(cowplot) # for side by side plots

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

head(mental_health_train, 5)
# select subset of training data for tree modeling 
# to reduce computational intensity specifically for random forest and boosting 
set.seed(1)
n = nrow(mental_health_train)
tree_train_samples = sample(1:n, round(0.01*n))
mental_health_tree_train = mental_health_train[tree_train_samples,]
table(mental_health_tree_train$mental_health)

# check for NAs
mental_health_train[rowSums(is.na(mental_health_train))!=0,]

# fit deepest possible tree
deepest_tree_fit = rpart(mental_health ~ .,
                         control = rpart.control(minsplit = 1,
                                                 minbucket = 1,
                                                 cp = 0),
                         data = mental_health_train)

# print the CP table for this tree
cp_table = deepest_tree_fit$cptable %>% as_tibble()

cp_table %>% arrange(desc(nsplit)) # nrow(mental_health_train) / 71190 = 3.34134 obs per terminal node

# produce CV plot based on info in CP table
cp_table_cv_plot = cp_table %>%
  filter(nsplit >= 2) %>%
  ggplot(aes(x = nsplit+1, y = xerror,
             ymin = xerror - xstd, ymax = xerror + xstd)) + 
  scale_x_log10() +
  geom_point() + geom_line() +
  geom_errorbar(width = 0.25) +
  xlab("Number of terminal nodes on log scale") + ylab("CV error") + 
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") + 
  theme_bw()

ggsave(filename = "../results/cp-table-cv-plot.png", 
       plot = cp_table_cv_plot, 
       device = "png", 
       width = 6, 
       height = 4)

optimal_tree_info = cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>% head(1)
# the number of splits for the optimal tree
optimal_tree_info$nsplit

# extract this optimal tree into an object
optimal_tree = prune(tree = deepest_tree_fit, cp = optimal_tree_info$CP)
save(optimal_tree, file = "../results/optimal_tree.Rda")

# create tree plot for optimal tree
png(width = 7, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "../results/optimal-tree-plot.png")
rpart.plot(optimal_tree)
dev.off()


# train a random forest with default settings on `mental_health_train`
set.seed(1)
rf_fit_default = randomForest(mental_health ~ ., 
                      data = mental_health_tree_train)
# value of mtry
rf_fit_default$mtry

save(rf_fit_default, file = "../results/rf_fit_default.Rda")

# plot OOB error as a function of number of trees
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "../results/default-oob-error-plot.png")
plot(rf_fit_default)
dev.off()


# tune the random forests with different values of m
# based on OOB error plot, a reasonable number of trees to grow without 
# significantly compromising prediction accuracy is 100
set.seed(1)
num_features = ncol(mental_health_tree_train) - 1
mvalues = seq.int(from = 1, to = num_features, length.out = 5)
mvalues = round(mvalues)
oob_errors = numeric(length(mvalues)) 
ntree = 100
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(mental_health ~ ., mtry = m, 
                        data = mental_health_tree_train) 
  oob_errors[idx] = rf_fit$mse[ntree]
}

# plot OOB error versus m
m_and_oob_errors = tibble(m = mvalues, oob_err = oob_errors) 
oob_error_vs_m_plot = m_and_oob_errors %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = mvalues) + 
  labs(x = "Value for m", y = "OOB error") +
  theme_bw()

ggsave(filename = "../results/oob-error-vs-m-plot.png", 
       plot = oob_error_vs_m_plot, 
       device = "png", 
       width = 6, 
       height = 4)


# extract m corresponding to min value of OOB error
best_m = m_and_oob_errors %>% arrange(oob_errors) %>% head(1) %>% pull(m)
# train a random forest on 500 trees, making sure OOB error has flattened out
set.seed(1)
rf_fit_tuned = randomForest(mental_health ~ ., mtry = best_m, 
                            ntree = 500,
                            importance = TRUE, 
                            data = mental_health_tree_train)

save(rf_fit_tuned, file = "../results/rf_fit_tuned.Rda")

# plot OOB error of random forest as function of number of trees
oob_error_vs_ntrees_plot = tibble(oob_error = rf_fit_tuned$mse, 
                                  trees = 1:500) %>%
  ggplot(aes(x = trees, y = oob_error)) + 
  geom_line() + 
  labs(x = "Number of trees", y = "OOB error") + 
  theme_bw()

ggsave(filename = "../results/oob-error-vs-ntrees-plot.png", 
       plot = oob_error_vs_ntrees_plot, 
       device = "png", 
       width = 6, 
       height = 4)

# produce variable importance plot for rf trained on optimal value of m
png(width = 8, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "../results/rf-var-importance-plot.png")
varImpPlot(rf_fit_tuned, n.var = 10)
dev.off()


# fit boosted tree models with interaction depths 1, 2, 3, and 4
# using a shrinkage factor of 0.1, 1000 trees, and 5-fold cross-validation
set.seed(1)
gbm_fit_1 = gbm(mental_health ~ .,
                distribution = "gaussian", 
                n.trees = 1000, 
                interaction.depth = 1, 
                shrinkage = 0.1,
                cv.folds = 5,
                data = mental_health_tree_train)

set.seed(1)
gbm_fit_2 = gbm(mental_health ~ .,
                distribution = "gaussian", 
                n.trees = 1000, 
                interaction.depth = 2, 
                shrinkage = 0.1,
                cv.folds = 5,
                data = mental_health_tree_train)

set.seed(1)
gbm_fit_3 = gbm(mental_health ~ .,
                distribution = "gaussian", 
                n.trees = 1000, 
                interaction.depth = 3, 
                shrinkage = 0.1,
                cv.folds = 5,
                data = mental_health_tree_train)

set.seed(1)
gbm_fit_4 = gbm(mental_health ~ .,
                distribution = "gaussian", 
                n.trees = 1000, 
                interaction.depth = 4, 
                shrinkage = 0.1,
                cv.folds = 5,
                data = mental_health_tree_train)


# extract CV errors
ntrees = 1000
cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth = 1), 
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth = 2), 
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth = 3),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_4$cv.error, depth = 4)
)

# plot the CV errors against the number of trees for each interaction depth
cv_errors_vs_ntrees_vary_depth_plot = cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = factor(depth))) + 
  # add horizontal dashed lines at the minima of the curves
  geom_hline(yintercept = min(gbm_fit_1$cv.error), 
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = min(gbm_fit_2$cv.error), 
             linetype = "dashed", color = "green") +
  geom_hline(yintercept = min(gbm_fit_3$cv.error), 
             linetype = "dashed", color = "blue") +
  geom_hline(yintercept = min(gbm_fit_4$cv.error), 
             linetype = "dashed", color = "purple") +
  geom_line() + 
  # set colors to match horizontal line minima
  scale_color_manual(labels = c("1", "2", "3", "4"), 
                     values = c("red", "green", "blue", "purple")) +
  labs(x = "Number of trees", y = "CV error", colour = "Interaction depth") +
  theme_bw()

ggsave(filename = "../results/cv-errors-vs-ntrees-vary-depth-plot.png", 
       plot = cv_errors_vs_ntrees_vary_depth_plot, 
       device = "png", 
       width = 6, 
       height = 4)


gbm_fit_tuned = gbm_fit_3 # set optimal interaction depth model
save(gbm_fit_tuned, file = "../results/gbm_fit_tuned.Rda")
optimal_num_trees = gbm.perf(gbm_fit_3, plot.it = FALSE)


# make sure there are enough trees that the CV curve has reached its minimum
tibble(Iteration = 1:1000, CV = gbm_fit_tuned$cv.error) %>%
  ggplot(aes(x = Iteration, y = CV)) + geom_line() + theme_bw()

# print first ten rows of relative influence table for optimal boosting model
summary(gbm_fit_tuned, n.trees = optimal_num_trees, plotit = FALSE) %>%
  head(10) %>% 
  write_tsv("../results/boosting-relative-influence-table.tsv")


# produce partial dependence plots for top three features based on rel influence
p1 = plot(gbm_fit_tuned, 
          i.var = "physical_health", 
          n.trees = optimal_num_trees)

p2 = plot(gbm_fit_tuned, 
          i.var = "employment", 
          n.trees = optimal_num_trees)

p3 = plot(gbm_fit_tuned, 
          i.var = "age", 
          n.trees = optimal_num_trees)

p4 = plot(gbm_fit_tuned, 
          i.var = "med_cost", 
          n.trees = optimal_num_trees)

# use cowplot to concatenate the three plots and save
png(width = 8, 
    height = 8,
    res = 300,
    units = "in", 
    filename = "../results/partial-dependence-plots.png")
plot_grid(p1, p2, p3, p4, nrow = 2)
dev.off()
