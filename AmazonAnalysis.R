#######################################
#### HIGHEST SCORE - CLASSIFICATION RANDOM FORESTS ####
#######################################

library(tidyverse)
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(embed)
library(ranger)
library(discrim)
library(naivebayes)
library(kernlab)
library(themis)

amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")

amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))

my_mod <- rand_forest(mtry=tune(),
                      min_n=tune(),
                      trees=500) %>%
  set_engine("ranger") %>%
  set_mode("classification")

my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
  step_mutate_at(all_numeric_predictors(), fn=factor) %>%
  #step_dummy(all_nominal_predictors()) %>%
  #step_other(all_nominal_predictors(), threshold=0.001) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>%
  step_normalize(all_predictors()) 
# %>%
# step_pca(all_predictors(), threshold = 0.9) %>%
# step_smote(all_outcomes(), neighbors=5)

# prep <- prep(my_recipe)
# baked <- bake(prep, new_data=amazonTest)

amazon_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data=amazonTrain)

tuning_grid <- grid_regular(mtry(range=c(1,1)),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(amazonTrain, v=5, repeats=1)

CV_results <- amazon_workflow %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))


# do any or call of these 
# metric_set(roc_auc, f_meas, sens, recall, spec, 
# precision, accuracy)

bestTune <- CV_results %>%
  select_best("roc_auc")

final_wf <- amazon_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=amazonTrain)

# final_wf %>%  
#   predict(new_data=amazonTest, type="prob")

amazon_predictions <- predict(final_wf,
                              new_data=amazonTest,
                              type="prob") %>%
  bind_cols(., amazonTest) %>%
  select(id, .pred_1) %>%
  rename(Action=.pred_1)

vroom_write(x=amazon_predictions, file="./1AmazonClassificationRF.csv", delim=",")

# ##### LOGISTIC REGRESSION #####
# #
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(embed)
library(discrim)
library(naivebayes)
library(kernlab)
library(themis)
## Read in the data
amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")
# table(amazonTrain$RESOURCE)
# table(amazonTrain$ROLE_ROLLUP_1, amazonTrain$ROLE_ROLLUP_2)
# you should have 112 columns
amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))

my_mod <- logistic_reg() %>%
  set_engine("glm")

# my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
#   step_mutate_at(all_numeric_predictors(), fn=factor) %>%
#   step_other(all_nominal_predictors(), threshold=0.01) %>%
#   step_dummy(all_nominal_predictors())

my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
  step_mutate_at(all_numeric_predictors(), fn=factor) %>%
  #step_dummy(all_nominal_predictors()) %>%
  #step_other(all_nominal_predictors(), threshold=0.001) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>%
  step_normalize(all_predictors()) %>%
  # step_pca(all_predictors(), threshold = 0.8) %>%
  step_smote(all_outcomes(), neighbors=5)

# prep <- prep(my_recipe)
# baked <- bake(prep, new_data=amazonTest)

amazon_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data=amazonTrain)

amazon_predictions <- predict(amazon_workflow,
                              new_data=amazonTest,
                              type="prob") %>%
  bind_cols(., amazonTest) %>%
  select(id, .pred_1) %>%
  rename(Action=.pred_1)

# predict(type="prob) %>%
# mutate(ACTION =ifelse(.pred_1 >0.1, 0))


# amazon_submit <- cbind(amazonTest, amazon_predictions) %>%
#   select(RESOURCE, .pred_1)
# colnames(amazon_submit) <- c("id", "Action")

vroom_write(x=amazon_predictions, file="./AmazonLGPredsSMOTE.csv", delim=",")

# # create ID and predicted prob columns
# # sample submission looks like ID and action
#
#
#
#
# #########################################
# ##### PENALIXED LOGISTIC REGRESSION #####
# #########################################
library(tidyverse)
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(embed)

amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")

amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))

my_mod <- logistic_reg(mixture=tune(), penalty=tune()) %>%
  set_engine("glmnet")
#
# # my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
# #   step_mutate_at(all_numeric_predictors(), fn=factor) %>%
# #   step_other(all_nominal_predictors(), threshold=0.001) %>%
# #   step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION))
# #
my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
  step_mutate_at(all_numeric_predictors(), fn=factor) %>%
  #step_dummy(all_nominal_predictors()) %>%
  #step_other(all_nominal_predictors(), threshold=0.001) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>%
  step_normalize(all_predictors()) %>%
  # step_pca(all_predictors(), threshold = 0.8) %>%
  step_smote(all_outcomes(), neighbors=5)

# # prep <- prep(my_recipe)
# # baked <- bake(prep, new_data=amazonTest)
#
amazon_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data=amazonTrain)
#
tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels=5)
#
folds <- vfold_cv(amazonTrain, v=5, repeats=1)
#
CV_results <- amazon_workflow %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))

#
# # do any or call of these
#   # metric_set(roc_auc, f_meas, sens, recall, spec,
#     # precision, accuracy)
#
bestTune <- CV_results %>%
  select_best("roc_auc")

final_wf <- amazon_workflow %>%
  finalize_workflow(bestTune)
#
# final_wf %>%
#   predict(new_data=amazonTest, type="prob")
#
amazon_predictions <- predict(final_wf,
                              new_data=amazonTest,
                              type="prob") %>%
  bind_cols(., amazonTest) %>%
  select(id, .pred_1) %>%
  rename(Action=.pred_1)

vroom_write(x=amazon_predictions, file="./AmazonSMOTEPenalizedPreds.csv", delim=",")
#

#######################################
#### CLASSIFICATION RANDOM FORESTS ####
#######################################
#
library(tidyverse)
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(embed)
library(ranger)
library(discrim)
library(naivebayes)
library(kernlab)
library(themis)
#
amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")

amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))
#
my_mod <- rand_forest(mtry=tune(),
                      min_n=tune(),
                      trees=500) %>%
  set_engine("ranger") %>%
  set_mode("classification")
# #
# # my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
# #   step_mutate_at(all_numeric_predictors(), fn=factor) %>%
# #   step_other(all_nominal_predictors(), threshold=0.001) %>%
# #   step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION))
my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
  step_mutate_at(all_numeric_predictors(), fn=factor) %>%
  #step_dummy(all_nominal_predictors()) %>%
  #step_other(all_nominal_predictors(), threshold=0.001) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), threshold = 0.8) %>%
  step_smote(all_outcomes(), neighbors=5)

#
# # prep <- prep(my_recipe)
# # baked <- bake(prep, new_data=amazonTest)
#
amazon_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data=amazonTrain)
#
tuning_grid <- grid_regular(mtry(range=c(1,5)),
                            min_n(),
                            levels=5)

folds <- vfold_cv(amazonTrain, v=5, repeats=1)
#
CV_results <- amazon_workflow %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))
#
#
#
#
# # do any or call of these
# # metric_set(roc_auc, f_meas, sens, recall, spec,
# # precision, accuracy)
#
bestTune <- CV_results %>%
  select_best("roc_auc")
#
final_wf <- amazon_workflow %>%
  finalize_workflow(bestTune)

# # final_wf %>%
# #   predict(new_data=amazonTest, type="prob")
#
amazon_predictions <- predict(final_wf,
                              new_data=amazonTest,
                              type="prob") %>%
  bind_cols(., amazonTest) %>%
  select(id, .pred_1) %>%
  rename(Action=.pred_1)
#
vroom_write(x=amazon_predictions, file="./AmazonSMOTEClassificationRF.csv", delim=",")



#
#
#
# #### NAIVE BAYES####
library(tidymodels)
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(embed)
library(discrim)
library(naivebayes)

amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")
#
amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))
#
nb_model <- naive_Bayes(Laplace=tune(), smoothness=tune()) %>%
  set_mode("classification") %>%
  set_engine("naivebayes")
#
# my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
#   step_mutate_at(all_numeric_predictors(), fn=factor) %>%
#   # step_other(all_nominal_predictors(), threshold=0.001) %>%
#   step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION))

#
nb_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(nb_model)
#
tuning_grid <- grid_regular(Laplace(),
                            smoothness(),
                            levels=5)
#
folds <- vfold_cv(amazonTrain, v=5, repeats=1)
#
CV_results <- nb_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))
# #
# # # # do any or call of these
# # #   # metric_set(roc_auc, f_meas, sens, recall, spec,
# # #     # precision, accuracy)
# #
bestTune <- CV_results %>%
  select_best("roc_auc")
# #
final_wf <- nb_wf %>%
  finalize_workflow(bestTune)
#
# final_wf %>%
#   predict(new_data=amazonTest, type="prob")
#
amazon_predictions <- predict(final_wf,
                              new_data=amazonTest,
                              type="prob") %>%
  bind_cols(., amazonTest) %>%
  select(id, .pred_1) %>%
  rename(Action=.pred_1)
#
vroom_write(x=amazon_predictions, file="./AmazonBayesPreds.csv", delim=",")
#
# #
# # ##### KNN #####
library(tidymodels)
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(embed)
# library(discrim)
# library(naivebayes)

amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")
#
amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))
# #
knn_model <- nearest_neighbor(neighbors=tune()) %>%
  set_mode("classification") %>%
  set_engine("kknn")
#
# # my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
# #   step_mutate_at(all_numeric_predictors(), fn=factor) %>%
# #   #step_other(all_nominal_predictors(), threshold=0.001) %>%
# #   step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION))
# #
knn_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(knn_model)
#
tuning_grid <- grid_regular(neighbors(),
                            levels=5)

folds <- vfold_cv(amazonTrain, v=5, repeats=1)

CV_results <- knn_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))
#
# # # do any or call of these
# #   # metric_set(roc_auc, f_meas, sens, recall, spec,
# #     # precision, accuracy)
# #
bestTune <- CV_results %>%
  select_best("roc_auc")
# #
final_wf <- knn_wf %>%
  finalize_workflow(bestTune)
#
# final_wf %>%
#   predict(new_data=amazonTest, type="prob")
#
amazon_predictions <- predict(final_wf,
                              new_data=amazonTest,
                              type="prob") %>%
  bind_cols(., amazonTest) %>%
  select(id, .pred_1) %>%
  rename(Action=.pred_1)
#
vroom_write(x=amazon_predictions, file="./AmazonSMOTEDimRedPreds.csv", delim=",")

### DIMENSION REDUCTION ####
library(tidymodels)
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(embed)
# library(discrim)
# library(naivebayes)

amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")
#
amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))
#
knn_model <- nearest_neighbor(neighbors=tune()) %>%
  set_mode("classification") %>%
  set_engine("kknn")
# #
# # my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
# #   step_normalize(all_predictors()) %>%
# #   step_mutate_at(all_numeric_predictors(), fn=factor) %>%
# #   #step_other(all_nominal_predictors(), threshold=0.001) %>%
# #   step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>%
# #   # step_dummy(all_nominal_predictors()) %>%
# #   step_pca(all_predictors(), threshold = 0.8)
# #
knn_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(knn_model)
#
tuning_grid <- grid_regular(neighbors(),
                            levels=5)

folds <- vfold_cv(amazonTrain, v=5, repeats=1)

CV_results <- knn_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))
#
# # # do any or call of these
# #   # metric_set(roc_auc, f_meas, sens, recall, spec,
# #     # precision, accuracy)
# #
bestTune <- CV_results %>%
  select_best("roc_auc")
#
final_wf <- knn_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=amazonTrain)

final_wf %>%
  predict(new_data=amazonTest, type="prob")
#
amazon_predictions <- predict(final_wf,
                              new_data=amazonTest,
                              type="prob") %>%
  bind_cols(., amazonTest) %>%
  select(id, .pred_1) %>%
  rename(Action=.pred_1)
#
vroom_write(x=amazon_predictions, file="./AmazonSMOTEDimRedPreds.csv", delim=",")
#
# #### DIM REDUCTION NAIVE BAYES####

library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(embed)
library(discrim)
library(naivebayes)

amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")
#
amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))
#
nb_model <- naive_Bayes(Laplace=tune(), smoothness=tune()) %>%
  set_mode("classification") %>%
  set_engine("naivebayes")
#
# my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
#   step_normalize(all_predictors()) %>%
#   step_mutate_at(all_numeric_predictors(), fn=factor) %>%
#   #step_other(all_nominal_predictors(), threshold=0.001) %>%
#   step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>%
#   # step_dummy(all_nominal_predictors()) %>%
#   step_pca(all_predictors(), threshold = 0.8)
#
nb_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(nb_model)
#
tuning_grid <- grid_regular(Laplace(),
                            smoothness(),
                            levels=5)

folds <- vfold_cv(amazonTrain, v=5, repeats=1)

CV_results <- nb_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))
#
# # # do any or call of these
# #   # metric_set(roc_auc, f_meas, sens, recall, spec,
# #     # precision, accuracy)
# #
bestTune <- CV_results %>%
  select_best("roc_auc")
# #
final_wf <- nb_wf %>%
  finalize_workflow(bestTune)
#
# final_wf %>%
#   predict(new_data=amazonTest, type="prob")
#
amazon_predictions <- predict(final_wf,
                              new_data=amazonTest,
                              type="prob") %>%
  bind_cols(., amazonTest) %>%
  select(id, .pred_1) %>%
  rename(Action=.pred_1)

vroom_write(x=amazon_predictions, file="./AmazonSMOTEBayesDimRedPreds.csv", delim=",")

## ideas to get higher - stacking, bart, pca SMOTE with and random forests

#### SUPPORT VECTOR MACHINES ####
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(embed)
library(discrim)
library(naivebayes)
library(kernlab)

amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")
#
amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))
# #
svm_radial_model <- svm_rbf(rbf_sigma=tune(), cost=tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

# my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
#   step_normalize(all_predictors()) %>%
#   step_mutate_at(all_numeric_predictors(), fn=factor) %>%
#   step_dummy(all_nominal_predictors()) %>%
#   #step_other(all_nominal_predictors(), threshold=0.001) %>%
#   step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>%
#   step_pca(all_predictors(), threshold = 0.8)
#
svm_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(svm_radial_model)
#
tuning_grid <- grid_regular(rbf_sigma(),
                            cost(),
                            levels=5)
#
folds <- vfold_cv(amazonTrain, v=5, repeats=1)

CV_results <- svm_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))
#
# # # do any or call of these
# #   # metric_set(roc_auc, f_meas, sens, recall, spec,
# #     # precision, accuracy)
# #
bestTune <- CV_results %>%
  select_best("roc_auc")
#
final_wf <- svm_wf %>%
  finalize_workflow(bestTune)
#
# final_wf %>%
#   predict(new_data=amazonTest, type="prob")
#
amazon_predictions <- predict(final_wf,
                              new_data=amazonTest,
                              type="prob") %>%
  bind_cols(., amazonTest) %>%
  select(id, .pred_1) %>%
  rename(Action=.pred_1)
#
vroom_write(x=amazon_predictions, file="./AmazonSMOTESVMPreds.csv", delim=",")

### BALANCING DATA ####
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(embed)
library(discrim)
library(naivebayes)
library(kernlab)
library(themis)

amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")
#
amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))
#
svm_radial_model <- svm_rbf(rbf_sigma=tune(), cost=tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
  step_mutate_at(all_numeric_predictors(), fn=factor) %>%
  step_normalize(all_predictors()) %>%
  #bstep_dummy(all_nominal_predictors()) %>%
  #step_other(all_nominal_predictors(), threshold=0.001) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>%
  step_pca(all_predictors(), threshold = 0.8) %>%
  step_smote(all_outcomes(), neighbors=5)

svm_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(svm_radial_model)

tuning_grid <- grid_regular(rbf_sigma(),
                            cost(),
                            levels=5)

folds <- vfold_cv(amazonTrain, v=5, repeats=1)

CV_results <- svm_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))

# # do any or call of these
#   # metric_set(roc_auc, f_meas, sens, recall, spec,
#     # precision, accuracy)
#
bestTune <- CV_results %>%
  select_best("roc_auc")
#
final_wf <- svm_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=amazonTrain)

final_wf %>%
  predict(new_data=amazonTest, type="prob")

amazon_predictions <- predict(final_wf,
                              new_data=amazonTest,
                              type="prob") %>%
  bind_cols(., amazonTest) %>%
  select(id, .pred_1) %>%
  rename(Action=.pred_1)

vroom_write(x=amazon_predictions, file="./AmazonSMOTEPreds.csv", delim=",")
