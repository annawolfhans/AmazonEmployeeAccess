##### LOGISTIC REGRESSION #####

library(tidyverse)
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(ggplot2)
library(embed)
## Read in the data
amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")
table(amazonTrain$RESOURCE)
table(amazonTrain$ROLE_ROLLUP_1, amazonTrain$ROLE_ROLLUP_2)
# you should have 112 columns 
amazonTrain <- amazonTrain %>%
  mutate(ACTION = as.factor(ACTION))

my_mod <- logistic_reg() %>%
  set_engine("glm")

my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
  step_mutate_at(all_numeric_predictors(), fn=factor) %>%
  step_other(all_nominal_predictors(), threshold=0.01) %>%
  step_dummy(all_nominal_predictors())

prep <- prep(my_recipe)
baked <- bake(prep, new_data=amazonTest)

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
  
vroom_write(x=amazon_predictions, file="./AmazonPreds.csv", delim=",")

# create ID and predicted prob columns 
# sample submission looks like ID and action




#########################################
##### PENALIXED LOGISTIC REGRESSION #####
#########################################
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

my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
  step_mutate_at(all_numeric_predictors(), fn=factor) %>%
  step_other(all_nominal_predictors(), threshold=0.001) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION))

# prep <- prep(my_recipe)
# baked <- bake(prep, new_data=amazonTest)

amazon_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod) %>%
  fit(data=amazonTrain)

tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels=5)
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

final_wf <- preg_wf %>%
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

vroom_write(x=amazon_predictions, file="./AmazonPenalizedPreds.csv", delim=",")
