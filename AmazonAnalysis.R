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


# amazon_submit <- cbind(amazonTest, amazon_predictions) %>%
#   select(RESOURCE, .pred_1)
# colnames(amazon_submit) <- c("id", "Action")
  
vroom_write(x=amazon_predictions, file="./AmazonPreds.csv", delim=",")

# create ID and predicted prob columns 
# sample submission looks like ID and action



