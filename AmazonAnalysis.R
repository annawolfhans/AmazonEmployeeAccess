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

my_recipe <- recipe(ACTION~., data=amazonTrain) %>%
  step_mutate_at(all_numeric_predictors(), fn=factor) %>%
  step_other(all_nominal_predictors(), threshold=0.01) %>%
  step_dummy(all_nominal_predictors())

prep <- prep(my_recipe)
baked <- bake(prep, new_data=amazonTrain)
