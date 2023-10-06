library(tidyverse)
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
## Read in the data
amazonTrain <- vroom("./train.csv")
amazonTest <- vroom("./test.csv")