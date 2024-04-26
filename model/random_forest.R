# Description: this is an attempt to do random forest model on the cleaned 
# data set done by Dylan 4/4/2024
# ------------------------------------------------------------------------------
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(tidyr)
library(randomForest)

data = read.csv("datasets/CleanedProsperData.csv")

# Set seed and partition data into 80%:20% training & testing ------------------
set.seed(656)
train_index = createDataPartition(data$LoanStatus, p = .8, list = FALSE) %>% 
  as.vector(.)
train_data = (data[train_index,])[,-c(1,2)]
test_data = (data[-train_index,])[,-c(1,2)]

# Run model --------------------------------------------------------------------
rf = randomForest(LoanStatus ~ ., data = train_data, ntree = 5000)

# Tuning random forest ---------------------------------------------------------

# Running rf again using tuned parameters --------------------------------------