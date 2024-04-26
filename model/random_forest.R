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
miss_data = data[rowSums(is.na(data)) > 0, ]
remain_data = data[rowSums(is.na(data)) == 0,]

# Set seed and partition data into 80%:20% training & testing ------------------
set.seed(656)
train_index = createDataPartition(remain_data$LoanStatus, p = .8, list = FALSE) %>% 
  as.vector(.)
train_data = (remain_data[train_index,])[,-c(1,2)]
test_data = (remain_data[-train_index,])[,-c(1,2)]

# Run model --------------------------------------------------------------------
<<<<<<< HEAD
rf = randomForest(LoanStatus ~ ., data = train_data, ntree = 5000)
=======
rf1 = randomForest(factor(LoanStatus) ~ ., data = train_data, 
                  sampsize = c('Chargedoff' = 300, 'Completed' = 300,
                               'Defaulted' = 300), 
                  ntree = 5000)

#rf2 = randomForest(factor(LoanStatus) ~ ., data = train_data, 
#                   sampsize = c('Chargedoff' = 400, 'Completed' = 400,
#                                'Defaulted' = 400), 
#                   ntree = 5000)

#rf3 = randomForest(factor(LoanStatus) ~ ., data = train_data, 
#                   sampsize = c('Chargedoff' = 500, 'Completed' = 500,
#                                'Defaulted' = 500), 
#                   ntree = 5000)

#rf4 = randomForest(factor(LoanStatus) ~ ., data = train_data, 
#                   sampsize = c('Chargedoff' = 600, 'Completed' = 600,
#                                'Defaulted' = 600), 
#                   ntree = 5000)

#rf5 = randomForest(factor(LoanStatus) ~ ., data = train_data, 
#                   sampsize = c('Chargedoff' = 700, 'Completed' = 700,
#                                'Defaulted' = 700), 
#                   ntree = 5000)

## plot oob error (all 4 types)
rf = rf1 # using 300 for all
oob_data = data.frame(
  trees = rep(1:nrow(rf$err.rate), 4), 
  type = rep(c("OOB","Chargedoff","Completed","Defaulted"), 
             each = nrow(rf$err.rate)),
  error = c(rf$err.rate[,"OOB"], rf$err.rate[,"Chargedoff"], 
            rf$err.rate[,"Completed"], rf$err.rate[,"Defaulted"]))
ggplot(data = oob_data, aes(x = trees, y= error)) + 
  geom_line(aes(color = type)) + ggtitle("Sampling 300 of each class")
>>>>>>> 29335e39e3d0e2c3a735a836219e0612f2473c4d

# Tuning random forest ---------------------------------------------------------

## plotting only overall oob error
plot((oob_data %>% filter(type == "OOB"))$trees,
     (oob_data %>% filter(type == "OOB"))$error, type = "l", 
     xlab = "Number of trees built",
     ylab = "OOB error",
     main = "OOB error for the number of trees built")
abline(v = 1000,col = rgb(0.49, 0.81, 0.54))

tuneRF(train_data[,-1], factor(train_data$LoanStatus), nTreeTry = 1000, 
       stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE,
       sampsize = c('Chargedoff' = 300, 'Completed' = 300,
                    'Defaulted' = 300))

# Running rf again using tuned parameters --------------------------------------
rf_final = randomForest(factor(LoanStatus) ~ ., data = train_data, 
                   sampsize = c('Chargedoff' = 300, 'Completed' = 300,
                                'Defaulted' = 300), 
                   ntree = 1000, mtry = 7)

## visualizing variable importance
## Get variable importance from the initial model fit using training data
imp_data = as.data.frame(importance(rf_final))
imp_data$Var.Names = row.names(imp_data)

## the higher the mean decrease gini = the higher the variable importance
ggplot(imp_data, aes(x=Var.Names, y=MeanDecreaseGini)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=MeanDecreaseGini), color="skyblue") +
  geom_point(aes(size = MeanDecreaseGini), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


# Using rf on testing data -------------------------------
rf_pred = predict(rf_final, test_data)
confusionMatrix(rf_pred, factor(test_data$LoanStatus))
