# Description: explore the data for missingness, what's there, etc using
# the cleaned dataset provided by Dylan 4/4/2024
# -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

data = read.csv("datasets/CleanedProsperData.csv")

ggplot_missing = function(x){
  # this function is from Dr. Homrighausen
  if(!require(reshape2)){warning('you need to install reshape2')} 
  require(reshape2)
  require(ggplot2)
  # This function produces a plot of the missing data pattern
  # in x. It is a modified version of a function in the 'neato' package
  x %>%
    is.na %>%
    melt %>% ggplot(data = .,
                    aes(x = Var2,
                        y = Var1)) +
    geom_raster(aes(fill = value)) + 
    scale_fill_grey(name = "", labels = c("Present","Missing")) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
} 
ggplot_missing(data)

miss_data = data[rowSums(is.na(data)) > 0, ]
remain_data = data[rowSums(is.na(data)) == 0,]

# Attempt at random forest if all observations with missing values removed -----
set.seed(656)
library(randomForest)
library(caret)

## partition 80% into training and 20% into testing
train_index = createDataPartition(remain_data$LoanStatus, p = .8, list = FALSE) %>% 
  as.vector(.)
train_data = (remain_data[train_index,])[,-c(1)] # removed just X instead of ListingKey as well
test_data = (remain_data[-train_index,])[,-c(1)]

rf = randomForest(factor(LoanStatus) ~ ., data = train_data, ntree = 1000)
print(rf)

# plot all 4 types of error
oob_data = data.frame(
  trees = rep(1:nrow(rf$err.rate), 4), 
  type = rep(c("OOB","Chargedoff","Completed","Defaulted"), 
             each = nrow(rf$err.rate)),
  error = c(rf$err.rate[,"OOB"], rf$err.rate[,"Chargedoff"], 
            rf$err.rate[,"Completed"], rf$err.rate[,"Defaulted"]))
ggplot(data = oob_data, aes(x = trees, y= error)) + 
  geom_line(aes(color = type))
