data <- read.csv("CleanedProsperData.csv")
library(caret)
library(randomForest)
Y     = factor(data$LoanStatus)
data$LoanStatus<- NULL 
data$BorrowerState <- factor(data$BorrowerState)
data$ListingKey <- NULL
data$ListingNumber <- NULL
data$ListingCreationDate <- NULL
data$CreditGrade <- factor(data$CreditGrade)
data$ClosedDate <- NULL
data$ProsperRating..Alpha. <- NULL
data$Occupation <- NULL
data$EmploymentStatus <- factor(data$EmploymentStatus)
data$GroupKey <- NULL
data$DateCreditPulled <- NULL
data$FirstRecordedCreditLine <- NULL
data$IncomeRange <- factor(data$IncomeRange)
data$LoanKey <- NULL
data$LoanOriginationDate <- NULL
data$LoanOriginationQuarter <- factor(data$LoanOriginationQuarter)
data$MemberKey <- NULL
data$LoanNumber <- NULL
data$IsBorrowerHomeowner <- as.numeric(data$IsBorrowerHomeowner)
data$CurrentlyInGroup <- as.numeric(data$CurrentlyInGroup)
data$IncomeVerifiable <- as.numeric(data$IncomeVerifiable)
data$X <- NULL

data <- data[,colSums(is.na(data)) == 0]
set.seed(656)
trainIndex <- createDataPartition(Y, p=0.8)[[1]]

xTrain = data[trainIndex, ]
xTest  = data[-trainIndex, ]
yTrain = Y[trainIndex]
yTest  = Y[-trainIndex]


K            = 10
trainControl = trainControl(method = "cv", number = K)
tuneGrid     = expand.grid('alpha'=c(0,.25,.5,.75,1),'lambda' = seq(00, .001, length.out = 30))

### Here, I fit a penalized regression model to our data, where I removed all of the factors as well.
# In order to find the best alpha/lambda values, I did the trainControl/train stuff. This gave us a
# 97.68% Acuracy, and it was pretty good at identifying almost everything. 

elasticOut = train(x = as.matrix(xTrain[,-c(1,6,7,12,18)]), y = yTrain,
                   method = "glmnet", 
                   trControl = trainControl, tuneGrid = tuneGrid)

glmnetOut      = cv.glmnet(x = as.matrix(xTrain[,-c(1,6,7,12,18)]), y = yTrain, 
                           alpha = elasticOut$bestTune$alpha, family = 'multinomial', 
                           standardize = FALSE)

vals <- predict(glmnetOut, as.matrix(xTest[,-c(1,6,7,12,18)]), type="response", 
                s=elasticOut$bestTune$lambda)

maxVals <- apply(vals, 1, which.max)
maxVals[which(maxVals == 1)] = "Chargedoff"
maxVals[which(maxVals == 2)] = "Completed"
maxVals[which(maxVals == 3)] = "Defaulted"

confusionMatrix(factor(maxVals), factor(yTest))


#### Obviously, the above results were really, really good. However there is a parameter that 
# almost perfectly identifies whether a loan is completed or not- which is the currentDaysDeliquent
# Obviously, this would be 0 for a completed loan, and not 0 for any loan that isn't completed on 
# time. So this below model resuses the same framework seen above, but also removes that variable.
# This gave an accuracy of 91.72%, but was horrible with defaulted loans (it got not many of them 
# right)

K1            = 10
trainControl1 = trainControl(method = "cv", number = K1)
tuneGrid1     = expand.grid('alpha'=c(0,.25,.5,.75,1),'lambda' = seq(00, .001, length.out = 30))

noDeqelasticOut1 = train(x = as.matrix(xTrain[,-c(1,6,7,12,15,18)]), y = yTrain,
                    method = "glmnet", 
                    trControl = trainControl1, tuneGrid = tuneGrid1)

noDeqglmnetOut1      = cv.glmnet(x = as.matrix(xTrain[,-c(1,6,7,12,15,18)]), y = yTrain, 
                            alpha = noDeqelasticOut1$bestTune$alpha, family = 'multinomial', 
                            standardize = FALSE)

noDeqvals1 <- predict(noDeqglmnetOut1, as.matrix(xTest[,-c(1,6,7,12,15,18)]), type="response", 
                 s=noDeqelasticOut1$bestTune$lambda)

noDeqmaxVals1 <- apply(noDeqvals1, 1, which.max)
noDeqmaxVals1[which(noDeqmaxVals1 == 1)] = "Chargedoff"
noDeqmaxVals1[which(noDeqmaxVals1 == 2)] = "Completed"
noDeqmaxVals1[which(noDeqmaxVals1 == 3)] = "Defaulted"

confusionMatrix(factor(noDeqmaxVals1), factor(yTest))

coef(noDeqglmnetOut1, s=noDeqelasticOut1$bestTune$lambda)
