rm(list=ls(all=TRUE))

library(DMwR)
library(e1071)

setwd("~/Desktop/GNQ3/20170211_Batch25_CSE7405c_SVMNotes&Lab")

data <- read.csv('BostonHousing.csv')
str(data)
summary(data)

# Divide the data into test and train
set.seed(123)

train_RowIDs = sample(1:nrow(data), nrow(data)*0.7)
train_Data = data[train_RowIDs,]
test_Data = data[-train_RowIDs,]
rm(train_RowIDs)

# Check how records are split with respect to target attribute.
summary(data$medv)
summary(train_Data$medv)
summary(test_Data$medv)
rm(data)

# Build best SVM model 
model = svm(x = train_Data[,1:13], 
            y = train_Data[,14], 
            type = "nu-regression", 
            kernel = "linear", cost = 1e-7) 

# Look at the model summary
summary(model)

# Predict on train data and check the performance
regr.eval(train_Data$medv, predict(model, train_Data[,1:13]))

# Predict on test data and check the performance  
regr.eval(test_Data$medv, predict(model, test_Data[,1:13]))

rm(model)

# Hyperparameter tuning 
tuned <- tune.svm(x = train_Data[,1:13], 
                  y = train_Data[,14], 
                  type = "nu-regression", 
                  gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)

rm(test_Data, train_Data, tuned)

