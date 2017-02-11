rm(list = ls())
setwd("~/Desktop/GNQ3 Machine   Learning/20170211_Batch25_CSE7405c_SVMNotes&Lab")
str(df)
library(vegan)
library(dummies)
library(e1071)

attr = c('id','age','exp','inc','zip','family','ccavg','edu','mortage',
         'loan','securities','cd','online','cc')

df <- read.csv("UniversalBank.csv",header = TRUE,col.names = attr)
str(df)

summary(df)

drop_attr <- c('id','zip','exp')

attr <- setdiff(attr,drop_attr)

df <- df[,attr]

sum(is.na(df))

catgr_with_0_1 <- c('loan','securities','cd','online','cc')

catr_with_more_than_0_1 <- c('family','edu') 

cat_data <- cbind(df[,catgr_with_0_1],df[,catr_with_more_than_0_1]) 

num_data <- setdiff(colnames(df),c(catgr_with_0_1,catr_with_more_than_0_1))


catgr_with_0_1 <- df[,catgr_with_0_1]

catr_with_more_than_0_1 <- df[,catr_with_more_than_0_1]

catgr_with_0_1 <- data.frame(sapply(catgr_with_0_1,as.numeric))

catr_with_more_than_0_1 <- data.frame(sapply(catr_with_more_than_0_1,as.factor))

str(catr_with_more_than_0_1)

num_data <- df[,num_data]

cat_data <- data.frame(sapply(cat_data,as.factor))


str(cat_data)

num_data <- data.frame(sapply(num_data,as.numeric))

str(num_data)

final_data <- cbind(num_data,catgr_with_0_1,catr_with_more_than_0_1)

str(final_data)

library(dummies)

final.data.new <- dummy.data.frame(final_data, sep = ".")

names(final.data.new)


standardize_data <- decostand(final.data.new[,c("age","inc","ccavg","mortage","loan","securities","cd","online","cc")],method = "range")

str(standardize_data)

str(final.data.new)

final.data.new[,c("age","inc","ccavg","mortage","loan","securities","cd","online","cc")] = standardize_data[,c("age","inc","ccavg","mortage","loan","securities","cd","online","cc")]


df <- final.data.new

df$loan <- as.factor(df$loan)
set.seed(123)
trainrows <- seq(1,nrow(df),1)
rows <- sample(trainrows,nrow(df)*.60)
train <- df[rows,]
remain <- df[-rows,]

trainrows <- seq(1,nrow(remain),1)
rows <- sample(trainrows,nrow(remain)*.50)
test <- remain[rows,]
eval <- remain[-rows,]


table(train$loan)

table(test$loan)

table(eval$loan)

str(train)

str(train[,-5])

model = svm(x = train[,-5],y = train$loan,type = "C-classification",kernel = "linear",cost = 10,gamma = 0.1)

summary(model)

#to convert the data to 2 dimension cmdscale. using eigen vectors. Wseful to see how data  is captured in 2 dimension 
plot(cmdscale(dist(train[,-5])),col=as.integer(train$loan),pch = c("o","+")[1:nrow(train)  %in% model$index +1])

predict_train = predict(model,train[,-5])

cm_train = table(train$loan,predict_train)

library(caret)

confusionMatrix(cm_train)

pred_Test = predict(model,test[,-5])
cm_test = table(test$loan,pred_Test)

confusionMatrix(cm_test)

pred_eval = predict(model,eval[,-5])
cm_eval <- table(eval$loan,pred_eval)
confusionMatrix(cm_eval)

tuneresult <- tune(svm,train[,-5],train$loan,ranges = list(gamma=10^(-6:-1),cost = 2^(2:3)))
print(tuneresult)

tunemodel  <- tuneresult$best.model
tunemodelY <- predict(tunemodel,as.matrix(train[,-5]))
conf <- table(train$loan,tunemodelY)

confusionMatrix(conf)


