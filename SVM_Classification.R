rm(list=ls(all=TRUE))
setwd("~/Desktop/GNQ3/20170211_Batch25_CSE7405c_SVMNotes&Lab")

# Load required libraries
library(vegan)
library(dummies)
library(dummy)
library(e1071)

attr = c('id', 'age', 'exp', 'inc', 'zip', 'family', 
         'ccavg', 'edu', 'mortgage', 'loan', 
         'securities', 'cd', 'online', 'cc')

# Read the data using csv file
data = read.csv(file = "UniversalBank.csv", 
                header = TRUE, col.names = attr)

# Removing the id, zip and experience. 
drop_Attr = c("id", "zip", "exp")
attr = setdiff(attr, drop_Attr)
data = data[, attr]
rm(drop_Attr)

# Convert attribute to appropriate type  
cat_Attr = c("family", "edu", "securities", 
             "cd", "online", "cc", "loan")
num_Attr = setdiff(attr, cat_Attr)
rm(attr)

cat_Data <- data.frame(sapply(data[,cat_Attr], as.factor))
num_Data <- data.frame(sapply(data[,num_Attr], as.numeric))

data = cbind(num_Data, cat_Data)
rm(cat_Data, num_Data)

# Do the summary statistics and check for missing values and outliers.
summary(data)

#------------------------------------------------------

# Build the classification model.
ind_Num_Attr = num_Attr
rm(num_Attr)
ind_Cat_Attr = setdiff(cat_Attr, "loan")
rm(cat_Attr)

# Standardizing the numeric data
cla_Data = decostand(data[,ind_Num_Attr], "range") 
rm(ind_Num_Attr)

# Convert all categorical attributes to numeric 
# 1. Using dummy function, convert education and family categorical attributes into numeric attributes 
edu = dummy(data.frame(data$edu))
family = dummy(data.frame(data$family))
cla_Data = cbind(cla_Data, edu, family)
ind_Cat_Attr = setdiff(ind_Cat_Attr, c("edu", "family"))
rm(edu, family)

# 2. Using as.numeric function, convert remaining categorical attributes into numeric attributes 
cla_Data = cbind(cla_Data, sapply(data[,ind_Cat_Attr], as.numeric))
rm(ind_Cat_Attr)
ind_Attr = names(cla_Data)

# Append the Target attribute 
cla_Data = cbind(cla_Data, loan=data[,"loan"]) 

str(cla_Data)
cla_Data <- data.frame(sapply(cla_Data,as.numeric))
summary(cla_Data)

# Divide the data into test and train
set.seed(123)

train_RowIDs = sample(1:nrow(cla_Data), nrow(cla_Data)*0.6)
train_Data = cla_Data[train_RowIDs,]
test_Data = cla_Data[-train_RowIDs,]
rm(train_RowIDs)

# Check how records are split with respect to target attribute.
table(cla_Data$loan)
table(train_Data$loan)
table(test_Data$loan)
rm(cla_Data)

# Build best SVM model 

str(train_Data)
train_Data

model = svm(x = train_Data[,ind_Attr], 
            y = train_Data$loan, 
            type = "C-classification", 
            kernel = "linear", cost = 10, gamma = 0.1) 

# Look at the model summary
summary(model)

model$index

plot(cmdscale(dist(train_Data[,ind_Attr])),
     col = as.integer(train_Data$loan),
     pch = c("o","+")[1:nrow(train_Data) %in% model$index + 1])

# Predict on train data  
pred_Train  =  predict(model, train_Data[,ind_Attr])  

# Build confusion matrix and find accuracy   
cm_Train = table(train_Data$loan, pred_Train)
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
rm(pred_Train, cm_Train)

# Predict on test data
pred_Test = predict(model, test_Data[,ind_Attr]) 

# Build confusion matrix and find accuracy   
cm_Test = table(test_Data$loan, pred_Test)
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)

accu_Train
accu_Test

rm(model, accu_Test, accu_Train, ind_Attr, train_Data, test_Data)

