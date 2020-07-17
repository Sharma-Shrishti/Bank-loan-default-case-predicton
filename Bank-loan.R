library(ggplot2)
library(GGally)
library(mltools)
library(data.table)
library(caTools)
set.seed(123)
library(readr)
library(dplyr)
library(caret)
library(randomForest)

setwd("C:/Users/USER/Desktop/Data Science/Questions")

data=read.csv("bank-loan.csv") # importing dataset

dim(data)   # dimension of data

summary(data)  # summary of data

str(data)

colSums(sapply(data,is.na)) # check missing values

data$ed=as.factor(data$ed)# Converting as category type

table(data$default)  #Distribution of target variable

ggplot(data,aes(x=default))+geom_bar(color='blue',fill='#FF6666')

# Plotting different graphs
  # Numeric graphs

ggplot(data,aes(x=age))+geom_histogram(fill="#DD1111")  
ggplot(data,aes(x=employ))+geom_histogram(fill="#FF6666")
ggplot(data,aes(x=income))+geom_histogram(fill="#DD1111")  
ggplot(data,aes(x=debtinc))+geom_histogram(fill="#FF6666")
ggplot(data,aes(x=creddebt))+geom_histogram(fill="#DD1111")  
ggplot(data,aes(x=othdebt))+geom_histogram(fill="#FF6666")

 # Numeric and categorical graphs

ggplot(data,aes(x=default,y=age))+geom_point()
ggplot(data,aes(x=default,y=employ))+geom_point()
ggplot(data,aes(x=default,y=income))+geom_point()
ggplot(data,aes(x=default,y=debtinc))+geom_point()
ggplot(data,aes(x=employ,y=age))+geom_point()

# Check for the outlier
boxplot(data$age,main="age")

boxplot(data$employ,main="employ")

boxplot(data$address,main="address")

boxplot(data$income, main="income")

boxplot(data$debtinc,main="debtinc")

boxplot(data$creddebt,main="creddebt")

boxplot(data$othdebt)

# Heatmap
data_num=select_if(data,is.numeric)
cor(data_num)
ggcorr(data_num,label=TRUE)

# Encoding
data = one_hot(as.data.table(data))

data$default=as.factor(data$default)


# Splitting data into train,test and validation data
 # test data
X_test=data[is.na(data$default),]
X_test=subset(X_test, select=-c(default))

train_data=data[!is.na(data$default),]

 # train and validation data
split = sample.split(data$default, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
val_set = subset(data, split == FALSE)


# Feature Scaling
training_set[,c(1:12)]=as.data.frame(scale(training_set[,c(1:12)]))
val_set[,c(1:12)]=as.data.frame(scale(val_set[,c(1:12)]))
X_test=as.data.frame(scale(X_test))


#Logistic Regression
classifier = glm(formula= default ~ ., family = binomial,data = training_set)
prob_pred = predict(classifier, type = 'response', newdata = val_set)
y_pred = ifelse(prob_pred > 0.5, 1, 0)

cm = as.matrix(table(val_set$default, y_pred > 0.5))
print(cm)

accuracy=sum(diag(cm)/sum(cm))
print(accuracy)

#Random Forest

fit.forest = randomForest(default~.,data = training_set, na.action = na.roughfix, importance=TRUE)

pred = predict(fit.forest,newdata = val_set[,-13])
cm=as.matrix(table(pred,val_set$default))
print(cm)

accuracy=sum(diag(cm)/sum(cm))
print(accuracy)

# Final Model Random forest
#predicting the output
pred_out=predict(fit.forest,new_data=X_test[,-12])

