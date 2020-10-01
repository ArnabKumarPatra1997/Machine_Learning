rm(list=ls(all=TRUE))
getwd()
setwd("E:/College Projects (Sem 3)/ML")
labour_data=read.csv("labour_income.csv")
#Lasso and ridge helps in reducing over fitting. Lasso is absolute sq of coeff. 
str(labour_data)
summary(labour_data)
set.seed(007)
train_rows<-sample(x=seq(1,nrow(labour_data),1),size=0.7*nrow(labour_data))
train_data<-labour_data[train_rows,]
test_data<-labour_data[train_rows,]

#Standardizing the Data(Data Normalization)
library(caret)
std_obj<-preProcess(x=train_data[,!colnames(train_data) %in% c("wages")],
                    method=c("center","scale"))
train_std_data<-predict(std_obj,train_data)
test_std_data<-predict(std_obj,train_data)

#Dummifying the Data
dummy_obj<- dummyVars(~.,train_std_data)
train_dummy_data<-as.data.frame(predict(dummy_obj,train_std_data))
test_dummy_data<-as.data.frame((predict(dummy_obj,train_std_data)))

#Getting the data into a compatible format
x_train<- as.matrix(train_dummy_data[,-1])
y_train<-as.matrix(train_dummy_data[,1])
x_test<-as.matrix(test_dummy_data[,-1])
y_test<-as.matrix(test_dummy_data[,1])

#Hyper Parametere Tuning(Choose an optimal lambda value for the ridge and lasso regression models by using cross validation)
#Choosing a lambda for Lasso Regression(Alpha value is 1 for lasso regression)
install.packages("glmnet")
library(glmnet)
cv_lasso<-cv.glmnet(x_train,y_train,alpha=1,type.measure = "mse",nfolds = 4)
plot(cv_lasso)

plot(cv_lasso$glmnet.fit,xvar = "lambda",label=TRUE)

print(cv_lasso$lambda.min)
coef(cv_lasso)

#Choosing a lambda for Ridge Regression(The alpha value is 0 for ridge regression)
cv_ridge<-cv.glmnet(x_train,y_train,alpha=0,type.measure = "mse",nfolds = 4)
plot(cv_ridge)                    
plot(cv_ridge$glmnet.fit,xvar = "lambda",label=TRUE)

print(cv_ridge$glmnet.fit.min)
coef(cv_ridge)

#Building the Final Model(Using the optimal lambda values obtained above we can build our ridge and lasso models)
#Building the final Lasso Regression Model
lasso_model<- glmnet(x_train,y_train,lambda = cv_lasso$lambda.min,alpha = 1)
coef(lasso_model)
preds_lasso<-predict(lasso_model,x_test)

#Building the Final Ridge Regression Model
ridge_model<-glmnet(x_train,y_train,lambda = cv_ridge$lambda.min,alpha = 0)
coef(ridge_model)
preds_ridge<-predict(ridge_model,x_test)

#Model Performance Evaluation(Lasso Regression Model Metrics)
library(DMwR)
regr.eval(trues=y_test,preds=preds_lasso)

#Ridge Regression Model Metrics
regr.eval(trues=y_test,preds=preds_ridge)
