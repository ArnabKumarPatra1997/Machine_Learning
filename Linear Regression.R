
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL building ------##
##----------------------------------------------------------------------------------##


##--- Step 1: Clear environment variables ------------------------------------------##
rm(list = ls(all=TRUE))

##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##
getwd()
##setwd if you want to change working directory

##__________________________________________________________________________________##


##--- Step 3: Read the data from the csv file --------------------------------------##
cars_data=read.csv(file = "Toyota_SimpleReg.csv",header=T)
names(cars_data)
str(cars_data)
summary(cars_data)

##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop any irrelevant attribute(s):
cars_data=cars_data[,-c(1,2)]
## Summary of the data and look for any missing values:
str(cars_data)
summary(cars_data)

## Correlation and Covariance between the attributes:
cov(cars_data)
#The cov of the Age of car and Price is -59136.11
#It indicates a neg linear relationship b/w the 2 variables
#This relation could be observed from the scatter plot also

#Describe how the covarainace and correlation coefficients 
plot(cars_data$Age_06_15,cars_data$Price)
#pch is for the thickness of each point plot
plot(cars_data$Age_06_15,cars_data$Price, xlab = "Age of the car", ylab = "Price of the car in($)",pch=10,col="purple")

cor(cars_data)
cor(cars_data$Age_06_15,cars_data$Price)
#Do the attributes have a good enough correlation coefficient to support linear regres  sion model building?

##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (80:20) ratio
rows = seq(1, nrow(cars_data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(cars_data))/100)
cars_train=cars_data[trainRows,]
cars_test=cars_data[-trainRows,]

trainRows1=sample(rows,(80*nrow(cars_data))/100)
cars_train1=cars_data[trainRows1,]
cars_test1=cars_data[-trainRows1,]

trainRows2=sample(rows,(90*nrow(cars_data))/100)
cars_train2=cars_data[trainRows2,]
cars_test2=cars_data[-trainRows2,]
##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##

LinReg=lm(Price~Age_06_15,data = cars_train)
LinReg
coefficients(LinReg)
LinReg1=lm(Price~Age_06_15,data = cars_train1)
coefficients(LinReg1)
LinReg2=lm(Price~Age_06_15,data = cars_train2)
coefficients(LinReg2)

## Summary of model:
summary(LinReg)#refer to signif. codes
plot(LinReg$residuals)
summary(LinReg1)
summary(LinReg2)
#Extract the intercept coefficient from the linear regression model
coefficients(LinReg)
coefficients(LinReg)[1]
coefficients(LinReg)[2]
names(coefficients(LinReg))
#Extract the residual values
LinReg$residuals
LinReg$rank
##__________________________________________________________________________________##
#To extract the train predictions
LinReg$fitted.values
plot(LinReg$fitted.values)
##--- Step 7: Check for validity of linear regression assumptions ------------------##
#HINT: plot the 4 graphs to check. Write your comments
par(mfrow=c(2,2))
plot(LinReg)
par(mfrow=c(1,1))
##__________________________________________________________________________________##


##--- Step 8: Predict on testdata --------------------------------------------------##
test_prediction=predict(LinReg,cars_test)
test_actual=cars_test$Price

##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##
install.packages("DMwR")
library(DMwR)
#Error verification on train data
regr.eval(cars_train$Price, LinReg$fitted.values)


#Error verification on test data
regr.eval(test_actual,test_prediction)


##__________________________________________________________________________________##


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
#Find the confidence and prediction intervals and plot them for the WHOLE dataset
Conf_Pred=data.frame(predict(LinReg,cars_test,interval = "confidence",lwd=2))
Pred_Pred=data.frame(predict(LinReg,cars_test,interval = "prediction",lwd=4))

names(Conf_Pred)
#Data Visualization
plot(cars_test$Age_06_15, cars_test$Price, xlab = "Age of the car", ylab = "Price in ($)")

points(cars_test$Age_06_15,Conf_Pred$fit,type="l", col="green", lwd=2)
points(cars_test$Age_06_15,Conf_Pred$lwr,pch="-", col="red", lwd=4)
points(cars_test$Age_06_15,Conf_Pred$upr,pch="-", col="red", lwd=4)
points(cars_test$Age_06_15,Pred_Pred$lwr,pch="-", col="blue", lwd=4)
points(cars_test$Age_06_15,Pred_Pred$upr,pch="-", col="blue", lwd=4)
##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##