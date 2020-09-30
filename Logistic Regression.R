rm(list=ls(all=TRUE))
getwd()
#Logistic Regression

#REad in Flier Response Data
flierresponse<-read.csv("FlierResponse.csv",header = T,sep = ",")
flierresponse
str(flierresponse)
flierresponse$Response<-as.factor(flierresponse$Response)
summary(flierresponse)
flierresponseglm<-glm(Response~Age,data = flierresponse,family = "binomial")
flierresponseglm
summary(flierresponseglm)
logLik(flierresponseglm)
deviance(flierresponseglm)
AIC(flierresponseglm)

#CASE Study - The Framingham Heart Study

#Read in the Framingham dataset
framingham<-read.csv("framingham.csv")
#Look at the structure
str(framingham)
framingham$education=factor(framingham$education)
str(framingham)

#Load the library caTools
install.packages("caTools")
library(caTools)
install.packages("DAAG")
library(DAAG)
install.packages("rms")
library(rms)
install.packages("car")
library(car)


#Randomly split the data into training and testing sets
set.seed(1000)
split=sample.split(framingham$TenYearCHD,SplitRatio = 0.70)

#Split up the data using subset
train=subset(framingham, split==TRUE)
test=subset(framingham,split==FALSE)

#Logistic Regression Model
framinghamLog=glm(TenYearCHD~.,data = train,family=binomial)
summary(framinghamLog)
car::vif(framinghamLog)
rms::vif(framinghamLog)
DAAG::vif(framinghamLog)

#Accuracy on the training set
predictTrain=predict(framinghamLog,type="response",newdata = train)
predictTrain
#Confusion Matrix with threshold of 0.5
table(train$TenYearCHD, predictTrain>0.5)
#The default value for threshold on which we generally get a confusion matric is 0.5. Threshold val
#Accuracy on Train set
(2170+30)/(2170+30+357+9)
#Precision (true positives/predicted positives)=TP/TP+FP
(2170)/(2170-357)
#Sensitivity aka Recall (true positives/all actual positives)=TP/TP+FN
(2170)/(2170+9)
#Specificity (true negatives/all actual negatives)=TN/TN+FP
(30)/(30+357)
#Predicions of the test set
predictTest=predict(framinghamLog,type = "response",newdata = test)

#Confusion matric with threshold of 0.5
table(test$TenYearCHD,predictTest>0.5)

#Accuracy on Test Set
(915+12)/(915+12+158+7)
#HW- Accuracy Levels

#Confusion matrix with threshold of 0.9
table(test$TenYearCHD,predictTest>0.9)
#do it for predictTest>0.7,0.5,0.3,0.1

#Test set AUC
install.packages("ROCR")
library(ROCR)
ROCRpred=prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred,"auc")@y.values)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
par(mfrow=c(1,1))
plot(ROCRperf,colorize=TRUE,print.cutoffs.at~seq(0.1,by~0.1),text.adj~c(-0.2,1.7))

