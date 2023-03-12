setwd("D:/r intellipaat assignment")
Customer_Churn<-read.csv("Customer_Churn.csv",header = T)
log_mod_1<-glm(Churn~TechSupport, data= Customer_Churn, family="binomial")
#-----------------------------#
summary(log_mod_1)
#----------------------------------#
predict(log_mod_1,data.frame(TechSupport="Yes"),type="response")
#---------------------------------------------#
predict(log_mod_1,data.frame(TechSupport="No"),type="response")
#-------------------------------------------#
predict(log_mod_1,data.frame(TechSupport="No internet service"),type="response")
#--------------------------------------#

log_mod_2<-glm(Dependents~tenure, data= Customer_Churn, family="binomial")
log_mod_2
#-----------------------------------#
summary(log_mod_2)
#-----------------------------------#
predict(log_mod_2,data.frame(tenure=10),type="response")
#_-------------------------------------#
predict(log_mod_2,data.frame(tenure=50),type="response")
#---------------------------------------#
predict(log_mod_2,data.frame(tenure=70),type="response")
#------------------------------------#

library(caTools)
split_tag<-sample.split(Customer_Churn$Churn,SplitRatio = 0.65)
train<-subset(Customer_Churn, split_tag==T)
test<-subset(Customer_Churn, split_tag==F)
#-------------------------------------#
log_mod_multi<-glm(gender~Dependents+InternetService+Contract, data= train, family="binomial")
#-------------------------------------#
result_log_multi<-predict(log_mod_multi,newdata = test,type="response")
#--------------------------------------#
range(result_log_multi)
table(test$gender, result_log_multi>0.49)
#--------------------------------------#
#accuracy=true positive+true negative/true positive+true negative+false positive+false negative#
((59+1154)/(59+1154+68+1184))
#---------------------------------------#

log_mod_multi2<-glm(gender~tenure+MonthlyCharges+PaymentMethod, data= train, family="binomial") 
#------------------------------------------#
result_log_multi2<-predict(log_mod_multi2,newdata = test,type="response")
#------------------------------------#
range(result_log_multi2)
table(test$gender, result_log_multi2>0.49)
#-------------------------------------#
#accuracy=true positive+true negative/true positive+true negative+false positive+false negative#
((125+1074)/(125+1074+124+1142))
#----------------------------------------#

library(caTools)
split_tag<-sample.split(Customer_Churn$Churn,SplitRatio = 0.80)
train<-subset(Customer_Churn, split_tag==T)
test<-subset(Customer_Churn, split_tag==F)
#------------------------------------------#
log_mod_roc<-glm(Churn~tenure+MonthlyCharges+PaymentMethod, data= train, family="binomial")
#-------------------------------------#
result_log_roc<-predict(log_mod_roc,newdata = test,type="response") 
#-------------------------------------#
library(ROCR)
predict_log_roc<-prediction(result_log_roc,test$Churn) 
accuracy<-performance(predict_log_roc,"acc")
plot(accuracy)
#--------------------------------------#
roc_curve<-performance(predict_log_roc,"tpr","fpr")
plot(roc_curve)
#--------------------------------------#
auc<-performance(predict_log_roc,"auc")
auc

