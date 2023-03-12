setwd("D:/r intellipaat assignment")
Customer_Churn<-read.csv("Customer_Churn.csv",header = T)
Customer_Churn
#---------------------#
library(caTools)
split_tag<-sample.split(Customer_Churn$tenure,SplitRatio = 0.70)
train<-subset(Customer_Churn, split_tag==T)
test<-subset(Customer_Churn, split_tag==F)
#---------------------#
model1<-lm(tenure~Contract, data=train)
#---------------------#
predicted_values<-predict(model1, newdata=test)
predicted_values
head(predicted_values)
#----------------------#
final_data<-cbind(Actual=test$tenure,Predicted=predicted_values)
final_data<-as.data.frame(final_data) 
head(final_data)
#-----------------------#
final_data_a<-final_data$Actual 
final_data_P<-final_data$Predicted
error<-final_data_a-final_data_P
head(error)
#------------------------------#
final_data<-cbind(final_data,error)
head(final_data)
#--------------------------------#
sqrt(mean((final_data$error)^2))
#-----------------------------------------------------------------#


library(caTools)
split_model<-sample.split(Customer_Churn$tenure,SplitRatio = 0.75)
train<-subset(Customer_Churn,split_model==T)
test<-subset(Customer_Churn, split_model==F)
#------------------------------------#
mod_multi_linear<-lm(MonthlyCharges~Dependents+MultipleLines+OnlineSecurity+OnlineBackup+DeviceProtection, data=train)
mod_multi_linear
#--------------------------------#
predicted_multi_linear<-predict(mod_multi_linear, newdata=test)
head(predicted_multi_linear)
#-------------------------------#
final_data<-cbind(Actual=test$MonthlyCharges,Predicted=predicted_multi_linear)
final_data<-as.data.frame(final_data) 
head(final_data)
#------------------------------------#
final_data_A<-final_data$Actual
final_data_p<-final_data$Predicted


error<-final_data_A-final_data_p
head(error)
#-------------------------------------#
final_data<-cbind(final_data,error)
head(final_data)
#-----------------------------------------#
sqrt(mean((final_data$error)^2))