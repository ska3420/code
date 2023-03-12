setwd("D:/r intellipaat assignment")
Customer_Churn<-read.csv("Customer_Churn.csv",header = T)
library(caTools)
split_tag<-sample.split(Customer_Churn$Dependents,SplitRatio = 0.70)
train<-subset(Customer_Churn, split_tag==T)
test<-subset(Customer_Churn, split_tag==F)
library(rpart)
mod_tree1<-rpart(Dependents~Partner, data = train, method = "class")
mod_tree1
plot(mod_tree1)
text(mod_tree1)

result_tree1<-predict(mod_tree1,newdata=test,type="class")
head(result_tree1)
#---------------------#
conf1<-table(test$Dependents, result_tree1)
conf1
#-----------#
sum(diag(conf1))/sum(conf1)
#--------------------------------#
mod_tree2<-rpart(Dependents~Partner+InternetService, data= train) 
mod_tree2
#----------------------------#
plot(mod_tree2)
text(mod_tree2)
#---------------------------------#
result_tree2<-predict(mod_tree2,newdata=test,type="class")
head(result_tree2)
#---------------------------------#
conf2<-table(test$Dependents, result_tree2)
#---------------------------------------#
sum(diag(conf2))/sum(conf2)
#------------------------------------------------#



library(caTools)
split_tag<-sample.split(Customer_Churn$gender,SplitRatio = 0.65)
train<-subset(Customer_Churn, split_tag==T)
test<-subset(Customer_Churn, split_tag==F)

library(randomForest)
mod_forest1<-randomForest(gender~MonthlyCharges+tenure, data=train,ntree=35)
#----------------------------#
importance(mod_forest1)
varImpPlot(mod_forest1)
#---------------------------#
result_forest1<-predict(mod_forest1,newdata=test,type="class")
head(result_forest1)
#---------------------------------#
table(test$gender, result_forest1)
#--------------------#
#accuracy#
((538+651)/(538+651+593+683))
#----------------------#


library(randomForest)
mod_forest2<-randomForest(gender~MonthlyCharges+tenure, data=train,ntree=350)
#---------------------#
importance(mod_forest2)
varImpPlot(mod_forest2)
#-----------------------#
result_forest2<-predict(mod_forest2,newdata=test,type="class")
head(result_forest2)
#---------------------------#
table(test$gender, result_forest2)
#-----------------#
#accuracy#
((544+676)/(544+676+568+677))
