setwd("D:/r intellipaat assignment")
Customer_Churn<-read.csv("Customer_Churn.csv",header = T)
library(dplyr)
as.data.frame->customer_features
as.data.frame
customer_features->customer_churn %>% select("tenure","MonthlyCharges","TotalCharges")
customer_features
#---------------------#
na.omit(customer_features) -> customer_features
#--------------------#
kmeans(customer_features,3) -> cluster_features
#---------------------------#
cbind(customer_features, Clusters=cluster_features$cluster) ->cluster_group