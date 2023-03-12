
install.packages("dplyr",verbose=T)
library(dplyr)
Customer_Churn <- churn$tenure
Customer_Churn
summary(Customer_Churn, mean_tenure =mean(R))
sd(Customer_Churn)
var(Customer_Churn)
#---------------------------------------------#
Customer_Churn <- churn$MonthlyCharges
Customer_Churn
summary(Customer_Churn, mean_MonthlyCharges =mean(R))
sd(Customer_Churn)
var(Customer_Churn)
#-------------------------------------------------#
