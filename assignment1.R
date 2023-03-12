setwd("D:/r intellipaat assignment")
Customer_Churn<-read.csv("Customer_Churn.csv",header = T)
Customer_Churn
Customer_Churn1<-read.csv("Customer_Churn.csv",header = T)
Customer_Churn1
Customer_Churn2<-read.csv("Customer_Churn.csv",header = T)
Customer_Churn2

#------------------------------------------------------#

#   QUESTION-1    #
Customer_Churn$MonthlyCharges[5]+5.00 -> Customer_Churn$MonthlyCharges[5]
Customer_Churn$MonthlyCharges[5]

#---------------------------------------------------------#
Customer_Churn$MonthlyCharges[6]-9.65 -> Customer_Churn$MonthlyCharges[6]
Customer_Churn$MonthlyCharges[6]
#----------------------------------------------------------#
Customer_Churn$MonthlyCharges[1]*3 -> Customer_Churn$MonthlyCharges[1]
Customer_Churn$MonthlyCharges[1]
#----------------------------------------------------------#
Customer_Churn$MonthlyCharges[37]/3 -> Customer_Churn$MonthlyCharges[37]
Customer_Churn$MonthlyCharges[37]

#    QUESTION-2    #
Customer_Churn$tenure[1]>Customer_Churn$tenure[10]

#----------------------------------------------------------#
Customer_Churn$tenure[3]==Customer_Churn$tenure[5]
#----------------------------------------------------------#

#   QUESTION-3    #

Customer_Churn$TechSupport=="Yes" & Customer_Churn$StreamingTV=="Yes" -> count_tech_stream
table(count_tech_stream)

#----------------------------------------------------------#
Customer_Churn$InternetService=="DSL" | Customer_Churn$InternetService=="Fiber optic" -> dsl_fiber
subset(Customer_Churn,dsl_fiber==T) -> internet_dsl_fiber
