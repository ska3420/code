#---------------------------------#
#DATA EXPLORATION#
#QUESTION-1#
Customer_internet_service<-Customer_Churn$InternetService 
Customer_internet_service
#-------------------------------------#
Customer_paperless_billing<-Customer_Churn$PaperlessBilling
Customer_paperless_billing
#--------------------------------------#
Customer_streaming_tv<-Customer_Churn$StreamingTV 
Customer_streaming_tv
#---------------------------------------#
#QUESTION-2#
Customer_Churn[,c(3,6,9)] -> Customer_random_columns
Customer_random_columns
#---------------------------------------------#
Customer_Churn[,10:20] -> Customer_10_20
Customer_10_20
#----------------------------------------------#
Customer_Churn[c(65,765,3726,7000),] -> Customer_random_rows
Customer_random_rows
#----------------------------------------------------#
Customer_random_rows<-Customer_Churn[655:6550,] 
Customer_random_rows
#----------------------------------------------------------#
#Flow Control Statements#
if(Customer_Churn$PaymentMethod[6]=="Electronic check"){
  print("Yes, the payment method is Electronic check")
}
#--------------------------------------------------------------#
if(Customer_Churn$Contract[12]=="Month-to-month"){
  print("The contract is on a month to month basis")
}else if(Customer_Churn$Contract[12]=="One year"){
  print("The contract is on a yearly basis")
}else{print("The contract is on a two-year basis'")}
#--------------------------------------------------------------#
Customer_Churn$MonthlyCharges[6]<-switch (as.character(Customer_Churn$gender[6]),"Male"=Customer_Churn$MonthlyCharges[6]*0.8,"Female"=Customer_Churn$MonthlyCharges[6]*0.5)
Customer_Churn$MonthlyCharges[6]
#--------------------------------------------------------------------#
count=0
for(i in 1:nrow(Customer_Churn)){
  if(Customer_Churn$InternetService[i]=="DSL"){
    count=count+1
  }
}
count
count=0
i=1
while(i<=nrow(Customer_Churn)){
  if(Customer_Churn$tenure[i]==2){
    count=count+1
  }
  i=i+1
}
count


#-------------------------------------------------------------------#
head(Customer_Churn$PhoneService,4)
#-----------------------------------------------------------------#
head(Customer_Churn$Contract,8)
#------------------------------------------------------------------#
tail(Customer_Churn$TotalChargeS,0)
#-----------------------------------------------------------------#
tail(Customer_Churn$tenure,5)
#---------------------------------------------------------------------#
mean(Customer_Churn$tenure)
#----------------------------------------------------------------------#
min(Customer_Churn$tenure)
#----------------------------------------------------------------------#
max(Customer_Churn$tenure)
#------------------------------------------------------------------------#
range(Customer_Churn$tenure)
#-----------------------------------------------
--------------------------#
sample(Customer_Churn$TotalCharges,10)
#-------------------------------------------------------#
table(Customer_Churn$PaymentMethod)
table(Customer_Churn$Contract)
#Data Structures #
fruits<- c('Apple', 'Guava', 'Banana', 'Mango')
fruits
#-----------------------------------------------------------#
hundred<- 1:100
hundred
#--------------------------------------------------------------#
logic_game<- c(TRUE,TRUE,FALSE,FALSE)
logic_game
#-------------------------------------------------------------#
jumbo <- list(c('A','B','C','D'))
jumbo
jumbo <- list(c(55:60))
jumbo
jumbo <- list(c(T,F))
jumbo
#----[-------------------------------------------------------------#
four_trouble<-matrix(data=1:16, nrow=4, ncol=4) 
four_trouble
#-----------------------------------------------------------------#
four_trouble<-matrix(data=1:16, nrow=4, ncol=4, byrow = T) 
four_trouble
#-------------------------------------------------------------#
sky_maze<-array(data=1:32, dim=c(4,4,2)) 
sky_maze
