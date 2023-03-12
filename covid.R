rm(list=ls())  #removed all variable stored previously
library(Hmisc) 

data <- read.csv("F:/apurva pdf/r prog/covid_19_r/COVID19_line_list_data.csv")
describe(data) #Hmisc command

#clean up death column
data$death_dummy=as.integer(data$death != 0)
unique(data$death_dummy)

#death rate
sum(data$death_dummy)/nrow(data)

#AGE
#CLAIM : PEOPLE WHO DIE ARE OLDER THAN PEOPLE WHP ARE YOUNGER

dead=subset(data,death_dummy==1)
alive=subset(data,death_dummy==0)
mean(dead$age)
mean(alive$age)
 
#is this statistically significant
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.95)

#here p-value=0 so we reject the null hypothesis
#and conclude that this is statistically significant

#GENDER
#claim gender has no effect
men=subset(data, gender=="male")
women=subset(data, gender=="female")
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)

#is this statistically significant
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.95)

#99% confidence: men have from 0.8% to 8.8% higher chance of dying
#here p-value = 0.002105< 0.005
#so this is statistically significant

