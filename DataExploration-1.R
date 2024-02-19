library(ggplot2)

Census2018 <- read.csv("/Users/jacobwatson/Downloads/Census_2018.csv")

yourdata <- read.csv("/Users/jacobwatson/Downloads/Census_2018.csv", na.strings = c("not reported", NA))

attributes(Census2018)

##How many attributes this data set has?
  #26
##How many records this data set has?
  #164610

range(Census2018$educ_num)
range(Census2018$uhrswork)
range(Census2018$incwage)

##What is the range of values for each of these attributes: 
  #educ_num: 1 - 11
  #uhrswork: 1 - 99
  #incwage: 0 - 99000

##The missing values in this data set are marked with “not reported”. Which three 
##attributes have missing values?
  #looking, availble, incwage

typeof(Census2018$city)
typeof(Census2018$educ)
typeof(Census2018$classwkr)
typeof(Census2018$inctot)

##What is the type of each of these attributes: 
  #city: Character
  #educ: Character
  #classwkr: Character
  #inctot: Integer

unique(Census2018$classwkrd)

##What are the categories in variable classwkrd?
  #"federal govt employee"           "state govt employee"            
  #"wage/salary, private"            "wage/salary at non-profit"      
  #"self-employed, not incorporated" "self-employed, incorporated"    
  #"local govt employee"             "unpaid family worker" 

mean(Census2018$inctot)
sd(Census2018$inctot)

##What is the mean and standard deviation for inctot?
#Mean = 65805.12
#STD = 83775.61

ggplot(yourdata, aes(x=incwage)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity", bins = 20) +
  geom_density(alpha=.1)

log_wage <- log(yourdata$incwage)

ggplot(yourdata, aes(x=log_wage)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity", bins = 20) +
  geom_density(alpha=.1)

ggplot(yourdata, aes(x=log_wage)) + 
  geom_point(aes(y=educ_num), alpha=0.5, 
                 position="identity", bins = 20) +
  geom_density(alpha=.1)

ggplot(yourdata, aes(x=classwkr)) + 
  geom_density(aes(y=educ_num), alpha=0.5, 
            position="identity") +
  geom_density(alpha=.1)



