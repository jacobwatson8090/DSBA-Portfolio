install.packages(ggplot2)
library(ggplot2)

Census2018 <- read.csv("")

UpdtdCensus <- read.csv("", na.strings = c("not reported", NA))

attributes(UpdtdCensus)

##How many attributes this data set has?
  #26
##How many records this data set has?
  #164610

range(UpdtdCensus$educ_num)
range(UpdtdCensus$uhrswork)
range(UpdtdCensus$incwage)

##What is the range of values for each of these attributes: 
  #educ_num: 1 - 11
  #uhrswork: 1 - 99
  #incwage: 0 - 99000

##The missing values in this data set are marked with “not reported”. Which three 
##attributes have missing values?
  #looking, availble, incwage

typeof(UpdtdCensus$city)
typeof(UpdtdCensus$educ)
typeof(UpdtdCensus$classwkr)
typeof(UpdtdCensus$inctot)

##What is the type of each of these attributes: 
  #city: Character
  #educ: Character
  #classwkr: Character
  #inctot: Integer

unique(UpdtdCensus$classwkrd)

##What are the categories in variable classwkrd?
  #"federal govt employee"           "state govt employee"            
  #"wage/salary, private"            "wage/salary at non-profit"      
  #"self-employed, not incorporated" "self-employed, incorporated"    
  #"local govt employee"             "unpaid family worker" 

mean(UpdtdCensus$inctot)
sd(UpdtdCensus$inctot)

##What is the mean and standard deviation for inctot?
#Mean = 65805.12
#STD = 83775.61

ggplot(Census_2018, aes(x=incwage)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity", bins = 15) +
  geom_density(alpha=.1)

log_wage <- log(Census_2018$incwage)

ggplot(Censu_2018, aes(x=log_wage)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity", bins = 15) +
  geom_density(alpha=.1)

ggplot(Census_2018, aes(x=log_wage)) + 
  geom_point(aes(y=educ_num), alpha=0.5, 
                 position="identity")

ggplot(Census_2018, aes(x=classwkr)) + 
  geom_boxplot(aes(y=educ_num), alpha=0.5, 
            position="identity") 



