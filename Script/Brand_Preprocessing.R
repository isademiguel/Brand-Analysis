library(readr)
library(rstudioapi)
library(caret)
library(ggplot2)
library(dplyr)

#### Upload and explore data ####

setwd(dirname(getActiveDocumentContext()$path))
CompleteResponses <- read.csv("../Data/CompleteResponses.csv")
setwd(dirname(getActiveDocumentContext()$path))
SurveyIncomplete <- read.csv("../Data/SurveyIncomplete.csv")

# Exploring data
attributes(CompleteResponses)
summary(CompleteResponses)
str(CompleteResponses)
sum(is.na(CompleteResponses))


#### Preprocessing ####

# Transforming data type

CompleteResponses <- CompleteResponses %>% 
  mutate(elevel=as.factor(elevel)) %>% 
  mutate(car=as.factor(car)) %>% 
  mutate(zipcode=as.factor(zipcode)) %>% 
  mutate(brand=as.factor(brand))

str(CompleteResponses)

SurveyIncomplete <- SurveyIncomplete %>% 
  mutate(elevel=as.factor(elevel)) %>% 
  mutate(car=as.factor(car)) %>% 
  mutate(zipcode=as.factor(zipcode)) %>% 
  mutate(brandprediction=as.factor(brandprediction))

str(SurveyIncomplete)

# Change brand names

levels(CompleteResponses$brand)[levels(CompleteResponses$brand)=="0"] <- "Acer"
levels(CompleteResponses$brand)[levels(CompleteResponses$brand)=="1"] <- "Sony"

# Check duplicates
duplicates <- CompleteResponses %>% 
  duplicated() %>% 
  table()

duplicates

# Check NA
sum(is.na(CompleteResponses))

# Check outliers
boxplot(CompleteResponses$salary)
boxplot(CompleteResponses$age)
boxplot(CompleteResponses$credit)


#### Data exploration - visualizations ####

plot(CompleteResponses$elevel, CompleteResponses$brand)

ggplot(CompleteResponses, aes(brand)) + geom_bar(aes(fill=brand)) + labs(title = "Brand comparison", x="Brand", y="Sales")
ggplot(CompleteResponses, aes(salary, 
                              fill=brand)) + geom_histogram(color="black", 
                                                            bins=30) + labs (title = "Distribution of salary per brand", x="Salary", y="Count")
ggplot(CompleteResponses, aes(x=age, fill=brand)) + geom_bar(aes(color=brand)) + labs (title="Distribution age per brand", x="Age", y="Count")

#### Analysis of correlation for feature selection ####

ggplot(data=CompleteResponses, aes(x=salary,y=age)) + geom_point(aes(color=brand)) + labs (title= "Correlation salary and age by brand", x="Salary", y="Age")

ggplot(data=CompleteResponses, aes(x=elevel,y=car)) + geom_point(aes(color=brand)) + labs (title="Correlation education and car by brand", x="Education level", y="Car")

summary(aov(CompleteResponses$salary ~ CompleteResponses$brand, data = CompleteResponses)) # There is a clear correlation between salary and the response variable
summary(aov(CompleteResponses$age ~ CompleteResponses$brand, data = CompleteResponses))
summary(aov(CompleteResponses$credit ~ CompleteResponses$brand, data = CompleteResponses))

chisq.test(CompleteResponses$elevel, CompleteResponses$brand)
chisq.test(CompleteResponses$car, CompleteResponses$brand)
chisq.test(CompleteResponses$zipcode, CompleteResponses$brand)
# Categorical features doesn't have a strong correlation with brand

# Checking collinearity to remove non neccessary numerical features
cor(CompleteResponses$salary, CompleteResponses$age)
cor(CompleteResponses$salary, CompleteResponses$credit)
cor(CompleteResponses$age, CompleteResponses$credit) 


#### Data exploration - third round ####

brand_bysalary <- CompleteResponses %>% 
  group_by(brand) %>% 
  summarize(avg_salary= mean(salary, na.rm=TRUE))
brand_bysalary

brand_credit <- CompleteResponses %>%
  group_by(brand) %>% 
  summarize(avg_credit = mean(credit, na.rm=TRUE))
brand_credit


level_dif <- CompleteResponses %>% 
  group_by(elevel) %>% 
  summarize(level_salary = mean(salary))
level_dif





