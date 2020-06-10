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

#### Data exploration - visualizations ####

plot(CompleteResponses$elevel, CompleteResponses$brand)
boxplot(CompleteResponses$salary)
boxplot(CompleteResponses$age)
boxplot(CompleteResponses$credit)
boxplot(CompleteResponses$credit)

ggplot(CompleteResponses, aes(brand)) + geom_bar(aes(fill=brand)) + labs(title = "Brand comparison", x="Brand", y="Sales")

ggplot(CompleteResponses, aes(salary, fill=brand)) + geom_histogram(color="black", bins=30)


