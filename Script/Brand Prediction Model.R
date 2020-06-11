#C5.0  10-fold cross validation
#dataframe = CompleteResponses
#Y Value = brand

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

#### Analysis of correlation for feature selection ####

ggplot(data=CompleteResponses, aes(x=salary,y=age)) + geom_point(aes(color=brand))

ggplot(data=CompleteResponses, aes(x=elevel,y=car)) + geom_point(aes(color=brand)) 

summary(aov(CompleteResponses$salary ~ CompleteResponses$brand, data = CompleteResponses))
summary(aov(CompleteResponses$age ~ CompleteResponses$brand, data = CompleteResponses))
summary(aov(CompleteResponses$credit ~ CompleteResponses$brand, data = CompleteResponses))
chisq.test(CompleteResponses$elevel, CompleteResponses$brand)
chisq.test(CompleteResponses$car, CompleteResponses$brand)
chisq.test(CompleteResponses$zipcode, CompleteResponses$brand)
cor(CompleteResponses$salary, CompleteResponses$age)
cor(CompleteResponses$salary, CompleteResponses$credit)
cor(CompleteResponses$age, CompleteResponses$credit)
ggplot(data = CompleteResponses, aes(x=CompleteResponses$salary, y=CompleteResponses$age)) + geom_point()

#Training and testing sets
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]
nrow(training)
nrow(testing)

#SetSeed
set.seed(123)

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#Training model all features OOB Automatic Tuning Grid with a tuneLength of 2
C5.0feat <- train(brand~ ., data = training, method = "C5.0",
                  trControl=fitControl, tuneLength = 2)
C5.0feat
#Feat selection
varImp(C5.0feat)

#Training model 2 features OOB Automatic Tuning Grid with a tuneLength of 2
C5.0feat2 <- train(brand~ salary+age, data = training, method = "C5.0",
                   trControl=fitControl, tuneLength = 2)
C5.0feat2

#Training model 2 features Optimized tuneLength of 20
C5.0feat2_opt <- train(brand~ salary+age, data = training, method = "C5.0",
                       trControl = fitControl, tuneLength = 20)
C5.0feat2_opt

#Test predictions
C5.0_predictions <- predict(C5.0feat2_opt,testing)
C5.0_predictions

#Metrics and errors
postResample(pred = C5.0_predictions,obs = testing$brand)
confusionMatrix(C5.0_predictions, testing$brand)
names(testing)
testing$prediction <- C5.0_predictions
testing$error <- testing$brand == testing$prediction
summary(testing$error)
ggplot(testing, aes(x=testing$age, y=testing$salary)) +geom_point(aes(col=testing$error))
ggplot(testing, aes(x=testing$age, y=testing$salary)) +geom_point(aes(col=testing$prediction))

#Predictions for incomplete survey
survey_predictions <- predict(C5.0feat2_opt,SurveyIncomplete)
survey_predictions
summary(survey_predictions)
plot(survey_predictions)
ggplot(SurveyIncomplete, aes(x=SurveyIncomplete$age, 
                             y=SurveyIncomplete$salary)) +geom_point(aes(col=survey_predictions))
SurveyIncomplete$brandprediction <- survey_predictions
write.csv(SurveyIncomplete)
write.csv(SurveyIncomplete,"~/Desktop/Ubiqum/Módulo 2/Task2/Data/SurveyIncomplete.csv")

#Global customer survey
SurveyIncomplete
SurveyIncomplete$brand <- SurveyIncomplete$brandprediction
summary(SurveyIncomplete)
SurveyIncomplete$brandprediction <- NULL
SurveyIncomplete$X <- NULL
CompleteResponses
totalbrand <- rbind(CompleteResponses, SurveyIncomplete)
summary(totalbrand)


f <- ggplot(as.data.frame(table(SurveyIncomplete$brandprediction)), aes(x = "",y = Freq, fill = Var1)) + 
  geom_bar(width = 1, stat = "identity")

f + coord_polar("y", start=0)

histogram(CompleteResponses$age)
