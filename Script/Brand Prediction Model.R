#C5.0  10-fold cross validation
#dataframe = CompleteResponses
#Y Value = brand

library(readr)
library(rstudioapi)
library(caret)
library(ggplot2)

#Uploading data set
setwd(dirname(getActiveDocumentContext()$path))
CompleteResponses <- read.csv("../Data/CompleteResponses.csv")
setwd(dirname(getActiveDocumentContext()$path)) # One time is enought
SurveyIncomplete <- read.csv("../Data/SurveyIncomplete.csv")

#Exploring data
attributes(CompleteResponses)
summary(CompleteResponses)
str(CompleteResponses)
sum(is.na(CompleteResponses))

#Preprocessing
CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
CompleteResponses$brand<-as.factor(CompleteResponses$brand)

SurveyIncomplete$elevel<-as.factor(SurveyIncomplete$elevel)
SurveyIncomplete$car<-as.factor(SurveyIncomplete$car)
SurveyIncomplete$zipcode<-as.factor(SurveyIncomplete$zipcode)
SurveyIncomplete$brand<-as.factor(SurveyIncomplete$brand)

#Plots
plot(CompleteResponses$elevel, CompleteResponses$brand)
boxplot(CompleteResponses$salary)
boxplot(CompleteResponses$age)
boxplot(CompleteResponses$credit)
boxplot(CompleteResponses$elevel) # Does not work
boxplot(CompleteResponses$zipcode) # Does not work
boxplot(CompleteResponses$car) # Does not work
qqnorm(CompleteResponses$credit)

#Analysis of correlation for feature selection
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
ggplot(testing, aes(x=testing$age, y=testing$salary)) +geom_point(aes(col=testing$error)) # Nice, but you can do it like this as well (more simple I think)
# Nice, but you can do it like this as well (more simple I think)
ggplot(testing, aes(age, salary, color = prediction)) +
  geom_point()

ggplot(testing, aes(x=testing$age, y=testing$salary)) +geom_point(aes(col=testing$prediction))

#Real predictions
survey_predictions <- predict(C5.0feat2_opt,SurveyIncomplete)
survey_predictions
summary(survey_predictions)
plot(survey_predictions)
ggplot(SurveyIncomplete, aes(x=SurveyIncomplete$age, y=SurveyIncomplete$salary)) +geom_point(aes(col=survey_predictions))
SurveyIncomplete$brandprediction <- survey_predictions
SurveyIncomplete$brand <- NULL
SurveyIncomplete$prediction <- NULL
write.csv(SurveyIncomplete)
write.csv(SurveyIncomplete,"~/Desktop/Ubiqum/Módulo 2/Task2/Data/SurveyIncomplete.cs"   )# Careful to keep making the path relative !