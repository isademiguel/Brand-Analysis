set.seed(123)

#C5.0  10-fold cross validation

library(readr)
library(rstudioapi)
library(caret)
library(ggplot2)
library(dplyr)

#### First model C5.0 ####

# Training and testing sets
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]
nrow(training)
nrow(testing)

# Checking distribution training and testing sets
ggplot(training, aes(age)) + geom_histogram(aes(fill=brand))
ggplot(testing, aes(age)) + geom_histogram(aes(fill=brand))
table <- prop.table(table(training$brand))
table
table2 <- prop.table(table(testing$brand))
table2

# 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

# Training model all features OOB Automatic Tuning Grid with a tuneLength of 2
C5.0feat <- train(brand~ ., data = training, method = "C5.0",
                  trControl=fitControl, tuneLength = 2)
C5.0feat
# Feat selection
varImp(C5.0feat) # Confirmed that salary and age are best predictors

# Training model 2 features OOB Automatic Tuning Grid with a tuneLength of 2
C5.0feat2 <- train(brand~ salary+age, data = training, method = "C5.0",
                   trControl=fitControl, tuneLength = 2)

C5.0feat2 # Same metrics with simpler model

# Training model 2 features Optimized tuneLength of 20
C5.0feat2_opt <- train(brand~ salary+age, data = training, method = "C5.0",
                       trControl = fitControl, tuneLength = 20)
C5.0feat2_opt # Better accuracy on training: 0.92 

# Test predictions
C5.0_predictions <- predict(C5.0feat2_opt,testing)
C5.0_predictions

# Metrics and errors
postResample(pred = C5.0_predictions,obs = testing$brand) # Same results also on testing
confusionMatrix(C5.0_predictions, testing$brand)
names(testing)
testing$prediction <- C5.0_predictions
testing$error <- testing$brand == testing$prediction
summary(testing$error)

# Plotiing errors
ggplot(testing, aes(x=testing$age, y=testing$salary)) +geom_point(aes(col=testing$error, shape=factor(error))) # More errors when age=60
ggplot(testing, aes(x=testing$age, y=testing$salary)) +geom_point(aes(col=testing$prediction))
ggplot(testing, aes(x=testing$age, y=testing$salary)) +geom_point(aes(col=testing$brand))

# Predictions for incomplete survey
survey_predictions <- predict(C5.0feat2_opt,SurveyIncomplete)
survey_predictions
summary(survey_predictions)
plot(survey_predictions)
ggplot(SurveyIncomplete, aes(x=age, 
                             y=salary)) +geom_point(aes(col=survey_predictions))
SurveyIncomplete$brandprediction <- survey_predictions

# Global customer survey
summary(SurveyIncomplete)
SurveyIncomplete$brand <- SurveyIncomplete$brandprediction
SurveyIncomplete$brandprediction <- NULL
SurveyIncomplete$X <- NULL
SurveyIncomplete$X.1 <- NULL
summary(CompleteResponses)
totalbrand <- rbind(CompleteResponses, SurveyIncomplete)
summary(totalbrand)


write.csv(totalbrand)
write.csv(totalbrand,"~/Desktop/Ubiqum/Módulo 2/Task2/Data/totalbrand.csv")
