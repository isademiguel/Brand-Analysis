#SVM 10-fold cross validation

#dataframe = CompleteResponses
#Y Value = brand

library(readr)
library(rstudioapi)
library(caret)

#Uploading data set
setwd(dirname(getActiveDocumentContext()$path))
CompleteResponses <- read.csv("../Data/CompleteResponses.csv")

#Exploring data
summary(CompleteResponses)
str(CompleteResponses)
cor(CompleteResponses$salary, CompleteResponses$age)
chisq.test(CompleteResponses$elevel, CompleteResponses$brand)
anova(CompleteResponses$salary, CompleteResponses$brand)
sum(is.na(CompleteResponses))

#Preprocessing
CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
CompleteResponses$brand<-as.factor(CompleteResponses$brand)

#Plots
plot(CompleteResponses$salary, CompleteResponses$brand)
scatter.smooth(x=CompleteResponses$age, y=CompleteResponses$brand, main="Resp ~ Predictor") 
boxplot(CompleteResponses$salary)
qqnorm(CompleteResponses$age)

#Training and test sets
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]
nrow(training)
nrow(testing)

#Setseed
set.seed(123)

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#Training model
system.time(SVMFit <- train(brand~., data = training, method = "svmPoly", trControl=fitControl))
SVMFit
