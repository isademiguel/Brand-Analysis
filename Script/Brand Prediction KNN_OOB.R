#KNN 10-fold cross validation

#dataframe = CompleteResponses
#Y Value = brand

#install.packages("kknn")
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
anova(CompleteResponses$salary, CompleteResponses$brand) # This is not working 
summary(aov(salary ~ brand, data = CompleteResponses)) # This is working
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
# When you yse system.time, you can add in comment how long time it took you to run the line
# So the person running it will have an idea before having to launch it. But well done !
system.time(KNNFit <- train(brand~., data = training, method = "kknn", trControl=fitControl))
KNNFit

# Don't test it ? : )