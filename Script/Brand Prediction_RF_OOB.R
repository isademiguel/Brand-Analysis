#Random Forest  10-fold cross validation and manually tune 5 different mtry values
#dataframe = CompleteResponses
#Y Value = brand

library(readr)
library(rstudioapi)
library(caret)

#Uploading data set
setwd(dirname(getActiveDocumentContext()$path))
CompleteResponses <- read.csv("../Data/CompleteResponses.csv")

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

#Plots
plot(CompleteResponses$elevel, CompleteResponses$brand)
boxplot(CompleteResponses$salary)
boxplot(CompleteResponses$age)
boxplot(CompleteResponses$credit)
# The three next line dies not work. You can not make a boxplot with categorical features
boxplot(CompleteResponses$elevel)
boxplot(CompleteResponses$zipcode)
boxplot(CompleteResponses$car)
qqnorm(CompleteResponses$credit)

#Analysis of variance for feature selection
summary(aov(CompleteResponses$salary ~ CompleteResponses$brand, data = CompleteResponses))
summary(aov(CompleteResponses$age ~ CompleteResponses$brand, data = CompleteResponses))
summary(aov(CompleteResponses$credit ~ CompleteResponses$brand, data = CompleteResponses))
chisq.test(CompleteResponses$elevel, CompleteResponses$brand)
chisq.test(CompleteResponses$car, CompleteResponses$brand)
chisq.test(CompleteResponses$zipcode, CompleteResponses$brand)
cor(CompleteResponses$salary, CompleteResponses$age)
cor(CompleteResponses$salary, CompleteResponses$credit)
cor(CompleteResponses$age, CompleteResponses$credit)
ggplot(data = CompleteResponses, 
       aes(x=CompleteResponses$salary, y=CompleteResponses$age))  + geom_point()

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

#Dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

#Training model all features OOB
system.time(rffeat <- train(brand~., data = training,
                            method = "rf", trControl=fitControl, tuneGrid=rfGrid))
rffeat
#Feat selection
varImp(rffeat)

#Training model 1 feature OOB
system.time(rffeat2 <- train(brand~salary, data = training,
                             method = "rf", trControl=fitControl, tuneGrid=rfGrid))
rffeat2

#Training model 2 features OOB
system.time(rffeat2_OOB <- train(brand~salary+age, data = training,
                             method = "rf", trControl=fitControl, tuneGrid=rfGrid))
rffeat2_OOB

#Training model 2 features optimized
system.time(rffeat2opt <- train(brand~salary+age, data = training,
                                method = "rf", trControl=fitControl, tuneGrid=rfGrid,
                                ntree= 300))
rffeat2opt

#Training model 2 features optimized
system.time(rffeat2opt2 <- train(brand~salary+age, data = training,
                                method = "rf", trControl=fitControl, tuneGrid=rfGrid,
                                ntree= 700))
rffeat2opt2

