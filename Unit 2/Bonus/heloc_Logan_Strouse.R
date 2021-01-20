# Loading libraries that will be used
library(readr)
library(dplyr)
library(plyr)
library(zoo)
library(psych)
library(ROCR)
library(corrplot)
library(car)
library(InformationValue)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(corrplot)
library(glm2)
library(aod)
library(mice)
library(VIM)
library(MASS)
library(leaps)
library(rpart)
library(PRROC)
library(randomForest)


#import and setting working directory
setwd("~/Desktop/NW MSDS/MSDS411/Unit 2/Bonus/")
train <- read.csv("heloc.csv")
test <- read.csv("heloc_test.csv")
#Explore and find N/A's
summary(train)
#mice
train <- complete(mice(train))
#check for N/A's
sapply(train, function(x) sum(is.na(x)))
#Clean and fix test data
summary(test)
test <- complete(mice(test))
sapply(test, function(x) sum(is.na(x)))
summary(test)
#decision tree
fit <- rpart(TARGET_FLAG ~ LOAN + MORTDUE + VALUE + REASON + JOB + YOJ + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC, data= train, method= "class") 
printcp(fit)
summary(fit)
#applying formula to test
test$P_TARGET_FLAG <- predict(fit, newdata = test, type = "class")
#random forrest
fit_2 <- randomForest(TARGET_FLAG ~ LOAN + MORTDUE + VALUE + REASON + JOB + YOJ + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,   data=train)
print(fit_2) 
importance(fit_2) 
test$P_TARGET_FLAG_2 <- predict(fit_2, newdata = test, type = "class")
#Scored Files
scores_1 <- test[c("INDEX","P_TARGET_FLAG")]
write.csv(scores_1, file = "LOGAN_STROUSE_Scored_HELOC_1.csv")
scores_2 <- test[c("INDEX","P_TARGET_FLAG_2")]
write.csv(scores_2, file = "LOGAN_STROUSE_Scored_HELOC_2.csv")

