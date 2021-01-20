## SET WORKING DIRECTORY
setwd("~/Desktop/NW MSDS/MSDS411/Unit 3/Bonus/")
train <- read.csv("zip_abalone-1.csv")
test <- read.csv("zip_abalone_test-1.csv")
#Importing Libraries
library(ggplot2)
library(MASS) 
library(pscl) 
library(dplyr) 
library(readr)
library(corrplot)
library(zoo)
library(psych)
library(ROCR)
library(car)
library(InformationValue)
library(pbkrtest)
library(leaps)
library(glm2)
library(aod)
#Inspecting Train Data
summary(train)
describe(train)
str(train)
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))
#there doesn't appear to be any NA Values
#looking at distribution of the Target Variable
ggplot(data=train, aes(train$TARGET_RINGS)) + 
  geom_histogram(binwidth =2, 
                 col="BLUE", 
                 aes(fill=..density..))+
  scale_fill_gradient("Count", low = "blue", high = "red")
#appears to be zero inflated
plot(x=train$Sex,y=train$TARGET_RINGS)
plot(x=train$Sex,y=train$ShuckedWeight)
#infants appear to be the ones with zero rings

#regular Linear Regression
lm_fit <- lm(TARGET_RINGS~ Sex + Length + Diameter + Height + WholeWeight + ShuckedWeight + VisceraWeight + ShellWeight, data = train)

summary(lm_fit)
coefficients(lm_fit)
train$fittedLM <-fitted(lm_fit)
AIC(lm_fit)
#logistic model
logistic_regression <- glm(TARGET_RINGS~ Sex + Length + Diameter + Height + WholeWeight + ShuckedWeight + VisceraWeight + ShellWeight, 
                      data = train, family="poisson"(link="log"))
summary(logistic_regression)
train$logistic_regression <- predict(logistic_regression, type = "response")
AIC(logistic_regression)
#AIC was worse
#negative binomial
NBR_Model <- glm.nb(TARGET_RINGS~ Sex + Length + Diameter + Height + WholeWeight + ShuckedWeight + VisceraWeight + ShellWeight, 
                           data = train)
summary(NBR_Model)
AIC(NBR_Model)
train$NBR <- predict(NBR_Model, newdata = train, type = "response")
#poor AIC with NBR
#fixing notation
options("scipen"=100, "digits"=4)
## Hurdle Model that was used in regular assignemnt
modelHurdle <- hurdle(TARGET_RINGS~ Sex + Length + Diameter + Height + WholeWeight + ShuckedWeight + VisceraWeight + ShellWeight,
                      dist    = "negbin",
                      data    = train)
summary(modelHurdle)
AIC (modelHurdle)
train$modelHurdle <-  predict(modelHurdle, type = "response")
#hurdle summary stats very close to that of actual target rings, will choose that for predictions.
#below is proof
summary(train$TARGET_RINGS)
summary(train$modelHurdle)
#scoring
test$TARGET_RINGS <- round(predict(modelHurdle, newdata = test, type = "response"),0)
summary(test)
scores <- test[c("INDEX","TARGET_RINGS")]
write.csv(scores, file = "Logan_Strouse_abalone_bonus.csv", row.names = FALSE)

