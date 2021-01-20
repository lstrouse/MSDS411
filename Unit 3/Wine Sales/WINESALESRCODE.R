#import and setting working directory
setwd("~/Desktop/NW MSDS/MSDS411/Unit 3/Wine Sales/")
train <- read.csv("Wine_Training.csv")
test <- read.csv("Wine_Test.csv")
#importing the libraries that will be used
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
library(car)
library(leaps)
library(glm2)
library(aod)
library(mice)
library(knitr)
library(rpart)
library(missForest)
#checking structure
str(train)
summary(train)
head(train)
describe(train)
sapply(train, function(x) sum(is.na(x)))
##distribution
ggplot(data=train, aes(train$TARGET)) + 
  geom_histogram(binwidth =1, 
                 col="Black", 
                 aes(fill=..density..,)) + ggtitle("Distribution of Wine Cases Purchased") + xlab("Wine Cases Purchased") + ylab("Total Instances") + labs("Density")+
scale_fill_gradientn("Density",colours = topo.colors(2))
##Cleaning up missing variables
train.imp <- missForest(train[,6:16])
train_clean <- train.imp$ximp
train_clean$INDEX <- train$INDEX
train_clean$TARGET <- train$TARGET
train_clean$FixedAcidity <- train$FixedAcidity
train_clean$VolatileAcidity <- train$VolatileAcidity
train_clean$CitricAcid <- train$CitricAcid
train_clean <- train_clean[,c(12,13,14,15,16,1,2,3,4,5,6,7,8,9,10,11)]
##checking to see if variables are clean
sapply(train_clean, function(x) sum(is.na(x)))
#working on test data set
sapply(test, function(x) sum(is.na(x)))
#cleaning up test data set
test.imp <- missForest(test[,6:16])
test_clean <- test.imp$ximp
test_clean$INDEX <- test$INDEX
test_clean$TARGET <- test$TARGET
test_clean$FixedAcidity <- test$FixedAcidity
test_clean$VolatileAcidity <- test$VolatileAcidity
test_clean$CitricAcid <- test$CitricAcid
test_clean <- test_clean[,c(12,13,14,15,16,1,2,3,4,5,6,7,8,9,10,11)]
#seeing if anything missing outside target
sapply(test_clean, function(x) sum(is.na(x)))
# checking star rating distribution
ggplot(data=train_clean, aes(train_clean$STARS)) + 
  geom_histogram(binwidth =1, 
                 col="Black", 
                 aes(fill=..density..,)) + ggtitle("Distribution Ratings") + xlab("Rating") + ylab("Counts") + labs("Density")+
  scale_fill_gradientn("Density",colours = heat.colors(2))
#binning
train_clean$STARS_bin[train_clean$STARS <= 2] <- "Lower Rating"
train_clean$STARS_bin[train_clean$STARS > 2] <- "Higher Rating"
train_clean$STARS_bin <- factor(train_clean$STARS_bin)
train_clean$STARS_bin <- factor(train_clean$STARS_bin, levels=c("Lower Rating","Higher Rating"))
#checking sulphates
ggplot(data=train_clean, aes(train_clean$Sulphates)) + 
  geom_histogram(binwidth =1, 
                 col="Black", 
                 aes(fill=..density..,)) + ggtitle("Distribution Sulphates") + xlab("Acidity") + ylab("Counts") + labs("Density")+
  scale_fill_gradientn("Density",colours = heat.colors(2))
summary(train_clean$Sulphates)
#binning sulphates on Quartiles
train_clean$Sulphates_bin[train_clean$Sulphates >= -3.130] <- "low"
train_clean$Sulphates_bin[train_clean$Sulphates >= .320 & train_clean$Sulphates <.500 ] <- "medium"
train_clean$Sulphates_bin[train_clean$Sulphates >= .500 & train_clean$Sulphates <.780 ] <- "high"
train_clean$Sulphates_bin[train_clean$Sulphates >= .780] <- "highest"
train_clean$Sulphates_bin <- factor(train_clean$Sulphates_bin)
train_clean$Sulphates_bin <- factor(train_clean$Sulphates_bin, levels=c("low","medium","high","highest"))
plot(train_clean$Sulphates_bin)
#fixing test clean data
test_clean$STARS_bin[test_clean$STARS <= 2] <- "Lower Rating"
test_clean$STARS_bin[test_clean$STARS > 2] <- "Higher Rating"
test_clean$STARS_bin <- factor(test_clean$STARS_bin)
test_clean$STARS_bin <- factor(test_clean$STARS_bin, levels=c("Lower Rating","Higher Rating"))
test_clean$Sulphates_bin[test_clean$Sulphates >= -3.130] <- "low"
test_clean$Sulphates_bin[test_clean$Sulphates >= .320 & test_clean$Sulphates <.500 ] <- "medium"
test_clean$Sulphates_bin[test_clean$Sulphates >= .500 & test_clean$Sulphates <.780 ] <- "high"
test_clean$Sulphates_bin[test_clean$Sulphates >= .780] <- "highest"
test_clean$Sulphates_bin <- factor(test_clean$Sulphates_bin)
test_clean$Sulphates_bin <- factor(test_clean$Sulphates_bin, levels=c("low","medium","high","highest"))
#tests
mean(train_clean$TARGET)
var(train_clean$TARGET)
# Standard Model

first_Linear_model <- lm(TARGET~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + pH + Sulphates_bin + Alcohol + LabelAppeal + AcidIndex + STARS, data = train_clean)
summary(first_Linear_model)
coefficients(first_Linear_model)
train_clean$first_Linear_model <-fitted(first_Linear_model)
AIC(first_Linear_model)

# Second Model using Stepwise AIC Regression

stepwise_lm <- stepAIC(first_Linear_model, direction="both")
stepwise_lm$anova
train_clean$stepwise_lm <-fitted(stepwise_lm)
AIC(stepwise_lm)

# Third Model Poisson

poisson_model <- glm(TARGET~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + pH + Sulphates_bin + Alcohol + LabelAppeal + AcidIndex + STARS, family="poisson"(link="log"), data=train_clean)

summary(poisson_model)
coef(poisson_model)

train_clean$poisson <- predict(poisson_model, newdata = train_clean, type = "response")
AIC(poisson_model)
# Fourth Model Negative Binomial

NBR_Model<-glm.nb(TARGET~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + pH + Sulphates_bin + Alcohol + LabelAppeal + AcidIndex + STARS, data=train_clean)
summary(NBR_Model)
train_clean$NBR <- predict(NBR_Model, newdata = train_clean, type = "response")
AIC(NBR_Model)

#fifth model ZIP

ZIP_Model<-zeroinfl(TARGET~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + pH + Sulphates_bin + Alcohol + LabelAppeal + AcidIndex + STARS, data=train_clean)
summary(ZIP_Model)
train_clean$ZIP <- predict(ZIP_Model, newdata = train_clean, type = "response")
AIC(ZIP_Model)

#6th Model ZINB

ZINB_Model<-zeroinfl(TARGET~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + pH + Sulphates_bin + Alcohol + LabelAppeal + AcidIndex + STARS, data=train_clean, dist = "negbin", EM=TRUE)

summary(ZINB_Model)

train_clean$ZINB <- predict(ZINB_Model, newdata = train_clean, type = "response")
AIC(ZINB_Model)

#7th Model Hurdle

modelHurdle <- hurdle(TARGET~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + pH + Sulphates_bin + Alcohol + LabelAppeal + AcidIndex + STARS,
                      dist    = "negbin",
                      data    = train_clean)
summary(modelHurdle)
AIC (modelHurdle)
train_clean$modelHurdle <- fitted(modelHurdle)

#champion model is the Hurdle Model

test_clean$TARGET <- predict(modelHurdle, newdata = test_clean, type = "response")
summary(test_clean)

#Score File 1
scores <- test_clean[c("INDEX","TARGET")]
write.csv(scores, file = "Logan_Strouse_Scored_File.csv", row.names = FALSE)

#backup model is ZIP model

test_clean$TARGET2 <- predict(ZIP_Model, newdata = test_clean, type = "response")
summary(test_clean)

#Score File 2
scores <- test_clean[c("INDEX","TARGET2")]
write.csv(scores, file = "Logan_Strouse_Scored_File_2.csv", row.names = FALSE)



