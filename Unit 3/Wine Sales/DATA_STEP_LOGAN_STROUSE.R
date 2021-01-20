#Below is my standalone scoring program
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
##Cleaning up missing variables
train.imp <- missForest(train[,6:16])
train_clean <- train.imp$ximp
train_clean$INDEX <- train$INDEX
train_clean$TARGET <- train$TARGET
train_clean$FixedAcidity <- train$FixedAcidity
train_clean$VolatileAcidity <- train$VolatileAcidity
train_clean$CitricAcid <- train$CitricAcid
train_clean <- train_clean[,c(12,13,14,15,16,1,2,3,4,5,6,7,8,9,10,11)]
#cleaning up test data set
test.imp <- missForest(test[,6:16])
test_clean <- test.imp$ximp
test_clean$INDEX <- test$INDEX
test_clean$TARGET <- test$TARGET
test_clean$FixedAcidity <- test$FixedAcidity
test_clean$VolatileAcidity <- test$VolatileAcidity
test_clean$CitricAcid <- test$CitricAcid
test_clean <- test_clean[,c(12,13,14,15,16,1,2,3,4,5,6,7,8,9,10,11)]
#binning
train_clean$STARS_bin[train_clean$STARS <= 2] <- "Lower Rating"
train_clean$STARS_bin[train_clean$STARS > 2] <- "Higher Rating"
train_clean$STARS_bin <- factor(train_clean$STARS_bin)
train_clean$STARS_bin <- factor(train_clean$STARS_bin, levels=c("Lower Rating","Higher Rating"))
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
#Model Hurdle
modelHurdle <- hurdle(TARGET~ FixedAcidity + VolatileAcidity + CitricAcid + ResidualSugar + Chlorides + FreeSulfurDioxide + TotalSulfurDioxide + Density + pH + Sulphates_bin + Alcohol + LabelAppeal + AcidIndex + STARS,
                      dist    = "negbin",
                      data    = train_clean)
test_clean$TARGET <- predict(modelHurdle, newdata = test_clean, type = "response")
summary(test_clean)
#Score File 1
scores <- test_clean[c("INDEX","TARGET")]
write.csv(scores, file = "scoredfile.csv", row.names = FALSE)
