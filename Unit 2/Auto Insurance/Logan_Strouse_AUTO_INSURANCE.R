#R Code for Auto Insurance project Logan Strouse

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

#import and setting working directory
setwd("~/Desktop/NW MSDS/MSDS411/Unit 2/Auto Insurance/")
train <- read.csv("logit_insurance.csv")
test <- read.csv("logit_insurance_test.csv")

#Assigning Factor Levels, Numerical & Categorical Checks

#Train Data Adjustments
train$INDEX <- as.factor(train$INDEX)
train$TARGET_FLAG <- as.factor(train$TARGET_FLAG)
train$SEX <- as.factor(train$SEX)
train$EDUCATION <- as.factor(train$EDUCATION)
train$PARENT1 <- as.factor(train$PARENT1)
train$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", train$INCOME)))
train$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", train$HOME_VAL)))
train$MSTATUS <- as.factor(train$MSTATUS)
train$REVOKED <- as.factor(train$REVOKED)
train$RED_CAR <- as.factor(ifelse(train$RED_CAR=="yes", 1, 0))
train$URBANICITY <- ifelse(train$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
train$URBANICITY <- as.factor(train$URBANICITY)
train$JOB <- as.factor(train$JOB)
train$CAR_USE <- as.factor(train$CAR_USE)
train$CAR_TYPE <- as.factor(train$CAR_TYPE)
train$DO_KIDS_DRIVE <- as.factor(ifelse(train$KIDSDRIV > 0, 1, 0 ))
train$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", train$HOME_VAL)))
train$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", train$BLUEBOOK)))
summary(train)
#write.csv(train_predicts, file = "trainref.csv")

#Test Data Adjustments 
test$INDEX <- as.factor(test$INDEX)
test$TARGET_FLAG <- as.factor(test$TARGET_FLAG)
test$SEX <- as.factor(test$SEX)
test$EDUCATION <- as.factor(test$EDUCATION)
test$PARENT1 <- as.factor(test$PARENT1)
test$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$INCOME)))
test$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$MSTATUS <- as.factor(test$MSTATUS)
test$REVOKED <- as.factor(test$REVOKED)
test$RED_CAR <- as.factor(ifelse(test$RED_CAR=="yes", 1, 0))
test$URBANICITY <- ifelse(test$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
test$URBANICITY <- as.factor(test$URBANICITY)
test$JOB <- as.factor(test$JOB)
test$CAR_USE <- as.factor(test$CAR_USE)
test$CAR_TYPE <- as.factor(test$CAR_TYPE)
test$DO_KIDS_DRIVE <- as.factor(ifelse(test$KIDSDRIV > 0, 1, 0 ))
test$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$BLUEBOOK)))
summary(test)

#EDA
#cor.ci(train, method ="spearman" )
#aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#aggr_plot
#MICE Analysis
#md.pairs(train)
#pbox(train,pos=1,int=TRUE,cex=0.7)
#imp <- mice(train)
#cleaning and identifying missing values train (N/A)
#attaching and reattaching index due to issues with index
sapply(train, function(x) sum(is.na(x)))
INDEX_TRAIN <- train[["INDEX"]]
train <- complete(mice(train[,2:27]))
train$INDEX <- INDEX_TRAIN
train <- train[,c(27,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]
sapply(train, function(x) sum(is.na(x)))
#need to fix oldclaim
train$OLDCLAIM <- ifelse(train$CAR_AGE < 5 & !is.na(train$CAR_AGE),0,train$OLDCLAIM)
train$OLDCLAIM <- na.aggregate(train$OLDCLAIM, train$CAR_AGE, mean, na.rm = TRUE )
sapply(train, function(x) sum(is.na(x)))


#cleaning and identifying missing values test (N/A)
#attaching and reattaching index due to issues with index
sapply(test, function(x) sum(is.na(x)))
INDEX_TEST <- test[["INDEX"]]
test <- complete(mice(test[,2:27]))
test$INDEX <- INDEX_TEST
test <- test[,c(27,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]
sapply(test, function(x) sum(is.na(x)))
#need to fix oldclaim
test$OLDCLAIM <- ifelse(test$CAR_AGE < 5 & !is.na(test$CAR_AGE),0,test$OLDCLAIM)
test$OLDCLAIM <- na.aggregate(test$OLDCLAIM, test$CAR_AGE, mean, na.rm = TRUE )
sapply(test, function(x) sum(is.na(x)))

#NEW/CAR OLD CAR COMPS

x <- train$CAR_AGE 
h<-hist(x, breaks=12, col="red", xlab="Car Age", 
        main="Normal Curve and Histogram of Car Age",labels = TRUE) 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
count(train, 'CAR_AGE')

#car type
count(train, 'CAR_TYPE')

#exploring income
hist(train$INCOME,col= 'red', breaks= 10)
boxplot(train$INCOME)

#binned income based on IQR ranges Train
train$INCOME_bin[is.na(train$INCOME)] <- "NA"
train$INCOME_bin[train$INCOME >= 0] <- "Zero"
train$INCOME_bin[train$INCOME >= 1 & train$INCOME < 27907] <- "Low"
train$INCOME_bin[train$INCOME >= 27907 & train$INCOME < 53841] <- "Medium"
train$INCOME_bin[train$INCOME >= 53841 & train$INCOME < 85731] <- "High"
train$INCOME_bin[train$INCOME >= 85731] <- "Affluent"
train$INCOME_bin <- factor(train$INCOME_bin)
train$INCOME_bin <- factor(train$INCOME_bin, levels=c("NA","Zero","Low","Medium","High","Affluent"))

#binned income based on IQR ranges Test
test$INCOME_bin[is.na(test$INCOME)] <- "NA"
test$INCOME_bin[test$INCOME >= 0] <- "Zero"
test$INCOME_bin[test$INCOME >= 1 & test$INCOME < 27907] <- "Low"
test$INCOME_bin[test$INCOME >= 27907 & test$INCOME < 53841] <- "Medium"
test$INCOME_bin[test$INCOME >= 53841 & test$INCOME < 85731] <- "High"
test$INCOME_bin[test$INCOME >= 85731] <- "Affluent"
test$INCOME_bin <- factor(test$INCOME_bin)
test$INCOME_bin <- factor(test$INCOME_bin, levels=c("NA","Zero","Low","Medium","High","Affluent"))


#Adding additional variables to the dataset to data frames to use
train$HOME_OWNER <- ifelse(train$HOME_VAL == 0, 0, 1)
test$HOME_OWNER <- ifelse(test$HOME_VAL == 0, 0, 1)
train$NEW_USED_CAR <- ifelse(train$CAR_AGE <= 2, 0, 1)
test$NEW_USED_CAR <- ifelse(test$CAR_AGE <= 2, 0, 1)

#correlations of numeric data
numeric_data <- subset(train, select = c(TARGET_AMT, AGE, HOMEKIDS,KIDSDRIV,OLDCLAIM, YOJ, INCOME, HOME_VAL,HOME_OWNER,NEW_USED_CAR, TRAVTIME, BLUEBOOK, TIF,
                                   CLM_FREQ, MVR_PTS, CAR_AGE), na.rm = TRUE)
corrplot(cor(numeric_data), method = "square")

train_predicts<-data.frame(train)


#model development
complete_model <- glm(TARGET_FLAG ~  AGE + BLUEBOOK + TRAVTIME + KIDSDRIV + SEX + URBANICITY + HOMEKIDS + INCOME + OLDCLAIM + DO_KIDS_DRIVE + HOME_OWNER + NEW_USED_CAR +
                        CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + TIF + EDUCATION + MSTATUS + PARENT1 + RED_CAR + 
                        CAR_USE + CAR_TYPE + YOJ + JOB + INCOME_bin + HOME_VAL, 
              data = train, family = binomial())
summary(complete_model)
train_predicts$Model1Prediction <- predict(complete_model, type = "response")
vif(complete_model)

Model2 <- glm(TARGET_FLAG ~  AGE + BLUEBOOK + TRAVTIME + KIDSDRIV + SEX + URBANICITY + HOMEKIDS + INCOME + OLDCLAIM + DO_KIDS_DRIVE + HOME_OWNER + NEW_USED_CAR +
                CLM_FREQ + REVOKED + MVR_PTS + TIF + EDUCATION + MSTATUS + PARENT1 + RED_CAR + 
                CAR_USE + CAR_TYPE + YOJ + JOB + INCOME_bin,
              data = train, family = binomial())
summary(Model2)
train_predicts$Model2Prediction <- predict(Model2, type = "response")

#backwards/step selection
backwards_model = step(complete_model)
formula(backwards_model)
summary(backwards_model)
train_predicts$Model3Prediction <- predict(backwards_model, type = "response")


#regsubsets
regfit.full = regsubsets(TARGET_FLAG ~ AGE + BLUEBOOK + TRAVTIME + KIDSDRIV + SEX + URBANICITY + HOMEKIDS + INCOME + OLDCLAIM + DO_KIDS_DRIVE + HOME_OWNER + NEW_USED_CAR +
                           CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + TIF + EDUCATION + MSTATUS + PARENT1 + RED_CAR + 
                           CAR_USE + CAR_TYPE + YOJ + JOB + INCOME_bin + HOME_VAL,nvmax = 10, nbest = 1,  data = train)
summary(regfit.full)
Model4 <- glm(TARGET_FLAG ~  URBANICITY + INCOME + JOB + MSTATUS + TIF + DO_KIDS_DRIVE + TRAVTIME + BLUEBOOK + REVOKED + MVR_PTS + CAR_USE,
              data = train, family = binomial())
summary(Model4)
train_predicts$Model4Prediction <- predict(Model4, type = "response")

#rfit model
rfit = rpart(TARGET_FLAG ~ AGE + BLUEBOOK + TRAVTIME + KIDSDRIV + SEX + URBANICITY + HOMEKIDS + INCOME + OLDCLAIM + DO_KIDS_DRIVE + HOME_OWNER + NEW_USED_CAR +
               CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + TIF + EDUCATION + MSTATUS + PARENT1 + RED_CAR + 
               CAR_USE + CAR_TYPE + YOJ + JOB + INCOME_bin + HOME_VAL, data = train , method = "class", cp = 0.0001)
summary(rfit)
train_predicts$Model5Prediction <- predict(rfit)

#mse
mse <- function(sm) 
  mean(sm$residuals^2)

AIC(complete_model)
AIC(Model2)
AIC(backwards_model)
AIC(Model4)
#AIC(rfit)
BIC(complete_model)
BIC(Model2)
BIC(backwards_model)
BIC(Model4)
#BIC(rfit)
print(-2*logLik(complete_model, REML = TRUE))
print(-2*logLik(Model2, REML = TRUE))
print(-2*logLik(backwards_model, REML = TRUE))
print(-2*logLik(Model4, REML = TRUE))
#print(-2*logLik(rfit, REML = TRUE))
ks_stat(actuals=train$TARGET_FLAG, predictedScores=train_predicts$Model1Prediction)
ks_stat(actuals=train$TARGET_FLAG, predictedScores=train_predicts$Model2Prediction)
ks_stat(actuals=train$TARGET_FLAG, predictedScores=train_predicts$Model3Prediction)
ks_stat(actuals=train$TARGET_FLAG, predictedScores=train_predicts$Model4Prediction)
#ks_stat(actuals=train$TARGET_FLAG, predictedScores=train_predicts$Model5Prediction)
mse(complete_model)
mse(Model2)
mse(backwards_model)
mse(Model4)

#ROCR

pred <- prediction(train_predicts$Model1Prediction, train$TARGET_FLAG)
perf <- performance(pred,"tpr","fpr")
performance(pred,'auc')
model1roc <- plot(perf,colorize=TRUE)

pred2 <- prediction(train_predicts$Model2Prediction, train$TARGET_FLAG)
perf <- performance(pred2,"tpr","fpr")
performance(pred2,'auc')
model2roc <- plot(perf,colorize=TRUE)

pred3 <- prediction(train_predicts$Model3Prediction, train$TARGET_FLAG)
perf <- performance(pred3,"tpr","fpr")
performance(pred3,'auc')
model3roc <- plot(perf,colorize=TRUE)

pred4 <- prediction(train_predicts$Model4Prediction, train$TARGET_FLAG)
perf <- performance(pred4,"tpr","fpr")
performance(pred4,'auc')
model4roc <- plot(perf,colorize=TRUE)

#pred5 <- prediction(train_predicts$Model5Prediction, train$TARGET_FLAG)
#perf <- performance(pred5,"tpr","fpr")
#performance(pred5,'auc')
#model5roc <- plot(perf,colorize=TRUE)

#choosing backwards model
coef(backwards_model)
summary(backwards_model)

#Target_AMT Model
subsets_AMT <- regsubsets(TARGET_AMT ~ AGE + BLUEBOOK + TRAVTIME + KIDSDRIV + SEX + URBANICITY + HOMEKIDS + INCOME + OLDCLAIM + DO_KIDS_DRIVE + HOME_OWNER + NEW_USED_CAR +
                            CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + TIF + EDUCATION + MSTATUS + PARENT1 + RED_CAR + 
                            CAR_USE + CAR_TYPE + YOJ + JOB + INCOME_bin + HOME_VAL,nvmax = 10, nbest = 1,  data = train)
subsets_AMT
summary(subsets_AMT)
plot(subsets_AMT, scale="adjr2")
subset_amt_model <- lm(TARGET_AMT ~ TRAVTIME + URBANICITY + INCOME + DO_KIDS_DRIVE + MVR_PTS + CAR_AGE + TIF + MSTATUS + PARENT1 + CAR_USE + JOB, data = train)
summary(subset_amt_model)
train_predicts$subsets_amt <- predict(subset_amt_model)


#Predicting
test$P_TARGET_FLAG <- predict(backwards_model, newdata = test, type = "response")
test$P_TARGET_AMT<- predict(subset_amt_model, newdata = test, type = "response")
test$P_TARGET_AMT[(test$P_TARGET_AMT <= 0)] = 0

#Scored File
scores <- test[c("INDEX","P_TARGET_FLAG", "P_TARGET_AMT")]
write.csv(scores, file = "LOGAN_STROUSE_Scored_INSURANCE.csv")
