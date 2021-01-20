#libraries
library(mice)
library(knitr)
library(VIM)
library(readr)
library(readr)
library(leaps)
library(MASS)
library(fastDummies)
#importing data and setting working direct.
setwd("~/Desktop/NW MSDS/MSDS411/Unit 1/BONUS/")
insurance=read.csv("insurance.csv",header=T)
str(insurance)
head(insurance)
summary(insurance)
names(insurance)
levels(KIDSDRIV)
levels(EDUCATION)
summary(insurance)
mean(insurance$HOMEKIDS)
sapply(insurance, class)
insurance2 <- dummy_cols(insurance, select_columns = c("PARENT1", "MSTATUS","SEX","EDUCATION","JOB","CAR_USE","CAR_TYPE","RED_CAR","REVOKED","URBANICITY"),
           remove_first_dummy = TRUE)
summary(insurance2)
head(insurance2)
insurance2 = subset(insurance2, select = -c(PARENT1, MSTATUS,SEX,EDUCATION,JOB,CAR_USE,CAR_TYPE,RED_CAR,REVOKED,URBANICITY))
insurance2$AGE[is.na(insurance2$AGE)] = mean(insurance2$AGE, na.rm = TRUE)
insurance2$YOJ[is.na(insurance2$YOJ)] = mean(insurance2$YOJ, na.rm = TRUE)
insurance2$INCOME[is.na(insurance2$INCOME)] = mean(insurance2$INCOME, na.rm = TRUE)
insurance2$HOME_VAL[is.na(insurance2$HOME_VAL)] = mean(insurance2$HOME_VAL, na.rm = TRUE)
insurance2$CAR_AGE[is.na(insurance2$CAR_AGE)] = mean(insurance2$CAR_AGE, na.rm = TRUE)
summary(insurance2)
write.csv(insurance2, file = "dummyfile.csv")
sapply(insurance2, class)
md.pattern(insurance2)
m <- md.pairs(insurance2);m
pbox(insurance2,pos=1,int=FALSE,cex=0.7)
dput(names(insurance2))
subsets <- regsubsets(TARGET ~ KIDSDRIV + AGE + HOMEKIDS + YOJ + INCOME + HOME_VAL + TRAVTIME + BLUEBOOK + EDUCATION_Masters + EDUCATION_PhD + JOB_Manager + JOB_Clerical + JOB_Lawyer + JOB_ + JOB_Professional + JOB_Student + JOB_Doctor + CAR_USE_Private + CAR_TYPE_Minivan + CAR_TYPE_z_SUV + CAR_TYPE_Pickup + CAR_TYPE_Van + RED_CAR_yes + REVOKED_Yes, 
                      data = insurance2, nvmax = 20, nbest = 1)
subsets
summary(subsets)
plot(subsets, scale="adjr2")

subsetmodel <- lm(TARGET ~ 
                    BLUEBOOK + AGE + INCOME +CAR_AGE, data = insurance2)
summary(subsetmodel)
AIC(subsetmodel)
mse <- function(sm) 
  mean(sm$residuals^2)
mse(subsetmodel)
insurance_test =read.csv("insurance_test.csv",header=T)

insurance_test <- dummy_cols(insurance_test, select_columns = c("PARENT1", "MSTATUS","SEX","EDUCATION","JOB","CAR_USE","CAR_TYPE","RED_CAR","REVOKED","URBANICITY"),
                         remove_first_dummy = TRUE)
summary(insurance_test)
head(insurance_test)
insurance_test = subset(insurance_test, select = -c(PARENT1, MSTATUS,SEX,EDUCATION,JOB,CAR_USE,CAR_TYPE,RED_CAR,REVOKED,URBANICITY))
insurance_test$AGE[is.na(insurance_test$AGE)] = mean(insurance_test$AGE, na.rm = TRUE)
insurance_test$YOJ[is.na(insurance_test$YOJ)] = mean(insurance_test$YOJ, na.rm = TRUE)
insurance_test$INCOME[is.na(insurance_test$INCOME)] = mean(insurance_test$INCOME, na.rm = TRUE)
insurance_test$HOME_VAL[is.na(insurance_test$HOME_VAL)] = mean(insurance_test$HOME_VAL, na.rm = TRUE)
insurance_test$CAR_AGE[is.na(insurance_test$CAR_AGE)] = mean(insurance_test$CAR_AGE, na.rm = TRUE)
summary(insurance_test)
summary(subsetmodel)

insurance_test$TARGET <- 
  14.32436209  
+  2.1042778481 * insurance_test$BLUEBOOK 
+  19.3650397 * insurance_test$AGE 
+  9.1969305 * insurance_test$INCOME
-  20.2732735895 * insurance_test$CAR_AGE
head(insurance_test)
prediction <- insurance_test[c("INDEX","TARGET")]
## writen csv for submission
write.csv(insurance_test, file = "logan_strouse_predictions.csv")

