

library(rJava)
library(readr)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(rJava)
library(xlsxjars)
library(xlsx)
#install.packages("VIM")
library("VIM")
#install.packages('corrplot',dependencies=TRUE)
library(corrplot)

#####
#Designated proper working environment on my computer. You will want to make sure it is in proper place for your computer.
#####

setwd("C:/Users/Downloads/Documents/NW/411/411/Unit01 HW Moneyball/") #setwd("D:/RFile/")
moneyball=read.csv("moneyball.csv",header=T)

############## Part 1: Data Exploration ##########################################################################
str(moneyball)
custom.summary<-function(dt,var){
  for(i in 2:length(var)){ # for(i in 2:length(vars)){
    print(names(dt)[i])
    print(c("Mean:;",mean(dt[[var[i]]],na.rm=TRUE)))
    print(c("Median:;",median(dt[[var[i]]],na.rm=TRUE)))
    print(c("Std Deviation:;",sd(dt[[var[i]]],na.rm=TRUE)))
    print(c("Min:;",min(dt[[var[i]]],na.rm=TRUE)))
    print(c("Max:;",max(dt[[var[i]]],na.rm=TRUE)))
    print(c("NAs:;",sum(is.na(dt[[var[i]]]))))
    print(c("1st quartile:;",quantile(dt[[var[i]]],.25,na.rm = TRUE)))
    print(c("3rd quartile:;",quantile(dt[[var[i]]],.75,na.rm = TRUE)))
    print(c("# of Outliers:;",length(dt[which(dt[[var[i]]]<=sd(dt[[var[i]]])*-3+mean(dt[[var[i]]])),])+length(dt[which(dt[[var[i]]]>=sd(dt[[var[i]]])*3+mean(dt[[var[i]]])),])))
    
  }
}
custom.summary(moneyball,names(moneyball))#summary(moneyball)

histograms<-function(nm)
  qplot(nm,geom="histogram")#ggplot(moneyball,aes(y=nm,x=1))+geom_boxplot()
sapply(moneyball,histograms)


# Wins - Use lower bound for lower outliers, upper bound for higher outliers.
par(mfrow=c(1,2))
hist(moneyball$TARGET_WINS, col = "#A71930", xlab = "TARGET_WINS", main = "Histogram of Wins")
boxplot(moneyball$TARGET_WINS, col = "#A71930", main = "Boxplot of Wins")
par(mfrow = c(1,1))

################# Batting ####################
# Hits and Doubles
par(mfrow=c(2,2))
hist(moneyball$TEAM_BATTING_H, col = "#A71930", xlab = "Team_Batting_H", main = "Histogram of Hits")
hist(moneyball$TEAM_BATTING_2B, col = "#09ADAD", xlab = "Doubles", main = "Histogram of Doubles")
boxplot(moneyball$TEAM_BATTING_H, col = "#A71930", main = "Boxplot of Hits")
boxplot(moneyball$TEAM_BATTING_2B, col = "#09ADAD", main = "Boxplot of Doubles")
par(mfrow=c(1,1))

# Triples and Home Runs
par(mfrow=c(2,2))
hist(moneyball$TEAM_BATTING_3B, col = "#A71930", xlab = "Triples", main = "Histogram of Triples")
hist(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", xlab = "Home Runs", main = "Histogram of Home Runs")
boxplot(moneyball$TEAM_BATTING_3B, col = "#A71930", main = "Boxplot of Triples")
boxplot(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", main = "Boxplot of Home Runs")
par(mfrow=c(1,1))

# Walks, Strikeouts, HBP
par(mfrow=c(2,3))
hist(moneyball$TEAM_BATTING_BB, col = "#A71930", xlab = "Walks", main = "Histogram of Walks")
hist(moneyball$TEAM_BATTING_SO, col = "#09ADAD", xlab = "Strikeouts", main = "Histogram of Strikeouts")
hist(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", xlab = "Hit By Pitches", main = "Histogram of HBP")
boxplot(moneyball$TEAM_BATTING_BB, col = "#A71930", main = "Boxplot of Walks")
boxplot(moneyball$TEAM_BATTING_SO, col = "#09ADAD", main = "Boxplot of Strikeouts")
boxplot(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", main = "Boxplot of HBP")
par(mfrow=c(1,1))

# Stolen Bases and Caught Stealing
par(mfrow=c(2,2))
hist(moneyball$TEAM_BASERUN_SB, col = "#A71930", xlab = "Stolen Bases", main = "Histogram of Steals")
hist(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", xlab = "Caught Stealing", main = "Histogram of CS")
boxplot(moneyball$TEAM_BASERUN_SB, col = "#A71930", main = "Boxplot of Steals")
boxplot(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", main = "Boxplot of CS")
par(mfrow=c(1,1))

################ Pitching ############
# Hits and Home Runs
par(mfrow=c(2,2))
hist(moneyball$TEAM_PITCHING_H, col = "#A71930", xlab = "Hits Against", main = "Histogram of Hits Against")
hist(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", xlab = "Home Runs Against", main = "Histograms of HR Against")
boxplot(moneyball$TEAM_PITCHING_H, col = "#A71930", main = "Boxplot of Hits Against")
boxplot(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", main = "Boxplot of HR Against")
par(mfrow=c(1,1))

# Walks and Strikeouts
par(mfrow=c(2,2))
hist(moneyball$TEAM_PITCHING_BB, col = "#A71930", xlab = "Walks Allowed", main = "Histogram of Walks Allowed")
hist(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", xlab = "Strikeouts", main = "Histograms of Strikeouts")
boxplot(moneyball$TEAM_PITCHING_BB, col = "#A71930", main = "Boxplot of Walks Allowed")
boxplot(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", main = "Boxplot of Strikeouts")
par(mfrow=c(1,1))

############## Fielding ###########
# Double Plays and Errors 
par(mfrow=c(2,2))
hist(moneyball$TEAM_FIELDING_DP, col = "#A71930", xlab = "Double Plays", main = "Histogram of Double Plays")
hist(moneyball$TEAM_FIELDING_E, col = "#09ADAD", xlab = "Errors Committed", main = "Histogram of Errors Committed")
boxplot(moneyball$TEAM_FIELDING_DP, col = "#A71930", main = "Boxplot of Double Plays")
boxplot(moneyball$TEAM_FIELDING_E, col = "#09ADAD", main = "Boxplot of Errors Committed")
par(mfrow=c(1,1))


######## Scatterplot Matrix ##########

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Batting Stats and Wins
pairs(moneyball[2:8], lower.panel=panel.smooth, upper.panel = panel.cor)

#Baserunning  Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_BASERUN_CS + moneyball$TEAM_BASERUN_SB, lower.panel = panel.smooth)

#Pitcher Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_PITCHING_BB + moneyball$TEAM_PITCHING_H + 
        moneyball$TEAM_PITCHING_HR + moneyball$TEAM_PITCHING_SO, lower.panel = panel.smooth)

pairs(moneyball[2,9,10,11,12,13])

######################### Part 2: Data Preparation #####################


#Target wins 40-115, average should be around 80
moneyball$TEAM_BATTING_1B <- moneyball$TEAM_BATTING_H - moneyball$TEAM_BATTING_HR - moneyball$TEAM_BATTING_3B -
  moneyball$TEAM_BATTING_2B
moneyball.input <- moneyball #saving a copy to not touch
moneyball.multiple<-moneyball #copy for multiple imputation

#in this copy, set outliers to the value at the 1st or 99th percentile for low and high outliers respectively.
outliersAdjust<-function(dt,var){
  for(i in 2:length(var)){ # for(i in 2:length(vars)){
    print(names(dt)[i])
    cutoff.lower<- quantile(dt[[var[i]]],.01,na.rm=TRUE)
    cutoff.higher<-quantile(dt[[var[i]]],.99,na.rm=TRUE)
    print(c(cutoff.lower,":",cutoff.higher))
    print("BEFORE")
    print(summary(dt[[var[i]]]))
    print("AFTER")
    dt[which(dt[[var[i]]]<cutoff.lower),][[var[i]]]<-cutoff.lower # Set the variables lower than the 1st percentile to NA to impute
    dt[which(dt[[var[i]]]>cutoff.higher),][[var[i]]]<-cutoff.higher # Set the variables lower than the 99th percentile to NA to impute
    print(summary(dt[[var[i]]]))
  }
  moneyball<-dt 
}
moneyball<-outliersAdjust(moneyball,names(moneyball))

# Fix outliers, set to NA then use MICE to impute, 0-1st percentile, 99-100th percentile.
outliersToNA<-function(dt,var){
  for(i in 2:length(var)){ # for(i in 2:length(vars)){
    print(names(dt)[i])
    cutoff.lower<- quantile(dt[[var[i]]],.01,na.rm=TRUE)
    cutoff.higher<-quantile(dt[[var[i]]],.99,na.rm=TRUE)
    print(c(cutoff.lower,":",cutoff.higher))
    print("BEFORE")
    print(summary(dt[[var[i]]]))
    print("AFTER")
    dt[which(dt[[var[i]]]<cutoff.lower),][[var[i]]]<-NA # Set the variables lower than the 1st percentile to NA to impute
    dt[which(dt[[var[i]]]>cutoff.higher),][[var[i]]]<-NA # Set the variables lower than the 99th percentile to NA to impute
    print(summary(dt[[var[i]]]))
  }
  moneyball.multiple<-dt 
}
moneyball.multiple<-outliersToNA(moneyball.multiple,names(moneyball.multiple))



# Create a flag for missingness for each column
for(i in 2:length(names(moneyball))){
  nm.orig<-names(moneyball)[i]
  nm<-paste(names(moneyball)[i],"_NA",sep="")
  moneyball[,nm]<-is.na(moneyball[,nm.orig])
  
}

for(i in 2:length(names(moneyball.multiple))){
  nm.orig<-names(moneyball.multiple)[i]
  nm<-paste(names(moneyball.multiple)[i],"_NA",sep="")
  moneyball.multiple[,nm]<-is.na(moneyball.multiple[,nm.orig])
}


#str(moneyball)
#str(moneyball.multiple)
#nrow(moneyball[which(is.na(moneyball$TARGET_WINS)==TRUE),c("TARGET_WINS","TARGET_WINS_NA")])

# MICE for multiple imputation, run before and after MICE for multiple imputation so data is in TEMP copy
moneyball.multiple$TEAM_BATTING_1B <- moneyball.multiple$TEAM_BATTING_H - moneyball.multiple$TEAM_BATTING_HR - moneyball.multiple$TEAM_BATTING_3B -
  moneyball.multiple$TEAM_BATTING_2B
moneyball.multiple$log_TEAM_BATTING_1B <- log(moneyball.multiple$TEAM_BATTING_1B)
moneyball.multiple$log_TEAM_BATTING_3B <- log(moneyball.multiple$TEAM_BATTING_3B)
moneyball.multiple$log_TEAM_BASERUN_SB <- log(moneyball.multiple$TEAM_BASERUN_SB)
moneyball.multiple$log_TEAM_BASERUN_CS <- log(moneyball.multiple$TEAM_BASERUN_CS)
moneyball.multiple$TEAM_BATTING_SO[is.na(moneyball.multiple$TEAM_BATTING_SO)] = mean(moneyball.multiple$TEAM_BATTING_SO, na.rm = TRUE)
moneyball.multiple$TEAM_FIELDING_E[(moneyball.multiple$TEAM_FIELDING_E > 500)] = 500
moneyball.multiple$sqrt_TEAM_PITCHING_HR <- sqrt(moneyball.multiple$TEAM_PITCHING_HR)
moneyball.multiple$SB_PCT <- moneyball.multiple$TEAM_BASERUN_SB/(1.0*moneyball.multiple$TEAM_BASERUN_SB+moneyball.multiple$TEAM_BASERUN_CS)
moneyball.multiple$OB_TO_SO_RATIO<-(moneyball.multiple$TEAM_BATTING_H+moneyball.multiple$TEAM_BATTING_BB)/moneyball.multiple$TEAM_BATTING_SO


#Fix Missing Values Using Mean of All Seasons for Single Imputation Copy
moneyball$TEAM_BATTING_SO[is.na(moneyball$TEAM_BATTING_SO)] = mean(moneyball$TEAM_BATTING_SO, na.rm = TRUE)
moneyball$TEAM_BATTING_HBP[is.na(moneyball$TEAM_BATTING_HBP)] = mean(moneyball$TEAM_BATTING_HBP, na.rm = TRUE)
moneyball$TEAM_BASERUN_SB[is.na(moneyball$TEAM_BASERUN_SB)] = mean(moneyball$TEAM_BASERUN_SB, na.rm = TRUE)
moneyball$TEAM_BASERUN_CS[is.na(moneyball$TEAM_BASERUN_CS)] = mean(moneyball$TEAM_BASERUN_CS, na.rm = TRUE)
moneyball$TEAM_FIELDING_DP[is.na(moneyball$TEAM_FIELDING_DP)] = mean(moneyball$TEAM_FIELDING_DP, na.rm = TRUE)
moneyball$TEAM_PITCHING_SO[is.na(moneyball$TEAM_PITCHING_SO)] = mean(moneyball$TEAM_PITCHING_SO, na.rm = TRUE)

# run MICE for multiple imputation
md.pattern(moneyball.multiple) # generates a summary table of missing values for each variables
moneyball_aggr = aggr(moneyball.multiple, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                      labels=names(moneyball.multiple), cex.axis=.3, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

tempData <- mice(moneyball.multiple, m=5,maxit=25,meth='rf',seed=23456)
densityplot(tempData)
moneyball_orig <- moneyball.multiple #another backup of the data
moneyball.multiple <- complete(tempData,1)


#Straighten Relationships
moneyball$TEAM_BATTING_1B <- moneyball$TEAM_BATTING_H - moneyball$TEAM_BATTING_HR - moneyball$TEAM_BATTING_3B -
  moneyball$TEAM_BATTING_2B
moneyball$log_TEAM_BATTING_1B <- log(moneyball$TEAM_BATTING_1B)
moneyball$log_TEAM_BATTING_3B <- log(moneyball$TEAM_BATTING_3B)
moneyball$log_TEAM_BASERUN_SB <- log(moneyball$TEAM_BASERUN_SB)
moneyball$log_TEAM_BASERUN_CS <- log(moneyball$TEAM_BASERUN_CS)
moneyball$TEAM_BATTING_SO[is.na(moneyball$TEAM_BATTING_SO)] = mean(moneyball$TEAM_BATTING_SO, na.rm = TRUE)
moneyball$TEAM_FIELDING_E[(moneyball$TEAM_FIELDING_E > 500)] = 500
moneyball$sqrt_TEAM_PITCHING_HR <- sqrt(moneyball$TEAM_PITCHING_HR)
moneyball$SB_PCT <- moneyball$TEAM_BASERUN_SB/(1.0*moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS)
moneyball$OB_TO_SO_RATIO<-(moneyball$TEAM_BATTING_H+moneyball$TEAM_BATTING_BB)/moneyball$TEAM_BATTING_SO

#Same for MICE copy
moneyball.multiple$TEAM_BATTING_1B <- moneyball.multiple$TEAM_BATTING_H - moneyball.multiple$TEAM_BATTING_HR - moneyball.multiple$TEAM_BATTING_3B -
  moneyball.multiple$TEAM_BATTING_2B
moneyball.multiple$log_TEAM_BATTING_1B <- log(moneyball.multiple$TEAM_BATTING_1B)
moneyball.multiple$log_TEAM_BATTING_3B <- log(moneyball.multiple$TEAM_BATTING_3B)
moneyball.multiple$log_TEAM_BASERUN_SB <- log(moneyball.multiple$TEAM_BASERUN_SB)
moneyball.multiple$log_TEAM_BASERUN_CS <- log(moneyball.multiple$TEAM_BASERUN_CS)
moneyball.multiple$TEAM_BATTING_SO[is.na(moneyball.multiple$TEAM_BATTING_SO)] = mean(moneyball.multiple$TEAM_BATTING_SO, na.rm = TRUE)
moneyball.multiple$TEAM_FIELDING_E[(moneyball.multiple$TEAM_FIELDING_E > 500)] = 500
moneyball.multiple$sqrt_TEAM_PITCHING_HR <- sqrt(moneyball.multiple$TEAM_PITCHING_HR)
moneyball.multiple$SB_PCT <- moneyball.multiple$TEAM_BASERUN_SB/(1.0*moneyball.multiple$TEAM_BASERUN_SB+moneyball.multiple$TEAM_BASERUN_CS)
moneyball.multiple$OB_TO_SO_RATIO<-(moneyball.multiple$TEAM_BATTING_H+moneyball.multiple$TEAM_BATTING_BB)/moneyball.multiple$TEAM_BATTING_SO



#Check that na's are gone. 
summary(moneyball)
summary(moneyball.multiple)
par(mfrow=c(1,1))
moneyball.cor <- cor(subset(moneyball,select=c(2:17,34:40))) # skip the index
moneyball.cor[,c("TARGET_WINS")] # Get listing of variables and their correlation to TARGET_WINS
moneyball.cor <- cor(subset(moneyball,select=c(2:8,34:36,41))) #batting
corrplot(moneyball.cor)
moneyball.cor <- cor(subset(moneyball,select=c(2,12:15,39))) #pitching
corrplot(moneyball.cor)
moneyball.cor <- cor(subset(moneyball,select=c(2,16,17)))#fielding
corrplot(moneyball.cor)
moneyball.cor <- cor(subset(moneyball,select=c(2,9,10,37,38,40)))#baserunning
corrplot(moneyball.cor)

#Remove bad data from data set

moneyball2 <- subset(moneyball, TARGET_WINS >= 21 & TARGET_WINS <= 120)
moneyball2 <- subset(moneyball2, TEAM_PITCHING_H < 2000)
moneyball3<-subset(moneyball.multiple, TARGET_WINS >= 21 & TARGET_WINS <= 120)
moneyball3 <- subset(moneyball3, TEAM_PITCHING_H < 2000)

#################### Part 3: Model Creation ############################################

#Function for Mean Square Error Calculation
mse <- function(sm) 
  mean(sm$residuals^2)

# Stepwise Approach
stepwisemodel <- lm(formula = TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
                      TEAM_BATTING_H + 
                      TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR + 
                      TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + log_TEAM_BATTING_1B + 
                      log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB + SB_PCT + log_TEAM_BASERUN_CS + 
                      sqrt_TEAM_PITCHING_HR+OB_TO_SO_RATIO, data = moneyball2)
stepwise <- stepAIC(stepwisemodel, direction = "both")
summary(stepwise) #Adjusted R-squared:  0.4011 
vif(stepwise)
sqrt(vif(stepwise)) > 2
summary(stepwise$fitted.values)

# All subsets regression
subsets <- regsubsets(TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
                        TEAM_BATTING_H + 
                        TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR + 
                        TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + 
                        SB_PCT + log_TEAM_BASERUN_CS + 
                        sqrt_TEAM_PITCHING_HR,  data = moneyball2, nbest = 2)
plot(subsets, scale="adjr2")

subset <- lm(TARGET_WINS ~ 
               TEAM_BATTING_1B + TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_SO + TEAM_BASERUN_SB +
               TEAM_BASERUN_CS + TEAM_FIELDING_E + TEAM_FIELDING_DP + SB_PCT, data = moneyball2)
summary(subset) #Adjusted R-squared:  0.3624 
summary(subset$fitted.values)

# Model 3
model3 <- lm(TARGET_WINS ~ 
               TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
               TEAM_BASERUN_SB + TEAM_BASERUN_CS + 
               TEAM_FIELDING_E + TEAM_FIELDING_DP +
               TEAM_PITCHING_SO + TEAM_PITCHING_BB, data = moneyball2)
summary(model3) #Adjusted R-squared:  0.3831
vif(model3)
summary(model3$fitted.values)

#full_model
model4 <- lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_1B + TEAM_BATTING_2B + #TEAM_BATTING_3B +
        TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB +
        TEAM_BASERUN_CS + TEAM_BATTING_HBP +
        TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_BB +
        TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + 
         log_TEAM_BATTING_1B + log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB +
        log_TEAM_BASERUN_CS + sqrt_TEAM_PITCHING_HR + SB_PCT + OB_TO_SO_RATIO,data=moneyball2)
summary(model4) #Adjusted R-squared:  0.4023 
vif(model4)
alias(model4)
summary(model4$fitted.values)

#subset_model
model5 <- lm(TARGET_WINS ~ TEAM_BATTING_H +TEAM_BATTING_2B +log_TEAM_BATTING_1B +log_TEAM_BATTING_3B+TEAM_BATTING_BB 
             +TEAM_BATTING_SO+TEAM_BASERUN_SB+TEAM_PITCHING_H +TEAM_PITCHING_BB +TEAM_PITCHING_SO +TEAM_FIELDING_E
             +TEAM_FIELDING_DP+SB_PCT+OB_TO_SO_RATIO 
             ,data=moneyball2)
summary(model5) #Adjusted R-squared:  0.3896 
vif(model5)
summary(model5$fitted.values)

model6.step <- lm(formula = TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
                      TEAM_BATTING_H + 
                      TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR + 
                      TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + log_TEAM_BATTING_1B + 
                      log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB + SB_PCT + log_TEAM_BASERUN_CS + 
                      sqrt_TEAM_PITCHING_HR+OB_TO_SO_RATIO+TEAM_BATTING_H_NA+TEAM_BATTING_2B_NA+TEAM_BATTING_3B_NA
                      +TEAM_BATTING_HR_NA+TEAM_BATTING_BB_NA+TEAM_BATTING_SO_NA+TEAM_BASERUN_SB_NA+TEAM_BASERUN_CS_NA+
                      TEAM_BATTING_HBP_NA+TEAM_PITCHING_H_NA+TEAM_PITCHING_HR_NA+TEAM_PITCHING_BB_NA + TEAM_PITCHING_SO_NA
                      +TEAM_FIELDING_E_NA+TEAM_FIELDING_DP_NA
                  , data = moneyball2)
model6 <- stepAIC(model6.step, direction = "both")
summary(model6) #Adjusted R-squared:  0.429 
vif(model6)
sqrt(vif(model6)) > 2
summary(model6$fitted.values)

model7.step <- lm(formula = TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
                    TEAM_BATTING_H + 
                    TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR + 
                    TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + log_TEAM_BATTING_1B + 
                    log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB + SB_PCT + log_TEAM_BASERUN_CS + 
                    sqrt_TEAM_PITCHING_HR+TEAM_BATTING_H_NA+TEAM_BATTING_2B_NA+TEAM_BATTING_3B_NA
                  +TEAM_BATTING_HR_NA+TEAM_BATTING_BB_NA+TEAM_BATTING_SO_NA+TEAM_BASERUN_SB_NA+TEAM_BASERUN_CS_NA+
                    TEAM_BATTING_HBP_NA+TEAM_PITCHING_H_NA+TEAM_PITCHING_HR_NA+TEAM_PITCHING_BB_NA + TEAM_PITCHING_SO_NA
                  +TEAM_FIELDING_E_NA+TEAM_FIELDING_DP_NA
                  , data = moneyball2)
model7 <- stepAIC(model7.step, direction = "both")
summary(model7) # Adjusted R-squared:  0.4275 
vif(model7)
sqrt(vif(model7)) > 2
summary(model7$fitted.values)

# MICE models
# Full Model
model8 <-
  with(
    tempData,
    lm(
      TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B +
        TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB +
        TEAM_BASERUN_CS + TEAM_BATTING_HBP +
        TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_BB +
        TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + 
        TEAM_BATTING_1B + log_TEAM_BATTING_1B + log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB +
        log_TEAM_BASERUN_CS + sqrt_TEAM_PITCHING_HR + SB_PCT + OB_TO_SO_RATIO
    )
  )
summary(pool(model8))
pool.r.squared(model8,adjusted=TRUE) #Adjusted R2 on the high side 0.3319148


model9 <-with(tempData,lm(TARGET_WINS~TEAM_FIELDING_E+TEAM_FIELDING_DP+TEAM_BASERUN_SB+TEAM_BATTING_3B+OB_TO_SO_RATIO
                          +TEAM_BATTING_2B+TEAM_BATTING_SO+TEAM_BATTING_H))
summary(pool(model9))
pool.r.squared(model9) #Adjusted R2 on the high side 0.2795922

######## Performance #######
AIC(stepwisemodel) #15383.4
AIC(subset) #15497.47
AIC(model3) #15431.93
AIC(model4) #15379.98
AIC(model5) #15414.47
AIC(model6) #15284.51
AIC(model7) #15289.88
#AIC model 8 #17800.23 17800.23 17800.23 17800.23 17800.23
mse(stepwisemodel) #116.7048
mse(subset) #124.8407
mse(model3) #120.734
mse(model4) #116.2766
mse(model5) #119.2212
mse(model6) #111.2364
mse(model7) #111.5327


#https://stackoverflow.com/questions/51815570/how-to-extract-aic-and-log-likelihood-from-pooled-glm
L_df<-mice::complete(tempData,"long",include=F)
AIC1<-c()
logLik1 <- c()
m<-max(L_df$.imp)
for(i in 1:m){
  model.imputed1 <- lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B +
      TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB +
      TEAM_BASERUN_CS + TEAM_BATTING_HBP +
      TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_BB +
      TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + 
      TEAM_BATTING_1B + log_TEAM_BATTING_1B + log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB +
      log_TEAM_BASERUN_CS + sqrt_TEAM_PITCHING_HR + SB_PCT + OB_TO_SO_RATIO,
      data= L_df[which(L_df$.imp == m),])
  AIC1[i] <- AIC(model.imputed1)
  logLik1[i] <- logLik(model.imputed1)
}
print(AIC1) #17800.23 17800.23 17800.23 17800.23 17800.23


# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(stepwisemodel)
plot(subset)
plot(model3)
plot(model4)
plot(model5)
plot(model6)
plot(model7)

# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(stepwisemodel$residuals)
qqnorm(subset$residuals)
qqnorm(model3$residuals)
qqnorm(model4$residuals)
qqnorm(model5$residuals)
qqline(stepwisemodel$residuals)
qqline(subset$residuals)
qqline(model3$residuals)
qqline(model4$residuals)
qqline(model5$residuals)
qqline(model6$residuals)
qqline(model7$residuals)


# Make a scatterplot
plot(moneyball2$TARGET_WINS,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_BATTING_H,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_BATTING_1B ,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_BATTING_2B,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_BATTING_HR,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_BATTING_BB,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_BATTING_SO,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_BASERUN_SB,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_BASERUN_CS,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_BATTING_HBP,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_PITCHING_HR,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_PITCHING_BB,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_PITCHING_SO,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_BATTING_H,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_FIELDING_E,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$TEAM_FIELDING_DP,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$log_TEAM_BATTING_1B,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$log_TEAM_BATTING_3B,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$log_TEAM_BASERUN_SB,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$log_TEAM_BASERUN_CS,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$sqrt_TEAM_PITCHING_HR,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$SB_PCT,model4$residuals)
title('Residual vs Predictor')

plot(moneyball2$OB_TO_SO_RATIO,model4$residuals)
title('Residual vs Predictor')


#####
#Designated proper working environment on my computer. You will want to make sure it is in proper place for your computer.
#####

#################### Test Data ##########################
setwd("C:/Users/Downloads/Documents/NW/411/411/Unit01 HW Moneyball/") #setwd("D:/RFile/")
moneyball_test=read.csv("moneyball_test.csv",header=T)

summary(moneyball_test)
# Fix singles
moneyball_test$TEAM_BATTING_1B <- moneyball_test$TEAM_BATTING_H - moneyball_test$TEAM_BATTING_HR -
  moneyball_test$TEAM_BATTING_3B -moneyball_test$TEAM_BATTING_2B

# Outliers outside the range of the train data equivalent will be set to 5th or 95th percentile
outliersToCap<-function(dt,var){
  for(i in 2:length(var)){ # for(i in 2:length(vars)){
    print(names(dt)[i])
    cutoff.lower<- quantile(dt[[var[i]]],.01,na.rm=TRUE) #.05
    cutoff.higher<-quantile(dt[[var[i]]],.99,na.rm=TRUE) #.99
    replace.lower<-quantile(moneyball2[[var[i]]],.01,na.rm=TRUE) #.1
    replace.higher<-quantile(moneyball2[[var[i]]],.99,na.rm=TRUE) #.95
    print(c(cutoff.lower,":",cutoff.higher))
    print("BEFORE")
    print(summary(dt[[var[i]]]))
    print("AFTER")
    dt[which(dt[[var[i]]]<cutoff.lower),][[var[i]]]<-replace.lower # Set the variables lower than the 1st percentile to NA to impute
    dt[which(dt[[var[i]]]>cutoff.higher),][[var[i]]]<-replace.higher # Set the variables lower than the 99th percentile to NA to impute
    print(summary(dt[[var[i]]]))
  }
  moneyball_test<-dt 
}
moneyball_test<-outliersToCap(moneyball_test,names(moneyball_test))





moneyball_test$TEAM_BATTING_SO_NATRUE<-is.na(moneyball_test$TEAM_BATTING_SO)
moneyball_test$TEAM_BASERUN_SB_NATRUE<-is.na(moneyball_test$TEAM_BASERUN_SB)
moneyball_test$TEAM_BASERUN_CS_NATRUE<-is.na(moneyball_test$TEAM_BASERUN_CS)
moneyball_test$TEAM_BATTING_HBP_NATRUE<-is.na(moneyball_test$TEAM_BATTING_HBP)
moneyball_test$TEAM_FIELDING_DP_NATRUE<-is.na(moneyball_test$TEAM_FIELDING_DP)
moneyball_test$TEAM_BATTING_SO[is.na(moneyball_test$TEAM_BATTING_SO)] = mean(moneyball_test$TEAM_BATTING_SO, na.rm = TRUE)
moneyball_test$TEAM_BATTING_HBP[is.na(moneyball_test$TEAM_BATTING_HBP)] = mean(moneyball_test$TEAM_BATTING_HBP, na.rm = TRUE)
moneyball_test$TEAM_BASERUN_SB[is.na(moneyball_test$TEAM_BASERUN_SB)] = mean(moneyball_test$TEAM_BASERUN_SB, na.rm = TRUE)
moneyball_test$TEAM_BASERUN_CS[is.na(moneyball_test$TEAM_BASERUN_CS)] = mean(moneyball_test$TEAM_BASERUN_CS, na.rm = TRUE)
moneyball_test$TEAM_FIELDING_DP[is.na(moneyball_test$TEAM_FIELDING_DP)] = mean(moneyball_test$TEAM_FIELDING_DP, na.rm = TRUE)
moneyball_test$TEAM_PITCHING_SO[is.na(moneyball_test$TEAM_PITCHING_SO)] = mean(moneyball_test$TEAM_PITCHING_SO, na.rm = TRUE)
moneyball_test$TEAM_BASERUN_CS[moneyball_test$TEAM_BASERUN_CS < 1] = 1
moneyball_test$SB_PCT <- moneyball_test$TEAM_BASERUN_SB/(1.0*moneyball_test$TEAM_BASERUN_SB+moneyball_test$TEAM_BASERUN_CS)
moneyball_test$SB_PCT[is.na(moneyball_test$SB_PCT)] = mean(moneyball_test$SB_PCT)
moneyball_test$log_TEAM_BASERUN_CS <- log(moneyball_test$TEAM_BASERUN_CS)
moneyball_test$OB_TO_SO_RATIO<-(moneyball_test$TEAM_BATTING_H+moneyball_test$TEAM_BATTING_BB)/moneyball_test$TEAM_BATTING_SO
moneyball_test$log_TEAM_BATTING_1B <- log(moneyball_test$TEAM_BATTING_1B)
moneyball_test$log_TEAM_BATTING_3B <- log(moneyball_test$TEAM_BATTING_3B)
moneyball_test$log_TEAM_BASERUN_SB <- log(moneyball_test$TEAM_BASERUN_SB)
moneyball_test$log_TEAM_BASERUN_CS <- log(moneyball_test$TEAM_BASERUN_CS)
moneyball_test$TEAM_FIELDING_E[(moneyball_test$TEAM_FIELDING_E > 500)] = 500
moneyball_test$TEAM_PITCHING_H[(moneyball_test$TEAM_PITCHING_H > 2000)] = 2000
moneyball_test$sqrt_TEAM_PITCHING_HR <- sqrt(moneyball_test$TEAM_PITCHING_HR)

summary(moneyball_test)
#moneyball2 <- subset(moneyball, TARGET_WINS >= 21 & TARGET_WINS <= 120)
#moneyball2 <- subset(moneyball2, TEAM_PITCHING_H < 2000)


# Stand Alone Scoring
# Chosen Model 7
moneyball_test$P_TARGET_WINS <- 
  -116.7+
  (0.2685 * moneyball_test$TEAM_BATTING_3B)+
  (0.3507 * moneyball_test$TEAM_BATTING_HR)+
  (0.03007 * moneyball_test$TEAM_BATTING_BB)+
  (-0.04358 * moneyball_test$TEAM_BATTING_SO)+
  (0.1007 * moneyball_test$TEAM_BASERUN_SB)+
  (0.1018 * moneyball_test$TEAM_BASERUN_CS)+
  (-0.224 * moneyball_test$TEAM_PITCHING_HR)+
  (0.02523 * moneyball_test$TEAM_PITCHING_SO)+
  (-0.1357 * moneyball_test$TEAM_FIELDING_E)+
  (-0.1018 * moneyball_test$TEAM_FIELDING_DP)+
  (35.14 * moneyball_test$log_TEAM_BATTING_1B)+
  (-3.678 * moneyball_test$log_TEAM_BATTING_3B)+
  (-8.478 * moneyball_test$SB_PCT)+
  (-10.15 * moneyball_test$log_TEAM_BASERUN_CS)+
  (8.711 * moneyball_test$TEAM_BATTING_SO_NATRUE)+
  (21.53 * moneyball_test$TEAM_BASERUN_SB_NATRUE)+
  (4.739 * moneyball_test$TEAM_BASERUN_CS_NATRUE)+
  (4.422 * moneyball_test$TEAM_BATTING_HBP_NATRUE)+
  (8.055 * moneyball_test$TEAM_FIELDING_DP_NATRUE)
  


#subset of data set for the deliverable "Scored data file"
moneyball_test[which(moneyball_test$P_TARGET_WINS<20),]$P_TARGET_WINS<-20 # Cap bottom
#moneyball_test[which(moneyball_test$P_TARGET_WINS>162),]$P_TARGET_WINS<-162 #Cap top
prediction <- moneyball_test[c("INDEX","P_TARGET_WINS")]
summary(prediction)


#####
#Note, this next function will output an Excel file in your work environment called write.xlsx.
#####

#Prediction File 
write.xlsx(prediction, file = "write.xlsx", sheetName = "Predictions",
           col.names = TRUE)