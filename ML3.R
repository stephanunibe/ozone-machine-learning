# Author: Stephan Raess
# Date: June 19, 2024

# (1) Libraries ----------------------------------------------------------------

library(utils)
library(boot)
library(leaps) 
library(readr)

# (2) Import data --------------------------------------------------------------

readData = function(filePath) {
  
  # Read CSV file and remove the first 5 rows
  df <- read_delim(filePath, delim = ";", skip = 5)
  
  # Modify column names
  names(df) <- as.character(unlist(df[1, ]))
  df <- df[-1, ]
  names(df) <- names(df) <- c('DATE','O3','NO2','SO2','CO','PM10','PM2_5','EC',
                              'CPC','NMVOC','NOX','TEMP','PREC','RAD')
  
  # Data type conversion
  df$O3 <- as.numeric(df$O3)
  df$NO2 <- as.numeric(df$NO2)
  df$SO2 <- as.numeric(df$SO2)
  df$CO <- as.numeric(df$CO)
  df$PM10 <- as.numeric(df$PM10)
  df$PM2_5 <- as.numeric(df$PM2_5)
  df$EC <- as.numeric(df$EC)
  df$CPC <- as.numeric(df$CPC)
  df$NMVOC <- as.numeric(df$NMVOC)
  df$NOX <- as.numeric(df$NOX)
  df$TEMP <- as.numeric(df$TEMP)
  df$PREC <- as.numeric(df$PREC)
  df$RAD <- as.numeric(df$RAD)
  
  return(df)
}

# Read multiple files and concatenate data frames 
dfAll <- NULL
count <- 0
for (year in 2016:2023){ 
  
  filePath <- paste0("data/lugano/LUG_",year,".csv")
  cat("Processing ", filePath, "\n")
  df <- readData(filePath)
  
  if(count==0){
    dfAll <- df
  }
  else{
    dfAll = rbind(dfAll, df)
  }
  
  count <- count + 1
  
}

# (3) Pre-process data ---------------------------------------------------------

nrow(dfAll) # number of rows
sum(is.na(dfAll)) # number of missing values
sum(is.na(dfAll))/nrow(dfAll) # ratio of missing values
data <- na.omit(dfAll) # remove rows with missing values
data2 <- data[,2:14] # remove columns with non-numeric values

# (4) Create a training and a test set -----------------------------------------

set.seed(1) 
train = sample(nrow(data2),nrow(data2)/2) # training set (50% of data)
test = (-train)

# (5) Simple linear regression -------------------------------------------------

lm.fit1 = lm(O3~RAD,data=data2) # for model comparison using anova()
lm.fit1p = lm(data2$O3~poly(data2$RAD,1)) 
summary(lm.fit1)
confint(lm.fit1)

# Estimate test error rates using different resampling methods

# Validation set approach
lm.fit1_vsa = lm(O3~RAD, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit1_vsa, data2))[test])^2 ) 

# Leave-One-Out Cross Validation (LOOCV)
set.seed(1)
glm.fit1 = glm(O3~RAD, data=data2) 
cvErr1_LOOCV = cv.glm(data2,glm.fit1)
cvErr1_LOOCV$delta # delta contains raw CV and adjusted CV estimate

# K-fold CV
set.seed(1)
cvErr1_k5 = cv.glm(data2,glm.fit1,K=5)$delta[1]
cvErr1_k5 
cvErr1_k10 = cv.glm(data2,glm.fit1,K=10)$delta[1]
cvErr1_k10 

# (6) Polynomial regression ----------------------------------------------------

# Fourth order polynomial
lm.fit2_4 = lm(O3~poly(RAD,4), data=data2) 
glm.fit2_4 = glm(O3~poly(RAD,4), data=data2) 
summary(glm.fit2_4)
confint(glm.fit2_4)

# Third order polynomial
lm.fit2_3 = lm(O3~poly(RAD,3), data=data2) 
glm.fit2_3 = glm(O3~poly(RAD,3), data=data2) 
summary(glm.fit2_3)
confint(glm.fit2_3)

# Second order polynomial
lm.fit2_2 = lm(O3~poly(RAD,2), data=data2) 
glm.fit2_2 = glm(O3~poly(RAD,2), data=data2) 
summary(glm.fit2_2)
confint(glm.fit2_2)

# K-fold CV
set.seed(1)
cvErr2_2_k10 = cv.glm(data2, glm.fit2_2, K=10)$delta[1]
cvErr2_2_k10 
cvErr2_3_k10 = cv.glm(data2, glm.fit2_3, K=10)$delta[1]
cvErr2_3_k10 
cvErr2_4_k10 = cv.glm(data2, glm.fit2_4, K=10)$delta[1]
cvErr2_4_k10 

# Validation set approach
lm.fit2_2_vsa = lm(O3~poly(RAD,2), data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit2_2_vsa, data2))[test])^2 ) 

lm.fit2_3_vsa = lm(O3~poly(RAD,3), data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit2_3_vsa, data2))[test])^2 ) 

lm.fit2_4_vsa = lm(O3~poly(RAD,4), data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit2_4_vsa, data2))[test])^2 ) 

# Comparison of models using hypothesis tests
lm.fit2_1 = lm(O3~poly(RAD,1), data=data2)
lm.fit2_3 = lm(O3~poly(RAD,3), data=data2)
anova(lm.fit2_1,lm.fit2_3) 

# (7) Model evaluation ---------------------------------------------------------

set.seed(1)

glm.fit3_1 = glm(O3~NO2+SO2+CO+NMVOC+NOX+TEMP+PREC+RAD, data=data2) 
summary(glm.fit3_1) 
confint(glm.fit3_1)
cv.glm(data2, glm.fit3_1, K=10)$delta[1] 
lm.fit3_1_vsa = lm(O3~NO2+SO2+CO+NMVOC+NOX+TEMP+PREC+RAD, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit3_1_vsa, data2))[test])^2 ) # MSE

glm.fit3_2 = glm(O3~NO2+SO2+NMVOC+NOX+TEMP+RAD, data=data2) 
summary(glm.fit3_2) 
cv.glm(data2, glm.fit3_2, K=10)$delta[1] 
lm.fit3_2_vsa = lm(O3~NO2+SO2+NMVOC+NOX+TEMP+RAD, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit3_2_vsa, data2))[test])^2 ) 

glm.fit3_3 = glm(O3~SO2+NMVOC+NOX+TEMP+RAD, data=data2) 
summary(glm.fit3_3) 
cv.glm(data2, glm.fit3_3, K=10)$delta[1] 
lm.fit3_3_vsa = lm(O3~SO2+NMVOC+NOX+TEMP+RAD, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit3_3_vsa, data2))[test])^2 ) 

glm.fit3_4 = glm(O3~SO2+NMVOC+NO2+TEMP+RAD, data=data2) 
summary(glm.fit3_4) 
cv.glm(data2, glm.fit3_4, K=10)$delta[1] 
lm.fit3_4_vsa = lm(O3~SO2+NMVOC+NO2+TEMP+RAD, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit3_4_vsa, data2))[test])^2 ) 

glm.fit3_5 = glm(O3~NO2+SO2+NMVOC+NOX+RAD, data=data2) 
summary(glm.fit3_5) 
cv.glm(data2, glm.fit3_5, K=10)$delta[1] 
lm.fit3_5_vsa = lm(O3~NO2+SO2+NMVOC+NOX+RAD, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit3_5_vsa, data2))[test])^2 ) 

glm.fit3_6 = glm(O3~NO2+SO2+NMVOC+NOX+TEMP, data=data2) 
summary(glm.fit3_6) 
cv.glm(data2, glm.fit3_6, K=10)$delta[1] 
lm.fit3_6_vsa = lm(O3~NO2+SO2+NMVOC+NOX+TEMP, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit3_6_vsa, data2))[test])^2 ) 

glm.fit3_7 = glm(O3~NO2+NOX+TEMP+RAD, data=data2) 
cv.glm(data2, glm.fit3_7, K=10)$delta[1] 
lm.fit3_7_vsa = lm(O3~NO2+NOX+TEMP+RAD, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit3_7_vsa, data2))[test])^2 ) 

# Best subset selection using Cp
regFit = regsubsets(O3~NO2+I(NO2^2)+I(NO2^3)+NOX+I(NOX^2)+I(NOX^3)+SO2+I(SO2^2)+I(SO2^3)
                    +NMVOC+I(NMVOC^2)+I(NMVOC^3)+TEMP+I(TEMP^2)+I(TEMP^3)+RAD+I(RAD^2)
                    +I(RAD^3)+NO2*SO2+NO2*NMVOC+NO2*NOX+NO2*TEMP+NO2*RAD+SO2*NMVOC
                    +SO2*NOX+SO2*TEMP+SO2*RAD+NMVOC*NOX+NMVOC*TEMP+NMVOC*RAD+NOX*TEMP
                    +NOX*RAD+TEMP*RAD,data2,nvmax=40) 
regSummary = summary(regFit)
plot(regFit, scale ="Cp") 
rssCp = min(regSummary$cp) 
nPredCp = which.min(regSummary$cp) 
stdErrCp = sqrt(var(regSummary$cp))/sqrt(length(regSummary$cp)) 
rssCp + stdErrCp
regSummary$cp 
coef(regFit,12)

glm.fit10 = glm(O3~NO2+I(NO2^2)+I(NO2^3)+NOX+I(NOX^2)+I(NOX^3)+SO2+I(SO2^2)+I(SO2^3)
                +NMVOC+I(NMVOC^2)+I(NMVOC^3)+TEMP+I(TEMP^2)+I(TEMP^3)+RAD+I(RAD^2)
                +I(RAD^3)+NO2*SO2+NO2*NMVOC+NO2*NOX+NO2*TEMP+NO2*RAD+SO2*NMVOC
                +SO2*NOX+SO2*TEMP+SO2*RAD+NMVOC*NOX+NMVOC*TEMP+NMVOC*RAD+NOX*TEMP
                +NOX*RAD+TEMP*RAD, data=data2)
summary(glm.fit10)
confint(glm.fit10)
cvErr10 = cv.glm(data2,glm.fit10)
cv.glm(data2, glm.fit10, K=10)$delta[1] 

lm.fit10_1_vsa = lm(O3~NO2+I(NO2^2)+I(NO2^3)+NOX+I(NOX^2)+I(NOX^3)+SO2+I(SO2^2)+I(SO2^3)
                    +NMVOC+I(NMVOC^2)+I(NMVOC^3)+TEMP+I(TEMP^2)+I(TEMP^3)+RAD+I(RAD^2)
                    +I(RAD^3)+NO2*SO2+NO2*NMVOC+NO2*NOX+NO2*TEMP+NO2*RAD+SO2*NMVOC
                    +SO2*NOX+SO2*TEMP+SO2*RAD+NMVOC*NOX+NMVOC*TEMP+NMVOC*RAD+NOX*TEMP
                    +NOX*RAD+TEMP*RAD, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit10_1_vsa, data2))[test])^2 )

# 21-component model
glm.fit10_2 = glm(O3~NO2+I(NO2^2)+NOX+I(NOX^2)+I(NOX^3)+SO2+NMVOC+TEMP+I(TEMP^2)
                  +I(RAD^2)+I(RAD^3)+NO2*TEMP+NO2*RAD+SO2*TEMP+SO2*RAD+NOX*NMVOC
                  +NMVOC*TEMP+NMVOC*RAD+NOX*TEMP+NOX*RAD+TEMP*RAD
                  -RAD, data=data2) 
summary(glm.fit10_2) 
cv.glm(data2, glm.fit10_2, K=10)$delta[1] 

lm.fit10_2_vsa = lm(O3~NO2+I(NO2^2)+NOX+I(NOX^2)+I(NOX^3)+SO2+NMVOC+TEMP+I(TEMP^2)
                    +I(RAD^2)+I(RAD^3)+NO2*TEMP+NO2*RAD+SO2*TEMP+SO2*RAD+NOX*NMVOC
                    +NMVOC*TEMP+NMVOC*RAD+NOX*TEMP+NOX*RAD+TEMP*RAD
                    -RAD, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit10_2_vsa, data2))[test])^2 ) 

# 12-component models
glm.fit10_3 = glm(O3~I(NO2^3)+NOX+I(NOX^2)+I(NOX^3)+I(TEMP^3)+NO2*TEMP+NO2*RAD
                 +SO2*NMVOC+SO2*TEMP+NMVOC*RAD+NOX*TEMP+NOX*RAD
                 -NO2-I(NO2^2)-TEMP-I(TEMP^2)-NMVOC-SO2-RAD, data=data2) 
summary(glm.fit10_3) 
cv.glm(data2, glm.fit10_3, K=10)$delta[1]

lm.fit10_3_vsa = lm(O3~I(NO2^3)+NOX+I(NOX^2)+I(NOX^3)+I(TEMP^3)+NO2*TEMP+NO2*RAD
                    +SO2*NMVOC+SO2*TEMP+NMVOC*RAD+NOX*TEMP+NOX*RAD
                    -NO2-I(NO2^2)-TEMP-I(TEMP^2)-NMVOC-SO2-RAD, data=data2, subset = train)
mean( ((data2$O3 - predict(lm.fit10_3_vsa, data2))[test])^2 ) 

summary(lm.fit10_3_vsa) 








































