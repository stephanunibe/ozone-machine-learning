# Author: Stephan Raess
# Date: June 19, 2024

# (1) Libraries ----------------------------------------------------------------

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

# Modified import function for data recorded at the stations in Zuerich (ZUE) and 
# Duebendorf (DUE); for these stations CPC data is not available
readData_ZUE_DUE <- function(filePath) {
  
  # Read CSV file and remove the first 5 rows
  df <- read_delim(filePath, delim = ";", skip = 5)
  
  # Modify column names
  names(df) <- as.character(unlist(df[1, ]))
  df <- df[-1, ]
  names(df) <- names(df) <- c('DATE','O3','NO2','SO2','CO','PM10','PM2_5','EC',
                              'NMVOC','NOX','TEMP','PREC','RAD')
  # Data type conversion
  df$O3 <- as.numeric(df$O3)
  df$NO2 <- as.numeric(df$NO2)
  df$SO2 <- as.numeric(df$SO2)
  df$CO <- as.numeric(df$CO)
  df$PM10 <- as.numeric(df$PM10)
  df$PM2_5 <- as.numeric(df$PM2_5)
  df$EC <- as.numeric(df$EC)
  #df$CPC <- as.numeric(df$CPC)
  df$NMVOC <- as.numeric(df$NMVOC)
  df$NOX <- as.numeric(df$NOX)
  df$TEMP <- as.numeric(df$TEMP)
  df$PREC <- as.numeric(df$PREC)
  df$RAD <- as.numeric(df$RAD)
  
  return(df)
}

# Load training data
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
    dfAll <- rbind(dfAll, df)
  }
  
  count <- count + 1
  
}
dfAll_fit <- dfAll

# Load data for testing
filePath <- "data/allStations2023/ZUE_2023.csv"
cat("Processing ", filePath, "\n")
dfAll_test <- readData_ZUE_DUE(filePath)
  
# (3) Pre-process data ---------------------------------------------------------

data <- na.omit(dfAll_fit) # remove missing values
data <- data[,2:14] # remove date column (contains non-numeric values)

dataTest <- na.omit(dfAll_test) # remove missing values
#dataTest <- dataTest[,2:14] # remove date column (contains non-numeric values)
dataTest <- dataTest[,2:13] # remove date column (for ZUE and DUE)

# Separate target and predictors
predictors <- dataTest[, -which(names(dataTest) %in% "O3")]
target <- dataTest[["O3"]] 

# (4) Model evaluation ---------------------------------------------------------

# Model 1
glmFit1 <- glm(O3~RAD, data=data) 
lmFit1 <- lm(O3~RAD, data=data) # (for F-statistic)
summary(glmFit1)
confint(glmFit1)
coefficients(glmFit1)

predictedO3_1 <- predict(glmFit1, dataTest, type = "response")
residuals1 <- target - predictedO3_1
MSE1 <- mean(residuals1^2)
MAE1 <- mean(abs(residuals1))
MEAN1 <- mean(predictedO3_1)

# Model 2
lmFit2 <- lm(O3~poly(RAD,3), data=data)
summary(lmFit2)
confint(lmFit2)
coefficients(lmFit2)

predictedO3_2 <- predict(lmFit2, dataTest, type = "response")
residuals2 <- target - predictedO3_2
MSE2 <- mean(residuals2^2)
MAE2 <- mean(abs(residuals2))
MEAN2 <- mean(predictedO3_2)

# Model 3
glmFit3 <- glm(O3~NO2+SO2+NMVOC+NOX+TEMP+RAD, data=data) 
summary(glmFit3)
confint(glmFit3)
coefficients(glmFit3)

predictedO3_3 <- predict(glmFit3, dataTest, type = "response")
residuals3 <- target - predictedO3_3
MSE3 <- mean(residuals3^2)
MAE3 <- mean(abs(residuals3))
MEAN3 <- mean(predictedO3_3)

# Model 4
glmFit4 <- glm(O3~I(NO2^3)+NOX+I(NOX^2)+I(NOX^3)+I(TEMP^3)+NO2*TEMP+NO2*RAD
              +SO2*NMVOC+SO2*TEMP+NMVOC*RAD+NOX*TEMP+NOX*RAD
              -NO2-I(NO2^2)-TEMP-I(TEMP^2)-NMVOC-SO2-RAD, data=data) 
summary(glmFit4)
confint(glmFit4)
coefficients(glmFit4)

predictedO3_4 <- predict(glmFit4, dataTest, type = "response")
residuals4 <- target - predictedO3_4
MSE4 <- mean(residuals4^2)
MAE4 <- mean(abs(residuals4))
MEAN4 <- mean(predictedO3_4)

# (5) Bootstrap (Training data) ------------------------------------------------

bootFunc <- function(data, index ){
  return (coef(glm(O3~NO2+SO2+NMVOC+NOX+TEMP+RAD, 
                   data=data, subset=index)))
}

set.seed(1)
# Bootstrap  
bootFunc(data, sample(nrow(data),nrow(data),replace =T))
boot(data,bootFunc,1000) 

# Standard errors of regression coefficients (computed using standard formulas)
summary(glmFit4)$coef




