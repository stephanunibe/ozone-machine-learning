# Author: Stephan Raess
# Date: June 19, 2024

# (1) Libraries ----------------------------------------------------------------

library(utils)
library(leaps) 
library(glmnet) 
library(pls) 
library(readr)

# (2) Import data --------------------------------------------------------------

# Function for reading and pre-processing data 
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

# (4) Best subset selection: adjustment to training error ----------------------

regFit <- regsubsets(O3~.,data2,nvmax=12) 
regSummary <- summary(regFit)

regSummary$rsq # print R^2 values

# (4.1) Adjusted R^2 
plot(regFit, scale ="adjr2") # plot selected var. (black square = selected var.)
adjR2 <- max(regSummary$adjr2) # max. value of adjusted R^2 values
nPredAdjR2 <- which.max(regSummary$adjr2) # index of maximum adj. R^2 value (best)

# Calculate std. err. of adj. R^2 values and then shift the maximum of the 
# adj. R^2 values by 1 std. error
stdErrAdjR2 <- sqrt(var(regSummary$adjr2))/sqrt(length(regSummary$adjr2)) 
adjR2 - stdErrAdjR2 
regSummary$adjr2 # print adjusted R^2 value
coef(regFit,1) # show coefficients of model with 1 predictor
coef(regFit,nPredAdjR2) # show coefficients of best model 

# (4.2) Cp
plot(regFit, scale ="Cp") # plot selected var. 
Cp <- min(regSummary$cp) # min. Cp value 
nPredCp <- which.min(regSummary$cp) # index of minimum Cp value (best) 
stdErrCp <- sqrt(var(regSummary$cp))/sqrt(length(regSummary$cp)) 

# Calculate std. err. of Cp values and then shift the minimal Cp value by 1 std. 
# error
Cp + stdErrCp 
regSummary$cp # print Cp
coef(regFit,4) # show coefficients of model with 4 predictors
coef(regFit,nPredCp) # show coefficients of best model

# (4.3) BIC
plot(regFit, scale ="bic") # plot selected var. 
BIC <- min(regSummary$bic)
nPredBIC <- which.min(regSummary$bic) # index of minimum BIC value (best)
stdErrBIC <- sqrt(var(regSummary$bic))/sqrt(length(regSummary$bic)) 

# Calculate std. err. of BIC values and then shift the minimal BIC value by 1 std. 
# error
BIC + stdErrBIC
regSummary$bic 
coef(regFit,4) # show coefficients of model with 4 predictors
coef(regFit,nPredBIC) # show coefficients of best model 

# (5) Best subset selection: cross-validation approach -------------------------

# Create k training sets and perform best subset selection within each of these
# sets.
set.seed(1)
k <- 10 # number of training sets

# Randomly allocate each of the observations to one of the training sets 
folds <- sample(1:k, nrow(data2), replace=TRUE)

# Create a matrix for storing the cross-validation errors (init. with NA). This
# matrix has k rows (one per training set) and 12 columns (one per predictor)
cvErr <- matrix(NA, k, 12, dimnames=list(NULL,paste(1:12)))

# Create predict() function for regsubsets()
predict.regsubsets = function(obj, newData, id, ...) {
  formula <- as.formula(obj$call[[2]])
  matr <- model.matrix(formula, newData)
  coef <- coef(obj,id=id)
  xVars <- names(coef)
  matr[,xVars]%*%coef
}

# Perform cross-validation using a for-loop. In each iteration use the i-th fold 
# as the training set and the remainder as the test set; for each model size make 
# predictions using the training set and then compute MSE using the test set
for (i in 1:k){
  # Perform best subset selection in i-th fold
  bestFits <- regsubsets(O3~.,data=data2[folds!=i,], nvmax=12)

  # For each model size compute MSE and store it in matrix cvErr 
  # cvErr[i,l] contains the MSE of the best l-variable model (selected by 
  # regsubsets) of the i-th fold (test set)
  for (l in 1:12){ 
    pred <- predict(bestFits, data2[folds==i,], id=l) # make prediction
    cvErr[i,l] <- mean((data2$O3[folds==i] - pred)^2) # calculate MSE
  }
}

# Compute the average MSE of each model size by averaging over the columns of 
# the cvErr matrix
meanCvErr <- apply(cvErr,2,mean)
meanCvErr
minMSE <- min(meanCvErr) 
nPred <- which.min(meanCvErr) 
cvStdErr <- sqrt(var(meanCvErr))/sqrt(length(meanCvErr)) 
minMSE + cvStdErr 

# Perform best subset selection on the full data set 
regFull <- regsubsets(O3~., data=data2, nvmax=12)
coef(regFull,nPred)
coef(regFull,4)

# (6) Ridge regression ---------------------------------------------------------

X = model.matrix(O3~.,data2)[,-1] # predictor data (matrix)
y = data2$O3 # target data (vector)

set.seed(1)
train = sample(1:nrow(X), nrow(X)/2) # 50% of the rows for training and 50% for testing
test = (-train) # for testing select observations/rows that are not part of train
yTest = y[test]

# Create a sequence of lambda values (range of lambda values that will be tested)
lambdaValues <- 10^seq(10,-2,length=1000)
ridgeModel <- glmnet(X,y,alpha=0,lambda=lambdaValues,nFolds=10)

# Perform cross-validated ridge regression
cvRidge <- cv.glmnet(X[train,],y[train], alpha=0)
plot(cvRidge)
bestLambdaRidge <- cvRidge$lambda.min
bestLambdaRidge

# Calculate test MSE
ridgePred <- predict(ridgeModel, s=bestLambdaRidge, newx=X[test,])
mean((ridgePred - yTest)^2) # test MSE

# Fit ridge regression model on the full data set with the best lambda value
# and show predicted coefficients
predict(ridgeModel, type ="coefficients", s=bestLambdaRidge)[1:13,]

# (7) Lasso regression ---------------------------------------------------------

# Fit model
lassoModel <- glmnet(X[train,], y[train], alpha=1, lambda=lambdaValues, nFolds=10)
plot(lassoModel)

# Perform cross-validated lasso regression and compute test MSE
set.seed(1)
cvLasso <- cv.glmnet(X[train,],y[train], alpha =1)
plot(cvLasso)
bestLambdaLasso <- cvLasso$lambda.min
lassoPred <- predict(lassoModel, s=bestLambdaLasso, newx=X[test,])
mean((lassoPred - yTest)^2) # test MSE

# Fit lasso regression model on the full data set with the best lambda value
# and show predicted coefficients
predict(lassoModel, type ="coefficients", s=bestLambdaLasso)[1:13,]

# (8) Principal component regression -------------------------------------------

# Perform PCR on the training set and evaluate performance on the test set
# Standardize each predictor (scale=TRUE) and then compute 10-fold CV error
# for the different principal components
# Remark: pcr() returns the root mean squared error; to obtain MSE the errors 
# have to be squared
set.seed(1)
pcrFit <- pcr(O3~., data=data2, subset=train, scale=TRUE, validation ="CV")
summary(pcrFit)
validationplot(pcrFit, val.type = "MSEP")
pcrFit$coefficients # show coefficients of principal components

mseValues1 = c();
for(i in 1:12){
  # Compute test MSE
  pcrPred <- predict(pcrFit, X[test,], ncomp=i)
  mse <- mean((pcrPred - yTest)^2)
  mseValues1 <- append(mseValues1,mse)
}
mseValues1
min(mseValues1) # minimum MSE
mseStdErr = sd(mseValues1)/length(mseValues1) # std. err
mseStdErr + min(mseValues1)

# Perform PCR using full data set
pcrFitFull <- pcr(y~X, scale=TRUE, ncomp=11)
summary(pcrFitFull)

# (9) Principal least squares --------------------------------------------------

set.seed(1)
plsFit <- plsr(O3~., data=data2, subset=train, scale=TRUE, validation ="CV")
summary(plsFit)
validationplot(plsFit, val.type ="MSEP")

# Compute test MSE
mseValues2 = c();
for(i in 1:12){
  # Compute test MSE
  plsPred <- predict(plsFit, X[test,], ncomp=i)
  mse <- mean((plsPred-yTest)^2) 
  mseValues2 <- append(mseValues2,mse)
}
mseValues2
min(mseValues2) # minimum MSE
mseStdErr = sd(mseValues2)/length(mseValues2) # std. err
mseStdErr + min(mseValues2)

# Perform PLS using the full data set
plsFitFull <- plsr(O3~., data=data2, scale=TRUE, ncomp=9)
summary(plsFitFull)







