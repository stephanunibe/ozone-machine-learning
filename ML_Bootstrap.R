# Author: Stephan Raess
# Date: June 19, 2024

# (1) Libraries ----------------------------------------------------------------

library(keras)
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

data <- na.omit(dfAll) # remove missing values
data2 <- data[,2:14] # remove date column (contains non-numeric values)

# Separate target and predictors
predictors = data2[, -which(names(data2) %in% "O3")] 
target = data2$O3 

# Split data into a training and a test set
set.seed(1)
trainingPercent = 0.8
indices <- sample(1:nrow(data), size = trainingPercent*nrow(data2))
xTrain <- predictors[indices,]
yTrain <- target[indices]
xTest <- predictors[-indices,]
yTest <- target[-indices]

# Normalize data
meanTrain <- apply(xTrain,2,mean)
sdTrain <- apply(xTrain,2,sd)
xTest <- scale(xTest,center=meanTrain,scale=sdTrain)
xTrain <- scale(xTrain)

# Define model
model = keras_model_sequential() %>%
  layer_dense(units=2, activation='relu', input_shape=ncol(xTrain)) %>%
  layer_dense(units=3, activation='relu') %>%
  layer_dense(units=1)

# Compile model
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = 'mse',
  metrics = c('mean_squared_error')
)

summary(model)

# Number of bootstrap samples
nSamples <- 10

# Vector for bootstrap samples
bsWeightsBiases <- vector("list", nSamples)

# Generate bootstrap samples and train the model
for(i in 1:nSamples){
  
  # Sampling with replacement 
  bsSample <- sample(1:nrow(xTrain), replace = TRUE)
  bsX <- xTrain[bsSample, ]
  bsY <- yTrain[bsSample]
  
  # Train the model on the bootstrap sample
  model %>% fit(bsX, bsY, epochs = 10, batchSize = 32, verbose = 0)
  
  # Store the weights
  bsWeightsBiases[[i]] <- get_weights(model) # model %>% get_weights() 
}

# The function get_weights() returns arrays containing the weights and biases for 
# the model's layers. The return value's structure is
# 1. weights for 1st layer
# 2. biases for 1st layer
# 3. weights for 2nd layer
# 4. biases for 2nd layer 
# and so on...

# The variable "wbIndex" determines which of the aforementioned outputs should 
# be used for the calculation of the standard deviations
wbIndex = 1 # "1" means weights for 1st layer, "2" means baises for 1st layer etc.

# Determine the dimensions of the array in which the weights/biases are stored
len1 <- nSamples # number of samples
len2 <- dim(bsWeightsBiases[[1]][[wbIndex]])[1] # number of units of previous layer
len3 <- dim(bsWeightsBiases[[1]][[wbIndex]])[2] # number of units in evaluated layer

valueArr <- array(dim = c(len1, len2))
if(!is.na(len3)){
  valueArr <- array(dim = c(len1, len2, len3))
}

# Extract the weights/biases and store them in valueArr
for(i in 1:len1){
  
  weightsBiases <- bsWeightsBiases[[i]]
  coeffs <- weightsBiases[[wbIndex]] 
  
  for(j in 1:len2){
    
    if(is.na(len3)){
      valueArr[i,j] <- coeffs[j]
    }
    else{
      for(k in 1:len3){
        valueArr[i,j,k] <- coeffs[j,k]
      }
    }

  }
}

# Iterate over the different weights/biases and calculate the standard deviation 
# of the samples 
sdArr <- array(dim = c(len2))
if(!is.na(len3)){
  sdArr <- array(dim = c(len2,len3))
}

for(j in 1:len2){
  
  if(is.na(len3)){
    sdArr[j] <- sd(valueArr[,j])
  }
  else{
    for(k in 1:len3){
      sdArr[j,k] <- sd(valueArr[,j,k]) 
    }
  }

}

sdArr # show array containing the standard deviations
