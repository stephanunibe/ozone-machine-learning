# Author: Stephan Raess
# Date: June 19, 2024

# (1) Libraries ----------------------------------------------------------------

library(keras)
library(readr)

# (2) Import data --------------------------------------------------------------

readData <- function(filePath) {
  
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
for (year in 2024:2024){ 
  
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
predictorData <- data2[, -which(names(data2) %in% "O3")] 
targetData <- data2$O3 

# Load model
model <- load_model_hdf5("models/FFO_6pred_1.keras")

# Show model summary
summary(model)

# Get the model's weights and biases 
weights <- model %>% get_weights()

# Get weights of first layer
firstLayerWeights <- weights[[1]]

# Scale data using the means and std. deviations of the data set with which the 
# model was trained
meanTrain <- c(2.386067e+01, 1.406707e+00, 2.930761e-01, 1.558880e+01, 
               1.118275e+01, 6.396873e-01, 1.104099e+04, 1.047907e-01, 
               3.390787e+01, 1.368709e+01, 4.246747e+00, 1.619863e+02)

sdTrain <- c(1.316772e+01, 1.237071e+00, 1.118617e-01, 1.099585e+01, 9.418593e+00,
             5.013235e-01, 5.675089e+03, 5.719868e-02, 2.687202e+01, 6.927770e+00,
             1.222513e+01, 9.976300e+01)
predictorData <- scale(predictorData, center = meanTrain, scale = sdTrain)

# Make predictions
predictions <- model %>% predict(predictorData)

# Compute mean squared error
mse <- mean((targetData - predictions)^2)
mean(predictions)

# Compute mean absolute error
mae <- mean(abs(targetData - predictions))

