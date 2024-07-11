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

data = na.omit(dfAll) # remove missing values
data2 = data[,2:14] # remove columns containing non-numeric values

predictors = data2[, -which(names(data2) %in% "O3")]
predictors = scale(predictors) # normalization of predictors

target = data2[["O3"]] # target values

# Split data into a training (80%) and a test set (20%)
trainingPercent = 0.8
indices = sample(1:nrow(data), size = trainingPercent*nrow(data2))
xTrain = predictors[indices,]
yTrain = target[indices]
xTest = predictors[-indices,]
yTest = target[-indices]

# Define model
model = keras_model_sequential() %>%
        layer_dense(units=16, activation='relu', input_shape=ncol(xTrain)) %>%
        layer_dropout(0.2) %>%
        layer_dense(units=16, activation='tanh') %>%
        layer_dropout(0.2) %>%
        layer_dense(units=1)

# Show model summary
model %>% summary() 

# Show model configuration 
model %>% get_config() 

# Compile model
model %>% compile(
  optimizer = 'rmsprop',
  loss = 'mse',
  metrics = c('mean_squared_error')
)

# Train model
modelEpochs = 10

history = model %>% fit(
  xTrain,
  yTrain,
  epochs = modelEpochs,
  batch_size = 32, 
  validation_split = 0.1
)

# Compute the model's test error 
result = model %>% evaluate(xTest, yTest)
MSE = getElement(result, 'loss') 
cat("MSE:", MSE, "\n") # print MSE

# Predict ozone concentrations using the trained model
predictions <- predict(model, xTest)
mean(predictions) # mean
sd(predictions) # std. dev.





