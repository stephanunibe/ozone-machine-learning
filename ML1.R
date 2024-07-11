# Author: Stephan Raess
# Date: June 19, 2024

# (1) Libraries ----------------------------------------------------------------

library(utils)
library(plotly)
library(readr)
library(ggplot2)

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

data <- dfAll;

# (3) Inspect data -------------------------------------------------------------

head(data) 

# Remove missing values
sum(is.na(data)) # count number of rows with missing values
data = na.omit(data) # remove rows with missing values

# Remove columns with non-numeric values
data2 = data[,2:14] # remove first column (date)

# Compute correlations
correl = cor(data2) # calculate correlations
print(correl)
R2 = cor(data2)^2 # calculate R^2 value
pairs(data2) # plot pairs

# Plot pairs with highest R^2 values
plot(data2$NO2, data2$O3, xlab="NO2", ylab="O3")
plot(data2$NOX, data2$O3, xlab="NOX", ylab="O3")
plot(data2$RAD, data2$O3, xlab="RAD", ylab="O3")
plot(data2$TEMP, data2$O3, xlab="TEMP", ylab="O3")
plot(data2$EC, data2$O3, xlab="EC", ylab="O3")
plot(data2$CPC, data2$O3, xlab="CPC", ylab="O3")
plot(data2$CO, data2$O3, xlab="CO", ylab="O3")
plot(data2$SO2, data2$O3, xlab="SO2", ylab="O3")
plot(data2$NMVOC, data2$O3, xlab="NMVOC", ylab="O3")

# Plot ozone as a function of time
fig <- plot_ly(x = as.Date(dfAll$DATE,"%d.%m.%Y"), y = dfAll$O3, type = 'scatter', mode = 'markers',
               marker = list(symbol = 'circle', size = 12, opacity = 0.5, color='black'))

fig <- fig %>% layout(
  xaxis = list(
    title = 'Year',
    titlefont = list(size = 35), 
    tickfont = list(size = 35)   
  ),
  yaxis = list(
    title = 'Ozone concentration [µg/m³]',
    titlefont = list(size = 35), 
    tickfont = list(size = 35)   
  ),
  title = '',
  resolution(300)
)

fig # show figure

# Compute mean and variance 
apply(data2,2,mean) # mean 
apply(data2,2,var) # variance 





