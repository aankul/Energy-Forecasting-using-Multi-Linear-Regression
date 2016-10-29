library(ISLR)
library(forecast)

#Read the csv file
forecastData <- read.csv("forecastData.csv")


#Select the variables from the feature selection by Part 2
selectedData <- data.frame(forecastData$Time,forecastData$TemperatureF,forecastData$Humidity)

#Derive the required variables as shown in part 1
selectedData$Date <- format(as.Date(substr(selectedData$forecastData.Time,1,10),format = "%Y-%m-%d"),"%m/%d/%Y")
selectedData$hour <- as.numeric(substr(selectedData$forecastData.Time,12,13))
selectedData$"Day of Week" = wday(as.Date(x = selectedData$Date,'%m/%d/%Y')) - 1
selectedData$Weekday <- ifelse(selectedData$"Day of Week" == 0,0,ifelse(selectedData$"Day of Week" == 6,0,1))

Peakhour <- function(x){
  if(x>7 && x<=19)
    return(1)
  else
    return(0)
}

selectedData$Peakhour <- sapply(as.numeric(selectedData$hour), Peakhour)


#Add all variables to a dataframe
dffinal <-  data.frame(selectedData$Date,selectedData$hour,selectedData$Weekday,selectedData$Peakhour,selectedData$forecastData.TemperatureF,selectedData$forecastData.Humidity)


#Change the name of the variables
dffinal$Date <- dffinal$selectedData.Date
dffinal$hour <- dffinal$selectedData.hour
dffinal$Weekday <- dffinal$selectedData.Weekday
dffinal$Peakhour <- dffinal$selectedData.Peakhour
dffinal$TemperatureF <- dffinal$selectedData.forecastData.TemperatureF
dffinal$Humidity <- dffinal$selectedData.forecastData.Humidity

#Drop the variables
dffinal$selectedData.Peakhour <- NULL
dffinal$selectedData.Date <- NULL
dffinal$selectedData.hour <- NULL
dffinal$selectedData.Weekday <- NULL
dffinal$selectedData.forecastData.TemperatureF <- NULL
dffinal$selectedData.forecastData.Humidity <- NULL

#Group the rows to remove duplicates (Selects the last row)
groupedWData = sqldf("SELECT * FROM dffinal GROUP BY Date,hour")

#Read data from school
completeData <- read.csv("final_sample_format.csv")

#Convert hour as factors for model to identify
groupedWData$hour <- factor(groupedWData$hour)
completeData$hour <- factor(completeData$hour)


#Run linear Regression with the selected variables in part 2
lm.fit=lm(kWh~ (Weekday + Peakhour + sqrt(TemperatureF) + sqrt(Humidity)), data=completeData)

#Get the predicted kWh
predicted.kwh = predict(lm.fit, groupedWData)

#Add the predicted value to the dataframe
predictedDf <- data.frame(groupedWData,predicted.kwh)

write.csv(predictedDf,file="forecastOutput.csv",row.names=FALSE)


