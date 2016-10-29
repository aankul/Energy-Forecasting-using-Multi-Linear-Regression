install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("reshape")
install.packages("lubridate")
install.packages("weatherData")
install.packages("sqldf")
install.packages("tcltk")
install.packages("dummy")
install.packages("zoo")



library("sqldf")
library("dplyr")
library("reshape")
library("stringr")
library("lubridate")
library("weatherData")
library("tidyr")
library("tcltk")
library("dummy")
library("zoo")

setwd("C:/Users/amuly/Desktop/Fall 2016/ADS/Assignment 2")
#Reading CSV into dataframe
df <- read.csv("rawData1.csv")

#Filtering the unwanted values
df_tbl <- df %>% tbl_df %>% filter (Channel == 'MILDRED SCHOOL 1')
#View(df_tbl)

#Reshaping the structure for calculations
mdata <- melt(as.data.frame(df_tbl), id=c("Account","Date","Channel","Units")) %>% tbl_df
#View(mdata)

#Removing "X" character for calculations
mdata$timeInterval = gsub("X","",mdata$variable)

#Dropping the variable column
mdata$variable <- NULL

#Labelling the hour
mdata$hour <- cut(as.numeric(mdata$timeInterval), seq(0,24,1),right=TRUE,label=seq(0,23,1))
#View(mdata)

#Finding the sum per hour
aggregatedData <- mdata %>% group_by(Account, Date, hour) %>% summarise(kWh = sum(value))

#Splitting the Date field and converting it into Data frame
a <- strsplit(as.character(aggregatedData$Date), "/")
mat <- matrix(unlist(a), ncol=3, byrow=TRUE)
df   <- as.data.frame(mat)


#Setting the values of month,day, year and Day of the week
aggregatedData$month = df$V1
aggregatedData$day = df$V2
aggregatedData$year = df$V3
aggregatedData$"Day of Week" = wday(as.Date(aggregatedData$Date,'%m/%d/%Y')) - 1
aggregatedData$Weekday <- ifelse(aggregatedData$"Day of Week" == 0,0,ifelse(aggregatedData$"Day of Week" == 6,0,1))

#Function to calculate the Peakhour
Peakhour <- function(x){
  if(x>7 && x<=19)
    return(1)
  else
    return(0)
}

#Applying the value to the function
aggregatedData$Peakhour <- sapply(as.numeric(aggregatedData$hour), Peakhour)

char_date <- as.character(aggregatedData$Date)
aggregatedData$Date <- format(strptime(char_date,"%m/%d/%Y"),"%m/%d/%Y")

#View the data as a table
#View(aggregatedData)


#Get distinct Date
distinctDate <- distinct(aggregatedData)

#Get latest and earliest date
a = distinctDate[order(as.Date(distinctDate$Date, format="%m/%d/%Y")),] %>% select(Date)
startDate = format(as.Date(head(a$Date,1), format="%m/%d/%Y"),"%Y-%m-%d")
endDate = format(as.Date(tail(a$Date,1), format="%m/%d/%Y"),"%Y-%m-%d")

#Creating a dummy DataFrame
d3 <- getWeatherForDate("BOS", start_date=startDate,
                          end_date = endDate,
                          opt_detailed = TRUE,
                          opt_all_columns = TRUE) 

#Change to EST timezone
d3$Time <- as.character(d3$Time)
d3$Time <- as.POSIXct(d3$Time,tz="CET")
attributes(d3$Time)$tzone <- "America/New_York"

#Get Date
#d3$Date <- sub("0?(.+)/0?(.+)/????", "\\3\\1/\\3", format(as.Date(substr(d3$Time,1,10),format = "%Y-%m-%d"),"%m/%d/%Y"))

#Get Date
d3$Date <- format(as.Date(substr(d3$Time,1,10),format = "%Y-%m-%d"),"%m/%d/%Y")

#Get hour
d3$hour <- as.numeric(substr(d3$Time,12,13))

#Select Required Column
weatherData <- select(d3,Date,hour,TemperatureF,Dew_PointF,Humidity,Sea_Level_PressureIn,
                      VisibilityMPH,Wind_SpeedMPH,Conditions,WindDirDegrees)

#Group Data
groupedWData = sqldf("SELECT * FROM weatherData GROUP BY Date,hour")

#Left outer Join
processedOp <- merge(aggregatedData, weatherData, by=c("Date","hour"),all.x = TRUE)

#Convert 
processedOp$Wind_SpeedMPH[processedOp$Wind_SpeedMPH=="Calm"] <- 0 

#Convert character column to numeric
processedOp$Wind_SpeedMPH <- as.numeric(processedOp$Wind_SpeedMPH)

#Remove the empty 
processedOp$Conditions[processedOp$Conditions==""] <- NA

cond <- data.frame(processedOp$Date,processedOp$Conditions)
class(cond)
View(cond)

Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

cond %>% group_by(cond$processedOp.Date) %>% na.(mode = Mode(cond$processedOp.Conditions))


#Remove the outliers using Boxplot
processedOp$TemperatureF[processedOp$TemperatureF %in% boxplot.stats(processedOp$TemperatureF)$out] <- NA
processedOp$Dew_PointF[processedOp$Dew_PointF %in% boxplot.stats(processedOp$Dew_PointF)$out] <- NA
processedOp$Humidity[processedOp$Humidity %in% boxplot.stats(processedOp$Humidity)$out] <- NA
processedOp$Sea_Level_PressureIn[processedOp$Sea_Level_PressureIn %in% boxplot.stats(processedOp$Sea_Level_PressureIn)$out] <- NA
processedOp$VisibilityMPH[processedOp$VisibilityMPH %in% boxplot.stats(processedOp$VisibilityMPH)$out] <- NA
processedOp$Wind_SpeedMPH[processedOp$Wind_SpeedMPH %in% boxplot.stats(processedOp$Wind_SpeedMPH)$out] <- NA
processedOp$WindDirDegrees[processedOp$WindDirDegrees %in% boxplot.stats(processedOp$WindDirDegrees)$out] <- NA


#Replacing NA values with linear interpolation
processedOp$TemperatureF <- na.approx(processedOp$TemperatureF,na.rm = FALSE) 
processedOp$Dew_PointF <- na.approx(processedOp$Dew_PointF,na.rm = FALSE) 
processedOp$Humidity <- na.approx(processedOp$Humidity,na.rm = FALSE) 
processedOp$Sea_Level_PressureIn <- na.approx(processedOp$Sea_Level_PressureIn,na.rm = FALSE)
processedOp$VisibilityMPH <- na.approx(processedOp$VisibilityMPH,na.rm = FALSE) 
processedOp$Wind_SpeedMPH <- na.approx(processedOp$Wind_SpeedMPH,na.rm = FALSE)
processedOp$WindDirDegrees <- na.approx(processedOp$WindDirDegrees,na.rm = FALSE) 

#processedOp$Conditions <- factor(processedOp$Conditions)

 



View(processedOp)



#write.csv(processedOp,file="sample_format.csv")





