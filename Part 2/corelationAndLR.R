install.packages("ISLR")

library(ISLR)

df_test <- read.csv("final_sample_format.csv")

df_test$hour = factor(df_test$hour)
df_test$month = factor(df_test$month)
df_test$day = factor(df_test$day)
df_test$Day.of.Week = factor(df_test$Day.of.Week)

class(df_test$hour)

df$Conditions <- as.factor(df$Conditions)
#lm.fit.train=lm(kWh~ (Day.of.Week + Weekday + Peakhour + Humidity), data=train)
plot(log(df$kWh) ~ (df$Day.of.Week), data=df,xlim=range(0:7),ylim=range(3:8))

plot(df$kWh ~ df$Dew_PointF, data=df,xlim=range(20:70),ylim=range(0:600))
#Constant

plot(df_test$kWh ~ df_test$hour, data=df_test,xlim=range(0:24),ylim=range(0:600))
#Exponential

plot(df_test$kWh ~ df_test$month, data=df_test,xlim=range(0:13),ylim=range(80:400))
#Constant


plot(df_test$kWh ~ df_test$day, data=df_test,xlim=range(0:32),ylim=range(0:600))
#Constant


plot(df_test$kWh ~ df_test$Day.of.Week, data=df_test,xlim=range(0:7),ylim=range(0:600))
#Constant


plot(df_test$kWh ~ df_test$Weekday, data=df_test,xlim=range(0:7),ylim=range(0:600))
#Linear

plot(df$Peakhour ~ df$kWh, data=df,xlim=range(0:600),ylim=range(0:2))
#Linear

plot(factor(df_test$WindDirDegrees),df_test$kWh)
plot(df_test$kWh*df_test$kWh,df_test$TemperatureF)

plot(df_test$kWh ~ df_test$TemperatureF, data=df_test)
#Almost constant

?plot
plot(df$kWh ~ df$Humidity, data=df,xlim=range(25:100),ylim=range(0:600))


plot(df$Peakhour ~ df$Weekday, data=df,xlim=range(0:2),ylim=range(0:2))

plot(df$kWh ~ df$Sea_Level_PressureIn)

plot(df$kWh ~ df$Humidity, data=df,xlim=range(0:7),ylim=range(0:600))

plot(df$kWh ~ df$VisibilityMPH, data=df,xlim=range(0:7),ylim=range(0:600))

plot(df$kWh ~ df$Wind_SpeedMPH, data=df,xlim=range(0:7),ylim=range(0:600))

plot(df$kWh ~ df$WindDirDegrees, data=df,xlim=range(0:7),ylim=range(0:600))





plot()

x = "month + Day.of.Week + Weekday + Peakhour + TemperatureF + Dew_PointF + Humidity"

lm.fit=lm(kWh~month + Day.of.Week + Weekday + Peakhour + TemperatureF + Dew_PointF + Humidity, data=df)
#lm.fit=lm(kWh~.-Date, data=df)
summary(lm.fit)
