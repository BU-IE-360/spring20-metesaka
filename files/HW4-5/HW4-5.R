# Loading Packages

library(data.table)
library(stats)
library(dplyr)
library(forecast)


# Importing data as data. table

traffic_data <- fread("Metro_Interstate_Traffic_Volume.csv")
str(traffic_data)

# Manipulating data in order to start our analysis

traffic_data[,c('date','time'):= tstrsplit(date_time," ") ]
traffic_data[,c('year','month','day'):=tstrsplit(date,'-')]
traffic_data[,c('hour','min','sec'):= tstrsplit(time,':')]
traffic_data$min=NULL
traffic_data$sec=NULL
traffic_data$time=NULL
traffic_data$date=NULL

traffic_data$hour=as.numeric(traffic_data$hour)
traffic_data$day=as.numeric(traffic_data$day)
traffic_data$month=as.numeric(traffic_data$month)
traffic_data$year=as.numeric(traffic_data$year)

traffic_data[,weather_main:=as.factor(weather_main)]
traffic_data[,weather_description:=as.factor(weather_description)]
traffic_data[,holiday:=as.factor(holiday)]
traffic_data[,date_time := as.Date(date_time, format = "%Y-%m-%d %H:%M:%S")]

traffic_data[,weekday := weekdays(date_time)]
traffic_data[,week := week(date_time)]


# Dividing the data into Train and Test Data

train_data_traffic <- traffic_data[as.numeric(format(date_time,"%Y")) <= 2017]
test_data_traffic <- traffic_data[as.numeric(format(date_time,"%Y")) == 2018]

# Creating daily, weekly and monthly mean datasets

traffic_data_daily <- train_data_traffic[,list(daily_mean = mean(traffic_volume)), by = list(year,month,day)]
traffic_data_weekly <- train_data_traffic[,list(weekly_mean = mean(traffic_volume)), by = list(year,month,week)]
traffic_data_monthly <- train_data_traffic[,list(monthly_mean = mean(traffic_volume)), by = list(year,month)]


# Linear Regression Analysis


train_data_traffic[,month:=as.factor(month)]
train_data_traffic[,day:=as.factor(day)]
train_data_traffic[,hour:=as.factor(hour)]
train_data_traffic[,week:=as.factor(week)]

# Initial Model
# In our initial model I will use every meaningful attribute to see their effects on the model. 


model1 <-  lm(traffic_volume~.-date_time,train_data_traffic)
summary(model1)$r.squared
AIC(model1)

# Second Model
# I will eliminate insignificant attribute "holiday" from the model.

model2 <-  lm(traffic_volume~.-date_time-holiday,train_data_traffic)
summary(model2)
AIC(model2)

# Third Model
# I eliminated insignificant attribute "rain_1h" from the model.

model3 <-  lm(traffic_volume~.-date_time-holiday-rain_1h,train_data_traffic)

summary(model3)
AIC(model3)

# Fourth Model
# I eliminated insignificant attribute "month" from the model.

model4 <-  lm(traffic_volume~.-date_time-holiday-rain_1h-month,train_data_traffic)

summary(model4)
AIC(model4)

# Fifth Model
# I eliminated insignificant attribute "year" from the model.


model5 <-  lm(traffic_volume~.-date_time-holiday-rain_1h-year-month,train_data_traffic)

summary(model5)
AIC(model5)

#I create dummy varibles on these situation and eliminate the weahter_main attribute.



train_data_traffic[,is_Rainy := {
  if(weather_main == "Rain")1
  else 0
},by=weather_main]
train_data_traffic[,is_Snow := {
  if(weather_main == "Snow")1
  else 0
},by=weather_main]
train_data_traffic[,is_Mist := {
  if(weather_main == "Mist")1
  else 0
},by=weather_main]
train_data_traffic[,is_Cloudy := {
  if(weather_main == "Clouds")1
  else 0
},by=weather_main]


train_data_traffic[,is_Rainy:=as.factor(is_Rainy)]
train_data_traffic[,is_Snow:=as.factor(is_Snow)]
train_data_traffic[,is_Mist:=as.factor(is_Mist)]
train_data_traffic[,is_Cloudy:=as.factor(is_Cloudy)]


# Sixth Model

model6 <-  lm(traffic_volume~.-date_time-holiday-rain_1h-year-month-weather_main
              -week-weather_description,train_data_traffic)
summary(model6)
AIC(model6)

# Seventh Model
# I eliminated insignificant attribute "day" from the model.



model7 <-  lm(traffic_volume~.-date_time-holiday-rain_1h-year-month-weather_main
              -week-weather_description-day,train_data_traffic)

summary(model7)
AIC(model7)

# Eighth Model
# I eliminated insignificant attribute "snow1_h" from the model.

model8 <-  lm(traffic_volume~.-date_time-holiday-rain_1h-year-month-weather_main
              -week-weather_description-day-snow_1h,train_data_traffic)

summary(model8)
AIC(model8)

# Nineth Model
# I eliminated insignificant attribute "is_Mist" from the model.

model9 <-  lm(traffic_volume~.-date_time-holiday-rain_1h-year-month-weather_main
              -week-weather_description-day-snow_1h-is_Mist,train_data_traffic)

summary(model9)
AIC(model9)

# Tenth Model
# I eliminated insignificant attribute "is_Rainy" from the model.

model10 <-  lm(traffic_volume~.-date_time-holiday-rain_1h-year-month-weather_main
               -week-weather_description-day-snow_1h-is_Mist-is_Rainy,train_data_traffic)

summary(model10)
AIC(model10)



# Adding lagged columns to see that whether there is an effect of previous hours on the current traffic volume. 
# I choose lag 2, 12 and 24. The reason of these numbers is 2 is a close time interval which can make me 
# able to see the effect of a close interval, 12 is represent opposite behaviors of cars like going and 
# coming back to work and 24 to see the effect of the same hour of the previous day.

train_data_traffic[,lag2:= shift(traffic_volume,2)]
train_data_traffic[,lag12:= shift(traffic_volume,12)]
train_data_traffic[,lag24:= shift(traffic_volume,24)]

# Eleventh Model
# I add the new attributes.

model11 <-  lm(traffic_volume~.-date_time-holiday-rain_1h-year-month-weather_main-week-weather_description-day-snow_1h-is_Mist-is_Rainy,train_data_traffic)

summary(model11)
AIC(model11)


#AIC value is reduced and r^2 value has increased. There is no insignificant attributes. 
#I will move on with this model. 


# ARIMA model of the data
acf(traffic_data$traffic_volume,200, main = "Auto-Correlation Graph of Traffic Volume")

acf(traffic_data_daily$daily_mean,200, main = "Auto-Correlation Graph of Traffic Volume")
acf(traffic_data_weekly$weekly_mean,200, main = "Auto-Correlation Graph of Traffic Volume")
acf(traffic_data_monthly$monthly_mean,200, main = "Auto-Correlation Graph of Traffic Volume")

# Decomposing seasonality

hourly_coefficient <- train_data_traffic[,list(hour_mean = mean(traffic_volume)), by = list(hour)]
mean_hour <- mean(hourly_coefficient$hour_mean)
hourly_coefficient[,total_mean := mean_hour]
hourly_coefficient[,coefficient := hour_mean/total_mean]

weekday_coefficient <- train_data_traffic[,list(weekday_mean = mean(traffic_volume)), by = list(weekday)]
mean_weekday <- mean(weekday_coefficient$weekday_mean)
weekday_coefficient[,total_mean := mean_weekday]
weekday_coefficient[,coefficient := weekday_mean/total_mean]

train_data_traffic[,hour_coefficient := {
  if(hour == 9) hourly_coefficient$coefficient[1]
  else if(hour == 10) hourly_coefficient$coefficient[2]
  else if(hour == 11) hourly_coefficient$coefficient[3]
  else if(hour == 12) hourly_coefficient$coefficient[4]
  else if(hour == 13) hourly_coefficient$coefficient[5]
  else if(hour == 14) hourly_coefficient$coefficient[6]
  else if(hour == 15) hourly_coefficient$coefficient[7]
  else if(hour == 16) hourly_coefficient$coefficient[8]
  else if(hour == 17) hourly_coefficient$coefficient[9]
  else if(hour == 18) hourly_coefficient$coefficient[10]
  else if(hour == 19) hourly_coefficient$coefficient[11]
  else if(hour == 20) hourly_coefficient$coefficient[12]
  else if(hour == 21) hourly_coefficient$coefficient[13]
  else if(hour == 22) hourly_coefficient$coefficient[14]
  else if(hour == 23) hourly_coefficient$coefficient[15]
  else if(hour == 00) hourly_coefficient$coefficient[16]
  else if(hour == 1) hourly_coefficient$coefficient[17]
  else if(hour == 2) hourly_coefficient$coefficient[18]
  else if(hour == 3) hourly_coefficient$coefficient[19]
  else if(hour == 4) hourly_coefficient$coefficient[20]
  else if(hour == 5) hourly_coefficient$coefficient[21]
  else if(hour == 6) hourly_coefficient$coefficient[22]
  else if(hour == 7) hourly_coefficient$coefficient[23]
  else if(hour == 8) hourly_coefficient$coefficient[24]
}
, by = hour]

train_data_traffic[,weekday_coefficients := {
  if(weekday == "Salı") weekday_coefficient$coefficient[1]
  else if(weekday == "Çarşamba")  weekday_coefficient$coefficient[2]
  else if(weekday == "Perşembe")  weekday_coefficient$coefficient[3]
  else if(weekday == "Cuma")      weekday_coefficient$coefficient[4]
  else if(weekday == "Cumartesi") weekday_coefficient$coefficient[5]
  else if(weekday == "Pazar")     weekday_coefficient$coefficient[6]
  else if(weekday == "Pazartesi") weekday_coefficient$coefficient[7]
  
  
}
, by = weekday]

train_data_traffic[,deseasonalized := traffic_volume/weekday_coefficients/hour_coefficient]

acf(train_data_traffic$deseasonalized,200)

#I checked these models and used model 5 and 6

model1 <- arima(train_data_traffic$deseasonalized, c(1,1,1))
checkresiduals(model1)
AIC(model1)

model2 <- arima(train_data_traffic$deseasonalized, c(2,1,1))
checkresiduals(model2)
AIC(model2)

model3 <- arima(train_data_traffic$deseasonalized, c(2,1,2))
checkresiduals(model3)
AIC(model3)

model4 <- arima(train_data_traffic$deseasonalized, c(3,1,2))
checkresiduals(model4)
AIC(model4)

model5 <- arima(train_data_traffic$deseasonalized, c(3,1,3))
checkresiduals(model5)
AIC(model5)

model6 <- auto.arima(train_data_traffic$deseasonalized)
checkresiduals(model6)
AIC(model6)


# Validation of the models

## Syncronization of the test data with the train data

test_data_traffic[,hour_coefficient := {
  if(hour == 9) hourly_coefficient$coefficient[1]
  else if(hour == 10) hourly_coefficient$coefficient[2]
  else if(hour == 11) hourly_coefficient$coefficient[3]
  else if(hour == 12) hourly_coefficient$coefficient[4]
  else if(hour == 13) hourly_coefficient$coefficient[5]
  else if(hour == 14) hourly_coefficient$coefficient[6]
  else if(hour == 15) hourly_coefficient$coefficient[7]
  else if(hour == 16) hourly_coefficient$coefficient[8]
  else if(hour == 17) hourly_coefficient$coefficient[9]
  else if(hour == 18) hourly_coefficient$coefficient[10]
  else if(hour == 19) hourly_coefficient$coefficient[11]
  else if(hour == 20) hourly_coefficient$coefficient[12]
  else if(hour == 21) hourly_coefficient$coefficient[13]
  else if(hour == 22) hourly_coefficient$coefficient[14]
  else if(hour == 23) hourly_coefficient$coefficient[15]
  else if(hour == 00) hourly_coefficient$coefficient[16]
  else if(hour == 1) hourly_coefficient$coefficient[17]
  else if(hour == 2) hourly_coefficient$coefficient[18]
  else if(hour == 3) hourly_coefficient$coefficient[19]
  else if(hour == 4) hourly_coefficient$coefficient[20]
  else if(hour == 5) hourly_coefficient$coefficient[21]
  else if(hour == 6) hourly_coefficient$coefficient[22]
  else if(hour == 7) hourly_coefficient$coefficient[23]
  else if(hour == 8) hourly_coefficient$coefficient[24]
}
, by = hour]

test_data_traffic[,weekday_coefficients := {
  if(weekday == "Salı") weekday_coefficient$coefficient[1]
  else if(weekday == "Çarşamba")  weekday_coefficient$coefficient[2]
  else if(weekday == "Perşembe")  weekday_coefficient$coefficient[3]
  else if(weekday == "Cuma")      weekday_coefficient$coefficient[4]
  else if(weekday == "Cumartesi") weekday_coefficient$coefficient[5]
  else if(weekday == "Pazar")     weekday_coefficient$coefficient[6]
  else if(weekday == "Pazartesi") weekday_coefficient$coefficient[7]
  
  
}
, by = weekday]


test_data_traffic[,is_Rainy := {
  if(weather_main == "Rain")1
  else 0
},by=weather_main]
test_data_traffic[,is_Snow := {
  if(weather_main == "Snow")1
  else 0
},by=weather_main]
test_data_traffic[,is_Mist := {
  if(weather_main == "Mist")1
  else 0
},by=weather_main]
test_data_traffic[,is_Cloudy := {
  if(weather_main == "Clouds")1
  else 0
},by=weather_main]


test_data_traffic[,is_Rainy:=as.factor(is_Rainy)]
test_data_traffic[,is_Snow:=as.factor(is_Snow)]
test_data_traffic[,is_Mist:=as.factor(is_Mist)]
test_data_traffic[,is_Cloudy:=as.factor(is_Cloudy)]

test_data_traffic[,month:=as.factor(month)]
test_data_traffic[,day:=as.factor(day)]
test_data_traffic[,hour:=as.factor(hour)]
test_data_traffic[,week:=as.factor(week)]

test_data_traffic[,lag2:= shift(traffic_volume,2)]
test_data_traffic[,lag12:= shift(traffic_volume,12)]
test_data_traffic[,lag24:= shift(traffic_volume,24)]


#Predictions
Lm_predictions <-  predict(model11, test_data_traffic)
arima_predictions1 <- forecast(model1,7949)
arima_predictions2 <- forecast(model2,7949)

#placing the values
test_data_traffic[,"LM_model":=Lm_predictions*hour_coefficient*weekday_coefficients]
test_data_traffic[,"arima_model1":=arima_predictions1$mean*hour_coefficient*weekday_coefficients]
test_data_traffic[,"arima_model2":=arima_predictions2$mean*hour_coefficient*weekday_coefficients]

#Calculating mean absolue error
MAE_lm <- mean(abs(test_data_traffic$traffic_volume-test_data_traffic$LM_model),na.rm=T)
MAE_arima1 <- mean(abs(test_data_traffic$traffic_volume-test_data_traffic$arima_model1),na.rm=T)
MAE_arima2 <- mean(abs(test_data_traffic$traffic_volume-test_data_traffic$arima_model2),na.rm=T)

