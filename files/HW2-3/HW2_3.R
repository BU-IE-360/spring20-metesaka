setwd("/Users/umsaka/Desktop/360_HW2-3")

# loading required packages

library(data.table)
library(zoo)
library(xts)
library(ggplot2)
library(stats)
library(dplyr)
library(forecast)

# Importing data and converting into data.table and renaming columns

data_elec <- data.table(read.csv("data_electricity.csv"))
names(data_elec) <- c("Date", "Time" , "Consumption_MWh")

# Adding Day, Month, Year, Weekday columns
data_elec$Date <- as.character(data_elec$Date)
data_elec[,c("day","month","year"):=tstrsplit(Date, "[.]")]
data_elec$day <- as.integer(data_elec$day)
data_elec$month <- as.integer(data_elec$month)
data_elec$year <- as.integer(data_elec$year)
data_elec[,"Day_of_the_week" := format(as.Date(data_elec$Date,format = "%d.%m.%Y"),format = "%A")]



# Converting consumption amount into numeric class
data_elec$Consumption_MWh <- as.character(data_elec$Consumption_MWh)
temp1 <- c()
for(i in 1:length(data_elec$Consumption_MWh)){
  temp2 <- 1000*as.numeric(paste(unlist(strsplit(data_elec$Consumption_MWh[i],",")),collapse = ""))
  temp1 <- c(temp1,temp2)
}
data_elec$Consumption_MWh <- temp1


#Adding week column
temp3 <- seq(0,0,length.out = length(data_elec$Consumption_MWh))
data_elec[,"Week" := temp3]
temp4 <- 1

for(i in 1:length(data_elec$Consumption_MWh) ){
  if(data_elec$month[i] == 1 & data_elec$Time[i] == "00:00" & data_elec$day[i] == 1){
    temp4 <- 1
  }
  
  if(data_elec$Day_of_the_week[i] == "Pazartesi" & data_elec$Time[i] == as.factor("00:00")){
    temp4 <- temp4+1
  }
  data_elec$Week[i] <- temp4
}


# Grouping daily, monthly and weekly data by summing the hourly consumption datas

data_elec_daily <-  data_elec[,list(daily_total=sum(Consumption_MWh)), by = list(year,month,day)]
data_elec_monthly <- data_elec[,list(monthly_total=sum(Consumption_MWh)), by = list(year,month)]
data_elec_weekly <-  data_elec[,list(weekly_total=sum(Consumption_MWh)), by = list(year,month,Week)]

# Creating time series of daily, hourly, monthly and weekly data
hourly_ts <-  ts(data_elec$Consumption_MWh, start = c(2016,1),freq = 24*365)
daily_ts <- ts(data_elec_daily$daily_total, start = c(2016,1), frequency = 365)
weekly_ts <- ts(data_elec_weekly$weekly_total,start = c(2016,1), frequency = 52)
monthly_ts <- ts(data_elec_monthly$monthly_total,start = c(2016,1), frequency = 12)


# Plotting the series
par(mfrow = c(2,2))
plot(hourly_ts , xlab = "Year",ylab ="Consumption_MWh", main ="Hourly Consumption over Time")
plot(daily_ts , xlab = "Year",ylab ="Consumption_MWh", main ="Daily mean Consumption over Time")
plot(weekly_ts , xlab = "Year",ylab ="Consumption_MWh", main ="Weekly mean Consumption over Time")
plot(monthly_ts , xlab = "Year",ylab ="Consumption_MWh", main ="Monthly mean Consumption over Time")

# Plotting Autocorrelation Functions of daily, hourly, monthly and weekly time series
par(mfrow=c(2,2))
ggAcf(hourly_ts,lag.max = 200,main = "Autocorrelation of Hourly data")
ggAcf(daily_ts,lag.max = 40,main = "Autocorrelation of Daily data")
ggAcf(weekly_ts,lag.max = 100,main = "Autocorrelation of Weekly data")
ggAcf(monthly_ts,lag.max = 24,main = "Autocorrelation of Monthly data")

# Decomposition of Time series 

dec_hourly_ts <- decompose(hourly_ts,type = "multiplicative")
dec_daily_ts <- decompose(daily_ts,type = "multiplicative")
dec_weekly_ts <- decompose(weekly_ts,type = "multiplicative")
dec_monthly_ts <- decompose(monthly_ts,type = "multiplicative")

# Plots of the decompositions
autoplot(dec_hourly_ts,main="Decomposition of hourly data")
autoplot(dec_daily_ts, main = "Decomposition of daily data")
autoplot(dec_weekly_ts,main = "Decomposition of weekly data")
autoplot(dec_monthly_ts , main = "Decomposition of monthly data")

# Decompsing hourly data
dec_hourly_ts <- decompose(hourly_ts,type = "multiplicative")

# Detrending the data and plotting
detrend_hourly_ts <- hourly_ts / dec_hourly_ts$trend
plot (detrend_hourly_ts , main = "Hourly series with removed trend")

# Deseasonalizing the data and plotting
deseasonalized_hourly_ts <-  hourly_ts /dec_hourly_ts$seasonal
plot (deseasonalized_hourly_ts , main = "Hourly series with removed seasonality")

# Plot of the data with removed seansonality and trend

plot(dec_hourly_ts$random , main = "Hourly series with removed trend and seasonality")
plot(deseasonalized_hourly_ts)

# Auto correlation plot of the data with removed seansonality and trend

random_hourly_ts <- dec_hourly_ts$random
ggAcf(random_hourly_ts,na.action = na.pass ,lag.max = 200)

# Application of Auto Regressive Model

AIC_AR <- c()
BIC_AR <- c()
for ( i in 1:5){
  AR_model <-  arima(random_hourly_ts, order=c(i,0,0))
  #Akaike Information Criteria
  AIC_AR <- c(AIC_AR,AIC(AR_model))
  
  #Bayesian Information Criteria
  BIC_AR <- c(BIC_AR,BIC(AR_model))  
}

#Akaike Information Criteria
AIC_AR
#Bayesian Information Criteria
BIC_AR



#### Application of Moving Average Model

AIC_MA <- c()
BIC_MA <- c()
for ( i in 1:5){
  MA_model <-  arima(random_hourly_ts, order=c(0,0,i))
  #Akaike Information Criteria
  AIC_MA <- c(AIC_MA,AIC(MA_model))
  
  #Bayesian Information Criteria
  BIC_MA <- c(BIC_MA,BIC(MA_model))  

#We will check the Akaike and Bayesian Information criteria

  #Akaike Information Criteria
  AIC_MA
  #Bayesian Information Criteria
  BIC_MA


#Moving average model performs much higher AIC and BIC. 
#Therefore we will move on with Auto regressive model.


#### Making Predictions

# 24 hour prediction based on AR model.

  
# Using the last trend value of the decomposed trend to  find the residuals
  random_hourly_ts <- na.omit(random_hourly_ts)
  trend_coeff <- as.numeric(tail(na.omit(dec_hourly_ts$trend),1))
  seasonal_coeff <-  dec_hourly_ts$seasonal[(37848-7*24+1):(37848-6*24)]
  
  AR_model_hourly <- arima(random_hourly_ts, order = c(5,0,0))
  Data_model_fit <- random_hourly_ts-residuals(AR_model_hourly)
  
  hourly_forecast <- predict(AR_model_hourly, n.ahead = 24)$pred
  
  Forecasted_values <- hourly_forecast * trend_coeff * seasonal_coeff
  
  Forecasted_values <-  as.numeric(Forecasted_values)
  
  print(Forecasted_values)
  
  time_of_forecast <-  seq(as.POSIXct("2020-04-27 00:00:00"),as.POSIXct("2020-04-27 23:00:00"),by = "hour")
  
  
  
  filtered <- data.table(data_elec %>%
                           filter(year==2020) %>%
                           filter(month== 04))
  
  filtered[,"time_index" :=as.POSIXct(paste(Date,as.character(Time),sep = "/"), format = "%d.%m.%Y/%H:%M")
           ]
  
  plot(filtered$time_index, as.numeric(filtered$Consumption_MWh) , type = "l",xlim = c(as.POSIXct("2020-04-01"),as.POSIXct("2020-04-30")) , xlab = "Time", ylab = "Consumption (MWh)" , main  = "Electricity Consumption in April")
  
  points(cbind(time_of_forecast,Forecasted_values), type = "l", col = 2)
  
  
  #Test Data and calculating error
  test_data <- fread("test_data.csv")
  names(test_data) <- c("Date", "Time" , "Consumption_MWh")
  
  
  test_data$Consumption_MWh <- as.character(test_data$Consumption_MWh)
  temp7 <- c()
  for(i in 1:length(test_data$Consumption_MWh)){
    temp8 <- 1000*as.numeric(paste(unlist(strsplit(test_data$Consumption_MWh[i],",")),collapse = ""))
    temp7 <- c(temp7,temp8)
  }
  test_data$Consumption_MWh <- temp7
  
  Real_life_consumption <- test_data$Consumption_MWh
  
  #MAPE
  mean(abs(Forecasted_values-Real_life_consumption)/Real_life_consumption)
  
  #RMSE
  mean((Forecasted_values-Real_life_consumption)^2)^0.5
  
  

