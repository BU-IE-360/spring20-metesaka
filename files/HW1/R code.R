# Loading Required Packages

library(data.table)
library(zoo)
library(xts)
library(readxl)
library(ggplot2)
library(stats)

# Importing data and converting into data.table

data_unemp<-data.table(read_xlsx("unemp_mon.xlsx"))
data_exc_rates<-data.table(read_xlsx("usd_eur_mon.xlsx"))
data_cpi<-data.table(read_xlsx("CPI_mon.xlsx"))
data_bud_sp<-data.table(read_xlsx("bud_spe_mon.xlsx"))

# Creating the Currency Basket

data_exc_basket<-data_exc_rates[,list(Currency_Basket_Rate = sum(`TP DK USD A YTL`, `TP DK EUR A YTL`)/2)
                                ,by=list(Tarih)]

# Merging the data into one table

temp<-merge(data_exc_basket,data_unemp)
temp<-merge(temp,data_bud_sp)
all_data<-merge(temp,data_cpi)

# Setting the column names of my table and checking it

setnames(all_data,names(all_data),
         c("Date","Currency_Basket_Rate","Unemployment_Rate","Government_Spending","Consumer_Price_Index"))

head(all_data)

# Converting Government Spending Data from Thousands of TRY into billions of TRY 

all_data$Government_Spending<-all_data$Government_Spending/1e6

# Converting the data into extended time series

all_data$Date <- as.Date(paste(all_data$Date,"01",sep="-"))
data_xts<- as.xts(all_data)


# Plotting my data 

par(mfrow=c(2,1))
plot(data_xts$Currency_Basket_Rate)
plot(data_xts$Unemployment_Rate)
plot(data_xts$Government_Spending )
plot(data_xts$Consumer_Price_Index )

# Summarizing my data and analyzing its descriptive statistics

summary(data_xts)

# Showing standart deviation of the data

sqrt(data.frame(var(data_xts$Currency_Basket_Rate),
                var(data_xts$Unemployment_Rate),
                var(data_xts$Government_Spending),
                var(data_xts$Consumer_Price_Index),row.names = "St. Deviation"))

# Covariance and correlation coefficients between CPI and the selected variables

var(data_xts,data_xts$Consumer_Price_Index)
cor(data_xts,data_xts$Consumer_Price_Index)

# Creating Scatter plots 

ggplot(data_xts,aes(Consumer_Price_Index,Currency_Basket_Rate ,color = index(data_xts))) + geom_point()
ggplot(data_xts,aes(Consumer_Price_Index,Unemployment_Rate ,color = index(data_xts))) + geom_point()
ggplot(data_xts,aes(Consumer_Price_Index,Government_Spending ,color = index(data_xts))) + geom_point()

# Plotting Auto-Correlation functions of the data

par(mfrow=c(2,1))

acf(data_xts$Currency_Basket_Rate)
acf(data_xts$Unemployment_Rate)
acf(data_xts$Government_Spending)
acf(data_xts$Consumer_Price_Index)

# Plots of the data with moving average with k = 12

par(mfrow=c(2,1))
plot(rollmean(data_xts$Currency_Basket_Rate,12))
plot(rollmean(data_xts$Unemployment_Rate,12))
plot(rollmean(data_xts$Government_Spending,12))
plot(rollmean(data_xts$Consumer_Price_Index,12))

# Taking log of the data 

log_data_xts <- log(data_xts)
par(mfrow=c(2,1))
plot(log_data_xts$Currency_Basket_Rate)
plot(log_data_xts$Unemployment_Rate)
plot(log_data_xts$Government_Spending)
plot(log_data_xts$Consumer_Price_Index)

# Calculating the difference of the data

Dif_data_xts <- na.omit(diff(data_xts))
Dif_log_data_xts <- na.omit(diff(log_data_xts))

summary(Dif_data_xts)
summary(Dif_log_data_xts)

par(mfrow=c(3,1))

plot(Dif_data_xts$Currency_Basket_Rate,main="Dif_Currency")
plot(Dif_log_data_xts$Currency_Basket_Rate,main="Dif_Log_Currency")
plot(Dif_data_xts$Unemployment_Rate,main="Dif_Unemployment")
plot(Dif_log_data_xts$Unemployment_Rate,main="Dif_Log_Unemployment")
plot(Dif_data_xts$Government_Spending,main="Dif_GovSpending")
plot(Dif_log_data_xts$Currency_Basket_Rate,main="Dif_Log_GovSpending")
par(mfrow = c(2,1))
plot(Dif_data_xts$Consumer_Price_Index,main="Dif_CPI")
plot(Dif_log_data_xts$Consumer_Price_Index,main="Dif_Log_CPI")

# Calculating correlation between differenced sets

cor(Dif_data_xts,Dif_data_xts$Consumer_Price_Index)

# Calculating lagged data 

lag_cor<-cor(na.omit(lag(Dif_data_xts$Consumer_Price_Index,0)),Dif_data_xts$Currency_Basket_Rate[1:119])

for(i in 1:16){
  
  lag_cor<-c(lag_cor,cor(na.omit(lag(Dif_data_xts$Consumer_Price_Index,i)),Dif_data_xts$Currency_Basket_Rate[1:(119-i)]))
}

lag_cor

# Auto-correlation functions of differenced sets

par(mfrow = c(2,1))
acf(Dif_data_xts$Currency_Basket_Rate)
acf(Dif_data_xts$Unemployment_Rate)
acf(Dif_data_xts$Government_Spending)
acf(Dif_data_xts$Consumer_Price_Index)

par(mfrow = c(2,1))
acf(Dif_log_data_xts$Currency_Basket_Rate)
acf(Dif_log_data_xts$Unemployment_Rate)
acf(Dif_log_data_xts$Government_Spending)
acf(Dif_log_data_xts$Consumer_Price_Index)

# Further analysis of Unemployment 
#Divide the data to years and calculate correlations between CPI

data_10 <- data_xts["2010"]
data_11 <- data_xts["2011"]
data_12 <- data_xts["2012"]
data_13 <- data_xts["2013"]
data_14 <- data_xts["2014"]
data_15 <- data_xts["2015"]
data_16 <- data_xts["2016"]
data_17 <- data_xts["2017"]
data_18 <- data_xts["2018"]
data_19 <- data_xts["2019"]

#correlation between the data in 2010
cor(data_10$Unemployment_Rate,data_10$Consumer_Price_Index)
#correlation between the data in 2011
cor(data_11$Unemployment_Rate,data_11$Consumer_Price_Index)
#correlation between the data in 2012
cor(data_12$Unemployment_Rate,data_12$Consumer_Price_Index)
#correlation between the data in 2013
cor(data_13$Unemployment_Rate,data_13$Consumer_Price_Index)
#correlation between the data in 2014
cor(data_14$Unemployment_Rate,data_14$Consumer_Price_Index)
#correlation between the data in 2015
cor(data_15$Unemployment_Rate,data_15$Consumer_Price_Index)
#correlation between the data in 2016
cor(data_16$Unemployment_Rate,data_16$Consumer_Price_Index)
#correlation between the data in 2017
cor(data_17$Unemployment_Rate,data_17$Consumer_Price_Index)
#correlation between the data in 2018
cor(data_18$Unemployment_Rate,data_18$Consumer_Price_Index)
#correlation between the data in 2019
cor(data_19$Unemployment_Rate,data_19$Consumer_Price_Index)


#correlation between the Unemployment data in 2010 and CPI data in 2011
cor(data_10$Unemployment_Rate,data_11$Consumer_Price_Index)
#correlation between the Unemployment data in 2011 and CPI data in 2012
cor(data_11$Unemployment_Rate,data_12$Consumer_Price_Index)
#correlation between the Unemployment data in 2012 and CPI data in 2013
cor(data_12$Unemployment_Rate,data_13$Consumer_Price_Index)
#correlation between the Unemployment data in 2013 and CPI data in 2014
cor(data_13$Unemployment_Rate,data_14$Consumer_Price_Index)
#correlation between the Unemployment data in 2014 and CPI data in 2015
cor(data_14$Unemployment_Rate,data_15$Consumer_Price_Index)
#correlation between the Unemployment data in 2015 and CPI data in 2016
cor(data_15$Unemployment_Rate,data_16$Consumer_Price_Index)
#correlation between the Unemployment data in 2016 and CPI data in 2017
cor(data_16$Unemployment_Rate,data_17$Consumer_Price_Index)
#correlation between the Unemployment data in 2017 and CPI data in 2018
cor(data_17$Unemployment_Rate,data_18$Consumer_Price_Index)
#correlation between the Unemployment data in 2018 and CPI data in 2019
cor(data_18$Unemployment_Rate,data_19$Consumer_Price_Index)





