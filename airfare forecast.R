#install packages
install.packages ("tidyverse")
install.packages("TTR")

#load libraries
library(tidyverse)
library(TTR)

#set working directory (adjust this for your own computer)
setwd("C:/Users/Lucy Wu/Documents")

#read dataset into R
airfaredf <- read.csv("airfare.csv")
View(airfaredf)

#create a time series plot showing annual of airfare
ggplot(data = airfaredf, mapping = aes(x = Year, y = Airfare)) +
  geom_line () +
  geom_point() +
  (breaks = seq(2003, 2019, by = 1)) +
  labs(title = "Annual Airfare for Domestic", x = "Year", y = "Airfare")

#create a separate vector for the actual airfare
airfare_actuals<-airfaredf$Airfare

#use the naive method to forecast annual of airfare
naive13 <- c(NA, airfare_actuals)
naive13

#The last value in the vector is the forecast for annual of airfare

#Create functions for the accuracy measures with vector of actual values 
#and vector of predicted values as inputs
mae<-function(actual,pred){
  mae <- mean(abs(actual-pred), na.rm=TRUE)
  return (mae)
}

mse<-function(actual,pred){
  mse <- mean((actual-pred)^2, na.rm=TRUE)
  return (mse)
}

rmse<-function(actual,pred){
  rmse <- sqrt(mean((actual-pred)^2, na.rm=TRUE))
  return (rmse)
}  

mape<-function(actual,pred){
  mape <- mean(abs((actual - pred)/actual), na.rm=TRUE)*100
  return (mape)
}

#Adjust the vector of predicted values to align with the sales_actuals vector
Naive_pred <- naive13[-length(naive13)]


#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(sales_actuals, Naive_pred)
mse(sales_actuals, Naive_pred)
rmse(sales_actuals, Naive_pred)
mape(sales_actuals, Naive_pred)

#use the simple moving average method to forecast the annual of airfare

sma1<-SMA (sales_actuals, n=3)
sma1

#The last value in the vector is the forecast for annual of airfare

#Adjust the vector of predicted values to align with the sales_actuals vector
sales_ma_pred<-c(NA, sma1[-length(sma1)]) 
sales_ma_pred

#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(sales_actuals, sales_ma_pred)
mse(sales_actuals, sales_ma_pred)
rmse(sales_actuals, sales_ma_pred)
mape(sales_actuals, sales_ma_pred)


#use the exponential smoothing method with alpha = 0.2 to forecast the 
#annual of airfare
exp1 <- EMA (sales_actuals, n=1, ratio = .2)
exp1

#The last value in the vector is the forecast for sales for annual of airfare

#Adjust the vector of predicted values to align with the sales_actuals vector
exp_pred <- c(NA, exp1[-length(exp1)])

#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mape(sales_actuals, exp_pred)
mae(sales_actuals, exp_pred)
mse(sales_actuals, exp_pred)
rmse(sales_actuals, exp_pred)


#use the exponential smoothing method with alpha = 0.4 to forecast the 
#annual of airfare
exp1_4 <- EMA (sales_actuals, n=1, ratio = .8)
exp1_4

#The last value in the vector is the forecast for sales for annual of airfare

#Adjust the vector of predicted values to align with the sales_actuals vector
exp_pred_4 <- c(NA, exp1_4[-length(exp1_4)])

#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(sales_actuals, exp_pred_4)
mse(sales_actuals, exp_pred_4)
rmse(sales_actuals, exp_pred_4)
mape(sales_actuals, exp_pred_4)