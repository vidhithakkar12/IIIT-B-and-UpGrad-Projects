##############################################################################################################################
####################################### Retail-Giant Sales Forecasting  ######################################################
##############################################################################################################################

# Submission by : Vidhi Thakkar, Sahana K, Varsha Atmakuri,  Hema Kalidindi

###############################################################################################################################
# -------------------------------------- 1. Business Understanding ------------------------------------------------------------

# "Global Mart" is an online store super giant having worldwide operations. It takes orders and delivers across the globe 
# and deals with all the major product categories - consumer, corporate & home office.
# The company wants to finalise the plan for the next 6 months. So, they want to forecast the sales and the demand 
# for the next 6 months, that would help to manage the revenue and inventory accordingly.

#---------------------------------------------------------------------------------------------------------*
# Goal:                                                                                                   *
# To create a model to forecast the sales/demand for next 6 months using Time Series analysis.                         
#---------------------------------------------------------------------------------------------------------*

# a) Remove the previous data (if any)

rm(list=ls())


# b) Install necessary packages

if(!require(forecast)) {
  install.packages("forecast")
}

if(!require(tseries)) {
  install.packages("tseries")
}

if(!require(graphics)) {
  install.packages("graphics")
}

if(!require(dplyr)) {
  install.packages("dplyr")
}


if(!require(ggplot2)) {
  install.packages("ggplot2")
}


# c) Load the Packages

library(forecast)
library(tseries)
library(graphics)
library(dplyr)
library(ggplot2)

# d) Set working directory

setwd("C:/Users/thakk/Downloads")

################################################################################################################
# ------------------------------------- 2. Data Understanding --------------------------------------------------

# The data currently has the transaction level data, where each row represents a particular order made on the online store. 
# There are 24 attributes related to each such transaction. 
# The "Market" attribute has 7-factor levels representing the geographical market sector that the customer belongs to. 
# The "Segment" attribute tells which of the 3 segments that customer belongs to.

# DATA DICTIONARY (attributes)

# Order ID       -	Unique ID of the transaction 
# Order Date     -	Date on which the order was placed
# Ship Date      -	Date on which the shipment was made
# Ship Mode      -	The mode of shipment (category)
# Customer ID    -	The unique ID of the customer
# Customer Name  -	Name of the customer
# Segment        -	The market segment to which the product belongs
# City           -	City of the delivery address
# State          -	State of the delivery address
# Country        -	Country of the delivery address
# Postal Code    -	Postal code of the delivery address
# Market         -	Market segment to which the customer belongs
# Region         -	Geographical region of the customer
# Product ID     -	Unique ID of the product
# Category       -	Category of the product
# Sub-Category   -	Sub-category of the product
# Product Name   -	Name of the product
# Sales          -	Total sales value of the transaction
# Quantity       -	Quantity of the product ordered
# Discount       -	Discount percentage offered on the product
# Profit         -	Profit made on the transaction
# Shipping Cost  -	Shipping cost incured on the transaction
# Order Priority -	Priority assigned to the order


# a) Load the file 

Superstore<- read.csv("Global Superstore.csv")
View(Superstore)

#looking at the structure of the dataset
str(Superstore)
dim(Superstore)   # 51290    24

#################################################################################################################
# ------------------------------------- 3. Data Preparation/Cleaning --------------------------------------------

# a) Look for duplicate rows
sum(duplicated(Superstore))  # No duplicate rows

# b) Check for Missing/Blank values
sapply(Superstore, function(x) length(which(x == "")))  # No Blank Values

# c) Look for NA values
sapply(Superstore, function(x) sum(is.na(x)))  # Only Postal.Code field has NA values

# d) Removing unnecessary columns

#As State and City are present Postal Code is not needed for analysis, we are removing it
Superstore$Postal.Code <- NULL;

length(unique(Superstore$Row.ID));
# Removing Row.ID as they are distinct for each record and no analysis can be done
Superstore$Row.ID <- NULL;

length(unique(Superstore$Order.ID));
length(unique(Superstore$Customer.ID));
# Removing Order.ID and Customer.ID as ID's are not useful for analysis

Superstore$Order.ID <- NULL;
Superstore$Customer.ID <- NULL;
Superstore$Customer.Name <- NULL;

# Removing the following columns as we are not analyzing by product
Superstore$Product.ID <- NULL;
Superstore$Product.Name <- NULL;
Superstore$Category <- NULL;
Superstore$Sub.Category <- NULL;

# Removing the following columns as it will not be use for analysis
Superstore$Ship.Date <- NULL;
Superstore$Ship.Mode <- NULL;
Superstore$Order.Priority <- NULL;
Superstore$Shipping.Cost <- NULL;
Superstore$Discount <- NULL;


# e) Converting date column to date format
Superstore$Order.Date<-as.Date(Superstore$Order.Date,"%d-%m-%Y")

# f) Analyzing Market and Segment to find out 2 most profitable Market and Segment.

# Viewing the number of segments and markets : total 21
levels(Superstore$Segment)    # 3 Segments: "Consumer" "Corporate" "Home Office"
levels(Superstore$Market)     # 7 Markets: "Africa" "APAC" "Canada" "EMEA" "EU" "LATAM" "US"


market_names <- unique(Superstore$Market);
market_names <- as.character(market_names);

segment_names <- unique(Superstore$Segment);
segment_names <- as.character(segment_names);

######################################################################################################
# ------------------------------------ 4. Derived Metrics --------------------------------------------

# Segmenting the data into 21 subsets based on number of combinations via market and segment

# a) Taking a sum of profit, quantity and sales for each of this 21 subsets by Year and Month

k=1;
Superstore_aggr=list();
Superstore_Monthly_aggr=list();
for(i in market_names) {
  for(j in segment_names) {
   
    Superstore_Monthly_aggr[[k]] <-
        Superstore %>%
        filter(Segment==j & Market == i) %>%
        select(c(Market,Segment,Sales,Profit,Quantity,Order.Date)) %>%
        group_by(Market,Segment,as.character(Order.Date,"%Y-%m")) %>%
        summarise(TotalSales=sum(Sales), AvgSales=mean(Sales), TotalProfit=sum(Profit), AvgProfit=mean(Profit), ProfitPerc=((sum(Profit)/sum(Sales))*100), TotalQuantity=sum(Quantity));
    
    Superstore_aggr[[k]] <-
      Superstore %>%
      filter(Segment==j & Market == i) %>%
      select(c(Market,Segment,Sales,Profit,Quantity)) %>%
      group_by(Market,Segment) %>%
      summarise(TotalSales=sum(Sales), AvgSales=mean(Sales), TotalProfit=sum(Profit), AvgProfit=mean(Profit), ProfitPerc=((sum(Profit)/sum(Sales))*100), TotalQuantity=sum(Quantity));
    
    
    names(Superstore_Monthly_aggr[[k]])<-c('Market', 'Segment', 'MonthYr', 'TotalSalesAmount', 'AverageSalesAmount', 'TotalProfit', 'AverageProfit', 'Profitperc', 'TotalQuantity');
    names(Superstore_aggr[[k]])<-c('Market', 'Segment', 'TotalSalesAmount', 'AverageSalesAmount', 'TotalProfit', 'AverageProfit', 'Profitperc', 'TotalQuantity');
    k<-k+1;
  }
  
}

# b) Measuring coefficient of variation (CV)  in profit to select 2 most consistently profitable segments

#  It is the ratio of the standard deviation to the mean 
#  CV tells us the variation of sample to its mean.

cv=data.frame(cvmeasure = numeric());
i=1;
for (i in seq(1:(k-1))) {
 
  cv[i,'cvmeasure'] <- ((sd(Superstore_Monthly_aggr[[i]]$TotalProfit)/mean(Superstore_Monthly_aggr[[i]]$TotalProfit)));
  cv[i,'Market_Segment'] <- paste(Superstore_Monthly_aggr[[i]]$Market[1], Superstore_Monthly_aggr[[i]]$Segment[1], sep='_');
  print(cv[i,'cvmeasure'])
}

################################################################################################################
#------------------------------------ 5. Exploratory Data Analysis --------------------------------------------


# a) Plotting the coefficient of variance
ggplot(data = cv, aes(x = cvmeasure, y = Market_Segment)) + geom_point();


# b) Plotting Market Segment Vs. Profit

# 1) plotting Market and Segment based on total profit
plot1 <- ggplot(Superstore,aes(x=Superstore$Market,y=Superstore$Profit,fill=Superstore$Segment)) 
plot1+stat_summary(fun.y = sum, geom='bar',position='dodge')+xlab("Market")+ylab("Profit")+ggtitle("Total Profit accross segments")

# 2) plotting Market and Segment based on average profit
plot2 <- ggplot(Superstore,aes(x=Superstore$Market,y=Superstore$Profit,fill=Superstore$Segment))
plot2+stat_summary(fun.y = mean, geom='bar',position='dodge')+xlab("Market")+ylab("Profit %age")+ggtitle("Average Profit accross segments")

#APAC Consumer and EU Consumer have the highest profit.
#Canada has highest average profit is high but the total profit is less.

# 3) plotting Market and Segment based on coefficient of variation
plot3 <- ggplot(cv,aes(x=cv$Market_Segment,y=cv$cvmeasure))
plot3+coord_flip()+geom_bar(stat="identity",position="dodge")+xlab("Market_Segment")+ylab("Coeff. of variance of profit")+ggtitle("Coeff of variance in  profit Vs. Market Segment")

# Based on the maximum profits and low Coefficient of Variance for monthly profit to Sales ratio,
# The top 2 Market Segements chosen are:

# 1. APAC Consumer
# 2. EU Consumer

###############################################################################################################################
# ------------------------------------ 6. Model building ---------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------
#                                      1.  APAC Consumer
#----------------------------------------------------------------------------------------------------


#Market = APAC, Segment = Consumer
APAC_Consumer <- Superstore_Monthly_aggr[[4]]


# a) Preparing the Time series data
APAC_Consumer <- APAC_Consumer[order(APAC_Consumer$MonthYr),]
APAC_Consumer$Num <- c(1:nrow(APAC_Consumer))

APAC_Consumer_Sales <- APAC_Consumer[,c("Num","TotalSalesAmount")] 
APAC_Consumer_Qty <-  APAC_Consumer[,c("Num","TotalQuantity")]

# b) Seperating test and train data
APAC_SalesTest <- APAC_Consumer_Sales[c((nrow(APAC_Consumer_Sales)-5):nrow(APAC_Consumer_Sales)),]
APAC_SalesTrain <- APAC_Consumer_Sales[c(1:(nrow(APAC_Consumer_Sales)-6)),]
APAC_QtyTest <- APAC_Consumer_Qty[c((nrow(APAC_Consumer_Qty)-5):nrow(APAC_Consumer_Qty)),]
APAC_QtyTrain <- APAC_Consumer_Qty[c(1:(nrow(APAC_Consumer_Qty)-6)),]


#*******************************************************************************************************
# -------------------------------- Plotting timeseries data for Sales ----------------------------------
#*******************************************************************************************************


# a) Creating and plotting time series for Sales
APAC_Sales_TS <- ts(APAC_SalesTrain[,2])
plot(APAC_Sales_TS)

# b) Decompose timeseries to see the components using decompose function at frequency = 4

plot(decompose(ts(APAC_SalesTrain[,2],frequency=4)))

# Decomposotion shows
# 1. Trend is a high wavelength sine curve
# 2. Seasonality is a low wavelength sine curve

#---------------------------------- Smoothening the time series ----------------------------------------------------------

# a) Smoothening the curve - Moving Average Smoothing
w <- 1
APAC_Sales_TSSmooth <- stats::filter(APAC_Sales_TS, 
                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                     method='convolution', sides=2) 

# b) Smoothing left end of the time series
diff <- APAC_Sales_TSSmooth[w+2] - APAC_Sales_TSSmooth[w+1]
for (i in seq(w,1,-1)) {
  APAC_Sales_TSSmooth[i] <- APAC_Sales_TSSmooth[i+1] - diff}

# c) Smoothing right end of the time series
n <- length(APAC_Sales_TS)
diff <- APAC_Sales_TSSmooth[n-w] - APAC_Sales_TSSmooth[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_Sales_TSSmooth[i] <- APAC_Sales_TSSmooth[i-1] + diff}

# d) Plot the smoothed time series
plot(APAC_Sales_TS)
lines(APAC_Sales_TSSmooth, col="blue", lwd=2)

#---------------------------------- Classical Decomposition --------------------------------------------

# a) Converting the time series to a dataframe
timevals <- APAC_SalesTrain$Num
APAC_Sales_TSSmooth_df <- as.data.frame(cbind(timevals, as.vector(APAC_Sales_TSSmooth)))
colnames(APAC_Sales_TSSmooth_df) <- c('Month', 'Sales')

APAC_Sales_TSSmooth_df$Month<-as.numeric(APAC_Sales_TSSmooth_df$Month)
APAC_Sales_TSSmooth_df$Sales<-as.numeric(APAC_Sales_TSSmooth_df$Sales)

# b) Fitting multiplicative model with trend and seasonality to the data using sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3)+ cos(0.5*Month) * poly(Month,3) + Month, 
            data=APAC_Sales_TSSmooth_df)

summary(lmfit)
accuracy(lmfit)

# c) Locally predictable series(ARMA series) ***

global_pred <- predict(lmfit, Month=timevals)
summary(global_pred)
lines(timevals, global_pred, col="red", lwd=2)

# d) Manual Arima ***

local_pred <- APAC_Sales_TS - global_pred
plot(local_pred, col='red', type = "l")

acf(local_pred)
acf(local_pred, type="partial")

armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit

# e) Checking if the residual series is white noise

residual <- local_pred-fitted(armafit)
adf.test(residual,alternative = "stationary")
kpss.test(residual)

# f) Evaluating the model using MAPE
timevals.test <- APAC_SalesTest$Num
global_pred_out <- predict(lmfit,data.frame(Month=timevals.test))
fcast <- global_pred_out

# g) Comparing the predicted values with the actual values using MAPE
MAPE_class_dec <- accuracy(fcast,APAC_SalesTest$TotalSalesAmount)[5]
MAPE_class_dec
#31.07429

# h) Plotting the predictions wrt original values

class_pred <- c(ts(global_pred),ts(global_pred_out))
plot(APAC_Sales_TS, col = "black")
lines(class_pred, col = "red")

APAC_Sales_nxt6mths <- predict(lmfit,data.frame(Month=seq(1:54)))[49:54]

#---------------------------------- Auto Arima --------------------------------------------------------

APAC_autoarima <- auto.arima(APAC_Sales_TS)
APAC_autoarima
par(mar=c(1,1,1,1))
tsdiag(APAC_autoarima)

# a) Plotting the values
plot(APAC_autoarima$x, col="black")
lines(fitted(APAC_autoarima), col="red")
accuracy(APAC_autoarima)

# b) Check if the residual series is white noise
resi_autoarima <- APAC_Sales_TS - fitted(APAC_autoarima)

adf.test(resi_autoarima,alternative = "stationary")
kpss.test(resi_autoarima)

# c) Evaluating the model using MAPE
fcast_autoarima <- predict(APAC_autoarima, n.ahead = 6)

MAPE_autoarima <- accuracy(fcast_autoarima$pred,APAC_SalesTest$TotalSalesAmount)[5]
MAPE_autoarima 
# 27.68952

# d) Plotting the predictions wrt original values

autoarima_pred <- c(fitted(APAC_autoarima),ts(fcast_autoarima$pred))
plot(ts(APAC_Consumer_Sales$TotalSalesAmount), col = "black")
lines(autoarima_pred, col = "green")

# e) Prediction for next 6 months.
fcast_auto_arima_Nx6mths_APAC_Sales <- predict(APAC_autoarima, n.ahead = 12)$pred[7:12]
fcast_auto_arima_Nx6mths_APAC_Sales

# Conclusion:
# Auto ARIMA is better than Classical Decomposition for APAC Consumer (Sales)


#*******************************************************************************************************
# ------------------------ Plotting timeseries data for Quantity -----------------------------------------
#*******************************************************************************************************



# a) Creating and plotting time series for Quantity
APAC_Qty_TS <- ts(APAC_QtyTrain[,2])
plot(APAC_Qty_TS)

# b) Decompose timeseries to see the components using decompose function at frequency = 12
# Frequency is set for 12 because decomposition cannot be performed at frequency = 4
plot(decompose(ts(APAC_QtyTrain[,2],frequency=12)))

# Decomposotion shows
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

#---------------------------------- Smoothening the time series ----------------------------------------------------------

# a) Smoothening the curve - Moving Average Smoothing
w <-1
APAC_Qty_TSSmooth <- stats::filter(APAC_Qty_TS,filter=rep(1/(2*w+1),(2*w+1)), 
                                   method='convolution', sides=2)

# b) Smoothing left end of the time series
diff <- APAC_Qty_TSSmooth[w+2] - APAC_Qty_TSSmooth[w+1]

for (i in seq(w,1,-1)) {
  APAC_Qty_TSSmooth[i] <- APAC_Qty_TSSmooth[i+1] - diff
}

timevals <- APAC_QtyTrain$Num
timevals.test <- APAC_QtyTest$Num

# c) Smoothing right end of the time series
n <- length(APAC_Qty_TS)
diff <- APAC_Qty_TSSmooth[n-w] - APAC_Qty_TSSmooth[n-w-1]

for (i in seq(n-w+1, n)) {
  APAC_Qty_TSSmooth[i] <- APAC_Qty_TSSmooth[i-1] + diff
}

# d) Plot the smoothed time series
plot(APAC_Qty_TS)
lines(APAC_Qty_TSSmooth, col="blue", lwd=2)


#---------------------------------- Classical Decomposition --------------------------------------------

# a) Converting the time series to a dataframe

APAC_Qty_TSSmooth_df <- as.data.frame(cbind(timevals, as.vector(APAC_Qty_TSSmooth)))
colnames(APAC_Qty_TSSmooth_df) <- c('Month', 'Quantity')

APAC_Qty_TSSmooth_df$Month<-as.numeric(APAC_Qty_TSSmooth_df$Month)
APAC_Qty_TSSmooth_df$Quantity<-as.numeric(APAC_Qty_TSSmooth_df$Quantity)


# b) Fitting multiplicative model with trend and seasonality to the data using sinusoid function

lmfit <- lm(Quantity ~ sin(0.6*Month) * poly(Month,2)+cos(0.6*Month) * poly(Month,2) + sin(0.05*Month) * Month, 
            data=APAC_Qty_TSSmooth_df)

summary(lmfit)

# c) Locally predictable series(ARMA series) ***

global_pred <- predict(lmfit, data.frame(Month=timevals))
lines(timevals, global_pred, col="red", lwd=2)

# d) Manual Arima ***

local_pred <- APAC_Qty_TS - global_pred
plot(local_pred, col='red')

acf(local_pred)
acf(local_pred, type="partial")

armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)

# e) Checking if the residual series is white noise

residual <- local_pred-fitted(armafit)

adf.test(residual,alternative = "stationary")
kpss.test(residual)

# f) Evaluating the model using MAPE

timevals.test <- APAC_QtyTest$Num
global_pred_out <- predict(lmfit,data.frame(Month=timevals.test))
fcast <- global_pred_out

# g) Comparing the predicted values with the actual values using MAPE

MAPE_class_dec <- accuracy(fcast,APAC_QtyTest$TotalQuantity)[5]
MAPE_class_dec
#31.76748

# h) Plotting the predictions wrt original values

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(APAC_Consumer_Qty$TotalQuantity), col = "black")
lines(class_dec_pred, col = "red")


APAC_Demand_nxt6mths <- predict(lmfit,data.frame(Month=seq(1:54)))[49:54]

#---------------------------------- Auto Arima -------------------------------------------------------

APAC_autoarima <- auto.arima(APAC_Qty_TS)
APAC_autoarima
par(mar=c(1,1,1,1))
tsdiag(APAC_autoarima)

# a) Plotting the values
plot(APAC_autoarima$x, col="black")
lines(fitted(APAC_autoarima), col="red")

# b) Check if the residual series is white noise
resi_autoarima <- APAC_Qty_TS - fitted(APAC_autoarima)

adf.test(resi_autoarima,alternative = "stationary")
kpss.test(resi_autoarima)

# c) Evaluating the model using MAPE
fcast_autoarima <- predict(APAC_autoarima, n.ahead = 6)

MAPE_autoarima <- accuracy(fcast_autoarima$pred,APAC_QtyTest$TotalQuantity)[5]
MAPE_autoarima
#26.24458

# d) Plotting the predictions wrt original values

autoarima_pred <- c(fitted(APAC_autoarima),ts(fcast_autoarima$pred))

plot(ts(APAC_Consumer_Qty$TotalQuantity), col = "black")
lines(autoarima_pred, col = "green")

# e) Prediction for next 6 months.
fcast_auto_arima_Nx6mths_APAC_Demand <- predict(APAC_autoarima, n.ahead = 12)$pred[7:12]
fcast_auto_arima_Nx6mths_APAC_Demand

# Conclusion:
# Auto ARIMA is better than Classical Decomposition for APAC Consumer (Quantity)


#-----------------------------------------------------------------------------------------------------
#                                       2.  EU Consumer
#----------------------------------------------------------------------------------------------------

# #Market = EU, Segment = Consumer
EU_Consumer <- EU_Consumer <- Superstore_Monthly_aggr[[7]]

# a) Preparing the Time series data
EU_Consumer <- EU_Consumer[order(EU_Consumer$MonthYr),]
EU_Consumer$Num <- c(1:nrow(EU_Consumer))

EU_Consumer_Sales <- EU_Consumer[,c("Num","TotalSalesAmount")] 
EU_Consumer_Qty <-  EU_Consumer[,c("Num","TotalQuantity")]

# b) Seperating test and train data
EU_SalesTest <- EU_Consumer_Sales[c((nrow(EU_Consumer_Sales)-5):nrow(EU_Consumer_Sales)),]
EU_SalesTrain <- EU_Consumer_Sales[c(1:(nrow(EU_Consumer_Sales)-6)),]
EU_QtyTest <- EU_Consumer_Qty[c((nrow(EU_Consumer_Qty)-5):nrow(EU_Consumer_Qty)),]
EU_QtyTrain <- EU_Consumer_Qty[c(1:(nrow(EU_Consumer_Qty)-6)),]


#*******************************************************************************************************
# -------------------------------- Plotting timeseries data for Sales ----------------------------------
#*******************************************************************************************************


# a) Creating and plotting time series
EU_Sales_TS <- ts(EU_SalesTrain[,2])
plot(EU_Sales_TS)

# b) Decompose timeseries to see the components using decompose function
plot(decompose(ts(EU_SalesTrain[,2],frequency=4)))

# Decomposotion shows:
# 1. Trend is a high wavelength sine curve
# 2. Seasonality is a low wavelength sine curve


#---------------------------------- Smoothening the time series ----------------------------------------------------------

# a) Smoothening the curve - Moving Average Smoothing
w <- 1
EU_Sales_TSSmooth <- stats::filter(EU_Sales_TS, 
                                   filter=rep(1/(2*w+1),(2*w+1)), 
                                   method='convolution', sides=2)

# b) Smoothing left end of the time series
diff <- EU_Sales_TSSmooth[w+2] - EU_Sales_TSSmooth[w+1]
for (i in seq(w,1,-1)) {
  EU_Sales_TSSmooth[i] <- EU_Sales_TSSmooth[i+1] - diff}

# c) Smoothing right end of the time series
n <- length(EU_Sales_TS)
diff <- EU_Sales_TSSmooth[n-w] - EU_Sales_TSSmooth[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_Sales_TSSmooth[i] <- EU_Sales_TSSmooth[i-1] + diff}


# d) Plot the smoothed time series
plot(EU_Sales_TS)
lines(EU_Sales_TSSmooth, col="blue", lwd=2)


#---------------------------------- Classical Decomposition --------------------------------------------

# a) Converting the time series to a dataframe

timevals <- EU_SalesTrain$Num
EU_Sales_TSSmooth_df <- as.data.frame(cbind(timevals, as.vector(EU_Sales_TSSmooth)))
colnames(EU_Sales_TSSmooth_df) <- c('Month', 'Sales')

EU_Sales_TSSmooth_df$Month<-as.numeric(EU_Sales_TSSmooth_df$Month)
EU_Sales_TSSmooth_df$Sales<-as.numeric(EU_Sales_TSSmooth_df$Sales)

# b) Fitting multiplicative model with trend and seasonality to the data using sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) *
              poly(Month,2) +
              cos(0.5*Month) * 
              poly(Month,2) +
              Month, 
            data=EU_Sales_TSSmooth_df)

summary(lmfit)
accuracy(lmfit)

# c) Locally predictable series(ARMA series) ***

global_pred <- predict(lmfit, data.frame(Month=timevals))
lines(timevals, global_pred, col="red", lwd=2)

# d) Manual Arima ***

local_pred <- EU_Sales_TS - global_pred
plot(local_pred, col='red', type = "l")

acf(local_pred)
acf(local_pred, type="partial")

armafit <- auto.arima(local_pred)
tsdiag(armafit)


# e) Checking if the residual series is white noise

residual <- local_pred-fitted(armafit)
adf.test(residual,alternative = "stationary")
kpss.test(residual)

# f) Evaluating the model using MAPE
timevals.test <- EU_SalesTest$Num
global_pred_out <- predict(lmfit,data.frame(Month=timevals.test))
fcast <- global_pred_out

# g) Comparing the predicted values with the actual values using MAPE
MAPE_class_dec <- accuracy(fcast,EU_SalesTest$TotalSalesAmount)[5]
MAPE_class_dec
#34.35913

# h) Plotting the predictions wrt original values

class_pred <- c(ts(global_pred),ts(global_pred_out))
plot(EU_Sales_TS, col = "black")
lines(class_pred, col = "red")

EU_Sales_nxt6mths <- predict(lmfit,data.frame(Month=seq(1:54)))[49:54]

#---------------------------------- Auto Arima -------------------------------------------------------

EU_autoarima <- auto.arima(EU_Sales_TS)
EU_autoarima
par(mar=c(1,1,1,1))
tsdiag(EU_autoarima)

# a) Plotting the values
plot(EU_autoarima$x, col="black")
lines(fitted(EU_autoarima), col="red")
accuracy(EU_autoarima)

# b) Check if the residual series is white noise
resi_autoarima <- EU_Sales_TS - fitted(EU_autoarima)

adf.test(resi_autoarima,alternative = "stationary")
kpss.test(resi_autoarima)

# c) Evaluating the model using MAPE
fcast_autoarima <- predict(EU_autoarima, n.ahead = 6)

MAPE_autoarima <- accuracy(fcast_autoarima$pred,EU_SalesTest$TotalSalesAmount)[5]
MAPE_autoarima 
# 28.9226

# d) Plotting the predictions wrt original values

autoarima_pred <- c(fitted(EU_autoarima),ts(fcast_autoarima$pred))
plot(ts(EU_Consumer_Sales$TotalSalesAmount), col = "black")
lines(autoarima_pred, col = "green")

# e) Prediction for next 6 months.
fcast_auto_arima_Nx6mths_EU_Sales <- predict(EU_autoarima, n.ahead = 12)$pred[7:12]
fcast_auto_arima_Nx6mths_EU_Sales

# Conclusion:
# Auto ARIMA is better than Classical Decomposition for EU Consumer (Sales)


#*******************************************************************************************************
# ------------------------ Plotting timeseries data for Quantity ---------------------------------------
#*******************************************************************************************************


# a) Creating and plotting time series for Quantity
EU_Qty_TS <- ts(EU_QtyTrain[,2])
plot(EU_Qty_TS)

# b) Decompose timeseries to see the components using decompose function at frequency = 12
# Frequency is set for 12 because decomposition cannot be performed at frequency = 4

plot(decompose(ts(EU_QtyTrain[,2],frequency=12)))

# Decomposotion shows
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve


#---------------------------------- Smoothening the time series ----------------------------------------------------------

# a) Smoothening the curve - Moving Average Smoothing
w <-1
EU_Qty_TSSmooth <- stats::filter(EU_Qty_TS,filter=rep(1/(2*w+1),(2*w+1)), 
                                 method='convolution', sides=2)

# b) Smoothing left end of the time series
diff <- EU_Qty_TSSmooth[w+2] - EU_Qty_TSSmooth[w+1]

for (i in seq(w,1,-1)) {
  EU_Qty_TSSmooth[i] <- EU_Qty_TSSmooth[i+1] - diff
}

timevals <- EU_QtyTrain$Num
timevals.test <- EU_QtyTest$Num

# c) Smoothing right end of the time series
n <- length(EU_Qty_TS)
diff <- EU_Qty_TSSmooth[n-w] - EU_Qty_TSSmooth[n-w-1]

for (i in seq(n-w+1, n)) {
  EU_Qty_TSSmooth[i] <- EU_Qty_TSSmooth[i-1] + diff
}

# d) Plot the smoothed time series
plot(EU_Qty_TS)
lines(EU_Qty_TSSmooth, col="blue", lwd=2)


#---------------------------------- Classical Decomposition --------------------------------------------

# a) Converting the time series to a dataframe

EU_Qty_TSSmooth_df <- as.data.frame(cbind(timevals, as.vector(EU_Qty_TSSmooth)))
colnames(EU_Qty_TSSmooth_df) <- c('Month', 'Quantity')

EU_Qty_TSSmooth_df$Month<-as.numeric(EU_Qty_TSSmooth_df$Month)
EU_Qty_TSSmooth_df$Quantity<-as.numeric(EU_Qty_TSSmooth_df$Quantity)

# b) Fitting multiplicative model with trend and seasonality to the data using sinusoid function
lmfit <- lm(Quantity ~ sin(0.5*Month) *
              poly(Month,3) +
              cos(0.5*Month) * 
              poly(Month,3) +
              Month, 
            data=EU_Qty_TSSmooth_df)

summary(lmfit)

# c) Locally predictable series(ARMA series) ***

global_pred <- predict(lmfit,data.frame(Month=timevals))
lines(timevals, global_pred, col="red", lwd=2)

# d) Manual Arima ***

local_pred <- EU_Qty_TS - global_pred
plot(local_pred, col='red')

acf(local_pred)
acf(local_pred, type="partial")

armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)

# e) Checking if the residual series is white noise

residual <- local_pred-fitted(armafit)

adf.test(residual,alternative = "stationary")
kpss.test(residual)

# f) Evaluating the model using MAPE

global_pred_out <- predict(lmfit,data.frame(Month=timevals.test))
fcast <- global_pred_out

# g) Comparing the predicted values with the actual values using MAPE

MAPE_class_dec <- accuracy(fcast,EU_QtyTest$TotalQuantity)[5]
MAPE_class_dec
#30.39741

# h) Plotting the predictions wrt original values

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(EU_Consumer_Qty$TotalQuantity), col = "black")
lines(class_dec_pred, col = "red")

EU_Demand_nxt6mths <- predict(lmfit,data.frame(Month=seq(1:54)))[49:54]

#---------------------------------- Auto Arima -------------------------------------------------------

EU_autoarima <- auto.arima(EU_Qty_TS)
EU_autoarima
par(mar=c(1,1,1,1))
tsdiag(EU_autoarima)

# a) Plotting the values
plot(EU_autoarima$x, col="black")
lines(fitted(EU_autoarima), col="red")

# b) Check if the residual series is white noise
resi_autoarima <- EU_Qty_TS - fitted(EU_autoarima)

adf.test(resi_autoarima,alternative = "stationary")
kpss.test(resi_autoarima)

# c) Evaluating the model using MAPE
fcast_autoarima <- predict(EU_autoarima, n.ahead = 6)

MAPE_autoarima <- accuracy(fcast_autoarima$pred,EU_QtyTest$TotalQuantity)[5]
MAPE_autoarima
#30.13319

# d) Plotting the predictions wrt original values

autoarima_pred <- c(fitted(EU_autoarima),ts(fcast_autoarima$pred))

plot(ts(EU_Consumer_Qty$TotalQuantity), col = "black")
lines(autoarima_pred, col = "green")

# e) Prediction for next 6 months.
fcast_auto_arima_Nx6mths_EU_Demand <- predict(EU_autoarima, n.ahead = 12)$pred[7:12]
fcast_auto_arima_Nx6mths_EU_Demand

# Conclusion
# MAPE value is quite similar for both but Auto ARIMA has slightly lesser value
# Auto ARIMA is better than Classical Decomposition for EU Consumer (Quantity)


######################################################################################################
# ------------------------------------ 7. Forecasting ------------------------------------------------

# a) Forecast For APAC Consumer
fcast_auto_arima_Nx6mths_APAC_Sales
fcast_auto_arima_Nx6mths_APAC_Demand

# b) Forecast For EU Consumer
fcast_auto_arima_Nx6mths_EU_Sales
fcast_auto_arima_Nx6mths_EU_Demand

# c) Forecasting for Sales of APAC and EU Consumer
forecasted_data_Sales = data.frame();
forecasted_data_Sales <- cbind(fcast_auto_arima_Nx6mths_EU_Sales, fcast_auto_arima_Nx6mths_APAC_Sales, Month = seq(48:52))

forecasted_data_Sales = data.frame(forecasted_data_Sales)

# d) Plotting the forecast for Sales
plot1 <- ggplot()  
plot1 <- plot1 + geom_line(data=forecasted_data_Sales, aes(x=Month, y=fcast_auto_arima_Nx6mths_EU_Sales, color='blue') )
plot1 <- plot1 + geom_line(data=forecasted_data_Sales, aes(x=Month, y=fcast_auto_arima_Nx6mths_APAC_Sales, color='red' ))
plot1 <- plot1 + ylab('Sales Amount') + xlab('Month') + ggtitle('Sales For Next 6 Months')
plot1 <- plot1 + scale_color_discrete(name = "Market Segment", labels = c("APAC Consumer", "EU Consumer"))
plot1

# e) Forecasting for Demand of APAC and EU Consumer
forecasted_data_Demand = data.frame();
forecasted_data_Demand <- cbind(fcast_auto_arima_Nx6mths_EU_Demand, fcast_auto_arima_Nx6mths_APAC_Demand, Month = seq(48:52))

forecasted_data_Demand = data.frame(forecasted_data_Demand)

# f) Plotting the forecast for Demand
plot2 <- ggplot()
plot2 <- plot2 + geom_line(data=forecasted_data_Demand, aes(x=Month, y=fcast_auto_arima_Nx6mths_EU_Demand, color='blue') )
plot2 <- plot2 + geom_line(data=forecasted_data_Demand, aes(x=Month, y=fcast_auto_arima_Nx6mths_APAC_Demand, color='red' ))
plot2 <- plot2 + ylab('Demand') + xlab('Month') + ggtitle('Demand For Next 6 Months')
plot2 <- plot2 + scale_color_discrete(name = "Market Segment", labels = c("APAC Consumer", "EU Consumer"))
plot2

#----------------------------------- Forecast via Classical Decomposition ---------------------------------

# a) Forecast For APAC Consumer
APAC_Sales_nxt6mths
APAC_Demand_nxt6mths

# b) Forecast For EU Consumer
EU_Sales_nxt6mths
EU_Demand_nxt6mths

# c) Plotting the forecast for Sales
forecasted_data_Sales1 = data.frame();
forecasted_data_Sales1 <- cbind( APAC_Sales_nxt6mths, EU_Sales_nxt6mths, Month = seq(49:54))
forecasted_data_Sales1 = data.frame(forecasted_data_Sales1)

# d) Plotting the forecast for Sales
plot3 <- ggplot()  
plot3 <- plot3 + geom_line(data=forecasted_data_Sales1, aes(x=Month, y=EU_Sales_nxt6mths, color='blue') )
plot3 <- plot3 + geom_line(data=forecasted_data_Sales1, aes(x=Month, y=APAC_Sales_nxt6mths, color='red' ))
plot3 <- plot3 + ylab('Sales Amount') + xlab('Month') + ggtitle('Sales For Next 6 Months(Classical Decomposition)')
plot3 <- plot3 + scale_color_discrete(name = "Market Segment", labels = c("APAC Consumer", "EU Consumer"))
plot3

# e) Forecasting for Demand of APAC and EU Consumer
forecasted_data_Demand1 = data.frame();
forecasted_data_Demand1 <- cbind(APAC_Demand_nxt6mths, EU_Demand_nxt6mths, Month = seq(49:54))

forecasted_data_Demand1 = data.frame(forecasted_data_Demand1)

# f) Plotting the forecast for Demand
plot4 <- ggplot()
plot4 <- plot4 + geom_line(data=forecasted_data_Demand1, aes(x=Month, y=EU_Demand_nxt6mths, color='blue') )
plot4 <- plot4 + geom_line(data=forecasted_data_Demand1, aes(x=Month, y=APAC_Demand_nxt6mths, color='red' ))
plot4 <- plot4 + ylab('Demand') + xlab('Month') + ggtitle('Demand For Next 6 Months(Classical Decomposition)')
plot4 <- plot4 + scale_color_discrete(name = "Market Segment", labels = c("APAC Consumer", "EU Consumer"))
plot4


#------------------------------------------------------------------------------------------------------
#                                           THE END
#------------------------------------------------------------------------------------------------------

