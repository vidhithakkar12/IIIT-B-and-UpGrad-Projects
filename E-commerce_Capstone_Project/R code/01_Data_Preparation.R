####################################################################################################################################
####################################################################################################################################
#                     :::::::: E-commerce Capstone Project - Market Mix Modelling ::::::::
####################################################################################################################################                              
####################################################################################################################################

# Submission by : Sathish Devunuri, Shalini Bulusu, Varsha Atmakuri, Vidhi Thakkar


####################################################################################################################################
#------------------------------------ 1. Business Understanding --------------------------------------------------------------------
####################################################################################################################################


# ElecKart is an e-commerce firm specialising in electronic products. Over the last one year, 
# they had spent a significant amount of money in marketing. Occasionally, 
# they had also offered big-ticket promotions (similar to the Big Billion Day). 
# They are about to create a marketing budget for the next year which includes spending on commercials, 
# online campaigns, and pricing & promotion strategies. 
# The CFO feels that the money spent over last 12 months on marketing was not sufficiently impactful, and, 
# that they can either cut on the budget or reallocate it optimally across marketing levers to improve the revenue response.

# Goal:- 

# The aim is to develop a market mix model to identify the most impactful channels for marketing spends over the last year.
# And to identify other factors having positive impact on sales of 3 product categories - Camera Accessory, Game Accessory, & Home Audio
# Basically the company needs to optimize the marketing levers to improve the revenue response.


#------------------------------------------------------------------------------------------------------------------------------------------

# a) Remove the previous data (if any)

rm(list=ls())


# b) Installing necessary packages

if(!require(dplyr)) {
  install.packages("dplyr")
}
if(!require(lubridate)) {
  install.packages("lubridate")
}
if(!require(readxl)) {
  install.packages("readxl")
}
if(!require(ggplot2)) {
  install.packages("ggplot2")
}
if(!require(tidyr)) {
  install.packages("tidyr")
}
if(!require(stringr)) {
  install.packages("stringr")
}
if(!require(DataCombine)) {
  install.packages("DataCombine")
}
if(!require(DataCombine)) {
  install.packages("reshape2")
}
if(!require(pracma)) {
  install.packages("pracma")
}
if(!require(car)) {
  install.packages("car")
}
if(!require(MASS)) {
  install.packages("MASS")
}
if(!require(DAAG)) {
  install.packages("DAAG")
}


# c) Loading the Packages

library('dplyr');
library('lubridate');
library('readxl');
library('ggplot2');
library('tidyr');
library('stringr');
library('DataCombine');
library('reshape2');
library('pracma');
library('car');
library('MASS');
library('DAAG');


# d) Set working directory

setwd("C:/Users/thakk/Desktop/Capstone Project")


##################################################################################################################################################
#-------------------------------------- 2. Data Understanding ------------------------------------------------------------------------------------
###################################################################################################################################################

# DATA DICTIONARY 

# The data consists of the following types of information:

# Order level data

# FSN ID: The unique identification of each SKU
# Order Date: Date on which the order was placed
# Order ID: The unique identification number of each order
# Order item ID: Suppose you order 2 different products under the same order, it generates 2 different order Item IDs under the same order ID; orders are tracked by the Order Item ID.
# GMV: Gross Merchandise Value or Revenue
# Units: Number of units of the specific product sold
# Order payment type: How the order was paid - prepaid or cash on delivery
# SLA: Number of days it typically takes to deliver the product
# Cust id: Unique identification of a customer
# Product MRP: Maximum retail price of the product
# Product procurement SLA: Time typically taken to procure the product


# Apart from this, the following information is also available:

# Monthly spends on various advertising channels
# Days when there was any special sale
# Monthly NPS score - this may work as a proxy to 'voice of customer'



# a) Loading the data (all the files)

# Consumer Electronics.
ConsumerElectronics <- read.csv('ConsumerElectronics.csv',stringsAsFactors = FALSE);

# Loading all the sheets individually from Media data and other information file

# Product List  
Product_list_data <- read_excel("Media data and other information.xlsx", sheet = 1, col_names = F);

# Media Investment 
Media_Investment = read_excel("Media data and other information.xlsx", sheet = 2, col_names = F);

# Special Sale Calendar
Special_Sale_Calendar <- read_excel("Media data and other information.xlsx", sheet=3, col_names = F);

# Monthly NPS Score 
NPS_Score_data <- read_excel("Media data and other information.xlsx", sheet = 4, col_names = F);


# b) Look at the structure of the dataset

# Consumer_Electronics
str(ConsumerElectronics)
dim(ConsumerElectronics)   # 1648824   20

# Product_List
str(Product_list_data)
dim(Product_list_data)   # 76  3

# Media_Investment
str(Media_Investment)
dim(Media_Investment)   # 14 12

# Special_Sale_Calendar
str(Special_Sale_Calendar)
dim(Special_Sale_Calendar)   # 13  2

# Monthly_NPS_Score
str(NPS_Score_data)
dim(NPS_Score_data)   # 2 13


#####################################################################################################################################################
#------------------------------------- 3. Data Preparation/Cleaning ---------------------------------------------------------------------------------
#####################################################################################################################################################


#-------------------------------------------- Product_List --------------------------------------------------------------------------

# a) Adjusting for right column names
colnames(Product_list_data) <- Product_list_data[1,];
Product_list_data <- Product_list_data[-1,]
colnames(Product_list_data)[1] <- 'product_analytic_vertical';

# b) Data Type conversion
Product_list_data$Frequency <- as.numeric(Product_list_data$Frequency);
Product_list_data$Percent <- as.numeric(Product_list_data$Percent);


#-------------------------------------------- Special_Sale_Calendar ----------------------------------------------------------------------------------

# a) Hardcoding all the holiday dates given in the data in a new data frame

Holidays_Sale_Dates <- data.frame(holiday_dates = as.Date(
  c(
    '07/18/2015',
    '07/19/2015',
    '08/15/2015',
    '08/16/2015',
    '08/17/2015',
    '08/28/2015',
    '08/29/2015',
    '08/30/2015',
    '10/15/2015',
    '10/16/2015',
    '10/17/2015',
    '11/07/2015',
    '11/08/2015',
    '11/09/2015',
    '11/10/2015',
    '11/11/2015',
    '11/12/2015',
    '11/13/2015',
    '11/14/2015',
    '12/25/2015',
    '12/26/2015',
    '12/27/2015',
    '12/28/2015',
    '12/29/2015',
    '12/30/2015',
    '12/31/2015',
    '01/01/2016',
    '01/02/2016',
    '01/03/2016',
    '01/20/2016',
    '01/21/2016',
    '01/22/2016',
    '02/01/2016',
    '02/02/2016',
    '02/20/2016',
    '02/21/2016',
    '02/14/2016',
    '02/15/2016',
    '03/07/2016',
    '03/08/2016',
    '03/09/2016',
    '05/25/2016',
    '05/26/2016',
    '05/27/2016'
  ),
  "%m/%d/%Y"
))


# b) Adding more columns to Holidays_Sale_Dates Dataframe - Derived Variables Year, Month and Week
Holidays_Sale_Dates$Year <- year(Holidays_Sale_Dates$holiday_dates);
Holidays_Sale_Dates$Month <- month(Holidays_Sale_Dates$holiday_dates);

Week <- seq(1:53);
Weekly_Dates <- data.frame(Week);
Weekstart <- seq(as.Date("07/01/2015", "%m/%d/%Y"), as.Date("06/30/2016", "%m/%d/%Y"),by = "week")
Weekly_Dates$Weekstart <- Weekstart;
Weekend <- seq(as.Date("07/07/2015", "%m/%d/%Y"),by = "week", length=53);
Weekly_Dates$Weekend <- Weekend;
Weekly_Dates[53,3] <- as.Date("06/30/2016", "%m/%d/%Y");


Holidays_Sale_Dates <- merge(Weekly_Dates, Holidays_Sale_Dates, all=TRUE);
                          

Holidays_Sale_Dates <- Holidays_Sale_Dates %>% 
  filter(holiday_dates >= Weekstart, holiday_dates <= Weekend)

Holidays_Sale_Dates <- Holidays_Sale_Dates[,-c(2,3)];



#-------------------------------------------- Media_Investment ------------------------------------------------------------------------------------------

# a) Removing unnecessary headers and adjusting for right column names.
colnames(Media_Investment)<-Media_Investment[2,]
Media_Investment <- Media_Investment[-1,];
Media_Investment <- Media_Investment[-1,];

# b) Data Type conversion
Media_Investment <- as.data.frame(sapply(Media_Investment, function(x){x<-as.numeric(x)}));


#-------------------------------------------- Monthly_NPS_Score -----------------------------------------------------------------------------------------

# Net Promoter Score is an index ranging from -100 to 100 
# It measures the willingness of customers to recommend company's products or services to others

# a) Removing unnecessary headers and adjusting for right column names
NPS_Score_data <- as.data.frame(t(NPS_Score_data)); # transpose


# b) Making Month and year as column name.
colnames(NPS_Score_data) <- c('Month_Year', 'NPS_Score');
NPS_Score_data <- NPS_Score_data[-1,];


# c) Separating Month and Year into 2 different columns inorder to be consisitent with other dataframes.
NPS_Score_data <- NPS_Score_data %>% separate(Month_Year, into = c("Month","Year"));
NPS_Score_data$Month <- match(str_to_title(substr(NPS_Score_data$Month, 1, 3)), month.abb);
NPS_Score_data$Year <- sapply(NPS_Score_data$Year, function(x) {paste('20', x, sep='')});


# d) Converting NPS Score to weekly level
NPS_Score_data$NPS_Score <-as.numeric(as.character(NPS_Score_data$NPS_Score));
NPS_Score_data <- merge(Weekly_Dates, NPS_Score_data, all=TRUE);

NPS_Score_data <- NPS_Score_data %>% 
  filter(Month == as.numeric(format(Weekstart, "%m")) | Month == as.numeric(format(Weekend, "%m")))


NPS_Score_data$NPS_Score <-
      ifelse(
        as.numeric(format(NPS_Score_data$Weekstart, "%m")) == as.numeric(format(NPS_Score_data$Weekend, "%m"))
          ,NPS_Score_data$NPS_Score,
          ifelse(NPS_Score_data$Month == as.numeric(format(NPS_Score_data$Weekstart, "%m")),
             ((as.Date(paste(format(NPS_Score_data$Weekstart, format="%Y-%m"),"-", days_in_month(NPS_Score_data$Weekstart), sep=""), format="%Y-%m-%d")  - NPS_Score_data$Weekstart) + 1 )* (1/7) * NPS_Score_data$NPS_Score,
             ((NPS_Score_data$Weekend - as.Date(paste(format(NPS_Score_data$Weekend, format="%Y-%m"),"-", "01", sep ="")) ) + 1) * (1/7) * NPS_Score_data$NPS_Score)
      );     


NPS_Score_data <- NPS_Score_data %>%
    group_by(Week) %>%
    summarise(NPS_Score = sum(NPS_Score));

#-------------------------------------------- Consumer_Electronics --------------------------------------------------------------------------------------

View(head(ConsumerElectronics,30));

# a) Renaming few columns
colnames(ConsumerElectronics)[1]<-'FSN_ID';
colnames(ConsumerElectronics)[11] <- 'Order_Payment_Type'


# b) Data should be from July 2015 to June 2016
distinct(ConsumerElectronics, Year, Month);

# The data is beyond the requested period for analysis 
# Filtering the data and considering only for the range period July 2015 to June 2016

ConsumerElectronics <- ConsumerElectronics %>%
  filter(!((Year == 2015 & Month == 5) | (Year == 2015 & Month == 6) | (Year == 2016 & Month == 7)));

distinct(ConsumerElectronics, Year, Month);
dim(ConsumerElectronics);
#1648215 rows and 20 columns.

# 609 records removed from the file for further analysis 


# c) Checking for duplicate rows.
nrow(distinct(ConsumerElectronics));
#1543372

# Removing duplicate rows.
ConsumerElectronics <- distinct(ConsumerElectronics);


# d) Extracting rows with sub categories as camera accessory, home audio and gaming accessory
distinct(ConsumerElectronics, product_analytic_sub_category);

ConsumerElectronics <- ConsumerElectronics %>%
  filter(product_analytic_sub_category %in% c('CameraAccessory', 'GamingAccessory', 'HomeAudio'));

distinct(ConsumerElectronics, product_analytic_sub_category);

# product_analytic_sub_category
# 1               CameraAccessory
# 2               GamingAccessory
# 3                     HomeAudio



# e) Checking Super Category 
levels(ConsumerElectronics$product_analytic_super_category);

# Super Category has only  value 'CE' and therefore cannot be used for analysis. Hence removing it.
ConsumerElectronics$product_analytic_super_category <- NULL;


# f) Converting order_date to Date format
ConsumerElectronics$order_date <- ymd_hms(ConsumerElectronics$order_date);
ConsumerElectronics$OrderDate <- as.Date(format(ConsumerElectronics$order_date, "%Y-%m-%d"), format="%Y-%m-%d");


# g) Introducing a week column, as the analysis has to be on a weekly level
ConsumerElectronics <- merge(ConsumerElectronics, Weekly_Dates, all=TRUE);


ConsumerElectronics <- ConsumerElectronics %>% 
  filter(OrderDate >= Weekstart, OrderDate <= Weekend)


# h) Check for free products
summary(ConsumerElectronics$product_mrp);
# The products with mrp = 0 are free items. So, removing them.

nrow(ConsumerElectronics[ConsumerElectronics$product_mrp==0,])
#3240 

#There are 3240 transactions with MRP=0 and we should remove these transactions to have clean model and further analysis 
#There will be no product with Zero MRP value 

nrow(ConsumerElectronics[is.na(ConsumerElectronics$product_mrp),])
#0 


# i) Considering the above decision of removing the records where MRP ==0, we will consider only MRP > 0 
ConsumerElectronics <- ConsumerElectronics[ConsumerElectronics$product_mrp>0,];
nrow(ConsumerElectronics[ConsumerElectronics$product_mrp==0,])
#0 

#There are rows where mrp * units > gmv. This means the product is sold at a higher price than mrp.
#ConsumerElectronics_test <- ConsumerElectronics[(ConsumerElectronics$product_mrp * ConsumerElectronics$units) < ConsumerElectronics$gmv, ];
#Though we are not removing these resords from the final data set as they are considered as offer / discount 


# j) For gmv = 0

# gmv for some orders is 0, so there is a possibility that those products are sold under an offer 
# Since they are valid transaction we are not removing 

nrow(ConsumerElectronics[ConsumerElectronics$gmv==0,])
# 1619 orders are considered as Zero gmv 

# Down the line we are creating derived KPI's like Selling Price or List price which uses this variable directly and zero will be the result 
# these zeros will derive a huge impact on the overall models 
# we will be using 1 instead of zero for all the 1619 orders 
# this will impact the individual numbers but very minimal effect like instead of 100% discount it shows 99.999 

ConsumerElectronics$gmv <- ifelse(ConsumerElectronics$gmv == 0, 1, ConsumerElectronics$gmv);



# k) Missing Value Treatment for deliverybdays and deliverycdays

nrow(ConsumerElectronics[ConsumerElectronics$deliverybdays=='\\N',])
#408741
nrow(ConsumerElectronics[ConsumerElectronics$deliverycdays=='\\N',])
#408741

#Approximatly 78% of Data have \N, that means there is only delay for 20% of orders 

#For a better further analysis all the NO DELAY values \N are going to convert as 0 and convert the both fields to Number 

ConsumerElectronics$deliverybdays[ConsumerElectronics$deliverybdays=='\\N'] <- '0'
ConsumerElectronics$deliverycdays[ConsumerElectronics$deliverycdays=='\\N'] <- '0'

# Data Type Conversion
ConsumerElectronics$deliverybdays <- as.integer(ConsumerElectronics$deliverybdays)
ConsumerElectronics$deliverycdays <- as.integer(ConsumerElectronics$deliverycdays)


# l) Omit the NA records from the data set 

test <- na.omit(ConsumerElectronics)
nrow(test) - nrow(ConsumerElectronics)
#-1358

#  Total 1358 records have NA and it is very less in % so we omit it

ConsumerElectronics <- na.omit(ConsumerElectronics)


########################################################################################################################################################
#----------------------------------- 4.Deriving Engineered KPIs :: For Future Analysis-------------------------------------------------------------------
#########################################################################################################################################################


# Introducing Derived Columns for further Analysis 

# a) SellingPrice = gmv/units 
ConsumerElectronics$SellingPrice <- ConsumerElectronics$gmv / ConsumerElectronics$units;

#Selling price should not be greater than mrp. Hence, removing those.
ConsumerElectronics <- ConsumerElectronics[ConsumerElectronics$SellingPrice <= ConsumerElectronics$product_mrp,];


# b) Discount percentage 
# Discount = (mrp-sellingPrice)/ mrp  in % 
ConsumerElectronics$Discount <- ((ConsumerElectronics$product_mrp - ConsumerElectronics$SellingPrice) / ConsumerElectronics$product_mrp) * 100;


# c) Converting the Order_Payment_Type to Wide for further analysis 
ConsumerElectronics$COD     = as.integer(ConsumerElectronics$Order_Payment_Type=='COD')
ConsumerElectronics$Prepaid = as.integer(ConsumerElectronics$Order_Payment_Type!='COD')


# d) Product Clustering 

# Cluster divides the products into three categories based on MRP and numer product units sold 
# mass market, medium market and premium product are considered as 3 clusters 

ConsumerElectronics$product_analytic_vertical <- factor(ConsumerElectronics$product_analytic_vertical)

cluster<- aggregate(cbind(units,SellingPrice,product_mrp)~product_analytic_vertical,ConsumerElectronics,mean)


if(nrow(cluster)<=3){
  
  cluster$p_tag <-NA
  
  # Assuming premium product:- 
  
  cluster$P_tag[which(cluster$product_mrp>=mean(cluster$product_mrp))] <- "Middle_p"
  cluster$P_tag[-which(cluster$product_mrp>=mean(cluster$product_mrp))] <- "Mass_p"
  
  cluster <- cluster[,-c(2:4)]
  
  dataset <-merge(ConsumerElectronics,cluster,by="product_analytic_vertical")
  
} else {
  
  #str(cluster) 
  
  cluster$SellingPrice_1 <- scale(cluster$SellingPrice)
  cluster$product_mrp_1<- scale(cluster$product_mrp)
  cluster$units_1 <- scale(cluster$units)
  
  #str(cluster)
  
  k1 <- cluster[,-c(2:4)]
  
  
  clust <- kmeans(k1[,-1], centers = 3,iter.max = 50,nstart = 50)
  
  cluster$P_tag <- as.factor(clust$cluster)
  
  cluster <- cluster[,c(1,8)]
  
  # Add extra column in dataset with 
  
  ConsumerElectronics <-merge(ConsumerElectronics,cluster,by=c("product_analytic_vertical"),all.x=TRUE)
  

  k2 <- table(ConsumerElectronics$P_tag)
  
  levels(ConsumerElectronics$P_tag)[which(k2==max(table(ConsumerElectronics$P_tag)))] <- "Mass_p"
  levels(ConsumerElectronics$P_tag)[which(k2==min(table(ConsumerElectronics$P_tag)))] <- "Premium_p"
  levels(ConsumerElectronics$P_tag)[which(k2!=max(table(ConsumerElectronics$P_tag))& k2!=min(table(ConsumerElectronics$P_tag)))] <- "Middle_p"
  
  
}


# e) Converting the P_tag to Wide for further analysis.

ConsumerElectronics$Mass_p = as.integer(ConsumerElectronics$P_tag=='Mass_p')
ConsumerElectronics$Premium_p = as.integer(ConsumerElectronics$P_tag=='Premium_p')
ConsumerElectronics$Middle_p = as.integer(ConsumerElectronics$P_tag=='Middle_p')


# f) Combining Holiday Data set with Consumer Electronics.

# Introducing IsHolidayWeek, No_of_Holidays (in the week) in ConsumerElectronics data.
ConsumerElectronics$IsHolidayWeek <- ifelse (ConsumerElectronics$Week %in% Holidays_Sale_Dates$Week, 1, 0);
ConsumerElectronics$IsHoliday <- ifelse (ConsumerElectronics$OrderDate %in% Holidays_Sale_Dates$holiday_dates, 1, 0);

Weekly_no_of_Holidays <- Holidays_Sale_Dates %>%
  group_by(Week) %>%
  summarise(No_of_Holidays = n());

ConsumerElectronics <- merge(ConsumerElectronics, Weekly_no_of_Holidays, by='Week', all.x = TRUE)



# g) Dividing the media - investment by No.of days of that month so that the values are on a daily basis.

Media_Investment <- cbind(
                      Media_Investment[,c(1,2)],
                      Media_Investment[,-c(1,2)] /
                                  days_in_month(as.Date(paste("01", Media_Investment$Month, Media_Investment$Year, sep="/"), "%d/%m/%Y"))
                          );


Media_Investment <- merge(Weekly_Dates, Media_Investment, all=TRUE);


Media_Investment <- Media_Investment %>% 
  filter(Month == as.numeric(format(Weekstart, "%m")) | Month == as.numeric(format(Weekend, "%m")))



Media_Investment[(as.numeric(format(Media_Investment$Weekstart, "%m")) == as.numeric(format(Media_Investment$Weekend, "%m"))) , -c(1,2,3,4,5)] <-
Media_Investment[(as.numeric(format(Media_Investment$Weekstart, "%m")) == as.numeric(format(Media_Investment$Weekend, "%m"))) , -c(1,2,3,4,5)] * 7;



cond1 <- !(as.numeric(format(Media_Investment$Weekstart, "%m")) == as.numeric(format(Media_Investment$Weekend, "%m")))
cond2 <-  Media_Investment$Month == as.numeric(format(Media_Investment$Weekstart, "%m"))
cond3 <- Media_Investment$Month == as.numeric(format(Media_Investment$Weekend, "%m"))

Media_Investment[cond1 & cond2, -c(1,2,3,4,5)] <-

  Media_Investment[cond1 & cond2, -c(1,2,3,4,5)] *
  (as.numeric(as.Date(paste(format(Media_Investment[cond1 & cond2, "Weekstart"], format="%Y-%m"),"-", days_in_month(Media_Investment[cond1 & cond2, "Weekstart"]), sep=""), format="%Y-%m-%d") - Media_Investment[cond1 & cond2, "Weekstart"]) + 1 ) 


Media_Investment[cond1 & cond3, -c(1,2,3,4,5)] <-
  
  Media_Investment[cond1 & cond3, -c(1,2,3,4,5)] *
  (as.numeric(Media_Investment[cond1 & cond3, "Weekend"] - as.Date(paste(format(Media_Investment[cond1 & cond3, "Weekend"], format="%Y-%m"),"-", "01", sep =""), format="%Y-%m-%d")) + 1) 


columnnames <- c(colnames(Media_Investment))


Media_Investment <- Media_Investment %>%
  group_by(Week) %>%
  summarise(`Total Investment` <- sum(`Total Investment`),
            TV <- sum(TV),
            Digital <- sum(Digital),
            Sponsorship <- sum(Sponsorship),
            `Content Marketing` <- sum(`Content Marketing`),
            `Online marketing` <- sum(`Online marketing`),
            Affiliates <- sum(Affiliates),
            SEM <- sum(SEM),
            Radio <- sum(Radio),
            Other <- sum(Other)
  );

colnames(Media_Investment) <- columnnames[-c(2,3,4,5)];


# h) Handling NA data in Media_Investment

Media_Investment$Radio <- ifelse(
  is.na(Media_Investment$Radio),
  0,
  Media_Investment$Radio
);

Media_Investment$Other <- ifelse(
  is.na(Media_Investment$Other),
  0,
  Media_Investment$Other
);


# j) Creating Adstocks
adstock_rate = 0.50
 
# Creating the adstock for each media investment
df <- data.frame(Week=1:53)
 

for(i in 3:ncol(Media_Investment)){
       df[[paste0(colnames(Media_Investment)[i],"_adstock")]] <-
            stats::filter(x=Media_Investment[i], 
            filter=adstock_rate, method="recursive");
       
}
# View(df)
 
 Media_Investment <- df;
 Media_Investment[,c(2:10)] <- lapply(Media_Investment[,c(2:10)], function(x) as.numeric(as.character(x)))
 
 

#---------------------------- Aggregating Consumer Electronics data at a weekly level -----------------------------------------------------------------------------

# a) Unnecessary columns like FSN_ID, product_analytical_vertical, pincode, custid, 
#    order date, year, month, order id and order item id are not taken in this dataframe
 
 ConsumerElectronics_weekly <-  ConsumerElectronics %>% 
   group_by(product_analytic_sub_category,
            Week) %>% 
   summarise(
     no_of_orders = n(),
     gmv=sum(gmv), 
     product_mrp=mean(product_mrp),
     SellingPrice=mean(SellingPrice),
     units=sum(units),
     Discount=mean(Discount),
     sla=mean(sla), 
     procurement_sla=mean(product_procurement_sla),
     COD=sum(COD),
     Prepaid = sum(Prepaid),
     IsHolidayWeek = mean(IsHolidayWeek),
     No_of_Holidays = mean(No_of_Holidays),
     deliverybdays=mean(deliverybdays),
     deliverycdays=mean(deliverycdays),
     Mass_p = sum(Mass_p),
     Premium_p = sum(Premium_p),
     Middle_p = sum(Middle_p)
   )

# b) Handling NA values in final Consumer Electronics Data

sapply(ConsumerElectronics_weekly, function(x){sum(is.na(x))})
# NA values in No_of_Holidays column

ConsumerElectronics_weekly$No_of_Holidays <- ifelse(
  is.na(ConsumerElectronics_weekly$No_of_Holidays),
  0,
  ConsumerElectronics_weekly$No_of_Holidays
);


#------------------------------------ Merge MediaInvestment & NPS ---------------------------------------------------------------------------------

Media_NPS <- merge(Media_Investment, NPS_Score_data, by = 'Week', 
                   all.x = TRUE)

#------------------------------------- Merge Data & Media_NPS ------------------------------------------------------------------------------------

Merged_Data <- merge(ConsumerElectronics_weekly, Media_NPS, by = 'Week', all.x = TRUE)


#------------------------------------ Separate merged data for 3 sub categories ------------------------------------------------------------------

CameraAccessoryData <- Merged_Data %>%
  filter(product_analytic_sub_category %in% c('CameraAccessory'));
GamingAccessoryData <- Merged_Data %>%
  filter(product_analytic_sub_category %in% c('GamingAccessory'));
HomeAudioData <- Merged_Data %>%
  filter(product_analytic_sub_category %in% c('HomeAudio'));


# a) Lag can also be considered for Discount and selling price

# CameraAccessoryData
CameraAccessoryData <- slide(CameraAccessoryData, Var="Discount", slideBy = -1);
CameraAccessoryData <- slide(CameraAccessoryData, Var="Discount", slideBy = -2);
CameraAccessoryData <- slide(CameraAccessoryData, Var="Discount", slideBy = -3);
CameraAccessoryData <- slide(CameraAccessoryData, Var="SellingPrice", slideBy = -1);
CameraAccessoryData <- slide(CameraAccessoryData, Var="SellingPrice", slideBy = -2);
CameraAccessoryData <- slide(CameraAccessoryData, Var="SellingPrice", slideBy = -3);

# GamingAccessoryData
GamingAccessoryData <- slide(GamingAccessoryData, Var="Discount", slideBy = -1);
GamingAccessoryData <- slide(GamingAccessoryData, Var="Discount", slideBy = -2);
GamingAccessoryData <- slide(GamingAccessoryData, Var="Discount", slideBy = -3);
GamingAccessoryData <- slide(GamingAccessoryData, Var="SellingPrice", slideBy = -1);
GamingAccessoryData <- slide(GamingAccessoryData, Var="SellingPrice", slideBy = -2);
GamingAccessoryData <- slide(GamingAccessoryData, Var="SellingPrice", slideBy = -3);

# HomeAudioData
HomeAudioData <- slide(HomeAudioData, Var="Discount", slideBy = -1);
HomeAudioData <- slide(HomeAudioData, Var="Discount", slideBy = -2);
HomeAudioData <- slide(HomeAudioData, Var="Discount", slideBy = -3);
HomeAudioData <- slide(HomeAudioData, Var="SellingPrice", slideBy = -1);
HomeAudioData <- slide(HomeAudioData, Var="SellingPrice", slideBy = -2);
HomeAudioData <- slide(HomeAudioData, Var="SellingPrice", slideBy = -3);


# b) For Holidays we take the lead, to see if upcoming holidays have an impact on current sales.

# CameraAccessoryData
CameraAccessoryData <- slide(CameraAccessoryData, Var="IsHolidayWeek", slide = 1);
CameraAccessoryData <- slide(CameraAccessoryData, Var="IsHolidayWeek", slide = 2);

# GamingAccessoryData
GamingAccessoryData <- slide(GamingAccessoryData, Var="IsHolidayWeek", slide = 1);
GamingAccessoryData <- slide(GamingAccessoryData, Var="IsHolidayWeek", slide = 2);

# HomeAudioData
HomeAudioData <- slide(HomeAudioData, Var="IsHolidayWeek", slide = 1);
HomeAudioData <- slide(HomeAudioData, Var="IsHolidayWeek", slide = 2);



# c) For Discount and SellingPrice, we add variables of inflation. 
#    (How much Discount has varied within the past 3 weeks)

# CameraAccessoryData
CameraAccessoryData$Discount_Lag_1 <- (CameraAccessoryData$Discount - CameraAccessoryData$`Discount-1`) / CameraAccessoryData$`Discount-1`;
CameraAccessoryData$Discount_Lag_2 <- (CameraAccessoryData$Discount - CameraAccessoryData$`Discount-2`) / CameraAccessoryData$`Discount-2`;
CameraAccessoryData$Discount_Lag_3 <- (CameraAccessoryData$Discount - CameraAccessoryData$`Discount-3`) / CameraAccessoryData$`Discount-3`;

CameraAccessoryData$SP_Lag_1 <- (CameraAccessoryData$SellingPrice - CameraAccessoryData$`SellingPrice-1`) / CameraAccessoryData$`SellingPrice-1`;
CameraAccessoryData$SP_Lag_2 <- (CameraAccessoryData$SellingPrice - CameraAccessoryData$`SellingPrice-2`) / CameraAccessoryData$`SellingPrice-2`;
CameraAccessoryData$SP_Lag_3 <- (CameraAccessoryData$SellingPrice - CameraAccessoryData$`SellingPrice-2`) / CameraAccessoryData$`SellingPrice-3`;


# GamingAccessoryData
GamingAccessoryData$Discount_Lag_1 <- (GamingAccessoryData$Discount - GamingAccessoryData$`Discount-1`) / GamingAccessoryData$`Discount-1`;
GamingAccessoryData$Discount_Lag_2 <- (GamingAccessoryData$Discount - GamingAccessoryData$`Discount-2`) / GamingAccessoryData$`Discount-2`;
GamingAccessoryData$Discount_Lag_3 <- (GamingAccessoryData$Discount - GamingAccessoryData$`Discount-3`) / GamingAccessoryData$`Discount-3`;

GamingAccessoryData$SP_Lag_1 <- (GamingAccessoryData$SellingPrice - GamingAccessoryData$`SellingPrice-1`) / GamingAccessoryData$`SellingPrice-1`;
GamingAccessoryData$SP_Lag_2 <- (GamingAccessoryData$SellingPrice - GamingAccessoryData$`SellingPrice-2`) / GamingAccessoryData$`SellingPrice-2`;
GamingAccessoryData$SP_Lag_3 <- (GamingAccessoryData$SellingPrice - GamingAccessoryData$`SellingPrice-2`) / GamingAccessoryData$`SellingPrice-3`;


# HomeAudioData
HomeAudioData$Discount_Lag_1 <- (HomeAudioData$Discount - HomeAudioData$`Discount-1`) / HomeAudioData$`Discount-1`;
HomeAudioData$Discount_Lag_2 <- (HomeAudioData$Discount - HomeAudioData$`Discount-2`) / HomeAudioData$`Discount-2`;
HomeAudioData$Discount_Lag_3 <- (HomeAudioData$Discount - HomeAudioData$`Discount-3`) / HomeAudioData$`Discount-3`;

HomeAudioData$SP_Lag_1 <- (HomeAudioData$SellingPrice - HomeAudioData$`SellingPrice-1`) / HomeAudioData$`SellingPrice-1`;
HomeAudioData$SP_Lag_2 <- (HomeAudioData$SellingPrice - HomeAudioData$`SellingPrice-2`) / HomeAudioData$`SellingPrice-2`;
HomeAudioData$SP_Lag_3 <- (HomeAudioData$SellingPrice - HomeAudioData$`SellingPrice-2`) / HomeAudioData$`SellingPrice-3`;


# d) Removing variables created by slide

# CameraAccessoryData
CameraAccessoryData$`Discount-1` <- NULL;
CameraAccessoryData$`Discount-2` <- NULL;
CameraAccessoryData$`Discount-3` <- NULL;
CameraAccessoryData$`SellingPrice-1` <- NULL;
CameraAccessoryData$`SellingPrice-2` <- NULL;
CameraAccessoryData$`SellingPrice-3` <- NULL;

# GamingAccessoryData
GamingAccessoryData$`Discount-1` <- NULL;
GamingAccessoryData$`Discount-2` <- NULL;
GamingAccessoryData$`Discount-3` <- NULL;
GamingAccessoryData$`SellingPrice-1` <- NULL;
GamingAccessoryData$`SellingPrice-2` <- NULL;
GamingAccessoryData$`SellingPrice-3` <- NULL;

# HomeAudioData
HomeAudioData$`Discount-1` <- NULL;
HomeAudioData$`Discount-2` <- NULL;
HomeAudioData$`Discount-3` <- NULL;
HomeAudioData$`SellingPrice-1` <- NULL;
HomeAudioData$`SellingPrice-2` <- NULL;
HomeAudioData$`SellingPrice-3` <- NULL;

# e) Handling NA values
CameraAccessoryData[is.na(CameraAccessoryData)] <- 0;
GamingAccessoryData[is.na(GamingAccessoryData)] <- 0;
HomeAudioData[is.na(HomeAudioData)] <- 0;


#------------------------------------------ Calculate the Moving Avarage ------------------------------------------------------------------------ 

# a) Moving Average
CameraAccessoryData$MA_SellingPrice  <- movavg(CameraAccessoryData$SellingPrice, 3, type=c("s"))
CameraAccessoryData$MA_Discount <-  movavg(CameraAccessoryData$Discount, 3, type=c("s"))

GamingAccessoryData$MA_SellingPrice  <- movavg(GamingAccessoryData$SellingPrice, 3, type=c("s"))
GamingAccessoryData$MA_Discount <-  movavg(GamingAccessoryData$Discount, 3, type=c("s"))

HomeAudioData$MA_SellingPrice  <- movavg(HomeAudioData$SellingPrice, 3, type=c("s"))
HomeAudioData$MA_Discount <-  movavg(HomeAudioData$Discount, 3, type=c("s"))



# b) Adding incremental Moving average.
# To see how SellingPrice and Discount has increased/decreased from the avg of last 3 months.

CameraAccessoryData$MA_Increase_SP <- (CameraAccessoryData$SellingPrice - CameraAccessoryData$MA_SellingPrice) / CameraAccessoryData$MA_SellingPrice;
CameraAccessoryData$MA_Increase_Discount <- (CameraAccessoryData$Discount - CameraAccessoryData$MA_Discount) / CameraAccessoryData$MA_Discount;


GamingAccessoryData$MA_Increase_SP <- (GamingAccessoryData$SellingPrice - GamingAccessoryData$MA_SellingPrice) / GamingAccessoryData$MA_SellingPrice;
GamingAccessoryData$MA_Increase_Discount <- (GamingAccessoryData$Discount - GamingAccessoryData$MA_Discount) / GamingAccessoryData$MA_Discount;


HomeAudioData$MA_Increase_SP <- (HomeAudioData$SellingPrice - HomeAudioData$MA_SellingPrice) / HomeAudioData$MA_SellingPrice;
HomeAudioData$MA_Increase_Discount <- (HomeAudioData$Discount - HomeAudioData$MA_Discount) / HomeAudioData$MA_Discount;


#------------------------------------------ Creating CSV files for each category -------------------------------------------------------------------

write.csv(CameraAccessoryData,"CameraAccessoryData.csv")
write.csv(GamingAccessoryData,"GamingAccessoryData.csv")
write.csv(HomeAudioData,"HomeAudioData.csv")


#############################################################################################################################################
#------------------------------------------ Data Preparation Completed ---------------------------------------------------------------------- 
#############################################################################################################################################


