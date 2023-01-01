#############################################################################################################################################################
#------------------------------------------------ Exploratory Data Analysis ---------------------------------------------------------------------------------
#############################################################################################################################################################


# a) Install necessary packages

if(!require(gridExtra)) {
  install.packages("gridExtra")
}
if(!require(cowplot)) {
  install.packages("cowplot")
}

# b) Loading required library

library('gridExtra');
library("cowplot")


# c) Source the output from the Data Preperation File (files aggregated Weekly for individual categories)

CameraAccessoryData  <- read.csv('CameraAccessoryData.csv',stringsAsFactors = FALSE);
GamingAccessoryData  <- read.csv('GamingAccessoryData.csv',stringsAsFactors = FALSE);
HomeAudioData        <- read.csv('HomeAudioData.csv',stringsAsFactors = FALSE);

#############################################################################################################################################################
#----------------------------------------- 1. Marketing spend Across Channels ---------------------------------------------------------------------------------
#############################################################################################################################################################


# a) GamingAccessoryData Marketing Spend By Week 

matplot(
  GamingAccessoryData$Week,
  cbind(
    GamingAccessoryData$Digital_adstock,
    GamingAccessoryData$TV_adstock,
    GamingAccessoryData$Sponsorship_adstock,
    GamingAccessoryData$ContentMarketing_adstock,
    GamingAccessoryData$OnlineMarketing_adstock,
    GamingAccessoryData$Affiliates_adstock,
    GamingAccessoryData$SEM_adstock,
    GamingAccessoryData$Radio_adstock,
    GamingAccessoryData$Other_adstock
  ),
  type = 'l',
  lwd = 2,
  xlab = 'week',
  ylab = ' GamingAccessoryData Marketing Spend'
  
)
legend(
  'topright',
  inset = 0,
  legend = colnames(GamingAccessoryData[, c(16:24)]),
  lty = c(1:9),
  lwd = 2,
  col = c(1:9),
  cex = 0.5
)


# b) CameraAccessoryData Marketing Spend By Week 

matplot(
  CameraAccessoryData$Week,
  cbind(
    CameraAccessoryData$Digital_adstock,
    CameraAccessoryData$TV_adstock,
    CameraAccessoryData$Sponsorship_adstock,
    CameraAccessoryData$ContentMarketing_adstock,
    CameraAccessoryData$OnlineMarketing_adstock,
    CameraAccessoryData$Affiliates_adstock,
    CameraAccessoryData$SEM_adstock,
    CameraAccessoryData$Radio_adstock,
    CameraAccessoryData$Other_adstock
  ),
  type = 'l',
  lwd = 2,
  xlab = 'week',
  ylab = 'CameraAccessoryData Marketing Spend'
)
legend(
  'topright',
  inset = 0,
  legend = colnames(CameraAccessoryData[, c(16:24)]),
  lty = c(1:9),
  lwd = 2,
  col = c(1:9),
  cex = 0.5
)


# c) HomeAudioData Marketing Spend By Week 

matplot(
  HomeAudioData$Week,
  cbind(
    HomeAudioData$Digital_adstock,
    HomeAudioData$TV_adstock,
    HomeAudioData$Sponsorship_adstock,
    HomeAudioData$ContentMarketing_adstock,
    HomeAudioData$OnlineMarketing_adstock,
    HomeAudioData$Affiliates_adstock,
    HomeAudioData$SEM_adstock,
    HomeAudioData$Radio_adstock,
    HomeAudioData$Other_adstock
  ),
  type = 'l',
  lwd = 2,
  xlab = 'week',
  ylab = 'HomeAudioData Marketing Spend'
)
legend(
  'topright',
  inset = 0,
  legend = colnames(HomeAudioData[, c(16:24)]),
  lty = c(1:9),
  lwd = 2,
  col = c(1:9),
  cex = 0.5
)


#------------------------------------------------- Insights -------------------------------------------------------------------------------

#' A large portion of marketing spend goes towards sponsorships followed by online
#' *Same trend was observed for all Categories identified 
#'  During Week 10 to 20 : Observed a significant Marketing spend for all categories 
#'  During Week 30: Observed a increase in "other type" of Marketing Spend where all other types are dropped down  

#------------------------------------------------------------------------------------------------------------------------------------------


#############################################################################################################################################################
#------------------------------- 2. Detailed level of analysis at each level of Markating channels ----------------------------------------------------
#############################################################################################################################################################

# a) GamingAccessoryData Analyis 


# TV_adstock vs gmv
Gplot_1 <- ggplot(GamingAccessoryData, aes(x = TV_adstock, y = gmv)) + geom_line() +
  ggtitle("TV Adstock and GMV") +
  labs(x ="TV Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Digital_adstock vs gmv 
Gplot_2 <- ggplot(GamingAccessoryData, aes(x = Digital_adstock, y = gmv)) + geom_line() +
  ggtitle("Digital Adstock and GMV") +
  labs(x ="Digital Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Sponsorship_adstock vs gmv 
Gplot_3 <- ggplot(GamingAccessoryData, aes(x = Sponsorship_adstock, y = gmv)) + geom_line() +
  ggtitle("Sponsorship Adstock and GMV") +
  labs(x ="Sponsorship Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Content.Marketing_adstock vs gmv 
Gplot_4 <- ggplot(GamingAccessoryData, aes(x = GamingAccessoryData$Content.Marketing_adstock, y = gmv)) + geom_line() +
  ggtitle("Content Marketing Adstock and GMV") +
  labs(x ="Content Marketing Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Online.marketing_adstock vs gmv 
Gplot_5<- ggplot(GamingAccessoryData, aes(x = GamingAccessoryData$Online.marketing_adstock, y = gmv)) + geom_line() +
  ggtitle("Online Marketing Adstock and GMV") +
  labs(x ="Online Marketing Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Affiliates_adstock vs gmv 
Gplot_6<- ggplot(GamingAccessoryData, aes(x = Affiliates_adstock, y = gmv)) + geom_line() +
  ggtitle("Affiliates Adstock and GMV") +
  labs(x ="Affiliates Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# SEM_adstock vs gmv
Gplot_7<- ggplot(GamingAccessoryData, aes(x = SEM_adstock, y = gmv)) + geom_line() +
  ggtitle("SEM Adstock and GMV") +
  labs(x ="SEM Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Radio_adstock vs gmv 
Gplot_8 <- ggplot(GamingAccessoryData, aes(x = Radio_adstock, y = gmv)) + geom_line() +
  ggtitle("Radio Adstock and GMV") +
  labs(x ="Radio Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Other_adstock vs gmv
Gplot_9 <- ggplot(GamingAccessoryData, aes(x = Other_adstock, y = gmv)) + geom_line() +
  ggtitle("Other Adstock and GMV") +
  labs(x ="Other Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

#------------------------------------ Plot All GamingAccessoryData graphs in a Grid ------------------------------------------------------------------------

plot_grid(Gplot_1,Gplot_2,Gplot_3,Gplot_4,Gplot_5,Gplot_6,Gplot_7,Gplot_8,Gplot_9,hjust = 0, vjust = 1)


#############################################################################################################################################################
#------------------------------- 2. Detailed level of analysis at each level of Markating channels ----------------------------------------------------
#############################################################################################################################################################

# b) CameraAccessoryData Analyis 


# TV_adstock vs gmv
Cplot_1 <- ggplot(CameraAccessoryData, aes(x = TV_adstock, y = gmv)) + geom_line() +
  ggtitle("TV Adstock and GMV") +
  labs(x ="TV Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Digital_adstock vs gmv
Cplot_2 <- ggplot(CameraAccessoryData, aes(x = Digital_adstock, y = gmv)) + geom_line() +
  ggtitle("Digital Adstock and GMV") +
  labs(x ="Digital Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Sponsorship_adstock vs gmv 
Cplot_3 <- ggplot(CameraAccessoryData, aes(x = Sponsorship_adstock, y = gmv)) + geom_line() +
  ggtitle("Sponsorship Adstock and GMV") +
  labs(x ="Sponsorship Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Content.Marketing_adstock vs gmv 
Cplot_4 <- ggplot(CameraAccessoryData, aes(x = CameraAccessoryData$Content.Marketing_adstock, y = gmv)) + geom_line() +
  ggtitle("Content Marketing Adstock and GMV") +
  labs(x ="Content Marketing Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Online.marketing_adstock vs gmv
Cplot_5<- ggplot(CameraAccessoryData, aes(x = CameraAccessoryData$Online.marketing_adstock, y = gmv)) + geom_line() +
  ggtitle("Online Marketing Adstock and GMV") +
  labs(x ="Online Marketing Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Affiliates_adstock vs gmv 
Cplot_6<- ggplot(CameraAccessoryData, aes(x = Affiliates_adstock, y = gmv)) + geom_line() +
  ggtitle("Affiliates Adstock and GMV") +
  labs(x ="Affiliates Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# SEM_adstock vs gmv
Cplot_7<- ggplot(CameraAccessoryData, aes(x = SEM_adstock, y = gmv)) + geom_line() +
  ggtitle("SEM Adstock and GMV") +
  labs(x ="SEM Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Radio_adstock vs gmv 
Cplot_8 <- ggplot(CameraAccessoryData, aes(x = Radio_adstock, y = gmv)) + geom_line() +
  ggtitle("Radio Adstock and GMV") +
  labs(x ="Radio Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Other_adstock vs gmv 
Cplot_9 <- ggplot(CameraAccessoryData, aes(x = Other_adstock, y = gmv)) + geom_line() +
  ggtitle("Other Adstock and GMV") +
  labs(x ="Other Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 


#--------------------------------- Plot All CameraAccessoryData graphs in a Grid -------------------------------------------------------------------------


plot_grid(Cplot_1,Cplot_2,Cplot_3,Cplot_4,Cplot_5,Cplot_6,Cplot_7,Cplot_8,Cplot_9,hjust = 0, vjust = 1)



#############################################################################################################################################################
#------------------------------- 2. Detailed level of analysis at each level of Markating channels ----------------------------------------------------
#############################################################################################################################################################

# c) HomeAudioData Analyis 


# TV_adstock vs gmv
Hplot_1 <- ggplot(HomeAudioData, aes(x = TV_adstock, y = gmv)) + geom_line() +
  ggtitle("TV Adstock and GMV") +
  labs(x ="TV Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Digital_adstock vs gmv
Hplot_2 <- ggplot(HomeAudioData, aes(x = Digital_adstock, y = gmv)) + geom_line() +
  ggtitle("Digital Adstock and GMV") +
  labs(x ="Digital Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Sponsorship_adstock vs gmv 
Hplot_3 <- ggplot(HomeAudioData, aes(x = Sponsorship_adstock, y = gmv)) + geom_line() +
  ggtitle("Sponsorship Adstock and GMV") +
  labs(x ="Sponsorship Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Content.Marketing_adstock vs gmv 
Hplot_4 <- ggplot(HomeAudioData, aes(x = HomeAudioData$Content.Marketing_adstock, y = gmv)) + geom_line() +
  ggtitle("Content Marketing Adstock and GMV") +
  labs(x ="Content Marketing Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Online.marketing_adstock vs gmv
Hplot_5<- ggplot(HomeAudioData, aes(x = HomeAudioData$Online.marketing_adstock, y = gmv)) + geom_line() +
  ggtitle("Online Marketing Adstock and GMV") +
  labs(x ="Online Marketing Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Affiliates_adstock vs gmv 
Hplot_6<- ggplot(HomeAudioData, aes(x = Affiliates_adstock, y = gmv)) + geom_line() +
  ggtitle("Affiliates Adstock and GMV") +
  labs(x ="Affiliates Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# SEM_adstock vs gmv 
Hplot_7<- ggplot(HomeAudioData, aes(x = SEM_adstock, y = gmv)) + geom_line() +
  ggtitle("SEM Adstock and GMV") +
  labs(x ="SEM Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Radio_adstock vs gmv
Hplot_8 <- ggplot(HomeAudioData, aes(x = Radio_adstock, y = gmv)) + geom_line() +
  ggtitle("Radio Adstock and GMV") +
  labs(x ="Radio Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 

# Other_adstock vs gmv 
Hplot_9 <- ggplot(HomeAudioData, aes(x = Other_adstock, y = gmv)) + geom_line() +
  ggtitle("Other Adstock and GMV") +
  labs(x ="Other Adstock", y = "Gross Merchandise Value") +
  geom_smooth(method = "loess" ) 


#------------------------------------ Plot All Home Audio graphs in a Grid -----------------------------------------------------------------



plot_grid(Hplot_1,Hplot_2,Hplot_3,Hplot_4,Hplot_5,Hplot_6,Hplot_7,Hplot_8,Hplot_9,hjust = 0, vjust = 1)



####################################################################################################################################################
#-------------------------------------- 3. Discount vs Revenue/GMV  Analysis ------------------------------------------------------------------------------
####################################################################################################################################################

require(scales);
require(cowplot);

Dis_Gmv_1 <- ggplot(CameraAccessoryData, aes(y=gmv,x=Discount)) +
  geom_smooth(method = 'loess') + 
  scale_x_continuous(labels = comma , limits = c(40,90)) + scale_y_continuous(labels = comma )+
  ggtitle("Discount vs Revenue/ GMV CameraAccessoryData")

Dis_Gmv_2 <- ggplot(GamingAccessoryData, aes(y=gmv,x=Discount)) +
  geom_smooth(method = 'loess') + 
  scale_x_continuous(labels = comma , limits = c(30,90)) + scale_y_continuous(labels = comma)+
  ggtitle("Discount vs Revenue/ GMV GamingAccessoryData")

Dis_Gmv_3 <- ggplot(HomeAudioData, aes(y=gmv,x=Discount)) +
  geom_smooth(method = 'loess') + 
  scale_x_continuous(labels = comma , limits = c(30,50)) + scale_y_continuous(labels = comma)+
  ggtitle("Discount vs Revenue/ GMV HomeAudioData")

plot_grid(Dis_Gmv_1,Dis_Gmv_2,Dis_Gmv_3)


#------------------------------------------------- Insights ---------------------------------------------------------------------------

#' Revenue is maximum when discount is around 50%
#' Revenue increases linearly from discount 20% to 50%.
#' At a Discount point from 55-60 the Revenue droped down even the discount if high 
#' This Analysis shows customers are not really showing interest beyond a discount level 
#' More discount the revenue going down after a % of Discount 

#---------------------------------------------------------------------------------------------------------------------------------------


############################################################################################################################################################
#----------------------------------------- 4. No_of_orders vs Is Holiday Week. Analysis ---------------------------------------------------------------- 
############################################################################################################################################################


Holiday_Ord_C <- ggplot(CameraAccessoryData, aes(x=Week)) +
  geom_bar(aes(y=no_of_orders, fill=factor(IsHolidayWeek)), stat='identity' )+
  labs(fill='Is Holiday Week' , y= 'No Of Orders')+
  ggtitle("No_of_orders vs Is Holiday Week CameraAccessoryData")+ scale_x_continuous(breaks = c(10,20,30,40,50))+
  scale_y_continuous(labels = comma, limits=c(0, 11000))

Holiday_Ord_G <- ggplot(GamingAccessoryData, aes(x=Week)) +
  geom_bar(aes(y=no_of_orders, fill=factor(IsHolidayWeek)), stat='identity' )+
  labs(fill='Is Holiday Week' , y= 'No Of Orders')+
  ggtitle("No_of_orders vs Is Holiday Week GamingAccessoryData")+ scale_x_continuous(breaks = c(10,20,30,40,50))+
  scale_y_continuous(labels = comma, limits=c(0, 11000))

Holiday_Ord_H <- ggplot(HomeAudioData, aes(x=Week)) +
  geom_bar(aes(y=no_of_orders, fill=factor(IsHolidayWeek)), stat='identity' )+
  labs(fill='Is Holiday Week' , y= 'No Of Orders')+
  ggtitle("No_of_orders vs Is Holiday Week HomeAudioData")+ scale_x_continuous(breaks = c(10,20,30,40,50))+
  scale_y_continuous(labels = comma, limits=c(0, 8000))

plot_grid(Holiday_Ord_C,Holiday_Ord_G,Holiday_Ord_H)


###########################################################################################################################################################
#------------------------------------------ 5. Revenue / GMV  vs. Is Holiday Week. Analysis -------------------------------------------------------------
############################################################################################################################################################

Holiday_gmv_C <- ggplot(CameraAccessoryData, aes(x=Week)) +
  geom_bar(aes(y=gmv, fill=factor(IsHolidayWeek)), stat='identity')  +
  labs(fill='Is Holiday Week' , y= 'Revenue')+ scale_x_continuous(breaks = c(10,20,30,40,50))+
  scale_y_continuous(labels = comma, limits=c(0, 13000000))+
  ggtitle("Revenue/GMV vs Is Holiday Week CameraAccessoryData")

Holiday_gmv_G <- ggplot(GamingAccessoryData, aes(x=Week)) +
  geom_bar(aes(y=gmv, fill=factor(IsHolidayWeek)), stat='identity')  +
  labs(fill='Is Holiday Week' , y= 'Revenue')+ scale_x_continuous(breaks = c(10,20,30,40,50))+
  scale_y_continuous(labels = comma, limits=c(0, 8000000))+
  ggtitle("Revenue/GMV vs Is Holiday Week GamingAccessoryData")

Holiday_gmv_H <- ggplot(HomeAudioData, aes(x=Week)) +
  geom_bar(aes(y=gmv, fill=factor(IsHolidayWeek)), stat='identity')  +
  labs(fill='Is Holiday Week' , y= 'Revenue') + scale_x_continuous(breaks = c(10,20,30,40,50))+
  scale_y_continuous(labels = comma, limits=c(0, 20000000))+
  ggtitle("Revenue/GMV vs Is Holiday Week HomeAudioData")


plot_grid(Holiday_gmv_C,Holiday_gmv_G,Holiday_gmv_H)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------
