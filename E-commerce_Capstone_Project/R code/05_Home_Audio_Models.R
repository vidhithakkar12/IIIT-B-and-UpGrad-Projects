#############################################################################################################################################################
#------------------------------------------------ Home Audio - Model Building -------------------------------------------------------------------------
#############################################################################################################################################################

# All necessary packages have been installed in the data preparation file


############################################################################################################################################################
#------------------------------------------------ 1. Linear Model ------------------------------------------------------------------------------------------
############################################################################################################################################################

# a) Load Home Audio Data

HomeAudioData <- read.csv("HomeAudioData.csv", stringsAsFactors = FALSE)

str(HomeAudioData);
colnames(HomeAudioData);


# b) Removing Unnecessary variables

HomeAudioData$product_analytic_sub_category <- NULL;
HomeAudioData$X <- NULL;

HomeAudioData$Week <- NULL;
HomeAudioData$no_of_orders <- NULL;
HomeAudioData$units <- NULL;

HomeAudioData$COD <- NULL;
HomeAudioData$Prepaid <- NULL;
HomeAudioData$Mass_p <- NULL;
HomeAudioData$Middle_p <- NULL;
HomeAudioData$Premium_p <- NULL;

HomeAudioData$product_mrp <- NULL;
HomeAudioData$SellingPrice <- NULL;
HomeAudioData$MA_Discount <- NULL;
HomeAudioData$MA_SellingPrice <- NULL;


# c) Scaling all the variables for modelling

HomeAudioData$gmv <- scale(HomeAudioData$gmv);
HomeAudioData$Discount <- scale(HomeAudioData$Discount);
HomeAudioData$sla <- scale(HomeAudioData$sla);
HomeAudioData$procurement_sla <- scale(HomeAudioData$procurement_sla);

HomeAudioData$No_of_Holidays <- scale(HomeAudioData$No_of_Holidays)
HomeAudioData$deliverybdays <- scale(HomeAudioData$deliverybdays);

HomeAudioData$deliverycdays <- scale(HomeAudioData$deliverycdays);

HomeAudioData$TV_adstock <- scale(HomeAudioData$TV_adstock);
HomeAudioData$Digital_adstock <- scale(HomeAudioData$Digital_adstock);

HomeAudioData$Sponsorship_adstock <- scale(HomeAudioData$Sponsorship_adstock);
HomeAudioData$`Content.Marketing_adstock` <- scale(HomeAudioData$`Content.Marketing_adstock`);
HomeAudioData$`Online.marketing_adstock` <- scale(HomeAudioData$`Online.marketing_adstock`);

HomeAudioData$Affiliates_adstock <- scale(HomeAudioData$Affiliates_adstock);
HomeAudioData$SEM_adstock <- scale(HomeAudioData$SEM_adstock);
HomeAudioData$Radio_adstock <- scale(HomeAudioData$Radio_adstock);

HomeAudioData$Other_adstock <-scale(HomeAudioData$Other_adstock);
HomeAudioData$NPS_Score <-scale(HomeAudioData$NPS_Score); 
HomeAudioData$Discount_Lag_1 <- scale(HomeAudioData$Discount_Lag_1);
HomeAudioData$Discount_Lag_2 <- scale(HomeAudioData$Discount_Lag_2);
HomeAudioData$Discount_Lag_3 <- scale(HomeAudioData$Discount_Lag_3);

HomeAudioData$SP_Lag_1 <- scale(HomeAudioData$SP_Lag_1);
HomeAudioData$SP_Lag_2 <- scale(HomeAudioData$SP_Lag_2);
HomeAudioData$SP_Lag_3 <- scale(HomeAudioData$SP_Lag_3);

HomeAudioData$MA_Increase_SP <- scale(HomeAudioData$MA_Increase_SP);
HomeAudioData$MA_Increase_Discount <- scale(HomeAudioData$MA_Increase_Discount);

# d) Modelling

model_1 <- lm(gmv~.,HomeAudioData)

model_2 <- stepAIC(model_1,direction = "both")

model_2
summary(model_2);
vif(model_2);
#	Adjusted R-squared:  0.743 


#MA_Increase_Discount, vif - 304.19, p value - 0.09300 .
#Removing it.
model_3 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                TV_adstock + Affiliates_adstock + SEM_adstock + Radio_adstock + 
                Other_adstock + NPS_Score + Discount_Lag_1 + Discount_Lag_2 + 
                SP_Lag_1 + SP_Lag_2 + MA_Increase_SP, 
              data = HomeAudioData);

summary(model_3);
vif(model_3);
#	Adjusted R-squared:  0.728


#MA_Increase_SP, vif - 298.40, p value -  0.01177 *
#Removing it.
model_4 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                TV_adstock + Affiliates_adstock + SEM_adstock + Radio_adstock + 
                Other_adstock + NPS_Score + Discount_Lag_1 + Discount_Lag_2 + 
                SP_Lag_1 + SP_Lag_2, 
              data = HomeAudioData);

summary(model_4);
vif(model_4);
#	Adjusted R-squared:  0.681

#Affiliates_adstock, vif - 12.48, p value -  0.03381 *
#Removing it.
model_5 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                TV_adstock +  SEM_adstock + Radio_adstock + 
                Other_adstock + NPS_Score + Discount_Lag_1 + Discount_Lag_2 + 
                SP_Lag_1 + SP_Lag_2, 
              data = HomeAudioData);

summary(model_5);
vif(model_5);
#	Adjusted R-squared:  0.646

#Radio_adstock, vif - 12.48, p value -   0.00036 ***
#Removing it.
model_6 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                TV_adstock +  SEM_adstock +
                Other_adstock + NPS_Score + Discount_Lag_1 + Discount_Lag_2 + 
                SP_Lag_1 + SP_Lag_2, 
              data = HomeAudioData);

summary(model_6);
vif(model_6);
#	Adjusted R-squared:  0.508

#SP_Lag_2,  p value -   0.892 
#Removing it.
model_7 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                TV_adstock +  SEM_adstock +
                Other_adstock + NPS_Score + Discount_Lag_1 + Discount_Lag_2 + 
                SP_Lag_1 , 
              data = HomeAudioData);

summary(model_7);
vif(model_7);
#	Adjusted R-squared:  0.521

#Discount_Lag_1,  p value - 0.8399   
#Removing it.
model_8 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                TV_adstock +  SEM_adstock +
                Other_adstock + NPS_Score + Discount_Lag_2 + 
                SP_Lag_1 , 
              data = HomeAudioData);

summary(model_8);
vif(model_8);
#	Adjusted R-squared:  0.532

#TV_adstock,  p value - 0.7437    
#Removing it.
model_9 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                 SEM_adstock +
                Other_adstock + NPS_Score + Discount_Lag_2 + 
                SP_Lag_1 , 
              data = HomeAudioData);

summary(model_9);
vif(model_9);
#	Adjusted R-squared:  0.543


#Other_adstock,  p value -  0.7039 
#Removing it.
model_10 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                SEM_adstock + NPS_Score + Discount_Lag_2 + 
                SP_Lag_1 , 
              data = HomeAudioData);

summary(model_10);
vif(model_10);
#	Adjusted R-squared:  0.552

#deliverybdays,  p value -  0.6336  
#Removing it.
model_11 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + 
                 SEM_adstock + NPS_Score + Discount_Lag_2 + 
                 SP_Lag_1 , 
               data = HomeAudioData);

summary(model_11);
vif(model_11);
#	Adjusted R-squared:  0.561


#Discount_Lag_2,  p value -   0.6086   
#Removing it.
model_12 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + 
                 SEM_adstock + NPS_Score + 
                 SP_Lag_1 , 
               data = HomeAudioData);

summary(model_12);
vif(model_12);
#	Adjusted R-squared:  0.568


#No_of_Holidays,  p value -  0.2730    
#Removing it.
model_13 <- lm(formula = gmv ~ Discount + sla + 
                 SEM_adstock + NPS_Score + 
                 SP_Lag_1 , 
               data = HomeAudioData);

summary(model_13);
vif(model_13);
#	Adjusted R-squared:  0.566


#NPS_Score,  p value - 0.2335     
#Removing it.
model_14 <- lm(formula = gmv ~ Discount + sla + 
                 SEM_adstock + 
                 SP_Lag_1 , 
               data = HomeAudioData);

summary(model_14);
vif(model_14);
#	Adjusted R-squared:  0.561

#sla,  p value - 0.0177 *   
#Removing it.
model_15 <- lm(formula = gmv ~ Discount + 
                 SEM_adstock + 
                 SP_Lag_1 , 
               data = HomeAudioData);

summary(model_15);
vif(model_15);
#	Adjusted R-squared:  0.513


#SP_Lag_1,  p value -  0.02371 *   
#Removing it.
model_16 <- lm(formula = gmv ~ Discount + 
                 SEM_adstock ,
               data = HomeAudioData);

summary(model_16);
vif(model_16);
#	Adjusted R-squared:  0.467



final_model <- model_16;
# Final Adjusted R square - 0.467


#----------------------------------------------------------------------------------------------------------------------------------


# 10 fold cross validation
temp_crossval <- cv.lm(data = HomeAudioData, form.lm = formula(gmv ~ Discount + 
                                                                 SEM_adstock 
                                                                  ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval, "ms")
# 0.754


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(final_model$coefficients[var]*
                       mean(HomeAudioData[,var])/mean(HomeAudioData$gmv))
  
  return(elax1)
} 


Home_Var_Elasticity <- data.frame(names(final_model$coefficients)[-1]);

for (i in 2:length(final_model$coefficients)) {
  Home_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(final_model$coefficients)[i]);
}
Home_Var_Elasticity$Direction <- ifelse(Home_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Home_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Home_Var_Elasticity


ggplot(data=Home_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Home audio Accessory - Linear Model") +xlab("Variables")



################################################################################################################################################
#----------------------------------------- 2. Multiplicative Model --------------------------------------------------------------------------------
################################################################################################################################################


# a) Load Home Audio Data
HomeAudio_Mul <- read.csv('HomeAudioData.csv',stringsAsFactors = FALSE);

# b) Removing Unnecessary variables 
HomeAudio_Mul$product_analytic_sub_category <- NULL;
HomeAudio_Mul$X <- NULL;

HomeAudio_Mul$Week <- NULL;
HomeAudio_Mul$no_of_orders <- NULL;
HomeAudio_Mul$units <- NULL;

HomeAudio_Mul$COD <- NULL;
HomeAudio_Mul$Prepaid <- NULL;
HomeAudio_Mul$Mass_p <- NULL;
HomeAudio_Mul$Middle_p <- NULL;
HomeAudio_Mul$Premium_p <- NULL;

HomeAudio_Mul$product_mrp <- NULL;
HomeAudio_Mul$SellingPrice <- NULL;
HomeAudio_Mul$MA_Discount <- NULL;
HomeAudio_Mul$MA_SellingPrice <- NULL;


# c) Removing 0 values in data changing from 0 to 0.01. As log 0 will be undefined.

# Looking for 0 values in data
sapply(HomeAudio_Mul, function(x){sum(x==0)})

HomeAudio_Mul[HomeAudio_Mul == 0] <- 0.001;


sapply(HomeAudio_Mul, function(x){sum(x<0)})
#There are negative values.

# For negative values we apply the following transaformation -> -log(-x + 1) , Yeo-Johnson Power Transformations

for(i in 1:ncol(HomeAudio_Mul)) {
  HomeAudio_Mul[,c(i)]<- ifelse(
    HomeAudio_Mul[,c(i)]>0,
    log(HomeAudio_Mul[,c(i)]), 
    -log(-(HomeAudio_Mul[,c(i)])+1)
  );
}


# d) Modelling

m_model_1 <- lm(gmv ~ ., HomeAudio_Mul);

m_model_2 <-stepAIC(m_model_1,direction = "both")
summary(m_model_2);

#Adjusted R-squared:  0.66 
vif(m_model_2);


#Radio_adstock, vif  - 0.10584  , p value- 0.10584 
m_model_3 <- lm(formula = gmv ~ Discount + sla + procurement_sla + deliverycdays + 
                  TV_adstock + Digital_adstock + Sponsorship_adstock + Online.marketing_adstock + 
                  Affiliates_adstock +  Other_adstock + NPS_Score + 
                  IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_2, data = HomeAudio_Mul);

summary(m_model_3);

#Adjusted R-squared:  0.643
vif(m_model_3);

#Online.marketing_adstock , vif  - 750.80,  p value - 0.00197 ** 
#Removing it
m_model_4 <- lm(formula = gmv ~ Discount + sla + procurement_sla + deliverycdays + 
                  TV_adstock + Digital_adstock + Sponsorship_adstock + 
                  Affiliates_adstock +  Other_adstock + NPS_Score + 
                  IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_2, data = HomeAudio_Mul);

summary(m_model_4);

#Adjusted R-squared:  0.542
vif(m_model_4);

#Sponsorship_adstock , vif  - 9.39,  p value -  0.3995 
#Removing it
m_model_5 <- lm(formula = gmv ~ Discount + sla + procurement_sla + deliverycdays + 
                  TV_adstock + Digital_adstock + 
                  Affiliates_adstock +  Other_adstock + NPS_Score + 
                  IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_2, data = HomeAudio_Mul);

summary(m_model_5);

#Adjusted R-squared:  0.545
vif(m_model_5);


#Affiliates_adstock , vif  - 21.00,  p value - 0.0604 .
#Removing it
m_model_6 <- lm(formula = gmv ~ Discount + sla + procurement_sla + deliverycdays + 
                  TV_adstock + Digital_adstock + 
                   Other_adstock + NPS_Score + 
                  IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_2, data = HomeAudio_Mul);

summary(m_model_6);

#Adjusted R-squared:  0.512
vif(m_model_6);

#Discount_Lag_2  ,  p value -0.7412
#Removing it
m_model_7 <- lm(formula = gmv ~ Discount + sla + procurement_sla + deliverycdays + 
                  TV_adstock + Digital_adstock + 
                  Other_adstock + NPS_Score + 
                  IsHolidayWeek1 + IsHolidayWeek2 , data = HomeAudio_Mul);

summary(m_model_7);

#Adjusted R-squared:  0.524
vif(m_model_7);

#IsHolidayWeek1  ,  p value -0.6937  
#Removing it
m_model_8 <- lm(formula = gmv ~ Discount + sla + procurement_sla + deliverycdays + 
                  TV_adstock + Digital_adstock + 
                  Other_adstock + NPS_Score + 
                   IsHolidayWeek2 , data = HomeAudio_Mul);

summary(m_model_8);

#Adjusted R-squared:  0.534
vif(m_model_8);

#deliverycdays  ,  p value -0.6149 
#Removing it
m_model_9 <- lm(formula = gmv ~ Discount + sla + procurement_sla + 
                  TV_adstock + Digital_adstock + 
                  Other_adstock + NPS_Score + 
                  IsHolidayWeek2 , data = HomeAudio_Mul);

summary(m_model_9);

#Adjusted R-squared:  0.542
vif(m_model_9);

#procurement_sla  ,  p value -0.28035 
#Removing it
m_model_10 <- lm(formula = gmv ~ Discount + sla + 
                  TV_adstock + Digital_adstock + 
                  Other_adstock + NPS_Score + 
                  IsHolidayWeek2 , data = HomeAudio_Mul);

summary(m_model_10);

#Adjusted R-squared:  0.54
vif(m_model_10);

#IsHolidayWeek2  ,  p value -0.24972   
#Removing it
m_model_11 <- lm(formula = gmv ~ Discount + sla + 
                   TV_adstock + Digital_adstock + 
                   Other_adstock + NPS_Score 
                   , data = HomeAudio_Mul);

summary(m_model_11);

#Adjusted R-squared:  0.536
vif(m_model_11);

#TV_adstock  ,  p value -0.19147    
#Removing it
m_model_12 <- lm(formula = gmv ~ Discount + sla + 
                   Digital_adstock + 
                   Other_adstock + NPS_Score 
                 , data = HomeAudio_Mul);

summary(m_model_12);

#Adjusted R-squared:  0.528
vif(m_model_12);

#Digital_adstock  ,  p value -0.10473  
#Removing it
m_model_13 <- lm(formula = gmv ~ Discount + sla + 
                   Other_adstock + NPS_Score 
                 , data = HomeAudio_Mul);

summary(m_model_13);

#Adjusted R-squared:  0.51
vif(m_model_13);


Final_mul_Home_Model <- m_model_13;
#Final Adjusted R square - 0.51

#--------------------------------------------------------------------------------------------------------------------------------------------

# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = HomeAudio_Mul, form.lm = formula( gmv ~ Discount + sla + 
                                                                    Other_adstock + NPS_Score  ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval_1, "ms")
# 0.199


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_mul_Home_Model$coefficients[var]*
                       mean(HomeAudio_Mul[,var])/mean(HomeAudio_Mul$gmv))
  
  return(elax1)
} 


Home_mul_Var_Elasticity <- data.frame(names(Final_mul_Home_Model$coefficients)[-1]);

for (i in 2:length(Final_mul_Home_Model$coefficients)) {
  Home_mul_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_mul_Home_Model$coefficients)[i]);
}
Home_mul_Var_Elasticity$Direction <- ifelse(Home_mul_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Home_mul_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Home_mul_Var_Elasticity


ggplot(data=Home_mul_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Home Audio  - Multplicative Model") +xlab("Variables")




############################################################################################################################################
#-------------------------------------------- 3. Koyck model ------------------------------------------------------------------------------
############################################################################################################################################


# a) Load Home Audio Data
HomeAudioData_Koyck <- read.csv('HomeAudioData.csv',stringsAsFactors = FALSE);


HomeAudioData_Koyck <- slide(HomeAudioData_Koyck, Var = "gmv",slideBy = -1)

str(HomeAudioData_Koyck);
colnames(HomeAudioData_Koyck);


# b) Removing Unnecessary variables 
HomeAudioData_Koyck$product_analytic_sub_category <- NULL;
HomeAudioData_Koyck$X <- NULL;

HomeAudioData_Koyck$Week <- NULL;
HomeAudioData_Koyck$no_of_orders <- NULL;
HomeAudioData_Koyck$units <- NULL;

HomeAudioData_Koyck$COD <- NULL;
HomeAudioData_Koyck$Prepaid <- NULL;
HomeAudioData_Koyck$Mass_p <- NULL;
HomeAudioData_Koyck$Middle_p <- NULL;
HomeAudioData_Koyck$Premium_p <- NULL;

HomeAudioData_Koyck$product_mrp <- NULL;
HomeAudioData_Koyck$SellingPrice <- NULL;
HomeAudioData_Koyck$MA_Discount <- NULL;
HomeAudioData_Koyck$MA_SellingPrice <- NULL;


# c) Scaling all the variables for modelling

HomeAudioData_Koyck$gmv <- scale(HomeAudioData_Koyck$gmv);
HomeAudioData_Koyck$Discount <- scale(HomeAudioData_Koyck$Discount);
HomeAudioData_Koyck$sla <- scale(HomeAudioData_Koyck$sla);
HomeAudioData_Koyck$procurement_sla <- scale(HomeAudioData_Koyck$procurement_sla);

HomeAudioData_Koyck$No_of_Holidays <- scale(HomeAudioData_Koyck$No_of_Holidays)
HomeAudioData_Koyck$deliverybdays <- scale(HomeAudioData_Koyck$deliverybdays);

HomeAudioData_Koyck$deliverycdays <- scale(HomeAudioData_Koyck$deliverycdays);

HomeAudioData_Koyck$TV_adstock <- scale(HomeAudioData_Koyck$TV_adstock);
HomeAudioData_Koyck$Digital_adstock <- scale(HomeAudioData_Koyck$Digital_adstock);

HomeAudioData_Koyck$Sponsorship_adstock <- scale(HomeAudioData_Koyck$Sponsorship_adstock);
HomeAudioData_Koyck$`Content.Marketing_adstock` <- scale(HomeAudioData_Koyck$`Content.Marketing_adstock`);
HomeAudioData_Koyck$`Online.marketing_adstock` <- scale(HomeAudioData_Koyck$`Online.marketing_adstock`);

HomeAudioData_Koyck$Affiliates_adstock <- scale(HomeAudioData_Koyck$Affiliates_adstock);
HomeAudioData_Koyck$SEM_adstock <- scale(HomeAudioData_Koyck$SEM_adstock);
HomeAudioData_Koyck$Radio_adstock <- scale(HomeAudioData_Koyck$Radio_adstock);

HomeAudioData_Koyck$Other_adstock <-scale(HomeAudioData_Koyck$Other_adstock);
HomeAudioData_Koyck$NPS_Score <-scale(HomeAudioData_Koyck$NPS_Score); 
HomeAudioData_Koyck$Discount_Lag_1 <- scale(HomeAudioData_Koyck$Discount_Lag_1);
HomeAudioData_Koyck$Discount_Lag_2 <- scale(HomeAudioData_Koyck$Discount_Lag_2);
HomeAudioData_Koyck$Discount_Lag_3 <- scale(HomeAudioData_Koyck$Discount_Lag_3);

HomeAudioData_Koyck$SP_Lag_1 <- scale(HomeAudioData_Koyck$SP_Lag_1);
HomeAudioData_Koyck$SP_Lag_2 <- scale(HomeAudioData_Koyck$SP_Lag_2);
HomeAudioData_Koyck$SP_Lag_3 <- scale(HomeAudioData_Koyck$SP_Lag_3);

HomeAudioData_Koyck$MA_Increase_SP <- scale(HomeAudioData_Koyck$MA_Increase_SP);
HomeAudioData_Koyck$MA_Increase_Discount <- scale(HomeAudioData_Koyck$MA_Increase_Discount);
HomeAudioData_Koyck$`gmv-1` <- scale(HomeAudioData_Koyck$`gmv-1`);


# d) Modelling

Home_Koyck_Model_1 <- lm(gmv~.,HomeAudioData_Koyck);

Home_Koyck_Model_2 <- stepAIC(Home_Koyck_Model_1,direction = "both")
summary(Home_Koyck_Model_2);
vif(Home_Koyck_Model_2);

#Adjusted R-squared:  0.788

#SP_Lag_2, vif - 164.43, p value - 0.18754   
#Removing it.

Home_Koyck_Model_3 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                           TV_adstock + Digital_adstock + Online.marketing_adstock + 
                           Radio_adstock + Other_adstock + NPS_Score + Discount_Lag_1 + 
                           Discount_Lag_2 + SP_Lag_1 +  SP_Lag_3 + MA_Increase_SP + 
                           `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_3);
vif(Home_Koyck_Model_3);
#	Adjusted R-squared:  0.782 

#MA_Increase_SP, vif - 154.27, p value - 0.00461 **   
#Removing it.

Home_Koyck_Model_4 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                           TV_adstock + Digital_adstock + Online.marketing_adstock + 
                           Radio_adstock + Other_adstock + NPS_Score + Discount_Lag_1 + 
                           Discount_Lag_2 + SP_Lag_1 +  SP_Lag_3 +  
                           `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_4);
vif(Home_Koyck_Model_4);
#	Adjusted R-squared:  0.728

#Radio_adstock, vif - 103.91, p value - 3.7e-05 ***
#Removing it.

Home_Koyck_Model_5 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                           TV_adstock + Digital_adstock + Online.marketing_adstock + 
                           Other_adstock + NPS_Score + Discount_Lag_1 + 
                           Discount_Lag_2 + SP_Lag_1 +  SP_Lag_3 +  
                           `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_5);
vif(Home_Koyck_Model_5);
#	Adjusted R-squared:  0.554


#Online.marketing_adstock, vif - 8.05, p value - 0.3758 
#Removing it.

Home_Koyck_Model_6 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                           TV_adstock + Digital_adstock + 
                           Other_adstock + NPS_Score + Discount_Lag_1 + 
                           Discount_Lag_2 + SP_Lag_1 +  SP_Lag_3 +  
                           `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_6);
vif(Home_Koyck_Model_6);
#	Adjusted R-squared:  0.557


#SP_Lag_3,  p value - 0.7417 
#Removing it.

Home_Koyck_Model_7 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays + deliverybdays + 
                           TV_adstock + Digital_adstock + 
                           Other_adstock + NPS_Score + Discount_Lag_1 + 
                           Discount_Lag_2 + SP_Lag_1 +   
                           `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_7);
vif(Home_Koyck_Model_7);
#	Adjusted R-squared:  0.568


#deliverybdays,  p value - 0.53075  
#Removing it.

Home_Koyck_Model_8 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays +
                           TV_adstock + Digital_adstock + 
                           Other_adstock + NPS_Score + Discount_Lag_1 + 
                           Discount_Lag_2 + SP_Lag_1 +   
                           `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_8);
vif(Home_Koyck_Model_8);
#	Adjusted R-squared:  0.575


#NPS_Score,  p value -  0.55104   
#Removing it.

Home_Koyck_Model_9 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays +
                           TV_adstock + Digital_adstock + 
                           Other_adstock +  Discount_Lag_1 + 
                           Discount_Lag_2 + SP_Lag_1 +   
                           `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_9);
vif(Home_Koyck_Model_9);
#	Adjusted R-squared:  0.582


#TV_adstock,  p value - 0.42195   
#Removing it.

Home_Koyck_Model_10 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays +
                           Digital_adstock + 
                           Other_adstock +  Discount_Lag_1 + 
                           Discount_Lag_2 + SP_Lag_1 +   
                           `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_10);
vif(Home_Koyck_Model_10);
#	Adjusted R-squared:  0.585


#Other_adstock,  p value - 0.52135     
#Removing it.

Home_Koyck_Model_11 <- lm(formula = gmv ~ Discount + sla + No_of_Holidays +
                            Digital_adstock + 
                             Discount_Lag_1 + 
                            Discount_Lag_2 + SP_Lag_1 +   
                            `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_11);
vif(Home_Koyck_Model_11);
#	Adjusted R-squared:  0.591



#No_of_Holidays,  p value - 0.31951  
#Removing it.

Home_Koyck_Model_12 <- lm(formula = gmv ~ Discount + sla + 
                            Digital_adstock + 
                            Discount_Lag_1 + 
                            Discount_Lag_2 + SP_Lag_1 +   
                            `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_12);
vif(Home_Koyck_Model_12);
#	Adjusted R-squared:  0.591


#Discount_Lag_2,  p value - 0.28021  
#Removing it.

Home_Koyck_Model_13<- lm(formula = gmv ~ Discount + sla + 
                            Digital_adstock + 
                            Discount_Lag_1 + 
                             SP_Lag_1 +   
                            `gmv-1`, data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_13);
vif(Home_Koyck_Model_13);
#	Adjusted R-squared:  0.589

#`gmv-1`,  p value -  0.05217 . 
#Removing it.

Home_Koyck_Model_14 <- lm(formula = gmv ~ Discount + sla + 
                           Digital_adstock + 
                           Discount_Lag_1 + 
                           SP_Lag_1 , data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_14);
vif(Home_Koyck_Model_14);
#	Adjusted R-squared:  0.572


#Discount_Lag_1,  p value -  0.19732  
#Removing it.

Home_Koyck_Model_15 <- lm(formula = gmv ~ Discount + sla + 
                            Digital_adstock +
                            SP_Lag_1 , data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_15);
vif(Home_Koyck_Model_15);
#	Adjusted R-squared:  0.565


#sla,  p value - 0.03985 *  
#Removing it.

Home_Koyck_Model_16 <- lm(formula = gmv ~ Discount + 
                            Digital_adstock +
                            SP_Lag_1 , data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_16);
vif(Home_Koyck_Model_16);
#	Adjusted R-squared:  0.532


#SP_Lag_1,  p value -0.025 *  
#Removing it.

Home_Koyck_Model_17 <- lm(formula = gmv ~ Discount + 
                            Digital_adstock 
                            , data = HomeAudioData_Koyck);

summary(Home_Koyck_Model_17);
vif(Home_Koyck_Model_17);
#	Adjusted R-squared:  0.489




Final_Home_koyck_model <- Home_Koyck_Model_17;
#Final Adjusted R square = 0.489


#--------------------------------------------------------------------------------------------------------------------------------------------------------

# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = HomeAudioData_Koyck, form.lm = formula(   gmv ~ Discount + Digital_adstock),m = 10, plotit=FALSE)

# Mean Square Error
attr(temp_crossval_1, "ms")
# 0.744


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Home_koyck_model$coefficients[var]*
                       mean(HomeAudioData_Koyck[,var])/mean(HomeAudioData_Koyck$gmv))
  
  return(elax1)
} 


Home_koyck_Var_Elasticity <- data.frame(names(Final_Home_koyck_model$coefficients)[-1]);

for (i in 2:length(Final_Home_koyck_model$coefficients)) {
  Home_koyck_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Home_koyck_model$coefficients)[i]);
}
Home_koyck_Var_Elasticity$Direction <- ifelse(Home_koyck_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Home_koyck_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Home_koyck_Var_Elasticity


ggplot(data=Home_koyck_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Home Audio - Koyck Model") +xlab("Variables")



############################################################################################################################################
#---------------------------------------- 4. Distributed Lag Model -----------------------------------------------------------------------
#############################################################################################################################################


# a) Load Home Audio Data

HomeAudioData_Dis <- read.csv('HomeAudioData.csv',stringsAsFactors = FALSE);

HomeAudioData_Dis <- slide(HomeAudioData_Dis, Var = "gmv",slideBy = -1)
HomeAudioData_Dis <- slide(HomeAudioData_Dis, Var = "gmv",slideBy = -2)
HomeAudioData_Dis <- slide(HomeAudioData_Dis, Var = "gmv",slideBy = -3)

HomeAudioData_Dis <- slide(HomeAudioData_Dis, Var = "SellingPrice",slideBy = -1)
HomeAudioData_Dis <- slide(HomeAudioData_Dis, Var = "SellingPrice",slideBy = -2)
HomeAudioData_Dis <- slide(HomeAudioData_Dis, Var = "SellingPrice",slideBy = -3)

HomeAudioData_Dis <- slide(HomeAudioData_Dis, Var = "Discount",slideBy = -1)
HomeAudioData_Dis <- slide(HomeAudioData_Dis, Var = "Discount",slideBy = -2)
HomeAudioData_Dis <- slide(HomeAudioData_Dis, Var = "Discount",slideBy = -3)

str(HomeAudioData_Dis);
colnames(HomeAudioData_Dis);
HomeAudioData_Dis <- na.omit(HomeAudioData_Dis)


# b) Removing Unnecessary variables 

HomeAudioData_Dis$product_analytic_sub_category <- NULL;
HomeAudioData_Dis$X <- NULL;

HomeAudioData_Dis$Week <- NULL;
HomeAudioData_Dis$no_of_orders <- NULL;
HomeAudioData_Dis$units <- NULL;

HomeAudioData_Dis$COD <- NULL;
HomeAudioData_Dis$Prepaid <- NULL;
HomeAudioData_Dis$Mass_p <- NULL;
HomeAudioData_Dis$Middle_p <- NULL;
HomeAudioData_Dis$Premium_p <- NULL;

HomeAudioData_Dis$product_mrp <- NULL;
HomeAudioData_Dis$MA_Discount <- NULL;
HomeAudioData_Dis$MA_SellingPrice <- NULL;


# c) Scaling all the variables for modelling

HomeAudioData_Dis$gmv <- scale(HomeAudioData_Dis$gmv);
HomeAudioData_Dis$Discount <- scale(HomeAudioData_Dis$Discount);
HomeAudioData_Dis$`Discount-1` <- scale(HomeAudioData_Dis$`Discount-1`);
HomeAudioData_Dis$`Discount-2` <- scale(HomeAudioData_Dis$`Discount-2`);
HomeAudioData_Dis$`Discount-3` <- scale(HomeAudioData_Dis$`Discount-3`);

HomeAudioData_Dis$sla <- scale(HomeAudioData_Dis$sla);
HomeAudioData_Dis$procurement_sla <- scale(HomeAudioData_Dis$procurement_sla);

HomeAudioData_Dis$No_of_Holidays <- scale(HomeAudioData_Dis$No_of_Holidays)
HomeAudioData_Dis$deliverybdays <- scale(HomeAudioData_Dis$deliverybdays);

HomeAudioData_Dis$deliverycdays <- scale(HomeAudioData_Dis$deliverycdays);

HomeAudioData_Dis$TV_adstock <- scale(HomeAudioData_Dis$TV_adstock);
HomeAudioData_Dis$Digital_adstock <- scale(HomeAudioData_Dis$Digital_adstock);

HomeAudioData_Dis$Sponsorship_adstock <- scale(HomeAudioData_Dis$Sponsorship_adstock);
HomeAudioData_Dis$`Content.Marketing_adstock` <- scale(HomeAudioData_Dis$`Content.Marketing_adstock`);
HomeAudioData_Dis$`Online.marketing_adstock` <- scale(HomeAudioData_Dis$`Online.marketing_adstock`);

HomeAudioData_Dis$Affiliates_adstock <- scale(HomeAudioData_Dis$Affiliates_adstock);
HomeAudioData_Dis$SEM_adstock <- scale(HomeAudioData_Dis$SEM_adstock);
HomeAudioData_Dis$Radio_adstock <- scale(HomeAudioData_Dis$Radio_adstock);

HomeAudioData_Dis$Other_adstock <-scale(HomeAudioData_Dis$Other_adstock);
HomeAudioData_Dis$NPS_Score <-scale(HomeAudioData_Dis$NPS_Score); 
HomeAudioData_Dis$Discount_Lag_1 <- scale(HomeAudioData_Dis$Discount_Lag_1);
HomeAudioData_Dis$Discount_Lag_2 <- scale(HomeAudioData_Dis$Discount_Lag_2);
HomeAudioData_Dis$Discount_Lag_3 <- scale(HomeAudioData_Dis$Discount_Lag_3);

HomeAudioData_Dis$SP_Lag_1 <- scale(HomeAudioData_Dis$SP_Lag_1);
HomeAudioData_Dis$SP_Lag_2 <- scale(HomeAudioData_Dis$SP_Lag_2);
HomeAudioData_Dis$SP_Lag_3 <- scale(HomeAudioData_Dis$SP_Lag_3);

HomeAudioData_Dis$MA_Increase_SP <- scale(HomeAudioData_Dis$MA_Increase_SP);
HomeAudioData_Dis$MA_Increase_Discount <- scale(HomeAudioData_Dis$MA_Increase_Discount);
HomeAudioData_Dis$`gmv-1` <- scale(HomeAudioData_Dis$`gmv-1`);
HomeAudioData_Dis$`gmv-2` <- scale(HomeAudioData_Dis$`gmv-2`);
HomeAudioData_Dis$`gmv-3` <- scale(HomeAudioData_Dis$`gmv-3`);

HomeAudioData_Dis$SellingPrice <- scale(HomeAudioData_Dis$SellingPrice);
HomeAudioData_Dis$`SellingPrice-1` <- scale(HomeAudioData_Dis$`SellingPrice-1`);
HomeAudioData_Dis$`SellingPrice-2` <- scale(HomeAudioData_Dis$`SellingPrice-2`);
HomeAudioData_Dis$`SellingPrice-3` <- scale(HomeAudioData_Dis$`SellingPrice-3`);


# d) Modelling

Home_Dis_Model_1 <- lm(gmv~.,HomeAudioData_Dis);

Home_Dis_Model_2 <- stepAIC(Home_Dis_Model_1,direction = "both")
summary(Home_Dis_Model_2);
vif(Home_Dis_Model_2);
#Adjusted R-squared:  0.938 


#MA_Increase_SP, vif -  3866.20 , pvalue - 0.24683
#Removing it.
Home_Dis_Model_3 <- lm(formula = gmv ~ SellingPrice + sla + IsHolidayWeek + No_of_Holidays + 
                         deliverybdays + deliverycdays + Digital_adstock + Content.Marketing_adstock + 
                         SEM_adstock + Radio_adstock + NPS_Score + Discount_Lag_1 + 
                         Discount_Lag_2 + Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + 
                          MA_Increase_Discount + `gmv-2` + `gmv-3` + 
                         `SellingPrice-1` + `SellingPrice-2` + `SellingPrice-3` + 
                         `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_3);
vif(Home_Dis_Model_3);
#Adjusted R-squared:  0.937


#deliverycdays, vif -  495.41 , pvalue - 0.42742
#Removing it.
Home_Dis_Model_4 <- lm(formula = gmv ~ SellingPrice + sla + IsHolidayWeek + No_of_Holidays + 
                         deliverybdays + Digital_adstock + Content.Marketing_adstock + 
                         SEM_adstock + Radio_adstock + NPS_Score + Discount_Lag_1 + 
                         Discount_Lag_2 + Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + 
                         MA_Increase_Discount + `gmv-2` + `gmv-3` + 
                         `SellingPrice-1` + `SellingPrice-2` + `SellingPrice-3` + 
                         `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_4);
vif(Home_Dis_Model_4);
#Adjusted R-squared:  0.938


#`SellingPrice-2`, vif - 491.45  , pvalue -  0.36893  
#Removing it.
Home_Dis_Model_5 <- lm(formula = gmv ~ SellingPrice + sla + IsHolidayWeek + No_of_Holidays + 
                         deliverybdays + Digital_adstock + Content.Marketing_adstock + 
                         SEM_adstock + Radio_adstock + NPS_Score + Discount_Lag_1 + 
                         Discount_Lag_2 + Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + 
                         MA_Increase_Discount + `gmv-2` + `gmv-3` + 
                         `SellingPrice-1` +  `SellingPrice-3` + 
                         `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_5);
vif(Home_Dis_Model_5);
#Adjusted R-squared:  0.938



#MA_Increase_Discount, vif - 740.98  , pvalue -   0.05943 .
#Removing it.
Home_Dis_Model_6 <- lm(formula = gmv ~ SellingPrice + sla + IsHolidayWeek + No_of_Holidays + 
                         deliverybdays + Digital_adstock + Content.Marketing_adstock + 
                         SEM_adstock + Radio_adstock + NPS_Score + Discount_Lag_1 + 
                         Discount_Lag_2 + Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + 
                          `gmv-2` + `gmv-3` + 
                         `SellingPrice-1` +  `SellingPrice-3` + 
                         `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_6);
vif(Home_Dis_Model_6);
#Adjusted R-squared:  0.93



#Discount_Lag_1, vif -337.70   , pvalue -  0.25575 
#Removing it.
Home_Dis_Model_7 <- lm(formula = gmv ~ SellingPrice + sla + IsHolidayWeek + No_of_Holidays + 
                         deliverybdays + Digital_adstock + Content.Marketing_adstock + 
                         SEM_adstock + Radio_adstock + NPS_Score + 
                         Discount_Lag_2 + Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + 
                         `gmv-2` + `gmv-3` + 
                         `SellingPrice-1` +  `SellingPrice-3` + 
                         `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_7);
vif(Home_Dis_Model_7);
#Adjusted R-squared:  0.929


#SP_Lag_3, vif -  916.56     , pvalue -  2.8e-05 ***
#Removing it.
Home_Dis_Model_8 <- lm(formula = gmv ~ SellingPrice + sla + IsHolidayWeek + No_of_Holidays + 
                         deliverybdays + Digital_adstock + Content.Marketing_adstock + 
                         SEM_adstock + Radio_adstock + NPS_Score + 
                         Discount_Lag_2 + Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                         `gmv-2` + `gmv-3` + 
                         `SellingPrice-1` +  `SellingPrice-3` + 
                         `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_8);
vif(Home_Dis_Model_8);
#Adjusted R-squared:  0.857



#Discount_Lag_2, vif -  340.46     , pvalue - 0.0269 * 
#Removing it.
Home_Dis_Model_9 <- lm(formula = gmv ~ SellingPrice + sla + IsHolidayWeek + No_of_Holidays + 
                         deliverybdays + Digital_adstock + Content.Marketing_adstock + 
                         SEM_adstock + Radio_adstock + NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                         `gmv-2` + `gmv-3` + 
                         `SellingPrice-1` +  `SellingPrice-3` + 
                         `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_9);
vif(Home_Dis_Model_9);
#Adjusted R-squared:  0.832


#Content.Marketing_adstock, vif - 84.53    , pvalue -  0.2351   
#Removing it.
Home_Dis_Model_10 <- lm(formula = gmv ~ SellingPrice + sla + IsHolidayWeek + No_of_Holidays + 
                         deliverybdays + Digital_adstock + 
                         SEM_adstock + Radio_adstock + NPS_Score + 
                         Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                         `gmv-2` + `gmv-3` + 
                         `SellingPrice-1` +  `SellingPrice-3` + 
                         `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_10);
vif(Home_Dis_Model_10);
#Adjusted R-squared:  0.829



#SEM_adstock, vif -  126.73   , pvalue -  0.0698 .   
#Removing it.
Home_Dis_Model_11 <- lm(formula = gmv ~ SellingPrice + sla + IsHolidayWeek + No_of_Holidays + 
                          deliverybdays + Digital_adstock + 
                           Radio_adstock + NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                          `gmv-2` + `gmv-3` + 
                          `SellingPrice-1` +  `SellingPrice-3` + 
                          `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_11);
vif(Home_Dis_Model_11);
#Adjusted R-squared:  0.814

#SellingPrice, vif -  233.31  , pvalue -  5.7e-07 ***
#Removing it.
Home_Dis_Model_12 <- lm(formula = gmv ~  sla + IsHolidayWeek + No_of_Holidays + 
                          deliverybdays + Digital_adstock + 
                          Radio_adstock + NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                          `gmv-2` + `gmv-3` + 
                          `SellingPrice-1` +  `SellingPrice-3` + 
                          `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_12);
vif(Home_Dis_Model_12);
#Adjusted R-squared:  0.554



#No_of_Holidays, vif -   7.72  , pvalue -  0.247   
#Removing it.
Home_Dis_Model_13 <- lm(formula = gmv ~  sla + IsHolidayWeek +
                          deliverybdays + Digital_adstock + 
                          Radio_adstock + NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                          `gmv-2` + `gmv-3` + 
                          `SellingPrice-1` +  `SellingPrice-3` + 
                          `Discount-2` + `Discount-3` + `Discount-1`, data = HomeAudioData_Dis);
summary(Home_Dis_Model_13);
vif(Home_Dis_Model_13);
#Adjusted R-squared:  0.548

#`Discount-1`   , pvalue -  0.8655  
#Removing it.
Home_Dis_Model_14 <- lm(formula = gmv ~  sla + IsHolidayWeek +
                          deliverybdays + Digital_adstock + 
                          Radio_adstock + NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                          `gmv-2` + `gmv-3` + 
                          `SellingPrice-1` +  `SellingPrice-3` + 
                          `Discount-2` + `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_14);
vif(Home_Dis_Model_14);
#Adjusted R-squared:  0.562


#Radio_adstock  , pvalue - 0.788   
#Removing it.
Home_Dis_Model_15 <- lm(formula = gmv ~  sla + IsHolidayWeek +
                          deliverybdays + Digital_adstock + 
                           NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                          `gmv-2` + `gmv-3` + 
                          `SellingPrice-1` +  `SellingPrice-3` + 
                          `Discount-2` + `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_15);
vif(Home_Dis_Model_15);
#Adjusted R-squared:  0.575

#`SellingPrice-3`  , pvalue -0.6747 
#Removing it.
Home_Dis_Model_16 <- lm(formula = gmv ~  sla + IsHolidayWeek +
                          deliverybdays + Digital_adstock + 
                          NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                          `gmv-2` + `gmv-3` + 
                          `SellingPrice-1` +  
                          `Discount-2` + `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_16);
vif(Home_Dis_Model_16);
#Adjusted R-squared:  0.585

#deliverybdays  , pvalue -0.5759 
#Removing it.
Home_Dis_Model_17 <- lm(formula = gmv ~  sla + IsHolidayWeek +
                           Digital_adstock + 
                          NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                          `gmv-2` + `gmv-3` + 
                          `SellingPrice-1` +  
                          `Discount-2` + `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_17);
vif(Home_Dis_Model_17);
#Adjusted R-squared:  0.594

#`Discount-2`  , pvalue -0.4743  
#Removing it.
Home_Dis_Model_18 <- lm(formula = gmv ~  sla + IsHolidayWeek +
                          Digital_adstock + 
                          NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                          `gmv-2` + `gmv-3` + 
                          `SellingPrice-1` +  
                          `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_18);
vif(Home_Dis_Model_18);
#Adjusted R-squared:  0.599

#`gmv-2`  , pvalue -0.41699 
#Removing it.
Home_Dis_Model_19 <- lm(formula = gmv ~  sla + IsHolidayWeek +
                          Digital_adstock + 
                          NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                           `gmv-3` + 
                          `SellingPrice-1` +  
                          `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_19);
vif(Home_Dis_Model_19);
#Adjusted R-squared:  0.603



#`gmv-3`  , pvalue -0.4212 
#Removing it.
Home_Dis_Model_20 <- lm(formula = gmv ~  sla + IsHolidayWeek +
                          Digital_adstock + 
                          NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                          `SellingPrice-1` +  
                          `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_20);
vif(Home_Dis_Model_20);
#Adjusted R-squared:  0.607




#IsHolidayWeek , pvalue -0.31104 
#Removing it.
Home_Dis_Model_21 <- lm(formula = gmv ~  sla + 
                          Digital_adstock + 
                          NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  
                          `SellingPrice-1` +  
                          `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_21);
vif(Home_Dis_Model_21);
#Adjusted R-squared:  0.606



#SP_Lag_2   , pvalue -0.16775   
#Removing it.
Home_Dis_Model_22 <- lm(formula = gmv ~  sla + 
                          Digital_adstock + 
                          NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 +  
                          `SellingPrice-1` +  
                          `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_22);
vif(Home_Dis_Model_22);
#Adjusted R-squared:  0.596


#`SellingPrice-1`   , pvalue -0.21430   
#Removing it.
Home_Dis_Model_23 <- lm(formula = gmv ~  sla + 
                          Digital_adstock + 
                          NPS_Score + 
                          Discount_Lag_3 + SP_Lag_1 +  
                          `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_23);
vif(Home_Dis_Model_23);
#Adjusted R-squared:  0.59


#NPS_Score   , pvalue -0.14520
#Removing it.
Home_Dis_Model_24 <- lm(formula = gmv ~  sla + 
                          Digital_adstock + 
                          Discount_Lag_3 + SP_Lag_1 +  
                          `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_24);
vif(Home_Dis_Model_24);
#Adjusted R-squared:  0.578




#SP_Lag_1   , pvalue -0.00547 ** 
#Removing it.
Home_Dis_Model_25 <- lm(formula = gmv ~  sla + 
                          Digital_adstock + 
                          Discount_Lag_3 +  
                          `Discount-3` , data = HomeAudioData_Dis);
summary(Home_Dis_Model_25);
vif(Home_Dis_Model_25);
#Adjusted R-squared:  0.502




Final_Home_Dis_Model <- Home_Dis_Model_25;
#Final Adjusted R-squared:  0.502

#-----------------------------------------------------------------------------------------------------------------------------------------

# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = HomeAudioData_Dis, form.lm = formula( gmv ~  sla + 
                                                                        Digital_adstock + 
                                                                        Discount_Lag_3 +  
                                                                        `Discount-3`   ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval_1, "ms")
# 0.74


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Home_Dis_Model$coefficients[var]*
                       mean(HomeAudioData_Dis[,str_replace_all(var, "`", "")])/mean(HomeAudioData_Dis$gmv))
  return(elax1)
} 



Home_Dis_Var_Elasticity <- data.frame(names(Final_Home_Dis_Model$coefficients)[-1]);


for (i in 2:length(Final_Home_Dis_Model$coefficients)) {
  Home_Dis_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Home_Dis_Model$coefficients)[i]);
}


Home_Dis_Var_Elasticity$Direction <- ifelse(Home_Dis_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Home_Dis_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Home_Dis_Var_Elasticity


ggplot(data=Home_Dis_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Home Audio -Distributive Lag Model") +xlab("Variables")




##############################################################################################################################################
#------------------------------------------ 5. Multiplicative and distributed model --------------------------------------------------------
##############################################################################################################################################


# a) Load Home Audio Data

HomeAudioData_MulDis <- read.csv('HomeAudioData.csv',stringsAsFactors = FALSE);

# b) Removing Unnecessary variables

HomeAudioData_MulDis$product_analytic_sub_category <- NULL;
HomeAudioData_MulDis$X <- NULL;

HomeAudioData_MulDis$Week <- NULL;
HomeAudioData_MulDis$no_of_orders <- NULL;
HomeAudioData_MulDis$units <- NULL;

HomeAudioData_MulDis$COD <- NULL;
HomeAudioData_MulDis$Prepaid <- NULL;
HomeAudioData_MulDis$Mass_p <- NULL;
HomeAudioData_MulDis$Middle_p <- NULL;
HomeAudioData_MulDis$Premium_p <- NULL;

HomeAudioData_MulDis$product_mrp <- NULL;
HomeAudioData_MulDis$MA_Discount <- NULL;
HomeAudioData_MulDis$MA_SellingPrice <- NULL;


HomeAudioData_MulDis <- slide(HomeAudioData_MulDis, Var = "gmv",slideBy = -1)
HomeAudioData_MulDis <- slide(HomeAudioData_MulDis, Var = "gmv",slideBy = -2)
HomeAudioData_MulDis <- slide(HomeAudioData_MulDis, Var = "gmv",slideBy = -3)

HomeAudioData_MulDis <- slide(HomeAudioData_MulDis, Var = "SellingPrice",slideBy = -1)
HomeAudioData_MulDis <- slide(HomeAudioData_MulDis, Var = "SellingPrice",slideBy = -2)
HomeAudioData_MulDis <- slide(HomeAudioData_MulDis, Var = "SellingPrice",slideBy = -3)

HomeAudioData_MulDis <- slide(HomeAudioData_MulDis, Var = "Discount",slideBy = -1)
HomeAudioData_MulDis <- slide(HomeAudioData_MulDis, Var = "Discount",slideBy = -2)
HomeAudioData_MulDis <- slide(HomeAudioData_MulDis, Var = "Discount",slideBy = -3)


# c) Removing 0 values in data changing from 0 to 0.01. As log 0 will be undefined.

HomeAudioData_MulDis <- na.omit(HomeAudioData_MulDis);

# Looking for 0 values in data
sapply(HomeAudioData_MulDis, function(x){sum(x==0)})

HomeAudioData_MulDis[HomeAudioData_MulDis == 0] <- 0.001;


sapply(HomeAudioData_MulDis, function(x){sum(x<0)})
#There are negative values.

# For negative values we apply the following transaformation -> -log(-x + 1) , Yeo-Johnson Power Transformations

for(i in 1:ncol(HomeAudioData_MulDis)) {
  HomeAudioData_MulDis[,c(i)]<- ifelse(
    HomeAudioData_MulDis[,c(i)]>0,
    log(HomeAudioData_MulDis[,c(i)]), 
    -log(-(HomeAudioData_MulDis[,c(i)])+1)
  );
}


# d) Modelling

model_muldis_1 <- lm(gmv ~ ., HomeAudioData_MulDis);

model_muldis_2 <-stepAIC(model_muldis_1,direction = "both")
summary(model_muldis_2);
vif(model_muldis_2);
#Adjusted R-squared:  0.687 

# Online.marketing_adstock, Affiliates_adstock
# vif  66615, 54745, p value -  0.00517 **, 0.00430 **
# cor(HomeAudioData_MulDis$Online.marketing_adstock, HomeAudioData_MulDis$Affiliates_adstock) - 0.99
#Removing Online.marketing_adstock

model_muldis_3 <- lm(formula = gmv ~ SellingPrice + Discount + sla + IsHolidayWeek + 
                       No_of_Holidays + deliverybdays + TV_adstock + Digital_adstock + 
                       Sponsorship_adstock + Content.Marketing_adstock + 
                       Affiliates_adstock + SEM_adstock + Radio_adstock + Other_adstock + 
                       IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                       Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + MA_Increase_SP + 
                       MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = HomeAudioData_MulDis);

summary(model_muldis_3);
vif(model_muldis_3);
#Adjusted R-squared:  0.4576 



#SP_Lag_3, vif - 10343.0000 ,p value - 0.8381 
model_muldis_4 <- lm(formula = gmv ~ SellingPrice + Discount + sla + IsHolidayWeek + 
                       No_of_Holidays + deliverybdays + TV_adstock + Digital_adstock + 
                       Sponsorship_adstock + Content.Marketing_adstock + 
                       Affiliates_adstock + SEM_adstock + Radio_adstock + Other_adstock + 
                       IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                       Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  MA_Increase_SP + 
                       MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = HomeAudioData_MulDis);

summary(model_muldis_4);
vif(model_muldis_4);
#Adjusted R-squared:  0.4922


#IsHolidayWeek, vif -  807.0600  ,p value - 0.80045
model_muldis_5 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                       No_of_Holidays + deliverybdays + TV_adstock + Digital_adstock + 
                       Sponsorship_adstock + Content.Marketing_adstock + 
                       Affiliates_adstock + SEM_adstock + Radio_adstock + Other_adstock + 
                       IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                       Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  MA_Increase_SP + 
                       MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = HomeAudioData_MulDis);

summary(model_muldis_5);
vif(model_muldis_5);
#Adjusted R-squared:  0.5218



#Other_adstock, vif -  779.3800   ,p value - 0.98056
model_muldis_6 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                       No_of_Holidays + deliverybdays + TV_adstock + Digital_adstock + 
                       Sponsorship_adstock + Content.Marketing_adstock + 
                       Affiliates_adstock + SEM_adstock + Radio_adstock + 
                       IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                       Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  MA_Increase_SP + 
                       MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = HomeAudioData_MulDis);

summary(model_muldis_6);
vif(model_muldis_6);
#Adjusted R-squared:  0.5499



#SEM_adstock, vif -  31.0210   ,p value - 0.58790   
model_muldis_7 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                       No_of_Holidays + deliverybdays + TV_adstock + Digital_adstock + 
                       Sponsorship_adstock + Content.Marketing_adstock + 
                       Affiliates_adstock +  Radio_adstock + 
                       IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                       Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  MA_Increase_SP + 
                       MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = HomeAudioData_MulDis);

summary(model_muldis_7);
vif(model_muldis_7);
#Adjusted R-squared:  0.5673




#Sponsorship_adstock, vif -  9.4447   ,p value - 0.14938 
model_muldis_8 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                       No_of_Holidays + deliverybdays + TV_adstock + Digital_adstock + 
                       Content.Marketing_adstock + 
                       Affiliates_adstock +  Radio_adstock + 
                       IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                       Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  MA_Increase_SP + 
                       MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = HomeAudioData_MulDis);

summary(model_muldis_8);
vif(model_muldis_8);
#Adjusted R-squared:  0.5384






#Content.Marketing_adstock, vif -  12.3890   ,p value - 0.11585  
model_muldis_9 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                       No_of_Holidays + deliverybdays + TV_adstock + Digital_adstock + 
                       Affiliates_adstock +  Radio_adstock + 
                       IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                       Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  MA_Increase_SP + 
                       MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = HomeAudioData_MulDis);

summary(model_muldis_9);
vif(model_muldis_9);
#Adjusted R-squared:  0.4988





#TV_adstock, vif -   0.21633      ,p value -  10.8390
model_muldis_10 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                       No_of_Holidays + deliverybdays + Digital_adstock + 
                       Affiliates_adstock +  Radio_adstock + 
                       IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                       Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  MA_Increase_SP + 
                       MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = HomeAudioData_MulDis);

summary(model_muldis_10);
vif(model_muldis_10);
#Adjusted R-squared:  0.4838



#deliverybdays, vif -   7.0890       ,p value -   0.14635   
model_muldis_11 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        No_of_Holidays +  Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                        Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  MA_Increase_SP + 
                        MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                        `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                      data = HomeAudioData_MulDis);

summary(model_muldis_11);
vif(model_muldis_11);
#Adjusted R-squared:  0.4539




# `Discount-2`, vif -   4.0545          ,p value -   0.14934  
model_muldis_12 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        No_of_Holidays +  Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                        Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  MA_Increase_SP + 
                        MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                        `SellingPrice-2` + `SellingPrice-3` +  `Discount-3`, 
                      data = HomeAudioData_MulDis);

summary(model_muldis_12);
vif(model_muldis_12);
#Adjusted R-squared:  0.4246




# `Discount-3`, p value - 0.81578     
model_muldis_13 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        No_of_Holidays +  Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                        Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +  MA_Increase_SP + 
                        MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_13);
vif(model_muldis_13);
#Adjusted R-squared:  0.4473


# MA_Increase_SP, p value - 0.783540    
model_muldis_14 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        No_of_Holidays +  Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        IsHolidayWeek1 + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                        Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 +   
                        MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_14);
vif(model_muldis_14);
#Adjusted R-squared:  0.4677


# SP_Lag_2, p value - 0.603709  
model_muldis_15 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        No_of_Holidays +  Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        IsHolidayWeek1 + IsHolidayWeek2 +  Discount_Lag_2 + 
                        Discount_Lag_3 + SP_Lag_1 +   
                        MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_15);
vif(model_muldis_15);
#Adjusted R-squared:  0.4983

# SP_Lag_1, p value - 0.559896    
model_muldis_16 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        No_of_Holidays +  Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        IsHolidayWeek1 + IsHolidayWeek2 +  Discount_Lag_2 + 
                        Discount_Lag_3 + 
                        MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_16);
vif(model_muldis_16);
#Adjusted R-squared:  0.51

# No_of_Holidays, p value - 0.540860    
model_muldis_17 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                          Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        IsHolidayWeek1 + IsHolidayWeek2 +  Discount_Lag_2 + 
                        Discount_Lag_3 + 
                        MA_Increase_Discount + `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_17);
vif(model_muldis_17);
#Adjusted R-squared:  0.5204

# MA_Increase_Discount, p value - 0.524366    
model_muldis_18 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        IsHolidayWeek1 + IsHolidayWeek2 +  Discount_Lag_2 + 
                        Discount_Lag_3 + 
                        `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_18);
vif(model_muldis_18);
#Adjusted R-squared:  0.5297


# IsHolidayWeek1, p value - 0.464436        
model_muldis_19 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        IsHolidayWeek2 + 
                        Discount_Lag_3 + 
                        `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_19);
vif(model_muldis_19);
#Adjusted R-squared:  0.546

# IsHolidayWeek2, p value - 0.396915        
model_muldis_20 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        Discount_Lag_3 + 
                        `gmv-1` + `gmv-2` + `gmv-3` + `SellingPrice-1` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_20);
vif(model_muldis_20);
#Adjusted R-squared:  0.5496

# `SellingPrice-1`, p value -  0.371592       
model_muldis_21 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        Discount_Lag_3 + 
                        `gmv-1` + `gmv-2` + `gmv-3` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_21);
vif(model_muldis_21);
#Adjusted R-squared:  0.552


# Discount_Lag_3, p value -  0.451549   
model_muldis_22 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        `gmv-1` + `gmv-2` + `gmv-3` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_22);
vif(model_muldis_22);
#Adjusted R-squared:  0.5574



# `gmv-2`, p value -  0.353531    
model_muldis_23 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        `gmv-1` + `gmv-3` + 
                        `SellingPrice-2` + `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_23);
vif(model_muldis_23);
#Adjusted R-squared:  0.5588


# `SellingPrice-2` , p value -  0.220572    
model_muldis_24 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        `gmv-1` + `gmv-3` + 
                         `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_24);
vif(model_muldis_24);
#Adjusted R-squared:  0.5522


# `gmv-3` , p value -  0.223512    
model_muldis_25 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        `gmv-1` + 
                        `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_25);
vif(model_muldis_25);
#Adjusted R-squared:  0.5459



# `gmv-1` , p value -  0.152615    
model_muldis_26 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  Radio_adstock + 
                        `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_26);
vif(model_muldis_26);
#Adjusted R-squared:  0.5327


# Radio_adstock , p value -   0.255554    
model_muldis_27 <- lm(formula = gmv ~ SellingPrice + Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  
                        `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_27);
vif(model_muldis_27);
#Adjusted R-squared:  0.5289


# SellingPrice , p value -  0.22412     
model_muldis_28 <- lm(formula = gmv ~ Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock +  
                        `SellingPrice-3` , 
                      data = HomeAudioData_MulDis);

summary(model_muldis_28);
vif(model_muldis_28);
#Adjusted R-squared:  0.5228


# `SellingPrice-3` , p value -  0.16757       
model_muldis_29 <- lm(formula = gmv ~ Discount + sla +
                        Digital_adstock + 
                        Affiliates_adstock,
                      data = HomeAudioData_MulDis);

summary(model_muldis_29);
vif(model_muldis_29);
#Adjusted R-squared:  0.5118


# Digital_adstock , p value -  0.112178    
model_muldis_30 <- lm(formula = gmv ~ Discount + sla +
                        Affiliates_adstock,
                      data = HomeAudioData_MulDis);

summary(model_muldis_30);
vif(model_muldis_30);
#Adjusted R-squared:  0.4932




Final_Home_MulDis_Model <- model_muldis_30;
#Final Adjusted R-squared:  0.4932

# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = HomeAudioData_MulDis, form.lm = formula( gmv ~ Discount + sla +
                                                                           Affiliates_adstock ),m = 10, plotit=FALSE)
#Mean Square Error
attr(temp_crossval_1, "ms")
#0.228



elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Home_MulDis_Model$coefficients[var]*
                       mean(HomeAudioData_MulDis[,var])/mean(HomeAudioData_MulDis$gmv))
  
  return(elax1)
} 



Home_MulDis_Var_Elasticity <- data.frame(names(Final_Home_MulDis_Model$coefficients)[-1]);

for (i in 2:length(Final_Home_MulDis_Model$coefficients)) {
  Home_MulDis_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Home_MulDis_Model$coefficients)[i]);
}
Home_MulDis_Var_Elasticity$Direction <- ifelse(Home_MulDis_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Home_MulDis_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Home_MulDis_Var_Elasticity


ggplot(data=Home_MulDis_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Home Audio- Distributed + Multiplicative") +xlab("Variables")


#--------------------------------------------------------- The End ------------------------------------------------------------------------------
