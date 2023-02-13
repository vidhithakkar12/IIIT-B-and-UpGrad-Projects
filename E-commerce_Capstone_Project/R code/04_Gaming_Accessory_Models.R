#############################################################################################################################################################
#------------------------------------------------ Gaming Accessory - Model Building -------------------------------------------------------------------------
#############################################################################################################################################################

# All necessary packages have been installed in the data preparation file


############################################################################################################################################################
#----------------------------------------------- 1. Linear Model ------------------------------------------------------------------------------------------
############################################################################################################################################################


# a) Load Gaming Accessory Data
GamingAccessoryData <- read.csv("GamingAccessoryData.csv", stringsAsFactors = FALSE)

str(GamingAccessoryData);
colnames(GamingAccessoryData);

# b) Removing Unnecessary variables 

GamingAccessoryData$product_analytic_sub_category <- NULL;
GamingAccessoryData$X <- NULL;

GamingAccessoryData$Week <- NULL;
GamingAccessoryData$no_of_orders <- NULL;
GamingAccessoryData$units <- NULL;

GamingAccessoryData$COD <- NULL;
GamingAccessoryData$Prepaid <- NULL;
GamingAccessoryData$Mass_p <- NULL;
GamingAccessoryData$Middle_p <- NULL;
GamingAccessoryData$Premium_p <- NULL;

GamingAccessoryData$product_mrp <- NULL;
GamingAccessoryData$SellingPrice <- NULL;
GamingAccessoryData$MA_Discount <- NULL;
GamingAccessoryData$MA_SellingPrice <- NULL;

# c) Scaling all the variables for modelling

GamingAccessoryData$gmv <- scale(GamingAccessoryData$gmv);
GamingAccessoryData$Discount <- scale(GamingAccessoryData$Discount);
GamingAccessoryData$sla <- scale(GamingAccessoryData$sla);
GamingAccessoryData$procurement_sla <- scale(GamingAccessoryData$procurement_sla);

GamingAccessoryData$No_of_Holidays <- scale(GamingAccessoryData$No_of_Holidays)
GamingAccessoryData$deliverybdays <- scale(GamingAccessoryData$deliverybdays);

GamingAccessoryData$deliverycdays <- scale(GamingAccessoryData$deliverycdays);

GamingAccessoryData$TV_adstock <- scale(GamingAccessoryData$TV_adstock);
GamingAccessoryData$Digital_adstock <- scale(GamingAccessoryData$Digital_adstock);

GamingAccessoryData$Sponsorship_adstock <- scale(GamingAccessoryData$Sponsorship_adstock);
GamingAccessoryData$`Content.Marketing_adstock` <- scale(GamingAccessoryData$`Content.Marketing_adstock`);
GamingAccessoryData$`Online.marketing_adstock` <- scale(GamingAccessoryData$`Online.marketing_adstock`);

GamingAccessoryData$Affiliates_adstock <- scale(GamingAccessoryData$Affiliates_adstock);
GamingAccessoryData$SEM_adstock <- scale(GamingAccessoryData$SEM_adstock);
GamingAccessoryData$Radio_adstock <- scale(GamingAccessoryData$Radio_adstock);

GamingAccessoryData$Other_adstock <-scale(GamingAccessoryData$Other_adstock);
GamingAccessoryData$NPS_Score     <-scale(GamingAccessoryData$NPS_Score); 

GamingAccessoryData$Discount_Lag_1 <- scale(GamingAccessoryData$Discount_Lag_1);
GamingAccessoryData$Discount_Lag_2 <- scale(GamingAccessoryData$Discount_Lag_2);
GamingAccessoryData$Discount_Lag_3 <- scale(GamingAccessoryData$Discount_Lag_3);

GamingAccessoryData$SP_Lag_1 <- scale(GamingAccessoryData$SP_Lag_1);
GamingAccessoryData$SP_Lag_2 <- scale(GamingAccessoryData$SP_Lag_2);
GamingAccessoryData$SP_Lag_3 <- scale(GamingAccessoryData$SP_Lag_3);

GamingAccessoryData$MA_Increase_SP <- scale(GamingAccessoryData$MA_Increase_SP);
GamingAccessoryData$MA_Increase_Discount <- scale(GamingAccessoryData$MA_Increase_Discount);


# d) Modelling

set.seed(100);

model_1 <- lm(gmv~.,GamingAccessoryData)

model_2 <- stepAIC(model_1,direction = "both")


model_2
summary(model_2);
vif(model_2);
#Adjusted R-squared:  0.706 

#deliverybdays, deliverycdays, vif - 5587.40,  5664.70 
#p value - 6.9e-05 ***, 9.1e-05 ***
#Removing deliverycdays
model_3 <- lm(formula = gmv ~ sla + procurement_sla + deliverybdays + 
                TV_adstock + Online.marketing_adstock + SEM_adstock + Radio_adstock + 
                Other_adstock + IsHolidayWeek2 + Discount_Lag_2 + MA_Increase_SP, 
              data = GamingAccessoryData);

summary(model_3);
vif(model_3);
#Adjusted R-squared:  0.578


#Radio_adstock vif - 78.69 
#p value -  0.06282 . 
#Removing Radio_adstock
model_4 <- lm(formula = gmv ~ sla + procurement_sla + deliverybdays + 
                TV_adstock + Online.marketing_adstock + SEM_adstock +
                Other_adstock + IsHolidayWeek2 + Discount_Lag_2 + MA_Increase_SP, 
              data = GamingAccessoryData);

summary(model_4);
vif(model_4);
#Adjusted R-squared:  0.551

#All vif are bw 1 and 2. Considering only p value to remove variables.

#sla - p value -  0.8169
#Removing it.
model_5 <- lm(formula = gmv ~ procurement_sla + deliverybdays + 
                TV_adstock + Online.marketing_adstock + SEM_adstock +
                Other_adstock + IsHolidayWeek2 + Discount_Lag_2 + MA_Increase_SP, 
              data = GamingAccessoryData);

summary(model_5);
vif(model_5);

#Adjusted R-squared:  0.561


#SEM_adstock - p value -  0.35352 
#Removing it.
model_6 <- lm(formula = gmv ~ procurement_sla + deliverybdays + 
                TV_adstock + Online.marketing_adstock + 
                Other_adstock + IsHolidayWeek2 + Discount_Lag_2 + MA_Increase_SP, 
              data = GamingAccessoryData);

summary(model_6);
vif(model_6);

#Adjusted R-squared:  0.562

#MA_Increase_SP - p value - 0.36505 
#Removing it.
model_7 <- lm(formula = gmv ~ procurement_sla + deliverybdays + 
                TV_adstock + Online.marketing_adstock + 
                Other_adstock + IsHolidayWeek2 + Discount_Lag_2, 
              data = GamingAccessoryData);

summary(model_7);
vif(model_7);

#Adjusted R-squared:  0.563

#TV_adstock - p value - 0.17416
#Removing it.
model_8 <- lm(formula = gmv ~ procurement_sla + deliverybdays + 
                Online.marketing_adstock + 
                Other_adstock + IsHolidayWeek2 + Discount_Lag_2, 
              data = GamingAccessoryData);

summary(model_8);
vif(model_8);

#Adjusted R-squared:  0.555

#Other_adstock - p value - 0.27364 
#Removing it.
model_9 <- lm(formula = gmv ~ procurement_sla + deliverybdays + 
                Online.marketing_adstock + 
                IsHolidayWeek2 + Discount_Lag_2, 
              data = GamingAccessoryData);

summary(model_9);
vif(model_9);

#Adjusted R-squared:  0.553

#Discount_Lag_2 - p value - 0.19627 
#Removing it.
model_10 <- lm(formula = gmv ~ procurement_sla + deliverybdays + 
                Online.marketing_adstock + 
                IsHolidayWeek2 ,
              data = GamingAccessoryData);

summary(model_10);
vif(model_10);

#Adjusted R-squared:  0.546



model_final <- model_10;
# Final Adjusted R square - 0.546


#----------------------------------------------------------------------------------------------------------------------------------

# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = GamingAccessoryData, form.lm = formula( gmv ~ procurement_sla + deliverybdays + 
                                                                          Online.marketing_adstock + IsHolidayWeek2),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval_1, "ms")
# 0.486


# Elasticity
elasticity_calc <- function(var){
  
  elax1 <-as.numeric(model_final$coefficients[var]*
                       mean(GamingAccessoryData[,var])/mean(GamingAccessoryData$gmv))
  
  return(elax1)
} 


gaming_Var_Elasticity <- data.frame(names(model_final$coefficients)[-1]);

for (i in 2:length(model_final$coefficients)) {
  gaming_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(model_final$coefficients)[i]);
}
gaming_Var_Elasticity$Direction <- ifelse(gaming_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(gaming_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
gaming_Var_Elasticity


  ggplot(data=gaming_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill ="green" , color="red") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Linear Model") +xlab("Variables")



################################################################################################################################################
#----------------------------------------- 2. Multiplicative Model --------------------------------------------------------------------------------
################################################################################################################################################
  
# a) Load Gaming Accessory Data
GamingAccessory_Mul <- read.csv('GamingAccessoryData.csv',stringsAsFactors = FALSE);

# b) Removing Unnecessary variables 
GamingAccessory_Mul$product_analytic_sub_category <- NULL;
GamingAccessory_Mul$X <- NULL;

GamingAccessory_Mul$Week <- NULL;
GamingAccessory_Mul$no_of_orders <- NULL;
GamingAccessory_Mul$units <- NULL;

GamingAccessory_Mul$COD <- NULL;
GamingAccessory_Mul$Prepaid <- NULL;
GamingAccessory_Mul$Mass_p <- NULL;
GamingAccessory_Mul$Middle_p <- NULL;
GamingAccessory_Mul$Premium_p <- NULL;

GamingAccessory_Mul$product_mrp <- NULL;
GamingAccessory_Mul$SellingPrice <- NULL;
GamingAccessory_Mul$MA_Discount <- NULL;
GamingAccessory_Mul$MA_SellingPrice <- NULL;


# c) Removing 0 values in data changing from 0 to 0.01. As log 0 will be undefined.

# Looking for 0 values in data
sapply(GamingAccessory_Mul, function(x){sum(x==0)})

GamingAccessory_Mul[GamingAccessory_Mul == 0] <- 0.001;


sapply(GamingAccessory_Mul, function(x){sum(x<0)})
#There are negative values.

# For negative values we apply the following transaformation -> -log(-x + 1) , Yeo-Johnson Power Transformations

for(i in 1:ncol(GamingAccessory_Mul)) {
  GamingAccessory_Mul[,c(i)]<- ifelse(
    GamingAccessory_Mul[,c(i)]>0,
    log(GamingAccessory_Mul[,c(i)]), 
    -log(-(GamingAccessory_Mul[,c(i)])+1)
  );
}


# d) Modelling

m_model_1 <- lm(gmv ~ ., GamingAccessory_Mul);

m_model_2 <-stepAIC(m_model_1,direction = "both")
summary(m_model_2);
vif(m_model_2);
#Adjusted R-squared:  0.836 

#Affiliates_adstock, vif - 3584.70, p value 3.9e-05 ***
#Removing it.
m_model_3 <- lm(formula = gmv ~ Discount + procurement_sla + No_of_Holidays + 
                  Digital_adstock + Sponsorship_adstock + Content.Marketing_adstock + 
                  Online.marketing_adstock + SEM_adstock + 
                  NPS_Score + SP_Lag_3, data = GamingAccessory_Mul);

summary(m_model_3);
vif(m_model_3);
#Adjusted R-squared:  0.757 


#NPS_Score, vif -  12.66, p value 0.9851  
#Removing it.
m_model_4 <- lm(formula = gmv ~ Discount + procurement_sla + No_of_Holidays + 
                  Digital_adstock + Sponsorship_adstock + Content.Marketing_adstock + 
                  Online.marketing_adstock + SEM_adstock + 
                   SP_Lag_3, data = GamingAccessory_Mul);

summary(m_model_4);
vif(m_model_4);
#Adjusted R-squared:  0.763 


#SEM_adstock, vif -   8.09 , p value  0.83170     
#Removing it.
m_model_5 <- lm(formula = gmv ~ Discount + procurement_sla + No_of_Holidays + 
                  Digital_adstock + Sponsorship_adstock + 
                  Online.marketing_adstock + 
                  SP_Lag_3, data = GamingAccessory_Mul);

summary(m_model_5);
vif(m_model_5);

#Adjusted R-squared:  0.767


#Sponsorship_adstock, vif -  4.95  , p value  0.34645       
#Removing it.
m_model_6 <- lm(formula = gmv ~ Discount + procurement_sla + No_of_Holidays + 
                  Digital_adstock +
                  Online.marketing_adstock + 
                  SP_Lag_3, data = GamingAccessory_Mul);

summary(m_model_6);
vif(m_model_6);

#Adjusted R-squared:  0.767

#All vif are bw 1 and 2.
#Considering only p values to remove variables.

#Digital_adstock,  p value  0.5031    
#Removing it.
m_model_7 <- lm(formula = gmv ~ Discount + procurement_sla + No_of_Holidays + 
                  Online.marketing_adstock + 
                  SP_Lag_3, data = GamingAccessory_Mul);

summary(m_model_7);
vif(m_model_7);
#Adjusted R-squared:  0.77 

#No_of_Holidays,  p value  0.5328 
#Removing it.
m_model_8 <- lm(formula = gmv ~ Discount + procurement_sla + 
                  Online.marketing_adstock + 
                  SP_Lag_3, data = GamingAccessory_Mul);

summary(m_model_8);
vif(m_model_8);
#Adjusted R-squared:  0.773

#Discount,  p value 0.30622    
#Removing it.
m_model_9 <- lm(formula = gmv ~ procurement_sla + 
                  Online.marketing_adstock + 
                  SP_Lag_3, data = GamingAccessory_Mul);

summary(m_model_9);
vif(m_model_9);
#Adjusted R-squared:  0.773


Final_Gaming_mul_model <- m_model_9;
#Final Adjusted R square = 0.773


#--------------------------------------------------------------------------------------------------------------------------------------------------

# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = GamingAccessory_Mul, form.lm = formula(  gmv ~ procurement_sla + 
                                                                           Online.marketing_adstock + 
                                                                           SP_Lag_3 ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval_1, "ms")
# 0.992


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Gaming_mul_model$coefficients[var]*
                       mean(GamingAccessory_Mul[,var])/mean(GamingAccessory_Mul$gmv))
  
  return(elax1)
} 


Gaming_mul_Var_Elasticity <- data.frame(names(Final_Gaming_mul_model$coefficients)[-1]);

for (i in 2:length(Final_Gaming_mul_model$coefficients)) {
  Gaming_mul_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Gaming_mul_model$coefficients)[i]);
}
Gaming_mul_Var_Elasticity$Direction <- ifelse(Gaming_mul_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Gaming_mul_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Gaming_mul_Var_Elasticity


ggplot(data=Gaming_mul_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("GamingAccessory - Mulitplicative Model") +xlab("Variables")


###############################################################################################################################################
#--------------------------------------------- 3. Koyck model----------------------------------------------------------------------------------
###############################################################################################################################################

# a) Load Gaming Accessory Data

GamingAccessoryData_Koyck <- read.csv('GamingAccessoryData.csv',stringsAsFactors = FALSE);

GamingAccessoryData_Koyck <- slide(GamingAccessoryData_Koyck, Var = "gmv",slideBy = -1)

str(GamingAccessoryData_Koyck);
colnames(GamingAccessoryData_Koyck);


# b) Removing Unnecessary variables 

GamingAccessoryData_Koyck$product_analytic_sub_category <- NULL;
GamingAccessoryData_Koyck$X <- NULL;

GamingAccessoryData_Koyck$Week <- NULL;
GamingAccessoryData_Koyck$no_of_orders <- NULL;
GamingAccessoryData_Koyck$units <- NULL;

GamingAccessoryData_Koyck$COD <- NULL;
GamingAccessoryData_Koyck$Prepaid <- NULL;
GamingAccessoryData_Koyck$Mass_p <- NULL;
GamingAccessoryData_Koyck$Middle_p <- NULL;
GamingAccessoryData_Koyck$Premium_p <- NULL;

GamingAccessoryData_Koyck$product_mrp <- NULL;
GamingAccessoryData_Koyck$SellingPrice <- NULL;
GamingAccessoryData_Koyck$MA_Discount <- NULL;
GamingAccessoryData_Koyck$MA_SellingPrice <- NULL;


# c) Scaling all the variables for modelling

GamingAccessoryData_Koyck$gmv <- scale(GamingAccessoryData_Koyck$gmv);
GamingAccessoryData_Koyck$Discount <- scale(GamingAccessoryData_Koyck$Discount);
GamingAccessoryData_Koyck$sla <- scale(GamingAccessoryData_Koyck$sla);
GamingAccessoryData_Koyck$procurement_sla <- scale(GamingAccessoryData_Koyck$procurement_sla);

GamingAccessoryData_Koyck$No_of_Holidays <- scale(GamingAccessoryData_Koyck$No_of_Holidays)
GamingAccessoryData_Koyck$deliverybdays <- scale(GamingAccessoryData_Koyck$deliverybdays);

GamingAccessoryData_Koyck$deliverycdays <- scale(GamingAccessoryData_Koyck$deliverycdays);

GamingAccessoryData_Koyck$TV_adstock <- scale(GamingAccessoryData_Koyck$TV_adstock);
GamingAccessoryData_Koyck$Digital_adstock <- scale(GamingAccessoryData_Koyck$Digital_adstock);

GamingAccessoryData_Koyck$Sponsorship_adstock <- scale(GamingAccessoryData_Koyck$Sponsorship_adstock);
GamingAccessoryData_Koyck$`Content.Marketing_adstock` <- scale(GamingAccessoryData_Koyck$`Content.Marketing_adstock`);
GamingAccessoryData_Koyck$`Online.marketing_adstock` <- scale(GamingAccessoryData_Koyck$`Online.marketing_adstock`);

GamingAccessoryData_Koyck$Affiliates_adstock <- scale(GamingAccessoryData_Koyck$Affiliates_adstock);
GamingAccessoryData_Koyck$SEM_adstock <- scale(GamingAccessoryData_Koyck$SEM_adstock);
GamingAccessoryData_Koyck$Radio_adstock <- scale(GamingAccessoryData_Koyck$Radio_adstock);

GamingAccessoryData_Koyck$Other_adstock <-scale(GamingAccessoryData_Koyck$Other_adstock);
GamingAccessoryData_Koyck$NPS_Score <-scale(GamingAccessoryData_Koyck$NPS_Score); 
GamingAccessoryData_Koyck$Discount_Lag_1 <- scale(GamingAccessoryData_Koyck$Discount_Lag_1);
GamingAccessoryData_Koyck$Discount_Lag_2 <- scale(GamingAccessoryData_Koyck$Discount_Lag_2);
GamingAccessoryData_Koyck$Discount_Lag_3 <- scale(GamingAccessoryData_Koyck$Discount_Lag_3);

GamingAccessoryData_Koyck$SP_Lag_1 <- scale(GamingAccessoryData_Koyck$SP_Lag_1);
GamingAccessoryData_Koyck$SP_Lag_2 <- scale(GamingAccessoryData_Koyck$SP_Lag_2);
GamingAccessoryData_Koyck$SP_Lag_3 <- scale(GamingAccessoryData_Koyck$SP_Lag_3);

GamingAccessoryData_Koyck$MA_Increase_SP <- scale(GamingAccessoryData_Koyck$MA_Increase_SP);
GamingAccessoryData_Koyck$MA_Increase_Discount <- scale(GamingAccessoryData_Koyck$MA_Increase_Discount);
GamingAccessoryData_Koyck$`gmv-1` <- scale(GamingAccessoryData_Koyck$`gmv-1`);


# d) Modelling

Gaming_Koyck_Model_1 <- lm(gmv~.,GamingAccessoryData_Koyck);

Gaming_Koyck_Model_2 <- stepAIC(Gaming_Koyck_Model_1,direction = "both")
summary(Gaming_Koyck_Model_2);
vif(Gaming_Koyck_Model_2);
#Adjusted R-squared:  0.776 

#deliverycdays, vif -  7797.40 , p value - 5.4e-06 ***
#Removing it
Gaming_Koyck_Model_3 <- lm(formula = gmv ~ Discount + sla + procurement_sla + IsHolidayWeek + 
                             No_of_Holidays + deliverybdays +  Digital_adstock + 
                             Other_adstock + NPS_Score + Discount_Lag_1 + SP_Lag_1 + SP_Lag_3 + 
                             MA_Increase_SP + `gmv-1` + SP_Lag_2, data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_3);
vif(Gaming_Koyck_Model_3);
#Adjusted R-squared:  0.603 


#SP_Lag_3, vif -  17.85 , p value - 0.812  
#Removing it
Gaming_Koyck_Model_4 <- lm(formula = gmv ~ Discount + sla + procurement_sla + IsHolidayWeek + 
                             No_of_Holidays + deliverybdays +  Digital_adstock + 
                             Other_adstock + NPS_Score + Discount_Lag_1 + SP_Lag_1 +
                             MA_Increase_SP + `gmv-1` + SP_Lag_2, data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_4);
vif(Gaming_Koyck_Model_4);
#Adjusted R-squared:  0.613


#MA_Increase_SP, vif -   12.76 , p value -0.0955 .  
#Removing it
Gaming_Koyck_Model_5 <- lm(formula = gmv ~ Discount + sla + procurement_sla + IsHolidayWeek + 
                             No_of_Holidays + deliverybdays +  Digital_adstock + 
                             Other_adstock + NPS_Score + Discount_Lag_1 + SP_Lag_1 +
                             `gmv-1` + SP_Lag_2, data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_5);
vif(Gaming_Koyck_Model_5);
#Adjusted R-squared:  0.593



#No_of_Holidays, vif -  7.40, p value - 0.1831    
#Removing it
Gaming_Koyck_Model_6 <- lm(formula = gmv ~ Discount + sla + procurement_sla + IsHolidayWeek + 
                             deliverybdays +  Digital_adstock + 
                             Other_adstock + NPS_Score + Discount_Lag_1 + SP_Lag_1 +
                             `gmv-1` + SP_Lag_2, data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_6);
vif(Gaming_Koyck_Model_6);
#Adjusted R-squared:  0.593



#Discount, vif -   5.35  , p value - 0.039 *  
#Removing it
Gaming_Koyck_Model_7 <- lm(formula = gmv ~ sla + procurement_sla + IsHolidayWeek + 
                             deliverybdays +  Digital_adstock + 
                             Other_adstock + NPS_Score + Discount_Lag_1 + SP_Lag_1 +
                             `gmv-1` + SP_Lag_2, data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_7);
vif(Gaming_Koyck_Model_7);
#Adjusted R-squared:  0.593

#All vif are bw 1 and 3. Considering only p values to remove variable.

#Digital_adstock , p value - 0.71700
#Removing it
Gaming_Koyck_Model_8 <- lm(formula = gmv ~ sla + procurement_sla + IsHolidayWeek + 
                             deliverybdays +  
                             Other_adstock + NPS_Score + Discount_Lag_1 + SP_Lag_1 +
                             `gmv-1` + SP_Lag_2, data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_8);
vif(Gaming_Koyck_Model_8);
#Adjusted R-squared:  0.557

#sla , p value -0.64112    
#Removing it
Gaming_Koyck_Model_9 <- lm(formula = gmv ~  procurement_sla + IsHolidayWeek + 
                             deliverybdays +  
                             Other_adstock + NPS_Score + Discount_Lag_1 + SP_Lag_1 +
                             `gmv-1` + SP_Lag_2, data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_9);
vif(Gaming_Koyck_Model_9);
#Adjusted R-squared:  0.565

#SP_Lag_2 , p value -0.32206
#Removing it
Gaming_Koyck_Model_10 <- lm(formula = gmv ~  procurement_sla + IsHolidayWeek + 
                             deliverybdays +  
                             Other_adstock + NPS_Score + Discount_Lag_1 + SP_Lag_1 +
                             `gmv-1` , data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_10);
vif(Gaming_Koyck_Model_10);
#Adjusted R-squared:  0.565

#`gmv-1`  , p value -0.19629  
#Removing it
Gaming_Koyck_Model_11 <- lm(formula = gmv ~  procurement_sla + IsHolidayWeek + 
                              deliverybdays +  
                              Other_adstock + NPS_Score + Discount_Lag_1 + SP_Lag_1 
                              , data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_11);
vif(Gaming_Koyck_Model_11);
#Adjusted R-squared:  0.553

#Discount_Lag_1  , p value - 0.2855   
#Removing it
Gaming_Koyck_Model_12 <- lm(formula = gmv ~  procurement_sla + IsHolidayWeek + 
                              deliverybdays +  
                              Other_adstock + NPS_Score  + SP_Lag_1 
                            , data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_12);
vif(Gaming_Koyck_Model_12);
#Adjusted R-squared:  0.551

#SP_Lag_1  , p value -0.3668    
#Removing it
Gaming_Koyck_Model_13 <- lm(formula = gmv ~  procurement_sla + IsHolidayWeek + 
                              deliverybdays +  
                              Other_adstock + NPS_Score  
                            , data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_13);
vif(Gaming_Koyck_Model_13);
#Adjusted R-squared:  0.553

#Other_adstock  , p value -0.0975 .  
#Removing it
Gaming_Koyck_Model_14 <- lm(formula = gmv ~  procurement_sla + IsHolidayWeek + 
                              deliverybdays +   NPS_Score  
                            , data = GamingAccessoryData_Koyck);

summary(Gaming_Koyck_Model_14);
vif(Gaming_Koyck_Model_14);
#Adjusted R-squared:  0.535



Final_Gaming_koyck_model <- Gaming_Koyck_Model_14;
#Final Adjusted R square = 0.535

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = GamingAccessoryData_Koyck, form.lm = formula( gmv ~  procurement_sla + IsHolidayWeek + 
                                                                                deliverybdays +   NPS_Score  ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval_1, "ms")
#0.542


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Gaming_koyck_model$coefficients[var]*
                       mean(GamingAccessoryData_Koyck[,var])/mean(GamingAccessoryData_Koyck$gmv))
  
  return(elax1)
} 


Gaming_koyck_Var_Elasticity <- data.frame(names(Final_Gaming_koyck_model$coefficients)[-1]);

for (i in 2:length(Final_Gaming_koyck_model$coefficients)) {
  Gaming_koyck_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Gaming_koyck_model$coefficients)[i]);
}
Gaming_koyck_Var_Elasticity$Direction <- ifelse(Gaming_koyck_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Gaming_koyck_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Gaming_koyck_Var_Elasticity


ggplot(data=Gaming_koyck_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Koyck") +xlab("Variables")




############################################################################################################################################
#-------------------------------------------- 4. Distributed Lag Model ---------------------------------------------------------------------
############################################################################################################################################


# a) Load Gaming Accessory Data

GamingAccessoryData_Dis <- read.csv('GamingAccessoryData.csv',stringsAsFactors = FALSE);


GamingAccessoryData_Dis <- slide(GamingAccessoryData_Dis, Var = "gmv",slideBy = -1)
GamingAccessoryData_Dis <- slide(GamingAccessoryData_Dis, Var = "gmv",slideBy = -2)
GamingAccessoryData_Dis <- slide(GamingAccessoryData_Dis, Var = "gmv",slideBy = -3)


GamingAccessoryData_Dis <- slide(GamingAccessoryData_Dis, Var = "SellingPrice",slideBy = -1)
GamingAccessoryData_Dis <- slide(GamingAccessoryData_Dis, Var = "SellingPrice",slideBy = -2)
GamingAccessoryData_Dis <- slide(GamingAccessoryData_Dis, Var = "SellingPrice",slideBy = -3)


GamingAccessoryData_Dis <- slide(GamingAccessoryData_Dis, Var = "Discount",slideBy = -1)
GamingAccessoryData_Dis <- slide(GamingAccessoryData_Dis, Var = "Discount",slideBy = -2)
GamingAccessoryData_Dis <- slide(GamingAccessoryData_Dis, Var = "Discount",slideBy = -3)


str(GamingAccessoryData_Dis);
colnames(GamingAccessoryData_Dis);
GamingAccessoryData_Dis <- na.omit(GamingAccessoryData_Dis)

# b) Removing Unnecessary variables 

GamingAccessoryData_Dis$product_analytic_sub_category <- NULL;
GamingAccessoryData_Dis$X <- NULL;

GamingAccessoryData_Dis$Week <- NULL;
GamingAccessoryData_Dis$no_of_orders <- NULL;
GamingAccessoryData_Dis$units <- NULL;

GamingAccessoryData_Dis$COD <- NULL;
GamingAccessoryData_Dis$Prepaid <- NULL;
GamingAccessoryData_Dis$Mass_p <- NULL;
GamingAccessoryData_Dis$Middle_p <- NULL;
GamingAccessoryData_Dis$Premium_p <- NULL;

GamingAccessoryData_Dis$product_mrp <- NULL;
GamingAccessoryData_Dis$MA_Discount <- NULL;
GamingAccessoryData_Dis$MA_SellingPrice <- NULL;


# c) Scaling all the variables for modelling

GamingAccessoryData_Dis$gmv <- scale(GamingAccessoryData_Dis$gmv);
GamingAccessoryData_Dis$Discount <- scale(GamingAccessoryData_Dis$Discount);
GamingAccessoryData_Dis$`Discount-1` <- scale(GamingAccessoryData_Dis$`Discount-1`);
GamingAccessoryData_Dis$`Discount-2` <- scale(GamingAccessoryData_Dis$`Discount-2`);
GamingAccessoryData_Dis$`Discount-3` <- scale(GamingAccessoryData_Dis$`Discount-3`);

GamingAccessoryData_Dis$sla <- scale(GamingAccessoryData_Dis$sla);
GamingAccessoryData_Dis$procurement_sla <- scale(GamingAccessoryData_Dis$procurement_sla);

GamingAccessoryData_Dis$No_of_Holidays <- scale(GamingAccessoryData_Dis$No_of_Holidays)
GamingAccessoryData_Dis$deliverybdays <- scale(GamingAccessoryData_Dis$deliverybdays);

GamingAccessoryData_Dis$deliverycdays <- scale(GamingAccessoryData_Dis$deliverycdays);

GamingAccessoryData_Dis$TV_adstock <- scale(GamingAccessoryData_Dis$TV_adstock);
GamingAccessoryData_Dis$Digital_adstock <- scale(GamingAccessoryData_Dis$Digital_adstock);

GamingAccessoryData_Dis$Sponsorship_adstock <- scale(GamingAccessoryData_Dis$Sponsorship_adstock);
GamingAccessoryData_Dis$`Content.Marketing_adstock` <- scale(GamingAccessoryData_Dis$`Content.Marketing_adstock`);
GamingAccessoryData_Dis$`Online.marketing_adstock` <- scale(GamingAccessoryData_Dis$`Online.marketing_adstock`);

GamingAccessoryData_Dis$Affiliates_adstock <- scale(GamingAccessoryData_Dis$Affiliates_adstock);
GamingAccessoryData_Dis$SEM_adstock <- scale(GamingAccessoryData_Dis$SEM_adstock);
GamingAccessoryData_Dis$Radio_adstock <- scale(GamingAccessoryData_Dis$Radio_adstock);

GamingAccessoryData_Dis$Other_adstock <-scale(GamingAccessoryData_Dis$Other_adstock);
GamingAccessoryData_Dis$NPS_Score <-scale(GamingAccessoryData_Dis$NPS_Score); 
GamingAccessoryData_Dis$Discount_Lag_1 <- scale(GamingAccessoryData_Dis$Discount_Lag_1);
GamingAccessoryData_Dis$Discount_Lag_2 <- scale(GamingAccessoryData_Dis$Discount_Lag_2);
GamingAccessoryData_Dis$Discount_Lag_3 <- scale(GamingAccessoryData_Dis$Discount_Lag_3);

GamingAccessoryData_Dis$SP_Lag_1 <- scale(GamingAccessoryData_Dis$SP_Lag_1);
GamingAccessoryData_Dis$SP_Lag_2 <- scale(GamingAccessoryData_Dis$SP_Lag_2);
GamingAccessoryData_Dis$SP_Lag_3 <- scale(GamingAccessoryData_Dis$SP_Lag_3);

GamingAccessoryData_Dis$MA_Increase_SP <- scale(GamingAccessoryData_Dis$MA_Increase_SP);
GamingAccessoryData_Dis$MA_Increase_Discount <- scale(GamingAccessoryData_Dis$MA_Increase_Discount);
GamingAccessoryData_Dis$`gmv-1` <- scale(GamingAccessoryData_Dis$`gmv-1`);
GamingAccessoryData_Dis$`gmv-2` <- scale(GamingAccessoryData_Dis$`gmv-2`);
GamingAccessoryData_Dis$`gmv-3` <- scale(GamingAccessoryData_Dis$`gmv-3`);

GamingAccessoryData_Dis$SellingPrice <- scale(GamingAccessoryData_Dis$SellingPrice);
GamingAccessoryData_Dis$`SellingPrice-1` <- scale(GamingAccessoryData_Dis$`SellingPrice-1`);
GamingAccessoryData_Dis$`SellingPrice-2` <- scale(GamingAccessoryData_Dis$`SellingPrice-2`);
GamingAccessoryData_Dis$`SellingPrice-3` <- scale(GamingAccessoryData_Dis$`SellingPrice-3`);



# d) Modelling

Gaming_Dis_Model_1 <- lm(gmv~.,GamingAccessoryData_Dis);

Gaming_Dis_Model_2 <- stepAIC(Gaming_Dis_Model_1,direction = "both")
summary(Gaming_Dis_Model_2);
vif(Gaming_Dis_Model_2);
#Adjusted R-squared:   0.9 

#deliverybdays and deliverycdays has high vif (9020.20 , 9478.70 )
#p value  2.9e-05 ***, 5.6e-05 ***

# Removing deliverycdays

Gaming_Dis_Model_3 <- lm(formula = gmv ~ SellingPrice + Discount + procurement_sla + 
                           No_of_Holidays + deliverybdays +  TV_adstock + 
                           Digital_adstock + Sponsorship_adstock + Affiliates_adstock + 
                           SEM_adstock + Radio_adstock + Other_adstock + NPS_Score + 
                           IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + SP_Lag_1 + 
                           SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                           `gmv-3` + `SellingPrice-1` + `SellingPrice-2` + `Discount-1` + 
                           `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_3);
vif(Gaming_Dis_Model_3);
#Adjusted R-squared:   0.79


#Discount_Lag_2 , vif -651.70 
#p value    0.41758     
# Removing it

Gaming_Dis_Model_4 <- lm(formula = gmv ~ SellingPrice + Discount + procurement_sla + 
                           No_of_Holidays + deliverybdays +  TV_adstock + 
                           Digital_adstock + Sponsorship_adstock + Affiliates_adstock + 
                           SEM_adstock + Radio_adstock + Other_adstock + NPS_Score + 
                           IsHolidayWeek2 + Discount_Lag_1 + SP_Lag_1 + 
                           SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                           `gmv-3` + `SellingPrice-1` + `SellingPrice-2` + `Discount-1` + 
                           `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_4);
vif(Gaming_Dis_Model_4);
#Adjusted R-squared:   0.793

#Discount_Lag_1 , vif -407.03 
#p value   0.15219    
# Removing it

Gaming_Dis_Model_5 <- lm(formula = gmv ~ SellingPrice + Discount + procurement_sla + 
                           No_of_Holidays + deliverybdays +  TV_adstock + 
                           Digital_adstock + Sponsorship_adstock + Affiliates_adstock + 
                           SEM_adstock + Radio_adstock + Other_adstock + NPS_Score + 
                           IsHolidayWeek2 +  SP_Lag_1 + 
                           SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                           `gmv-3` + `SellingPrice-1` + `SellingPrice-2` + `Discount-1` + 
                           `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_5);
vif(Gaming_Dis_Model_5);
#Adjusted R-squared:   0.783



#SellingPrice , vif - 380.60 
#p value  0.63133       
# Removing it

Gaming_Dis_Model_6 <- lm(formula = gmv ~  Discount + procurement_sla + 
                           No_of_Holidays + deliverybdays +  TV_adstock + 
                           Digital_adstock + Sponsorship_adstock + Affiliates_adstock + 
                           SEM_adstock + Radio_adstock + Other_adstock + NPS_Score + 
                           IsHolidayWeek2 +  SP_Lag_1 + 
                           SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                           `gmv-3` + `SellingPrice-1` + `SellingPrice-2` + `Discount-1` + 
                           `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_6);
vif(Gaming_Dis_Model_6);
#Adjusted R-squared:   0.789



#SP_Lag_2 , vif -  17.12   
#p value 0.57624    
# Removing it

Gaming_Dis_Model_7 <- lm(formula = gmv ~  Discount + procurement_sla + 
                           No_of_Holidays + deliverybdays +  TV_adstock + 
                           Digital_adstock + Sponsorship_adstock + Affiliates_adstock + 
                           SEM_adstock + Radio_adstock + Other_adstock + NPS_Score + 
                           IsHolidayWeek2 +  SP_Lag_1 + 
                           SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                           `gmv-3` + `SellingPrice-1` + `SellingPrice-2` + `Discount-1` + 
                           `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_7);
vif(Gaming_Dis_Model_7);
#Adjusted R-squared:   0.795



#`SellingPrice-2` , vif -   46.97 
#p value 0.12548    
# Removing it

Gaming_Dis_Model_8 <- lm(formula = gmv ~  Discount + procurement_sla + 
                           No_of_Holidays + deliverybdays +  TV_adstock + 
                           Digital_adstock + Sponsorship_adstock + Affiliates_adstock + 
                           SEM_adstock + Radio_adstock + Other_adstock + NPS_Score + 
                           IsHolidayWeek2 +  SP_Lag_1 + 
                           SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                           `gmv-3` + `SellingPrice-1` + `Discount-1` + 
                           `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_8);
vif(Gaming_Dis_Model_8);
#Adjusted R-squared:   0.783




#NPS_Score , vif -  44.52
#p value 0.10219    
# Removing it

Gaming_Dis_Model_9<- lm(formula = gmv ~  Discount + procurement_sla + 
                           No_of_Holidays + deliverybdays +  TV_adstock + 
                           Digital_adstock + Sponsorship_adstock + Affiliates_adstock + 
                           SEM_adstock + Radio_adstock + Other_adstock + 
                           IsHolidayWeek2 +  SP_Lag_1 + 
                           SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                           `gmv-3` + `SellingPrice-1` + `Discount-1` + 
                           `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_9);
vif(Gaming_Dis_Model_9);
#Adjusted R-squared:   0.769




#Digital_adstock , vif - 360.35
#p value 0.17768      
# Removing it

Gaming_Dis_Model_10<- lm(formula = gmv ~  Discount + procurement_sla + 
                          No_of_Holidays + deliverybdays +  TV_adstock + 
                           Sponsorship_adstock + Affiliates_adstock + 
                          SEM_adstock + Radio_adstock + Other_adstock + 
                          IsHolidayWeek2 +  SP_Lag_1 + 
                          SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                          `gmv-3` + `SellingPrice-1` + `Discount-1` + 
                          `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_10);
vif(Gaming_Dis_Model_10);
#Adjusted R-squared:   0.762




#SP_Lag_1  , vif -  18.73  
#p value 0.34749    
# Removing it

Gaming_Dis_Model_11<- lm(formula = gmv ~  Discount + procurement_sla + 
                           No_of_Holidays + deliverybdays +  TV_adstock + 
                           Sponsorship_adstock + Affiliates_adstock + 
                           SEM_adstock + Radio_adstock + Other_adstock + 
                           IsHolidayWeek2 +  
                           SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                           `gmv-3` + `SellingPrice-1` + `Discount-1` + 
                           `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_11);
vif(Gaming_Dis_Model_11);
#Adjusted R-squared:   0.762


#Discount  , vif -  275.81
#p value 0.01062 *  
# Removing it

Gaming_Dis_Model_12<- lm(formula = gmv ~   procurement_sla + 
                           No_of_Holidays + deliverybdays +  TV_adstock + 
                           Sponsorship_adstock + Affiliates_adstock + 
                           SEM_adstock + Radio_adstock + Other_adstock + 
                           IsHolidayWeek2 +  
                           SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                           `gmv-3` + `SellingPrice-1` + `Discount-1` + 
                           `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_12);
vif(Gaming_Dis_Model_12);
#Adjusted R-squared:   0.713



#Radio_adstock  , vif -   90.33  
#p value 0.07275 .  
# Removing it

Gaming_Dis_Model_13 <- lm(formula = gmv ~   procurement_sla + 
                           No_of_Holidays + deliverybdays +  TV_adstock + 
                           Sponsorship_adstock + Affiliates_adstock + 
                           SEM_adstock +  Other_adstock + 
                           IsHolidayWeek2 +  
                           SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                           `gmv-3` + `SellingPrice-1` + `Discount-1` + 
                           `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_13);
vif(Gaming_Dis_Model_13);
#Adjusted R-squared:   0.691

#`gmv-3`    
#p value 0.61510    
# Removing it

Gaming_Dis_Model_14 <- lm(formula = gmv ~   procurement_sla + 
                            No_of_Holidays + deliverybdays +  TV_adstock + 
                            Sponsorship_adstock + Affiliates_adstock + 
                            SEM_adstock +  Other_adstock + 
                            IsHolidayWeek2 +  
                            SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                            `SellingPrice-1` + `Discount-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_14);
vif(Gaming_Dis_Model_14);
#Adjusted R-squared:   0.698


#No_of_Holidays
#p value  0.58165    
# Removing it

Gaming_Dis_Model_15 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays +  TV_adstock + 
                            Sponsorship_adstock + Affiliates_adstock + 
                            SEM_adstock +  Other_adstock + 
                            IsHolidayWeek2 +  
                            SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                            `SellingPrice-1` + `Discount-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_15);
vif(Gaming_Dis_Model_15);
#Adjusted R-squared:   0.704



#SEM_adstock
#p value  0.59552    
# Removing it

Gaming_Dis_Model_16 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays +  TV_adstock + 
                            Sponsorship_adstock + Affiliates_adstock + 
                             Other_adstock + 
                            IsHolidayWeek2 +  
                            SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                            `SellingPrice-1` + `Discount-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_16);
vif(Gaming_Dis_Model_16);
#Adjusted R-squared:   0.71


#`gmv-2` 
#p value  0.09622 .  
# Removing it

Gaming_Dis_Model_17 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays +  TV_adstock + 
                            Sponsorship_adstock + Affiliates_adstock + 
                            Other_adstock + 
                            IsHolidayWeek2 +  
                            SP_Lag_3 + MA_Increase_Discount + `gmv-1`+
                            `SellingPrice-1` + `Discount-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_17);
vif(Gaming_Dis_Model_17);
#Adjusted R-squared:   0.695


#IsHolidayWeek2
#p value 0.1528    
# Removing it

Gaming_Dis_Model_18 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays +  TV_adstock + 
                            Sponsorship_adstock + Affiliates_adstock + 
                            Other_adstock + 
                            SP_Lag_3 + MA_Increase_Discount + `gmv-1`+
                            `SellingPrice-1` + `Discount-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_18);
vif(Gaming_Dis_Model_18);
#Adjusted R-squared:   0.685



#MA_Increase_Discount
#p value 0.1872    
# Removing it

Gaming_Dis_Model_19 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays +  TV_adstock + 
                            Sponsorship_adstock + Affiliates_adstock + 
                            Other_adstock + 
                            SP_Lag_3 +  `gmv-1`+
                            `SellingPrice-1` + `Discount-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_19);
vif(Gaming_Dis_Model_19);
#Adjusted R-squared:   0.679



#TV_adstock
#p value0.03344 *  
# Removing it

Gaming_Dis_Model_20 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays + 
                            Sponsorship_adstock + Affiliates_adstock + 
                            Other_adstock + 
                            SP_Lag_3 +  `gmv-1`+
                            `SellingPrice-1` + `Discount-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_20);
vif(Gaming_Dis_Model_20);
#Adjusted R-squared:   0.647


#Other_adstock
#p value 0.08250 .  
# Removing it

Gaming_Dis_Model_21 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays + 
                            Sponsorship_adstock + Affiliates_adstock + 
                            SP_Lag_3 +  `gmv-1`+
                            `SellingPrice-1` + `Discount-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_21);
vif(Gaming_Dis_Model_21);
#Adjusted R-squared:   0.628


#Sponsorship_adstock
#p value  0.1905    
# Removing it

Gaming_Dis_Model_22 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays + 
                             Affiliates_adstock + 
                            SP_Lag_3 +  `gmv-1`+
                            `SellingPrice-1` + `Discount-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_22);
vif(Gaming_Dis_Model_22);
#Adjusted R-squared:   0.621

#SP_Lag_3
#p value  0.0159 *  
# Removing it

Gaming_Dis_Model_23 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays + 
                            Affiliates_adstock + 
                             `gmv-1`+
                            `SellingPrice-1` + `Discount-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_23);
vif(Gaming_Dis_Model_23);
#Adjusted R-squared:   0.573


#`Discount-1`      
#p value  0.19704    
# Removing it

Gaming_Dis_Model_24 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays + 
                            Affiliates_adstock + 
                            `gmv-1`+
                            `SellingPrice-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_24);
vif(Gaming_Dis_Model_24);
#Adjusted R-squared:   0.565


#`gmv-1` 
#p value  0.15977    
# Removing it

Gaming_Dis_Model_25 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays + 
                            Affiliates_adstock + 
                            `SellingPrice-1` + 
                            `Discount-2`, data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_25);
vif(Gaming_Dis_Model_25);
#Adjusted R-squared:   0.555


#`Discount-2` 
#p value   0.11490  
# Removing it

Gaming_Dis_Model_26 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays + 
                            Affiliates_adstock + 
                            `SellingPrice-1` , data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_26);
vif(Gaming_Dis_Model_26);
#Adjusted R-squared:   0.539


#`SellingPrice-1`
#p value   0.10345 
# Removing it

Gaming_Dis_Model_27 <- lm(formula = gmv ~   procurement_sla + 
                            deliverybdays + 
                            Affiliates_adstock , data = GamingAccessoryData_Dis);
summary(Gaming_Dis_Model_27);
vif(Gaming_Dis_Model_27);
#Adjusted R-squared:   0.522




Final_Gaming_Dis_Model <- Gaming_Dis_Model_27;
#Final Adjusted R-squared:  0.522

#---------------------------------------------------------------------------------------------------------------------------------------------------------

# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = GamingAccessoryData_Dis, form.lm = formula( gmv ~   procurement_sla + 
                                                                              deliverybdays + 
                                                                              Affiliates_adstock ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval_1, "ms")
# 0.578


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Gaming_Dis_Model$coefficients[var]*
                       mean(GamingAccessoryData_Dis[,var])/mean(GamingAccessoryData_Dis$gmv))
  
  return(elax1)
} 


Gaming_Dis_Var_Elasticity <- data.frame(names(Final_Gaming_Dis_Model$coefficients)[-1]);

for (i in 2:length(Final_Gaming_Dis_Model$coefficients)) {
  Gaming_Dis_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Gaming_Dis_Model$coefficients)[i]);
}
Gaming_Dis_Var_Elasticity$Direction <- ifelse(Gaming_Dis_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Gaming_Dis_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Gaming_Dis_Var_Elasticity


ggplot(data=Gaming_Dis_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Distributed Lag Model") +xlab("Variables")



####################################################################################################################################
#------------------------------------ 5. Multiplicative and distributed model ----------------------------------------------------
####################################################################################################################################


# a) Load Gaming Accessory Data

GamingAccessoryData_MulDis <- read.csv('GamingAccessoryData.csv',stringsAsFactors = FALSE);


# b) Removing Unnecessary variables 

GamingAccessoryData_MulDis$product_analytic_sub_category <- NULL;
GamingAccessoryData_MulDis$X <- NULL;

GamingAccessoryData_MulDis$Week <- NULL;
GamingAccessoryData_MulDis$no_of_orders <- NULL;
GamingAccessoryData_MulDis$units <- NULL;

GamingAccessoryData_MulDis$COD <- NULL;
GamingAccessoryData_MulDis$Prepaid <- NULL;
GamingAccessoryData_MulDis$Mass_p <- NULL;
GamingAccessoryData_MulDis$Middle_p <- NULL;
GamingAccessoryData_MulDis$Premium_p <- NULL;

GamingAccessoryData_MulDis$product_mrp <- NULL;
GamingAccessoryData_MulDis$MA_Discount <- NULL;
GamingAccessoryData_MulDis$MA_SellingPrice <- NULL;


GamingAccessoryData_MulDis <- slide(GamingAccessoryData_MulDis, Var = "gmv",slideBy = -1)
GamingAccessoryData_MulDis <- slide(GamingAccessoryData_MulDis, Var = "gmv",slideBy = -2)
GamingAccessoryData_MulDis <- slide(GamingAccessoryData_MulDis, Var = "gmv",slideBy = -3)


GamingAccessoryData_MulDis <- slide(GamingAccessoryData_MulDis, Var = "SellingPrice",slideBy = -1)
GamingAccessoryData_MulDis <- slide(GamingAccessoryData_MulDis, Var = "SellingPrice",slideBy = -2)
GamingAccessoryData_MulDis <- slide(GamingAccessoryData_MulDis, Var = "SellingPrice",slideBy = -3)


GamingAccessoryData_MulDis <- slide(GamingAccessoryData_MulDis, Var = "Discount",slideBy = -1)
GamingAccessoryData_MulDis <- slide(GamingAccessoryData_MulDis, Var = "Discount",slideBy = -2)
GamingAccessoryData_MulDis <- slide(GamingAccessoryData_MulDis, Var = "Discount",slideBy = -3)



# c) Removing 0 values in data changing from 0 to 0.01. As log 0 will be undefined.

GamingAccessoryData_MulDis <- na.omit(GamingAccessoryData_MulDis);

# Looking for 0 values in data
sapply(GamingAccessoryData_MulDis, function(x){sum(x==0)})

GamingAccessoryData_MulDis[GamingAccessoryData_MulDis == 0] <- 0.001;


sapply(GamingAccessoryData_MulDis, function(x){sum(x<0)})
#There are negative values.

# For negative values we apply the following transaformation -> -log(-x + 1) , Yeo-Johnson Power Transformations

for(i in 1:ncol(GamingAccessoryData_MulDis)) {
  GamingAccessoryData_MulDis[,c(i)]<- ifelse(
    GamingAccessoryData_MulDis[,c(i)]>0,
    log(GamingAccessoryData_MulDis[,c(i)]), 
    -log(-(GamingAccessoryData_MulDis[,c(i)])+1)
  );
}


# d) Modelling

model_muldis_1 <- lm(gmv ~ ., GamingAccessoryData_MulDis);

model_muldis_2 <-stepAIC(model_muldis_1,direction = "both")
summary(model_muldis_2);
vif(model_muldis_2);
#Adjusted R-squared:  0.872 

#Online.marketing_adstock, vif - 8440.70 , p value - 0.19192 
#Removing it
model_muldis_3 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       deliverybdays + deliverycdays + TV_adstock + Sponsorship_adstock + 
                       Affiliates_adstock + Radio_adstock + 
                       Other_adstock + NPS_Score + IsHolidayWeek1 + SP_Lag_1 + SP_Lag_2 + 
                       SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_3);
vif(model_muldis_3);
#Adjusted R-squared:  0.872 


#deliverybdays, vif -  4497.30 , p value -  0.17964    
#Removing it
model_muldis_4 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       deliverycdays + TV_adstock + Sponsorship_adstock + 
                       Affiliates_adstock + Radio_adstock + 
                       Other_adstock + NPS_Score + IsHolidayWeek1 + SP_Lag_1 + SP_Lag_2 + 
                       SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_4);
vif(model_muldis_4);
#Adjusted R-squared:  0.864 


#Other_adstock, vif - 313.89 , p value -  0.26959    
#Removing it
model_muldis_5 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       deliverycdays + TV_adstock + Sponsorship_adstock + 
                       Affiliates_adstock + Radio_adstock + 
                        NPS_Score + IsHolidayWeek1 + SP_Lag_1 + SP_Lag_2 + 
                       SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_5);
vif(model_muldis_5);
#Adjusted R-squared:  0.863


#Sponsorship_adstock, vif - 16.69  , p value -    0.8949    
#Removing it
model_muldis_6 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       deliverycdays + TV_adstock + 
                       Affiliates_adstock + Radio_adstock + 
                       NPS_Score + IsHolidayWeek1 + SP_Lag_1 + SP_Lag_2 + 
                       SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_6);
vif(model_muldis_6);
#Adjusted R-squared:  0.867



#SP_Lag_2, vif - 853.60    , p value -   0.00107 ** 
#Removing it
model_muldis_7<- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       deliverycdays + TV_adstock + 
                       Affiliates_adstock + Radio_adstock + 
                       NPS_Score + IsHolidayWeek1 + SP_Lag_1 +
                       SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_7);
vif(model_muldis_7);
#Adjusted R-squared:  0.813



#NPS_Score, vif -  10.02   , p value -  0.5739  
#Removing it
model_muldis_8<- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                      deliverycdays + TV_adstock + 
                      Affiliates_adstock + Radio_adstock + 
                       IsHolidayWeek1 + SP_Lag_1 +
                      SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                      `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                    data = GamingAccessoryData_MulDis);

summary(model_muldis_8);
vif(model_muldis_8);
#Adjusted R-squared:  0.817




#TV_adstock, vif -   17.41    , p value -  0.0555 . 
#Removing it
model_muldis_9<- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                      deliverycdays + 
                      Affiliates_adstock + Radio_adstock + 
                      IsHolidayWeek1 + SP_Lag_1 +
                      SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                      `SellingPrice-2` + `SellingPrice-3` + `Discount-2` + `Discount-3`, 
                    data = GamingAccessoryData_MulDis);

summary(model_muldis_9);
vif(model_muldis_9);
#Adjusted R-squared:  0.801




#`Discount-3`, vif -    5.11   , p value -   0.9996    
#Removing it
model_muldis_10<- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                      deliverycdays + 
                      Affiliates_adstock + Radio_adstock + 
                      IsHolidayWeek1 + SP_Lag_1 +
                      SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                      `SellingPrice-2` + `SellingPrice-3` + `Discount-2` ,
                    data = GamingAccessoryData_MulDis);

summary(model_muldis_10);
vif(model_muldis_10);
#Adjusted R-squared:  0.807


#SP_Lag_1, vif -    1.85   , p value -    0.9917 
#Removing it
model_muldis_11<- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       deliverycdays + 
                       Affiliates_adstock + Radio_adstock + 
                       IsHolidayWeek1 +
                       SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                       `SellingPrice-2` + `SellingPrice-3` + `Discount-2` ,
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_11);
vif(model_muldis_11);
#Adjusted R-squared:  0.812


#`SellingPrice-3` , vif -  1.87 , p value -   0.9424 
#Removing it
model_muldis_12<- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       deliverycdays + 
                       Affiliates_adstock + Radio_adstock + 
                       IsHolidayWeek1 +
                       SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                       `SellingPrice-2` + `Discount-2` ,
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_12);
vif(model_muldis_12);
#Adjusted R-squared:  0.818


#deliverycdays , vif - 4, p value -   0.8881
#Removing it
model_muldis_13<- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       Affiliates_adstock + Radio_adstock + 
                       IsHolidayWeek1 +
                       SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                       `SellingPrice-2` + `Discount-2` ,
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_13);
vif(model_muldis_13);
#Adjusted R-squared:  0.823



#Radio_adstock , p value - 0.8726 
#Removing it
model_muldis_14<- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       Affiliates_adstock + 
                       IsHolidayWeek1 +
                       SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-3` + 
                       `SellingPrice-2` + `Discount-2` ,
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_14);
vif(model_muldis_14);
#Adjusted R-squared:  0.827


#MA_Increase_Discount , p value - 0.8577
#Removing it
model_muldis_15<- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       Affiliates_adstock + 
                       IsHolidayWeek1 +
                       SP_Lag_3 + MA_Increase_SP +  `gmv-3` + 
                       `SellingPrice-2` + `Discount-2` ,
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_15);
vif(model_muldis_15);
#Adjusted R-squared:  0.832


#SP_Lag_3 , p value - 0.73561   
#Removing it
model_muldis_16<- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       Affiliates_adstock + 
                       IsHolidayWeek1 +
                       MA_Increase_SP +  `gmv-3` + 
                       `SellingPrice-2` + `Discount-2` ,
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_16);
vif(model_muldis_16);
#Adjusted R-squared:  0.836


#`gmv-3`  , p value -  0.75423 
#Removing it
model_muldis_17 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       Affiliates_adstock + 
                       IsHolidayWeek1 +
                       MA_Increase_SP +  
                       `SellingPrice-2` + `Discount-2` ,
                     data = GamingAccessoryData_MulDis);

summary(model_muldis_17);
vif(model_muldis_17);
#Adjusted R-squared:  0.839


#IsHolidayWeek1  , p value -   0.67576    
#Removing it
model_muldis_18 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                        Affiliates_adstock + 
                        MA_Increase_SP +  
                        `SellingPrice-2` + `Discount-2` ,
                      data = GamingAccessoryData_MulDis);

summary(model_muldis_18);
vif(model_muldis_18);
#Adjusted R-squared:  0.842


#MA_Increase_SP  , p value - 0.57214   
#Removing it
model_muldis_19 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                        Affiliates_adstock + 
                        `SellingPrice-2` + `Discount-2` ,
                      data = GamingAccessoryData_MulDis);

summary(model_muldis_19);
vif(model_muldis_19);
#Adjusted R-squared:  0.845


#Discount  , p value - 0.58460  
#Removing it
model_muldis_20 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Affiliates_adstock + 
                        `SellingPrice-2` + `Discount-2` ,
                      data = GamingAccessoryData_MulDis);

summary(model_muldis_20);
vif(model_muldis_20);
#Adjusted R-squared:  0.847


#`SellingPrice-2`  , p value - 0.40795  
#Removing it
model_muldis_21 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Affiliates_adstock + 
                        `Discount-2` ,
                      data = GamingAccessoryData_MulDis);

summary(model_muldis_21);
vif(model_muldis_21);
#Adjusted R-squared:  0.849


#`Discount-2`   , p value -0.11754 
#Removing it
model_muldis_22 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Affiliates_adstock  ,
                      data = GamingAccessoryData_MulDis);

summary(model_muldis_22);
vif(model_muldis_22);
#Adjusted R-squared:  0.843


#sla  , p value -0.01119 * 
#Removing it
model_muldis_23 <- lm(formula = gmv ~ SellingPrice +  procurement_sla + 
                        Affiliates_adstock  ,
                      data = GamingAccessoryData_MulDis);

summary(model_muldis_23);
vif(model_muldis_23);
#Adjusted R-squared:  0.823




Final_Gaming_MulDis_Model <- model_muldis_23;
#Final Adjusted R-squared:  0.823

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = GamingAccessoryData_MulDis, form.lm = formula( gmv ~ SellingPrice +  procurement_sla + 
                                                                                 Affiliates_adstock   ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval_1, "ms")
# 1.01


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Gaming_MulDis_Model$coefficients[var]*
                       mean(GamingAccessoryData_MulDis[,var])/mean(GamingAccessoryData_MulDis$gmv))
  
  return(elax1)
} 


Gaming_MulDis_Var_Elasticity <- data.frame(names(Final_Gaming_MulDis_Model$coefficients)[-1]);

for (i in 2:length(Final_Gaming_MulDis_Model$coefficients)) {
  Gaming_MulDis_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Gaming_MulDis_Model$coefficients)[i]);
}
Gaming_MulDis_Var_Elasticity$Direction <- ifelse(Gaming_MulDis_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Gaming_MulDis_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Gaming_MulDis_Var_Elasticity


ggplot(data=Gaming_MulDis_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Disributed + Multiplicative Model") +xlab("Variables")


#-------------------------------------------------------------------------------------------------------------------------------------

