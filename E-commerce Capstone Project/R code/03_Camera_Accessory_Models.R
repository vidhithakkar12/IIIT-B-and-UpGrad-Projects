#############################################################################################################################################################
#------------------------------------------------ Camera Accessory - Model Building -------------------------------------------------------------------------
#############################################################################################################################################################

# All necessary packages have been installed in the data preparation file

############################################################################################################################################################
#--------------------------------------------------- 1. Linear Model ------------------------------------------------------------------------------------------
############################################################################################################################################################

# a) Load Camera Accessory Data

CameraAccessoryData <- read.csv('CameraAccessoryData.csv',stringsAsFactors = FALSE);

str(CameraAccessoryData);
colnames(CameraAccessoryData);

# b) Removing Unnecessary variables 

CameraAccessoryData$product_analytic_sub_category <- NULL;
CameraAccessoryData$X <- NULL;

CameraAccessoryData$Week <- NULL;
CameraAccessoryData$no_of_orders <- NULL;
CameraAccessoryData$units <- NULL;

CameraAccessoryData$COD <- NULL;
CameraAccessoryData$Prepaid <- NULL;
CameraAccessoryData$Mass_p <- NULL;
CameraAccessoryData$Middle_p <- NULL;
CameraAccessoryData$Premium_p <- NULL;

CameraAccessoryData$product_mrp <- NULL;
CameraAccessoryData$SellingPrice <- NULL;
CameraAccessoryData$MA_Discount <- NULL;
CameraAccessoryData$MA_SellingPrice <- NULL;

# c) Scaling the variables for modelling

CameraAccessoryData$gmv <- scale(CameraAccessoryData$gmv);
CameraAccessoryData$Discount <- scale(CameraAccessoryData$Discount);
CameraAccessoryData$sla <- scale(CameraAccessoryData$sla);
CameraAccessoryData$procurement_sla <- scale(CameraAccessoryData$procurement_sla);

CameraAccessoryData$No_of_Holidays <- scale(CameraAccessoryData$No_of_Holidays)
CameraAccessoryData$deliverybdays <- scale(CameraAccessoryData$deliverybdays);

CameraAccessoryData$deliverycdays <- scale(CameraAccessoryData$deliverycdays);

CameraAccessoryData$TV_adstock <- scale(CameraAccessoryData$TV_adstock);
CameraAccessoryData$Digital_adstock <- scale(CameraAccessoryData$Digital_adstock);

CameraAccessoryData$Sponsorship_adstock <- scale(CameraAccessoryData$Sponsorship_adstock);
CameraAccessoryData$`Content.Marketing_adstock` <- scale(CameraAccessoryData$`Content.Marketing_adstock`);
CameraAccessoryData$`Online.marketing_adstock` <- scale(CameraAccessoryData$`Online.marketing_adstock`);

CameraAccessoryData$Affiliates_adstock <- scale(CameraAccessoryData$Affiliates_adstock);
CameraAccessoryData$SEM_adstock <- scale(CameraAccessoryData$SEM_adstock);
CameraAccessoryData$Radio_adstock <- scale(CameraAccessoryData$Radio_adstock);

CameraAccessoryData$Other_adstock <-scale(CameraAccessoryData$Other_adstock);
CameraAccessoryData$NPS_Score <-scale(CameraAccessoryData$NPS_Score); 
CameraAccessoryData$Discount_Lag_1 <- scale(CameraAccessoryData$Discount_Lag_1);
CameraAccessoryData$Discount_Lag_2 <- scale(CameraAccessoryData$Discount_Lag_2);
CameraAccessoryData$Discount_Lag_3 <- scale(CameraAccessoryData$Discount_Lag_3);

CameraAccessoryData$SP_Lag_1 <- scale(CameraAccessoryData$SP_Lag_1);
CameraAccessoryData$SP_Lag_2 <- scale(CameraAccessoryData$SP_Lag_2);
CameraAccessoryData$SP_Lag_3 <- scale(CameraAccessoryData$SP_Lag_3);

CameraAccessoryData$MA_Increase_SP <- scale(CameraAccessoryData$MA_Increase_SP);
CameraAccessoryData$MA_Increase_Discount <- scale(CameraAccessoryData$MA_Increase_Discount);

# d) Modelling

set.seed(100);

model_1 <- lm(gmv~.,CameraAccessoryData)

model_2 <- stepAIC(model_1,direction = "both")

model_2
summary(model_2);
vif(model_2);
#Adjusted R-squared:  0.743

#deliverybdays and deliverycdays has high vif (  1568.00    ,  1570.50    ), 
#P value -  0.00178 **  , 0.00140 **  
#cor(CameraAccessoryData$deliverybdays,CameraAccessoryData$deliverycdays) - 1, high correlation
# We will remove deliverybdays

model_3 <- lm(formula = gmv ~ Discount + sla + IsHolidayWeek + deliverycdays + 
                Online.marketing_adstock + Affiliates_adstock + 
                Radio_adstock + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                SP_Lag_2 + MA_Increase_SP + MA_Increase_Discount, data = CameraAccessoryData);

summary(model_3);
vif(model_3);
#Adjusted R-squared:  0.673


#Affiliates_adstock, vif - 81.24, p value - 0.13324
#Removing it.
model_4 <- lm(formula = gmv ~ Discount + sla + IsHolidayWeek + deliverycdays + 
                Online.marketing_adstock + 
                Radio_adstock + IsHolidayWeek2 + Discount_Lag_1 + Discount_Lag_2 + 
                SP_Lag_2 + MA_Increase_SP + MA_Increase_Discount, data = CameraAccessoryData);

summary(model_4);
vif(model_4);
#Adjusted R-squared:  0.662 


#Discount_Lag_1, vif - 95.98, p value - 0.00398 ** .
#Removing it.
model_5 <- lm(formula = gmv ~ Discount + sla + IsHolidayWeek + deliverycdays + 
                Online.marketing_adstock + 
                Radio_adstock + IsHolidayWeek2 +  Discount_Lag_2 + 
                SP_Lag_2 + MA_Increase_SP + MA_Increase_Discount, data = CameraAccessoryData);

summary(model_5);
vif(model_5);
#Adjusted R-squared:  0.591 

#Discount - vif - 11.27, p value - 0.7679
#Removing it.
model_6 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverycdays + 
                Online.marketing_adstock + 
                Radio_adstock + IsHolidayWeek2 +  Discount_Lag_2 + 
                SP_Lag_2 + MA_Increase_SP + MA_Increase_Discount, data = CameraAccessoryData);

summary(model_6);
vif(model_6);
#Adjusted R-squared:  0.589 

# MA_Increase_SP, vif - 5.27, p value - 0.10703 
#Removing it.
model_7 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverycdays + 
                Online.marketing_adstock + 
                Radio_adstock + IsHolidayWeek2 +  Discount_Lag_2 + 
                SP_Lag_2 + MA_Increase_Discount, data = CameraAccessoryData);

summary(model_7);
vif(model_7);
#Adjusted R-squared:  0.572 

#Discount_Lag_2, vif - 8.19, p value - 0.0044 ** 
#Removing it.
model_8 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverycdays + 
                Online.marketing_adstock + 
                Radio_adstock + IsHolidayWeek2 +  
                SP_Lag_2 + MA_Increase_Discount, data = CameraAccessoryData);

summary(model_8);
vif(model_8);
#Adjusted R-squared:  0.492 

#All vif are now bw 1 and 2 . So considering only p values to remove  variables.

#Radio_adstock, p value  - 0.6576.
#Removing it.
model_9 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverycdays + 
                Online.marketing_adstock + 
                IsHolidayWeek2 +  
                SP_Lag_2 + MA_Increase_Discount, data = CameraAccessoryData);

summary(model_9);
vif(model_9);
#Adjusted R-squared:  0.501 

#SP_Lag_2, p value  - 0.3235 
#Removing it.
model_10 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverycdays + 
                Online.marketing_adstock + 
                IsHolidayWeek2 +  
                 MA_Increase_Discount, data = CameraAccessoryData);

summary(model_10);
vif(model_10);
#Adjusted R-squared:  0.501

#MA_Increase_Discount, p value  - 0.462 
#Removing it.
model_11 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverycdays + 
                 Online.marketing_adstock + 
                 IsHolidayWeek2 , data = CameraAccessoryData);

summary(model_11);
vif(model_11);
#Adjusted R-squared:  0.51 


#IsHolidayWeek, p value  - 0.1963
#Removing it.
model_12 <- lm(formula = gmv ~ sla + deliverycdays + 
                 Online.marketing_adstock + 
                 IsHolidayWeek2 , data = CameraAccessoryData);

summary(model_12);
vif(model_12);
#Adjusted R-squared:  0.499

#deliverycdays, p value - 0.0944 .
#Removing it.
model_13 <- lm(formula = gmv ~ sla + 
                 Online.marketing_adstock + 
                 IsHolidayWeek2 , data = CameraAccessoryData);

summary(model_13);
vif(model_13);
#Adjusted R-squared:  0.479 




Final_Camera_Model <- model_13;
# Final Adjusted R square - 0.479

#----------------------------------------------------------------------------------------------------------------------------------

# 10 fold cross validation
temp_crossval <- cv.lm(data = CameraAccessoryData, form.lm = formula(  gmv ~ sla + 
                                                                           Online.marketing_adstock + 
                                                                           IsHolidayWeek2),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval, "ms")
# 0.572


# Elasticity
elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Camera_Model$coefficients[var]*
                       mean(CameraAccessoryData[,var])/mean(CameraAccessoryData$gmv))
  
  return(elax1)
} 


Camera_Var_Elasticity <- data.frame(names(Final_Camera_Model$coefficients)[-1]);


for (i in 2:length(Final_Camera_Model$coefficients)) {
  Camera_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Camera_Model$coefficients)[i]);
}
Camera_Var_Elasticity$Direction <- ifelse(Camera_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Camera_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Camera_Var_Elasticity


ggplot(data=Camera_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Camera Accessory - Linear Model") +xlab("Variables")



################################################################################################################################################
#-------------------------------------------- 2. Multiplicative Model --------------------------------------------------------------------------------
################################################################################################################################################


# a) Load Camera Accessory Data
CameraAccessoryData_Mul <- read.csv('CameraAccessoryData.csv',stringsAsFactors = FALSE);

# b) Removing Unnecessary variables
CameraAccessoryData_Mul$product_analytic_sub_category <- NULL;
CameraAccessoryData_Mul$X <- NULL;

CameraAccessoryData_Mul$Week <- NULL;
CameraAccessoryData_Mul$no_of_orders <- NULL;
CameraAccessoryData_Mul$units <- NULL;

CameraAccessoryData_Mul$COD <- NULL;
CameraAccessoryData_Mul$Prepaid <- NULL;
CameraAccessoryData_Mul$Mass_p <- NULL;
CameraAccessoryData_Mul$Middle_p <- NULL;
CameraAccessoryData_Mul$Premium_p <- NULL;

CameraAccessoryData_Mul$product_mrp <- NULL;
CameraAccessoryData_Mul$SellingPrice <- NULL;
CameraAccessoryData_Mul$MA_Discount <- NULL;
CameraAccessoryData_Mul$MA_SellingPrice <- NULL;


# c) Removing 0 values in data changing from 0 to 0.01. As log 0 will be undefined.

# Looking for 0 values in data
sapply(CameraAccessoryData_Mul, function(x){sum(x==0)})

CameraAccessoryData_Mul[CameraAccessoryData_Mul == 0] <- 0.001;


sapply(CameraAccessoryData_Mul, function(x){sum(x<0)})
#There are negative values.

# For negative values we apply the following transaformation -> -log(-x + 1) , Yeo-Johnson Power Transformations

for(i in 1:ncol(CameraAccessoryData_Mul)) {
  CameraAccessoryData_Mul[,c(i)]<- ifelse(
    CameraAccessoryData_Mul[,c(i)]>0,
    log(CameraAccessoryData_Mul[,c(i)]), 
    -log(-(CameraAccessoryData_Mul[,c(i)])+1)
  );
}

# d) Modelling

m_model_1 <- lm(gmv ~ ., CameraAccessoryData_Mul);

m_model_2 <-stepAIC(m_model_1,direction = "both")
summary(m_model_2);
vif(m_model_2);

#Adjusted R-squared:  0.881

#deliverybdays, vif - 3585.89 , p value - 0.22260
#Removing it.
m_model_3 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                  Digital_adstock + Sponsorship_adstock + Content.Marketing_adstock + 
                  Online.marketing_adstock + Affiliates_adstock + SEM_adstock + 
                  Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                  Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount, 
                data = CameraAccessoryData_Mul);
summary(m_model_3);
vif(m_model_3);
#Adjusted R-squared:  0.879 


#Affiliates_adstock and Online.marketing_adstock are highly correlated

#Affiliates_adstock, vif - 6076.85, p value - 0.00040 ***
#Removing it.
m_model_4 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                  Digital_adstock + Sponsorship_adstock + Content.Marketing_adstock + 
                  Online.marketing_adstock +  SEM_adstock + 
                  Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                  Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount, 
                data = CameraAccessoryData_Mul);
summary(m_model_4);
vif(m_model_4);
#Adjusted R-squared:  0.827 

#Content.Marketing_adstock , vif - 23.76, p value - 0.468
#Removing it.
m_model_5 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                  Digital_adstock + Sponsorship_adstock +
                  Online.marketing_adstock +  SEM_adstock + 
                  Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                  Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount, 
                data = CameraAccessoryData_Mul);
summary(m_model_5);
vif(m_model_5);
#Adjusted R-squared:  0.829 


#SEM_adstock , vif - 17.03, p value - 0.39902
#Removing it.
m_model_6 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                  Digital_adstock + Sponsorship_adstock +
                  Online.marketing_adstock +   
                  Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                  Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount, 
                data = CameraAccessoryData_Mul);
summary(m_model_6);
vif(m_model_6);
#Adjusted R-squared:  0.83 


#NPS_Score , vif - 12.60, p value - 0.2105
#Removing it.
m_model_7 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                  Digital_adstock + Sponsorship_adstock +
                  Online.marketing_adstock +   
                  Radio_adstock + IsHolidayWeek1 + Discount_Lag_2 + 
                  Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount, 
                data = CameraAccessoryData_Mul);
summary(m_model_7);
vif(m_model_7);
#Adjusted R-squared:  0.827



#Radio_adstock , vif - 5.01, p value - 0.12671
#Removing it.
m_model_8 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                  Digital_adstock + Sponsorship_adstock +
                  Online.marketing_adstock +   
                   IsHolidayWeek1 + Discount_Lag_2 + 
                  Discount_Lag_3 + SP_Lag_1 + SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount, 
                data = CameraAccessoryData_Mul);
summary(m_model_8);
vif(m_model_8);
#Adjusted R-squared:  0.821


#SP_Lag_2 , vif -  8.03 , p value - 0.05563 .
#Removing it.
m_model_9 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                  Digital_adstock + Sponsorship_adstock +
                  Online.marketing_adstock +   
                  IsHolidayWeek1 + Discount_Lag_2 + 
                  Discount_Lag_3 + SP_Lag_1 + SP_Lag_3 + MA_Increase_Discount, 
                data = CameraAccessoryData_Mul);
summary(m_model_9);
vif(m_model_9);
#Adjusted R-squared:  0.808


#Sponsorship_adstock , vif -  5.26 , p value - 0.22625
#Removing it.
m_model_10 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                  Digital_adstock + 
                  Online.marketing_adstock +   
                  IsHolidayWeek1 + Discount_Lag_2 + 
                  Discount_Lag_3 + SP_Lag_1 + SP_Lag_3 + MA_Increase_Discount, 
                data = CameraAccessoryData_Mul);
summary(m_model_10);
vif(m_model_10);
#Adjusted R-squared:  0.805

#All vif are now bw 1 and 2. Considering only p values to remove variables.


#SP_Lag_1 ,  p value -  0.75414 
#Removing it.
m_model_11 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                   Digital_adstock + 
                   Online.marketing_adstock +   
                   IsHolidayWeek1 + Discount_Lag_2 + 
                   Discount_Lag_3 +  SP_Lag_3 + MA_Increase_Discount, 
                 data = CameraAccessoryData_Mul);
summary(m_model_11);
vif(m_model_11);
#Adjusted R-squared:  0.809

#Digital_adstock ,  p value -  0.66587 
#Removing it.
m_model_12 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                   Online.marketing_adstock +   
                   IsHolidayWeek1 + Discount_Lag_2 + 
                   Discount_Lag_3 +  SP_Lag_3 + MA_Increase_Discount, 
                 data = CameraAccessoryData_Mul);
summary(m_model_12);
vif(m_model_12);
#Adjusted R-squared:  0.813

#MA_Increase_Discount ,  p value -  0.36758 
#Removing it.
m_model_13 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                   Online.marketing_adstock +   
                   IsHolidayWeek1 + Discount_Lag_2 + 
                   Discount_Lag_3 +  SP_Lag_3 , 
                 data = CameraAccessoryData_Mul);
summary(m_model_13);
vif(m_model_13);
#Adjusted R-squared:  0.814



#Discount_Lag_2 ,  p value -  0.5185 
#Removing it.
m_model_14 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                   Online.marketing_adstock +   
                   IsHolidayWeek1 +  
                   Discount_Lag_3 +  SP_Lag_3 , 
                 data = CameraAccessoryData_Mul);
summary(m_model_14);
vif(m_model_14);
#Adjusted R-squared:  0.816



#SP_Lag_3 ,  p value -  0.2308 
#Removing it.
m_model_15 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                   Online.marketing_adstock +   
                   IsHolidayWeek1 +  
                   Discount_Lag_3 , 
                 data = CameraAccessoryData_Mul);
summary(m_model_15);
vif(m_model_15);
#Adjusted R-squared:  0.814




#IsHolidayWeek1 ,  p value -   0.01313 * 
#Removing it.
m_model_16<- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                   Online.marketing_adstock + 
                   Discount_Lag_3 , 
                 data = CameraAccessoryData_Mul);
summary(m_model_16);
vif(m_model_16);
#Adjusted R-squared:  0.791


#Discount_Lag_3 ,  p value -  0.00450 ** 
#Removing it.
m_model_17 <- lm(formula = gmv ~ Discount + sla + deliverycdays + 
                  Online.marketing_adstock , 
                data = CameraAccessoryData_Mul);
summary(m_model_17);
vif(m_model_17);
#Adjusted R-squared:  0.756


Final_Camera_mul_model <- m_model_17;
#Final Adjusted R square = 0.756.


#----------------------------------------------------------------------------------------------------------------------------------


# 10 fold cross validation
temp_crossval <- cv.lm(data = CameraAccessoryData_Mul, form.lm = formula( gmv ~ Discount + sla + deliverycdays + 
                                                                              Online.marketing_adstock ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval, "ms")
# 1.84


# Elasticity
elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Camera_mul_model$coefficients[var]*
                       mean(CameraAccessoryData_Mul[,var])/mean(CameraAccessoryData_Mul$gmv))
  
  return(elax1)
} 


Camera_mul_Var_Elasticity <- data.frame(names(Final_Camera_mul_model$coefficients)[-1]);

for (i in 2:length(Final_Camera_mul_model$coefficients)) {
  Camera_mul_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Camera_mul_model$coefficients)[i]);
}
Camera_mul_Var_Elasticity$Direction <- ifelse(Camera_mul_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Camera_mul_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Camera_mul_Var_Elasticity


ggplot(data=Camera_mul_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity", fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative Model") +xlab("Variables")




###########################################################################################################################################################
#------------------------------------------------- 3. Koyck model -------------------------------------------------------------------------------------------
###########################################################################################################################################################


# a) Load Camera Accessory Data

CameraAccessoryData_Koyck <- read.csv('CameraAccessoryData.csv',stringsAsFactors = FALSE);

CameraAccessoryData_Koyck <- slide(CameraAccessoryData_Koyck, Var = "gmv",slideBy = -1)

str(CameraAccessoryData_Koyck);
colnames(CameraAccessoryData_Koyck);


# b) Removing Unnecessary variables

CameraAccessoryData_Koyck$product_analytic_sub_category <- NULL;
CameraAccessoryData_Koyck$X <- NULL;

CameraAccessoryData_Koyck$Week <- NULL;
CameraAccessoryData_Koyck$no_of_orders <- NULL;
CameraAccessoryData_Koyck$units <- NULL;

CameraAccessoryData_Koyck$COD <- NULL;
CameraAccessoryData_Koyck$Prepaid <- NULL;
CameraAccessoryData_Koyck$Mass_p <- NULL;
CameraAccessoryData_Koyck$Middle_p <- NULL;
CameraAccessoryData_Koyck$Premium_p <- NULL;

CameraAccessoryData_Koyck$product_mrp <- NULL;
CameraAccessoryData_Koyck$SellingPrice <- NULL;
CameraAccessoryData_Koyck$MA_Discount <- NULL;
CameraAccessoryData_Koyck$MA_SellingPrice <- NULL;


# c) Scaling all the variables for modelling

CameraAccessoryData_Koyck$gmv <- scale(CameraAccessoryData_Koyck$gmv);
CameraAccessoryData_Koyck$Discount <- scale(CameraAccessoryData_Koyck$Discount);
CameraAccessoryData_Koyck$sla <- scale(CameraAccessoryData_Koyck$sla);
CameraAccessoryData_Koyck$procurement_sla <- scale(CameraAccessoryData_Koyck$procurement_sla);

CameraAccessoryData_Koyck$No_of_Holidays <- scale(CameraAccessoryData_Koyck$No_of_Holidays)
CameraAccessoryData_Koyck$deliverybdays <- scale(CameraAccessoryData_Koyck$deliverybdays);

CameraAccessoryData_Koyck$deliverycdays <- scale(CameraAccessoryData_Koyck$deliverycdays);

CameraAccessoryData_Koyck$TV_adstock <- scale(CameraAccessoryData_Koyck$TV_adstock);
CameraAccessoryData_Koyck$Digital_adstock <- scale(CameraAccessoryData_Koyck$Digital_adstock);

CameraAccessoryData_Koyck$Sponsorship_adstock <- scale(CameraAccessoryData_Koyck$Sponsorship_adstock);
CameraAccessoryData_Koyck$`Content.Marketing_adstock` <- scale(CameraAccessoryData_Koyck$`Content.Marketing_adstock`);
CameraAccessoryData_Koyck$`Online.marketing_adstock` <- scale(CameraAccessoryData_Koyck$`Online.marketing_adstock`);

CameraAccessoryData_Koyck$Affiliates_adstock <- scale(CameraAccessoryData_Koyck$Affiliates_adstock);
CameraAccessoryData_Koyck$SEM_adstock <- scale(CameraAccessoryData_Koyck$SEM_adstock);
CameraAccessoryData_Koyck$Radio_adstock <- scale(CameraAccessoryData_Koyck$Radio_adstock);

CameraAccessoryData_Koyck$Other_adstock <-scale(CameraAccessoryData_Koyck$Other_adstock);
CameraAccessoryData_Koyck$NPS_Score <-scale(CameraAccessoryData_Koyck$NPS_Score); 
CameraAccessoryData_Koyck$Discount_Lag_1 <- scale(CameraAccessoryData_Koyck$Discount_Lag_1);
CameraAccessoryData_Koyck$Discount_Lag_2 <- scale(CameraAccessoryData_Koyck$Discount_Lag_2);
CameraAccessoryData_Koyck$Discount_Lag_3 <- scale(CameraAccessoryData_Koyck$Discount_Lag_3);

CameraAccessoryData_Koyck$SP_Lag_1 <- scale(CameraAccessoryData_Koyck$SP_Lag_1);
CameraAccessoryData_Koyck$SP_Lag_2 <- scale(CameraAccessoryData_Koyck$SP_Lag_2);
CameraAccessoryData_Koyck$SP_Lag_3 <- scale(CameraAccessoryData_Koyck$SP_Lag_3);

CameraAccessoryData_Koyck$MA_Increase_SP <- scale(CameraAccessoryData_Koyck$MA_Increase_SP);
CameraAccessoryData_Koyck$MA_Increase_Discount <- scale(CameraAccessoryData_Koyck$MA_Increase_Discount);
CameraAccessoryData_Koyck$`gmv-1` <- scale(CameraAccessoryData_Koyck$`gmv-1`);


# d) Modelling

Cam_Koyck_Model_1 <- lm(gmv~.,CameraAccessoryData_Koyck);

Cam_Koyck_Model_2 <- stepAIC(Cam_Koyck_Model_1,direction = "both")
summary(Cam_Koyck_Model_2);
vif(Cam_Koyck_Model_2);

#Adjusted R-squared:  0.771 

#deliverycdays , vif -  1726.10 , p value - 0.01049 *
Cam_Koyck_Model_3 <- lm(formula = gmv ~ sla + IsHolidayWeek + No_of_Holidays + deliverybdays + 
                          Online.marketing_adstock + Affiliates_adstock + 
                          Radio_adstock + Other_adstock + IsHolidayWeek1 + IsHolidayWeek2 + 
                          Discount_Lag_1 + Discount_Lag_2 + Discount_Lag_3 + SP_Lag_2 + 
                          SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-1`, 
                        data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_3);
vif(Cam_Koyck_Model_3);
#Adjusted R-squared:  0.725

#Online.marketing_adstock and Affiliates_adstock are higlhly correlated.

#Affiliates_adstock , vif -  138.92 , p value - 0.00011 ***
Cam_Koyck_Model_4 <- lm(formula = gmv ~ sla + IsHolidayWeek + No_of_Holidays + deliverybdays + 
                          Online.marketing_adstock +  
                          Radio_adstock + Other_adstock + IsHolidayWeek1 + IsHolidayWeek2 + 
                          Discount_Lag_1 + Discount_Lag_2 + Discount_Lag_3 + SP_Lag_2 + 
                          SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-1`, 
                        data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_4);
vif(Cam_Koyck_Model_4);
#Adjusted R-squared:  0.572


#Discount_Lag_1 , vif -  57.52 , p value - 0.3344 
Cam_Koyck_Model_5 <- lm(formula = gmv ~ sla + IsHolidayWeek + No_of_Holidays + deliverybdays + 
                          Online.marketing_adstock +  
                          Radio_adstock + Other_adstock + IsHolidayWeek1 + IsHolidayWeek2 + 
                          Discount_Lag_2 + Discount_Lag_3 + SP_Lag_2 + 
                          SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-1`, 
                        data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_5);
vif(Cam_Koyck_Model_5);
#Adjusted R-squared:  0.573



#Radio_adstock , vif -  11.57, p value -   0.7111
Cam_Koyck_Model_6 <- lm(formula = gmv ~ sla + IsHolidayWeek + No_of_Holidays + deliverybdays + 
                          Online.marketing_adstock +  
                           Other_adstock + IsHolidayWeek1 + IsHolidayWeek2 + 
                          Discount_Lag_2 + Discount_Lag_3 + SP_Lag_2 + 
                          SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-1`, 
                        data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_6);
vif(Cam_Koyck_Model_6);
#Adjusted R-squared:  0.583



#SP_Lag_2 , vif -  10.38 , p value -   0.1794 
Cam_Koyck_Model_7 <- lm(formula = gmv ~ sla + IsHolidayWeek + No_of_Holidays + deliverybdays + 
                          Online.marketing_adstock +  
                          Other_adstock + IsHolidayWeek1 + IsHolidayWeek2 + 
                          Discount_Lag_2 + Discount_Lag_3 +
                          SP_Lag_3 + MA_Increase_SP + MA_Increase_Discount + `gmv-1`, 
                        data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_7);
vif(Cam_Koyck_Model_7);
#Adjusted R-squared:  0.573


#MA_Increase_SP , vif -  8.71  , p value -  0.2925 
Cam_Koyck_Model_8 <- lm(formula = gmv ~ sla + IsHolidayWeek + No_of_Holidays + deliverybdays + 
                          Online.marketing_adstock +  
                          Other_adstock + IsHolidayWeek1 + IsHolidayWeek2 + 
                          Discount_Lag_2 + Discount_Lag_3 +
                          SP_Lag_3 +  MA_Increase_Discount + `gmv-1`, 
                        data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_8);
vif(Cam_Koyck_Model_8);
#Adjusted R-squared:  0.572


#No_of_Holidays , vif -  7.68  , p value -  0.1341 
Cam_Koyck_Model_9 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverybdays + 
                          Online.marketing_adstock +  
                          Other_adstock + IsHolidayWeek1 + IsHolidayWeek2 + 
                          Discount_Lag_2 + Discount_Lag_3 +
                          SP_Lag_3 +  MA_Increase_Discount + `gmv-1`, 
                        data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_9);
vif(Cam_Koyck_Model_9);
#Adjusted R-squared:  0.556



#Discount_Lag_2 , vif -   11.94   , p value -  0.0361 *  
Cam_Koyck_Model_10 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverybdays + 
                          Online.marketing_adstock +  
                          Other_adstock + IsHolidayWeek1 + IsHolidayWeek2 + 
                         Discount_Lag_3 +
                          SP_Lag_3 +  MA_Increase_Discount + `gmv-1`, 
                        data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_10);
vif(Cam_Koyck_Model_10);
#Adjusted R-squared:  0.514

#All vif are bw 1 and 3. Considering only p value to remove variables.

#Other_adstock  , p value -  0.8660
Cam_Koyck_Model_11 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverybdays + 
                           Online.marketing_adstock +  
                           IsHolidayWeek1 + IsHolidayWeek2 + 
                           Discount_Lag_3 +
                           SP_Lag_3 +  MA_Increase_Discount + `gmv-1`, 
                         data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_11);
vif(Cam_Koyck_Model_11);

#Adjusted R-squared:  0.526 


#SP_Lag_3  , p value -  0.7416
Cam_Koyck_Model_12 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverybdays + 
                           Online.marketing_adstock +  
                           IsHolidayWeek1 + IsHolidayWeek2 + 
                           Discount_Lag_3 +
                            MA_Increase_Discount + `gmv-1`, 
                         data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_12);
vif(Cam_Koyck_Model_12);

#Adjusted R-squared:  0.536 



#IsHolidayWeek2  , p value -   0.4956  
Cam_Koyck_Model_13 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverybdays + 
                           Online.marketing_adstock +  
                           IsHolidayWeek1 + 
                           Discount_Lag_3 +
                           MA_Increase_Discount + `gmv-1`, 
                         data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_13);
vif(Cam_Koyck_Model_13);

#Adjusted R-squared:  0.542

#IsHolidayWeek1  , p value -   0.1878  
Cam_Koyck_Model_14 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverybdays + 
                           Online.marketing_adstock +  
                           Discount_Lag_3 +
                           MA_Increase_Discount + `gmv-1`, 
                         data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_14);
vif(Cam_Koyck_Model_14);

#Adjusted R-squared:  0.534



#`gmv-1`  , p value -  0.0340 *  
Cam_Koyck_Model_15 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverybdays + 
                           Online.marketing_adstock +  
                           Discount_Lag_3 +
                           MA_Increase_Discount , 
                         data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_15);
vif(Cam_Koyck_Model_15);

#Adjusted R-squared:  0.529



#MA_Increase_Discount , p value -  0.0713 .
Cam_Koyck_Model_16 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverybdays + 
                           Online.marketing_adstock +  
                           Discount_Lag_3 ,
                         data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_16);
vif(Cam_Koyck_Model_16);

#Adjusted R-squared:  0.504


#Discount_Lag_3 , p value -   0.269  
Cam_Koyck_Model_17 <- lm(formula = gmv ~ sla + IsHolidayWeek + deliverybdays + 
                           Online.marketing_adstock  ,
                         data = CameraAccessoryData_Koyck);

summary(Cam_Koyck_Model_17);
vif(Cam_Koyck_Model_17);

#Adjusted R-squared:  0.501



Final_Camera_koyck_model <- Cam_Koyck_Model_17;
#Final Adjusted R square = 0.501


#-------------------------------------------------------------------------------------------------------------------------------------------------------


# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = CameraAccessoryData_Koyck, form.lm = formula(  gmv ~ sla + IsHolidayWeek + deliverybdays + 
                                                                                 Online.marketing_adstock ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval_1, "ms")
# 0.567


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Camera_koyck_model$coefficients[var]*
                       mean(CameraAccessoryData_Koyck[,var])/mean(CameraAccessoryData_Koyck$gmv))
  
  return(elax1)
} 


Camera_koyck_Var_Elasticity <- data.frame(names(Final_Camera_koyck_model$coefficients)[-1]);

for (i in 2:length(Final_Camera_koyck_model$coefficients)) {
  Camera_koyck_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Camera_koyck_model$coefficients)[i]);
}
Camera_koyck_Var_Elasticity$Direction <- ifelse(Camera_koyck_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Camera_koyck_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Camera_koyck_Var_Elasticity


ggplot(data=Camera_koyck_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Camera Accessory - Koyck") +xlab("Variables")



#########################################################################################################################################################
#-------------------------------------------- 4. Distributed Lag Model ------------------------------------------------------------------------------------
#########################################################################################################################################################


# a) Load Camera Accessory Data

CameraAccessoryData_Dis <- read.csv('CameraAccessoryData.csv',stringsAsFactors = FALSE);

CameraAccessoryData_Dis <- slide(CameraAccessoryData_Dis, Var = "gmv",slideBy = -1)
CameraAccessoryData_Dis <- slide(CameraAccessoryData_Dis, Var = "gmv",slideBy = -2)
CameraAccessoryData_Dis <- slide(CameraAccessoryData_Dis, Var = "gmv",slideBy = -3)

CameraAccessoryData_Dis <- slide(CameraAccessoryData_Dis, Var = "SellingPrice",slideBy = -1)
CameraAccessoryData_Dis <- slide(CameraAccessoryData_Dis, Var = "SellingPrice",slideBy = -2)
CameraAccessoryData_Dis <- slide(CameraAccessoryData_Dis, Var = "SellingPrice",slideBy = -3)

CameraAccessoryData_Dis <- slide(CameraAccessoryData_Dis, Var = "Discount",slideBy = -1)
CameraAccessoryData_Dis <- slide(CameraAccessoryData_Dis, Var = "Discount",slideBy = -2)
CameraAccessoryData_Dis <- slide(CameraAccessoryData_Dis, Var = "Discount",slideBy = -3)

str(CameraAccessoryData_Dis);
colnames(CameraAccessoryData_Dis);
CameraAccessoryData_Dis <- na.omit(CameraAccessoryData_Dis)


# b) Removing Unnecessary variables 

CameraAccessoryData_Dis$product_analytic_sub_category <- NULL;
CameraAccessoryData_Dis$X <- NULL;

CameraAccessoryData_Dis$Week <- NULL;
CameraAccessoryData_Dis$no_of_orders <- NULL;
CameraAccessoryData_Dis$units <- NULL;

CameraAccessoryData_Dis$COD <- NULL;
CameraAccessoryData_Dis$Prepaid <- NULL;
CameraAccessoryData_Dis$Mass_p <- NULL;
CameraAccessoryData_Dis$Middle_p <- NULL;
CameraAccessoryData_Dis$Premium_p <- NULL;

CameraAccessoryData_Dis$product_mrp <- NULL;
CameraAccessoryData_Dis$MA_Discount <- NULL;
CameraAccessoryData_Dis$MA_SellingPrice <- NULL;



# c) Scaling all the variables for modelling

CameraAccessoryData_Dis$gmv <- scale(CameraAccessoryData_Dis$gmv);
CameraAccessoryData_Dis$Discount <- scale(CameraAccessoryData_Dis$Discount);
CameraAccessoryData_Dis$`Discount-1` <- scale(CameraAccessoryData_Dis$`Discount-1`);
CameraAccessoryData_Dis$`Discount-2` <- scale(CameraAccessoryData_Dis$`Discount-2`);
CameraAccessoryData_Dis$`Discount-3` <- scale(CameraAccessoryData_Dis$`Discount-3`);


CameraAccessoryData_Dis$sla <- scale(CameraAccessoryData_Dis$sla);
CameraAccessoryData_Dis$procurement_sla <- scale(CameraAccessoryData_Dis$procurement_sla);

CameraAccessoryData_Dis$No_of_Holidays <- scale(CameraAccessoryData_Dis$No_of_Holidays)
CameraAccessoryData_Dis$deliverybdays <- scale(CameraAccessoryData_Dis$deliverybdays);

CameraAccessoryData_Dis$deliverycdays <- scale(CameraAccessoryData_Dis$deliverycdays);

CameraAccessoryData_Dis$TV_adstock <- scale(CameraAccessoryData_Dis$TV_adstock);
CameraAccessoryData_Dis$Digital_adstock <- scale(CameraAccessoryData_Dis$Digital_adstock);

CameraAccessoryData_Dis$Sponsorship_adstock <- scale(CameraAccessoryData_Dis$Sponsorship_adstock);
CameraAccessoryData_Dis$`Content.Marketing_adstock` <- scale(CameraAccessoryData_Dis$`Content.Marketing_adstock`);
CameraAccessoryData_Dis$`Online.marketing_adstock` <- scale(CameraAccessoryData_Dis$`Online.marketing_adstock`);

CameraAccessoryData_Dis$Affiliates_adstock <- scale(CameraAccessoryData_Dis$Affiliates_adstock);
CameraAccessoryData_Dis$SEM_adstock <- scale(CameraAccessoryData_Dis$SEM_adstock);
CameraAccessoryData_Dis$Radio_adstock <- scale(CameraAccessoryData_Dis$Radio_adstock);

CameraAccessoryData_Dis$Other_adstock <-scale(CameraAccessoryData_Dis$Other_adstock);
CameraAccessoryData_Dis$NPS_Score <-scale(CameraAccessoryData_Dis$NPS_Score); 
CameraAccessoryData_Dis$Discount_Lag_1 <- scale(CameraAccessoryData_Dis$Discount_Lag_1);
CameraAccessoryData_Dis$Discount_Lag_2 <- scale(CameraAccessoryData_Dis$Discount_Lag_2);
CameraAccessoryData_Dis$Discount_Lag_3 <- scale(CameraAccessoryData_Dis$Discount_Lag_3);

CameraAccessoryData_Dis$SP_Lag_1 <- scale(CameraAccessoryData_Dis$SP_Lag_1);
CameraAccessoryData_Dis$SP_Lag_2 <- scale(CameraAccessoryData_Dis$SP_Lag_2);
CameraAccessoryData_Dis$SP_Lag_3 <- scale(CameraAccessoryData_Dis$SP_Lag_3);

CameraAccessoryData_Dis$MA_Increase_SP <- scale(CameraAccessoryData_Dis$MA_Increase_SP);
CameraAccessoryData_Dis$MA_Increase_Discount <- scale(CameraAccessoryData_Dis$MA_Increase_Discount);
CameraAccessoryData_Dis$`gmv-1` <- scale(CameraAccessoryData_Dis$`gmv-1`);
CameraAccessoryData_Dis$`gmv-2` <- scale(CameraAccessoryData_Dis$`gmv-2`);
CameraAccessoryData_Dis$`gmv-3` <- scale(CameraAccessoryData_Dis$`gmv-3`);

CameraAccessoryData_Dis$SellingPrice <- scale(CameraAccessoryData_Dis$SellingPrice);
CameraAccessoryData_Dis$`SellingPrice-1` <- scale(CameraAccessoryData_Dis$`SellingPrice-1`);
CameraAccessoryData_Dis$`SellingPrice-2` <- scale(CameraAccessoryData_Dis$`SellingPrice-2`);
CameraAccessoryData_Dis$`SellingPrice-3` <- scale(CameraAccessoryData_Dis$`SellingPrice-3`);


# d) Modelling

Cam_Dis_Model_1 <- lm(gmv~.,CameraAccessoryData_Dis);

Cam_Dis_Model_2 <- stepAIC(Cam_Dis_Model_1,direction = "both")
summary(Cam_Dis_Model_2);
vif(Cam_Dis_Model_2);

#Adjusted R-squared:  0.778 

#Online.marketing_adstock and Affiliates_adstock are highly correlated.
#Affiliates_adstock - vif - 1349.30 , p value - 0.0015 **
#Removing it
Cam_Dis_Model_3 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                        IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                        Digital_adstock + Sponsorship_adstock + Online.marketing_adstock + 
                        SEM_adstock + Other_adstock + NPS_Score + 
                        IsHolidayWeek1 + Discount_Lag_2 + Discount_Lag_3 + SP_Lag_1 + 
                        SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                        `SellingPrice-1` + `SellingPrice-2` + `SellingPrice-3` + 
                        `Discount-1` + `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_3);
vif(Cam_Dis_Model_3);
#Adjusted R-squared:  0.636


#Discount_Lag_3 - vif - 526.83  , p value -  0.535 
#Removing it
Cam_Dis_Model_4 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                        IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                        Digital_adstock + Sponsorship_adstock + Online.marketing_adstock + 
                        SEM_adstock + Other_adstock + NPS_Score + 
                        IsHolidayWeek1 + Discount_Lag_2 +  SP_Lag_1 + 
                        SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                        `SellingPrice-1` + `SellingPrice-2` + `SellingPrice-3` + 
                        `Discount-1` + `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_4);
vif(Cam_Dis_Model_4);
#Adjusted R-squared:  0.646



#Discount - vif - 144  , p value -   0.3500  
#Removing it
Cam_Dis_Model_5 <- lm(formula = gmv ~ SellingPrice + sla + procurement_sla + 
                        IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                        Sponsorship_adstock + Online.marketing_adstock + 
                        SEM_adstock + Other_adstock + NPS_Score + 
                        IsHolidayWeek1 + Discount_Lag_2 +  SP_Lag_1 + 
                        SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                        `SellingPrice-1` + `SellingPrice-2` + `SellingPrice-3` + 
                        `Discount-1` + `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_5);
vif(Cam_Dis_Model_5);
#Adjusted R-squared:  0.662


#`Discount-1` - vif -99.28, p value -    0.0894 . 
#Removing it
Cam_Dis_Model_6 <- lm(formula = gmv ~ SellingPrice + sla + procurement_sla + 
                        IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                        Sponsorship_adstock + Online.marketing_adstock + 
                        SEM_adstock + Other_adstock + NPS_Score + 
                        IsHolidayWeek1 + Discount_Lag_2 +  SP_Lag_1 + 
                        SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                        `SellingPrice-1` + `SellingPrice-2` + `SellingPrice-3` + 
                        `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_6);
vif(Cam_Dis_Model_6);
#Adjusted R-squared:  0.632


#Discount_Lag_2 - vif -  48.46 , p value -   0.256  
#Removing it
Cam_Dis_Model_7 <- lm(formula = gmv ~ SellingPrice + sla + procurement_sla + 
                        IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                        Sponsorship_adstock + Online.marketing_adstock + 
                        SEM_adstock + Other_adstock + NPS_Score + 
                        IsHolidayWeek1 +  SP_Lag_1 + 
                        SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                        `SellingPrice-1` + `SellingPrice-2` + `SellingPrice-3` + 
                        `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_7);
vif(Cam_Dis_Model_7);
#Adjusted R-squared:  0.627



#SP_Lag_1 - vif -  14.03  , p value -  0.3548  
#Removing it
Cam_Dis_Model_8 <- lm(formula = gmv ~ SellingPrice + sla + procurement_sla + 
                        IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                        Sponsorship_adstock + Online.marketing_adstock + 
                        SEM_adstock + Other_adstock + NPS_Score + 
                        IsHolidayWeek1 +  
                        SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                        `SellingPrice-1` + `SellingPrice-2` + `SellingPrice-3` + 
                        `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_8);
vif(Cam_Dis_Model_8);
#Adjusted R-squared:  0.628


#SellingPrice - vif -  17.55  , p value -  0.0233 *
#Removing it
Cam_Dis_Model_9 <- lm(formula = gmv ~ sla + procurement_sla + 
                        IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                        Sponsorship_adstock + Online.marketing_adstock + 
                        SEM_adstock + Other_adstock + NPS_Score + 
                        IsHolidayWeek1 +  
                        SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                        `SellingPrice-1` + `SellingPrice-2` + `SellingPrice-3` + 
                        `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_9);
vif(Cam_Dis_Model_9);
#Adjusted R-squared:  0.562


#NPS_Score - vif - 24.27 , p value -  0.4331 
#Removing it
Cam_Dis_Model_10 <- lm(formula = gmv ~ sla + procurement_sla + 
                        IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                        Sponsorship_adstock + Online.marketing_adstock + 
                        SEM_adstock + Other_adstock +
                        IsHolidayWeek1 +  
                        SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                        `SellingPrice-1` + `SellingPrice-2` + `SellingPrice-3` + 
                        `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_10);
vif(Cam_Dis_Model_10);
#Adjusted R-squared:  0.562


#`SellingPrice-2`  - vif -  10.74   , p value -  0.73121  
#Removing it
Cam_Dis_Model_11 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                         Sponsorship_adstock + Online.marketing_adstock + 
                         SEM_adstock + Other_adstock +
                         IsHolidayWeek1 +  
                         SP_Lag_2 + SP_Lag_3 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                         `SellingPrice-1` + `SellingPrice-3` + 
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_11);
vif(Cam_Dis_Model_11);
#Adjusted R-squared:  0.581


#SP_Lag_3  - vif - 13.32    , p value -   0.17967  
#Removing it
Cam_Dis_Model_12 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                         Sponsorship_adstock + Online.marketing_adstock + 
                         SEM_adstock + Other_adstock +
                         IsHolidayWeek1 +  
                         SP_Lag_2 + MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                         `SellingPrice-1` + `SellingPrice-3` + 
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_12);
vif(Cam_Dis_Model_12);
#Adjusted R-squared:  0.569

#SP_Lag_2  - vif -  5.08    , p value -   0.56195  
#Removing it
Cam_Dis_Model_13 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + No_of_Holidays + deliverybdays + TV_adstock + 
                         Sponsorship_adstock + Online.marketing_adstock + 
                         SEM_adstock + Other_adstock +
                         IsHolidayWeek1 +  MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                         `SellingPrice-1` + `SellingPrice-3` + 
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_13);
vif(Cam_Dis_Model_13);
#Adjusted R-squared:  0.578


#No_of_Holidays  - vif -  7.84    , p value -   0.08632 . 
#Removing it
Cam_Dis_Model_14 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + TV_adstock + 
                         Sponsorship_adstock + Online.marketing_adstock + 
                         SEM_adstock + Other_adstock +
                         IsHolidayWeek1 +  MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                         `SellingPrice-1` + `SellingPrice-3` + 
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_14);
vif(Cam_Dis_Model_14);
#Adjusted R-squared:  0.55


#Sponsorship_adstock  - vif -   5.19     , p value -  5.19 
#Removing it
Cam_Dis_Model_15 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + TV_adstock + 
                        Online.marketing_adstock + 
                         SEM_adstock + Other_adstock +
                         IsHolidayWeek1 +  MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                         `SellingPrice-1` + `SellingPrice-3` + 
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_15);
vif(Cam_Dis_Model_15);
#Adjusted R-squared:  0.558

# All vif are bw 1 and 3. Considering only p values to remove variables.


#`SellingPrice-3`   , p value -  0.87921 
#Removing it
Cam_Dis_Model_16 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + TV_adstock + 
                         Online.marketing_adstock + 
                         SEM_adstock + Other_adstock +
                         IsHolidayWeek1 +  MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                         `SellingPrice-1` + 
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_16);
vif(Cam_Dis_Model_16);
#Adjusted R-squared:  0.571



#TV_adstock   , p value -  0.60747  
#Removing it
Cam_Dis_Model_17 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + 
                         Online.marketing_adstock + 
                         SEM_adstock + Other_adstock +
                         IsHolidayWeek1 +  MA_Increase_Discount + `gmv-1` + `gmv-2` + 
                         `SellingPrice-1` + 
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_17);
vif(Cam_Dis_Model_17);
#Adjusted R-squared:  0.58



# `gmv-2`   , p value -  0.45575   
#Removing it
Cam_Dis_Model_18 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + 
                         Online.marketing_adstock + 
                         SEM_adstock + Other_adstock +
                         IsHolidayWeek1 +  MA_Increase_Discount + `gmv-1` +
                         `SellingPrice-1` + 
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_18);
vif(Cam_Dis_Model_18);
#Adjusted R-squared:  0.585


#Other_adstock  , p value -  0.3036   
#Removing it
Cam_Dis_Model_19 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + 
                         Online.marketing_adstock + 
                         SEM_adstock + 
                         IsHolidayWeek1 +  MA_Increase_Discount + `gmv-1` +
                         `SellingPrice-1` + 
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_19);
vif(Cam_Dis_Model_19);
#Adjusted R-squared:  0.584


#SEM_adstock  , p value - 0.3642   
#Removing it
Cam_Dis_Model_20 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + 
                         Online.marketing_adstock + 
                         IsHolidayWeek1 +  MA_Increase_Discount + `gmv-1` +
                         `SellingPrice-1` + 
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_20);
vif(Cam_Dis_Model_20);
#Adjusted R-squared:  0.586



#`SellingPrice-1`   , p value -  0.4872 
#Removing it
Cam_Dis_Model_21 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + 
                         Online.marketing_adstock + 
                         IsHolidayWeek1 +  MA_Increase_Discount + `gmv-1` +
                         `Discount-3`, data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_21);
vif(Cam_Dis_Model_21);
#Adjusted R-squared:  0.591



#`Discount-3`   , p value - 0.23671 
#Removing it
Cam_Dis_Model_22 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + 
                         Online.marketing_adstock + 
                         IsHolidayWeek1 +  MA_Increase_Discount + `gmv-1` , data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_22);
vif(Cam_Dis_Model_22);
#Adjusted R-squared:  0.587



#IsHolidayWeek1  , p value -  0.07911 .
#Removing it
Cam_Dis_Model_23 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + 
                         Online.marketing_adstock + 
                        MA_Increase_Discount + `gmv-1` , data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_23);
vif(Cam_Dis_Model_23);
#Adjusted R-squared:  0.564


#`gmv-1`   , p value -  0.04189 * 
#Removing it
Cam_Dis_Model_24 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + 
                         Online.marketing_adstock + 
                         MA_Increase_Discount  , data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_24);
vif(Cam_Dis_Model_24);
#Adjusted R-squared:  0.529



#MA_Increase_Discount    , p value -  0.08561 . 
#Removing it
Cam_Dis_Model_25 <- lm(formula = gmv ~ sla + procurement_sla + 
                         IsHolidayWeek + deliverybdays + 
                         Online.marketing_adstock , data = CameraAccessoryData_Dis)
summary(Cam_Dis_Model_25);
vif(Cam_Dis_Model_25);
#Adjusted R-squared:  0.506



Final_Camera_Dis_Model <- Cam_Dis_Model_25;
#Final Adjusted R-squared:  0.506

#-------------------------------------------------------------------------------------------------------------------------------------------------------


# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = CameraAccessoryData_Dis, form.lm = formula( gmv ~ sla + procurement_sla + 
                                                                              IsHolidayWeek + deliverybdays + 
                                                                              Online.marketing_adstock  ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval_1, "ms")
# 0.588


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Camera_Dis_Model$coefficients[var]*
                       mean(CameraAccessoryData_Dis[,var])/mean(CameraAccessoryData_Dis$gmv))
  
  return(elax1)
} 


Camera_Dis_Var_Elasticity <- data.frame(names(Final_Camera_Dis_Model$coefficients)[-1]);

for (i in 2:length(Final_Camera_Dis_Model$coefficients)) {
  Camera_Dis_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Camera_Dis_Model$coefficients)[i]);
}
Camera_Dis_Var_Elasticity$Direction <- ifelse(Camera_Dis_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Camera_Dis_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Camera_Dis_Var_Elasticity


ggplot(data=Camera_Dis_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Camera Accessory - Distributed Lag Model") +xlab("Variables");



####################################################################################################################################################
#------------------------------------------ 5. Multiplicative and  distributed model ------------------------------------------------------------------
####################################################################################################################################################


# a) Load Camera Accessory Data
CameraAccessoryData_MulDis <- read.csv('CameraAccessoryData.csv',stringsAsFactors = FALSE);


# b) Removing Unnecessary variables 
CameraAccessoryData_MulDis$product_analytic_sub_category <- NULL;
CameraAccessoryData_MulDis$X <- NULL;

CameraAccessoryData_MulDis$Week <- NULL;
CameraAccessoryData_MulDis$no_of_orders <- NULL;
CameraAccessoryData_MulDis$units <- NULL;

CameraAccessoryData_MulDis$COD <- NULL;
CameraAccessoryData_MulDis$Prepaid <- NULL;
CameraAccessoryData_MulDis$Mass_p <- NULL;
CameraAccessoryData_MulDis$Middle_p <- NULL;
CameraAccessoryData_MulDis$Premium_p <- NULL;

CameraAccessoryData_MulDis$product_mrp <- NULL;
CameraAccessoryData_MulDis$MA_Discount <- NULL;
CameraAccessoryData_MulDis$MA_SellingPrice <- NULL;


CameraAccessoryData_MulDis <- slide(CameraAccessoryData_MulDis, Var = "gmv",slideBy = -1)
CameraAccessoryData_MulDis <- slide(CameraAccessoryData_MulDis, Var = "gmv",slideBy = -2)
CameraAccessoryData_MulDis <- slide(CameraAccessoryData_MulDis, Var = "gmv",slideBy = -3)


CameraAccessoryData_MulDis <- slide(CameraAccessoryData_MulDis, Var = "SellingPrice",slideBy = -1)
CameraAccessoryData_MulDis <- slide(CameraAccessoryData_MulDis, Var = "SellingPrice",slideBy = -2)
CameraAccessoryData_MulDis <- slide(CameraAccessoryData_MulDis, Var = "SellingPrice",slideBy = -3)


CameraAccessoryData_MulDis <- slide(CameraAccessoryData_MulDis, Var = "Discount",slideBy = -1)
CameraAccessoryData_MulDis <- slide(CameraAccessoryData_MulDis, Var = "Discount",slideBy = -2)
CameraAccessoryData_MulDis <- slide(CameraAccessoryData_MulDis, Var = "Discount",slideBy = -3)




# c) Removing 0 values in data changing from 0 to 0.01. As log 0 will be undefined.

CameraAccessoryData_MulDis <- na.omit(CameraAccessoryData_MulDis);


# Looking for 0 values in data
sapply(CameraAccessoryData_MulDis, function(x){sum(x==0)})

CameraAccessoryData_MulDis[CameraAccessoryData_MulDis == 0] <- 0.001;


sapply(CameraAccessoryData_MulDis, function(x){sum(x<0)})
#There are negative values.

# For negative values we apply the following transaformation -> -log(-x + 1) , Yeo-Johnson Power Transformations

for(i in 1:ncol(CameraAccessoryData_MulDis)) {
  CameraAccessoryData_MulDis[,c(i)]<- ifelse(
    CameraAccessoryData_MulDis[,c(i)]>0,
    log(CameraAccessoryData_MulDis[,c(i)]), 
    -log(-(CameraAccessoryData_MulDis[,c(i)])+1)
  );
}


# d) Modelling

model_muldis_1 <- lm(gmv ~ ., CameraAccessoryData_MulDis);

model_muldis_2 <-stepAIC(model_muldis_1,direction = "both")
summary(model_muldis_2);
vif(model_muldis_2);
#Adjusted R-squared:  0.943 


#SP_Lag_3 , vif - 191.92, p value -  0.16082  
#Removing it.
model_muldis_3 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       IsHolidayWeek + No_of_Holidays + deliverycdays + TV_adstock + 
                       Sponsorship_adstock + Content.Marketing_adstock + Online.marketing_adstock + 
                       Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                       SP_Lag_2 + MA_Increase_SP + `gmv-1` + `gmv-2` + 
                       `gmv-3` + `SellingPrice-1` + `Discount-2` + `Discount-3`, 
                     data = CameraAccessoryData_MulDis);
summary(model_muldis_3);
vif(model_muldis_3);
#Adjusted R-squared:  0.94


#No_of_Holidays , vif -   474.92, p value -  0.23273 
#Removing it.
model_muldis_4 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       IsHolidayWeek + deliverycdays + TV_adstock + 
                       Sponsorship_adstock + Online.marketing_adstock + 
                       Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                       SP_Lag_2 + MA_Increase_SP + `gmv-1` + `gmv-2` + 
                       `gmv-3` + `SellingPrice-1` + `Discount-2` + `Discount-3`, 
                     data = CameraAccessoryData_MulDis);
summary(model_muldis_4);
vif(model_muldis_4);
#Adjusted R-squared:  0.939


#TV_adstock , vif -    20.22, p value -  0.3684 
#Removing it.
model_muldis_5 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       IsHolidayWeek + deliverycdays +
                       Sponsorship_adstock + Online.marketing_adstock + 
                       Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                       SP_Lag_2 + MA_Increase_SP + `gmv-1` + `gmv-2` + 
                       `gmv-3` + `SellingPrice-1` + `Discount-2` + `Discount-3`, 
                     data = CameraAccessoryData_MulDis);
summary(model_muldis_5);
vif(model_muldis_5);
#Adjusted R-squared:  0.909

#`gmv-2`  , vif -   16.09 , p value -  0.1719  
#Removing it.
model_muldis_6 <- lm(formula = gmv ~ SellingPrice + Discount + sla + procurement_sla + 
                       IsHolidayWeek + deliverycdays +
                       Sponsorship_adstock + Online.marketing_adstock + 
                       Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                       SP_Lag_2 + MA_Increase_SP + `gmv-1` + 
                       `gmv-3` + `SellingPrice-1` + `Discount-2` + `Discount-3`, 
                     data = CameraAccessoryData_MulDis);
summary(model_muldis_6);
vif(model_muldis_6);
#Adjusted R-squared:  0.906

# Discount vif -  13.56  , p value -  0.14253
#Removing it.
model_muldis_7 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                       IsHolidayWeek + deliverycdays +
                       Sponsorship_adstock + Online.marketing_adstock + 
                       Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                       SP_Lag_2 + MA_Increase_SP + `gmv-1` + 
                       `gmv-3` + `SellingPrice-1` + `Discount-2` + `Discount-3`, 
                     data = CameraAccessoryData_MulDis);
summary(model_muldis_7);
vif(model_muldis_7);
#Adjusted R-squared:  0.902

# deliverycdays vif -   13.26 , p value - 0.02809 * 
#Removing it.
model_muldis_8 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                       IsHolidayWeek + 
                       Sponsorship_adstock + Online.marketing_adstock + 
                       Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                       SP_Lag_2 + MA_Increase_SP + `gmv-1` + 
                       `gmv-3` + `SellingPrice-1` + `Discount-2` + `Discount-3`, 
                     data = CameraAccessoryData_MulDis);
summary(model_muldis_8);
vif(model_muldis_8);
#Adjusted R-squared:  0.888



# Sponsorship_adstock vif -    6.25 , p value - 0.26586  
#Removing it.
model_muldis_9 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                       IsHolidayWeek + 
                       Online.marketing_adstock + 
                       Radio_adstock + NPS_Score + IsHolidayWeek1 + Discount_Lag_2 + 
                       SP_Lag_2 + MA_Increase_SP + `gmv-1` + 
                       `gmv-3` + `SellingPrice-1` + `Discount-2` + `Discount-3`, 
                     data = CameraAccessoryData_MulDis);
summary(model_muldis_9);
vif(model_muldis_9);
#Adjusted R-squared:  0.887




# NPS_Score vif -     7.67  , p value -  0.09825 .  
#Removing it.
model_muldis_10 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                       IsHolidayWeek + 
                       Online.marketing_adstock + 
                       Radio_adstock + IsHolidayWeek1 + Discount_Lag_2 + 
                       SP_Lag_2 + MA_Increase_SP + `gmv-1` + 
                       `gmv-3` + `SellingPrice-1` + `Discount-2` + `Discount-3`, 
                     data = CameraAccessoryData_MulDis);
summary(model_muldis_10);
vif(model_muldis_10);
#Adjusted R-squared:  0.881



# IsHolidayWeek  , p value - 0.87663 
#Removing it.
model_muldis_11 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Online.marketing_adstock + 
                        Radio_adstock + IsHolidayWeek1 + Discount_Lag_2 + 
                        SP_Lag_2 + MA_Increase_SP + `gmv-1` + 
                        `gmv-3` + `SellingPrice-1` + `Discount-2` + `Discount-3`, 
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_11);
vif(model_muldis_11);
#Adjusted R-squared:  0.884


# `Discount-3`   , p value -  0.69373    
#Removing it.
model_muldis_12 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Online.marketing_adstock + 
                        Radio_adstock + IsHolidayWeek1 + Discount_Lag_2 + 
                        SP_Lag_2 + MA_Increase_SP + `gmv-1` + 
                        `gmv-3` + `SellingPrice-1` + `Discount-2` ,
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_12);
vif(model_muldis_12);
#Adjusted R-squared:  0.887


# `gmv-3`    , p value -   0.66978 
model_muldis_13 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Online.marketing_adstock + 
                        Radio_adstock + IsHolidayWeek1 + Discount_Lag_2 + 
                        SP_Lag_2 + MA_Increase_SP + `gmv-1` + 
                        `SellingPrice-1` + `Discount-2` ,
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_13);
vif(model_muldis_13);
#Adjusted R-squared:  0.889


# SP_Lag_2  , p value - 0.44330   
model_muldis_14 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Online.marketing_adstock + 
                        Radio_adstock + IsHolidayWeek1 + Discount_Lag_2 + 
                       MA_Increase_SP + `gmv-1` + 
                        `SellingPrice-1` + `Discount-2` ,
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_14);
vif(model_muldis_14);
#Adjusted R-squared:  0.891



# Radio_adstock  , p value - 0.49980   
model_muldis_15 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Online.marketing_adstock + 
                         IsHolidayWeek1 + Discount_Lag_2 + 
                        MA_Increase_SP + `gmv-1` + 
                        `SellingPrice-1` + `Discount-2` ,
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_15);
vif(model_muldis_15);
#Adjusted R-squared:  0.892


# Discount_Lag_2  , p value -0.2934   
model_muldis_16 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Online.marketing_adstock + 
                        IsHolidayWeek1 + 
                        MA_Increase_SP + `gmv-1` + 
                        `SellingPrice-1` + `Discount-2` ,
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_16);
vif(model_muldis_16);
#Adjusted R-squared:  0.892


# MA_Increase_SP  , p value -0.07277 . 
model_muldis_17 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Online.marketing_adstock + 
                        IsHolidayWeek1 + 
                        `gmv-1` + 
                        `SellingPrice-1` + `Discount-2` ,
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_17);
vif(model_muldis_17);
#Adjusted R-squared:  0.885


# `Discount-2`  , p value - 0.07571 .
model_muldis_18 <- lm(formula = gmv ~ SellingPrice +  sla + procurement_sla + 
                        Online.marketing_adstock + 
                        IsHolidayWeek1 + 
                        `gmv-1` + 
                        `SellingPrice-1`,
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_18);
vif(model_muldis_18);
#Adjusted R-squared:  0.879


# procurement_sla  , p value -0.04275 * 
model_muldis_19 <- lm(formula = gmv ~ SellingPrice +  sla + 
                        Online.marketing_adstock + 
                        IsHolidayWeek1 + 
                        `gmv-1` + 
                        `SellingPrice-1`,
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_19);
vif(model_muldis_19);
#Adjusted R-squared:  0.869

#gmv-1 has vif of 4. So removing it.
model_muldis_20 <- lm(formula = gmv ~ SellingPrice +  sla + 
                        Online.marketing_adstock + 
                        IsHolidayWeek1 + 
                        `SellingPrice-1`,
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_20);
vif(model_muldis_20);
#Adjusted R-squared:  0.829


#`SellingPrice-1`-p value - 0.1691  
model_muldis_21 <- lm(formula = gmv ~ SellingPrice +  sla + 
                        Online.marketing_adstock + 
                        IsHolidayWeek1 ,
                      data = CameraAccessoryData_MulDis);
summary(model_muldis_21);
vif(model_muldis_21);
#Adjusted R-squared:  0.825



Final_Camera_MulDis_Model <- model_muldis_21;
#Final Adjusted R-squared:  0.825

#-------------------------------------------------------------------------------------------------------------------------------------------------------


# 10 fold cross validation
temp_crossval_1 <- cv.lm(data = CameraAccessoryData_MulDis, form.lm = formula(   gmv ~ SellingPrice +  sla + 
                                                                                   Online.marketing_adstock + 
                                                                                   IsHolidayWeek1    ),m = 10, plotit=FALSE)
# Mean Square Error
attr(temp_crossval_1, "ms")
# 0.839


# Elasticity

elasticity_calc <- function(var){
  
  elax1 <-as.numeric(Final_Camera_MulDis_Model$coefficients[var]*
                       mean(CameraAccessoryData_MulDis[,var])/mean(CameraAccessoryData_MulDis$gmv))
  
  return(elax1)
} 


Camera_MulDis_Var_Elasticity <- data.frame(names(Final_Camera_MulDis_Model$coefficients)[-1]);

for (i in 2:length(Final_Camera_MulDis_Model$coefficients)) {
  Camera_MulDis_Var_Elasticity[i-1,'Elasticity']<-
    elasticity_calc(names(Final_Camera_MulDis_Model$coefficients)[i]);
}
Camera_MulDis_Var_Elasticity$Direction <- ifelse(Camera_MulDis_Var_Elasticity$Elasticity > 0, "Positive", "Negative");

names(Camera_MulDis_Var_Elasticity) <- c('var_names', 'Elasticity', 'Direction');
Camera_MulDis_Var_Elasticity


ggplot(data=Camera_MulDis_Var_Elasticity, aes(x=reorder(var_names,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity" , fill="green" , color="red") + 
  coord_flip() +
  ggtitle("Camera Accessory - Distributed Lag + Multiplicative Model") +xlab("Variables")


#------------------------------------------------------------------------------------------------------------------------------------------------------------
