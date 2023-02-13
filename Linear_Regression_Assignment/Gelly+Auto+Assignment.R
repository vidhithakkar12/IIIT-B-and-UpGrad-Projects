##############################################################################################################
############################## Assignment-Linear Regression (Geely Auto) #####################################
##############################################################################################################

# Submission by : Vidhi Thakkar

# ----------------------------------- 1. Business Understanding ----------------------------------------------

# A Chinese automobile company Geely Auto aspires to enter the US market by producing cars locally.
# They want to understand the factors which affect the pricing of a car in the American market.

# Essentially, the company wants to know:
# Which variables are significant in predicting the price of a car
# How well those variables describe the price of a car


# a) Installing necessary packages

# install.packages("ggplot2")   
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("MASS")     
# install.packages("car")      
# install.packages("corrplot")  
# install.packages("outliers")

# b) Loading the Packages

library(ggplot2)              # for ggplot
library(dplyr)
library(tidyr)
library(stringr)
library(MASS)                 # for StepAIC
library(car)                  # for VIF
library(corrplot)             # for correlation plot

# c) Set working directory

setwd("C:/Users/vidhi/Downloads")

################################################################################################################

# ------------------------------------- 2. Data Understanding --------------------------------------------------

## DATA DICTIONARY:

# Car_ID			      Unique id of each observation (Interger)		
# Symboling 			  Its assigned insurance risk rating, 
#                   A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical) 		
# carCompany			  Name of car company (Categorical)		
# fueltype		  	  Car fuel type i.e gas or diesel (Categorical)		
# aspiration			  Aspiration used in a car (Categorical)		
# doornumber			  Number of doors in a car (Categorical)		
# carbody			      body of car (Categorical)		
# drivewheel			  type of drive wheel (Categorical)		
# enginelocation		Location of car engine (Categorical)		
# wheelbase			    Weelbase of car (Numeric)		
# carlength			    Length of car (Numeric)		
# carwidth			    Width of car (Numeric)		
# carheight			    height of car (Numeric)		
# curbweight			  The weight of a car without occupants or baggage. (Numeric)		
# enginetype			  Type of engine. (Categorical)		
# cylindernumber		cylinder placed in the car (Categorical)		
# enginesize			  Size of car (Numeric)		
# fuelsystem			  Fuel system of car (Categorical)		
# boreratio			    Boreratio of car (Numeric)		
# stroke			      Stroke or volume inside the engine (Numeric)		
# compressionratio	compression ratio of car (Numeric)		
# horsepower			  Horsepower (Numeric)		
# peakrpm			      car peak rpm (Numeric)		
# citympg			      Mileage in city (Numeric)		
# highwaympg		  	Mileage on highway (Numeric)
# price       		  Price of car (Numeric)		(Dependent variable)


# a) Reading/loading the file into a dataframe

Cars<- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)
View(Cars)
str(Cars)  # 205 Observations of 26 variables

# The data has Categorical Variables like 
# CarName,fueltype,aspiration,doornumber,carbody,drivewheel,enginelocation,
# enginetype,cylindernumber, fuelsystem		

# The data has Quantitative Variables like 
# symboling, wheelbase,carlength,carwidth,carheight,curbweight,enginesize,boreratio,stroke,compressionratio,		
# horsepower,peakrpm,citympg,highwaympg,price(Dependent variable)		


#################################################################################################################

# ------------------------------------- 3. Data Preparation/Cleaning -------------------------------

# a) Check for NA values in the data frame
sum(is.na(Cars))                # There are no NA values in the data frame

# b) Look for duplicate values
sum(duplicated(Cars$car_ID))

# c) Check for Unique Car Names
no_of_cars <- unique(Cars$CarName)
length(no_of_cars)  # 147 unique cars are given out of 205 Car Names

# d) Check for blank "" values
sapply(Cars, function(x) length(which(x == ""))) # There are none

# e) Remove CarID as it is just serial number and of no use in the data analysis
Cars <- Cars[ ,-1]
View(Cars)
str(Cars)

# f) Lets check the engine types
summary(factor(Cars$enginetype))

# Here dohc is same as dohcv,there is no such type of engine called 'dohcv' (Reference:Google)
# It is clearly a mistake as we have only one entry for it, let's correct that

Cars$enginetype[Cars$enginetype=='dohcv'] <- "dohc"

# let's check the summary again
summary(factor(Cars$enginetype))


# g) Now let's check fuel systems
summary(factor(Cars$fuelsystem))

# Here mpfi and mfi both are the same, meaning Multi-point/port fuel injection (Reference:Google)
# It is clearly a mistake as we have only one entry for it, let's correct it

Cars$fuelsystem[Cars$fuelsystem=='mfi'] <- "mpfi"

# let's check the summary again
summary(factor(Cars$fuelsystem))


#################################################################################################################

# ------------------------------------ 4. Derived Metrics --------------------------------------------------------

# a) Since it has been mentioned in the instructions to use only Car Company name 
#    Separating carcompany name with their models and replacing the original column with the separated

Car_Price<- separate(Cars, CarName, into = ("carcompany"), sep = ' ', remove = TRUE, extra = 'drop' )

# Converting the first letter of all company names to Uppercase for uniformity
Car_Price$carcompany<- str_to_title(Car_Price$carcompany)

# Let's do the same for other categorical variables 

Car_Price$fueltype       <- str_to_title(Car_Price$fueltype)
Car_Price$aspiration     <- str_to_title(Car_Price$aspiration)
Car_Price$doornumber     <- str_to_title(Car_Price$doornumber)
Car_Price$carbody        <- str_to_title(Car_Price$carbody)
Car_Price$drivewheel     <- str_to_title(Car_Price$drivewheel)
Car_Price$enginelocation <- str_to_title(Car_Price$enginelocation)
Car_Price$enginetype     <- str_to_title(Car_Price$enginetype)
Car_Price$cylindernumber <- str_to_title(Car_Price$cylindernumber)
Car_Price$fuelsystem     <- str_to_title(Car_Price$fuelsystem)

View(Car_Price)

# Checking the levels of CarCompany Variable
summary(factor(Car_Price$carcompany))

# There is a spelling mistake in a Car Company named Alfa-Romero which should be Alfa-Romeo (Reference: Google)
# There is a spelling mistake in a Car Company named Maxda (which does not exist) & it should be Mazda
# There is a spelling mistake in a Car Company named Porcshce which should be Porsche
# There is a spelling mistake in a Car Company named Toyouta which should be Toyota
# There is a spelling mistake in a Car Company named Vokswagen which should be Volkswagen
# Volkswagen also has an abbreviation called Vw which should be considered under Volkswagen

# Correcting all these spelling mistakes
Car_Price$carcompany[Car_Price$carcompany=='Alfa-Romero'] <- "Alfa-Romeo"
Car_Price$carcompany[Car_Price$carcompany=='Maxda'] <- "Mazda"
Car_Price$carcompany[Car_Price$carcompany=='Porcshce'] <- "Porsche"
Car_Price$carcompany[Car_Price$carcompany=='Toyouta'] <- "Toyota"
Car_Price$carcompany[Car_Price$carcompany=='Vokswagen'] <- "Volkswagen"
Car_Price$carcompany[Car_Price$carcompany=='Vw'] <- "Volkswagen"

summary(factor(Car_Price$carcompany)) # Checking the changed levels of CarCompany
View(Car_Price)


# b) Let's check the summary of symboling
summary(factor(Car_Price$symboling))

# Let's convert symboling into two levels namely safe and risky
Car_Price$symboling[Car_Price$symboling == -2 | Car_Price$symboling == -1 | Car_Price$symboling == 0] <- "Safe"
Car_Price$symboling[Car_Price$symboling == 3 | Car_Price$symboling == 2 | Car_Price$symboling == 1] <- "Risky"

# Let's check the summary again
summary(factor(Car_Price$symboling))

##############################################################################################################

# -------------------------------- 5. Univariate Analysis-----------------------------------------------------

# Let's check the dependent variable that is price
hist(Car_Price$price, main = "Price Historgram", xlab = "price")

# We can see that the frequency of cars with the price of 10000 is highest


################################# OUTLIER IDENTIFICATION AND REPLACEMENT ######################################

# Let's check the min, max mean and median values of all the numerical variables
summary(Car_Price)  # We can clearly see that there are outliers in the data

# Checking the Outliers for all the numerical variables individually and replacing them

# Let's check the outlier for our dependent variable 'price' using a box plot
boxplot(Car_Price$price)            # It has Outliers

# Let's check which values are outliers in this variable
boxplot.stats(Car_Price$price)$out
quantile(Car_Price$price, seq(0,1,0.01))  # Let's cap at 92%

# Let's replace the outlier values with the value at 92%
Car_Price$price[which(Car_Price$price>27336.32)] <- 27336.32

# Let's check the box plot again
boxplot(Car_Price$price)       # No outliers remaining


# Similarly, Let's remove outliers from all the other numerical variables

# For wheelbase
boxplot(Car_Price$wheelbase)         # It has Outliers
boxplot.stats(Car_Price$wheelbase)$out
quantile(Car_Price$wheelbase, seq(0,1,0.01)) # Let's cap at 98%
Car_Price$wheelbase[which(Car_Price$wheelbase>114.2)] <- 114.2
boxplot(Car_Price$wheelbase)  # No outliers remaining

# For carlength
boxplot(Car_Price$carlength)         # It has Outliers
boxplot.stats(Car_Price$carlength)$out
quantile(Car_Price$carlength, seq(0,1,0.01)) # Let's cap before 1%
Car_Price$carlength[which(Car_Price$carlength<144.8)] <- 144.8
boxplot(Car_Price$carlength)  # No outliers remaining

# For carwidth
boxplot(Car_Price$carwidth)          # It has Outliers
boxplot.stats(Car_Price$carwidth)$out
quantile(Car_Price$carwidth, seq(0,1,0.01))  # Let's cap at 96%
Car_Price$carwidth[which(Car_Price$carwidth>70.9)] <- 70.9
boxplot(Car_Price$carwidth)  # No outliers remaining

# For carheight
boxplot(Car_Price$carheight)      # No outliers
boxplot.stats(Car_Price$carheight)$out

# For curbweight
boxplot(Car_Price$curbweight)     # No outliers
boxplot.stats(Car_Price$curbweight)$out

# For enginesize
boxplot(Car_Price$enginesize)        # It has Outliers
boxplot.stats(Car_Price$enginesize)$out
quantile(Car_Price$enginesize, seq(0,1,0.01))    # Let's cap at 95%
Car_Price$enginesize[which(Car_Price$enginesize>202)] <- 202
boxplot(Car_Price$enginesize)  # No outliers remaining

# For boreratio
boxplot(Car_Price$boreratio)    # No outliers
boxplot.stats(Car_Price$boreratio)$out

# For stroke
boxplot(Car_Price$stroke)            # It has Outliers
boxplot.stats(Car_Price$stroke)$out
quantile(Car_Price$stroke, seq(0,1,0.01))     # Let's cap at 97% and below 8%
Car_Price$stroke[which(Car_Price$stroke>3.8600)] <- 3.8600
Car_Price$stroke[which(Car_Price$stroke<2.7056)] <- 2.7056
boxplot(Car_Price$stroke)  # No outliers remaining

# For compressionratio
boxplot(Car_Price$compressionratio)  # It has Outliers
boxplot.stats(Car_Price$compressionratio)$out
quantile(Car_Price$compressionratio, seq(0,1,0.01))     # Let's cap at 89% and below 4%
Car_Price$compressionratio[which(Car_Price$compressionratio>10)] <- 10
Car_Price$compressionratio[which(Car_Price$compressionratio<7.50)] <- 7.50
boxplot(Car_Price$compressionratio)  # No outliers remaining

# For horsepower
boxplot(Car_Price$horsepower)        # It has Outliers
boxplot.stats(Car_Price$horsepower)$out
quantile(Car_Price$horsepower, seq(0,1,0.01))     # Let's cap at 97%
Car_Price$horsepower[which(Car_Price$horsepower>184)] <- 184
boxplot(Car_Price$horsepower)  # No outliers remaining

# For peakrpm
boxplot(Car_Price$peakrpm)           # It has Outliers
boxplot.stats(Car_Price$peakrpm)$out
quantile(Car_Price$peakrpm, seq(0,1,0.01))     # Let's cap at 99%
Car_Price$peakrpm[which(Car_Price$peakrpm>6000)] <- 6000
boxplot(Car_Price$peakrpm)  # No outliers remaining

# For citympg
boxplot(Car_Price$citympg)           # It has Outliers
boxplot.stats(Car_Price$citympg)$out
quantile(Car_Price$citympg, seq(0,1,0.01))       # Let's cap at 99%
Car_Price$citympg[which(Car_Price$citympg>44.72)] <- 44.72
boxplot(Car_Price$citympg)  # No outliers remaining

# For highwaympg
boxplot(Car_Price$highwaympg)        # It has Outliers
boxplot.stats(Car_Price$highwaympg)$out
quantile(Car_Price$highwaympg, seq(0,1,0.01))        # Let's cap at 98%
Car_Price$highwaympg[which(Car_Price$highwaympg>46.92)] <- 46.92
boxplot(Car_Price$highwaympg)  # No outliers remaining



# No need to check the Outliers for all the categorical variables
# Categories which have less than 10% of the data are considered as categorical outliers
# These outliers will drop when we create dummy variables and make the model

################################################################################################################

# --------------------------------- 6. Bivariate Analysis ------------------------------------------------------

# Checking the correlation between the numerical variables

# Let's make a dataframe with numerical variables
Cars_Price_correlation  <-  Car_Price[ ,-c(1:8,14,15,17)]

# Let's find out the correlation between these variables
Final_correlation <- cor(Cars_Price_correlation)
View(round(Final_correlation,2))

# Let's plot this correlation
corrplot(Final_correlation, method = "circle", 
                              type = "lower", title = "Correlation of numerical variables")


###############################################################################################################

# --------------------------------- 7. Linear Regression Model -----------------------------------------------


# DUMMY VARIABLE CREATION 

# Let us see the structure of all the categorical variables 

# With 2 levels: "symboling", "fueltype", "aspiration", "doornumber", "enginelocation"
summary(factor(Car_Price$symboling))
summary(factor(Car_Price$fueltype))
summary(factor(Car_Price$aspiration))
summary(factor(Car_Price$doornumber))
summary(factor(Car_Price$enginelocation))

# Let us convert these categorical variables into dummies (numeric variables for LR model)

# Risky into 0 and Safe into 1
Car_Price$symboling <- as.factor(Car_Price$symboling)
levels(Car_Price$symboling)<-c(0,1)
Car_Price$symboling <- as.numeric(levels(Car_Price$symboling))[Car_Price$symboling]

# Diesel into 0 and Gas into 1
Car_Price$fueltype <- as.factor(Car_Price$fueltype)
levels(Car_Price$fueltype)<-c(0,1)
Car_Price$fueltype <- as.numeric(levels(Car_Price$fueltype))[Car_Price$fueltype]

# Std into 1 and Turbo into 0
Car_Price$aspiration <- as.factor(Car_Price$aspiration)
levels(Car_Price$aspiration)<-c(1,0)
Car_Price$aspiration <- as.numeric(levels(Car_Price$aspiration))[Car_Price$aspiration]

# Four into 1 and Two into 0
Car_Price$doornumber <- as.factor(Car_Price$doornumber)
levels(Car_Price$doornumber)<-c(1,0)
Car_Price$doornumber <- as.numeric(levels(Car_Price$doornumber))[Car_Price$doornumber]

# Front into 1 and rear into 0
Car_Price$enginelocation <- as.factor(Car_Price$enginelocation)
levels(Car_Price$enginelocation)<-c(1,0)
Car_Price$enginelocation <- as.numeric(levels(Car_Price$enginelocation))[Car_Price$enginelocation]


# With 3 or more levels : "carbody", "drivewheel", "enginetype", "cylindernumber", fuelsystem"

summary(factor(Car_Price$carbody))
summary(factor(Car_Price$drivewheel))
summary(factor(Car_Price$enginetype))
summary(factor(Car_Price$cylindernumber))
summary(factor(Car_Price$fuelsystem))


dummy_1 <- data.frame(model.matrix( ~carbody, data = Car_Price))
dummy_1 <- dummy_1[,-1]

dummy_2 <- data.frame(model.matrix( ~drivewheel, data = Car_Price))
dummy_2 <- dummy_2[,-1]

dummy_3 <- data.frame(model.matrix( ~enginetype, data = Car_Price))
dummy_3 <- dummy_3[,-1]

dummy_4 <- data.frame(model.matrix( ~fuelsystem, data = Car_Price))
dummy_4 <- dummy_4[,-1]

dummy_5 <- data.frame(model.matrix( ~carcompany, data = Car_Price))
dummy_5 <- dummy_5[,-1]

dummy_6 <- data.frame(model.matrix( ~cylindernumber, data = Car_Price))
dummy_6 <- dummy_6[,-1]

View(dummy_1)
View(dummy_2)
View(dummy_3)
View(dummy_4)
View(dummy_5)
View(dummy_6)


# Keeping all these converted variables into a dataframe

dummies <- cbind(dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6)
View(dummies)

# Removing the original categorical columns from the data frame
View(Car_Price)
Car_model <- Car_Price[ ,-c(2,6,7,14,15,17)]

# Combining the dummy variables to the main data set
Car_final_model <- cbind(Car_model, dummies)
View(Car_final_model)
str(Car_final_model)


# MODEL CREATION

# Divide into training and test data set

#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(Car_final_model), 0.7*nrow(Car_final_model))

# generate the train data set
train = Car_final_model[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = Car_final_model[-trainindices,]

# Here the dependent variable is price

# Building linear model using lm() by taking all the independent variables and storing it into an object called model_1  
model_1 <- lm(price~.,data=train)

# Now, let's check the summary of model_1
summary(model_1)

# Check if the correlation matrix gives some insight.

corrs = cor(train)
View(round(corrs,2))


# Here we are using stepwise varible selection method due to large number of varibles

# Now, let's calculate stepAIC
step <- stepAIC(model_1, direction = "both")  # 'Mass' package already installed
step 

# Let's build second model using the result that we got in stepAIC

model_2 <- stepAIC(model_1, direction = "both")

# Let's check the summary of model_2
summary(model_2)


# Now let's check multicolinearity by using VIF
vif(model_2)                                # 'car' package already installed

# Now let's create model_3 by removing variable with highest VIF (cylindernumberFour) and high p value

model_3 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + horsepower
              + carbodyHardtop + carbodyHatchback + carbodySedan + carbodyWagon + drivewheelFwd + enginetypeL
              + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl + fuelsystemMpfi 
              + fuelsystemSpdi + carcompanyAudi + carcompanyBmw + carcompanyBuick + carcompanyChevrolet
              + carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda + carcompanyMitsubishi 
              + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault + carcompanyToyota
              + carcompanyVolkswagen + carcompanyVolvo + cylindernumberFive + cylindernumberSix,data=train)

# Let's check the summary to model_3
summary(model_3)       # There is not much change in the R-squared and adjusted R-squared



# Now let's check the vif of model_3 
vif(model_3)


# Enginesize has highest vif and it has high correlation with several variables
# p-value of cylindernumberSix is more amongst all so let's remove that


model_4 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + horsepower
              + carbodyHardtop + carbodyHatchback + carbodySedan + carbodyWagon + drivewheelFwd + enginetypeL
              + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl + fuelsystemMpfi 
              + fuelsystemSpdi + carcompanyAudi + carcompanyBmw + carcompanyBuick + carcompanyChevrolet
              + carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda + carcompanyMitsubishi 
              + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault + carcompanyToyota
              + carcompanyVolkswagen + carcompanyVolvo + cylindernumberFive,data=train)

# Let's check the summary and VIF of model_4
summary(model_4)              # There is negligible change in the R-squared and adjusted R-squared

vif(model_4)

# Enginesize has highest vif and it has high correlation with several variables
# p-value of stroke is more amongst all so let's remove that 

model_5 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
              + carbodyHardtop + carbodyHatchback + carbodySedan + carbodyWagon + drivewheelFwd + enginetypeL
              + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl + fuelsystemMpfi 
              + fuelsystemSpdi + carcompanyAudi + carcompanyBmw + carcompanyBuick + carcompanyChevrolet
              + carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda + carcompanyMitsubishi 
              + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault + carcompanyToyota
              + carcompanyVolkswagen + carcompanyVolvo + cylindernumberFive, data=train)

# Let's check the summary and VIF of model_5
summary(model_5)              # There is negligible change in the R-squared and adjusted R-squared

vif(model_5)

# Enginesize has highest vif and it has high correlation with several variables
# p-value of carbodyHardtop is more amongst all so let's remove that 

model_6 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
              + carbodyHatchback + carbodySedan + carbodyWagon + drivewheelFwd + enginetypeL
              + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl + fuelsystemMpfi 
              + fuelsystemSpdi + carcompanyAudi + carcompanyBmw + carcompanyBuick + carcompanyChevrolet
              + carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda + carcompanyMitsubishi 
              + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault + carcompanyToyota
              + carcompanyVolkswagen + carcompanyVolvo + cylindernumberFive, data=train)

# Let's check the summary and VIF of model_6
summary(model_6)              # There is negligible change in the R-squared and adjusted R-squared

vif(model_6)

# curbweight has highest vif and it has high correlation with several variables
# p-value of carcompanyVolvo is more amongst all so let's remove that 

model_7 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
              + carbodyHatchback + carbodySedan + carbodyWagon + drivewheelFwd + enginetypeL
              + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl + fuelsystemMpfi 
              + fuelsystemSpdi + carcompanyAudi + carcompanyBmw + carcompanyBuick + carcompanyChevrolet
              + carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda + carcompanyMitsubishi 
              + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault + carcompanyToyota
              + carcompanyVolkswagen + cylindernumberFive, data=train)

# Let's check the summary and VIF of model_7
summary(model_7)              # There is negligible change in the R-squared and adjusted R-squared

vif(model_7)

# Let's remove carbodySedan as it has high vif and high p value

model_8 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
              + carbodyHatchback + carbodyWagon + drivewheelFwd + enginetypeL
              + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl + fuelsystemMpfi 
              + fuelsystemSpdi + carcompanyAudi + carcompanyBmw + carcompanyBuick + carcompanyChevrolet
              + carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda + carcompanyMitsubishi 
              + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault + carcompanyToyota
              + carcompanyVolkswagen + cylindernumberFive, data=train)

# Let's check the summary and VIF of model_8
summary(model_8)              # There is no change in the R-squared and adjusted R-squared

vif(model_8)

# Let's remove cylindernumberFive as it has high vif and high p value

model_9 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
              + carbodyHatchback + carbodyWagon + drivewheelFwd + enginetypeL
              + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl + fuelsystemMpfi 
              + fuelsystemSpdi + carcompanyAudi + carcompanyBmw + carcompanyBuick + carcompanyChevrolet
              + carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda + carcompanyMitsubishi 
              + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault + carcompanyToyota
              + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_9
summary(model_9)              # There is no change in the R-squared and adjusted R-squared

vif(model_9)

#  Let's remove drivewheelFwd as it has high vif and high p value

model_10 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
              + carbodyHatchback + carbodyWagon + enginetypeL + enginetypeOhc
              + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl + fuelsystemMpfi 
              + fuelsystemSpdi + carcompanyAudi + carcompanyBmw + carcompanyBuick + carcompanyChevrolet
              + carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda + carcompanyMitsubishi 
              + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault + carcompanyToyota
              + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_10
summary(model_10)              # There is no change in the R-squared and adjusted R-squared

vif(model_10)

#  Let's remove carbodyHatchback as it has high vif and high p value

model_11 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
               + carbodyWagon + enginetypeL + enginetypeOhc
               + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl + fuelsystemMpfi 
               + fuelsystemSpdi + carcompanyAudi + carcompanyBmw + carcompanyBuick + carcompanyChevrolet
               + carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda + carcompanyMitsubishi 
               + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault + carcompanyToyota
               + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_11
summary(model_11)              # There is no change in the R-squared and adjusted R-squared

vif(model_11)

#  Let's remove carbodyWagon as it has high vif and high p value

model_12 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
               + enginetypeL + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl 
               + fuelsystemMpfi + fuelsystemSpdi + carcompanyAudi +  carcompanyBmw + carcompanyBuick 
               + carcompanyChevrolet+ carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_12
summary(model_12)              # There is not much change in the R-squared and adjusted R-squared

vif(model_12)

#  Let's remove fuelsystemSpdi as it has high p value

model_13 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
               + enginetypeL + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl 
               + fuelsystemMpfi + carcompanyAudi +  carcompanyBmw + carcompanyBuick 
               + carcompanyChevrolet+ carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_13
summary(model_13)              # There is no change in the R-squared and adjusted R-squared

vif(model_13)

#  Let's remove carcompanyAudi as it has high p value

model_14 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
               + enginetypeL + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl 
               + fuelsystemMpfi +  carcompanyBmw + carcompanyBuick 
               + carcompanyChevrolet+ carcompanyDodge + carcompanyIsuzu + carcompanyJaguar + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_14
summary(model_14)              # There is no change in the R-squared and adjusted R-squared

vif(model_14) 

#  Let's remove carcompanyJaguar as it has high p value

model_15 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
               + enginetypeL + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl 
               + fuelsystemMpfi +  carcompanyBmw + carcompanyBuick 
               + carcompanyChevrolet+ carcompanyDodge + carcompanyIsuzu + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_15
summary(model_15)              # There is not much change in the R-squared and adjusted R-squared

vif(model_15) 

#  Let's remove carcompanyIsuzu as it has high p value

model_16 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
               + enginetypeL + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl 
               + fuelsystemMpfi +  carcompanyBmw + carcompanyBuick 
               + carcompanyChevrolet+ carcompanyDodge + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyPorsche + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_16
summary(model_16)              # There is not much change in the R-squared and adjusted R-squared

vif(model_16)

#  Let's remove carcompanyPorsche as it has high p value

model_17 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
               + enginetypeL + enginetypeOhc + enginetypeOhcf + enginetypeRotor + fuelsystem2Bbl 
               + fuelsystemMpfi +  carcompanyBmw + carcompanyBuick 
               + carcompanyChevrolet+ carcompanyDodge + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_17
summary(model_17)              # There is not much change in the R-squared and adjusted R-squared

vif(model_17)

#  Let's remove enginetypeRotor as it has high p value

model_18 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
               + enginetypeL + enginetypeOhc + enginetypeOhcf  + fuelsystem2Bbl 
               + fuelsystemMpfi +  carcompanyBmw + carcompanyBuick 
               + carcompanyChevrolet+ carcompanyDodge + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_18
summary(model_18)              # There is not much change in the R-squared and adjusted R-squared

vif(model_18)

#  Let's remove fuelsystem2Bbl as it has high p value

model_19 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + enginesize + horsepower
               + enginetypeL + enginetypeOhc + enginetypeOhcf + fuelsystemMpfi +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet+ carcompanyDodge + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_19
summary(model_19)              # There is not much change in the R-squared and adjusted R-squared

vif(model_19)

#  Let's remove enginesize as it has high p value

model_20 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight + horsepower
               + enginetypeL + enginetypeOhc + enginetypeOhcf + fuelsystemMpfi +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet+ carcompanyDodge + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_20
summary(model_20)              # There is not much change in the R-squared and adjusted R-squared

vif(model_20)

#  Let's remove horsepower as it has high p value

model_21 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight 
               + enginetypeL + enginetypeOhc + enginetypeOhcf + fuelsystemMpfi +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet+ carcompanyDodge + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_21
summary(model_21)              # There is not much change in the R-squared and adjusted R-squared

vif(model_21)

#  Let's remove fuelsystemMpfi as it has high p value

model_22 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight 
               + enginetypeL + enginetypeOhc + enginetypeOhcf +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet+ carcompanyDodge + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyNissan + carcompanyPlymouth + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_22
summary(model_22)              # There is not much change in the R-squared and adjusted R-squared

vif(model_22)

#  Let's remove carcompanyNissan as it has high p value

model_23 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight 
               + enginetypeL + enginetypeOhc + enginetypeOhcf +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet+ carcompanyDodge + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyPlymouth + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_23
summary(model_23)              # There is not much change in the R-squared and adjusted R-squared

vif(model_23)

#  Let's remove carcompanyDodge as it has high p value

model_24 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight 
               + enginetypeL + enginetypeOhc + enginetypeOhcf +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyPlymouth + carcompanyRenault
               + carcompanyToyota + carcompanyVolkswagen, data=train)

# Let's check the summary and VIF of model_24
summary(model_24)              # There is not much change in the R-squared and adjusted R-squared

vif(model_24)

#  Let's remove carcompanyVolkswagen as it has high p value

model_25 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight 
               + enginetypeL + enginetypeOhc + enginetypeOhcf +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyPlymouth + carcompanyRenault
               + carcompanyToyota, data=train)

# Let's check the summary and VIF of model_25
summary(model_25)              # There is not much change in the R-squared and adjusted R-squared

vif(model_25)

#  Let's remove carcompanyPlymouth as it has high p value

model_26 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight 
               + enginetypeL + enginetypeOhc + enginetypeOhcf +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet + carcompanyMazda 
               + carcompanyMitsubishi + carcompanyRenault
               + carcompanyToyota, data=train)

# Let's check the summary and VIF of model_26
summary(model_26)              # There is not much change in the R-squared and adjusted R-squared

vif(model_26)

#  Let's remove carcompanyMazda as it has high p value

model_27 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight 
               + enginetypeL + enginetypeOhc + enginetypeOhcf +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet  
               + carcompanyMitsubishi + carcompanyRenault
               + carcompanyToyota, data=train)

# Let's check the summary and VIF of model_27
summary(model_27)              # There is not much change in the R-squared and adjusted R-squared

vif(model_27)

#  Let's remove carcompanyRenault as it has high p value

model_28 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight 
               + enginetypeL + enginetypeOhc + enginetypeOhcf +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet  
               + carcompanyMitsubishi + carcompanyToyota, data=train)
               
# Let's check the summary and VIF of model_28
summary(model_28)              # There is not much change in the R-squared and adjusted R-squared

vif(model_28)


#  Let's remove carcompanyMitsubishi as it has high p value

model_29 <- lm(price~ + aspiration + enginelocation + carwidth + curbweight 
               + enginetypeL + enginetypeOhc + enginetypeOhcf +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet  
               + carcompanyToyota, data=train)

# Let's check the summary and VIF of model_29
summary(model_29)              # There is not much change in the R-squared and adjusted R-squared

vif(model_29)

#  Let's remove aspiration as it has high p value

model_30 <- lm(price~ + enginelocation + carwidth + curbweight 
               + enginetypeL + enginetypeOhc + enginetypeOhcf +  carcompanyBmw
               + carcompanyBuick + carcompanyChevrolet  
               + carcompanyToyota, data=train)

# Let's check the summary and VIF of model_30
summary(model_30)              # There is not much change in the R-squared and adjusted R-squared

vif(model_30)


# Model_30 has all the significant variables which affect the price of the car in American market

# The significant variables are
# enginelocation, carwidth, curbweight, enginetypeL, enginetypeOhc, enginetypeOhcf, carcompanyBmw
# carcompanyBuick, carcompanyChevrolet, carcompanyToyota

# Predict the prices in the testing dataset

Predict_1 <- predict(model_30,test[,-19])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation

r <- cor(test$price,test$test_price)

# calculate R squared by squaring correlation

rsquared <- cor(test$price,test$test_price)^2

# check R-squared

rsquared
