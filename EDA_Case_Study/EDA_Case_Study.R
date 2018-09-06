
# Name: Gramener Case Study 
# Submitted by: Anjala A R, Asha Katla, Vighneshwar Mishra, Vidhi Thakkar
# Date: 01-04-2018


## ------------ Part 1: Data understanding and preparation ----------------------

# The given data contains the information about past accepted loan applicants and whether they 'defaulted' or not.

# Set working directory

setwd()

# Load the Data to a dataframe

loandata <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)
# View(loandata)

str(loandata)          # Structure of the dataframe
summary(loandata)      # Summary of the dataframe


# Install Necessary Packages

install.packages("lubridate")
install.packages("ggplot2")
install.packages("plyr")
install.packages("tidyverse")
install.packages("gmodels")
install.packages("tidyr")
install.packages("corrplot")
install.packages("stats")

# Load the installed Packages

library(lubridate)
library(ggplot2)
library(plyr)
library(tidyverse)
library(gmodels)
library(tidyr)
library(corrplot)
library(stats)

# ----------------------------------------------------------------------------------------------------

## ----------------- Part 2: Data Cleaning and Manipulation ------------------------

#**Making a copy of the data, and cleaning the data to make sense during analysis**

loan_working_copy <- loandata[,sapply(loandata, function(x) length(unique(x))>1)]

loan_working_copy$term                <- as.factor(loan_working_copy$term)
loan_working_copy$grade               <- as.factor(loan_working_copy$grade)
loan_working_copy$sub_grade           <- as.factor(loan_working_copy$sub_grade)
loan_working_copy$verification_status <- as.factor(loan_working_copy$verification_status)
loan_working_copy$loan_status         <- as.factor(loan_working_copy$loan_status)

# Dividing based on the charged off status into two Categories i.e. Defaulters and Paid/Paying

loan_working_copy$status              <- as.factor(ifelse((loan_working_copy$loan_status=="Charged Off"),"Defaulter","Paid/Paying"))

# Making two new columns by removing % from int_rate column and months from term column

loan_working_copy$int_rate_act        <- as.numeric(gsub("\\%", "", loan_working_copy$int_rate))
loan_working_copy$term_act            <- as.numeric(gsub("\\ months","",loan_working_copy$term))

# Formatting dates by adding dummy day to these 4 columns

loan_working_copy$issue_d             <- paste("01",loan_working_copy$issue_d,sep = "-")
loan_working_copy$issue_d             <- as.Date(loan_working_copy$issue_d,format = "%d-%b-%y")

loan_working_copy$earliest_cr_line    <- paste("01",loan_working_copy$earliest_cr_line,sep = "-")
loan_working_copy$earliest_cr_line    <- as.Date(loan_working_copy$earliest_cr_line,format = "%d-%b-%y")

loan_working_copy$last_pymnt_d        <- paste("01",loan_working_copy$last_pymnt_d,sep = "-")
loan_working_copy$last_pymnt_d        <- as.Date(loan_working_copy$last_pymnt_d,format = "%d-%b-%y")

loan_working_copy$last_credit_pull_d  <- paste("01",loan_working_copy$last_credit_pull_d,sep =  "-")
loan_working_copy$last_credit_pull_d  <- as.Date(loan_working_copy$last_credit_pull_d,format = "%d-%b-%y")



##**There are 22 variables that are important to the analysis. Removing all the other variables**


# Also, removing columns like interest rate,term and revol_util as they are duplicates

loan_working_copy <- loan_working_copy[,-c(6,7)]
loan_working_copy <- loan_working_copy[,-31]

# Calculating the percentage of NA values for each column
na_percent <- as.data.frame(round(colMeans(is.na(loan_working_copy)),2))
which(na_percent>.7)

# Now the table has columns which has at least 70% of the data

loan_working_copy <- loan_working_copy[,-41]
loan_working_copy <- loan_working_copy[,-27]

#Plotting correlation to see if there are highly correlated variables and remove them

loan_numeric <- Filter(is.numeric,loan_working_copy)
cordata_before_cleaning <- as.data.frame(cor(loan_working_copy[sapply(loan_working_copy, is.numeric)]))

##Removing data not important to analysis

loan_working_copy <- loan_working_copy[,-c(1:2)]
loan_working_copy <- loan_working_copy[,-c(2:3)]
loan_working_copy <- loan_working_copy[,-5]
loan_working_copy <- loan_working_copy[,-8]
loan_working_copy <- loan_working_copy[,-c(9:11)]
loan_working_copy <- loan_working_copy[,-c(10:13)]
loan_working_copy <- loan_working_copy[,-13]
loan_working_copy <- loan_working_copy[,-c(15:32)]

cordata_after_cleaning <- as.data.frame(cor(loan_working_copy[sapply(loan_working_copy, is.numeric)]))

# Let's correlate the data with the available fields

corrplot(cor(loan_working_copy[sapply(loan_working_copy, is.numeric)]),method = "circle")

# ----------------------------------------------------------------------------------------------------

## --------------- Part 3: Data Analysis ------------------------------------

# Creating Dervived metrics useful for analysis

# Creating 4 types of income levels

for(i in 1:nrow(loan_working_copy)) {
  if((loan_working_copy$annual_inc[i]>=0) & (loan_working_copy$annual_inc[i]<24999)) {
    loan_working_copy$income_level[i] <-"Federal poverty level"
  } else if((loan_working_copy$annual_inc[i]>=25000) & (loan_working_copy$annual_inc[i]<34999 )) {
    loan_working_copy$income_level[i] <-"Low Income"
  } else if((loan_working_copy$annual_inc[i] >=35000) & (loan_working_copy$annual_inc[i] <49999)) {
    loan_working_copy$income_level[i] <-"Middle Income"
  } else if((loan_working_copy$annual_inc[i]>=50000) & (loan_working_copy$annual_inc[i]<74999)) {
    loan_working_copy$income_level[i] <-"High Income"
  } else {
    loan_working_copy$income_level[i] <-"Super Rich" 
  }
}

# ---------------------------------------------------------------------------------------------------- 

# Adding derived metrics columns that may help in analysis

z <- Sys.Date()

loan_working_copy$earliest_cr_line <- as.Date(loan_working_copy$earliest_cr_line,format="%Y/%m/%d")

for(i in 1:nrow(loan_working_copy)) {
  loan_working_copy$credit_line_days[i]<-z-loan_working_copy$earliest_cr_line[i]
}

for(i in 1:nrow(loan_working_copy)) {
  loan_working_copy$annual_installment[i]<-loan_working_copy$installment[i]*12
}

for(i in 1:nrow(loan_working_copy)) {
  loan_working_copy$income_to_installment[i]<-loan_working_copy$annual_installment[i]/loan_working_copy$annual_inc[i]
}


for(i in 1:nrow(loan_working_copy)) {
  loan_working_copy$loan_income[i]<-loan_working_copy$loan_amnt[i]/loan_working_copy$annual_inc[i]
}

# Creating categories based on job experience

category1 <- c("< 1 year","1 year","2 years","n/a")
category2 <- c("3 years","4 years","5 years","6 years")
category3 <- c("7 years","8 years","9 years","10+ years")


for(i in 1:nrow(loan_working_copy)) {
  if(loan_working_copy$emp_length[i] %in% category1) {
    loan_working_copy$emp_experience[i] <- "Junior/Not available"  
  } else if(loan_working_copy$emp_length[i] %in% category2) {
    loan_working_copy$emp_experience[i] <- "mid-Level" 
  }   else {
    loan_working_copy$emp_experience[i] <- "High Experienced"
  }
}

# ----------------------------------------------------------------------------------------------------

# Univariate Analysis

# 1. Status
bar_status<-ggplot(loan_working_copy, aes(x = factor(1), fill = factor(status))) + geom_bar(width = 1)
pie_status <- bar_status + coord_polar(theta = "y")

# 2. Grades
bar_grade<-ggplot(loan_working_copy, aes(x = factor(1), fill = factor(grade))) + geom_bar(width = 1)
pie_grade <- bar_grade + coord_polar(theta = "y")

# 3. Home Ownership
bar_own<-ggplot(loan_working_copy, aes(x = factor(1), fill = factor(home_ownership))) + geom_bar(width = 1)
pie_own <- bar_own + coord_polar(theta = "y")

# 4. Emp Experience
bar_exp<-ggplot(loan_working_copy, aes(x = factor(1), fill = factor(emp_experience))) + geom_bar(width = 1)
pie_exp <- bar_exp + coord_polar(theta = "y")

# 5. Purpose
bar_purpose<-ggplot(loan_working_copy, aes(x = factor(1), fill = factor(purpose))) + geom_bar(width = 1)
pie_purpose <- bar_purpose + coord_polar(theta = "y")

# 6. Income Level
bar_Income<-ggplot(loan_working_copy, aes(x = factor(1), fill = factor(income_level))) + geom_bar(width = 1)
pie_Income <- bar_Income + coord_polar(theta = "y")

# ----------------------------------------------------------------------------------------------------

# Bivariate Analysis

# 1. Understanding the defaulters:-


CrossTable(loan_working_copy$status)

#  |   Defaulter | Paid/Paying | 
#  |-------------|-------------|
#  |        5627 |       34090 | 
#  |       0.142 |       0.858 | 
#  |-------------|-------------|


ggplot(loan_working_copy, aes(x=as.factor(status), fill=as.factor(status) )) +  
  geom_bar( ) + scale_fill_discrete(name="Understanding defaulters", breaks=c("0","1"),labels=c("Paid/Paying","Defaulters"))


# Understanding defaulters accross the finalized categorical variables

# 2.Understanding defaulters accross Grades

CrossTable(loan_working_copy$grade,loan_working_copy$status,prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
ggplot(loan_working_copy,aes(status,fill=grade)) + geom_bar() + ggtitle("Defaulters accross Grades") 

#                 | loan_working_copy$status 
#    |   Defaulter | Paid/Paying |   Row Total | 
# ------------------------|-------------|-------------|-------------|
#  A |         602 |        9483 |       10085 | 
#    |       0.107 |       0.278 |             | 
#  ------------------------|-------------|-------------|-------------|
#  B |        1425 |       10595 |       12020 | 
#    |       0.253 |       0.311 |             | 
#  ------------------------|-------------|-------------|-------------|
#  C |        1347 |        6751 |        8098 | 
#    |       0.239 |       0.198 |             | 
#  ------------------------|-------------|-------------|-------------|
#  D |        1118 |        4189 |        5307 | 
#    |       0.199 |       0.123 |             | 
#  ------------------------|-------------|-------------|-------------|
#  E |         715 |        2127 |        2842 | 
#    |       0.127 |       0.062 |             | 
#  ------------------------|-------------|-------------|-------------|
#  F |         319 |         730 |        1049 | 
#    |       0.057 |       0.021 |             | 
#  ------------------------|-------------|-------------|-------------|
#  G |         101 |         215 |         316 | 
#    |       0.018 |       0.006 |             | 
#  ------------------------|-------------|-------------|-------------|
#     Column Total |        5627 |       34090 |       39717 | 
#    |       0.142 |       0.858 |             | 
# --_____------------------------|-------------|-------------|-------------|

#There are max defaulters in Grade B and C

# 3.Understanding defaulters accross home ownership

CrossTable(loan_working_copy$home_ownership,loan_working_copy$status,prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
ggplot(loan_working_copy,aes(status,fill=home_ownership)) + geom_bar() + ggtitle("Defaulters accross home ownership") 


#   |   Defaulter | Paid/Paying |   Row Total | 
#  ---------------------------------|-------------|-------------|-------------|
#  MORTGAGE |        2327 |       15332 |       17659 | 
#           |       0.414 |       0.450 |             | 
#   ---------------------------------|-------------|-------------|-------------|
#      NONE |           0 |           3 |           3 | 
#           |       0.000 |       0.000 |             | 
#  ---------------------------------|-------------|-------------|-------------|
#     OTHER |          18 |          80 |          98 | 
#           |       0.003 |       0.002 |             | 
#  ---------------------------------|-------------|-------------|-------------|
#       OWN |         443 |        2615 |        3058 | 
#           |       0.079 |       0.077 |             | 
#  ---------------------------------|-------------|-------------|-------------|
#      RENT |        2839 |       16060 |       18899 | 
#           |       0.505 |       0.471 |             | 
#  ---------------------------------|-------------|-------------|-------------|
#            Column Total |        5627 |       34090 |       39717 | 
#           |       0.142 |       0.858 |             | 
#  ---------------------------------|-------------|-------------|-------------|


#We have more defaulters in Mortage, Rent people in House_Ownership

# 4.Understanding defaulters accross Experience Level
CrossTable(loan_working_copy$emp_experience,loan_working_copy$status,prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
ggplot(loan_working_copy,aes(status,fill=emp_experience)) + geom_bar() + ggtitle("Defaulters accross Experience Level") 

# 5.Understanding defaulters accross Loan category
CrossTable(loan_working_copy$purpose,loan_working_copy$status,prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
ggplot(loan_working_copy,aes(status,fill=purpose)) + geom_bar() + ggtitle("Defaulters accross Loan category") 


#| loan_working_copy$status 
#loan_working_copy$purpose |   Defaulter | Paid/Paying |   Row Total | 
#  --------------------------|-------------|-------------|-------------|
#             car     |         160 |        1389 |        1549 | 
#  |          0.028   |       0.041 |             | 
#  --------------------------|-------------|-------------|-------------|
#         credit_card |         542 |        4588 |        5130 | 
#  |            0.096 |       0.135 |             | 
#  --------------------------|-------------|-------------|-------------|
#  debt_consolidation |        2767 |       15874 |       18641 | 
#  |            0.492 |       0.466 |             | 
#  --------------------------|-------------|-------------|-------------|
#         educational |          56 |         269 |         325 | 
#       |       0.010 |       0.008 |             | 
#  --------------------------|-------------|-------------|-------------|
#    home_improvement |         347 |        2629 |        2976 | 
#       |       0.062 |       0.077 |             | 
#  --------------------------|-------------|-------------|-------------|
# house |          59 |         322 |         381 | 
#       |       0.010 |       0.009 |             | 
#  --------------------------|-------------|-------------|-------------|
#      major_purchase |         222 |        1965 |        2187 | 
#       |       0.039 |       0.058 |             | 
#  --------------------------|-------------|-------------|-------------|
#             medical |         106 |         587 |         693 | 
#       |       0.019 |       0.017 |             | 
#  --------------------------|-------------|-------------|-------------|
#              moving |          92 |         491 |         583 | 
#       |       0.016 |       0.014 |             | 
#  --------------------------|-------------|-------------|-------------|
#               other |         633 |        3360 |        3993 | 
#       |       0.112 |       0.099 |             | 
#  --------------------------|-------------|-------------|-------------|
#    renewable_energy |          19 |          84 |         103 | 
#       |       0.003 |       0.002 |             | 
#  --------------------------|-------------|-------------|-------------|
#      small_business |         475 |        1353 |        1828 | 
#       |       0.084 |       0.040 |             | 
#  --------------------------|-------------|-------------|-------------|
#            vacation |          53 |         328 |         381 | 
#       |       0.009 |       0.010 |             | 
#  --------------------------|-------------|-------------|-------------|
#             wedding |          96 |         851 |         947 | 
#       |       0.017 |       0.025 |             | 
#  --------------------------|-------------|-------------|-------------|
#        Column Total |        5627 |       34090 |       39717 | 
#       |       0.142 |       0.858 |             | 
#  --------------------------|-------------|-------------|-------------|

#Debt consolidation seems to be the riskiest purpose to give out a loan. 
# Where as wedding,vacation,non business seems to be relatively non risky

# 6.Understanding defaulters accross Income Level

CrossTable(loan_working_copy$income_level,loan_working_copy$status,prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
ggplot(loan_working_copy,aes(status,fill=income_level)) + geom_bar() + ggtitle("Defaulters accross Income Level") 

#  loan_working_copy$income_level |   Defaulter | Paid/Paying |   Row Total | 
#  -------------------------------|-------------|-------------|-------------|
#           Federal poverty level |         450 |        1876 |        2326 | 
#                   |       0.080 |       0.055 |             | 
#  -------------------------------|-------------|-------------|-------------|
#       High Income |        1741 |       10531 |       12272 | 
#                   |       0.309 |       0.309 |             | 
#  -------------------------------|-------------|-------------|-------------|
#        Low Income |         652 |        3180 |        3832 | 
#     |       0.116 |       0.093 |             | 
#  -------------------------------|-------------|-------------|-------------|
#     Middle Income |        1347 |        7181 |        8528 | 
#     |       0.239 |       0.211 |             | 
#  -------------------------------|-------------|-------------|-------------|
#        Super Rich |        1437 |       11322 |       12759 | 
#     |       0.255 |       0.332 |             | 
#  -------------------------------|-------------|-------------|-------------|
#      Column Total |        5627 |       34090 |       39717 | 
#     |       0.142 |       0.858 |             | 
#  -------------------------------|-------------|-------------|-------------|


#The most number of defaults are in middle income,high-income and super rich category

# 7.Understanding defaulters accross Interest rate

CrossTable(loan_working_copy$int_rate_act,loan_working_copy$status,prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
ggplot(loan_working_copy,aes(int_rate_act,fill=status)) + geom_bar(width = 0.1) + ggtitle("Defaulters accross Interest rate") 

# There are more defaulters falls in the range of 10 to 14 as per the above plot

# ----------------------------------------------------------------------------------------------------

# Understanding categorical variables


# 1. Analysing the income to installment ratio variation

ggplot(loan_working_copy,aes(x=annual_inc,y=as.factor(income_level)))+geom_point(color="red",fill="orange",alpha=0.2)

# There are clearly outliers in the income data, where individuals in the super rich category are earning at an abnormal level
# Hence removing the outliers for further calculations

outlier_annual_income        <- which(loan_working_copy$annual_inc > 206496)
outlier_data                 <- loan_working_copy[-outlier_annual_income,]

# Outliers are any income value above a calculated income level
# The calculated value is 3*(Q3-Q1), where Q3 and Q1 are third and first quantile values

income_vs_installment <- aggregate(
  income_to_installment~status,
  data = outlier_data,
  FUN = mean)

#status         income_to_installment
#1   Defaulter        0.07373165
#2 Paid/Paying        0.06449199


ggplot(outlier_data, aes(x=annual_inc, y=annual_installment, color=status)) + geom_point(size=1, alpha=0.1) 


int_status <- aggregate(
  int_rate_act~status,
  data = loan_working_copy,
  FUN = mean)

#       status    int_rate_act
#1   Defaulter     13.61180
#2 Paid/Paying     11.57506

ggplot(loan_working_copy, aes(x=status, y=int_rate_act, color=status)) + geom_point(size=1, alpha=0.1)


