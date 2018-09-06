#######################################################################################################################
####################################### HR Analytics Case Study  ######################################################
#######################################################################################################################

# Submission by : Hema Kalidindi, Sahana K, Varsha Atmakuri, Vidhi Thakkar

#######################################################################################################################
# -------------------------------------- 1. Business Understanding ----------------------------------------------------

# XYZ company employs around 4000 employees, at any given point of time. 
# Every year, around 15% of its employees leave the company (either on their own or because they got fired)
# These employees have to be replaced by other talented individuals.

# This level of attrition is bad for the company, because of the following reasons:

# 1) The projects get delayed, difficult to meet timelines, results in reputation loss among consumers and partners.
# 2) A sizeable department has to be maintained, for the purpose of recruiting new talent.
# 3) These new employees have to be trained for the job and have to be given time to adjust in the new work enviornment.

# Hence, HR analytics firm has been hired to do the following:

# a) to understand what factors they should focus on, in order to curb attrition.
# b) to know what changes they should make to their workplace, in order to get most of their employees to stay.
# c) to know which of these variables is most important and needs to be addressed right away.

# ---------------------------------------------------------------------------------------------------------------------------
## AIM:                                                                                                   
# To create a model with the probability of attrition using logistic regression.                         
# To check if an employee would leave or not and to find the factors affecting the attrition of employees. 


# a) Remove the previous data (if any)

rm(list=ls())

# b) Install necessary packages

install.packages("ggplot2")   
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("MASS")     
install.packages("car")      
install.packages("corrplot")  
install.packages("e1071")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("caTools")


# c) Load the Packages

library(ggplot2)              # for ggplot
library(dplyr)
library(tidyr)
library(stringr)
library(MASS)                 # for StepAIC
library(car)                  # for VIF
library(corrplot)             # for correlation plot
library(e1071)
library(caret)
library(caTools)

# d) Set working directory

setwd("C:/Users/vidhi/Desktop/HR Analytics Case study")

################################################################################################################
# ------------------------------------- 2. Data Understanding --------------------------------------------------

# DATA DICTIONARY

# 1) Categorical Variables:

# Attrition:	Whether the employee left in the previous year or not
# BusinessTravel:	How frequently the employees travelled for business purposes in the last year
# Department:	Department in company
# Education:  Education Level - 1 'Below College' 2 'College' 3 'Bachelor' 4 'Master' 5 'Doctor'
# EducationField:	Field of education
# EmployeeCount:	Employee count
# EmployeeNumber:	Employee number/id
# EnvironmentSatisfaction: Work Environment Satisfaction Level - 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
# Gender:	Gender of employee
# JobInvolvement:	Job Involvement Level - 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
# JobLevel:	Job level at company on a scale of 1 to 5
# JobRole:	Name of job role in company
# JobSatisfaction:	Job Satisfaction Level -  1 'Low' 2 'Medium' 3 'High' 4 'Very High'
# MaritalStatus:	Marital status of the employee
# Over18:	Whether the employee is above 18 years of age or not
# PerformanceRating:	Performance rating for last year - 1 'Low' 2 'Good' 3 'Excellent' 4 'Outstanding'
# WorkLifeBalance:	Work life balance level - 1 'Bad' 2 'Good' 3 'Better' 4 'Best'

# 2) Numerical Variables:

# Age: Age of the employee
# DistanceFromHome:	Distance from home in kms
# MonthlyIncome:	Monthly income in rupees per month
# NumCompaniesWorked:	Total number of companies the employee has worked for
# PercentSalaryHike:	Percent salary hike for last year
# StandardHours:	Standard hours of work for the employee
# StockOptionLevel:	Stock option level of the employee
# TotalWorkingYears:	Total number of years the employee has worked so far
# TrainingTimesLastYear:	Number of times training was conducted for this employee last year
# YearsAtCompany:	Total number of years spent at the company by the employee
# YearsSinceLastPromotion:	Number of years since last promotion
# YearsWithCurrManager:	Number of years under current manager

# a) Load all the files 

general_data         <- read.csv("general_data.csv", stringsAsFactors = FALSE)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
manager_survey_data  <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)

# Here column names are digits hence we use check.names function

in_time  <- read.csv("in_time.csv", stringsAsFactors = FALSE, check.names = FALSE) 
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE, check.names = FALSE )   

str(general_data)            #4410 obs. of  24 variables
str(employee_survey_data)    #4410 obs. of  4 variables
str(manager_survey_data)     #4410 obs. of  3 variables

# str(in_time)                 #4410 obs. of  262 variables
# str(out_time)                #4410 obs. of  262 variables

# general_data gives all the general details about the employees including their attrition
# employee_survey_data gives the details about the survey taken from all the employees
# manager_survey_data gives the details about the performance of employees, survey taken from their managers
# in_time and out_time containes in and out timestamps of all the employees.

#################################################################################################################
# ------------------------------------- 3. Data Preparation/Cleaning --------------------------------------------

# a) Missing column name 
colnames(in_time)[1]  <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

# b) Check if the headers of the in_time and out_time are identical.
sum(names(in_time) != names(out_time))  # Identical

# c) Look for duplicate values
sapply(list(general_data, manager_survey_data,
            employee_survey_data,in_time,
            out_time), function(x) sum(!duplicated(x$EmployeeID)))
# There are no duplicates

# d) Check for Missing/Blank values
sapply(list(general_data, manager_survey_data,
            employee_survey_data, in_time,
            out_time), function(x) length(which(x == "")))
# There are no Missing values


# e) Look for NA values in three files: general_data, manager_survey_data, employee_survey_data
sapply(list(general_data, 
            manager_survey_data,
            employee_survey_data), function(x) length(which(is.na(x))))
#  NA's in general_data(28) and employee_survey_data(83) 

# Identify which columns have how many NA's
sapply(employee_survey_data, function(x) length(which(is.na(x))))  
sapply(general_data, function(x) length(which(is.na(x))))   

# employee_survey_data - EnvironmentSatisfaction (25), JobSatisfaction(20), WorkLifeBalance(38) 
# general_data - NumCompaniesWorked(19) and TotalWorkingYears(9) 

# 1. Imputing the Categorical variables: 
# NA's are replaced with the mode, i.e the value that appears most time.

# EnvironmentSatisfaction
summary(factor(employee_survey_data$EnvironmentSatisfaction)) #the most frequently occuring value is 3
employee_survey_data[which(is.na(as.numeric(employee_survey_data$EnvironmentSatisfaction))),2] <- 3 

# JobSatisfaction
summary(factor(employee_survey_data$JobSatisfaction)) #the most frequently occuring value is 4
employee_survey_data[which(is.na(as.numeric(employee_survey_data$JobSatisfaction))),3] <- 4 

# WorkLifeBalance
summary(factor(employee_survey_data$WorkLifeBalance)) #the most frequently occuring value is 3
employee_survey_data[which(is.na(as.numeric(employee_survey_data$WorkLifeBalance))),4] <- 3

# Cross-check for NA 
sum(is.na(employee_survey_data))
# None


# 2. Imputing the Continuous variables: 
# NumCompaniesWorked & TotalWorkingYears have NA Values in different rows
which(is.na(general_data$NumCompaniesWorked))
which(is.na(general_data$TotalWorkingYears))

# So, let's apply the following logic:

# #If NumCompaniesWorked=0 or 1, then TotalWorkingYears = YearsAtCompany
general_data$TotalWorkingYears[(which(is.na(general_data$TotalWorkingYears)))] <- ifelse(general_data$NumCompaniesWorked[(which(is.na(general_data$TotalWorkingYears)))]==0,general_data$YearsAtCompany[(which(is.na(general_data$TotalWorkingYears)))], 
                                                                                 ifelse(general_data$NumCompaniesWorked[(which(is.na(general_data$TotalWorkingYears)))]==1,general_data$YearsAtCompany[(which(is.na(general_data$TotalWorkingYears)))],NA))
#If the difference between TotalWorkingYears and YearsAtCompany is 0, 
#then current company is the employee's first company. So NumCompaniesWorked =1.
#If the difference between TotalWorkingYears and YearsAtCompany is 1, 
#then we can assume that current company is employee's second company. NumCompaniesWorked =2

#For NumCompaniesWorked=0, If the difference between TotalWorkingYears and YearsAtCompany is 0,
#impute with 1 assuming that the current company is the first company
#For NumCompaniesWorked=0, If the difference between TotalWorkingYears and YearsAtCompany is 1,
#impute with 2 assuming that the current company is the second company

general_data$NumCompaniesWorked[(which(is.na(general_data$NumCompaniesWorked)))] <- ifelse((general_data$TotalWorkingYears[(which(is.na(general_data$NumCompaniesWorked)))]-general_data$YearsAtCompany[(which(is.na(general_data$NumCompaniesWorked)))])==0,1,
                                                                                   ifelse((general_data$TotalWorkingYears[(which(is.na(general_data$NumCompaniesWorked)))]-general_data$YearsAtCompany[(which(is.na(general_data$NumCompaniesWorked)))])==1,2,NA))

general_data$NumCompaniesWorked[(which(general_data$NumCompaniesWorked==0))] <- ifelse((general_data$TotalWorkingYears[(which(general_data$NumCompaniesWorked==0))]-general_data$YearsAtCompany[(which(general_data$NumCompaniesWorked==0))])==0,1,
                                                                               ifelse((general_data$TotalWorkingYears[(which(general_data$NumCompaniesWorked==0))]-general_data$YearsAtCompany[(which(general_data$NumCompaniesWorked==0))])==1,2,0))

colSums(is.na(general_data))
#9 NA values for NumCompaniesWorked and 5 NA for TotalWorkingYears 


# Remaining NA's can be replaced with the median values, 

# NumCompaniesWorked
summary(general_data$NumCompaniesWorked) #median is 2, lets replace the missing values with 2
general_data[which(is.na(general_data$NumCompaniesWorked)),15] <- 2  #replacing missing values with 2

# TotalWorkingYears
summary(general_data$TotalWorkingYears) #median value is 10
general_data[which(is.na(general_data$TotalWorkingYears)),20] <- 10

# Cross-check for NA 
sum(is.na(general_data))            
# None


# f) Convert time data into Average Working Hours for each employee

work_hours <- in_time

for(i in 2:length(in_time))
{
  work_hours[,i] <- as.numeric(difftime(strptime(out_time[,i], "%Y-%m-%d %H:%M"),strptime(in_time[,i], "%Y-%m-%d %H:%M")))
}

# Create a new dataframe for Average work hour record of each employee
working_time <- cbind(work_hours[,1],data.frame(rowMeans(work_hours[,-1], na.rm = TRUE, dims = 1)))

# Define column names for the new dataframe
names(working_time)[1] <- "EmployeeID"
names(working_time)[2] <- "AvgWorkHours"

View(working_time)

# Check for NA
sum(is.na(working_time))



#################################################################################################################################
# ---------------------------------------- 4. Derived Metrics --------------------------------------------

# a) Collate these four files together in one single file

setdiff(general_data$EmployeeID, working_time$EmployeeID)         # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID, employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID, manager_survey_data$EmployeeID)  # Identical EmployeeID across these datasets
# since setdiff is 0, we conclude all data is for same employees only.


merge_1  <- merge(general_data, employee_survey_data, by = "EmployeeID", all = F)
merge_2  <- merge(merge_1, manager_survey_data, by = "EmployeeID", all = F)
emp_data <- merge(merge_2, working_time, by = "EmployeeID", all = F)

View(emp_data)  # Master file

# Check for NA in the Master file
sum(is.na(emp_data))

str(emp_data)    # 4410 obs. of 30 variables

# The variable of interest is attrition
summary(factor(emp_data$Attrition))  # 711 people left the company last year


# b) Remove unnecessary columns

#Names of columns having only 1 unique value.
names(which(sapply(emp_data, function(x) {length(unique(x[!is.na(x)])) == 1})) == T)

# These columns are only having 1 value and therefore cannot be an important factor. 
table(emp_data$Over18);
emp_data$Over18 <- NULL;

table(emp_data$EmployeeCount);
emp_data$EmployeeCount <- NULL;

table(emp_data$StandardHours);
emp_data$StandardHours <- NULL;

# Let's assign 1 for employees who worked more than 8 hours(Average) and 0 for others
emp_data$Overtime <- ifelse(emp_data$AvgWorkHours>8,1,0)

#Remove AvgWorkHours as we have derived Overtime variable from it 
emp_data$AvgWorkHours <- NULL;

#Removing EmployeeID since it will not be useful as files are merged now.
emp_data$EmployeeID <- NULL;

View(emp_data)

# c) Data Quality Checks

# Total Working years less than a Year at Company
sum(emp_data$TotalWorkingYears < emp_data$YearsAtCompany,na.rm = T)

# Monthly Income is Zero
sum(emp_data$MonthlyIncome == 0)


# d) Converting the below variables into it's factors as given in data dictionary.

# Education
emp_data$Education[which(emp_data$Education==1)]<-'Below College'
emp_data$Education[which(emp_data$Education==2)]<-'College'
emp_data$Education[which(emp_data$Education==3)]<-'Bachelor'
emp_data$Education[which(emp_data$Education==4)]<-'Master'
emp_data$Education[which(emp_data$Education==5)]<-'Doctor'
emp_data$Education <- as.factor(emp_data$Education)  

# EnvironmentSatisfaction
emp_data$EnvironmentSatisfaction[which(emp_data$EnvironmentSatisfaction==1)]<-'Low'
emp_data$EnvironmentSatisfaction[which(emp_data$EnvironmentSatisfaction==2)]<-'Medium'
emp_data$EnvironmentSatisfaction[which(emp_data$EnvironmentSatisfaction==3)]<-'High'
emp_data$EnvironmentSatisfaction[which(emp_data$EnvironmentSatisfaction==4)]<-'Very High'
emp_data$EnvironmentSatisfaction <- as.factor(emp_data$EnvironmentSatisfaction)

# JobInvolvement
emp_data$JobInvolvement[which(emp_data$JobInvolvement==1)]<-'Low'
emp_data$JobInvolvement[which(emp_data$JobInvolvement==2)]<-'Medium'
emp_data$JobInvolvement[which(emp_data$JobInvolvement==3)]<-'High'
emp_data$JobInvolvement[which(emp_data$JobInvolvement==4)]<-'Very High'
emp_data$JobInvolvement <- as.factor(emp_data$JobInvolvement)

# JobSatisfaction
emp_data$JobSatisfaction[which(emp_data$JobSatisfaction==1)]<-'Low'
emp_data$JobSatisfaction[which(emp_data$JobSatisfaction==2)]<-'Medium'
emp_data$JobSatisfaction[which(emp_data$JobSatisfaction==3)]<-'High'
emp_data$JobSatisfaction[which(emp_data$JobSatisfaction==4)]<-'Very High'
emp_data$JobSatisfaction <- as.factor(emp_data$JobSatisfaction)

# PerformanceRating
emp_data$PerformanceRating[which(emp_data$PerformanceRating==1)]<-'Low'
emp_data$PerformanceRating[which(emp_data$PerformanceRating==2)]<-'Good'
emp_data$PerformanceRating[which(emp_data$PerformanceRating==3)]<-'Excellent'
emp_data$PerformanceRating[which(emp_data$PerformanceRating==4)]<-'Outstanding'
emp_data$PerformanceRating <- as.factor(emp_data$PerformanceRating)

# WorkLifeBalance
emp_data$WorkLifeBalance[which(emp_data$WorkLifeBalance==1)]<-'Bad'
emp_data$WorkLifeBalance[which(emp_data$WorkLifeBalance==2)]<-'Good'
emp_data$WorkLifeBalance[which(emp_data$WorkLifeBalance==3)]<-'Better'
emp_data$WorkLifeBalance[which(emp_data$WorkLifeBalance==4)]<-'Best'
emp_data$WorkLifeBalance <- as.factor(emp_data$WorkLifeBalance)

# e) Create levels for MonthlyIncome
emp_data$IncomeLevel <- ifelse(
  emp_data$MonthlyIncome <= quantile(emp_data$MonthlyIncome,0.25),
  'Low',
  ifelse(
    emp_data$MonthlyIncome <= quantile(emp_data$MonthlyIncome,0.75),
    'Medium',
    'High'));

emp_data$IncomeLevel <- as.factor(emp_data$IncomeLevel)

# f) Convert the remaining categorical variables into factor
str(emp_data)  # 4410 obs. of 27 variables

cols <- c("Attrition","BusinessTravel","Department", "EducationField", "Gender", "JobRole", "MaritalStatus")
emp_data[cols] <- lapply(emp_data[cols], factor)

str(emp_data)

# g) Derive three new variables

# Tenure per job: It defines time period for which an employee remains in one organisation.

emp_data$TenurePerJob        <- ifelse(emp_data$NumCompaniesWorked!=0, emp_data$TotalWorkingYears/emp_data$NumCompaniesWorked,0)

# Years without Change: We create two variables to see how many years it has been for an employee 
#                       without any sort of change using Promotion and Job Change. 

emp_data$YearsWithoutChange1 <- emp_data$YearsAtCompany - emp_data$YearsSinceLastPromotion
emp_data$YearsWithoutChange2 <- emp_data$TotalWorkingYears - emp_data$YearsSinceLastPromotion

View(emp_data)
#######################################################################################################################

# ------------------------------ 5. Exploratory Data Analysis  --------------------------------------------------------

# a) Barcharts for Categorical Variables

# The Variable of interest is Attrition

ggplot(emp_data,aes(Attrition,fill=Attrition))+geom_bar()
prop.table(table(emp_data$Attrition))      #16 % of Attrition

#-------------------------------------------------------------------------------------------------------------------------------
# Categorical Variables are:

# BusinessTravel, Department, Education, EducationField, Gender, JobLevel, JobRole, MaritalStatus
# EnviornmentSatisfaction, JobInvolvement, JobSatisfaction, PerformanceRating, WorkLifeBalance, IncomeLevel

# Arrange multiple plots on a page using grid.arrange

library(grid)
library(gridExtra)

travelplot <- ggplot(emp_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position="dodge")
deptplot   <- ggplot(emp_data, aes(x=Department,fill=Attrition))+ geom_bar(position="dodge")
eduplot    <- ggplot(emp_data, aes(x=Education,fill=Attrition))+ geom_bar(position="dodge")
genplot    <- ggplot(emp_data, aes(x=Gender,fill=Attrition))+ geom_bar(position="dodge")
grid.arrange(travelplot,deptplot,eduplot,genplot,ncol=2,top="Plot 1")

edufieldplot <- ggplot(emp_data, aes(x=EducationField,fill=Attrition))+ geom_bar(position="dodge")
envplot      <- ggplot(emp_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position="dodge")
jobInvplot   <- ggplot(emp_data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position="dodge")
jobSatplot   <- ggplot(emp_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position="dodge")
grid.arrange(edufieldplot,envplot,jobInvplot,jobSatplot,ncol=2,top="Plot 2")

Incomeplot   <- ggplot(emp_data, aes(x=IncomeLevel,fill=Attrition))+ geom_bar(position="dodge")
maritalplot  <- ggplot(emp_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position="dodge")
performplot  <- ggplot(emp_data, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(position="dodge")
workplot     <- ggplot(emp_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position="dodge")
grid.arrange(Incomeplot,maritalplot,performplot,workplot,ncol=2,top="Plot 3")

jobRoleplot  <- ggplot(emp_data, aes(x=JobRole,fill=Attrition))+ geom_bar(position="dodge")
grid.arrange(jobRoleplot,ncol=1,top="Plot 4") # To see the Job Roles clearly we keep it as a single plot 

tenureplot <- ggplot(emp_data,aes(TenurePerJob))+geom_density()+facet_grid(~Attrition)
changeplot1 <- ggplot(emp_data,aes(YearsWithoutChange1))+geom_density()+facet_grid(~Attrition)
changeplot2 <- ggplot(emp_data,aes(YearsWithoutChange2))+geom_density()+facet_grid(~Attrition)
grid.arrange(tenureplot,changeplot1,changeplot2,ncol=2,top = "Plot 4")


#Conclusions :

#BusinessTravel: Among the Employees who leave, most travel frequently
#Department: There is more attrition in R&D Department and less attrition for HR department.
#Education: Among people attrited employees from HR department are less. It is because of low proportion of HR in the organization
#Gender: Attrition of Male is high, since 61% of employees in our dataset are Male.
#EducationField: Attrition is less in case of Human Resources and Others and more in life sciences
#EnvironmentSatisfaction: People who are least satisfied with the environment tend to leave.
#JobInvolvement: We see that majority of employees who leave are either Very Highly involved or Low Involved in their Jobs.
#JobSatisfaction: We see higher attrition levels in among lower Job Satisfaction levels.Very few Doctors leave the organisation since their total number is quit less
#IncomeLevel: We see higher levels of attrition among the lower segment of monthly income. 
#MaritalStatus: Attrition rate is more in case of Single and less among divorcees.
#PerformanceRating: We see that we have employees of only 3 and 4 ratings. Lesser proportion of 4 raters quit.
#WorkLifeBalance: if considering the proportion of data, attrition is more in level 1 which is as expected.
#JobRole: More attrition in case of Research Scientists and Sales Executives and less in case of HR
#TenurePerJob: Usually, people who have worked with many companies for small periods tend to leave early.
#Overtime: Larger Proportion of Overtime Employees are quitting.

# For the newly derived metrics, YearsWithoutChange and YearsWithoutChange2,
# We can see with low tenure and low years without change tend to leave the organisation.

# -------------------------------------------------------------------------------------------------


# b) Histogram for numeric variables

# Numerical Variables are:

# Age,DistanceFromHome, MonthlyIncome, NumCompaniesWorked, PercentSalaryHike
# StockOtionLevel, TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany,
# YearsSinceLastPromotion, YearsWithCurrentManager


# We see that majority of employees leaving the org are around 30 Years
ggplot(emp_data,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
ggplot(emp_data,aes(Age,fill=Attrition))+ geom_histogram(binwidth = 10)

# Majority of employees who have left the organization are near to the Office
ggplot(emp_data, aes(DistanceFromHome,fill=Attrition))+ geom_histogram(binwidth = 10)

# Higher levels of attrition among the lower segment of monthly income
ggplot(emp_data,aes(MonthlyIncome,fill=Attrition))+geom_density()
ggplot(emp_data, aes(MonthlyIncome,fill=Attrition))+ geom_histogram(binwidth = 10000)

#Employess tend to change the jobs in the beginiing years of the career more.
ggplot(emp_data, aes(NumCompaniesWorked,fill=Attrition))+ geom_histogram(binwidth = 5)

# We see that people with 15% hike have more chances to leave
ggplot(emp_data, aes(x=PercentSalaryHike,fill=Attrition))+geom_histogram(binwidth=5)

# Larger proportions of levels 0 & 1 quit
ggplot(emp_data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()

# people with less than 10 years of experience tend to leave company more.
ggplot(emp_data,aes(x=TotalWorkingYears,fill=Attrition)) + geom_histogram(binwidth=10)

# People who have been trained 2-4 times have more attrition rate
ggplot(emp_data, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar()
ggplot(emp_data, aes(TrainingTimesLastYear,fill=Attrition))+ geom_histogram(binwidth = 1)

# Harger proportion of new comers are quitting the organization. 
ggplot(emp_data, aes(YearsAtCompany,fill=Attrition))+ geom_histogram(binwidth = 10)

# Larger proportion of people who have been promoted recently have quit the organization.
ggplot(emp_data, aes(YearsSinceLastPromotion,fill=Attrition))+ geom_histogram(binwidth = 5)

# As expected a new Manager is a big cause for quitting
ggplot(emp_data, aes(YearsWithCurrManager,fill=Attrition))+ geom_histogram(binwidth = 10)

# Larger Proportion of Overtime Employees are quitting
ggplot(emp_data, aes(Overtime,fill=Attrition))+ geom_histogram(binwidth = 10)



################################# OUTLIER IDENTIFICATION AND REPLACEMENT ######################################

# c) Boxplot for outlier detection

# 1) No Outliers
boxplot(emp_data$Age)
boxplot(emp_data$DistanceFromHome)
boxplot(emp_data$PercentSalaryHike)

# 2) Variables with Outliers

# For MonthlyIncome
boxplot(emp_data$MonthlyIncome)            
quantile(emp_data$MonthlyIncome,seq(0,1,0.01))
emp_data$MonthlyIncome[which(emp_data$MonthlyIncome>152020)] <- 152020

# For NumCompaniesWorked
boxplot(emp_data$NumCompaniesWorked)       
quantile(emp_data$NumCompaniesWorked,seq(0,1,0.01))
emp_data$NumCompaniesWorked[which(emp_data$NumCompaniesWorked>8)] <- 8

# For StockOptionLevel
boxplot(emp_data$StockOptionLevel)         
quantile(emp_data$StockOptionLevel,seq(0,1,0.01))
emp_data$StockOptionLevel[which(emp_data$StockOptionLevel>2)] <- 2

# For TotalWorkingYears
boxplot(emp_data$TotalWorkingYears)        
quantile(emp_data$TotalWorkingYears,seq(0,1,0.01))
emp_data$TotalWorkingYears[which(emp_data$TotalWorkingYears>26)] <- 26

# For TrainingTimesLastYear
boxplot(emp_data$TrainingTimesLastYear)    
quantile(emp_data$TrainingTimesLastYear,seq(0,1,0.01))
emp_data$TrainingTimesLastYear[which(emp_data$TrainingTimesLastYear>4)] <- 4
emp_data$TrainingTimesLastYear[which(emp_data$TrainingTimesLastYear<1)] <- 1

# For YearsAtCompany
boxplot(emp_data$YearsAtCompany)           
quantile(emp_data$YearsAtCompany,seq(0,1,0.01))
emp_data$YearsAtCompany[which(emp_data$YearsAtCompany>17)] <- 17

# For YearsSinceLastPromotion
boxplot(emp_data$YearsSinceLastPromotion)  
quantile(emp_data$YearsSinceLastPromotion,seq(0,1,0.01))
emp_data$YearsSinceLastPromotion[which(emp_data$YearsSinceLastPromotion>7)] <- 7

# For YearsWithCurrManager
boxplot(emp_data$YearsWithCurrManager)     
quantile(emp_data$YearsWithCurrManager,seq(0,1,0.01))
emp_data$YearsWithCurrManager[which(emp_data$YearsWithCurrManager>14)] <- 14

# For TenurePerJob
boxplot(emp_data$TenurePerJob)
quantile(emp_data$TenurePerJob,seq(0,1,0.01))
emp_data$TenurePerJob[which(emp_data$TenurePerJob>16)] <- 16

# For YearsWithoutChange1
boxplot(emp_data$YearsWithoutChange1)
quantile(emp_data$YearsWithoutChange1,seq(0,1,0.01))
emp_data$YearsWithoutChange1[which(emp_data$YearsWithoutChange1>12)] <- 12

#For YearsWithoutChange2
boxplot(emp_data$YearsWithoutChange2)
quantile(emp_data$YearsWithoutChange2,seq(0,1,0.01))
emp_data$YearsWithoutChange2[which(emp_data$YearsWithoutChange2>21)] <- 21


# d) Correlation between Numerical Variables

Final_correlation <- cor(emp_data[,c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","StockOptionLevel","TotalWorkingYears",
                                     "TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","Overtime", "TenurePerJob",
                                     "YearsWithoutChange1","YearsWithoutChange2","JobLevel")])
View(round(Final_correlation,2))

# Let's plot this correlation
corrplot(Final_correlation, method = "circle", 
         type = "lower", title = "Correlation of numerical variables")


#################################################################################################################

# ------------------------------------ 6. Model Building  --------------------------------------------------------

# a) Scale all the Numerical Variables(Normalising continuous features)

emp_data$Age                      <- scale(emp_data$Age)
emp_data$DistanceFromHome         <- scale(emp_data$DistanceFromHome)
emp_data$MonthlyIncome            <- scale(emp_data$MonthlyIncome)
emp_data$NumCompaniesWorked       <- scale(emp_data$NumCompaniesWorked)
emp_data$PercentSalaryHike        <- scale(emp_data$PercentSalaryHike)
emp_data$TotalWorkingYears        <- scale(emp_data$TotalWorkingYears)
emp_data$TrainingTimesLastYear    <- scale(emp_data$TrainingTimesLastYear)
emp_data$YearsAtCompany           <- scale(emp_data$YearsAtCompany)
emp_data$YearsSinceLastPromotion  <- scale(emp_data$YearsSinceLastPromotion)
emp_data$YearsWithCurrManager     <- scale(emp_data$YearsWithCurrManager)
emp_data$TenurePerJob             <- scale(emp_data$TenurePerJob)
emp_data$YearsWithoutChange1      <- scale(emp_data$YearsWithoutChange1)
emp_data$YearsWithoutChange2      <- scale(emp_data$YearsWithoutChange2)


# b) Create Dummy Variables

# Let us see the structure of all the categorical variables

str(emp_data)

# Convert target variable Attrition from No/Yes character to factorwith levels 0/1 
levels(emp_data$Attrition)<-c(0,1);
emp_data$Attrition <- as.numeric(levels(emp_data$Attrition))[emp_data$Attrition];
summary(factor(emp_data$Attrition))

# Variables with 2 levels:

# 0 for Male and 1 for Female
levels(emp_data$Gender)<-c(1,0);
names(emp_data)[grep("Gender", colnames(emp_data))]<-'Gender_Female';
emp_data$Gender_Female <- as.numeric(levels(emp_data$Gender_Female))[emp_data$Gender_Female];
summary(factor(emp_data$Gender))

# 0 for '4' and 1 for '3'
levels(emp_data$PerformanceRating)<-c(1,0);
emp_data$PerformanceRating <- as.numeric(levels(emp_data$PerformanceRating))[emp_data$PerformanceRating];
names(emp_data)[grep("PerformanceRating", colnames(emp_data))]<-'PerformanceRating_3';
summary(factor(emp_data$PerformanceRating))

# With 3 or more levels : StockOptionLevel, JobLevel and Overtime are also taken into categorical variable 

# creating a dataframe of categorical features
emp_dummy <- emp_data[,c(3,4,6,7,9:11,15,21:24,26,27)]

# converting categorical attributes to factor
emp_fact <- data.frame(sapply(emp_dummy, function(x) factor(x)))
str(emp_fact)


# creating dummy variables for factor attributes
dummies<- data.frame(sapply(emp_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =emp_fact))[,-1]))

View(dummies)

#Remove original categorical variables and add these dummy variables
emp_data_1 <- emp_data[,-c(3,4,6,7,9:11,15,21:24,26,27)]
final_emp_data <- cbind(emp_data_1, dummies)

# ------------------------------------------------------------------------------------------------------------------------

# c) MODEL CREATION

# Divide into training and test data set

set.seed(100)

indices = sample.split(final_emp_data$Attrition, SplitRatio = 0.7)

train = final_emp_data[indices,]

test = final_emp_data[!(indices),]

######################################################################################################################
# Logistic Regression: 

# Initial model (containing all the variables)
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Here we are using stepwise varible selection method due to large number of variables
# Stepwise selection
model_2 <- stepAIC(model_1, direction="both")   # 'Mass' package already installed

summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)            # 'car' package already installed


# EducationField.xLife.Sciences  - vif - 15.295806 and high p-value of 0.120830. So removing it.

model_3 <- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsAtCompany + YearsWithCurrManager + 
                 YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xBelow.College + Education.xCollege + 
                 EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xVery.High + Overtime, family = "binomial", 
               data = train);

summary(model_3)   # Negligible change in AIC

# Removing multicollinearity through VIF check
vif(model_3)

# YearsAtCompany has vif of 5.331195 and p-value of 0.000301 *** (low)
#  BusinessTravel.xTravel_Frequently - vif of 4.601 and p-value of 1.10e-10 *** (low)
# BusinessTravel.xTravel_Rarely - vif of 4.560 and p-value of 6.61e-05 ***
#  Department.xSales  - vif of 4.22 and p-value - 8.16e-05 ***


# Selecting YearsAtCompany for removal.

model_4 <- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsWithCurrManager + 
                 YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xBelow.College + Education.xCollege + 
                 EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xVery.High + Overtime, family = "binomial", 
               data = train);

summary(model_4) # AIC increased from 2090.1 to 2100.4

# Removing multicollinearity through VIF check
vif(model_4)


# BusinessTravel.xTravel_Frequently - VIF of 4.62 and p-value of 5.42e-11 ***
# Department.xSales - VIF of 4.25 and p-value of 0.000101 ***
# BusinessTravel.xTravel_Rarely - VIF of 4.58 and p-value of 4.93e-05 ***


# Selecting Department.xSales for removal as it has high p-value comparatively.

model_5 <- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsWithCurrManager + 
                 YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Education.xBelow.College + Education.xCollege + 
                 EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xVery.High + Overtime, family = "binomial", 
               data = train);

summary(model_5) # AIC increased to 2113.1

# Removing multicollinearity through VIF check
vif(model_5)


# BusinessTravel.xTravel_Frequently - vif of 4.641902 and p-value of 7.40e-11 ***
# BusinessTravel.xTravel_Rarely - vif of 4.601015 and p-value of 2.65e-05 ***

# Removing BusinessTravel.xTravel_Rarely

model_6 <- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsWithCurrManager + 
                 YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Education.xBelow.College + Education.xCollege + 
                 EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xVery.High + Overtime, family = "binomial", 
               data = train);

summary(model_6) # AIC increased to 2133

# Removing multicollinearity through VIF check
vif(model_6)

# WorkLifeBalance.xBetter - vif of 3.463989 and p-value of 2.20e-08 ***
# WorkLifeBalance.xGood  - vif of 3.026097 and p-value of 2.25e-05 ***

# Removing WorkLifeBalance.xGood
model_7 <- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsWithCurrManager + 
                 YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Education.xBelow.College + Education.xCollege + 
                 EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + JobInvolvement.xLow + 
                 JobInvolvement.xVery.High + Overtime, family = "binomial", 
               data = train);

summary(model_7) # AIC increased to 2148.3

# Removing multicollinearity through VIF check
vif(model_7)

# All vif are either 1 or 2. So checking for p-values alone.

# WorkLifeBalance.xBest - p-value - 0.54382. Removing it.
model_8 <- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsWithCurrManager + 
                 YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Education.xBelow.College + Education.xCollege + 
                 EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter + JobInvolvement.xLow + 
                 JobInvolvement.xVery.High + Overtime, family = "binomial", 
               data = train);

summary(model_8) # Slight decrease in AIC
vif(model_8)

# JobRole.xResearch.Scientist  - p-value - 0.39. Removing this.

model_9 <- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsWithCurrManager + 
                 YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Education.xBelow.College + Education.xCollege + 
                 EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter + JobInvolvement.xLow + 
                 JobInvolvement.xVery.High + Overtime, family = "binomial", 
               data = train);

summary(model_9) # Slight decrease in AIC

# EducationField.xMedical - p-value - 0.35

model_10 <- glm(formula = Attrition ~ Age + DistanceFromHome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsWithCurrManager + 
                 YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Education.xBelow.College + Education.xCollege + 
                 EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter + JobInvolvement.xLow + 
                 JobInvolvement.xVery.High + Overtime, family = "binomial", 
               data = train);

summary(model_10) # Slight decrease in AIC

# DistanceFromHome - p-value of 0.14. Removing this.
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Education.xBelow.College + Education.xCollege + 
                  EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter + JobInvolvement.xLow + 
                  JobInvolvement.xVery.High + Overtime, family = "binomial", 
                data = train);

summary(model_11) # Negligible change in AIC

# JobInvolvement.xVery.High    - p-value of 0.14. Removing this.
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Education.xBelow.College + Education.xCollege + 
                  EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter + JobInvolvement.xLow + 
                  Overtime, family = "binomial", 
                data = train);

summary(model_12) # No change in AIC

# JobInvolvement.xLow  - p-value of 0.16. removing this.
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Education.xBelow.College + Education.xCollege + 
                  EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_13) # Negligible change in AIC

# Education.xBelow.College - p-value of 0.111221. Removing this.
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Education.xCollege + 
                  EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_14) # Negligible increase in AIC

#StockOptionLevel.x1  - p- value of 0.065717 . Removing this.
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Education.xCollege + 
                  EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_15) # AIC increased

# Education.xCollege  - p-value of 0.7. Removing this.
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange1 + YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_16) # AIC increased


# YearsWithoutChange1  - p-value of 0.069356 . Removing this.
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                   YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_17) # AIC increased

# EducationField.xTechnical.Degree - p-value of 0.07. Removing this.
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  EducationField.xMarketing + 
                  EducationField.xOther + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_18) # AIC increased

# Department.xResearch...Development  - p-value of 0.076695 .  Removing this.
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  EducationField.xMarketing + 
                  EducationField.xOther + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_19) # AIC increased

# EducationField.xMarketing - p-value of 0.233161
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  EducationField.xOther + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_20) # Negligible change in AIC

# MaritalStatus.xMarried - p value of 0.049745 * . Removing this.
model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  EducationField.xOther + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_21) # AIC increased

# EducationField.xOther - p-value of 0.036277 * .Removing this.
model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_22) # AIC increased

# JobRole.xManufacturing.Director p-value of 0.023182 * .Removing this.
model_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  JobLevel.x2 + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_23) # AIC increased

# EnvironmentSatisfaction.xVery.High p-value of 0.028442  .Removing this
model_24 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  JobLevel.x2 + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_24) # AIC increased

# JobLevel.x2 .High p-value of 0.014734 *   .Removing this .
model_25 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                   JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_25) # AIC increased

# Age - p-value of  0.004623 **   .Removing this .

model_26 <- glm(formula = Attrition ~  NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_26) # AIC increased

# JobRole.xResearch.Director  p-value of  0.004008 **   .Removing this .
model_27 <- glm(formula = Attrition ~  NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_27) # AIC increased

# JobRole.xSales.Executive  p-value of 0.003252 ** .Removing this .
model_28 <- glm(formula = Attrition ~  NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_28) # AIC increased

# TrainingTimesLastYear p-value of 0.004723 **  .Removing this .
model_29 <- glm(formula = Attrition ~  NumCompaniesWorked + 
                  YearsWithCurrManager + 
                  YearsWithoutChange2 + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter  +
                  Overtime, family = "binomial", 
                data = train);

summary(model_29) # AIC increased
vif(model_29)

# With 10 significant variables in the model

final_model<- model_29

################################################################################################################

# ------------------------------------ 7. Model Evaluation --------------------------------------------------------

# Predicted Probabilities of Attrition for test data
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

# Let's see the summary
summary(test_pred)

test$prob <- test_pred
View(test)



# Let's find out the optimal probability cutoff 

test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)

s = seq(.002514,.85,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# As cutoff increases sensitivity decreases.
# As cutoff increases specificity and Accuracy increases.


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
#Choosing cutoff where these 3 lines meet.

#cutoff = 0.1566
test_pred_attrition <- factor(ifelse(test_pred >= 0.1566, "Yes", "No"))


#Confusion Matrix
conf_final <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec



##################################################################################################
### KS -statistic - Test Data ######

test_pred_attrition <- ifelse(test_pred_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_pred_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

plot(performance_measures_test,col="darkgreen")



ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

# k-s statistic is 0.47 and hence 47%
# Ideal k-s statistic should be  > 40%.


####################################################################

# Lift & Gain table


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred_attrition, groups = 10)
# We are abe to capture 66% of the people who will leave in the 3rd decile.
# At the 3rd decile the lift is 2.21 and hence the model is 2 times better than a random model.



# Gain Chart
plot(x=c(0, 10), y=c(0, 100), col="darkred", type="l", lwd=2, ylab="Cumulative Gain", xlab="Decile")
lines(x=attrition_decile$bucket, y=attrition_decile$Gain, type="b",  col=4, lwd=2)
legend(4,20,col=c("darkred",4),lwd=c(2,2),c("Random Model", "R generated Model"))


#Lift Chart
plot(x=attrition_decile$bucket, y=attrition_decile$Cumlift, type="b",  col=4, lwd=2,  ylab="Lift", xlab="Decile")
lines(x=c(0, 10), y=c(1, 1), col="darkred", type="l", lwd=2)
legend(4.5,2,col=c("darkred",4),lwd=c(2,2),c("Lift-Random Model", "Lift- R generated Model"))

