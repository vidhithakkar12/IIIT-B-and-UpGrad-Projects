
# Set working directory

setwd("")
getwd()

# Read the CSV file

Uber <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)
View(Uber)
str(Uber)       # Structure of the file


# Data Cleaning and Preparation----------------------------------------------

# Changing the two types of Date-Time formats into a single type

# Installing necessary package to replace the strings in Date

install.packages("stringr")
library(stringr)

# Function to replace all "/" in dates to "-"

Uber$Request.timestamp<- str_replace_all(Uber$Request.timestamp, "/", "-")
Uber$Drop.timestamp<- str_replace_all(Uber$Drop.timestamp, "/", "-")

# Function to format date & time in standard form

Uber$Request.timestamp <- as.POSIXct(Uber$Request.timestamp, format = "%d-%m-%Y %H:%M")
Uber$Drop.timestamp <- as.POSIXct(Uber$Drop.timestamp, format = "%d-%m-%Y %H:%M")

View(Uber)

# Deriving a column of requested hour for further analysis to be done based on time-slots

Uber$Requested_hour <- format(Uber$Request.timestamp,"%H")

View(Uber)

#Results-----------------------------------------------------------------

# 1. Visually identifying the most pressing problems for Uber using bar charts

# Installing ggplot2 package for the same

install.packages("ggplot2")
library(ggplot2)

# Plot to visualize the frequency of requests that gets 'Completed' or 'Cancelled' or shows 'no cars available'

ggplot(Uber, aes(x = Status, fill = Pickup.point)) + geom_bar(position = "dodge")

# Identifying the different time slots and deriving a coloumn for the same

Uber$Time_Slot <- ifelse((Uber$Requested_hour>=0 & Uber$Requested_hour<11),"Morning",
                                ifelse((Uber$Requested_hour>=11 & Uber$Requested_hour<19),"Afternoon","Night"))
View(Uber)

# Plot to visualize the frequency of requests in different time slots

ggplot(Uber, aes (x=Time_Slot, fill = Pickup.point)) + geom_bar(position = "dodge")

# Alternatively, the same problem can be derived using the frequency of requests of pickup points.
# We take two dataframes, one where pickup point is Airport and other is City

Airport_to_City <- subset(Uber, Pickup.point == "Airport")
View(Airport_to_City)
City_to_Airport <- subset(Uber, Pickup.point == "City")
View(City_to_Airport)

# Installing package to use a function grid.arrange to show side-by-side plots

install.packages("gridExtra") 
library(gridExtra)

AC <- ggplot(Airport_to_City, aes (x=Time_Slot, fill = Status)) + geom_bar(position = "dodge")
CA <- ggplot(City_to_Airport, aes (x=Time_Slot, fill = Status)) + geom_bar(position = "dodge")
grid.arrange(AC,CA,nrow=1,ncol=2)


# 2. Creating plots of the gap between the Supply and the demand 

# Supply means the trips that were completed

Supply <- subset(Uber, Uber$Status == "Trip Completed" ) 
View(Supply)

# Demand means all the trips that were requested (Complted+Cancelled+No Cars)

Demand <- Uber
View(Demand)

# Supply-Demand Plots based on Time-Slots

Supply_time_plot <- ggplot(Supply, aes (x=Time_Slot, fill = Pickup.point)) + geom_bar(position = "fill")
Demand_time_plot <- ggplot(Demand, aes (x=Time_Slot, fill = Pickup.point)) + geom_bar(position = "fill")
grid.arrange(Supply_time_plot, Demand_time_plot, nrow = 1, ncol = 2)


# Supply-Demand Plots based on Pickup points

Supply_status_plot <- ggplot(Supply, aes (x=Pickup.point, fill = Time_Slot)) + geom_bar(position = "fill")
Demand_status_plot <- ggplot(Demand, aes (x=Pickup.point, fill = Time_Slot)) + geom_bar(position = "fill")
grid.arrange(Supply_status_plot, Demand_status_plot, nrow = 1, ncol = 2)

# Calculating Idle time is not a feasible option here,
# As the data of other rides by the same driver is not given.
