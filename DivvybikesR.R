# Analysis on Bike-Share datasets

##Skill used: Aggregate Functions, Joins, Converting Data Types, 
## Create visuals with ggplot, Data Cleaning...
  
------------------------------------------------------------------------------------------------------

setwd("C:/Users/MR JOHN/Documents/Divvybikes/cyclistic-csv")

## Install and load packages required for cleaning and analysis

install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("dply")
install.packages("dplyr")
install.packages("plotrix")

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2) 
library(janitor)
library(dplyr)
library(plotrix)
library(readr)

#Load all the 12 months datas and bind them into one dataframe.

rides_data<- list.files(path = "~/Divvybikes/cyclistic-csv", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                           
  bind_rows

write.table(rides_data, file = "rides_data_v2.csv", sep = ",", row.names = FALSE)

## Inspect the data

View(rides_data)
summary(rides_data)
colnames(rides_data)
dim(rides_data)

# Check the unique values in these columns, user_type and the bike_type to avoid error

paste("Unique values in rideable_type dataset:- ",unique(rides_data$member_casual))
paste("Unique values in rideable_type dataset:- ", unique(rides_data$rideable_type))

## Rename some of the columns for better understanding elements in the columns

rides_data<- rides_data%>%
  rename(start_time = started_at,
         end_time = ended_at,
         bike_type = rideable_type,
         user_type = member_casual
         )

# extract the date, month, day, and year of each ride ito a new column
# This will allow us to aggregate ride data for each month, day, or year.#The default format is yyyy-mm-dd

rides_data$date <- as.Date(rides_data$start_time) #Convert start_time to date format
rides_data$month <- format(as.Date(rides_data$date), "%b")
rides_data$day <- format(as.Date(rides_data$date), "%d")
rides_data$year <- format(as.Date(rides_data$date), "%Y")
rides_data$day_of_week <- format(as.Date(rides_data$date), "%A")
rides_data$hour<-format(as.POSIXct(rides_data$start_time),"%H")


colnames(rides_data)
str(rides_data)

#Calculate the time different between the start time and the end time of each trip as ride_length

rides_data<- rides_data%>%
  mutate(ride_length = round(difftime(end_time, start_time, units = "mins"), digits = 1))

# Convert "ride_length" from Factor to numeric so we can run calculations on the data

rides_data$ride_length <- as.numeric(as.character(rides_data$ride_length))

# Remove "bad" data

#i. Any trips that were below 60 seconds in length, they are potentially 
# false starts or users trying to re-dock a bike to ensure it was secure and
#ii. the entries when bikes were taken out of docks and checked for
# quality by Divvy or ride_length was negative.

#Also, remove rows where start_station_name and end_station_name.

# create a new version of the dataframe (v2) since data is being removed

rides_data_v2<- rides_data%>%
  filter( !ride_length <1)%>%
  na.omit(start_station_name, end_station_name)

dim(rides_data_v2)
colnames(rides_data_v2)

# Notice that the days of the week and months are out of order. Let's fix that.

rides_data_v2$day_of_week <- ordered(rides_data_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
rides_data_v2$month <- ordered(rides_data_v2$month,levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#Export the dataset for further analysis.

write.table(rides_data_v2, file = "rides_data_v2.csv", sep = ",", row.names = FALSE)


View(rides_data_v2)
dim(rides_data_v2)

# Extract casual riders dataset.

rides_data_casual<- rides_data_v2%>%
  filter(user_type == "casual")


write(rides_data_casual, file = "rides_data_casual.csv", sep = ",", row.names = FALSE)



# Check the start stations casual riders use the most

most_crowded<- table(rides_data_casual$start_station_name)

View(rides_data_casual)

# calculate the frequency table to determine the most crowded stations in 
# order to know where to major outdoor advertisement e.g digital billboard

most_crowded<- table(rides_data_casual$start_station_name)

View(most_crowded)

write(most_crowded, file = "Crowed_stations.csv", sep = ",", row.names = FALSE)
-----------------------------------------------------------------------------------------------
# In order to see how user type use cyclist differently during different season
# of the year, create a column for the four seasons of the year.

  #Create a new version to include the season column.
  
rides_data_v3<- rides_data_v2%>%
  mutate(season = recode(month, 'Dec' = 'winter', 'Jan' = 'winter', 'Feb' = 'winter', 'Mar' = 'spring',
                         'Apr' = 'spring', 'May' = 'spring', 'Jun' = 'summer', 'Jul' = 'summer', 
                         'Aug' = 'summer','Sep' = 'autumn', 'Oct' = 'autumn', 'Nov' = 'autumn'))
------------------------------------------------------------------------------------------------------------------

#export the dataset for further analysis.
  
write.table(rides_data_v3, file = "rides_data_v3.csv", sep = ",", row.names = FALSE)  
  
## DESCRIPTIVE ANALYSIS

#Compare members and casual users (ride_length in mins)

rides_data_v3%>%
  group_by(user_type)%>%
  summarize(ride_length_min = min(ride_length), 
            ride_length_mean = mean(ride_length),
            ride_length_max = max(ride_length))


# Display the annual members and casual mean ride length for days of the week.


aggregate(rides_data_v3$ride_length ~ rides_data_v3$user_type + rides_data_v3$day_of_week, FUN = mean)


# Analysis

dim(rides_data_v3)


----------------------------------------------------------------------------
# Check the number of rides per user type 
rides_data_v3 %>%
  group_by(user_type)%>%
  summarise(Count_of_ride = length(ride_id))


----------------------------------------------------------------------------------------------------------------------------
#Create a data to show how the number of rides per user type

x<- c(1745576, 2590133)
user_type<- c("casual", "member")

# Plot the chart with title and rainbow color pallet.
piepercent<- paste(round(x/1000000, 2),"M")

pie3D(x,radius=0.9,labels=piepercent,explode=0.1,theta= 0.8, main= "Riders population distribution",
      col=c("skyblue","#7d7d7d"), border = "white")


legend("topleft", c("Casual","Member"), cex = 0.8,
       fill = c("skyblue", "#7d7d7d"))


# Give the chart file a name.
png(file = "Riders_distributionChart.jpg")
--------------------------------------------------------------------------------------------------------------------------------
#lets visualize days of the week ride count for each rider type

rides_data_v3 %>% 
  group_by(user_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(user_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge") + theme(panel.background = element_rect(fill = "white")) + 
  scale_fill_manual(values = c("skyblue","#7d7d7d")) + 
  labs (title = "Days of the week ride count", x = "Days of the week", y = " Number of rides") +
  theme(axis.text.x = element_text(angle = 45))

----------------------------------------------------------------------------------------------------------------
#Let's visualize days of the week average ride duration for each rider type 

rides_data_v3%>%
  group_by(user_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(user_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = user_type)) +
  geom_col(position = "dodge") + theme(panel.background = element_rect(fill = "white")) + 
  scale_fill_manual(values = c("skyblue","#7d7d7d")) + 
  labs (title = "Days of the week average ride duration ", x = "Days of the week", y = "Average ride length ") +
  theme(axis.text.x = element_text(angle = 45))


----------------------------------------------------------------------------------------------------------
#Let's Visualize months of the year ride count for the user types.

rides_data_v3%>%
  group_by(user_type, month, season) %>% 
  summarise(number_of_rides = n(),
            average_rides = mean(number_of_rides))%>% 
  arrange(user_type, month, season)%>% 
  ggplot(aes(x = month, y= number_of_rides, fill = user_type)) +
  geom_col(position = "dodge") + 
  theme(panel.background = element_rect(fill = "white")) + 
  scale_fill_manual(values = c("skyblue","#7d7d7d")) + 
  labs (title = "Months of the year ride count", subtitle = "Dec, 2021 - Nov, 2022", x= "Months of the year", y = "Number of rides ") +
  theme(axis.text.x = element_text(angle = 45)) 
-----------------------------------------------------------------------------------------------------------------------------------------------------

# Let's Visualize the ride count for different seasons of the year.

rides_data_v3%>%
  group_by(user_type, month, season) %>% 
  summarise(number_of_rides = n())%>% 
  arrange(user_type, month, season)%>% 
  ggplot(aes(x = month, y= number_of_rides, fill = user_type)) +
  geom_col(position = "dodge") + theme(panel.background = element_rect(fill = "white")) + 
  facet_wrap(~season) +
  scale_fill_manual(values = c("skyblue","#7d7d7d")) + 
  labs (title = "Seasonal ride count ", subtitle ="Dec, 2021 - Nov, 2022", x = "Months of the year", y = "Number of rides") +
  theme(axis.text.x = element_text(angle = 45)) 
------------------------------------------------------------------------------------------------------------------------------------------
rides_data_v3%>%
  group_by(user_type, month, season) %>% 
  summarise(number_of_rides = n(),
            average_rides = mean(number_of_rides))%>% 
  arrange(user_type, month, season)%>% 
  ggplot(aes(x = month, y= number_of_rides, fill = user_type)) +
  geom_col(position = "dodge") + 
  geom_line(aes(x = month, y = number_of_rides), stat = "identity", size = 1.5, color="brown", group = 2) +
  scale_y_continuous(sec.axis=sec_axis(~.*0.01,name="Percentage")) +
  theme(panel.background = element_rect(fill = "white")) + 
  scale_fill_manual(values = c("skyblue","grey")) + 
  labs (title = "Number of rides  by months of the year") +
  theme(axis.text.x = element_text(angle = 45)) 
------------------------------------------------------------------------------------------------------------------

# Import the images of visuals of most used stations by both riders and only casual riders 
  
![RIDERS](/Users/MR JOHN/Documents/Data Analysis Projects/Divvybikes/cyclistic-csv/riders chart.png){width=50%}
![CASUAL](/Users/MR JOHN/Documents/Data Analysis Projects/Divvybikes/cyclistic-csv/casual chart.png){width=50%}   



 


