#install and load packages

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

#load trip data into data frames

trip_data_2021_01 <- read.csv("202101-divvy-tripdata.csv")
trip_data_2021_02 <- read.csv("202102-divvy-tripdata.csv")
trip_data_2021_03 <- read.csv("202103-divvy-tripdata.csv")
trip_data_2021_04 <- read.csv("202104-divvy-tripdata.csv")
trip_data_2021_05 <- read.csv("202105-divvy-tripdata.csv")
trip_data_2021_06 <- read.csv("202106-divvy-tripdata.csv")
trip_data_2021_07 <- read.csv("202107-divvy-tripdata.csv")
trip_data_2021_08 <- read.csv("202108-divvy-tripdata.csv")
trip_data_2021_09 <- read.csv("202109-divvy-tripdata.csv")
trip_data_2021_10 <- read.csv("202110-divvy-tripdata.csv")
trip_data_2021_11 <- read.csv("202111-divvy-tripdata.csv")
trip_data_2021_12 <- read.csv("202112-divvy-tripdata.csv")

#verify column names are the same for all the data frames

colnames(trip_data_2021_01) == colnames(trip_data_2021_02)
colnames(trip_data_2021_02) == colnames(trip_data_2021_03)
colnames(trip_data_2021_03) == colnames(trip_data_2021_04)
colnames(trip_data_2021_04) == colnames(trip_data_2021_05)
colnames(trip_data_2021_05) == colnames(trip_data_2021_06)
colnames(trip_data_2021_06) == colnames(trip_data_2021_07)
colnames(trip_data_2021_07) == colnames(trip_data_2021_08)
colnames(trip_data_2021_08) == colnames(trip_data_2021_09)
colnames(trip_data_2021_09) == colnames(trip_data_2021_10)
colnames(trip_data_2021_10) == colnames(trip_data_2021_11)
colnames(trip_data_2021_11) == colnames(trip_data_2021_12)

#combine data frames into one

trips_2021<- bind_rows(
  trip_data_2021_01, 
  trip_data_2021_02, 
  trip_data_2021_03, 
  trip_data_2021_04, 
  trip_data_2021_05,
  trip_data_2021_06,
  trip_data_2021_07,
  trip_data_2021_08,
  trip_data_2021_09,
  trip_data_2021_10,
  trip_data_2021_11,
  trip_data_2021_12)

#verify column names

colnames(trips_2021) 

#add ride length column

trips_2021 <- mutate(trips_2021, ride_length = difftime(ended_at, started_at, units = "secs"))

##clean data

#drop unneeded columns and verify

trips_2021 <- subset(trips_2021, select = -c(start_lat:end_lng))

#verify trimmed data frame

colnames(trips_2021)

#remove zero trip length

trips_2021 <- subset(trips_2021, ride_length > 0)

## ANALYSIS

#add aggregation columns

trips_2021$day_of_week <- wday(trips_2021$started_at, label=TRUE, abbr=FALSE)
trips_2021$month <- month(trips_2021$started_at, label=TRUE, abbr=FALSE)
trips_2021$hour <- hour(trips_2021$started_at)

#summarize weekly data into one data frame

trips_by_day <- trips_2021 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(mean = mean(ride_length), count = n())

#summarize monthly data into one data frame

trips_by_month <- trips_2021 %>% 
  group_by(member_casual, month) %>% 
  summarize(mean = mean(ride_length), count = n())

#summarize hourly data into one data frame

trips_by_hour <- trips_2021 %>% 
  group_by(member_casual, hour) %>% 
  summarize(mean = mean(ride_length), count = n())

## VISUALIZE

#remove scientific notation from plots

options(scipen = 999)

#plot trips by day of the week 

trips_by_day %>% 
  ggplot(aes(x = day_of_week, y = count, fill = member_casual)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d(option = "mako", end = .8, begin = 0.15, direction = -1) +
  labs(x = "Day of the Week", y = "Number of Rides", title = "Number of Rides vs Day of the Week", subtitle = "2021 Cyclistic bike ride data", fill = "Membership Status")

#vs ride length

trips_by_day %>% 
  ggplot(aes(x = day_of_week, y = mean / 60, fill = member_casual)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous() +
  scale_fill_viridis_d(option = "mako", end = .8, begin = 0.15, direction = -1) +
  labs(x = "Day of the Week", y = "Average Ride Length (min)", title = "Average Ride Length vs Day of the Week", subtitle = "2021 Cyclistic bike ride data", fill = "Membership Status")


#plot trips by month

#vs number of rides

trips_by_month %>% 
  ggplot(aes(x = month, y = count, fill = member_casual)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_viridis_d(option = "mako", end = .8, begin = 0.15, direction = -1) +
  labs(x = "Month", y = "Number of Rides", title = "Number of Rides vs Month", subtitle = "2021 Cyclistic bike ride data", fill = "Membership Status")

#vs ride length

trips_by_month %>% 
  ggplot(aes(x = month, y = mean / 60, fill = member_casual)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_y_continuous() +
  scale_fill_viridis_d(option = "mako", end = .8, begin = 0.15, direction = -1) +
  labs(x = "Month", y = "Average Ride Length (min)", title = "Average Ride Length vs Month", subtitle = "2021 Cyclistic bike ride data", fill = "Membership Status")

#plot trips by hour

#vs number of rides

trips_by_hour %>% 
  ggplot(aes(x = hour, y = count, fill = member_casual)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_continuous(breaks = c(seq(0,23,by=1))) +
  scale_fill_viridis_d(option = "mako", end = .8, begin = 0.15, direction = -1) +
  labs(x = "Hour", y = "Number of Rides", title = "Number of Rides vs Hour of the Day", subtitle = "2021 Cyclistic bike ride data", fill = "Membership Status")

#vs ride length

trips_by_hour %>% 
  ggplot(aes(x = hour, y = mean / 60, fill = member_casual)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_continuous(breaks = c(seq(0,23,by=1))) +
  scale_y_continuous() +
  scale_fill_viridis_d(option = "mako", end = .8, begin = 0.15, direction = -1) +
  labs(x = "Hour", y = "Average Ride Length (min)", title = "Average Ride Length vs Hour of the Day", subtitle = "2021 Cyclistic bike ride data", fill = "Membership Status")

