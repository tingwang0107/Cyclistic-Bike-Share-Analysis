#############################Preparing the data
#Install packages
install.packages("tidyverse")
install.packages("janitor")
install.packages("ggplot2")
install.packages("ggrepel")

library(tidyverse) 
library(lubridate)  #For date functions
library(janitor)  #For examining and cleaning dirty data
library(ggplot2)  #For visualizations
library(ggrepel)  #Automatically position non-overlapping text labels with ggplot2

##Import data and rename them with simple names
sep21 <- read_csv("202109-divvy-tripdata.csv")
oct21 <- read_csv("202110-divvy-tripdata.csv")
nov21 <- read_csv("202111-divvy-tripdata.csv")
dec21 <- read_csv("202112-divvy-tripdata.csv")
jan22 <- read_csv("202201-divvy-tripdata.csv")
feb22 <- read_csv("202202-divvy-tripdata.csv")
mar22 <- read_csv("202203-divvy-tripdata.csv")
apr22 <- read_csv("202204-divvy-tripdata.csv")
may22 <- read_csv("202205-divvy-tripdata.csv")
jun22 <- read_csv("202206-divvy-tripdata.csv")
jul22 <- read_csv("202207-divvy-tripdata.csv")
aug22 <- read_csv("202208-divvy-tripdata.csv")

#Check the column names and data format of each column
str(sep21)

#Compare this to other files to check if we can integrate them or not
compare_df_cols(sep21, oct21, nov21, dec21, jan22, feb22, 
                mar22, apr22, may22, jun22, jul22, aug22)

#Combine tables into a single file bind_rows() from dplyr package
trips_data <- bind_rows(sep21, oct21, nov21, dec21, jan22, feb22, 
                        mar22, apr22, may22, jun22, jul22, aug22)




#############################Process
#Inspect the data frame
str(trips_data) #Check data structure
colnames(trips_data)  #List of column names
dim(trips_data)  #Dimensions
head(trips_data)  #View first 6 rows
tail(trips_data)  #View last 6 rows
table(trips_data$member_casual)  #Total number of causal riders and members

#Rename the column member_casual to customer type
trips_data <- trips_data %>%
  rename(customer_type = member_casual)
glimpse(trips_data)
summary(trips_data)  #Pending, it will have all min, median, mean and max for all columns

#Add columns that list the date, month, day, year, and day of the week.
trips_data$date <- as.Date(trips_data$started_at)  #yyy-mm-dd
trips_data$month <- format(as.Date(trips_data$date), "%b")  #Abbreviated month
trips_data$day <- format(as.Date(trips_data$date), "%d")  #Day of the month
trips_data$year <- format(as.Date(trips_data$date), "%Y")  #Year with century
trips_data$day_of_week <- format(as.Date(trips_data$date), "%a")  #Full weekday

trips_data$starttime_hm <- format(trips_data$started_at, format = "%H:%M")  #Hour:Minute
trips_data$starttime <- as.POSIXct(trips_data$starttime_hm, format = "%H:%M")  #Change type to date time

#Calculate the ride duration in seconds ==(ended_at - started_at) using difftime()
trips_data$ride_length <- as.numeric(
  difftime(trips_data$ended_at, trips_data$started_at, units = "secs"))
glimpse(trips_data) #Inspect the structure of columns


#Clean data
#Remove ride lengths < 0
print(paste("Minimum of ride duration is", min(trips_data$ride_length)))
trips_data_v1 <- trips_data %>%
  filter(!(ride_length < 0))
print(paste("Removed", nrow(trips_data) - nrow(trips_data_v1), "rows with negative ride length."))

#Check for Duplicates
#None Duplicates
trips_data_noduplicate <- 
  trips_data_v1[!duplicated(trips_data_v1$ride_id),]
print(paste("Removed", nrow(trips_data_v1) - nrow(trips_data_noduplicate), "duplicated rows."))

#Remove incomplete rows
trips_data_cleaned <- trips_data_v1 %>%
  filter(
    !(is.na(start_station_name) | start_station_name == "")) %>%
  filter(
    !(is.na(end_station_name) | end_station_name == ""))
print(paste("Removed", nrow(trips_data_v1) - nrow(trips_data_cleaned), 
            "rows for negative ride duration and incomplete rides."))

#To Test fir TEST rides
#grepl() function will is used to return the value TRUE if the specified string pattern is found in the vector
#and FALSE if it is not found.
#None need to remove
test_data <- trips_data_cleaned %>%
  filter(grepl("TEST | Test", start_station_name))
test_data$start_station_name



#############################Analyze
##Summarize the cleaned data
summary(trips_data_cleaned$ride_length)

#Create function which calculates mode
find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate((match(x, ux))))]
}
#Create a data frame which summarize the trips_data_cleaned dataset by important variables
summary_data <- trips_data_cleaned %>%
  group_by(customer_type) %>%
  summarize(min_ride_length = min(ride_length),
            median_ride_length = median(ride_length),
            mean_ride_length = round(mean(ride_length), digits = 0),
            max_ride_length = max(ride_length),
            mode_day_of_week = find_mode(day_of_week),
            mode_month = find_mode(month),
            ) %>%
  ungroup()
summary_data

#Most popular start stations
start_station <- trips_data_cleaned %>%
  select(start_station_name, start_lat, start_lng) %>%
  group_by(start_station_name) %>%
  mutate(num_trips = n()) %>%
  distinct(start_station_name, .keep_all = TRUE) %>%
  arrange(desc(num_trips)) %>%
  ungroup()
head(start_station, 5)
#Export to Tableau for viz
write.csv(start_station, "start_station.csv")

#Most popular end stations
end_station <- trips_data_cleaned %>%
  select(end_station_name, end_lat, end_lng) %>%
  group_by(end_station_name) %>%
  mutate(num_trips = n()) %>%
  distinct(end_station_name, .keep_all = TRUE) %>%
  arrange(desc(num_trips)) %>%
  ungroup()
head(end_station, 5)
#Export to Tableau for viz
write.csv(end_station, "end_station.csv")


#############################Visualization
##Viz 1: Per Each Customer type
#Total number of rides per each customer 
total_ride <- 
  trips_data_cleaned %>% 
  group_by(customer_type) %>%
  summarize(num_rides = n()) %>%
  ungroup()

ggplot(data = total_ride,
       aes(x = customer_type, 
           y = num_rides, 
           fill = customer_type)) +
  geom_col() +
  geom_text(aes(label = num_rides), 
            vjust =-0.5) +  #vertical adjustment 
  labs (title = "Viz 1: Total Rides vs Customer Type",
        x = "Customer Types", y = "Number of Rides",
        fill = "Customer Types")
ggsave("Viz 1 Total Rides vs Customer Type.png")

##Another way to plot, no label
ggplot(data = trips_data_cleaned) +
  geom_bar(mapping=aes(x = customer_type, 
                       fill = customer_type)) +
  labs (title = "Viz 1: Total Rides vs Customer Type",
        x = "Customer Types", y = "Number of Rides",
        fill = "Customer Types")
ggsave("Viz 1 Total Rides vs Customer Type.png")

##Viz 2:Average ride length v.s. customer type
#Average ride time for members vs casual users
avg_ride <- 
  trips_data_cleaned %>% 
  group_by(customer_type) %>%
  summarize(avg_duration = round(mean(ride_length), digits =0)) %>%
  ungroup()
ggplot(data = avg_ride,
       aes(x = customer_type, 
           y = avg_duration, 
           fill = customer_type)) +
  geom_col() +
  geom_text(aes(label = avg_duration), 
            vjust =-0.5) +
  labs (title = "Viz 2: Average Ride Duration vs Customer Type",
        x = "Customer Types", y = "Average Ride Duration (seconds)",
        fill = "Customer Types")
ggsave("Viz 2 Average Ride Duration vs Customer Type.png")


##Each Month
#arrange month in order
trips_data_cleaned$month <- 
  ordered(trips_data_cleaned$month, 
          levels=c("Sep", "Oct", "Nov", "Dec",
                   "Jan", "Feb", "Mar", "Apr", 
                   "May", "Jun", "Jul", "Aug"))
#create a smaller data frame
demand_month <- trips_data_cleaned %>%
  group_by(customer_type, month) %>%
  summarise(num_trips = n(),
            avg_duration = round(mean(ride_length), digits = 0),
            .groups = 'drop') %>%
  ungroup()

#Viz 3: Total ride number each month
#get the max ride number per customer (didn't used)
demand_month <- demand_month %>%
  group_by(customer_type) %>%
  mutate(my_label = ifelse(num_rides == max(num_rides),
                           paste(num_rides), 
                           "")
  ) %>%
  ungroup()
#plotting
demand_month %>%
  ggplot(aes(x= month, 
             y = num_trips, 
             fill = customer_type)) +
  geom_col(width = 0.5, 
           position = "dodge") +  #Placing bars side by side
  labs (title = "Viz 3: Total Rides by Customer each Month", 
        x = "Month", y = "Total Number of Rides",
        fill = "Customer Types")
ggsave("Viz 3 Total trips by Customers per Month.png")

#Viz 4: avg ride Duration each month
#Get the max avg length per customer (didn't used)
demand_month <- demand_month %>%
  group_by(customer_type) %>%
  mutate(my_label_2 = ifelse(avg_duration == max(avg_duration),
                             paste(avg_duration), 
                             "")
  ) %>%
  ungroup()
#Plotting
demand_month %>%
  ggplot(aes(x= month, 
             y = avg_duration, 
             fill = customer_type)) +
  geom_col(width = 0.5, position = "dodge") +
  labs (title = "Viz 4: Average Ride Duration by Customer each Month", 
        x = "Month", y = "Average Ride Duration (seconds)",
        fill = "Customer Types")
ggsave("Viz 4 Average Ride Duration by Customer each Month.png")


##In day of week
#arrange day of month in order
trips_data_cleaned$day_of_week <- 
  ordered(trips_data_cleaned$day_of_week, 
          levels=c("Mon", "Tue", "Wed", "Thu", 
                   "Fri", "Sat", "Sun"))

#Analyze ridership data by type and weekday
trips_data_week <- 
  trips_data_cleaned %>% 
  group_by(customer_type, day_of_week) %>%
  summarize(num_rides = n(),
            avg_duration = round(mean(ride_length), digits = 0),
            .groups = 'drop') %>%
  arrange(customer_type, day_of_week) %>%         #Sorting
  ungroup()



  
#Export the data
##Also Used this file for other two viz in Tableau
write.csv(trips_data_week, "ride data by type and day of week.csv")

##Viz 5: Total trips by customers v.s. Day of week
#Get the max ride number per customer
trips_data_week <- 
  trips_data_week %>%
  group_by(customer_type) %>%
  mutate(my_label = ifelse(num_rides == max(num_rides),
                           paste(num_rides), 
                           "")
  ) %>%
  ungroup()
trips_data_week %>%
  ggplot(aes(x= day_of_week, 
             y = num_rides, 
             fill = customer_type)) +
  geom_text(aes(label = my_label), 
            vjust = -0.5) +
  geom_col(width = 0.5, position = "dodge") +
  labs (title = "Viz 5: Total Rides by Customer v.s. Day of Week", 
        x = "Customer Types", y = "Total Number of Rides",
        fill = "Customer Types") 
ggsave("Viz 5 Total trips by Customers vs Day of Week.png")

##Viz 6: Average duration by Customer v.s. Day of week
#Get the max ride length per customer
trips_data_week <- 
  trips_data_week %>%
  group_by(customer_type) %>%
  mutate(my_label_2 = ifelse(avg_duration == max(avg_duration),
                             paste(avg_duration), 
                             "")
  ) %>%
  ungroup()
trips_data_week %>%
  ggplot(aes(x= day_of_week, 
             y = avg_duration, 
             fill = customer_type)) +
  geom_text(aes(label = my_label_2), 
            vjust = -0.5, 
            hjust = -0.1) +
  geom_col(width = 0.5, position = "dodge") +
  labs (title = "Viz 6: Average Ride Duration by Customer v.s. Day of Week", 
        x = "Customer Types", y = "Average Ride Duration (seconds)",
        fill = "Customer Types")
ggsave("Viz 6 Average Duration by Customers vs Day of Week.png")

##Demand in a day
#Most popular time in a day
demand_1_day <- trips_data_cleaned %>%
  group_by(customer_type, starttime, starttime_hm) %>%
  summarise(num_rides = n(), 
            avg_duration = round(mean(ride_length), digits = 0),
            .groups = 'drop') %>%
  arrange(desc(num_rides)) %>%
  ungroup()
head(demand_1_day, 10)

demand_1_day$starttime_hm <- format(demand_1_day$starttime, format = "%H:%M") ##No need, changed top already
#Make a new column giving the column for the label (my_label)
demand_1_day <- demand_1_day %>% 
  group_by(customer_type) %>%
  mutate(my_label = ifelse(num_rides == max(num_rides),
                           paste(starttime_hm), 
                           "")
         ) %>%
  ungroup()

#Viz 7: Demand in a day
demand_1_day %>%
  ggplot(aes(x= starttime, 
             y = num_rides, 
             color = customer_type)) + 
  geom_line() + 
  geom_text_repel(aes(label = my_label), 
                  box.padding =  2,  #additional padding around each text label
                  max.overlaps = Inf) +  #to override overlaps and show all labels
  scale_x_datetime(date_breaks = "1 hour",  #Set distance between 1 hour breaks
                   minor_breaks = NULL,  #no minor breaks
                   date_labels = "%H:%M",  #Specify labels as H:M
                   expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Viz 7: Total Rides by Customer v.s. 24 Hours",
       x = "Time of One Day", 
       y = "Total Number of Rides",
       fill = "Customer Types")
ggsave("Viz 7 Total Rides by Customer v.s. 24 Hours.png")

#Bike type
bike_type <- 
  trips_data_cleaned %>% 
  group_by(customer_type, rideable_type) %>%
  summarize(num_rides = n(), 
            avg_duration = round(mean(ride_length), digits = 0),
            .groups = 'drop') %>%
  ungroup()
bike_type

#Viz 8: Types of bike vs number of rides
bike_type %>%
  ggplot(aes(x = customer_type,
             y = num_rides,
             fill = customer_type)) +
  geom_bar(stat = 'identity',     #To use the y-value for the dependent variable
           width = 0.5) +
  facet_wrap(~rideable_type) +
  geom_text(aes(label = num_rides), vjust = -0.5, size = 3) + 
  labs(title = "Viz 8: Total Rides by Customer each Bike Type",
       x = "Customer Type", 
       y = "Total Number of Rides",
       fill = "Customer Types")
ggsave("Viz 8 Total Rides by Customer each Bike Type.png")

#Viz 9: Types of bike vs average ride
bike_type %>%
  ggplot(aes(x = customer_type,
             y = avg_duration,
             fill = customer_type)) +
  geom_bar(stat = 'identity',     #To use the y-value for the dependent variable
           width = 0.5) +
  facet_wrap(~rideable_type) +
  geom_text(aes(label = avg_duration), vjust = -0.5, size = 3) + 
  labs(title = "Viz 9: Average Ride Length by Customer and each Bike Type",
       x = "Customer Type", 
       y = "Average Ride Duration (seconds)",
       fill = "Customer Types")
ggsave("Viz 9 Average Ride Length by Customer and each Bike Type.png")

#############################For Tableau
demand_time <- trips_data_cleaned %>%
  select(customer_type, ride_length, rideable_type,
         month, day_of_week, starttime_hm)
#Summarize based on unique rows
demand_time <- demand_time %>%
  group_by(customer_type, rideable_type, 
           month, day_of_week, starttime_hm) %>%
  summarise(num_trips = n(), 
            avg_duration = round(mean(ride_length), digits =0),
            .groups ='keep') %>%
  distinct(customer_type, rideable_type,
           month, day_of_week, starttime_hm,
           .keep_all = TRUE) %>%
  ungroup()
#Export to csv for Tableau Viz
write.csv(demand_time, "demand_time.csv")