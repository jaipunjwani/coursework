library(dplyr)

load('trips.RData')

# count the number of trips (= rows in the data frame)
summarize(trips, n())

# find the earliest and latest birth years (see help for max and min to deal with NAs)
summarize(trips, max_year=max(birth_year, na.rm = TRUE))
summarize(trips, min_year=min(birth_year, na.rm = TRUE))

# use filter and grepl to find all trips that either start or end on broadway
filter(trips, (grepl('Broadway', start_station_name)) | (grepl('Broadway', end_station_name)))

# do the same, but find all trips that both start and end on broadway
filter(trips, (grepl('Broadway', start_station_name)) & (grepl('Broadway', end_station_name)))

# use filter, select, and distinct to find all unique station names
trips_start = select(trips, start_station_name)
trips_end = select(trips, end_station_name)
trips_end = rename(trips_end, stations = end_station_name)
trips_start = rename(trips_start, stations = start_station_name)
rbind(trips_start, trips_end) %>% distinct()

# count trips by gender
trips %>% count(gender)

# find the 10 most frequent station-to-station trips
trips %>% mutate(station_to_station = paste(start_station_name, end_station_name))  %>% count(station_to_station) %>% arrange(desc(n)) %>% head(n=10) 
#alternate solution
trips %>% group_by(start_station_name, end_station_name) %>% count(start_station_name, end_station_name)%>% arrange(desc(n)) %>% head(10) 

# count all trips that start and end on broadway
filter(trips, (grepl('Broadway', start_station_name)) & (grepl('Broadway', end_station_name))) %>% select(start_station_name, end_station_name) %>% count()
