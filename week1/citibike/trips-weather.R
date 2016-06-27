#Used to merge citibike 2014 trips with min. temperature from weather.csv
#MANUAL METHOD 
#get trips from 2014, with column of trips for each day
numtrips_2014 = trips %>% group_by(ymd) %>% select(ymd) %>% distinct(n()) %>% filter(grepl("2014.*", ymd))
#create new data frame with 3 desired columns (ymd, numtrips, tmin)
trips_weather_2014 = data.frame(numtrips_2014$ymd, numtrips_2014$`n()`, weather$tmin)

#rename columns 
#trips_weather_2014 = rename(trips_weather_2014, c("tmin" = weather.tmin,"numtrips" = numtrips_2014..n...,"ymd" = numtrips_2014.ymd )) #doesn't work..
trips_weather_2014 = rename(trips_weather_2014,"numtrips" = numtrips_2014..n...)
trips_weather_2014 = rename(trips_weather_2014, "tmin" = weather.tmin)
trips_weather_2014 = rename(trips_weather_2014, "ymd" = numtrips_2014.ymd)


#USING JOINS
#inner_join on ymd


#get random indexes to create 80%training/20%test data
indexes <- sample(1:nrow(trips_weather_2014), size=0.2*nrow(trips_weather_2014))
trips_weather_test=trips_weather_2014[indexes, ] 
trips_weather_training=trips_weather_2014[-indexes, ]

#try finding a model that fits
twmodel = lm(numtrips ~ tmin, trips_weather_2014)