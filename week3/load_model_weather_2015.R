#2014 citibike model - to be used to predict 2015's number of trips
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

weather_2015 <- read.table('weather_2015.csv', header=T, sep=',')

# extract just a few columns, lowercase column names, and parse dates
weather_2015 <- select(weather_2015, DATE, PRCP, SNWD, SNOW, TMAX, TMIN)
names(weather_2015) <- tolower(names(weather_2015))
weather_2015 <- mutate(weather_2015,
                  tmin = tmin / 10,
                  tmax = tmax / 10,
                  ymd = as.Date(parse_datetime(date, "%Y%m%d")))
weather_2015 <- tbl_df(weather_2015)

df = weather_2015 %>% select(ymd, tmax, tmin,  prcp)

holidays15 = as.Date(c("2015-01-01", "2015-01-19", "2015-02-16", "2015-05-25", "2015-07-03", "2015-09-07", "2015-10-12", "2015-11-11", "2015-11-26", "2015-12-25"))

#SET WD to source directory, so these functions can run
source("C:/Users/Jai Punjwani/Desktop/coursework/week2/is_weekend.R")
source("C:/Users/Jai Punjwani/Desktop/coursework/week2/season_function.R")

df$holiday = df$ymd %in% holidays15
df = df %>% mutate(tavg = (tmax+tmin)/2, weekend = is_weekend(ymd), rainy = prcp>0.3, season=season(ymd), weekday = !weekend)

#enforces that column names match my model's name
colnames(trips_weather_df) = c("ymd", "tmax", "tmin", "rain", "holiday", "tavg", "weekend", "rainy", "season", "weekday")




