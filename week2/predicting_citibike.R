#Predicting daily Citibike trips
library(dplyr)
library(ggplot2)
library(lubridate)
library(glmnet)
#create DF - add weekend, holiday, and tavg columns among other relevant data
trips_2014 = trips %>% filter(grepl("2014.*", ymd))
weather = weather %>% mutate(tavg = (tmax+tmin)/2) #add average temp column #average reduces variance, but not necessarily best reflection of tmax/tmin correlation with numtrips
holidays = as.Date(c("2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27", "2014-12-25"))
numtrips_2014 = trips %>%
  group_by(ymd) %>% 
  select(ymd) %>% 
  distinct(n()) %>% 
  filter(grepl("2014.*", ymd))

#note, need to load is_weekend, season, kfoldcv functions into environment

trips_weather_df = inner_join(select(weather, ymd, prcp, tavg, tmin, tmax), numtrips_2014)
trips_weather_df$holiday = trips_weather_df$ymd %in% holidays
#rename columns
colnames(trips_weather_df) = c("ymd", "rain", "tavg", "tmin", "tmax", "numtrips", "holiday")
trips_weather_df = trips_weather_df %>%
  mutate(weekend = is_weekend(ymd), rainy = rain>0.3, season=season(ymd), weekday = !weekend)


#split data into training/testing sets
set.seed(42)
indexes <- sample(1:nrow(trips_weather_df), size=0.2*nrow(trips_weather_df))
test_data = trips_weather_df[indexes,]
training_data = trips_weather_df[-indexes,]

#constructing my model
my_model = lm(numtrips ~ tmax + rain + weekend, training_data)
summary(my_model)$r.squared

training_data$predicted = fitted(my_model)
test_data$predicted = predict(my_model, test_data)

ggplot(training_data, aes(x=tavg, y=numtrips)) + geom_point() + geom_line() + facet_wrap(~ weekend)
ggplot(test_data, aes(x=tavg, y=numtrips)) + geom_point() + geom_line() + facet_wrap(~ weekend)

cor(test_data$predicted, test_data$numtrips)^2

RMSE <- sqrt(mean((test_data$numtrips-test_data$predicted)^2))
RMSE

colnames(trips_weather_df)[find_best_feature(trips_weather_df, "numtrips")]


#Show points of actual values, and lines of predicted values
ggplot(test_data) + geom_point(aes(x=ymd, y=numtrips, shape = holiday)) + geom_line(aes(x=ymd,y=predicted))

#plot actual trips vs. predicted value (slope closer to 1 indicates good fit)
ggplot(test_data, aes(x=predicted, y=numtrips)) + geom_point(aes(shape = holiday)) + geom_abline(slope=1, color="blue")

#goal - get RMSE <3000

kfoldcv(data= trips_weather_df, formula = "numtrips ~ tmax + rain + weekend", folds= 5, y="numtrips")
kfoldcv(data= trips_weather_df, formula = "numtrips ~ tmax + rainy*rain + holiday + weekend", folds= 5, y="numtrips") #good numbers so far
kfoldcv(data= trips_weather_df, formula = "numtrips ~ tmax + rainy*rain + season*weekend", folds= 5, y="numtrips")
kfoldcv(data= trips_weather_df, formula = "numtrips ~ tmax*season*weekend + rainy*rain", folds= 5, y="numtrips") #good rsquared and rmse
kfoldcv(data= trips_weather_df, formula = "numtrips ~ tmax + season*rain*rainy + weekend", folds= 5, y="numtrips")

#Usng glmnet()
x_matrix = model.matrix (numtrips~tmax + rainy*rain + holiday + weekend,data=test_data)
y_vector = test_data$numtrips

cvglmnet = cv.glmnet(x_matrix, y_vector)
coef(cvglmnet)

#predict(object, ...)
#object type invokes predict.type
#for ex: predict(cvglmnet) is equivalent to calling predict.cv.glmnet(...)
#params: object, newx, s (lambda), 
#example below from predict.cv.glmnet documentation
x=matrix(rnorm(100*20),100,20)
y=rnorm(100)
cv.fit=cv.glmnet(x,y)
predict(cv.fit,newx=x[1:5,])
coef(cv.fit)
coef(cv.fit,s="lambda.min")
predict(cv.fit,newx=x[1:5,],s=c(0.001,0.002))



#test on 2015
#ensure that columns are the same

