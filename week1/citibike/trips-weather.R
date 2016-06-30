library(dplyr)
library(ggplot2)
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

#train a model on training set, and evaluate on both (train and test) sets of data
twmodel = lm(numtrips ~ tmin, trips_weather_training)
trips_weather_training$predicted = fitted(twmodel)
trips_weather_test$predicted = predict(twmodel, trips_weather_test)

summary(twmodel)$r.squared
#R-squared: 0.6769

#visual representation of actual vs. fitted data for training
ggplot(trips_weather_training) + geom_point(aes(x=tmin, y=numtrips)) + geom_line(aes(x=tmin, y=predicted))
cor(trips_weather_training$predicted, trips_weather_training$numtrips)^2 #honest R-squared: 0.6768572

#visual representation of actual vs. fitted data for test data
ggplot(trips_weather_test) + geom_point(aes(x=tmin, y=numtrips)) + geom_line(aes(x=tmin, y=predicted))
cor(trips_weather_test$predicted, trips_weather_test$numtrips)^2 #honest R-squared: 0.6864689

#add a quadratic term to your model and repeat
twmodel = lm(numtrips ~ tmin + poly(tmin, 2), trips_weather_training)
trips_weather_training$predicted = fitted(twmodel)
trips_weather_test$predicted = predict(twmodel, trips_weather_test)

summary(twmodel)$r.squared
#R-squared: 0.6768639

#visual representation of actual vs. fitted data for training
ggplot(trips_weather_training) + geom_point(aes(x=tmin, y=numtrips)) + geom_smooth(aes(x=tmin, y=predicted))
cor(trips_weather_training$predicted, trips_weather_training$numtrips)^2 #honest R-squared: 0.6768639

#visual representation of actual vs. fitted data for test data
ggplot(trips_weather_test) + geom_point(aes(x=tmin, y=numtrips)) + geom_smooth(aes(x=tmin, y=predicted))
cor(trips_weather_test$predicted, trips_weather_test$numtrips)^2 #honest R-squared: 0.6865667

#results: honest-R squared improved SLIGHTLY with model. However, t-value on tmin^2 is <2, suggesting that it was not significant
#Question: Was graph supposed to be quadratic?? Also, intercept has low t-value, does this mean we neglect it?

twtrain_cor = is(mode="double", length = 20)
twtest_cor = vector(mode="double", length = 20)
for(k in 1:20)
{
  twmodel = lm(numtrips ~ tmin + poly(tmin, k), trips_weather_training) #are we adding a successive term with a higherpower each time? or just increasing the power by 1?
  trips_weather_training$predicted = fitted(twmodel)
  trips_weather_test$predicted = predict(twmodel,trips_weather_test)
  twtrain_cor[k] = cor(trips_weather_training$predicted, trips_weather_training$numtrips)^2
  twtest_cor[k] = cor(trips_weather_test$predicted, trips_weather_test$numtrips)^2
  
}

#create data frame with columns 'k' and 'R-squared'
#CAREFUL: Make sure we plot cor^2 - taken care of
train_df = data.frame(1:20, twtrain_cor[1:20])
train_df = rename(train_df,"k" = X1.20)
train_df = rename(train_df, "rsquared" = twtrain_cor.1.20.)
ggplot(train_df) + geom_point(aes(x=k, y=rsquared))

test_df = data.frame(1:20, twtest_cor[1:20])
test_df = rename(test_df,"k" = X1.20)
test_df = rename(test_df, "rsquared" = twtest_cor.1.20.)
ggplot(test_df) + geom_point(aes(x=k, y=rsquared))

ggplot(test_df) + geom_point(aes(x=k, y=rsquared)) + geom_point(data = train_df, aes(x=k, y=rsquared))

#Reveals which value of k has best performance
testmax = (test_df %>% filter(rsquared == max(test_df$rsquared)))  #k=7
bestK = testmax$k[1]

#construct best model with optimal k value, and plot this on actual vs. predicted values on test data
bestmodel = lm(numtrips ~ tmin + poly(tmin, bestK), trips_weather_training)
trips_weather_test$predicted = predict(bestmodel,trips_weather_test) 
ggplot(trips_weather_test) + geom_point(aes(x=tmin, y=numtrips)) + geom_smooth(aes(x=tmin, y=predicted, color="red", method = "lm"))




