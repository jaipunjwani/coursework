#5-fold cross validation 
library(dplyr)

#randomly shuffle data
set.seed(42)
indexes <- sample(1:nrow(trips_weather_df))
trips_weather_df = trips_weather_df[indexes,]
colnames(trips_weather_df) = c("ymd", "rain", "tavg", "tmin", "tmax", "numtrips", "weekend", "holiday")

foldSize = nrow(trips_weather_df)/5
rsquared = vector(mode="double", length = 5)
rmse = vector(mode="double", length = 5)

for(k in 1:5)
{
  #Use each k-fold as test data, and the rest as training
  start = (k-1)*foldSize+1
  end = k*foldSize
  test_indices = start:end
  test_data = trips_weather_df[test_indices,]
  train_data = trips_weather_df[-test_indices,]
  colnames(test_data) = colnames(trips_weather_df)
  colnames(train_data) = colnames(trips_weather_df)

  #train model
  model = lm(numtrips ~ tmax + rain + weekend, train_data)
 
  #use model to predicted values on train/test data
  train_data$predicted = fitted(model)
  test_data$predicted = predict(model, test_data)
  
  #obtain r-squared and rmse
  
  rsquared[k] = cor(test_data$predicted, test_data$numtrips)^2
  rmse[k] = sqrt(mean((test_data$numtrips-test_data$predicted)^2))
  
  #get r^2, add to total
  #get RMSE, add to total
}

c(mean(rsquared),mean(rmse))


