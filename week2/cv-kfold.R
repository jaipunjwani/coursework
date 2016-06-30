#parameters:
#data - data frame used to train/test model
#formula - character string of entire formula (i.e. numtrips ~ tmax + weekend)
#folds - number of folds to perform (usually 5 or 10?)
#y - dependent variable (i.e. numtrips)


#run this to create function in environment
kfoldcv = function(data, formula, folds, y)
{
  #randomly shuffle data
  set.seed(42)
  indexes <- sample(1:nrow(data))
  data = data[indexes,]
  
  foldSize = nrow(data)/folds
  rsquared = vector(mode="double", length = folds)
  mse = vector(mode="double", length = folds)
  rmse = vector(mode="double", length = folds)
  
  for(k in 1:folds)
  {
    #Use each k-fold as test data, and the rest as training
    start = (k-1)*foldSize+1
    end = k*foldSize
    test_indices = start:end
    test_data = data[test_indices,]
    train_data = data[-test_indices,]
    colnames(test_data) = colnames(data)
    colnames(train_data) = colnames(data)
    
    #train model
    model = lm(as.formula(formula), train_data)
    
    #use model to predicted values on train/test data
    train_data$predicted = fitted(model)
    test_data$predicted = predict(model, test_data)
    
    #obtain r-squared and rmse
    
    rsquared[k] = cor(test_data$predicted, test_data[[y]])^2
    mse[k] = mean((test_data[[y]]-test_data$predicted)^2)
    rmse[k] = sqrt(mse[k])
    
  }
  
  #return vector with average rsquared, average rmse, and standard error
  c(mean(rsquared),mean(rmse), sd(rmse)/(sqrt(folds))
  
}

