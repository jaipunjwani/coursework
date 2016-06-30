#NOTE: REGSUBSETS OF PACKAGE LEAP DOES THIS, BUT FINDS THE NEXT BEST MODELS AS WELL
#needs more general definition; include args for dependent variable, and ignore variable
find_best_feature = function(df, y)
{
col_names = names(df)
rsquared = vector(mode="numeric", length = length(col_names))
#rmse_vec

for(i in 1:length(col_names))
{
  col = col_names[i]
  if(col == y || col == "ymd")
  {
    #columns we don't care about (either the dependent variable itself, or something else)
    rsquared[i] = -1
  }
  else
  {
  testmodel = lm(df[[y]] ~ df[[col_names[i]]], df)
  rsquared[i] = summary(testmodel)$r.squared
  }
}
best_feature = which(rsquared == max(rsquared))
}

testdf = inner_join(weather, numtrips_2014)
colnames(testdf)[9] = "numtrips"
colnames(testdf)[find_best_feature(testdf, "numtrips")]
find_best_feature(testdf, "numtrips")
