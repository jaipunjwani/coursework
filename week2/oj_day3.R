#Let's return to the orange juice assignment and investigate how store demographics are related to demand.
#Let's start with the following model: logmove ~ log(price)*brand*feat and add in the store demographics as linear features (e.g., + AGE60 + EDUC + ETHNIC + INCOME). Try them individually and then all together.
lm.fit = lm(data = oj, logmove ~ log(price)*brand*feat)
#R-squared:  0.5354 
lm.fit = lm(data = oj, logmove ~ log(price)*brand*feat + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM + HVAL150)
#R-squared:  0.5681 (improved by ~ 0.03) 

#What demographics are significantly (t > 2 standard deviations) related to demand?
#All of the demographic factors that were included are related to demand (t>2)

#Let's focus on two variables HHLARGE ("fraction of households that are large") and EDUC ("fraction of shoppers with advanced education").
#What are the means and percentiles of each of these variables?
lm.fit = lm(data = oj, logmove ~ log(price)*HHLARGE + log(price)*EDUC)
mean(oj$EDUC) #0.2252196
mean(oj$HHLARGE) #0.1156024

#25, 50, 75 percentiles
quantile(oj$HHLARGE, c(0.25, 0.5, 0.75))
quantile(oj$EDUC, c(0.25, 0.5, 0.75))

#Using your coefficient estimates from the regression in 1b:
#If we move from the median value of HHLARGE to the 75th percentile (3rd quartile), how much does logmove change each week on average? You can estimate this visually if you plot the fitted model, or you can compare the predicted values for rows that have the median and 75th percentiles for HHLARGE.
model = lm(data = oj, logmove ~ log(price)*HHLARGE + log(price)*EDUC) #probably the wrong model
oj$predicted = fitted(model)
median_75_oj = oj %>% filter(HHLARGE == quantile(oj$HHLARGE, 0.75) | HHLARGE == median(oj$HHLARGE))
ggplot(median_75_oj, aes(x=HHLARGE, y=predicted)) + geom_smooth(method="lm")
#from the plot above, the change in the predicted sales is approx. -0.14

#If we move from the median value of EDUC to the 75th percentile (3rd quartile), how much does logmove change each week on average?
median_75_oj = oj %>% filter(EDUC == quantile(oj$EDUC, 0.75) |EDUC == median(oj$EDUC))
ggplot(median_75_oj, aes(x=EDUC, y=predicted)) + geom_smooth(method="lm") + geom_vline(xintercept = median(oj$EDUC)) + geom_vline(xintercept = quantile(oj$EDUC, 0.75))
#from the plot above, the change in the predicted sales is approx. 0.04

#Based on this analysis, which is the more important predictor of demand?
#since HHLARGE has the bigger average effect on price, it is the more important predictor in this analysis

#Now let's see if these variables impact price sensitivity. Add two interaction terms (with logprice) to the model to test this.
#What are the coefficients on the interaction terms?
model = lm(data = oj, logmove ~ log(price)*HHLARGE + log(price)*EDUC)
coef(model)
#log(price):HHLARGE -3.84550  |  log(price):EDUC     3.60242

#Recall, positive values indicate lower price sensitivity and negative values indicate greater price sensitivity. Do your estimates make sense based on your intuition?
#HHLARGE has a negative beta value, meaning it has higher sensitivitiy than education, which is positive
#contextually this makes sense because household size probably has a bigger impact on buying OJ vs. education level. Also, the analysis from the previous part supports this as well

#What are the coefficient estimates on the constants EDUC and HHLARGE? How do they compare to your regression from 1b?
#previously HHLARGE -1.18884 ; EDUC  0.47262 
#now        HHLARGE  0.42150 ; EDUC -2.99777 

#Similar to 2b, if we move from the median value of each variable to the 3rd quartile, how much does elasticity change? Based on this, which is more important to price sensitivity?
#possibly duplicate answer as 2b, because we used same model there

#You should notice that the coefficients on EDUC and HHLARGE have flipped sign once we include interaction terms with price. HHLARGE now appears to be a positive demand shifter and increases price sensitivity. Explain in words or pictures what is going on.
#Now education (negative) has a higher effect on price sensitivity?

#Let's split our data into a training set and a test set. An easy way to do this is with the sample command. The following will randomly select 20% of the rows in our data frame: indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))
#Now let's use this index to create a training and a test set, try: OJtest=oj[index, ] and Ojtrain=oj[-index, ]. What did this do? How many rows does the test set have? How many rows does the training set have?
#this generated random index numbers for the OJ observations, and assigned 20% of the rows to testing, and 80% to training

#      4        #
#Now let's run the very simple model logmove ~ log(price) + brand on the training data.
#Use LM on this model and report the R-squared.
#R-squared: 0.3972
#Use predict(model, Ojtest) to predict log sales for the test set.
#Compute cor(predicted_sales,logmove)^2 on the test set. This is our "honest R-squared". How does it compare to the value in (a)?
cor(predicted_sales, OJtest$logmove)
#0.4757573
#R-squared: 0.226345 , smaller than the previous R-squared (meaning this model is worse than previous)

#      5       #
#Now let's run better models.
#Run our "previous favorite" logmove ~ brand*log(price)*feat on the training data. Use LM to get regular R-squared. Now, follow the procedure in (3) to compute "honest R-squared". What is it? How do they compare?
modelTrain = lm(logmove~ brand*log(price)*feat, OJtrain)
#R-squared: 0.54
cor(predict(modelTrain, OJtest), OJtest$logmove)^2
#honest R-squared: 0.516225  #higher R-squared means much better model

#Now add in all the demographics. What is the regular R-squared on training data? What is the honest R-squared on the test set?
modelTrain = lm(logmove~ brand*log(price)*feat + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM + HVAL150, OJtrain)
summary(modelTrain) #R-squared: 0.5746
cor(predict(modelTrain, OJtest), OJtest$logmove)^2
#honest R-squared: 0.5558506  #so far this is the best model, according to R-squared