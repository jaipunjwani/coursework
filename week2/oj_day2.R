#Make a plot of the distribution of prices.
ggplot(data = oj, aes(x=price)) + geom_histogram()

#Change the x-axis on this plot to use a logarithmic scale using scale_x_log10()
ggplot(data = oj, aes(x=price)) + geom_histogram() + scale_x_log10()

#Repeat previous questions, faceted by brand.
ggplot(data = oj, aes(x=price)) + geom_histogram() + facet_wrap(~ brand)
ggplot(data = oj, aes(x=price)) + geom_histogram() + scale_x_log10() + facet_wrap(~brand)


#What do these graphs tell you about the variation in price? Why do the log plots look different? Do you find them more/less informative?
#The log plots look different because a log scale is less than a linear scale and allows us to see variation in data better. Gaps that were seemingly present 
#in the linear scale were filled in in the log scale graph. 

attach(oj)
oj$logprice = log(price)

# Visualizing the quantity/price relationship.
#Plot logmove (the log of quantity sold) vs. log price.
ggplot(oj, aes(x=logmove, y=logprice)) + geom_point() 

#Color each point by brand. What do insights can you derive that were not apparent before?
ggplot(oj, aes(x=logmove, y=logprice, color=brand)) + geom_point() 

#Do a regression of logmove on log price. How well does the model fit? What is the elasticity (the coefficient on log price), and does it make sense? 
lm.fit = lm(data = oj, logmove ~logprice) #elasticity of price: -1.601307 

#Now add in an intercept term for each brand (by adding brand to the regression formula). How do the results change? How should we interpret these coefficients?
lm.fit = lm(data = oj, logmove ~logprice + brand -1)

#Now add interaction terms to allow the elasticities to differ by brand, by including a brand:log price term in the regression formula. Note the estimate coefficients will "offset" the base estimates. What is the insights we get from this regression? What is the elasticity for each firm? Do the elasticities make sense?
plot(lm(logmove ~logprice + brand + brand:logprice))

#Which brand is featured the most? Make a plot to show this.
oj %>% filter(feat==1) %>% ggplot(aes(feat)) + geom_bar() + facet_wrap(~ brand)

#How should we incorporate the "featured in store" variable into our regression? Start with an additive formulation (e.g. feature impacts sales, but not through price).
#Now run a model where features can impact sales and price sensitivity.
#Now run a model where each brand can have a different impact of being featured and a different impact on price sensitivity. Produce a table of elasticties for each brand, one row for "featured" and one row for "not featured" (you need 6 estimates).