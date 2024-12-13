Airbnb = read.csv("Listings.csv")
is.na(Airbnb)
sum(is.na(Airbnb))
Airbnb = read.csv("Listings.csv")
#1 . Report your exploratory analysis of the data. This can include data visualization, summary tables,
#changes made to the data, or any other insightful findings about the data.

str(Airbnb)
nrow(Airbnb)
ncol(Airbnb)
summary(Airbnb, na.rm=TRUE)
Color.names = c("black","green","yellow","red","orange","grey45")
library(tidyverse)
Price_neighborhood = aggregate(price ~ neighborhood, data = Airbnb, FUN = mean, na.rm = TRUE)
barplot(Price_neighborhood$price, names.arg = Price_neighborhood$neighborhood, main = "AVG PRICE BY NEIGHBORHOOD",xlab = "Neighbohood", ylab = "Price", col = Color.names)
str(Airbnb)
with(Airbnb, mean(avg_rating), na.rm=TRUE)

#3 . Write an R function that takes three arguments: a confidence level, the name of a numerical
#variable, and a data frame. The function should return the confidence interval for the average value
#of the specified variable at the given confidence level. Apply this function to compute a 95%
#confidence interval for the average price of the listings in the dataset. Provide an interpretation of
#the computed confidence interval in the context of the Airbnb listings

AvgPrice = mean(Airbnb$price, na.rm =TRUE)
N = length(na.omit(Airbnb$price))
SD = sd(Airbnb$price, na.rm = TRUE)
Standard_error = SD/sqrt(N)
alpha = 0.05
degrees_of_freedom = N - 1
t_score = qt(p = alpha/2, df = degrees_of_freedom, lower.tail = F)
margin_of_error = t_score * Standard_error
lower_bound = AvgPrice - margin_of_error
upper_bound = AvgPrice + margin_of_error
con_interval = function(a,b){ 
  pos_interval = a + b
  neg_interval = a - b
  var_name = deparse(substitute(Airbnb$price))
  DataSet = deparse(substitute(Airbnb))
  con_interval = c(pos_interval, neg_interval,var_name,DataSet)
  return(con_interval)
}
con_interval(AvgPrice,margin_of_error)

#5. Visualize price to test for normality and comment on the results (diagnostics plots generated for
#this question will be counted as only 1 plot in your report - see below for details).

Price_Visualization = hist(na.omit(Airbnb$price), xlab = "Price (Dollars)", ylim = c(0,800), xlim = c(0,850), main = "Price of Airbnb Listings", col = "blue")
#The graphical representation is positive skewed to the right. 

#7 Implement a multiple linear regression model for the “price” of the listings. Report the regression
#coefficients and measures of fit, and write an interpretation of the regression coefficients in the
#context of this model. Are there any violations of model assumptions?
  
Price.lr = lm(price ~ bedrooms + superhost, data = na.omit(Airbnb))
summary(Price.lr)
coefficients(Price.lr)
Price.lr$residuals
fitted(Price.lr)
plot(Price.lr)
# the line for this model is as followed, Price = 40.96 + 108.91x1 + 7.32x2. 
# it can be explained as the base price for a listing starts at 40.96 dollars and is then increased by bedrooms by 108.91 but then is
# impacted again by the superhost variable. The p value is < 0.05 which shows that the variables do hold a statistical significance. 
# the r-squared  is showing .4797 meaning that the two variables account for 47.97% of the price in the airbnb listings with a residual/standard error of 100.5.
#
predict(Price.lr, data.frame(bedrooms = c(5, 3), superhost = (c(FALSE, TRUE))))

