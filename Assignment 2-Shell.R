#Programming 1 - Assignment 2
#Your name(s):


# Preparation: Load the required packages.
library(tidyverse)

#Part 1:

#Q1:
# Load the World values survey data "WorldValues3.csv" and save it as WorldData. 
# Load the country code data "CountryCodes3.csv" and save it as CountryData.
WorldData = read.csv("WorldValues3.csv")
CountryData = read.csv("CountryCodes3.csv")


#Q2:
# Join the country name from the CountryCode data into WorldData. 
# Note that country code is called *ID* in CountryData and *CountryCode* in WorldData. 
# Return the first 10 unique countries. 
WorldData = WorldData %>%  rename(ID = "CountryCode")
WorldData = WorldData %>%  left_join(CountryData, by = "ID")
WorldData %>% distinct(Country) %>% slice(1:10)
print(WorldData)

#Q3:
# Update WorldData by dropping income, education, and lifeexpect variables.
# What is the dimension of the updated WorldData?
worldData = WorldData %>%  select(-Income,-Education,-lifeexpect)
print(worldData)
dim(worldData)

#Q4:
# Using pipes, calculate the average age and percent immigrant (i.e., average ratio of immigrants) for every country.
# Display the top 5 countries in terms of average age. 
# Hint: For displaying the top 5, refer to the help for slice( ) to find the right form of the function to use.
# Calculate percent immigrant for every country in dataset.
worldData = worldData %>%  mutate(RatioImmigrants = Immigrant/Satisfied, na.rm =TRUE)
worldData = worldData %>%  mutate(Age = SurveyYear - BirthYear, na.rm = TRUE)
SummaryWorldData = worldData %>% group_by(Country) %>% summarise(avg_age = mean(Age, na.rm = TRUE), percentImmigrant = mean(RatioImmigrants, na.rm = TRUE))
TopFive = SummaryWorldData %>% arrange(desc(avg_age)) %>% slice(1:5)
print(TopFive)

#Q5:
# Group WorldData by country and marital status and show the percent of people in each category who are immigrants.
# Show the results for China. (the coding for the *Marital* variable is 1 = Married, 2 = Living together, 3 = Divorced, 
# 4 = Separated, 5 = Widowed, and 6 = Single).
MaritalworldData = worldData %>% group_by(Country , Marital) %>% summarise(mean(RatioImmigrants, na.rm = TRUE))
print( MaritalworldData %>% filter(Country == "China"))

#Q6: 
# Using pipes, create a function that takes a country name and calculates its average life satisfaction ("Satisfied") 
# broken down by "immigrant". Name this function meanSat. Show the results for China.
meanSat = function(country_name, worldData) {worldData %>% filter(Country == country_name) %>% group_by(Immigrant) %>% summarise(mean_sat = mean(Satisfied, na.rm = TRUE))}
meanSat("China", worldData)

#Part 2: 
# Load the web analytics data "WebAnalytics3.csv" and save it as analytics.
analytics = read.csv("WebAnalytics3.csv")


#Q7:
# Use base R to draw the scatterplot of TimeOnPage (x axis) versus PageViews (y axis).
# Make sure to change the shape, color, and size of the points. 
# Also, set your own labels for both axes and add a title to the plot. 
# In addition, set a limit on the x axis to be between 0 and 300,000.
plot(analytics$TimeOnPage ~ analytics$PageViews, data = analytics, pch = 19, col = "lightblue", cex = 2, xlim = c(0, 300000), xlab = "PagesViewed", ylab = "TimesOnPage")


#Q8:
# Recreate the same plot using ggplot. 
# (You can refer to ggplot cheatsheet or its reference page online for an overview of all the arguments.)
# (For example, here are some examples of geom_point() options: 
# https://ggplot2.tidyverse.org/reference/geom_point.html
analytics %>% ggplot(aes(x = TimeOnPage, y = PageViews)) + geom_point( color = "lightblue") + labs(x = "PagesViewed", y = "TimesOnPage") + xlim (0, 300000)


#Q9:
# a. Use ggplot to draw the scatterplot of TotalDownloads (x axis) versus ConversionRate (y axis).
# b. Draw a similar plot but add jitters. Use a theme and a color.
# (For both parts, read TotalDownloads as a factor for visual clarity on the x axis.)
analytics %>% ggplot(aes(x = TotalDownloads, y = ConversionRate)) + geom_jitter( color = "lightblue") + labs(x = "PagesViewed", y = "TimesOnPage", title = "Analytics") + theme(plot.title = element_text(color = "red", hjust = .5))


#Q10:
# Create a boxplot with Region as categories (x-axis) and ConversionRate as the outcome of interest (y-axis).
# Change the (border/points) color of the boxplots.
# In addition, add a title, a subtitle, and a blank x axis label. 
# In so doing, adjust the location, size, and the color of the title and the subtitle.
analytics %>% ggplot(aes(x = Region, y = ConversionRate)) + geom_boxplot(color = "purple") + labs(title = "Analytics2", subtitle = "RegionVConersionRate", x = "", Y = "ConversionRate") +
  theme(plot.title = element_text(color = "Orange", hjust = .5, size = 15)) + theme(plot.subtitle = element_text(color = "pink"))


#Q11:
# Create the same box plot of Q10 but use facet wrap to separate by Source.
# Also, fill each boxplot with different colors based on Source and 
# add a color legend to the bottom of the plot.


