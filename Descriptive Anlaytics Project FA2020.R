################################################
#### DESCRIPTIVE ANALYTICS PROJECT FA2020"######
################################################

###IMPORT DATASET####
housing <- read.csv("~/Downloads/housing.csv")
install.packages("dplyr")
library(dplyr)
install.packages("reshape")
library(reshape2)

###PREPROCESSING####
View(housing)
head(housing)
tail(housing)
summary(housing) ##check for any NA's and confirm numeric variables and categorical variables have proper outputs
levels(housing$ocean_proximity) ###lists the number of classes of categorical variable 
plot(housing$population,housing$mean_bedrooms) ###2 outliers, could be apartments?
hist(housing$median_house_value,
     main="Median House Value",
     xlab = "Median House Value, in USD",
     ylab = "Count",
   ) ###figure 3
hist(housing$housing_median_age) ### farily normally distrubuted 
mean(housing$median_house_value)###mean house value is $206,855.80
mean(housing$housing_median_age)###mean house age is 28.64 years
mean(housing$median_income)### mean income is $38706.71
install.packages('tseries') #installs the tseries package if not already installed
library(tseries) #loads "tseries" library - need to first install "tseries" package
#conducts a hypothesis test for normality called the Jarque-Bera test
jarque.bera.test(housing$median_house_value) #null hypothesis: data is distributed normally
##does not follow a normal distrubution 
ggplot(data = melt(housing), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')
sns.set()
housing %>% ggplot(aes(x = ocean_proximity)) +
  geom_bar() ###Fig. 6


####CLEANING PROCESS####
housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms , na.rm = TRUE) ##removes NAs from total bedrooms variable
summary(housing)
housing$mean_bedrooms = housing$total_bedrooms/housing$households ###changes the total bedrooms to average number of bedrooms 
housing$mean_rooms = housing$total_rooms/housing$households ###changes total rooms to average number of rooms 
drops = c('total_bedrooms', 'total_rooms') 
housing = housing[ , !(names(housing) %in% drops)]
head(housing)
categories = levels(housing$ocean_proximity) ###creates values that separates the categories 
housing_cat = data.frame(ocean_proximity = housing$ocean_proximity)###creates new table with categorical values only
View(housing_cat)
for(cat in categories){
  housing_cat[,cat] = rep(0, times= nrow(housing_cat))
}
head(housing_cat)
for(i in 1:length(housing_cat$ocean_proximity)){
  cat = as.character(housing_cat$ocean_proximity[i])
  housing_cat[,cat][i] = 1
}
head(housing_cat)
columns_cat = names(housing_cat)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
housing_cat = select(housing_cat,one_of(keep_columns))
tail(housing_cat)
colnames(housing)
drops = c('ocean_proximity','median_house_value') ###want to scale numerical data so remove categorical variable and variable of interest which is house value
num_housing =  housing[ , !(names(housing) %in% drops)]
head(num_housing)
scaled_num_housing = scale(housing_num) ##scale numerical variables 
head(scaled_num_housing)
housing_cleaned = cbind(housing_cat, scaled_housing_num, median_house_value=housing$median_house_value)
head(housing_cleaned)
summary(housing_cleaned)
View(housing_cleaned)
write.csv(housing_cleaned,"TIDY_Housing_Dataset.csv") ###exports tidy dataset to computer 


###EXPLORATORY PROCESS####
dim(housing_cleaned) ##20640 observations, 14 variables 
head(housing_cleaned)
install.packages('ggplot2')
library(ggplot2)
class(housing_cleaned)
?house_cleaned
ggplot(housing_cleaned, aes(x = population, y = households)) + 
  geom_point() ###Fig. 5
ggplot(housing_cleaned, aes(x = median_income)) + geom_histogram() ##Fig 6
ggplot(housing_cleaned, aes(x = housing_median_age, y= median_house_value)) + 
  geom_point() 















