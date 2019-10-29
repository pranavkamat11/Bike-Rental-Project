# BikeRentalProject
Predicting count of bike rentals based on seasonal and environmental factors using machine learning

## Dataset
day.csv is the dataset which contains 15 independent variables

* instant: record index
* dteday: Date
* season: Season(1: spring , 2: summer, 3: fall , 4: winter)
* yr: Year(0:2011 , 1:2012)
* mnth: (Month 1-12)
* holiday: whether a day is a holiday or not
* weekday: day of the week
* workingday: If a day is neither working day nor holiday then 1, otherwise 0
* weathersit: (weather condition, 1:Clear , 2:Mist + cloudy , 3:Light snow , 4:Heavy rain)
* temp: normalised temperature in celsius
* atemp: Normalised feeling temperature in celsius
* hum: Normalised humidity
* casual: count of casual bike users
* registered: count of registered bike users
* count: total number of bikes rented. This is the dependent continuous variable

## Steps followed
1. Missing Value Analysis
2. Outlier Analysis done using boxplots and imputing the outliers
3. Performed Exploratory Data Analysis 
3. Multicollinearity tackled using Pearson Correlation and analysis of Variance Inflation Factor
4. Used ANOVA for categorical feature selection
5. Model development
### Regression techniques applied
* Decision Tree Regression
* Random Forest Regression
* Gradient Boosting Regression



