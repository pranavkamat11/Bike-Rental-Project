rm(list=ls(all=T))

setwd("C:/Users/Pranav/Desktop/Bike Rental Project")


library(ggplot2)
library(scales)
library(corrgram)
library(DMwR)
library(caret)
library(randomForest)
library(rpart)
library(HH)


## Read the data from csv file
data = read.csv("day.csv", header = T)




# remove dteday as it is redundant.We already have yr, mnth, holiday, weekday, workingday as attributes
# we also remove instant as it has no use in predictions
data$dteday = NULL
data$instant = NULL

# we also remove casual and registered as casual + registered = cnt. 
# if such is the case applying machine learning algoruthms for predictions make no sense
data$registered = NULL
data$casual = NULL



numerical_variables = c("temp","atemp","hum","windspeed")
categorical_vars = c("season","yr","mnth","holiday","weekday","workingday","weathersit")

#######  Oulier Analysis

for(i in 1:length(numerical_variables)){
  assign(paste0("gn",i), ggplot(aes_string(y = (numerical_variables[i]), x = "cnt"), data = subset(data))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "green" ,outlier.shape=15,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=numerical_variables[i],x="cnt")+
           ggtitle(paste("Box plot of cnt for",numerical_variables[i])))
}

#plot boxplots
gridExtra::grid.arrange(gn1,gn2,ncol=2)

gridExtra::grid.arrange(gn3,gn4,ncol=2)




#Replace all outliers with NA
for(i in numerical_variables){
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
 
  data[,i][data[,i] %in% val] = NA
}

# replacing NA's with means
for(i in numerical_variables){
  
  data[,i][is.na(data[,i])] <- mean(data[,i], na.rm = TRUE)
}

##### Outlier Analysis ends





############ Exploratory data analysis on numerical variables

# plot correlation matrix
# correlation plots can only be done on continuous variables

corrgram(data[numerical_variables], order=FALSE,
         main="correlation plot",upper.panel=panel.pie,text.panel=panel.txt)


# we observe that temp is highly correlated with atemp
# hence we have to remove one of them

##VIF calculation

print(vif(data[,c(8,9,10,11)]))

# after calculatinf vif we find that "atemp" higher vif than "temp". Later we will remove "atemp"




## plot scatterplots for continuous variables
scatterplotlist = list()

for(i in numerical_variables){
  plot = ggplot(data, aes_string(x = data[,i] , y = data$cnt)) +
         geom_point() +
         theme_bw() + ylab("cnt") + xlab(i) + ggtitle("Scatter Plot Analysis") +
         theme(text = element_text(size = 15))+
         scale_x_continuous(breaks = pretty_breaks(n = 10)) +
         scale_y_continuous(breaks =  pretty_breaks(n = 10))
  
  t  = length(scatterplotlist) + 1
  scatterplotlist[[t]] = plot
  
}
grid.arrange(grobs = scatterplotlist)


## plot histograms for continuous variables
histogramlist = list()


for(i in numerical_variables){
  plot = ggplot(data , aes_string( x = data[,i])) +
    geom_histogram(fill = "cornsilk" , colour = "black") + geom_density() +
    scale_y_continuous(breaks = pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    theme_bw() + xlab(i) + ylab("Frequency") + 
    ggtitle(i) + theme(text = element_text(size = 15))
  
  
  t  = length(histogramlist) + 1
  histogramlist[[t]] = plot
  
}
grid.arrange(grobs = histogramlist)

############ EDA on numerical variables ends.
# conclusion after EDA on continous variables
# clearly atemp has higher vif . hence we drop the variable


data$atemp = NULL


########################## Exploratory data analysis on categorical features

# bar plot of categorical features versus dependent variable cnt

barplotlist = list()

for(i in categorical_vars){
  
  
  plot = ggplot(data , aes_string( x = as.factor(data[,i]) , y = data$cnt)) +
    geom_bar(stat = "summary" , fill = "DarkSlateBlue" , fun.y =mean) + theme_bw() +
    xlab(i) + ylab("cnt") + scale_y_continuous(breaks = pretty_breaks(n = 8)) +
    ggtitle("Bike rental analysis") + theme(text = element_text(size = 15))
  
  t  = length(barplotlist) + 1
  barplotlist[[t]] = plot
  
}

grid.arrange(barplotlist[[1]], barplotlist[[2]])
grid.arrange(barplotlist[[3]], barplotlist[[4]])
grid.arrange(barplotlist[[5]], barplotlist[[6]])
grid.arrange(barplotlist[[7]])



################ ANOVA using f statistic
Fvalue_list = c()
k = 1
df = data.frame( matrix( ncol = 2 , nrow = 7))

x = c("categorical_variable","F_value")
colnames(df) = x

for(i in categorical_vars){
  
  a = aov(data[,i] ~ data$cnt , data = data)
  t = summary(a)
  Fvalue_list[[k]] = t[[1]]$F[[1]]
  k = k+1
}

df$categorical_variable = categorical_vars
df$F_value = Fvalue_list

ggplot(df,aes(x= reorder(df$categorical_variable,-df$F_value),df$F_value))+geom_bar(stat ="identity")+
  xlab("categorical_variable") + ylab("Fvalue")

### ANOVA ends



####### Split the data into train and test data sets

set.seed(1234)
# Divide the data set into train and test
train_index = sample(1:nrow(data) , 0.75 * nrow(data))

train = data[train_index,]
test = data[-train_index,]


############################################### Model Development #########################################################



######################################### Decision tree using  RPART (CART) ##################################################
# anova is used for regression
set.seed(1234)

fit = rpart(cnt ~. , data = train , method = "anova")

# Predict for new test cases
predictions = predict(fit,test[,-11])


regr.eval(test[,11], predictions , stats = c('mae','rmse','mape','mse'))

rpart.plot::rpart.plot(fit)


# tuning the model using caret

rpart_grid = expand.grid(cp = c(0.1,0.2,0.01,0.02,0.001,0.002))

# Define the control
trControl <- trainControl(method = "cv", number = 3, search = "grid",  
                          verboseIter = TRUE)

set.seed(1234)

tuned_tree_model = caret::train(cnt ~ .,
                                data = train,
                                method = "rpart",
                                metric = "MAE",
                                trControl = trControl,
                                tuneGrid = rpart_grid)

predictions = predict(tuned_tree_model,test[,-11])

regr.eval(test[,11], predictions , stats = c('mae','rmse','mape','mse'))




################################################## RANDOM FOREST REGRESSION #############################################

set.seed(1234)
rf_model = randomForest(cnt~.,data = train)

# Predict for new test cases
rf_predictions = predict(rf_model,test[,-11])


regr.eval(test[,11], rf_predictions , stats = c('mae','rmse','mape','mse'))


                        
                                  ###### Hyperparameter tuning for Random Forest



set.seed(1234)

# default ntrees = 500
rf_tuned_model = caret::train(cnt~.,
                          data = train,
                          method = 'rf',
                          metric = 'MAE',
                          trControl = trainControl(method = "repeatedcv",
                                                   number = 10,
                                                   repeats = 3,
                                                   verboseIter = TRUE
                                                   ),
                          tuneGrid = expand.grid(.mtry = c(1:10)),
                          ntree = 500
                          )


rf_tuned_predictions = predict(rf_tuned_model,test[,-11])

regr.eval(test[,11], rf_tuned_predictions , stats = c('mae','rmse','mape','mse'))




################################################## gradient boosting Regression ##########################################

gbmGrid = expand.grid(interaction.depth = c(4,6,8),
                      n.trees =(1:40)*50,
                      shrinkage = c(0.1,0.01) ,
                      n.minobsinnode = c(10,20))

# Define the control
trControl <- trainControl(method = "cv", number = 3, search = "grid",  
                          verboseIter = TRUE)

set.seed(1234)
gbr_tuned_model = caret::train(cnt ~ .,data = train, method = "gbm", metric = "MAE",  
                               trControl = trControl, tuneGrid = gbmGrid , 
                               verbose = FALSE)

# use model$bestTune to see the optimising parameters

gbr_tuned_predictions = predict(gbr_tuned_model,test[,-11])

regr.eval(test[,11], gbr_tuned_predictions , stats = c('mae','rmse','mape','mse'))




##################### Conclusion ####################

## Gradient Boost Regression performs the best on the dataset. It have the least MAE value



#store results of gradient boost regression to csv
df = data.frame( matrix( ncol = 2 , nrow = nrow(test)))
x = c("instant","count_predictions")
colnames(df) = x

df$instant = as.numeric(rownames(test))
df$count_predictions = round(gbr_tuned_predictions)

file = "Predictions using R.csv"

write.csv(df,file, row.names = FALSE)

print(paste0("csv file ",file," with predicted count values has been generated successfully"))
