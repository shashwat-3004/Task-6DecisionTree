# Attaching the required packages
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(GGally)
library(caret)


# Using iris dataset
data<-iris



# Basic summary of the data
head(data)




str(data)

summary(data)




# Some data visualization
ggpairs(data,aes(colour=Species))


# Splitting the data into training and test dataset
split<-sample.split(data,SplitRatio = 0.8)
training_set<-data[split,]
test_set<-data[!split,]



# Fitting the decision tree model using 2 variables
fit_sepal<-rpart(Species~Sepal.Length+Sepal.Width,training_set)
rpart.plot(fit_sepal,type=2)



fit_petal<-rpart(Species~Petal.Length+Petal.Width,training_set)
rpart.plot(fit_petal,type=2)

# Fitting the decision tree model using 4 variables

fit_all<-rpart(Species~Petal.Length+Petal.Width+Sepal.Width+Sepal.Length,training_set)
rpart.plot(fit_all,type=2)

# Prediction on test dataset
prediction_data<-predict(fit_all,test_set,type = "class")

tab<-table(test_set$Species,prediction_data)
tab

# Some insights about model accuracy and staistics
confusionMatrix(tab)
