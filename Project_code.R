rm(list=ls())
cat("\014")

weather <- read.csv("/Users/hardikbhojwani/Downloads/weatherAUS.csv", stringsAsFactors = TRUE)

set.seed(345)
library(rpart)
library(rpart.plot)

#NA values in each variable
sapply(weather,function(x)sum(is.na(x)))

#---------------------------------Decision Tree-------------------------------------

Weather.nona <- weather[!is.na(weather$RainTomorrow), -1]

region.stats <- data.frame(Location=character()
                           ,Training_Accuracy=double()
                           ,Testing_Accuracy=double()
                           ,Accuracy_Difference=double())

for (k in unique(weather$Location)){
  print(k)
  Weather.temp <- Weather.nona[(Weather.nona$Location == k), ]
  # Weather.rain <- Weather.temp[(Weather.temp$RainTomorrow == "Yes"), ]
  # Weather.norain <- Weather.temp[(Weather.temp$RainTomorrow == "No"), ]
  # Weather.norain <- Weather.norain[sample(nrow(Weather.norain), nrow(Weather.rain)), ]
  # Weather.balanced <- rbind(Weather.rain, Weather.norain); rm(Weather.rain, Weather.norain)
  
  train <- sample(1:nrow(Weather.temp),nrow(Weather.temp)*(2/3))
  weather.train <- Weather.temp[train,]
  weather.test <- Weather.temp[-train,]
  
  fit <- rpart(RainTomorrow ~ .,
               data=weather.train,
               method="class",
               control=rpart.control(xval=0, minsplit=100),
               parms=list(split="information"))
  
  rpart.plot(fit, type = 1, extra = 4, main=paste("Classification Tree for Rainfall Prediction", k))
  
  pred <- predict(fit, weather.train, type="class")
  actual <- weather.train$RainTomorrow
  pt <- prop.table(table(pred, actual))
  
  pred.test <- predict(fit, weather.test, type="class")
  actual.test <- weather.test$RainTomorrow
  pt.test <- prop.table(table(pred.test, actual.test))
  
  de <- list(Location=k
             ,Training_Accuracy=pt[1,1] + pt[2,2]
             ,Testing_Accuracy=pt.test[1,1] + pt.test[2,2]
             ,Accuracy_Difference=(pt[1,1] + pt[2,2])-(pt.test[1,1] + pt.test[2,2]))
  region.stats[nrow(region.stats) + 1,] = de
  
}

# now we will split the data into testing and training data sets
# we will first randomly select 2/3 of the rows
train <- sample(1:nrow(Weather.nona),nrow(Weather.nona)*(2/3))

# Use the train index set to split the dataset
weather.train <- Weather.nona[train,]
weather.test <- Weather.nona[-train,]

# Classification Tree with rpart
# Fit model to training data
fit <- rpart(RainTomorrow ~ .,
             data=weather.train,
             method="class",
             control=rpart.control(xval=0, minsplit=100),
             parms=list(split="information"))

rpart.plot(fit, type = 1, extra = 4, main="Classification Tree for Rainfall Prediction")

pred.test <- predict(fit, weather.test, type="class")
actual.test <- weather.test$RainTomorrow
confusion_matrix <- prop.table(table(pred.test, actual.test))
#accuracy
confusion_matrix[1,1] + confusion_matrix[2,2]

rm(Weather.temp,weather.test,weather.train,actual,k,pred,pt,train,pt.test,pred.test,actual.test,de)


#------------------------------------------------------------------------------------------------

weather <- subset(weather,select = -c(Date,Evaporation,Cloud3pm,Sunshine,Cloud9am, Location))

summary(weather)

weather$RainTomorrow<-factor(weather$RainTomorrow,levels = c('Yes','No'))

weather[sapply(weather, is.character)] <- lapply(weather[sapply(weather, is.character)], 
                                                 as.factor)
weather <-na.omit(weather)

#for balanced data
# We can remove lines 99-103 for unbalanced data
weather.yesrain <- weather[(weather$RainTomorrow == "Yes"), ]
weather.norain <- weather[(weather$RainTomorrow == "No"), ]
weather.norain <- weather.norain[sample(nrow(weather.norain), nrow(weather.yesrain)), ]
weather.balanced <- rbind(weather.yesrain, weather.norain); rm(weather.yesrain, weather.norain)
weather <- weather.balanced


library('dplyr')

#histograms for different numerical variables
par(mfrow=c(2,3))
weather$MinTemp %>% hist(main='MinTemp')
weather$MaxTemp %>% hist(main='MaxTemp')
weather$Humidity9am %>% hist(main='Humidity9am')
weather$Humidity3pm %>% hist(main='Humidity3pm')
weather$Pressure9am %>% hist(main='Pressure9am')
weather$Pressure3pm %>% hist(main='Pressure3pm')
title(main = 'Comparison of each variable(After cleaning)', outer=TRUE)

library('ggplot2')
#install.packages('gridExtra')
library('gridExtra')

# box plot for numerical variables 
p_MaxTemp <- weather%>% ggplot(aes(y=MaxTemp,
                                 x=factor(RainTomorrow),
                                 fill=factor(RainTomorrow))) +
  geom_boxplot() + labs(title = "MaxTemp vs RainTomorrow")

p_MinTemp <- weather %>% ggplot(aes(y=MinTemp,
                                 x=factor(RainTomorrow),
                                 fill=factor(RainTomorrow))) +
  geom_boxplot() + labs(title = "MinTemp vs Raintomorrow")

p_Humidity9am <- weather %>% ggplot(aes(y=Humidity9am,
                                     x=factor(RainTomorrow),
                                     fill=factor(RainTomorrow))) +
  geom_boxplot() + labs(title = "Humidity9am vs RainTomorrow")

p_Humidity3pm <- weather %>% ggplot(aes(y=Humidity3pm,
                                     x=factor(RainTomorrow),
                                     fill=factor(RainTomorrow))) +
  geom_boxplot() + labs(title = "Humidity3pm vs RainTomorrow")

p_Pressure9am <- weather %>% ggplot(aes(y=Pressure9am,
                                     x=factor(RainTomorrow),
                                     fill=factor(RainTomorrow))) +
  geom_boxplot() + labs(title = "Pressure9am vs RainTomorrow")

p_Pressure3pm <- weather %>% ggplot(aes(y=Pressure3pm,
                                     x=factor(RainTomorrow),
                                     fill=factor(RainTomorrow))) +
  geom_boxplot() + labs(title = "Pressure3pm vs RainTomorrow")

grid.arrange(p_MaxTemp, p_MinTemp, p_Humidity9am, p_Humidity3pm,
             p_Pressure9am, p_Pressure3pm, ncol=2, top='Comparison of predictor varibales vs target variable')

#---------------------------------Naive Bayes Classifier-------------------------------------

set.seed(345)   # for reproducible results
train <- sample(1:nrow(weather), (0.8)*nrow(weather))
train.weather <- weather[train,]
test.weather <- weather[-train,]

library(e1071)

fit.nb <- naiveBayes(RainTomorrow ~ Temp3pm+Temp9am+Pressure9am+Pressure3pm+Humidity3pm+Humidity9am+WindSpeed9am+WindSpeed3pm+WindGustSpeed+Rainfall+MaxTemp+MinTemp, data = train.weather)
fit.nb


nb_test_predicted<-predict(fit.nb,test.weather)
tab1<- table(nb_test_predicted, test.weather$RainTomorrow)
tab1

#testing accuracy for naive bayes
testacc=sum(diag(tab1))/sum(tab1)
accuracy_test = testacc*100
accuracy_test

#---------------------------------Logistic regression-------------------------------------


classifier <-glm(formula = RainTomorrow ~ ., family = binomial, data = train.weather)

summary(classifier)
prob_pred = predict(classifier, type = 'response')
prob_prd_glm=predict(classifier, type = 'response', newdata = test.weather)
# use predict() with type = "response" to compute predicted probabilities. 
test.weather$logit.reg.pred <- predict(classifier, test.weather, type = "response")
# we choose 0.5 as the cutoff here for 1 vs. 0 classes
test.weather$RainTommorow_predict <- ifelse(test.weather$logit.reg.pred > 0.5, 1, 0)
test.weather$RainTommorow_predict = ifelse(prob_prd_glm > 0.5, 1, 0)
actual <- test.weather$RainTomorrow
predict <- test.weather$RainTommorow_predict
cm <- table(predict, actual)
cm#
tp <- cm[2,2]
tn <- cm[1,1]
fp <- cm[2,1]
fn <- cm[1,2]
# accuracy for logistic regression
test_accuracy <- (tp + tn)/(tp + tn + fp + fn)
test_accuracy

