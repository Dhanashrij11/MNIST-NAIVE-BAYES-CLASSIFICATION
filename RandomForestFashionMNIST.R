# Install required packages
# Uncomment lines below to install packages only once
# ---
install.packages("ISLR")
install.packages("tree")
install.packages("randomForest")
install.packages("class")
install.packages("MASS")
install.packages("gmodels")
# ---

# Load library ISLR
library (ISLR)
# Load library tree
library(tree)
# Load rabdomforest library
library(randomForest)
library(class)
library(MASS)
library(gmodels)

#Loading the train dataset
Myfashion_train <- read.csv(file.choose(), header = TRUE, stringsAsFactors= FALSE)
#View the dataset
View(Myfashion_train)
#Loading the test dataset
Myfashion_test <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
View(Myfashion_test)
#View test dataset

#Get the Fashion MNIST Data and attachig it to R
attach(Myfashion_train)
head(Myfashion_train)

#Check the structure of the data
str(Myfashion_train)
str(Myfashion_test)

#Checking the  distribution of X9 values(label here)
table(Myfashion_train$X9)
table(Myfashion_test$X9)

#Recode to factor and label X9 values
Myfashion_train$X9 <- factor(Myfashion_train$X9 ,
                          levels =c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                          labels = c("Tshirt", "Top", "Pullover", "Dress", "Coat", "Sandal", "Shirt", "Sneaker", "Bag", "AnkleBoot"))
X9
Myfashion_test$X9 <- factor(Myfashion_test$X9, 
                         levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                         labels = c("Tshirt", "Top", "Pullover", "Dress", "Coat", "Sandal", "Shirt", "Sneaker", "Bag", "AnkleBoot"))

# Show that X9 values have been labeled as mentioned 
round(prop.table(table(Myfashion_train$X9)) * 100, digits = 1)
round(prop.table(table(Myfashion_test$X9)) * 100, digits = 1)

#Checking the values of X0.17 before normalizing
#Values ranging from 0-255
summary(Myfashion_train$X0.12)
summary(Myfashion_test$X0.12)

#Create a function to normalize the numeric predictors to rescale them
NormMyNewData <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Test normalization function
#Using two sample vectors before and after normalization
NormMyNewData(c(1, 2, 3, 4, 5))
NormMyNewData(c(10, 20, 30, 40, 50))

#Create new variable with normalized data in all numeric columns
NormData_Train <- as.data.frame(lapply(Myfashion_train[2:785], NormMyNewData))
NormData_Test <- as.data.frame(lapply(Myfashion_test[2:785], NormMyNewData))

View(NormData_Train)
View(NormData_Test)

summary(NormData_Train$X0.12)
summary(NormData_Test$X0.12)

#Create target variable (X9) vectors
MyTrainData.labels <- Myfashion_train[1:59999, 1]
MyTestData.labels <- Myfashion_test[1:9999, 1]

library(tree)
set.seed(1991)

Dtree = tree(X9 ~ .,data = Myfashion_test)
summary(Dtree)
plot(Dtree)
text(Dtree)

MyPrediction <- predict(Dtree, Myfashion_test)
MSEdt <-  mean((Myfashion_test$X9 - MyPrediction)^2)
MSEdt

X9

library(randomForest)
set.seed(121)
rf.model <- randomForest(X9 ~ ., data = Myfashion_test, mtry = 10, ntree = 100, importance = T)
rf.pred <- predict(rf.model, Myfashion_test)

MSErf <- mean((Myfashion_test$X9 - rf.pred)^2)
MSErf  #0.3571055

#finding important variables
importance(rf.model)
plot(rf.model)

library(randomForest)
set.seed(121)
rf.model <- randomForest(X9 ~ ., data = Myfashion_test, mtry = 10, ntree = 10, importance = T)
rf.pred <- predict(rf.model, Myfashion_test)

MSErf <- mean((Myfashion_test$X9 - rf.pred)^2)
MSErf  #0.3571055

#finding important variables
importance(rf.model)
plot(rf.model)

library(randomForest)
set.seed(121)
rf.model <- randomForest(X9 ~ ., data = Myfashion_test, mtry = 10, ntree = 1, importance = T)
rf.pred <- predict(rf.model, Myfashion_test)

MSErf <- mean((Myfashion_test$X9 - rf.pred)^2)
MSErf  #0.3571055

#finding important variables
importance(rf.model)
plot(rf.model)

library(randomForest)
set.seed(121)
rf.model <- randomForest(X9 ~ ., data = Myfashion_test, mtry = 10, ntree = 1000, importance = T)
rf.pred <- predict(rf.model, Myfashion_test)

MSErf <- mean((Myfashion_test$X9 - rf.pred)^2)
MSErf  #0.3571055

#finding important variables
importance(rf.model)
plot(rf.model)