# Install required packages
# Uncomment lines below to install packages only once
# ---
install.packages("ISLR")
install.packages("tree")
install.packages("randomForest")
install.packages("class")
install.packages("MASS")
install.packages("gmodels")
install.packages("caret")
# ---

# Load library ISLR
library (ISLR)
# Load library tree
library(tree)
# Load rabdomforest library
library(randomForest)
#Laoding the class library
library(class)
#loading the mass Library
library(MASS)
#loading the gmodels library 
library(gmodels)
#loading the caret library
library(caret)

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

#Change to factor and label X9 values
Myfashion_train$X9 <- factor(Myfashion_train$X9 ,
                          levels =c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                          labels = c("Tshirt", "Top", "Pullover", "Dress", "Coat", "Sandal", "Shirt", "Sneaker", "Bag", "AnkleBoot"))
X9
Myfashion_test$X9 <- factor(Myfashion_test$X9, 
                         levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                         labels = c("Tshirt", "Top", "Pullover", "Dress", "Coat", "Sandal", "Shirt", "Sneaker", "Bag", "AnkleBoot"))

# Check if theX9 values have been labeled as mentioned 
round(prop.table(table(Myfashion_train$X9)) * 100, digits = 1)
round(prop.table(table(Myfashion_test$X9)) * 100, digits = 1)

#Checking the values of X0.12 before normalizing
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

#Building a decision tree
# No need to load library if already loaded 
library(tree)
#setting the seed to obtain the random results 
set.seed(1991)

#Tree model to be used on the entire training dataset 
Dtree = tree(X9 ~ .,data = Myfashion_train)
# Show the model content 
summary(Dtree)
#Graphically display tree model
plot(Dtree)
#Add node labels to the plot 
#Use category names for factor predictors 
text(Dtree)
Dtree

