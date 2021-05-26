#Naive Bayes
#Load the required library
library(e1071)

# Load training dataset
train_data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
train_data
#load test dataset
test_data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

# See the distribution of each class
table(train_data[,1])
table(test_data[,1])

#Convert both data sets to data frame
train_set <- as.data.frame(train_data)
test_set <- as.data.frame(test_data)

#Check class for the first column (output/class/label)
class(train_set[ ,1])
class(test_set[ ,1])

#Transform Labels as a factor in both data sets
train_set[ ,1] <- as.factor(train_set[ ,1])
test_set[ ,1] <- as.factor(test_set[ ,1])

#Check class
class(train_set[ ,1])
class(test_set[ ,1])

# Generate Naive Bayes Model
MyModel <- naiveBayes(label~., data = train_set)

# Check the summary of the generated model
# Exaplin results if using it
summary(MyModel) #output of the model

# The distribution of each class in the model - apriori
MyModel$apriori

# Predict values using model and test data set
MyPredictions <- predict(MyModel, test_set[ ,-c(1)] )

#Show accuracy data
table(test_set$label, MyPredictions)

#Error classification and checking the accuracy of the algorithm 
error.rate.naiveBayes <- sum(test_set$label != MyPredictions)/nrow(test_set) ##error classification and accuracy checking algorithm 
#Printing the results for accuracy 
print(paste0("Accuary (Precision): ", 1 - error.rate.naiveBayes))##printing the accuracy output

# Predict number 7
#Get all rows with label 7
MyRows <- test_set[which(test_set[ ,1] == "7"), ]

# Pick one row to test
#TestRow
testRow <-109

# Predict one selected row
MyPredict <- predict(MyModel, MyRows[testRow, -c(1)])

# Output result of predictions
cat("Predicted ", format(MyPredict), '. Actual label: ', format(MyRows[testRow,1]))


# Helper function to plot the image
rotate <- function(x) {
  return(t(apply(x, 2, rev)))
}

plot_matrix <- function(vec) {
  q <- matrix(vec, 28, 28, byrow = TRUE)
  nq <- apply(q, 2, as.numeric)
  image(rotate(nq), col = gray((0:255)/255))
}

# Plot the tested original label
plot_matrix(MyRows[testRow, -c(1)])

