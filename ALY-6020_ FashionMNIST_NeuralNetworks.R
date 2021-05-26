#Install tensorflow pakage for R
#Install tensorflow
install.packages("tensorflow")
library(tensorflow)
install_tensorflow()

#Uncomment to run once
#install.packages("keras")
#Install Keras package for R
install_keras()
#Chek installation suceeded
library(keras)
library(reticulate)
#reticulate ::py_config()
#Check if the tensorflow package is fully installed
tf$constant("Hellow Tensorflow")

#Load the training dataset
Myfashion_train <- read.csv(file.choose(), header = TRUE, stringsAsFactors= FALSE)
#View the fashion training dataset 
View(Myfashion_train)
#loading the test dataset
Myfashion_test <- read.csv(file.choose(), header = TRUE, stringsAsFactors= FALSE)
# To View the test dataset 
View(Myfashion_test)

#Training set separation for labels and images 
#To separate the images of the data representing pixels 
train_img1<- Myfashion_train[,-1]
View(train_img1)
#Separating the labels of the training data
train_lab <- Myfashion_train$label
View(train_lab)

#Test set separation for labels and images 
#Separating the images for the test data 
test_img1<- Myfashion_test[,-1]
#Separating the labels for the test data
test_lab <- Myfashion_test$label

# Converting the images from the trained dataset into matrix
train_images <- data.matrix(train_img1)
# Converting the images of test dataset into matrix
test_images <- data.matrix(test_img1)
View(test_images)

# Shows structure of the training data
str(train_images)
# Shows structure of the test data
str(test_images)


# Build the network
network <- keras_model_sequential() %>%
   layer_dense(units = 512, activation = "relu", input_shape = c(28 * 28)) %>%
   layer_dense(units = 10, activation = "softmax")

# Show network data
network


# Set data for the compile function
network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# Categorically encode labels for boyj training and test data sets
train_labels <- to_categorical(train_lab)
test_labels <- to_categorical(test_lab)

# Train the network using fit function
network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)

# Evaluate model using test data
metrics <- network %>% evaluate(test_images, test_labels)

# Show evaluation data
metrics

#Predict first 20 samples of the test data set
network %>% predict_classes(test_images[1:20,])

#Show first 20 labels from the original test data set
Myfashion_test$label[1:20]

# Extract 10th Image from training data
Image <- Myfashion_train$pixel.10
# Visualize the matrix in to image
plot(as.raster(Image, max = 255))






