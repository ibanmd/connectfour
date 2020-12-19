library(keras)
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test)

model %>% predict_classes(x_test)


#################################################################
#################################################################



library(keras)

# Parameters --------------------------------------------------------------

batch_size <- 32
epochs <- 200
data_augmentation <- TRUE


# Data Preparation --------------------------------------------------------

# See ?dataset_cifar10 for more info
cifar10 <- dataset_cifar10()

# Feature scale RGB values in test and train inputs
x_train <- cifar10$train$x/255
x_test <- cifar10$test$x/255
y_train <- to_categorical(cifar10$train$y, num_classes = 10)
y_test <- to_categorical(cifar10$test$y, num_classes = 10)


# Defining Model ----------------------------------------------------------

# Initialize sequential model
model <- keras_model_sequential()

model %>%

  # Start with hidden 2D convolutional layer being fed 32x32 pixel images
  layer_conv_2d(
    filter = 32, kernel_size = c(3,3), padding = "same",
    input_shape = c(32, 32, 3)
  ) %>%
  layer_activation("relu") %>%

  # Second hidden layer
  layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
  layer_activation("relu") %>%

  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%

  # 2 additional hidden 2D convolutional layers
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same") %>%
  layer_activation("relu") %>%
  layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
  layer_activation("relu") %>%

  # Use max pooling once more
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%

  # Flatten max filtered output into feature vector
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(512) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%

  # Outputs from dense layer are projected onto 10 unit output layer
  layer_dense(10) %>%
  layer_activation("softmax")

opt <- optimizer_rmsprop(lr = 0.0001, decay = 1e-6)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = opt,
  metrics = "accuracy"
)


# Training ----------------------------------------------------------------

if(!data_augmentation){

  model %>% fit(
    x_train, y_train,
    batch_size = batch_size,
    epochs = epochs,
    validation_data = list(x_test, y_test),
    shuffle = TRUE
  )

} else {

  datagen <- image_data_generator(
    rotation_range = 20,
    width_shift_range = 0.2,
    height_shift_range = 0.2,
    horizontal_flip = TRUE
  )

  datagen %>% fit_image_data_generator(x_train)

  model %>% fit_generator(
    flow_images_from_data(x_train, y_train, datagen, batch_size = batch_size),
    steps_per_epoch = as.integer(50000/batch_size),
    epochs = epochs,
    validation_data = list(x_test, y_test)
  )

}
