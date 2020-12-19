
library(neuralnet)
library(keras)
library(ggplot2)

train_neural_network <- function(training_data, weights = NULL){

  # All data is in Player 1's perspective.  (On the board, 1 means self, 2 means opponent,
  # and 20 means Player 1 won, 0 means start of the game, and -20 means opponent won the game)

  training_data <-
    read_simulation_results(simulation_output_folder =
                              paste0("/Users/marioibanez/Desktop/Studying/ConnectFour/Training_Runs/training_run_20201214_trainedp1_1layer_yes_mirror_randomp2//"),
                                           mirror = TRUE)

  x_train <- array_reshape(training_data$board_array,
                           dim = c(dim(training_data$board_array)[1], 6, 7, 1))

  x_train[x_train == 2] <- -1

  #x_train[33,,,]

  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 4,
                  kernel_size = c(4, 4),
                  activation = 'relu',
                  padding = "same",
                  input_shape = c(6, 7, 1)) %>%
    # layer_conv_2d(filters = 4,
    #               kernel_size = c(4, 4),
    #               padding = "same",
    #               activation = 'relu') %>%
    layer_flatten() %>%
    layer_dense(units = 10,
                activation = 'relu') %>%
    layer_dense(units = 5,
                activation = 'relu') %>%
    layer_dense(units = 1)

  model %>% summary()

  model %>% compile(
    loss = loss_mean_squared_error,
    optimizer = optimizer_adadelta(),
    metrics = c('accuracy')
  )

  batch_size <- 2000
  epochs <- 100

  # Train model
  model %>% fit(
    x_train,
    training_data$board_value_array,
    batch_size = batch_size,
    epochs = epochs,
    validation_split = 0.2
  )

  predictions <- model %>% predict(x_train) %>% round()
  print(ggplot(data.frame(actual = training_data$board_value_array,
                    predicted = predictions),
         aes(actual, predicted)) +
    geom_jitter(alpha = 0.2) +
    geom_smooth(se = FALSE))

  print("Saving model")
  model %>% save_model_weights_hdf5("/Users/marioibanez/Desktop/Studying/ConnectFour/Models/model_one_layer_yes_mirror_20weight.hdf5",
                                    overwrite = TRUE)

}











