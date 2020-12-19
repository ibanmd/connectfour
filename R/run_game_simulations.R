
library(parallel)

run_game_simulations <- function(number_of_games = 10,
                                 output_folder = NULL,
                                 number_of_cores = 1,
                                 player1_model_path = NULL,
                                 player2_model_path = NULL){

  player1_model_path <- "/Users/marioibanez/Desktop/Studying/ConnectFour/Models/model_one_layer_yes_mirror_20weight.hdf5"
  output_folder <- paste0("/Users/marioibanez/Desktop/Studying/ConnectFour/Training_Runs/",
                          "training_run_20201214_trainedp1_1layer_yes_mirror_randomp2/")
  # Ensure that the output folder exists, where simulations will be saved
  dir.create(output_folder)
  stopifnot(dir.exists(output_folder))

  # Record the start time
  simulation_start_time <- Sys.time()
  start_time <- gsub(":", "_", simulation_start_time)

  # Print message
  print(paste0("Running ", number_of_games, " simulations, using ", number_of_cores, " cores, ",
               "saving results to ", output_folder))

  # Read in the model files for player 1 and 2
  if (!is.null(player1_model_path)) {
    print("using model for Player 1")
    #player1_model <- readRDS(player1_model_path)
    # Load up model
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

    # Load up weights from memory
    load_model_weights_hdf5(object = model,
                            filepath = player1_model_path)
  } else {
    player1_model <- NULL
  }

  # This is going to stay as the random model
  if (!is.null(player2_model_path) & FALSE) {
    print("using model for Player 2")
    player2_model <- readRDS(player2_model_path)
  } else {
    player2_model <- NULL
  }

  # Run simulation games in parallel and save results to output folder
  mclapply(1:number_of_games, FUN = function(x) {
    saveRDS(simulate_full_game(player1_model = player1_model_path,
                               player2_model = player2_model),
            file.path(output_folder, paste0(start_time, "_game_", x, ".rds")))
    return(NULL)
  }, mc.preschedule = TRUE, mc.cores = 1, mc.silent = TRUE)

  # Print message of how long the run took
  print(paste0("Runs complete, simulations took ", difftime(Sys.time(),
                                                            simulation_start_time,
                                                            units = "mins"), " minutes"))

}

