
library(parallel)

run_game_simulations <- function(number_of_games = 10,
                                 output_folder = NULL,
                                 number_of_cores = 1,
                                 player1_model_path = NULL,
                                 player2_model_path = NULL){

  # Ensure that the output folder exists, where simulations will be saved
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
    player1_model <- readRDS(player1_model_path)
  } else {
    player1_model <- NULL
  }

  if (!is.null(player2_model_path)) {
    print("using model for Player 2")
    player2_model <- readRDS(player2_model_path)
  } else {
    player2_model <- NULL
  }

  # Run simulation games in parallel and save results to output folder
  mclapply(1:number_of_games, FUN = function(x) {
    saveRDS(simulate_full_game(player1_model = player1_model,
                               player2_model = player2_model),
            file.path(output_folder, paste0(start_time, "_game_", x, ".rds")))
    return(NULL)
  }, mc.preschedule = TRUE, mc.cores = 2, mc.silent = TRUE)

  # Print message of how long the run took
  print(paste0("Runs complete, simulations took ", difftime(Sys.time(),
                                                            simulation_start_time,
                                                            units = "mins"), " minutes"))

}

