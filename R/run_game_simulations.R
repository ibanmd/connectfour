
library(parallel)

run_game_simulations <- function(number_of_games = 10,
                                 output_folder = NULL,
                                 number_of_cores = 1,
                                 player1_model_path = NULL){

  stopifnot(dir.exists(output_folder))

  simulation_start_time <- Sys.time()

  start_time <- gsub(":", "_", simulation_start_time)

  print(paste0("Running ", number_of_games, " simulations, using ", number_of_cores, " cores, ",
               "saving results to ", output_folder))

  if (!is.null(player1_model_path)) {
    print("using model for Player 1")
    player1_model <- readRDS(player1_model_path)
  }

  mclapply(1:number_of_games, FUN = function(x) {
    saveRDS(simulate_full_game(player1_model = player1_model,
                               player2_model = NULL),
            file.path(output_folder, paste0(start_time, "_game_", x, ".rds")))
    return(NULL)
  }, mc.preschedule = TRUE, mc.cores = 2, mc.silent = TRUE)

  print(paste0("Runs complete, simulations took ", difftime(Sys.time(),
                                                            simulation_start_time,
                                                            units = "mins"), " minutes"))

}

