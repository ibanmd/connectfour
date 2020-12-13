
# Purpose of this file is just to plot the % of 100 games one after
# the model is trained for 100 iterations, and then repeats

while(FALSE){

  tmp_output_folder <- "../../ConnectFour/tmp_simulations/"
  unlink(tmp_output_folder, recursive = TRUE)
  dir.create(tmp_output_folder)
  dir.create(paste0(tmp_output_folder, "/model/"))
  dir.create(paste0(tmp_output_folder, "/simulations/"))

  # Initialize null weights
  weights <- NULL

  # Initialize data.table
  dt_wins <- data.table(Round = c(NA),
                        Percent_Wins = c(NA))[!is.na(Round)]

  # train model with 100 iterations with those weights
  trained_model <- train_neural_network(training_data = sim_results,
                                        #iterations = 1000,
                                        weights = weights)

  saveRDS(trained_model, paste0(tmp_output_folder, "/model/player_1_model.rds"))

  run_game_simulations(number_of_games = 50,
                       output_folder = paste0(tmp_output_folder, "/simulations/"),
                       number_of_cores = 1,
                       player1_model_path = paste0(tmp_output_folder,
                                                   "/model/player_1_model.rds"))

  winners <- read_simulation_game_winners(simulation_output_folder = paste0(tmp_output_folder,
                                                                            "/simulations/"))

}
