
read_simulation_game_winners <- function(simulation_output_folder){

  library(data.table)

  # Do these for all four scenarios, and also make some plots and stuff
  simulation_output_folder <- "/Users/marioibanez/Desktop/Studying/ConnectFour/Training_Runs/training_run_20201213_random1_vs_random2/"
  simulation_output_folder <- "/Users/marioibanez/Desktop/Studying/ConnectFour/Training_Runs/training_run_20201213_trainedp1_2layer_nomirror_randomp2/"
  simulation_output_folder <- "/Users/marioibanez/Desktop/Studying/ConnectFour/Training_Runs/training_run_20201214_trainedp1_1layer_nomirror_randomp2/"
  simulation_output_folder <- "/Users/marioibanez/Desktop/Studying/ConnectFour/Training_Runs/training_run_20201214_trainedp1_1layer_yes_mirror_randomp2//"


  simulation_runs <- lapply(list.files(simulation_output_folder, full.names = TRUE, pattern = ".rds$"),
                            readRDS)

  results <- rbindlist(lapply(1:length(simulation_runs), function(x){

    data.table(NumberOfMoves = length(simulation_runs[[x]]$game_history),
               Winner = simulation_runs[[x]]$winner)

  }))

  results[, .(Total_Wins = .N,
              Win_Percentage = paste0(round(100 * .N / nrow(results), 2), "%"),
              Average_Num_Turns = mean(NumberOfMoves),
              Total_Turns = sum(NumberOfMoves)), by = .(Winner)]
  nrow(results)

}

t.test(c(rep(1, 783), rep(0, 217)),
       c(rep(1, 902), rep(0, 98)))
