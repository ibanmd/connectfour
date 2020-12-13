
read_simulation_game_winners <- function(simulation_output_folder){

  library(data.table)

  simulation_runs <- lapply(list.files(simulation_output_folder, full.names = TRUE),
                            readRDS)

  results <- rbindlist(lapply(1:length(simulation_runs), function(x){

    data.table(NumberOfMoves = length(simulation_runs[[x]]$game_history),
               Winner = simulation_runs[[x]]$winner)

  }))

  results

}
