
read_simulation_results <- function(simulation_output_folder){

  library(data.table)

  simulation_runs <- lapply(list.files(simulation_output_folder, full.names = TRUE),
                            readRDS)

  player_1_wins <- Filter(function(x) x$winner == "Player 1", x = simulation_runs)

  player_2_wins <- Filter(function(x) x$winner == "Player 2", x = simulation_runs)

  player_1_winning_moves <- rbindlist(lapply(player_1_wins, function(x){
    player_turns <- Filter(function(y) y$player_turn == 1, x$game_history)
    rbindlist(lapply(player_turns, function(y){
      data.table(Game_State = paste0(as.vector(y$game_board), collapse = ","),
                 Chosen_Position = y$chosen_position)
    }))
  }))

  player_2_winning_moves <- rbindlist(lapply(player_2_wins, function(x){
    player_turns <- Filter(function(y) y$player_turn == 2, x$game_history)
    rbindlist(lapply(player_turns, function(y){
      data.table(Game_State = paste0(as.vector(y$game_board), collapse = ","),
                 Chosen_Position = y$chosen_position)
    }))
  }))

  player_1_winning_moves$Player <- 1
  player_2_winning_moves$Player <- 2

  player_winning_moves <- rbind(player_1_winning_moves, player_2_winning_moves)

  player_winning_moves

}
