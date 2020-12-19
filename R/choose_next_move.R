

choose_next_move <- function(game_board = NULL,
                             player_model = NULL){

  # Find which cells are free
  free_moves <- which(game_board[1, ] == 0)

  game_board[game_board == 2] <- -1

  state_values <- unlist(lapply(free_moves, function(x){
    played_move <- add_player_move(game_board = game_board, player_num = 1, position = x)
    updated_board <- array_reshape(played_move, c(1, 6, 7, 1))
    selected_positions <- model %>% predict(updated_board) %>% round()
  }))

  if(all(state_values == 0)){
    position <- sample(free_moves, size = 1)
  } else {
    position <- free_moves[which.max(state_values)]
  }

  return(position)

}
