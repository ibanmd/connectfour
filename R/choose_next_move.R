

choose_next_move <- function(game_board = NULL,
                             player_model = NULL){

  # Find which cells are free
  free_moves <- which(game_board[1, ] == 0)

  # Prepare data for model
  nndata <- as.data.frame(matrix(as.vector(game_board), nrow = 1))
  names(nndata) <- paste0("X", 1:42)

  # Find which positions the model chose
  selected_positions <- round(predict(player_model, nndata), 4)[1, ]

  selected_positions <- data.table(Positions = 1:7,
                                   Percentage = selected_positions)

  selected_positions <- selected_positions[selected_positions$Positions %in% free_moves, ]
  position <- selected_positions[order(-selected_positions$Percentage), ]$Positions[1]

  # position <- choose_random_position(game_board = game_board)

  return(position)

}
