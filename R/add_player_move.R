
add_player_move <- function(game_board, player_num, position = NULL){

  # find columns that have an empty spot
  free_moves <- which(game_board[1, ] == 0)

  # print(paste0("found free moves at positions"))
  # print(free_moves)

  # If board is full, break while loop, board is full
  if (length(free_moves) == 0) stop("game board is full")

  # If no position supplied, chose a random move
  if (is.null(position)) {
    if (length(free_moves) == 1){
      position <- free_moves
    } else {
      position <- sample(x = free_moves, size = 1)
    }
  }

  if (! position %in% free_moves) stop("chosen position is not an available move")

  # Sanity check
  stopifnot(position %in% free_moves)

  # place a tile in that column as far down the board as needed
  game_board[max(which(game_board[, position] == 0)), position] <- player_num

  return(game_board)

}
