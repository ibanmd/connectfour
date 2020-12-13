
choose_random_position <- function(game_board){

  # find columns that have an empty spot
  free_moves <- which(game_board[1, ] == 0)

  # print(paste0("found free moves at positions"))
  # print(free_moves)

  # If board is full, break while loop, board is full
  if (length(free_moves) == 0) stop("game board is full")

  # If no position supplied, chose a random move
  if (length(free_moves) == 1){
    position <- free_moves
  } else {
    position <- sample(x = free_moves, size = 1)
  }


  position

}
