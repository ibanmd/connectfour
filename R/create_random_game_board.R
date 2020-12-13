
create_random_game_board <- function(num_moves = 7){

  stopifnot(num_moves >= 0 & num_moves <= 42 & is.integer(num_moves))

  # Connect four has 6 rows and 7 columns
  game_board <- matrix(data = rep(0, 42), nrow = 6)

  # initialize player 1 as the current player
  player_num <- 1

  # initialize counter
  i <- 1

  # Need to drop in the number of pieces according to num_moves
  while(i <= num_moves){

    # print to screen
    print(paste0(" - playing move ", i))

    # find columns that have an empty spot
    free_moves <- which(game_board[1, ] == 0)

    # print(paste0("found free moves at positions"))
    # print(free_moves)

    # If board is full, break while loop, board is full
    if (length(free_moves) == 0) next

    if (length(free_moves) == 1)
      # if only one free move, set the chosen column to that final option
      rand_col <- free_moves
    else {
      # choose a random column among the available options
      rand_col <- sample(x = free_moves, size = 1, replace = FALSE)
    }

    # print(paste("placing piece in column ", rand_col))

    # Sanity check
    stopifnot(rand_col %in% free_moves)

    # place a tile in that column as far down the board as needed
    game_board[max(which(game_board[, rand_col] == 0)), rand_col] <- player_num

    # next player's turn
    player_num <- ifelse(player_num == 1, yes = 2, no = 1)

    # add one to num moves
    i <- i + 1

  }

  return(game_board)

}
